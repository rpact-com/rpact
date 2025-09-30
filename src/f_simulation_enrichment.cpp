#include <Rcpp.h>
#include <cmath>

// [[Rcpp::plugins(cpp11)]]

#include "f_utilities.h"
#include "f_analysis_base.h"
#include "rpact_types.h"

using namespace Rcpp;

// Perform Closed Combination Test for Simulation Enrichment
//
// Performs the closed combination test for enrichment simulation
//
// @param stageResults List containing stage results with matrices:
//   testStatistics, separatePValues, selectedPopulations, 
//   populationEventsPerStage (optional), subjectsPerStage (optional)
// @param design Trial design object
// @param indices Matrix of indices for intersection hypotheses
// @param intersectionTest String specifying intersection test method:
//   "SpiessensDebois", "Bonferroni", "Simes", "Sidak"
// @param successCriterion String specifying success criterion: "all" or "any"
//
// [[Rcpp::export(name = ".performClosedCombinationTestForSimulationEnrichmentCpp")]]
List performClosedCombinationTestForSimulationEnrichment(
    List stageResults,
    Environment design,
    IntegerMatrix indices,
    std::string intersectionTest,
    std::string successCriterion) {
    
    // Check design type compatibility
    std::string designClass = getClassName(design);
    bool isGroupSequential = designClass == "TrialDesignGroupSequential";
    int kMax = Rcpp::as<int>(design.get("kMax"));
    
    if (isGroupSequential && kMax > 1) {
        stop("Group sequential design cannot be used for enrichment designs with population selection");
    }
    
    // Extract stage results
    NumericMatrix testStatistics = stageResults["testStatistics"];
    NumericMatrix separatePValues = stageResults["separatePValues"];
    LogicalMatrix selectedPopulations = stageResults["selectedPopulations"];
    
    int gMax = testStatistics.nrow();
    int nIntersections = indices.nrow();

    if (kMax != testStatistics.ncol() || kMax != separatePValues.ncol() || kMax != selectedPopulations.ncol()) {
        stop("Number of columns in stage results matrices must equal kMax");
    }

    // Verify that nIntersections equals 2^gMax - 1
    int expectedIntersections = static_cast<int>(pow(2, gMax)) - 1; // 2^gMax - 1
    if (nIntersections != expectedIntersections) {
        stop("Number of intersections must equal 2^gMax - 1");
    }

    // Initialize result matrices
    NumericMatrix adjustedStageWisePValues(nIntersections, kMax);
    std::fill(adjustedStageWisePValues.begin(), adjustedStageWisePValues.end(), NA_REAL);
    
    NumericMatrix overallAdjustedTestStatistics(nIntersections, kMax);
    std::fill(overallAdjustedTestStatistics.begin(), overallAdjustedTestStatistics.end(), NA_REAL);
    
    LogicalMatrix rejected(gMax, kMax);
    std::fill(rejected.begin(), rejected.end(), false);
    
    LogicalMatrix rejectedIntersections(nIntersections, kMax);
    std::fill(rejectedIntersections.begin(), rejectedIntersections.end(), false);
    
    LogicalMatrix futility(gMax, kMax - 1);
    std::fill(futility.begin(), futility.end(), false);
    
    LogicalMatrix futilityIntersections(nIntersections, kMax - 1);
    std::fill(futilityIntersections.begin(), futilityIntersections.end(), false);
    
    LogicalVector rejectedIntersectionsBefore(nIntersections);
    std::fill(rejectedIntersectionsBefore.begin(), rejectedIntersectionsBefore.end(), false);
    
    LogicalVector successStop(kMax);
    std::fill(successStop.begin(), successStop.end(), false);
    
    LogicalVector futilityStop(kMax - 1);
    std::fill(futilityStop.begin(), futilityStop.end(), false);
    
    // Get weights based on design type
    bool isDesignFisher = designClass == "TrialDesignFisher";
    bool isDesignInverseNormal = designClass == "TrialDesignInverseNormal";
    NumericVector weights;    
    if (isDesignFisher) {
        weights = getWeightsFisher(design);
    } else {
        weights = getWeightsInverseNormal(design);
    }
    
    // For single population, force Bonferroni
    if (gMax == 1) {
        intersectionTest = "Bonferroni";
    }
    
    // Main loop over stages
    for (int k = 0; k < kMax; k++) {
        
        NumericVector stageSeparatePValues = separatePValues(_, k);

        // Loop over intersection hypotheses
        for (int i = 0; i < nIntersections; i++) {
            
            // Get p-values available for this intersection, if there are none continue to next intersection
            LogicalVector isInIntersection = indices(i, _) > 0;
            NumericVector intersectStageSeparatePValues = stageSeparatePValues[isInIntersection];
            intersectStageSeparatePValues = na_omit(intersectStageSeparatePValues);
            int nAvailablePVals = intersectStageSeparatePValues.size();
            bool hasValidPValues = nAvailablePVals > 0;
            if (!hasValidPValues) continue;
            
            // Calculate adjusted stage-wise p-value based on intersection test
            if (intersectionTest == "SpiessensDebois") {
                // Handle SpiessensDebois method
                NumericMatrix sigma(1, 1);
                sigma(0, 0) = 1.0; // Default to identity
                
                // Check if subjectsPerStage is available
                bool hasSubjects = stageResults.containsElementNamed("subjectsPerStage");
                if (hasSubjects) {
                    NumericMatrix subjectsPerStage = stageResults["subjectsPerStage"];
                    NumericVector subjectsThisStage = subjectsPerStage(_, k);
                    LogicalVector isSubjectsSelected = isInIntersection & selectedPopulations(_, k);
                    NumericVector subjectsSelected = subjectsThisStage[isSubjectsSelected];
                    subjectsSelected = na_omit(subjectsSelected);

                    if (subjectsSelected.size() == 1) {
                        sigma = NumericMatrix(1, 1);
                        sigma(0, 0) = 1.0;
                    } else {
                        sigma = NumericMatrix(2, 2);
                        double corr = sqrt(subjectsSelected[0] / sum(subjectsSelected));
                        sigma(0, 0) = 1.0;
                        sigma(0, 1) = corr;
                        sigma(1, 0) = corr;
                        sigma(1, 1) = 1.0;
                    }
                } else {
                    // Use populationEventsPerStage
                    NumericMatrix populationEventsPerStage = stageResults["populationEventsPerStage"];
                    NumericVector populationEventsThisStage = populationEventsPerStage(_, k);  
                    LogicalVector isEventsSelected = isInIntersection & selectedPopulations(_, k);                  
                    NumericVector eventsSelected = populationEventsThisStage[isEventsSelected];
                    eventsSelected = na_omit(eventsSelected);     
                    
                    if (eventsSelected.size() <= 1) {
                        sigma = NumericMatrix(1, 1);
                        sigma(0, 0) = 1.0;
                    } else {
                        sigma = NumericMatrix(2, 2);
                        double corr = sqrt(eventsSelected[0] / eventsSelected[1]);
                        sigma(0, 0) = 1.0;
                        sigma(0, 1) = corr;
                        sigma(1, 0) = corr;
                        sigma(1, 1) = 1.0;
                    }
                }
                
                // Find maximum test statistic for this intersection
                NumericVector testStatsThisStage = testStatistics(_, k);
                NumericVector testStats = testStatsThisStage[isInIntersection];
                testStats = na_omit(testStats);
                double maxTestStatistic = max(testStats);

                adjustedStageWisePValues(i, k) = 1.0 - getMultivarNormalDistribution(wrap(maxTestStatistic), sigma);

            } else if (intersectionTest == "Bonferroni") {
                // Bonferroni adjusted p-values            
                double minAvailablePvals = min(intersectStageSeparatePValues);
                double bonferroniAdjustedPval = nAvailablePVals * minAvailablePvals;
                adjustedStageWisePValues(i, k) = std::min(1.0, bonferroniAdjustedPval);

            } else if (intersectionTest == "Simes") {
                // Simes adjusted p-values
                NumericVector sortedPValues = clone(intersectStageSeparatePValues);
                std::sort(sortedPValues.begin(), sortedPValues.end());
                IntegerVector seqAlongPvals = seq_len(nAvailablePVals);
                NumericVector seqAlongPvalsDouble = Rcpp::as<NumericVector>(seqAlongPvals);
                NumericVector simesAdjustedPValues = rep(nAvailablePVals * 1.0, nAvailablePVals) / 
                    seqAlongPvalsDouble * sortedPValues;
                adjustedStageWisePValues(i, k) = std::min(1.0, min(simesAdjustedPValues));

            } else if (intersectionTest == "Sidak") {
                // Sidak adjusted p-values
                double minAvailablePvals = min(intersectStageSeparatePValues);
                double OneMinusMinPvalsToPower = pow(1.0 - minAvailablePvals, nAvailablePVals);
                adjustedStageWisePValues(i, k) = 1.0 - OneMinusMinPvalsToPower;
            }
            
            // Calculate overall adjusted test statistic            
            if (isDesignFisher) {
                double product = 1.0;
                for (int j = 0; j <= k; j++) {
                    product *= pow(adjustedStageWisePValues(i, j), weights[j]);
                }
                overallAdjustedTestStatistics(i, k) = product;
            } else {
                double numerator = 0.0;
                double denominator = 0.0;
                for (int j = 0; j <= k; j++) {                    
                    numerator += weights[j] * getOneMinusQNorm(adjustedStageWisePValues(i, j));
                    denominator += pow(weights[j], 2.0);                    
                }
                overallAdjustedTestStatistics(i, k) = numerator / sqrt(denominator);
            }
            
            
            // Determine rejection and futility
            NumericVector criticalValues = design.get("criticalValues");
            
            if (isDesignFisher) {
                rejectedIntersections(i, k) = overallAdjustedTestStatistics(i, k) <= criticalValues[k];
                if (k < kMax - 1) {
                    NumericVector alpha0Vec = design.get("alpha0Vec");
                    futilityIntersections(i, k) = adjustedStageWisePValues(i, k) >= alpha0Vec[k];
                }
            } else if (isDesignInverseNormal) {
                rejectedIntersections(i, k) = overallAdjustedTestStatistics(i, k) >= criticalValues[k];
                if (k < kMax - 1) {
                    NumericVector futilityBounds = design.get("futilityBounds");
                    futilityIntersections(i, k) = overallAdjustedTestStatistics(i, k) <= futilityBounds[k];
                }
            }
            
            // Handle missing values
            if (R_IsNA(rejectedIntersections(i, k))) {
                rejectedIntersections(i, k) = false;
            }
            
            // Early termination check for final stage
            if (k == kMax - 1 && !rejectedIntersections(0, k)) {
                // We currently don't do that early break because then we don't get the adjusted p-values
                // for the other intersection hypotheses.

                // break;
            }
        }
        
        rejectedIntersections(_, k) = rejectedIntersections(_, k) | rejectedIntersectionsBefore;
        rejectedIntersectionsBefore = rejectedIntersections(_, k);

        LogicalVector futilityIntersectionsBefore;
        if (k < kMax - 1) {
            futilityIntersectionsBefore = futilityIntersections(_, k);
        }

        // Determine population-level rejections and futility
        for (int j = 0; j < gMax; j++) {
            LogicalVector isInStage = indices(_, j) == rep(1, nIntersections);      
            LogicalVector rejectedIntersectionInStage = rejectedIntersectionsBefore[isInStage];
            rejected(j, k) = Rcpp::as<bool>(all(na_omit(rejectedIntersectionInStage)));           
            
            if (k < kMax - 1) {                
                LogicalVector futilityIntersectionInStage = futilityIntersectionsBefore[isInStage];
                futility(j, k) = Rcpp::as<bool>(any(na_omit(futilityIntersectionInStage)));
            }
        }

        // Determine success stopping        
        LogicalVector selectedPopulationsThisStage = selectedPopulations(_, k);
        LogicalVector rejectedThisStage = rejected(_, k);

        if (successCriterion == "all") {
            LogicalVector rejectedThisStageThisPop = rejectedThisStage[selectedPopulationsThisStage];
            successStop[k] = Rcpp::as<bool>(all(rejectedThisStageThisPop));
        } else {
            successStop[k] = Rcpp::as<bool>(any(rejectedThisStage));
        }
        
        // Determine futility stopping for intermediate stages
        if (k < kMax - 1) {
            LogicalVector futilityThisStage = futility(_, k);
            LogicalVector futilityThisStageThisPop = futilityThisStage[selectedPopulationsThisStage];
            futilityStop[k] = Rcpp::as<bool>(all(futilityThisStageThisPop));

            LogicalVector selectedPopulationsNextStage = selectedPopulations(_, k + 1);
            bool noPopInNextStage = Rcpp::as<bool>(all(na_omit(!selectedPopulationsNextStage)));
            if (noPopInNextStage) {
                futilityStop[k] = true;
            }
        }
    }
    
    return List::create(
        _["separatePValues"] = separatePValues,
        _["adjustedStageWisePValues"] = adjustedStageWisePValues,
        _["overallAdjustedTestStatistics"] = overallAdjustedTestStatistics,
        _["rejected"] = rejected,
        _["rejectedIntersections"] = rejectedIntersections,
        _["selectedPopulations"] = selectedPopulations,
        _["successStop"] = successStop,
        _["futilityStop"] = futilityStop
    );
}

