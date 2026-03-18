#include <Rcpp.h>
#include <cmath>
#include <algorithm>

// [[Rcpp::plugins(cpp11)]]

#include "f_utilities.h"
#include "f_analysis_base.h"
#include "rpact_types.h"

using namespace Rcpp;

// Select Populations
//
// Selects populations based on effect vector and selection criteria
//
// @param effectVector Numeric vector of effect estimates
// @param typeOfSelection String specifying selection type: "all", "best", "rBest", "epsilon"
// @param epsilonValue Epsilon value for epsilon selection (ignored if not applicable)
// @param rValue Number of populations to select for rBest (ignored if not applicable)  
// @param threshold Threshold value - populations with effects <= threshold are excluded
//
// [[Rcpp::export(name = ".selectPopulationsCpp")]]
LogicalVector selectPopulations(NumericVector effectVector,
                               std::string typeOfSelection,
                               double epsilonValue = NA_REAL,
                               int rValue = NA_INTEGER,
                               double threshold = NA_REAL) {

    int gMax = effectVector.size();
    LogicalVector selectedPopulations(gMax, false);
    
    // Convert typeOfSelection to lowercase for case-insensitive comparison
    std::string typeOfSelectionLower = typeOfSelection;
    std::transform(typeOfSelectionLower.begin(), typeOfSelectionLower.end(), 
                   typeOfSelectionLower.begin(), ::tolower);
    
    if (typeOfSelectionLower == "all") {
        std::fill(selectedPopulations.begin(), selectedPopulations.end(), true);        
    } else if (typeOfSelectionLower == "best") {
        // Find index of maximum effect (excluding NA values)
        LogicalVector is_not_na = !is_na(effectVector);
        IntegerVector which_is_not_na = which(is_not_na);
        NumericVector availableEffectVector = effectVector[which_is_not_na];
        if (availableEffectVector.size() > 0) {
            auto maxIndex = std::max_element(availableEffectVector.begin(), availableEffectVector.end());
            selectedPopulations[which_is_not_na[std::distance(availableEffectVector.begin(), maxIndex)]] = true;
        }
    } else if (typeOfSelectionLower == "rbest") {
        if (R_IsNA(rValue) || rValue <= 0) {
            stop("rValue must be a positive integer for rBest selection");
        }
        rValue = std::min(rValue, gMax); // Ensure rValue does not exceed gMax
        LogicalVector is_not_na = !is_na(effectVector);
        IntegerVector which_is_not_na = which(is_not_na);
        NumericVector availableEffectVector = effectVector[which_is_not_na];
        IntegerVector effectOrder = order(-availableEffectVector);
        effectOrder = effectOrder[seq(0, rValue - 1)]; // Get top rValue indices
        IntegerVector mapped_indices = which_is_not_na[effectOrder - 1];
        selectedPopulations[mapped_indices] = true; // Convert to 0-based index
    } else if (typeOfSelectionLower == "epsilon") {
        if (R_IsNA(epsilonValue) || epsilonValue <= 0) {
            stop("positive epsilonValue must be specified for epsilon selection");
        }      
        double maxEffect = max(effectVector);
        NumericVector deltaToMax = maxEffect - effectVector;
        LogicalVector withinEpsilon = deltaToMax <= epsilonValue;
        selectedPopulations = withinEpsilon;
        selectedPopulations[which(is_na(effectVector))] = false; // Exclude NAs
    } else {
        stop("Invalid typeOfSelection. Must be 'all', 'best', 'rBest', or 'epsilon'");
    }
    LogicalVector effectBelowThreshold = is_na(effectVector) | (effectVector <= threshold);
    selectedPopulations[effectBelowThreshold] = false; // Exclude populations below threshold

    return selectedPopulations;
}

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

