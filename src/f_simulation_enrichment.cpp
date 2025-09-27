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
// @param design Trial design environment object
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
        
        // Loop over intersection hypotheses
        for (int i = 0; i < nIntersections; i++) {
            
            // Check if any p-values available for this intersection
            bool hasValidPValues = false;
            for (int g = 0; g < gMax; g++) {
                if (indices(i, g) == 1 && !R_IsNA(separatePValues(g, k))) {
                    hasValidPValues = true;
                    break;
                }
            }
            
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
                    NumericVector subjectsSelected;
                    
                    for (int g = 0; g < gMax; g++) {
                        if (indices(i, g) == 1 && selectedPopulations(g, k) && 
                            !R_IsNA(subjectsPerStage(g, k))) {
                            subjectsSelected.push_back(subjectsPerStage(g, k));
                        }
                    }
                    
                    if (subjectsSelected.size() == 1) {
                        sigma = NumericMatrix(1, 1);
                        sigma(0, 0) = 1.0;
                    } else if (subjectsSelected.size() == 2) {
                        sigma = NumericMatrix(2, 2);
                        double corr = sqrt(subjectsSelected[0] / (subjectsSelected[0] + subjectsSelected[1]));
                        sigma(0, 0) = 1.0;
                        sigma(0, 1) = corr;
                        sigma(1, 0) = corr;
                        sigma(1, 1) = 1.0;
                    }
                } else {
                    // Use populationEventsPerStage if available
                    bool hasEvents = stageResults.containsElementNamed("populationEventsPerStage");
                    if (hasEvents) {
                        NumericMatrix populationEventsPerStage = stageResults["populationEventsPerStage"];
                        NumericVector eventsSelected;
                        
                        for (int g = 0; g < gMax; g++) {
                            if (indices(i, g) == 1 && selectedPopulations(g, k) && 
                                !R_IsNA(populationEventsPerStage(g, k))) {
                                eventsSelected.push_back(populationEventsPerStage(g, k));
                            }
                        }
                        
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
                }
                
                // Find maximum test statistic for this intersection
                double maxTestStatistic = R_NegInf;
                for (int g = 0; g < gMax; g++) {
                    if (indices(i, g) == 1 && !R_IsNA(testStatistics(g, k))) {
                        maxTestStatistic = std::max(maxTestStatistic, testStatistics(g, k));
                    }
                }
                
                if (!std::isinf(maxTestStatistic)) {
                    NumericVector upper(1);
                    upper[0] = maxTestStatistic;
                    adjustedStageWisePValues(i, k) = 1.0 - getMultivarNormalDistribution(upper, sigma);
                }
                
            } else if (intersectionTest == "Bonferroni") {
                // Bonferroni adjusted p-values
                double minPValue = R_PosInf;
                int numValidPValues = 0;
                
                for (int g = 0; g < gMax; g++) {
                    if (indices(i, g) == 1 && !R_IsNA(separatePValues(g, k))) {
                        minPValue = std::min(minPValue, separatePValues(g, k));
                        numValidPValues++;
                    }
                }
                
                if (numValidPValues > 0) {
                    adjustedStageWisePValues(i, k) = std::min(1.0, numValidPValues * minPValue);
                }
                
            } else if (intersectionTest == "Simes") {
                // Simes adjusted p-values
                NumericVector selectedPValues;
                for (int g = 0; g < gMax; g++) {
                    if (indices(i, g) == 1 && !R_IsNA(separatePValues(g, k))) {
                        selectedPValues.push_back(separatePValues(g, k));
                    }
                }
                
                if (selectedPValues.size() > 0) {
                    std::sort(selectedPValues.begin(), selectedPValues.end());
                    double minSimes = R_PosInf;
                    
                    for (int j = 0; j < selectedPValues.size(); j++) {
                        double simesValue = (selectedPValues.size() / (j + 1.0)) * selectedPValues[j];
                        minSimes = std::min(minSimes, simesValue);
                    }
                    adjustedStageWisePValues(i, k) = minSimes;
                }
                
            } else if (intersectionTest == "Sidak") {
                // Sidak adjusted p-values
                double minPValue = R_PosInf;
                int numValidPValues = 0;
                
                for (int g = 0; g < gMax; g++) {
                    if (indices(i, g) == 1 && !R_IsNA(separatePValues(g, k))) {
                        minPValue = std::min(minPValue, separatePValues(g, k));
                        numValidPValues++;
                    }
                }
                
                if (numValidPValues > 0) {
                    adjustedStageWisePValues(i, k) = 1.0 - pow(1.0 - minPValue, numValidPValues);
                }
            }
            
            // Calculate overall adjusted test statistic
            if (!R_IsNA(adjustedStageWisePValues(i, k))) {
                if (isDesignFisher) {
                    double product = 1.0;
                    for (int j = 0; j <= k; j++) {
                        if (!R_IsNA(adjustedStageWisePValues(i, j))) {
                            product *= pow(adjustedStageWisePValues(i, j), weights[j]);
                        }
                    }
                    overallAdjustedTestStatistics(i, k) = product;
                } else {
                    double numerator = 0.0;
                    double denominator = 0.0;
                    
                    for (int j = 0; j <= k; j++) {
                        if (!R_IsNA(adjustedStageWisePValues(i, j))) {
                            numerator += weights[j] * getOneMinusQNorm(adjustedStageWisePValues(i, j));
                            denominator += weights[j] * weights[j];
                        }
                    }
                    
                    if (denominator > 0) {
                        overallAdjustedTestStatistics(i, k) = numerator / sqrt(denominator);
                    }
                }
            }
            
            // Determine rejection and futility
            if (!R_IsNA(overallAdjustedTestStatistics(i, k))) {
                NumericVector criticalValues = Rcpp::as<NumericVector>(design.get("criticalValues"));
                
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
            }
            
            // Handle missing values
            if (R_IsNA(rejectedIntersections(i, k))) {
                rejectedIntersections(i, k) = false;
            }
            
            // Early termination check for final stage
            if (k == kMax - 1 && !rejectedIntersections(0, k)) {
                // break; // Could implement early break if needed
            }
        }
        
        // Update rejectedIntersections with previous results
        for (int i = 0; i < nIntersections; i++) {
            rejectedIntersections(i, k) = rejectedIntersections(i, k) || rejectedIntersectionsBefore[i];
        }
        
        // Update rejectedIntersectionsBefore for next iteration
        for (int i = 0; i < nIntersections; i++) {
            rejectedIntersectionsBefore[i] = rejectedIntersections(i, k);
        }
        
        // Determine population-level rejections and futility
        for (int j = 0; j < gMax; j++) {
            bool allIntersectionsRejected = true;
            for (int i = 0; i < nIntersections; i++) {
                if (indices(i, j) == 1 && !rejectedIntersections(i, k)) {
                    allIntersectionsRejected = false;
                    break;
                }
            }
            rejected(j, k) = allIntersectionsRejected;
            
            if (k < kMax - 1) {
                bool anyIntersectionFutile = false;
                for (int i = 0; i < nIntersections; i++) {
                    if (indices(i, j) == 1 && futilityIntersections(i, k)) {
                        anyIntersectionFutile = true;
                        break;
                    }
                }
                futility(j, k) = anyIntersectionFutile;
            }
        }
        
        // Determine success stopping
        if (successCriterion == "all") {
            bool allSelectedRejected = true;
            for (int j = 0; j < gMax; j++) {
                if (selectedPopulations(j, k) && !rejected(j, k)) {
                    allSelectedRejected = false;
                    break;
                }
            }
            successStop[k] = allSelectedRejected;
        } else {
            bool anyRejected = false;
            for (int j = 0; j < gMax; j++) {
                if (rejected(j, k)) {
                    anyRejected = true;
                    break;
                }
            }
            successStop[k] = anyRejected;
        }
        
        // Determine futility stopping for intermediate stages
        if (k < kMax - 1) {
            bool allSelectedFutile = true;
            for (int j = 0; j < gMax; j++) {
                if (selectedPopulations(j, k) && !futility(j, k)) {
                    allSelectedFutile = false;
                    break;
                }
            }
            futilityStop[k] = allSelectedFutile;
            
            // Check if no populations selected for next stage
            bool anySelectedNext = false;
            if (k + 1 < selectedPopulations.ncol()) {
                for (int j = 0; j < gMax; j++) {
                    if (selectedPopulations(j, k + 1)) {
                        anySelectedNext = true;
                        break;
                    }
                }
            }
            if (!anySelectedNext) {
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

