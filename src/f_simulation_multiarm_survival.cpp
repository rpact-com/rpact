#include <Rcpp.h>
#include <cmath>

// [[Rcpp::plugins(cpp11)]]

#include "f_utilities.h"

using namespace Rcpp;

// Get Simulation Survival Multi-Arm Stage Events
//
// Calculates stage events for specified conditional power (multi-arm)
//
// @param stage Stage number (1-based, like in R)
// @param directionUpper Direction of test
// @param conditionalPower Conditional power (NA if not used)
// @param conditionalCriticalValue Conditional critical values per stage (length kMax-1)
// @param plannedEvents Planned events per stage (length kMax)
// @param allocationRatioPlanned Allocation ratio planned per stage (length kMax)
// @param selectedArms Whether arms are selected (rows: arms, columns: stages)
// @param thetaH1 Alternative hypothesis hazard ratio (NA if not specified)
// @param overallEffects Matrix of overall effects (rows: arms, columns: stages)
// @param minNumberOfEventsPerStage Minimum number of events per stage (length kMax)
// @param maxNumberOfEventsPerStage Maximum number of events per stage (length kMax)
//
// [[Rcpp::export(name = ".getSimulationSurvivalMultiArmStageEventsCpp")]]
double getSimulationSurvivalMultiArmStageEvents(int stage,
                                                bool directionUpper,
                                                double conditionalPower,
                                                NumericVector conditionalCriticalValue,
                                                NumericVector plannedEvents,
                                                NumericVector allocationRatioPlanned,
                                                LogicalMatrix selectedArms,
                                                double thetaH1,
                                                NumericMatrix overallEffects,
                                                NumericVector minNumberOfEventsPerStage,
                                                NumericVector maxNumberOfEventsPerStage) {
    // To be consistent with the non-multiarm situation:
    stage = stage - 1;

    int gMax = overallEffects.nrow();

    double newEvents;

    if (!R_IsNA(conditionalPower)) {
        bool anySelected = Rcpp::as<bool>(any(na_omit(selectedArms(_, stage))));        

        if (anySelected) {
            // Determine thetaStandardized
            double thetaStandardized = NA_REAL;
            if (R_IsNA(thetaH1)) {

                NumericVector stageOverallEffects = overallEffects(_, stage - 1);
                stageOverallEffects = stageOverallEffects[selectedArms(_, stage)];
                stageOverallEffects = na_omit(stageOverallEffects);

                if (R_IsNA(directionUpper) || directionUpper) {
                    double minStageOverallEffect = min(stageOverallEffects);
                    thetaStandardized = std::log(std::max(1.0 + 1e-07, minStageOverallEffect));
                } else {
                    double maxStageOverallEffect = max(stageOverallEffects);
                    thetaStandardized = std::log(std::min(1.0 - 1e-07, maxStageOverallEffect));
                }
            } else {
                double adjustment = (R_IsNA(directionUpper) || directionUpper) ? 1e-07 : -1e-07;
                thetaStandardized = std::log(std::min(thetaH1, 1.0 + adjustment));
            }
            if (conditionalCriticalValue[stage - 1] > 8.0) {
                newEvents = maxNumberOfEventsPerStage[stage];
            } else {
                double alloc = allocationRatioPlanned[stage - 1];
                double criticalPart = std::max(0.0, conditionalCriticalValue[stage - 1] + getQNorm(conditionalPower));
                double numerator = std::pow((1.0 + alloc) * criticalPart, 2.0);
                double denominator = alloc * std::pow(thetaStandardized, 2.0);
                newEvents = numerator / denominator;
                // Bound by min/max
                double minEv = minNumberOfEventsPerStage[stage];
                double maxEv = maxNumberOfEventsPerStage[stage];
                if (newEvents < minEv) newEvents = minEv;
                if (newEvents > maxEv) newEvents = maxEv;
            }
        } else {
            newEvents = 0.0;
        }
    } else {
            newEvents = plannedEvents[stage] - plannedEvents[stage - 1];
    }

    return newEvents;
}
