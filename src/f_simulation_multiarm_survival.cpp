#include <Rcpp.h>
#include <cmath>

// [[Rcpp::plugins(cpp11)]]

#include "f_utilities.h"
#include "f_simulation_base_survival.h"

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

// Log-Rank Test Multi-Arm
//
// @param survivalDataSet DataFrame with columns: 
//   accrualTime, survivalTime, dropoutTime, treatmentArm, subGroup
// @param time Time point for analysis
// @param comparedTreatmentArms treatment arms to be compared (e.g. c(1,2))
// @param directionUpper Direction of test
// @param thetaH0 Null hypothesis hazard ratio
//
// [[Rcpp::export(name = ".logRankTestMultiArmCpp")]]
List logRankTestMultiArm(DataFrame survivalDataSet,
						 double time,
						 IntegerVector comparedTreatmentArms,
						 bool directionUpper = true,
						 double thetaH0 = 1.0) {

	NumericVector accrualTime = survivalDataSet["accrualTime"];
	NumericVector survivalTime = survivalDataSet["survivalTime"];
	NumericVector dropoutTime = survivalDataSet["dropoutTime"];
	IntegerVector treatmentArm = survivalDataSet["treatmentArm"];

    // Assert comparedTreatmentArms properties.
    if (comparedTreatmentArms.size() != 2) {
        stop("comparedTreatmentArms must be of length 2");
    }
    if (comparedTreatmentArms[0] == comparedTreatmentArms[1]) {
        stop("comparedTreatmentArms must contain two different treatment arms");
    }
    if (comparedTreatmentArms[0] < 1 || comparedTreatmentArms[1] < 1) {
        stop("comparedTreatmentArms must be positive integers");
    }

    // Check which patients are in the treatment arms to be compared
    // and recode treatmentArm to 1 and 2, because that is assumed
    // by logRankTest() below.
    LogicalVector isInFirstArm = (treatmentArm == comparedTreatmentArms[0]);
    LogicalVector isInSecondArm = (treatmentArm == comparedTreatmentArms[1]);
	treatmentArm[isInFirstArm] = 1;
    treatmentArm[isInSecondArm] = 2;	

    // Select subjects in the treatment arms to be compared and call
    // logRankTest().    
    LogicalVector isInSelectedArms = isInFirstArm | isInSecondArm;
    List results = logRankTest(
        accrualTime[isInSelectedArms],
        survivalTime[isInSelectedArms],
        dropoutTime[isInSelectedArms],
        treatmentArm[isInSelectedArms],
        time,
        directionUpper,
        thetaH0,
        false
    );
    NumericVector result = results["result"];

    double logRank = result[0];
    int subjectNumber = result[1];
    int events1 = result[2];
    int events2 = result[3];		

	return List::create(
		_["logRank"] = logRank,
		_["thetaH0"] = thetaH0,
		_["directionUpper"] = directionUpper,
		_["subjectNumber"] = subjectNumber,
		_["events"] = IntegerVector::create(events1, events2)
	);
}
