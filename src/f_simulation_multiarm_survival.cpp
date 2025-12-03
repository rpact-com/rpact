#include <Rcpp.h>
#include <cmath>

// [[Rcpp::plugins(cpp11)]]

#include "f_utilities.h"
#include "f_simulation_base_survival.h"
#include "f_simulation_survival_utilities.h"
#include "f_simulation_multiarm.h"

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

// Get Treatments Multi-Arm
//
// Generates treatment assignments for multi-arm trials
//
// @param gMax Number of treatment arms (excluding control)
// @param maxNumberOfSubjects Maximum number of subjects
// @param allocationFraction Allocation fractions for treatment vs control
//
// [[Rcpp::export(name = ".getTreatmentsMultiArmCpp")]]
IntegerVector getTreatmentsMultiArm(int gMax, 
                                    int maxNumberOfSubjects,
                                    IntegerVector allocationFraction) {
    IntegerVector treatments(0);
    
    while (treatments.size() < maxNumberOfSubjects) {
        if (allocationFraction[0] > allocationFraction[1]) {
            // Add all arms (1 to gMax+1) repeated allocationFraction[1] times
            for (int r = 0; r < allocationFraction[1]; r++) {
                for (int g = 1; g <= gMax + 1; g++) {
                    treatments.push_back(g);
                }
            }
            // Add treatment arms only (1 to gMax) for remaining allocation
            int remaining = allocationFraction[0] - allocationFraction[1];
            for (int r = 0; r < remaining; r++) {
                for (int g = 1; g <= gMax; g++) {
                    treatments.push_back(g);
                }
            }
        } else {
            // Add all arms repeated allocationFraction[0] times
            for (int r = 0; r < allocationFraction[0]; r++) {
                for (int g = 1; g <= gMax + 1; g++) {
                    treatments.push_back(g);
                }
            }
            // Add control only for remaining allocation
            int remaining = allocationFraction[1] - allocationFraction[0];
            for (int r = 0; r < remaining; r++) {
                treatments.push_back(gMax + 1);
            }
        }
    }
    
    return treatments[seq_len(maxNumberOfSubjects) - 1];
}

// Generate survival and dropout times
List getSurvDropoutTimesMultiArm(int numberOfSubjects, 
                                 int gMax,
						         IntegerVector treatments,
						         NumericVector lambdaVector,
						         double kappa,
						         NumericVector phi) {
	// This is important to ensure that R's random number generator state is properly managed.
	Rcpp::RNGScope scope; 

	NumericVector survivalTime(numberOfSubjects);
	NumericVector dropoutTime(numberOfSubjects);
	
	for (int i = 0; i < numberOfSubjects; i++) {
        int treatmentArm = treatments[i] - 1; // Convert to 0-based!
        double u = R::runif(0, 1);
        survivalTime[i] = std::pow(-std::log(1 - u), 1.0 / kappa) / lambdaVector[treatmentArm];
        
        if (any(phi > 0).is_true()) {
            if (treatmentArm < gMax && phi[0] > 0) {
                dropoutTime[i] = -std::log(1 - R::runif(0, 1)) / phi[0];
            } else if (treatmentArm == gMax && phi[1] > 0) {
                dropoutTime[i] = -std::log(1 - R::runif(0, 1)) / phi[1];
            }
        } else {
            dropoutTime[i] = NA_REAL;
        }
    }
    
	return List::create(
		_["survivalTime"] = survivalTime,
		_["dropoutTime"] = dropoutTime
	);
}


// Update Treatments Vector
//
// Updates treatment assignments for subsequent stages based on selected arms
//
// [[Rcpp::export(name = ".updateTreatmentsVectorCpp")]]
IntegerVector updateTreatmentsVector(int k,
                                    int gMax,
                                    int maxNumberOfSubjects,
                                    IntegerVector numberOfSubjects,
                                    IntegerVector treatments,
                                    LogicalMatrix selectedArms,
                                    IntegerVector allocationFraction) {
    // Keep existing assignments up to previous stage
    IntegerVector newTreatments = treatments[seq_len(numberOfSubjects[k - 1]) - 1];
    
    // Find which arms are selected at stage k
    LogicalVector selected = selectedArms(_, k);
    IntegerVector whichSelected = which(selected) + 1; // Convert to 1-based
    
    while (newTreatments.size() < maxNumberOfSubjects) {
        if (allocationFraction[0] > allocationFraction[1]) {
            // Add selected arms + control repeated allocationFraction[1] times
            for (int r = 0; r < allocationFraction[1]; r++) {
                for (int i = 0; i < whichSelected.size(); i++) {
                    newTreatments.push_back(whichSelected[i]);
                }
                newTreatments.push_back(gMax + 1);
            }
            // Add selected arms only for remaining allocation
            int remaining = allocationFraction[0] - allocationFraction[1];
            for (int r = 0; r < remaining; r++) {
                for (int i = 0; i < whichSelected.size(); i++) {
                    newTreatments.push_back(whichSelected[i]);
                }
            }
        } else {
            // Add selected arms + control repeated allocationFraction[0] times
            for (int r = 0; r < allocationFraction[0]; r++) {
                for (int i = 0; i < whichSelected.size(); i++) {
                    newTreatments.push_back(whichSelected[i]);
                }
                newTreatments.push_back(gMax + 1);
            }
            // Add control only for remaining allocation
            int remaining = allocationFraction[1] - allocationFraction[0];
            for (int r = 0; r < remaining; r++) {
                newTreatments.push_back(gMax + 1);
            }
        }
    }
    
    return newTreatments[seq_len(maxNumberOfSubjects) - 1];
}

// Get Simulated Stage Results Survival Multi-Arm Subjects Based
//
// Calculates stage results for each simulation iteration step
//
// [[Rcpp::export(name = ".getSimulatedStageResultsSurvivalMultiArmSubjectsBasedCpp")]]
List getSimulatedStageResultsSurvivalMultiArmSubjectsBased(
        Environment design,
        NumericVector weights,
        bool directionUpper,
        NumericVector omegaVector,
        double piControl,
        double kappa,
        NumericVector phi,
        double eventTime,
        NumericVector plannedEvents,
        NumericVector recruitmentTimes,
        IntegerVector allocationFraction,
        std::string typeOfSelection,
        std::string effectMeasure,
        LogicalVector adaptations,
        double epsilonValue,
        double rValue,
        double threshold,
        NumericVector minNumberOfEventsPerStage,
        NumericVector maxNumberOfEventsPerStage,
        double conditionalPower,
        double thetaH1,
        Nullable<Function> calcEventsFunction = R_NilValue,
        bool calcEventsFunctionIsUserDefined = false,
        Nullable<Function> selectArmsFunction = R_NilValue) {
    
    // This is important to ensure that R's random number generator state is properly managed
    Rcpp::RNGScope scope;
    
    bool isDesignFisher = getClassName(design) == "TrialDesignFisher";
    bool isDesignConditionalDunnett = getClassName(design) == "TrialDesignConditionalDunnett";
    
    int kMax = plannedEvents.size();
    int gMax = omegaVector.size();
    int maxNumberOfSubjects = recruitmentTimes.size();
    double allocationRatio = (1.0 * allocationFraction[0]) / allocationFraction[1];
    
    // Initialize matrices and vectors
    NumericMatrix singleEventsPerStage(gMax + 1, kMax);
    std::fill(singleEventsPerStage.begin(), singleEventsPerStage.end(), NA_INTEGER);
    NumericMatrix cumulativeEventsPerStage(gMax, kMax);
    std::fill(cumulativeEventsPerStage.begin(), cumulativeEventsPerStage.end(), NA_INTEGER);
    NumericMatrix simSurvival(gMax, kMax);
    std::fill(simSurvival.begin(), simSurvival.end(), NA_REAL);
    NumericMatrix overallEffects(gMax, kMax);
    std::fill(overallEffects.begin(), overallEffects.end(), NA_REAL);
    NumericMatrix testStatistics(gMax, kMax);
    std::fill(testStatistics.begin(), testStatistics.end(), NA_REAL);
    NumericMatrix overallTestStatistics(gMax, kMax);
    std::fill(overallTestStatistics.begin(), overallTestStatistics.end(), NA_REAL);
    NumericMatrix separatePValues(gMax, kMax);
    std::fill(separatePValues.begin(), separatePValues.end(), NA_REAL);
    NumericVector conditionalCriticalValue(kMax - 1);
    std::fill(conditionalCriticalValue.begin(), conditionalCriticalValue.end(), NA_REAL);
    NumericVector conditionalPowerPerStage(kMax);
    std::fill(conditionalPowerPerStage.begin(), conditionalPowerPerStage.end(), NA_REAL);
    LogicalMatrix selectedArms(gMax, kMax);
    std::fill(selectedArms.begin(), selectedArms.end(), false);
    selectedArms(_, 0) = rep(true, gMax); // First stage has all arms selected.
    NumericVector adjustedPValues(kMax);
    std::fill(adjustedPValues.begin(), adjustedPValues.end(), NA_REAL);
    NumericVector analysisTime(kMax);
    std::fill(analysisTime.begin(), analysisTime.end(), NA_REAL);
    IntegerVector numberOfSubjects(kMax);
    std::fill(numberOfSubjects.begin(), numberOfSubjects.end(), NA_INTEGER);
    LogicalVector eventsNotAchieved(kMax);
    std::fill(eventsNotAchieved.begin(), eventsNotAchieved.end(), false);
    
    // Generate treatment assignments
    IntegerVector treatments = getTreatmentsMultiArm(gMax, maxNumberOfSubjects, allocationFraction);
    
    // Calculate hazards
    double lambdaControl = getLambdaByPi(piControl, eventTime, kappa);
    NumericVector lambdaVector(gMax + 1);
    for (int g = 0; g < gMax; g++) {
        lambdaVector[g] = omegaVector[g] * lambdaControl;
    }
    lambdaVector[gMax] = lambdaControl;
    
    // Generate survival and dropout times
    List tmp = getSurvDropoutTimesMultiArm(
        maxNumberOfSubjects,
        gMax,
        treatments,
        lambdaVector,
        kappa,
        phi
    );
    NumericVector survivalTime = tmp["survivalTime"];
	NumericVector dropoutTime = tmp["dropoutTime"];
    
    // Create survival dataset
    DataFrame survivalDataSet = DataFrame::create(
        _["accrualTime"] = recruitmentTimes,
        _["treatmentArm"] = treatments,
        _["survivalTime"] = survivalTime,
        _["dropoutTime"] = dropoutTime
    );
    
    // Main simulation loop over stages
    for (int k = 0; k < kMax; k++) {
        if (k == 0) {
            // First stage
            analysisTime[k] = findObservationTime(
                recruitmentTimes,
                survivalTime,
                dropoutTime,
                plannedEvents[k]
            );
            
            if (R_IsNA(analysisTime[k])) {
                eventsNotAchieved[k] = true;
                break;
            } else {
                numberOfSubjects[k] = sum(recruitmentTimes <= analysisTime[k]);
                
                for (int g = 0; g < gMax; g++) {
                    IntegerVector comparedArms = IntegerVector::create(g + 1, gMax + 1);
                    List logRankResult = logRankTestMultiArm(
                        survivalDataSet,
                        analysisTime[k],
                        comparedArms,
                        directionUpper
                    );
                    
                    testStatistics(g, k) = logRankResult["logRank"];
                    overallTestStatistics(g, k) = logRankResult["logRank"];
                    IntegerVector events = logRankResult["events"];
                    cumulativeEventsPerStage(g, k) = sum(events);
                    singleEventsPerStage(g, k) = events[0];
                    
                    if (g == gMax - 1) {
                        singleEventsPerStage(gMax, k) = events[1];
                    }
                }
            }
        } else {
            // Subsequent stages
            double maxAccrual = max(recruitmentTimes);
            bool analysisTimeOK = analysisTime[k - 1] < maxAccrual;
            
            LogicalVector selInBoth = selectedArms(_, k) & selectedArms(_, k - 1);
            bool allInBoth = Rcpp::as<bool>(all(selInBoth));
            bool diffInSelectedArms = !allInBoth;
            
            if (analysisTimeOK && diffInSelectedArms) {
                // Update treatments vector
                treatments = updateTreatmentsVector(
                    k,
                    gMax,
                    maxNumberOfSubjects,
                    numberOfSubjects,
                    treatments,
                    selectedArms,
                    allocationFraction
                );
                
                survivalDataSet["treatmentArm"] = treatments;
                
                // Generate new survival/dropout times for new subjects
                for (int i = numberOfSubjects[k - 1]; i < maxNumberOfSubjects; i++) {
                    int treatmentArm = treatments[i] - 1; // now 0-based!
                    
                    if (treatmentArm < gMax && selectedArms(treatmentArm, k)) {
                        double u = R::runif(0, 1);
                        survivalTime[i] = std::pow(-std::log(1 - u), 1.0 / kappa) / lambdaVector[treatmentArm];
                    }
                    
                    if (any(phi > 0).is_true()) {
                        if (treatmentArm < gMax && selectedArms(treatmentArm, k) && phi[0] > 0) {
                            dropoutTime[i] = -std::log(1 - R::runif(0, 1)) / phi[0];
                        } else if (treatmentArm == gMax && phi[1] > 0) {
                            dropoutTime[i] = -std::log(1 - R::runif(0, 1)) / phi[1];
                        } else {
                            dropoutTime[i] = NA_REAL;
                        }
                    } else {
                        dropoutTime[i] = NA_REAL;
                    }
                }
                
                survivalDataSet["survivalTime"] = survivalTime;
                survivalDataSet["dropoutTime"] = dropoutTime;
            }
            
            // Filter to selected arms + control
            IntegerVector selectedArmIndices = which(selectedArms(_, k)) + 1;
            selectedArmIndices.push_back(gMax + 1);
            
            IntegerVector treatmentArms = survivalDataSet["treatmentArm"];
            LogicalVector inSelectedArms(maxNumberOfSubjects);
            for (int i = 0; i < maxNumberOfSubjects; i++) {
                inSelectedArms[i] = false;
                for (int j = 0; j < selectedArmIndices.size(); j++) {
                    if (treatmentArms[i] == selectedArmIndices[j]) {
                        inSelectedArms[i] = true;
                        break;
                    }
                }
            }
            
            DataFrame survivalDataSetSelected = DataFrame::create(
                _["accrualTime"] = as<NumericVector>(survivalDataSet["accrualTime"])[inSelectedArms],
                _["treatmentArm"] = as<IntegerVector>(survivalDataSet["treatmentArm"])[inSelectedArms],
                _["survivalTime"] = as<NumericVector>(survivalDataSet["survivalTime"])[inSelectedArms],
                _["dropoutTime"] = as<NumericVector>(survivalDataSet["dropoutTime"])[inSelectedArms]
            );
            
            analysisTime[k] = findObservationTime(
                survivalDataSetSelected["accrualTime"],
                survivalDataSetSelected["survivalTime"],
                survivalDataSetSelected["dropoutTime"],
                plannedEvents[k]
            );
            
            if (R_IsNA(analysisTime[k])) {
                eventsNotAchieved[k] = true;
                break;
            } else {
                numberOfSubjects[k] = sum(recruitmentTimes <= analysisTime[k]);
                
                for (int g = 0; g < gMax; g++) {
                    if (selectedArms(g, k)) {
                        IntegerVector comparedArms = IntegerVector::create(g + 1, gMax + 1);
                        List logRankResult = logRankTestMultiArm(
                            survivalDataSet,
                            analysisTime[k],
                            comparedArms,
                            directionUpper
                        );
                        
                        overallTestStatistics(g, k) = logRankResult["logRank"];
                        IntegerVector events = logRankResult["events"];
                        singleEventsPerStage(g, k) = events[0];
                        cumulativeEventsPerStage(g, k) = sum(events);
                        
                        // Stage-wise test statistic
                        double numerator = std::sqrt(cumulativeEventsPerStage(g, k)) * overallTestStatistics(g, k) -
                                         std::sqrt(cumulativeEventsPerStage(g, k - 1)) * overallTestStatistics(g, k - 1);
                        double denominator = std::sqrt(cumulativeEventsPerStage(g, k) - cumulativeEventsPerStage(g, k - 1));
                        testStatistics(g, k) = numerator / denominator;
                        
                        if (g == gMax - 1) {
                            singleEventsPerStage(gMax, k) = events[1];
                        }
                    }
                }
            }
        }
        
        // Calculate separate p-values
        for (int g = 0; g < gMax; g++) {
            if (!R_IsNA(testStatistics(g, k))) {
                separatePValues(g, k) = R::pnorm(testStatistics(g, k), 0.0, 1.0, 0, 0);
            }
        }
        
        // Calculate overall effects
        for (int g = 0; g < gMax; g++) {
            if (!R_IsNA(overallTestStatistics(g, k))) {
                double exponent = (2.0 * directionUpper - 1.0) *
                                overallTestStatistics(g, k) *
                                (1.0 + allocationRatio) /
                                std::sqrt(allocationRatio) /
                                std::sqrt(cumulativeEventsPerStage(g, k));
                overallEffects(g, k) = std::exp(exponent);
            }
        }
        
        // Intermediate stage processing
        if (k < kMax - 1) {
            int numSelected = sum(selectedArms(_, k));
            if (numSelected == 0) {
                break;
            }
            
            // Bonferroni adjustment
            double minPValue = min(na_omit(separatePValues(_, k)));
            adjustedPValues[k] = std::min(minPValue * numSelected, 1.0 - 1e-07);
            
            // Conditional critical value
            if (isDesignConditionalDunnett) {
                double alpha = design["alpha"];
                double informationAtInterim = design["informationAtInterim"];
                conditionalCriticalValue[k] = (getOneMinusQNorm(alpha) -
                    getOneMinusQNorm(adjustedPValues[k]) * std::sqrt(informationAtInterim)) /
                    std::sqrt(1.0 - informationAtInterim);
            } else {
                NumericVector criticalValues = design["criticalValues"];
                if (isDesignFisher) {
                    double product = 1.0;
                    for (int j = 0; j <= k; j++) {
                        product *= std::pow(adjustedPValues[j], weights[j]);
                    }
                    double ratio = std::pow(criticalValues[k + 1] / product, 1.0 / weights[k + 1]);
                    conditionalCriticalValue[k] = getOneMinusQNorm(std::min(ratio, 1.0 - 1e-07));
                } else {
                    NumericVector informationRates = design["informationRates"];
                    double weightedSum = 0.0;
                    for (int j = 0; j <= k; j++) {
                        weightedSum += weights[j] * getOneMinusQNorm(adjustedPValues[j]);
                    }
                    double numerator = criticalValues[k + 1] * std::sqrt(informationRates[k + 1]) - weightedSum;
                    double denominator = std::sqrt(informationRates[k + 1] - informationRates[k]);
                    conditionalCriticalValue[k] = numerator / denominator;
                }
            }
            
            // Adaptations
            if (adaptations[k]) {
                // Prepare effect vector for selection
                NumericVector effectVector(gMax);
                double thresholdArg = threshold;
                if (effectMeasure == "testStatistic") {
                    effectVector = overallTestStatistics(_, k);
                } else {
                    if (R_IsNA(directionUpper) || directionUpper) {
                        effectVector = overallEffects(_, k);
                    } else {
                        effectVector = 1.0 / overallEffects(_, k);
                        thresholdArg = 1.0 / thresholdArg;
                    }
                }
                
                // Select arms
                LogicalVector selectedNow;
                if (typeOfSelection == "userDefined") {
                    List selectArmsFunctionArgs = List::create(
                        _["effectVector"] = effectVector,
                        _["stage"] = k + 1, // R 1-based
                        _["directionUpper"] = directionUpper,
                        _["conditionalPower"] = conditionalPower,
                        _["conditionalCriticalValue"] = conditionalCriticalValue,
                        _["plannedEvents"] = plannedEvents,
                        _["allocationRatioPlanned"] = allocationFraction[0] / (1.0 * allocationFraction[1]),
                        _["selectedArms"] = selectedArms,
                        _["thetaH1"] = thetaH1,
                        _["overallEffects"] = overallEffects
                    );
                    Function selectTreatmentArmsR(".selectTreatmentArms");

                    selectedNow = selectTreatmentArmsR(
                        Named("typeOfSelection") = typeOfSelection,
						Named("epsilonValue") = epsilonValue,
						Named("rValue") = rValue,
						Named("threshold") = thresholdArg,
						Named("selectArmsFunction") = selectArmsFunction,
						Named("selectArmsFunctionArgs") = selectArmsFunctionArgs,
                        Named("survival") = true
                    );
                } else {
                    selectedNow = selectTreatmentArms(
                        effectVector,
                        typeOfSelection,
                        epsilonValue,
                        rValue,
                        thresholdArg,
                        true // survival = true
                    );
                }
                selectedArms(_, k + 1) = selectedArms(_, k) & selectedNow;
                
                // Calculate new events
                double newEventsValue;
                if (calcEventsFunctionIsUserDefined) {
                    Function calcEventsFunc = calcEventsFunction.get();                    
                    RObject newEvents = calcEventsFunc(
                        _["stage"] = k + 2, // R 1-based
                        _["directionUpper"] = directionUpper,
                        _["conditionalPower"] = conditionalPower,
                        _["conditionalCriticalValue"] = conditionalCriticalValue,
                        _["plannedEvents"] = plannedEvents,
                        _["allocationRatioPlanned"] = rep(allocationRatio, k + 2),
                        _["selectedArms"] = selectedArms,
                        _["thetaH1"] = thetaH1,
                        _["overallEffects"] = overallEffects,
                        _["minNumberOfEventsPerStage"] = minNumberOfEventsPerStage,
                        _["maxNumberOfEventsPerStage"] = maxNumberOfEventsPerStage
                    );

                    if (Rf_isNull(newEvents) || Rf_length(newEvents) != 1 || !Rf_isReal(newEvents) || R_IsNA(REAL(newEvents)[0])) {
						stop(
							"'calcEventsFunction' returned an illegal or undefined result; ",
							"the output must be a single numeric value"
						);
					}
					newEventsValue = REAL(newEvents)[0];
                } else {
                    newEventsValue = getSimulationSurvivalMultiArmStageEvents(
                        k + 2, // R 1-based
                        directionUpper,
                        conditionalPower,
                        conditionalCriticalValue,
                        plannedEvents,
                        rep(allocationRatio, k + 2),
                        selectedArms,
                        thetaH1,
                        overallEffects,
                        minNumberOfEventsPerStage,
                        maxNumberOfEventsPerStage
                    );
                }
                
                if (!R_IsNA(conditionalPower) || calcEventsFunctionIsUserDefined) {
                    NumericVector newEventsIncrements = cumsum(rep(newEventsValue, kMax - 1 - k));
					plannedEvents[seq(k + 1, kMax - 1)] = plannedEvents[k] + newEventsIncrements;
                }
            } else {
                selectedArms(_, k + 1) = selectedArms(_, k);
            }
            
            // Conditional power
            double thetaStandardized;
            if (R_IsNA(thetaH1)) {
                LogicalVector nowSelected = selectedArms(_, k);
				NumericVector effectsThisStage = overallEffects(_, k);
				NumericVector nowEffects = effectsThisStage[nowSelected];
				NumericVector minMaxEffect = applyDirectionOfAlternative(
					nowEffects,
					directionUpper,
					"minMax",
					"planning"
				);
                thetaStandardized = std::log(minMaxEffect[0]);
            } else {
                thetaStandardized = std::log(thetaH1);
            }
            thetaStandardized = (2.0 * directionUpper - 1.0) * thetaStandardized;
            double numerator = thetaStandardized * sqrt(plannedEvents[k + 1] - plannedEvents[k]) *
				sqrt(allocationRatio);
			double denominator = 1.0 + allocationRatio;
			double quantile = conditionalCriticalValue[k] - numerator / denominator;
			// pnorm5(double x, double mu, double sigma, int lower_tail, int log_p)
			// pnorm(quantile, lower.tail = FALSE):
			conditionalPowerPerStage[k] = R::pnorm(quantile, 0.0, 1.0, 0, 0);
        }
    }
       
    return List::create(
        _["eventsNotAchieved"] = eventsNotAchieved,
        _["singleEventsPerStage"] = singleEventsPerStage,
        _["cumulativeEventsPerStage"] = cumulativeEventsPerStage,
        _["plannedEvents"] = plannedEvents,
        _["analysisTime"] = analysisTime,
        _["numberOfSubjects"] = numberOfSubjects,
        _["allocationRatioPlanned"] = rep(allocationRatio, kMax),
        _["testStatistics"] = testStatistics,
        _["overallEffects"] = overallEffects,
        _["overallTestStatistics"] = overallTestStatistics,
        _["separatePValues"] = separatePValues,
        _["conditionalCriticalValue"] = conditionalCriticalValue,
        _["conditionalPowerPerStage"] = conditionalPowerPerStage,
        _["selectedArms"] = selectedArms
    );
}

