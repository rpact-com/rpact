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
List logRankTestMultiArm(const DataFrame& survivalDataSet,
						 double time,
						 IntegerVector comparedTreatmentArms,
						 bool directionUpper = true,
						 double thetaH0 = 1.0) {

	NumericVector accrualTime = survivalDataSet["accrualTime"];
	NumericVector survivalTime = survivalDataSet["survivalTime"];
	NumericVector dropoutTime = survivalDataSet["dropoutTime"];
	IntegerVector treatmentArm = survivalDataSet["treatmentArm"];
    IntegerVector treatmentArmModified = clone(treatmentArm);

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
	treatmentArmModified[isInFirstArm] = 1;
    treatmentArmModified[isInSecondArm] = 2;	

    // Select subjects in the treatment arms to be compared and call
    // logRankTest().    
    LogicalVector isInSelectedArms = isInFirstArm | isInSecondArm;
    List results = logRankTest(
        accrualTime[isInSelectedArms],
        survivalTime[isInSelectedArms],
        dropoutTime[isInSelectedArms],
        treatmentArmModified[isInSelectedArms],
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
	
	// Match R's loop structure: for each subject, generate survival then dropout
	for (int i = 0; i < numberOfSubjects; i++) {
        int treatmentArm = treatments[i] - 1; // Convert to 0-based
        
        // Generate survival time
        double u = R::runif(0, 1);
        survivalTime[i] = std::pow(-std::log(1 - u), 1.0 / kappa) / lambdaVector[treatmentArm];
        
        // Then handle dropout within same subject iteration
        // Match R's exact structure with separate if statements
        dropoutTime[i] = NA_REAL; // Initialize
        
        if (any(phi > 0).is_true()) {
            if (phi[0] > 0) {
                for (int g = 0; g < gMax; g++) {
                    if (treatmentArm == g) {
                        dropoutTime[i] = -std::log(1 - R::runif(0, 1)) / phi[0];
                    }
                }
            }
            if (phi[1] > 0) {
                if (treatmentArm == gMax) {
                    dropoutTime[i] = -std::log(1 - R::runif(0, 1)) / phi[1];
                }
            }
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
                                    const IntegerVector& treatments,
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
    std::fill(singleEventsPerStage.begin(), singleEventsPerStage.end(), NA_REAL);
    NumericMatrix cumulativeEventsPerStage(gMax, kMax);
    std::fill(cumulativeEventsPerStage.begin(), cumulativeEventsPerStage.end(), NA_REAL);
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
    NumericVector survivalTime = clone(as<NumericVector>(tmp["survivalTime"]));
	NumericVector dropoutTime = clone(as<NumericVector>(tmp["dropoutTime"]));
    
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
                
                int lastControlEvents = NA_INTEGER;
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
                    lastControlEvents = events[1];
                }
                singleEventsPerStage(gMax, k) = lastControlEvents;
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
                
                // Generate new survival times for new subjects (matching R loop order)
                for (int i = numberOfSubjects[k - 1] - 1; i < maxNumberOfSubjects; i++) {
                    int treatmentArm = treatments[i] - 1; // 0-based
                    for (int g = 0; g < gMax; g++) {
                        if (treatmentArm == g && selectedArms(g, k)) {
                            double u = R::runif(0, 1);
                            survivalTime[i] = std::pow(-std::log(1 - u), 1.0 / kappa) / lambdaVector[g];
                        }
                    }
                    if (any(phi > 0).is_true()) {
                        if (phi[0] > 0) { 
                            for (int g = 0; g < gMax; g++) {
                                if (treatmentArm == g && selectedArms(g, k)) {
                                    dropoutTime[i] = -std::log(1 - R::runif(0, 1)) / phi[0];
                                }
                            }
                            if (treatmentArm == gMax) {
                                dropoutTime[i] = -std::log(1 - R::runif(0, 1)) / phi[1];
                            }
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
            LogicalVector inSelectedArms = intInSet(treatmentArms, selectedArmIndices);
            
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
                
                int lastControlEvents = NA_INTEGER;
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
                        lastControlEvents = events[1];
                        
                        // Stage-wise test statistic
                        double numerator = std::sqrt(cumulativeEventsPerStage(g, k)) * overallTestStatistics(g, k) -
                                         std::sqrt(cumulativeEventsPerStage(g, k - 1)) * overallTestStatistics(g, k - 1);
                        double denominator = std::sqrt(cumulativeEventsPerStage(g, k) - cumulativeEventsPerStage(g, k - 1));
                        testStatistics(g, k) = numerator / denominator;
                    }
                }
                if (lastControlEvents != NA_INTEGER) {
                    singleEventsPerStage(gMax, k) = lastControlEvents;
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

// Perform Simulation Multi-Arm Survival Loop
//
// Performs the main simulation loop for multi-arm survival simulation
//
// @param cols Number of effect scenarios  
// @param maxNumberOfIterations Maximum number of iterations per scenario
// @param design Trial design object
// @param weights Numeric vector of weights per stage (note: these
//   are not used for a conditional Dunnett design)
// @param directionUpper Direction of test
// @param effectMatrix Matrix of effects (rows: scenarios, columns: arms)
// @param omegaMaxVector Vector of omega max values per scenario
// @param piControl Control group hazard
// @param kappa Shape parameter for Weibull distribution
// @param phi Dropout rates for treatment groups
// @param eventTime Event time
// @param plannedEvents Planned events per stage
// @param recruitmentTimes Recruitment times
// @param typeOfSelection Type of arm selection
// @param effectMeasure Effect measure type
// @param adaptations Adaptation indicators per stage
// @param epsilonValue Epsilon value for selection
// @param rValue R value for selection
// @param threshold Threshold for selection
// @param allocationFraction Allocation fractions
// @param minNumberOfEventsPerStage Minimum events per stage
// @param maxNumberOfEventsPerStage Maximum events per stage
// @param conditionalPower Conditional power
// @param thetaH1 Alternative hypothesis hazard ratio
// @param calcEventsFunction Events calculation function
// @param calcEventsFunctionIsUserDefined Whether calc function is user defined
// @param selectArmsFunction Arm selection function
// @param indices Matrix of indices for intersection hypotheses
// @param intersectionTest String specifying intersection test method
// @param criticalValuesDunnett Critical values for Dunnett test (can be NULL)
// @param successCriterion String specifying success criterion
// @param gMax Number of arms
// @param kMax Number of stages
//
// [[Rcpp::export(name = ".performSimulationMultiArmSurvivalLoopCpp")]]
List performSimulationMultiArmSurvivalLoop(
		int cols,
		int maxNumberOfIterations,
		Environment design,
        NumericVector weights,
		bool directionUpper,
		NumericMatrix effectMatrix,
		NumericVector omegaMaxVector,
		double piControl,
		double kappa,
		NumericVector phi,
		double eventTime,
		NumericVector plannedEvents,
		NumericVector recruitmentTimes,
		std::string typeOfSelection,
		std::string effectMeasure,
		LogicalVector adaptations,
		double epsilonValue,
		double rValue,
		double threshold,
		IntegerVector allocationFraction,
		NumericVector minNumberOfEventsPerStage,
		NumericVector maxNumberOfEventsPerStage,
		double conditionalPower,
		double thetaH1,
		Nullable<Function> calcEventsFunction,
		bool calcEventsFunctionIsUserDefined,
		Nullable<Function> selectArmsFunction,
		LogicalMatrix indices,
		std::string intersectionTest,
		Nullable<NumericVector> criticalValuesDunnett,
		std::string successCriterion,
		int gMax,
		int kMax) {
	// Initialize simulation result matrices
	IntegerMatrix simulatedNumberEventsNotAchieved(kMax, cols);
	NumericMatrix simulatedAnalysisTime(kMax, cols);
	NumericMatrix simulatedNumberOfSubjects(kMax, cols);
	// This is how an array() is created in Rcpp: Create a NumericVector and set the "dim" attribute.
	NumericVector simulatedSelections(kMax * cols * gMax);
	simulatedSelections.attr("dim") = IntegerVector::create(kMax, cols, gMax);
	NumericVector simulatedRejections(kMax * cols * gMax);
	simulatedRejections.attr("dim") = IntegerVector::create(kMax, cols, gMax);
	NumericMatrix simulatedNumberOfActiveArms(kMax, cols);
	NumericVector simulatedSingleEventsPerStage(kMax * cols * (gMax + 1));
	simulatedSingleEventsPerStage.attr("dim") = IntegerVector::create(kMax, cols, gMax + 1);
	NumericMatrix simulatedPlannedEvents(kMax, cols);
	NumericMatrix simulatedSuccessStopping(kMax, cols);
	NumericMatrix simulatedFutilityStopping(kMax - 1, cols);
	NumericMatrix simulatedConditionalPower(kMax, cols);
	NumericVector simulatedRejectAtLeastOne(cols);
	NumericVector expectedNumberOfEvents(cols);
	NumericVector expectedNumberOfSubjects(cols);
	NumericVector expectedStudyDuration(cols);
	NumericMatrix iterations(kMax, cols);
	
	// Initialize all these with zeros
	std::fill(simulatedNumberEventsNotAchieved.begin(), simulatedNumberEventsNotAchieved.end(), 0);
	std::fill(simulatedAnalysisTime.begin(), simulatedAnalysisTime.end(), 0.0);
	std::fill(simulatedNumberOfSubjects.begin(), simulatedNumberOfSubjects.end(), 0.0);
	std::fill(simulatedSelections.begin(), simulatedSelections.end(), 0.0);
	std::fill(simulatedRejections.begin(), simulatedRejections.end(), 0.0);
	std::fill(simulatedNumberOfActiveArms.begin(), simulatedNumberOfActiveArms.end(), 0.0);
	std::fill(simulatedSingleEventsPerStage.begin(), simulatedSingleEventsPerStage.end(), 0.0);
	std::fill(simulatedPlannedEvents.begin(), simulatedPlannedEvents.end(), 0.0);
	std::fill(simulatedSuccessStopping.begin(), simulatedSuccessStopping.end(), 0.0);
	std::fill(simulatedFutilityStopping.begin(), simulatedFutilityStopping.end(), 0.0);
	std::fill(simulatedConditionalPower.begin(), simulatedConditionalPower.end(), 0.0);
	std::fill(simulatedRejectAtLeastOne.begin(), simulatedRejectAtLeastOne.end(), 0.0);
	std::fill(expectedNumberOfEvents.begin(), expectedNumberOfEvents.end(), 0.0);
	std::fill(expectedNumberOfSubjects.begin(), expectedNumberOfSubjects.end(), 0.0);
	std::fill(expectedStudyDuration.begin(), expectedStudyDuration.end(), 0.0);
	std::fill(iterations.begin(), iterations.end(), 0.0);
	
	// Initialize data collection vectors
	int len = maxNumberOfIterations * kMax * gMax * cols;
	NumericVector dataIterationNumber(len, NA_REAL);
	NumericVector dataStageNumber(len, NA_REAL);
	NumericVector dataArmNumber(len, NA_REAL);
	NumericVector dataAlternative(len, NA_REAL);
	NumericVector dataEffect(len, NA_REAL);
	NumericVector dataAnalysisTime(len, NA_REAL);
	NumericVector dataNumberOfSubjects(len, NA_REAL);
	NumericVector dataNumberOfEvents(len, NA_REAL);
	LogicalVector dataRejectPerStage(len, NA_LOGICAL);
	LogicalVector dataFutilityStop(len, NA_LOGICAL);
	LogicalVector dataSuccessStop(len, NA_LOGICAL);
	NumericVector dataTestStatistics(len, NA_REAL);
	NumericVector dataConditionalCriticalValue(len, NA_REAL);
	NumericVector dataConditionalPowerAchieved(len, NA_REAL);
	NumericVector dataEffectEstimate(len, NA_REAL);
	NumericVector dataPValuesSeparate(len, NA_REAL);
	
	// Check design type
	bool isDesignConditionalDunnett = (getClassName(design) == "TrialDesignConditionalDunnett");
	
	int index = 0;
	
	// Main simulation loop
	for (int i = 0; i < cols; i++) {
		for (int j = 0; j < maxNumberOfIterations; j++) {
			NumericVector omegaVectorThisScenario = effectMatrix(i, _);
			
			List stageResults = getSimulatedStageResultsSurvivalMultiArmSubjectsBased(
				design,
				weights,
				directionUpper,
				omegaVectorThisScenario,
				piControl,
				kappa,
				phi,
				eventTime,
				plannedEvents,
				recruitmentTimes,
				allocationFraction,
				typeOfSelection,
				effectMeasure,
				adaptations,
				epsilonValue,
				rValue,
				threshold,
				minNumberOfEventsPerStage,
				maxNumberOfEventsPerStage,
				conditionalPower,
				thetaH1,
				calcEventsFunction,
				calcEventsFunctionIsUserDefined,
				selectArmsFunction
			);
			
			List closedTest;
			if (isDesignConditionalDunnett) {
				closedTest = performClosedConditionalDunnettTestForSimulation(
					stageResults,
					design,
					indices,
					criticalValuesDunnett.get(),
					successCriterion
				);
			} else {
				closedTest = performClosedCombinationTestForSimulationMultiArm(
					stageResults,
					design,
					indices,
					intersectionTest,
					successCriterion
				);
			}
			
			bool rejectAtSomeStage = false;
			LogicalVector rejectedArmsBefore(gMax, false);
			
			// Extract stage results components
			LogicalVector eventsNotAchieved = stageResults["eventsNotAchieved"];
			NumericVector analysisTime = stageResults["analysisTime"];
			IntegerVector numberOfSubjects = stageResults["numberOfSubjects"];
			NumericMatrix singleEventsPerStage = stageResults["singleEventsPerStage"];
			NumericMatrix cumulativeEventsPerStage = stageResults["cumulativeEventsPerStage"];
			NumericMatrix testStatistics = stageResults["testStatistics"];
			NumericMatrix overallEffects = stageResults["overallEffects"];
			NumericMatrix separatePValues = stageResults["separatePValues"];
			NumericVector conditionalCriticalValue = stageResults["conditionalCriticalValue"];
			NumericVector conditionalPowerPerStage = stageResults["conditionalPowerPerStage"];
			LogicalMatrix selectedArms = stageResults["selectedArms"];
			
			// Extract closed test results
			LogicalMatrix rejected = closedTest["rejected"];
			LogicalMatrix selectedArmsTest = closedTest["selectedArms"];
			LogicalVector successStop = closedTest["successStop"];
			LogicalVector futilityStop = closedTest["futilityStop"];
			NumericMatrix separatePValuesTest = closedTest["separatePValues"];
			
			// Loop over stages
			for (int k = 0; k < kMax; k++) {
				if (eventsNotAchieved[k]) {
					simulatedNumberEventsNotAchieved(k, i)++;
				} else {
					simulatedAnalysisTime(k, i) += analysisTime[k];
					simulatedNumberOfSubjects(k, i) += numberOfSubjects[k];
					
					// Update rejections - need to combine rejected and selected arms or previously rejected
					for (int g = 0; g < gMax; g++) {
						if ((rejected(g, k) && selectedArmsTest(g, k)) || rejectedArmsBefore[g]) {
							simulatedRejections[k + i * kMax + g * kMax * cols]++;
						}
						if (selectedArmsTest(g, k)) {
							simulatedSelections[k + i * kMax + g * kMax * cols]++;
						}
					}
					
					// Count active arms
					simulatedNumberOfActiveArms(k, i) += sum(selectedArmsTest(_, k));
					
					bool successStopComplete = !Rcpp::as<bool>(any(is_na(successStop)));
					if (successStopComplete) {
						simulatedSuccessStopping(k, i) += successStop[k];
					}
					
					if ((kMax > 1) && (k < kMax - 1)) {
						bool futilityStopComplete = !Rcpp::as<bool>(any(is_na(futilityStop)));
						if (futilityStopComplete) {
							if (futilityStop[k] && !successStop[k]) {
								simulatedFutilityStopping(k, i)++;
							}
						}
						if (!successStop[k] && !futilityStop[k]) {
							simulatedConditionalPower(k + 1, i) += conditionalPowerPerStage[k];
						}
					}
					
					// Update single events per stage
					for (int g = 0; g < gMax; g++) {
						if (!R_IsNA(cumulativeEventsPerStage(g, k))) {
							simulatedSingleEventsPerStage[k + i * kMax + g * kMax * cols] += 
								singleEventsPerStage(g, k);
						}
					}
					simulatedSingleEventsPerStage[k + i * kMax + gMax * kMax * cols] += 
						singleEventsPerStage(gMax, k);					
					simulatedPlannedEvents(k, i) += sum(na_omit(singleEventsPerStage(_, k)));

					iterations(k, i)++;
					
					// Collect detailed data
					for (int g = 0; g < gMax; g++) {
						dataIterationNumber[index] = j + 1; // Careful: R uses 1-based indexing
						dataStageNumber[index] = k + 1;
						dataArmNumber[index] = g + 1;
						dataAlternative[index] = omegaMaxVector[i];
						dataEffect[index] = effectMatrix(i, g);
						dataAnalysisTime[index] = analysisTime[k];
						dataNumberOfSubjects[index] = numberOfSubjects[k];
						dataNumberOfEvents[index] = round(cumulativeEventsPerStage(g, k), 1);
						dataRejectPerStage[index] = rejected(g, k);
						dataTestStatistics[index] = testStatistics(g, k);
						dataSuccessStop[index] = successStop[k];
						if (k < kMax - 1) {
							dataFutilityStop[index] = futilityStop[k];
							dataConditionalCriticalValue[index] = conditionalCriticalValue[k];
							dataConditionalPowerAchieved[index + 1] = conditionalPowerPerStage[k];
						}
						dataEffectEstimate[index] = overallEffects(g, k);
						dataPValuesSeparate[index] = separatePValuesTest(g, k);
						index++;
					}
					
                    // Check if at least one hypothesis is rejected
                    if (!rejectAtSomeStage) {
                        bool anyRejected = false;
                        for (int g = 0; g < gMax; g++) {
                            if ((rejected(g, k) && selectedArmsTest(g, k)) || rejectedArmsBefore[g]) {
                                anyRejected = true;
                                break;
                            }
                        }
                        if (anyRejected) {
                            simulatedRejectAtLeastOne[i]++;
                            rejectAtSomeStage = true;
                        }
                    }
					
					// Early stopping
					if ((k < kMax - 1) && (successStop[k] || futilityStop[k])) {
						// Rejected hypotheses remain rejected also in case of early stopping
						for (int l = k + 1; l < kMax; l++) {
							for (int g = 0; g < gMax; g++) {
								if ((rejected(g, k) && selectedArmsTest(g, k)) || rejectedArmsBefore[g]) {
									simulatedRejections[l + i * kMax + g * kMax * cols]++;
								}
							}
						}
						break;
					}
					
					// Update rejected arms
					for (int g = 0; g < gMax; g++) {
						if ((rejected(g, k) && selectedArmsTest(g, k)) || rejectedArmsBefore[g]) {
							rejectedArmsBefore[g] = true;
						}
					}
				}
			}
		}
		
		// Post-processing for this scenario
		for (int g = 0; g <= gMax; g++) {
			for (int k = 0; k < kMax; k++) {
				if (iterations(k, i) > 0) {
					simulatedSingleEventsPerStage[k + i * kMax + g * kMax * cols] /= iterations(k, i);
				}
			}
		}
		
		for (int k = 0; k < kMax; k++) {
			if (iterations(k, i) > 0) {
				simulatedPlannedEvents(k, i) /= iterations(k, i);
				simulatedNumberOfSubjects(k, i) /= iterations(k, i);
				simulatedAnalysisTime(k, i) /= iterations(k, i);
				simulatedNumberOfActiveArms(k, i) /= iterations(k, i);
			}
		}
		
		if (kMax > 1) {
			// Adjust rejections for stage-wise differences		
			// Need to process in reverse order to avoid using modified values
			for (int k = kMax - 1; k >= 1; k--) {
				for (int g = 0; g < gMax; g++) {
					simulatedRejections[k + i * kMax + g * kMax * cols] -= 
						simulatedRejections[(k - 1) + i * kMax + g * kMax * cols];
				}
			}
			
			// Calculate expected values
			NumericVector stopping(kMax - 1);
			for (int k = 0; k < kMax - 1; k++) {
				stopping[k] = (simulatedSuccessStopping(k, i) + simulatedFutilityStopping(k, i)) / 
					maxNumberOfIterations;
			}
			NumericVector cumStopping = cumsum(stopping);
			
			expectedNumberOfEvents[i] = simulatedPlannedEvents(0, i);
			expectedNumberOfSubjects[i] = simulatedNumberOfSubjects(0, i);
			expectedStudyDuration[i] = simulatedAnalysisTime(0, i);
			
			for (int k = 1; k < kMax; k++) {
				double factor = 1.0 - cumStopping[k - 1];
				expectedNumberOfEvents[i] += factor * 
					(simulatedPlannedEvents(k, i) - simulatedPlannedEvents(k - 1, i));
				expectedNumberOfSubjects[i] += factor * 
					(simulatedNumberOfSubjects(k, i) - simulatedNumberOfSubjects(k - 1, i));
				expectedStudyDuration[i] += factor * 
					(simulatedAnalysisTime(k, i) - simulatedAnalysisTime(k - 1, i));
			}
		} else {
			expectedNumberOfEvents[i] = simulatedPlannedEvents(0, i);
			expectedNumberOfSubjects[i] = simulatedNumberOfSubjects(0, i);
			expectedStudyDuration[i] = simulatedAnalysisTime(0, i);
		}
	}
	
	// Set first stage conditional power to NA
	for (int i = 0; i < cols; i++) {
		simulatedConditionalPower(0, i) = NA_REAL;
	}
	
	// Calculate conditional power averages
	if (kMax > 1) {
		for (int k = 1; k < kMax; k++) {
			for (int i = 0; i < cols; i++) {
				double denom = iterations(k, i) + simulatedNumberEventsNotAchieved(k, i);
				simulatedConditionalPower(k, i) /= denom;
			}
		}
	}
	
	// Filter out NA rows in effectEstimate
	LogicalVector validRows = !is_na(dataEffectEstimate);
	
	// Create data frame
    dataConditionalCriticalValue = round(dataConditionalCriticalValue, 6);
    dataConditionalPowerAchieved = round(dataConditionalPowerAchieved, 6);
	DataFrame filteredData = DataFrame::create(
		_["iterationNumber"] = dataIterationNumber[validRows],
		_["stageNumber"] = dataStageNumber[validRows],
		_["armNumber"] = dataArmNumber[validRows],
		_["omegaMax"] = dataAlternative[validRows],
		_["effect"] = dataEffect[validRows],
		_["analysisTime"] = dataAnalysisTime[validRows],
		_["numberOfSubjects"] = dataNumberOfSubjects[validRows],
		_["numberOfEvents"] = dataNumberOfEvents[validRows],
		_["effectEstimate"] = dataEffectEstimate[validRows],
		_["testStatistics"] = dataTestStatistics[validRows],
		_["pValue"] = dataPValuesSeparate[validRows],
		_["conditionalCriticalValue"] = dataConditionalCriticalValue[validRows],
		_["conditionalPowerAchieved"] = dataConditionalPowerAchieved[validRows],
		_["rejectPerStage"] = dataRejectPerStage[validRows],
		_["successStop"] = dataSuccessStop[validRows],
		_["futilityPerStage"] = dataFutilityStop[validRows]
	);
	
	return List::create(
		_["simulatedNumberEventsNotAchieved"] = simulatedNumberEventsNotAchieved,
		_["simulatedAnalysisTime"] = simulatedAnalysisTime,
		_["simulatedNumberOfSubjects"] = simulatedNumberOfSubjects,
		_["simulatedSelections"] = simulatedSelections,
		_["simulatedRejections"] = simulatedRejections,
		_["simulatedNumberOfActiveArms"] = simulatedNumberOfActiveArms,
		_["simulatedSingleEventsPerStage"] = simulatedSingleEventsPerStage,
		_["simulatedPlannedEvents"] = simulatedPlannedEvents,
		_["simulatedSuccessStopping"] = simulatedSuccessStopping,
		_["simulatedFutilityStopping"] = simulatedFutilityStopping,
		_["simulatedConditionalPower"] = simulatedConditionalPower,
		_["simulatedRejectAtLeastOne"] = simulatedRejectAtLeastOne,
		_["expectedNumberOfEvents"] = expectedNumberOfEvents,
		_["expectedNumberOfSubjects"] = expectedNumberOfSubjects,
		_["expectedStudyDuration"] = expectedStudyDuration,
		_["iterations"] = iterations,
		_["data"] = filteredData
	);
}

