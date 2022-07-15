/**
 *
 * -- Simulation of survival data with group sequential and combination test --
 *
 * This file is part of the R package rpact:
 * Confirmatory Adaptive Clinical Trial Design and Analysis
 *
 * Author: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
 * Licensed under "GNU Lesser General Public License" version 3
 * License text can be found here: https://www.r-project.org/Licenses/LGPL-3
 *
 * RPACT company website: https://www.rpact.com
 * rpact package website: https://www.rpact.org
 *
 * Contact us for information about our services: info@rpact.com
 *
 * File version: $Revision: 6285 $
 * Last changed: $Date: 2022-06-10 10:49:23 +0200 (Fri, 10 Jun 2022) $
 * Last changed by: $Author: pahlke $
 *
 */

#include <Rcpp.h>
#include "f_utilities.h"
#include "f_simulation_survival_utilities.h"
using namespace Rcpp;

// Log Rank Test
//
// This function calculates the logrank test statistic for the survival data set at given time,
// i.e., it determines whether an event or a dropout
// was observed, calculates the time under risk, and the logrank statistic.
//
// @param accrualTime An double vector
//
List logRankTest(NumericVector accrualTime, NumericVector survivalTime,
		NumericVector dropoutTime, IntegerVector treatmentGroup,
		double time, bool directionUpper, double thetaH0, bool returnRawData) {

	int numberOfSubjects = accrualTime.size();
	int subjectsT1 = 0;
	int subjectsT2 = 0;

	NumericVector timeUnderObservation = NumericVector(numberOfSubjects, 0.0);
	LogicalVector event = LogicalVector(numberOfSubjects, NA_LOGICAL);
	LogicalVector dropoutEvent = LogicalVector(numberOfSubjects, NA_LOGICAL);

	for (int i = 0; i < numberOfSubjects; i++) {

		if (accrualTime[i] > time) {
			treatmentGroup[i] = -treatmentGroup[i];
			event[i] = false;
			dropoutEvent[i] = false;
		} else {
			if (treatmentGroup[i] == 1) {
				subjectsT1++;
			}
			else if (treatmentGroup[i] == 2) {
				subjectsT2++;
			}

			if (treatmentGroup[i] > 0 && accrualTime[i] + survivalTime[i] < time &&
					(R_IsNA((double) dropoutTime[i]) || dropoutTime[i] > survivalTime[i])) {
				event[i] = true;
			} else {
				event[i] = false;
			}

			if (treatmentGroup[i] > 0 && accrualTime[i] + dropoutTime[i] < time &&
					!R_IsNA((double) dropoutTime[i]) && dropoutTime[i] < survivalTime[i]) {
				dropoutEvent[i] = true;
			} else {
				dropoutEvent[i] = false;
			}
		}

		if (event[i]) {
			timeUnderObservation[i] = survivalTime[i];
		} else if (dropoutEvent[i]) {
			timeUnderObservation[i] = dropoutTime[i];
		} else {
			timeUnderObservation[i] = time - accrualTime[i];
		}
	}

	int numberOfSubjets = subjectsT1 + subjectsT2;

	NumericVector timeUnderObservationSorted = clone(timeUnderObservation).sort();
	IntegerVector sortedIndex = match(timeUnderObservationSorted, timeUnderObservation);
	sortedIndex = sortedIndex - 1;
	LogicalVector eventSorted = event[sortedIndex];
	IntegerVector treatmentGroupSorted = treatmentGroup[sortedIndex];
	eventSorted = eventSorted[treatmentGroupSorted > 0];
	treatmentGroupSorted = treatmentGroupSorted[treatmentGroupSorted > 0];
	treatmentGroup = abs(treatmentGroup);

	double numerator = 0;
	double denominator = 0;
	int events1 = 0;
	int events2 = 0;

	for (int i = 0; i < eventSorted.size(); i++) {
		if (eventSorted[i]) {
			if (treatmentGroupSorted[i] == 1) {
				if (subjectsT1 + subjectsT2 > 0) {
					numerator -= subjectsT2 / (thetaH0 * subjectsT1 + subjectsT2);
				}
				events1++;
			} else if (treatmentGroupSorted[i] == 2) {
				if (subjectsT1 + subjectsT2 > 0) {
					numerator += 1 - subjectsT2 / (thetaH0 * subjectsT1 + subjectsT2);
				}
				events2++;
			}
			if (subjectsT1 + subjectsT2 > 0) {
				denominator += thetaH0 * subjectsT1 * subjectsT2 /
					pow(thetaH0 * subjectsT1 + subjectsT2, 2);
			}
		}
		if (treatmentGroupSorted[i] == 1) {
			subjectsT1--;
		}
		else if (treatmentGroupSorted[i] == 2) {
			subjectsT2--;
		}
	}

	double logRank;
	if (denominator > 0) {
		logRank = -numerator / sqrt(denominator);
	} else {
		logRank = R_NegInf;
	}

	if (!directionUpper) {
		logRank = -logRank;
	}

	NumericVector out(4);
	out[0] = logRank;
	out[1] = numberOfSubjets;
	out[2] = events1;
	out[3] = events2;

	if (returnRawData) {
		return List::create(
			_["result"] = out,
			_["timeUnderObservation"] = timeUnderObservation,
			_["event"] = event,
			_["dropoutEvent"] = dropoutEvent
		);
	}

	return List::create(
		_["result"] = out
	);
}

NumericVector getIndependentIncrements(int stage, NumericVector eventsPerStage, NumericVector logRankOverStages) {
	NumericVector independentIncrements = NumericVector(stage, NA_REAL);
	independentIncrements[0] = logRankOverStages[0];

	const IntegerVector indices1 = seq(0, stage - 2);
	const IntegerVector indices2 = seq(1, stage - 1);

	independentIncrements[indices2] = vectorDivide(
		vectorMultiply(vectorSqrt(eventsPerStage[indices2]), logRankOverStages[indices2]) -
		vectorMultiply(vectorSqrt(eventsPerStage[indices1]), logRankOverStages[indices1]),
		vectorSqrt(eventsPerStage[indices2] - eventsPerStage[indices1]));

	return independentIncrements;
}

// Get Test Statistics
// @param designNumber The design number:
//        1: Group sequential design
//        2: Inverse normal design
//        3: Fisher design
//
NumericVector getTestStatistics(int stage, int designNumber, NumericVector informationRates,
		NumericVector eventsPerStage, NumericVector logRankOverStages) {

	// Group sequential design
	if (designNumber == 1) {
		return NumericVector::create(logRankOverStages[stage - 1], NA_REAL);
	}

	// Inverse normal design
	if (designNumber == 2) {

		if (stage == 1) {
			return NumericVector::create(logRankOverStages[0], NA_REAL);
		}

		NumericVector independentIncrements = getIndependentIncrements(stage, eventsPerStage, logRankOverStages);

		const IntegerVector indices1 = seq(0, stage - 2);
		const IntegerVector indices2 = seq(1, stage - 1);

		double value = (sqrt((double) informationRates[0]) * independentIncrements[0] +
			vectorProduct(vectorSqrt(informationRates[indices2] - informationRates[indices1]),
					independentIncrements[indices2])) /
			sqrt((double) informationRates[stage - 1]);

		return NumericVector::create(value, NA_REAL);
	}

	// Fisher design
	NumericVector independentIncrements = NumericVector(stage, NA_REAL);
	independentIncrements[0] = logRankOverStages[0];

	NumericVector weightFisher = NumericVector(stage, NA_REAL);
	weightFisher[0] = 1;

	if (stage > 1) {
		independentIncrements = getIndependentIncrements(stage, eventsPerStage, logRankOverStages);

		const IntegerVector indices1 = seq(0, stage - 2);
		const IntegerVector indices2 = seq(1, stage - 1);

		weightFisher[indices2] = vectorDivide(
				vectorSqrt(informationRates[indices2] - informationRates[indices1]),
				sqrt((double) informationRates[0]));
	}

	const IntegerVector indices0 = seq(0, stage - 1);
	double value = vectorProduct(vectorPow(1 - pnorm(as<NumericVector>(independentIncrements[indices0])),
			as<NumericVector>(weightFisher[indices0])));
	double pValueSeparate = 1 - getNormalDistribution((double) independentIncrements[stage - 1]);

	return NumericVector::create(value, pValueSeparate);
}

// Get Recalculated Event Sizes
// @param designNumber The design number:
//        1: Group sequential design
//        2: Inverse normal design
//        3: Fisher design
//
NumericVector getRecalculatedEventSizes(int designNumber, int stage, int kMax,
		NumericVector criticalValues, NumericVector informationRates,
		double conditionalPower, NumericVector plannedEvents,
		double thetaH1, NumericVector eventsPerStage, NumericVector logRankOverStages,
		NumericVector testStatisticOverStages, NumericVector minNumberOfEventsPerStage,
		NumericVector maxNumberOfEventsPerStage,
		bool directionUpper, double allocation1, double allocation2) {

	double requiredStageEvents = plannedEvents[stage - 1];

	if (stage == 1) {
		NumericVector result = NumericVector(3, NA_REAL);
		result[0] = requiredStageEvents;
		return result;
	}

	// Used effect size is either estimated from test statistic of pre-fixed
	double estimatedTheta;
	if (R_IsNA(thetaH1)) {
		estimatedTheta = exp((double) logRankOverStages[stage - 2] *
			(1 + allocation1 / allocation2) / sqrt(allocation1 /
			allocation2 * eventsPerStage[stage - 2]));
	} else {
		estimatedTheta = thetaH1;
		if (!directionUpper) {
			estimatedTheta = 1 / estimatedTheta;
		}
	}

	// Conditional critical value to reject the null hypotheses at the last stage of the trial
	double conditionalCriticalValue;
	if (designNumber == 3) { // Fisher design
		conditionalCriticalValue = getNormalQuantile(
			1 - pow((double) criticalValues[stage - 1] / testStatisticOverStages[stage - 2],
				1
				/
				sqrt((double)
					(informationRates[stage - 1] - informationRates[stage - 2])
					/
					informationRates[0]
				)
			));
	} else {
		conditionalCriticalValue = (sqrt((double) informationRates[stage - 1]) * criticalValues[stage - 1] -
				testStatisticOverStages[stage - 2] * sqrt((double) informationRates[stage - 2])) /
			sqrt((double) informationRates[stage - 1] - informationRates[stage - 2]);
	}

	if (!R_IsNA(conditionalPower)) {
		double theta;

		theta = max(NumericVector::create(1 + 1E-12, estimatedTheta));

		requiredStageEvents = pow(max(NumericVector::create(0,
			conditionalCriticalValue + getNormalQuantile(conditionalPower))), 2) *
			pow(1 + allocation1 / allocation2, 2) * allocation2 / allocation1 /
			pow(log(theta), 2);

		requiredStageEvents = min(NumericVector::create(
			max(NumericVector::create(minNumberOfEventsPerStage[stage - 1], requiredStageEvents)),
			maxNumberOfEventsPerStage[stage - 1])) + eventsPerStage[stage - 2];
	}

	NumericVector result = NumericVector(3, NA_REAL);
	result[0] = requiredStageEvents;
	result[1] = conditionalCriticalValue;
	result[2] = estimatedTheta;
	return result;
}

NumericMatrix getSimulationStepResultsSurvival(
		int designNumber,
		int kMax,
		int sided,
		NumericVector criticalValues,
		NumericVector informationRates,
		double conditionalPower,
		NumericVector plannedEvents,
		double thetaH1,
		NumericVector minNumberOfEventsPerStage,
		NumericVector maxNumberOfEventsPerStage,
		bool directionUpper,
		double allocation1,
		double allocation2,
		NumericVector accrualTime,
		NumericVector survivalTime,
		NumericVector dropoutTime,
		IntegerVector treatmentGroup,
		double thetaH0,
		NumericVector futilityBounds,
		NumericVector alpha0Vec) {

	NumericVector eventsPerStage = NumericVector(kMax, 0.0);
	NumericVector logRankOverStages = NumericVector(kMax, 0.0);
	NumericVector testStatisticOverStages = NumericVector(kMax, 0.0);

	NumericVector analysisTime = NumericVector(kMax, 0.0);
	NumericVector subjects = NumericVector(kMax, 0.0);
	NumericVector expectedNumberOfEvents1 = NumericVector(kMax, 0.0);
	NumericVector expectedNumberOfEvents2 = NumericVector(kMax, 0.0);
	NumericVector expectedNumberOfEvents = NumericVector(kMax, 0.0);
	NumericVector rejections = NumericVector(kMax, 0.0);
	NumericVector eventsNotAchieved = NumericVector(kMax, 0.0);
	NumericVector futilityStops = NumericVector(kMax, 0.0);
	NumericVector pValuesSeparate = NumericVector(kMax, NA_REAL);
	NumericVector duration = NumericVector(kMax, 0.0);
	NumericVector iterations = NumericVector(kMax, 0.0);

	NumericVector hazardRates1 = NumericVector(kMax, 0.0);
	NumericVector hazardRates2 = NumericVector(kMax, 0.0);
	NumericVector hazardRatiosEstimate = NumericVector(kMax, 0.0);

	NumericVector observationTimePerStage = NumericVector(kMax, NA_REAL);
	NumericVector conditionalPowerAchieved = NumericVector(kMax, 0.0);

	for (int k = 1; k <= kMax; k++) {

		NumericVector recalculatedEventSizes = getRecalculatedEventSizes(
				designNumber, k, kMax, criticalValues, informationRates,
				conditionalPower, plannedEvents,
				thetaH1, eventsPerStage, logRankOverStages, testStatisticOverStages,
				minNumberOfEventsPerStage, maxNumberOfEventsPerStage,
				directionUpper, allocation1, allocation2);

		double requiredStageEvents = recalculatedEventSizes[0];
		double observationTime = findObservationTime(accrualTime,
				survivalTime, dropoutTime, requiredStageEvents);

		if (R_IsNA(observationTime)) {
			eventsNotAchieved[k - 1]++;
			break;
		}

		if (k > 1) {
			double conditionalCriticalValue = recalculatedEventSizes[1];
			double theta = recalculatedEventSizes[2];

			conditionalPowerAchieved[k - 1] =
				1 - getNormalDistribution(conditionalCriticalValue - log(theta) * sqrt(requiredStageEvents - eventsPerStage[k - 2]) *
				sqrt(allocation1 / allocation2) / (1 + allocation1 / allocation2));
		} else {
			conditionalPowerAchieved[k - 1] = NA_REAL;
		}

		observationTimePerStage[k - 1] = observationTime;

		List result = logRankTest(
			accrualTime, survivalTime, dropoutTime, treatmentGroup,
			observationTime, directionUpper, thetaH0, false);

		NumericVector survivalResult = result["result"];
		double logRank = survivalResult[0];
		double numberOfSubjects = survivalResult[1];
		double events1 = survivalResult[2];
		double events2 = survivalResult[3];

		hazardRates1[k - 1] = NA_REAL;
		hazardRates2[k - 1] = NA_REAL;
		hazardRatiosEstimate[k - 1] = NA_REAL;

		eventsPerStage[k - 1] = events1 + events2;
		logRankOverStages[k - 1] = logRank;

		NumericVector testStatistic = getTestStatistics(k, designNumber,
				informationRates, eventsPerStage, logRankOverStages);

		testStatisticOverStages[k - 1] = testStatistic[0];

		int trialStopEventCounter = 0;
		if (designNumber == 3) { // Fisher design
			pValuesSeparate[k - 1] = testStatistic[1];
			if (testStatistic[0] <= criticalValues[k - 1]) {
				rejections[k - 1]++;
				trialStopEventCounter++;
			}
			if (k < kMax && (testStatistic[1] >= alpha0Vec[k - 1])) {
				futilityStops[k - 1]++;
				trialStopEventCounter++;
			}
		} else { // all other designs
			if ((sided == 1 && testStatistic[0] >= criticalValues[k - 1]) ||
					(sided == 2 && std::abs((double) testStatistic[0]) >= criticalValues[k - 1])) {
				rejections[k - 1]++;
				trialStopEventCounter++;
			}

			if (sided == 1 && k < kMax && testStatistic[0] <= futilityBounds[k - 1]) {
				futilityStops[k - 1]++;
				trialStopEventCounter++;
			}
		}

		if (trialStopEventCounter > 0) {
			for (int i = 0; i < trialStopEventCounter; i++) {
				duration[k - 1] += observationTime;
				subjects[k - 1] += numberOfSubjects;
			}
		} else {
			subjects[k - 1] += numberOfSubjects;
			if (k == kMax) {
				duration[k - 1] += observationTime;
			}
		}

		expectedNumberOfEvents1[k - 1] += events1;
		expectedNumberOfEvents2[k - 1] += events2;

		double x = events1 + events2;
		if (k > 1) {
		  x -= eventsPerStage[k - 2];
		}
		expectedNumberOfEvents[k - 1] += x;

		analysisTime[k - 1] += observationTime;
		iterations[k - 1]++;

		if (trialStopEventCounter > 0) {
			break;
		}
	}

	NumericMatrix result(kMax, 18);
	result(_, 0) = analysisTime;
	result(_, 1) = subjects;
	result(_, 2) = expectedNumberOfEvents1;
	result(_, 3) = expectedNumberOfEvents2;
	result(_, 4) = expectedNumberOfEvents;
	result(_, 5) = rejections;
	result(_, 6) = eventsNotAchieved;
	result(_, 7) = futilityStops;
	result(_, 8) = duration;
	result(_, 9) = iterations;
	result(_, 10) = testStatisticOverStages;
	result(_, 11) = logRankOverStages;
	result(_, 12) = hazardRates1;
	result(_, 13) = hazardRates2;
	result(_, 14) = hazardRatiosEstimate;
	result(_, 15) = observationTimePerStage;
	result(_, 16) = conditionalPowerAchieved;
	result(_, 17) = pValuesSeparate;
	return result;
}

NumericMatrix getExtendedSurvivalDataSet(IntegerVector treatmentGroup, int maxNumberOfSubjects,
		double lambda1, double lambda2, double phi1, double phi2, double kappa) {

	NumericVector survivalTime = NumericVector(maxNumberOfSubjects, NA_REAL);
	NumericVector dropoutTime = NumericVector(maxNumberOfSubjects, NA_REAL);

	for (int i = 0; i < maxNumberOfSubjects; i++) {

		if (treatmentGroup[i] == 1) {
			survivalTime[i] = getRandomSurvivalDistribution(lambda1, kappa);
			if (phi1 > 0) {
				dropoutTime[i] = getRandomSurvivalDistribution(phi1, 1);
			}
		} else {
			survivalTime[i] = getRandomSurvivalDistribution(lambda2, kappa);
			if (phi2 > 0) {
				dropoutTime[i] = getRandomSurvivalDistribution(phi2, 1);
			}
		}
	}

	NumericMatrix result(maxNumberOfSubjects, 2);
	result(_, 0) = survivalTime;
	result(_, 1) = dropoutTime;
	return result;
}

NumericMatrix getExtendedSurvivalDataSet(IntegerVector treatmentGroup,
		int maxNumberOfSubjects, NumericVector piecewiseSurvivalTime,
		NumericVector cdfValues1, NumericVector cdfValues2,
		NumericVector lambdaVec1, NumericVector lambdaVec2, double phi1, double phi2) {

	NumericVector survivalTime = NumericVector(maxNumberOfSubjects, NA_REAL);
	NumericVector dropoutTime = NumericVector(maxNumberOfSubjects, NA_REAL);

	for (int i = 0; i < maxNumberOfSubjects; i++) {
		if (treatmentGroup[i] == 1) {
			survivalTime[i] = getRandomPiecewiseExponentialDistribution(cdfValues1, lambdaVec1, piecewiseSurvivalTime);
			if (phi1 > 0) {
				dropoutTime[i] = getRandomPiecewiseExponentialDistribution(
					cdfValues1, rep(phi1, lambdaVec1.size()), piecewiseSurvivalTime);
			}
		} else {
			survivalTime[i] = getRandomPiecewiseExponentialDistribution(cdfValues2, lambdaVec2, piecewiseSurvivalTime);
			if (phi2 > 0) {
				dropoutTime[i] = getRandomPiecewiseExponentialDistribution(
					cdfValues2, rep(phi2, lambdaVec2.size()), piecewiseSurvivalTime);
			}
		}
	}

	NumericMatrix result(maxNumberOfSubjects, 2);
	result(_, 0) = survivalTime;
	result(_, 1) = dropoutTime;
	return result;
}

/* Get Simulation Results
 *
 *  This function calculates the simulation results for survival data.
 *
 *  @param kappa The kappa value for the Weibull distribution;
 *               if kappa = 1, then the exponential distribution will be used for simulation.
 */
// [[Rcpp::export]]
List getSimulationSurvivalCpp(
		int designNumber,
		int kMax,
		int sided,
		NumericVector criticalValues,
		NumericVector informationRates,
		double conditionalPower,
		NumericVector plannedEvents,
		double thetaH1,
		NumericVector minNumberOfEventsPerStage,
		NumericVector maxNumberOfEventsPerStage,
		bool directionUpper,
		double allocation1,
		double allocation2,
		NumericVector accrualTime,
		IntegerVector treatmentGroup,
		double thetaH0,
		NumericVector futilityBounds,
		NumericVector alpha0Vec,
		NumericVector pi1Vec,
		double pi2,
		double eventTime,
		NumericVector piecewiseSurvivalTime,
		NumericVector cdfValues1,
		NumericVector cdfValues2,
		NumericVector lambdaVec1,
		NumericVector lambdaVec2,
		NumericVector phi,
		int maxNumberOfSubjects,
		int maxNumberOfIterations,
		int maxNumberOfRawDatasetsPerStage,
		double kappa) {

	bool pwExpEnabled = isPiecewiseExponentialSurvivalEnabled(lambdaVec2);

	int n = 1;
	if (!pwExpEnabled) {
		n = pi1Vec.size();
	}
	if (n < 1) {
		throw Rcpp::exception(tfm::format(
			"'pi1Vec' must have minimum length %s (is %s)",
			1, pi1Vec.size()).c_str());
	}

	int sumVectorLength = kMax * n;
	IntegerVector stages = IntegerVector(sumVectorLength, NA_INTEGER);
	NumericVector pi1Column = NumericVector(sumVectorLength, 0.0);
	NumericVector hazardRatioColumn = NumericVector(sumVectorLength, 0.0);

	NumericVector analysisTimeSum = NumericVector(sumVectorLength, 0.0);
	NumericVector subjectsSum = NumericVector(sumVectorLength, 0.0);
	NumericVector eventsSum = NumericVector(sumVectorLength, 0.0);
	NumericVector rejectionsSum = NumericVector(sumVectorLength, 0.0);
	NumericVector eventsNotAchievedSum = NumericVector(sumVectorLength, 0.0);
	NumericVector futilityStopsSum = NumericVector(sumVectorLength, 0.0);
	NumericVector durationsSum = NumericVector(sumVectorLength, 0.0);
	NumericVector iterationsSum = NumericVector(sumVectorLength, 0.0);
	NumericVector conditionalPowerAchievedSum = NumericVector(sumVectorLength, 0.0);

	int simResultsVectorLength = sumVectorLength * maxNumberOfIterations;
	IntegerVector iterationNumbers = IntegerVector(simResultsVectorLength, NA_INTEGER);
	IntegerVector stageNumbers = IntegerVector(simResultsVectorLength, NA_INTEGER);
	NumericVector pi1Values = NumericVector(simResultsVectorLength, NA_REAL);
	NumericVector hazardRatios = NumericVector(simResultsVectorLength, NA_REAL);

	NumericVector analysisTime = NumericVector(simResultsVectorLength, NA_REAL);
	NumericVector subjects = NumericVector(simResultsVectorLength, NA_REAL);
	NumericVector events1 = NumericVector(simResultsVectorLength, NA_REAL);
	NumericVector events2 = NumericVector(simResultsVectorLength, NA_REAL);
	NumericVector events = NumericVector(simResultsVectorLength, NA_REAL);
	NumericVector rejections = NumericVector(simResultsVectorLength, NA_REAL);
	NumericVector eventsNotAchieved = NumericVector(simResultsVectorLength, NA_REAL);
	NumericVector futilityStops = NumericVector(simResultsVectorLength, NA_REAL);
	NumericVector pValuesSeparate = NumericVector(simResultsVectorLength, NA_REAL);
	NumericVector testStatistics = NumericVector(simResultsVectorLength, NA_REAL);
	NumericVector logRankStatistics = NumericVector(simResultsVectorLength, NA_REAL);
	NumericVector hazardRates1 = NumericVector(simResultsVectorLength, NA_REAL);
	NumericVector hazardRates2 = NumericVector(simResultsVectorLength, NA_REAL);
	NumericVector hazardRatiosEstimate = NumericVector(simResultsVectorLength, NA_REAL);
	NumericVector conditionalPowerAchieved = NumericVector(simResultsVectorLength, NA_REAL);

	// raw datasets per stage
	int rawDataVectorLength = maxNumberOfRawDatasetsPerStage * n * kMax *maxNumberOfSubjects;
	IntegerVector rawDataPerStage = IntegerVector(kMax, 0);

	NumericVector rawDataIterationNumbers = NumericVector(rawDataVectorLength, NA_REAL);
	IntegerVector rawDataStageNumbers = IntegerVector(rawDataVectorLength, NA_INTEGER);
	NumericVector rawDataPi1Values = NumericVector(rawDataVectorLength, NA_REAL);

	IntegerVector rawDataSubjectIds = IntegerVector(rawDataVectorLength, NA_INTEGER);
	NumericVector rawDataAccrualTime = NumericVector(rawDataVectorLength, NA_REAL);
	IntegerVector rawDataTreatmentGroups = IntegerVector(rawDataVectorLength, NA_INTEGER);
	NumericVector rawDataSurvivalTime = NumericVector(rawDataVectorLength, NA_REAL);
	NumericVector rawDataDropoutTime = NumericVector(rawDataVectorLength, NA_REAL);

	NumericVector rawDataObservationTime = NumericVector(rawDataVectorLength, NA_REAL);
	NumericVector rawDataTimeUnderObservation = NumericVector(rawDataVectorLength, NA_REAL);
	LogicalVector rawDataEvent = LogicalVector(rawDataVectorLength, NA_LOGICAL);
	LogicalVector rawDataDropoutEvent = LogicalVector(rawDataVectorLength, NA_LOGICAL);

	IntegerVector rawDataCensorIndicator = IntegerVector(rawDataVectorLength, NA_INTEGER);

	NumericMatrix survivalDataSet;
	int index = 0;
	for (int pi1Index = 0; pi1Index < n; pi1Index++) {

		double pi1 = NA_REAL;
		double hazardRatio = NA_REAL;
		double lambda1 = NA_REAL;
		double lambda2 = NA_REAL;

		if (!pwExpEnabled) {
			if (R_IsNA((double) pi1Vec[pi1Index])) {
				lambda1 = lambdaVec1[pi1Index];
				lambda2 = lambdaVec2[0];
			} else {
				pi1 = pi1Vec[pi1Index];
				lambda1 = getLambdaByPi(pi1, eventTime, kappa);
				lambda2 = getLambdaByPi(pi2, eventTime, kappa);
			}
			hazardRatio = pow(lambda1 / lambda2, kappa);
		}

		for (int k = 0; k < kMax; k++) {
			stages[pi1Index * kMax + k] = k + 1;
		}
		vectorInitC(pi1Index, kMax, REAL(pi1Column), pi1);
		vectorInitC(pi1Index, kMax, REAL(hazardRatioColumn), hazardRatio);

		for (int iterationIndex = 0; iterationIndex < maxNumberOfIterations; iterationIndex++) {

			if (!pwExpEnabled) {
				survivalDataSet = getExtendedSurvivalDataSet(
					treatmentGroup, maxNumberOfSubjects,
					lambda1, lambda2, (double) phi[0], (double) phi[1], kappa);
			} else {
				survivalDataSet = getExtendedSurvivalDataSet(treatmentGroup,
					maxNumberOfSubjects, piecewiseSurvivalTime,
					cdfValues1, cdfValues2, lambdaVec1, lambdaVec2, (double) phi[0], (double) phi[1]);
			}

			NumericVector survivalTime = survivalDataSet(_, 0);
			NumericVector dropoutTime = survivalDataSet(_, 1);

			NumericMatrix stepResults = getSimulationStepResultsSurvival(
				designNumber,
				kMax,
				sided,
				criticalValues,
				informationRates,
				conditionalPower,
				plannedEvents,
				thetaH1,
				minNumberOfEventsPerStage,
				maxNumberOfEventsPerStage,
				directionUpper,
				allocation1,
				allocation2,
				accrualTime,
				survivalTime,
				dropoutTime,
				treatmentGroup,
				thetaH0,
				futilityBounds,
				alpha0Vec);

			vectorSumC(pi1Index, 0, kMax, REAL(analysisTimeSum), stepResults);
			vectorSumC(pi1Index, 1, kMax, REAL(subjectsSum), stepResults);
			vectorSumC(pi1Index, 4, kMax, REAL(eventsSum), stepResults);
			vectorSumC(pi1Index, 5, kMax, REAL(rejectionsSum), stepResults);
			vectorSumC(pi1Index, 6, kMax, REAL(eventsNotAchievedSum), stepResults);
			vectorSumC(pi1Index, 7, kMax, REAL(futilityStopsSum), stepResults);
			vectorSumC(pi1Index, 8, kMax, REAL(durationsSum), stepResults);
			vectorSumC(pi1Index, 9, kMax, REAL(iterationsSum), stepResults);
			vectorSumC(pi1Index, 16, kMax, REAL(conditionalPowerAchievedSum), stepResults); // conditionalPowerAchieved

			// get data
			for (int k = 0; k < kMax; k++) {
				if (stepResults(k, 9) > 0) {
					iterationNumbers[index] = iterationIndex + 1;
					stageNumbers[index] = k + 1;
					pi1Values[index] = pi1;
					hazardRatios[index] = hazardRatio;

					analysisTime[index]       = stepResults(k, 0);
					subjects[index]           = stepResults(k, 1);
					events1[index]            = stepResults(k, 2);
					events2[index]            = stepResults(k, 3);
					events[index]             = stepResults(k, 4);
					rejections[index]         = stepResults(k, 5);
					eventsNotAchieved[index]  = stepResults(k, 6);
					futilityStops[index]      = stepResults(k, 7);
					testStatistics[index]     = stepResults(k, 10);
					logRankStatistics[index]  = stepResults(k, 11);

					hazardRates1[index] = stepResults(k, 12);
					hazardRates2[index] = stepResults(k, 13);
					hazardRatiosEstimate[index] = stepResults(k, 14);

					conditionalPowerAchieved[index] = stepResults(k, 16);
					pValuesSeparate[index] = stepResults(k, 17);

					index++;
				}
			}

			// get raw datasets per stage
			if (maxNumberOfRawDatasetsPerStage > 0) {
				for (int k = kMax - 1; k >= 0; k--) {
					if (rawDataPerStage[k] < maxNumberOfRawDatasetsPerStage && stepResults(k, 9) > 0) {

						int start = k * maxNumberOfSubjects + pi1Index * kMax * maxNumberOfSubjects +
								rawDataPerStage[k] * n * kMax * maxNumberOfSubjects;

						double observationTime = stepResults(k, 15);

						if (R_IsNA(observationTime)) {
							break;
						}

						List logRankResult = logRankTest(
							accrualTime, survivalTime, dropoutTime, treatmentGroup,
							observationTime, directionUpper, thetaH0, true);

						NumericVector timeUnderObservation = logRankResult["timeUnderObservation"];
						LogicalVector event = logRankResult["event"];
						LogicalVector dropoutEvent = logRankResult["dropoutEvent"];

						for (int i = 0; i < maxNumberOfSubjects; i++) {
							rawDataPi1Values[start + i] = pi1;
							rawDataIterationNumbers[start + i] = iterationIndex + 1;
							rawDataStageNumbers[start + i] = k + 1;

							rawDataSubjectIds[start + i] = i + 1;
							rawDataAccrualTime[start + i] = accrualTime[i];
							rawDataTreatmentGroups[start + i] = treatmentGroup[i];
							rawDataSurvivalTime[start + i] = survivalTime[i];
							rawDataDropoutTime[start + i] = dropoutTime[i];

							rawDataObservationTime[start + i] = observationTime;
							rawDataTimeUnderObservation[start + i] = timeUnderObservation[i];
							rawDataEvent[start + i] = event[i];
							rawDataDropoutEvent[start + i] = dropoutEvent[i];

							if (survivalTime[i] >= dropoutTime[i]) {
								rawDataCensorIndicator[start + i] = 0;
							} else {
								rawDataCensorIndicator[start + i] = 1;
							}
						}

						rawDataPerStage[k]++;

						break;
					}
				}
			}
		}
	}

	NumericVector overallRejections = NumericVector(n, 0.0);
	NumericVector overallFutilityStops = NumericVector(n, 0.0);
	NumericVector duration = NumericVector(n, 0.0);

	NumericVector rejectionsRelative = vectorDivide(rejectionsSum, maxNumberOfIterations);
	NumericVector futilityStopsRelative = vectorDivide(futilityStopsSum, maxNumberOfIterations);
	for (int i = 0; i < n; i++) {
		double s1 = 0;
		double s2 = 0;
		double s3 = 0;
		for (int j = 0; j < kMax; j++) {
			s1 += rejectionsRelative[i * kMax + j];
			s2 += futilityStopsRelative[i * kMax + j];
			s3 += durationsSum[i * kMax + j];
		}
		overallRejections[i] = s1;
		overallFutilityStops[i] = s2;
		duration[i] = s3 / maxNumberOfIterations;
	}

	DataFrame overview = DataFrame::create(
		Named("stages") = stages,
		Named("pi2") = NumericVector(sumVectorLength, pi2),
		Named("pi1") = pi1Column,
		Named("hazardRatioEstimate1") = hazardRatioColumn,
		Named("iterations") = iterationsSum,
		Named("eventsPerStage") = vectorDivide(eventsSum, iterationsSum),
		Named("eventsNotAchieved") = vectorDivide(eventsNotAchievedSum, maxNumberOfIterations),
		Named("numberOfSubjects") = vectorDivide(subjectsSum, iterationsSum),
		Named("rejectPerStage") = rejectionsRelative,
		Named("overallReject") = vectorRepEachValue(overallRejections, kMax),
		Named("futilityPerStage") = futilityStopsRelative,
		Named("futilityStop") = vectorRepEachValue(overallFutilityStops, kMax),
		Named("analysisTime") = vectorDivide(analysisTimeSum, iterationsSum),
		Named("studyDuration") =  vectorRepEachValue(duration, kMax),
		Named("conditionalPowerAchieved") = vectorDivide(conditionalPowerAchievedSum, iterationsSum)
	);

	DataFrame data = DataFrame::create(
		Named("iterationNumber") = iterationNumbers,
		Named("stageNumber") = stageNumbers,
		Named("pi1") = pi1Values,
		Named("pi2") = NumericVector(simResultsVectorLength, pi2),
		Named("hazardRatio") = hazardRatios,
		Named("analysisTime") = analysisTime,
		Named("numberOfSubjects") = subjects,
		Named("overallEvents1") = events1,
		Named("overallEvents2") = events2,
		Named("eventsPerStage") = events,
		Named("rejectPerStage") = rejections,
		Named("eventsNotAchieved") = eventsNotAchieved,
		Named("futilityPerStage") = futilityStops,
		Named("testStatistic") = testStatistics,
		Named("logRankStatistic") = logRankStatistics,
		Named("conditionalPowerAchieved") = conditionalPowerAchieved,
		Named("pValuesSeparate") = pValuesSeparate
	);

	if (maxNumberOfRawDatasetsPerStage > 0) {
		DataFrame rawData = DataFrame::create(
			Named("iterationNumber") = rawDataIterationNumbers,
			Named("stopStage") = rawDataStageNumbers,
			Named("pi1") = rawDataPi1Values,
			Named("pi2") = NumericVector(rawDataVectorLength, pi2),

			Named("subjectId") = rawDataSubjectIds,
			Named("accrualTime") = rawDataAccrualTime,
			Named("treatmentGroup") = rawDataTreatmentGroups,
			Named("survivalTime") = rawDataSurvivalTime,
			Named("dropoutTime") = rawDataDropoutTime,

			Named("observationTime") = rawDataObservationTime,
			Named("timeUnderObservation") = rawDataTimeUnderObservation,
			Named("event") = rawDataEvent,
			Named("dropoutEvent") = rawDataDropoutEvent,

			Named("censorIndicator") = rawDataCensorIndicator
		);

		return List::create(
			_["overview"] = overview,
			_["data"] = data,
			_["rawData"] = rawData
		);
	}

	return List::create(
		_["overview"] = overview,
		_["data"] = data
	);
}


