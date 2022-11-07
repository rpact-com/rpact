/**
 *
 * -- Simulation of means with group sequential and combination test --
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
 * File version: $Revision: 6417 $
 * Last changed: $Date: 2022-07-15 09:24:52 +0200 (Fri, 15 Jul 2022) $
 * Last changed by: $Author: pahlke $
 *
 */

#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

#include <cmath>
#include "f_utilities.h"
using namespace Rcpp;

List getTestStatisticsMeans(double designNumber, NumericVector informationRates,
		double groups, bool normalApproximation,
		bool meanRatio, double thetaH0, double allocationRatioPlanned,
		NumericVector sampleSizesPerStage, NumericVector testStatisticsPerStage) {

	NumericVector pValuesSeparate, standardizedEffectEstimate;
	double value = 1;
	int stage = sampleSizesPerStage.length();
	double overallTestStatistic = vectorProduct(sqrt(sampleSizesPerStage), testStatisticsPerStage) /
			sqrt(sum(sampleSizesPerStage));
	if (normalApproximation) {
		pValuesSeparate = 1.0 - pnorm(testStatisticsPerStage);
	} else {
		for (int i = 0; i < pValuesSeparate.length(); ++i) {
			double df = sampleSizesPerStage[i] - groups;
			pValuesSeparate[i] = 1.0 - R::pt((double) testStatisticsPerStage[i], df, true, false);
		}
	}
	if (designNumber == 1L) {
		if (normalApproximation) {
			value = overallTestStatistic;
		} else {
			value = getQNorm(R::pt(overallTestStatistic, sum(sampleSizesPerStage) - groups, true, false));
		}
	} else if (designNumber == 2L) {
		if (stage == 1) {
			if (normalApproximation) {
				value = testStatisticsPerStage[0];
			} else {
				value = getQNorm(R::pt((double) testStatisticsPerStage[0],
						(double) sampleSizesPerStage[0] - groups, true, false));
			}
		} else {
			if (normalApproximation) {
				value = (sqrt((double) informationRates[0]) * testStatisticsPerStage[0] +
						vectorProduct(sqrt(tail(head(informationRates, stage), stage - 1) -
								head(informationRates, stage - 1)),
								tail(head(testStatisticsPerStage, stage), stage - 1))) /
										sqrt((double) informationRates[stage - 1]);

			} else {
				NumericVector helperVec;
				for (int i = 1; i < stage; i++) {
					helperVec.push_back(getQNorm(R::pt((double) testStatisticsPerStage[i],
							(double) sampleSizesPerStage[i] - groups, true, false)));
				}
				value = (sqrt((double) informationRates[0]) *
						getQNorm(R::pt((double) testStatisticsPerStage[0],
								(double) sampleSizesPerStage[0] - groups, true, false)) +
						vectorProduct(sqrt(tail(head(informationRates, stage), stage - 1) -
								head(informationRates, stage - 1)), helperVec)) /
										sqrt((double) informationRates[stage - 1]);
			}
		}
	} else if (designNumber == 3L) {
		NumericVector weightsFisher = rep(NA_REAL, stage);
		weightsFisher[0] = 1;
		if (stage > 1) {
			for (int i = 1; i < stage; ++i) {
				weightsFisher[i] = sqrt((double) informationRates[i] - informationRates[i - 1]) /
						sqrt((double) informationRates[0]);
			}
		}
		if (normalApproximation) {
			NumericVector helperVec = 1 - pnorm(head(testStatisticsPerStage, stage));
			for (int i = 0; i < stage; ++i) {
				value *= pow((double) helperVec[i], (double) weightsFisher[i]);
			}
		} else {
			for (int i = 0; i < stage; ++i) {
				double q = testStatisticsPerStage[i];
				int df = sampleSizesPerStage[i] - groups;
				double tDistribution = 1 - R::pt(q, df, true, false);
				value *= pow(tDistribution, (double) weightsFisher[i]);
			}
		}
	}

	if (groups == 1) {
		standardizedEffectEstimate = overallTestStatistic / sqrt(sum(sampleSizesPerStage));
	} else {
		if (!meanRatio) {
			standardizedEffectEstimate = overallTestStatistic /
					sqrt(allocationRatioPlanned * sum(sampleSizesPerStage)) *
					(1 + allocationRatioPlanned);
		} else {
			standardizedEffectEstimate = overallTestStatistic /
					sqrt(allocationRatioPlanned * sum(sampleSizesPerStage)) *
					sqrt((1 + allocationRatioPlanned) *
							(1 + pow(thetaH0, 2) * allocationRatioPlanned));
		}
	}

	return List::create(
		_["value"] = value,
		_["overallTestStatistic"] = overallTestStatistic,
		_["standardizedEffectEstimate"] = standardizedEffectEstimate,
		_["pValuesSeparate"] = pValuesSeparate
	);
}

double getSimulationMeansStageSubjects(double stage,
		bool meanRatio, double thetaH0, double groups, NumericVector plannedSubjects,
		double allocationRatioPlanned,
		NumericVector minNumberOfSubjectsPerStage,
		NumericVector maxNumberOfSubjectsPerStage,
		double thetaH1,
		double stDevH1,
		NumericVector conditionalPower,
		double conditionalCriticalValue) {

	if (NumericVector::is_na((double) conditionalPower[0])) {
		return plannedSubjects[stage - 1] - plannedSubjects[stage - 2];
	}
	double thetaStandardized = thetaH1 / stDevH1;

	double mult = 1;
	if (groups == 2) {
		thetaH0 = meanRatio ? thetaH0 : 1;
		mult = 1.0 + 1.0 / allocationRatioPlanned + pow(thetaH0, 2) * (1.0 + allocationRatioPlanned);
	}

	double stageSubjects = pow(std::max(0.0, conditionalCriticalValue + getQNorm((double) conditionalPower[0])), 2) *
			mult / pow(std::max(1e-12, thetaStandardized), 2);

	stageSubjects = std::min(
			std::max(minNumberOfSubjectsPerStage[stage - 1], stageSubjects),
			maxNumberOfSubjectsPerStage[stage - 1]);

	return stageSubjects;
}

List getInitialSimulationStepMeans(
		int k,
		double kMax,
		double designNumber,
		NumericVector informationRates,
		NumericVector futilityBounds,
		NumericVector alpha0Vec,
		NumericVector criticalValues,
		bool meanRatio,
		double thetaH0,
		double alternative,
		double stDev,
		double groups,
		bool normalApproximation,
		NumericVector plannedSubjects,
		bool directionUpper,
		double allocationRatioPlanned,
		NumericVector minNumberOfSubjectsPerStage,
		NumericVector maxNumberOfSubjectsPerStage,
		NumericVector conditionalPower,
		double thetaH1,
		double stDevH1) {

	double nz, simulatedConditionalPower, testResult;
	double stageSubjects = plannedSubjects[0];
	if (groups == 1) {
		nz = (alternative - thetaH0) / stDev * sqrt(stageSubjects);
		if (normalApproximation) {
			testResult = (2 * directionUpper - 1) * R::rnorm(nz, 1);
		} else {
			testResult = (2 * directionUpper - 1) *
				getRandomTDistribution(stageSubjects - 1, nz);
		}
	} else {
		if (!meanRatio) {
			nz = (alternative - thetaH0) / stDev * sqrt(allocationRatioPlanned * stageSubjects) /
					(1 + allocationRatioPlanned);
		} else {
			nz = (alternative - thetaH0) / stDev * sqrt(allocationRatioPlanned * stageSubjects) /
					sqrt((1 + allocationRatioPlanned) * (1 + pow(thetaH0,2) * allocationRatioPlanned));
		}
		if (normalApproximation) {
			testResult = (2 * directionUpper - 1) * R::rnorm(nz, 1);
		} else {
			testResult = (2 * directionUpper - 1) *
				getRandomTDistribution(stageSubjects - 2, nz);
		}
	}

	NumericVector sampleSizesPerStage = NumericVector::create(stageSubjects);
	NumericVector testStatisticsPerStage = NumericVector::create(testResult);
	List testStatistic = getTestStatisticsMeans(designNumber, informationRates, groups,
			normalApproximation, meanRatio,
			thetaH0, allocationRatioPlanned, sampleSizesPerStage,
			testStatisticsPerStage);

	double effectEstimate = (double) testStatistic["standardizedEffectEstimate"] * stDev;

	double simulatedRejections = 0;
	double simulatedFutilityStop = 0;
	bool trialStop = false;
	if (k == kMax) {
		trialStop = true;
	}
	if (designNumber == 3L) {
		if (!R_IsNA((double) testStatistic["value"]) && !R_IsNA((double) criticalValues[k - 1]) &&
				(double) testStatistic["value"] <= criticalValues[k - 1]) {
			simulatedRejections = 1;
			trialStop = true;
		}
		NumericVector testStatisticPValuesSeparate = testStatistic["pValuesSeparate"];
		if (!R_IsNA((double) testStatisticPValuesSeparate[k - 1]) && !R_IsNA((double) alpha0Vec[k - 1]) &&
				k < kMax && (double) testStatisticPValuesSeparate[k - 1] >= alpha0Vec[k - 1]) {
			simulatedFutilityStop = 1;
			trialStop = true;
		}
	} else {
		if (!R_IsNA((double) testStatistic["value"]) && !R_IsNA((double) criticalValues[k - 1]) &&
				(double) testStatistic["value"] >= criticalValues[k - 1]) {
			simulatedRejections = 1;
			trialStop = true;
		}
		if (!R_IsNA((double) testStatistic["value"]) && !R_IsNA((double) futilityBounds[k - 1]) &&
				k < kMax && (double) testStatistic["value"] <= futilityBounds[k - 1]) {
			simulatedFutilityStop = 1;
			trialStop = true;
		}
	}

	if (!directionUpper) {
		effectEstimate = -effectEstimate;
	}

	return List::create(
		_["trialStop"] = trialStop,
		_["sampleSizesPerStage"] = sampleSizesPerStage,
		_["testStatisticsPerStage"] = testStatisticsPerStage,
		_["testStatistic"] = testStatistic,
		_["effectEstimate"] = effectEstimate,
		_["simulatedSubjects"] = stageSubjects,
		_["simulatedRejections"] = simulatedRejections,
		_["simulatedFutilityStop"] = simulatedFutilityStop,
		_["simulatedConditionalPower"] = simulatedConditionalPower
	);
}

List getSimulationStepMeans(
		int k,
		double kMax,
		double designNumber,
		NumericVector informationRates,
		NumericVector futilityBounds,
		NumericVector alpha0Vec,
		NumericVector criticalValues,
		bool meanRatio,
		double thetaH0,
		double alternative,
		double stDev,
		double groups,
		bool normalApproximation,
		NumericVector plannedSubjects,
		bool directionUpper,
		double allocationRatioPlanned,
		NumericVector minNumberOfSubjectsPerStage,
		NumericVector maxNumberOfSubjectsPerStage,
		NumericVector conditionalPower,
		double thetaH1,
		double stDevH1,
		double effectEstimate,
		NumericVector sampleSizesPerStage,
		NumericVector testStatisticsPerStage,
		List testStatistic) {

	double nz, testResult, thetaStandardized, simulatedConditionalPower, conditionalCriticalValue;
	double stageSubjects = plannedSubjects[0];

	// perform sample size size recalculation for stages 2, ..., kMax
	simulatedConditionalPower = 0;
	if (k > 1) {

		// used effect size is either estimated from test statistic or pre-fixed
		if (R_IsNA(thetaH1)) {
			thetaH1 = effectEstimate;
		} else {
			thetaH1 = thetaH1 - thetaH0;
		}
		thetaStandardized = thetaH1 / stDevH1;

		if (!directionUpper) {
			thetaH1 = -thetaH1;
			thetaStandardized = -thetaStandardized;
		}
		// conditional critical value to reject the null hypotheses at the next stage of the trial
		if (designNumber == 3L) {
			conditionalCriticalValue = getOneMinusQNorm(pow((double) criticalValues[k - 1] /
					(double) testStatistic["value"],
					1.0 / sqrt((double) (informationRates[k - 1] -
							informationRates[k - 2]) / informationRates[0])));
		} else {
			conditionalCriticalValue = (criticalValues[k - 1] *
					sqrt((double) informationRates[k - 1]) -
					(double) testStatistic["value"] * sqrt((double) informationRates[k - 2])) /
							sqrt((double) informationRates[k - 1] - informationRates[k - 2]);
		}

		stageSubjects = getSimulationMeansStageSubjects(k, meanRatio, thetaH0, groups, plannedSubjects,
				allocationRatioPlanned,
				minNumberOfSubjectsPerStage, maxNumberOfSubjectsPerStage,
				thetaH1, stDevH1, conditionalPower,
				conditionalCriticalValue);

		// calculate conditional power for computed stageSubjects
		if (groups == 2.0) {
			if (!meanRatio) {
				thetaStandardized = thetaStandardized * sqrt((double) allocationRatioPlanned) / (1.0 + allocationRatioPlanned);
			} else {
				thetaStandardized = thetaStandardized * sqrt(allocationRatioPlanned) /
						sqrt((1.0 + allocationRatioPlanned) * (1.0 + thetaH0 * allocationRatioPlanned));
			}
		}
		simulatedConditionalPower = 1 - R::pnorm(conditionalCriticalValue - thetaStandardized * sqrt(stageSubjects),0,1,1,0);
	}

	if (groups == 1.0) {
		nz = (alternative - thetaH0) / stDev * sqrt(stageSubjects);
		if (normalApproximation) {
			testResult = (2.0 * directionUpper - 1.0) * R::rnorm(nz, 1.0);
		} else {
			testResult = (2.0 * directionUpper - 1.0) *
				getRandomTDistribution(stageSubjects - 1, nz);
		}
	} else {
		if (!meanRatio) {
			nz = (alternative - thetaH0) / stDev * sqrt(allocationRatioPlanned * stageSubjects) /
					(1 + allocationRatioPlanned);
		} else {
			nz = (alternative - thetaH0) / stDev * sqrt(allocationRatioPlanned * stageSubjects) /
					sqrt((1 + allocationRatioPlanned) * (1 + pow(thetaH0,2) * allocationRatioPlanned));
		}
		if (normalApproximation) {
			testResult = (2.0 * directionUpper - 1.0) * R::rnorm(nz, 1.0);
		} else {
			testResult = (2.0 * directionUpper - 1.0) *
				getRandomTDistribution(stageSubjects - 2, nz);
		}
	}

	sampleSizesPerStage.push_back(stageSubjects);
	testStatisticsPerStage.push_back(testResult);
	testStatistic = getTestStatisticsMeans(designNumber, informationRates, groups,
			normalApproximation, meanRatio,
			thetaH0, allocationRatioPlanned, sampleSizesPerStage,
			testStatisticsPerStage);

	effectEstimate = (double) testStatistic["standardizedEffectEstimate"] * stDev;

	double simulatedRejections = 0;
	double simulatedFutilityStop = 0;
	bool trialStop = false;
	if (k == kMax) {
		trialStop = true;
	}
	if (designNumber == 3L) {
		if (!R_IsNA((double) testStatistic["value"]) && !R_IsNA((double) criticalValues[k - 1]) &&
				(double) testStatistic["value"] <= criticalValues[k - 1]) {
			simulatedRejections = 1;
			trialStop = true;
		}
		NumericVector testStatisticPValuesSeparate = testStatistic["pValuesSeparate"];
		if (!R_IsNA((double) testStatisticPValuesSeparate[k - 1]) && !R_IsNA((double) alpha0Vec[k - 1]) &&
				k < kMax && (double) testStatisticPValuesSeparate[k - 1] >= alpha0Vec[k - 1]) {
			simulatedFutilityStop = 1;
			trialStop = true;
		}
	} else {
		if (!R_IsNA((double) testStatistic["value"]) && !R_IsNA((double) criticalValues[k - 1]) &&
				(double) testStatistic["value"] >= criticalValues[k - 1]) {
			simulatedRejections = 1;
			trialStop = true;
		}
		if (!R_IsNA((double) testStatistic["value"]) && !R_IsNA((double) futilityBounds[k - 1]) &&
				k < kMax && (double) testStatistic["value"] <= futilityBounds[k - 1]) {
			simulatedFutilityStop = 1;
			trialStop = true;
		}
	}

	if (!directionUpper) {
		effectEstimate = -effectEstimate;
	}
	return List::create(
		_["trialStop"] = trialStop,
		_["sampleSizesPerStage"] = sampleSizesPerStage,
		_["testStatisticsPerStage"] = testStatisticsPerStage,
		_["testStatistic"] = testStatistic,
		_["effectEstimate"] = effectEstimate,
		_["simulatedSubjects"] = stageSubjects,
		_["simulatedRejections"] = simulatedRejections,
		_["simulatedFutilityStop"] = simulatedFutilityStop,
		_["simulatedConditionalPower"] = simulatedConditionalPower
	);
}

// [[Rcpp::export]]
List getSimulationMeansLoopCpp(
		NumericVector alternative,
		int kMax,
		int maxNumberOfIterations,
		int designNumber,
		NumericVector informationRates,
		NumericVector futilityBounds,
		NumericVector alpha0Vec,
		NumericVector criticalValues,
		bool meanRatio,
		double thetaH0,
		double stDev,
		double groups,
		bool normalApproximation,
		NumericVector plannedSubjects,
		bool directionUpper,
		double allocationRatioPlanned,
		NumericVector minNumberOfSubjectsPerStage,
		NumericVector maxNumberOfSubjectsPerStage,
		NumericVector conditionalPower,
		double thetaH1,
		double stDevH1,
		Function calcSubjectsFunction) {

    int cols = alternative.length();

	NumericMatrix sampleSizes(kMax, cols);
	std::fill(sampleSizes.begin(), sampleSizes.end(), 0.0) ;

	NumericMatrix rejectPerStage(kMax, cols);
	std::fill(rejectPerStage.begin(), rejectPerStage.end(), 0.0) ;

	NumericVector overallReject = NumericVector(cols, 0.0);

    NumericMatrix futilityPerStage(kMax - 1, cols);
	std::fill(futilityPerStage.begin(), futilityPerStage.end(), 0.0) ;

    NumericVector futilityStop = NumericVector(cols, 0.0);

    NumericMatrix iterations(kMax, cols);
	std::fill(iterations.begin(), iterations.end(), 0.0) ;

    NumericVector expectedNumberOfSubjects = NumericVector(cols, 0.0);

    NumericMatrix conditionalPowerAchieved (kMax, cols);
	std::fill(conditionalPowerAchieved.begin(), conditionalPowerAchieved.end(), NA_REAL) ;

	int len = alternative.length() * maxNumberOfIterations * kMax;
	NumericVector dataIterationNumber = NumericVector(len, NA_REAL);
	NumericVector dataStageNumber = NumericVector(len, NA_REAL);
	NumericVector dataAlternative = NumericVector(len, NA_REAL);
	NumericVector dataNumberOfSubjects = NumericVector(len, NA_REAL);
	NumericVector dataNumberOfCumulatedSubjects = NumericVector(len, NA_REAL);
	NumericVector dataRejectPerStage = NumericVector(len, NA_REAL);
	NumericVector dataFutilityPerStage = NumericVector(len, NA_REAL);
	NumericVector dataTestStatistic = NumericVector(len, NA_REAL);
	NumericVector dataTestStatisticsPerStage = NumericVector(len, NA_REAL);
	NumericVector dataTrialStop = NumericVector(len, NA_REAL);
	NumericVector dataConditionalPowerAchieved = NumericVector(len, NA_REAL);
	NumericVector dataEffectEstimate = NumericVector(len, NA_REAL);
	NumericVector dataPValuesSeparate = NumericVector(len, NA_REAL);

	double index = 1;
	for (int i = 1; i <= alternative.length(); i++) {
		NumericVector simulatedSubjects (kMax);
		NumericVector simulatedRejections (kMax);
		NumericVector simulatedFutilityStop (kMax - 1);
		NumericVector simulatedConditionalPower (kMax);

		for (int j = 1; j <= maxNumberOfIterations; j++) {
			bool trialStop = false;
			List testStatistic;
			NumericVector sampleSizesPerStage;
			NumericVector testStatisticsPerStage;
			double effectEstimate = 0;

			for (int k = 1; k <= kMax; k++) {
				if (!trialStop) {
					List stepResult;
					if (testStatistic.size() == 0 || sampleSizesPerStage.size() == 0 || testStatisticsPerStage.size() == 0) {
						stepResult = getInitialSimulationStepMeans(
							k,
							kMax,
							designNumber,
							informationRates,
							futilityBounds,
							alpha0Vec,
							criticalValues,
							meanRatio,
							thetaH0,
							(double) alternative[i - 1],
							stDev,
							groups,
							normalApproximation,
							plannedSubjects,
							directionUpper,
							allocationRatioPlanned,
							minNumberOfSubjectsPerStage,
							maxNumberOfSubjectsPerStage,
							conditionalPower,
							thetaH1,
							stDevH1);
					} else {
						stepResult = getSimulationStepMeans(
							k,
							kMax,
							designNumber,
							informationRates,
							futilityBounds,
							alpha0Vec,
							criticalValues,
							meanRatio,
							thetaH0,
							(double) alternative[i - 1],
							stDev,
							groups,
							normalApproximation,
							plannedSubjects,
							directionUpper,
							allocationRatioPlanned,
							minNumberOfSubjectsPerStage,
							maxNumberOfSubjectsPerStage,
							conditionalPower,
							thetaH1,
							stDevH1,
							effectEstimate,
							sampleSizesPerStage,
							testStatisticsPerStage,
							testStatistic);
					}

					trialStop = stepResult["trialStop"];
					sampleSizesPerStage = stepResult["sampleSizesPerStage"];
					testStatisticsPerStage = stepResult["testStatisticsPerStage"];
					testStatistic = stepResult["testStatistic"];
					double simulatedSubjectsStep = stepResult["simulatedSubjects"];
					double simulatedRejectionsStep = stepResult["simulatedRejections"];
					double simulatedFutilityStopStep = stepResult["simulatedFutilityStop"];
					effectEstimate = stepResult["effectEstimate"];
					double simulatedConditionalPowerStep = NA_REAL;
					if (k > 1) {
						simulatedConditionalPowerStep = stepResult["simulatedConditionalPower"];
					}
					iterations(k - 1, i - 1) = iterations(k - 1, i - 1) + 1;
					simulatedSubjects[k - 1] = simulatedSubjects[k - 1] + simulatedSubjectsStep;
					simulatedRejections[k - 1] = simulatedRejections[k - 1] + simulatedRejectionsStep;
					if (k < kMax) {
						simulatedFutilityStop[k - 1] = simulatedFutilityStop[k - 1] + simulatedFutilityStopStep;
					}
					simulatedConditionalPower[k - 1] = simulatedConditionalPower[k - 1] + simulatedConditionalPowerStep;

					dataIterationNumber[index - 1] = j;
					dataStageNumber[index - 1] = k;
					dataAlternative[index - 1] = alternative[i - 1];
					dataNumberOfSubjects[index - 1] = simulatedSubjectsStep;
					dataNumberOfCumulatedSubjects[index - 1] = sum(sampleSizesPerStage);
					dataRejectPerStage[index - 1] = simulatedRejectionsStep;
					dataFutilityPerStage[index - 1] = simulatedFutilityStopStep;
					dataTestStatistic[index - 1] = testStatistic["value"];
					dataTestStatisticsPerStage[index - 1] = testStatisticsPerStage[k - 1];
					dataTrialStop[index - 1] = trialStop;
					dataConditionalPowerAchieved[index - 1] = simulatedConditionalPowerStep;
					dataEffectEstimate[index - 1] = effectEstimate;
					if (designNumber == 3L) {
						NumericVector helperVec = testStatistic["pValuesSeparate"];
						dataPValuesSeparate[index - 1] = helperVec[k - 1];
					}
					index++;
				}
			}
		}

		double simulatedOverallSubjects = sum(rangeVector(simulatedSubjects, 0, kMax - 1));
		sampleSizes(_, i - 1) = simulatedSubjects / iterations(_, i - 1);
		rejectPerStage(_, i - 1) = simulatedRejections / maxNumberOfIterations;
		overallReject[i - 1] = sum(simulatedRejections / maxNumberOfIterations);
		futilityPerStage(_, i - 1) = simulatedFutilityStop / maxNumberOfIterations;
		futilityStop[i - 1] = sum(simulatedFutilityStop / maxNumberOfIterations);
		expectedNumberOfSubjects[i - 1] = simulatedOverallSubjects / maxNumberOfIterations;
		if (kMax > 1) {
			NumericVector helper = conditionalPowerAchieved(_, i - 1);
			NumericVector result = simulatedConditionalPower / iterations(_, i - 1);
			for(int n = 1; n < kMax; n++) {
				helper[n] = result[n];
			}
			conditionalPowerAchieved(_, i - 1) = helper;
		}
	}

	DataFrame data = DataFrame::create(
		_["iterationNumber"] = dataIterationNumber,
		_["stageNumber"] = dataStageNumber,
		_["alternative"] = dataAlternative,
		_["numberOfSubjects"] = dataNumberOfSubjects,
		_["numberOfCumulatedSubjects"] = dataNumberOfCumulatedSubjects,
		_["rejectPerStage"] = dataRejectPerStage,
		_["futilityPerStage"] = dataFutilityPerStage,
		_["testStatistic"] = dataTestStatistic,
		_["testStatisticsPerStage"] = dataTestStatisticsPerStage,
		_["effectEstimate"] = dataEffectEstimate,
		_["trialStop"] = dataTrialStop,
		_["conditionalPowerAchieved"] = dataConditionalPowerAchieved,
		_["pValue"] = dataPValuesSeparate
	);

	return List::create(
		_["sampleSizes"] = sampleSizes,
		_["iterations"] = iterations,
		_["rejectPerStage"] = rejectPerStage,
		_["overallReject"] = overallReject,
		_["futilityPerStage"] = futilityPerStage,
		_["futilityStop"] = futilityStop,
		_["expectedNumberOfSubjects"] = expectedNumberOfSubjects,
		_["conditionalPowerAchieved"] = conditionalPowerAchieved,
		_["data"] = data
	);
}
