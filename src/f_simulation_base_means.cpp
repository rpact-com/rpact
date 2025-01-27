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
 * File version: $Revision: 8491 $
 * Last changed: $Date: 2025-01-20 11:02:58 +0100 (Mo, 20 Jan 2025) $
 * Last changed by: $Author: wassmer $
 *
 */

#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

#include "f_utilities.h"
#include "rpact_types.h"

using namespace Rcpp;

NumericVector getTestStatisticsMeans(
		double designNumber,
		NumericVector informationRates,
		int groups,
		bool normalApproximation,
		bool meanRatio,
		double thetaH0,
		NumericVector allocationRatioPlanned,
		NumericVector sampleSizesPerStage,
		NumericVector testStatisticsPerStage) {

	NumericVector pValuesSeparate = NumericVector(testStatisticsPerStage.size(), NA_REAL);
	double value = 1;
	int stage = sampleSizesPerStage.length();
	double overallTestStatistic = vectorProduct(sqrt(sampleSizesPerStage), testStatisticsPerStage) /
			sqrt(sum(sampleSizesPerStage));
	if (normalApproximation) {
		pValuesSeparate = 1.0 - Rcpp::pnorm(testStatisticsPerStage);
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

	double standardizedEffectEstimate;
	if (groups == 1) {
		standardizedEffectEstimate = overallTestStatistic / sqrt(sum(sampleSizesPerStage));
	} else {
		NumericVector allocationRatios = rangeVector(allocationRatioPlanned, 0, stage - 1);
		double numerator = !meanRatio ? 1 : pow(thetaH0, 2);
		standardizedEffectEstimate = overallTestStatistic *
			sqrt(1 / sum(allocationRatios / (1 + allocationRatios) * sampleSizesPerStage) +
				numerator / sum(1 / (1 + allocationRatios) * sampleSizesPerStage));
	}

	NumericVector result = NumericVector(3 + pValuesSeparate.size(), NA_REAL);
	result[0] = value;
	result[1] = overallTestStatistic;
	result[2] = standardizedEffectEstimate;
	for (int i = 0; i < pValuesSeparate.size(); i++) {
		result[3 + i] = pValuesSeparate[i];
	}
	return result;
}

double getSimulationMeansStageSubjects(
		int stage,
		bool meanRatio,
		double thetaH0,
		int groups,
		NumericVector plannedSubjects,
		NumericVector allocationRatioPlanned,
		NumericVector minNumberOfSubjectsPerStage,
		NumericVector maxNumberOfSubjectsPerStage,
		NumericVector sampleSizesPerStage,
		double thetaH1,
		NumericVector stDevH1,
		double conditionalPower,
		double conditionalCriticalValue) {

	if (R_IsNA(conditionalPower)) {
		return plannedSubjects[stage - 1] - plannedSubjects[stage - 2];
	}

	double stDev1H1 = stDevH1[0];
	double stDev2H1 = stDev1H1;
	if (stDevH1.length() > 1) {
		stDev2H1 = stDevH1[1];
	}

	double thetaStandardized;
	
	if (groups == 1) {
	  thetaStandardized = thetaH1 / stDev1H1;
	} else {
	  double allocationRatio = allocationRatioPlanned[stage - 1];	  
	  thetaH0 = meanRatio ? thetaH0 : 1;
    thetaStandardized = thetaH1 /
	      sqrt(pow(stDev1H1, 2) * (1.0 + 1 / allocationRatio) + 
	        pow(stDev2H1, 2) * pow(thetaH0, 2) * (1.0 + allocationRatio)); 
	}

	double stageSubjects = pow(std::max(0.0, conditionalCriticalValue + getQNorm(conditionalPower)), 2) /
			  pow(std::max(1e-12, thetaStandardized), 2);

	return std::min(
		std::max((double) minNumberOfSubjectsPerStage[stage - 1], stageSubjects),
		(double) maxNumberOfSubjectsPerStage[stage - 1]);
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
		NumericVector stDev,
		int groups,
		bool normalApproximation,
		NumericVector plannedSubjects,
		bool directionUpper,
		NumericVector allocationRatioPlanned,
		NumericVector minNumberOfSubjectsPerStage,
		NumericVector maxNumberOfSubjectsPerStage,
		double conditionalPower,
		double thetaH1,
		NumericVector stDevH1,
		double effectEstimate,
		NumericVector sampleSizesPerStage,
		NumericVector testStatisticsPerStage,
		NumericVector testStatistic,
		int calcSubjectsFunctionType,
		Nullable<Function> calcSubjectsFunctionR,
		Rcpp::XPtr<calcSubjectsFunctionMeansPtr> calcSubjectsFunctionCppXPtr) {

	double nz, testResult, thetaStandardized, simulatedConditionalPower, conditionalCriticalValue;
	double stageSubjects = plannedSubjects[0];
	double testStatisticValue;
	double criticalValue = criticalValues[k - 1];

	double stDev1H1 = stDevH1[0];
	double stDev2H1 = stDev1H1; 
	if (stDevH1.length() > 1) {
		stDev2H1 = stDevH1[1];
	}

	// perform sample size size recalculation for stages 2,..., kMax
	simulatedConditionalPower = 0;
	if (k > 1) {

		testStatisticValue = testStatistic[0]; // value

		// used effect size is either estimated from test statistic or pre-fixed
		if (R_IsNA(thetaH1)) {
			thetaH1 = effectEstimate;
		} else {
			thetaH1 = thetaH1 - thetaH0;
		}
		if (groups == 1) {
		  thetaStandardized = thetaH1 / stDev1H1;
		} else {
		  double allocationRatio = allocationRatioPlanned[k - 1];	  
		  if (!meanRatio) {
		    thetaStandardized = thetaH1 / 
		      sqrt(pow(stDev1H1, 2) * (1.0 + 1 / allocationRatio) + 
		      pow(stDev2H1, 2) * (1.0 + allocationRatio)); 
		  } else {
		    thetaStandardized = thetaH1 /
		      sqrt(pow(stDev1H1, 2) * (1.0 + 1 / allocationRatio) + 
		        pow(stDev2H1, 2) * pow(thetaH0, 2) * (1.0 + allocationRatio)); 
		  }
		}
		
		if (!R_IsNA(directionUpper) && !directionUpper) {
			thetaH1 = -thetaH1;
			thetaStandardized = -thetaStandardized;
		}

		// conditional critical value to reject the null hypotheses at the next stage of the trial
		if (designNumber == 3L) {
			conditionalCriticalValue = getOneMinusQNorm(pow(criticalValue /
					testStatisticValue,
					1.0 / sqrt((double) (informationRates[k - 1] -
							informationRates[k - 2]) / informationRates[0])));
		} else {
			conditionalCriticalValue = (criticalValue *
					sqrt((double) informationRates[k - 1]) -
					testStatisticValue * sqrt((double) informationRates[k - 2])) /
							sqrt((double) informationRates[k - 1] - informationRates[k - 2]);
		}

		if (calcSubjectsFunctionType == 1 && calcSubjectsFunctionR.isNotNull()) {
			stageSubjects = Rf_asReal(
				as<Function>(calcSubjectsFunctionR)(
					_["stage"] = k,
					_["meanRatio"] = meanRatio,
					_["thetaH0"] = thetaH0,
					_["groups"] = groups,
					_["plannedSubjects"] = plannedSubjects,
					_["sampleSizesPerStage"] = sampleSizesPerStage,
					_["allocationRatioPlanned"] = allocationRatioPlanned,
					_["minNumberOfSubjectsPerStage"] = minNumberOfSubjectsPerStage,
					_["maxNumberOfSubjectsPerStage"] = maxNumberOfSubjectsPerStage,
					_["conditionalPower"] = conditionalPower,
					_["thetaH1"] = thetaH1,
					_["stDevH1"] = stDevH1,
					_["conditionalCriticalValue"] = conditionalCriticalValue));
		} else {
			calcSubjectsFunctionMeansPtr fun = *calcSubjectsFunctionCppXPtr;
			stageSubjects = fun(
				k,
				meanRatio,
				thetaH0,
				groups,
				plannedSubjects,
				allocationRatioPlanned,
				minNumberOfSubjectsPerStage,
				maxNumberOfSubjectsPerStage,
				sampleSizesPerStage,
				thetaH1,
				stDevH1,
				conditionalPower,
				conditionalCriticalValue);
		}

		simulatedConditionalPower = getOneMinusPNorm(conditionalCriticalValue -
			thetaStandardized * sqrt(stageSubjects));
	}

	double stDev1 = stDev[0];
	double stDev2 = stDev1; 
	if (stDev.length() > 1) {
		stDev2 = stDev[1];
	}

	if (groups == 1) {
		nz = (alternative - thetaH0) / stDev1 * sqrt(stageSubjects);
		if (normalApproximation) {
			testResult = (2.0 * directionUpper - 1.0) * R::rnorm(nz, 1.0);
		} else {
			testResult = (2.0 * directionUpper - 1.0) *
				getRandomTDistribution(stageSubjects - 1, nz);
		}
	} else {
		double allocationRatio = allocationRatioPlanned[k - 1];
		if (!meanRatio) {
			nz = (alternative - thetaH0) * sqrt(stageSubjects) / 
			  sqrt(pow(stDev1, 2) * (1.0 + 1 / allocationRatio) + 
			  pow(stDev2, 2) * (1.0 + allocationRatio)); 
		} else {
			nz = (alternative - thetaH0) * sqrt(stageSubjects) /
			  sqrt(pow(stDev1, 2) * (1.0 + 1 / allocationRatio) + 
			    pow(stDev2, 2) * pow(thetaH0, 2) * (1.0 + allocationRatio)); 
		}
		if (normalApproximation) {
			testResult = (2.0 * directionUpper - 1.0) * R::rnorm(nz, 1.0);
		} else {
			testResult = (2.0 * directionUpper - 1.0) *
				getRandomTDistribution(stageSubjects - 2, nz);
		}
	}

	if (k > 1) {
		sampleSizesPerStage.push_back(stageSubjects);
		testStatisticsPerStage.push_back(testResult);
	} else {
		sampleSizesPerStage = NumericVector::create(stageSubjects);
		testStatisticsPerStage = NumericVector::create(testResult);
	}
	testStatistic = getTestStatisticsMeans(designNumber, informationRates, groups,
			normalApproximation, meanRatio,
			thetaH0, allocationRatioPlanned, sampleSizesPerStage,
			testStatisticsPerStage);
	testStatisticValue = testStatistic[0]; // value
	effectEstimate = testStatistic[2] * stDev1; // standardizedEffectEstimate

	double simulatedRejections = 0;
	double simulatedFutilityStop = 0;
	bool trialStop = false;
	if (k == kMax) {
		trialStop = true;
	}
	if (designNumber == 3L) {
		if (!R_IsNA(testStatisticValue) && !R_IsNA(criticalValue) && testStatisticValue <= criticalValue) {
			simulatedRejections = 1;
			trialStop = true;
		}
		double testStatisticPValueSeparate = testStatistic[3 + k - 1]; // pValuesSeparate
		if (k < kMax && !R_IsNA(testStatisticPValueSeparate)) {
			double alpha0 = alpha0Vec[k - 1];
			if (!R_IsNA(alpha0) && testStatisticPValueSeparate >= alpha0) {
				simulatedFutilityStop = 1;
				trialStop = true;
			}
		}
	} else {
		if (!R_IsNA(testStatisticValue) && !R_IsNA(criticalValue) && testStatisticValue >= criticalValue) {
			simulatedRejections = 1;
			trialStop = true;
		}
		if (k < kMax && !R_IsNA(testStatisticValue)) {
			double futilityBound = futilityBounds[k - 1];
			if (!R_IsNA(futilityBound) && testStatisticValue <= futilityBound) {
				simulatedFutilityStop = 1;
				trialStop = true;
			}
		}
	}

	if (!R_IsNA(directionUpper) && !directionUpper) {
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

Rcpp::XPtr<calcSubjectsFunctionMeansPtr> getSimulationMeansStageSubjectsXPtr() {
  return Rcpp::XPtr<calcSubjectsFunctionMeansPtr>(new calcSubjectsFunctionMeansPtr(&getSimulationMeansStageSubjects));
}

// [[Rcpp::export(name = ".getSimulationMeansLoopCpp")]]
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
		NumericVector stDev,
		int groups,
		bool normalApproximation,
		NumericVector plannedSubjects,
		bool directionUpper,
		NumericVector allocationRatioPlanned,
		NumericVector minNumberOfSubjectsPerStage,
		NumericVector maxNumberOfSubjectsPerStage,
		double conditionalPower,
		double thetaH1,
		NumericVector stDevH1,
		int calcSubjectsFunctionType,
		Nullable<Function> calcSubjectsFunctionR,
		SEXP calcSubjectsFunctionCpp) {

	Rcpp::XPtr<calcSubjectsFunctionMeansPtr> calcSubjectsFunctionCppXPtr = getSimulationMeansStageSubjectsXPtr();
	if (calcSubjectsFunctionType == 0) {
		calcSubjectsFunctionR = NULL;
	}
	else if (calcSubjectsFunctionType == 2) {
		calcSubjectsFunctionR = NULL;
		calcSubjectsFunctionCppXPtr = Rcpp::XPtr<calcSubjectsFunctionMeansPtr>(calcSubjectsFunctionCpp);
	}

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

	bool trialStop;
	List stepResult;
	double effectEstimate;

	double simulatedSubjectsStep;
	double simulatedRejectionsStep;
	double simulatedFutilityStopStep;
	double simulatedConditionalPowerStep;

	NumericVector testStatistic;
	NumericVector sampleSizesPerStage;
	NumericVector testStatisticsPerStage;

	double index = 1;
	for (int i = 1; i <= alternative.length(); i++) {
		NumericVector simulatedSubjects(kMax);
		NumericVector simulatedRejections(kMax);
		NumericVector simulatedFutilityStop(kMax - 1);
		NumericVector simulatedConditionalPower(kMax);

		for (int j = 1; j <= maxNumberOfIterations; j++) {
			trialStop = false;
			effectEstimate = 0;

			for (int k = 1; k <= kMax; k++) {
				if (trialStop) {
					break;
				}

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
					testStatistic,
					calcSubjectsFunctionType,
					calcSubjectsFunctionR,
					calcSubjectsFunctionCppXPtr);

				trialStop = stepResult["trialStop"];
				sampleSizesPerStage = stepResult["sampleSizesPerStage"];
				testStatisticsPerStage = stepResult["testStatisticsPerStage"];
				testStatistic = stepResult["testStatistic"];
				simulatedSubjectsStep = stepResult["simulatedSubjects"];
				simulatedRejectionsStep = stepResult["simulatedRejections"];
				simulatedFutilityStopStep = stepResult["simulatedFutilityStop"];
				effectEstimate = stepResult["effectEstimate"];
				if (k > 1) {
					simulatedConditionalPowerStep = stepResult["simulatedConditionalPower"];
				} else {
					simulatedConditionalPowerStep = NA_REAL;
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
				dataTestStatistic[index - 1] = testStatistic[0]; // value
				dataTestStatisticsPerStage[index - 1] = testStatisticsPerStage[k - 1];
				dataTrialStop[index - 1] = trialStop;
				dataConditionalPowerAchieved[index - 1] = simulatedConditionalPowerStep;
				dataEffectEstimate[index - 1] = effectEstimate;
				if (designNumber == 3L) {
					dataPValuesSeparate[index - 1] = testStatistic[3 + k - 1]; // pValuesSeparate
				}
				index++;
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
