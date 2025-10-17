/**
 *
 * -- Simulation of rates with group sequential and combination test --
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
 */

#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

#include "f_assertions.h"
#include "f_utilities.h"
#include "rpact_types.h"

using namespace Rcpp;

double sign(double x) {
	return x < 0 ? -1 : x == 0 ? 0 : 1;
}

NumericVector getFarringtonManningValuesDiffCpp(double rate1, double rate2, double theta, double allocation) {
	if (theta == 0) {
		double ml1 = (allocation * rate1 + rate2) / (1 + allocation);
		double ml2 = ml1;
		return NumericVector::create(ml1, ml2);
	}

	double a = 1 + 1 / allocation;
	double b = -(1 + 1 / allocation + rate1 + rate2 / allocation + theta * (1 / allocation + 2));
	double c = pow(theta, 2) + theta * (2 * rate1 + 1 / allocation + 1) + rate1 + rate2 / allocation;
	double d = -theta * (1 + theta) * rate1;
	NumericVector v = NumericVector::create(pow(b, 3) / pow(3 * a, 3) - b * c / (6 * pow(a, 2)) + d / (2 * a));
	double u, w;

	if (!R_IsNA((double) v[0]) && (v[0] == 0)) {
		u = sqrt(pow(b, 2) / pow(3 * a, 2) - c / (3 * a));
		w = std::acos(-1) / 2;
	} else {
		u = sign((double) v[0]) * sqrt(pow(b, 2) / pow(3 * a, 2) - c / (3 * a));
		w = 1.0 / 3.0 * (std::acos(-1.0) + std::acos((double) v[0] / pow(u, 3)));
	}

	double ml1 = std::min(std::max(0.0, 2 * u * cos(w) - b / (3 * a)), 1.0);
	double ml2 = std::min(std::max(0.0, ml1 - theta), 1.0);

	return NumericVector::create(ml1, ml2);
}

NumericVector getFarringtonManningValuesRatioCpp(double rate1, double rate2, double theta, double allocation) {
	if (theta == 1) {
		return rep((allocation * rate1 + rate2) / (1 + allocation), 2);
	}

	double a = 1 + 1 / allocation;
	double b = -((1 + rate2 / allocation) * theta + 1 / allocation + rate1);
	double c = (rate1 + rate2 / allocation) * theta;
	double ml1 = (-b - sqrt(pow(b, 2) - 4 * a * c)) / (2 * a);
	double ml2 = ml1 / theta;

	return NumericVector::create(ml1, ml2);
}

List getTestStatisticsRatesCpp(int designNumber, NumericVector informationRates, int groups,
		bool normalApproximation, bool riskRatio, double thetaH0, bool directionUpper, NumericMatrix eventsPerStage,
		NumericMatrix sampleSizesPerStage, Nullable<NumericVector> testStatisticsPerStage_) {

	int stage = sampleSizesPerStage.ncol();
	NumericVector testStatisticsPerStage = NumericVector(stage, NA_REAL);
	if (stage > 1 && testStatisticsPerStage_.isNotNull()) {
		NumericVector testStatisticsPerStageTemp;
		testStatisticsPerStageTemp = testStatisticsPerStage_;
		for (int i = 0; i < stage - 1; i++) {
			testStatisticsPerStage[i] = testStatisticsPerStageTemp[i];
		}
	}

	NumericVector stagewiseRates(groups);
	NumericVector overallRate(groups);
	double value = NA_REAL;
	NumericVector pValuesSeparate = NumericVector(stage, NA_REAL);
	if (groups == 1L) {
		stagewiseRates = eventsPerStage[stage - 1] / sampleSizesPerStage[stage - 1];
		double eventsPerStageSum = 0;
		double sampleSizesPerStageSum = 0;
		for (int i = 0; i < stage; i++) {
			eventsPerStageSum += eventsPerStage[i];
			sampleSizesPerStageSum += sampleSizesPerStage[i];
		}
		overallRate[0] = eventsPerStageSum / sampleSizesPerStageSum;
	} else {
		stagewiseRates = eventsPerStage(_, stage - 1) / sampleSizesPerStage(_, stage - 1);
		if (stage == 1) {
			overallRate = eventsPerStage(_, 0) / sampleSizesPerStage(_, 0);
		} else {
			NumericVector eventsPerStageSums(2);
			NumericVector sampleSizesPerStageSums(2);
			for (int i = 0; i < stage; i++) {
				eventsPerStageSums[0] += eventsPerStage(0, i);
				eventsPerStageSums[1] += eventsPerStage(1, i);
				sampleSizesPerStageSums[0] += sampleSizesPerStage(0, i);
				sampleSizesPerStageSums[1] += sampleSizesPerStage(1, i);
			}
			overallRate = eventsPerStageSums / sampleSizesPerStageSums;
		}
	}

	if (designNumber == 1L) {
		double n1 = sum(sampleSizesPerStage(0, _));
		double e1 = sum(eventsPerStage(0, _));
		double r1 = e1 / n1;
		if (groups == 1L) {
			if (!normalApproximation) {
				if (directionUpper) {
					value = getOneMinusQNorm(R::pbinom(e1 - 1.0, n1, thetaH0, false, false));
				} else {
					value = getOneMinusQNorm(R::pbinom(e1, n1, thetaH0, true, false));
				}
			} else {
				value = (2.0 * directionUpper - 1.0) * (r1 - thetaH0) / sqrt(thetaH0 * (1.0 - thetaH0)) * sqrt(n1);
			}
		} else {
			double n2 = sum(sampleSizesPerStage(1, _));
			double e2 = sum(eventsPerStage(1, _));
			double r2 = e2 / n2;
			if (!normalApproximation) {
				if (directionUpper) {
					value = getOneMinusQNorm(R::phyper(e1 - 1.0, e1 + e2, n1 + n2 - e1 - e2, n1, false, false));
				} else {
					value = getOneMinusQNorm(R::phyper(e1, e1 + e2, n1 + n2 - e1 - e2, n1, true, false));
				}
			} else {
				if (!riskRatio) {
					if (r1 - r2 - thetaH0 == 0) {
						value = 0;
					} else {
						NumericVector fm = getFarringtonManningValuesDiffCpp(r1, r2, thetaH0, n1 / n2);
						value = (r1 - r2 - thetaH0)
							/ sqrt((double) fm[0] * (1.0 - fm[0]) / n1 + fm[1] * (1.0 - fm[1]) / n2);
					}
				} else {
					if (r1 - r2 * thetaH0 == 0) {
						value = 0;
					} else {
						NumericVector fm = getFarringtonManningValuesRatioCpp(r1, r2, thetaH0, n1 / n2);
						value = (r1 - r2 * thetaH0)
							/ sqrt((double) fm[0] * (1.0 - fm[0]) / n1 + pow(thetaH0, 2) * fm[1] * (1.0 - fm[1]) / n2);
					}
				}
				value = (2.0 * directionUpper - 1.0) * value;
			}
		}
	} else {
		if (stage == 1L) {
			double n1 = sampleSizesPerStage(0, 0);
			double e1 = eventsPerStage(0, 0);
			double r1 = e1 / n1;
			if (groups == 1L) {
				if (!normalApproximation) {
					if (directionUpper) {
						testStatisticsPerStage[0] = getOneMinusQNorm(R::pbinom(e1 - 1, n1, thetaH0, false, false));
					} else {
						testStatisticsPerStage[0] = getOneMinusQNorm(R::pbinom(e1, n1, thetaH0, true, false));
					}
				} else {
					testStatisticsPerStage[0] = (2.0 * directionUpper - 1.0) * (r1 - thetaH0)
						/ sqrt(thetaH0 * (1.0 - thetaH0)) * sqrt(n1);
				}
			} else {
				double n2 = sampleSizesPerStage(1, 0);
				double e2 = eventsPerStage(1, 0);
				double r2 = e2 / n2;
				if (!normalApproximation) {
					if (directionUpper) {
						testStatisticsPerStage[0] = getOneMinusQNorm(
							R::phyper(e1 - 1, e1 + e2, n1 + n2 - e1 - e2, n1, false, false));
					} else {
						testStatisticsPerStage[0] = getOneMinusQNorm(
							R::phyper(e1, e1 + e2, n1 + n2 - e1 - e2, n1, true, false));
					}
				} else {
					if (!riskRatio) {
						if (r1 - r2 - thetaH0 == 0) {
							testStatisticsPerStage[0] = 0;
						} else {
							NumericVector fm = getFarringtonManningValuesDiffCpp(r1, r2, thetaH0, n1 / n2);
							testStatisticsPerStage[0] = (2.0 * directionUpper - 1) * (r1 - r2 - thetaH0)
								/ sqrt((double) fm[0] * (1.0 - fm[0]) / n1 + fm[1] * (1.0 - fm[1]) / n2);
						}
					} else {
						if (r1 - r2 * thetaH0 == 0) {
							testStatisticsPerStage[0] = 0;
						} else {
							NumericVector fm = getFarringtonManningValuesRatioCpp(r1, r2, thetaH0, n1 / n2);
							testStatisticsPerStage[0] = (2.0 * directionUpper - 1) * (r1 - r2 * thetaH0)
								/ sqrt(
									(double) fm[0] * (1.0 - fm[0]) / n1 + pow(thetaH0, 2) * fm[1] * (1.0 - fm[1]) / n2);
						}
					}
				}
			}
		} else {
			double n1 = sampleSizesPerStage(0, stage - 1);
			double e1 = eventsPerStage(0, stage - 1);
			double r1 = e1 / n1;
			if (groups == 1L) {
				if (!normalApproximation) {
					if (directionUpper) {
						testStatisticsPerStage[stage - 1] =
							getOneMinusQNorm(R::pbinom(e1 - 1.0, n1, thetaH0, false, false));
					} else {
						testStatisticsPerStage[stage - 1] = getOneMinusQNorm(R::pbinom(e1, n1, thetaH0, true, false));
					}
				} else {
					testStatisticsPerStage[stage - 1] =
						(2.0 * directionUpper - 1.0) * (r1 - thetaH0) / sqrt(thetaH0 * (1.0 - thetaH0)) * sqrt(n1);
				}
			} else {
				double n2 = sampleSizesPerStage(1.0, stage - 1);
				double e2 = eventsPerStage(1.0, stage - 1);
				double r2 = e2 / n2;
				if (!normalApproximation) {
					if (directionUpper) {
						testStatisticsPerStage[stage - 1] =
							getOneMinusQNorm(R::phyper(e1 - 1.0, e1 + e2, n1 + n2 - e1 - e2, n1, false, false));
					} else {
						testStatisticsPerStage[stage - 1] =
							getOneMinusQNorm(R::phyper(e1, e1 + e2, n1 + n2 - e1 - e2, n1, true, false));
					}
				} else {
					if (!riskRatio) {
						if (r1 - r2 - thetaH0 == 0) {
							testStatisticsPerStage[stage - 1] = 0;
						} else {
							NumericVector fm = getFarringtonManningValuesDiffCpp(r1, r2, thetaH0, n1 / n2);
							testStatisticsPerStage[stage - 1] =
								(2.0 * directionUpper - 1.0) * (r1 - r2 - thetaH0)
									/ sqrt((double) fm[0] * (1.0 - fm[0]) / n1 + fm[1] * (1.0 - fm[1]) / n2);
						}
					} else {
						if (r1 - r2 * thetaH0 == 0) {
							testStatisticsPerStage[stage - 1] = 0;
						} else {
							NumericVector fm = getFarringtonManningValuesRatioCpp(r1, r2, thetaH0, n1 / n2);
							testStatisticsPerStage[stage - 1] =
								(2.0 * directionUpper - 1.0) * (r1 - r2 * thetaH0)
									/ sqrt(
										(double) fm[0] * (1.0 - fm[0]) / n1
											+ pow(thetaH0, 2) * fm[1] * (1.0 - fm[1]) / n2);
						}
					}
				}
			}
		}

		if (designNumber == 2L) {
			if (stage == 1) {
				value = testStatisticsPerStage[0];
			} else {
				value = (sqrt((double) informationRates[0]) * testStatisticsPerStage[0]
					+ vectorProduct(
						sqrt(rangeVector(informationRates, 1, stage - 1) - rangeVector(informationRates, 0, stage - 2)),
						rangeVector(testStatisticsPerStage, 1, stage - 1)))
					/ sqrt((double) informationRates[stage - 1]);
			}
		} else if (designNumber == 3L) {
			if (stage == 1) {
				value = getOneMinusPNorm((double) testStatisticsPerStage[0]);
			} else {
				NumericVector weightsFisher(stage);
				weightsFisher[0] = 1;
				for (int i = 1; i < stage; i++) {
					weightsFisher[i] = sqrt((double) informationRates[i] - informationRates[i - 1])
						/ sqrt((double) informationRates[0]);
				}

				value = 1;
				for (int i = 0; i < stage; i++) {
					value *= pow(getOneMinusPNorm((double) testStatisticsPerStage[i]),
						(double) weightsFisher[i]);
				}
			}
		}

		pValuesSeparate = NumericVector(stage, NA_REAL);
		for (int i = 0; i < stage; i++) {
			pValuesSeparate[i] = getOneMinusPNorm((double) testStatisticsPerStage[i]);
		}
	}
	return List::create(
		_["value"] = value,
		_["stagewiseRates"] = stagewiseRates,
		_["overallRate"] = overallRate,
		_["sampleSizesPerStage"] = sampleSizesPerStage,
		_["testStatisticsPerStage"] = testStatisticsPerStage,
		_["pValuesSeparate"] = pValuesSeparate);
}

double getSimulationRatesStageSubjectsCpp(
		int stage,
		bool riskRatio,
		double thetaH0,
		int groups,
		NumericVector plannedSubjects,
		bool directionUpper,
		NumericVector allocationRatioPlanned,
		NumericVector minNumberOfSubjectsPerStage,
		NumericVector maxNumberOfSubjectsPerStage,
		NumericVector sampleSizesPerStage,
		NumericVector conditionalPower,
		NumericVector overallRate,
		double conditionalCriticalValue,
		double farringtonManningValue1,
		double farringtonManningValue2) {

	if (R_IsNA((double) conditionalPower[0])) {
		return plannedSubjects[stage - 1] - plannedSubjects[stage - 2];
	}

	double stageSubjects;
	if (groups == 1L) {
		stageSubjects = pow(
			std::max(0.0,
				conditionalCriticalValue * sqrt(thetaH0 * (1 - thetaH0))
					+ getQNorm((double) conditionalPower[0]) * sqrt((double) overallRate[0] * (1 - overallRate[0]))), 2)
			/ pow(std::max(1e-12, (2 * directionUpper - 1) * ((double) overallRate[0] - thetaH0)), 2);
	} else {
		double allocationRatio = allocationRatioPlanned[stage - 1];
		double mult = 1;
		double corr = thetaH0;
		if (riskRatio) {
			mult = thetaH0;
			corr = 0;
		}
		stageSubjects = (1 + 1 / allocationRatio)
			* pow(
				std::max(0.0,
					conditionalCriticalValue
						* sqrt(
							farringtonManningValue1 * (1 - farringtonManningValue1)
								+ farringtonManningValue2 * (1 - farringtonManningValue2) * allocationRatio
									* pow(mult, 2))
						+ getQNorm((double) conditionalPower[0])
							* sqrt(
								(double) overallRate[0] * (1 - overallRate[0])
									+ overallRate[1] * (1 - overallRate[1]) * allocationRatio * pow(mult, 2))),
				2)
			/ pow(std::max(1e-12, (2 * directionUpper - 1) * ((double) overallRate[0] - mult * overallRate[1] - corr)),
				2);
	}
	stageSubjects = ceil(
		std::min(std::max((double) minNumberOfSubjectsPerStage[stage - 1], stageSubjects),
			(double) maxNumberOfSubjectsPerStage[stage - 1]));
	return stageSubjects;
}

Rcpp::XPtr<calcSubjectsFunctionRatesPtr> getSimulationRatesStageSubjectsXPtrCpp() {
  return(Rcpp::XPtr<calcSubjectsFunctionRatesPtr>(new calcSubjectsFunctionRatesPtr(&getSimulationRatesStageSubjectsCpp)));
}

double getConditionalCriticalValueFisher(
		NumericVector criticalValues,
		List testStatistic,
		NumericVector informationRates,
		int k) {

	assertIsInInterval(k, "k", 1.0, informationRates.length());
	assertIsInInterval(k, "k", 1.0, criticalValues.length());

	double criticalValue = criticalValues[k - 1];
	NumericVector testStatisticValue = testStatistic["value"];
	double value = testStatisticValue[0];

	double informationRateDiff = informationRates[k - 1] - informationRates[k - 2];
	double baseValue = criticalValue / value;
	double powerValue = 1.0 / sqrt(informationRateDiff / informationRates[0]);

    return getOneMinusQNorm(pow(baseValue, powerValue));
}

List getSimulationStepRatesCpp(int k, int kMax, int designNumber, NumericVector informationRates,
		NumericVector futilityBounds, NumericVector alpha0Vec, NumericVector criticalValues, bool riskRatio, double thetaH0,
		double pi1, double pi2, int groups, bool normalApproximation, NumericVector plannedSubjects, bool directionUpper,
		NumericVector allocationRatioPlanned, NumericVector minNumberOfSubjectsPerStage, NumericVector maxNumberOfSubjectsPerStage,
		NumericVector conditionalPower, NumericVector pi1H1, NumericVector pi2H1, NumericMatrix sampleSizesPerStage,
		NumericMatrix eventsPerStage, Nullable<NumericVector> testStatisticsPerStage, List testStatistic,
		int calcSubjectsFunctionType, Nullable<Function> calcSubjectsFunctionR,
		Rcpp::XPtr<calcSubjectsFunctionRatesPtr> calcSubjectsFunctionCpp) {

	NumericVector testStatisticsPerStage_ = NumericVector(0);
	if (testStatisticsPerStage.isNotNull()) {
		testStatisticsPerStage_ = testStatisticsPerStage;
	}

	double stageSubjects = plannedSubjects[0];

	double allocationRatio = NA_REAL;
	if (groups == 2L) {
		allocationRatio = allocationRatioPlanned[k - 1];
	}

	// perform event size recalculation for stages 2, ..., kMax
	double simulatedConditionalPower = 0;

	double conditionalCriticalValue = NA_REAL;
	if (k > 1) {
		double theta = 0;
		NumericVector fm;

		// used effect size is either estimated from test statistic or pre-fixed
		NumericVector overallRate = testStatistic["overallRate"];
		if (!R_IsNA((double) pi1H1[0])) {
			overallRate[0] = pi1H1[0];
		}
		if (groups == 2L && !R_IsNA((double) pi2H1[0])) {
			overallRate[1] = pi2H1[0];
		}

		// conditional critical value to reject the null hypotheses at the next stage of the trial
		if (designNumber == 3L) {
			conditionalCriticalValue = getConditionalCriticalValueFisher(
					criticalValues,
					testStatistic,
					informationRates,
					k);
		} else {
			if (criticalValues[k - 1] >= 6) {
				conditionalCriticalValue = R_PosInf;
			} else {
				conditionalCriticalValue = (criticalValues[k - 1] * sqrt((double) informationRates[k - 1])
					- as<NumericVector>(testStatistic["value"])[0] * sqrt((double) informationRates[k - 2]))
					/ sqrt((double) informationRates[k - 1] - informationRates[k - 2]);
			}
		}

		if (groups == 2L) {
			if (!riskRatio) {
				fm = getFarringtonManningValuesDiffCpp((double) overallRate[0], (double) overallRate[1], thetaH0,
					allocationRatio);
			} else {
				fm = getFarringtonManningValuesRatioCpp((double) overallRate[0], (double) overallRate[1], thetaH0,
					allocationRatio);
			}
		} else {
			fm = rep(0.0, 2);
		}

		if (calcSubjectsFunctionType == 1 && calcSubjectsFunctionR.isNotNull()) {
			stageSubjects = Rf_asReal(
				as<Function>(calcSubjectsFunctionR)(
					_["stage"] = k,
					_["riskRatio"] = riskRatio,
					_["thetaH0"] = thetaH0,
					_["groups"] = groups,
					_["plannedSubjects"] = plannedSubjects,
					_["directionUpper"] = directionUpper,
					_["allocationRatioPlanned"] = allocationRatioPlanned,
					_["minNumberOfSubjectsPerStage"] = minNumberOfSubjectsPerStage,
					_["maxNumberOfSubjectsPerStage"] = maxNumberOfSubjectsPerStage,
					_["sampleSizesPerStage"] = sampleSizesPerStage,
					_["conditionalPower"] = conditionalPower,
					_["overallRate"] = overallRate,
					_["conditionalCriticalValue"] = conditionalCriticalValue,
					_["farringtonManningValue1"] = fm[0],
					_["farringtonManningValue2"] = fm[1]));
		} else {
			calcSubjectsFunctionRatesPtr fun = *calcSubjectsFunctionCpp;
			stageSubjects = fun(k, riskRatio, thetaH0, groups, plannedSubjects,
				directionUpper, allocationRatioPlanned, minNumberOfSubjectsPerStage, maxNumberOfSubjectsPerStage,
				sampleSizesPerStage, conditionalPower, overallRate, conditionalCriticalValue, (double) fm[0],
				(double) fm[1]);
		}

		// calculate conditional power for selected stageSubjects
		if (groups == 1L) {
			if (overallRate[0] * (1.0 - overallRate[0]) == 0) {
				theta = 0;
			} else {
				theta = (overallRate[0] - thetaH0) / sqrt((double) overallRate[0] * (1.0 - overallRate[0]))
					+ sign((double) overallRate[0] - thetaH0) * conditionalCriticalValue
						* (1.0 - sqrt(thetaH0 * (1.0 - thetaH0) / (overallRate[0] * (1.0 - overallRate[0]))))
						/ sqrt(stageSubjects);
			}
		} else {
			if (overallRate[0] * (1.0 - overallRate[0]) + overallRate[1] * (1.0 - overallRate[1]) == 0) {
				theta = 0;
			} else {
				if (!riskRatio) {
					theta = sqrt(allocationRatio) / (1.0 + allocationRatio)
						* ((overallRate[0] - overallRate[1] - thetaH0) * sqrt(1.0 + allocationRatio)
							/ sqrt(
								(double) overallRate[0] * (1.0 - overallRate[0])
									+ allocationRatio * overallRate[1] * (1.0 - overallRate[1]))
							+ sign((double) overallRate[0] - overallRate[1] - thetaH0) * conditionalCriticalValue
								* (1.0
									- sqrt(
										(double) fm[0] * (1.0 - fm[0]) + allocationRatio * fm[1] * (1.0 - fm[1]))
										/ sqrt(
											(double) overallRate[0] * (1.0 - overallRate[0])
												+ allocationRatio * overallRate[1] * (1.0 - overallRate[1])))
								* (1.0 + allocationRatio) / sqrt(allocationRatio * stageSubjects));
				} else {
					theta = sqrt(allocationRatio) / (1.0 + allocationRatio)
						* ((overallRate[0] - thetaH0 * overallRate[1]) * sqrt(1.0 + allocationRatio)
							/ sqrt(
								(double) overallRate[0] * (1.0 - overallRate[0])
									+ allocationRatio * pow(thetaH0, 2) * overallRate[1]
										* (1.0 - overallRate[1]))
							+ sign((double) overallRate[0] - thetaH0 * overallRate[1]) * conditionalCriticalValue
								* (1.0
									- sqrt(
										(double) fm[0] * (1.0 - fm[0])
											+ allocationRatio * pow(thetaH0, 2) * fm[1] * (1.0 - fm[1]))
										/ sqrt(
											(double) overallRate[0] * (1.0 - overallRate[0])
												+ allocationRatio * pow(thetaH0, 2) * overallRate[1]
													* (1.0 - overallRate[1]))) * (1.0 + allocationRatio)
								/ sqrt(allocationRatio * stageSubjects));
				}
			}
		}
		if (!R_IsNA(directionUpper) && !directionUpper) {
			theta = -theta;
		}
		simulatedConditionalPower = getOneMinusPNorm(conditionalCriticalValue - theta * sqrt(stageSubjects));
	}

	// simulate events with achieved sample size
	if (groups == 1L) {
		double n1 = stageSubjects;
		double v1 = R::rbinom(n1, pi1);
		eventsPerStage = cbind(eventsPerStage, NumericVector::create(v1));
		sampleSizesPerStage = cbind(sampleSizesPerStage, NumericVector::create(n1));
	} else {
		int n1 = std::ceil(allocationRatio * stageSubjects / (1.0 + allocationRatio));
		int n2 = stageSubjects - n1;
		double v1 = R::rbinom(n1, pi1);
		double v2 = R::rbinom(n2, pi2);
		NumericVector eventsPerStageCol = NumericVector::create(v1, v2);
		NumericVector sampleSizesPerStageCol = NumericVector::create(n1, n2);
		eventsPerStage = cbind(eventsPerStage, eventsPerStageCol);
		sampleSizesPerStage = cbind(sampleSizesPerStage, sampleSizesPerStageCol);
	}

	testStatistic = getTestStatisticsRatesCpp(designNumber, informationRates, groups, normalApproximation, riskRatio,
		thetaH0, directionUpper, eventsPerStage, sampleSizesPerStage, testStatisticsPerStage_);
	NumericVector testStatisticsPerStageRet = testStatistic["testStatisticsPerStage"];
	double var = testStatisticsPerStageRet[k - 1];
	testStatisticsPerStage_.push_back(var);

	double simulatedRejections = 0;
	double simulatedFutilityStop = 0;
	bool trialStop = k == kMax;
	NumericVector testStatisticValues = testStatistic["value"];
	double testStatisticValue = testStatisticValues[0];
	double criticalValue = NA_REAL;
	if (k - 1 < criticalValues.size()) {
		criticalValue = criticalValues[k - 1];
	}

	if (designNumber == 3L) {
		if (!R_IsNA(testStatisticValue) && !R_IsNA(criticalValue)
				&& testStatisticValue <= criticalValue) {
			simulatedRejections = 1;
			trialStop = true;
		}

		if (k < kMax) {
			NumericVector testStatisticPValuesSeparate = testStatistic["pValuesSeparate"];
			double testStatisticPValueSeparate = NA_REAL;
			if (k - 1 < testStatisticPValuesSeparate.size()) {
				testStatisticPValueSeparate = testStatisticPValuesSeparate[k - 1];
			}

			double alpha0 = NA_REAL;
			if (k - 1 < alpha0Vec.size()) {
				alpha0 = alpha0Vec[k - 1];
			}

			if (!R_IsNA(testStatisticPValueSeparate)
					&& !R_IsNA(alpha0)
					&& testStatisticPValueSeparate >= alpha0) {
				simulatedFutilityStop = 1;
				trialStop = true;
			}
		}

	} else {
		if (!R_IsNA(testStatisticValue) && !R_IsNA(criticalValue)
				&& testStatisticValue >= criticalValue) {
			simulatedRejections = 1;
			trialStop = true;
		}

		if (k < kMax) {
			double futilityBound = NA_REAL;
			if (k - 1 < futilityBounds.size()) {
				futilityBound = futilityBounds[k - 1];
			}
			if (!R_IsNA(testStatisticValue)
					&& !R_IsNA(futilityBound)
					&& testStatisticValue <= futilityBound) {
				simulatedFutilityStop = 1;
				trialStop = true;
			}
		}
	}
  
	return List::create(
		_["trialStop"] = trialStop,
		_["sampleSizesPerStage"] = sampleSizesPerStage,
		_["eventsPerStage"] = eventsPerStage,
		_["testStatisticsPerStage"] = testStatisticsPerStage_,
		_["testStatistic"] = testStatistic,
		_["simulatedSubjects"] = stageSubjects,
		_["simulatedRejections"] = simulatedRejections,
		_["simulatedFutilityStop"] = simulatedFutilityStop,
		_["simulatedConditionalPower"] = simulatedConditionalPower
	);
}

// [[Rcpp::export(name = ".getSimulationRatesCpp")]]
List getSimulationRatesCpp(
		int kMax,
		NumericVector informationRates,
		NumericVector criticalValues,
		NumericVector pi1,
		double pi2,
		int maxNumberOfIterations,
		int designNumber,
		int groups,
		NumericVector futilityBounds,
		NumericVector alpha0Vec,
		NumericVector minNumberOfSubjectsPerStage,
		NumericVector maxNumberOfSubjectsPerStage,
		NumericVector conditionalPower,
		NumericVector pi1H1,
		NumericVector pi2H1,
		bool normalApproximation,
		NumericVector plannedSubjects,
		bool directionUpper,
		NumericVector allocationRatioPlanned,
		bool riskRatio,
		double thetaH0,
		int calcSubjectsFunctionType,
		Nullable<Function> calcSubjectsFunctionR,
		SEXP calcSubjectsFunctionCpp) {

	Rcpp::XPtr<calcSubjectsFunctionRatesPtr> calcSubjectsFunctionCppXPtr = getSimulationRatesStageSubjectsXPtrCpp();
	if (calcSubjectsFunctionType == 0) {
		calcSubjectsFunctionR = NULL;
	}
	else if (calcSubjectsFunctionType == 2) {
		calcSubjectsFunctionR = NULL;
		calcSubjectsFunctionCppXPtr = Rcpp::XPtr<calcSubjectsFunctionRatesPtr>(calcSubjectsFunctionCpp);
	}

	int cols = pi1.length();
	NumericMatrix sampleSizes(kMax, cols);
	NumericMatrix rejectPerStage(kMax, cols);
	NumericVector overallReject(cols);
	NumericMatrix futilityPerStage(kMax - 1, cols);
	NumericVector futilityStop(cols);
	NumericMatrix iterations(kMax, cols);
	NumericVector expectedNumberOfSubjects(cols);
	NumericMatrix conditionalPowerAchieved(kMax, cols);
	conditionalPowerAchieved.fill(NA_REAL);

	int dataLen = cols * maxNumberOfIterations * kMax;
	NumericVector dataIterationNumber = rep(NA_REAL, dataLen);
	NumericVector dataStageNumber = rep(NA_REAL, dataLen);
	NumericVector dataPi1 = rep(NA_REAL, dataLen);
	NumericVector dataPi2 = rep(pi2, dataLen);
	NumericVector dataNumberOfSubjects = rep(NA_REAL, dataLen);
	NumericVector dataNumberOfCumulatedSubjects = rep(NA_REAL, dataLen);
	NumericVector dataRejectPerStage = rep(NA_REAL, dataLen);
	NumericVector dataFutilityPerStage = rep(NA_REAL, dataLen);
	NumericVector dataTestStatistic = rep(NA_REAL, dataLen);
	NumericVector dataTestStatisticsPerStage = rep(NA_REAL, dataLen);
	NumericVector dataOverallRate1 = rep(NA_REAL, dataLen);
	NumericVector dataOverallRate2 = rep(NA_REAL, dataLen);
	NumericVector dataStagewiseRates1 = rep(NA_REAL, dataLen);
	NumericVector dataStagewiseRates2 = rep(NA_REAL, dataLen);
	NumericVector dataSampleSizesPerStage1 = rep(NA_REAL, dataLen);
	NumericVector dataSampleSizesPerStage2 = rep(NA_REAL, dataLen);
	LogicalVector dataTrialStop(dataLen);
	dataTrialStop.fill(NA_LOGICAL);
	NumericVector dataConditionalPowerAchieved = rep(NA_REAL, dataLen);
	NumericVector dataPValuesSeparate;

	if (designNumber != 1L) {
		dataPValuesSeparate = rep(NA_REAL, dataLen);
	}

	NumericVector overallRate;

	int index = 1;
	for (int i = 1; i <= pi1.size(); i++) {
		NumericVector simulatedSubjects(kMax);
		NumericVector simulatedRejections(kMax);
		NumericVector simulatedFutilityStop(kMax - 1);
		double simulatedOverallSubjects = 0;
		NumericVector simulatedConditionalPower(kMax);

		for (int j = 1; j <= maxNumberOfIterations; j++) {
			bool trialStop = false;
			NumericMatrix sampleSizesPerStage(groups, 0);
			NumericMatrix eventsPerStage(groups, 0);
			NumericVector testStatisticsPerStage = NumericVector::create();
			List testStatistic;

			for (int k = 1; k <= kMax; k++) {
				if (!trialStop) {
					List stepResult = getSimulationStepRatesCpp(
						k,
						kMax,
						designNumber,
						informationRates,
						futilityBounds,
						alpha0Vec,
						criticalValues,
						riskRatio,
						thetaH0,
						(double) pi1[i - 1],
						pi2,
						groups,
						normalApproximation,
						plannedSubjects,
						directionUpper,
						allocationRatioPlanned,
						minNumberOfSubjectsPerStage,
						maxNumberOfSubjectsPerStage,
						conditionalPower,
						pi1H1,
						pi2H1,
						sampleSizesPerStage,
						eventsPerStage,
						testStatisticsPerStage,
						testStatistic,
						calcSubjectsFunctionType,
						calcSubjectsFunctionR,
						calcSubjectsFunctionCppXPtr);

					trialStop = stepResult["trialStop"];
					sampleSizesPerStage = as<NumericMatrix>(stepResult["sampleSizesPerStage"]);
					eventsPerStage = as<NumericMatrix>(stepResult["eventsPerStage"]);
					testStatisticsPerStage = as<NumericVector>(stepResult["testStatisticsPerStage"]);
					testStatistic = as<List>(stepResult["testStatistic"]);
					double simulatedSubjectsStep = stepResult["simulatedSubjects"];
					double simulatedRejectionsStep = stepResult["simulatedRejections"];
					double simulatedFutilityStopStep = stepResult["simulatedFutilityStop"];
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
					dataPi1[index - 1] = pi1[i - 1];
					dataNumberOfSubjects[index - 1] = simulatedSubjectsStep;
					dataNumberOfCumulatedSubjects[index - 1] = sum(sampleSizesPerStage);
					dataRejectPerStage[index - 1] = simulatedRejectionsStep;
					dataFutilityPerStage[index - 1] = simulatedFutilityStopStep;
					dataTestStatistic[index - 1] = testStatistic["value"];
					dataTestStatisticsPerStage[index - 1] = testStatisticsPerStage[k - 1];

					//overallRate = testStatistic["overallRate"];
					//dataOverallRate1[index - 1] = overallRate[0];
					dataOverallRate1[index - 1] = as<NumericVector>(testStatistic["overallRate"])[0];

					dataStagewiseRates1[index - 1] = as<NumericVector>(testStatistic["stagewiseRates"])[0];
					dataSampleSizesPerStage1[index - 1] = as<NumericMatrix>(testStatistic["sampleSizesPerStage"])(0,
						k - 1);
					if (as<NumericVector>(testStatistic["stagewiseRates"]).size() > 1) {
						dataOverallRate2[index - 1] = as<NumericVector>(testStatistic["overallRate"])[1];
						dataStagewiseRates2[index - 1] = as<NumericVector>(testStatistic["stagewiseRates"])[1];
						dataSampleSizesPerStage2[index - 1] = as<NumericVector>(testStatistic["sampleSizesPerStage"])(1,
							k - 1);
					} else {
						dataStagewiseRates2[index - 1] = NA_REAL;
						dataOverallRate2[index - 1] = NA_REAL;
						dataSampleSizesPerStage2[index - 1] = NA_REAL;
					}
					dataTrialStop[index - 1] = trialStop;
					dataConditionalPowerAchieved[index - 1] = simulatedConditionalPowerStep;
					if (designNumber != 1L) {
						dataPValuesSeparate[index - 1] = as<NumericVector>(testStatistic["pValuesSeparate"])[k - 1];
					}
					index++;
				}
			}
		}

		simulatedOverallSubjects = sum(simulatedSubjects[Range(0, kMax - 1)]);

		sampleSizes(_, i - 1) = simulatedSubjects / iterations(_, i - 1);
		rejectPerStage(_, i - 1) = simulatedRejections / maxNumberOfIterations;
		overallReject[i - 1] = sum(simulatedRejections / maxNumberOfIterations);
		futilityPerStage(_, i - 1) = simulatedFutilityStop / maxNumberOfIterations;
		futilityStop[i - 1] = sum(simulatedFutilityStop / maxNumberOfIterations);
		expectedNumberOfSubjects[i - 1] = simulatedOverallSubjects / maxNumberOfIterations;
		if (kMax > 1) {
			for (int m = 2; m <= kMax; m++) {
				conditionalPowerAchieved(m - 1, i - 1) = simulatedConditionalPower(m - 1) / iterations(m - 1, i - 1);
			}
		}
	}

	for (int m = 0; m < sampleSizes.length(); m++) {
		if (NumericVector::is_na((double) sampleSizes[m])) {
			sampleSizes[m] = 0;
		}
	}

	NumericVector earlyStop;
	if (kMax > 1) {
		if (pi1.length() == 1) {
			earlyStop = sum(futilityPerStage)
				+ sum(as<NumericVector>(rejectPerStage)[Range(0, kMax - 2)]);
		} else {
			NumericVector rejectPerStageColSum;
			if (kMax > 2) {
				rejectPerStageColSum = colSums(rejectPerStage(Range(0, kMax - 2), _));
			} else {
				rejectPerStageColSum = rejectPerStage(0, _);
			}
			earlyStop = colSums(futilityPerStage) + rejectPerStageColSum;
		}
	} else {
		earlyStop = rep(0, pi1.length());
	}

	DataFrame data = DataFrame::create(
		_["iterationNumber"] = dataIterationNumber,
		_["stageNumber"] = dataStageNumber,
		_["pi1"] = dataPi1,
		_["pi2"] = pi2,
		_["numberOfSubjects"] = dataNumberOfSubjects,
		_["numberOfCumulatedSubjects"] = dataNumberOfCumulatedSubjects,
		_["rejectPerStage"] = dataRejectPerStage,
		_["futilityPerStage"] = dataFutilityPerStage,
		_["testStatistic"] = dataTestStatistic,
		_["testStatisticsPerStage"] = dataTestStatisticsPerStage,
		_["overallRate1"] = dataOverallRate1,
		_["overallRate2"] = dataOverallRate2,
		_["stagewiseRates1"] = dataStagewiseRates1,
		_["stagewiseRates2"] = dataStagewiseRates2,
		_["sampleSizesPerStage1"] = dataSampleSizesPerStage1,
		_["sampleSizesPerStage2"] = dataSampleSizesPerStage2,
		_["trialStop"] = dataTrialStop,
		_["conditionalPowerAchieved"] = round(dataConditionalPowerAchieved, 6));

	if (designNumber != 1L) {
		data["pValue"] = dataPValuesSeparate;
		data = Rcpp::DataFrame(data);
	}

	return List::create(
		_["iterations"] = iterations,
		_["sampleSizes"] = sampleSizes,
		_["rejectPerStage"] = rejectPerStage,
		_["overallReject"] = overallReject,
		_["futilityPerStage"] = futilityPerStage,
		_["futilityStop"] = futilityStop,
		_["earlyStop"] = earlyStop,
		_["expectedNumberOfSubjects"] = expectedNumberOfSubjects,
		_["conditionalPowerAchieved"] = conditionalPowerAchieved,
		_["data"] = data
	);
}
