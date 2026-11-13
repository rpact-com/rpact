// |
// |  rpact: Confirmatory Adaptive Clinical Trial Design and Analysis
// |  This file is part of the rpact R package.
// |
// |  RPACT — Statistical Tools for Drug Development
// |
// |  Authors: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
// |  SPDX-FileCopyrightText: RPACT GmbH, Germany <info@rpact.com>
// |  SPDX-License-Identifier: LGPL-3.0-only
// |
// |  Licensed under the GNU Lesser General Public License, version 3.
// |  License text: https://www.gnu.org/licenses/lgpl-3.0.html
// |
// |  Package documentation: https://www.rpact.org
// |  Resources and customer portal: https://connect.rpact.app
// |  Consulting and services: https://www.rpact.com
// |  Contact: info@rpact.com
// |

#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

#include "f_utilities.h"

using namespace Rcpp;

// [[Rcpp::export(name = ".findObservationTimeCpp")]]
double findObservationTime(
		NumericVector accrualTime,
		NumericVector survivalTime,
		NumericVector dropoutTime,
		double requiredStageEvents) {

	int numberOfSubjects = accrualTime.size();

	double upperBound = 1;
	double numberOfEvents;
	while (true) {
		numberOfEvents = 0;
		for (int i = 0; i < numberOfSubjects; i++) {
			if (accrualTime[i] + survivalTime[i] < upperBound &&
					(R_IsNA((double) dropoutTime[i]) || dropoutTime[i] > survivalTime[i])) {
				numberOfEvents = numberOfEvents + 1;
			}
		}
		upperBound = 2 * upperBound;
		if (numberOfEvents >= requiredStageEvents || upperBound > 1E12) {
			break;
		}
	}

	if (upperBound > 1E12) {
		return NA_REAL;
	}

	double lower = 0;
	double upper = upperBound;
	double time;
	while (true) {
		time = (lower + upper) / 2;
		numberOfEvents = 0;
		for (int i = 0; i < numberOfSubjects; i++) {
			if (accrualTime[i] + survivalTime[i] <= time &&
					(R_IsNA((double) dropoutTime[i]) || dropoutTime[i] > survivalTime[i])) {
				numberOfEvents = numberOfEvents + 1;
			}
		}

		if (numberOfEvents >= requiredStageEvents) {
			upper = time;
		} else {
			lower = time;
		}

		if (upper - lower < 1E-05) {
			break;
		}
	}

	if (numberOfEvents > requiredStageEvents) {
		time -= 1E-05;
	}
	else if (numberOfEvents < requiredStageEvents) {
		time += 1E-05;
	}

	return time;
}

/**
 * ::Rf_pnorm5 identical to R::pnorm
 */
double getNormalDistribution(double p) {
	return R::pnorm(p, 0.0, 1.0, 1, 0); // p, mu, sigma, lt, lg
}

/**
 * ::Rf_qnorm5 identical to R::qnorm
 */
double getNormalQuantile(double p) {
	return R::qnorm(p, 0.0, 1.0, 1, 0); // p, mu, sigma, lt, lg
}

/**
 * ::Rf_rexp identical to
 * R::rexp(rate);
 * Rcpp::rexp(1, rate)[0];
 */
double getRandomExponentialDistribution(double rate) {
	return Rcpp::rexp(1, rate)[0];
}

/**
 * Weibull: (-log(1 - runif(0.0, 1.0)))^(1 / kappa) / rate
 */
double getRandomSurvivalDistribution(double rate, double kappa) {
	return pow(-log(1 - R::runif(0.0, 1.0)), 1 / kappa) / rate;
}

double getRandomPiecewiseExponentialDistribution(
		NumericVector cdfValues,
		NumericVector piecewiseLambda,
		NumericVector piecewiseSurvivalTime) {

	double y;
	NumericVector s;
	double p = R::runif(0.0, 1.0);
	int n = piecewiseSurvivalTime.size();

	if (n == 0) {
		return -log(1 - p) / piecewiseLambda[0];
	}

	for (int i = 0; i < n; i++) {
		if (p <= cdfValues[i]) {
			if (i == 0) {
				return -log(1 - p) / piecewiseLambda[0];
			}

			y = piecewiseLambda[0] * piecewiseSurvivalTime[0];
			if (i > 1) {
				s = vectorSum(piecewiseSurvivalTime[seq(1, i - 1)], -piecewiseSurvivalTime[seq(0, i - 2)]);
				y += vectorProduct(piecewiseLambda[seq(1, i - 1)], s);
			}
			return piecewiseSurvivalTime[i - 1] - (log(1 - p) + y) / piecewiseLambda[i];
		}
	}

	if (n == 1) {
		return piecewiseSurvivalTime[0] - (log(1 - p) + piecewiseLambda[0] *
			piecewiseSurvivalTime[0]) / piecewiseLambda[1];
	}

	s = vectorSum(piecewiseSurvivalTime[seq(1, n - 1)], -piecewiseSurvivalTime[seq(0, n - 2)]);
	y = piecewiseLambda[0] * piecewiseSurvivalTime[0] + vectorProduct(piecewiseLambda[seq(1, n - 1)], s);
	return piecewiseSurvivalTime[n - 1] - (log(1 - p) + y) / piecewiseLambda[n];
}

bool isPiecewiseExponentialSurvivalEnabled(NumericVector lambdaVec2) {
	if (lambdaVec2.size() <= 1) {
		return false;
	}
	for (int i = 0; i < lambdaVec2.size(); i++) {
		if (R_IsNA((double) lambdaVec2[i])) {
			return false;
		}
	}
	return true;
}

double getLambdaByPi(double pi, double eventTime, double kappa) {
	return pow(-log(1 - pi), 1 / kappa) / eventTime;
}

NumericVector getLambdasByPis(
		NumericVector pis,
		double eventTime,
		double kappa) {

	int n = pis.size();
	NumericVector lambdas(n);
	for (int i = 0; i < n; i++) {
		lambdas[i] = getLambdaByPi(pis[i], eventTime, kappa);
	}
	return lambdas;
}

double getPiByLambda(double lambda, double eventTime, double kappa) {
	return 1 - exp(-pow(lambda * eventTime, kappa));
}

double getHazardRatio(double pi1, double pi2, double eventTime, double kappa) {
	return pow(getLambdaByPi(pi1, eventTime, kappa) / getLambdaByPi(pi2, eventTime, kappa), kappa);
}

