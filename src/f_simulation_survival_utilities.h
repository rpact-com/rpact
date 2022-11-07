/**
 *
 * -- Simulation survival utilities --
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
 * File version: $Revision: 4248 $
 * Last changed: $Date: 2021-01-22 15:57:53 +0100 (Fri, 22 Jan 2021) $
 * Last changed by: $Author: pahlke $
 *
 */

#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;

#ifndef PKG_RPACT_H2
#define PKG_RPACT_H2

double findObservationTime(
	NumericVector accrualTime,
	NumericVector survivalTime,
	NumericVector dropoutTime,
	double requiredStageEvents);

double getNormalDistribution(double p);

double getNormalQuantile(double p);

double getRandomExponentialDistribution(double rate);

double getRandomSurvivalDistribution(double rate, double kappa);

double getRandomPiecewiseExponentialDistribution(
	NumericVector cdfValues, NumericVector piecewiseLambda,
	NumericVector piecewiseSurvivalTime);

bool isPiecewiseExponentialSurvivalEnabled(NumericVector lambdaVec2);

double getLambdaByPi(double pi, double eventTime, double kappa);

double getPiByLambda(double lambda, double eventTime, double kappa);

double getHazardRatio(double pi1, double pi2, double eventTime, double kappa);

#endif

