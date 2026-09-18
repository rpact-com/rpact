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
 */

#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;

#ifndef PKG_RPACT_H2
#define PKG_RPACT_H2

// Validated once in R, then reused for all patients in a simulation iteration.
// Use the same exponential draws as the R reference implementation.
class PiecewiseSurvivalSampler {
private:
	NumericVector intervalStarts;
	NumericMatrix activeHazards;
	NumericMatrix controlHazards;
public:
	bool enabled;
	explicit PiecewiseSurvivalSampler(Nullable<List> specification) :
		intervalStarts(0), activeHazards(0, 0), controlHazards(0, 0),
		enabled(specification.isNotNull()) {
		if (enabled) {
			List settings(specification.get());
			intervalStarts = as<NumericVector>(settings["intervalStarts"]);
			activeHazards = as<NumericMatrix>(settings["activeHazards"]);
			controlHazards = as<NumericMatrix>(settings["controlHazards"]);
		}
	}
	double draw(int group, bool active) const {
		const NumericMatrix& hazards = active ? activeHazards : controlHazards;
		double time = R::rexp(1.0 / hazards(group, 0));
		for (int interval = 1; interval < intervalStarts.size(); ++interval) {
			if (time < intervalStarts[interval]) {
				break;
			}
			time = intervalStarts[interval] + R::rexp(1.0 / hazards(group, interval));
		}
		return time;
	}
};

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

NumericVector getLambdasByPis(
		NumericVector pis,
		double eventTime,
		double kappa);

double getPiByLambda(double lambda, double eventTime, double kappa);

double getHazardRatio(double pi1, double pi2, double eventTime, double kappa);

#endif
