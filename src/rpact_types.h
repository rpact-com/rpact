/**
 *
 * -- Type definitions --
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
 * File version: $Revision: 6784 $
 * Last changed: $Date: 2023-01-31 10:11:06 +0100 (Di, 31 Jan 2023) $
 * Last changed by: $Author: pahlke $
 *
 */

#ifndef SRC_RPACT_TYPES_H_
#define SRC_RPACT_TYPES_H_

#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;

typedef double (*calcSubjectsFunctionMeansPtr)(
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
	double stDevH1,
	double conditionalPower,
	double conditionalCriticalValue);

typedef double (*calcSubjectsFunctionRatesPtr)(
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
	double farringtonManningValue2);

typedef double (*calcEventsFunctionSurvivalPtr)(
	int stage,
	double conditionalPower,
	double thetaH0,
	double estimatedTheta,
	NumericVector plannedEvents,
	NumericVector eventsOverStages,
	NumericVector minNumberOfEventsPerStage,
	NumericVector maxNumberOfEventsPerStage,
	double allocationRatioPlanned,
	double conditionalCriticalValue);

#endif /* SRC_RPACT_TYPES_H_ */
