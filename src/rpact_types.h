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
	NumericVector stDevH1,
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
