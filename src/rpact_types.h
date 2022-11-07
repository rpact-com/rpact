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
 * File version: $Revision: 6621 $
 * Last changed: $Date: 2022-10-20 17:09:27 +0200 (Thu, 20 Oct 2022) $
 * Last changed by: $Author: pahlke $
 *
 */

#ifndef SRC_RPACT_TYPES_H_
#define SRC_RPACT_TYPES_H_

#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;

typedef double (*calcSubjectsFunctionPtr)(int stage, bool riskRatio, double thetaH0, double groups, NumericVector plannedSubjects,
	bool directionUpper, double allocationRatioPlanned, NumericVector minNumberOfSubjectsPerStage,
	NumericVector maxNumberOfSubjectsPerStage, NumericVector sampleSizesPerStage, NumericVector conditionalPower,
	NumericVector overallRate, double conditionalCriticalValue, double farringtonManningValue1,
	double farringtonManningValue2);

#endif /* SRC_RPACT_TYPES_H_ */
