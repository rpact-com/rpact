/**
 *
 * -- Simulation enrichment utilities --
 *
 * This file is part of the R package rpact:
 * Confirmatory Adaptive Clinical Trial Design and Analysis
 *
 * Author: Gernot Wassmer, PhD, Daniel Sabanes Bove, PhD, and Friedrich Pahlke, PhD
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

LogicalVector selectPopulations(
	NumericVector effectVector,
	std::string typeOfSelection,
	double epsilonValue = NA_REAL,
	int rValue = NA_INTEGER,
	double threshold = NA_REAL);

List performClosedCombinationTestForSimulationEnrichment(
    List stageResults,
    Environment design,
    IntegerMatrix indices,
    std::string intersectionTest,
    std::string successCriterion);
