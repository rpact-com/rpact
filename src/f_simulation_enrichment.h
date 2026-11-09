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

// Original authors: Gernot Wassmer, PhD, Daniel Sabanes Bove, PhD, and Friedrich Pahlke, PhD

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
