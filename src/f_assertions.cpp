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

using namespace Rcpp;

void assertIsInInterval(double x, Rcpp::CharacterVector xName, double lower, double upper,
		bool lowerInclusive, bool upperInclusive) {
	std::string fmt = Rcpp::as<std::string>(xName);
	if (!(x > lower && x < upper) && !lowerInclusive && !upperInclusive) {
		stop("Argument out of bounds: '%d' (%s) is out of bounds (%d; %d)",
			fmt, x, lower, upper);
	} else if (!(x >= lower && x < upper) && lowerInclusive
			&& !upperInclusive) {
		stop("Argument out of bounds: '%d' (%s) is out of bounds [%d; %d)",
			fmt, x, lower, upper);
	} else if (!(x > lower && x <= upper) && !lowerInclusive
			&& upperInclusive) {
		stop("Argument out of bounds: '%d' (%s) is out of bounds (%d; %d]",
			fmt, x, lower, upper);
	} else if (!(x >= lower && x <= upper) && lowerInclusive
			&& upperInclusive) {
		stop("Argument out of bounds: '%d' (%s) is out of bounds [%d; %d]",
			fmt, x, lower, upper);
	}
}

void assertIsInInterval(double x, Rcpp::CharacterVector xName, double lower, double upper) {
	assertIsInInterval(x, xName, lower, upper, true, true);
}



