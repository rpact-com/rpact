
/**
 *
 * -- Group sequential design --
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



