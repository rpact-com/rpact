#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

#include "f_utilities.h"
#include "f_simulation_survival_utilities.h"
#include "rpact_types.h"

using namespace Rcpp;

// Create SubGroups
//
// Returns a character vector of subgroup names depending on gMax
//
// @param gMax The number of groups
//
// [[Rcpp::export(name = ".createSubGroupsCpp")]]
CharacterVector createSubGroups(int gMax) {
	if (gMax == 2) {
		return CharacterVector::create("S", "R");
	} else if (gMax == 3) {
		return CharacterVector::create("S1", "S2", "S12", "R");
	} else if (gMax == 4) {
		return CharacterVector::create("S1", "S2", "S3", "S12", "S13", "S23", "S123", "R");
	}
	return CharacterVector::create(NA_STRING);
}

