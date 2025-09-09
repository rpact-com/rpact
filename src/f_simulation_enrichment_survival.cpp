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

// Create SubGroups From Population
//
// Returns a character vector of subgroup names depending on gMax and subPopulation
//
// @param gMax The number of groups
// @param subPopulation The subpopulation index
//
// [[Rcpp::export(name = ".createSubGroupsFromPopulationCpp")]]
CharacterVector createSubGroupsFromPopulation(int gMax, int subPopulation) {
	if (gMax == 2) {
		if (subPopulation == 1) {
			return CharacterVector::create("S");
		} else if (subPopulation == 2) {
			return CharacterVector::create("S", "R");
		}
	} else if (gMax == 3) {
		if (subPopulation == 1) {
			return CharacterVector::create("S1", "S12");
		} else if (subPopulation == 2) {
			return CharacterVector::create("S2", "S12");
		} else if (subPopulation == 3) {
			return CharacterVector::create("S1", "S2", "S12", "R");
		}
	} else if (gMax == 4) {
		if (subPopulation == 1) {
			return CharacterVector::create("S1", "S12", "S13", "S123");
		} else if (subPopulation == 2) {
			return CharacterVector::create("S2", "S12", "S23", "S123");
		} else if (subPopulation == 3) {
			return CharacterVector::create("S3", "S13", "S23", "S123");
		} else if (subPopulation == 4) {
			return CharacterVector::create("S1", "S2", "S3", "S12", "S13", "S23", "S123", "R");
		}
	}
	return CharacterVector::create(NA_STRING);
}

