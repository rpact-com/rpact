#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

#include "f_utilities.h"
#include "f_simulation_base_survival.h"
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

// Log Rank Test Enrichment
//
// Calculates the stratified logrank test statistic for the enrichment survival data set
// and a specified population at given time.
//
// @param gMax Number of groups
// @param survivalDataSet DataFrame with columns: 
//   accrualTime, survivalTime, dropoutTime, treatmentArm, subGroup
// @param time Time point for analysis
// @param subPopulation Subpopulation index
// @param stratifiedAnalysis Whether to use stratified analysis
// @param directionUpper Direction of test
// @param thetaH0 Null hypothesis hazard ratio
//
// [[Rcpp::export(name = ".logRankTestEnrichmentCpp")]]
List logRankTestEnrichment(int gMax,
						   DataFrame survivalDataSet,
						   double time,
						   int subPopulation,
						   bool stratifiedAnalysis,
						   bool directionUpper = true,
						   double thetaH0 = 1.0) {
	CharacterVector subGroups = createSubGroupsFromPopulation(gMax, subPopulation);

	NumericVector accrualTime = survivalDataSet["accrualTime"];
	NumericVector survivalTime = survivalDataSet["survivalTime"];
	NumericVector dropoutTime = survivalDataSet["dropoutTime"];
	IntegerVector treatmentArm = survivalDataSet["treatmentArm"];
	CharacterVector subGroupVec = survivalDataSet["subGroup"];

	double logRank = R_NaReal;
	int events1 = 0;
	int events2 = 0;
	int subjectNumber = 0;

	if (stratifiedAnalysis) {
		double stratifiedNumerator = 0.0;
		double stratifiedDenominator = 0.0;
		int stratifiedEvents1 = 0;
		int stratifiedEvents2 = 0;
		int stratifiedSubjectNumber = 0;
		for (int sg = 0; sg < subGroups.size(); sg++) {
			// Select subjects in this subgroup indexed by sg.
			LogicalVector subGroupMatch = charInSet(subGroupVec, wrap(subGroups[sg]));
			IntegerVector isInSubgroup = which(subGroupMatch);
			if (isInSubgroup.size() == 0) continue;

			// Obtain log-rank test results for the subgroup and add them to
			// overall, i.e. stratified results.
			List subgroupResults = logRankTest(
				accrualTime[isInSubgroup],
				survivalTime[isInSubgroup],
				dropoutTime[isInSubgroup],
				treatmentArm[isInSubgroup],
				time,
				directionUpper,
				thetaH0,
				false
			);
			NumericVector result = subgroupResults["result"];

			stratifiedSubjectNumber += result[1];
			stratifiedEvents1 += result[2];
			stratifiedEvents2 += result[3];			
			stratifiedNumerator += result[4];
			stratifiedDenominator += result[5];			
		}
		if (stratifiedDenominator > 0) {
			logRank = -stratifiedNumerator / sqrt(stratifiedDenominator);
		} else {
			logRank = R_NegInf;
		}
		if (!directionUpper) logRank = -logRank;
		subjectNumber = stratifiedSubjectNumber;
		events1 = stratifiedEvents1;
		events2 = stratifiedEvents2;		
	} else {
		// Select all subjects in any of the subGroups
		LogicalVector subGroupMatch = charInSet(subGroupVec, subGroups);
		IntegerVector isInSubgroup = which(subGroupMatch);
		
		List results = logRankTest(
			accrualTime[isInSubgroup],
			survivalTime[isInSubgroup],
			dropoutTime[isInSubgroup],
			treatmentArm[isInSubgroup],
			time,
			directionUpper,
			thetaH0,
			false
		);
		NumericVector result = results["result"];

		logRank = result[0];
		subjectNumber = result[1];
		events1 = result[2];
		events2 = result[3];		
	}
	return List::create(
		_["logRank"] = logRank,
		_["thetaH0"] = thetaH0,
		_["directionUpper"] = directionUpper,
		_["subjectNumber"] = subjectNumber,
		_["events"] = IntegerVector::create(events1, events2)
	);
}
