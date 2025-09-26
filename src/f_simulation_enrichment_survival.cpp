#include <Rcpp.h>
#include <cmath>

// [[Rcpp::plugins(cpp11)]]

#include "f_utilities.h"
#include "f_simulation_base_survival.h"
#include "f_simulation_survival_utilities.h"
#include "rpact_types.h"

using namespace Rcpp;

// Create Selected Subsets
//
// Creates selected subsets based on selected populations at a stage
//
// @param selectedPopulationsAtStagek Logical vector of selected populations
//
// [[Rcpp::export(name = ".createSelectedSubsetsCpp")]]
LogicalVector createSelectedSubsets(LogicalVector selectedPopulationsAtStagek) {
    int gMax = selectedPopulationsAtStagek.size();
    int vectorSize = static_cast<int>(pow(2, gMax - 1));
    LogicalVector selectedVector(vectorSize, false);

    if (gMax == 1) {
        selectedVector[0] = selectedPopulationsAtStagek[0];
    } else if (gMax == 2) {
        selectedVector[0] = selectedPopulationsAtStagek[0] || selectedPopulationsAtStagek[1];
        selectedVector[1] = selectedPopulationsAtStagek[1];
    } else if (gMax == 3) {
        selectedVector[0] = selectedPopulationsAtStagek[0] || selectedPopulationsAtStagek[2];
        selectedVector[1] = selectedPopulationsAtStagek[1] || selectedPopulationsAtStagek[2];
        selectedVector[2] = selectedPopulationsAtStagek[0] || selectedPopulationsAtStagek[1] || selectedPopulationsAtStagek[2];
        selectedVector[3] = selectedPopulationsAtStagek[2];
    } else if (gMax == 4) {
        selectedVector[0] = selectedPopulationsAtStagek[0] || selectedPopulationsAtStagek[3];
        selectedVector[1] = selectedPopulationsAtStagek[1] || selectedPopulationsAtStagek[3];
        selectedVector[2] = selectedPopulationsAtStagek[2] || selectedPopulationsAtStagek[3];
        selectedVector[3] = selectedPopulationsAtStagek[0] || selectedPopulationsAtStagek[1] || selectedPopulationsAtStagek[3];
        selectedVector[4] = selectedPopulationsAtStagek[0] || selectedPopulationsAtStagek[2] || selectedPopulationsAtStagek[3];
        selectedVector[5] = selectedPopulationsAtStagek[1] || selectedPopulationsAtStagek[2] || selectedPopulationsAtStagek[3];
        selectedVector[6] = selectedPopulationsAtStagek[0] || selectedPopulationsAtStagek[1] || selectedPopulationsAtStagek[2] || selectedPopulationsAtStagek[3];
        selectedVector[7] = selectedPopulationsAtStagek[3];
    }
    return selectedVector;
}

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
// @param subPopulation Subpopulation index (1-based!)
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
			logRank = stratifiedNumerator / sqrt(stratifiedDenominator);
		} else {
			logRank = 0.0;
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

// Get Simulation Survival Enrichment Stage Events
//
// Calculates stage events for specified conditional power
//
// @param stage Stage number
// @param directionUpper Direction of test
// @param conditionalPower Conditional power
// @param conditionalCriticalValue Conditional critical values per stage
// @param plannedEvents Planned events per stage
// @param allocationRatioPlanned Allocation ratio planned per stage
// @param selectedPopulations Whether populations are selected (rows: populations, columns: stages)
// @param thetaH1 Alternative hypothesis hazard ratio
// @param overallEffects Matrix of overall effects (rows: populations, columns: stages)
// @param minNumberOfEventsPerStage Minimum number of events per stage
// @param maxNumberOfEventsPerStage Maximum number of events per stage
//
// [[Rcpp::export(name = ".getSimulationSurvivalEnrichmentStageEventsCpp")]]
double getSimulationSurvivalEnrichmentStageEvents(int stage,
												  bool directionUpper,
												  double conditionalPower,
												  NumericVector conditionalCriticalValue,
												  NumericVector plannedEvents,
												  NumericVector allocationRatioPlanned,
												  LogicalMatrix selectedPopulations,
												  double thetaH1,
												  NumericMatrix overallEffects,
												  NumericVector minNumberOfEventsPerStage,
												  NumericVector maxNumberOfEventsPerStage) {
	stage = stage - 1; // to be consistent with non-enrichment situation
	int gMax = overallEffects.nrow();

	double newEvents;

	if (!R_IsNA(conditionalPower)) {
		// Check if any populations are selected for the next stage
		bool anySelected = false;
		for (int g = 0; g < gMax; g++) {
			if (selectedPopulations(g, stage)) {
				anySelected = true;
				break;
			}
		}

		if (anySelected) {
			double thetaStandardized;
			
			if (R_IsNA(thetaH1)) {
				if (R_IsNA(directionUpper) || directionUpper) {
					// Find minimum of selected effects
					double minEffect = R_PosInf;
					for (int g = 0; g < gMax; g++) {
						if (selectedPopulations(g, stage)) {
							if (overallEffects(g, stage - 1) < minEffect) {
								minEffect = overallEffects(g, stage - 1);
							}
						}
					}
					thetaStandardized = log(std::max(minEffect, 1.0 + 1e-07));
				} else {
					// Find maximum of selected effects
					double maxEffect = R_NegInf;
					for (int g = 0; g < gMax; g++) {
						if (selectedPopulations(g, stage)) {
							if (overallEffects(g, stage - 1) > maxEffect) {
								maxEffect = overallEffects(g, stage - 1);
							}
						}
					}
					thetaStandardized = log(std::min(maxEffect, 1.0 - 1e-07));
				}
			} else {
				double adjustment = (R_IsNA(directionUpper) || directionUpper) ? 1e-07 : -1e-07;
				thetaStandardized = log(std::min(thetaH1, 1.0 + adjustment));
			}

			if (conditionalCriticalValue[stage - 1] > 8.0) {
				newEvents = maxNumberOfEventsPerStage[stage];
			} else {
				double allocRatioSquared = pow(1.0 + allocationRatioPlanned[stage - 1], 2.0);
				double criticalPart = std::max(0.0, conditionalCriticalValue[stage - 1] + R::qnorm(conditionalPower, 0.0, 1.0, 1, 0));
				double numerator = allocRatioSquared * pow(criticalPart, 2.0);
				double allocRatio = allocationRatioPlanned[stage - 1];
				double denominator = pow(allocRatio, 2.0) * pow(thetaStandardized, 2.0);
				newEvents = numerator / denominator;
				newEvents = std::min(std::max(minNumberOfEventsPerStage[stage], newEvents), maxNumberOfEventsPerStage[stage]);
			}
		} else {
			newEvents = 0.0;
		}
	} else {
		newEvents = plannedEvents[stage] - plannedEvents[stage - 1];
	}

	return newEvents;
}

// Get Treatments and Subgroups
// [[Rcpp::export(name = ".getTreatmentsSubgroupsCpp")]]
List getTreatmentsSubgroups(int maxNumberOfSubjects,
							IntegerVector allocationFraction,
							CharacterVector subGroups,
							NumericVector prevalences) {
	IntegerVector twoGroups = IntegerVector::create(1, 2);
	IntegerVector allocatedTreatments = repInt(twoGroups, allocationFraction);
	IntegerVector treatments = rep_len(allocatedTreatments, maxNumberOfSubjects);

	CharacterVector subGroupsVector = sample(subGroups, maxNumberOfSubjects, true, prevalences);
	return List::create(
		_["treatments"] = treatments,
		_["subGroups"] = subGroupsVector
	);
}

// Generate survival and dropout times
List getSurvDropoutTimes(int numberOfSubjects, 
						IntegerVector treatments,
						CharacterVector subGroupVector,
						CharacterVector subGroups,
						NumericVector lambdaControl,
						NumericVector lambdaActive,
						double kappa,
						NumericVector phi) {
	NumericVector survivalTime(numberOfSubjects);
	NumericVector dropoutTime(numberOfSubjects);
	
	for (int i = 0; i < numberOfSubjects; i++) {
		// Find which subgroup this subject belongs to
		int subGroupIndex = firstMatch(subGroups, Rcpp::as<std::string>(subGroupVector[i]));

		// Generate survival time
		double thisLambda = treatments[i] == 1 ? lambdaActive[subGroupIndex] : lambdaControl[subGroupIndex];
		survivalTime[i] = pow(-log(1 - R::runif(0.0, 1.0)), 1.0 / kappa) / thisLambda;
		
		// Generate dropout time
		bool anyPhiPositive = Rcpp::as<bool>(any(phi > 0));		
		if (anyPhiPositive) {
			if (phi[0] > 0) {
				if (treatments[i] == 1) {
					dropoutTime[i] = -log(1 - R::runif(0.0, 1.0)) / phi[0];
				}
			}
			if (phi[1] > 0) {
				if (treatments[i] == 2) {
					dropoutTime[i] = -log(1 - R::runif(0.0, 1.0)) / phi[1];
				}
			}
		} else {
			dropoutTime[i] = NA_REAL;
		}
	}
	return List::create(
		_["survivalTime"] = survivalTime,
		_["dropoutTime"] = dropoutTime
	);
}


 
// Get Simulated Stage Results Survival Enrichment Subjects Based
//
// Calculates stage results for each simulation iteration step
//
// @param design Trial design object
// @param weights Numeric vector of weights per stage, defined by the design
// @param subGroups Character vector of subgroup names
// @param prevalences Numeric vector of prevalences for each subgroup
// @param piControls Numeric vector of control group event rates
// @param kappa Shape parameter for Weibull distribution
// @param phi Dropout rates for treatment groups
// @param eventTime Event time
// @param hazardRatios Numeric vector of hazard ratios
// @param directionUpper Direction of test
// @param stratifiedAnalysis Whether to use stratified analysis
// @param plannedEvents Planned events per stage
// @param recruitmentTimes Recruitment times
// @param allocationFraction Allocation fractions
// @param typeOfSelection Type of population selection
// @param effectMeasure Effect measure type
// @param adaptations Adaptation indicators per stage
// @param epsilonValue Epsilon value for selection
// @param rValue R value for selection
// @param threshold Threshold for selection
// @param minNumberOfEventsPerStage Minimum events per stage
// @param maxNumberOfEventsPerStage Maximum events per stage
// @param conditionalPower Conditional power
// @param thetaH1 Alternative hypothesis hazard ratio
// @param calcEventsFunction Events calculation function
// @param calcEventsFunctionIsUserDefined Whether calc function is user defined
// @param selectPopulationsFunction Population selection function
//
// [[Rcpp::export(name = ".getSimulatedStageResultsSurvivalEnrichmentSubjectsBasedCpp")]]
List getSimulatedStageResultsSurvivalEnrichmentSubjectsBased(
		Environment design,
		NumericVector weights,
		CharacterVector subGroups,
		NumericVector prevalences,
		NumericVector piControls,
		double kappa,
		NumericVector phi,
		double eventTime,
		NumericVector hazardRatios,
		bool directionUpper,
		bool stratifiedAnalysis,
		NumericVector plannedEvents,
		NumericVector recruitmentTimes,
		IntegerVector allocationFraction,
		std::string typeOfSelection,
		std::string effectMeasure,
		LogicalVector adaptations,
		double epsilonValue,
		double rValue,
		double threshold,
		NumericVector minNumberOfEventsPerStage,
		NumericVector maxNumberOfEventsPerStage,
		double conditionalPower,
		double thetaH1,
		Nullable<Function> calcEventsFunction = R_NilValue,
		bool calcEventsFunctionIsUserDefined = false,
		Nullable<Function> selectPopulationsFunction = R_NilValue) {
	
	if (!design.hasAttribute("class")) {
		stop("Design has no class attribute.");
	}
	std::vector<std::string> designClass = design.attr("class");
	bool isDesignFisher = designClass[0] == "TrialDesignFisher";

	int kMax = plannedEvents.size();
	int pMax = hazardRatios.size();
	int gMax = static_cast<int>(log2(hazardRatios.size())) + 1;

	int maxNumberOfSubjects = recruitmentTimes.size();

	// Initialize matrices and vectors
	NumericMatrix populationEventsPerStage(gMax, kMax);
	std::fill(populationEventsPerStage.begin(), populationEventsPerStage.end(), NA_REAL);
	NumericMatrix populationEventsPerStageCumulated(gMax, kMax);
	std::fill(populationEventsPerStageCumulated.begin(), populationEventsPerStageCumulated.end(), NA_REAL);
	NumericMatrix overallEffects(gMax, kMax);
	std::fill(overallEffects.begin(), overallEffects.end(), NA_REAL);
	NumericMatrix testStatistics(gMax, kMax);
	std::fill(testStatistics.begin(), testStatistics.end(), NA_REAL);
	NumericMatrix overallTestStatistics(gMax, kMax);
	std::fill(overallTestStatistics.begin(), overallTestStatistics.end(), NA_REAL);
	NumericMatrix separatePValues(gMax, kMax);
	std::fill(separatePValues.begin(), separatePValues.end(), NA_REAL);
	NumericVector conditionalCriticalValue(kMax - 1);
	std::fill(conditionalCriticalValue.begin(), conditionalCriticalValue.end(), NA_REAL);
	NumericVector conditionalPowerPerStage(kMax);
	std::fill(conditionalPowerPerStage.begin(), conditionalPowerPerStage.end(), NA_REAL);
	LogicalMatrix selectedPopulations(gMax, kMax);
	selectedPopulations(_, 0) = rep(TRUE, gMax); // First stage all populations selected
	LogicalMatrix selectedsubGroupsIndices(pMax, kMax);
	selectedsubGroupsIndices(_, 0) = rep(TRUE, pMax); // First stage all subgroups selected
	NumericVector adjustedPValues(kMax);
	std::fill(adjustedPValues.begin(), adjustedPValues.end(), NA_REAL);
	NumericVector analysisTime(kMax);
	std::fill(analysisTime.begin(), analysisTime.end(), NA_REAL);
	NumericVector numberOfSubjects(kMax);
	std::fill(numberOfSubjects.begin(), numberOfSubjects.end(), NA_REAL);
	LogicalVector eventsNotAchieved(kMax);

	// Generate treatments and random subgroups according to allocation numbers and prevalences
	List tmp = getTreatmentsSubgroups(
		maxNumberOfSubjects,
		allocationFraction,
		subGroups,
		prevalences
	);
	IntegerVector treatments = tmp["treatments"];
	CharacterVector subGroupVector = tmp["subGroups"];

	// Calculate hazards for control and active treatment
	NumericVector lambdaControl = getLambdasByPis(piControls, eventTime, kappa);
	NumericVector lambdaActive = hazardRatios * lambdaControl;

	// Generate random survival and dropout times
	tmp = getSurvDropoutTimes(
		maxNumberOfSubjects,
		treatments,
		subGroupVector,
		subGroups,
		lambdaControl,
		lambdaActive,
		kappa,
		phi
	);
	NumericVector survivalTime = tmp["survivalTime"];
	NumericVector dropoutTime = tmp["dropoutTime"];

	// Combine into a DataFrame
	DataFrame survivalDataSet = DataFrame::create(
		_["accrualTime"] = recruitmentTimes,
		_["subGroup"] = subGroupVector,
		_["treatmentArm"] = treatments,
		_["survivalTime"] = survivalTime,
		_["dropoutTime"] = dropoutTime
	);

	// Main simulation loop for stages
	for (int k = 0; k < kMax; k++) {
		if (k == 0) {
			// First stage
			analysisTime[k] = findObservationTime(
				recruitmentTimes, 
				survivalTime, 
				dropoutTime, 
				plannedEvents[k]
			);						
			if (R_IsNA(analysisTime[k])) {
				eventsNotAchieved[k] = true;
				break;
			} else {
				numberOfSubjects[k] = sum(recruitmentTimes <= analysisTime[k]);
				
				for (int g = 0; g < gMax; g++) {
					if (selectedPopulations(g, k)) {
						List logRank = logRankTestEnrichment(
							gMax, 
							survivalDataSet, 
							analysisTime[k], 
							g + 1, // need 1-based index here!
							stratifiedAnalysis, 
							directionUpper
						);
						testStatistics(g, k) = logRank["logRank"];
						overallTestStatistics(g, k) = testStatistics(g, k);
						IntegerVector events = logRank["events"];
						populationEventsPerStage(g, k) = sum(events);
					}
				}
			}
		} else {
			// Subsequent stages
			LogicalVector selectedAtK = selectedPopulations(_, k);
			selectedsubGroupsIndices(_, k) = createSelectedSubsets(selectedAtK);
			
			bool analysisTimeOK = analysisTime[k - 1] < max(survivalDataSet["accrualTime"]);
			LogicalVector selInBoth = selectedPopulations(_, k) & selectedPopulations(_, k - 1);
			bool allInBoth = Rcpp::as<bool>(all(selInBoth));
			bool diffInSelectedPopulations = !allInBoth;

			if (analysisTimeOK && diffInSelectedPopulations) {
				// Create new survival and dropout times for selected populations
				
				// Calculate adjusted prevalences for selected subgroups
				NumericVector prevInSelected = prevalences[selectedsubGroupsIndices(_, k)];
				NumericVector prevSelected = prevalences / sum(prevInSelected);
				prevSelected[!selectedsubGroupsIndices(_, k)] = 0.0;
						
				// Sample new subgroups for new subjects
				int numberNewSubjects = maxNumberOfSubjects - numberOfSubjects[k - 1];
				CharacterVector newSubGroupVector = sample(
					subGroups,
					numberNewSubjects,
					true,
					prevSelected
				);
				// We keep subGroupVector[seq(0, numberOfSubjects[k - 1] - 1)]
				// and just update the rest
				IntegerVector newSubjectsInds = seq(numberOfSubjects[k - 1], maxNumberOfSubjects - 1);
				subGroupVector[newSubjectsInds] = newSubGroupVector;
				
				tmp = getSurvDropoutTimes(
					numberNewSubjects,
					treatments[newSubjectsInds],
					subGroupVector[newSubjectsInds],
					subGroups,
					lambdaControl,
					lambdaActive,
					kappa,
					phi
				);
				NumericVector newSurvivalTime = tmp["survivalTime"];
				NumericVector newDropoutTime = tmp["dropoutTime"];

				survivalTime[newSubjectsInds] = newSurvivalTime;
				dropoutTime[newSubjectsInds] = newDropoutTime;

				// Update the DataFrame with new values
				survivalDataSet["subGroup"] = subGroupVector;
				survivalDataSet["survivalTime"] = survivalTime;
				survivalDataSet["dropoutTime"] = dropoutTime;
			}

			LogicalVector subGroupNowSelected = selectedsubGroupsIndices(_, k);
			CharacterVector allSubGroups = createSubGroups(gMax);
			CharacterVector selectedsubGroups = allSubGroups[subGroupNowSelected];
			IntegerVector selectedSubjects = which(charInSet(survivalDataSet["subGroup"], selectedsubGroups));
			DataFrame survivalDatasetSelected = getRows(survivalDataSet, selectedSubjects);

			analysisTime[k] = findObservationTime(
				survivalDatasetSelected["accrualTime"], 
				survivalDatasetSelected["survivalTime"], 
				survivalDatasetSelected["dropoutTime"], 
				plannedEvents[k]
			);
			
			if (R_IsNA(analysisTime[k])) {
				eventsNotAchieved[k] = true;
				break;
			} else {
				numberOfSubjects[k] = sum(recruitmentTimes <= analysisTime[k]);
				
				for (int g = 0; g < gMax; g++) {
					if (selectedPopulations(g, k)) {
						List logRank = logRankTestEnrichment(
							gMax, 
							survivalDataSet, 
							analysisTime[k], 
							g + 1, // need 1-based index here!
							stratifiedAnalysis, 
							directionUpper
						);
						overallTestStatistics(g, k) = logRank["logRank"];
						IntegerVector events = logRank["events"];
						populationEventsPerStage(g, k) = events[0] + events[1];
						
						if (populationEventsPerStage(g, k) - populationEventsPerStage(g, k - 1) > 0) {
							double denom = sqrt(populationEventsPerStage(g, k) - populationEventsPerStage(g, k - 1));
							double numerator = sqrt(populationEventsPerStage(g, k)) * overallTestStatistics(g, k) - 
								sqrt(populationEventsPerStage(g, k - 1)) * overallTestStatistics(g, k - 1);
							testStatistics(g, k) = numerator / denom;
						}
					}
				}
			}

			IntegerVector notSelectedPopulations = which(!selectedPopulations(_, k));
			for (int i = 0; i < notSelectedPopulations.size(); i++) {
				int g = notSelectedPopulations[i];
				testStatistics(g, k) = NA_REAL;
				overallEffects(g, k) = NA_REAL;
				overallTestStatistics(g, k) = NA_REAL;
			}
		}

		// Calculate separate p-values
		for (int g = 0; g < gMax; g++) {
			if (!R_IsNA(testStatistics(g, k))) {
				separatePValues(g, k) = 1.0 - R::pnorm(testStatistics(g, k), 0.0, 1.0, 1, 0);
			} else {
				separatePValues(g, k) = NA_REAL;
			}
		}

		// Calculate overall effects
		for (int g = 0; g < gMax; g++) {
			if (!R_IsNA(overallTestStatistics(g, k)) && populationEventsPerStage(g, k) > 0) {
				double direction = (2.0 * directionUpper - 1.0);
				double numerator = direction * overallTestStatistics(g, k) * (1.0 + allocationFraction[0] / allocationFraction[1]);
				double denominator = sqrt(allocationFraction[0] / (1.0 * allocationFraction[1])) * sqrt(populationEventsPerStage(g, k));
				overallEffects(g, k) = exp(numerator / denominator);
			}
		}
		double allocationRatioPlanned = (1.0 * allocationFraction[0]) / allocationFraction[1];

		// Stage-specific calculations for intermediate stages
		if (k < kMax - 1) {
			// Check if any populations selected
			int selectedCount = sum(selectedPopulations(_, k));
			if (selectedCount == 0) {
				break;
			}

			// Bonferroni adjustment
			double minPValue = min(na_omit(separatePValues(_, k)));
			adjustedPValues[k] = std::min(minPValue * selectedCount, 1.0 - 1e-07);

			// Conditional critical value to reject the null hypotheses at the next stage of the trial
			NumericVector criticalValues = design.get("criticalValues");			

			if (isDesignFisher) {
				double numerator = criticalValues[k + 1];
				NumericVector expPvals(k + 1);
				for (int i = 0; i <= k; i++) {
					expPvals[i] = pow(adjustedPValues[i], weights[i]);
				}				
				double denominator = pow(
					// prod(expPvals):
					std::accumulate(expPvals.begin(), expPvals.end(), 1, std::multiplies<double>()), 
					1.0 / weights[k + 1]
				);
				conditionalCriticalValue[k] = getOneMinusQNorm(std::min(numerator / denominator, 1.0 - 1e-07));
			} else {
				NumericVector informationRates = design.get("informationRates");
				double minuend = criticalValues[k + 1] * sqrt(informationRates[k + 1]);
				double subtrahend = 0.0;
				for (int j = 0; j < k; j++) {
					subtrahend += weights[j] * getOneMinusQNorm(adjustedPValues[j]);
				}
				double numerator = minuend - subtrahend;
				double denominator = sqrt(informationRates[k + 1] - informationRates[k]);
				conditionalCriticalValue[k] = numerator / denominator;
			}

			if (adaptations[k]) {
				List selectPopulationsFunctionArgs = List::create(
					_["effectVector"] = R_NilValue,
					_["stage"] = k,
					_["directionUpper"] = directionUpper,
					_["conditionalPower"] = conditionalPower,
					_["conditionalCriticalValue"] = conditionalCriticalValue,
					_["plannedEvents"] = plannedEvents,
					_["allocationRatioPlanned"] = allocationRatioPlanned,
					_["selectedPopulations"] = selectedPopulations,
					_["thetaH1"] = thetaH1,
					_["overallEffects"] = overallEffects
				);

				double thresholdArg = threshold;
				if (effectMeasure == "testStatistic") {
					selectPopulationsFunctionArgs["effectVector"] = overallTestStatistics(_, k);
				} else if (effectMeasure == "effectEstimate") {
					if (R_IsNA(directionUpper) || directionUpper) {
						selectPopulationsFunctionArgs["effectVector"] = overallEffects(_, k);
					} else {
						selectPopulationsFunctionArgs["effectVector"] = 1.0 / overallEffects(_, k);
						thresholdArg = 1.0 / thresholdArg;
					}
				}

				Function selectPopulations(".selectPopulations");
				LogicalVector selectedNow = selectPopulations(
					Named("typeOfSelection") = typeOfSelection,
					Named("epsilonValue") = epsilonValue,
					Named("rValue") = rValue,
					Named("threshold") = thresholdArg,
					Named("selectPopulationsFunction") = selectPopulationsFunction,
					Named("selectPopulationsFunctionArgs") = selectPopulationsFunctionArgs
				);
				selectedPopulations(_, k + 1) = selectedPopulations(_, k) & selectedNow;

				Function calcEventsFunc = calcEventsFunction.get();
				RObject newEvents = calcEventsFunc(
					Named("stage") = k + 2, // need 1-based index here!
					Named("directionUpper") = directionUpper,
					Named("conditionalPower") = conditionalPower,
					Named("conditionalCriticalValue") = conditionalCriticalValue,
					Named("plannedEvents") = plannedEvents,
					Named("allocationRatioPlanned") = allocationRatioPlanned,
					Named("selectedPopulations") = selectedPopulations,
					Named("thetaH1") = thetaH1,
					Named("overallEffects") = overallEffects,
					Named("minNumberOfEventsPerStage") = minNumberOfEventsPerStage,
					Named("maxNumberOfEventsPerStage") = maxNumberOfEventsPerStage
				);

				if (Rf_isNull(newEvents) || Rf_length(newEvents) != 1 || !Rf_isReal(newEvents) || R_IsNA(REAL(newEvents)[0])) {
					stop(
						"'calcEventsFunction' returned an illegal or undefined result; ",
						"the output must be a single numeric value"
					);
				}

				double newEventsValue = REAL(newEvents)[0];
                if (!R_IsNA(conditionalPower) || calcEventsFunctionIsUserDefined) {
					NumericVector newEventsIncrements = cumsum(rep(newEventsValue, kMax - 1 - k));
					plannedEvents[seq(k + 1, kMax - 1)] = plannedEvents[k] + newEventsIncrements;
				}
			} else {
				selectedPopulations(_, k + 1) = selectedPopulations(_, k);
			}

			double thetaStandardized;
			if (R_IsNA(thetaH1)) {
				LogicalVector nowSelected = selectedPopulations(_, k);
				NumericVector effectsThisStage = overallEffects(_, k);
				NumericVector nowEffects = effectsThisStage[nowSelected];
				NumericVector minMaxEffect = applyDirectionOfAlternative(
					nowEffects,
					directionUpper,
					"minMax",
					"planning"
				);
				thetaStandardized = log(minMaxEffect[0]);
			} else {
				thetaStandardized = log(thetaH1);
			}
			thetaStandardized = (2.0 * directionUpper - 1.0) * thetaStandardized;					
			double numerator = thetaStandardized * sqrt(plannedEvents[k + 1] - plannedEvents[k]) *
				sqrt(allocationFraction[0] / (1.0 * allocationFraction[1]));
			double denominator = (1.0 + allocationFraction[0] / allocationFraction[1]);
			double quantile = conditionalCriticalValue[k] - numerator / denominator;
			// pnorm(quantile, lower.tail = FALSE):
			conditionalPowerPerStage[k] = R::pnorm(quantile, 0.0, 1.0, 0, 0);
		}
	}

	return List::create(
		_["eventsNotAchieved"] = eventsNotAchieved,
		_["populationEventsPerStage"] = populationEventsPerStage,
		_["plannedEvents"] = plannedEvents,
		_["analysisTime"] = analysisTime,
		_["numberOfSubjects"] = numberOfSubjects,
		_["testStatistics"] = testStatistics,
		_["overallEffects"] = overallEffects,
		_["overallTestStatistics"] = overallTestStatistics,
		_["separatePValues"] = separatePValues,
		_["conditionalCriticalValue"] = conditionalCriticalValue,
		_["conditionalPowerPerStage"] = conditionalPowerPerStage,
		_["selectedPopulations"] = selectedPopulations
	);
}
