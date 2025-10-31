#include <Rcpp.h>
#include <cmath>

// [[Rcpp::plugins(cpp11)]]

#include "f_utilities.h"
#include "f_simulation_base_survival.h"
#include "f_simulation_survival_utilities.h"
#include "f_simulation_enrichment.h"
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
  if (gMax == 1) {
    return CharacterVector::create("F");
  } else if (gMax == 2) {
		return CharacterVector::create("S", "R");
	} else if (gMax == 3) {
		return CharacterVector::create("S1", "S2", "S12", "R");
	} else if (gMax == 5) {
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
	if (gMax == 1) {
		return CharacterVector::create("F");
	} else if (gMax == 2) {
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
	// This is important to ensure that R's random number generator state is properly managed.
	Rcpp::RNGScope scope; 

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
	// This is important to ensure that R's random number generator state is properly managed.
	Rcpp::RNGScope scope; 

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

// [[Rcpp::export(name = ".updateSubGroupVectorCpp")]]
CharacterVector updateSubGroupVector(int k,
						  int maxNumberOfSubjects,
						  IntegerVector numberOfSubjects,
						  const CharacterVector& subGroupVector,
						  CharacterVector subGroups,
						  NumericVector prevSelected) {
	// This is important to ensure that R's random number generator state is properly managed.
	Rcpp::RNGScope scope; 

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
	CharacterVector result = clone(subGroupVector);
	result[newSubjectsInds] = newSubGroupVector;
	return result;
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
	
	bool isDesignFisher = getClassName(design) == "TrialDesignFisher";

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
	IntegerVector numberOfSubjects(kMax);
	std::fill(numberOfSubjects.begin(), numberOfSubjects.end(), NA_INTEGER);
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
						
				int numberNewSubjects = maxNumberOfSubjects - numberOfSubjects[k - 1];
				IntegerVector newSubjectsInds = seq(numberOfSubjects[k - 1], maxNumberOfSubjects - 1);

				// Sample new subgroups for new subjects
				subGroupVector = updateSubGroupVector(
					k,
					maxNumberOfSubjects,
					numberOfSubjects,
					subGroupVector,
					subGroups,
					prevSelected
				);				
				
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

			DataFrame survivalDatasetSelected = DataFrame::create(
				_["accrualTime"] = as<NumericVector>(survivalDataSet["accrualTime"])[selectedSubjects],
				_["subGroup"] = as<CharacterVector>(survivalDataSet["subGroup"])[selectedSubjects],
				_["treatmentArm"] = as<IntegerVector>(survivalDataSet["treatmentArm"])[selectedSubjects],
				_["survivalTime"] = as<NumericVector>(survivalDataSet["survivalTime"])[selectedSubjects],
				_["dropoutTime"] = as<NumericVector>(survivalDataSet["dropoutTime"])[selectedSubjects]
			);

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
		double allocationRatioPlanned = (1.0 * allocationFraction[0]) / allocationFraction[1];
		for (int g = 0; g < gMax; g++) {
			if (!R_IsNA(overallTestStatistics(g, k)) && populationEventsPerStage(g, k) > 0) {
				double direction = (2.0 * directionUpper - 1.0);
				double numerator = direction * overallTestStatistics(g, k) * (1.0 + allocationRatioPlanned);
				double denominator = sqrt(allocationRatioPlanned) * sqrt(populationEventsPerStage(g, k));
				overallEffects(g, k) = exp(numerator / denominator);
			}
		}	

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
				// prod(expPvals)
				double denominator = std::accumulate(expPvals.begin(), expPvals.end(), 1, std::multiplies<double>());
				double term = pow(numerator / denominator, 1.0 / weights[k + 1]);
				conditionalCriticalValue[k] = getOneMinusQNorm(std::min(term, 1.0 - 1e-07));
			} else {
				NumericVector informationRates = design.get("informationRates");
				double minuend = criticalValues[k + 1] * sqrt(informationRates[k + 1]);
				double subtrahend = 0.0;
				for (int j = 0; j < k;  j++) {
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

// Perform Simulation Enrichment Survival Loop
//
// Performs the main simulation loop for enrichment survival simulation
//
// @param cols Number of effect scenarios  
// @param maxNumberOfIterations Maximum number of iterations per scenario
// @param design Trial design object
// @param weights Numeric vector of weights per stage
// @param effectList List containing effect parameters
// @param kappa Shape parameter for Weibull distribution
// @param phi Dropout rates for treatment groups
// @param eventTime Event time
// @param recruitmentTimes Recruitment times
// @param allocationFraction Allocation fractions
// @param directionUpper Direction of test
// @param stratifiedAnalysis Whether to use stratified analysis
// @param plannedEvents Planned events per stage
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
// @param indices Matrix of indices for intersection hypotheses
// @param intersectionTest String specifying intersection test method
// @param successCriterion String specifying success criterion
// @param gMax Number of populations
// @param kMax Number of stages
//
// [[Rcpp::export(name = ".performSimulationEnrichmentSurvivalLoopCpp")]]
List performSimulationEnrichmentSurvivalLoop(
		int cols,
		int maxNumberOfIterations,
		Environment design,
		NumericVector weights,
		List effectList,
		double kappa,
		NumericVector phi,
		double eventTime,
		NumericVector recruitmentTimes,
		IntegerVector allocationFraction,
		bool directionUpper,
		bool stratifiedAnalysis,
		NumericVector plannedEvents,
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
		Nullable<Function> calcEventsFunction,
		bool calcEventsFunctionIsUserDefined,
		Nullable<Function> selectPopulationsFunction,
		IntegerMatrix indices,
		std::string intersectionTest,
		std::string successCriterion,
		int gMax,
		int kMax) {
	// Initialize simulation result matrices
	IntegerMatrix simulatedNumberEventsNotAchieved(kMax, cols);
	NumericMatrix simulatedAnalysisTime(kMax, cols);
	NumericMatrix simulatedNumberOfSubjects(kMax, cols);
	// This is how an array() is created in Rcpp: Create a NumericVector and set the "dim" attribute.
	NumericVector simulatedSelections(kMax * cols * gMax);
	simulatedSelections.attr("dim") = IntegerVector::create(kMax, cols, gMax);
	NumericVector simulatedRejections(kMax * cols * gMax);
	simulatedRejections.attr("dim") = IntegerVector::create(kMax, cols, gMax);
	NumericMatrix simulatedNumberOfPopulations(kMax, cols);
	NumericVector simulatedPopulationEventsPerStage(kMax * cols * gMax);
	simulatedPopulationEventsPerStage.attr("dim") = IntegerVector::create(kMax, cols, gMax);
	NumericMatrix simulatedNumberOfEvents(kMax, cols);
	NumericMatrix simulatedSuccessStopping(kMax, cols);
	NumericMatrix simulatedFutilityStopping(kMax - 1, cols);
	NumericMatrix simulatedConditionalPower(kMax, cols);
	NumericVector simulatedRejectAtLeastOne(cols);
	NumericVector expectedNumberOfEvents(cols);
	NumericVector expectedNumberOfSubjects(cols);
	NumericVector expectedStudyDuration(cols);
	NumericMatrix iterations(kMax, cols);
	
	// Initialize all these with zeros
	std::fill(simulatedNumberEventsNotAchieved.begin(), simulatedNumberEventsNotAchieved.end(), 0);
	std::fill(simulatedAnalysisTime.begin(), simulatedAnalysisTime.end(), 0.0);
	std::fill(simulatedNumberOfSubjects.begin(), simulatedNumberOfSubjects.end(), 0.0);
	std::fill(simulatedSelections.begin(), simulatedSelections.end(), 0.0);
	std::fill(simulatedRejections.begin(), simulatedRejections.end(), 0.0);
	std::fill(simulatedNumberOfPopulations.begin(), simulatedNumberOfPopulations.end(), 0.0);
	std::fill(simulatedPopulationEventsPerStage.begin(), simulatedPopulationEventsPerStage.end(), 0.0);
	std::fill(simulatedNumberOfEvents.begin(), simulatedNumberOfEvents.end(), 0.0);
	std::fill(simulatedSuccessStopping.begin(), simulatedSuccessStopping.end(), 0.0);
	std::fill(simulatedFutilityStopping.begin(), simulatedFutilityStopping.end(), 0.0);
	std::fill(simulatedConditionalPower.begin(), simulatedConditionalPower.end(), 0.0);
	std::fill(simulatedRejectAtLeastOne.begin(), simulatedRejectAtLeastOne.end(), 0.0);
	std::fill(expectedNumberOfEvents.begin(), expectedNumberOfEvents.end(), 0.0);
	std::fill(expectedNumberOfSubjects.begin(), expectedNumberOfSubjects.end(), 0.0);
	std::fill(expectedStudyDuration.begin(), expectedStudyDuration.end(), 0.0);
	std::fill(iterations.begin(), iterations.end(), 0.0);
	
	// Initialize data collection vectors
	int len = maxNumberOfIterations * kMax * gMax * cols;
	NumericVector dataIterationNumber(len, NA_REAL);
	NumericVector dataStageNumber(len, NA_REAL);
	NumericVector dataArmNumber(len, NA_REAL);
	NumericVector dataAlternative(len, NA_REAL);
	NumericVector dataEffect(len, NA_REAL);
	NumericVector dataAnalysisTime(len, NA_REAL);
	NumericVector dataNumberOfSubjects(len, NA_REAL);
	NumericVector dataNumberOfEvents(len, NA_REAL);
	LogicalVector dataRejectPerStage(len, NA_LOGICAL);
	LogicalVector dataFutilityStop(len, NA_LOGICAL);
	LogicalVector dataSuccessStop(len, NA_LOGICAL);
	NumericVector dataTestStatistics(len, NA_REAL);
	NumericVector dataConditionalCriticalValue(len, NA_REAL);
	NumericVector dataConditionalPowerAchieved(len, NA_REAL);
	NumericVector dataEffectEstimate(len, NA_REAL);
	NumericVector dataPValuesSeparate(len, NA_REAL);
	
	// Extract effect list components
	CharacterVector subGroups = effectList["subGroups"];
	NumericVector prevalences = effectList["prevalences"];
	NumericVector piControls = effectList["piControls"];
	NumericMatrix hazardRatios = effectList["hazardRatios"];
	
	int index = 0;
	
	// Main simulation loop
	for (int i = 0; i < cols; i++) {
		for (int j = 0; j < maxNumberOfIterations; j++) {

			NumericVector hazardRatiosThisScenario = hazardRatios(i, _);
			
			List stageResults = getSimulatedStageResultsSurvivalEnrichmentSubjectsBased(
				design,
				weights,
				subGroups,
				prevalences,
				piControls,
				kappa,
				phi,
				eventTime,
				hazardRatiosThisScenario,
				directionUpper,
				stratifiedAnalysis,
				plannedEvents,
				recruitmentTimes,
				allocationFraction,
				typeOfSelection,
				effectMeasure,
				adaptations,
				epsilonValue,
				rValue,
				threshold,
				minNumberOfEventsPerStage,
				maxNumberOfEventsPerStage,
				conditionalPower,
				thetaH1,
				calcEventsFunction,
				calcEventsFunctionIsUserDefined,
				selectPopulationsFunction
			);
			
			List closedTest = performClosedCombinationTestForSimulationEnrichment(
				stageResults,
				design,
				indices,
				intersectionTest,
				successCriterion
			);
			
			bool rejectAtSomeStage = false;
			LogicalVector rejectedPopulationsBefore(gMax, false);
			
			// Extract stage results components
			LogicalVector eventsNotAchieved = stageResults["eventsNotAchieved"];
			NumericVector analysisTime = stageResults["analysisTime"];
			NumericVector numberOfSubjects = stageResults["numberOfSubjects"];
			NumericMatrix populationEventsPerStage = stageResults["populationEventsPerStage"];
			NumericVector plannedEventsStage = stageResults["plannedEvents"];
			NumericMatrix testStatistics = stageResults["testStatistics"];
			NumericMatrix overallEffects = stageResults["overallEffects"];
			NumericMatrix separatePValues = stageResults["separatePValues"];
			NumericVector conditionalCriticalValue = stageResults["conditionalCriticalValue"];
			NumericVector conditionalPowerPerStage = stageResults["conditionalPowerPerStage"];
			LogicalMatrix selectedPopulations = stageResults["selectedPopulations"];

			// Extract closed test results
			LogicalMatrix rejected = closedTest["rejected"];
			LogicalMatrix selectedPopulationsTest = closedTest["selectedPopulations"];
			LogicalVector successStop = closedTest["successStop"];
			LogicalVector futilityStop = closedTest["futilityStop"];
			NumericMatrix separatePValuesTest = closedTest["separatePValues"];
			
			// Loop over stages
			for (int k = 0; k < kMax; k++) {
				if (eventsNotAchieved[k]) {
					simulatedNumberEventsNotAchieved(k, i)++;
				} else {
					simulatedAnalysisTime(k, i) += analysisTime[k];
					simulatedNumberOfSubjects(k, i) += numberOfSubjects[k];
					
					// Update rejections
					for (int g = 0; g < gMax; g++) {
						bool isRejected = (rejected(g, k) && selectedPopulationsTest(g, k)) || rejectedPopulationsBefore[g];
						simulatedRejections[k + i * kMax + g * kMax * cols] += (isRejected ? 1.0 : 0.0);
						simulatedSelections[k + i * kMax + g * kMax * cols] += (selectedPopulationsTest(g, k) ? 1.0 : 0.0);
						
						if (!R_IsNA(populationEventsPerStage(g, k))) {
							simulatedPopulationEventsPerStage[k + i * kMax + g * kMax * cols] += populationEventsPerStage(g, k);
						}
					}
					
					simulatedNumberOfPopulations(k, i) += sum(selectedPopulationsTest(_, k));
					
					bool successStopComplete = !Rcpp::as<bool>(any(is_na(successStop)));
					if (successStopComplete) {
						simulatedSuccessStopping(k, i) += (successStop[k] ? 1.0 : 0.0);
					}
					
					if ((kMax > 1) && (k < kMax - 1)) {
						bool futilityStopComplete = !Rcpp::as<bool>(any(is_na(futilityStop)));
						if (futilityStopComplete) {
							bool futilityAndNotSuccess = futilityStop[k] && !successStop[k];
							simulatedFutilityStopping(k, i) += (futilityAndNotSuccess ? 1.0 : 0.0);
						}
						if (!successStop[k] && !futilityStop[k]) {
							simulatedConditionalPower(k + 1, i) += conditionalPowerPerStage[k];
						}
					}
					
					iterations(k, i)++;
					simulatedNumberOfEvents(k, i) += plannedEventsStage[k];
					
					// Collect detailed data
					for (int g = 0; g < gMax; g++) {
						dataIterationNumber[index] = j + 1; // Careful: R uses 1-based indexing
						dataStageNumber[index] = k + 1;
						dataArmNumber[index] = g + 1;
						dataAlternative[index] = i + 1;
						dataEffect[index] = hazardRatios(i, g);
						dataAnalysisTime[index] = analysisTime[k];
						dataNumberOfSubjects[index] = numberOfSubjects[k];
						dataNumberOfEvents[index] = populationEventsPerStage(g, k);
						dataRejectPerStage[index] = rejected(g, k);
						dataTestStatistics[index] = testStatistics(g, k);
						dataSuccessStop[index] = successStop[k];
						if (k < kMax - 1) {
							dataFutilityStop[index] = futilityStop[k];
							dataConditionalCriticalValue[index] = conditionalCriticalValue[k];
							if (index + 1 < len) {
								dataConditionalPowerAchieved[index + 1] = conditionalPowerPerStage[k];
							}
						}
						dataEffectEstimate[index] = overallEffects(g, k);
						dataPValuesSeparate[index] = separatePValuesTest(g, k);
						index++;
					}
					
					// Check for rejection at some stage
					if (!rejectAtSomeStage) {
						bool anyRejected = false;
						for (int g = 0; g < gMax; g++) {
							if ((rejected(g, k) && selectedPopulationsTest(g, k)) || rejectedPopulationsBefore[g]) {
								anyRejected = true;
								break;
							}
						}
						if (anyRejected) {
							simulatedRejectAtLeastOne[i]++;
							rejectAtSomeStage = true;
						}
					}
					
					// Check for early stopping
					if ((k < kMax - 1) && (successStop[k] || futilityStop[k])) {
						// Fill in rejections for remaining stages
						for (int futureK = k + 1; futureK < kMax; futureK++) {
							for (int g = 0; g < gMax; g++) {
								bool isRejected = (rejected(g, k) && selectedPopulationsTest(g, k)) || rejectedPopulationsBefore[g];
								simulatedRejections[futureK + i * kMax + g * kMax * cols] += (isRejected ? 1.0 : 0.0);
							}
						}
						break;
					}
					
					// Update rejected populations before
					for (int g = 0; g < gMax; g++) {
						rejectedPopulationsBefore[g] = (rejected(g, k) && selectedPopulationsTest(g, k)) || rejectedPopulationsBefore[g];
					}
				}
			}
		}
		
		// Post-processing for this scenario
		for (int g = 0; g < gMax; g++) {
			for (int k = 0; k < kMax; k++) {
				if (iterations(k, i) > 0) {
					simulatedPopulationEventsPerStage[k + i * kMax + g * kMax * cols] = 
						round(simulatedPopulationEventsPerStage[k + i * kMax + g * kMax * cols] / iterations(k, i) * 10.0) / 10.0;
				}
			}
		}
		
		for (int k = 0; k < kMax; k++) {
			if (iterations(k, i) > 0) {
				simulatedNumberOfEvents(k, i) /= iterations(k, i);
				simulatedNumberOfSubjects(k, i) /= iterations(k, i);
				simulatedAnalysisTime(k, i) /= iterations(k, i);
			}
		}
		
		if (kMax > 1) {
			// Adjust rejections for stage-wise differences
			for (int k = 1; k < kMax; k++) {
				for (int g = 0; g < gMax; g++) {
					simulatedRejections[k + i * kMax + g * kMax * cols] -= 
						simulatedRejections[(k-1) + i * kMax + g * kMax * cols];
				}
			}
			
			// Calculate expected values
			NumericVector stopping(kMax - 1);
			for (int k = 0; k < kMax - 1; k++) {
				stopping[k] = (simulatedSuccessStopping(k, i) + simulatedFutilityStopping(k, i)) / maxNumberOfIterations;
			}
			NumericVector cumStopping = cumsum(stopping);
			
			expectedNumberOfEvents[i] = simulatedNumberOfEvents(0, i);
			expectedNumberOfSubjects[i] = simulatedNumberOfSubjects(0, i);
			expectedStudyDuration[i] = simulatedAnalysisTime(0, i);
			
			for (int k = 1; k < kMax; k++) {
				double continuationProb = (1.0 - cumStopping[k - 1]);
				expectedNumberOfEvents[i] += continuationProb * (simulatedNumberOfEvents(k, i) - simulatedNumberOfEvents(k-1, i));
				expectedNumberOfSubjects[i] += continuationProb * (simulatedNumberOfSubjects(k, i) - simulatedNumberOfSubjects(k-1, i));
				expectedStudyDuration[i] += continuationProb * (simulatedAnalysisTime(k, i) - simulatedAnalysisTime(k-1, i));
			}
		} else {
			expectedNumberOfEvents[i] = simulatedNumberOfEvents(0, i);
			expectedNumberOfSubjects[i] = simulatedNumberOfSubjects(0, i);
			expectedStudyDuration[i] = simulatedAnalysisTime(0, i);
		}
	}

	// Filter out NA rows in effectEstimate
	LogicalVector validRows = !is_na(dataEffectEstimate);

	// Create data frame
	DataFrame filteredData = DataFrame::create(
		_["iterationNumber"] = dataIterationNumber[validRows],
		_["stageNumber"] = dataStageNumber[validRows],
		_["populationNumber"] = dataArmNumber[validRows],
		_["omegaMax"] = dataAlternative[validRows],
		_["effect"] = dataEffect[validRows],
		_["numberOfEvents"] = dataNumberOfEvents[validRows],
		_["analysisTime"] = dataAnalysisTime[validRows],
		_["numberOfSubjects"] = dataNumberOfSubjects[validRows],
		_["effectEstimate"] = dataEffectEstimate[validRows],
		_["testStatistics"] = dataTestStatistics[validRows],
		_["pValue"] = dataPValuesSeparate[validRows],
		_["conditionalCriticalValue"] = dataConditionalCriticalValue[validRows],
		_["conditionalPowerAchieved"] = dataConditionalPowerAchieved[validRows],
		_["rejectPerStage"] = dataRejectPerStage[validRows],
		_["successStop"] = dataSuccessStop[validRows],
		_["futilityPerStage"] = dataFutilityStop[validRows]
	);
	
	return List::create(
		_["simulatedNumberEventsNotAchieved"] = simulatedNumberEventsNotAchieved,
		_["simulatedAnalysisTime"] = simulatedAnalysisTime,
		_["simulatedNumberOfSubjects"] = simulatedNumberOfSubjects,
		_["simulatedSelections"] = simulatedSelections,
		_["simulatedRejections"] = simulatedRejections,
		_["simulatedNumberOfPopulations"] = simulatedNumberOfPopulations,
		_["simulatedPopulationEventsPerStage"] = simulatedPopulationEventsPerStage,
		_["simulatedNumberOfEvents"] = simulatedNumberOfEvents,
		_["simulatedSuccessStopping"] = simulatedSuccessStopping,
		_["simulatedFutilityStopping"] = simulatedFutilityStopping,
		_["simulatedConditionalPower"] = simulatedConditionalPower,
		_["simulatedRejectAtLeastOne"] = simulatedRejectAtLeastOne,
		_["expectedNumberOfEvents"] = expectedNumberOfEvents,
		_["expectedNumberOfSubjects"] = expectedNumberOfSubjects,
		_["expectedStudyDuration"] = expectedStudyDuration,
		_["iterations"] = iterations,
		_["data"] = filteredData
	);
}

