
## |
## |  *Simulation result classes*
## | 
## |  This file is part of the R package rpact: 
## |  Confirmatory Adaptive Clinical Trial Design and Analysis
## | 
## |  Author: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
## |  Licensed under "GNU Lesser General Public License" version 3
## |  License text can be found here: https://www.r-project.org/Licenses/LGPL-3
## | 
## |  RPACT company website: https://www.rpact.com
## |  rpact package website: https://www.rpact.org
## | 
## |  Contact us for information about our services: info@rpact.com
## | 
## |  File version: $Revision: 6276 $
## |  Last changed: $Date: 2022-06-09 14:07:33 +0200 (Thu, 09 Jun 2022) $
## |  Last changed by: $Author: pahlke $
## | 

#' @include f_core_utilities.R
NULL

#'
#' @name SimulationResults_names
#' 
#' @title
#' Names of a Simulation Results Object
#'
#' @description
#' Function to get the names of a \code{\link{SimulationResults}} object.
#' 
#' @param x A \code{\link{SimulationResults}} object created by \code{getSimulationResults[MultiArm/Enrichment][Means/Rates/Survival]}.
#' 
#' @details
#' Returns the names of a simulation results that can be accessed by the user.
#' 
#' @template return_names
#'
#' @export
#' 
#' @keywords internal
#' 
names.SimulationResults <- function(x) {
	namesToShow <- c(".design", ".data", ".rawData")
	if (inherits(x, "SimulationResultsSurvival")) {
		namesToShow <- c(namesToShow, ".piecewiseSurvivalTime", ".accrualTime")
	}
	namesToShow <- c(namesToShow, x$.getVisibleFieldNames())
	return(namesToShow)
}

#' 
#' @name SimulationResults
#' 
#' @title
#' Class for Simulation Results
#' 
#' @description
#' A class for simulation results.
#' 
#' @details 
#' \code{SimulationResults} is the basic class for 
#' \itemize{
#'   \item \code{\link{SimulationResultsMeans}}, 
#'   \item \code{\link{SimulationResultsRates}},  
#'   \item \code{\link{SimulationResultsSurvival}},
#'   \item \code{\link{SimulationResultsMultiArmMeans}}, 
#'   \item \code{\link{SimulationResultsMultiArmRates}}, and  
#'   \item \code{\link{SimulationResultsMultiArmSurvival}}.
#' }
#' 
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_design.R
#' @include f_core_constants.R
#' @include class_time.R
#' @include f_simulation_base_survival.R
#' @include f_simulation_utilities.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#'
SimulationResults <- setRefClass("SimulationResults",
	contains = "ParameterSet",
	fields = list(
		.plotSettings = "PlotSettings",
		.design = "TrialDesign",
		.data = "data.frame",
		.rawData = "data.frame",
		.showStatistics = "logical",
		
		maxNumberOfIterations = "integer",
		seed = "numeric",
		allocationRatioPlanned = "numeric",	
		conditionalPower = "numeric",
		
		iterations = "matrix",
		futilityPerStage = "matrix",
		futilityStop = "numeric"
	),
	methods = list(
		
		initialize = function(design, ..., showStatistics = FALSE) {
			callSuper(.design = design, .showStatistics = showStatistics, ...)
			
			.plotSettings <<- PlotSettings()
			.parameterNames <<- .getParameterNames(design = design, designPlan = .self)
			.parameterFormatFunctions <<- C_PARAMETER_FORMAT_FUNCTIONS
		},
		
		getPlotSettings = function() {
			return(.plotSettings)
		},
		
		setShowStatistics = function(showStatistics) {
			.assertIsSingleLogical(showStatistics, "showStatistics")
			.showStatistics <<- showStatistics
		},
		
		show = function(showType = 1, digits = NA_integer_, showStatistics = FALSE) {
			.show(showType = showType, digits = digits, showStatistics = showStatistics, consoleOutputEnabled = TRUE)
		},
		
		.show = function(showType = 1, digits = NA_integer_, showStatistics = FALSE, consoleOutputEnabled = TRUE) {
			'Method for automatically printing simulation result objects'	
			.resetCat()
			if (showType == 3) {
				.createSummary(.self, digits = digits)$.show(showType = 1, 
					digits = digits, consoleOutputEnabled = consoleOutputEnabled)
			}
			else if (showType == 2) {
				callSuper(showType = showType, digits = digits, consoleOutputEnabled = consoleOutputEnabled)
			} else {
				if (is.null(showStatistics) || length(showStatistics) != 1) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
						"'showStatistics' (", .arrayToString(showStatistics), 
						") must be a single logical or character")
				}
				
				if (!is.character(showStatistics) || showStatistics != "exclusive") {
					.cat(.toString(startWithUpperCase = TRUE), ":\n\n", heading = 1,
						consoleOutputEnabled = consoleOutputEnabled)
					
					.showParametersOfOneGroup(.getDesignParametersToShow(.self), "Design parameters",
						orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
					
					userDefinedParameters <- .getUserDefinedParameters()
					if (inherits(.self, "SimulationResultsSurvival") && 
						.self$.piecewiseSurvivalTime$delayedResponseEnabled) {
						userDefinedParameters <- c(userDefinedParameters, 
							".piecewiseSurvivalTime$delayedResponseEnabled")
					}
					.showParametersOfOneGroup(userDefinedParameters, "User defined parameters",
						orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
					derivedParameters <- .getDerivedParameters()
					if (length(derivedParameters) > 0) {
						.showParametersOfOneGroup(derivedParameters, "Derived from user defined parameters",
							orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
					}
					.showParametersOfOneGroup(.getDefaultParameters(), "Default parameters",
						orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
					.showParametersOfOneGroup(.getGeneratedParameters(), "Results",
						orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
					.showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)
				}
				
				## statistics of simulated data
				if (isTRUE(showStatistics) || .showStatistics || 
						(is.character(showStatistics) && showStatistics == "exclusive")) {
					
					.cat("Simulated data:\n", heading = 2, consoleOutputEnabled = consoleOutputEnabled)
					params <- c()
					if (inherits(.self, "SimulationResultsMeans")) {
						params <- c(
							"effectMeasure",
							"numberOfSubjects",
							"testStatistic")
					}
					else if (inherits(.self, "SimulationResultsRates")) {
						params <- c(
							"effectMeasure",
							"numberOfSubjects",
							"testStatistic")
					}
					else if (inherits(.self, "SimulationResultsSurvival")) {
						params <- c(
							"effectMeasure",
							"analysisTime",
							"numberOfSubjects",
							"eventsPerStage1",
							"eventsPerStage2",
							"eventsPerStage",
                            "testStatistic",
                            "logRankStatistic",
                            "hazardRatioEstimateLR")
                    }
					else if (inherits(.self, "SimulationResultsMultiArmMeans") || 
							inherits(.self, "SimulationResultsMultiArmRates")) {
						params <- c(
							"effectMeasure",
							"subjectsActiveArm",
							"testStatistic",
							"conditionalCriticalValue",
							"rejectPerStage",
							"successStop",
							"futilityPerStage"
						)
					}
					else if (inherits(.self, "SimulationResultsEnrichmentMeans") || 
							inherits(.self, "SimulationResultsEnrichmentRates")) {
						params <- c(
								"effectMeasure",
								"subjectsPopulation",
								"testStatistic",
								"conditionalCriticalValue",
								"rejectPerStage",
								"successStop",
								"futilityPerStage"
						)
					}
					else if (inherits(.self, "SimulationResultsMultiArmSurvival") ||
							inherits(.self, "SimulationResultsEnrichmentSurvival")) {
						params <- c(
							"effectMeasure",
							"numberOfEvents",
							"singleNumberOfEventsPerStage",
							"testStatistic",
							"conditionalCriticalValue",
							"rejectPerStage",
							"successStop",
							"futilityPerStage"
						)
					}
					
				
					if (!is.null(.self[["conditionalPowerAchieved"]]) && 
							!all(is.na(conditionalPowerAchieved)) && 
							any(!is.na(conditionalPowerAchieved)) && 
							any(na.omit(conditionalPowerAchieved) != 0)) {
						params <- c(params, "conditionalPowerAchieved")
					}
					
					stages <- sort(unique(.self$.data$stageNumber))
					
					variedParameterName1 <- .getVariedParameterName(1)
					variedParameterName2 <- .getVariedParameterName(2)
					parameterValues1 <- .getVariedParameterValues(variedParameterName1)
					parameterValues2 <- .getVariedParameterValues(variedParameterName2)
					
					for (parameterName in params) {
						paramCaption <- .parameterNames[[parameterName]]
						if (is.null(paramCaption)) {
							paramCaption <- paste0("%", parameterName, "%")
						}
						
						for (parameterValue1 in parameterValues1) {
							for (parameterValue2 in parameterValues2) {
								for (stage in stages) {
									if (length(parameterValues1) > 1) {
										.catStatisticsLine(stage = stage, 
											parameterName = parameterName, 
											paramCaption = paramCaption,
											parameterValue1 = parameterValue1, 
											variedParameterName1 = variedParameterName1, 
											parameterValue2 = parameterValue2, 
											variedParameterName2 = variedParameterName2, 
											consoleOutputEnabled = consoleOutputEnabled)
									} else {
										.catStatisticsLine(stage = stage,
											parameterName = parameterName, 
											paramCaption = paramCaption,
											parameterValue1 = parameterValue2, 
											variedParameterName1 = variedParameterName2, 
											consoleOutputEnabled = consoleOutputEnabled)
									}
								}
							}
							if (parameterName == "subjectsActiveArm" && variedParameterName2 == "armNumber") {
								parameterName2 <- "subjectsControlArm"
								paramCaption2 <- .parameterNames[[parameterName2]]
								if (is.null(paramCaption2)) {
									paramCaption2 <- paste0("%", parameterName2, "%")
								}
								for (stage in stages) {
									.catStatisticsLine(stage = stage, 
										parameterName = parameterName2, 
										paramCaption = paramCaption2,
										parameterValue1 = parameterValue1, 
										variedParameterName1 = variedParameterName1, 
										parameterValue2 = unique(parameterValues2), 
										variedParameterName2 = variedParameterName2, 
										consoleOutputEnabled = consoleOutputEnabled)
								}
							}
						}
					}
					.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
				}
				
				twoGroupsEnabled <- !inherits(.self, "SimulationResultsMeans")
				multiArmSurvivalEnabled <- inherits(.self, "SimulationResultsMultiArmSurvival")
				enrichmentEnabled <- grepl("SimulationResultsEnrichment", .getClassName(.self))
				
				if (.design$kMax > 1 || twoGroupsEnabled || multiArmSurvivalEnabled) {
					
					.cat("Legend:\n", heading = 2, consoleOutputEnabled = consoleOutputEnabled)
					
					if (multiArmSurvivalEnabled) {
						.cat("  (i): values of treatment arm i compared to control\n", consoleOutputEnabled = consoleOutputEnabled)
						.cat("  {j}: values of treatment arm j\n", consoleOutputEnabled = consoleOutputEnabled)
					}
					else if (enrichmentEnabled) {
						matrixName <- .getSimulationEnrichmentEffectMatrixName(.self)
						if (nrow(.self$effectList[[matrixName]]) > 1) { 
							.cat("  (i): results of situation i\n", consoleOutputEnabled = consoleOutputEnabled)
						}
					}
					else if (twoGroupsEnabled) {
						.cat("  (i): values of treatment arm i\n", consoleOutputEnabled = consoleOutputEnabled)
					}
					if (.design$kMax > 1) {
						.cat("  [k]: values at stage k\n", consoleOutputEnabled = consoleOutputEnabled)
					}
					
					if (enrichmentEnabled) {
						if (length(.self$effectList$subGroups) > 1) {
							.cat(paste0("  S[i]: population i\n"), consoleOutputEnabled = consoleOutputEnabled)
						}
						.cat(paste0("  F: full population\n"), consoleOutputEnabled = consoleOutputEnabled)
						if (length(.self$effectList$subGroups) > 1) {
							.cat(paste0("  R: remaining population\n"), consoleOutputEnabled = consoleOutputEnabled)
						}
					}
					
					.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
				}
			}
		},
		
		.getVariedParameterName = function(number = 1) {
            if (number == 2) {
               if (!inherits(.self, "SimulationResultsMeans") &&
                        !inherits(.self, "SimulationResultsRates") && 
                        !inherits(.self, "SimulationResultsSurvival") &&
                        grepl("MultiArm", .getClassName(.self))) {
                    return("armNumber")
                }
                return(NA_character_)
            }
            
            variedParameterName1 <- NA_character_
            if (inherits(.self, "SimulationResultsMeans")) {
                variedParameterName1 <- "alternative"
            } else if (inherits(.self, "SimulationResultsRates") || inherits(.self, "SimulationResultsSurvival")) {
                variedParameterName1 <- "pi1"
            } else if (grepl("MultiArm", .getClassName(.self))) {
                if (inherits(.self, "SimulationResultsMultiArmMeans")) {
                    variedParameterName1 <- "muMax"
                }
                else if (inherits(.self, "SimulationResultsMultiArmRates")) {
                    variedParameterName1 <- "piMax"
                }
                else if (inherits(.self, "SimulationResultsMultiArmSurvival")) {
                    variedParameterName1 <- "omegaMax"
                }
            }
			return(variedParameterName1)
		},
		
		.getVariedParameterValues = function(variedParameterName) {
			if (is.na(variedParameterName)) {
				return(NA_real_)
			}
			
			parameterValues <- .self$.data[[variedParameterName]]
			if (is.null(parameterValues)) {
				return(NA_real_)
			}
			
			parameterValues <- unique(parameterValues)
			if (length(parameterValues) > 1 && !any(is.na(parameterValues))) {
				parameterValues <- sort(parameterValues)
			}
			return(parameterValues)
		},
		
		.getVariedParameterValueString = function(variedParameterName, parameterValue) {
			if (variedParameterName %in% c("armNumber")) {
				return(paste0(" (", parameterValue[1], ")"))
			} 
			variedParameterName <- sub("Max$", "_max", variedParameterName)
			return(paste0(", ", variedParameterName, " = ", round(parameterValue[1], 4)))
		},
		
		.catStatisticsLine = function(..., stage, parameterName, paramCaption,
				parameterValue1, variedParameterName1, parameterValue2 = NA_real_, 
				variedParameterName2 = NA_character_, consoleOutputEnabled = TRUE) {
			
			if (stage == 1 && parameterName == "conditionalPowerAchieved") {
				return(invisible())
			}
			
			postfix <- ""
			if (!is.na(parameterValue1)) {
				if (!all(is.na(parameterValue2))) {
					postfix <- paste0(postfix, .getVariedParameterValueString(
						variedParameterName1, parameterValue1))
					if (parameterName != "subjectsControlArm") {
						postfix <- paste0(postfix, .getVariedParameterValueString(
							variedParameterName2, parameterValue2))
					}
					paramValue <- .self$.data[[parameterName]][
						.self$.data$stageNumber == stage & 
						.self$.data[[variedParameterName1]] == parameterValue1 & 
						.self$.data[[variedParameterName2]] %in% parameterValue2]
				} else {
					postfix <- paste0(postfix, .getVariedParameterValueString(
						variedParameterName1, parameterValue1))
					paramValue <- .self$.data[[parameterName]][
						.self$.data$stageNumber == stage & 
						.self$.data[[variedParameterName1]] == parameterValue1]
				}
			} else {
				paramValue <- .self$.data[[parameterName]][
					.self$.data$stageNumber == stage]
			}
			if (.design$kMax > 1) {
				postfix <- paste0(postfix, " [", stage, "]")
			}
			
			if (!consoleOutputEnabled) {
				paramCaption <- paste0("*", paramCaption, "*")
			}
			
			variableNameFormatted <- .getFormattedVariableName(name = paramCaption, 
				n = .getNChar(), prefix = "", postfix = postfix)
			
			if (!is.null(paramValue)) {
				paramValue <- stats::na.omit(paramValue)
				if (length(paramValue) > 0 && is.numeric(paramValue)) {
					paramValueFormatted <- paste0("median [range]: ", round(stats::median(paramValue), 3), 
						" [", paste(round(base::range(paramValue), 3), collapse = " - "), "]; ", 
						"mean +/-sd: ", round(base::mean(paramValue), 3), " +/-", round(stats::sd(paramValue), 3))
				} else {
					paramValueFormatted <- "median [range]: NA [NA - NA]; mean +/sd: NA +/-NA"
				}
				output <- paste(variableNameFormatted, paramValueFormatted, "\n")
				if (!grepl(": median \\[range\\]: NA \\[NA - NA\\]", output)) {
					.cat(output, consoleOutputEnabled = consoleOutputEnabled)
				}
			}
		},
		
		.toString = function(startWithUpperCase = FALSE) {
			s <- "simulation of"
			
			if (grepl("MultiArm", .getClassName(.self)) && !is.null(.self[["activeArms"]]) && .self$activeArms > 1) {
				s <- paste(s, "multi-arm")
			}
			
			if (grepl("Enrichment", .getClassName(.self)) && !is.null(.self[["populations"]]) && .self$populations > 1) {
				s <- paste(s, "enrichment")
			}
			
			if (inherits(.self, "SimulationResultsBaseMeans")) {
				s <- paste(s, "means")
			}
			else if (inherits(.self, "SimulationResultsBaseRates")) {
				s <- paste(s, "rates")
			}
			else if (inherits(.self, "SimulationResultsBaseSurvival")) {
				s <- paste(s, "survival data")
			} 
			else {
				s <- paste(s, "results")
			}
			
			if (.design$kMax > 1) {
				if (.isTrialDesignGroupSequential(.design)) {
					s <- paste(s, "(group sequential design)")
				}
				else if (.isTrialDesignInverseNormal(.design)) {
					s <- paste(s, "(inverse normal combination test design)")
				}
				else if (.isTrialDesignFisher(.design)) {
					s <- paste(s, "(Fisher's combination test design)")
				}
				else if (.isTrialDesignConditionalDunnett(.design)) {
					s <- paste(s, "(conditional Dunnett design)")
				}
			} else {
				s <- paste(s, "(fixed sample size design)")
			}
			return(ifelse(startWithUpperCase, .firstCharacterToUpperCase(s), s))
		},
		
		.getParametersToShow = function() {
			parametersToShow <- .getVisibleFieldNames()
			y <- c(
                "eventsPerStage",
                "overallEventsPerStage", 
                "iterations",
				"overallReject",  # base
				"rejectAtLeastOne",
				"rejectPerStage",
				"rejectedArmsPerStage",
				"rejectedPopulationsPerStage"
			)
			if (.design$kMax > 2) {
				y <- c(y, "futilityStop")
			}
			y <- c(y, 
				"futilityPerStage",
				"earlyStop",      # base
				"successPerStage",
				"selectedArms",
				"selectedPopulations", 
				"numberOfActiveArms",
				"numberOfPopulations",
				"expectedNumberOfSubjects",
				"expectedNumberOfEvents",
				"sampleSizes",
				"singleNumberOfEventsPerStage",
				"conditionalPowerAchieved" # base
			)
			parametersToShow <- c(parametersToShow[!(parametersToShow %in% y)], y[y %in% parametersToShow])
			return(parametersToShow)
		},
		
		.isSampleSizeObject = function() {
			return(FALSE)
		},
        
        getRawDataResults = function(maxNumberOfIterations = NA_integer_) {
            return(.getSimulationParametersFromRawData(.self$.data, 
                variantName = .getVariedParameterName(),
                maxNumberOfIterations = maxNumberOfIterations))
        }
	)
)

SimulationResultsBaseMeans <- setRefClass("SimulationResultsBaseMeans",
	contains = "SimulationResults",
	fields = list(
		stDev = "numeric",
		plannedSubjects = "numeric",
		minNumberOfSubjectsPerStage = "numeric",
		maxNumberOfSubjectsPerStage = "numeric",
		thetaH1 = "numeric",
		stDevH1 = "numeric",
		calcSubjectsFunction = "function",
		
		expectedNumberOfSubjects = "numeric"
	),
	methods = list(
		initialize = function(design, ...) {
			callSuper(design = design, ...)
			generatedParams <- c(
				"iterations",
				"expectedNumberOfSubjects",					
				"sampleSizes",
				"overallReject",					
				"rejectPerStage", 
				"futilityPerStage",					
				"earlyStop")
			if (design$kMax > 2) {
				generatedParams <- c(generatedParams, "futilityStop")
			}
			for (generatedParam in generatedParams) {
				.setParameterType(generatedParam, C_PARAM_GENERATED)
			}
		}
	)
)

#' 
#' @name SimulationResultsMeans
#' 
#' @title
#' Class for Simulation Results Means
#' 
#' @description
#' A class for simulation results means.
#' 
#' @details 
#' Use \code{\link{getSimulationMeans}} to create an object of this type.
#' 
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_design.R
#' @include f_core_constants.R
#' @include class_time.R
#' @include f_simulation_base_survival.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#'
SimulationResultsMeans <- setRefClass("SimulationResultsMeans",
	contains = "SimulationResultsBaseMeans",
	fields = list(
		meanRatio = "logical",
		thetaH0 = "numeric", 
		normalApproximation = "logical",
		alternative = "numeric",
		groups = "integer",
		directionUpper = "logical",
		
		thetaH1 = "numeric",
		srDevH1 = "numeric",
		
		effect = "numeric",
		earlyStop = "numeric",
		sampleSizes = "matrix",		
		overallReject = "numeric", # = rejectedArmsPerStage in multi-arm 
		rejectPerStage = "matrix",
		conditionalPowerAchieved = "matrix"
	),
	methods = list(
		initialize = function(design, ...) {
			callSuper(design = design, ...)
		}
	)
)


#' 
#' @name SimulationResultsMultiArmMeans
#' 
#' @title
#' Class for Simulation Results Multi-Arm Means
#' 
#' @description
#' A class for simulation results means in multi-arm designs.
#' 
#' @details 
#' Use \code{\link{getSimulationMultiArmMeans}} to create an object of this type.
#' 
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_design.R
#' @include f_core_constants.R
#' @include class_time.R
#' @include f_simulation_base_survival.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#'
SimulationResultsMultiArmMeans <- setRefClass("SimulationResultsMultiArmMeans",
	contains = "SimulationResultsBaseMeans",
	fields = list(
		activeArms = "integer", 
		effectMatrix = "matrix",
		typeOfShape = "character",
		muMaxVector = "numeric", 
		gED50 = "numeric",
		slope = "numeric",
		intersectionTest = "character", 
		adaptations = "logical",
		typeOfSelection = "character", 
		effectMeasure = "character",
		successCriterion = "character",
		epsilonValue = "numeric", 
		rValue = "numeric",
		threshold = "numeric",		
		selectArmsFunction = "function",
		
		earlyStop = "matrix",
		selectedArms = "array",
		numberOfActiveArms = "matrix",
		rejectAtLeastOne = "numeric",
		rejectedArmsPerStage = "array",
		successPerStage = "matrix",
		sampleSizes = "array",
		conditionalPowerAchieved = "matrix"
	),
	methods = list(
		initialize = function(design, ...) {
			callSuper(design = design, ...)
			
			for (generatedParam in c(
					"rejectAtLeastOne",
					"selectedArms", 
					"numberOfActiveArms",
					"rejectedArmsPerStage", 
					"successPerStage")) {
				.setParameterType(generatedParam, C_PARAM_GENERATED)
			}
		}
	)
)

SimulationResultsBaseRates <- setRefClass("SimulationResultsBaseRates",
	contains = "SimulationResults",
	fields = list(
		directionUpper = "logical",
		plannedSubjects = "numeric", 
		minNumberOfSubjectsPerStage = "numeric",
		maxNumberOfSubjectsPerStage = "numeric",
		calcSubjectsFunction = "function",
		
		expectedNumberOfSubjects = "numeric"
	),
	methods = list(
		initialize = function(design, ...) {
			callSuper(design = design, ...)
			generatedParams <- c(
				"iterations",
				"expectedNumberOfSubjects", 
				"sampleSizes", 
				"overallReject",
				"rejectPerStage",
				"futilityPerStage", 
				"earlyStop")
			if (design$kMax > 2) {
				generatedParams <- c(generatedParams, "futilityStop")
			}
			for (generatedParam in generatedParams) {
				.setParameterType(generatedParam, C_PARAM_GENERATED)
			}
		}
	)
)


#' 
#' @name SimulationResultsRates
#' 
#' @title
#' Class for Simulation Results Rates
#' 
#' @description
#' A class for simulation results rates.
#' 
#' @details 
#' Use \code{\link{getSimulationRates}} to create an object of this type.
#' 
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_design.R
#' @include f_core_constants.R
#' @include class_time.R
#' @include f_simulation_base_survival.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#'
SimulationResultsRates <- setRefClass("SimulationResultsRates",
	contains = "SimulationResultsBaseRates",
	fields = list(
		riskRatio = "logical",
		thetaH0 = "numeric", 
		normalApproximation = "logical",
		pi1 = "numeric",
		pi2 = "numeric",
		groups = "integer",
		directionUpper = "logical",
		
		pi1H1 = "numeric",
		pi2H1 = "numeric",
		
		effect = "numeric",
		earlyStop = "numeric",
		sampleSizes = "matrix", 
		overallReject = "numeric", 
		rejectPerStage = "matrix",
		conditionalPowerAchieved = "matrix" 
	),
	methods = list(
		initialize = function(design, ...) {
			callSuper(design = design, ...)
			generatedParams <- c(
				"effect",					
				"iterations", 
				"sampleSizes",
				"eventsNotAchieved", 
				"expectedNumberOfSubjects", 
				"overallReject",
				"rejectPerStage",
				"futilityPerStage", 
				"earlyStop",
				"analysisTime", 
				"studyDuration")
			if (design$kMax > 2) {
				generatedParams <- c(generatedParams, "futilityStop")
			}
			for (generatedParam in generatedParams) {
				.setParameterType(generatedParam, C_PARAM_GENERATED)
			}
		}
	)
)


#' 
#' @name SimulationResultsMultiArmRates
#' 
#' @title
#' Class for Simulation Results Multi-Arm Rates
#' 
#' @description
#' A class for simulation results rates in multi-arm designs.
#' 
#' @details 
#' Use \code{\link{getSimulationMultiArmRates}} to create an object of this type.
#' 
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_design.R
#' @include f_core_constants.R
#' @include class_time.R
#' @include f_simulation_base_survival.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#'
SimulationResultsMultiArmRates <- setRefClass("SimulationResultsMultiArmRates",
	contains = "SimulationResultsBaseRates",
	fields = list(
		activeArms = "integer", 
		effectMatrix = "matrix", 
		typeOfShape = "character",
		piMaxVector = "numeric", 
		piControl = "numeric", 
		piH1 = "numeric", 
		piControlH1 = "numeric", 
		gED50 = "numeric",
		slope = "numeric",
		intersectionTest = "character", 
		adaptations = "logical",
		typeOfSelection = "character", 
		effectMeasure = "character",
		successCriterion = "character",
		epsilonValue = "numeric", 
		rValue = "numeric",
		threshold = "numeric",		
		selectArmsFunction = "function",
		
		earlyStop = "matrix",
		selectedArms = "array",
		numberOfActiveArms = "matrix",
		rejectAtLeastOne = "numeric",
		rejectedArmsPerStage = "array",
		successPerStage = "matrix",
		sampleSizes = "array",
		conditionalPowerAchieved = "matrix"
	),
	methods = list(
		initialize = function(design, ...) {
			callSuper(design = design, ...)
			
			for (generatedParam in c(
					"rejectAtLeastOne",
					"selectedArms", 
					"numberOfActiveArms", 
					"rejectedArmsPerStage", 
					"successPerStage")) {
				.setParameterType(generatedParam, C_PARAM_GENERATED)
			}
		}
	)
)

SimulationResultsBaseSurvival <- setRefClass("SimulationResultsBaseSurvival",
	contains = "SimulationResults",
	fields = list(
		directionUpper = "logical",
		plannedEvents = "numeric", 
		minNumberOfEventsPerStage = "numeric",
		maxNumberOfEventsPerStage = "numeric", 
		thetaH1 = "numeric",
		calcEventsFunction = "function", 
		expectedNumberOfEvents = "numeric"
	),
	methods = list(
		initialize = function(design, ...) {
			callSuper(design = design, ...)
			generatedParams <- c(
				"iterations",
				"expectedNumberOfEvents",					
				"eventsPerStage", 
				"overallReject",					
				"rejectPerStage", 					
				"futilityPerStage", 
				"earlyStop")
			if (design$kMax > 2) {
				generatedParams <- c(generatedParams, "futilityStop")
			}
			for (generatedParam in generatedParams) {
				.setParameterType(generatedParam, C_PARAM_GENERATED)
			}
		}
	)
)

#' 
#' @name SimulationResultsSurvival
#' 
#' @title
#' Class for Simulation Results Survival
#' 
#' @description
#' A class for simulation results survival.
#' 
#' @details 
#' Use \code{\link{getSimulationSurvival}} to create an object of this type.
#' 
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_design.R
#' @include f_core_constants.R
#' @include class_time.R
#' @include f_simulation_base_survival.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#'
SimulationResultsSurvival <- setRefClass("SimulationResultsSurvival",
	contains = "SimulationResultsBaseSurvival",
	fields = list(
			.piecewiseSurvivalTime = "PiecewiseSurvivalTime",
			.accrualTime = "AccrualTime",
			
			pi1 = "numeric",
			pi2 = "numeric",
			median1 = "numeric", 
			median2 = "numeric", 
			maxNumberOfSubjects = "numeric",
			accrualTime = "numeric", 
			accrualIntensity = "numeric",
			dropoutRate1 = "numeric",
			dropoutRate2 = "numeric",
			dropoutTime = "numeric",
			eventTime = "numeric",
			thetaH0 = "numeric", 
			allocation1 = "numeric",
			allocation2 = "numeric",
			kappa = "numeric",
			piecewiseSurvivalTime = "numeric", 
			lambda1 = "numeric",
			lambda2 = "numeric",
			
			earlyStop = "numeric",
			hazardRatio = "numeric",
			analysisTime = "matrix",
			studyDuration = "numeric",
			eventsNotAchieved = "matrix",
			numberOfSubjects = "matrix", 
			numberOfSubjects1 = "matrix",
			numberOfSubjects2 = "matrix",
			eventsPerStage = "matrix", 
            overallEventsPerStage = "matrix",
            expectedNumberOfSubjects = "numeric", 
			rejectPerStage = "matrix",
			overallReject = "numeric",		
			conditionalPowerAchieved = "matrix"
	),
	methods = list(
		initialize = function(design, ...) {
			callSuper(design = design, ...)
			generatedParams <- c(
				"hazardRatio", 
				"iterations", 
				"eventsPerStage",
				"singleNumberOfEventsPerStage",
				"expectedNumberOfEvents", 
				"eventsNotAchieved", 
				"numberOfSubjects", 
				"expectedNumberOfSubjects", 
				"overallReject",
				"rejectPerStage", 
				"futilityPerStage",					
				"earlyStop",
				"analysisTime", 
				"studyDuration",
				"allocationRatioPlanned")
			if (design$kMax > 2) {
				generatedParams <- c(generatedParams, "futilityStop")
			}
			for (generatedParam in generatedParams) {
				.setParameterType(generatedParam, C_PARAM_GENERATED)
			}
			.setParameterType("numberOfSubjects1", C_PARAM_NOT_APPLICABLE)
			.setParameterType("numberOfSubjects2", C_PARAM_NOT_APPLICABLE)
			.setParameterType("median1", C_PARAM_NOT_APPLICABLE)
			.setParameterType("median2", C_PARAM_NOT_APPLICABLE)
		}
	)
)

#' 
#' @name SimulationResultsMultiArmSurvival
#' 
#' @title
#' Class for Simulation Results Multi-Arm Survival
#' 
#' @description
#' A class for simulation results survival in multi-arm designs.
#' 
#' @details 
#' Use \code{\link{getSimulationMultiArmSurvival}} to create an object of this type.
#' 
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_design.R
#' @include f_core_constants.R
#' @include class_time.R
#' @include f_simulation_base_survival.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#'
SimulationResultsMultiArmSurvival <- setRefClass("SimulationResultsMultiArmSurvival",
	contains = "SimulationResultsBaseSurvival",
	fields = list(
		activeArms = "integer", 
		effectMatrix = "matrix", 
		typeOfShape = "character",
		omegaMaxVector = "numeric", 
		gED50 = "numeric",
		slope = "numeric",
		intersectionTest = "character", 
		adaptations = "logical",
		typeOfSelection = "character", 
		effectMeasure = "character",
		successCriterion = "character",
		epsilonValue = "numeric", 
		rValue = "numeric",
		threshold = "numeric",		
		selectArmsFunction = "function",
		correlationComputation = "character",
		
		earlyStop = "matrix",
		selectedArms = "array",
		numberOfActiveArms = "matrix",
		rejectAtLeastOne = "numeric",
		rejectedArmsPerStage = "array",
		successPerStage = "matrix",
		eventsPerStage = "array",
		singleNumberOfEventsPerStage = "array",
		conditionalPowerAchieved = "matrix"
	),
	methods = list(
		initialize = function(design, ...) {
			callSuper(design = design, ...)
			
			for (generatedParam in c(
					"rejectAtLeastOne",
					"selectedArms", 
					"numberOfActiveArms", 
					"rejectedArmsPerStage", 
					"successPerStage")) {
				.setParameterType(generatedParam, C_PARAM_GENERATED)
			}
		}
	)
)

#' 
#' @name SimulationResultsEnrichmentMeans
#' 
#' @title
#' Class for Simulation Results Enrichment Means
#' 
#' @description
#' A class for simulation results means in enrichment designs.
#' 
#' @details 
#' Use \code{\link{getSimulationEnrichmentMeans}} to create an object of this type.
#' 
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_design.R
#' @include f_core_constants.R
#' @include class_time.R
#' @include f_simulation_base_survival.R
#' @include class_simulation_results.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#'
SimulationResultsEnrichmentMeans <- setRefClass("SimulationResultsEnrichmentMeans",
	contains = "SimulationResultsBaseMeans",
	fields = list(
		populations = "integer", 
		effectList = "list", 
		intersectionTest = "character",
		stratifiedAnalysis = "logical",
		adaptations = "logical",
		typeOfSelection = "character", 
		effectMeasure = "character",
		successCriterion = "character",
		epsilonValue = "numeric", 
		rValue = "numeric",
		threshold = "numeric",		
		selectPopulationsFunction = "function",
		earlyStop = "matrix",
		selectedPopulations = "array",
		numberOfPopulations = "matrix",
		rejectAtLeastOne = "numeric",
		rejectedPopulationsPerStage = "array",
		successPerStage = "matrix",
		sampleSizes = "array",
		conditionalPowerAchieved = "matrix"
	),
	methods = list(
		initialize = function(design, ...) {
			callSuper(design = design, ...)
			
			for (generatedParam in c(
				"rejectAtLeastOne",
				"selectedPopulations", 
				"numberOfPopulations",
				"rejectedPopulationsPerStage", 
				"successPerStage")) {
				.setParameterType(generatedParam, C_PARAM_GENERATED)
			}
		}
	)
)

#' 
#' @name SimulationResultsEnrichmentRates
#' 
#' @title
#' Class for Simulation Results Enrichment Rates
#' 
#' @description
#' A class for simulation results rates in enrichment designs.
#' 
#' @details 
#' Use \code{\link{getSimulationEnrichmentRates}} to create an object of this type.
#' 
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_design.R
#' @include f_core_constants.R
#' @include class_time.R
#' @include f_simulation_base_survival.R
#' @include class_simulation_results.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#'
SimulationResultsEnrichmentRates <- setRefClass("SimulationResultsEnrichmentRates",
	contains = "SimulationResultsBaseRates",
	fields = list(
		populations = "integer", 
		effectList = "list", 
		intersectionTest = "character",
		stratifiedAnalysis = "logical",
		adaptations = "logical",
		piTreatmentH1 = "numeric", 
		piControlH1 = "numeric", 
		typeOfSelection = "character", 
		effectMeasure = "character",
		successCriterion = "character",
		epsilonValue = "numeric", 
		rValue = "numeric",
		threshold = "numeric",		
		selectPopulationsFunction = "function",
		earlyStop = "matrix",
		selectedPopulations = "array",
		numberOfPopulations = "matrix",
		rejectAtLeastOne = "numeric",
		rejectedPopulationsPerStage = "array",
		successPerStage = "matrix",
		sampleSizes = "array",
		conditionalPowerAchieved = "matrix"
	),
	methods = list(
		initialize = function(design, ...) {
			callSuper(design = design, ...)
			
			for (generatedParam in c(
				"rejectAtLeastOne",
				"selectedPopulations", 
				"numberOfPopulations",
				"rejectedPopulationsPerStage", 
				"successPerStage")) {
				.setParameterType(generatedParam, C_PARAM_GENERATED)
			}
		}
	)
)

#' 
#' @name SimulationResultsEnrichmentSurvival
#' 
#' @title
#' Class for Simulation Results Enrichment Survival
#' 
#' @description
#' A class for simulation results survival in enrichment designs.
#' 
#' @details 
#' Use \code{\link{getSimulationEnrichmentSurvival}} to create an object of this type.
#' 
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_design.R
#' @include f_core_constants.R
#' @include class_time.R
#' @include f_simulation_base_survival.R
#' @include class_simulation_results.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#'
SimulationResultsEnrichmentSurvival <- setRefClass("SimulationResultsEnrichmentSurvival",
	contains = "SimulationResultsBaseSurvival",
	fields = list(
		populations = "integer", 
		effectList = "list", 
		intersectionTest = "character",
		stratifiedAnalysis = "logical",
		adaptations = "logical",
		typeOfSelection = "character", 
		effectMeasure = "character",
		successCriterion = "character",
		epsilonValue = "numeric", 
		rValue = "numeric",
		threshold = "numeric",		
		selectPopulationsFunction = "function",
		correlationComputation = "character",
		earlyStop = "matrix",
		selectedPopulations = "array",
		numberOfPopulations = "matrix",
		rejectAtLeastOne = "numeric",
		rejectedPopulationsPerStage = "array",
		successPerStage = "matrix",
		eventsPerStage = "array",
		singleNumberOfEventsPerStage = "array",
		conditionalPowerAchieved = "matrix"
	),
	methods = list(
		initialize = function(design, ...) {
			callSuper(design = design, ...)
			
			for (generatedParam in c(
				"rejectAtLeastOne",
				"selectedPopulations", 
				"numberOfPopulations",
				"rejectedPopulationsPerStage", 
				"successPerStage")) {
				.setParameterType(generatedParam, C_PARAM_GENERATED)
			}
		}
	)
)

.assertIsValidVariedParameterVectorForSimulationResultsPlotting <- function(simulationResults, plotType) {
	if (inherits(simulationResults, "SimulationResultsMeans")) {
		if (is.null(simulationResults$alternative) || 
				any(is.na(simulationResults$alternative)) || 
				length(simulationResults$alternative) <= 1) { 
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "plot type ", plotType, 
					" is only available if 'alternative' with length > 1 is defined")
		}
	}
	else if (inherits(simulationResults, "SimulationResultsRates")) {
		if (is.null(simulationResults$pi1) || 
				any(is.na(simulationResults$pi1)) || 
				length(simulationResults$pi1) <= 1) { 
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "plot type ", plotType, 
					" is only available if 'pi1' with length > 1 is defined")
		}
	}
	else if (inherits(simulationResults, "SimulationResultsSurvival")) {
		if (is.null(simulationResults$hazardRatio) || 
				any(is.na(simulationResults$hazardRatio)) || 
				length(simulationResults$hazardRatio) <= 1) { 
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "plot type ", plotType, 
					" is only available if 'hazardRatio' with length > 1 is defined or derived")
		}
		if (length(simulationResults$hazardRatio) != length(simulationResults$overallReject)) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "plot type ", plotType, 
					" is not available for piecewise survival (only type 13 and 14)")
		}
	}
}

.getSimulationPlotXAxisParameterName <- function(simulationResults, showSource = FALSE, simulationResultsName = NA_character_) {
	if (grepl("SimulationResultsEnrichment", .getClassName(simulationResults))) {
		effectDataList <- .getSimulationEnrichmentEffectData(simulationResults)
		if (ncol(effectDataList$effectData) == 1) {
			if (!isFALSE(showSource)) {
				return(paste0(simulationResultsName, "$effectList$", effectDataList$effectMatrixName, "[, 1]"))
			}
			
			return(sub("s$", "", effectDataList$effectMatrixName))
		}
		
		if (!isFALSE(showSource)) {
			numberOfSituations <- nrow(simulationResults$effectList[[effectDataList$effectMatrixName]])
			return(paste0("c(1:", numberOfSituations, ")"))
		}
		
		return("situation")
	}
	
	survivalEnabled <- grepl("Survival", .getClassName(simulationResults))
	meansEnabled <- grepl("Means", .getClassName(simulationResults))
	if (grepl("MultiArm", .getClassName(simulationResults))) {
		if (!isFALSE(showSource)) {
			gMax <- nrow(simulationResults$effectMatrix)
			return(paste0(simulationResultsName, "$effectMatrix[", gMax, ", ]"))
		}
		
		return("effectMatrix")
	}
	
	if (grepl("Survival", .getClassName(simulationResults))) {
		return("hazardRatio")
	}	
	
	return("effect")
}

.getSimulationPlotXAxisLabel <- function(simulationResults, xlab = NULL) {
	if (grepl("SimulationResultsEnrichment", .getClassName(simulationResults))) {
		effectDataList <- .getSimulationEnrichmentEffectData(simulationResults)
		if (ncol(effectDataList$effectData) == 1) {
			xLabel <- simulationResults$.parameterNames[[effectDataList$effectMatrixName]]
			return(sub("s$", "", xLabel))
		}
		
		return("Situation")
	}
	
	multiArmEnabled <- grepl("MultiArm", .getClassName(simulationResults))
	userDefinedEffectMatrix <- multiArmEnabled && simulationResults$.getParameterType("effectMatrix") == C_PARAM_USER_DEFINED
	if (!is.null(xlab) && !is.na(xlab)) {
		return(xlab)
	}
	
	if (!multiArmEnabled) {
		return("Effect")
	}
	
	return(ifelse(userDefinedEffectMatrix, "Effect Matrix Row", "Maximum Effect"))
}

.getPowerAndStoppingProbabilities <- function(simulationResults, xValues, parameters) {
	yParameterNames <- c()
	
	if ("expectedNumberOfEvents" %in% parameters) {
		yParameterNames <- c(yParameterNames, "expectedNumberOfEvents")
	}
	if ("expectedNumberOfSubjects" %in% parameters) {
		yParameterNames <- c(yParameterNames, "expectedNumberOfSubjects")
	}
	if ("rejectAtLeastOne" %in% parameters) {
		yParameterNames <- c(yParameterNames, "rejectAtLeastOne")
	}
	if ("futilityStop" %in% parameters) {
		yParameterNames <- c(yParameterNames, "futilityStop")
	}
	
	yParameterNamesSrc <- yParameterNames
	
	data <- NULL
	for (yParameterName in yParameterNames) {
		category <- simulationResults$.parameterNames[[yParameterName]]
		part <- data.frame(
			categories = rep(category, length(xValues)),
			xValues = xValues,
			yValues = simulationResults[[yParameterName]]
		)
		if (is.null(data)) {
			data <- part
		} else {
			data <- rbind(data, part)
		}
	}
	
	if ("earlyStop" %in% parameters) {
		yParameterNames <- c(yParameterNames, "earlyStop")
		
		maxEarlyStoppingStages <- nrow(simulationResults$earlyStop) 
		for (k in 1:maxEarlyStoppingStages) {
			category <- "Early stop"
			if (maxEarlyStoppingStages > 1) {
				category <- paste0(category, ", stage ", k)
			}
			part <- data.frame(
				categories = rep(category, ncol(simulationResults$earlyStop)),
				xValues = xValues,
				yValues = simulationResults$earlyStop[k, ]
			)
			data <- rbind(data, part)
			yParameterNamesSrc <- c(yParameterNamesSrc, paste0("earlyStop[", k, ", ]"))
		}
	}
	
	return(list(
		data = data,
		yParameterNames = yParameterNames,
		yParameterNamesSrc = yParameterNamesSrc
	))
}

.plotSimulationResults <- function(simulationResults, designMaster, type = 5L, main = NA_character_, 
		xlab = NA_character_, ylab = NA_character_, palette = "Set1",
		theta = seq(-1, 1, 0.02), plotPointsEnabled = NA, 
		legendPosition = NA_integer_, showSource = FALSE, 
		simulationResultsName = NA_character_, plotSettings = NULL, ...) {
	
	.assertGgplotIsInstalled()
	.assertIsSimulationResults(simulationResults) 
	.assertIsValidLegendPosition(legendPosition)
	.assertIsSingleInteger(type, "type", naAllowed = FALSE, validateType = FALSE)
	theta <- .assertIsValidThetaRange(thetaRange = theta)
	
	if (is.null(plotSettings)) {
		plotSettings <- simulationResults$.plotSettings
	}
	
	survivalEnabled <- grepl("Survival", .getClassName(simulationResults))
	meansEnabled <- grepl("Means", .getClassName(simulationResults))
	multiArmEnabled <- grepl("MultiArm", .getClassName(simulationResults))
	enrichmentEnabled <- grepl("Enrichment", .getClassName(simulationResults))
	userDefinedEffectMatrix <- multiArmEnabled && simulationResults$.getParameterType("effectMatrix") == C_PARAM_USER_DEFINED
	
	gMax <- NA_integer_
	if (multiArmEnabled || enrichmentEnabled) {
		gMax <- ifelse(multiArmEnabled, 
			simulationResults$activeArms, 
			simulationResults$populations)
	}
	
	if (survivalEnabled) {
		nMax <- simulationResults$expectedNumberOfEvents[1] # use first value for plotting
	} else {
		nMax <- simulationResults$expectedNumberOfSubjects[1] # use first value for plotting
	}
	
	if (type %in% c(1:3) && !multiArmEnabled && !enrichmentEnabled) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'type' (", type, 
			") is not available for non-multi-arm/non-enrichment simulation results (type must be > 3)")
	}
	
	if ((!survivalEnabled || multiArmEnabled || enrichmentEnabled) && type %in% c(10:14)) {
		if (multiArmEnabled || enrichmentEnabled) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'type' (", type, 
				") is only available for non-multi-arm/non-enrichment survival simulation results")
		} else {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'type' (", type, 
				") is only available for survival simulation results")
		}
	}
	
	variedParameters <- logical(0)
	
	if (is.na(plotPointsEnabled)) { 
		plotPointsEnabled <- userDefinedEffectMatrix
	}
	
	showSourceHint <- ""
	
	discreteXAxis <- FALSE
	effectData <- NULL
	xValues <- NA_integer_
	if (multiArmEnabled) {
		effectData <- simulationResults$effectMatrix
		effectDataParamName <- paste0("effectMatrix", "[", gMax, ", ]")
		xParameterNameSrc <- paste0(simulationResultsName, "$", effectDataParamName)
		xValues <- effectData[gMax, ]
	} else {
		if (enrichmentEnabled) {
			effectDataList <- .getSimulationEnrichmentEffectData(simulationResults)
			xValues <- effectDataList$xValues
			discreteXAxis <- effectDataList$discreteXAxis
			if (length(xValues) <= 1) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "2 ore more situations must be specifed in ", 
					sQuote(paste0("effectList$", effectDataList$effectMatrixName)))
			}
		}
		
		xParameterNameSrc <- .getSimulationPlotXAxisParameterName(simulationResults, 
			showSource = showSource, simulationResultsName = simulationResultsName)
	}
	
	armCaption <- ifelse(enrichmentEnabled, "Population", "Arm")
	armsCaption <- paste0(armCaption, "s")
	
	srcCmd <- NULL
	if (type == 1) { # Multi-arm, Overall Success
		.assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, type)
		if (is.na(main)) {
			main <- PlotSubTitleItems(title = "Overall Success")
			.addPlotSubTitleItems(simulationResults, designMaster, main, type)
		}
		
		data <- data.frame(
			xValues = xValues,
			yValues = colSums(simulationResults$successPerStage)
		)
		if (userDefinedEffectMatrix) {
			data$xValues <- 1:nrow(data)
		}
		
		legendPosition <- ifelse(is.na(legendPosition), C_POSITION_LEFT_CENTER, legendPosition)
		
		srcCmd <- .showPlotSourceInformation(objectName = simulationResultsName, 
			xParameterName = xParameterNameSrc, 
			yParameterNames = paste0("colSums(", simulationResultsName, "$successPerStage)"), 
			hint = showSourceHint, nMax = nMax,
			type = type, showSource = showSource)
		if (!is.null(srcCmd)) {
			if (.isSpecialPlotShowSourceArgument(showSource)) {
				return(invisible(srcCmd))
			}
			return(srcCmd)
		}
		
		return(.plotDataFrame(data, mainTitle = main, 
				xlab = NA_character_, ylab = NA_character_, 
				xAxisLabel = .getSimulationPlotXAxisLabel(simulationResults),
				yAxisLabel1 = "Overall Success", 
				yAxisLabel2 = NA_character_, 
				plotPointsEnabled = plotPointsEnabled, legendTitle = NA_character_,
				legendPosition = legendPosition, sided = designMaster$sided,
				palette = palette, plotSettings = plotSettings,
				discreteXAxis = discreteXAxis))
	}
	else if (type == 2) { # Multi-arm, Success per Stage
		.assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, type)
		if (is.na(main)) {
			main <- PlotSubTitleItems(title = "Success per Stage")
			.addPlotSubTitleItems(simulationResults, designMaster, main, type)
		}
		
		yParameterNamesSrc <- c()
		data <- NULL
		if (designMaster$kMax > 1) {
			for (k in 1:designMaster$kMax) {
				part <- data.frame(
					categories = rep(k, length(xValues)),
					xValues = xValues,
					yValues = simulationResults$successPerStage[k, ]
				)
				if (userDefinedEffectMatrix) {
					part$xValues <- 1:nrow(part)
				}
				if (is.null(data)) {
					data <- part
				} else {
					data <- rbind(data, part)
				}
				yParameterNamesSrc <- c(yParameterNamesSrc, paste0("successPerStage[", k, ", ]"))
			}
		} else {
			data <- data.frame(
				xValues = xValues,
				yValues = simulationResults$successPerStage[1, ]
			)
			if (userDefinedEffectMatrix) {
				data$xValues <- 1:nrow(data)
			}
			yParameterNamesSrc <- c(yParameterNamesSrc, "successPerStage[1, ]")
		}
		
		legendPosition <- ifelse(is.na(legendPosition), C_POSITION_LEFT_TOP, legendPosition)
		
		srcCmd <- .showPlotSourceInformation(objectName = simulationResultsName, 
			xParameterName = xParameterNameSrc, 
			yParameterNames = yParameterNamesSrc, 
			hint = showSourceHint, nMax = nMax,
			type = type, showSource = showSource)
		if (!is.null(srcCmd)) {
			if (.isSpecialPlotShowSourceArgument(showSource)) {
				return(invisible(srcCmd))
			}
			return(srcCmd)
		}
		
		return(.plotDataFrame(data, mainTitle = main, 
				xlab = NA_character_, ylab = NA_character_, 
				xAxisLabel = .getSimulationPlotXAxisLabel(simulationResults),
				yAxisLabel1 = "Success", 
				yAxisLabel2 = NA_character_, 
				plotPointsEnabled = plotPointsEnabled, legendTitle = "Stage",
				legendPosition = legendPosition, sided = designMaster$sided,
				palette = palette, plotSettings = plotSettings,
				discreteXAxis = discreteXAxis))
	}
	else if (type == 3) { # Multi-arm, Selected Arms/Populations per Stage
			
		.assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, type)
		if (is.na(main)) {
			main <- PlotSubTitleItems(title = paste0("Selected ", armsCaption, " per Stage"))
			.addPlotSubTitleItems(simulationResults, designMaster, main, type)
		}
		
		selectedDataParamName <- ifelse(multiArmEnabled, "selectedArms", "selectedPopulations")
		selectedData <- simulationResults[[selectedDataParamName]]	
		
		yParameterNamesSrc <- c()	
		data <- NULL
		if (designMaster$kMax > 1) {
			for (g in 1:gMax) {
				for (k in 2:designMaster$kMax) {
					stages <- rep(k, length(xValues))
					
					populationCaption <- g
					if (enrichmentEnabled) {
						populationCaption <- ifelse(g == gMax, "F", paste0("S", g))	
					}
					
					part <- data.frame(
						categories = ifelse(designMaster$kMax > 2, 
							paste0(populationCaption, ", ", stages), populationCaption),
						xValues = xValues,
						yValues = selectedData[k, , g]
					)
					if (userDefinedEffectMatrix) {
						part$xValues <- 1:nrow(part)
					}
					if (is.null(data)) {
						data <- part
					} else {
						data <- rbind(data, part)
					}
					yParameterNamesSrc <- c(yParameterNamesSrc, paste0(selectedDataParamName, "[", k, ", , ", g, "]"))
				}
			}
		} else {
			for (g in 1:gMax) {
				part <- data.frame(
					categories = g,
					xValues = xValues,
					yValues = selectedData[1, , g]
				)
				if (userDefinedEffectMatrix) {
					data$xValues <- 1:nrow(data)
				}
				if (is.null(data)) {
					data <- part
				} else {
					data <- rbind(data, part)
				}
				yParameterNamesSrc <- c(yParameterNamesSrc, paste0(selectedDataParamName, "[1, , ", g, "]"))
			}
		}
		
		legendPosition <- ifelse(is.na(legendPosition), C_POSITION_LEFT_TOP, legendPosition)
		
		srcCmd <- .showPlotSourceInformation(objectName = simulationResultsName, 
			xParameterName = xParameterNameSrc, 
			yParameterNames = yParameterNamesSrc, 
			hint = showSourceHint, nMax = nMax,
			type = type, showSource = showSource)
		if (!is.null(srcCmd)) {
			if (.isSpecialPlotShowSourceArgument(showSource)) {
				return(invisible(srcCmd))
			}
			return(srcCmd)
		}
		
		legendTitle <- ifelse(gMax > 1, 
			ifelse(designMaster$kMax > 2, paste0(armCaption, ", Stage"), armCaption),
			ifelse(designMaster$kMax > 2, "Stage", armCaption))
		return(.plotDataFrame(data, mainTitle = main, 
				xlab = NA_character_, ylab = NA_character_, 
				xAxisLabel = .getSimulationPlotXAxisLabel(simulationResults),
				yAxisLabel1 = paste0("Selected ", armsCaption), 
				yAxisLabel2 = NA_character_, 
				plotPointsEnabled = plotPointsEnabled, 
				legendTitle = legendTitle,
				legendPosition = legendPosition, sided = designMaster$sided,
				palette = palette, plotSettings = plotSettings,
				discreteXAxis = discreteXAxis))
	}
	else if (type == 4) { # Multi-arm, Rejected Arms/Populations per Stage
		.assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, type)
		if (is.na(main)) {
			main <- PlotSubTitleItems(title = ifelse(!multiArmEnabled, 
				"Reject per Stage", 
				ifelse(designMaster$kMax > 1, 
					paste0("Rejected ", armsCaption, " per Stage"), paste0("Rejected ", armsCaption))))
			.addPlotSubTitleItems(simulationResults, designMaster, main, type)
		}
		
		yParameterNamesSrc <- c()	
		data <- NULL
		if (multiArmEnabled || enrichmentEnabled) {
			rejectedDataParamName <- ifelse(multiArmEnabled, "rejectedArmsPerStage", "rejectedPopulationsPerStage")
			rejectedData <- simulationResults[[rejectedDataParamName]]			
			if (designMaster$kMax > 1) {
				for (g in 1:gMax) {
					for (k in 1:designMaster$kMax) {
						stages <- rep(k, length(xValues))
						populationCaption <- g
						if (enrichmentEnabled) {
							populationCaption <- ifelse(g == gMax, "F", paste0("S", g))
						}
						part <- data.frame(
							categories = ifelse(gMax > 1, paste0(populationCaption, ", ", stages), stages),
							xValues = xValues,
							yValues = rejectedData[k, , g]
						)
						if (userDefinedEffectMatrix) {
							part$xValues <- 1:nrow(part)
						}
						if (is.null(data)) {
							data <- part
						} else {
							data <- rbind(data, part)
						}
						yParameterNamesSrc <- c(yParameterNamesSrc, paste0(rejectedDataParamName, "[", k, ", , ", g, "]"))
					}
				}
			} else {
				for (g in 1:gMax) {
					part <- data.frame(
						categories = g,
						xValues = xValues,
						yValues = rejectedData[1, , g]
					)
					if (userDefinedEffectMatrix) {
						part$xValues <- 1:nrow(part)
					}
					if (is.null(data)) {
						data <- part
					} else {
						data <- rbind(data, part)
					}
					yParameterNamesSrc <- c(yParameterNamesSrc, paste0(rejectedDataParamName, "[1, , ", g, "]"))
				}
			}
		} else {
			xParameterName <- .getSimulationPlotXAxisParameterName(simulationResults)
			if (designMaster$kMax > 1) {
				for (k in 1:designMaster$kMax) {
					part <- data.frame(
						categories = k,
						xValues = simulationResults[[xParameterName]],
						yValues = simulationResults$rejectPerStage[k, ]
					)
					if (userDefinedEffectMatrix) {
						part$xValues <- 1:nrow(part)
					}
					if (is.null(data)) {
						data <- part
					} else {
						data <- rbind(data, part)
					}
					yParameterNamesSrc <- c(yParameterNamesSrc, paste0("rejectPerStage[", k, ", ]"))
				}
			} else {
				data <- data.frame(
					xValues = simulationResults[[xParameterName]],
					yValues = simulationResults$rejectPerStage[1, ]
				)
				if (userDefinedEffectMatrix) {
					data$xValues <- 1:nrow(data)
				}
				yParameterNamesSrc <- c(yParameterNamesSrc, "rejectPerStage[1, ]")
			}
		}
		
		legendPosition <- ifelse(is.na(legendPosition), C_POSITION_LEFT_TOP, legendPosition)
		
		srcCmd <- .showPlotSourceInformation(objectName = simulationResultsName, 
			xParameterName = xParameterNameSrc, 
			yParameterNames = yParameterNamesSrc, 
			hint = showSourceHint, nMax = nMax,
			type = type, showSource = showSource)
		if (!is.null(srcCmd)) {
			if (.isSpecialPlotShowSourceArgument(showSource)) {
				return(invisible(srcCmd))
			}
			return(srcCmd)
		}
		
		palette <- NULL
		
		legendTitle <- "Stage"
		if (multiArmEnabled) {
			legendTitle <- ifelse(designMaster$kMax > 1, paste0(armCaption, ", Stage"), armCaption)
		}
		else if (enrichmentEnabled) {
			legendTitle <- ifelse(gMax > 1, paste0(armCaption, ", Stage"), "Stage")
		}
		yAxisLabel1 <- ifelse(.isMultiArmSimulationResults(simulationResults), 
			paste0("Rejected ", armsCaption), "Rejection Probability")
		return(.plotDataFrame(data, mainTitle = main, 
				xlab = NA_character_, ylab = NA_character_, 
				xAxisLabel = .getSimulationPlotXAxisLabel(simulationResults),
				yAxisLabel1 = yAxisLabel1, 
				yAxisLabel2 = NA_character_, 
				plotPointsEnabled = plotPointsEnabled, 
				legendTitle = legendTitle,
				legendPosition = legendPosition, sided = designMaster$sided,
				palette = palette, plotSettings = plotSettings,
				discreteXAxis = discreteXAxis))
	}
	else if (type == 5) { # Power and Stopping Probabilities 
		
		.assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, type)
		
		if (is.na(main)) {
			main <- PlotSubTitleItems(title = ifelse(designMaster$kMax == 1, 
					"Overall Power", "Overall Power and Early Stopping"))
			.addPlotSubTitleItems(simulationResults, designMaster, main, type)
		}

		xParameterName <- .getSimulationPlotXAxisParameterName(simulationResults)
		
		if ((multiArmEnabled || enrichmentEnabled) && designMaster$kMax > 1) {
			powerAndStoppingProbabilities <- .getPowerAndStoppingProbabilities(simulationResults, 
				xValues = xValues, 
				parameters = c("rejectAtLeastOne", "futilityStop", "earlyStop"))
			data <- powerAndStoppingProbabilities$data
			yParameterNames <- powerAndStoppingProbabilities$yParameterNames
			yParameterNamesSrc <- powerAndStoppingProbabilities$yParameterNamesSrc
		} else {
			yParameterNames <- ifelse(multiArmEnabled || enrichmentEnabled, "rejectAtLeastOne", "overallReject")
			if (designMaster$kMax > 1) {
				if (!multiArmEnabled && !enrichmentEnabled) {
					yParameterNames <- c(yParameterNames, "earlyStop")
				}
				yParameterNames <- c(yParameterNames, "futilityStop")
			}
			yParameterNamesSrc <- yParameterNames
		}
		
		xlab <- .getSimulationPlotXAxisLabel(simulationResults, xlab)
		ylab <- ifelse(is.na(ylab), "", ylab)
		legendPosition <- ifelse(is.na(legendPosition), C_POSITION_LEFT_TOP, legendPosition)
		
		srcCmd <- .showPlotSourceInformation(objectName = simulationResultsName, 
			xParameterName = xParameterNameSrc, 
			yParameterNames = yParameterNamesSrc, 
			hint = showSourceHint, nMax = nMax,
			type = type, showSource = showSource)
		if (!is.null(srcCmd)) {
			if (.isSpecialPlotShowSourceArgument(showSource)) {
				return(invisible(srcCmd))
			}
			return(srcCmd)
		}
		
		if ((multiArmEnabled || enrichmentEnabled) && designMaster$kMax > 1) {
			return(.plotDataFrame(data, mainTitle = main, 
				xlab = xlab, ylab = ylab, 
				xAxisLabel = .getSimulationPlotXAxisLabel(simulationResults),
				yAxisLabel1 = NA_character_, 
				yAxisLabel2 = NA_character_, 
				plotPointsEnabled = plotPointsEnabled, 
				legendTitle = NA_character_,
				legendPosition = legendPosition, sided = designMaster$sided,
				palette = palette, plotSettings = plotSettings,
				discreteXAxis = discreteXAxis))
		} else {
			if (is.null(list(...)[["ylim"]])) {
				ylim <- c(0, 1)
				return(.plotParameterSet(parameterSet = simulationResults, designMaster = designMaster, 
					xParameterName = xParameterName,
					yParameterNames = yParameterNames, mainTitle = main, xlab = xlab, ylab = ylab,
					palette = palette, theta = theta, nMax = nMax, plotPointsEnabled = plotPointsEnabled,
					legendPosition = legendPosition, variedParameters = variedParameters, 
					qnormAlphaLineEnabled = FALSE, yAxisScalingEnabled = FALSE, 
					plotSettings = plotSettings, ylim = ylim, ...)) # ratioEnabled = TRUE
			} else {
				return(.plotParameterSet(parameterSet = simulationResults, designMaster = designMaster, 
					xParameterName = xParameterName,
					yParameterNames = yParameterNames, mainTitle = main, xlab = xlab, ylab = ylab,
					palette = palette, theta = theta, nMax = nMax, plotPointsEnabled = plotPointsEnabled,
					legendPosition = legendPosition, variedParameters = variedParameters, 
					qnormAlphaLineEnabled = FALSE, yAxisScalingEnabled = FALSE, 
					plotSettings = plotSettings, ...))
			}
		}
	} 
	
	else if (type == 6) { # Average Sample Size / Average Event Number
		.assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, type) 
		
		if (is.na(main)) {
			titlePart <- paste0("Expected ", ifelse(survivalEnabled, "Number of Events", "Number of Subjects"))
			main <- PlotSubTitleItems(title = paste0(titlePart, 
				ifelse(designMaster$kMax == 1, "", paste0(" and Power", 
					ifelse(multiArmEnabled, "", " / Early Stop")))))
			.addPlotSubTitleItems(simulationResults, designMaster, main, type)
		}
		
		xParameterName <- .getSimulationPlotXAxisParameterName(simulationResults)
		yParameterNames <- ifelse(survivalEnabled, "expectedNumberOfEvents", "expectedNumberOfSubjects")
		if (designMaster$kMax > 1) {
			if (multiArmEnabled || enrichmentEnabled) {
				yParameterNames <- c(yParameterNames, "rejectAtLeastOne") 
			} else {
				yParameterNames <- c(yParameterNames, "overallReject") 
			}
			yParameterNames <- c(yParameterNames, "earlyStop")
		}
		
		xlab <- .getSimulationPlotXAxisLabel(simulationResults, xlab)
		legendPosition <- ifelse(is.na(legendPosition), C_POSITION_LEFT_CENTER, legendPosition)
		srcCmd <- .showPlotSourceInformation(objectName = simulationResultsName, 
			xParameterName = xParameterNameSrc, 
			yParameterNames = yParameterNames, 
			hint = showSourceHint, nMax = nMax,
			type = type, showSource = showSource)
	}
	
	else if (type == 7) {
		.assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, type) 
		if (is.na(main)) {
			main <- PlotSubTitleItems(title = "Overall Power")
			.addPlotSubTitleItems(simulationResults, designMaster, main, type)
		}
		
		xParameterName <- .getSimulationPlotXAxisParameterName(simulationResults)
		yParameterNames <- ifelse(multiArmEnabled || enrichmentEnabled, "rejectAtLeastOne", "overallReject")
		xlab <- .getSimulationPlotXAxisLabel(simulationResults, xlab)
		legendPosition <- ifelse(is.na(legendPosition), C_POSITION_RIGHT_CENTER, legendPosition)
		srcCmd <- .showPlotSourceInformation(objectName = simulationResultsName, 
				xParameterName = xParameterNameSrc, 
				yParameterNames = yParameterNames, 
				hint = showSourceHint, nMax = nMax,
				type = type, showSource = showSource)
	}
	
	else if (type == 8) {
		if (designMaster$kMax == 1) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "plot type 8 (Early Stopping) is not available for 'kMax' = 1")
		}
		
		.assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, type) 
		
		futilityStopEnabled <- !is.null(simulationResults[["futilityStop"]]) && 
			!all(na.omit(simulationResults$futilityStop) == 0)
		
		if (is.na(main)) {
			main <- PlotSubTitleItems(title = paste0("Overall Early Stopping", 
				ifelse(futilityStopEnabled, " and Futility Stopping", "")))
			.addPlotSubTitleItems(simulationResults, designMaster, main, type)
		}
		
		xParameterName <- .getSimulationPlotXAxisParameterName(simulationResults)
		yParameterNames <- c("earlyStop")
		if (futilityStopEnabled) {
			yParameterNames <- c(yParameterNames, "futilityStop")
		}
		xlab <- .getSimulationPlotXAxisLabel(simulationResults, xlab)
		legendPosition <- ifelse(is.na(legendPosition), C_POSITION_LEFT_CENTER, legendPosition)
		srcCmd <- .showPlotSourceInformation(objectName = simulationResultsName, 
			xParameterName = xParameterNameSrc, 
			yParameterNames = yParameterNames, 
			hint = showSourceHint, nMax = nMax,
			type = type, showSource = showSource)
	}
	
	else if (type == 9) {
		.assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, type) 
		
		if (is.na(main)) {
			main <- PlotSubTitleItems(title = ifelse(survivalEnabled,
				"Expected Number of Events", "Expected Number of Subjects"))
			.addPlotSubTitleItems(simulationResults, designMaster, main, type)
		}
		
		xParameterName <- .getSimulationPlotXAxisParameterName(simulationResults)
		yParameterNames <- ifelse(survivalEnabled, "expectedNumberOfEvents", "expectedNumberOfSubjects")
		xlab <- .getSimulationPlotXAxisLabel(simulationResults, xlab)
		srcCmd <- .showPlotSourceInformation(objectName = simulationResultsName, 
			xParameterName = xParameterNameSrc, 
			yParameterNames = yParameterNames, 
			hint = showSourceHint, nMax = nMax,
			type = type, showSource = showSource)
	}
	
	else if (type == 10) { # Study Duration
		.assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, type)
		if (is.na(main)) {
			main <- PlotSubTitleItems(title = "Study Duration")
			.addPlotSubTitleItems(simulationResults, designMaster, main, type)
		}
		xParameterName <- "hazardRatio"
		yParameterNames <- "studyDuration"
		srcCmd <- .showPlotSourceInformation(objectName = simulationResultsName, 
			xParameterName = xParameterName, 
			yParameterNames = yParameterNames, 
			hint = showSourceHint, nMax = nMax,
			type = type, showSource = showSource)
	}
	
	else if (type == 11) {
		.assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, type)
		if (is.na(main)) {
			main <- PlotSubTitleItems(title = "Expected Number of Subjects")
			.addPlotSubTitleItems(simulationResults, designMaster, main, type)
		}
		xParameterName <- "hazardRatio"
		yParameterNames <- "expectedNumberOfSubjects" 
		srcCmd <- .showPlotSourceInformation(objectName = simulationResultsName, 
			xParameterName = xParameterName, 
			yParameterNames = yParameterNames, 
			hint = showSourceHint, nMax = nMax,
			type = type, showSource = showSource)
	}
	
	else if (type == 12) { # Analysis Time
		.assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, type)
		if (is.na(main)) {
			main <- PlotSubTitleItems(title = "Analysis Time")
			.addPlotSubTitleItems(simulationResults, designMaster, main, type)
		}
		
		xParameterName <- "hazardRatio"
		yParameterNames <- "analysisTime"
		yParameterNamesSrc <- c()
		for (i in 1:nrow(simulationResults[["analysisTime"]])) {
			yParameterNamesSrc <- c(yParameterNamesSrc, paste0("analysisTime[", i, ", ]"))				
		}
		
		data <- NULL
		for (k in 1:designMaster$kMax) {
			part <- data.frame(
					categories = rep(k, length(simulationResults$hazardRatio)),
					xValues = simulationResults$hazardRatio,
					yValues = simulationResults$analysisTime[k, ]
			)
			if (is.null(data)) {
				data <- part
			} else {
				data <- rbind(data, part)
			}
		}
		
		if (is.na(legendPosition)) {
			legendPosition <- C_POSITION_LEFT_CENTER
		}
		
		srcCmd <- .showPlotSourceInformation(objectName = simulationResultsName, 
			xParameterName = xParameterName, 
			yParameterNames = yParameterNamesSrc, 
			hint = showSourceHint, nMax = nMax,
			type = type, showSource = showSource)
		if (!is.null(srcCmd)) {
			if (.isSpecialPlotShowSourceArgument(showSource)) {
				return(invisible(srcCmd))
			}
			return(srcCmd)
		}
		
		return(.plotDataFrame(data, mainTitle = main, 
			xlab = NA_character_, ylab = NA_character_, xAxisLabel = "Hazard Ratio",
			yAxisLabel1 = "Analysis Time", yAxisLabel2 = NA_character_, 
			plotPointsEnabled = TRUE, legendTitle = "Stage",
			legendPosition = legendPosition, sided = designMaster$sided, plotSettings = plotSettings,
			discreteXAxis = discreteXAxis))
	}
	
	else if (type == 13 || type == 14) { # Cumulative Distribution Function / Survival function
		return(.plotSurvivalFunction(simulationResults, designMaster = designMaster, type = type, main = main, 
			xlab = xlab, ylab = ylab, palette = palette,
			legendPosition = legendPosition, designPlanName = simulationResultsName, 
			showSource = showSource, plotSettings = plotSettings))
	} else {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'type' (", type, ") is not allowed; must be 5, 6, ..., 14")	
	}
	
	if (!is.null(srcCmd)) {
		if (.isSpecialPlotShowSourceArgument(showSource)) {
			return(invisible(srcCmd))
		}
		return(srcCmd)
	}
	
	return(.plotParameterSet(parameterSet = simulationResults, designMaster = designMaster, 
		xParameterName = xParameterName,
		yParameterNames = yParameterNames, mainTitle = main, xlab = xlab, ylab = ylab,
		palette = palette, theta = theta, nMax = nMax, plotPointsEnabled = plotPointsEnabled,
		legendPosition = legendPosition, variedParameters = variedParameters, 
		qnormAlphaLineEnabled = (type != 2), ratioEnabled = TRUE, plotSettings = plotSettings, ...))
}

#'
#' @title
#' Simulation Results Plotting
#' 
#' @param x The simulation results, obtained from \cr
#'        \code{\link{getSimulationSurvival}}.
#' @param y Not available for this kind of plot (is only defined to be compatible to the generic plot function).
#' @param main The main title.
#' @param xlab The x-axis label.
#' @param ylab The y-axis label.
#' @inheritParams param_palette
#' @inheritParams param_theta
#' @inheritParams param_plotPointsEnabled
#' @inheritParams param_showSource
#' @inheritParams param_plotSettings
#' @inheritParams param_legendPosition
#' @inheritParams param_grid
#' @param type The plot type (default = \code{1}). The following plot types are available:
#' \itemize{
#'   \item \code{1}: creates a 'Overall Success' plot (multi-arm only)
#'   \item \code{2}: creates a 'Success per Stage' plot (multi-arm only)
#'   \item \code{3}: creates a 'Selected Arms per Stage' plot (multi-arm only)
#'   \item \code{4}: creates a 'Reject per Stage' or 'Rejected Arms per Stage' plot
#'   \item \code{5}: creates a 'Overall Power and Early Stopping' plot
#'   \item \code{6}: creates a 'Expected Number of Subjects and Power / Early Stop' or 
#'         'Expected Number of Events and Power / Early Stop' plot
#'   \item \code{7}: creates an 'Overall Power' plot
#'   \item \code{8}: creates an 'Overall Early Stopping' plot
#'   \item \code{9}: creates an 'Expected Sample Size' or 'Expected Number of Events' plot
#'   \item \code{10}: creates a 'Study Duration' plot (non-multi-arm survival only)
#'   \item \code{11}: creates an 'Expected Number of Subjects' plot (non-multi-arm survival only)
#'   \item \code{12}: creates an 'Analysis Times' plot (non-multi-arm survival only)
#'   \item \code{13}: creates a 'Cumulative Distribution Function' plot (non-multi-arm survival only)
#'   \item \code{14}: creates a 'Survival Function' plot (non-multi-arm survival only)
#'   \item \code{"all"}: creates all available plots and returns it as a grid plot or list
#' }
#' @inheritParams param_three_dots_plot
#' 
#' @description
#' Plots simulation results.
#' 
#' @details
#' Generic function to plot all kinds of simulation results.
#' 
#' @template return_object_ggplot
#' 
#' @examples 
#' \donttest{
#' results <- getSimulationMeans(alternative = 0:4, stDev = 5, 
#'     plannedSubjects = 40, maxNumberOfIterations = 1000)
#' plot(results, type = 5)
#' }
#' 
#' @export
#'
plot.SimulationResults <- function(x, y, ..., main = NA_character_,
		xlab = NA_character_, ylab = NA_character_, type = 1L, palette = "Set1",
		theta = seq(-1, 1, 0.01), plotPointsEnabled = NA, 
		legendPosition = NA_integer_, showSource = FALSE, 
		grid = 1, plotSettings = NULL) {
		
	fCall = match.call(expand.dots = FALSE)
	simulationResultsName <- deparse(fCall$x)
	.assertIsSingleInteger(grid, "grid", validateType = FALSE)
	typeNumbers <- .getPlotTypeNumber(type, x)
	if (is.null(plotSettings)) {
		plotSettings <- .getGridPlotSettings(x, typeNumbers, grid)
	}
	p <- NULL
	plotList <- list()
	for (typeNumber in typeNumbers) {
		p <- .plotSimulationResults(simulationResults = x, designMaster = x$.design, 
			main = main, xlab = xlab, ylab = ylab, type = typeNumber,
			palette = palette, theta = theta, plotPointsEnabled = plotPointsEnabled, 
			legendPosition = .getGridLegendPosition(legendPosition, typeNumbers, grid), 
			showSource = showSource, simulationResultsName = simulationResultsName, 
			plotSettings = plotSettings, ...)
		.printPlotShowSourceSeparator(showSource, typeNumber, typeNumbers)
		if (length(typeNumbers) > 1) {
			caption <- .getPlotCaption(x, typeNumber, stopIfNotFound = TRUE)
			plotList[[caption]] <- p
		}
	}
	if (length(typeNumbers) == 1) {
		if (.isSpecialPlotShowSourceArgument(showSource)) {
			return(invisible(p))
		}
		
		return(p)
	} 
	
	if (.isSpecialPlotShowSourceArgument(showSource)) {
		return(invisible(plotList))
	}
	
	return(.createPlotResultObject(plotList, grid))
}

#'
#' @name SimulationResults_print
#' 
#' @title
#' Print Simulation Results
#' 
#' @description
#' \code{print} prints its \code{SimulationResults} argument and returns it invisibly (via \code{invisible(x)}). 
#' 
#' @param x The \code{\link{SimulationResults}} object to print.
#' @param markdown If \code{TRUE}, the object \code{x} will be printed using markdown syntax; 
#'        normal representation will be used otherwise (default is \code{FALSE})
#' @inheritParams param_three_dots
#' 
#' @details
#' Prints the parameters and results of an \code{SimulationResults} object.
#'
#' @export
#' 
#' @keywords internal
#' 
print.SimulationResults <- function(x, ..., showStatistics = FALSE, markdown = FALSE) {
	if (markdown) {
		x$.catMarkdownText(showStatistics = showStatistics)
		return(invisible(x))
	}
	
	x$show(showStatistics = showStatistics)
	invisible(x)
}

#'
#' @title
#' Get Simulation Data
#' 
#' @description
#' Returns the aggregated simulation data.
#' 
#' @param x A \code{\link{SimulationResults}} object created by \code{\link{getSimulationMeans}},\cr 
#'  \code{\link{getSimulationRates}}, \code{\link{getSimulationSurvival}}, \code{\link{getSimulationMultiArmMeans}},\cr 
#'  \code{\link{getSimulationMultiArmRates}}, or \code{\link{getSimulationMultiArmSurvival}}.
#'
#' @details
#' This function can be used to get the aggregated simulated data from an simulation results  
#' object, for example, obtained by \code{\link{getSimulationSurvival}}.
#' In this case, the data frame contains the following columns:
#' \enumerate{
#'   \item \code{iterationNumber}: The number of the simulation iteration.
#'   \item \code{stageNumber}: The stage.
#'   \item \code{pi1}: The assumed or derived event rate in the treatment group.
#'   \item \code{pi2}: The assumed or derived event rate in the control group.
#'   \item \code{hazardRatio}: The hazard ratio under consideration (if available).
#'   \item \code{analysisTime}: The analysis time.
#'   \item \code{numberOfSubjects}: The number of subjects under consideration when the 
#'         (interim) analysis takes place.
#'   \item \code{eventsPerStage1}: The observed number of events per stage 
#'         in treatment group 1.
#'   \item \code{eventsPerStage2}: The observed number of events per stage 
#'         in treatment group 2.
#'   \item \code{eventsPerStage}: The observed number of events per stage 
#'         in both treatment groups.
#'   \item \code{rejectPerStage}: 1 if null hypothesis can be rejected, 0 otherwise. 
#'   \item \code{eventsNotAchieved}: 1 if number of events could not be reached with 
#'         observed number of subjects, 0 otherwise.
#'   \item \code{futilityPerStage}: 1 if study should be stopped for futility, 0 otherwise.
#'   \item \code{testStatistic}: The test statistic that is used for the test decision, 
#'         depends on which design was chosen (group sequential, inverse normal, 
#'         or Fisher combination test)'  
#'   \item \code{logRankStatistic}: Z-score statistic which corresponds to a one-sided 
#'         log-rank test at considered stage. 
#'   \item \code{conditionalPowerAchieved}: The conditional power for the subsequent stage of the trial for 
#'         selected sample size and effect. The effect is either estimated from the data or can be
#'         user defined with \code{thetaH1} or \code{pi1H1} and \code{pi2H1}.
#'   \item \code{trialStop}: \code{TRUE} if study should be stopped for efficacy or futility or final stage, \code{FALSE} otherwise.  
#'   \item \code{hazardRatioEstimateLR}: The estimated hazard ratio, derived from the 
#'         log-rank statistic.

#' }
#' A subset of variables is provided for \code{\link{getSimulationMeans}}, \code{\link{getSimulationRates}}, \code{\link{getSimulationMultiArmMeans}},\cr 
#'  \code{\link{getSimulationMultiArmRates}}, or \code{\link{getSimulationMultiArmSurvival}}.
#' 
#' @template return_dataframe
#' 
#' @examples 
#' results <- getSimulationSurvival(pi1 = seq(0.3,0.6,0.1), pi2 = 0.3, eventTime = 12, 
#'     accrualTime = 24, plannedEvents = 40, maxNumberOfSubjects = 200, 
#'     maxNumberOfIterations = 50)
#' data <- getData(results)
#' head(data)
#' dim(data)
#'  
#' @export
#' 
getData <- function(x) {
	if (!inherits(x, "SimulationResults")) { #  or 'Dataset'
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
				"'x' must be a 'SimulationResults' object; for example, use getSimulationMeans() to create one")
	}
	
	return(x$.data)
}

#' @rdname getData
#' @export
getData.SimulationResults <- function(x) {
	return(x$.data)
}

.getAggregatedDataByIterationNumber <- function(rawData, iterationNumber, pi1 = NA_real_) {
	if (!is.na(pi1)) {
		if (is.null(rawData[["pi1"]])) {
			stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'rawData' does not contains a 'pi1' column")
		}
		subData <- rawData[rawData$iterationNumber == iterationNumber & rawData$pi1 == pi1, ]
		if (nrow(subData) == 0) {
			return(NULL)
		}
	} else {
		subData <- rawData[rawData$iterationNumber == iterationNumber, ]
	}
	
	eventsPerStage1 <- sum(subData$event[subData$treatmentGroup == 1])
	eventsPerStage2 <- sum(subData$event[subData$treatmentGroup == 2])
	
	result <- data.frame(
			iterationNumber = iterationNumber,
			pi1 = pi1,
			stageNumber = subData$stopStage[1],
			analysisTime = max(subData$observationTime),
			numberOfSubjects = nrow(subData),
			eventsPerStage1 = eventsPerStage1,
			eventsPerStage2 = eventsPerStage2,
			eventsPerStage = eventsPerStage1 + eventsPerStage2
	)
	
	if (is.na(pi1)) {
		result <- result[, colnames(result) != "pi1"]
	}
	
	return(result)
}

.getAggregatedData <- function(rawData) {
	iterationNumbers <- sort(unique(rawData$iterationNumber))
	pi1Vec <- rawData[["pi1"]]
	if (!is.null(pi1Vec)) {
		pi1Vec <- sort(unique(na.omit(rawData$pi1)))
	}
	
	data <- NULL
	if (!is.null(pi1Vec) && length(pi1Vec) > 0) {
		for (iterationNumber in iterationNumbers) {
			for (pi1 in pi1Vec) {
				row <- .getAggregatedDataByIterationNumber(rawData, iterationNumber, pi1)
				if (!is.null(row)) {
					if (is.null(data)) {
						data <- row
					} else {
						data <- rbind(data, row)
					}
				}
			}
		}
	} else {
		for (iterationNumber in iterationNumbers) {
			row <- .getAggregatedDataByIterationNumber(rawData, iterationNumber)
			if (!is.null(row)) {
				if (is.null(data)) {
					data <- row
				} else {
					data <- rbind(data, row)
				}
			}
		}
	}
	return(data)
}

#'
#' @title
#' Get Simulation Raw Data for Survival
#' 
#' @description
#' Returns the raw survival data which was generated for simulation.
#' 
#' @param x An \code{\link{SimulationResults}} object created by \code{\link{getSimulationSurvival}}.
#' @param aggregate Logical. If \code{TRUE} the raw data will be aggregated similar to
#'        the result of \code{\link{getData}}, default is \code{FALSE}.
#'
#' @details
#' This function works only if \code{\link{getSimulationSurvival}} was called with a \cr
#' \code{maxNumberOfRawDatasetsPerStage} > 0 (default is \code{0}).
#' 
#' This function can be used to get the simulated raw data from a simulation results  
#' object obtained by \code{\link{getSimulationSurvival}}. Note that \code{\link{getSimulationSurvival}} 
#' must called before with \code{maxNumberOfRawDatasetsPerStage} > 0.
#' The data frame contains the following columns: 
#' \enumerate{
#'   \item \code{iterationNumber}: The number of the simulation iteration.
#'   \item \code{stopStage}: The stage of stopping.
#'   \item \code{subjectId}: The subject id (increasing number 1, 2, 3, ...)
#'   \item \code{accrualTime}: The accrual time, i.e., the time when the subject entered the trial.
#'   \item \code{treatmentGroup}: The treatment group number (1 or 2).
#'   \item \code{survivalTime}: The survival time of the subject.
#'   \item \code{dropoutTime}: The dropout time of the subject (may be \code{NA}).
#'   \item \code{observationTime}: The specific observation time.
#'   \item \code{timeUnderObservation}: The time under observation is defined as follows:\cr
#'         if (event == TRUE) {\cr
#'             timeUnderObservation <- survivalTime;\cr
#'         } else if (dropoutEvent == TRUE) {\cr
#'             timeUnderObservation <- dropoutTime;\cr
#'         } else {\cr
#'             timeUnderObservation <- observationTime - accrualTime;\cr
#'         }
#'   \item \code{event}: \code{TRUE} if an event occurred; \code{FALSE} otherwise.
#'   \item \code{dropoutEvent}: \code{TRUE} if an dropout event occurred; \code{FALSE} otherwise. 
#' }
#' 
#' @template return_dataframe
#' 
#' @examples 
#' \donttest{
#' results <- getSimulationSurvival(pi1 = seq(0.3,0.6,0.1), pi2 = 0.3, eventTime = 12, 
#'     accrualTime = 24, plannedEvents = 40, maxNumberOfSubjects = 200, 
#'     maxNumberOfIterations = 50, maxNumberOfRawDatasetsPerStage = 5)
#' rawData <- getRawData(results)
#' head(rawData)
#' dim(rawData)
#' }
#'  
#' @export
#' 
getRawData <- function(x, aggregate = FALSE) {
	if (!inherits(x, "SimulationResultsSurvival")) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
				"'x' must be a 'SimulationResultsSurvival' object; use getSimulationSurvival() to create one")
	}
	
	rawData <- x$.rawData
	if (is.null(rawData) || ncol(rawData) == 0 || nrow(rawData) == 0) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
				"simulation results contain no raw data; ",
				"choose a 'maxNumberOfRawDatasetsPerStage' > 0, e.g., ", 
				"getSimulationSurvival(..., maxNumberOfRawDatasetsPerStage = 1)")
	}
	
	if (!aggregate) {
		return(rawData)
	}
	
	return(.getAggregatedData(rawData))
}
