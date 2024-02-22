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
## |  File version: $Revision: 7651 $
## |  Last changed: $Date: 2024-02-20 15:45:44 +0100 (Di, 20 Feb 2024) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_core_utilities.R
NULL

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
#' @template field_seed
#' @template field_iterations
#'
#' @details
#' \code{SimulationResults} is the basic class for
#' \itemize{
#'   \item \code{\link{SimulationResultsMeans}},
#'   \item \code{\link{SimulationResultsRates}},
#'   \item \code{\link{SimulationResultsSurvival}},
#'   \item \code{\link{SimulationResultsMultiArmMeans}},
#'   \item \code{\link{SimulationResultsMultiArmRates}},
#'   \item \code{\link{SimulationResultsMultiArmSurvival}},
#'   \item \code{\link{SimulationResultsEnrichmentMeans}},
#'   \item \code{\link{SimulationResultsEnrichmentRates}}, and
#'   \item \code{\link{SimulationResultsEnrichmentSurvival}}.
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
        },
        getPlotSettings = function() {
            return(.plotSettings)
        },
        setShowStatistics = function(showStatistics) {
            .assertIsSingleLogical(showStatistics, "showStatistics")
            .showStatistics <<- showStatistics
        },
        show = function(showType = 1, digits = NA_integer_, showStatistics = FALSE) {
            .show(
                showType = showType, digits = digits, showStatistics = showStatistics,
                consoleOutputEnabled = TRUE
            )
        },
        .show = function(..., showType = 1, digits = NA_integer_,
                showStatistics = FALSE, consoleOutputEnabled = TRUE, performanceScore = NULL) {
            "Method for automatically printing simulation result objects"

            .resetCat()
            if (showType == 3) {
                .createSummary(.self, digits = digits)$.show(
                    showType = 1,
                    digits = digits, consoleOutputEnabled = consoleOutputEnabled
                )
            } else if (showType == 2) {
                callSuper(showType = showType, digits = digits, consoleOutputEnabled = consoleOutputEnabled)
            } else {
                if (is.null(showStatistics) || length(showStatistics) != 1) {
                    stop(
                        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                        "'showStatistics' (", .arrayToString(showStatistics),
                        ") must be a single logical or character"
                    )
                }

                if (!is.character(showStatistics) || showStatistics != "exclusive") {
                    .cat(.toString(startWithUpperCase = TRUE), ":\n\n",
                        heading = 1,
                        consoleOutputEnabled = consoleOutputEnabled
                    )

                    .showParametersOfOneGroup(.getDesignParametersToShow(.self), "Design parameters",
                        orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                    )

                    userDefinedParameters <- .getUserDefinedParameters()
                    if (inherits(.self, "SimulationResultsSurvival") &&
                            .self$.piecewiseSurvivalTime$delayedResponseEnabled) {
                        userDefinedParameters <- c(
                            userDefinedParameters,
                            ".piecewiseSurvivalTime$delayedResponseEnabled"
                        )
                    }
                    .showParametersOfOneGroup(userDefinedParameters, "User defined parameters",
                        orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                    )
                    derivedParameters <- .getDerivedParameters()
                    if (length(derivedParameters) > 0) {
                        .showParametersOfOneGroup(derivedParameters, "Derived from user defined parameters",
                            orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                        )
                    }
                    .showParametersOfOneGroup(.getDefaultParameters(), "Default parameters",
                        orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                    )
                    .showParametersOfOneGroup(.getGeneratedParameters(), "Results",
                        orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                    )
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
                            "testStatistic"
                        )
                    } else if (inherits(.self, "SimulationResultsRates")) {
                        params <- c(
                            "effectMeasure",
                            "numberOfSubjects",
                            "testStatistic"
                        )
                    } else if (inherits(.self, "SimulationResultsSurvival")) {
                        params <- c(
                            "effectMeasure",
                            "analysisTime",
                            "numberOfSubjects",
                            "eventsPerStage1",
                            "eventsPerStage2",
                            "cumulativeEventsPerStage",
                            "testStatistic",
                            "logRankStatistic",
                            "hazardRatioEstimateLR"
                        )
                    } else if (inherits(.self, "SimulationResultsMultiArmMeans") ||
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
                    } else if (inherits(.self, "SimulationResultsEnrichmentMeans") ||
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
                    } else if (inherits(.self, "SimulationResultsMultiArmSurvival") ||
                            inherits(.self, "SimulationResultsEnrichmentSurvival")) {
                        params <- c(
                            "effectMeasure",
                            "numberOfEvents",
                            "singleEventsPerArmAndStage",
                            "singleEventsPerPopulationAndStage",
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
                        paramCaption <- .getParameterCaption(parameterName, .self)
                        if (is.null(paramCaption)) {
                            paramCaption <- paste0("%", parameterName, "%")
                        }

                        for (parameterValue1 in parameterValues1) {
                            for (parameterValue2 in parameterValues2) {
                                for (stage in stages) {
                                    if (length(parameterValues1) > 1) {
                                        .catStatisticsLine(
                                            stage = stage,
                                            parameterName = parameterName,
                                            paramCaption = paramCaption,
                                            parameterValue1 = parameterValue1,
                                            variedParameterName1 = variedParameterName1,
                                            parameterValue2 = parameterValue2,
                                            variedParameterName2 = variedParameterName2,
                                            consoleOutputEnabled = consoleOutputEnabled
                                        )
                                    } else {
                                        .catStatisticsLine(
                                            stage = stage,
                                            parameterName = parameterName,
                                            paramCaption = paramCaption,
                                            parameterValue1 = parameterValue2,
                                            variedParameterName1 = variedParameterName2,
                                            consoleOutputEnabled = consoleOutputEnabled
                                        )
                                    }
                                }
                            }
                            if (parameterName == "subjectsActiveArm" && variedParameterName2 == "armNumber") {
                                parameterName2 <- "subjectsControlArm"
                                paramCaption2 <- .getParameterCaption(parameterName2, .self)
                                if (is.null(paramCaption2)) {
                                    paramCaption2 <- paste0("%", parameterName2, "%")
                                }
                                for (stage in stages) {
                                    .catStatisticsLine(
                                        stage = stage,
                                        parameterName = parameterName2,
                                        paramCaption = paramCaption2,
                                        parameterValue1 = parameterValue1,
                                        variedParameterName1 = variedParameterName1,
                                        parameterValue2 = unique(parameterValues2),
                                        variedParameterName2 = variedParameterName2,
                                        consoleOutputEnabled = consoleOutputEnabled
                                    )
                                }
                            }
                        }
                    }
                    .cat("\n", consoleOutputEnabled = consoleOutputEnabled)
                }

                twoGroupsEnabled <- !inherits(.self, "SimulationResultsMeans")
                multiArmSurvivalEnabled <- inherits(.self, "SimulationResultsMultiArmSurvival")
                enrichmentEnabled <- grepl("SimulationResultsEnrichment", .getClassName(.self))

                if (!is.null(performanceScore)) {
                    performanceScore$.showParametersOfOneGroup(
                        performanceScore$.getGeneratedParameters(), "Performance",
                        orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                    )
                    performanceScore$.showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)
                }

                if (.design$kMax > 1 || twoGroupsEnabled || multiArmSurvivalEnabled) {
                    .cat("Legend:\n", heading = 2, consoleOutputEnabled = consoleOutputEnabled)

                    if (multiArmSurvivalEnabled) {
                        .cat("  (i): values of treatment arm i compared to control\n", consoleOutputEnabled = consoleOutputEnabled)
                        .cat("  {j}: values of treatment arm j\n", consoleOutputEnabled = consoleOutputEnabled)
                    } else if (enrichmentEnabled) {
                        matrixName <- .getSimulationEnrichmentEffectMatrixName(.self)
                        if (nrow(.self$effectList[[matrixName]]) > 1) {
                            .cat("  (i): results of situation i\n", consoleOutputEnabled = consoleOutputEnabled)
                        }
                    } else if (twoGroupsEnabled) {
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
                } else if (inherits(.self, "SimulationResultsMultiArmRates")) {
                    variedParameterName1 <- "piMax"
                } else if (inherits(.self, "SimulationResultsMultiArmSurvival")) {
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
                        variedParameterName1, parameterValue1
                    ))
                    if (parameterName != "subjectsControlArm") {
                        postfix <- paste0(postfix, .getVariedParameterValueString(
                            variedParameterName2, parameterValue2
                        ))
                    }
                    paramValue <- .self$.data[[parameterName]][
                        .self$.data$stageNumber == stage &
                            .self$.data[[variedParameterName1]] == parameterValue1 &
                            .self$.data[[variedParameterName2]] %in% parameterValue2
                    ]
                } else {
                    postfix <- paste0(postfix, .getVariedParameterValueString(
                        variedParameterName1, parameterValue1
                    ))
                    paramValue <- .self$.data[[parameterName]][
                        .self$.data$stageNumber == stage &
                            .self$.data[[variedParameterName1]] == parameterValue1
                    ]
                }
            } else {
                paramValue <- .self$.data[[parameterName]][
                    .self$.data$stageNumber == stage
                ]
            }
            if (.design$kMax > 1) {
                postfix <- paste0(postfix, " [", stage, "]")
            }

            if (!consoleOutputEnabled) {
                paramCaption <- paste0("*", paramCaption, "*")
            }

            variableNameFormatted <- .getFormattedVariableName(
                name = paramCaption,
                n = .getNChar(), prefix = "", postfix = postfix
            )

            if (!is.null(paramValue)) {
                paramValue <- stats::na.omit(paramValue)
                if (length(paramValue) > 0 && is.numeric(paramValue)) {
                    paramValueFormatted <- paste0(
                        "median [range]: ", round(stats::median(paramValue), 3),
                        " [", paste(round(base::range(paramValue), 3), collapse = " - "), "]; ",
                        "mean +/-sd: ", round(base::mean(paramValue), 3), " +/-", round(stats::sd(paramValue), 3)
                    )
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
            } else if (inherits(.self, "SimulationResultsBaseRates")) {
                s <- paste(s, "rates")
            } else if (inherits(.self, "SimulationResultsBaseSurvival")) {
                s <- paste(s, "survival data")
            } else if (inherits(.self, "SimulationResultsBaseCountData")) {
                s <- paste(s, "count data")
            } else {
                s <- paste(s, "UNDEFINED")
            }

            if (.design$kMax > 1) {
                if (.isTrialDesignGroupSequential(.design)) {
                    s <- paste(s, "(group sequential design)")
                } else if (.isTrialDesignInverseNormal(.design)) {
                    s <- paste(s, "(inverse normal combination test design)")
                } else if (.isTrialDesignFisher(.design)) {
                    s <- paste(s, "(Fisher's combination test design)")
                } else if (.isTrialDesignConditionalDunnett(.design)) {
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
                "singleEventsPerStage",
                "cumulativeEventsPerStage",
                "iterations",
                "overallReject", # base
                "rejectAtLeastOne",
                "rejectPerStage",
                "rejectedArmsPerStage",
                "rejectedPopulationsPerStage"
            )
            if (.design$kMax > 2) {
                y <- c(y, "futilityStop")
            }
            y <- c(
                y,
                "futilityPerStage",
                "earlyStop", # base
                "successPerStage",
                "selectedArms",
                "selectedPopulations",
                "numberOfActiveArms",
                "numberOfPopulations",
                "expectedNumberOfSubjects",
                "expectedNumberOfEvents",
                "sampleSizes",
                "singleEventsPerArmAndStage",
                "singleEventsPerPopulationAndStage",
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
                maxNumberOfIterations = maxNumberOfIterations
            ))
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
        calcSubjectsFunction = "ANY",
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
                "earlyStop"
            )
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
#' @template field_maxNumberOfIterations
#' @template field_seed
#' @template field_allocationRatioPlanned
#' @template field_conditionalPower
#' @template field_iterations
#' @template field_futilityPerStage
#' @template field_futilityStop
#' @template field_stDev
#' @template field_plannedSubjects
#' @template field_minNumberOfSubjectsPerStage
#' @template field_maxNumberOfSubjectsPerStage
#' @template field_thetaH1
#' @template field_stDevH1
#' @template field_calcSubjectsFunction
#' @template field_expectedNumberOfSubjects
#' @template field_meanRatio
#' @template field_thetaH0
#' @template field_normalApproximation
#' @template field_alternative
#' @template field_groups
#' @template field_directionUpper
#' @template field_effect
#' @template field_earlyStop
#' @template field_sampleSizes
#' @template field_overallReject
#' @template field_rejectPerStage
#' @template field_conditionalPowerAchieved
#'
#' @details
#' Use \code{\link[=getSimulationMeans]{getSimulationMeans()}} to create an object of this type.
#'
#' \code{SimulationResultsMeans} is the basic class for
#' \itemize{
#'   \item \code{\link{SimulationResultsMeans}},
#'   \item \code{\link{SimulationResultsMultiArmMeans}}, and
#'   \item \code{\link{SimulationResultsEnrichmentMeans}}.
#' }
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
#' @template field_maxNumberOfIterations
#' @template field_seed
#' @template field_allocationRatioPlanned
#' @template field_conditionalPower
#' @template field_iterations
#' @template field_futilityPerStage
#' @template field_futilityStop
#' @template field_stDev
#' @template field_plannedSubjects
#' @template field_minNumberOfSubjectsPerStage
#' @template field_maxNumberOfSubjectsPerStage
#' @template field_thetaH1
#' @template field_stDevH1
#' @template field_calcSubjectsFunction
#' @template field_expectedNumberOfSubjects
#' @template field_activeArms
#' @template field_effectMatrix
#' @template field_typeOfShape
#' @template field_muMaxVector
#' @template field_gED50
#' @template field_slope
#' @template field_intersectionTest
#' @template field_adaptations
#' @template field_typeOfSelection
#' @template field_effectMeasure
#' @template field_successCriterion
#' @template field_epsilonValue
#' @template field_rValue
#' @template field_threshold
#' @template field_selectArmsFunction
#' @template field_earlyStop
#' @template field_selectedArms
#' @template field_numberOfActiveArms
#' @template field_rejectAtLeastOne
#' @template field_rejectedArmsPerStage
#' @template field_successPerStage
#' @template field_sampleSizes
#' @template field_conditionalPowerAchieved
#'
#' @details
#' Use \code{\link[=getSimulationMultiArmMeans]{getSimulationMultiArmMeans()}} to create an object of this type.
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
                "successPerStage"
            )) {
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
        calcSubjectsFunction = "ANY",
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
                "earlyStop"
            )
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
#' @template field_maxNumberOfIterations
#' @template field_seed
#' @template field_allocationRatioPlanned
#' @template field_conditionalPower
#' @template field_iterations
#' @template field_futilityPerStage
#' @template field_futilityStop
#' @template field_directionUpper
#' @template field_plannedSubjects
#' @template field_maxNumberOfSubjects
#' @template field_calcSubjectsFunction
#' @template field_expectedNumberOfSubjects
#' @template field_riskRatio
#' @template field_thetaH0
#' @template field_normalApproximation
#' @template field_pi1
#' @template field_pi2
#' @template field_groups
#' @template field_pi1H1
#' @template field_pi2H1
#' @template field_effect
#' @template field_earlyStop
#' @template field_sampleSizes
#' @template field_overallReject
#' @template field_rejectPerStage
#' @template field_conditionalPowerAchieved
#'
#'
#' @details
#' Use \code{\link[=getSimulationRates]{getSimulationRates()}} to create an object of this type.
#'
#' \code{SimulationResultsRates} is the basic class for
#' \itemize{
#'   \item \code{\link{SimulationResultsRates}},
#'   \item \code{\link{SimulationResultsMultiArmRates}}, and
#'   \item \code{\link{SimulationResultsEnrichmentRates}}.
#' }
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
                "studyDuration"
            )
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
#' @template field_maxNumberOfIterations
#' @template field_seed
#' @template field_allocationRatioPlanned
#' @template field_conditionalPower
#' @template field_iterations
#' @template field_futilityPerStage
#' @template field_futilityStop
#' @template field_directionUpper
#' @template field_plannedSubjects
#' @template field_maxNumberOfSubjects
#' @template field_calcSubjectsFunction
#' @template field_expectedNumberOfSubjects
#' @template field_activeArms
#' @template field_effectMatrix
#' @template field_typeOfShape
#' @template field_piMaxVector
#' @template field_piControl
#' @template field_piH1
#' @template field_piControlH1
#' @template field_gED50
#' @template field_slope
#' @template field_intersectionTest
#' @template field_adaptations
#' @template field_typeOfSelection
#' @template field_effectMeasure
#' @template field_successCriterion
#' @template field_epsilonValue
#' @template field_rValue
#' @template field_threshold
#' @template field_selectArmsFunction
#' @template field_earlyStop
#' @template field_selectedArms
#' @template field_numberOfActiveArms
#' @template field_rejectAtLeastOne
#' @template field_rejectedArmsPerStage
#' @template field_successPerStage
#' @template field_sampleSizes
#' @template field_conditionalPowerAchieved
#'
#'
#' @details
#' Use \code{\link[=getSimulationMultiArmRates]{getSimulationMultiArmRates()}} to create an object of this type.
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
        piTreatmentsH1 = "numeric",
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
                "successPerStage"
            )) {
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
        calcEventsFunction = "ANY",
        expectedNumberOfEvents = "numeric"
    ),
    methods = list(
        initialize = function(design, ...) {
            callSuper(design = design, ...)
            generatedParams <- c(
                "iterations",
                "expectedNumberOfEvents",
                "cumulativeEventsPerStage",
                "overallReject",
                "rejectPerStage",
                "futilityPerStage",
                "earlyStop"
            )
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
#' @template field_maxNumberOfIterations
#' @template field_seed
#' @template field_allocationRatioPlanned
#' @template field_conditionalPower
#' @template field_iterations
#' @template field_futilityPerStage
#' @template field_futilityStop
#' @template field_directionUpper
#' @template field_plannedEvents
#' @template field_minNumberOfEventsPerStage
#' @template field_maxNumberOfEventsPerStage
#' @template field_thetaH1
#' @template field_calcEventsFunction
#' @template field_expectedNumberOfEvents
#' @template field_pi1_survival
#' @template field_pi2_survival
#' @template field_median1
#' @template field_median2
#' @template field_maxNumberOfSubjects
#' @template field_accrualTime
#' @template field_accrualIntensity
#' @template field_dropoutRate1
#' @template field_dropoutRate2
#' @template field_dropoutTime
#' @template field_eventTime
#' @template field_thetaH0
#' @template field_allocation1
#' @template field_allocation2
#' @template field_kappa
#' @template field_piecewiseSurvivalTime
#' @template field_lambda1
#' @template field_lambda2
#' @template field_earlyStop
#' @template field_hazardRatio
#' @template field_studyDuration
#' @template field_eventsNotAchieved
#' @template field_numberOfSubjects
#' @template field_numberOfSubjects1
#' @template field_numberOfSubjects2
#' @template field_singleEventsPerStage
#' @template field_cumulativeEventsPerStage
#' @template field_expectedNumberOfSubjects
#' @template field_rejectPerStage
#' @template field_overallReject
#' @template field_conditionalPowerAchieved
#'
#' @details
#' Use \code{\link[=getSimulationSurvival]{getSimulationSurvival()}} to create an object of this type.
#'
#' \code{SimulationResultsSurvival} is the basic class for
#' \itemize{
#'   \item \code{\link{SimulationResultsSurvival}},
#'   \item \code{\link{SimulationResultsMultiArmSurvival}}, and
#'   \item \code{\link{SimulationResultsEnrichmentSurvival}}.
#' }
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
        eventsPerStage = "matrix", # deprecated
        singleEventsPerStage = "matrix",
        cumulativeEventsPerStage = "matrix",
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
                "allocationRatioPlanned"
            )
            if (inherits(.self, "SimulationResultsMultiArmSurvival")) {
                generatedParams <- c(generatedParams, 
                    "cumulativeEventsPerStage", "singleEventsPerArmAndStage")
            } else {
                generatedParams <- c(generatedParams, "singleEventsPerPopulationAndStage")
            }
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
#' @template field_maxNumberOfIterations
#' @template field_seed
#' @template field_allocationRatioPlanned
#' @template field_conditionalPower
#' @template field_iterations
#' @template field_futilityPerStage
#' @template field_futilityStop
#' @template field_directionUpper
#' @template field_plannedEvents
#' @template field_minNumberOfEventsPerStage
#' @template field_maxNumberOfEventsPerStage
#' @template field_expectedNumberOfEvents
#' @template field_activeArms
#' @template field_effectMatrix
#' @template field_typeOfShape
#' @template field_omegaMaxVector
#' @template field_gED50
#' @template field_slope
#' @template field_intersectionTest
#' @template field_adaptations
#' @template field_epsilonValue
#' @template field_rValue
#' @template field_threshold
#' @template field_selectArmsFunction
#' @template field_correlationComputation
#' @template field_earlyStop
#' @template field_selectedArms
#' @template field_numberOfActiveArms
#' @template field_rejectAtLeastOne
#' @template field_rejectedArmsPerStage
#' @template field_successPerStage
#' @template field_eventsPerStage
#' @template field_singleNumberOfEventsPerStage
#' @template field_singleEventsPerArmAndStage
#' @template field_singleEventsPerStage
#' @template field_cumulativeEventsPerStage
#' @template field_conditionalPowerAchieved
#'
#' @details
#' Use \code{\link[=getSimulationMultiArmSurvival]{getSimulationMultiArmSurvival()}} to create an object of this type.
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
        eventsPerStage = "array", # deprecated
        singleEventsPerStage = "array",
        cumulativeEventsPerStage = "array", 
        singleEventsPerArmAndStage = "array",
        singleNumberOfEventsPerStage = "array", # deprecated
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
                "successPerStage"
            )) {
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
#' @template field_maxNumberOfIterations
#' @template field_seed
#' @template field_allocationRatioPlanned
#' @template field_conditionalPower
#' @template field_iterations
#' @template field_futilityPerStage
#' @template field_futilityStop
#' @template field_stDev
#' @template field_plannedSubjects
#' @template field_minNumberOfSubjectsPerStage
#' @template field_maxNumberOfSubjectsPerStage
#' @template field_thetaH1
#' @template field_stDevH1
#' @template field_calcSubjectsFunction
#' @template field_expectedNumberOfSubjects
#' @template field_populations
#' @template field_effectList
#' @template field_intersectionTest
#' @template field_stratifiedAnalysis
#' @template field_adaptations
#' @template field_typeOfSelection
#' @template field_effectMeasure
#' @template field_successCriterion
#' @template field_epsilonValue
#' @template field_rValue
#' @template field_threshold
#' @template field_selectPopulationsFunction
#' @template field_earlyStop
#' @template field_selectedPopulations
#' @template field_numberOfPopulations
#' @template field_rejectAtLeastOne
#' @template field_rejectedPopulationsPerStage
#' @template field_successPerStage
#' @template field_sampleSizes
#' @template field_conditionalPowerAchieved
#'
#' @details
#' Use \code{\link[=getSimulationEnrichmentMeans]{getSimulationEnrichmentMeans()}} to create an object of this type.
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
                "successPerStage"
            )) {
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
#' @template field_maxNumberOfIterations
#' @template field_seed
#' @template field_allocationRatioPlanned
#' @template field_conditionalPower
#' @template field_iterations
#' @template field_futilityPerStage
#' @template field_futilityStop
#' @template field_directionUpper
#' @template field_plannedSubjects
#' @template field_minNumberOfSubjectsPerStage
#' @template field_maxNumberOfSubjectsPerStage
#' @template field_calcSubjectsFunction
#' @template field_expectedNumberOfSubjects
#' @template field_populations
#' @template field_effectList
#' @template field_intersectionTest
#' @template field_stratifiedAnalysis
#' @template field_adaptations
#' @template field_piTreatmentH1
#' @template field_piControlH1
#' @template field_typeOfSelection
#' @template field_effectMeasure
#' @template field_successCriterion
#' @template field_epsilonValue
#' @template field_rValue
#' @template field_threshold
#' @template field_selectPopulationsFunction
#' @template field_earlyStop
#' @template field_selectedPopulations
#' @template field_numberOfPopulations
#' @template field_rejectAtLeastOne
#' @template field_rejectedPopulationsPerStage
#' @template field_successPerStage
#' @template field_sampleSizes
#' @template field_conditionalPowerAchieved
#'
#' @details
#' Use \code{\link[=getSimulationEnrichmentRates]{getSimulationEnrichmentRates()}} to create an object of this type.
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
                "successPerStage"
            )) {
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
#' @template field_maxNumberOfIterations
#' @template field_seed
#' @template field_allocationRatioPlanned
#' @template field_conditionalPower
#' @template field_iterations
#' @template field_futilityPerStage
#' @template field_futilityStop
#' @template field_directionUpper
#' @template field_plannedSubjects
#' @template field_minNumberOfSubjectsPerStage
#' @template field_maxNumberOfSubjectsPerStage
#' @template field_thetaH1_survival
#' @template field_calcEventsFunction
#' @template field_expectedNumberOfEvents
#' @template field_populations
#' @template field_effectList
#' @template field_intersectionTest
#' @template field_stratifiedAnalysis
#' @template field_adaptations
#' @template field_typeOfSelection
#' @template field_effectMeasure
#' @template field_successCriterion
#' @template field_epsilonValue
#' @template field_rValue
#' @template field_threshold
#' @template field_selectPopulationsFunction
#' @template field_correlationComputation
#' @template field_earlyStop
#' @template field_selectedPopulations
#' @template field_numberOfPopulations
#' @template field_rejectAtLeastOne
#' @template field_rejectedPopulationsPerStage
#' @template field_successPerStage
#' @template field_eventsPerStage
#' @template field_singleNumberOfEventsPerStage
#' @template field_singleEventsPerPopulationAndStage
#' @template field_conditionalPowerAchieved
#'
#' @details
#' Use \code{\link[=getSimulationEnrichmentSurvival]{getSimulationEnrichmentSurvival()}} to create an object of this type.
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
        eventsPerStage = "array", # deprecated
        singleEventsPerPopulationAndStage = "array",
        singleNumberOfEventsPerStage = "array", # deprecated
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
                "successPerStage"
            )) {
                .setParameterType(generatedParam, C_PARAM_GENERATED)
            }
        }
    )
)

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
