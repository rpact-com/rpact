## |
## |  *Analysis result classes*
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
## |  File version: $Revision: 6514 $
## |  Last changed: $Date: 2022-08-22 14:31:53 +0200 (Mo, 22 Aug 2022) $
## |  Last changed by: $Author: pahlke $
## |

#'
#' @name ConditionalPowerResults
#'
#' @title
#' Conditional Power Results
#'
#' @description
#' Class for conditional power calculations
#'
#' @details
#' This object cannot be created directly; use \code{\link{getConditionalPower}}
#' with suitable arguments to create the results of a group sequential or a combination test design.
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
ConditionalPowerResults <- setRefClass("ConditionalPowerResults",
    contains = "ParameterSet",
    fields = list(
        .plotSettings = "PlotSettings",
        .design = "TrialDesign",
        .stageResults = "StageResults",
        .plotData = "list",
        nPlanned = "numeric",
        allocationRatioPlanned = "numeric",
        iterations = "integer",
        seed = "numeric",
        simulated = "logical"
    ),
    methods = list(
        initialize = function(...) {
            callSuper(...)

            .plotSettings <<- PlotSettings()
            .parameterNames <<- C_PARAMETER_NAMES
            .parameterFormatFunctions <<- C_PARAMETER_FORMAT_FUNCTIONS

            if (!is.null(.stageResults) && is.null(.design)) {
                .design <<- .stageResults$.design
            }

            if (is.null(simulated) || length(simulated) == 0 || is.na(simulated)) {
                .self$simulated <<- FALSE
            }

            if (!is.null(.design) && length(.design$kMax) == 1 && .design$kMax == 1L) {
                .setParameterType("nPlanned", C_PARAM_NOT_APPLICABLE)
                .setParameterType("allocationRatioPlanned", C_PARAM_NOT_APPLICABLE)
                .setParameterType("conditionalPower", C_PARAM_NOT_APPLICABLE)
            } else {
                .setParameterType("nPlanned", C_PARAM_GENERATED)
                .setParameterType("allocationRatioPlanned", C_PARAM_USER_DEFINED)
                .setParameterType("conditionalPower", C_PARAM_GENERATED)
            }
            .setParameterType("simulated", C_PARAM_NOT_APPLICABLE)
        },
        show = function(showType = 1, digits = NA_integer_) {
            .show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
        },
        .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
            "Method for automatically printing conditional power result objects"
            .resetCat()
            if (showType == 2) {
                callSuper(showType = showType, digits = digits, consoleOutputEnabled = consoleOutputEnabled)
            } else {
                if (!is.null(.design) && length(.design$kMax) == 1 && .design$kMax == 1) {
                    .cat(.toString(), ": not applicable for fixed design (kMax = 1)\n",
                        heading = 1,
                        consoleOutputEnabled = consoleOutputEnabled
                    )
                } else {
                    .cat(.toString(), ":\n\n",
                        heading = 1,
                        consoleOutputEnabled = consoleOutputEnabled
                    )
                }
                .showParametersOfOneGroup(.getUserDefinedParameters(), "User defined parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                .showParametersOfOneGroup(.getDefaultParameters(), "Default parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                .showParametersOfOneGroup(.getGeneratedParameters(), "Output",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                .showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)
            }
        },
        .toString = function(startWithUpperCase = FALSE) {
            return("Conditional power results")
        }
    )
)

ConditionalPowerResultsMeans <- setRefClass("ConditionalPowerResultsMeans",
    contains = "ConditionalPowerResults",
    fields = list(
        conditionalPower = "numeric",
        thetaH1 = "numeric",
        assumedStDev = "numeric"
    ),
    methods = list(
        initialize = function(...) {
            callSuper(...)

            if ((is.null(conditionalPower) || length(conditionalPower) == 0) &&
                    !is.null(.design) && !is.null(.design$kMax) && length(.design$kMax) > 0) {
                conditionalPower <<- rep(NA_real_, .design$kMax)
            }

            if (is.null(thetaH1) || length(thetaH1) == 0 || all(is.na(thetaH1))) {
                thetaH1 <<- NA_real_
            }
            if (is.null(assumedStDev) || length(assumedStDev) == 0 || all(is.na(assumedStDev))) {
                assumedStDev <<- NA_real_
            }
        },
        .toString = function(startWithUpperCase = FALSE) {
            return("Conditional power results means")
        }
    )
)

ConditionalPowerResultsMultiHypotheses <- setRefClass("ConditionalPowerResultsMultiHypotheses",
    contains = "ConditionalPowerResults",
    fields = list(
        conditionalPower = "matrix"
    ),
    methods = list(
        initialize = function(...) {
            callSuper(...)

            if (.readyForInitialization()) {
                gMax <- getGMax()
                kMax <- .design$kMax
                if (is.null(conditionalPower) || (nrow(conditionalPower) == 0 && ncol(conditionalPower) == 0)) {
                    conditionalPower <<- matrix(rep(NA_real_, gMax * kMax), nrow = gMax, ncol = kMax)
                }
            }
        },
        .toString = function(startWithUpperCase = FALSE) {
            s <- "Conditional power results"
            s <- paste0(s, " ", ifelse(grepl("Enrichment", .getClassName(.stageResults)), "enrichment", "multi-arm"))
            if (grepl("Means", .getClassName(.self))) {
                s <- paste0(s, " means")
            } else if (grepl("Rates", .getClassName(.self))) {
                s <- paste0(s, " rates")
            } else if (grepl("Survival", .getClassName(.self))) {
                s <- paste0(s, " survival")
            }
            return(s)
        },
        getGMax = function() {
            return(.stageResults$getGMax())
        },
        .readyForInitialization = function() {
            if (is.null(.design)) {
                return(FALSE)
            }

            if (length(.design$kMax) != 1) {
                return(FALSE)
            }

            if (is.null(.stageResults)) {
                return(FALSE)
            }

            if (is.null(.stageResults$testStatistics)) {
                return(FALSE)
            }

            return(TRUE)
        }
    )
)

ConditionalPowerResultsMultiArmMeans <- setRefClass("ConditionalPowerResultsMultiArmMeans",
    contains = "ConditionalPowerResultsMultiHypotheses",
    fields = list(
        thetaH1 = "numeric",
        assumedStDevs = "numeric"
    ),
    methods = list(
        initialize = function(...) {
            callSuper(...)

            if (.readyForInitialization()) {
                gMax <- getGMax()
                if (is.null(thetaH1) || length(thetaH1) == 0 || all(is.na(thetaH1))) {
                    thetaH1 <<- rep(NA_real_, gMax)
                }
                if (is.null(assumedStDevs) || length(assumedStDevs) == 0 || all(is.na(assumedStDevs))) {
                    assumedStDevs <<- rep(NA_real_, gMax)
                }
            }
        }
    )
)

ConditionalPowerResultsRates <- setRefClass("ConditionalPowerResultsRates",
    contains = "ConditionalPowerResults",
    fields = list(
        conditionalPower = "numeric",
        pi1 = "numeric",
        pi2 = "numeric"
    ),
    methods = list(
        initialize = function(...) {
            callSuper(...)

            if ((is.null(conditionalPower) || length(conditionalPower) == 0) &&
                    !is.null(.design) && !is.null(.design$kMax) && length(.design$kMax) > 0) {
                conditionalPower <<- rep(NA_real_, .design$kMax)
            }

            if (is.null(pi1) || length(pi1) == 0 || all(is.na(pi1))) {
                pi1 <<- NA_real_
            }
            if (is.null(pi2) || length(pi2) == 0 || all(is.na(pi2))) {
                pi2 <<- NA_real_
            }
        },
        .toString = function(startWithUpperCase = FALSE) {
            return("Conditional power results rates")
        }
    )
)

ConditionalPowerResultsMultiArmRates <- setRefClass("ConditionalPowerResultsMultiArmRates",
    contains = "ConditionalPowerResultsMultiHypotheses",
    fields = list(
        piTreatments = "numeric",
        piControl = "numeric"
    ),
    methods = list(
        initialize = function(...) {
            callSuper(...)

            if (.readyForInitialization()) {
                gMax <- getGMax()
                if (is.null(piControl) || length(piControl) == 0 || all(is.na(piControl))) {
                    piControl <<- NA_real_
                }
                if (is.null(piTreatments) || length(piTreatments) == 0 || all(is.na(piTreatments))) {
                    piTreatments <<- rep(NA_real_, gMax)
                }
            }
        }
    )
)

ConditionalPowerResultsSurvival <- setRefClass("ConditionalPowerResultsSurvival",
    contains = "ConditionalPowerResults",
    fields = list(
        conditionalPower = "numeric",
        thetaH1 = "numeric"
    ),
    methods = list(
        initialize = function(...) {
            callSuper(...)

            if ((is.null(conditionalPower) || length(conditionalPower) == 0) &&
                    !is.null(.design) && !is.null(.design$kMax) && length(.design$kMax) > 0) {
                conditionalPower <<- rep(NA_real_, .design$kMax)
            }

            if (is.null(thetaH1) || length(thetaH1) == 0 || all(is.na(thetaH1))) {
                thetaH1 <<- NA_real_
            }
        },
        .toString = function(startWithUpperCase = FALSE) {
            return("Conditional power results survival")
        }
    )
)

ConditionalPowerResultsMultiArmSurvival <- setRefClass("ConditionalPowerResultsMultiArmSurvival",
    contains = "ConditionalPowerResultsMultiHypotheses",
    fields = list(
        thetaH1 = "numeric"
    ),
    methods = list(
        initialize = function(...) {
            callSuper(...)

            if (.readyForInitialization()) {
                gMax <- getGMax()
                if (is.null(thetaH1) || length(thetaH1) == 0 || all(is.na(thetaH1))) {
                    thetaH1 <<- rep(NA_real_, gMax)
                }
            }
        }
    )
)

ConditionalPowerResultsEnrichmentMeans <- setRefClass("ConditionalPowerResultsEnrichmentMeans",
    contains = "ConditionalPowerResultsMultiArmMeans"
)


ConditionalPowerResultsEnrichmentRates <- setRefClass("ConditionalPowerResultsEnrichmentRates",
    contains = "ConditionalPowerResultsMultiHypotheses",
    fields = list(
        piTreatments = "numeric",
        piControls = "numeric"
    ),
    methods = list(
        initialize = function(...) {
            callSuper(...)

            if (.readyForInitialization()) {
                gMax <- getGMax()
                if (is.null(piControls) || length(piControls) == 0 || all(is.na(piControls))) {
                    piControls <<- rep(NA_real_, gMax)
                }
                if (is.null(piTreatments) || length(piTreatments) == 0 || all(is.na(piTreatments))) {
                    piTreatments <<- rep(NA_real_, gMax)
                }
            }
        }
    )
)


ConditionalPowerResultsEnrichmentSurvival <- setRefClass("ConditionalPowerResultsEnrichmentSurvival",
    contains = "ConditionalPowerResultsMultiArmSurvival"
)

#'
#' @name ClosedCombinationTestResults
#'
#' @title
#' Analysis Results Closed Combination Test
#'
#' @description
#' Class for multi-arm analysis results based on a closed combination test.
#'
#' @details
#' This object cannot be created directly; use \code{\link{getAnalysisResults}}
#' with suitable arguments to create the multi-arm analysis results of a closed combination test design.
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
ClosedCombinationTestResults <- setRefClass("ClosedCombinationTestResults",
    contains = "ParameterSet",
    fields = list(
        .plotSettings = "PlotSettings",
        .design = "TrialDesign",
        .enrichment = "logical",
        intersectionTest = "character",
        indices = "matrix",
        adjustedStageWisePValues = "matrix",
        overallAdjustedTestStatistics = "matrix",
        separatePValues = "matrix",
        conditionalErrorRate = "matrix",
        secondStagePValues = "matrix",
        rejected = "matrix",
        rejectedIntersections = "matrix"
    ),
    methods = list(
        initialize = function(...) {
            callSuper(...)

            .plotSettings <<- PlotSettings()
            .parameterNames <<- C_PARAMETER_NAMES
            .parameterFormatFunctions <<- C_PARAMETER_FORMAT_FUNCTIONS

            .setParameterType("intersectionTest", C_PARAM_USER_DEFINED)

            parametersGenerated <- c(
                "indices",
                "separatePValues",
                "rejected",
                "rejectedIntersections"
            )
            if (inherits(.design, "TrialDesignConditionalDunnett")) {
                parametersGenerated <- c(
                    parametersGenerated,
                    "conditionalErrorRate",
                    "secondStagePValues"
                )
            } else {
                parametersGenerated <- c(
                    parametersGenerated,
                    "adjustedStageWisePValues",
                    "overallAdjustedTestStatistics"
                )
            }
            for (param in parametersGenerated) {
                .setParameterType(param, C_PARAM_GENERATED)
            }

            if (!is.null(.design) && inherits(.design, C_CLASS_NAME_TRIAL_DESIGN_FISHER)) {
                .parameterFormatFunctions$overallAdjustedTestStatistics <<- ".formatTestStatisticsFisher"
            }
        },
        show = function(showType = 1, digits = NA_integer_) {
            .show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
        },
        .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
            "Method for automatically printing closed combination test result objects"
            .resetCat()
            if (showType == 2) {
                callSuper(showType = showType, digits = digits, consoleOutputEnabled = consoleOutputEnabled)
            } else {
                .cat(.toString(), ":\n\n",
                    heading = 1,
                    consoleOutputEnabled = consoleOutputEnabled
                )

                .showParametersOfOneGroup(.getUserDefinedParameters(), "User defined parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )

                designParametersToShow <- c(
                    ".design$stages",
                    ".design$alpha"
                )
                if (inherits(.design, "TrialDesignConditionalDunnett")) {
                    designParametersToShow <- c(
                        designParametersToShow,
                        ".design$informationAtInterim",
                        ".design$secondStageConditioning"
                    )
                }
                .showParametersOfOneGroup(designParametersToShow, "Design parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )

                .showParametersOfOneGroup(.getGeneratedParameters(), "Output",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )

                .showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)

                .cat("Legend:\n", heading = 2, consoleOutputEnabled = consoleOutputEnabled)
                if (isTRUE(.enrichment)) {
                    .cat(paste0("  S[i]: population i\n"), consoleOutputEnabled = consoleOutputEnabled)
                    .cat(paste0("  F: full population\n"), consoleOutputEnabled = consoleOutputEnabled)
                } else {
                    .cat(paste0(
                        "  (i): results of treatment arm i vs. control group ",
                        (nrow(separatePValues) + 1), "\n"
                    ), consoleOutputEnabled = consoleOutputEnabled)
                    .cat("  [i]: hypothesis number\n",
                        consoleOutputEnabled = consoleOutputEnabled
                    )
                }
            }
        },
        .toString = function(startWithUpperCase = FALSE) {
            s <- "Closed combination test results"
            if (inherits(.design, "TrialDesignConditionalDunnett")) {
                s <- paste0(s, " (Conditional Dunnett)")
            }
            return(s)
        },
        .getHypothesisTreatmentArms = function(number) {
            result <- c()
            for (i in 1:ncol(indices)) {
                if (indices[number, i] == 1) {
                    result <- c(result, i)
                }
            }
            return(result)
        },
        .getHypothesisTreatmentArmVariants = function() {
            result <- c()
            for (number in 1:nrow(indices)) {
                arms <- .getHypothesisTreatmentArms(number)
                result <- c(result, paste0(arms, collapse = ", "))
            }
            return(result)
        },
        .getHypothesisPopulationVariants = function() {
            result <- c()
            gMax <- 1
            for (number in 1:nrow(indices)) {
                arms <- .getHypothesisTreatmentArms(number)
                if (number == 1) {
                    gMax <- length(arms)
                }
                arms <- paste0("S", arms)
                arms[arms == paste0("S", gMax)] <- "F"
                result <- c(result, paste0(arms, collapse = ", "))
            }
            return(result)
        }
    )
)

#'
#' @name AnalysisResults
#'
#' @title
#' Basic Class for Analysis Results
#'
#' @description
#' A basic class for analysis results.
#'
#' @details
#' \code{AnalysisResults} is the basic class for
#' \itemize{
#'   \item \code{\link{AnalysisResultsFisher}},
#'   \item \code{\link{AnalysisResultsGroupSequential}}, and
#'   \item \code{\link{AnalysisResultsInverseNormal}}.
#' }
#'
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_analysis_dataset.R
#' @include class_design.R
#' @include f_core_constants.R
#' @include class_analysis_stage_results.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
AnalysisResults <- setRefClass("AnalysisResults",
    contains = "ParameterSet",
    fields = list(
        .plotSettings = "PlotSettings",
        .design = "TrialDesign",
        .dataInput = "Dataset",
        .stageResults = "StageResults",
        .conditionalPowerResults = "ConditionalPowerResults",
        normalApproximation = "logical",
        directionUpper = "logical",
        thetaH0 = "numeric",
        pi1 = "numeric",
        pi2 = "numeric",
        nPlanned = "numeric",
        allocationRatioPlanned = "numeric"
    ),
    methods = list(
        initialize = function(design, dataInput, ...) {
            callSuper(.design = design, .dataInput = dataInput, ...)

            .plotSettings <<- PlotSettings()
            .parameterNames <<- .getParameterNames(design = design, analysisResults = .self)
            .parameterFormatFunctions <<- C_PARAMETER_FORMAT_FUNCTIONS
        },
        .setStageResults = function(stageResults) {
            .stageResults <<- stageResults
            .parameterNames <<- .getParameterNames(design = .design, stageResults = stageResults, analysisResults = .self)
        },
        getPlotSettings = function() {
            return(.plotSettings)
        },
        show = function(showType = 1, digits = NA_integer_) {
            .show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
        },
        .getStageResultParametersToShow = function() {
            stageResultParametersToShow <- c()
            if (.design$kMax > 1) {
                if (!grepl("Rates", .getClassName(.dataInput)) || .dataInput$getNumberOfGroups() > 1) {
                    stageResultParametersToShow <- c(stageResultParametersToShow, ".stageResults$effectSizes")
                }

                if (grepl("Means", .getClassName(.dataInput))) {
                    stageResultParametersToShow <- c(stageResultParametersToShow, ".stageResults$overallStDevs")
                }
                if (grepl("Rates", .getClassName(.dataInput))) {
                    if (.isMultiArmAnalysisResults(.self)) {
                        stageResultParametersToShow <- c(stageResultParametersToShow, ".stageResults$overallPiTreatments")
                        stageResultParametersToShow <- c(stageResultParametersToShow, ".stageResults$overallPiControl")
                    } else if (.isEnrichmentAnalysisResults(.self)) {
                        stageResultParametersToShow <- c(stageResultParametersToShow, ".stageResults$overallPisTreatment")
                        stageResultParametersToShow <- c(stageResultParametersToShow, ".stageResults$overallPisControl")
                    } else {
                        stageResultParametersToShow <- c(stageResultParametersToShow, ".stageResults$overallPi1")
                        if (.dataInput$getNumberOfGroups() > 1) {
                            stageResultParametersToShow <- c(stageResultParametersToShow, ".stageResults$overallPi2")
                        }
                    }
                }
            }
            stageResultParametersToShow <- c(stageResultParametersToShow, ".stageResults$testStatistics")
            if (grepl("(MultiArm|Dunnett|Enrichment)", .getClassName(.self))) {
                stageResultParametersToShow <- c(stageResultParametersToShow, ".stageResults$separatePValues")
            } else {
                stageResultParametersToShow <- c(stageResultParametersToShow, ".stageResults$pValues")
            }

            if (.design$kMax == 1) {
                # return(stageResultParametersToShow)
            }

            # show combination test statistics
            if (.isTrialDesignInverseNormal(.design)) {
                stageResultParametersToShow <- c(stageResultParametersToShow, ".stageResults$combInverseNormal")
            } else if (.isTrialDesignGroupSequential(.design)) {
                stageResultParametersToShow <- c(stageResultParametersToShow, ".stageResults$overallTestStatistics")
                stageResultParametersToShow <- c(stageResultParametersToShow, ".stageResults$overallPValues")
            } else if (.isTrialDesignFisher(.design)) {
                stageResultParametersToShow <- c(stageResultParametersToShow, ".stageResults$combFisher")
            }
            return(stageResultParametersToShow)
        },
        .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
            "Method for automatically printing analysis result objects"
            .resetCat()
            if (showType == 2) {
                callSuper(showType = showType, digits = digits, consoleOutputEnabled = consoleOutputEnabled)
            } else {
                .cat(.toString(startWithUpperCase = TRUE), ":\n\n",
                    heading = 1,
                    consoleOutputEnabled = consoleOutputEnabled
                )

                .showParametersOfOneGroup(.getDesignParametersToShow(.self), "Design parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )

                .showParametersOfOneGroup(.getUserDefinedParameters(), "User defined parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )

                .showParametersOfOneGroup(.getDefaultParameters(), "Default parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )

                .showParametersOfOneGroup(.getStageResultParametersToShow(), "Stage results",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )

                # show multi-arm parameters
                if (grepl("(MultiArm|Dunnett|Enrichment)", .getClassName(.self))) {
                    if (.isTrialDesignConditionalDunnett(.design)) {
                        .showParametersOfOneGroup(".closedTestResults$conditionalErrorRate",
                            "Conditional error rate",
                            orderByParameterName = FALSE,
                            consoleOutputEnabled = consoleOutputEnabled
                        )
                        .showParametersOfOneGroup(".closedTestResults$secondStagePValues",
                            "Second stage p-values",
                            orderByParameterName = FALSE,
                            consoleOutputEnabled = consoleOutputEnabled
                        )
                    } else {
                        .showParametersOfOneGroup(".closedTestResults$adjustedStageWisePValues",
                            "Adjusted stage-wise p-values",
                            orderByParameterName = FALSE,
                            consoleOutputEnabled = consoleOutputEnabled
                        )
                        .showParametersOfOneGroup(".closedTestResults$overallAdjustedTestStatistics",
                            "Overall adjusted test statistics",
                            orderByParameterName = FALSE,
                            consoleOutputEnabled = consoleOutputEnabled
                        )
                    }

                    .showParametersOfOneGroup(".closedTestResults$rejected", "Test actions",
                        orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                    )
                }

                generatedParams <- .getGeneratedParameters()
                generatedParams <- generatedParams[!(generatedParams %in%
                    c("assumedStDevs", "thetaH1", "pi1", "pi2", "piTreatments", "piTreatments", "piControl", "piControls"))]

                if (grepl("(MultiArm|Dunnett|Enrichment)", .getClassName(.self))) {
                    
                    if (all(c("conditionalPowerSimulated", "conditionalRejectionProbabilities") %in% generatedParams)) {
                        generatedParams <- .moveValue(generatedParams, 
                            "conditionalPowerSimulated", "conditionalRejectionProbabilities")
                    }
                    
                    .showParametersOfOneGroup(generatedParams, "Further analysis results",
                        orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                    )
                } else {
                    .showParametersOfOneGroup(generatedParams, "Analysis results",
                        orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                    )
                }

                .showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)

                if (grepl("(MultiArm|Dunnett)", .getClassName(.self))) {
                    .cat("Legend:\n", heading = 2, consoleOutputEnabled = consoleOutputEnabled)
                    .cat(paste0(
                        "  (i): results of treatment arm i vs. control group ",
                        .dataInput$getNumberOfGroups(), "\n"
                    ),
                    consoleOutputEnabled = consoleOutputEnabled
                    )
                } else if (.isEnrichmentAnalysisResults(.self)) {
                    .cat("Legend:\n", heading = 2, consoleOutputEnabled = consoleOutputEnabled)
                    .cat(paste0("  S[i]: population i\n"), consoleOutputEnabled = consoleOutputEnabled)
                    .cat(paste0("  F: full population\n"), consoleOutputEnabled = consoleOutputEnabled)
                } else if (grepl("Rates", .getClassName(.dataInput)) && .dataInput$getNumberOfGroups() == 2) {
                    .cat("Legend:\n", heading = 2, consoleOutputEnabled = consoleOutputEnabled)
                    .cat("  (i): values of treatment arm i\n", consoleOutputEnabled = consoleOutputEnabled)
                }
            }
        },
        .toString = function(startWithUpperCase = FALSE) {
            str <- "analysis results"
            if (inherits(.self, "AnalysisResultsMultiArm")) {
                str <- paste0("multi-arm ", str)
            } else if (inherits(.self, "AnalysisResultsEnrichment")) {
                str <- paste0("enrichment ", str)
            }
            if (startWithUpperCase) {
                str <- .firstCharacterToUpperCase(str)
            }

            numberOfGroups <- .dataInput$getNumberOfGroups()
            str <- paste0(str, " (")

            str <- paste0(str, tolower(sub("Dataset(Enrichment)?", "", .getClassName(.dataInput))))
            if (grepl("Survival", .getClassName(.getClassName))) {
                str <- paste0(str, " data")
            }

            if (numberOfGroups == 1) {
                str <- paste0(str, " of one group")
            } else {
                str <- paste0(str, " of ", numberOfGroups, " groups")
            }

            if (.design$kMax > 1) {
                if (grepl("GroupSequential", .getClassName(.self))) {
                    str <- paste0(str, ", group sequential design")
                } else if (grepl("InverseNormal", .getClassName(.self))) {
                    str <- paste0(str, ", inverse normal combination test design")
                } else if (grepl("Fisher", .getClassName(.self))) {
                    str <- paste0(str, ", Fisher's combination test design")
                } else if (grepl("Dunnett", .getClassName(.self))) {
                    str <- paste0(str, ", conditional Dunnett design")
                }
            } else {
                str <- paste0(str, ", fixed sample size design")
            }

            str <- paste0(str, ")")
            return(str)
        },
        getNumberOfStages = function() {
            return(.stageResults$getNumberOfStages())
        },
        getDataInput = function() {
            return(.dataInput)
        }
    )
)

AnalysisResultsBase <- setRefClass("AnalysisResultsBase",
    contains = "AnalysisResults",
    fields = list(
        thetaH1 = "numeric",
        assumedStDev = "numeric",
        equalVariances = "logical",
        testActions = "character",
        conditionalRejectionProbabilities = "numeric",
        conditionalPower = "numeric",
        repeatedConfidenceIntervalLowerBounds = "numeric",
        repeatedConfidenceIntervalUpperBounds = "numeric",
        repeatedPValues = "numeric",
        finalStage = "integer",
        finalPValues = "numeric",
        finalConfidenceIntervalLowerBounds = "numeric",
        finalConfidenceIntervalUpperBounds = "numeric",
        medianUnbiasedEstimates = "numeric"
    ),
    methods = list(
        initialize = function(design, dataInput, ...) {
            callSuper(design = design, dataInput = dataInput, ...)
            finalStage <<- NA_integer_
        }
    )
)

#'
#' @name AnalysisResultsMultiHypotheses
#'
#' @title
#' Basic Class for Analysis Results Multi-Hypotheses
#'
#' @description
#' A basic class for multi-hypotheses analysis results.
#'
#' @details
#' \code{AnalysisResultsMultiHypotheses} is the basic class for
#' \itemize{
#'   \item \code{\link{AnalysisResultsMultiArm}} and
#'   \item \code{\link{AnalysisResultsEnrichment}}.
#' }
#'
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_analysis_dataset.R
#' @include class_design.R
#' @include f_core_constants.R
#' @include class_analysis_stage_results.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
AnalysisResultsMultiHypotheses <- setRefClass("AnalysisResultsMultiHypotheses",
    contains = "AnalysisResults",
    fields = list(
        .closedTestResults = "ClosedCombinationTestResults",
        thetaH1 = "matrix", # means only
        assumedStDevs = "matrix", # means only
        piTreatments = "matrix", # rates only
        intersectionTest = "character",
        varianceOption = "character",
        conditionalRejectionProbabilities = "matrix",
        conditionalPower = "matrix",
        repeatedConfidenceIntervalLowerBounds = "matrix",
        repeatedConfidenceIntervalUpperBounds = "matrix",
        repeatedPValues = "matrix"
    ),
    methods = list(
        initialize = function(design, dataInput, ...) {
            callSuper(design = design, dataInput = dataInput, ...)
            for (param in c("thetaH1", "assumedStDevs", "piTreatments")) {
                .setParameterType(param, C_PARAM_NOT_APPLICABLE)
            }
        }
    )
)

#'
#' @name AnalysisResultsMultiArm
#'
#' @title
#' Basic Class for Analysis Results Multi-Arm
#'
#' @description
#' A basic class for multi-arm analysis results.
#'
#' @details
#' \code{AnalysisResultsMultiArm} is the basic class for
#' \itemize{
#'   \item \code{\link{AnalysisResultsMultiArmFisher}},
#'   \item \code{\link{AnalysisResultsMultiArmInverseNormal}}, and
#'   \item \code{\link{AnalysisResultsConditionalDunnett}}.
#' }
#'
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_analysis_dataset.R
#' @include class_design.R
#' @include f_core_constants.R
#' @include class_analysis_stage_results.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
AnalysisResultsMultiArm <- setRefClass("AnalysisResultsMultiArm",
    contains = "AnalysisResultsMultiHypotheses",
    fields = list(
        piControl = "matrix" # rates only
    ),
    methods = list(
        initialize = function(design, dataInput, ...) {
            callSuper(design = design, dataInput = dataInput, ...)
            .setParameterType("piControl", C_PARAM_NOT_APPLICABLE)
        },
        .getParametersToShow = function() {
            parametersToShow <- .getVisibleFieldNames()

            if ("piTreatments" %in% parametersToShow && "piControl" %in% parametersToShow) {
                index <- which(parametersToShow == "piTreatments")
                parametersToShow <- parametersToShow[parametersToShow != "piControl"]
                parametersToShow <- c(
                    parametersToShow[1:index],
                    "piControl", parametersToShow[(index + 1):length(parametersToShow)]
                )
            }

            return(parametersToShow)
        }
    )
)

#'
#' @name AnalysisResultsEnrichment
#'
#' @title
#' Basic Class for Analysis Results Enrichment
#'
#' @description
#' A basic class for enrichment analysis results.
#'
#' @details
#' \code{AnalysisResultsEnrichment} is the basic class for
#' \itemize{
#'   \item \code{\link{AnalysisResultsEnrichmentFisher}} and
#'   \item \code{\link{AnalysisResultsEnrichmentInverseNormal}}.
#' }
#'
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_analysis_dataset.R
#' @include class_design.R
#' @include f_core_constants.R
#' @include class_analysis_stage_results.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
AnalysisResultsEnrichment <- setRefClass("AnalysisResultsEnrichment",
    contains = "AnalysisResultsMultiHypotheses",
    fields = list(
        piControls = "matrix" # rates only
    ),
    methods = list(
        initialize = function(design, dataInput, ...) {
            callSuper(design = design, dataInput = dataInput, ...)
            .setParameterType("piControls", C_PARAM_NOT_APPLICABLE)
        }
    )
)

#'
#' @title
#' Analysis Results Summary
#'
#' @description
#' Displays a summary of \code{\link{AnalysisResults}} object.
#'
#' @param object An \code{\link{AnalysisResults}} object.
#' @inheritParams param_digits
#' @inheritParams param_three_dots
#'
#' @details
#' Summarizes the parameters and results of an analysis results object.
#'
#' @template details_summary
#'
#' @template return_object_summary
#' @template how_to_get_help_for_generics
#'
#' @export
#'
#' @keywords internal
#'
summary.AnalysisResults <- function(object, ..., type = 1, digits = NA_integer_) {
    return(summary.ParameterSet(object = object, ..., type = type, digits = digits))
}

#'
#' @title
#' Coerce AnalysisResults to a Data Frame
#'
#' @description
#' Returns the \code{\link{AnalysisResults}} object as data frame.
#'
#' @param x An \code{\link{AnalysisResults}} object created by \code{\link{getAnalysisResults}}.
#' @inheritParams param_three_dots
#'
#' @details
#' Coerces the analysis results to a data frame.
#'
#' @template return_dataframe
#'
#' @export
#'
#' @keywords internal
#'
as.data.frame.AnalysisResults <- function(x, row.names = NULL, optional = FALSE, ..., 
        niceColumnNamesEnabled = FALSE) {
        
    parametersToShow <- .getDesignParametersToShow(x)
    if (inherits(x, "AnalysisResultsMultiArm")) {
        parametersToShow <- c(parametersToShow, ".closedTestResults$rejected")
    }
    parametersToShow <- c(parametersToShow, x$.getUserDefinedParameters())
    parametersToShow <- c(parametersToShow, x$.getDefaultParameters())
    parametersToShow <- c(parametersToShow, x$.getStageResultParametersToShow())
    parametersToShow <- c(parametersToShow, x$.getGeneratedParameters())

    parametersToShow <- parametersToShow[!(parametersToShow %in% c(
        "finalStage", "allocationRatioPlanned", "thetaH0", "thetaH1", "pi1", "pi2"
    ))]
    return(.getAsDataFrame(
        parameterSet = x, 
        parameterNames = parametersToShow,
        tableColumnNames = .getTableColumnNames(design = x$.design),
        niceColumnNamesEnabled = niceColumnNamesEnabled
    ))
}

#'
#' @title
#' Names of a Analysis Results Object
#'
#' @description
#' Function to get the names of an \code{\link{AnalysisResults}} object.
#'
#' @param x An \code{\link{AnalysisResults}} object created by \code{\link{getAnalysisResults}}.
#'
#' @details
#' Returns the names of an analysis results that can be accessed by the user.
#'
#' @template return_names
#'
#' @export
#'
#' @keywords internal
#'
names.AnalysisResults <- function(x) {
    namesToShow <- c(".design", ".dataInput", ".stageResults", ".conditionalPowerResults")
    if (.isMultiArmAnalysisResults(x)) {
        namesToShow <- c(namesToShow, ".closedTestResults")
    }
    namesToShow <- c(namesToShow, x$.getVisibleFieldNames())
    return(namesToShow)
}

#'
#' @name AnalysisResultsGroupSequential
#'
#' @title
#' Analysis Results Group Sequential
#'
#' @description
#' Class for analysis results results based on a group sequential design.
#'
#' @details
#' This object cannot be created directly; use \code{\link{getAnalysisResults}}
#' with suitable arguments to create the analysis results of a group sequential design.
#'
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_analysis_dataset.R
#' @include class_design.R
#' @include f_core_constants.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
AnalysisResultsGroupSequential <- setRefClass("AnalysisResultsGroupSequential",
    contains = "AnalysisResultsBase",
    fields = list(
        maxInformation = "integer",
        informationEpsilon = "numeric"
    ),
    methods = list(
        initialize = function(design, dataInput, ...) {
            callSuper(design = design, dataInput = dataInput, ...)
            .setParameterType("maxInformation", C_PARAM_NOT_APPLICABLE)
            .setParameterType("informationEpsilon", C_PARAM_NOT_APPLICABLE)
        }
    )
)

#'
#' @name AnalysisResultsInverseNormal
#'
#' @title
#' Analysis Results Inverse Normal
#'
#' @description
#' Class for analysis results results based on an inverse normal design.
#'
#' @details
#' This object cannot be created directly; use \code{\link{getAnalysisResults}}
#' with suitable arguments to create the analysis results of a inverse normal design.
#'
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_analysis_dataset.R
#' @include class_design.R
#' @include f_core_constants.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
AnalysisResultsInverseNormal <- setRefClass("AnalysisResultsInverseNormal",
    contains = "AnalysisResultsBase"
)

#'
#' @name AnalysisResultsMultiArmInverseNormal
#'
#' @title
#' Analysis Results Multi-Arm Inverse Normal
#'
#' @description
#' Class for multi-arm analysis results based on a inverse normal design.
#'
#' @details
#' This object cannot be created directly; use \code{\link{getAnalysisResults}}
#' with suitable arguments to create the multi-arm analysis results of an inverse normal design.
#'
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_analysis_dataset.R
#' @include class_design.R
#' @include f_core_constants.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
AnalysisResultsMultiArmInverseNormal <- setRefClass("AnalysisResultsMultiArmInverseNormal",
    contains = "AnalysisResultsMultiArm"
)

#'
#' @name AnalysisResultsEnrichmentInverseNormal
#'
#' @title
#' Analysis Results Enrichment Inverse Normal
#'
#' @description
#' Class for enrichment analysis results based on a inverse normal design.
#'
#' @details
#' This object cannot be created directly; use \code{\link{getAnalysisResults}}
#' with suitable arguments to create the enrichment analysis results of an inverse normal design.
#'
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_analysis_dataset.R
#' @include class_design.R
#' @include f_core_constants.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
AnalysisResultsEnrichmentInverseNormal <- setRefClass("AnalysisResultsEnrichmentInverseNormal",
    contains = "AnalysisResultsEnrichment",
    fields = list(
        stratifiedAnalysis = "logical"
    )
)

#'
#' @name AnalysisResultsFisher
#'
#' @title
#' Analysis Results Fisher
#'
#' @description
#' Class for analysis results based on a Fisher combination test design.
#'
#' @details
#' This object cannot be created directly; use \code{\link{getAnalysisResults}}
#' with suitable arguments to create the analysis results of a Fisher combination test design.
#'
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_analysis_dataset.R
#' @include class_design.R
#' @include f_core_constants.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
AnalysisResultsFisher <- setRefClass("AnalysisResultsFisher",
    contains = "AnalysisResultsBase",
    fields = list(
        conditionalPowerSimulated = "numeric",
        iterations = "integer",
        seed = "numeric"
    ),
    methods = list(
        initialize = function(design, dataInput, ...) {
            callSuper(design = design, dataInput = dataInput, ...)
            conditionalPowerSimulated <<- -1
        }
    )
)

#'
#' @name AnalysisResultsMultiArmFisher
#'
#' @title
#' Analysis Results Multi-Arm Fisher
#'
#' @description
#' Class for multi-arm analysis results based on a Fisher combination test design.
#'
#' @details
#' This object cannot be created directly; use \code{\link{getAnalysisResults}}
#' with suitable arguments to create the multi-arm analysis results of a Fisher combination test design.
#'
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_analysis_dataset.R
#' @include class_design.R
#' @include f_core_constants.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
AnalysisResultsMultiArmFisher <- setRefClass("AnalysisResultsMultiArmFisher",
    contains = "AnalysisResultsMultiArm",
    fields = list(
        conditionalPowerSimulated = "matrix",
        iterations = "integer",
        seed = "numeric"
    )
)

#'
#' @name AnalysisResultsMultiArmFisher
#'
#' @title
#' Analysis Results Multi-Arm Fisher
#'
#' @description
#' Class for multi-arm analysis results based on a Fisher combination test design.
#'
#' @details
#' This object cannot be created directly; use \code{\link{getAnalysisResults}}
#' with suitable arguments to create the multi-arm analysis results of a Fisher combination test design.
#'
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_analysis_dataset.R
#' @include class_design.R
#' @include f_core_constants.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
AnalysisResultsEnrichmentFisher <- setRefClass("AnalysisResultsEnrichmentFisher",
    contains = "AnalysisResultsEnrichment",
    fields = list(
        conditionalPowerSimulated = "matrix",
        iterations = "integer",
        seed = "numeric",
        stratifiedAnalysis = "logical"
    )
)

#'
#' @name AnalysisResultsConditionalDunnett
#'
#' @title
#' Analysis Results Multi-Arm Conditional Dunnett
#'
#' @description
#' Class for multi-arm analysis results based on a conditional Dunnett test design.
#'
#' @details
#' This object cannot be created directly; use \code{\link{getAnalysisResults}}
#' with suitable arguments to create the multi-arm analysis results of a conditional Dunnett test design.
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
AnalysisResultsConditionalDunnett <- setRefClass("AnalysisResultsConditionalDunnett",
    contains = "AnalysisResultsMultiArm",
    fields = list()
)

.getAnalysisResultsPlotArguments <- function(x,
        nPlanned = NA_real_, allocationRatioPlanned = NA_real_) {
    if (all(is.na(nPlanned))) {
        nPlanned <- stats::na.omit(x$nPlanned)
    }

    if (is.na(allocationRatioPlanned) && length(x$allocationRatioPlanned) == 1) {
        allocationRatioPlanned <- x$allocationRatioPlanned
    }

    if (length(allocationRatioPlanned) != 1) {
        allocationRatioPlanned <- NA_real_
    }

    if ((.isConditionalPowerEnabled(x$nPlanned) || .isConditionalPowerEnabled(nPlanned)) && is.na(allocationRatioPlanned)) {
        allocationRatioPlanned <- 1
    }

    return(list(
        stageResults = x$.stageResults,
        nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned
    ))
}

.getConfidenceIntervalPlotLegendLabels <- function(x, treatmentArmsToShow) {
    if (.isEnrichmentAnalysisResults(x)) {
        gMax <- x$.stageResults$getGMax()
        labels <- paste0("S", treatmentArmsToShow)
        labels[treatmentArmsToShow == gMax] <- "F"
        labels <- factor(labels, levels = unique(labels))
        return(labels)
    }

    return(paste0(treatmentArmsToShow, " vs control"))
}

.getConfidenceIntervalData <- function(x, treatmentArmsToShow = NULL) {
    data <- .getConfidenceIntervalDataPerBound(x, "lower", treatmentArmsToShow)
    data$upper <- .getConfidenceIntervalDataPerBound(x, "upper", treatmentArmsToShow)$upper
    data$yValues <- (data$upper + data$lower) / 2
    data <- na.omit(data)
    return(data)
}

.getConfidenceIntervalDataPerBound <- function(x, ciName = c("lower", "upper"), treatmentArmsToShow = NULL) {
    ciName <- match.arg(ciName)
    paramName <- ifelse(ciName == "lower", "repeatedConfidenceIntervalLowerBounds", "repeatedConfidenceIntervalUpperBounds")
    data <- x[[paramName]]

    if (is.matrix(data) && !is.null(treatmentArmsToShow) &&
            length(treatmentArmsToShow) > 0 && !any(is.na(treatmentArmsToShow))) {
        data <- data[treatmentArmsToShow, ]
    }

    if (is.matrix(data) && nrow(data) == 1) {
        data <- as.numeric(data)
    }

    if (is.matrix(data)) {
        kMax <- ncol(data)
        if (is.null(treatmentArmsToShow) || length(treatmentArmsToShow) == 0 || all(is.na(treatmentArmsToShow))) {
            treatmentArmsToShow <- 1:nrow(data)
        }
        groups <- length(treatmentArmsToShow)
        result <- data.frame(ci = data[, 1])
        colnames(result) <- ciName
        result$xValues <- rep(1, groups)
        result$categories <- .getConfidenceIntervalPlotLegendLabels(x, treatmentArmsToShow)
        if (kMax == 1) {
            return(result)
        }

        for (stage in 2:kMax) {
            resultPart <- data.frame(ci = data[, stage])
            colnames(resultPart) <- ciName
            resultPart$xValues <- rep(stage, groups)
            resultPart$categories <- .getConfidenceIntervalPlotLegendLabels(x, treatmentArmsToShow)
            result <- rbind(result, resultPart)
        }
        return(result)
    }

    if (is.null(treatmentArmsToShow) || length(treatmentArmsToShow) == 0 || all(is.na(treatmentArmsToShow))) {
        treatmentArmsToShow <- 1
    }

    kMax <- length(data)
    result <- data.frame(ci = data)
    colnames(result) <- ciName
    result$xValues <- 1:kMax
    result$categories <- rep(.getConfidenceIntervalPlotLegendLabels(x, treatmentArmsToShow), kMax)
    return(result)
}

#'
#' @title
#' Analysis Results Plotting
#'
#' @description
#' Plots the conditional power together with the likelihood function.
#'
#' @param x The analysis results at given stage, obtained from \code{\link{getAnalysisResults}}.
#' @param y Not available for this kind of plot (is only defined to be compatible to the generic plot function).
#' @inheritParams param_nPlanned
#' @inheritParams param_stage
#' @inheritParams param_allocationRatioPlanned
#' @param main The main title, default is \code{"Dataset"}.
#' @param xlab The x-axis label, default is \code{"Stage"}.
#' @param ylab The y-axis label.
#' @param legendTitle The legend title, default is \code{""}.
#' @inheritParams param_palette
#' @inheritParams param_showSource
#' @inheritParams param_plotSettings
#' @inheritParams param_legendPosition
#' @inheritParams param_grid
#' @param type The plot type (default = 1). Note that at the moment only one type (the conditional power plot) is available.
#' @param ... Optional \link[=param_three_dots_plot]{plot arguments}. Furthermore the following arguments can be defined:
#' \itemize{
#' \item \code{thetaRange}: A range of assumed effect sizes if testing means or a survival design was specified.
#'       Additionally, if testing means was selected, \code{assumedStDev} (assumed standard deviation)
#'       can be specified (default is \code{1}).
#' \item \code{piTreatmentRange}: A range of assumed rates pi1 to calculate the conditional power.
#'       Additionally, if a two-sample comparison was selected, \code{pi2} can be specified (default is the value from
#'       \code{getAnalysisResults}).
#' \item \code{directionUpper}: Specifies the direction of the alternative,
#'       only applicable for one-sided testing; default is \code{TRUE}
#'       which means that larger values of the test statistics yield smaller p-values.
#' \item \code{\link[=param_thetaH0]{thetaH0}}: The null hypothesis value, default is \code{0} for
#'       the normal and the binary case, it is \code{1} for the survival case.
#'       For testing a rate in one sample, a value thetaH0 in (0, 1) has to be specified for
#'       defining the null hypothesis H0: \code{pi = thetaH0}.
#' }
#'
#' @details
#' The conditional power is calculated only if effect size and sample size is specified.
#'
#' @template return_object_ggplot
#'
#' @template examples_plot_analysis_results
#'
#' @export
#'
plot.AnalysisResults <- function(x, y, ..., type = 1L,
        nPlanned = NA_real_,
        allocationRatioPlanned = NA_real_,
        main = NA_character_, xlab = NA_character_, ylab = NA_character_,
        legendTitle = NA_character_, palette = "Set1", legendPosition = NA_integer_,
        showSource = FALSE, grid = 1, plotSettings = NULL) {
    .assertGgplotIsInstalled()
    functionCall <- match.call(expand.dots = TRUE)
    analysisResultsName <- as.character(functionCall$x)[1]
    .assertIsSingleInteger(grid, "grid", validateType = FALSE)
    typeNumbers <- .getPlotTypeNumber(type, x)
    p <- NULL
    plotList <- list()
    for (typeNumber in typeNumbers) {
        p <- .plotAnalysisResults(
            x = x, y = y, type = typeNumber,
            nPlanned = nPlanned,
            allocationRatioPlanned = allocationRatioPlanned,
            main = main, xlab = xlab, ylab = ylab,
            legendTitle = legendTitle, palette = palette, legendPosition = legendPosition,
            showSource = showSource, functionCall = functionCall,
            analysisResultsName = analysisResultsName, plotSettings = plotSettings, ...
        )
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

.plotAnalysisResultsRCI <- function(...,
        x, y, nPlanned, allocationRatioPlanned, main, xlab, ylab,
        legendTitle, palette, legendPosition, showSource, analysisResultsName, plotSettings = NULL) {
    .assertIsAnalysisResults(x)
    .warnInCaseOfUnknownArguments(functionName = "plot", ignore = c("treatmentArms", "populations"), ...)

    if (.isEnrichmentAnalysisResults(x)) {
        gMax <- x$.stageResults$getGMax()
        treatmentArmsToShow <- .getPopulationsToShow(x, gMax = gMax, ...)
    } else {
        treatmentArmsToShow <- .getTreatmentArmsToShow(x, ...)
    }

    data <- .getConfidenceIntervalData(x, treatmentArmsToShow)
    if (nrow(data) == 0) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "unable to create plot because no RCIs are available in the specified analysis result"
        )
    }

    .warnInCaseOfUnusedArgument(nPlanned, "nPlanned", NA_real_, "plot")
    .warnInCaseOfUnusedArgument(allocationRatioPlanned, "allocationRatioPlanned", NA_real_, "plot")

    plotData <- list(
        main = "Repeated Confidence Intervals",
        xlab = "Stage",
        ylab = "RCI",
        sub = NA_character_ # subtitle
    )

    if (is.na(legendPosition)) {
        if (!.isMultiHypothesesAnalysisResults(x)) {
            legendPosition <- ifelse(length(treatmentArmsToShow) == 1 && treatmentArmsToShow == 1,
                -1, C_POSITION_RIGHT_CENTER
            )
        } else {
            legendPosition <- C_POSITION_RIGHT_TOP
        }
    }

    treatmentArmsToShowCmd <- ""
    if (!is.null(treatmentArmsToShow) && !identical(sort(unique(treatmentArmsToShow)), 1:nrow(data))) {
        treatmentArmsToShowCmd <- paste0(", ", .arrayToString(treatmentArmsToShow, mode = "vector"))
    }
    dataCmd <- paste0("rpact:::.getConfidenceIntervalData(", analysisResultsName, treatmentArmsToShowCmd, ")")
    srcCmd <- .showPlotSourceInformation(
        objectName = analysisResultsName,
        xParameterName = paste0(dataCmd, "$xValues"),
        yParameterNames = c(
            paste0(dataCmd, "$lower"),
            paste0(dataCmd, "$yValues"),
            paste0(dataCmd, "$upper")
        ),
        type = 2L, showSource = showSource, lineType = FALSE
    )

    p <- .createAnalysisResultsPlotObject(x,
        data = data, plotData = plotData, main = main, xlab = xlab, ylab = ylab,
        legendTitle = legendTitle, palette = palette, legendPosition = legendPosition,
        kMax = x$.design$kMax, plotSettings = plotSettings
    )
    p <- p + ggplot2::expand_limits(x = c(1, x$.design$kMax))
    return(p)
}

.plotAnalysisResults <- function(...,
        x, y, type, nPlanned, allocationRatioPlanned, main, xlab, ylab,
        legendTitle, palette, legendPosition, showSource, functionCall,
        analysisResultsName, plotSettings = NULL) {
    .assertIsSingleInteger(type, "type", naAllowed = FALSE, validateType = FALSE)
    if (!(type %in% c(1, 2))) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'type' (", type, ") is not allowed; must be 1 or 2")
    }

    .assertIsAnalysisResults(x)
    .assertIsValidLegendPosition(legendPosition = legendPosition)

    if (type == 2) {
        return(.plotAnalysisResultsRCI(
            x = x, y = y, nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned,
            main = main, xlab = xlab, ylab = ylab,
            legendTitle = legendTitle, palette = palette,
            legendPosition = legendPosition, showSource = showSource,
            analysisResultsName = analysisResultsName,
            plotSettings = plotSettings, ...
        ))
    }

    if (!.isConditionalPowerEnabled(x$nPlanned) && !.isConditionalPowerEnabled(nPlanned)) {
        stop("'nPlanned' must be defined to create conditional power plot")
    }

    .warnInCaseOfUnknownArguments(
        functionName = "plot",
        ignore = c("thetaRange", "assumedStDev", "assumedStDevs", "treatmentArms", "populations", "pi2", "piTreatmentRange"),
        ...
    )

    if (is.na(legendPosition)) {
        legendPosition <- C_POSITION_RIGHT_CENTER
    }

    plotArgs <- .getAnalysisResultsPlotArguments(
        x = x, nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned
    )

    functionCall$x <- x$.stageResults
    functionCall$y <- NULL
    functionCall$stageResultsName <- paste0(analysisResultsName, "$.stageResults")
    functionCall$nPlanned <- plotArgs$nPlanned
    functionCall$main <- main
    functionCall$xlab <- xlab
    functionCall$ylab <- ylab
    functionCall$legendTitle <- legendTitle
    functionCall$palette <- palette
    functionCall$legendPosition <- legendPosition
    functionCall$type <- type
    functionCall$plotSettings <- plotSettings
    functionCall$allocationRatioPlanned <- plotArgs$allocationRatioPlanned
    if (.isTrialDesignFisher(x$.design)) {
        functionCall$iterations <- x$iterations
        functionCall$seed <- x$seed
    }

    if (x$getDataInput()$isDatasetMeans()) {
        if (.isMultiHypothesesAnalysisResults(x)) {
            assumedStDevs <- eval.parent(functionCall$assumedStDevs)
            if (is.null(assumedStDevs)) {
                assumedStDevs <- as.numeric(x$assumedStDevs)
            }
            functionCall$assumedStDevs <- assumedStDevs
        } else {
            assumedStDev <- eval.parent(functionCall$assumedStDev)
            if (is.null(assumedStDev)) {
                assumedStDev <- x$assumedStDev
            }
            functionCall$assumedStDev <- assumedStDev
        }
    }

    if (x$getDataInput()$isDatasetMeans() || x$getDataInput()$isDatasetSurvival()) {
        thetaRange <- eval.parent(functionCall$thetaRange)
        if (is.null(thetaRange)) {
            thetaRangeMin <- min(x$thetaH0, min(na.omit(as.numeric(x$thetaH1))))
            thetaRangeMax <- 2 * max(x$thetaH0, max(na.omit(as.numeric(x$thetaH1))))
            thetaRange <- seq(
                thetaRangeMin, thetaRangeMax,
                (thetaRangeMax - thetaRangeMin) / C_THETA_RANGE_SEQUENCE_LENGTH_DEFAULT
            )
        } else {
            thetaRange <- .assertIsValidThetaRange(
                thetaRange = thetaRange,
                survivalDataEnabled = x$getDataInput()$isDatasetSurvival()
            )
        }
        functionCall$thetaRange <- thetaRange
    } else if (x$getDataInput()$isDatasetRates()) {
        if (.isMultiArmAnalysisResults(x)) {
            piControl <- eval.parent(functionCall$piControl)
            if (is.null(piControl)) {
                piControl <- as.numeric(x$piControl)
            }
            functionCall$piControl <- piControl
        } else if (.isEnrichmentAnalysisResults(x)) {
            piControl <- eval.parent(functionCall$piControl)
            if (is.null(piControl)) {
                piControls <- as.numeric(x$piControls)
            }
            functionCall$piControls <- piControls
        } else {
            pi2 <- eval.parent(functionCall$pi2)
            if (is.null(pi2)) {
                pi2 <- x$pi2
            }
            functionCall$pi2 <- pi2
        }

        piTreatmentRange <- eval.parent(functionCall$piTreatmentRange)
        if (is.null(piTreatmentRange)) {
            piTreatmentRange <- seq(0, 1, 1 / C_THETA_RANGE_SEQUENCE_LENGTH_DEFAULT) # default
        } else {
            piTreatmentRange <- .assertIsValidPiTreatmentRange(piTreatmentRange = piTreatmentRange)
        }
        functionCall$piTreatmentRange <- piTreatmentRange
    }

    functionCall[[1L]] <- as.name("plot")
    return(eval.parent(functionCall))
}
