## |
## |  *Core assertions*
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

#' @include f_core_utilities.R
NULL

.stopWithWrongDesignMessage <- function(
        design,
        ...,
        inclusiveConditionalDunnett = TRUE) {
    stopIllegalArgument("'design' must be an instance of ",
        .arrayToString(.getTrialDesignClassNames(inclusiveConditionalDunnett = inclusiveConditionalDunnett),
            vectorLookAndFeelEnabled = FALSE
        ), " (is ", .getClassName(design, quote = TRUE), ")",
        functionName = ".stopWithWrongDesignMessage",
        parameter = "design",
        value = design
    )
}

.stopWithWrongDesignMessageEnrichment <- function(
        design,
        ...,
        inclusiveConditionalDunnett = TRUE) {
    trialDesignClassNames <- c(C_CLASS_NAME_TRIAL_DESIGN_INVERSE_NORMAL, C_CLASS_NAME_TRIAL_DESIGN_FISHER)
    stopIllegalArgument("'design' must be an instance of ",
        .arrayToString(trialDesignClassNames, vectorLookAndFeelEnabled = FALSE),
        " (is ", .getClassName(design, quote = TRUE), ")",
        functionName = ".stopWithWrongDesignMessageEnrichment",
        parameter = "design",
        value = design
    )
}

.isParameterSet <- function(x) {
    return(.isResultObjectBaseClass(x) && inherits(x, "ParameterSet"))
}

.assertIsParameterSetClass <- function(x, objectName = "x") {
    if (!.isParameterSet(x)) {
        stopIllegalArgument(.pQuote(objectName), " (", .getClassName(x), ") must be a S4 class ",
            "which inherits from class 'ParameterSet' ",
            functionName = ".assertIsParameterSetClass",
            parameter = "ParameterSet"
        )
    }
}

.assertIsTrialDesignSet <- function(x, objectName = "x") {
    if (!.isTrialDesignSet(x)) {
        stopIllegalArgument("'designSet' must be an instance of 'TrialDesignSet' ",
            "(is ", .getClassName(x, quote = TRUE), ")",
            functionName = ".assertIsTrialDesignSet",
            parameter = "designSet",
            relatedParameter = "TrialDesignSet"
        )
    }
}

.isTrialDesignSet <- function(x) {
    return(identical(.getClassName(x), "TrialDesignSet"))
}

.isTrialDesignFixed <- function(design) {
    return(identical(.getClassName(design), C_CLASS_NAME_TRIAL_DESIGN_FIXED))
}

.isTrialDesignGroupSequential <- function(design) {
    return(identical(.getClassName(design), C_CLASS_NAME_TRIAL_DESIGN_GROUP_SEQUENTIAL))
}

.isTrialDesignGroupSequentialOrFixed <- function(design) {
    return(.isTrialDesignFixed(design) || .isTrialDesignGroupSequential(design))
}

.isTrialDesignInverseNormal <- function(design) {
    return(identical(.getClassName(design), C_CLASS_NAME_TRIAL_DESIGN_INVERSE_NORMAL))
}

.isTrialDesignInverseNormalOrFixed <- function(design) {
    return(.isTrialDesignInverseNormal(design) || .isTrialDesignFixed(design))
}

.isTrialDesignFisher <- function(design) {
    return(identical(.getClassName(design), C_CLASS_NAME_TRIAL_DESIGN_FISHER))
}

.isTrialDesignConditionalDunnett <- function(design) {
    return(identical(.getClassName(design), C_CLASS_NAME_TRIAL_DESIGN_CONDITIONAL_DUNNETT))
}

.isTrialDesignInverseNormalOrGroupSequential <- function(design) {
    return(.isTrialDesignFixed(design) || .isTrialDesignInverseNormal(design) || .isTrialDesignGroupSequential(design))
}

.isTrialDesignInverseNormalOrFisher <- function(design) {
    return(.isTrialDesignInverseNormal(design) || .isTrialDesignFisher(design))
}

.isTrialDesign <- function(design) {
    return(
        .isTrialDesignFixed(design) ||
            .isTrialDesignInverseNormal(design) ||
            .isTrialDesignGroupSequential(design) ||
            .isTrialDesignFisher(design) ||
            .isTrialDesignConditionalDunnett(design)
    )
}

.isTrialDesignPlanMeans <- function(designPlan) {
    return(identical(.getClassName(designPlan), "TrialDesignPlanMeans"))
}

.isTrialDesignPlanRates <- function(designPlan) {
    return(identical(.getClassName(designPlan), "TrialDesignPlanRates"))
}

.isTrialDesignPlanSurvival <- function(designPlan) {
    return(identical(.getClassName(designPlan), "TrialDesignPlanSurvival"))
}

.isTrialDesignPlanCountData <- function(designPlan) {
    return(identical(.getClassName(designPlan), "TrialDesignPlanCountData"))
}

.isTrialDesignPlan <- function(designPlan) {
    return(.isTrialDesignPlanMeans(designPlan) ||
        .isTrialDesignPlanRates(designPlan) ||
        .isTrialDesignPlanSurvival(designPlan) ||
        .isTrialDesignPlanCountData(designPlan))
}

.assertIsTrialDesignPlan <- function(designPlan) {
    if (!.isTrialDesignPlan(designPlan)) {
        stopIllegalArgument("'designPlan' must be an instance of 'TrialDesignPlan' (is ", .getClassName(designPlan, quote = TRUE), ")",
            functionName = ".assertIsTrialDesignPlan",
            parameter = "designPlan",
            value = designPlan
        )
    }
}

.assertIsTrialDesign <- function(design) {
    if (!.isTrialDesign(design)) {
        stopIllegalArgument("'design' must be an instance of ",
            .arrayToString(.getTrialDesignClassNames(), vectorLookAndFeelEnabled = FALSE),
            " (is ", .getClassName(design, quote = TRUE), ")",
            functionName = ".assertIsTrialDesign",
            parameter = "design",
            value = design
        )
    }
}

.assertIsTrialDesignInverseNormal <- function(design) {
    if (!.isTrialDesignInverseNormal(design)) {
        stopIllegalArgument("'design' must be an instance of class ",
            "'TrialDesignInverseNormal' (is ", .getClassName(design, quote = TRUE), ")",
            functionName = ".assertIsTrialDesignInverseNormal",
            parameter = "design",
            value = design
        )
    }
}

.assertIsTrialDesignInverseNormalOrFixed <- function(design) {
    if (!.isTrialDesignInverseNormal(design) && !.isTrialDesignFixed(design)) {
        stopIllegalArgument("'design' must be an instance of class ",
            "'TrialDesignInverseNormal', ", "'TrialDesignFixed' (is ", .getClassName(design, quote = TRUE), ")",
            functionName = ".assertIsTrialDesignInverseNormalOrFixed",
            parameter = "design",
            value = design
        )
    }
}

.assertIsTrialDesignFisher <- function(design) {
    if (!.isTrialDesignFisher(design)) {
        stopIllegalArgument("'design' must be an instance of class ",
            "'TrialDesignFisher' (is ", .getClassName(design, quote = TRUE), ")",
            functionName = ".assertIsTrialDesignFisher",
            parameter = "design",
            value = design
        )
    }
}

.assertIsTrialDesignGroupSequential <- function(design) {
    if (!.isTrialDesignGroupSequential(design)) {
        stopIllegalArgument("'design' must be an instance of class ",
            "'TrialDesignGroupSequential' (is ", .getClassName(design, quote = TRUE), ")",
            functionName = ".assertIsTrialDesignGroupSequential",
            parameter = "design",
            value = design
        )
    }
}

.assertIsTrialDesignGroupSequentialOrFixed <- function(design) {
    if (!.isTrialDesignGroupSequential(design) && !.isTrialDesignFixed(design)) {
        stopIllegalArgument("'design' must be an instance of class 'TrialDesignFixed' or ",
            "'TrialDesignGroupSequential' (is ", .getClassName(design, quote = TRUE), ")",
            functionName = ".assertIsTrialDesignGroupSequentialOrFixed",
            parameter = "design",
            value = design
        )
    }
}

.assertIsTrialDesignConditionalDunnett <- function(design) {
    if (!.isTrialDesignConditionalDunnett(design)) {
        stopIllegalArgument("'design' must be an instance of class ",
            "'TrialDesignConditionalDunnett' (is ", .getClassName(design, quote = TRUE), ")",
            functionName = ".assertIsTrialDesignConditionalDunnett",
            parameter = "design",
            value = design
        )
    }
}

.assertIsTrialDesignInverseNormalOrGroupSequential <- function(design) {
    if (!.isTrialDesignInverseNormalOrGroupSequential(design)) {
        stopIllegalArgument("'design' must be an instance of class ",
            "'TrialDesignInverseNormal' or 'TrialDesignGroupSequential' (is ", .getClassName(design, quote = TRUE), ")",
            functionName = ".assertIsTrialDesignInverseNormalOrGroupSequential",
            parameter = "design",
            value = design
        )
    }
}

.assertIsTrialDesignInverseNormalOrGroupSequentialOrFixed <- function(design) {
    if (!.isTrialDesignInverseNormalOrGroupSequential(design) && !.isTrialDesignFixed(design)) {
        stopIllegalArgument("'design' must be an instance of class ",
            "'TrialDesignInverseNormal', ", "'TrialDesignGroupSequential', or ",
            "'TrialDesignFixed' (is ", .getClassName(design, quote = TRUE), ")",
            functionName = ".assertIsTrialDesignInverseNormalOrGroupSequentialOrFixed",
            parameter = "design",
            value = design
        )
    }
}

.isTrialDesignInverseNormalOrGroupSequentialOrFisher <- function(design) {
    return(.isTrialDesignInverseNormalOrGroupSequential(design) || .isTrialDesignFisher(design))
}

.isTrialDesignInverseNormalOrGroupSequentialOrFixed <- function(design) {
    return(.isTrialDesignInverseNormalOrGroupSequential(design) || .isTrialDesignFixed(design))
}

.assertIsTrialDesignInverseNormalOrGroupSequentialOrFisher <- function(design) {
    if (!.isTrialDesignInverseNormalOrGroupSequentialOrFisher(design)) {
        stopIllegalArgument("'design' must be an instance of class ",
            "'TrialDesignInverseNormal', ", "'TrialDesignGroupSequential', or ",
            "'TrialDesignFisher' (is ", .getClassName(design, quote = TRUE), ")",
            functionName = ".assertIsTrialDesignInverseNormalOrGroupSequentialOrFisher",
            parameter = "design",
            value = design
        )
    }
}

.assertIsTrialDesignInverseNormalOrGroupSequentialOrFisherOrFixed <- function(design) {
    if (!.isTrialDesignInverseNormalOrGroupSequentialOrFisher(design) &&
            !.isTrialDesignFixed(design)) {
        stopIllegalArgument("'design' must be an instance of class ",
            "'TrialDesignInverseNormal', ", "'TrialDesignGroupSequential', ",
            "'TrialDesignFisher', or ", "'TrialDesignFixed' (is ", .getClassName(design, quote = TRUE), ")",
            functionName = ".assertIsTrialDesignInverseNormalOrGroupSequentialOrFisherOrFixed",
            parameter = "design",
            value = design
        )
    }
}

.assertIsTrialDesignInverseNormalOrFisherOrFixed <- function(design) {
    if (!.isTrialDesignInverseNormalOrFisher(design) &&
            !.isTrialDesignFixed(design)) {
        stopIllegalArgument("'design' must be an instance of class ",
            "'TrialDesignInverseNormal', ", "'TrialDesignFisher', or ",
            "'TrialDesignFixed' (is ", .getClassName(design, quote = TRUE), ")",
            functionName = ".assertIsTrialDesignInverseNormalOrFisherOrFixed",
            parameter = "design",
            value = design
        )
    }
}

.assertIsTrialDesignInverseNormalOrFisherOrConditionalDunnettOrFixed <- function(design) {
    if (!.isTrialDesignInverseNormalOrFisher(design) &&
            !.isTrialDesignConditionalDunnett(design) &&
            !.isTrialDesignFixed(design)) {
        stopIllegalArgument("'design' must be an instance of class 'TrialDesignInverseNormal', ",
            "'TrialDesignFisher', 'TrialDesignConditionalDunnett', or 'TrialDesignFixed' (is ", .getClassName(design, quote = TRUE), ")",
            functionName = ".assertIsTrialDesignInverseNormalOrFisherOrConditionalDunnettOrFixed",
            parameter = "design",
            value = design
        )
    }
}

.isSimulationResults <- function(simulationResults) {
    return(inherits(simulationResults, "SimulationResults"))
}

.assertIsSimulationResults <- function(simulationResults) {
    if (!.isSimulationResults(simulationResults)) {
        stopIllegalArgument("'simulationResults' must be an instance of SimulationResults ",
            "(is ", .getClassName(simulationResults, quote = TRUE), ")",
            functionName = ".assertIsSimulationResults",
            parameter = "simulationResults",
            value = simulationResults
        )
    }
}

.isStageResults <- function(stageResults) {
    return(inherits(stageResults, "StageResults"))
}

.isStageResultsMultiArmMeans <- function(stageResults) {
    return(.getClassName(stageResults) == "StageResultsMultiArmMeans")
}

.isStageResultsMultiArmSurvival <- function(stageResults) {
    return(.getClassName(stageResults) == "StageResultsMultiArmSurvival")
}

.isStageResultsEnrichmentMeans <- function(stageResults) {
    return(.getClassName(stageResults) == "StageResultsEnrichmentMeans")
}

.isStageResultsEnrichmentSurvival <- function(stageResults) {
    return(.getClassName(stageResults) == "StageResultsEnrichmentSurvival")
}

.assertIsStageResults <- function(stageResults) {
    if (!.isStageResults(stageResults)) {
        stopIllegalArgument("'stageResults' ", "must be a 'StageResults' object",
            " (is ", .getClassName(stageResults, quote = TRUE), ")",
            functionName = ".assertIsStageResults",
            parameter = "stageResults",
            value = stageResults
        )
    }
}

#'
#' Assert Values Are in a Closed Interval
#'
#' @description
#' Checks if every element of \code{x} is within the closed interval defined by \code{lower} and \code{upper}.
#' If any element is outside the interval, an error is thrown.
#'
#' @param x Numeric vector. The values to be checked.
#' @param xName Character. Used to label \code{x} in error messages.
#' @param ... Additional arguments (unused).
#' @param lower Numeric. The inclusive lower bound of the interval.
#' @param upper Numeric. The inclusive upper bound of the interval. If \code{NULL} or \code{NA}, only the lower bound is enforced.
#' @param naAllowed Logical. Indicates if \code{NA} values are permitted. Default is \code{FALSE}.
#' @param call. Logical. If \code{TRUE} the error message will include the call. Default is \code{FALSE}.
#'
#' @return Invisibly returns \code{x} if all elements are within the specified interval.
#'
#' @examples
#' \dontrun{
#' .assertIsInClosedInterval(1:10, "x", lower = 1, upper = 10)
#' .assertIsInClosedInterval(c(1, NA, 5), "x", lower = 1, upper = 10, naAllowed = TRUE)
#' }

#'
#' @noRd
#'
.assertIsInClosedInterval <- function(
        x,
        xName,
        ...,
        lower,
        upper,
        naAllowed = FALSE,
        call. = FALSE) {
    .warnInCaseOfUnknownArguments(functionName = ".assertIsInClosedInterval", ...)
    if (naAllowed && all(is.na(x))) {
        return(invisible())
    }

    functionName <- paste(deparse(sys.call()), collapse = "")
    .assertIsNumericVector(x, xName, naAllowed = naAllowed, 
		functionName = functionName, call. = call.)

    if (is.null(upper) || is.na(upper)) {
        if (any(x < lower, na.rm = TRUE)) {
            prefix <- ifelse(length(x) > 1, "each value of ", "")
            stopArgumentOutOfBounds(prefix, sQuote(xName), " (", .arrayToString(x), ") must be >= ", lower,
                parameter = xName,
                value = x, constraint = paste0(sQuote(xName), " >= ", lower),
                functionName = functionName, lowerBound = lower,
                upperBound = NULL
            )
        }
    } else if (any(x < lower, na.rm = TRUE) || any(x > upper, na.rm = TRUE)) {
        stopArgumentOutOfBounds(sQuote(xName), " (", .arrayToString(x), ") is out of bounds [", lower, "; ",
            upper, "]",
            parameter = xName, value = x, constraint = paste0(
                sQuote(xName), " >= ", lower, " and ",
                sQuote(xName), " <= ", upper
            ), 
		functionName = functionName, lowerBound = lower, upperBound = upper
        )
    }
}

#' Assert Values Are in an Open Interval
#'
#' @description
#' Checks if every element of \code{x} is within the open
#' interval defined by \code{lower} and \code{upper}.
#' If any element is outside the interval (i.e., less than
#' or equal to \code{lower} or greater than or equal to \code{upper}),
#' an error is thrown.
#'
#' @param x Numeric vector. The values to be checked.
#' @param xName Character. Label used for \code{x} in error messages.
#' @param lower Numeric. The exclusive lower bound of the interval.
#' @param upper Numeric. The exclusive upper bound of the interval.
#' @param naAllowed Logical. Indicates if \code{NA} values are permitted. Default is \code{FALSE}.
#' @param call. Logical. If \code{TRUE}, the error message will
#'        include the original function call. Default is \code{FALSE}.
#'
#' @return Invisibly returns \code{x} if all elements are within the specified open interval.
#'
#' @examples
#' \dontrun{
#' .assertIsInOpenInterval(1:10, "x", lower = 0, upper = 11)
#' }

#'
#' @noRd
#'
.assertIsInOpenInterval <- function(x, xName, ..., lower, upper, naAllowed = FALSE) {
    if (naAllowed && all(is.na(x))) {
        return(invisible())
    }

    functionName <- paste(deparse(sys.call()), collapse = "")

    if (!naAllowed && length(x) > 1 && anyNA(x)) {
        stopIllegalArgument(.pQuote(xName), " (", .arrayToString(x), ") ",
            "must be a valid numeric vector or a single NA",
            functionName = functionName,
            parameter = xName,
            value = x
        )
    }

    if (is.null(upper) || is.na(upper)) {
        if (any(x <= lower, na.rm = TRUE)) {
            prefix <- ifelse(length(x) > 1, "each value of ", "")
            stopArgumentOutOfBounds(prefix, .pQuote(xName), " (", .arrayToString(x), ") must be > ", lower,
                functionName = functionName,
                parameter = xName,
                value = x,
                constraint = paste0(sQuote(xName), " > ", lower),
                lowerBound = lower,
                upperBound = NULL
            )
        }
    } else if (any(x <= lower, na.rm = TRUE) || any(x >= upper, na.rm = TRUE)) {
        stopArgumentOutOfBounds(.pQuote(xName), " (", .arrayToString(x),
            ") is out of bounds (", lower, "; ", upper, ")",
            functionName = functionName,
            parameter = xName,
            value = x,
            constraint = paste0(sQuote(xName), " > ", lower, " and ", sQuote(xName), " < ", upper),
            lowerBound = lower,
            upperBound = upper
        )
    }
}

.assertIsValidDataInput <- function(dataInput, design = NULL, stage = NULL) {
    .assertIsDataset(dataInput)
    if (!is.null(design)) {
        .assertIsTrialDesign(design)
    }

    if (dataInput$.enrichmentEnabled && dataInput$getNumberOfGroups() != 2) {
        stopIllegalDataInput("only population enrichment data with 2 groups can be analyzed but ",
            dataInput$getNumberOfGroups(),
            " group", ifelse(dataInput$getNumberOfGroups() == 1, " is", "s are"), " defined",
            functionName = ".assertIsValidDataInput"
        )
    }

    stages <- dataInput$stages
    l1 <- length(stages)
    for (fieldName in dataInput$.getVisibleFieldNames()) {
        l2 <- length(dataInput[[fieldName]])
        if (fieldName != "stages" && l1 != l2) {
            stopIllegalDataInput("all parameters must have the same length ",
                "('stages' has length ", l1, ", ", .pQuote(fieldName), " has length ", l2, ")",
                functionName = ".assertIsValidDataInput",
                parameter = "stages"
            )
        }
    }

    if (!is.null(stage)) {
        if (dataInput$getNumberOfGroups() == 1) {
            if (.isDatasetMeans(dataInput)) {
                if (any(na.omit(dataInput$getStDevsUpTo(stage)) <= 0)) {
                    stopIllegalDataInput("all standard deviations must be > 0",
                        functionName = ".assertIsValidDataInput"
                    )
                }
                if (any(na.omit(dataInput$getSampleSizesUpTo(stage)) <= 0)) {
                    stopIllegalDataInput("all sample sizes must be > 0",
                        functionName = ".assertIsValidDataInput"
                    )
                }
            } else if (.isDatasetRates(dataInput)) {
                if (any(na.omit(dataInput$getEventsUpTo(stage)) < 0)) {
                    stopIllegalDataInput("all events must be >= 0",
                        functionName = ".assertIsValidDataInput"
                    )
                }
                if (any(na.omit(dataInput$getSampleSizesUpTo(stage)) <= 0)) {
                    stopIllegalDataInput("all sample sizes must be > 0",
                        functionName = ".assertIsValidDataInput"
                    )
                }
                if (any(na.omit(dataInput$getEventsUpTo(stage)) >
                        na.omit(dataInput$getSampleSizesUpTo(stage)))) {
                    stopIllegalDataInput("all events must be <= corresponding sample size",
                        functionName = ".assertIsValidDataInput"
                    )
                }
            }
        } else if (dataInput$getNumberOfGroups() == 2) {
            if (.isDatasetMeans(dataInput)) {
                if (any(na.omit(dataInput$getStDevsUpTo(stage, 1)) <= 0) ||
                        any(na.omit(dataInput$getStDevsUpTo(stage, 2)) <= 0)) {
                    stopIllegalDataInput("all standard deviations must be > 0",
                        functionName = ".assertIsValidDataInput"
                    )
                }
                if (any(na.omit(dataInput$getSampleSizesUpTo(stage, 1)) <= 0) ||
                        any(na.omit(dataInput$getSampleSizesUpTo(stage, 2)) <= 0)) {
                    stopIllegalDataInput("all sample sizes must be > 0",
                        functionName = ".assertIsValidDataInput"
                    )
                }
            } else if (.isDatasetRates(dataInput)) {
                if (any(na.omit(dataInput$getEventsUpTo(stage, 1)) < 0) ||
                        any(na.omit(dataInput$getEventsUpTo(stage, 2)) < 0)) {
                    stopIllegalDataInput("all events must be >= 0",
                        functionName = ".assertIsValidDataInput"
                    )
                }
                if (any(na.omit(dataInput$getSampleSizesUpTo(stage, 1)) <= 0) ||
                        any(na.omit(dataInput$getSampleSizesUpTo(stage, 2)) <= 0)) {
                    stopIllegalDataInput("all sample sizes must be > 0",
                        functionName = ".assertIsValidDataInput"
                    )
                }
                if (any(na.omit(dataInput$getEventsUpTo(stage, 1)) > na.omit(dataInput$getSampleSizesUpTo(stage, 1))) ||
                        any(na.omit(dataInput$getEventsUpTo(stage, 2)) > na.omit(dataInput$getSampleSizesUpTo(stage, 2)))) {
                    stopIllegalDataInput("all events must be <= corresponding sample size",
                        functionName = ".assertIsValidDataInput"
                    )
                }
            }
        }

        if (.isDatasetSurvival(dataInput)) {
            if (any(na.omit(dataInput$getOverallEventsUpTo(stage)) < 0)) {
                stopIllegalDataInput("all cumulative events must be >= 0",
                    functionName = ".assertIsValidDataInput"
                )
            }

            if (any(na.omit(dataInput$getOverallAllocationRatiosUpTo(stage)) <= 0)) {
                stopIllegalDataInput("all cumulative allocation ratios must be > 0",
                    functionName = ".assertIsValidDataInput"
                )
            }
        }
    }

    if (!is.null(design)) {
        numberOfStages <- length(unique(stats::na.omit(stages)))
        kMax <- design$kMax
        if (numberOfStages > kMax) {
            s <- numberOfStages - kMax
            plural <- ifelse(s == 1, "", "s")
            warning(sprintf(
                paste0(
                    "The data of the last %s in the dataset will be ",
                    "ignored because the design has specified kMax = %s"
                ),
                ifelse(s == 1, "stage", paste0(s, " stages")), kMax
            ), call. = FALSE)
        } else if (numberOfStages < kMax) {
            dataInput$.fillWithNAs(kMax)
        }
    }

    invisible(dataInput)
}

.assertIsDataset <- function(dataInput) {
    if (!.isDataset(dataInput)) {
        stopIllegalArgument("'dataInput' must be an instance of class ",
            "'DatasetMeans', 'DatasetRates' or 'DatasetSurvival' ",
            "(is ", .getClassName(dataInput, quote = TRUE), ")",
            functionName = ".assertIsDataset",
            parameter = "dataInput",
            value = dataInput
        )
    }
}

.assertIsDatasetMeans <- function(dataInput) {
    if (!.isDatasetMeans(dataInput = dataInput)) {
        stopIllegalArgument("'dataInput' must be an instance of class ",
            "'DatasetMeans' (is ", .getClassName(dataInput, quote = TRUE), ")",
            functionName = ".assertIsDatasetMeans",
            parameter = "dataInput",
            value = dataInput
        )
    }
}

.assertIsDatasetRates <- function(dataInput) {
    if (!.isDatasetRates(dataInput = dataInput)) {
        stopIllegalArgument("'dataInput' must be an instance of class ",
            "'DatasetRates' (is ", .getClassName(dataInput, quote = TRUE), ")",
            functionName = ".assertIsDatasetRates",
            parameter = "dataInput",
            value = dataInput
        )
    }
}

.assertIsDatasetSurvival <- function(dataInput) {
    if (!.isDatasetSurvival(dataInput = dataInput)) {
        stopIllegalArgument("'dataInput' must be an instance of class ",
            "'DatasetSurvival' or 'DatasetEnrichmentSurvival' ",
            "(is ", .getClassName(dataInput, quote = TRUE), ")",
            functionName = ".assertIsDatasetSurvival",
            parameter = "dataInput",
            value = dataInput
        )
    }
}

.isDataset <- function(dataInput) {
    return(.isDatasetMeans(dataInput) ||
        .isDatasetRates(dataInput) ||
        .isDatasetSurvival(dataInput))
}

.isDatasetMeans <- function(dataInput) {
    return(inherits(dataInput, "DatasetMeans"))
}

.isDatasetRates <- function(dataInput) {
    return(inherits(dataInput, "DatasetRates"))
}

.isDatasetSurvival <- function(dataInput) {
    return(inherits(dataInput, "DatasetSurvival") ||
        inherits(dataInput, "DatasetEnrichmentSurvival"))
}

.assertIsNumericVector <- function(
        x,
        argumentName,
        ...,
        naAllowed = FALSE,
        noDefaultAvailable = FALSE,
        len = NA_integer_,
        call. = FALSE) {
    functionName <- paste(deparse(sys.call()), collapse = "")
    argumentValue <- NULL
    if (!missing(x)) {
        argumentValue <- x
    }

    if (missing(x) || is.null(x) || length(x) == 0) {
        .assertIsNoDefault(argumentValue, argumentName, noDefaultAvailable, checkNA = FALSE)
        stopMissingArgument(sQuote(argumentName), " must be a valid numeric value or vector",
            parameter = argumentName,
            constraint = "must be a valid numeric value or vector",
            functionName = functionName,
            value = if (!missing(x)) x else NULL
        )
    }

    .assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = TRUE)

    illegalMatrix <- !all(is.na(x)) && is.matrix(x) && all(dim(x) > 1)
    if (illegalMatrix || (!naAllowed && anyNA(x)) || !is.numeric(x)) {
        stopIllegalArgument(sQuote(argumentName), " (", .arrayToString(x), ") must be a valid numeric value or vector",
            ifelse(!is.vector(x), paste0(" (is ", .getClassName(x), ")"), ""),
            parameter = argumentName, value = x,
            constraint = "must be a valid numeric value or vector",
            functionName = functionName,
            relatedParameter = paste0("class of ", argumentName),
            relatedValue = .getClassName(x)
        )
    }

    if (!anyNA(len) && !length(x) %in% len) {
        stopIllegalArgument(sQuote(argumentName), " (", .arrayToString(x), ") must have length ",
            .arrayToString(len, mode = "or"),
            parameter = argumentName,
            value = x,
            constraint = paste0("length must be ", .arrayToString(len,
                mode = "or"
            )),
            functionName = functionName,
            relatedParameter = "expected length of argument",
            relatedValue = .arrayToString(len, mode = "or")
        )
    }

    return(invisible(as.numeric(x)))
}

.assertIsIntegerVector <- function(
        x,
        argumentName,
        ...,
        naAllowed = FALSE,
        validateType = TRUE,
        noDefaultAvailable = FALSE,
        call. = FALSE) {
    if (missing(x) || is.null(x) || length(x) == 0) {
        .assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = FALSE)
        stopMissingArgument(.pQuote(argumentName), " must be a valid integer value or vector",
            functionName = ".assertIsIntegerVector",
            parameter = argumentName,
            value = if (!missing(x)) x else NULL
        )
    }

    .assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = TRUE)

    if (naAllowed && all(is.na(x))) {
        return(invisible(if (all(is.finite(x))) as.integer(x) else x))
    }

    if (!is.numeric(x) || (!naAllowed && anyNA(x)) || (validateType && !is.integer(x)) ||
            (!validateType && any(as.integer(na.omit(x)) != na.omit(x)))) {
        stopIllegalArgument(.pQuote(argumentName), " (", .arrayToString(x),
            ") must be a valid integer value or vector",
            functionName = ".assertIsIntegerVector",
            parameter = argumentName,
            value = x
        )
    }

    return(invisible(if (all(is.finite(x))) as.integer(x) else x))
}

.assertIsLogicalVector <- function(
        x,
        argumentName,
        ...,
        naAllowed = FALSE,
        noDefaultAvailable = FALSE,
        call. = FALSE) {
    if (missing(x) || is.null(x) || length(x) == 0) {
        .assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = FALSE)
        stopMissingArgument(.pQuote(argumentName), " ", "must be a valid logical value or vector",
            functionName = ".assertIsLogicalVector",
            parameter = argumentName,
            value = if (!missing(x)) x else NULL
        )
    }

    .assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = TRUE)

    if ((!naAllowed && anyNA(x)) || !is.logical(x)) {
        stopIllegalArgument(.pQuote(argumentName), " (", x, ") ",
            "must be a valid logical value or vector",
            functionName = ".assertIsLogicalVector",
            parameter = argumentName,
            value = x
        )
    }

    return(invisible(as.logical(x)))
}

.assertIsNoDefault <- function(x, argumentName, noDefaultAvailable, ..., checkNA = FALSE) {
    if (noDefaultAvailable && (!checkNA || all(is.na(x)))) {
        stopMissingArgument(.pQuote(argumentName), " ",
            "must be specified, there is no default value",
            functionName = ".assertIsNoDefault",
            parameter = argumentName,
            value = if (!missing(x)) x else NULL
        )
    }
}

.assertIsSingleLogical <- function(x, argumentName, ..., naAllowed = FALSE, noDefaultAvailable = FALSE) {
    if (missing(x) || is.null(x) || length(x) == 0) {
        .assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = FALSE)
        stopMissingArgument(.pQuote(argumentName), " ",
            "must be a single logical value",
            functionName = ".assertIsSingleLogical",
            parameter = argumentName,
            value = if (!missing(x)) x else NULL
        )
    }

    .assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = TRUE)

    if (length(x) > 1) {
        stopIllegalArgument(.pQuote(argumentName), " ",
            .arrayToString(x, vectorLookAndFeelEnabled = TRUE), " must be a single logical value",
            functionName = ".assertIsSingleLogical",
            parameter = argumentName,
            value = x
        )
    }

    if ((!naAllowed && is.na(x)) || !is.logical(x)) {
        stopIllegalArgument(.pQuote(argumentName), " (",
            ifelse(.isResultObjectBaseClass(x), .getClassName(x), x),
            ") must be a single logical value",
            functionName = ".assertIsSingleLogical",
            parameter = argumentName,
            value = x
        )
    }

    return(invisible(as.logical(x)))
}

.assertIsSingleNumber <- function(
        x,
        argumentName,
        ...,
        naAllowed = FALSE,
        noDefaultAvailable = FALSE,
        call. = FALSE) {
    if (missing(x) || is.null(x) || length(x) == 0) {
        .assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = FALSE)
        stopMissingArgument(.pQuote(argumentName), " must be a valid numeric value",
            functionName = ".assertIsSingleNumber",
            parameter = argumentName,
            value = if (!missing(x)) x else NULL
        )
    }

    .assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = TRUE)

    if (length(x) > 1) {
        stopIllegalArgument(.pQuote(argumentName), " ",
            .arrayToString(x, vectorLookAndFeelEnabled = TRUE),
            " must be a single numeric value",
            functionName = ".assertIsSingleNumber",
            parameter = argumentName,
            value = x
        )
    }

    if ((!naAllowed && is.na(x)) || !is.numeric(x)) {
        stopIllegalArgument(.pQuote(argumentName), " (",
            ifelse(.isResultObjectBaseClass(x), .getClassName(x), x), ") must be a valid numeric value",
            functionName = ".assertIsSingleNumber",
            parameter = argumentName,
            value = x
        )
    }

    return(invisible(as.numeric(x)))
}

.assertIsSingleInteger <- function(
        x,
        argumentName,
        ...,
        naAllowed = FALSE,
        validateType = TRUE,
        noDefaultAvailable = FALSE,
        call. = FALSE) {
    return(invisible(.assertIsSinglePositiveInteger(
        x = x,
        argumentName = argumentName,
        naAllowed = naAllowed,
        validateType = validateType,
        mustBePositive = FALSE,
        noDefaultAvailable = noDefaultAvailable,
        call. = call.
    )))
}

.assertIsSinglePositiveInteger <- function(
        x,
        argumentName,
        ...,
        naAllowed = FALSE,
        validateType = TRUE,
        mustBePositive = TRUE,
        noDefaultAvailable = FALSE,
        call. = FALSE) {
    prefix <- ifelse(mustBePositive, "single positive ", "single ")
    if (missing(x) || is.null(x) || length(x) == 0) {
        .assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = FALSE)
        stopMissingArgument(.pQuote(argumentName), " must be a ", prefix, "integer value",
            functionName = ".assertIsSinglePositiveInteger",
            parameter = argumentName,
            relatedParameter = "prefix",
            relatedValue = prefix,
            value = if (!missing(x)) x else NULL
        )
    }

    .assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = TRUE)

    if (length(x) > 1) {
        stopIllegalArgument(.pQuote(argumentName), " ",
            .arrayToString(x, vectorLookAndFeelEnabled = TRUE), " must be a ",
            prefix, "integer value",
            functionName = ".assertIsSinglePositiveInteger",
            parameter = argumentName,
            value = x
        )
    }

    if (!is.numeric(x) || (!naAllowed && is.na(x)) || (validateType && !is.integer(x)) ||
            (!validateType && !is.na(x) && !is.infinite(x) && as.integer(x) != x)) {
        stopIllegalArgument(.pQuote(argumentName), " (",
            ifelse(.isResultObjectBaseClass(x), .getClassName(x), x),
            ") must be a ", prefix, "integer value",
            functionName = ".assertIsSinglePositiveInteger",
            parameter = argumentName,
            value = x
        )
    }

    if (mustBePositive && !is.na(x) && !is.infinite(x) && x <= 0) {
        stopIllegalArgument(.pQuote(argumentName), " (",
            ifelse(.isResultObjectBaseClass(x), .getClassName(x), x), ") must be a ", prefix,
            "integer value",
            functionName = ".assertIsSinglePositiveInteger",
            parameter = argumentName,
            value = x
        )
    }

    return(invisible(if (all(is.finite(x))) as.integer(x) else x))
}

.assertIsSingleCharacter <- function(
        x,
        argumentName,
        ...,
        naAllowed = FALSE,
        noDefaultAvailable = FALSE,
        call. = FALSE) {
    if (missing(x) || is.null(x) || length(x) == 0) {
        .assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = FALSE)
        stopMissingArgument(.pQuote(argumentName), " must be a valid character value",
            functionName = ".assertIsSingleCharacter",
            parameter = argumentName,
            value = if (!missing(x)) x else NULL
        )
    }

    .assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = TRUE)

    if (length(x) > 1) {
        stopIllegalArgument(.pQuote(argumentName), " ",
            .arrayToString(x, vectorLookAndFeelEnabled = TRUE), " must be a single character value",
            functionName = ".assertIsSingleCharacter",
            parameter = argumentName,
            value = x
        )
    }

    if (!is.character(x)) {
        stopIllegalArgument(
            sprintf(
                "'%s' must be a valid character value (is an instance of class '%s')", argumentName,
                .getClassName(x)
            ),
            parameter = argumentName,
            value = x,
            constraint = "must be a valid character value",
            functionName = ".assertIsSingleCharacter"
        )
    }

    if (!naAllowed && is.na(x)) {
        stopIllegalArgument(sprintf("'%s' (NA) must be a valid character value", argumentName),
            parameter = argumentName,
            value = x,
            constraint = "must be a valid character value",
            functionName = ".assertIsSingleCharacter"
        )
    }

    return(invisible(as.character(x)))
}

.assertIsCharacter <- function(x, argumentName, ..., naAllowed = FALSE) {
    if (missing(x) || is.null(x) || length(x) == 0) {
        stopMissingArgument(.pQuote(argumentName), " must be a valid character value or vector",
            functionName = ".assertIsCharacter",
            parameter = argumentName,
            value = if (!missing(x)) x else NULL
        )
    }

    if (!all(is.character(x))) {
        stopIllegalArgument(
            sprintf(
                "'%s' must be a valid character value or vector (is an instance of class '%s')",
                argumentName, .getClassName(x)
            ),
            parameter = argumentName,
            value = x,
            constraint = "must be a valid character value or vector",
            functionName = ".assertIsCharacter"
        )
    }

    if (!naAllowed && anyNA(x)) {
        stopIllegalArgument(
            sprintf(
                "'%s' (%s) must be a valid character value (NA is not allowed)", argumentName,
                .arrayToString(x)
            ),
            parameter = argumentName,
            value = x,
            constraint = "must be a valid character value; NA is not allowed",
            functionName = ".assertIsCharacter"
        )
    }

    return(invisible(as.character(x)))
}

.assertDesignParameterExists <- function(
        design,
        parameterName,
        defaultValue,
        ...,
        relatedParameter = NULL,
        relatedValue = NULL) {
    if (missing(design)) {
        stopMissingArgument("'design' must be defined",
            functionName = ".assertDesignParameterExists",
            parameter = "design"
        )
    }

    if (missing(parameterName)) {
        stopMissingArgument("'parameterName' must be defined",
            functionName = ".assertDesignParameterExists",
            parameter = "parameterName"
        )
    }

    if (missing(defaultValue)) {
        stopMissingArgument("'defaultValue' must be defined",
            functionName = ".assertDesignParameterExists",
            parameter = "defaultValue"
        )
    }

    value <- design[[parameterName]]
    if (is.null(value) || length(value) == 0 || all(is.na(value))) {
        constraint <- NULL
        if (identical(relatedParameter, "typeOfDesign") && !is.null(relatedValue)) {
            constraint <- paste0(
                "must be specified for ", .pQuote(relatedParameter), " = ", .pQuote(relatedValue)
            )
        }

        stopMissingArgument("parameter ", .pQuote(parameterName), " must be specified in design",
            functionName = ".assertDesignParameterExists",
            parameter = parameterName,
            relatedParameter = relatedParameter,
            relatedValue = relatedValue,
            constraint = constraint
        )
    }

    if (is.null(defaultValue) || length(defaultValue) == 0 || all(is.na(defaultValue))) {
        design$.setParameterType(parameterName, C_PARAM_USER_DEFINED)
        return(invisible())
    }

    if (all(value == defaultValue)) {
        design$.setParameterType(parameterName, C_PARAM_DEFAULT_VALUE)
    } else {
        design$.setParameterType(parameterName, C_PARAM_USER_DEFINED)
    }
}

.designParameterExists <- function(design, parameterName) {
    if (missing(design)) {
        stopMissingArgument("'design' must be defined",
            functionName = ".designParameterExists",
            parameter = "design"
        )
    }

    if (missing(parameterName)) {
        stopMissingArgument("'parameterName' must be defined",
            functionName = ".designParameterExists",
            parameter = "parameterName"
        )
    }

    value <- design[[parameterName]]
    if (is.null(value)) {
        return(FALSE)
    }

    if (length(value) > 1) {
        return(sum(is.na(value)) < length(value))
    }

    return(!is.na(value))
}

.assertIsOptimizationCriterion <- function(x) {
    if (!.isOptimizationCriterion(x)) {
        stopIllegalArgument("optimization criterion must be one of the following: ", .printOptimizationCriterion(),
            functionName = ".assertIsOptimizationCriterion"
        )
    }
}

.showParameterOutOfValidatedBoundsMessage <- function(
        parameterValue,
        parameterName,
        ...,
        lowerBound = NA_real_,
        upperBound = NA_real_,
        spendingFunctionName = NA_character_,
        closedLowerBound = TRUE,
        closedUpperBound = TRUE,
        suffix = NA_character_,
        naAllowed = FALSE) {
    if (naAllowed && is.na(parameterValue)) {
        return(invisible())
    }

    .assertIsSingleNumber(lowerBound, "lowerBound", naAllowed = TRUE)
    .assertIsSingleNumber(upperBound, "upperBound", naAllowed = TRUE)
    if (is.na(lowerBound) && is.na(upperBound)) {
        stopMissingArgument("'lowerBound' or 'upperBound' must be defined",
            functionName = ".showParameterOutOfValidatedBoundsMessage",
            parameter = "lowerBound",
            relatedParameter = "upperBound",
            value = lowerBound
        )
    }

    if (is.na(lowerBound)) {
        lowerBound <- -Inf
    }

    if (is.na(upperBound)) {
        upperBound <- Inf
    }

    if (closedLowerBound) {
        bracketLowerBound <- "["
        conditionLowerBound <- parameterValue < lowerBound
    } else {
        bracketLowerBound <- "("
        conditionLowerBound <- parameterValue <= lowerBound
    }
    if (closedUpperBound) {
        bracketUpperBound <- "]"
        conditionUpperBound <- parameterValue > upperBound
    } else {
        bracketUpperBound <- ")"
        conditionUpperBound <- parameterValue >= upperBound
    }

    if (conditionLowerBound || conditionUpperBound) {
        if (!is.null(spendingFunctionName) && !is.na(spendingFunctionName)) {
            spendingFunctionName <- paste0("for ", spendingFunctionName, " function ")
        } else {
            spendingFunctionName <- ""
        }

        if (is.na(suffix)) {
            suffix <- ""
        } else {
            suffix <- paste0(" ", trimws(suffix))
        }

        type <- .getEnvironmentVariable(
            "RPACT_OUT_OF_VALIDATED_BOUNDS_MESSAGE_TYPE",
            "rpact.out.of.validated.bounds.message.type",
            default = "warning",
            type = "character"
        )
        if (identical(type, "warning")) {
            warning("The parameter ", sQuote(parameterName), " (", parameterValue, ") ",
                spendingFunctionName, "is out of validated bounds ",
                bracketLowerBound, lowerBound, "; ", upperBound, bracketUpperBound, suffix,
                call. = FALSE
            )
        } else if (identical(type, "message")) {
            message(
                "Note that parameter ", sQuote(parameterName), " (", parameterValue, ") ",
                spendingFunctionName, "is out of validated bounds ",
                bracketLowerBound, lowerBound, "; ", upperBound, bracketUpperBound, suffix
            )
        }
    }
}

.assertIsValidKappa <- function(kappa) {
    .assertIsSingleNumber(kappa, "kappa")
    .assertIsInOpenInterval(kappa, "kappa", lower = 0, upper = NULL)
}

.assertIsValidLambda <- function(lambda, lambdaNumber = 0) {
    argumentName <- "lambda"
    if (lambdaNumber >= 1) {
        argumentName <- paste0("lambda", lambdaNumber)
    }
    lambda <- .assertIsNumericVector(lambda, argumentName, naAllowed = TRUE)
    if (all(is.na(lambda))) {
        return(invisible(lambda))
    }

    if (anyNA(lambda)) {
        stopIllegalArgument(.pQuote(argumentName), " (", .arrayToString(lambda),
            ") must be a valid numeric vector",
            functionName = ".assertIsValidLambda",
            parameter = argumentName,
            relatedParameter = "lambda",
            relatedValue = lambda
        )
    }

    .assertIsInClosedInterval(lambda, argumentName, lower = 0, upper = NULL)
    if (all(lambda == 0)) {
        stopIllegalArgument(.pQuote(argumentName), " (", .arrayToString(lambda),
            ") not allowed: ", "at least one lambda value must be > 0",
            functionName = ".assertIsValidLambda",
            parameter = argumentName,
            relatedParameter = "lambda",
            relatedValue = lambda
        )
    }

    return(invisible(lambda))
}

.assertIsValidFollowUpTime <- function(followUpTime) {
    if (is.null(followUpTime) || length(followUpTime) == 0 || is.na(followUpTime)) {
        return(invisible())
    }

    .assertIsSingleNumber(followUpTime, "followUpTime", naAllowed = TRUE)
    if (followUpTime < 0) {
        stopIllegalArgument("'followUpTime' (", followUpTime, ") must be >= 0",
            functionName = ".assertIsValidFollowUpTime",
            parameter = "followUpTime", value = followUpTime
        )
    }
}

.assertIsValidAccrualTime <- function(accrualTime, ..., naAllowed = TRUE) {
    accrualTime <- .assertIsNumericVector(accrualTime, "accrualTime", naAllowed = naAllowed)

    if (length(accrualTime) == 0 || all(is.na(accrualTime))) {
        return(invisible(accrualTime))
    }

    if (length(accrualTime) > 1 && accrualTime[1] != 0) {
        stopIllegalArgument("the first value of 'accrualTime' ",
            "(", .arrayToString(accrualTime), ") must be 0",
            functionName = ".assertIsValidAccrualTime",
            parameter = "accrualTime",
            value = accrualTime
        )
    }

    if (identical(accrualTime, 0) || identical(accrualTime, 0L)) {
        stopIllegalArgument("single 'accrualTime' is not allowed to be 0",
            functionName = ".assertIsValidAccrualTime",
            parameter = "accrualTime",
            value = accrualTime
        )
    }

    .assertIsInClosedInterval(accrualTime, "accrualTime",
        lower = 0, upper = NULL, naAllowed = naAllowed
    )
    .assertValuesAreStrictlyIncreasing(accrualTime, "accrualTime")

    return(invisible(accrualTime))
}

.assertIsValidStandardDeviation <- function(
        stDev,
        groups = 1L,
        ...,
        name = "stDev",
        naAllowed = TRUE) {
    if (groups == 1L) {
        stDev <- .assertIsSingleNumber(stDev, name, naAllowed = naAllowed)
    } else {
        stDev <- .assertIsNumericVector(stDev, name,
            len = unique(c(1L, groups)), naAllowed = naAllowed
        )
    }

    if (naAllowed && all(is.na(stDev))) {
        return(invisible(stDev))
    }

    if (any(stDev <= 0)) {
        stopArgumentOutOfBounds("standard deviation ", .pQuote(name), " ",
            "(", .arrayToString(stDev), ") must be > 0",
            functionName = ".assertIsValidStandardDeviation",
            parameter = "name",
            value = name,
            relatedParameter = "stDev",
            relatedValue = stDev
        )
    }

    return(invisible(stDev))
}

.assertIsValidAlpha <- function(alpha, ..., naAllowed = FALSE) {
    .assertIsSingleNumber(alpha, "alpha", naAllowed = naAllowed)
    .assertIsInOpenInterval(alpha, "alpha", lower = 0, upper = 1, naAllowed = naAllowed)
    .showParameterOutOfValidatedBoundsMessage(alpha, "alpha",
        lowerBound = 1e-06, upperBound = 0.5,
        closedUpperBound = FALSE, naAllowed = naAllowed
    )
}

.assertIsValidBeta <- function(beta, alpha, ..., naAllowed = FALSE) {
    .assertIsSingleNumber(beta, "beta", naAllowed = naAllowed)
    .assertIsSingleNumber(alpha, "alpha", naAllowed = naAllowed)
    .assertIsInOpenInterval(beta, "beta", lower = 0, upper = 1, naAllowed = naAllowed)
    .showParameterOutOfValidatedBoundsMessage(beta, "beta",
        lowerBound = 1e-04,
        upperBound = 1 - alpha, closedUpperBound = FALSE,
        suffix = "condition: 1e-06 <= alpha < 1 - beta <= 1 - 1e-04",
        naAllowed = naAllowed
    )
}

.assertIsValidAlphaAndBeta <- function(alpha, beta, ..., naAllowed = FALSE) {
    .assertIsValidAlpha(alpha, naAllowed = naAllowed)
    .assertIsValidBeta(beta, alpha, naAllowed = naAllowed)
}

.assertIsValidStage <- function(stage, kMax) {
    .assertIsSingleNumber(stage, "stage")
    if (stage < 1 || stage > kMax) {
        stopArgumentOutOfBounds("'stage' (", stage, ") is out of bounds [1; ", kMax, "]",
            functionName = ".assertIsValidStage",
            parameter = "stage", value = stage
        )
    }
}

.assertIsValidIterationsAndSeed <- function(iterations, seed, ..., zeroIterationsAllowed = TRUE) {
    .assertIsSingleInteger(iterations, "iterations", validateType = FALSE)

    if (zeroIterationsAllowed) {
        if (iterations < 0) {
            stopIllegalArgument("'iterations' (", iterations, ") must be >= 0",
                functionName = ".assertIsValidIterationsAndSeed",
                parameter = "iterations", value = iterations
            )
        }
    } else {
        if (iterations < 1) {
            stopIllegalArgument("'iterations' (", iterations, ") must be > 0",
                functionName = ".assertIsValidIterationsAndSeed",
                parameter = "iterations", value = iterations
            )
        }
    }

    if (is.null(seed) || length(seed) == 0 || (!is.na(seed) && !is.numeric(seed))) {
        stopIllegalArgument("'seed' (", seed, ") must be a valid integer value",
            functionName = ".assertIsValidIterationsAndSeed",
            parameter = "seed", value = seed
        )
    }
}

.assertIsValidLegendPosition <- function(legendPosition) {
    if (is.null(legendPosition) || length(legendPosition) != 1) {
        stopIllegalArgument("'legendPosition' (", .arrayToString(legendPosition), ") ",
            "must be a single integer or character value",
            functionName = ".assertIsValidLegendPosition",
            parameter = "legendPosition",
            value = legendPosition
        )
    }

    if (is.na(legendPosition)) {
        return(invisible())
    }

    if (grepl("^-?[0-9]+$", legendPosition)) {
        legendPosition <- as.integer(legendPosition)
    }

    if (!is.numeric(legendPosition) && !is.character(legendPosition)) {
        stopIllegalArgument("'legendPosition' (", legendPosition, ") ",
            "must be a single integer or character value",
            functionName = ".assertIsValidLegendPosition",
            parameter = "legendPosition",
            value = legendPosition
        )
    }

    if (is.numeric(legendPosition)) {
        .assertIsSingleInteger(legendPosition, "legendPosition", validateType = FALSE)
        .assertIsInClosedInterval(legendPosition, "legendPosition", lower = -1, upper = 6)
    } else {
        validLegendPositions <- c("none", "left", "right", "bottom", "top")
        if (!(legendPosition %in% validLegendPositions)) {
            stopIllegalArgument("'legendPosition' (", legendPosition, ") ",
                "must be one of the following values: ",
                .arrayToString(validLegendPositions),
                functionName = ".assertIsValidLegendPosition",
                parameter = "legendPosition",
                value = legendPosition
            )
        }
    }
}

.assertIsValidKMax <- function(
        kMax,
        kMaxLowerBound = 1,
        kMaxUpperBound = C_KMAX_UPPER_BOUND,
        ...,
        showWarnings = FALSE) {
    .assertIsSingleInteger(kMax, "kMax", validateType = FALSE)
    .assertIsInClosedInterval(kMax, "kMax", lower = kMaxLowerBound, upper = kMaxUpperBound)
    if (showWarnings && kMax > 10) {
        warning("The usage of 'kMax' (", kMax, ") > 10 is not validated", call. = FALSE)
    }
}

.assertAreValidInformationRates <- function(
        informationRates,
        kMax = length(informationRates),
        kMaxLowerBound = 1L,
        kMaxUpperBound = C_KMAX_UPPER_BOUND) {
    if (length(informationRates) < kMaxLowerBound) {
        upperBound <- ifelse(kMax >= kMaxLowerBound && kMax < C_KMAX_UPPER_BOUND,
            kMax, C_KMAX_UPPER_BOUND
        )
        stopArgumentLengthOutOfBounds(
            sprintf(
                "length of 'informationRates' (%s) is out of bounds [%s; %s]",
                length(informationRates), kMaxLowerBound, upperBound
            ),
            parameter = "informationRates",
            value = informationRates,
            lowerBound = kMaxLowerBound,
            upperBound = upperBound,
            actualLength = length(informationRates),
            functionName = ".assertAreValidInformationRates"
        )
    }

    if (length(informationRates) != kMax) {
        stopConflictingArguments(
            sprintf(
                "length of 'informationRates' (%s) must be equal to 'kMax' (%s)", length(informationRates),
                kMax
            ),
            parameter = "informationRates",
            value = informationRates,
            relatedParameter = "kMax",
            relatedValue = kMax,
            functionName = ".assertAreValidInformationRates"
        )
    }

    if (kMax == 1) {
        return(invisible())
    }

    .assertValuesAreInsideBounds("informationRates", informationRates,
        0, 1,
        lowerBoundInclusive = FALSE
    )

    if (min(informationRates) <= 0 || max(informationRates) > 1 ||
            any(informationRates[2:kMax] <= informationRates[1:(kMax - 1)])) {
        stopIllegalArgument(
            sprintf(
                "'informationRates' (%s) must be strictly increasing: 0 < x_1 < .. < x_%s <= 1",
                .arrayToString(informationRates, vectorLookAndFeelEnabled = FALSE), kMax
            ),
            parameter = "informationRates",
            value = informationRates, constraint = sprintf(
                "must be strictly increasing: 0 < x_1 < .. < x_%s <= 1",
                kMax
            ), 
		functionName = ".assertAreValidInformationRates"
        )
    }

    if (kMax > 1 && kMax <= 10 && (any(informationRates[2:kMax] - informationRates[1:(kMax - 1)] < 0.05 - 1e-10))) {
        warning("Chosen 'informationRates' (",
            .arrayToString(informationRates, vectorLookAndFeelEnabled = FALSE),
            ") outside validated range",
            call. = FALSE
        )
    }
}

.assertValuesAreInsideBounds <- function(
        parameterName,
        values,
        lowerBound,
        upperBound,
        ...,
        lowerBoundInclusive = TRUE,
        upperBoundInclusive = TRUE) {
    lower <- min(values)
    upper <- max(values)
    lowerInvalid <- ifelse(lowerBoundInclusive, lower < lowerBound, lower <= lowerBound)
    upperInvalid <- ifelse(upperBoundInclusive, upper > upperBound, upper >= upperBound)
    if (!is.na(lowerInvalid)) {
        if (lowerInvalid || upperInvalid) {
            stopArgumentOutOfBounds(
                sprintf(
                    "'%s' (%s) is out of bounds %s%s; %s%s", parameterName, .arrayToString(values,
                        vectorLookAndFeelEnabled = FALSE
                    ), ifelse(lowerBoundInclusive, "[", "("), lowerBound, upperBound,
                    ifelse(upperBoundInclusive, "]", ")")
                ),
                parameter = parameterName,
                value = values,
                lowerBound = lowerBound,
                upperBound = upperBound,
                functionName = ".assertValuesAreInsideBounds"
            )
        }
    }
}

.assertContainsNoNas <- function(values, parameterName) {
    if (anyNA(values)) {
        stopIllegalArgument(
            sprintf(
                "'%s' (%s) must contain valid numeric values (NA is not allowed)", parameterName,
                .arrayToString(values, vectorLookAndFeelEnabled = FALSE)
            ),
            parameter = parameterName,
            value = values,
            constraint = "must contain valid numeric values; NA is not allowed",
            functionName = ".assertContainsNoNas"
        )
    }
}

.assertContainsOnlyNasAtTheEnd <- function(values, parameterName) {
    if (length(values) <= 1) {
        return(invisible())
    }

    for (i in length(values):2) {
        if (!is.na(values[i]) && is.na(values[i - 1])) {
            stopIllegalArgument(
                sprintf(
                    "'%s' (%s) must contain valid numeric values (NAs are only allowed at the end of the vector)",
                    parameterName, .arrayToString(values, vectorLookAndFeelEnabled = FALSE)
                ),
                parameter = parameterName,
                value = values,
                constraint = "must contain valid numeric values; NAs are only allowed at the end of the vector",
                functionName = ".assertContainsOnlyNasAtTheEnd"
            )
        }
    }
}

.assertValuesAreStrictlyIncreasing <- function(
        values,
        parameterName,
        ...,
        endingNasAllowed = FALSE,
        naAllowed = FALSE) {
    if (isTRUE(naAllowed) && all(is.na(values))) {
        return(invisible())
    }

    len <- length(values)
    if (len <= 1) {
        return(invisible())
    }

    if (!endingNasAllowed) {
        .assertContainsNoNas(values, parameterName)
    }

    .assertContainsOnlyNasAtTheEnd(values, parameterName)

    valuesTemp <- values
    values <- na.omit(values)
    len <- length(values)
    if (len <= 1) {
        return(invisible())
    }

    if (any(values[2:len] <= values[1:(len - 1)])) {
        stopIllegalArgument(
            sprintf(
                "'%s' (%s) must be strictly increasing: x_1 < .. < x_%s", parameterName,
                .arrayToString(valuesTemp, vectorLookAndFeelEnabled = FALSE), len
            ),
            parameter = parameterName,
            value = valuesTemp,
            constraint = sprintf("must be strictly increasing: x_1 < .. < x_%s", len),
            functionName = ".assertValuesAreStrictlyIncreasing"
        )
    }
}

.assertValuesAreMonotoneIncreasing <- function(values, parameterName, endingNasAllowed = FALSE) {
    len <- length(values)
    if (len <= 1) {
        return(invisible())
    }

    if (!endingNasAllowed) {
        .assertContainsNoNas(values, parameterName)
    }

    .assertContainsOnlyNasAtTheEnd(values, parameterName)

    valuesTemp <- values
    values <- na.omit(values)
    len <- length(values)
    if (len <= 1) {
        return(invisible())
    }

    if (any(values[2:len] < values[1:(len - 1)])) {
        stopIllegalArgument(
            sprintf(
                "'%s' (%s) must be increasing: x_1 <= .. <= x_%s",
                parameterName, .arrayToString(valuesTemp,
                    vectorLookAndFeelEnabled = FALSE
                ), len
            ),
            parameter = parameterName,
            value = valuesTemp,
            constraint = sprintf(
                "must be increasing: x_1 <= .. <= x_%s",
                len
            ),
            functionName = ".assertValuesAreMonotoneIncreasing"
        )
    }
}

.assertAreValidFutilityBounds <- function(
        futilityBounds,
        kMax = length(futilityBounds) + 1,
        directionUpper = TRUE,
        kMaxLowerBound = 1,
        kMaxUpperBound = C_KMAX_UPPER_BOUND) {
    if (length(futilityBounds) < kMaxLowerBound - 1) {
        upperBound <- ifelse(kMax >= kMaxLowerBound && kMax < C_KMAX_UPPER_BOUND,
            kMax - 1, C_KMAX_UPPER_BOUND - 1
        )
        stopArgumentLengthOutOfBounds(
            sprintf(
                "length of 'futilityBounds' (%s) is out of bounds [%s; %s]", length(futilityBounds),
                kMaxLowerBound - 1, upperBound
            ),
            parameter = "futilityBounds",
            value = futilityBounds,
            lowerBound = kMaxLowerBound - 1,
            upperBound = upperBound,
            actualLength = length(futilityBounds),
            functionName = ".assertAreValidFutilityBounds"
        )
    }

    .assertIsValidKMax(kMax, kMaxLowerBound = kMaxLowerBound, kMaxUpperBound = kMaxUpperBound)

    if (length(futilityBounds) != kMax - 1) {
        stopConflictingArguments("length of 'futilityBounds' (", length(futilityBounds), ") must be equal to 'kMax' (",
            kMax, ") - 1",
            parameter = "futilityBounds", value = futilityBounds, 
		relatedParameter ="kMax", 
		relatedValue = kMax,
            functionName = ".assertAreValidFutilityBounds"
        )
    }

    lowerBound <- ifelse(isFALSE(directionUpper), -6, -Inf)
    upperBound <- ifelse(isFALSE(directionUpper), Inf, 6)
    .assertValuesAreInsideBounds("futilityBounds", futilityBounds,
        lowerBound = lowerBound, upperBound = upperBound
    )
}

.assertIsValidCipher <- function(key, value) {
    if (!(key %in% names(C_CIPHERS)) || (getCipheredValue(value) != C_CIPHERS[[key]])) {
        stopIllegalArgument("'token' and/or 'secret' unkown",
            functionName = ".assertIsValidCipher", 
		parameter ="token",
            relatedParameter = "secret"
        )
    }
}

.assertIsValidAlpha0Vec <- function(
        alpha0Vec,
        kMax = length(alpha0Vec) - 1,
        kMaxLowerBound = 1,
        kMaxUpperBound = C_KMAX_UPPER_BOUND) {
    .assertIsValidKMax(kMax, kMaxLowerBound = kMaxLowerBound, kMaxUpperBound = kMaxUpperBound)

    if (length(alpha0Vec) < kMaxLowerBound - 1) {
        stopArgumentLengthOutOfBounds(
            sprintf(
                "length of 'alpha0Vec' (%s) is out of bounds [%s; %s]", length(alpha0Vec),
                kMaxLowerBound - 1, kMax - 1
            ),
            parameter = "alpha0Vec",
            value = alpha0Vec,
            lowerBound = kMaxLowerBound - 1,
            upperBound = kMax - 1,
            actualLength = length(alpha0Vec),
            functionName = ".assertIsValidAlpha0Vec"
        )
    }

    if (length(alpha0Vec) != kMax - 1) {
        stopConflictingArguments("length of 'alpha0Vec' (", length(alpha0Vec),
            ") must be equal to 'kMax' (", kMax, ") - 1",
            parameter = "alpha0Vec",
            value = alpha0Vec,
            relatedParameter = "kMax",
            relatedValue = kMax,
            functionName = ".assertIsValidAlpha0Vec"
        )
    }

    .assertValuesAreInsideBounds("alpha0Vec", alpha0Vec, 0, 1, lowerBoundInclusive = FALSE)
}

.assertIsValidSidedParameter <- function(sided) {
    if (is.null(match.call(expand.dots = FALSE)[["sided"]])) {
        stopMissingArgument("'sided' must be defined",
            functionName = ".assertIsValidSidedParameter",
            parameter = "sided"
        )
    }
    if (sided != 1 && sided != 2) {
        stopIllegalArgument("'sided' (", sided, ") must be 1 or 2",
            functionName = ".assertIsValidSidedParameter",
            parameter = "sided",
            value = sided
        )
    }
}

.assertIsValidGroupsParameter <- function(groups) {
    if (is.null(match.call(expand.dots = FALSE)[["groups"]])) {
        stopMissingArgument("'groups' must be defined",
            functionName = ".assertIsValidGroupsParameter",
            parameter = "groups"
        )
    }
    if (groups != 1 && groups != 2) {
        stopIllegalArgument("'groups' (", groups, ") must be 1 or 2",
            functionName = ".assertIsValidGroupsParameter",
            parameter = "groups",
            value = groups
        )
    }
}

.allArgumentsAreNotNull <- function(...) {
    args <- list(...)
    naCounter <- 0
    for (arg in args) {
        if (!is.null(arg)) {
            naCounter <- naCounter + sum(is.na(arg))
        }
    }
    return(naCounter == 0)
}

.assertAssociatedArgumentsAreDefined <- function(...) {
    .associatedArgumentsAreDefined(..., warningOnlyEnabled = FALSE)
}

.associatedArgumentsAreDefined <- function(..., warningOnlyEnabled = TRUE) {
    args <- NULL
    tryCatch(expr = {
        args <- list(...)
    }, error = function(e) {
        stopMissingArgument(e$message,
            call = e$call,
            functionName = ".associatedArgumentsAreDefined"
        )
    })

    if (.allArgumentsAreNotNull(...)) {
        return(invisible(TRUE))
    }

    args <- args[args != "warningOnlyEnabled" & !is.null(args)]
    argNames <- names(args)
    if (sum(argNames == "") > 0) {
        stopRuntimeIssue("each argument must have a name defined, e.g. a = a",
            functionName = ".associatedArgumentsAreDefined"
        )
    }

    definedArguments <- c()
    undefinedArguments <- c()
    for (i in seq_len(length(args))) {
        arg <- args[[i]]
        argName <- argNames[i]
        if (missing(arg) || (!is.null(arg) && sum(is.na(arg)) > 0)) {
            undefinedArguments <- c(undefinedArguments, argName)
        } else {
            definedArguments <- c(definedArguments, argName)
        }
    }
    if (length(undefinedArguments) > 0 && length(definedArguments) > 0) {
        message <- paste0(
            .arrayToString(undefinedArguments, encapsulate = TRUE),
            " ", ifelse(warningOnlyEnabled, "should", "must"),
            " be defined because ", .arrayToString(definedArguments, encapsulate = TRUE),
            ifelse(length(definedArguments) > 1, " are", " is"), " defined"
        )
        if (warningOnlyEnabled) {
            warning(C_EXCEPTION_TYPE_INCOMPLETE_ARGUMENTS, message, call. = FALSE)
            return(FALSE)
        } else {
            stopMissingArgument(message,
                parameter = undefinedArguments,
                relatedParameter = definedArguments,
                functionName = ".associatedArgumentsAreDefined"
            )
        }
    }

    return(invisible(length(definedArguments) == length(args)))
}

.assertIsValidNPlanned <- function(nPlanned, kMax, stage, ..., required = TRUE) {
    if (length(nPlanned) > 0 && all(is.na(nPlanned))) {
        if (!required) {
            return(invisible())
        }
        stopMissingArgument("'nPlanned' must be specified",
            functionName = ".assertIsValidNPlanned",
            parameter = "nPlanned",
            value = nPlanned
        )
    }

    .assertIsSingleInteger(kMax, "kMax", validateType = FALSE)
    .assertIsSingleInteger(stage, "stage", validateType = FALSE)

    if (is.null(nPlanned) || length(nPlanned) != kMax - stage) {
        stopIllegalArgument(
            sprintf(
                paste0("'nPlanned' (%s) is invalid: ", "length must be equal to %s (kMax - stage = %s - %s)"),
                .arrayToString(nPlanned), kMax - stage, kMax, stage
            ),
            functionName = ".assertIsValidNPlanned", 
		parameter ="nPlanned",
            value = nPlanned
        )
    }

    if (sum(is.na(nPlanned)) > 0 || sum(nPlanned <= 0) > 0) {
        stopIllegalArgument(
            sprintf(
                paste0("'nPlanned' (%s) is invalid: ", "all values must be > 0"),
                .arrayToString(nPlanned)
            ),
            functionName = ".assertIsValidNPlanned",
            parameter = "nPlanned",
            value = nPlanned
        )
    }
}

.isValidNPlanned <- function(nPlanned, kMax, stage) {
    if (missing(nPlanned)) {
        warning("'nPlanned' is missing", call. = FALSE)
        return(FALSE)
    }

    if (all(is.na(nPlanned))) {
        return(TRUE)
    }

    if (length(nPlanned) != kMax - stage) {
        warning(sprintf(
            paste0(
                "'nPlanned' (%s) will be ignored: ",
                "length must be equal to %s (kMax - stage = %s - %s)"
            ),
            .arrayToString(nPlanned), kMax - stage, kMax, stage
        ), call. = FALSE)
        return(FALSE)
    }

    if (sum(nPlanned <= 0, na.rm = TRUE) > 0) {
        warning(sprintf(
            paste0(
                "'nPlanned' (%s) will be ignored: ",
                "all values must be > 0"
            ),
            .arrayToString(nPlanned)
        ), call. = FALSE)
        return(FALSE)
    }

    return(TRUE)
}

.warnInCaseOfUnknownArguments <- function(
        ...,
        functionName,
        ignore = character(),
        numberOfAllowedUnnamedParameters = 0,
        exceptionEnabled = FALSE) {
    args <- list(...)
    if (length(args) == 0) {
        return(invisible())
    }

    if (numberOfAllowedUnnamedParameters > 0) {
        ignore <- c(ignore, paste0("%param", 1:numberOfAllowedUnnamedParameters, "%"))
    }
    ignore <- c(ignore, "showWarnings")
    argNames <- names(args)
    for (i in seq_len(length(args))) {
        arg <- args[[i]]
        argName <- ifelse(is.null(argNames[i]) || argNames[i] == "",
            ifelse(inherits(arg, "StageResults"), "stageResultsName", paste0("%param", i, "%")),
            argNames[i]
        )
        if (is(arg, "FutilityBounds")) {
            if (grepl("^getDesign(InverseNormal|GroupSequential|Fisher)", functionName)) {
                next
            }

            if (grepl("^getDesignFisher", functionName)) {
                argName <- "alpha0Vec"
            } else {
                argName <- "futilityBounds"
            }
        }

        if (!(argName %in% ignore) && !grepl("^\\.", argName)) {
            if (.isResultObjectBaseClass(arg) || is.environment(arg)) {
                arg <- .getClassName(arg)
            }
            if (is.function(arg)) {
                arg <- "function(...)"
            }
            argValue <- paste0(" (", .getClassName(arg), ")")
            tryCatch(expr = {
                argValue <- .arrayToString(arg,
                    vectorLookAndFeelEnabled = length(arg) > 1,
                    encapsulate = is.character(arg)
                )
                argValue <- paste0(" = ", argValue)
            }, error = function(e) {})
            if (exceptionEnabled) {
                stopIllegalArgument("argument unknown in ", functionName, "(...): ", .pQuote(argName), argValue, " is not allowed",
                    functionName = ".warnInCaseOfUnknownArguments",
                    parameter = "functionName",
                    value = functionName,
                    relatedParameter = "argName",
                    relatedValue = argName
                )
            } else {
                warning("Argument unknown in ", functionName, "(...): ", .pQuote(argName),
                    argValue, " will be ignored",
                    call. = FALSE
                )
            }
        }
    }
}

.warnInCaseOfUnusedArgument <- function(arg, argName, defaultValue, functionName) {
    if (!identical(arg, defaultValue)) {
        warning("Unused argument in ", functionName, "(...): ", .pQuote(argName), " = ", .arrayToString(
                arg,
                vectorLookAndFeelEnabled = (length(arg) > 1),
                maxLength = 10,
                encapsulate = !is.null(arg) && any(is.character(arg))
            ),
            " will be ignored",
            call. = FALSE
        )
    }
}

.warnInCaseOfTwoSidedPowerArgument <- function(...) {
    args <- list(...)
    argNames <- names(args)
    if ("twoSidedPower" %in% argNames) {
        warning("'twoSidedPower' can only be defined in 'design'", call. = FALSE)
    }
}

.warnInCaseOfTwoSidedPowerIsDisabled <- function(design) {
    if (design$sided == 2 && !is.na(design$twoSidedPower) && !design$twoSidedPower &&
            design$isUserDefinedParameter("twoSidedPower")) {
        warning("design$twoSidedPower = FALSE will be ignored because design$sided = 2", call. = FALSE)
    }
}

.isTrialDesignWithValidFutilityBounds <- function(design) {
    if (is.null(design) || !.isTrialDesignInverseNormalOrGroupSequential(design)) {
        return(FALSE)
    }

    return(.hasApplicableFutilityBounds(design))
}

.anyFutilityBoundsAreInvalid <- function(futilityBounds, directionUpper) {
    if (is.null(futilityBounds) || length(futilityBounds) == 0 || all(is.na(futilityBounds))) {
        return(FALSE)
    }

    if (isFALSE(directionUpper)) {
        return(any(futilityBounds < C_FUTILITY_BOUNDS_MAX_VALUE, na.rm = TRUE))
    }

    return(any(futilityBounds > C_FUTILITY_BOUNDS_MIN_VALUE, na.rm = TRUE))
}

.hasApplicableFutilityBounds <- function(design) {
    return(.anyFutilityBoundsAreInvalid(design$futilityBounds, design$directionUpper))
}

.isTrialDesignWithValidAlpha0Vec <- function(design) {
    if (is.null(design) || !.isTrialDesignFisher(design)) {
        return(FALSE)
    }

    alpha0Vec <- design[["alpha0Vec"]]
    if (is.null(alpha0Vec)) {
        return(FALSE)
    }

    alpha0Vec <- na.omit(alpha0Vec)
    if (length(alpha0Vec) == 0 || all(is.na(alpha0Vec))) {
        return(FALSE)
    }

    return(any(alpha0Vec != C_ALPHA_0_VEC_DEFAULT))
}

.assertPackageIsInstalled <- function(packageName) {
    if (!.isPackageNamespaceLoaded(packageName, quietly = TRUE)) {
        stopRuntimeIssue("Package \"", packageName, "\" is needed for this function to work. ",
            "Please install using, e.g., install.packages(\"", packageName, "\")",
            parameter = "packageName",
            value = packageName,
            constraint = "installed package namespace",
            functionName = ".assertPackageIsInstalled"
        )
    }
}

.assertGgplotIsInstalled <- function() {
    .assertPackageIsInstalled("ggplot2")
}

.assertRcppIsInstalled <- function() {
    .assertPackageIsInstalled("Rcpp")
}

.assertTestthatIsInstalled <- function() {
    .assertPackageIsInstalled("testthat")
}

.assertIsValidThetaH0 <- function(
        thetaH0,
        ...,
        endpoint = c("means", "rates", "survival", "counts"),
        groups,
        ratioEnabled = FALSE,
        naAllowed = FALSE) {
    .warnInCaseOfUnknownArguments(functionName = ".assertIsValidThetaH0", ...)
    .assertIsSingleNumber(thetaH0, "thetaH0", naAllowed = TRUE)
    if (endpoint == "rates" && groups == 1 && is.na(thetaH0)) {
        stopMissingArgument("'thetaH0' must be specified for testing a rate in one sample",
            parameter = "thetaH0", constraint = "must be specified for testing a rate in one sample",
            functionName = ".assertIsValidThetaH0",
            value = thetaH0
        )
    }
    if (isFALSE(naAllowed)) {
        .assertIsSingleNumber(thetaH0, "thetaH0", naAllowed = naAllowed)
    }
    if (naAllowed && is.na(thetaH0)) {
        return(invisible())
    }

    endpoint <- match.arg(endpoint)
    if (endpoint == "means" || endpoint == "rates") {
        if (groups == 2 && ratioEnabled) {
            .assertIsInOpenInterval(thetaH0, "thetaH0", lower = 0, upper = NULL)
            return(invisible())
        }
    }

    if (endpoint == "rates") {
        if (groups == 1) {
            .assertIsInOpenInterval(thetaH0, "thetaH0", lower = 0, upper = 1)
        } else {
            .assertIsInOpenInterval(thetaH0, "thetaH0", lower = -1, upper = 1)
        }
    } else if (endpoint %in% c("survival", "counts")) {
        .assertIsInOpenInterval(thetaH0, "thetaH0", lower = 0, upper = NULL)
    }
}

.assertIsValidThetaH0DataInput <- function(thetaH0, dataInput, ..., naAllowed = TRUE) {
    .assertIsValidThetaH0(thetaH0,
        endpoint = .getDatasetEndpoint(dataInput),
        groups = dataInput$getNumberOfGroups(),
        naAllowed = naAllowed
    )
}

.assertIsValidThetaRange <- function(
        ...,
        thetaRange,
        thetaAutoSeqEnabled = TRUE,
        survivalDataEnabled = FALSE) {
    if (is.null(thetaRange) || (thetaAutoSeqEnabled && length(thetaRange) <= 1) ||
            anyNA(thetaRange)) {
        stopIllegalArgument("'thetaRange' (", .arrayToString(thetaRange), ") must be a vector ",
            "with two entries defining minimum and maximum ",
            "or a sequence of numeric values with length > 2",
            functionName = ".assertIsValidThetaRange",
            parameter = "thetaRange",
            value = thetaRange
        )
    } else if (length(thetaRange) == 2 && thetaAutoSeqEnabled) {
        minValue <- thetaRange[1]
        maxValue <- thetaRange[2]
        if (survivalDataEnabled) {
            minValue <- .assertIsValidHazardRatio(minValue, "thetaRange[1]")
            maxValue <- .assertIsValidHazardRatio(maxValue, "thetaRange[2]")
        }
        if (minValue >= maxValue) {
            stopIllegalArgument("'thetaRange' with length 2 must contain ",
                "minimum < maximum (", minValue, " >= ", maxValue, ")",
                functionName = ".assertIsValidThetaRange",
                parameter = "thetaRange",
                value = thetaRange
            )
        }
        by <- (maxValue - minValue) / C_THETA_RANGE_SEQUENCE_LENGTH_DEFAULT
        thetaRange <- seq(minValue, maxValue, by)
    }

    invisible(thetaRange)
}

.assertIsValidPiTreatmentRange <- function(..., piTreatmentRange, piAutoSeqEnabled = TRUE) {
    if (is.null(piTreatmentRange) || (piAutoSeqEnabled && length(piTreatmentRange) <= 1) ||
            anyNA(piTreatmentRange)) {
        stopIllegalArgument("'piTreatmentRange' (",
            .arrayToString(piTreatmentRange), ") must be a vector ",
            "with two entries defining minimum and maximum ",
            "or a sequence of numeric values with length > 2",
            functionName = ".assertIsValidPiTreatmentRange",
            parameter = "piTreatmentRange",
            value = piTreatmentRange
        )
    } else if (length(piTreatmentRange) == 2) {
        if (piAutoSeqEnabled) {
            minValue <- piTreatmentRange[1]
            maxValue <- piTreatmentRange[2]
            if (minValue == 0) {
                minValue <- 0.00000001
            }
            if (maxValue == 1) {
                maxValue <- 0.99999999
            }
            .assertIsValidPi(minValue, "piTreatmentRange[1]")
            .assertIsValidPi(maxValue, "piTreatmentRange[2]")
            if (minValue >= maxValue) {
                stopIllegalArgument("'piTreatmentRange' with length 2 ",
                    "must contain minimum < maximum (",
                    minValue, " >= ", maxValue, ")",
                    functionName = ".assertIsValidPiTreatmentRange",
                    parameter = "piTreatmentRange",
                    value = piTreatmentRange
                )
            }
            by <- (maxValue - minValue) / C_THETA_RANGE_SEQUENCE_LENGTH_DEFAULT
            piTreatmentRange <- seq(minValue, maxValue, by)
        }
    }

    invisible(piTreatmentRange)
}

.assertIsValidPi <- function(piValue, piName) {
    if (is.null(piValue) || length(piValue) == 0) {
        stopIllegalArgument(.pQuote(piName), " must be a valid numeric value",
            functionName = ".assertIsValidPi",
            parameter = "piName", value = piName
        )
    }

    if (all(is.na(piValue))) {
        return(invisible())
    }

    if (!is.numeric(piValue) || anyNA(piValue)) {
        stopIllegalArgument(.pQuote(piName), " (", .arrayToString(piValue),
            ") must be a valid numeric value",
            functionName = ".assertIsValidPi",
            parameter = "piName",
            value = piName,
            relatedParameter = "piValue",
            relatedValue = piValue
        )
    }

    if (any(piValue <= -1e-16) || any(piValue >= 1 + 1e-16)) {
        stopArgumentOutOfBounds(.pQuote(piName), " (",
            .arrayToString(piValue), ") ", "is out of bounds (0; 1) or event time too long",
            functionName = ".assertIsValidPi",
            parameter = "piName",
            value = piName,
            relatedParameter = "piValue",
            relatedValue = piValue
        )
    }
}

.assertIsValidPi1 <- function(pi1, stageResults = NULL, stage = NULL) {
    if (is.na(pi1) && !is.null(stageResults) && !is.null(stage)) {
        if (stageResults$isOneSampleDataset()) {
            pi1 <- stageResults$overallEvents[stage] / stageResults$overallSampleSizes[stage]
        } else {
            pi1 <- stageResults$overallEvents1[stage] / stageResults$overallSampleSizes1[stage]
        }
    }
    .assertIsInClosedInterval(pi1, "pi1", lower = 0, upper = 1)
    invisible(pi1)
}

.assertIsValidPi2 <- function(pi2, stageResults = NULL, stage = NULL) {
    if (is.na(pi2) && !is.null(stageResults) && !is.null(stage)) {
        pi2 <- stageResults$overallEvents2[stage] / stageResults$overallSampleSizes2[stage]
    }
    .assertIsInClosedInterval(pi2, "pi2", lower = 0, upper = 1)
    invisible(pi2)
}

.assertIsValidAllocationRatioPlanned <- function(allocationRatioPlanned, numberOfGroups) {
    if (numberOfGroups == 1) {
        if (allocationRatioPlanned != C_ALLOCATION_RATIO_DEFAULT) {
            warning(
                "Planned allocation ratio ", allocationRatioPlanned, " will be ignored ",
                "because the specified data has only one group",
                call. = FALSE
            )
        }
        return(invisible())
    }
    .assertIsSingleNumber(allocationRatioPlanned, "allocationRatioPlanned")
    .assertIsInOpenInterval(
        allocationRatioPlanned,
        "allocationRatioPlanned",
        lower = 0, upper = C_ALLOCATION_RATIO_MAXIMUM
    )
}

.assertIsValidAllocationRatioPlannedSampleSize <- function(
        allocationRatioPlanned,
        maxNumberOfSubjects = NA_integer_) {
    allocationRatioPlanned <- .assertIsNumericVector(
        allocationRatioPlanned, "allocationRatioPlanned",
        naAllowed = TRUE
    )
    .assertIsInClosedInterval(allocationRatioPlanned, "allocationRatioPlanned",
        lower = 0,
        upper = C_ALLOCATION_RATIO_MAXIMUM, naAllowed = TRUE
    )
    .assertIsValidMaxNumberOfSubjects(maxNumberOfSubjects, naAllowed = TRUE)

    if (anyNA(allocationRatioPlanned)) {
        allocationRatioPlanned <- C_ALLOCATION_RATIO_DEFAULT
    }

    if (length(maxNumberOfSubjects) == 1 && !is.na(maxNumberOfSubjects) &&
            maxNumberOfSubjects > 0 && all(allocationRatioPlanned == 0)) {
        stopIllegalArgument("determination of optimal allocation ratio not possible ",
            "if explicit or implicit 'maxNumberOfSubjects' (",
            maxNumberOfSubjects, ") > 0, i.e., follow-up time should be calculated ",
            "(please specify an 'allocationRatioPlanned' > 0)",
            functionName = ".assertIsValidAllocationRatioPlannedSampleSize",
            parameter = "maxNumberOfSubjects",
            value = maxNumberOfSubjects,
            relatedParameter = "allocationRatioPlanned"
        )
    }

    return(invisible(allocationRatioPlanned))
}

.assertIsValidThetaH1 <- function(thetaH1, stageResults = NULL,
                                          stage = NULL, ..., results = NULL) {
    if (is.na(thetaH1) && !is.null(stageResults) && !is.null(stage)) {
        thetaH1 <- stageResults$effectSizes[stage]
        if (!is.null(results)) {
            results$.setParameterType("thetaH1", C_PARAM_GENERATED)
        }
    }
    .assertIsSingleNumber(thetaH1, "thetaH1")
    invisible(thetaH1)
}

.assertIsValidAssumedStDev <- function(
        assumedStDev,
        stageResults = NULL,
        stage = NULL,
        ...,
        results = NULL) {
    if (is.na(assumedStDev) && !is.null(stageResults) && !is.null(stage)) {
        assumedStDev <- stageResults$overallStDevs[stage]
        if (!is.null(results)) {
            results$.setParameterType("assumedStDev", C_PARAM_GENERATED)
        }
    }
    .assertIsSingleNumber(assumedStDev, "assumedStDev")
    if (assumedStDev <= 0) {
        stopIllegalArgument("'assumedStDev' (", assumedStDev, ") must be > 0",
            functionName = ".assertIsValidAssumedStDev",
            parameter = "assumedStDev", value = assumedStDev
        )
    }
    invisible(assumedStDev)
}

.assertIsValidThetaH1ForMultiArm <- function(
        thetaH1,
        stageResults = NULL,
        stage = NULL,
        ...,
        results = NULL) {
    if (!is.null(stageResults) && all(is.na(thetaH1)) && !is.null(stage)) {
        thetaH1 <- stageResults$effectSizes[, stage]
        if (!is.null(results)) {
            results$.setParameterType("thetaH1", C_PARAM_GENERATED)
        }
    }

    .assertIsNumericVector(thetaH1, "thetaH1", naAllowed = TRUE)
    invisible(thetaH1)
}

.assertIsValidThetaH1ForEnrichment <- function(
        thetaH1,
        stageResults = NULL,
        stage = NULL,
        ...,
        results = NULL) {
    invisible(.assertIsValidThetaH1ForMultiArm(
        thetaH1 = thetaH1,
        stageResults = stageResults, stage = stage, results = results
    ))
}

.assertIsValidAssumedStDevForMultiHypotheses <- function(
        assumedStDev,
        stageResults = NULL,
        stage = NULL,
        ...,
        results = NULL) {
    if (!is.null(stageResults) && all(is.na(assumedStDev)) && !is.null(stage)) {
        if (is.matrix(stageResults$overallStDevs)) { # inherits(stageResults, "StageResultsMultiArmMeans")
            assumedStDev <- stageResults$overallStDevs[, stage]
        } else {
            assumedStDev <- stageResults$overallStDevs[stage]
        }

        if (!is.null(results)) {
            results$.setParameterType("assumedStDevs", C_PARAM_GENERATED)
        }
    }
    .assertIsNumericVector(assumedStDev, "assumedStDev", naAllowed = TRUE)
    if (any(assumedStDev <= 0, na.rm = TRUE)) {
        stopIllegalArgument("'assumedStDev' (", .arrayToString(assumedStDev), ") must be > 0",
            functionName = ".assertIsValidAssumedStDevForMultiHypotheses",
            parameter = "assumedStDev",
            value = assumedStDev
        )
    }

    invisible(assumedStDev)
}

.assertIsValidAssumedStDevs <- function(assumedStDevs, gMax) {
    if (length(assumedStDevs) != 1 && length(assumedStDevs) != gMax) {
        stopIllegalArgument(
            sprintf(
                paste0("length of 'assumedStDevs' (%s) ", "must be equal to 'gMax' (%s) or 1"),
                .arrayToString(assumedStDevs), gMax
            ),
            functionName = ".assertIsValidAssumedStDevs",
            parameter = "assumedStDevs",
            value = assumedStDevs,
            relatedParameter = "gMax",
            relatedValue = gMax
        )
    }
}

.assertIsValidPiTreatmentsForMultiArm <- function(
        piTreatments,
        stageResults = NULL,
        stage = NULL,
        ...,
        results = NULL) {
    if (!is.null(stageResults) && all(is.na(piTreatments)) && !is.null(stage)) {
        piTreatments <- stageResults$overallPiTreatments[, stage]
        if (!is.null(results)) {
            results$.setParameterType("piTreatments", C_PARAM_GENERATED)
        }
    }
    .assertIsNumericVector(piTreatments, "piTreatments", naAllowed = TRUE)
    .assertIsInClosedInterval(piTreatments, "piTreatments", lower = 0, upper = 1, naAllowed = TRUE)
    invisible(piTreatments)
}

.assertIsValidPiControlForMultiArm <- function(
        piControl,
        stageResults = NULL,
        stage = NULL,
        ...,
        results = NULL) {
    if (!is.null(stageResults) && is.na(piControl) && !is.null(stage)) {
        piControl <- stageResults$overallPiControl[, stage]
        if (!is.null(results)) {
            results$.setParameterType("piControl", C_PARAM_GENERATED)
        }
    }
    .assertIsNumericVector(piControl, "piControl", naAllowed = TRUE)
    .assertIsInClosedInterval(piControl, "piControl", lower = 0, upper = 1)
    invisible(piControl)
}

.assertIsValidPiTreatmentsForEnrichment <- function(
        piTreatments,
        stageResults = NULL,
        stage = NULL,
        ...,
        results = NULL) {
    if (!is.null(stageResults) && all(is.na(piTreatments)) && !is.null(stage)) {
        piTreatments <- stageResults$overallPisTreatment[, stage]
        if (!is.null(results)) {
            results$.setParameterType("piTreatments", C_PARAM_GENERATED)
        }
    }
    .assertIsNumericVector(piTreatments, "piTreatments", naAllowed = TRUE)
    .assertIsInClosedInterval(piTreatments, "piTreatments", lower = 0, upper = 1, naAllowed = TRUE)
    invisible(piTreatments)
}

.assertIsValidPiControlForEnrichment <- function(
        piControls,
        stageResults = NULL,
        stage = NULL,
        ...,
        results = NULL) {
    if (!is.null(stageResults) && all(is.na(piControls)) && !is.null(stage)) {
        piControls <- stageResults$overallPisControl[, stage]
        if (!is.null(results)) {
            results$.setParameterType("piControls", C_PARAM_GENERATED)
        }
    }
    piControls <- .assertIsNumericVector(piControls, "piControls", naAllowed = TRUE)
    .assertIsInClosedInterval(piControls, "piControls", lower = 0, upper = 1, naAllowed = TRUE)
    invisible(invisible(piControls))
}

.assertIsValidHazardRatio <- function(hazardRatio, thetaH0) {
    hazardRatio <- .assertIsNumericVector(hazardRatio, "hazardRatio")
    if (any(hazardRatio == thetaH0)) {
        stopIllegalArgument("alternative not correctly specified: ",
            "each hazard ratio (", .arrayToString(hazardRatio[1:min(
                length(hazardRatio),
                10
            )]), ") must be unequal to 'thetaH0' (", thetaH0, ")",
            functionName = ".assertIsValidHazardRatio",
            parameter = "thetaH0",
            value = thetaH0
        )
    }
    return(invisible(hazardRatio))
}

.assertIsValidHazardRatioVector <- function(hazardRatio) {
    hazardRatio <- .assertIsNumericVector(hazardRatio, "hazardRatio")
    if (any(hazardRatio <= 0)) {
        if (length(hazardRatio) == 1) {
            stopIllegalArgument("'hazardRatio' (", hazardRatio, ") must be > 0",
                functionName = ".assertIsValidHazardRatioVector",
                parameter = "hazardRatio", value = hazardRatio
            )
        } else {
            stopIllegalArgument("each 'hazardRatio' (",
                .arrayToString(hazardRatio[1:min(length(hazardRatio), 10)]),
                ") must be > 0",
                functionName = ".assertIsValidHazardRatioVector",
                parameter = "hazardRatio",
                value = hazardRatio
            )
        }
    }
    return(invisible(hazardRatio))
}

.warnInCaseOfIgnoredDirectionUpper <- function(
        directionUpper,
        sided,
        ...,
        userFunctionCallEnabled = TRUE) {
    if (sided == 2 && !is.na(directionUpper)) {
        if (userFunctionCallEnabled) {
            warning("'directionUpper' (", directionUpper, ") will be ignored because it ",
                "is not applicable for 'sided' = 2",
                call. = FALSE
            )
        }
        return(invisible(NA))
    }

    return(invisible(directionUpper))
}

.assertIsValidDirectionUpper <- function(
        directionUpper,
        design,
        ...,
        objectType = c("sampleSize", "power", "analysis"),
        userFunctionCallEnabled = FALSE,
        default = C_DIRECTION_UPPER_DEFAULT) {
    objectType <- match.arg(objectType)

    .assertIsSingleLogical(directionUpper, "directionUpper", naAllowed = TRUE)

    if (!is.na(directionUpper) && !is.na(design$directionUpper) &&
            !identical(directionUpper, design$directionUpper)) {
        stopConflictingArguments("in the design directionUpper = ",
            design$directionUpper, " is defined. ", "In the ",
            ifelse(objectType == "sampleSize", "sample size", objectType),
            " function the same direction must be specified, but it is ",
            directionUpper,
            functionName = ".assertIsValidDirectionUpper",
            parameter = "directionUpper",
            value = design$directionUpper
        )
    }

    if (objectType %in% c("sampleSize", "power", "analysis")) {
        if (design$sided == 1 && is.na(directionUpper)) {
            if (!is.na(design$directionUpper)) {
                directionUpper <- design$directionUpper
            } else {
                directionUpper <- default
            }
        }
        directionUpper <- .warnInCaseOfIgnoredDirectionUpper(
            directionUpper, design$sided,
            userFunctionCallEnabled = userFunctionCallEnabled
        )
    } else if (is.na(directionUpper)) {
        if (!is.na(design$directionUpper)) {
            directionUpper <- design$directionUpper
        } else {
            directionUpper <- default
        }
    }

    return(directionUpper)
}

.assertIsFunction <- function(fun) {
    if (is.null(fun)) {
        stopIllegalArgument("'fun' must be a valid function",
            functionName = ".assertIsFunction",
            parameter = "fun",
            value = fun
        )
    }
    if (!is.function(fun)) {
        stopIllegalArgument("'fun' must be a function (is ", .getClassName(fun), ")",
            functionName = ".assertIsFunction",
            parameter = "fun",
            value = fun
        )
    }
}

.assertIsValidFunction <- function(
        fun,
        ...,
        funArgName = "fun",
        expectedArguments = NULL,
        expectedFunction = NULL,
        identical = FALSE,
        validateThreeDots = TRUE,
        showUnusedArgumentsMessage = FALSE,
        namedArgumentsExpected = FALSE) {
    fCall <- match.call(expand.dots = FALSE)

    if (is.null(expectedArguments) && is.null(expectedFunction)) {
        stopIllegalArgument("'expectedArguments' or 'expectedFunction' must be not NULL",
            functionName = ".assertIsValidFunction",
            parameter = "expectedArguments",
            relatedParameter = "expectedFunction",
            value = expectedArguments
        )
    }

    if (!is.function(fun)) {
        stopIllegalArgument(.pQuote(funArgName), " must be a function",
            parameter = funArgName,
            value = fun,
            constraint = "must be a function",
            functionName = ".assertIsValidFunction"
        )
    }

    functionName <- as.character(fCall$fun)
    if (is.null(functionName) || functionName == funArgName) {
        functionName <- "function"
    }

    argNames <- methods::formalArgs(fun)
    if (!is.null(expectedArguments)) {
        argNamesExpected <- expectedArguments
    } else if (!is.null(expectedFunction)) {
        if (!is.function(expectedFunction)) {
            stopIllegalArgument("'expectedFunction' must be a function",
                parameter = "expectedFunction",
                value = expectedFunction,
                constraint = "must be a function",
                functionName = ".assertIsValidFunction"
            )
        }
        argNamesExpected <- methods::formalArgs(expectedFunction)
    }

    if (validateThreeDots) {
        if (!("..." %in% argNames)) {
            stopIllegalArgument(.pQuote(funArgName), " ",
                "must contain the three-dots argument '...', e.g., ", funArgName,
                " = ", functionName, "(", .arrayToString(argNames), ", ...)",
                parameter = funArgName,
                value = fun,
                constraint = "must contain the three-dots argument '...'",
                functionName = ".assertIsValidFunction"
            )
        }
    }
    argNames <- argNames[argNames != "..."]
    argNamesExpected <- argNamesExpected[argNamesExpected != "..."]

    if (length(argNamesExpected) < ifelse(namedArgumentsExpected, 1, 2) &&
            length(argNames) == length(argNamesExpected)) {
        return(invisible())
    }

    for (argName in argNames) {
        if (argName != "..." && !(argName %in% argNamesExpected)) {
            msg <- paste0(
                "the argument ", .pQuote(argName), " in ", .pQuote(funArgName), " (", functionName, ") is not allowed."
            )
            if (length(argNamesExpected) == 1) {
                stopIllegalArgument(msg, " Expected: ", .pQuote(argNamesExpected),
                    parameter = argName,
                    value = fun,
                    relatedParameter = funArgName,
                    relatedValue = functionName,
                    functionName = ".assertIsValidFunction"
                )
            }
            stopIllegalArgument(msg, "\n\n",
                "Use one or more of the following arguments:\n ",
                .arrayToString(argNamesExpected,
                    encapsulate = TRUE
                ),
                parameter = argName,
                value = fun,
                relatedParameter = funArgName,
                relatedValue = functionName,
                functionName = ".assertIsValidFunction"
            )
        }
    }

    if (identical) {
        for (argNameExpected in argNamesExpected) {
            if (argNameExpected != "..." && !(argNameExpected %in% argNames)) {
                stopIllegalArgument(.pQuote(funArgName), " (", functionName, ") must contain ",
                    "an argument with name ", .pQuote(argNameExpected),
                    parameter = funArgName, value = fun, constraint = paste0(
                        "must contain an argument with name ", .pQuote(argNameExpected)
                    ), 
		functionName = ".assertIsValidFunction", 
		relatedParameter ="functionName",
                    relatedValue = functionName
                )
            }
        }
        return(invisible())
    }

    counter <- 0
    unusedArgs <- c()
    for (argNameExpected in argNamesExpected) {
        if (argNameExpected %in% argNames) {
            counter <- counter + 1
        } else {
            unusedArgs <- c(unusedArgs, argNameExpected)
        }
    }

    if (counter == 0) {
        stopIllegalArgument(.pQuote(funArgName), " (", functionName, ") must contain at ",
            "least one of the following arguments: ",
            .arrayToString(argNamesExpected),
            parameter = funArgName, value = fun, constraint = paste0(
                "must contain at least one of the following arguments: ",
                .arrayToString(argNamesExpected)
            ),
            functionName = ".assertIsValidFunction",
            relatedParameter = "functionName",
            relatedValue = functionName
        )
    }

    if (showUnusedArgumentsMessage && length(unusedArgs) > 0) {
        message("Note that the following arguments can optionally be used in ", 
            .pQuote(funArgName), " (", functionName, "): \n",
            .arrayToString(unusedArgs),
            call. = FALSE
        )
    }
}

.assertIsValidThreshold <- function(threshold, activeArms) {
    .assertIsNumericVector(threshold, "threshold", naAllowed = TRUE)
    if ((length(threshold) != 1) && (length(threshold) != activeArms)) {
        stopIllegalArgument("'threshold' (", .arrayToString(threshold), ") ",
            "must be a single value or a vector of length ",
            activeArms,
            functionName = ".assertIsValidThreshold",
            parameter = "threshold",
            value = threshold
        )
    }
}

.assertIsValidPlannedSubjectsOrEvents <- function(
        design,
        plannedValues,
        parameterName = c("plannedSubjects", "plannedEvents")) {
    parameterName <- match.arg(parameterName)
    .assertIsIntegerVector(plannedValues, parameterName, validateType = FALSE)
    if (length(plannedValues) != design$kMax) {
        stopIllegalArgument(.pQuote(parameterName), " (",
            .arrayToString(plannedValues), ") ", "must have length ",
            design$kMax,
            functionName = ".assertIsValidPlannedSubjectsOrEvents",
            parameter = parameterName,
            relatedParameter = "plannedValues",
            relatedValue = plannedValues
        )
    }
    .assertIsInClosedInterval(plannedValues, parameterName, lower = 1, upper = NULL)
    .assertValuesAreStrictlyIncreasing(plannedValues, parameterName)
}

.assertIsValidNumberOfSubjectsPerStage <- function(
        parameterValues,
        parameterName,
        plannedSubjects,
        conditionalPower,
        calcSubjectsFunction,
        kMax,
        endpoint = c("means", "rates", "survival"),
        calcSubjectsFunctionEnabled = TRUE) {
    endpoint <- match.arg(endpoint)

    if (kMax == 1) {
        .ignoreParameterIfNotUsed(
            "conditionalPower",
            conditionalPower, kMax > 1, "design is fixed ('kMax' = 1)"
        )
        return(invisible(NA_real_))
    }

    .assertIsNumericVector(parameterValues, parameterName, naAllowed = TRUE)

    calcSubjectsFunctionName <- ifelse(endpoint == "survival",
        "calcEventsFunction", "calcSubjectsFunction"
    )

    if (is.na(conditionalPower) && is.null(calcSubjectsFunction)) {
        if (length(parameterValues) != 1 || !is.na(parameterValues)) {
            if (calcSubjectsFunctionEnabled) {
                warning(.pQuote(parameterName), " (", .arrayToString(parameterValues), ") ",
                    "will be ignored because neither 'conditionalPower' nor ", 
                    .pQuote(calcSubjectsFunctionName), " is defined",
                    call. = FALSE
                )
            } else {
                warning(.pQuote(parameterName), " (", .arrayToString(parameterValues), ") ",
                    "will be ignored because 'conditionalPower' is not defined",
                    call. = FALSE
                )
            }
        }
        return(invisible(NA_real_))
    }

    if (!is.na(conditionalPower) && length(parameterValues) == 0 ||
            (length(parameterValues) == 1 && is.na(parameterValues))) {
        if (calcSubjectsFunctionEnabled) {
            stopMissingArgument(.pQuote(parameterName), " must be defined ",
                "because 'conditionalPower' or ", .pQuote(calcSubjectsFunctionName), " is defined",
                functionName = ".assertIsValidNumberOfSubjectsPerStage",
                parameter = parameterName,
                relatedParameter = "conditionalPower",
                relatedValue = conditionalPower
            )
        } else {
            stopMissingArgument(.pQuote(parameterName), " must be defined ",
                "because 'conditionalPower' is defined",
                functionName = ".assertIsValidNumberOfSubjectsPerStage",
                parameter = parameterName,
                relatedParameter = "conditionalPower",
                relatedValue = conditionalPower
            )
        }
    }

    if (length(parameterValues) != kMax) {
        stopIllegalArgument(.pQuote(parameterName), " (",
            .arrayToString(parameterValues), ") must have length ", kMax,
            functionName = ".assertIsValidNumberOfSubjectsPerStage",
            parameter = parameterName,
            value = parameterValues
        )
    }

    if (any(is.na(parameterValues[2:length(parameterValues)]))) {
        stopIllegalArgument(.pQuote(parameterName), " (", .arrayToString(parameterValues),
            ") must contain valid numeric values",
            functionName = ".assertIsValidNumberOfSubjectsPerStage",
            parameter = parameterName,
            value = parameterValues
        )
    }

    if (!is.na(parameterValues[1]) && parameterValues[1] != plannedSubjects[1]) {
        warning("First value of ", .pQuote(parameterName), " ",
            "(", parameterValues[1], ") will be ignored",
            call. = FALSE
        )
    }

    parameterValues[1] <- plannedSubjects[1]

    .assertIsInClosedInterval(parameterValues, parameterName, lower = 1, upper = NULL)

    return(invisible(parameterValues))
}

.assertIsValidMaxNumberOfSubjects <- function(maxNumberOfSubjects, naAllowed = FALSE) {
    .assertIsSingleNumber(maxNumberOfSubjects,
        "maxNumberOfSubjects",
        naAllowed = naAllowed
    )
    .assertIsInClosedInterval(maxNumberOfSubjects,
        "maxNumberOfSubjects",
        lower = 1, upper = NULL, naAllowed = naAllowed
    )
}

.assertAreSuitableInformationRates <- function(design, dataInput, stage) {
    if (!.isTrialDesignGroupSequential(design) || stage == 1) {
        return(invisible())
    }

    param <- NA_character_
    paramValues <- NA_real_
    if (dataInput$isDatasetSurvival()) {
        if (any(abs(design$informationRates[2:stage] - dataInput$getOverallEventsUpTo(stage)[2:stage] /
                dataInput$getOverallEventsUpTo(1) * design$informationRates[1]) >
                C_ACCEPT_DEVIATION_INFORMATIONRATES)) {
            param <- "events"
            paramValues <- dataInput$getOverallEventsUpTo(stage)
        }
    } else {
        if (dataInput$getNumberOfGroups() == 1) {
            if (any(abs(design$informationRates[2:stage] -
                    dataInput$getOverallSampleSizesUpTo(stage)[2:stage] /
                        dataInput$getOverallSampleSizesUpTo(1) * design$informationRates[1]) >
                    C_ACCEPT_DEVIATION_INFORMATIONRATES)) {
                param <- "sample sizes"
                paramValues <- dataInput$getOverallSampleSizesUpTo(stage)
            }
        } else if (dataInput$getNumberOfGroups() == 2) {
            if (any(abs(design$informationRates[2:stage] -
                    dataInput$getOverallSampleSizesUpTo(stage)[2:stage] /
                        dataInput$getOverallSampleSizesUpTo(1) * design$informationRates[1]) >
                    C_ACCEPT_DEVIATION_INFORMATIONRATES) ||
                    any(abs(design$informationRates[2:stage] -
                        dataInput$getOverallSampleSizesUpTo(stage, 2)[2:stage] /
                            dataInput$getOverallSampleSizesUpTo(1, 2) * design$informationRates[1]) >
                        C_ACCEPT_DEVIATION_INFORMATIONRATES)) {
                param <- "sample sizes"
                paramValues <- dataInput$getOverallSampleSizesUpTo(stage) + dataInput$getOverallSampleSizesUpTo(stage, 2)
            }
        }
    }
    if (!is.na(param)) {
        warning("Observed ", param, " (", .arrayToString(paramValues),
            ") not according to specified information rates (",
            .arrayToString(design$informationRates[1:stage]), ") in ",
            "group sequential design. ",
            "Test procedure might not control Type I error rate",
            call. = FALSE
        )
    }
}

.assertIsOneSidedDesign <- function(
        design,
        designType = c("multi-arm", "enrichment"),
        engineType = c("simulation", "analysis")) {
    if (design$sided == 2) {
        designType <- match.arg(designType)
        engineType <- match.arg(engineType)
        stopIllegalArgument(designType, " ", engineType,
            " is only applicable for one-sided testing",
            functionName = ".assertIsOneSidedDesign",
            parameter = "designType",
            value = designType,
            relatedParameter = "engineType",
            relatedValue = engineType
        )
    }
}

.isMultiArmDataset <- function(dataInput) {
    return(inherits(dataInput, "Dataset") && dataInput$getNumberOfGroups() > 2)
}

.isMultiArmStageResults <- function(stageResults) {
    return(inherits(stageResults, "StageResults") && grepl("MultiArm", .getClassName(stageResults)))
}

.isEnrichmentStageResults <- function(stageResults) {
    return(inherits(stageResults, "StageResults") && grepl("Enrichment", .getClassName(stageResults)))
}

.isEnrichmentConditionalPowerResults <- function(conditionalPowerResults) {
    return(inherits(conditionalPowerResults, "ConditionalPowerResults") &&
        grepl("Enrichment", .getClassName(conditionalPowerResults)))
}

.isMultiHypothesesObject <- function(x) {
    return(.isEnrichmentAnalysisResults(x) || .isEnrichmentStageResults(x) ||
        .isMultiArmAnalysisResults(x) || .isMultiArmStageResults(x))
}

.isEnrichmentObject <- function(x) {
    return(.isEnrichmentAnalysisResults(x) || .isEnrichmentStageResults(x))
}

.isMultiArmAnalysisResults <- function(analysisResults) {
    return(inherits(analysisResults, "AnalysisResultsMultiArm"))
}

.isMultiHypothesesAnalysisResults <- function(x) {
    return(.isMultiArmAnalysisResults(x) || .isEnrichmentAnalysisResults(x))
}

.isEnrichmentDataset <- function(dataInput) {
    return(inherits(dataInput, "Dataset") && dataInput$.enrichmentEnabled)
}

.isEnrichmentAnalysisResults <- function(analysisResults) {
    return(inherits(analysisResults, "AnalysisResultsEnrichment"))
}

.isMultiArmSimulationResults <- function(simulationResults) {
    return(inherits(simulationResults, "SimulationResults") &&
        grepl("MultiArm", .getClassName(simulationResults)))
}

.isEnrichmentSimulationResults <- function(simulationResults) {
    return(inherits(simulationResults, "SimulationResults") &&
        grepl("Enrichment", .getClassName(simulationResults)))
}

.assertIsStageResultsMultiArm <- function(stageResults) {
    if (!inherits(stageResults, "StageResults")) {
        stopIllegalArgument("'stageResults' must be a multi-arm stage results object ",
            "(is ", .getClassName(stageResults), ")",
            functionName = ".assertIsStageResultsMultiArm",
            parameter = "stageResults",
            value = stageResults
        )
    }

    if (!.isMultiArmStageResults(stageResults)) {
        stopIllegalArgument("'stageResults' must be a multi-arm object ",
            "(is ", .getClassName(stageResults), ")",
            functionName = ".assertIsStageResultsMultiArm",
            parameter = "stageResults",
            value = stageResults
        )
    }
}

.assertIsStageResultsNonMultiHypotheses <- function(stageResults) {
    if (inherits(stageResults, "StageResults") && .isMultiArmStageResults(stageResults)) {
        stopIllegalArgument("'stageResults' must be a non-multi-arm object ",
            "(is ", .getClassName(stageResults), ")",
            functionName = ".assertIsStageResultsNonMultiHypotheses",
            parameter = "stageResults",
            value = stageResults
        )
    }

    if (inherits(stageResults, "StageResults") && .isEnrichmentStageResults(stageResults)) {
        stopIllegalArgument("'stageResults' must be a non-enrichment object ",
            "(is ", .getClassName(stageResults), ")",
            functionName = ".assertIsStageResultsNonMultiHypotheses",
            parameter = "stageResults",
            value = stageResults
        )
    }

    allowedClasses <- c(
        "StageResultsMeans",
        "StageResultsRates",
        "StageResultsSurvival"
    )
    if (!(.getClassName(stageResults) %in% allowedClasses)) {
        stopIllegalArgument("'stageResults' must be an instance of ",
            .arrayToString(allowedClasses, vectorLookAndFeelEnabled = FALSE),
            " (is ", .getClassName(stageResults, quote = TRUE), ")",
            functionName = ".assertIsStageResultsNonMultiHypotheses",
            parameter = "stageResults",
            value = stageResults
        )
    }
}

.assertIsDatasetNonMultiHypotheses <- function(dataInput) {
    if (.isMultiArmDataset(dataInput)) {
        stopIllegalArgument("'dataInput' must be a non-multi-arm dataset ",
            "(has ", dataInput$getNumberOfGroups(), " treatment arms)",
            functionName = ".assertIsDatasetNonMultiHypotheses",
            parameter = "dataInput",
            value = dataInput
        )
    }
    if (.isEnrichmentDataset(dataInput)) {
        stopIllegalArgument("'dataInput' must be a non-enrichment dataset ",
            "(has ", dataInput$getNumberOfSubsets(), " subsets)",
            functionName = ".assertIsDatasetNonMultiHypotheses",
            parameter = "dataInput",
            value = dataInput
        )
    }
}

.assertIsAnalysisResults <- function(analysisResults) {
    if (!inherits(analysisResults, "AnalysisResults")) {
        stopIllegalArgument("'analysisResults' ",
            "must be a valid 'AnalysisResults' object ",
            " (is ", .getClassName(analysisResults, quote = TRUE), ")",
            functionName = ".assertIsAnalysisResults",
            parameter = "analysisResults",
            value = analysisResults,
            relatedParameter = "AnalysisResults"
        )
    }
}

.isValidIntersectionTestMultiArm <- function(intersectionTest) {
    return(!is.null(intersectionTest) &&
        length(intersectionTest) == 1 && !is.na(intersectionTest) &&
        is.character(intersectionTest) &&
        intersectionTest %in% C_INTERSECTION_TESTS_MULTIARMED)
}

.getCorrectedIntersectionTestMultiArmIfNecessary <- function(
        design,
        intersectionTest,
        userFunctionCallEnabled = TRUE) {
    .assertIsCharacter(intersectionTest, "intersectionTest")
    intersectionTest <- intersectionTest[1]
    if (.isTrialDesignConditionalDunnett(design) && intersectionTest != "Dunnett") {
        if (userFunctionCallEnabled) {
            message <- paste0("Intersection test ", .pQuote(intersectionTest), " ")
            if (!.isValidIntersectionTestMultiArm(intersectionTest)) {
                message <- paste0(message, "is invalid, ")
            }
            message <- paste0(message, "will be ignored")
            message <- paste0(message, ifelse(!.isValidIntersectionTestMultiArm(intersectionTest), ", ", " "))
            message <- paste0(
                message, "and 'Dunnett' will be used instead ",
                "because conditional Dunnett test was specified as design"
            )
            warning(message, call. = FALSE)
        }
        intersectionTest <- "Dunnett"
    }
    return(intersectionTest)
}

.assertIsValidIntersectionTestMultiArm <- function(design, intersectionTest) {
    .assertIsCharacter(intersectionTest, "intersectionTest")
    intersectionTest <- intersectionTest[1]
    if (!.isValidIntersectionTestMultiArm(intersectionTest)) {
        stopIllegalArgument("'intersectionTest' ",
            "(", intersectionTest, ") must be one of ",
            .arrayToString(C_INTERSECTION_TESTS_MULTIARMED,
                encapsulate = TRUE
            ),
            functionName = ".assertIsValidIntersectionTestMultiArm",
            parameter = "intersectionTest",
            value = intersectionTest
        )
    }
    if (.isTrialDesignConditionalDunnett(design) &&
            intersectionTest != "Dunnett") {
        stopIllegalArgument("intersection test ",
            "(", .pQuote(intersectionTest), ") must be 'Dunnett' ",
            "because conditional Dunnett test was specified as design",
            functionName = ".assertIsValidIntersectionTestMultiArm",
            parameter = "intersectionTest",
            value = intersectionTest,
            relatedParameter = "design",
            relatedValue = design
        )
    }
}

.isValidIntersectionTestEnrichment <- function(intersectionTest) {
    return(!is.null(intersectionTest) &&
        length(intersectionTest) == 1 && !is.na(intersectionTest) &&
        is.character(intersectionTest) &&
        intersectionTest %in% C_INTERSECTION_TESTS_ENRICHMENT)
}

.assertIsValidIntersectionTestEnrichment <- function(design, intersectionTest) {
    .assertIsCharacter(intersectionTest, "intersectionTest")
    intersectionTest <- intersectionTest[1]
    if (!.isValidIntersectionTestEnrichment(intersectionTest)) {
        stopIllegalArgument("'intersectionTest' ",
            "(", intersectionTest, ") must be one of ",
            .arrayToString(C_INTERSECTION_TESTS_ENRICHMENT,
                encapsulate = TRUE
            ),
            functionName = ".assertIsValidIntersectionTestEnrichment",
            parameter = "intersectionTest",
            value = intersectionTest
        )
    }
    return(intersectionTest)
}

.ignoreParameterIfNotUsed <- function(
        paramName,
        paramValue,
        requirementLogical,
        requirementFailedReason,
        prefix = NA_character_) {
    if (all(is.na(paramValue)) || requirementLogical) {
        return(paramValue)
    }

    if (is.na(prefix) || trimws(prefix) == "") {
        prefix <- ""
    } else {
        prefix <- paste0(trimws(prefix), " ")
    }

    warning(prefix, .pQuote(paramName), " (",
        .arrayToString(paramValue), ") will be ignored because ",
        requirementFailedReason,
        call. = FALSE
    )
    return(NA_real_)
}

#
# This is a workaround for the following  R core bug:
#
# rCoreBugDemonstration <- function(stageX, ...) {
# 	result <- list(...); result$stageX <- stageX; return(result)
# }
# # bug: stage will be removed, stageX gets the value of stage
# rCoreBugDemonstration("A", stage = 1)
# # everything works as expected
# rCoreBugDemonstration("A", state = 1)
#
.stopInCaseOfIllegalStageDefinition <- function(stageResults, ...) {
    stage <- list(...)[["stage"]]
    if (is.null(stage) && is.numeric(stageResults) &&
            stageResults %in% 1L:C_KMAX_UPPER_BOUND) {
        stage <- stageResults
    }
    if (!is.null(stage)) {
        stopIllegalArgument("'stage' (", stage, ") can only be defined in ",
            "getStageResults() or getAnalysisResults()",
            functionName = ".stopInCaseOfIllegalStageDefinition",
            parameter = "stage",
            value = stage
        )
    }
}

.stopInCaseOfIllegalStageDefinition2 <- function(...) {
    forbiddenStage <- .getOptionalArgument("stage", ...)
    if (!is.null(forbiddenStage)) {
        stopIllegalArgument("'stage' (", forbiddenStage, ") can only be defined in ",
            "getStageResults() or getAnalysisResults()",
            functionName = ".stopInCaseOfIllegalStageDefinition2",
            parameter = "stage"
        )
    }
}

.assertIsValidTolerance <- function(tolerance) {
    .assertIsSingleNumber(tolerance, "tolerance")
    .assertIsInOpenInterval(tolerance, "tolerance", lower = 0, upper = 0.1)
}

.isValidVarianceOptionMultiArmed <- function(varianceOption) {
    return(!is.null(varianceOption) &&
        length(varianceOption) == 1 && !is.na(varianceOption) &&
        is.character(varianceOption) && varianceOption %in% C_VARIANCE_OPTIONS_MULTIARMED)
}

.assertIsValidVarianceOptionMultiArmed <- function(design, varianceOption) {
    if (!.isValidVarianceOptionMultiArmed(varianceOption)) {
        stopIllegalArgument("'varianceOption' should be one of ",
            .arrayToString(C_VARIANCE_OPTIONS_MULTIARMED, encapsulate = TRUE),
            functionName = ".assertIsValidVarianceOptionMultiArmed",
            parameter = "varianceOption",
            value = varianceOption
        )
    }
    if (.isTrialDesignConditionalDunnett(design) && varianceOption != C_VARIANCE_OPTION_DUNNETT) {
        stopIllegalArgument("variance option (", .pQuote(varianceOption), ") must be ", 
            .pQuote(C_VARIANCE_OPTION_DUNNETT), " because conditional Dunnett test was specified as design",
            functionName = ".assertIsValidVarianceOptionMultiArmed",
            parameter = "varianceOption",
            value = varianceOption,
            relatedParameter = "design",
            relatedValue = design
        )
    }
}

.assertIsValidVarianceOptionEnrichment <- function(varianceOption) {
    .assertIsSingleCharacter(varianceOption, "varianceOption")
    if (!varianceOption %in% C_VARIANCE_OPTIONS_ENRICHMENT) {
        stopIllegalArgument("'varianceOption' should be one of ",
            .arrayToString(C_VARIANCE_OPTIONS_ENRICHMENT, encapsulate = TRUE),
            functionName = ".assertIsValidVarianceOptionEnrichment",
            parameter = "varianceOption",
            value = varianceOption
        )
    }
}

.assertIsValidStdErrorEstimateRates <- function(stdErrorEstimate, dataInput) {
    .assertIsSingleCharacter(stdErrorEstimate, "stdErrorEstimate", naAllowed = TRUE)

    if (dataInput$getNumberOfGroups() == 1) {
        if (!is.na(stdErrorEstimate)) {
            warning("'stdErrorEstimate' (", stdErrorEstimate,
                ") will be ignored because data input has only one group",
                call. = FALSE
            )
        }

        return(invisible(stdErrorEstimate))
    }

    if (is.na(stdErrorEstimate)) {
        stdErrorEstimate <- C_RATES_STD_ERROR_ESTIMATE_DEFAULT
    }

    if (!stdErrorEstimate %in% C_RATES_STD_ERROR_ESTIMATE) {
        stopIllegalArgument("'stdErrorEstimate' (", stdErrorEstimate,
            ") must be ", .arrayToString(C_RATES_STD_ERROR_ESTIMATE,
                mode = "or", encapsulate = TRUE
            ),
            functionName = ".assertIsValidStdErrorEstimateRates",
            parameter = "stdErrorEstimate",
            value = stdErrorEstimate
        )
    }

    return(invisible(stdErrorEstimate))
}

.assertIsValidSummaryIntervalFormat <- function(intervalFormat) {
    .assertIsSingleCharacter(intervalFormat, "intervalFormat") # "[%s; %s]"
    if (!grepl("^[^%]*%s[^%]*%s[^%]*$", intervalFormat)) {
        stopIllegalArgument("'intervalFormat' (", intervalFormat,
            ") has an invalid format; ",
            "the control character %s must appear exactly twice; ",
            "to change it use 'options(\"rpact.summary.intervalFormat\" = \"[%s; %s]\")'",
            functionName = ".assertIsValidSummaryIntervalFormat",
            parameter = "intervalFormat",
            value = intervalFormat,
            relatedParameter = "options(\"rpact.summary.intervalFormat\" = \"[%s; %s]\")"
        )
    }
}

#'
#' @title
#' Check if Show Source Argument is Special Plot
#'
#' @description
#' The `.isSpecialPlotShowSourceArgument` function checks if the `showSource` argument
#' is a character and if it is one of the special plot show source arguments defined in `C_PLOT_SHOW_SOURCE_ARGUMENTS`.
#'
#' @param showSource The argument to be checked.
#'
#' @return
#' Returns `TRUE` if `showSource` is a character and is in `C_PLOT_SHOW_SOURCE_ARGUMENTS`, otherwise `FALSE`.
#'
#' @examples
#' .isSpecialPlotShowSourceArgument("commands")
#' .isSpecialPlotShowSourceArgument("invalidSource")
#'
#' @noRd
#'
.isSpecialPlotShowSourceArgument <- function(showSource) {
    return(is.character(showSource) && showSource %in% C_PLOT_SHOW_SOURCE_ARGUMENTS)
}

.assertIsValidTypeOfSelection <- function(typeOfSelection, rValue, epsilonValue, activeArms) {
    .assertIsCharacter(typeOfSelection, "typeOfSelection")
    typeOfSelection <- typeOfSelection[1]
    if (typeOfSelection == "rbest") {
        typeOfSelection <- "rBest"
    }
    if (!(typeOfSelection %in% C_TYPES_OF_SELECTION)) {
        stopIllegalArgument("'typeOfSelection' ", "(", typeOfSelection,
            ") must be one of ", .arrayToString(C_TYPES_OF_SELECTION,
                encapsulate = TRUE
            ),
            functionName = ".assertIsValidTypeOfSelection",
            parameter = "typeOfSelection",
            value = typeOfSelection
        )
    }

    if (typeOfSelection == "rBest") {
        .assertIsSingleNumber(rValue, "rValue",
            naAllowed = FALSE, noDefaultAvailable = TRUE
        )
        if (activeArms == 1) {
            warning("'typeOfSelection' (\"", typeOfSelection, "\") will be ignored ",
                "because 'activeArms' or 'populations' = 1",
                call. = FALSE
            )
        } else if (rValue > activeArms) {
            warning("'rValue' (", rValue, ") is larger than activeArms or populations ",
                "(", activeArms, ") and will be ignored",
                call. = FALSE
            )
        }
    } else if (!is.na(rValue)) {
        warning("'rValue' (", rValue, ") will be ignored because 'typeOfSelection' != \"rBest\"", call. = FALSE)
    }

    if (typeOfSelection == "epsilon") {
        .assertIsSingleNumber(epsilonValue, "epsilonValue",
            naAllowed = FALSE, noDefaultAvailable = TRUE
        )
        .assertIsInClosedInterval(epsilonValue, "epsilonValue",
            lower = 0, upper = NULL, naAllowed = TRUE
        )
    } else if (!is.na(epsilonValue)) {
        warning("'epsilonValue' (", epsilonValue, ") will be ignored ",
            "because 'typeOfSelection' != \"epsilon\"",
            call. = FALSE
        )
    }

    return(typeOfSelection)
}

.assertIsValidSuccessCriterion <- function(successCriterion) {
    .assertIsCharacter(successCriterion, "successCriterion")
    successCriterion <- successCriterion[1]
    if (!(successCriterion %in% C_SUCCESS_CRITERIONS)) {
        stopIllegalArgument("'successCriterion' ",
            "(", successCriterion, ") must be one of ", .arrayToString(C_SUCCESS_CRITERIONS,
                encapsulate = TRUE
            ),
            functionName = ".assertIsValidSuccessCriterion",
            parameter = "successCriterion",
            value = successCriterion
        )
    }
    return(successCriterion)
}

.assertIsValidEffectMeasure <- function(effectMeasure) {
    .assertIsCharacter(effectMeasure, "effectMeasure")
    effectMeasure <- effectMeasure[1]
    if (!(effectMeasure %in% C_EFFECT_MEASURES)) {
        stopIllegalArgument("'effectMeasure' ",
            "(", effectMeasure, ") must be one of ", .arrayToString(C_EFFECT_MEASURES,
                encapsulate = TRUE
            ),
            functionName = ".assertIsValidEffectMeasure",
            parameter = "effectMeasure",
            value = effectMeasure
        )
    }
    return(effectMeasure)
}

.assertIsValidMatrix <- function(
        x,
        argumentName,
        ...,
        expectedNumberOfColumns = NA_integer_,
        naAllowed = FALSE,
        returnSingleValueAsMatrix = FALSE,
        relatedParameter = NULL,
        relatedValue = NULL) {
    if (missing(x) || is.null(x) || length(x) == 0) {
        stopMissingArgument(.pQuote(argumentName), " must be specified",
            functionName = ".assertIsValidMatrix",
            parameter = argumentName,
            value = if (!missing(x)) x else NULL,
            relatedParameter = relatedParameter,
            relatedValue = relatedValue
        )
    }

    if (returnSingleValueAsMatrix && !is.matrix(x) &&
            (is.numeric(x) || is.character(x) || is.logical(x))) {
        if (length(x) == 1) {
            x <- matrix(x)
        } else if (length(x) > 1 && !is.na(expectedNumberOfColumns)) {
            if (length(x) %% expectedNumberOfColumns != 0) {
                stopIllegalArgument("the length of ",
                    .pQuote(argumentName), " (", .arrayToString(x), ") must be a divisor or a multiple ",
                    expectedNumberOfColumns,
                    functionName = ".assertIsValidMatrix",
                    parameter = argumentName,
                    value = x
                )
            }

            x <- matrix(x, ncol = expectedNumberOfColumns)
        }
    }

    if (!is.matrix(x)) {
        stopIllegalArgument(.pQuote(argumentName), " (", .getClassName(x), ") ",
            "must be a valid matrix",
            functionName = ".assertIsValidMatrix",
            parameter = argumentName,
            value = x
        )
    }

    if (!naAllowed && anyNA(x)) {
        stopIllegalArgument(.pQuote(argumentName), " (", .arrayToString(x), ") ",
            "must not contain NA's",
            functionName = ".assertIsValidMatrix",
            parameter = argumentName,
            value = x
        )
    }

    if (!is.numeric(x)) {
        stopIllegalArgument(.pQuote(argumentName), " (", .arrayToString(x), ") ",
            "must be a valid numeric matrix",
            functionName = ".assertIsValidMatrix",
            parameter = argumentName,
            value = x
        )
    }

    if (!is.na(expectedNumberOfColumns) && ncol(x) != expectedNumberOfColumns) {
        stopIllegalArgument(.pQuote(argumentName), " (", .arrayToString(x), ") ",
            "must be a numeric matrix with ",
            expectedNumberOfColumns, " columns",
            functionName = ".assertIsValidMatrix",
            parameter = argumentName,
            value = x
        )
    }

    return(invisible(x))
}

.assertIsValidDecisionMatrix <- function(decisionMatrix, kMax) {
    .assertIsValidMatrix(decisionMatrix, "decisionMatrix", naAllowed = FALSE)
    if (!(nrow(decisionMatrix) %in% c(2, 4))) {
        stopIllegalArgument("'decisionMatrix' must have two or four rows",
            functionName = ".assertIsValidDecisionMatrix", 
		parameter ="decisionMatrix",
            value = decisionMatrix
        )
    }
    if (ncol(decisionMatrix) != kMax) {
        stopIllegalArgument("'decisionMatrix' must have 'kMax' ",
            "(= length(informationRates) = ", kMax, ") columns",
            functionName = ".assertIsValidDecisionMatrix",
            parameter = "decisionMatrix",
            relatedParameter = "kMax",
            relatedValue = kMax,
            value = decisionMatrix
        )
    }
    if (any(decisionMatrix[2:nrow(decisionMatrix), ] < decisionMatrix[1:(nrow(decisionMatrix) - 1), ])) {
        stopIllegalArgument("'decisionMatrix' needs to be increasing in each column",
            functionName = ".assertIsValidDecisionMatrix",
            parameter = "decisionMatrix",
            value = decisionMatrix
        )
    }
}

.assertIsValidTypeOfShape <- function(typeOfShape) {
    .assertIsCharacter(typeOfShape, "typeOfShape")
    typeOfShape <- typeOfShape[1]
    if (!(typeOfShape %in% C_TYPES_OF_SHAPE)) {
        stopIllegalArgument("'typeOfShape' ", "(", typeOfShape, ") ",
            "must be one of ", .arrayToString(C_TYPES_OF_SHAPE,
                encapsulate = TRUE
            ),
            functionName = ".assertIsValidTypeOfShape",
            parameter = "typeOfShape",
            value = typeOfShape
        )
    }
    return(typeOfShape)
}

.assertHasLength <- function(x, argumentName, len, lenArgName, ..., naAllowed = FALSE) {
    if (isTRUE(naAllowed) && all(is.na(x))) {
        return(invisible())
    }

    if (length(x) != len) {
        stopIllegalArgument(sQuote(argumentName), " (", .arrayToString(x), ") ",
            "must have length ", sQuote(lenArgName), " (", len,
            ")",
            functionName = ".assertHasLength",
            parameter = argumentName,
            relatedParameter = lenArgName,
            value = x
        )
    }
}

.assertIsValidEffectMatrix <- function(
        ...,
        simulationResults,
        activeArms,
        typeOfShape = c("linear", "sigmoidEmax", "userDefined"),
        effectMatrix,
        valueMaxVector,
        valueMaxVectorName = c("muMaxVector", "piMaxVector", "omegaMaxVector"),
        piControl = NULL,
        gED50,
        gMax,
        slope,
        doseLevels) {
    typeOfShape <- match.arg(typeOfShape)
    valueMaxVectorName <- match.arg(valueMaxVectorName)

    .assertIsNumericVector(doseLevels, "doseLevels", naAllowed = TRUE)
    .assertHasLength(doseLevels, "doseLevels", gMax, "gMax", naAllowed = TRUE)
    .assertValuesAreStrictlyIncreasing(doseLevels, "doseLevels", naAllowed = TRUE)
    .assertIsInOpenInterval(doseLevels, "doseLevels", lower = 0, upper = NULL, naAllowed = TRUE)

    if (typeOfShape == "userDefined") {
        effectMatrix <- .assertIsValidMatrix(effectMatrix, "effectMatrix",
            expectedNumberOfColumns = gMax, naAllowed = FALSE, returnSingleValueAsMatrix = TRUE,
            relatedParameter = "typeOfShape", 
		relatedValue = typeOfShape
        )
        .assertIsNumericVector(valueMaxVector, valueMaxVectorName, naAllowed = TRUE)
        valueMaxVectorDefault <- C_ALTERNATIVE_POWER_SIMULATION_DEFAULT
        if (valueMaxVectorName == "piMaxVector") {
            .assertIsInOpenInterval(effectMatrix, "effectMatrix",
                lower = 0, upper = 1, naAllowed = FALSE
            )
            valueMaxVectorDefault <- C_PI_1_DEFAULT
        } else if (valueMaxVectorName == "omegaMaxVector") {
            .assertIsInOpenInterval(effectMatrix, "effectMatrix",
                lower = 0, upper = NULL, naAllowed = FALSE
            )
            valueMaxVectorDefault <- C_RANGE_OF_HAZARD_RATIOS_DEFAULT
        }
        if (!all(is.na(valueMaxVector)) && !identical(valueMaxVector, valueMaxVectorDefault)) {
            warning(sQuote(valueMaxVectorName), " (", .arrayToString(valueMaxVector),
                ") will be ignored because it will be set to first column of 'effectMatrix'",
                call. = FALSE
            )
        }
        if (!is.null(doseLevels) && !anyNA(doseLevels)) {
            warning("'doseLevels' (", .arrayToString(doseLevels), ") ",
                "will be ignored because 'typeOfShape' ",
                "is defined as ", .pQuote(typeOfShape), 
                call. = FALSE
            )
        }
    } else if (!is.null(effectMatrix)) {
        warning("'effectMatrix' will be ignored because 'typeOfShape' ",
            "is defined as ", .pQuote(typeOfShape), 
            call. = FALSE
        )
    }

    if (anyNA(doseLevels)) {
        doseLevels <- 1:gMax
    }

    if (typeOfShape %in% c("sigmoidEmax", "linear")) {
        .assertIsNumericVector(valueMaxVector, valueMaxVectorName,
            naAllowed = FALSE, noDefaultAvailable = TRUE
        )
        if (valueMaxVectorName %in% c("piMaxVector", "omegaMaxVector")) {
            .assertIsInOpenInterval(
                valueMaxVector,
                valueMaxVectorName,
                lower = 0,
                upper = if (valueMaxVectorName == "piMaxVector") 1 else NULL,
                naAllowed = FALSE
            )
        }
    }

    effectMatrix <- .createEffectMatrix(
        simulationResults = simulationResults,
        typeOfShape = typeOfShape,
        effectMatrix = effectMatrix,
        valueMaxVector = valueMaxVector,
        valueMaxVectorName = valueMaxVectorName,
        piControl = piControl,
        gED50 = gED50,
        gMax = gMax,
        slope = slope,
        doseLevels = doseLevels
    )
    simulationResults$effectMatrix <- t(effectMatrix)
    return(effectMatrix)
}

.assertIsValidEffectMatrixMeans <- function(
        ...,
        simulationResults,
        activeArms,
        typeOfShape,
        effectMatrix,
        muMaxVector,
        gED50,
        gMax,
        slope,
        doseLevels) {
    return(.assertIsValidEffectMatrix(
        simulationResults = simulationResults,
        activeArms = activeArms,
        typeOfShape = typeOfShape,
        effectMatrix = effectMatrix,
        valueMaxVector = muMaxVector,
        valueMaxVectorName = "muMaxVector",
        gED50 = gED50,
        gMax = gMax,
        slope = slope,
        doseLevels = doseLevels
    ))
}

.assertIsValidEffectMatrixRates <- function(
        ...,
        simulationResults,
        activeArms,
        typeOfShape,
        effectMatrix,
        piMaxVector,
        piControl,
        gED50,
        gMax,
        slope,
        doseLevels) {
    return(.assertIsValidEffectMatrix(
        simulationResults = simulationResults,
        activeArms = activeArms,
        typeOfShape = typeOfShape,
        effectMatrix = effectMatrix,
        valueMaxVector = piMaxVector,
        valueMaxVectorName = "piMaxVector",
        piControl = piControl,
        gED50 = gED50,
        gMax = gMax,
        slope = slope,
        doseLevels = doseLevels
    ))
}

.assertIsValidEffectMatrixSurvival <- function(
        ...,
        simulationResults,
        activeArms,
        typeOfShape,
        effectMatrix,
        omegaMaxVector,
        gED50,
        gMax,
        slope,
        doseLevels) {
    return(.assertIsValidEffectMatrix(
        simulationResults = simulationResults,
        activeArms = activeArms,
        typeOfShape = typeOfShape,
        effectMatrix = effectMatrix,
        valueMaxVector = omegaMaxVector,
        valueMaxVectorName = "omegaMaxVector",
        gED50 = gED50,
        gMax = gMax,
        slope = slope,
        doseLevels = doseLevels
    ))
}

.assertIsValidPlannedSubjects <- function(plannedSubjects, kMax) {
    .assertIsIntegerVector(plannedSubjects, "plannedSubjects", validateType = FALSE)
    if (length(plannedSubjects) != kMax) {
        stopIllegalArgument("'plannedSubjects' (", .arrayToString(plannedSubjects), ") ",
            "must have length 'kMax' (", kMax, ")",
            functionName = ".assertIsValidPlannedSubjects",
            parameter = "plannedSubjects",
            value = plannedSubjects,
            relatedParameter = "kMax",
            relatedValue = kMax
        )
    }
    .assertIsInClosedInterval(plannedSubjects, "plannedSubjects", lower = 1, upper = NULL)
    .assertValuesAreStrictlyIncreasing(plannedSubjects, "plannedSubjects")
}

.isAlphaSpendingDesign <- function(design) {
    if (!.isTrialDesignInverseNormalOrGroupSequential(design)) {
        return(FALSE)
    }

    return(grepl("^as", design$typeOfDesign))
}

.isDelayedInformationEnabled <- function(..., design = NULL, delayedInformation = NULL) {
    if (is.null(design) && is.null(delayedInformation)) {
        stopMissingArgument("either 'design' or 'delayedInformation' must be specified",
            functionName = ".isDelayedInformationEnabled",
            parameter = "design",
            relatedParameter = "delayedInformation"
        )
    }

    if (!is.null(design)) {
        if (.isTrialDesignFixed(design)) {
            return(FALSE)
        }

        if (!.isTrialDesignInverseNormalOrGroupSequential(design)) {
            return(FALSE)
        }

        delayedInformation <- design[["delayedInformation"]]
    }
    if (is.null(delayedInformation)) {
        return(FALSE)
    }

    return(all(!is.na(delayedInformation)) && any(delayedInformation >= 1e-03))
}

.assertIsValidCountsParameterCombination <- function(
        existingParamNames,
        forbiddenParamNames,
        params,
        ...,
        requiredParamNames = character()) {
    theta <- numeric(0)
    for (existingParamName in existingParamNames) {
        existingParamValue <- params[[existingParamName]]
        if (is.null(existingParamValue) || all(is.na(existingParamValue))) {
            return(invisible())
        }
        if (identical(existingParamName, "theta") &&
                any(c("lambda", "lambda1") %in% existingParamNames)) {
            theta <- existingParamValue
        }
    }

    paramNames <- names(params)
    if (length(requiredParamNames) > 0) {
        existingParamNamesWithValue <- paramNames[sapply(paramNames, function(name) {
            value <- params[[name]]
            return(!is.null(value) && !all(is.na(value)))
        })]
        requiredParamNamesFound <- requiredParamNames[requiredParamNames %in% existingParamNamesWithValue]
        if (length(requiredParamNamesFound) < length(requiredParamNames)) {
            stopMissingArgument("if ", .arrayToString(existingParamNames, mode = "and", encapsulate = TRUE), " ",
                ifelse(length(requiredParamNames) == 1, "is", "are"),
                " specified, ", .arrayToString(requiredParamNames,
                    mode = "and", encapsulate = TRUE
                ), " must also be specified",
                functionName = ".assertIsValidCountsParameterCombination",
                parameter = "existingParamNames",
                value = existingParamNames,
                relatedParameter = "requiredParamNames",
                relatedValue = requiredParamNames
            )
        }
    }

    foundParamNames <- character()
    for (forbiddenParamName in forbiddenParamNames) {
        forbiddenParamValue <- params[[forbiddenParamName]]
        if (!is.null(forbiddenParamValue) && !all(is.na(forbiddenParamValue))) {
            foundParamNames <- c(foundParamNames, forbiddenParamName)
        }
    }

    if (length(theta) > 1) {
        existingParamNames <- existingParamNames[existingParamNames != "theta"]
        stopIllegalArgument("'theta' cannot be specified as vector if ",
            sQuote(existingParamNames[1]), " is specified",
            functionName = ".assertIsValidCountsParameterCombination",
            parameter = "theta",
            relatedParameter = existingParamNames[1]
        )
    } else if (length(theta) > 0 && "lambda1" %in% paramNames && length(params$lambda1) > 1) {
        stopIllegalArgument("'lambda1' cannot be specified as vector if ",
            sQuote("theta"), " is specified",
            functionName = ".assertIsValidCountsParameterCombination",
            parameter = "lambda1",
            relatedParameter = "theta"
        )
    }

    if (length(foundParamNames) == 0) {
        return(invisible())
    }

    stopConflictingArguments("if ",
        .arrayToString(existingParamNames, mode = "and", encapsulate = TRUE),
        " are specified, ", .arrayToString(foundParamNames, mode = "and", encapsulate = TRUE),
        " must not be specified",
        functionName = ".assertIsValidCountsParameterCombination",
        parameter = "existingParamNames", value = existingParamNames,
        relatedParameter = "foundParamNames",
        relatedValue = foundParamNames
    )
}

.assertIsValidEffectCountData <- function(
        sampleSizeEnabled,
        sided,
        lambda1,
        lambda2,
        lambda,
        theta,
        thetaH0,
        overdispersion) {
    .assertIsSingleInteger(sided, "sided", validateType = FALSE)
    if (sided != 1 && sided != 2) {
        stopIllegalArgument("'sided' (", sided, ") must be defined as 1 or 2",
            parameter = "sided", value = sided,
            constraint = "must be defined as 1 or 2", 
		functionName = ".assertIsValidEffectCountData"
        )
    }
    .assertIsSingleNumber(lambda, "lambda", naAllowed = TRUE)
    .assertIsInOpenInterval(lambda, "lambda", lower = 0, upper = NULL, naAllowed = TRUE)
    .assertIsNumericVector(lambda1, "lambda1", naAllowed = TRUE)
    .assertIsInOpenInterval(lambda1, "lambda1", lower = 0, upper = NULL, naAllowed = TRUE)
    .assertIsSingleNumber(lambda2, "lambda2", naAllowed = TRUE)
    .assertIsInOpenInterval(lambda2, "lambda2", lower = 0, upper = NULL, naAllowed = TRUE)
    .assertIsNumericVector(theta, "theta", naAllowed = TRUE)
    .assertIsInOpenInterval(theta, "theta", lower = 0, upper = NULL, naAllowed = TRUE)
    .assertIsValidThetaH0(thetaH0, endpoint = "counts", groups = 2)
    .assertIsSingleNumber(overdispersion, "overdispersion", naAllowed = TRUE)
    .assertIsInClosedInterval(overdispersion, "overdispersion", lower = 0, upper = NULL, naAllowed = TRUE)

    params <- list(
        "lambda1" = lambda1,
        "lambda2" = lambda2,
        "lambda" = lambda,
        "theta" = theta,
        "thetaH0" = thetaH0,
        "overdispersion" = overdispersion
    )

    .assertIsValidCountsParameterCombination(c("lambda", "theta"), c("lambda1", "lambda2"), params)
    .assertIsValidCountsParameterCombination("lambda", c("lambda1", "lambda2"), params, requiredParamNames = "theta")
    .assertIsValidCountsParameterCombination(c("lambda2", "theta"), c("lambda1", "lambda"), params)
    .assertIsValidCountsParameterCombination(c("lambda1", "theta"), c("lambda2", "lambda"), params)
    .assertIsValidCountsParameterCombination(c("lambda", "lambda1"), c("lambda2", "theta"), params)
    .assertIsValidCountsParameterCombination(c("lambda", "lambda2"), c("lambda1", "theta"), params)
    .assertIsValidCountsParameterCombination(c("lambda1", "lambda2"), c("lambda", "theta"), params)

    numberOfParameters <- sum(is.na(lambda2), anyNA(lambda1), is.na(lambda), anyNA(theta))
    if (numberOfParameters != 2) {
        message <- "exactly two of the parameters 'lambda', 'lambda1', 'lambda2', 'theta' must be specified"
        do.call(
            what = if (numberOfParameters > 2) stopConflictingArguments else stopMissingArgument,
            args = list(message,
                functionName = ".assertIsValidEffectCountData",
                parameter = c("lambda", "lambda1", "lambda2", "theta"),
                relatedParameter = "numberOfParameters",
                relatedValue = numberOfParameters
            )
        )
    }

    if (!is.na(lambda2) && all(!is.na(theta))) {
        lambda1 <- lambda2 * theta
    } else if (all(!is.na(lambda1)) && all(!is.na(theta))) {
        lambda2 <- lambda1 / theta
    }

    if (sampleSizeEnabled && !all(is.na(lambda1)) && !all(is.na(lambda2)) && !is.na(thetaH0) &&
            any(abs(lambda1 / lambda2 - thetaH0) < 1e-12, na.rm = TRUE)) {
        stopConflictingArguments("'lambda1 / lambda2' (",
            .arrayToString(round(lambda1 / lambda2, 4)), ") must be != 'thetaH0' (",
            thetaH0, ")",
            functionName = ".assertIsValidEffectCountData",
            parameter = "lambda1 / lambda2",
            value = lambda1 / lambda2,
            relatedParameter = "thetaH0",
            relatedValue = thetaH0
        )
    }
}

.assertParametersAreSpecifiedCorrectlyTogether <- function(
        ...,
        case = c("notTogether", "eitherOr"),
        .paramNames = NULL) {
    params <- list(...)
    if (length(params) != 2) {
        if (is.null(names(params)) ||
                length(params[!(names(params) %in% c("case", ".paramNames"))]) < 2) {
            stopRuntimeIssue("two or more parameters must be specified",
                functionName = ".assertParametersAreSpecifiedCorrectlyTogether"
            )
        }
    }
    case <- match.arg(case)
    if (!is.null(.paramNames)) {
        paramNames <- .paramNames
    } else {
        paramNames <- names(params)
    }
    if (is.null(paramNames) || any(nchar(paramNames) == 0)) {
        stopRuntimeIssue("all arguments must be named",
            functionName = ".assertParametersAreSpecifiedCorrectlyTogether"
        )
    }
    if (case == "notTogether" && !all(is.na(params[[1]])) && !all(is.na(params[[2]]))) {
        paramVector <- c()
        for (i in seq_along(params)) {
            paramVector <- c(paramVector, paste0(
                sQuote(paramNames[i]),
                " (", .arrayToString(params[[i]]), ")"
            ))
        }
        stopIllegalArgument(.arrayToString(paramVector, mode = "and"),
            " cannot be specified together",
            functionName = ".assertParametersAreSpecifiedCorrectlyTogether",
            parameter = "paramVector",
            value = paramVector
        )
    } else if (case == "eitherOr" && all(is.na(params[[1]])) && all(is.na(params[[2]]))) {
        stopMissingArgument("either ",
            .arrayToString(paramNames, mode = "or", encapsulate = TRUE),
            " must be specified",
            functionName = ".assertParametersAreSpecifiedCorrectlyTogether",
            parameter = "paramNames",
            value = paramNames
        )
    }
}

.assertAreValidParametersCountData <- function(
        ...,
        sampleSizeEnabled,
        simulationEnabled,
        fixedExposureTime,
        followUpTime,
        accrualTime,
        accrualIntensity,
        maxNumberOfSubjects,
        accrualIntensityValidationEnabled = TRUE,
        accrualTimeIsMandatory = FALSE) {
    .assertIsSingleLogical(sampleSizeEnabled, "sampleSizeEnabled")
    .assertIsSingleNumber(fixedExposureTime, "fixedExposureTime", naAllowed = TRUE)
    .assertIsInOpenInterval(fixedExposureTime, "fixedExposureTime", lower = 0, upper = NULL, naAllowed = TRUE)
    .assertIsSingleNumber(followUpTime, "followUpTime", naAllowed = TRUE)
    .assertIsInClosedInterval(followUpTime, "followUpTime", lower = 0, upper = NULL, naAllowed = TRUE)
    .assertIsValidAccrualTime(accrualTime, naAllowed = isFALSE(accrualTimeIsMandatory))
    .assertIsNumericVector(accrualIntensity, "accrualIntensity", naAllowed = TRUE)
    .assertIsInClosedInterval(accrualIntensity, "accrualIntensity", lower = 0, upper = NULL, naAllowed = TRUE)
    .assertIsValidMaxNumberOfSubjects(maxNumberOfSubjects, naAllowed = TRUE)

    if (!all(is.na(accrualTime)) && length(accrualTime) > 2 &&
            all(is.na(accrualIntensity))) {
        stopIllegalArgument("'accrualIntensity' need to be specified if ",
            "piecewise accrual is enabled, i.e., ", "'accrualTime' (",
            .arrayToString(accrualTime), ") has more than two elements",
            functionName = ".assertAreValidParametersCountData",
            parameter = "accrualIntensity",
            relatedParameter = "accrualTime",
            relatedValue = accrualTime,
            value = accrualIntensity
        )
    }

    if (sampleSizeEnabled) {
        if (is.na(maxNumberOfSubjects) && anyNA(accrualIntensity)) {
            .assertParametersAreSpecifiedCorrectlyTogether(
                "fixedExposureTime" = fixedExposureTime,
                "followUpTime" = followUpTime
            )
            if (!all(is.na(accrualTime))) {
                .assertParametersAreSpecifiedCorrectlyTogether(
                    "fixedExposureTime" = fixedExposureTime,
                    "followUpTime" = followUpTime,
                    "accrualIntensity" = accrualIntensity,
                    case = "eitherOr"
                )
            } else {
                .assertParametersAreSpecifiedCorrectlyTogether(
                    "fixedExposureTime" = fixedExposureTime,
                    "followUpTime" = followUpTime,
                    case = "eitherOr"
                )
            }
        }
    } else {
        .assertParametersAreSpecifiedCorrectlyTogether(
            "fixedExposureTime" = fixedExposureTime,
            "followUpTime" = followUpTime
        )
        .assertParametersAreSpecifiedCorrectlyTogether(
            "fixedExposureTime" = fixedExposureTime,
            "followUpTime" = followUpTime,
            case = "eitherOr"
        )
        .assertParametersAreSpecifiedCorrectlyTogether(
            "maxNumberOfSubjects" = maxNumberOfSubjects,
            "accrualIntensity" = accrualIntensity,
            case = "eitherOr",
            .paramNames = if (simulationEnabled) c("maxNumberOfSubjects", "accrualIntensity") else NULL
        )
    }
    if (anyNA(accrualTime) && !is.na(followUpTime)) {
        stopIllegalArgument("'accrualTime' needs to be specified if ",
            "'followUpTime' (", followUpTime, ") is specified",
            functionName = ".assertAreValidParametersCountData",
            parameter = "accrualTime",
            relatedParameter = "followUpTime",
            relatedValue = followUpTime,
            value = accrualTime
        )
    }
    if (!is.na(followUpTime) && length(accrualTime) > 2) {
        stopIllegalArgument("'accrualTime' (", .arrayToString(accrualTime), ") ",
            "has more than two elements; ",
            "'followUpTime' (", followUpTime, ") can only be specified if ",
            "'accrualTime' has one or two elements",
            functionName = ".assertAreValidParametersCountData",
            parameter = "accrualTime",
            value = accrualTime,
            relatedParameter = "followUpTime",
            relatedValue = followUpTime
        )
    }
    if (!is.na(maxNumberOfSubjects) && length(accrualTime) > 2) {
        stopIllegalArgument("'accrualTime' (", .arrayToString(accrualTime), ") ",
            "has more than two elements; ",
            "'maxNumberOfSubjects' (", maxNumberOfSubjects, ") can only be ",
            "specified if 'accrualTime' has one or two elements",
            functionName = ".assertAreValidParametersCountData",
            parameter = "accrualTime",
            value = accrualTime,
            relatedParameter = "maxNumberOfSubjects",
            relatedValue = maxNumberOfSubjects
        )
    }
    if (sampleSizeEnabled) {
        .assertParametersAreSpecifiedCorrectlyTogether(
            "maxNumberOfSubjects" = maxNumberOfSubjects,
            "followUpTime" = followUpTime
        )
        .assertParametersAreSpecifiedCorrectlyTogether(
            "accrualIntensity" = accrualIntensity,
            "followUpTime" = followUpTime
        )
        .assertParametersAreSpecifiedCorrectlyTogether(
            "maxNumberOfSubjects" = maxNumberOfSubjects,
            "fixedExposureTime" = fixedExposureTime
        )
        .assertParametersAreSpecifiedCorrectlyTogether(
            "accrualIntensity" = accrualIntensity,
            "fixedExposureTime" = fixedExposureTime
        )
        if (!is.na(maxNumberOfSubjects) && anyNA(accrualTime)) {
            stopIllegalArgument("'accrualTime' needs to be specified if ",
                "'maxNumberOfSubjects' ", "(", maxNumberOfSubjects, ") is specified",
                functionName = ".assertAreValidParametersCountData",
                parameter = "accrualTime",
                relatedParameter = "maxNumberOfSubjects",
                relatedValue = maxNumberOfSubjects,
                value = accrualTime
            )
        }
    } else if (!simulationEnabled) {
        if (is.na(maxNumberOfSubjects) && (anyNA(accrualIntensity) || anyNA(accrualTime))) {
            stopIllegalArgument("'accrualTime' and 'accrualIntensity' ",
                "need to be specified if 'maxNumberOfSubjects' is not specified",
                functionName = ".assertAreValidParametersCountData",
                parameter = "accrualTime",
                relatedParameter = "accrualIntensity",
                value = accrualTime
            )
        }
        if (anyNA(accrualIntensity) && !anyNA(accrualTime) && !is.na(fixedExposureTime)) {
            warning(
                "Specification of 'accrualTime' has no ",
                "influence of calculation and will be ignored",
                call. = FALSE
            )
        }
    } else {
        if (is.na(maxNumberOfSubjects) && (anyNA(accrualIntensity) || anyNA(accrualTime))) {
            stopIllegalArgument("'accrualTime' and 'accrualIntensity' ",
                "need to be specified if 'maxNumberOfSubjects' is not specified",
                functionName = ".assertAreValidParametersCountData",
                parameter = "accrualTime",
                relatedParameter = "accrualIntensity",
                value = accrualTime
            )
        }
    }

    .assertParametersAreSpecifiedCorrectlyTogether(
        "maxNumberOfSubjects" = maxNumberOfSubjects,
        "accrualIntensity" = accrualIntensity,
        .paramNames = if (simulationEnabled) c("maxNumberOfSubjects", "accrualIntensity") else NULL
    )

    if (!anyNA(accrualIntensity) && (accrualTime[1] != 0) &&
            length(accrualIntensity) == 1 &&
            length(accrualTime) != length(accrualIntensity)) {
        stopIllegalArgument("'accrualTime' (", .arrayToString(accrualTime), ") and ",
            "'accrualIntensity' (", .arrayToString(accrualIntensity), ") does not match",
            functionName = ".assertAreValidParametersCountData",
            parameter = "accrualTime",
            value = accrualTime,
            relatedParameter = "accrualIntensity",
            relatedValue = accrualIntensity
        )
    }
    if (!anyNA(accrualIntensity) && (length(accrualIntensity) > 1) &&
            length(accrualTime) != length(accrualIntensity) + 1) {
        stopIllegalArgument("'accrualTime' (", .arrayToString(accrualTime), ") and ",
            "'accrualIntensity' (", .arrayToString(accrualIntensity), ") does not match",
            functionName = ".assertAreValidParametersCountData",
            parameter = "accrualTime", value = accrualTime,
            relatedParameter = "accrualIntensity",
            relatedValue = accrualIntensity
        )
    }
    if (accrualIntensityValidationEnabled && anyNA(accrualIntensity) &&
            length(accrualTime) > 1) {
        stopIllegalArgument("'accrualIntensity' (", .arrayToString(accrualIntensity), ") ",
            "is not correctly specified",
            functionName = ".assertAreValidParametersCountData",
            parameter = "accrualIntensity",
            value = accrualIntensity
        )
    }
}

.assertAreValidCalendarTimes <- function(plannedCalendarTime, kMax) {
    .assertIsNumericVector(plannedCalendarTime, "plannedCalendarTime", naAllowed = FALSE)
    .assertValuesAreStrictlyIncreasing(plannedCalendarTime, "plannedCalendarTime")
    .assertIsInOpenInterval(plannedCalendarTime, "plannedCalendarTime",
        lower = 0, upper = NULL, naAllowed = FALSE
    )
    if (length(plannedCalendarTime) != kMax) {
        stopConflictingArguments(
            sprintf(
                "length of 'plannedCalendarTime' (%s) must be equal to 'kMax' (%s)",
                length(plannedCalendarTime), kMax
            ),
            parameter = "plannedCalendarTime",
            value = plannedCalendarTime,
            relatedParameter = "kMax",
            relatedValue = kMax,
            functionName = ".assertAreValidCalendarTimes"
        )
    }
}

.assertIsValidPlotType <- function(type, naAllowed = FALSE) {
    if (is.null(type) || length(type) == 0 || (!naAllowed && all(is.na(type)))) {
        stopMissingArgument("'type' must be defined",
            functionName = ".assertIsValidPlotType",
            parameter = "type",
            value = type
        )
    }

    if (!is.numeric(type) && !is.character(type)) {
        stopMissingArgument("'type' must be an integer or character value or vector (is ",
            .getClassName(type), ")",
            functionName = ".assertIsValidPlotType",
            parameter = "type",
            value = .getClassName(type)
        )
    }

    if (is.numeric(type)) {
        .assertIsIntegerVector(type, "type",
            naAllowed = naAllowed, validateType = FALSE
        )
    }
}

.showFutilityBoundsUnnecessaryArgumentWarning <- function(
        argumentName,
        argValue,
        sourceScale,
        targetScale) {
    valueStr <- ""
    if (!is.null(argValue) && is.numeric(argValue) && length(argValue) > 0) {
        valueStr <- paste0(" (", .arrayToString(argValue), ")")
    }

    warning(
        sQuote(argumentName), valueStr, " will be ignored ",
        "because it is not required for the conversion from ", 
        .pQuote(sourceScale), " to ", .pQuote(targetScale),
        call. = FALSE
    )
}

.showFutilityBoundsMissingArgumentError <- function(
        argumentName,
        scaleName,
        scaleValue) {
    stopMissingArgument(sQuote(argumentName), " needs to be specified for ",
        sQuote(scaleName), " = ", dQuote(scaleValue),
        functionName = ".showFutilityBoundsMissingArgumentError",
        parameter = argumentName,
        relatedParameter = scaleName,
        relatedValue = scaleValue
    )
}

C_REQUIRED_FUTILITY_BOUNDS_ARGS_BY_SCALE <- list(
    "vector" = list(
        effectEstimate = c("information[1]"),
        conditionalPower = c("design", "theta", "information[2]"),
        condPowerAtObserved = c("design", "information"),
        predictivePower = c("design", "information"),
        reverseCondPower = c("design")
    ),
    "separate" = list(
        effectEstimate = c("information1"),
        conditionalPower = c("design", "theta", "information2"),
        condPowerAtObserved = c("design", "information1", "information2"),
        predictivePower = c("design", "information1", "information2"),
        reverseCondPower = c("design")
    )
)

.getValidFutilityBoundVectorIndices <- function(sourceScale, targetScale) {
    if (sourceScale == "effectEstimate" &&
            !targetScale %in% c("conditionalPower", "condPowerAtObserved", "predictivePower")) {
        return(1L)
    }

    if (targetScale == "effectEstimate" &&
            !sourceScale %in% c("conditionalPower", "condPowerAtObserved", "predictivePower")) {
        return(1L)
    }

    if (sourceScale == "conditionalPower" &&
            !targetScale %in% c("effectEstimate", "condPowerAtObserved", "predictivePower")) {
        return(2L)
    }

    if (targetScale == "conditionalPower" &&
            !sourceScale %in% c("effectEstimate", "condPowerAtObserved", "predictivePower")) {
        return(2L)
    }

    return(c(1L, 2L))
}

.isFutilityBoundsArgumentMissing <- function(name, value) {
    if (identical(name, "design")) {
        return(is.null(value))
    }

    return(all(is.na(value)))
}

.checkFutilityBoundsScaleArgs <- function(scaleValue, scaleLabel, args, informationVectorInput) {
    type <- ifelse(informationVectorInput, "vector", "separate")
    reqs <- C_REQUIRED_FUTILITY_BOUNDS_ARGS_BY_SCALE[[type]][[scaleValue]]
    if (length(reqs) == 0L) {
        return(invisible())
    }

    for (arg in reqs) {
        argName <- gsub("\\[.*\\]$", "", arg)
        if (grepl("\\[\\d+\\]$", arg)) {
            number <- sub("^.*\\[(\\d+)\\]$", "\\1", arg)
            argName <- paste0(argName, number)
        }
        argValue <- args[[argName]]
        if (.isFutilityBoundsArgumentMissing(arg, argValue)) {
            .showFutilityBoundsMissingArgumentError(arg, scaleLabel, scaleValue)
        }
    }
}

.checkForUnnecessaryFutilityBoundsScaleArgs <- function(argName, argValue, sourceScale, targetScale, allowedScales) {
    if (!(sourceScale %in% allowedScales || targetScale %in% allowedScales) &&
            !.isFutilityBoundsArgumentMissing(argName, argValue)) {
        .showFutilityBoundsUnnecessaryArgumentWarning(argName, argValue, sourceScale, targetScale)
    }
}

#' Assert Valid Futility Bounds Scale Arguments
#'
#' @description
#' Verifies that futility bounds scale arguments are specified correctly. It checks for
#' unnecessary or missing arguments based on the provided source and target scales.
#'
#' @param design Object. The trial design.
#' @param sourceScale Character. The scale on which the futility bounds are currently specified.
#' @param targetScale Character. The scale to which the futility bounds are to be converted.
#' @param theta Numeric. The theta value for conditional power calculations.
#' @param information1 Numeric. The first information rate.
#' @param information2 Numeric. The second information rate.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns \code{NULL} if the futility bounds scale arguments are valid.
#'
#' @examples
#' \dontrun{
#' .assertAreValidFutilityBoundsScaleArguments(design,
#'     sourceScale = "conditionalPower",
#'     targetScale = "effectEstimate",
#'     theta = 0.5,
#'     information1 = 0.2,
#'     information2 = 0.8
#' )
#' }

#'
#' @noRd
#'
.assertAreValidFutilityBoundsScaleArguments <- function(
        ...,
        design,
        sourceScale,
        targetScale,
        theta,
        information) {
    baseScales <- c(
        "conditionalPower",
        "condPowerAtObserved",
        "predictivePower"
    )
    scalesWithReverse <- c(baseScales, "reverseCondPower")
    scalesWithEffect <- c(baseScales, "effectEstimate")
    scalesWithEffectSecondStageOnly <- scalesWithEffect[scalesWithEffect != "conditionalPower"]

    infos <- .getFutilityBoundInformations(
        information = information,
        sourceScale = sourceScale,
        targetScale = targetScale,
        design = design,
        showWarnings = FALSE,
        ...
    )

    information1 <- infos$information1
    information2 <- infos$information2
    information <- infos$information

    .checkForUnnecessaryFutilityBoundsScaleArgs(
        "design",
        design, sourceScale, targetScale, scalesWithReverse
    )

    if (infos$vectorInput) {
        .checkForUnnecessaryFutilityBoundsScaleArgs(
            "information",
            information, sourceScale, targetScale, scalesWithEffect
        )
    } else {
        .checkForUnnecessaryFutilityBoundsScaleArgs(
            infos$paramNames[1], information1, sourceScale,
            targetScale, scalesWithEffectSecondStageOnly
        )
        .checkForUnnecessaryFutilityBoundsScaleArgs(
            infos$paramNames[2], information2, sourceScale,
            targetScale, baseScales
        )
    }
    .checkForUnnecessaryFutilityBoundsScaleArgs(
        "theta", theta, sourceScale, targetScale, "conditionalPower"
    )

    args <- list(
        design       = design,
        information  = c(information1, information2),
        information1 = information1,
        information2 = information2,
        theta        = theta
    )

    .checkFutilityBoundsScaleArgs(sourceScale, "sourceScale", args,
        informationVectorInput = infos$vectorInput
    )
    .checkFutilityBoundsScaleArgs(targetScale, "targetScale", args,
        informationVectorInput = infos$vectorInput
    )
}

.showWarningIfCalculatedFutiltyBoundsOutsideAcceptableRange <- function(
        futilityBounds,
        ...,
        lowerBound = 1e-12,
        upperBound = 1 - 1e-12) {
    if (all(is.na(futilityBounds))) {
        return(invisible())
    }

    if (!is.null(lowerBound) && length(lowerBound) > 0 && !anyNA(lowerBound) &&
            any(futilityBounds < lowerBound, na.rm = TRUE)) {
        warning(
            "At least one calculated futility bound outside acceptable range",
            call. = FALSE
        )
    }

    if (!is.null(upperBound) && length(upperBound) > 0 && !anyNA(upperBound) &&
            any(futilityBounds > upperBound, na.rm = TRUE)) {
        warning(
            "At least one calculated futility bound outside acceptable range",
            call. = FALSE
        )
    }
}
