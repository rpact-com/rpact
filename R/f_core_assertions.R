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
## |  File version: $Revision: 8579 $
## |  Last changed: $Date: 2025-03-04 16:57:07 +0100 (Di, 04 Mrz 2025) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_core_utilities.R
NULL

.stopWithWrongDesignMessage <- function(design, ..., inclusiveConditionalDunnett = TRUE) {
    stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'design' must be an instance of ", .arrayToString(
        .getTrialDesignClassNames(inclusiveConditionalDunnett = inclusiveConditionalDunnett),
        vectorLookAndFeelEnabled = FALSE
    ), " (is '", .getClassName(design), "')", call. = FALSE)
}

.stopWithWrongDesignMessageEnrichment <- function(design, ..., inclusiveConditionalDunnett = TRUE) {
    trialDesignClassNames <- c(C_CLASS_NAME_TRIAL_DESIGN_INVERSE_NORMAL, C_CLASS_NAME_TRIAL_DESIGN_FISHER)
    stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'design' must be an instance of ", .arrayToString(
        trialDesignClassNames,
        vectorLookAndFeelEnabled = FALSE
    ), " (is '", .getClassName(design), "')", call. = FALSE)
}

.isParameterSet <- function(x) {
    return(.isResultObjectBaseClass(x) && inherits(x, "ParameterSet"))
}

.assertIsParameterSetClass <- function(x, objectName = "x") {
    if (!.isParameterSet(x)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'", objectName, "' (", .getClassName(x), ") must be a S4 class ",
            "which inherits from class 'ParameterSet' ", call. = FALSE)
    }
}

.assertIsTrialDesignSet <- function(x, objectName = "x") {
    if (!.isTrialDesignSet(x)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'designSet' must be an instance of 'TrialDesignSet' ",
            "(is '", .getClassName(x), "')", call. = FALSE)
    }
}

.isTrialDesignSet <- function(x) {
    return(.getClassName(x) == "TrialDesignSet")
}

.isTrialDesignGroupSequential <- function(design) {
    return(.getClassName(design) == C_CLASS_NAME_TRIAL_DESIGN_GROUP_SEQUENTIAL)
}

.isTrialDesignInverseNormal <- function(design) {
    return(.getClassName(design) == C_CLASS_NAME_TRIAL_DESIGN_INVERSE_NORMAL)
}

.isTrialDesignFisher <- function(design) {
    return(.getClassName(design) == C_CLASS_NAME_TRIAL_DESIGN_FISHER)
}

.isTrialDesignConditionalDunnett <- function(design) {
    return(.getClassName(design) == C_CLASS_NAME_TRIAL_DESIGN_CONDITIONAL_DUNNETT)
}

.isTrialDesignInverseNormalOrGroupSequential <- function(design) {
    return(.isTrialDesignInverseNormal(design) || .isTrialDesignGroupSequential(design))
}

.isTrialDesignInverseNormalOrFisher <- function(design) {
    return(.isTrialDesignInverseNormal(design) || .isTrialDesignFisher(design))
}

.isTrialDesign <- function(design) {
    return(.isTrialDesignInverseNormal(design) || .isTrialDesignGroupSequential(design) ||
        .isTrialDesignFisher(design) || .isTrialDesignConditionalDunnett(design))
}

.isTrialDesignPlanMeans <- function(designPlan) {
    return(.getClassName(designPlan) == "TrialDesignPlanMeans")
}

.isTrialDesignPlanRates <- function(designPlan) {
    return(.getClassName(designPlan) == "TrialDesignPlanRates")
}

.isTrialDesignPlanSurvival <- function(designPlan) {
    return(.getClassName(designPlan) == "TrialDesignPlanSurvival")
}

.isTrialDesignPlanCountData <- function(designPlan) {
    return(.getClassName(designPlan) == "TrialDesignPlanCountData")
}

.isTrialDesignPlan <- function(designPlan) {
    return(.isTrialDesignPlanMeans(designPlan) ||
        .isTrialDesignPlanRates(designPlan) ||
        .isTrialDesignPlanSurvival(designPlan) ||
        .isTrialDesignPlanCountData(designPlan))
}

.assertIsTrialDesignPlan <- function(designPlan) {
    if (!.isTrialDesignPlan(designPlan)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'designPlan' must be an instance of 'TrialDesignPlan' (is '", .getClassName(designPlan), "')",
            call. = FALSE
        )
    }
}

.assertIsTrialDesign <- function(design) {
    if (!.isTrialDesign(design)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'design' must be an instance of ", .arrayToString(
            .getTrialDesignClassNames(),
            vectorLookAndFeelEnabled = FALSE
        ), " (is '", .getClassName(design), "')", call. = FALSE)
    }
}

.assertIsTrialDesignInverseNormal <- function(design) {
    if (!.isTrialDesignInverseNormal(design)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'design' must be an instance of class ",
            "'TrialDesignInverseNormal' (is '", .getClassName(design), "')",
            call. = FALSE
        )
    }
}

.assertIsTrialDesignFisher <- function(design) {
    if (!.isTrialDesignFisher(design)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'design' must be an instance of class ",
            "'TrialDesignFisher' (is '", .getClassName(design), "')",
            call. = FALSE
        )
    }
}

.assertIsTrialDesignGroupSequential <- function(design) {
    if (!.isTrialDesignGroupSequential(design)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'design' must be an instance of class ",
            "'TrialDesignGroupSequential' (is '", .getClassName(design), "')",
            call. = FALSE
        )
    }
}

.assertIsTrialDesignConditionalDunnett <- function(design) {
    if (!.isTrialDesignConditionalDunnett(design)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'design' must be an instance of class ",
            "'TrialDesignConditionalDunnett' (is '", .getClassName(design), "')",
            call. = FALSE
        )
    }
}

.assertIsTrialDesignInverseNormalOrGroupSequential <- function(design) {
    if (!.isTrialDesignInverseNormalOrGroupSequential(design)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'design' must be an instance of class ",
            "'TrialDesignInverseNormal' or 'TrialDesignGroupSequential' (is '",
            .getClassName(design), "')",
            call. = FALSE
        )
    }
}

.assertIsTrialDesignInverseNormalOrGroupSequentialOrFisher <- function(design) {
    if (!.isTrialDesignInverseNormalOrGroupSequential(design) && !.isTrialDesignFisher(design)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'design' must be an instance of class 'TrialDesignInverseNormal', ",
            "'TrialDesignGroupSequential', or 'TrialDesignFisher' (is '",
            .getClassName(design), "')",
            call. = FALSE
        )
    }
}

.assertIsTrialDesignInverseNormalOrFisher <- function(design) {
    if (!.isTrialDesignInverseNormalOrFisher(design)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'design' must be an instance of class ",
            "'TrialDesignInverseNormal' or 'TrialDesignFisher' (is '",
            .getClassName(design), "')",
            call. = FALSE
        )
    }
}

.assertIsTrialDesignInverseNormalOrFisherOrConditionalDunnett <- function(design) {
    if (!.isTrialDesignInverseNormalOrFisher(design) && !.isTrialDesignConditionalDunnett(design)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'design' must be an instance of class 'TrialDesignInverseNormal', ",
            "'TrialDesignFisher', or 'TrialDesignConditionalDunnett' (is '",
            .getClassName(design), "')", 
            call. = FALSE
        )
    }
}

.isSimulationResults <- function(simulationResults) {
    return(inherits(simulationResults, "SimulationResults"))
}

.assertIsSimulationResults <- function(simulationResults) {
    if (!.isSimulationResults(simulationResults)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'simulationResults' must be an instance of SimulationResults ",
            "(is '", .getClassName(simulationResults), "')", 
            call. = FALSE
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
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'stageResults' ",
            "must be a 'StageResults' object",
            " (is '", .getClassName(stageResults), "')", 
            call. = FALSE
        )
    }
}

.assertIsInClosedInterval <- function(x, xName, ..., lower, upper, naAllowed = FALSE, call. = FALSE) {
    .warnInCaseOfUnknownArguments(functionName = ".assertIsInClosedInterval", ...)
    if (naAllowed && all(is.na(x))) {
        return(invisible())
    }

    if (!naAllowed && length(x) > 1 && any(is.na(x))) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'", xName, "' (", .arrayToString(x), ") must be a valid numeric vector or a single NA",
            call. = call.
        )
    }

    if (is.null(upper) || is.na(upper)) {
        if (any(x < lower, na.rm = TRUE)) {
            prefix <- ifelse(length(x) > 1, "each value of ", "")
            stop(
                C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS, prefix,
                "'", xName, "' (", .arrayToString(x), ") must be >= ", lower,
                call. = call.
            )
        }
    } else if (any(x < lower, na.rm = TRUE) || any(x > upper, na.rm = TRUE)) {
        stop(
            C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS,
            "'", xName, "' (", .arrayToString(x), ") is out of bounds [", lower, "; ", upper, "]",
            call. = call.
        )
    }
}

.assertIsInOpenInterval <- function(x, xName, lower, upper, naAllowed = FALSE, call. = FALSE) {
    if (naAllowed && all(is.na(x))) {
        return(invisible())
    }

    if (!naAllowed && length(x) > 1 && any(is.na(x))) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'", xName, "' (", .arrayToString(x), ") ",
            "must be a valid numeric vector or a single NA", 
            call. = call.
        )
    }

    if (is.null(upper) || is.na(upper)) {
        if (any(x <= lower, na.rm = TRUE)) {
            prefix <- ifelse(length(x) > 1, "each value of ", "")
            stop(
                C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS, prefix,
                "'", xName, "' (", .arrayToString(x), ") must be > ", lower,
                call. = call.
            )
        }
    } else if (any(x <= lower, na.rm = TRUE) || any(x >= upper, na.rm = TRUE)) {
        stop(
            C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS,
            "'", xName, "' (", .arrayToString(x), ") is out of bounds (", lower, "; ", upper, ")",
            call. = call.
        )
    }
}

.assertIsValidDataInput <- function(dataInput, design = NULL, stage = NULL) {
    .assertIsDataset(dataInput)
    if (!is.null(design)) {
        .assertIsTrialDesign(design)
    }

    if (dataInput$.enrichmentEnabled && dataInput$getNumberOfGroups() != 2) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT,
            "only population enrichment data with 2 groups can be analyzed but ",
            dataInput$getNumberOfGroups(), " group",
            ifelse(dataInput$getNumberOfGroups() == 1, " is", "s are"), " defined", 
            call. = FALSE
        )
    }

    stages <- dataInput$stages
    l1 <- length(stages)
    for (fieldName in dataInput$.getVisibleFieldNames()) {
        l2 <- length(dataInput[[fieldName]])
        if (fieldName != "stages" && l1 != l2) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT,
                "all parameters must have the same length ('stages' has length ", l1,
                ", '", fieldName, "' has length ", l2, ")", 
                call. = FALSE
            )
        }
    }

    if (!is.null(stage)) {
        if (dataInput$getNumberOfGroups() == 1) {
            if (.isDatasetMeans(dataInput)) {
                if (any(na.omit(dataInput$getStDevsUpTo(stage)) <= 0)) {
                    stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, 
                        "all standard deviations must be > 0", 
                        call. = FALSE)
                }
                if (any(na.omit(dataInput$getSampleSizesUpTo(stage)) <= 0)) {
                    stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, 
                        "all sample sizes must be > 0", 
                        call. = FALSE)
                }
            } else if (.isDatasetRates(dataInput)) {
                if (any(na.omit(dataInput$getEventsUpTo(stage)) < 0)) {
                    stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, 
                        "all events must be >= 0", 
                        call. = FALSE)
                }
                if (any(na.omit(dataInput$getSampleSizesUpTo(stage)) <= 0)) {
                    stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, 
                        "all sample sizes must be > 0", 
                        call. = FALSE)
                }
                if (any(na.omit(dataInput$getEventsUpTo(stage)) > 
                        na.omit(dataInput$getSampleSizesUpTo(stage)))) {
                    stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, 
                        "all events must be <= corresponding sample size", 
                        call. = FALSE)
                }
            }
        } else if (dataInput$getNumberOfGroups() == 2) {
            if (.isDatasetMeans(dataInput)) {
                if (any(na.omit(dataInput$getStDevsUpTo(stage, 1)) <= 0) ||
                        any(na.omit(dataInput$getStDevsUpTo(stage, 2)) <= 0)) {
                    stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, 
                        "all standard deviations must be > 0", 
                        call. = FALSE)
                }
                if (any(na.omit(dataInput$getSampleSizesUpTo(stage, 1)) <= 0) ||
                        any(na.omit(dataInput$getSampleSizesUpTo(stage, 2)) <= 0)) {
                    stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, 
                        "all sample sizes must be > 0", 
                        call. = FALSE)
                }
            } else if (.isDatasetRates(dataInput)) {
                if (any(na.omit(dataInput$getEventsUpTo(stage, 1)) < 0) ||
                        any(na.omit(dataInput$getEventsUpTo(stage, 2)) < 0)) {
                    stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, 
                        "all events must be >= 0", 
                        call. = FALSE)
                }
                if (any(na.omit(dataInput$getSampleSizesUpTo(stage, 1)) <= 0) ||
                        any(na.omit(dataInput$getSampleSizesUpTo(stage, 2)) <= 0)) {
                    stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, 
                        "all sample sizes must be > 0", 
                        call. = FALSE)
                }
                if (any(na.omit(dataInput$getEventsUpTo(stage, 1)) > na.omit(dataInput$getSampleSizesUpTo(stage, 1))) ||
                        any(na.omit(dataInput$getEventsUpTo(stage, 2)) > na.omit(dataInput$getSampleSizesUpTo(stage, 2)))) {
                    stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, 
                        "all events must be <= corresponding sample size", 
                        call. = FALSE)
                }
            }
        }

        if (.isDatasetSurvival(dataInput)) {
            if (any(na.omit(dataInput$getOverallEventsUpTo(stage)) < 0)) {
                stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, 
                    "all cumulative events must be >= 0", 
                    call. = FALSE)
            }

            if (any(na.omit(dataInput$getOverallAllocationRatiosUpTo(stage)) <= 0)) {
                stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, 
                    "all cumulative allocation ratios must be > 0", 
                    call. = FALSE)
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
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
            "'dataInput' must be an instance of class ",
            "'DatasetMeans', 'DatasetRates' or 'DatasetSurvival' ",
            "(is '", .getClassName(dataInput), "')", 
            call. = FALSE
        )
    }
}

.assertIsDatasetMeans <- function(dataInput) {
    if (!.isDatasetMeans(dataInput = dataInput)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
            "'dataInput' must be an instance of class ",
            "'DatasetMeans' (is '", .getClassName(dataInput), "')", 
            call. = FALSE
        )
    }
}

.assertIsDatasetRates <- function(dataInput) {
    if (!.isDatasetRates(dataInput = dataInput)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'dataInput' must be an instance of class ",
            "'DatasetRates' (is '", .getClassName(dataInput), "')", 
            call. = FALSE
        )
    }
}

.assertIsDatasetSurvival <- function(dataInput) {
    if (!.isDatasetSurvival(dataInput = dataInput)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
            "'dataInput' must be an instance of class ",
            "'DatasetSurvival' or 'DatasetEnrichmentSurvival' ",
            "(is '", .getClassName(dataInput), "')", 
            call. = FALSE
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
    return(inherits(dataInput, "DatasetSurvival") || inherits(dataInput, "DatasetEnrichmentSurvival"))
}

.assertIsNumericVector <- function(
        x, 
        argumentName, 
        ..., 
        naAllowed = FALSE, 
        noDefaultAvailable = FALSE,
        len = NA_integer_,
        call. = FALSE) {
        
    if (missing(x) || is.null(x) || length(x) == 0) {
        .assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = FALSE)
        stop(
            C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'", argumentName,
            "' must be a valid numeric value or vector",
            call. = call.
        )
    }

    .assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = TRUE)

    if ((!naAllowed && any(is.na(x))) || !is.numeric(x)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' (",
            .arrayToString(x), ") must be a valid numeric value or vector",
            call. = call.
        )
    }
    
    if (!any(is.na(len)) && !length(x) %in% len) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' (",
            .arrayToString(x), ") must have length ", .arrayToString(len, mode = "or"),
            call. = call.
        )
    }
}

.assertIsIntegerVector <- function(x, argumentName, ..., naAllowed = FALSE,
        validateType = TRUE, noDefaultAvailable = FALSE, call. = FALSE) {
    if (missing(x) || is.null(x) || length(x) == 0) {
        .assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = FALSE)
        stop(
            C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'", argumentName,
            "' must be a valid integer value or vector",
            call. = call.
        )
    }

    .assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = TRUE)

    if (naAllowed && all(is.na(x))) {
        return(invisible())
    }

    if (!is.numeric(x) || (!naAllowed && any(is.na(x))) || (validateType && !is.integer(x)) ||
            (!validateType && any(as.integer(na.omit(x)) != na.omit(x)))) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' (",
            .arrayToString(x), ") must be a valid integer value or vector",
            call. = call.
        )
    }
}

.assertIsLogicalVector <- function(x, argumentName, ..., naAllowed = FALSE,
        noDefaultAvailable = FALSE, call. = FALSE) {
    if (missing(x) || is.null(x) || length(x) == 0) {
        .assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = FALSE)
        stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'", argumentName, "' ",
            "must be a valid logical value or vector",
            call. = call.
        )
    }

    .assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = TRUE)

    if ((!naAllowed && any(is.na(x))) || !is.logical(x)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' (", x, ") ",
            "must be a valid logical value or vector",
            call. = call.
        )
    }
}

.assertIsNoDefault <- function(x, argumentName, noDefaultAvailable, ..., checkNA = FALSE, call. = FALSE) {
    if (noDefaultAvailable && (!checkNA || all(is.na(x)))) {
        stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'", argumentName, "' ",
            "must be specified, there is no default value",
            call. = call.
        )
    }
}

.assertIsSingleLogical <- function(x, argumentName, ..., naAllowed = FALSE, noDefaultAvailable = FALSE, call. = FALSE) {
    if (missing(x) || is.null(x) || length(x) == 0) {
        .assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = FALSE)
        stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'", argumentName, "' must be a single logical value",
            call. = call.
        )
    }

    .assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = TRUE)

    if (length(x) > 1) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' ",
            .arrayToString(x, vectorLookAndFeelEnabled = TRUE), " must be a single logical value",
            call. = call.
        )
    }

    if ((!naAllowed && is.na(x)) || !is.logical(x)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' (",
            ifelse(.isResultObjectBaseClass(x), .getClassName(x), x), ") must be a single logical value",
            call. = call.
        )
    }
}

.assertIsSingleNumber <- function(x, argumentName, ..., naAllowed = FALSE, noDefaultAvailable = FALSE, call. = FALSE) {
    if (missing(x) || is.null(x) || length(x) == 0) {
        .assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = FALSE)
        stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'", argumentName, "' must be a valid numeric value",
            call. = call.
        )
    }

    .assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = TRUE)

    if (length(x) > 1) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' ",
            .arrayToString(x, vectorLookAndFeelEnabled = TRUE), " must be a single numeric value",
            call. = call.
        )
    }

    if ((!naAllowed && is.na(x)) || !is.numeric(x)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' (",
            ifelse(.isResultObjectBaseClass(x), .getClassName(x), x), ") must be a valid numeric value",
            call. = call.
        )
    }
}

.assertIsSingleInteger <- function(x, argumentName, ..., naAllowed = FALSE,
        validateType = TRUE, noDefaultAvailable = FALSE, call. = FALSE) {
    .assertIsSinglePositiveInteger(
        x = x,
        argumentName = argumentName,
        naAllowed = naAllowed,
        validateType = validateType,
        mustBePositive = FALSE,
        noDefaultAvailable = noDefaultAvailable,
        call. = call.
    )
}

.assertIsSinglePositiveInteger <- function(x,
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
        stop(
            C_EXCEPTION_TYPE_MISSING_ARGUMENT,
            "'", argumentName, "' must be a ", prefix, "integer value",
            call. = call.
        )
    }

    .assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = TRUE)

    if (length(x) > 1) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' ",
            .arrayToString(x, vectorLookAndFeelEnabled = TRUE),
            " must be a ", prefix, "integer value",
            call. = call.
        )
    }

    if (!is.numeric(x) || (!naAllowed && is.na(x)) || (validateType && !is.integer(x)) ||
            (!validateType && !is.na(x) && !is.infinite(x) && as.integer(x) != x)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'", argumentName, "' (", ifelse(.isResultObjectBaseClass(x), .getClassName(x), x), ") must be a ", prefix, "integer value",
            call. = call.
        )
    }

    if (mustBePositive && !is.na(x) && !is.infinite(x) && x <= 0) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'", argumentName, "' (", ifelse(.isResultObjectBaseClass(x), .getClassName(x), x), ") must be a ", prefix, "integer value",
            call. = call.
        )
    }
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
        stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'", argumentName, "' must be a valid character value",
            call. = call.
        )
    }

    .assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = TRUE)

    if (length(x) > 1) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' ",
            .arrayToString(x, vectorLookAndFeelEnabled = TRUE), " must be a single character value",
            call. = call.
        )
    }

    if (!is.character(x)) {
        stop(
            sprintf(paste0(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'%s' must be a valid character value (is an instance of class '%s')"
            ), argumentName, .getClassName(x)),
            call. = call.
        )
    }

    if (!naAllowed && is.na(x)) {
        stop(
            sprintf(paste0(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'%s' (NA) must be a valid character value"
            ), argumentName),
            call. = call.
        )
    }
}

.assertIsCharacter <- function(x, argumentName, ..., naAllowed = FALSE, call. = FALSE) {
    if (missing(x) || is.null(x) || length(x) == 0) {
        stop(
            C_EXCEPTION_TYPE_MISSING_ARGUMENT,
            "'", argumentName, "' must be a valid character value or vector",
            call. = call.
        )
    }

    if (!all(is.character(x))) {
        stop(
            sprintf(paste0(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'%s' must be a valid character value or vector ",
                "(is an instance of class '%s')"
            ), argumentName, .getClassName(x)),
            call. = call.
        )
    }

    if (!naAllowed && any(is.na(x))) {
        stop(
            sprintf(
                paste0(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'%s' (%s) must be a valid character value (NA is not allowed)"
                ),
                argumentName, .arrayToString(x)
            ),
            call. = call.
        )
    }
}

.assertDesignParameterExists <- function(design, parameterName, defaultValue) {
    if (missing(design)) {
        stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, 
            "'design' must be defined", 
            call. = FALSE)
    }

    if (missing(parameterName)) {
        stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, 
            "'parameterName' must be defined", 
            call. = FALSE)
    }

    if (missing(defaultValue)) {
        stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, 
            "'defaultValue' must be defined", 
            call. = FALSE)
    }

    value <- design[[parameterName]]
    if (is.null(value) || length(value) == 0 || all(is.na(value))) {
        stop(
            C_EXCEPTION_TYPE_MISSING_ARGUMENT, "parameter '", parameterName,
            "' must be specified in design", 
            call. = FALSE
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
        stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, 
            "'design' must be defined", 
            call. = FALSE)
    }

    if (missing(parameterName)) {
        stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, 
            "'parameterName' must be defined", 
            call. = FALSE)
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
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "optimization criterion must be one of the following: ", 
            .printOptimizationCriterion(), 
            call. = FALSE
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
        stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, 
            "'lowerBound' or 'upperBound' must be defined", 
            call. = FALSE)
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

        type <- getOption("rpact.out.of.validated.bounds.message.type", "warning")
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
    .assertIsNumericVector(lambda, argumentName, naAllowed = TRUE)
    if (all(is.na(lambda))) {
        return(invisible())
    }

    if (any(is.na(lambda))) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' (",
            .arrayToString(lambda), ") must be a valid numeric vector", 
            call. = FALSE
        )
    }

    .assertIsInClosedInterval(lambda, argumentName, lower = 0, upper = NULL)
    if (all(lambda == 0)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' (",
            .arrayToString(lambda), ") not allowed: ",
            "at least one lambda value must be > 0", 
            call. = FALSE
        )
    }
}

.assertIsValidFollowUpTime <- function(followUpTime) {
    if (is.null(followUpTime) || length(followUpTime) == 0 || is.na(followUpTime)) {
        return(invisible())
    }

    .assertIsSingleNumber(followUpTime, "followUpTime", naAllowed = TRUE)
    if (followUpTime < 0) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
            "'followUpTime' (", followUpTime, ") must be >= 0", 
            call. = FALSE)
    }
}

.assertIsValidAccrualTime <- function(accrualTime, ..., naAllowed = TRUE) {
    .assertIsNumericVector(accrualTime, "accrualTime", naAllowed = naAllowed)

    if (length(accrualTime) == 0 || all(is.na(accrualTime))) {
        return(invisible())
    }

    if (length(accrualTime) > 1 && accrualTime[1] != 0) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "the first value of 'accrualTime' ",
            "(", .arrayToString(accrualTime), ") must be 0", 
            call. = FALSE
        )
    }
    
    if (identical(accrualTime, 0) || identical(accrualTime, 0L)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "single 'accrualTime' is not allowed to be 0", 
            call. = FALSE
        )
    }
    
    .assertIsInClosedInterval(accrualTime, "accrualTime", lower = 0, upper = NULL, naAllowed = naAllowed)
    .assertValuesAreStrictlyIncreasing(accrualTime, "accrualTime")
}

.assertIsValidStandardDeviation <- function(stDev, groups = 1L, ..., name = "stDev", naAllowed = TRUE) {
    if (groups == 1L) {
        .assertIsSingleNumber(stDev, name, naAllowed = naAllowed)
    } else {
        .assertIsNumericVector(stDev, name, len = unique(c(1L, groups)), naAllowed = naAllowed) 
    }
    
    if (naAllowed && all(is.na(stDev))) {
        return(invisible())
    }

    if (any(stDev <= 0)) {
        stop(
            C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS,
            "standard deviation '", name, "' (", .arrayToString(stDev), ") must be > 0", 
            call. = FALSE
        )
    }
}

.assertIsValidAlpha <- function(alpha, ..., naAllowed = FALSE) {
    .assertIsSingleNumber(alpha, "alpha", naAllowed = naAllowed)
    .assertIsInOpenInterval(alpha, "alpha", lower = 0, upper = 1, naAllowed = naAllowed)
    .showParameterOutOfValidatedBoundsMessage(alpha, "alpha", 
        lowerBound = 1e-06, upperBound = 0.5, 
        closedUpperBound = FALSE, naAllowed = naAllowed)
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
        stop(
            C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS,
            "'stage' (", stage, ") is out of bounds [1; ", kMax, "]", 
            call. = FALSE
        )
    }
}

.assertIsValidIterationsAndSeed <- function(iterations, seed, ..., zeroIterationsAllowed = TRUE) {
    .assertIsSingleInteger(iterations, "iterations", validateType = FALSE)

    if (zeroIterationsAllowed) {
        if (iterations < 0) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'iterations' (", iterations, ") must be >= 0", 
                call. = FALSE
            )
        }
    } else {
        if (iterations < 1) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'iterations' (", iterations, ") must be > 0", 
                call. = FALSE
            )
        }
    }

    if (is.null(seed) || length(seed) == 0 || (!is.na(seed) && !is.numeric(seed))) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'seed' (", seed, ") must be a valid integer value", 
            call. = FALSE
        )
    }
}

.assertIsValidLegendPosition <- function(legendPosition) {
    if (is.null(legendPosition) || length(legendPosition) != 1) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'legendPosition' (", .arrayToString(legendPosition), ") ",
            "must be a single integer or character value", 
            call. = FALSE
        )
    }

    if (is.na(legendPosition)) {
        return(invisible())
    }
    
    if (grepl("^-?[0-9]+$", legendPosition)) {
        legendPosition <- as.integer(legendPosition)
    }

    if (!is.numeric(legendPosition) && !is.character(legendPosition)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'legendPosition' (", legendPosition, ") ",
            "must be a single integer or character value", 
            call. = FALSE
        )
    }

    if (is.numeric(legendPosition)) {
        .assertIsSingleInteger(legendPosition, "legendPosition", validateType = FALSE)
        .assertIsInClosedInterval(legendPosition, "legendPosition", lower = -1, upper = 6)
    } else {
        validLegendPositions <- c("none", "left", "right", "bottom", "top")
        if (!(legendPosition %in% validLegendPositions)) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'legendPosition' (", legendPosition, ") ",
                "must be one of the following values: ",
                .arrayToString(validLegendPositions), 
                call. = FALSE
            )
        }
    }
}

.assertIsValidKMax <- function(kMax, kMaxLowerBound = 1,
        kMaxUpperBound = C_KMAX_UPPER_BOUND, ..., showWarnings = FALSE) {
    .assertIsSingleInteger(kMax, "kMax", validateType = FALSE)
    .assertIsInClosedInterval(kMax, "kMax", lower = kMaxLowerBound, upper = kMaxUpperBound)
    if (showWarnings && kMax > 10) {
        warning("The usage of 'kMax' (", kMax, ") > 10 is not validated", call. = FALSE)
    }
}

.assertAreValidInformationRates <- function(informationRates, kMax = length(informationRates),
        kMaxLowerBound = 1L, kMaxUpperBound = C_KMAX_UPPER_BOUND) {
    if (length(informationRates) < kMaxLowerBound) {
        stop(sprintf(
            paste0(
                C_EXCEPTION_TYPE_ARGUMENT_LENGTH_OUT_OF_BOUNDS,
                "length of 'informationRates' (%s) is out of bounds [%s; %s]"
            ),
            length(informationRates), kMaxLowerBound,
            ifelse(kMax >= kMaxLowerBound && kMax < C_KMAX_UPPER_BOUND, kMax, C_KMAX_UPPER_BOUND)
        ), 
        call. = FALSE)
    }

    .assertIsValidKMax(kMax, kMaxLowerBound = kMaxLowerBound, kMaxUpperBound = kMaxUpperBound)

    if (length(informationRates) != kMax) {
        stop(sprintf(
            paste0(
                C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                "length of 'informationRates' (%s) must be equal to 'kMax' (%s)"
            ),
            length(informationRates), kMax
        ), 
        call. = FALSE)
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
        stop(sprintf(
            paste0(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'informationRates' (%s) ",
                "must be strictly increasing: 0 < x_1 < .. < x_%s <= 1"
            ),
            .arrayToString(informationRates, vectorLookAndFeelEnabled = FALSE), kMax
        ), 
        call. = FALSE)
    }
}

.assertValuesAreInsideBounds <- function(parameterName, values, lowerBound, upperBound, ...,
        lowerBoundInclusive = TRUE, upperBoundInclusive = TRUE) {
    lower <- min(values)
    upper <- max(values)
    lowerInvalid <- ifelse(lowerBoundInclusive, lower < lowerBound, lower <= lowerBound)
    upperInvalid <- ifelse(upperBoundInclusive, upper > upperBound, upper >= upperBound)
    if (!is.na(lowerInvalid)) {
        if (lowerInvalid || upperInvalid) {
            stop(sprintf(
                paste0(
                    C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS,
                    "'%s' (%s) is out of bounds %s%s; %s%s"
                ),
                parameterName, .arrayToString(values, vectorLookAndFeelEnabled = FALSE),
                ifelse(lowerBoundInclusive, "[", "("), lowerBound,
                upperBound, ifelse(upperBoundInclusive, "]", ")")
            ), 
            call. = FALSE)
        }
    }
}

.assertContainsNoNas <- function(values, parameterName) {
    if (any(is.na(values))) {
        stop(sprintf(
            paste0(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'%s' (%s) ",
                "must contain valid numeric values (NA is not allowed)"
            ),
            parameterName, .arrayToString(values, vectorLookAndFeelEnabled = FALSE)
        ), 
        call. = FALSE)
    }
}

.assertContainsOnlyNasAtTheEnd <- function(values, parameterName) {
    if (length(values) <= 1) {
        return(invisible())
    }

    for (i in length(values):2) {
        if (!is.na(values[i]) && is.na(values[i - 1])) {
            stop(sprintf(
                paste0(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'%s' (%s) ",
                    "must contain valid numeric values (NAs are only allowed at the end of the vector)"
                ),
                parameterName, .arrayToString(values, vectorLookAndFeelEnabled = FALSE)
            ), 
            call. = FALSE)
        }
    }
}

.assertValuesAreStrictlyIncreasing <- function(values,
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
        stop(sprintf(
            paste0(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'%s' (%s) ",
                "must be strictly increasing: x_1 < .. < x_%s"
            ),
            parameterName, .arrayToString(valuesTemp, vectorLookAndFeelEnabled = FALSE), len
        ), 
        call. = FALSE)
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
        stop(sprintf(
            paste0(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'%s' (%s) ",
                "must be increasing: x_1 <= .. <= x_%s"
            ),
            parameterName, .arrayToString(valuesTemp, vectorLookAndFeelEnabled = FALSE), len
        ), 
        call. = FALSE)
    }
}

.assertAreValidFutilityBounds <- function(futilityBounds, kMax = length(futilityBounds) + 1,
        kMaxLowerBound = 1, kMaxUpperBound = C_KMAX_UPPER_BOUND) {
    if (length(futilityBounds) < kMaxLowerBound - 1) {
        stop(sprintf(
            paste0(
                C_EXCEPTION_TYPE_ARGUMENT_LENGTH_OUT_OF_BOUNDS,
                "length of 'futilityBounds' (%s) is out of bounds [%s; %s]"
            ),
            length(futilityBounds), kMaxLowerBound - 1,
            ifelse(kMax >= kMaxLowerBound && kMax < C_KMAX_UPPER_BOUND, kMax - 1, C_KMAX_UPPER_BOUND - 1)
        ), 
        call. = FALSE)
    }

    .assertIsValidKMax(kMax, kMaxLowerBound = kMaxLowerBound, kMaxUpperBound = kMaxUpperBound)

    if (length(futilityBounds) != kMax - 1) {
        stop(
            C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
            "length of 'futilityBounds' (", length(futilityBounds),
            ") must be equal to 'kMax' (", kMax, ") - 1", 
            call. = FALSE
        )
    }

    .assertValuesAreInsideBounds("futilityBounds", futilityBounds, -Inf, 6)
}

.assertIsValidCipher <- function(key, value) {
    if (getCipheredValue(value) != C_CIPHERS[[key]]) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
            "'token' and/or 'secret' unkown", 
            call. = FALSE)
    }
}

.assertIsValidAlpha0Vec <- function(alpha0Vec, kMax = length(alpha0Vec) - 1,
        kMaxLowerBound = 1, kMaxUpperBound = C_KMAX_UPPER_BOUND) {
    if (length(alpha0Vec) < kMaxLowerBound - 1) {
        stop(sprintf(
            paste0(
                C_EXCEPTION_TYPE_ARGUMENT_LENGTH_OUT_OF_BOUNDS,
                "length of 'alpha0Vec' (%s) is out of bounds [%s; %s]"
            ),
            length(alpha0Vec), kMaxLowerBound - 1, kMax - 1
        ), 
        call. = FALSE)
    }

    .assertIsValidKMax(kMax, kMaxLowerBound = kMaxLowerBound, kMaxUpperBound = kMaxUpperBound)

    if (length(alpha0Vec) != kMax - 1) {
        stop(
            C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
            "length of 'alpha0Vec' (", length(alpha0Vec),
            ") must be equal to 'kMax' (", kMax, ") - 1", 
            call. = FALSE
        )
    }

    .assertValuesAreInsideBounds("alpha0Vec", alpha0Vec, 0, 1, lowerBoundInclusive = FALSE)
}

.assertIsValidSidedParameter <- function(sided) {
    if (is.null(match.call(expand.dots = FALSE)[["sided"]])) {
        stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, 
            "'sided' must be defined", 
            call. = FALSE)
    }
    if (sided != 1 && sided != 2) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
            "'sided' (", sided, ") must be 1 or 2", 
            call. = FALSE)
    }
}

.assertIsValidGroupsParameter <- function(groups) {
    if (is.null(match.call(expand.dots = FALSE)[["groups"]])) {
        stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, 
            "'groups' must be defined", 
            call. = FALSE)
    }
    if (groups != 1 && groups != 2) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
            "'groups' (", groups, ") must be 1 or 2", 
            call. = FALSE)
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
        stop(
            simpleError(paste0(C_EXCEPTION_TYPE_MISSING_ARGUMENT, e$message), call = e$call), 
            call. = FALSE
        )
    })

    if (.allArgumentsAreNotNull(...)) {
        return(invisible(TRUE))
    }

    args <- args[args != "warningOnlyEnabled" & !is.null(args)]
    argNames <- names(args)
    if (sum(argNames == "") > 0) {
        stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, 
            "each argument must have a name defined, e.g. a = a", 
            call. = FALSE)
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
            stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, message, call. = FALSE)
        }
    }

    return(invisible(length(definedArguments) == length(args)))
}

.assertIsValidNPlanned <- function(nPlanned, kMax, stage, ..., required = TRUE) {
    if (is.null(nPlanned) || (length(nPlanned) > 0 && all(is.na(nPlanned)))) {
        if (!required) {
            return(invisible())
        }
        stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, 
            "'nPlanned' must be specified", 
            call. = FALSE)
    }

    if (length(nPlanned) != kMax - stage) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            sprintf(
                paste0(
                    "'nPlanned' (%s) is invalid: ",
                    "length must be equal to %s (kMax - stage = %s - %s)"
                ),
                .arrayToString(nPlanned), kMax - stage, kMax, stage
            ), 
            call. = FALSE
        )
    }

    if (sum(is.na(nPlanned)) > 0 || sum(nPlanned <= 0) > 0) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            sprintf(
                paste0(
                    "'nPlanned' (%s) is invalid: ",
                    "all values must be > 0"
                ),
                .arrayToString(nPlanned)
            ), 
            call. = FALSE
        )
    }
}

.isValidNPlanned <- function(nPlanned, kMax, stage) {
    if (missing(nPlanned)) {
        warning("'nPlanned' is missing", call. = FALSE)
        return(FALSE)
    }
    if (!any(is.na(nPlanned))) {
        if ((length(nPlanned) != kMax - stage)) {
            warning(sprintf(
                paste0(
                    "'nPlanned' (%s) will be ignored: ",
                    "length must be equal to %s (kMax - stage = %s - %s)"
                ),
                .arrayToString(nPlanned), kMax - stage, kMax, stage
            ), call. = FALSE)
            return(FALSE)
        }

        if (sum(is.na(nPlanned)) > 0 || sum(nPlanned <= 0) > 0) {
            warning(sprintf(
                paste0(
                    "'nPlanned' (%s) will be ignored: ",
                    "all values must be > 0"
                ),
                .arrayToString(nPlanned)
            ), call. = FALSE)
            return(FALSE)
        }
    }
    return(TRUE)
}

.warnInCaseOfUnknownArguments <- function(..., functionName, ignore = character(),
        numberOfAllowedUnnamedParameters = 0, exceptionEnabled = FALSE) {
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
        if (!(argName %in% ignore) && !grepl("^\\.", argName)) {
            if (.isResultObjectBaseClass(arg) || is.environment(arg)) {
                arg <- .getClassName(arg)
            }
            if (is.function(arg)) {
                arg <- "function(...)"
            }
            argValue <- paste0(" (", .getClassName(arg), ")")
            tryCatch(expr = {
                argValue <- .arrayToString(arg, vectorLookAndFeelEnabled = length(arg) > 1, encapsulate = is.character(arg))
                argValue <- paste0(" = ", argValue)
            }, error = function(e) {})
            if (exceptionEnabled) {
                stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "argument unknown in ", functionName, "(...): '", argName, "'",
                    argValue, " is not allowed",
                    call. = FALSE
                )
            } else {
                warning("Argument unknown in ", functionName, "(...): '", argName, "'",
                    argValue, " will be ignored",
                    call. = FALSE
                )
            }
        }
    }
}

.warnInCaseOfUnusedArgument <- function(arg, argName, defaultValue, functionName) {
    if (!identical(arg, defaultValue)) {
        warning("Unused argument in ", functionName, "(...): '",
            argName, "' = ", .arrayToString(arg, vectorLookAndFeelEnabled = (length(arg) > 1), maxLength = 10),
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
            design$.getParameterType("twoSidedPower") == C_PARAM_USER_DEFINED) {
        warning("design$twoSidedPower = FALSE will be ignored because design$sided = 2", call. = FALSE)
    }
}

.isTrialDesignWithValidFutilityBounds <- function(design) {
    if (is.null(design) || !.isTrialDesignInverseNormalOrGroupSequential(design)) {
        return(FALSE)
    }

    futilityBounds <- design[["futilityBounds"]]
    if (is.null(futilityBounds)) {
        return(FALSE)
    }

    if (length(futilityBounds) == 0 || sum(is.na(futilityBounds)) == design$kMax) {
        return(FALSE)
    }

    return(any(na.omit(futilityBounds) > C_FUTILITY_BOUNDS_DEFAULT))
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
    if (!requireNamespace(packageName, quietly = TRUE)) {
        stop("Package \"", packageName, "\" is needed for this function to work. ",
            "Please install using, e.g., install.packages(\"", packageName, "\")",
            call. = FALSE
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

.assertIsValidThetaH0 <- function(thetaH0, ..., endpoint = c("means", "rates", "survival", "counts"),
        groups, ratioEnabled = FALSE) {
    .warnInCaseOfUnknownArguments(functionName = ".assertIsValidThetaH0", ...)

    if (is.na(thetaH0)) {
        return(invisible())
    }

    if (!is.numeric(thetaH0)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
            "'thetaH0' must be a valid numeric value", 
            call. = FALSE)
    }

    endpoint <- match.arg(endpoint)
    if (endpoint == "means" || endpoint == "rates") {
        if (groups == 2 && ratioEnabled) {
            if (thetaH0 <= 0) {
                stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
                    "'thetaH0' (", thetaH0, ") must be > 0", 
                    call. = FALSE)
            }
            return(invisible())
        }
    }

    if (endpoint == "rates") {
        if (groups == 1) {
            if (thetaH0 <= 0 || thetaH0 >= 1) {
                stop(
                    C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS,
                    "'thetaH0' (", thetaH0, ") is out of bounds (0; 1) or not specified", 
                    call. = FALSE
                )
            }
        } else {
            if (thetaH0 <= -1 || thetaH0 >= 1) {
                stop(
                    C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS,
                    "'thetaH0' (", thetaH0, ") is out of bounds (-1; 1)", 
                    call. = FALSE
                )
            }
        }
    } else if (endpoint %in% c("survival", "counts")) {
        if (thetaH0 <= 0) {
            stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
                "'thetaH0' (", thetaH0, ") must be > 0", 
                call. = FALSE)
        }
    }
}

.assertIsValidThetaH0DataInput <- function(thetaH0, dataInput) {
    if (.isDatasetRates(dataInput)) {
        endpoint <- "rates"
    } else if (.isDatasetSurvival(dataInput)) {
        endpoint <- "survival"
    } else {
        endpoint <- "means"
    }
    .assertIsValidThetaH0(thetaH0, endpoint = endpoint, groups = dataInput$getNumberOfGroups())
}

.assertIsValidThetaRange <- function(..., thetaRange, thetaAutoSeqEnabled = TRUE, survivalDataEnabled = FALSE) {
    if (is.null(thetaRange) || (thetaAutoSeqEnabled && length(thetaRange) <= 1) ||
            any(is.na(thetaRange))) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'thetaRange' (", .arrayToString(thetaRange), ") must be a vector ",
            "with two entries defining minimum and maximum ",
            "or a sequence of numeric values with length > 2", 
            call. = FALSE
        )
    } else if (length(thetaRange) == 2 && thetaAutoSeqEnabled) {
        minValue <- thetaRange[1]
        maxValue <- thetaRange[2]
        if (survivalDataEnabled) {
            .assertIsValidHazardRatio(minValue, "thetaRange[1]")
            .assertIsValidHazardRatio(maxValue, "thetaRange[2]")
        }
        if (minValue >= maxValue) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'thetaRange' with length 2 must contain minimum < maximum (",
                minValue, " >= ", maxValue, ")", 
                call. = FALSE
            )
        }
        by <- (maxValue - minValue) / C_THETA_RANGE_SEQUENCE_LENGTH_DEFAULT
        thetaRange <- seq(minValue, maxValue, by)
    }

    invisible(thetaRange)
}

.assertIsValidPiTreatmentRange <- function(..., piTreatmentRange, piAutoSeqEnabled = TRUE) {
    if (is.null(piTreatmentRange) || (piAutoSeqEnabled && length(piTreatmentRange) <= 1) ||
            any(is.na(piTreatmentRange))) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'piTreatmentRange' (", .arrayToString(piTreatmentRange), ") must be a vector ",
            "with two entries defining minimum and maximum ",
            "or a sequence of numeric values with length > 2", 
            call. = FALSE
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
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'piTreatmentRange' with length 2 must contain minimum < maximum (",
                    minValue, " >= ", maxValue, ")", 
                    call. = FALSE
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
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'", piName, "' must be a valid numeric value", 
            call. = FALSE
        )
    }

    if (all(is.na(piValue))) {
        return(invisible())
    }

    if (!is.numeric(piValue) || any(is.na(piValue))) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'", piName, "' (", .arrayToString(piValue), ") must be a valid numeric value", 
            call. = FALSE
        )
    }

    if (any(piValue <= -1e-16) || any(piValue >= 1 + 1e-16)) {
        stop(
            C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS,
            "'", piName, "' (", .arrayToString(piValue), ") ",
            "is out of bounds (0; 1) or event time too long", 
            call. = FALSE
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
        return(invisible())
    }

    .assertIsSingleNumber(allocationRatioPlanned, "allocationRatioPlanned")
    .assertIsInOpenInterval(
        allocationRatioPlanned,
        "allocationRatioPlanned", 0, C_ALLOCATION_RATIO_MAXIMUM
    )
    if (allocationRatioPlanned != C_ALLOCATION_RATIO_DEFAULT && numberOfGroups == 1) {
        warning("Planned allocation ratio ", allocationRatioPlanned, " will be ignored ",
            "because the specified data has only one group",
            call. = FALSE
        )
    }
}

.assertIsValidAllocationRatioPlannedSampleSize <- function(allocationRatioPlanned, maxNumberOfSubjects = NA_integer_) {
    .assertIsNumericVector(allocationRatioPlanned, "allocationRatioPlanned", naAllowed = TRUE)
    .assertIsInClosedInterval(allocationRatioPlanned, "allocationRatioPlanned",
        lower = 0,
        upper = C_ALLOCATION_RATIO_MAXIMUM, naAllowed = TRUE
    )
    .assertIsValidMaxNumberOfSubjects(maxNumberOfSubjects, naAllowed = TRUE)

    if (any(is.na(allocationRatioPlanned))) {
        allocationRatioPlanned <- C_ALLOCATION_RATIO_DEFAULT
    }

    if (length(maxNumberOfSubjects) == 1 && !is.na(maxNumberOfSubjects) &&
            maxNumberOfSubjects > 0 && all(allocationRatioPlanned == 0)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "determination of optimal allocation ratio not possible ",
            "if explicit or implicit 'maxNumberOfSubjects' (", maxNumberOfSubjects,
            ") > 0, i.e., follow-up time should be calculated ",
            "(please specify an 'allocationRatioPlanned' > 0)", 
            call. = FALSE
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

.assertIsValidAssumedStDev <- function(assumedStDev,
        stageResults = NULL, stage = NULL, ..., results = NULL) {
    if (is.na(assumedStDev) && !is.null(stageResults) && !is.null(stage)) {
        assumedStDev <- stageResults$overallStDevs[stage]
        if (!is.null(results)) {
            results$.setParameterType("assumedStDev", C_PARAM_GENERATED)
        }
    }
    .assertIsSingleNumber(assumedStDev, "assumedStDev")
    if (assumedStDev <= 0) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'assumedStDev' (", assumedStDev, ") must be > 0", 
            call. = FALSE
        )
    }
    invisible(assumedStDev)
}

.assertIsValidThetaH1ForMultiArm <- function(thetaH1,
        stageResults = NULL, stage = NULL, ..., results = NULL) {
    if (!is.null(stageResults) && all(is.na(thetaH1)) && !is.null(stage)) {
        thetaH1 <- stageResults$effectSizes[, stage]
        if (!is.null(results)) {
            results$.setParameterType("thetaH1", C_PARAM_GENERATED)
        }
    }

    .assertIsNumericVector(thetaH1, "thetaH1", naAllowed = TRUE)
    invisible(thetaH1)
}

.assertIsValidThetaH1ForEnrichment <- function(thetaH1,
        stageResults = NULL, stage = NULL, ..., results = NULL) {
    invisible(.assertIsValidThetaH1ForMultiArm(
        thetaH1 = thetaH1,
        stageResults = stageResults, stage = stage, results = results
    ))
}

.assertIsValidAssumedStDevForMultiHypotheses <- function(assumedStDev,
        stageResults = NULL, stage = NULL, ..., results = NULL) {
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
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'assumedStDev' (", .arrayToString(assumedStDev), ") must be > 0", 
            call. = FALSE
        )
    }

    invisible(assumedStDev)
}

.assertIsValidAssumedStDevs <- function(assumedStDevs, gMax) {
    if (length(assumedStDevs) != 1 && length(assumedStDevs) != gMax) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            sprintf(paste0(
                "length of 'assumedStDevs' (%s) ",
                "must be equal to 'gMax' (%s) or 1"
            ), .arrayToString(assumedStDevs), gMax), 
            call. = FALSE
        )
    }
}

.assertIsValidPiTreatmentsForMultiArm <- function(piTreatments,
        stageResults = NULL, stage = NULL, ..., results = NULL) {
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

.assertIsValidPiControlForMultiArm <- function(piControl,
        stageResults = NULL, stage = NULL, ..., results = NULL) {
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

.assertIsValidPiTreatmentsForEnrichment <- function(piTreatments,
        stageResults = NULL, stage = NULL, ..., results = NULL) {
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

.assertIsValidPiControlForEnrichment <- function(piControls,
        stageResults = NULL, stage = NULL, ..., results = NULL) {
    if (!is.null(stageResults) && all(is.na(piControls)) && !is.null(stage)) {
        piControls <- stageResults$overallPisControl[, stage]
        if (!is.null(results)) {
            results$.setParameterType("piControls", C_PARAM_GENERATED)
        }
    }
    .assertIsNumericVector(piControls, "piControls", naAllowed = TRUE)
    .assertIsInClosedInterval(piControls, "piControls", lower = 0, upper = 1, naAllowed = TRUE)
    invisible(piControls)
}

.assertIsValidHazardRatio <- function(hazardRatio, thetaH0) {
    .assertIsNumericVector(hazardRatio, "hazardRatio")
    if (any(hazardRatio == thetaH0)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "alternative not correctly specified: ",
            "each hazard ratio (",
            .arrayToString(hazardRatio[1:min(length(hazardRatio), 10)]),
            ") must be unequal to 'thetaH0' (", thetaH0, ")", 
            call. = FALSE
        )
    }
}

.assertIsValidHazardRatioVector <- function(hazardRatio) {
    .assertIsNumericVector(hazardRatio, "hazardRatio")
    if (any(hazardRatio <= 0)) {
        if (length(hazardRatio) == 1) {
            stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
                "'hazardRatio' (", hazardRatio, ") must be > 0", 
                call. = FALSE)
        } else {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "each 'hazardRatio' (",
                .arrayToString(hazardRatio[1:min(length(hazardRatio), 10)]),
                ") must be > 0", 
                call. = FALSE
            )
        }
    }
}

.assertIsValidDirectionUpper <- function(directionUpper,
        design,
        ...,
        objectType = c("sampleSize", "power", "analysis"),
        userFunctionCallEnabled = FALSE) {
    objectType <- match.arg(objectType)

    .assertIsSingleLogical(directionUpper, "directionUpper", naAllowed = TRUE)

    if (!is.na(directionUpper) && !is.na(design$directionUpper) &&
            !identical(directionUpper, design$directionUpper)) {
        stop(C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
            "in the design directionUpper = ", design$directionUpper, " is defined. ",
            "In the ", ifelse(objectType == "sampleSize", "sample size", objectType),
            " function the same direction must be specified, but it is ", directionUpper,
            call. = FALSE
        )
    }

    if (objectType %in% c("power", "analysis")) {
        sided <- design$sided
        if (sided == 1 && is.na(directionUpper)) {
            if (!is.na(design$directionUpper)) {
                directionUpper <- design$directionUpper
            } else {
                directionUpper <- C_DIRECTION_UPPER_DEFAULT
            }
        }
        if (userFunctionCallEnabled && sided == 2 && !is.na(directionUpper)) {
            warning("'directionUpper' will be ignored because it ",
                "is not applicable for 'sided' = 2",
                call. = FALSE
            )
        }
    } else if (is.na(directionUpper)) {
        if (!is.na(design$directionUpper)) {
            directionUpper <- design$directionUpper
        } else {
            directionUpper <- C_DIRECTION_UPPER_DEFAULT
        }
    }

    return(directionUpper)
}

.assertIsFunction <- function(fun) {
    if (is.null(fun)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
            "'fun' must be a valid function", 
            call. = FALSE)
    }
    if (!is.function(fun)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
            "'fun' must be a function (is ", .getClassName(fun), ")", 
            call. = FALSE)
    }
}

.assertIsValidFunction <- function(fun, ...,
        funArgName = "fun",
        expectedArguments = NULL,
        expectedFunction = NULL,
        identical = FALSE,
        validateThreeDots = TRUE,
        showUnusedArgumentsMessage = FALSE,
        namedArgumentsExpected = FALSE) {
    fCall <- match.call(expand.dots = FALSE)

    if (is.null(expectedArguments) && is.null(expectedFunction)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'expectedArguments' or 'expectedFunction' must be not NULL", 
            call. = FALSE
        )
    }

    if (!is.function(fun)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'", funArgName, "' must be a function", 
            call. = FALSE
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
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'expectedFunction' must be a function", 
                call. = FALSE
            )
        }
        argNamesExpected <- methods::formalArgs(expectedFunction)
    }

    if (validateThreeDots) {
        if (!("..." %in% argNames)) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'", funArgName, "' must contain the three-dots argument '...', e.g., ",
                funArgName, " = ", functionName, "(", .arrayToString(argNames), ", ...)", 
                call. = FALSE
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
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "the argument '", argName, "' in '", funArgName,
                "' (", functionName, ") is not allowed."
            )
            if (length(argNamesExpected) == 1) {
                stop(msg, " Expected: '", argNamesExpected, "'", call. = FALSE)
            }
            stop(
                msg, "\n\n", "Use one or more of the following arguments:\n ",
                .arrayToString(argNamesExpected, encapsulate = TRUE), 
                call. = FALSE
            )
        }
    }

    if (identical) {
        for (argNameExpected in argNamesExpected) {
            if (argNameExpected != "..." && !(argNameExpected %in% argNames)) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'", funArgName, "' (", functionName, ") must contain ",
                    "an argument with name '", argNameExpected, "'", 
                    call. = FALSE
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
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'", funArgName, "' (", functionName, ") must contain at ",
            "least one of the following arguments: ",
            .arrayToString(argNamesExpected), 
            call. = FALSE
        )
    }

    if (showUnusedArgumentsMessage && length(unusedArgs) > 0) {
        message("Note that the following arguments can optionally be used in '",
            funArgName, "' (", functionName, "): \n",
            .arrayToString(unusedArgs),
            call. = FALSE
        )
    }
}

.assertIsValidThreshold <- function(threshold, activeArms) {
    .assertIsNumericVector(threshold, "threshold", naAllowed = TRUE)
    if ((length(threshold) != 1) && (length(threshold) != activeArms)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'threshold' (", .arrayToString(threshold),
            ") must be a single value or a vector of length ", activeArms, 
            call. = FALSE
        )
    }
}

.assertIsValidPlannedSubjectsOrEvents <- function(design,
        plannedValues,
        parameterName = c("plannedSubjects", "plannedEvents")) {
    parameterName <- match.arg(parameterName)
    .assertIsIntegerVector(plannedValues, parameterName, validateType = FALSE)
    if (length(plannedValues) != design$kMax) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'", parameterName, "' (", .arrayToString(plannedValues), ") ",
            "must have length ", design$kMax, 
            call. = FALSE
        )
    }
    .assertIsInClosedInterval(plannedValues, parameterName, lower = 1, upper = NULL)
    .assertValuesAreStrictlyIncreasing(plannedValues, parameterName)
}

.assertIsValidNumberOfSubjectsPerStage <- function(parameterValues, parameterName, plannedSubjects,
        conditionalPower, calcSubjectsFunction, kMax,
        endpoint = c("means", "rates", "survival"), calcSubjectsFunctionEnabled = TRUE) {
    endpoint <- match.arg(endpoint)

    if (kMax == 1) {
        .ignoreParameterIfNotUsed(
            "conditionalPower",
            conditionalPower, kMax > 1, "design is fixed ('kMax' = 1)"
        )
        return(invisible(NA_real_))
    }

    .assertIsNumericVector(parameterValues, parameterName, naAllowed = TRUE)

    calcSubjectsFunctionName <- ifelse(endpoint == "survival", "calcEventsFunction", "calcSubjectsFunction")

    if (is.na(conditionalPower) && is.null(calcSubjectsFunction)) {
        if (length(parameterValues) != 1 || !is.na(parameterValues)) {
            if (calcSubjectsFunctionEnabled) {
                warning("'", parameterName, "' (", .arrayToString(parameterValues), ") ",
                    "will be ignored because neither 'conditionalPower' nor '",
                    calcSubjectsFunctionName, "' is defined",
                    call. = FALSE
                )
            } else {
                warning("'", parameterName, "' (", .arrayToString(parameterValues), ") ",
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
            stop(
                C_EXCEPTION_TYPE_MISSING_ARGUMENT,
                "'", parameterName, "' must be defined ",
                "because 'conditionalPower' or '", calcSubjectsFunctionName, "' is defined", 
                call. = FALSE
            )
        } else {
            stop(
                C_EXCEPTION_TYPE_MISSING_ARGUMENT,
                "'", parameterName, "' must be defined ",
                "because 'conditionalPower' is defined", 
                call. = FALSE
            )
        }
    }

    if (length(parameterValues) != kMax) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", parameterName, "' (",
            .arrayToString(parameterValues), ") must have length ", kMax, 
            call. = FALSE
        )
    }

    if (any(is.na(parameterValues[2:length(parameterValues)]))) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", parameterName, "' (",
            .arrayToString(parameterValues), ") must contain valid numeric values", 
            call. = FALSE
        )
    }

    if (!is.na(parameterValues[1]) && parameterValues[1] != plannedSubjects[1]) {
        warning("First value of '", parameterName, "' ",
            "(", parameterValues[1], ") will be ignored", call. = FALSE)
    }

    parameterValues[1] <- plannedSubjects[1]

    .assertIsInClosedInterval(parameterValues, parameterName, lower = 1, upper = NULL)

    return(invisible(parameterValues))
}

.assertIsValidMaxNumberOfSubjects <- function(maxNumberOfSubjects, naAllowed = FALSE) {
    .assertIsSingleNumber(maxNumberOfSubjects, "maxNumberOfSubjects", naAllowed = naAllowed)
    .assertIsInClosedInterval(maxNumberOfSubjects, "maxNumberOfSubjects", lower = 1, upper = NULL, naAllowed = naAllowed)
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

.assertIsOneSidedDesign <- function(design,
        designType = c("multi-arm", "enrichment"),
        engineType = c("simulation", "analysis")) {
    if (design$sided == 2) {
        designType <- match.arg(designType)
        engineType <- match.arg(engineType)
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            designType, " ", engineType, " is only applicable for one-sided testing", 
            call. = FALSE
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
    return(inherits(simulationResults, "SimulationResults") && grepl("MultiArm", .getClassName(simulationResults)))
}

.isEnrichmentSimulationResults <- function(simulationResults) {
    return(inherits(simulationResults, "SimulationResults") && grepl("Enrichment", .getClassName(simulationResults)))
}

.assertIsStageResultsMultiArm <- function(stageResults) {
    if (!inherits(stageResults, "StageResults")) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'stageResults' must be a multi-arm stage results object ",
            "(is ", .getClassName(stageResults), ")", 
            call. = FALSE
        )
    }

    if (!.isMultiArmStageResults(stageResults)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'stageResults' must be a multi-arm object ",
            "(is ", .getClassName(stageResults), ")", 
            call. = FALSE
        )
    }
}

.assertIsStageResultsNonMultiHypotheses <- function(stageResults) {
    if (inherits(stageResults, "StageResults") && .isMultiArmStageResults(stageResults)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'stageResults' must be a non-multi-arm object ",
            "(is ", .getClassName(stageResults), ")", 
            call. = FALSE
        )
    }

    if (inherits(stageResults, "StageResults") && .isEnrichmentStageResults(stageResults)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'stageResults' must be a non-enrichment object ",
            "(is ", .getClassName(stageResults), ")", 
            call. = FALSE
        )
    }

    allowedClasses <- c(
        "StageResultsMeans",
        "StageResultsRates",
        "StageResultsSurvival"
    )
    if (!(.getClassName(stageResults) %in% allowedClasses)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'stageResults' must be an instance of ",
            .arrayToString(allowedClasses, vectorLookAndFeelEnabled = FALSE),
            " (is '", .getClassName(stageResults), "')", 
            call. = FALSE
        )
    }
}

.assertIsDatasetNonMultiHypotheses <- function(dataInput) {
    if (.isMultiArmDataset(dataInput)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'dataInput' must be a non-multi-arm dataset ",
            "(has ", dataInput$getNumberOfGroups(), " treatment arms)", 
            call. = FALSE
        )
    }
    if (.isEnrichmentDataset(dataInput)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'dataInput' must be a non-enrichment dataset ",
            "(has ", dataInput$getNumberOfSubsets(), " subsets)", 
            call. = FALSE
        )
    }
}

.assertIsAnalysisResults <- function(analysisResults) {
    if (!inherits(analysisResults, "AnalysisResults")) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'analysisResults' ",
            "must be a valid 'AnalysisResults' object ",
            " (is '", .getClassName(analysisResults), "')", 
            call. = FALSE
        )
    }
}

.isValidIntersectionTestMultiArm <- function(intersectionTest) {
    return(!is.null(intersectionTest) && length(intersectionTest) == 1 && !is.na(intersectionTest) &&
        is.character(intersectionTest) && intersectionTest %in% C_INTERSECTION_TESTS_MULTIARMED)
}

.getCorrectedIntersectionTestMultiArmIfNecessary <- function(design, intersectionTest, userFunctionCallEnabled = TRUE) {
    .assertIsCharacter(intersectionTest, "intersectionTest")
    intersectionTest <- intersectionTest[1]
    if (.isTrialDesignConditionalDunnett(design) && intersectionTest != "Dunnett") {
        if (userFunctionCallEnabled) {
            message <- paste0("Intersection test '", intersectionTest, "' ")
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
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'intersectionTest' ",
            "(", intersectionTest, ") must be one of ",
            .arrayToString(C_INTERSECTION_TESTS_MULTIARMED, encapsulate = TRUE), 
            call. = FALSE
        )
    }
    if (.isTrialDesignConditionalDunnett(design) && intersectionTest != "Dunnett") {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "intersection test ",
            "('", intersectionTest, "') must be 'Dunnett' ",
            "because conditional Dunnett test was specified as design", 
            call. = FALSE
        )
    }
}

.isValidIntersectionTestEnrichment <- function(intersectionTest) {
    return(!is.null(intersectionTest) && length(intersectionTest) == 1 && !is.na(intersectionTest) &&
        is.character(intersectionTest) && intersectionTest %in% C_INTERSECTION_TESTS_ENRICHMENT)
}

.assertIsValidIntersectionTestEnrichment <- function(design, intersectionTest) {
    .assertIsCharacter(intersectionTest, "intersectionTest")
    intersectionTest <- intersectionTest[1]
    if (!.isValidIntersectionTestEnrichment(intersectionTest)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'intersectionTest' ",
            "(", intersectionTest, ") must be one of ",
            .arrayToString(C_INTERSECTION_TESTS_ENRICHMENT, encapsulate = TRUE), 
            call. = FALSE
        )
    }
    return(intersectionTest)
}

.ignoreParameterIfNotUsed <- function(paramName, paramValue, requirementLogical, requirementFailedReason,
        prefix = NA_character_) {
    if (all(is.na(paramValue)) || requirementLogical) {
        return(paramValue)
    }

    if (is.na(prefix) || trimws(prefix) == "") {
        prefix <- ""
    } else {
        prefix <- paste0(trimws(prefix), " ")
    }

    warning(prefix, "'", paramName, "' (", .arrayToString(paramValue), ") will be ignored because ",
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
    if (is.null(stage) && is.numeric(stageResults) && stageResults %in% 1L:C_KMAX_UPPER_BOUND) {
        stage <- stageResults
    }
    if (!is.null(stage)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'stage' (", stage, ") can only be defined in ",
            "getStageResults() or getAnalysisResults()", 
            call. = FALSE
        )
    }
}

.stopInCaseOfIllegalStageDefinition2 <- function(...) {
    forbiddenStage <- .getOptionalArgument("stage", ...)
    if (!is.null(forbiddenStage)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'stage' (", forbiddenStage, ") can only be defined in ",
            "getStageResults() or getAnalysisResults()", 
            call. = FALSE
        )
    }
}

.assertIsValidTolerance <- function(tolerance) {
    .assertIsSingleNumber(tolerance, "tolerance")
    .assertIsInOpenInterval(tolerance, "tolerance", lower = 0, upper = 0.1)
}

.isValidVarianceOptionMultiArmed <- function(varianceOption) {
    return(!is.null(varianceOption) && length(varianceOption) == 1 && !is.na(varianceOption) &&
        is.character(varianceOption) && varianceOption %in% C_VARIANCE_OPTIONS_MULTIARMED)
}

.assertIsValidVarianceOptionMultiArmed <- function(design, varianceOption) {
    if (!.isValidVarianceOptionMultiArmed(varianceOption)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'varianceOption' should be one of ",
            .arrayToString(C_VARIANCE_OPTIONS_MULTIARMED, encapsulate = TRUE), 
            call. = FALSE
        )
    }
    if (.isTrialDesignConditionalDunnett(design) && varianceOption != C_VARIANCE_OPTION_DUNNETT) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "variance option ('", varianceOption, "') must be '", C_VARIANCE_OPTION_DUNNETT, "' ",
            "because conditional Dunnett test was specified as design", 
            call. = FALSE
        )
    }
}

.isValidVarianceOptionEnrichment <- function(varianceOption) {
    return(!is.null(varianceOption) && length(varianceOption) == 1 && !is.na(varianceOption) &&
        is.character(varianceOption) && varianceOption %in% C_VARIANCE_OPTIONS_ENRICHMENT)
}

.assertIsValidVarianceOptionEnrichment <- function(design, varianceOption) {
    if (!.isValidVarianceOptionEnrichment(varianceOption)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'varianceOption' should be one of ",
            .arrayToString(C_VARIANCE_OPTIONS_ENRICHMENT, encapsulate = TRUE), 
            call. = FALSE
        )
    }
}


.assertIsValidSummaryIntervalFormat <- function(intervalFormat) {
    .assertIsSingleCharacter(intervalFormat, "intervalFormat") # "[%s; %s]"
    if (!grepl("^[^%]*%s[^%]*%s[^%]*$", intervalFormat)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'intervalFormat' (", intervalFormat, ") has an invalid format; ",
            "the control character %s must appear exactly twice; ",
            "to change it use 'options(\"rpact.summary.intervalFormat\" = \"[%s; %s]\")'", 
            call. = FALSE
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
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'typeOfSelection' ",
            "(", typeOfSelection, ") must be one of ",
            .arrayToString(C_TYPES_OF_SELECTION, encapsulate = TRUE), 
            call. = FALSE
        )
    }

    if (typeOfSelection == "rBest") {
        .assertIsSingleNumber(rValue, "rValue", naAllowed = FALSE, noDefaultAvailable = TRUE)
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
        .assertIsSingleNumber(epsilonValue, "epsilonValue", naAllowed = FALSE, noDefaultAvailable = TRUE)
        .assertIsInClosedInterval(epsilonValue, "epsilonValue", lower = 0, upper = NULL, naAllowed = TRUE)
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
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'successCriterion' ",
            "(", successCriterion, ") must be one of ",
            .arrayToString(C_SUCCESS_CRITERIONS, encapsulate = TRUE), 
            call. = FALSE
        )
    }
    return(successCriterion)
}

.assertIsValidEffectMeasure <- function(effectMeasure) {
    .assertIsCharacter(effectMeasure, "effectMeasure")
    effectMeasure <- effectMeasure[1]
    if (!(effectMeasure %in% C_EFFECT_MEASURES)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'effectMeasure' ",
            "(", effectMeasure, ") must be one of ",
            .arrayToString(C_EFFECT_MEASURES, encapsulate = TRUE), 
            call. = FALSE
        )
    }
    return(effectMeasure)
}

.assertIsValidMatrix <- function(x, argumentName, ...,
        expectedNumberOfColumns = NA_integer_, naAllowed = FALSE, returnSingleValueAsMatrix = FALSE) {
    if (missing(x) || is.null(x) || length(x) == 0) {
        stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, 
            "'", argumentName, "' must be a valid matrix", 
            call. = FALSE)
    }

    if (returnSingleValueAsMatrix && !is.matrix(x) && (is.numeric(x) || is.character(x) || is.logical(x))) {
        if (length(x) == 1) {
            x <- matrix(x)
        } else if (length(x) > 1 && !is.na(expectedNumberOfColumns)) {
            if (length(x) %% expectedNumberOfColumns != 0) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "the length of ",
                    "'", argumentName, "' (", .arrayToString(x),
                    ") must be a divisor or a multiple ", expectedNumberOfColumns, 
                    call. = FALSE
                )
            }

            x <- matrix(x, ncol = expectedNumberOfColumns)
        }
    }

    if (!is.matrix(x)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
            "'", argumentName, "' (", .getClassName(x), ") must be a valid matrix", 
            call. = FALSE)
    }

    if (!naAllowed && any(is.na(x))) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
            "'", argumentName, "' (", .arrayToString(x), ") must not contain NA's", 
            call. = FALSE)
    }

    if (!is.numeric(x)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' (",
            .arrayToString(x), ") must be a valid numeric matrix", 
            call. = FALSE
        )
    }

    if (!is.na(expectedNumberOfColumns) && ncol(x) != expectedNumberOfColumns) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' (",
            .arrayToString(x), ") must be a numeric matrix with ", expectedNumberOfColumns, " columns", 
            call. = FALSE
        )
    }

    return(invisible(x))
}

.assertIsValidDecisionMatrix <- function(decisionMatrix, kMax) {
    .assertIsValidMatrix(decisionMatrix, "decisionMatrix", naAllowed = FALSE)
    if (!(nrow(decisionMatrix) %in% c(2, 4))) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
            "'decisionMatrix' must have two or four rows", 
            call. = FALSE)
    }
    if (ncol(decisionMatrix) != kMax) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
            "'decisionMatrix' must have 'kMax' ",
            "(= length(informationRates) = ", kMax, ") columns", 
            call. = FALSE
        )
    }
    if (any(decisionMatrix[2:nrow(decisionMatrix), ] < decisionMatrix[1:(nrow(decisionMatrix) - 1), ])) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
            "'decisionMatrix' needs to be increasing in each column", 
            call. = FALSE)
    }
}

.assertIsValidTypeOfShape <- function(typeOfShape) {
    .assertIsCharacter(typeOfShape, "typeOfShape")
    typeOfShape <- typeOfShape[1]
    if (!(typeOfShape %in% C_TYPES_OF_SHAPE)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'typeOfShape' ",
            "(", typeOfShape, ") must be one of ",
            .arrayToString(C_TYPES_OF_SHAPE, encapsulate = TRUE), 
            call. = FALSE
        )
    }
    return(typeOfShape)
}

.assertHasLength <- function(x, argumentName, len, lenArgName, ..., naAllowed = FALSE) {
    if (isTRUE(naAllowed) && all(is.na(x))) {
        return(invisible())
    }

    if (length(x) != len) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            sQuote(argumentName), " (", .arrayToString(x), ") ",
            "must have length ", sQuote(lenArgName), " (", len, ")", 
            call. = FALSE
        )
    }
}

.assertIsValidEffectMatrix <- function(
        ...,
        simulationResults,
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
            expectedNumberOfColumns = gMax, naAllowed = FALSE, returnSingleValueAsMatrix = TRUE
        )
        .assertIsNumericVector(valueMaxVector, valueMaxVectorName, naAllowed = TRUE)
        valueMaxVectorDefault <- C_ALTERNATIVE_POWER_SIMULATION_DEFAULT
        if (valueMaxVectorName == "piMaxVector") {
            .assertIsInOpenInterval(effectMatrix, "effectMatrix", 0, 1, naAllowed = FALSE)
            valueMaxVectorDefault <- C_PI_1_DEFAULT
        } else if (valueMaxVectorName == "omegaMaxVector") {
            .assertIsInOpenInterval(effectMatrix, "effectMatrix", 0, NULL, naAllowed = FALSE)
            valueMaxVectorDefault <- C_RANGE_OF_HAZARD_RATIOS_DEFAULT
        }
        if (!all(is.na(valueMaxVector)) && !identical(valueMaxVector, valueMaxVectorDefault)) {
            warning(sQuote(valueMaxVectorName), " (", .arrayToString(valueMaxVector),
                ") will be ignored because it will be set to first column of 'effectMatrix'",
                call. = FALSE
            )
        }
        if (!is.null(doseLevels) && !any(is.na(doseLevels))) {
            warning("'doseLevels' (", .arrayToString(doseLevels), ") ",
                "will be ignored because 'typeOfShape' ",
                "is defined as '", typeOfShape, "'",
                call. = FALSE
            )
        }
    } else if (!is.null(effectMatrix)) {
        warning("'effectMatrix' will be ignored because 'typeOfShape' ",
            "is defined as '", typeOfShape, "'",
            call. = FALSE
        )
    }

    if (any(is.na(doseLevels))) {
        doseLevels <- 1:gMax
    }

    if (typeOfShape %in% c("sigmoidEmax", "linear")) {
        .assertIsNumericVector(valueMaxVector, valueMaxVectorName, 
            naAllowed = FALSE, noDefaultAvailable = TRUE)
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
        doseLevels = doseLevels)
    simulationResults$effectMatrix <- t(effectMatrix)
    return(effectMatrix)
}

.assertIsValidEffectMatrixMeans <- function(
        ...,
        simulationResults,
        typeOfShape,
        effectMatrix,
        muMaxVector,
        gED50,
        gMax,
        slope,
        doseLevels) {
    return(.assertIsValidEffectMatrix(
        simulationResults = simulationResults,
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
        typeOfShape,
        effectMatrix,
        omegaMaxVector,
        gED50,
        gMax,
        slope,
        doseLevels) {
    return(.assertIsValidEffectMatrix(
        simulationResults = simulationResults,
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
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'plannedSubjects' (", .arrayToString(plannedSubjects),
            ") must have length 'kMax' (", kMax, ")", 
            call. = FALSE
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
        stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, 
            "either 'design' or 'delayedInformation' must be specified", 
            call. = FALSE)
    }

    if (!is.null(design)) {
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

.assertIsValidEffectCountData <- function(sampleSizeEnabled,
        sided,
        lambda1,
        lambda2,
        lambda,
        theta,
        thetaH0,
        overdispersion) {
    .assertIsSingleInteger(sided, "sided", validateType = FALSE)
    if (sided != 1 && sided != 2) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
            "'sided' (", sided, ") must be defined as 1 or 2", 
            call. = FALSE)
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
    if (!is.na(lambda) && all(!is.na(theta))) {
        if (all(!is.na(lambda1)) || !is.na(lambda2)) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'lambda1' and/or 'lambda2' need not to be specified if 'lambda' and 'theta' are specified", 
                call. = FALSE
            )
        }
        if (length(theta) > 1) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "theta cannot be specified as vector if lambda is specified", 
                call. = FALSE
            )
        }
    } else if (!is.na(lambda2) && all(!is.na(theta))) {
        if (all(!is.na(lambda1)) || !is.na(lambda)) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'lambda1' and/or 'lambda' need not to be specified if 'lambda2' and 'theta' are specified", 
                call. = FALSE
            )
        }
    } else if (all(!is.na(lambda1)) && all(!is.na(theta))) {
        if (!is.na(lambda2) || !is.na(lambda)) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'lambda2' and/or 'lambda' need not to be specified if 'lambda1' and 'theta' are specified", 
                call. = FALSE
            )
        }
        if (length(theta) > 1) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "theta cannot be specified as vector if lambda1 is specified", 
                call. = FALSE
            )
        }
    } else if (all(!is.na(lambda1)) && !is.na(lambda2)) {
        if (!is.na(lambda) || all(!is.na(theta))) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'lambda' and/or 'theta' need not to be specified if 'lambda1' and 'lambda2' are specified", 
                call. = FALSE
            )
        }
    } else if (!is.na(lambda) && all(!is.na(lambda1))) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'lambda2' and/or 'theta' need not to be specified if 'lambda' and 'lambda1' are specified", 
            call. = FALSE
        )
    } else if (!is.na(lambda) && !is.na(lambda2)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'lambda1' and/or 'theta' need not to be specified if 'lambda' and 'lambda2' are specified", 
            call. = FALSE
        )
    } else if (sum(is.na(lambda2), any(is.na(lambda1)), is.na(lambda), any(is.na(theta))) != 2) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "this parameter configuration is not possible: exactly two of the ",
            "parameters 'lambda', 'lambda1', 'lambda2', 'theta' must be specified", 
            call. = FALSE
        )
    }
    if (!is.na(lambda2) && !any(is.na(theta))) {
        lambda1 <- lambda2 * theta
    } else if (!any(is.na(lambda1)) && !any(is.na(theta))) {
        lambda2 <- lambda1 / theta
    }
    if (!any(is.na(c(lambda1, lambda2))) && any(abs(lambda1 / lambda2 - thetaH0) < 1e-12) &&
            sampleSizeEnabled) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "any 'lambda1 / lambda2' (", .arrayToString(lambda1 / lambda2), ") must be != 'thetaH0' (", thetaH0, ")", 
            call. = FALSE
        )
    }
}

.assertParametersAreSpecifiedCorrectlyTogether <- function(...,
        case = c("notTogether", "eitherOr"),
        .paramNames = NULL) {
    params <- list(...)
    if (length(params) != 2) {
        if (is.null(names(params)) ||
                length(params[!(names(params) %in% c("case", ".paramNames"))]) != 2) {
            stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "exactly two parameters must be specified")
        }
    }
    case <- match.arg(case)
    if (!is.null(.paramNames)) {
        paramNames <- .paramNames
    } else {
        paramNames <- names(params)
    }
    if (is.null(paramNames) || any(nchar(paramNames) == 0)) {
        stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "all arguments must be named")
    }
    if (case == "notTogether" && !all(is.na(params[[1]])) && !all(is.na(params[[2]]))) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            sQuote(paramNames[1]), " (", .arrayToString(params[[1]]), ") ",
            "and ", sQuote(paramNames[2]), " (", .arrayToString(params[[2]]), ") ",
            "cannot be specified together", 
            call. = FALSE
        )
    } else if (case == "eitherOr" && all(is.na(params[[1]])) && all(is.na(params[[2]]))) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "either ", sQuote(paramNames[1]), " ",
            "or ", sQuote(paramNames[2]), " ",
            "needs to be specified", 
            call. = FALSE
        )
    }
}

.assertIsValidParametersCountData <- function(...,
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

    if (sampleSizeEnabled) {
        if (is.na(maxNumberOfSubjects) && any(is.na(accrualIntensity))) {
            .assertParametersAreSpecifiedCorrectlyTogether(
                "fixedExposureTime" = fixedExposureTime,
                "followUpTime" = followUpTime
            )
            .assertParametersAreSpecifiedCorrectlyTogether(
                "fixedExposureTime" = fixedExposureTime,
                "followUpTime" = followUpTime,
                case = "eitherOr"
            )
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
    if (any(is.na(accrualTime)) && !is.na(followUpTime)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'accrualTime' needs to be specified if 'followUpTime' (", followUpTime, ") is specified", 
            call. = FALSE
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
        if (!is.na(maxNumberOfSubjects) && any(is.na(accrualTime))) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'accrualTime' needs to be specified if 'maxNumberOfSubjects' ",
                "(", maxNumberOfSubjects, ") is specified", 
                call. = FALSE
            )
        }
    } else if (!simulationEnabled) {
        if (is.na(maxNumberOfSubjects) && (any(is.na(accrualIntensity)) || any(is.na(accrualTime)))) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'accrualTime' and 'accrualIntensity' need to be specified if 'maxNumberOfSubjects' is not specified", 
                call. = FALSE
            )
        }
        if (any(is.na(accrualIntensity)) && !any(is.na(accrualTime)) && !is.na(fixedExposureTime)) {
            warning(
                "Specification of 'accrualTime' has no influence of calculation and will be ignored",
                call. = FALSE
            )
        }
    } else {
        if (is.na(maxNumberOfSubjects) && (any(is.na(accrualIntensity)) || any(is.na(accrualTime)))) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'accrualTime' and 'accrualIntensity' need to be specified if 'maxNumberOfSubjects' is not specified", 
                call. = FALSE
            )
        }
    }

    .assertParametersAreSpecifiedCorrectlyTogether(
        "maxNumberOfSubjects" = maxNumberOfSubjects,
        "accrualIntensity" = accrualIntensity,
        .paramNames = if (simulationEnabled) c("maxNumberOfSubjects", "accrualIntensity") else NULL
    )

    if (!any(is.na(accrualIntensity)) && (accrualTime[1] != 0) &&
            length(accrualIntensity) == 1 &&
            length(accrualTime) != length(accrualIntensity)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'accrualTime' (", .arrayToString(accrualTime), ") and ",
            "'accrualIntensity' (", .arrayToString(accrualIntensity), ") does not match", 
            call. = FALSE
        )
    }
    if (!any(is.na(accrualIntensity)) && (length(accrualIntensity) > 1) &&
            length(accrualTime) != length(accrualIntensity) + 1) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'accrualTime' (", .arrayToString(accrualTime), ") and ",
            "'accrualIntensity' (", .arrayToString(accrualIntensity), ") does not match", 
            call. = FALSE
        )
    }
    if (accrualIntensityValidationEnabled && any(is.na(accrualIntensity)) &&
            length(accrualTime) > 1) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'accrualIntensity' (", .arrayToString(accrualIntensity), ") is not correctly specified", 
            call. = FALSE
        )
    }
}

.assertAreValidCalendarTimes <- function(plannedCalendarTime, kMax) {
    .assertIsNumericVector(plannedCalendarTime, "plannedCalendarTime", naAllowed = FALSE)
    .assertValuesAreStrictlyIncreasing(plannedCalendarTime, "plannedCalendarTime")
    .assertIsInOpenInterval(plannedCalendarTime, "plannedCalendarTime", lower = 0, upper = NULL, naAllowed = FALSE)
    if (length(plannedCalendarTime) != kMax) {
        stop(sprintf(
            paste0(
                C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                "length of 'plannedCalendarTime' (%s) must be equal to 'kMax' (%s)"
            ),
            length(plannedCalendarTime), kMax
        ), 
        call. = FALSE)
    }
}

.assertIsValidPlotType <- function(type, naAllowed = FALSE) {
    if (is.null(type) || length(type) == 0 || (!naAllowed && all(is.na(type)))) {
        stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'type' must be defined", call. = FALSE)
    }

    if (!is.numeric(type) && !is.character(type)) {
        stop(
            C_EXCEPTION_TYPE_MISSING_ARGUMENT,
            "'type' must be an integer or character value or vector (is ", .getClassName(type), ")", 
            call. = FALSE
        )
    }

    if (is.numeric(type)) {
        .assertIsIntegerVector(type, "type", naAllowed = naAllowed, validateType = FALSE)
    }
}

.hasApplicableFutilityBounds <- function(design) {
    return(
        !all(is.na(design$futilityBounds)) &&
            any(design$futilityBounds > C_FUTILITY_BOUNDS_DEFAULT, na.rm = TRUE)
    )
}
