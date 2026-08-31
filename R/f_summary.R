## |
## |  *Summary classes and functions*
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
#' @include f_core_assertions.R
NULL

.createSummaryHypothesisText <- function(object, summaryFactory) {
    if (!inherits(object, "AnalysisResults") && !inherits(object, "TrialDesignPlan") &&
            !inherits(object, "SimulationResults")) {
        stopIllegalArgument("'object' must be an instance of class ",
            "'AnalysisResults', 'TrialDesignPlan' ",
            "or 'SimulationResults' (is ", .getClassName(object, quote = TRUE), ")",
            functionName = ".createSummaryHypothesisText",
            parameter = "object",
            value = .getClassName(object)
        )
    }

    design <- object[[".design"]]
    if (is.null(design)) {
        stopRuntimeIssue("'.design' must be defined in specified ", .getClassName(object),
            functionName = ".createSummaryHypothesisText",
            parameter = ".design"
        )
    }

    settings <- .getSummaryObjectSettings(object)
    sided <- ifelse(settings$multiArmEnabled || settings$enrichmentEnabled, 1, design$sided)
    directionUpper <- object[["directionUpper"]]
    if (is.null(directionUpper) || length(directionUpper) != 1 || is.na(directionUpper)) {
        directionUpper <- TRUE
    }

    comparisonH0 <- " = "
    comparisonH1 <- NA_character_
    if (inherits(object, "AnalysisResults") && !is.null(directionUpper)) {
        comparisonH1 <- ifelse(sided == 2, " != ", ifelse(!isFALSE(directionUpper), " > ", " < "))
    }

    if (!is.null(object[["thetaH0"]])) {
        thetaH0 <- object$thetaH0
    } else {
        thetaH0 <- ifelse(settings$survivalEnabled, 1, 0)
    }

    treatmentArmIndex <- ifelse(settings$groups > 1, "(i)", "(treatment)")
    controlArmIndex <- ifelse(settings$groups > 1, "(i)", "(control)")

    if (settings$multiArmEnabled || settings$enrichmentEnabled) {
        if ((settings$survivalEnabled) && (settings$multiArmEnabled)) {
            treatmentArmIndex <- "(i)"
            controlArmIndex <- ""
        } else if ((settings$survivalEnabled) && (settings$enrichmentEnabled)) {
            treatmentArmIndex <- ""
            controlArmIndex <- ""
        } else if (settings$groups == 1) {
            treatmentArmIndex <- "(treatment)"
            controlArmIndex <- "(control)"
        } else {
            if (settings$enrichmentEnabled) {
                treatmentArmIndex <- "(treatment)"
            } else {
                treatmentArmIndex <- "(i)"
            }
            controlArmIndex <- "(control)"
        }
    } else {
        if (settings$groups == 1 || settings$survivalEnabled) {
            treatmentArmIndex <- ""
            controlArmIndex <- ""
        } else {
            treatmentArmIndex <- "(1)"
            controlArmIndex <- "(2)"
        }
    }

    value <- "?"
    if (settings$meansEnabled) {
        value <- "mu"
    } else if (settings$ratesEnabled) {
        value <- "pi"
    } else if (settings$survivalEnabled) {
        value <- "hazard ratio"
    } else if (settings$countDataEnabled) {
        value <- "lambda"
    }

    calcSep <- ifelse(settings$ratioEnabled || settings$countDataEnabled, " / ", " - ")
    hypothesis <- ""
    if (!settings$survivalEnabled && (settings$multiArmEnabled ||
            settings$enrichmentEnabled || settings$groups == 2)) {
        hypothesis <- paste0(
            hypothesis, "H0: ", value, treatmentArmIndex,
            calcSep, value, controlArmIndex, comparisonH0, thetaH0
        )
        if (!is.na(comparisonH1)) {
            hypothesis <- paste0(hypothesis, " against ")
            hypothesis <- paste0(
                hypothesis, "H1: ", value, treatmentArmIndex,
                calcSep, value, controlArmIndex, comparisonH1, thetaH0
            )
        }
    } else {
        hypothesis <- paste0(hypothesis, "H0: ", value, treatmentArmIndex, comparisonH0, thetaH0)
        if (!is.na(comparisonH1)) {
            hypothesis <- paste0(hypothesis, " against ")
            hypothesis <- paste0(hypothesis, "H1: ", value, treatmentArmIndex, comparisonH1, thetaH0)
        }
    }
    hypothesis <- .concatenateSummaryText(
        hypothesis,
        .createSummaryHypothesisPowerDirectionText(object, sided, directionUpper)
    )
    return(hypothesis)
}

.createSummaryHypothesisPowerDirectionText <- function(object, sided, directionUpper) {
    if (sided == 2 || is.null(directionUpper) || all(is.na(directionUpper))) {
        return("")
    }

    directionUpper <- unique(directionUpper)
    if (length(directionUpper) != 1) {
        return("")
    }

    if (inherits(object, "AnalysisResults")) {
        return("")
    }

    if (.isTrialDesignPlan(object) && object$.objectType != "power") {
        return("")
    }

    if (directionUpper) {
        return("power directed towards larger values")
    } else {
        return("power directed towards smaller values")
    }
}



.createSummary <- function(
        object,
        digits = NA_integer_,
        output = c("all", "title", "overview", "body")) {
    output <- match.arg(output)

    markdown <- attr(object, "markdown")
    if (is.null(markdown) || length(markdown) == 0 || !is.logical(markdown)) {
        markdown <- FALSE
    }

    if (inherits(object, "TrialDesignCharacteristics")) {
        return(.createSummaryDesignPlan(object,
            digits = digits,
            output = output,
            markdown = markdown
        ))
    }

    if (.isTrialDesign(object) || .isTrialDesignPlan(object) || inherits(object, "SimulationResults")) {
        return(.createSummaryDesignPlan(object,
            digits = digits,
            output = output,
            markdown = markdown
        ))
    }

    if (inherits(object, "AnalysisResults")) {
        return(.createSummaryAnalysisResults(object,
            digits = digits, output = output, markdown = markdown
        ))
    }

    if (inherits(object, "PerformanceScore")) {
        return(.createSummaryPerformanceScore(object,
            digits = digits, output = output, markdown = markdown
        ))
    }

    stopRuntimeIssue("function 'summary' not ", "implemented yet for class ", .getClassName(object),
        functionName = ".createSummary",
        parameter = "summary"
    )
}

.createSummaryPerformanceScore <- function(
        object,
        ...,
        digits = NA_integer_,
        output = c("all", "title", "overview", "body"),
        markdown = FALSE) {
    .createSummaryDesignPlan(object$.simulationResults,
        digits = digits,
        output = output,
        performanceScore = object,
        markdown = markdown
    )
}

#'
#' Main function for creating a summary of an analysis result
#'
#' @noRd
#'
.createSummaryAnalysisResults <- function(
        object,
        ...,
        digits = NA_integer_,
        output = c("all", "title", "overview", "body"),
        markdown = FALSE) {
    output <- match.arg(output)
    if (!inherits(object, "AnalysisResults")) {
        stopIllegalArgument("'object' must be a valid analysis result object (is class ", .getClassName(object),
            ")",
            functionName = ".createSummaryAnalysisResults",
            parameter = "object", value = object
        )
    }

    digitSettings <- .getSummaryDigits(digits)

    outputSize <- .getEnvironmentVariable(
        "RPACT_SUMMARY_OUTPUT_SIZE",
        "rpact.summary.output.size",
        default = C_SUMMARY_OUTPUT_SIZE_DEFAULT,
        type = "character"
    )

    intervalFormat <- .getEnvironmentVariable(
        "RPACT_SUMMARY_INTERVALFORMAT",
        "rpact.summary.intervalFormat",
        default = "[%s; %s]",
        type = "character"
    )
    .assertIsValidSummaryIntervalFormat(intervalFormat)

    multiArmEnabled <- .isMultiArmAnalysisResults(object)
    enrichmentEnabled <- .isEnrichmentAnalysisResults(object)
    multiHypothesesEnabled <- .isMultiHypothesesAnalysisResults(object)

    analysisResults <- object
    design <- analysisResults$.design
    stageResults <- analysisResults$.stageResults
    dataInput <- analysisResults$.dataInput
    closedTestResults <- NULL
    conditionalPowerResults <- NULL
    if (multiHypothesesEnabled) {
        closedTestResults <- analysisResults$.closedTestResults
        if (length(analysisResults$nPlanned) > 0 && !all(is.na(analysisResults$nPlanned))) {
            conditionalPowerResults <- analysisResults$.conditionalPowerResults
        }
    }

    summaryFactory <- SummaryFactory$new(
        object = object,
        intervalFormat = intervalFormat,
        output = output,
        markdown = markdown
    )

    .addDesignInformationToSummary(design, object, summaryFactory,
        output = output, digits = digits
    )

    summaryFactory$addParameter(stageResults,
        parameterName = "effectSizes",
        parameterCaption = ifelse(stageResults$isDatasetRates() &&
            dataInput$getNumberOfGroups() == 1,
        "Cumulative treatment rate", "Cumulative effect size"
        ),
        roundDigits = digitSettings$digitsGeneral
    )

    if (stageResults$isDatasetMeans()) {
        parameterCaption <- ifelse(stageResults$isOneSampleDataset(),
            "Cumulative standard deviation", "Cumulative (pooled) standard deviation"
        )
        parameterName <- ifelse(inherits(stageResults, "StageResultsMultiArmMeans") &&
            !inherits(stageResults, "StageResultsEnrichmentMeans"),
        "overallPooledStDevs", "overallStDevs"
        )
        summaryFactory$addParameter(stageResults,
            parameterName = parameterName,
            parameterCaption = parameterCaption,
            roundDigits = digitSettings$digitsGeneral,
            enforceFirstCase = (parameterName == "overallPooledStDevs")
        )
    } else if (stageResults$isDatasetRates()) {
        if (outputSize != "small" && dataInput$getNumberOfGroups() > 1) {
            treatmentRateParamName <- "overallPi1"
            controlRateParamName <- "overallPi2"
            enforceFirstCase <- TRUE
            if (.isEnrichmentStageResults(stageResults)) {
                treatmentRateParamName <- "overallPisTreatment"
                controlRateParamName <- "overallPisControl"
                enforceFirstCase <- FALSE
            } else if (.isMultiArmStageResults(stageResults)) {
                treatmentRateParamName <- "overallPiTreatments"
                controlRateParamName <- "overallPiControl"
            }
            summaryFactory$addParameter(stageResults,
                parameterName = treatmentRateParamName,
                parameterCaption = "Cumulative treatment rate",
                roundDigits = digitSettings$digitsGeneral
            )

            summaryFactory$addParameter(stageResults,
                parameterName = controlRateParamName,
                parameterCaption = "Cumulative control rate",
                roundDigits = digitSettings$digitsGeneral,
                enforceFirstCase = enforceFirstCase
            )
        }
    }

    if (.isTrialDesignGroupSequential(design)) {
        summaryFactory$addParameter(stageResults,
            parameterName = "overallTestStatistics",
            parameterCaption = "Overall test statistic",
            roundDigits = ifelse(digitSettings$digitsProbabilities > 1,
                digitSettings$digitsProbabilities - 1,
                digitSettings$digitsProbabilities
            ),
            smoothedZeroFormat = TRUE
        )
        summaryFactory$addParameter(stageResults,
            parameterName = ifelse(multiHypothesesEnabled,
                "separatePValues", "overallPValues"
            ),
            parameterCaption = "Overall p-value",
            roundDigits = digitSettings$digitsProbabilities
        )
    } else {
        summaryFactory$addParameter(stageResults,
            parameterName = "testStatistics",
            parameterCaption = "Stage-wise test statistic",
            roundDigits = ifelse(digitSettings$digitsProbabilities > 1,
                digitSettings$digitsProbabilities - 1,
                digitSettings$digitsProbabilities
            ),
            smoothedZeroFormat = TRUE
        )
        summaryFactory$addParameter(stageResults,
            parameterName = ifelse(multiHypothesesEnabled, "separatePValues", "pValues"),
            parameterCaption = "Stage-wise p-value",
            roundDigits = digitSettings$digitsProbabilities
        )
    }

    if (!is.null(closedTestResults)) {
        if (outputSize == "large") {
            if (.isTrialDesignConditionalDunnett(design)) {
                summaryFactory$addParameter(closedTestResults,
                    parameterName = "conditionalErrorRate",
                    parameterCaption = "Conditional error rate",
                    roundDigits = digitSettings$digitsProbabilities,
                    smoothedZeroFormat = TRUE
                )
                summaryFactory$addParameter(closedTestResults,
                    parameterName = "secondStagePValues",
                    parameterCaption = "Second stage p-value",
                    roundDigits = digitSettings$digitsProbabilities,
                    smoothedZeroFormat = TRUE
                )
            } else {
                summaryFactory$addParameter(closedTestResults,
                    parameterName = "adjustedStageWisePValues",
                    parameterCaption = "Adjusted stage-wise p-value",
                    roundDigits = digitSettings$digitsProbabilities,
                    smoothedZeroFormat = TRUE
                )
                summaryFactory$addParameter(closedTestResults,
                    parameterName = "overallAdjustedTestStatistics",
                    parameterCaption = "Overall adjusted test statistic",
                    roundDigits = digitSettings$digitsProbabilities -
                        ifelse(.isTrialDesignFisher(design) || digitSettings$digitsProbabilities <= 1, 0, 1),
                    smoothedZeroFormat = !.isTrialDesignFisher(design)
                )
            }
        } else if (outputSize == "medium") {
            legendEntry <- .addLegendEntry("treatmentArms")
            gMax <- stageResults$getGMax()
            if (.isTrialDesignConditionalDunnett(design)) {
                summaryFactory$addParameter(closedTestResults,
                    parameterName = "adjustedStageWisePValues",
                    values = closedTestResults$conditionalErrorRate[1, ],
                    parameterCaption = paste0(
                        "Conditional error rate (",
                        paste0(1:gMax, collapse = ", "), ")"
                    ), roundDigits = digitSettings$digitsProbabilities,
                    smoothedZeroFormat = TRUE,
                    legendEntry = legendEntry
                )
                summaryFactory$addParameter(closedTestResults,
                    parameterName = "overallAdjustedTestStatistics",
                    values = closedTestResults$secondStagePValues[1, ],
                    parameterCaption = paste0(
                        "Second stage p-value (",
                        paste0(1:gMax, collapse = ", "), ")"
                    ),
                    roundDigits = digitSettings$digitsProbabilities +
                        ifelse(.isTrialDesignFisher(design), 1, 0),
                    smoothedZeroFormat = !.isTrialDesignFisher(design),
                    legendEntry = legendEntry
                )
            } else {
                summaryFactory$addParameter(closedTestResults,
                    parameterName = "adjustedStageWisePValues",
                    values = closedTestResults$adjustedStageWisePValues[1, ],
                    parameterCaption = paste0(
                        "Adjusted stage-wise p-value (",
                        paste0(1:gMax, collapse = ", "), ")"
                    ), roundDigits = digitSettings$digitsProbabilities,
                    smoothedZeroFormat = TRUE, legendEntry = legendEntry
                )
                summaryFactory$addParameter(closedTestResults,
                    parameterName = "overallAdjustedTestStatistics",
                    values = closedTestResults$overallAdjustedTestStatistics[1, ],
                    parameterCaption = paste0(
                        "Overall adjusted test statistic (",
                        paste0(1:gMax, collapse = ", "), ")"
                    ),
                    roundDigits = digitSettings$digitsProbabilities -
                        ifelse(.isTrialDesignFisher(design) ||
                            digitSettings$digitsProbabilities <= 1, 0, 1),
                    smoothedZeroFormat = !.isTrialDesignFisher(design),
                    legendEntry = legendEntry
                )
            }
        }
    }

    if (multiHypothesesEnabled) {
        summaryFactory$addParameter(closedTestResults,
            parameterName = "rejected",
            parameterCaption = "Test action: reject",
            roundDigits = digitSettings$digitsGeneral
        )
    } else {
        if (.isTrialDesignFisher(design)) {
            summaryFactory$addParameter(stageResults,
                parameterName = "combFisher",
                parameterCaption = "Fisher combination",
                roundDigits = 0
            )
        } else if (.isTrialDesignInverseNormal(design)) {
            summaryFactory$addParameter(stageResults,
                parameterName = "combInverseNormal",
                parameterCaption = "Inverse normal combination",
                roundDigits = ifelse(digitSettings$digitsProbabilities > 1,
                    digitSettings$digitsProbabilities - 1,
                    digitSettings$digitsProbabilities
                ),
                smoothedZeroFormat = TRUE
            )
        }
        summaryFactory$addParameter(analysisResults,
            parameterName = "testActions",
            parameterCaption = "Test action",
            roundDigits = digitSettings$digitsGeneral
        )
    }

    if (design$kMax > 1 && !.isTrialDesignConditionalDunnett(design)) {
        summaryFactory$addParameter(analysisResults,
            parameterName = "conditionalRejectionProbabilities",
            parameterCaption = "Conditional rejection probability",
            roundDigits = digitSettings$digitsProbabilities,
            smoothedZeroFormat = TRUE
        )
    }

    if (design$kMax > 1) {
        if (!is.null(conditionalPowerResults)) {
            summaryFactory$addParameter(conditionalPowerResults,
                parameterName = "nPlanned",
                parameterCaption = "Planned sample size",
                roundDigits = -1
            )
        } else if (analysisResults$isApplicableParameter("nPlanned")) {
            summaryFactory$addParameter(analysisResults,
                parameterName = "nPlanned",
                parameterCaption = "Planned sample size",
                roundDigits = -1
            )
        }
    }

    if (design$kMax > 1) {
        if (!is.null(conditionalPowerResults)) {
            summaryFactory$addParameter(conditionalPowerResults,
                parameterName = "conditionalPower",
                parameterCaption = "Conditional power",
                roundDigits = digitSettings$digitsProbabilities,
                smoothedZeroFormat = TRUE
            )
        } else if (!multiHypothesesEnabled &&
                analysisResults$isApplicableParameter("nPlanned")) {
            parameterName <- "conditionalPower"
            if (!is.null(analysisResults[["conditionalPowerSimulated"]]) &&
                    length(analysisResults[["conditionalPowerSimulated"]]) > 0) {
                parameterName <- "conditionalPowerSimulated"
            }
            summaryFactory$addParameter(analysisResults,
                parameterName = parameterName,
                parameterCaption = "Conditional power",
                roundDigits = digitSettings$digitsProbabilities,
                smoothedZeroFormat = TRUE
            )
        }
    }

    ciLevel <- round((1 - design$alpha * (3 - design$sided)) * 100, 2)
    if (.isTrialDesignConditionalDunnett(design)) {
        parameterCaptionRepeatedPValues <- "Overall p-value"
        parameterCaptionRepeatedCI <- paste0(ciLevel, "% overall confidence interval")
    } else {
        parameterCaptionRepeatedPValues <- ifelse(design$kMax == 1,
            ifelse(design$sided == 1, "One-sided p-value", "Two-sided p-value"),
            "Repeated p-value"
        )
        parameterCaptionRepeatedCI <- paste0(
            ciLevel, "% ",
            ifelse(design$kMax == 1, "confidence interval", "repeated confidence interval")
        )
    }

    summaryFactory$addParameter(analysisResults,
        parameterName = c(
            "repeatedConfidenceIntervalLowerBounds",
            "repeatedConfidenceIntervalUpperBounds"
        ),
        parameterCaption = parameterCaptionRepeatedCI,
        roundDigits = digitSettings$digitsGeneral
    )

    summaryFactory$addParameter(analysisResults,
        parameterName = "repeatedPValues",
        parameterCaption = parameterCaptionRepeatedPValues,
        roundDigits = digitSettings$digitsProbabilities,
        formatRepeatedPValues = TRUE
    )

    if (!multiHypothesesEnabled && !is.null(analysisResults[["finalStage"]]) &&
            !all(is.na(analysisResults$finalStage))) {
        summaryFactory$addParameter(analysisResults,
            parameterName = "finalPValues",
            parameterCaption = "Final p-value",
            roundDigits = digitSettings$digitsProbabilities
        )
        summaryFactory$addParameter(analysisResults,
            parameterName = c(
                "finalConfidenceIntervalLowerBounds",
                "finalConfidenceIntervalUpperBounds"
            ),
            parameterCaption = "Final confidence interval",
            roundDigits = digitSettings$digitsGeneral
        )
        summaryFactory$addParameter(analysisResults,
            parameterName = "medianUnbiasedEstimates",
            parameterCaption = "Median unbiased estimate",
            roundDigits = digitSettings$digitsGeneral
        )
    }

    return(summaryFactory)
}

#'
#' Main function for creating a summary of a design or design plan
#'
#' @noRd
#'
.createSummaryDesignPlan <- function(
        object,
        digits = NA_integer_,
        output = c("all", "title", "overview", "body"),
        performanceScore = NULL,
        markdown = FALSE) {
    output <- match.arg(output)
    designPlan <- NULL
    if (.isTrialDesignPlan(object) || inherits(object, "SimulationResults")) {
        design <- object$.design
        designPlan <- object
    } else if (inherits(object, "TrialDesignCharacteristics")) {
        design <- object$.design
    } else if (.isTrialDesign(object)) {
        design <- object
    } else {
        stopIllegalArgument("'object' must be a valid design, design plan, ", "or simulation result object (is class ",
            .getClassName(object), ")",
            functionName = ".createSummaryDesignPlan",
            parameter = "object", value = object
        )
    }

    digitSettings <- .getSummaryDigits(digits)
    outputSize <- .getEnvironmentVariable(
        "RPACT_SUMMARY_OUTPUT_SIZE",
        "rpact.summary.output.size",
        default = C_SUMMARY_OUTPUT_SIZE_DEFAULT,
        type = "character"
    )

    intervalFormat <- .getEnvironmentVariable(
        "RPACT_SUMMARY_INTERVALFORMAT",
        "rpact.summary.intervalFormat",
        default = "[%s; %s]",
        type = "character"
    )
    .assertIsValidSummaryIntervalFormat(intervalFormat)

    summaryFactory <- SummaryFactory$new(
        object = object,
        intervalFormat = intervalFormat,
        output = output,
        markdown = markdown
    )

    if (output %in% c("all", "title", "overview")) {
        .addDesignInformationToSummary(design, designPlan,
            summaryFactory,
            output = output, digits = digits
        )
    }

    if (!(output %in% c("all", "body"))) {
        return(summaryFactory)
    }

    multiArmEnabled <- FALSE
    enrichmentEnabled <- FALSE
    baseEnabled <- FALSE
    planningEnabled <- FALSE
    simulationEnabled <- FALSE
    survivalEnabled <- FALSE
    survivalPatientWiseEnabled <- FALSE
    countDataEnabled <- FALSE
    if (!is.null(designPlan)) {
        multiArmEnabled <- grepl("MultiArm", .getClassName(designPlan))
        enrichmentEnabled <- grepl("Enrichment", .getClassName(designPlan))
        baseEnabled <- grepl(
            "(TrialDesignPlan|SimulationResults)(Means|Rates|Survival|CountData)",
            .getClassName(designPlan)
        )
        planningEnabled <- .isTrialDesignPlan(designPlan)
        simulationEnabled <- .isSimulationResults(designPlan)
        survivalEnabled <- grepl("Survival", .getClassName(designPlan))
        countDataEnabled <- grepl("CountData", .getClassName(designPlan))
        survivalPatientWiseEnabled <- simulationEnabled && survivalEnabled &&
            (baseEnabled ||
                identical(designPlan$simulationType, "patientWise") ||
                identical(designPlan$simulationType, "patientWiseBasic"))
    }

    if (planningEnabled) {
        legendEntry <- .addLegendEntry("treatmentEffectScale")
        if (!is.null(designPlan[["criticalValuesEffectScale"]]) &&
                ncol(designPlan$criticalValuesEffectScale) > 0 &&
                !all(is.na(designPlan$criticalValuesEffectScale))) {
            summaryFactory$addParameter(designPlan,
                parameterName = "criticalValuesEffectScale",
                parameterCaption = ifelse(.isDelayedInformationEnabled(design = design),
                    "Upper bounds of continuation (t)", "Efficacy boundary (t)"
                ),
                roundDigits = digitSettings$digitsGeneral,
                legendEntry = legendEntry
            )
        } else if (!is.null(designPlan[["criticalValuesEffectScaleUpper"]]) &&
                ncol(designPlan$criticalValuesEffectScaleUpper) > 0) {
            summaryFactory$addParameter(designPlan,
                parameterName = "criticalValuesEffectScaleLower",
                parameterCaption = "Lower efficacy boundary (t)",
                roundDigits = digitSettings$digitsGeneral,
                legendEntry = legendEntry
            )
            summaryFactory$addParameter(designPlan,
                parameterName = "criticalValuesEffectScaleUpper",
                parameterCaption = "Upper efficacy boundary (t)",
                roundDigits = digitSettings$digitsGeneral,
                legendEntry = legendEntry
            )
        }

        if (!is.null(designPlan[["futilityBoundsEffectScale"]]) &&
                ncol(designPlan$futilityBoundsEffectScale) > 0 &&
                !all(is.na(designPlan$futilityBoundsEffectScale))) {
            summaryFactory$addParameter(designPlan,
                parameterName = "futilityBoundsEffectScale",
                parameterCaption = ifelse(.isDelayedInformationEnabled(design = design),
                    "Lower bounds of continuation (t)", "Futility boundary (t)"
                ),
                roundDigits = digitSettings$digitsGeneral,
                legendEntry = legendEntry
            )
        } else if (!is.null(designPlan[["futilityBoundsEffectScaleUpper"]]) &&
                ncol(designPlan$futilityBoundsEffectScaleUpper) > 0 &&
                (any(!is.na(designPlan$futilityBoundsEffectScaleLower)) ||
                    any(!is.na(designPlan$futilityBoundsEffectScaleUpper)))) {
            summaryFactory$addParameter(designPlan,
                parameterName = "futilityBoundsEffectScaleLower",
                parameterCaption = "Lower futility boundary (t)",
                roundDigits = digitSettings$digitsGeneral,
                legendEntry = legendEntry
            )
            summaryFactory$addParameter(designPlan,
                parameterName = "futilityBoundsEffectScaleUpper",
                parameterCaption = "Upper futility boundary (t)",
                roundDigits = digitSettings$digitsGeneral,
                legendEntry = legendEntry
            )
        }
    }

    if (design$.isDelayedResponseDesign()) {
        summaryFactory$addParameter(design,
            parameterName = "decisionCriticalValues",
            parameterCaption = "Decision critical values",
            roundDigits = digitSettings$digitsGeneral,
            smoothedZeroFormat = TRUE
        )

        outputSize <- .getEnvironmentVariable(
            "RPACT_SUMMARY_OUTPUT_SIZE",
            "rpact.summary.output.size",
            default = C_SUMMARY_OUTPUT_SIZE_DEFAULT,
            type = "character"
        )
        if (outputSize == "large") {
            summaryFactory$addParameter(design,
                parameterName = "reversalProbabilities",
                parameterCaption = "Reversal probabilities",
                roundDigits = digitSettings$digitsProbabilities,
                smoothedZeroFormat = TRUE
            )
        }
    }

    designCharacteristics <- .getSummaryDesignCharacteristics(design, kMaxMin = 2)
    if (is.null(designPlan)) {
        return(.addDesignCharacteristicsToSummary(
            designCharacteristics,
            summaryFactory,
            digits = digits
        ))
    }

    if (baseEnabled && countDataEnabled && !is.null(designPlan[["lambda1"]]) &&
            designPlan$isGeneratedOrDerivedParameter("lambda1")) {
        summaryFactory$addParameter(designPlan,
            parameterName = "lambda1",
            parameterCaption = "Lambda(1)",
            roundDigits = digitSettings$digitsGeneral
        )
    }

    if (baseEnabled && countDataEnabled && !is.null(designPlan[["lambda2"]]) &&
            designPlan$isGeneratedOrDerivedParameter("lambda2")) {
        summaryFactory$addParameter(designPlan,
            parameterName = "lambda2",
            parameterCaption = "Lambda(2)",
            roundDigits = digitSettings$digitsGeneral
        )
    }

    if (simulationEnabled && (multiArmEnabled || enrichmentEnabled)) {
        summaryFactory$addParameter(designPlan,
            parameterName = "rejectAtLeastOne",
            parameterCaption = "Reject at least one",
            roundDigits = digitSettings$digitsProbabilities,
            smoothedZeroFormat = TRUE, transpose = TRUE,
            lastStage = design$kMax,
            legendEntry = {
                if (multiArmEnabled) list("(i)" = "treatment arm i") else list()
            }
        )

        if (outputSize == "large" && multiArmEnabled) {
            .addSimulationMultiArmArrayParameter(designPlan,
                parameterName = "rejectedArmsPerStage",
                parameterCaption = ifelse(design$kMax == 1,
                    "Rejected arms", "Rejected arms per stage"
                ),
                summaryFactory,
                roundDigits = digitSettings$digitsProbabilities,
                smoothedZeroFormat = TRUE
            )
        }

        if (outputSize == "large" && enrichmentEnabled) {
            .addSimulationArrayToSummary(designPlan,
                parameterName = "rejectedPopulationsPerStage",
                parameterCaption = ifelse(design$kMax == 1,
                    "Rejected populations", "Rejected populations per stage"
                ),
                summaryFactory,
                digitsSampleSize = digitSettings$digitsProbabilities,
                smoothedZeroFormat = TRUE
            )
        }

        if (!(survivalEnabled && multiArmEnabled)) {
            summaryFactory$addParameter(designPlan,
                parameterName = "successPerStage",
                parameterCaption = "Success per stage",
                roundDigits = digitSettings$digitsProbabilities,
                smoothedZeroFormat = TRUE,
                transpose = TRUE
            )
        }
    }

    if (baseEnabled) {
        parameterName <- "rejectPerStage"
        if (design$kMax == 1) {
            parameterName <- "overallReject"
        }
        if (any(!is.na(designPlan[[parameterName]]))) {
            summaryFactory$addParameter(designPlan,
                parameterName = parameterName,
                parameterCaption = ifelse(design$kMax == 1, "Power", "Cumulative power"),
                roundDigits = digitSettings$digitsProbabilities,
                cumsumEnabled = TRUE,
                smoothedZeroFormat = TRUE
            )
        }
    }

    if (simulationEnabled && (multiArmEnabled || enrichmentEnabled)) {
        if (outputSize %in% c("medium", "large")) {
            if (survivalEnabled) {
                if (enrichmentEnabled) {
                    parameterName <- "singleEventsPerSubsetAndStage"
                    parameterCaption <- "Single number of events"
                } else {
                    parameterName <- "cumulativeEventsPerStage"
                    parameterCaption <- "Cumulative number of events"
                }
            } else {
                parameterName <- "sampleSizes"
                parameterCaption <- "Stage-wise number of subjects"
            }
            .addSimulationArrayToSummary(
                designPlan,
                parameterName,
                parameterCaption,
                summaryFactory,
                digitsSampleSize = digitSettings$digitsSampleSize,
                smoothedZeroFormat = TRUE
            )
        }
    } else if (baseEnabled && outputSize %in% c("medium", "large")) {
        parameterNameSubjects <- NULL
        transpose <- FALSE
        lastStage <- NA_integer_
        if (baseEnabled) {
            if (inherits(designPlan, "SimulationResults")) {
                parameterNameSubjects <- ifelse(survivalEnabled || countDataEnabled,
                    "numberOfSubjects", "sampleSizes"
                )
            } else {
                if (design$kMax == 1 && (
                        designPlan$.isSampleSizeObject() ||
                            .isTrialDesignPlanMeans(designPlan) ||
                            .isTrialDesignPlanRates(designPlan) ||
                            countDataEnabled
                    )) {
                    parameterNameSubjects <- "nFixed"
                } else if (countDataEnabled) {
                    parameterNameSubjects <- "numberOfSubjects"
                } else if (design$kMax == 1 && designPlan$.isPowerObject()) {
                    parameterNameSubjects <- "expectedNumberOfSubjects"
                    transpose <- TRUE
                    lastStage <- design$kMax
                } else {
                    parameterNameSubjects <- "numberOfSubjects"
                }
            }
        }

        if (designPlan$isGeneratedParameter(parameterNameSubjects)) {
            subjectsCaption <- ifelse(design$kMax > 1 &&
                inherits(designPlan, "SimulationResults") && !survivalEnabled,
            "Stage-wise number of subjects",
            "Number of subjects"
            )
            summaryFactory$addParameter(designPlan,
                parameterName = parameterNameSubjects,
                parameterCaption = subjectsCaption,
                roundDigits = digitSettings$digitsSampleSize,
                validateParameterType = !countDataEnabled,
                showNA = TRUE,
                transpose = transpose,
                lastStage = lastStage
            )
        }
    }

    if (simulationEnabled && (multiArmEnabled || enrichmentEnabled || countDataEnabled)) {
        if (!survivalEnabled) {
            summaryFactory$addParameter(designPlan,
                parameterName = "expectedNumberOfSubjects",
                parameterCaption = "Expected number of subjects under H1",
                roundDigits = digitSettings$digitsSampleSize,
                transpose = TRUE,
                lastStage = design$kMax
            )
        }
    } else if (baseEnabled && design$kMax > 1) {
        parameterName <- ifelse(inherits(designPlan, "TrialDesignPlan") &&
            (designPlan$.isSampleSizeObject() || countDataEnabled),
        "expectedNumberOfSubjectsH1",
        "expectedNumberOfSubjects"
        )
        summaryFactory$addParameter(designPlan,
            parameterName = parameterName,
            parameterCaption = "Expected number of subjects under H1",
            roundDigits = digitSettings$digitsSampleSize,
            transpose = TRUE,
            validateParameterType = !countDataEnabled,
            lastStage = design$kMax
        )
        if (countDataEnabled &&
                (is.null(designPlan[[parameterName]]) || all(is.na(designPlan[[parameterName]]))) &&
                !is.null(designPlan[["maxNumberOfSubjects"]]) &&
                designPlan$isGeneratedParameter("maxNumberOfSubjects")) {
            summaryFactory$addParameter(designPlan,
                parameterName = "maxNumberOfSubjects",
                parameterCaption = "Maximum number of subjects",
                roundDigits = digitSettings$digitsSampleSize
            )
        }
    }

    if (baseEnabled && survivalEnabled && outputSize %in% c("medium", "large")) {
        parameterNameEvents <- "cumulativeEventsPerStage"
        if (!simulationEnabled) {
            if (design$kMax == 1 && (
                    designPlan$.isSampleSizeObject() ||
                        .isTrialDesignPlanMeans(designPlan) ||
                        .isTrialDesignPlanRates(designPlan) ||
                        countDataEnabled
                )) {
                parameterNameEvents <- "eventsFixed"
            } else if (design$kMax == 1 && designPlan$.isPowerObject()) {
                parameterNameEvents <- "expectedNumberOfEvents"
            }
        }
        summaryFactory$addParameter(designPlan,
            parameterName = parameterNameEvents,
            parameterCaption = ifelse(design$kMax == 1,
                "Number of events",
                "Cumulative number of events"
            ),
            roundDigits = digitSettings$digitsSampleSize,
            cumsumEnabled = FALSE
        )
    }

    if (survivalEnabled && design$kMax > 1) {
        # sample size survival: expectedEventsH1
        # power survival: expectedNumberOfEvents
        # simulation survival: expectedNumberOfEvents
        parameterNameEventsH1 <- NA_character_
        if (designPlan$isGeneratedParameter("expectedNumberOfEvents")) {
            parameterNameEventsH1 <- "expectedNumberOfEvents"
        } else if (designPlan$isGeneratedParameter("expectedEventsH1")) {
            parameterNameEventsH1 <- "expectedEventsH1"
        }
        if (!is.na(parameterNameEventsH1)) {
            summaryFactory$addParameter(designPlan,
                parameterName = parameterNameEventsH1,
                parameterCaption = "Expected number of events under H1",
                roundDigits = digitSettings$digitsSampleSize,
                transpose = TRUE,
                lastStage = design$kMax
            )
        }
    }

    if (simulationEnabled && (multiArmEnabled || enrichmentEnabled)) {
        if (multiArmEnabled && outputSize %in% c("medium", "large") && design$kMax > 1) {
            .addSimulationMultiArmArrayParameter(
                designPlan = designPlan,
                parameterName = "selectedArms",
                parameterCaption = "Selected arms",
                summaryFactory = summaryFactory,
                roundDigits = digitSettings$digitsProbabilities,
                smoothedZeroFormat = TRUE
            )
        }

        if (enrichmentEnabled && outputSize %in% c("medium", "large")) {
            .addSimulationArrayToSummary(
                designPlan = designPlan,
                parameterName = "selectedPopulations",
                parameterCaption = "Selected populations",
                summaryFactory = summaryFactory,
                digitsSampleSize = digitSettings$digitsProbabilities,
                smoothedZeroFormat = TRUE
            )
        }

        if (multiArmEnabled && outputSize %in% c("medium", "large") && design$kMax > 1) {
            summaryFactory$addParameter(designPlan,
                parameterName = "numberOfSelectedArms",
                parameterCaption = "Number of selected active arms",
                roundDigits = digitSettings$digitsGeneral,
                transpose = TRUE
            )
        }

        if (enrichmentEnabled && outputSize %in% c("medium", "large")) {
            summaryFactory$addParameter(designPlan,
                parameterName = "numberOfPopulations",
                parameterCaption = "Number of populations",
                roundDigits = digitSettings$digitsGeneral,
                transpose = TRUE
            )
        }

        if (outputSize == "large" && design$kMax > 1) {
            summaryFactory$addParameter(designPlan,
                parameterName = "conditionalPowerAchieved",
                parameterCaption = "Conditional power (achieved)",
                roundDigits = digitSettings$digitsProbabilities,
                transpose = TRUE
            )
        }
    }

    if (baseEnabled && countDataEnabled && !is.null(designPlan[["calendarTime"]]) &&
            designPlan$isGeneratedParameter("calendarTime")) {
        summaryFactory$addParameter(designPlan,
            parameterName = "calendarTime",
            parameterCaption = "Calendar time",
            roundDigits = digitSettings$digitsTime,
            showNA = TRUE
        )
    }

    if (baseEnabled && countDataEnabled && !is.null(designPlan[["expectedStudyDurationH1"]]) &&
            designPlan$isGeneratedParameter("expectedStudyDurationH1")) {
        summaryFactory$addParameter(designPlan,
            parameterName = "expectedStudyDurationH1",
            parameterCaption = "Expected study duration under H1",
            roundDigits = digitSettings$digitsTime,
            transpose = TRUE,
            lastStage = design$kMax
        )
    }

    if (baseEnabled && countDataEnabled && !is.null(designPlan[["studyTime"]]) &&
            designPlan$isGeneratedParameter("studyTime")) {
        summaryFactory$addParameter(designPlan,
            parameterName = "studyTime",
            parameterCaption = "Study time",
            roundDigits = digitSettings$digitsTime
        )
    }

    if (baseEnabled && countDataEnabled && design$kMax > 1 && outputSize %in% c("medium", "large") &&
            designPlan$.isSampleSizeObject()) {
        if (outputSize == "large") {
            summaryFactory$addParameter(designPlan,
                parameterName = "informationOverStages",
                parameterCaption = "Information over stages",
                roundDigits = digitSettings$digitsSampleSize,
                roundDigitsAsInformation = TRUE
            )
            summaryFactory$addParameter(designPlan,
                parameterName = "expectedInformationH0",
                parameterCaption = "Expected information under H0",
                roundDigits = digitSettings$digitsSampleSize,
                transpose = TRUE,
                roundDigitsAsInformation = TRUE,
                lastStage = design$kMax
            )
            summaryFactory$addParameter(designPlan,
                parameterName = "expectedInformationH01",
                parameterCaption = "Expected information under H0/H1",
                roundDigits = digitSettings$digitsSampleSize,
                transpose = TRUE,
                roundDigitsAsInformation = TRUE,
                lastStage = design$kMax
            )
        }
        summaryFactory$addParameter(designPlan,
            parameterName = "expectedInformationH1",
            parameterCaption = "Expected information under H1",
            roundDigits = digitSettings$digitsSampleSize,
            transpose = TRUE,
            roundDigitsAsInformation = TRUE,
            lastStage = design$kMax
        )
    }

    if (baseEnabled && countDataEnabled && designPlan$.isSampleSizeObject() && design$kMax == 1) {
        summaryFactory$addParameter(designPlan,
            parameterName = "maxInformation",
            parameterCaption = "Maximum information",
            roundDigits = digitSettings$digitsSampleSize,
            transpose = TRUE
        )
    }

    if (!is.null(designPlan[["studyDuration"]]) &&
            designPlan$isGeneratedParameter("studyDuration")) {
        if (outputSize == "large") {
            summaryFactory$addParameter(designPlan,
                parameterName = "analysisTime", 
                parameterCaption = "Analysis time",
                roundDigits = digitSettings$digitsTime,
                transpose = multiArmEnabled || enrichmentEnabled
            )
        }

        summaryFactory$addParameter(designPlan,
            parameterName = "studyDuration", 
            parameterCaption = "Expected study duration under H1",
            roundDigits = digitSettings$digitsTime,
            smoothedZeroFormat = TRUE,
            transpose = TRUE,
            lastStage = design$kMax
        )
    }

    if (!is.null(designPlan[["allocationRatioPlanned"]]) &&
            length(unique(designPlan$allocationRatioPlanned)) > 1) {
        summaryFactory$addParameter(designPlan,
            parameterName = "allocationRatioPlanned",
            parameterCaption = "Optimum allocation ratio",
            roundDigits = digitSettings$digitsGeneral
        )
    }

    if (inherits(designPlan, "SimulationResults") &&
            !grepl("SimulationResults(MultiArm|Enrichment|CountData)", .getClassName(designPlan))) {
        summaryFactory$addParameter(designPlan,
            parameterName = "conditionalPowerAchieved",
            parameterCaption = "Conditional power (achieved)",
            roundDigits = digitSettings$digitsProbabilities
        )
    }

    probsH0 <- NULL
    probsH1 <- NULL
    if (planningEnabled && design$kMax > 1) {
        if (!is.null(designCharacteristics) &&
                .isTrialDesignInverseNormalOrGroupSequential(design) &&
                length(designCharacteristics$shift) == 1 &&
                !is.na(designCharacteristics$shift) &&
                designCharacteristics$shift >= 1) {
            probsH0 <- getPowerAndAverageSampleNumber(design, theta = 0, nMax = designCharacteristics$shift)
            probsH1 <- getPowerAndAverageSampleNumber(design, theta = 1, nMax = designCharacteristics$shift)
        }
        if (!is.null(designPlan[["rejectPerStage"]])) {
            probsH1 <- list(
                earlyStop = designPlan$rejectPerStage[1:(design$kMax - 1), ] +
                    as.vector(designPlan$futilityPerStage),
                rejectPerStage = designPlan$rejectPerStage,
                futilityPerStage = designPlan$futilityPerStage
            )
            numberOfVariants <- 1
            if (inherits(designPlan, "ParameterSet")) {
                parameterNames <- designPlan$.getVisibleFieldNamesOrdered()
                numberOfVariants <- .getMultidimensionalNumberOfVariants(designPlan, parameterNames)
            }
            if (numberOfVariants > 1 && is.matrix(probsH1$earlyStop) && ncol(probsH1$earlyStop) == 1) {
                probsH1$earlyStop <- matrix(rep(probsH1$earlyStop, numberOfVariants), ncol = numberOfVariants)
                probsH1$rejectPerStage <- matrix(rep(probsH1$rejectPerStage, numberOfVariants), ncol = numberOfVariants)
                probsH1$futilityPerStage <- matrix(rep(probsH1$futilityPerStage, numberOfVariants), ncol = numberOfVariants)
            }
        }
    }

    if (design$kMax > 1 && 
            outputSize %in% c("medium", "large") &&
            !is.null(designPlan[["earlyStop"]])) {
        summaryFactory$addParameter(
            designPlan,
            parameterName = "earlyStop",
            parameterCaption = paste0("Stopping probability", 
                ifelse(survivalPatientWiseEnabled, " by stage", "")), # under H1
            roundDigits = digitSettings$digitsProbabilities,
            smoothedZeroFormat = TRUE,
            transpose = !is(designPlan, "SimulationResultsSurvival"),
            lastStage = design$kMax
        )
    }
    
    if (planningEnabled && !is.null(probsH1)) {
        if (design$kMax > 1) {
            if (!is.null(probsH0)) {
                probsH0$earlyStop <- matrix(
                    probsH0$earlyStop[1:(design$kMax - 1), 1], 
                    ncol = 1)
                probsH0$rejectPerStage <- matrix(
                    probsH0$rejectPerStage[1:(design$kMax - 1), 1], 
                    ncol = 1)
            }

            if (is.matrix(probsH1$rejectPerStage)) {
                if (design$kMax > 1 && designPlan$.isSampleSizeObject()) {
                    probsH1$rejectPerStage <- probsH1$rejectPerStage[1:(design$kMax - 1), 1]
                } else {
                    probsH1$rejectPerStage <- matrix(
                        probsH1$rejectPerStage[1:(design$kMax - 1), ],
                        ncol = ncol(probsH1$rejectPerStage)
                    )
                }
            } else {
                probsH1$rejectPerStage <- probsH1$rejectPerStage[1:(design$kMax - 1)]
            }

            if (.isTrialDesignWithValidFutilityBounds(design)) {
                if (is.matrix(probsH1$earlyStop)) {
                    probsH1$earlyStop <- matrix(
                        probsH1$earlyStop[1:(design$kMax - 1), ],
                        ncol = ncol(probsH1$earlyStop)
                    )
                } else {
                    probsH1$earlyStop <- probsH1$earlyStop[1:(design$kMax - 1)]
                }
                if (!is.null(probsH0)) {
                    summaryFactory$addParameter(
                        probsH0,
                        parameterName = "earlyStop",
                        parameterCaption = "Stopping probability by stage (under H0)",
                        roundDigits = digitSettings$digitsProbabilities,
                        smoothedZeroFormat = TRUE
                    )
                }
                x <- designPlan
                if (is.null(x)) {
                    x <- design
                }
                summaryFactory$addParameter(x,
                    parameterName = "earlyStop",
                    values = probsH1$earlyStop,
                    parameterCaption = "Stopping probability by stage",
                    roundDigits = digitSettings$digitsProbabilities,
                    smoothedZeroFormat = TRUE
                )
            }
            if (!is.null(probsH0)) {
                summaryFactory$addParameter(probsH0,
                    parameterName = "rejectPerStage",
                    parameterCaption = "Efficacy stopping probability by stage (under H0)",
                    roundDigits = digitSettings$digitsProbabilities,
                    smoothedZeroFormat = TRUE
                )
            }
            if (designPlan$.isPowerObject()) {
                summaryFactory$addParameter(designPlan,
                    parameterName = "rejectPerStage",
                    values = probsH1$rejectPerStage,
                    parameterCaption = "Efficacy stopping probability by stage",
                    roundDigits = digitSettings$digitsProbabilities,
                    smoothedZeroFormat = TRUE
                )
            } else {
                summaryFactory$addParameter(probsH1,
                    parameterName = "rejectPerStage",
                    parameterCaption = "Efficacy stopping probability by stage",
                    roundDigits = digitSettings$digitsProbabilities,
                    smoothedZeroFormat = TRUE
                )
            }
            
            if (.isTrialDesignWithValidFutilityBounds(design)) {
                if (!is.null(probsH0)) {
                    summaryFactory$addParameter(probsH0,
                        parameterName = "futilityPerStage",
                        parameterCaption = "Futility stopping probability by stage (under H0)",
                        roundDigits = digitSettings$digitsProbabilities,
                        smoothedZeroFormat = TRUE
                    )
                }
                futilityPerStage <- probsH1$futilityPerStage
                if (designPlan$.isSampleSizeObject() && ncol(futilityPerStage) > 1) {
                    futilityPerStage <- futilityPerStage[, 1]
                }
                summaryFactory$addParameter(designPlan,
                    parameterName = "futilityPerStage",
                    values = futilityPerStage,
                    parameterCaption = "Futility stopping probability by stage",
                    roundDigits = digitSettings$digitsProbabilities,
                    smoothedZeroFormat = TRUE
                )
            }
        }
    } else {
        rejectPerStageValues <- NULL
        rejectPerStageParameterSet <- designPlan
        rejectPerStageParameterName <- "rejectPerStage"
        rejectPerStageTranspose <- FALSE
        if (simulationEnabled && survivalEnabled && design$kMax > 1) {
            if (baseEnabled && !is.null(designPlan[["rejectPerStage"]])) {
                rejectPerStageValues <- designPlan$rejectPerStage[seq_len(design$kMax - 1), , drop = FALSE]
            } else if (multiArmEnabled && !is.null(designPlan[["successPerStage"]])) {
                rejectPerStageValues <- designPlan$successPerStage[seq_len(design$kMax - 1), , drop = FALSE]
                # For a single active arm, successPerStage is intentionally marked as
                # not applicable in the result object although it is needed here as
                # the trial-level efficacy stopping probability. Wrapping the object
                # retains its varied-parameter metadata without applying that field's
                # visibility setting.
                rejectPerStageParameterSet <- list(parameterSet = designPlan)
                rejectPerStageParameterName <- "successPerStage"
                rejectPerStageTranspose <- TRUE
            }
        } else if (!is.null(probsH1) && !simulationEnabled) {
            if (is.matrix(probsH1$rejectPerStage)) {
                rejectPerStageValues <- matrix(
                    probsH1$rejectPerStage[1:(design$kMax - 1), ], 
                    ncol = ncol(probsH1$rejectPerStage))
            } else {
                rejectPerStageValues <- probsH1$rejectPerStage[1:(design$kMax - 1)]
            }
        }
        if (!is.null(rejectPerStageValues)) {
            summaryFactory$addParameter(rejectPerStageParameterSet,
                parameterName = rejectPerStageParameterName,
                values = rejectPerStageValues,
                parameterCaption = "Efficacy stopping probability by stage",
                roundDigits = digitSettings$digitsProbabilities,
                smoothedZeroFormat = TRUE,
                transpose = rejectPerStageTranspose
            )
        }

        if (.isTrialDesignWithValidFutilityBounds(design) &&
                !is.null(designPlan[["futilityPerStage"]]) &&
                !anyNA(designPlan[["futilityPerStage"]]) &&
                any(designPlan$futilityPerStage != 0) &&
                any(designPlan$futilityPerStage > 1e-08)) {
            summaryFactory$addParameter(designPlan,
                parameterName = "futilityPerStage",
                parameterCaption = "Futility stopping probability by stage", # under H1
                roundDigits = digitSettings$digitsProbabilities,
                smoothedZeroFormat = TRUE,
                transpose = grepl("MultiArm|Enrichment", .getClassName(designPlan))
            )
        }
    }

    if (!is.null(performanceScore)) {
        print(performanceScore)
        summaryFactory$addParameter(performanceScore,
            parameterName = "performanceScore",
            parameterCaption = "Performance score",
            roundDigits = digitSettings$digitsProbabilities,
            smoothedZeroFormat = TRUE
        )
    }

    return(summaryFactory)
}
