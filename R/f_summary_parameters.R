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

.getSummaryObjectSettings <- function(object) {
    multiArmEnabled <- grepl("MultiArm", .getClassName(object)) ||
        grepl("ConditionalDunnett", .getClassName(object))
    enrichmentEnabled <- grepl("Enrichment", .getClassName(object))
    simulationEnabled <- grepl("Simulation", .getClassName(object))
    countDataEnabled <- FALSE
    generalEnabled <- FALSE
    ratioEnabled <- FALSE
    populations <- NA_integer_
    if (inherits(object, "AnalysisResults") || inherits(object, "StageResults")) {
        groups <- object$.dataInput$getNumberOfGroups()
        generalEnabled <- object$.dataInput$isDatasetGeneral()
        meansEnabled <- object$.dataInput$isDatasetMeans()
        ratesEnabled <- object$.dataInput$isDatasetRates()
        survivalEnabled <- object$.dataInput$isDatasetSurvival()
    } else {
        generalEnabled <- inherits(object, "Dataset") && object$isDatasetGeneral()
        meansEnabled <- grepl("Means", .getClassName(object))
        ratesEnabled <- grepl("Rates", .getClassName(object))
        survivalEnabled <- grepl("Survival", .getClassName(object))
        countDataEnabled <- grepl("CountData", .getClassName(object))
        if (simulationEnabled && multiArmEnabled) {
            groups <- object$activeArms
        } else if (simulationEnabled && enrichmentEnabled) {
            groups <- 2
            populations <- object$populations
        } else {
            # for analysis multi-arm / enrichment always 2 groups are applicable
            groups <- ifelse(multiArmEnabled || enrichmentEnabled || survivalEnabled, 2, object[["groups"]])
        }
        ratioEnabled <- .isRatioComparisonEnabled(object)
    }

    return(list(
        meansEnabled = meansEnabled,
        generalEnabled = generalEnabled,
        ratesEnabled = ratesEnabled,
        survivalEnabled = survivalEnabled,
        countDataEnabled = countDataEnabled,
        groups = groups,
        populations = populations,
        multiArmEnabled = multiArmEnabled,
        enrichmentEnabled = enrichmentEnabled,
        simulationEnabled = simulationEnabled,
        ratioEnabled = ratioEnabled
    ))
}

.getSummaryParameterCaptionCriticalValues <- function(design) {
    parameterCaption <- ifelse(.isTrialDesignFisher(design),
        "Efficacy boundary (p product scale)", "Efficacy boundary (z-value scale)"
    )
    parameterCaption <- ifelse(.isDelayedInformationEnabled(design = design),
        "Upper bounds of continuation", parameterCaption
    )
    return(parameterCaption)
}

.getSummaryParameterCaptionFutilityBounds <- function(design) {
    bindingInfo <- ifelse(design$bindingFutility, "binding", "non-binding")
    parameterCaption <- ifelse(.isDelayedInformationEnabled(design = design),
        paste0("Lower bounds of continuation (", bindingInfo, ")"),
        paste0("Futility boundary (z-value scale)")
    )
    return(parameterCaption)
}


.addDesignInformationToSummary <- function(
        design,
        designPlan,
        summaryFactory,
        output = c("all", "title", "overview", "body"),
        digits = NA_integer_) {
    if (!(output %in% c("all", "overview"))) {
        return(invisible(summaryFactory))
    }

    if (design$kMax == 1) {
        summaryFactory$addItem("Stage", "Fixed")
    } else {
        summaryFactory$addItem("Stage", c(1:design$kMax))
    }

    digitSettings <- .getSummaryDigits(digits)

    if (.isTrialDesignConditionalDunnett(design)) {
        summaryFactory$addItem(
            "Fixed information at interim",
            .getSummaryValuesInPercent(design$informationAtInterim, FALSE)
        )

        summaryFactory$addParameter(design,
            parameterName = "alpha",
            parameterCaption = "Significance level",
            roundDigits = digitSettings$digitsProbabilities,
            smoothedZeroFormat = TRUE
        )

        return(invisible(summaryFactory))
    }

    if (design$kMax > 1) {
        informationRatesCaption <- "Planned information rate"
        percentFormatEnabled <- TRUE
        if (.isTrialDesignFisher(design)) {
            weights <- .getWeightsFisher(design)
            informationRatesCaption <- "Fixed weight"
            percentFormatEnabled <- FALSE
        } else if (.isTrialDesignInverseNormal(design)) {
            weights <- .getWeightsInverseNormal(design)
            informationRatesCaption <- "Fixed weight"
            percentFormatEnabled <- FALSE
        } else {
            weights <- design$informationRates
        }
        summaryFactory$addItem(
            informationRatesCaption,
            .getSummaryValuesInPercent(weights, percentFormatEnabled = percentFormatEnabled)
        )
    }

    if (design$kMax > 1 && design$.isDelayedResponseDesign()) {
        summaryFactory$addItem(
            "Delayed information",
            .getSummaryValuesInPercent(design$delayedInformation,
                percentFormatEnabled = TRUE
            )
        )
    }

    if (design$kMax > 1) {
        summaryFactory$addParameter(design,
            parameterName = "alphaSpent",
            parameterCaption = "Cumulative alpha spent",
            roundDigits = digitSettings$digitsProbabilities,
            smoothedZeroFormat = TRUE
        )

        if (design$isGeneratedParameter("betaSpent")) {
            summaryFactory$addParameter(design,
                parameterName = "betaSpent",
                parameterCaption = "Cumulative beta spent",
                roundDigits = digitSettings$digitsProbabilities,
                smoothedZeroFormat = TRUE
            )
        }
    }

    summaryFactory$addParameter(design,
        parameterName = "stageLevels",
        twoSided = design$sided == 2,
        parameterCaption = paste0(
            "Stage level", ifelse(design$kMax > 1, "s", ""),
            " (", ifelse(design$sided == 2, "two", "one"), "-sided)"
        ),
        roundDigits = digitSettings$digitsProbabilities,
        smoothedZeroFormat = TRUE
    )

    summaryFactory$addParameter(design,
        parameterName = "criticalValues",
        parameterCaption = .getSummaryParameterCaptionCriticalValues(design),
        roundDigits = digitSettings$digitsProbabilities - ifelse(.isTrialDesignFisher(design) ||
            digitSettings$digitsProbabilities <= 1, 0, 1),
        smoothedZeroFormat = !.isTrialDesignFisher(design)
    )

    if (.isTrialDesignFisher(design)) {
        if (any(design$alpha0Vec < 1, na.rm = TRUE)) {
            summaryFactory$addParameter(design,
                parameterName = "alpha0Vec",
                parameterCaption = "Futility boundary (separate p-value scale)",
                roundDigits = digitSettings$digitsProbabilities,
                smoothedZeroFormat = TRUE
            )
        }
    } else {
        if (.isTrialDesignWithValidFutilityBounds(design)) {
            summaryFactory$addParameter(design,
                parameterName = "futilityBounds",
                parameterCaption = .getSummaryParameterCaptionFutilityBounds(design),
                roundDigits = ifelse(digitSettings$digitsProbabilities > 1,
                    digitSettings$digitsProbabilities - 1, digitSettings$digitsProbabilities
                ),
                smoothedZeroFormat = TRUE
            )
        }
    }

    return(invisible(summaryFactory))
}

.addDesignCharacteristicsToSummary <- function(
        designCharacteristics,
        summaryFactory,
        digits) {
    if (is.null(designCharacteristics)) {
        return(summaryFactory)
    }

    digitSettings <- .getSummaryDigits(digits)
    design <- designCharacteristics$.design

    summaryFactory$addParameter(
        designCharacteristics,
        parameterName = "power",
        parameterCaption = ifelse(design$kMax == 1, "Power", "Cumulative power"),
        roundDigits = digitSettings$digitsProbabilities,
        smoothedZeroFormat = TRUE
    )

    if (design$kMax > 1 && !is.null(designCharacteristics[["futilityProbabilities"]]) &&
            !anyNA(designCharacteristics$futilityProbabilities) &&
            any(designCharacteristics$futilityProbabilities > 0)) {
        summaryFactory$addParameter(designCharacteristics,
            parameterName = "futilityProbabilities",
            parameterCaption = "Futility probabilities under H1",
            roundDigits = digitSettings$digitsGeneral,
            smoothedZeroFormat = TRUE
        )
    }

    return(summaryFactory)
}

.getSummaryVariedParameterNameEnrichment <- function(designPlan) {
    if (grepl("Rates", .getClassName(designPlan))) {
        return("piTreatments")
    }
    if (grepl("Survival", .getClassName(designPlan))) {
        return("hazardRatios")
    }
    return("effects")
}

.getSummaryGroup <- function(
        parameterCaption,
        numberOfVariedParams,
        variedParamNumber,
        designPlan) {
    if (numberOfVariedParams <= 1) {
        return(list(
            groupCaption = parameterCaption,
            legendEntry = list()
        ))
    }

    enrichmentEnabled <- grepl("SimulationResultsEnrichment", .getClassName(designPlan))
    if (enrichmentEnabled) {
        variedParameterName <- .getSummaryVariedParameterNameEnrichment(designPlan)
        variedParameterValues <- designPlan$effectList[[variedParameterName]]
        if (variedParameterName == "piTreatments") {
            variedParameterCaption <- "pi(treatment)"
        } else {
            variedParameterCaption <- .getParameterCaption(variedParameterName)
        }
        if (is.matrix(variedParameterValues) && ncol(variedParameterValues) == 1) {
            variedParameterCaption <- sub("s$", "", variedParameterCaption)
        }
    } else {
        variedParameterName <- .getVariedParameterSimulationMultiArm(designPlan)
        variedParameterValues <- designPlan[[variedParameterName]]
        variedParameterCaption <- .getParameterCaption(variedParameterName)
    }

    userDefinedEffectMatrix <- !enrichmentEnabled &&
        designPlan$isUserDefinedOrDerivedParameter("effectMatrix")

    if (userDefinedEffectMatrix) {
        return(list(
            groupCaption = paste0(parameterCaption, " [", variedParamNumber, "]"),
            legendEntry = .addLegendEntry("effectMatrix")
        ))
    }
    if (is.matrix(variedParameterValues)) {
        values <- variedParameterValues[variedParamNumber, ]
        if (length(values) > 1) {
            values <- .arrayToString(values, vectorLookAndFeelEnabled = TRUE)
        }
    } else {
        values <- variedParameterValues[variedParamNumber]
    }
    if (is.numeric(values)) {
        values <- round(values, 2)
    }
    return(list(
        groupCaption = paste0(
            parameterCaption, ", ",
            tolower(variedParameterCaption), " = ", values
        ),
        legendEntry = list()
    ))
}

.getSummaryGroupCaption <- function(designPlan, parameterName, numberOfGroups, groupNumber) {
    listItemPrefix <- .getEnvironmentVariable(
        "RPACT_SUMMARY_LIST_ITEM_PREFIX",
        "rpact.summary.list.item.prefix",
        default = C_SUMMARY_LIST_ITEM_PREFIX_DEFAULT,
        type = "character"
    )

    if (grepl("Enrichment", .getClassName(designPlan))) {
        categoryCaption <- .getCategoryCaptionEnrichment(designPlan, parameterName, groupNumber)
        categoryCaption <- sub("^F$", "Full population F", categoryCaption)
        categoryCaption <- sub("^R$", "Remaining population R", categoryCaption)
        categoryCaption <- sub("^S", "Subset S", categoryCaption)

        return(paste0(listItemPrefix, categoryCaption))
    }

    treatments <- ifelse(grepl("Survival", .getClassName(designPlan)), 1, 2)
    treatmentCaption <- ifelse(numberOfGroups > treatments, paste0("Treatment arm ", groupNumber), "Treatment arm")

    if (!grepl("Survival", .getClassName(designPlan)) ||
            (inherits(designPlan, "SimulationResultsMultiArmSurvival") &&
                parameterName == "singleEventsPerArmAndStage")) {
        return(ifelse(groupNumber == numberOfGroups,
            paste0(listItemPrefix, "Control arm"),
            paste0(listItemPrefix, treatmentCaption)
        ))
    }

    return(paste0(listItemPrefix, treatmentCaption, " vs. control"))
}

.addSimulationArrayToSummary <- function(
        designPlan,
        parameterName,
        parameterCaption,
        summaryFactory,
        digitsSampleSize,
        smoothedZeroFormat = FALSE) {
    arrayData <- designPlan[[parameterName]]
    if (is.null(arrayData)) {
        stopRuntimeIssue(.getClassName(designPlan), " does not contain the field ", sQuote(parameterName),
            functionName = ".addSimulationArrayToSummary",
            parameter = parameterName
        )
    }

    numberOfVariedParams <- 1
    numberOfGroups <- 1
    arrayDataDim <- dim(arrayData)
    if (length(arrayDataDim) > 1 && arrayDataDim[1] > 1) {
        numberOfVariedParams <- arrayDataDim[2]
        if (length(arrayDataDim) > 2) {
            numberOfGroups <- arrayDataDim[3]
        }
    }

    if (is.na(numberOfGroups)) {
        stopRuntimeIssue("Unable to identify 'numberOfGroups' from ",
            sQuote(parameterName), "in ", .getClassName(designPlan),
            functionName = ".addSimulationArrayToSummary",
            parameter = parameterName
        )
    }

    for (variedParamNumber in 1:numberOfVariedParams) {
        summaryGroup <- .getSummaryGroup(
            parameterCaption,
            numberOfVariedParams,
            variedParamNumber,
            designPlan
        )
        groupCaption <- summaryGroup$groupCaption
        legendEntry <- summaryGroup$legendEntry
        if (numberOfGroups > 1) {
            summaryFactory$addItem(groupCaption, "", legendEntry = legendEntry)
        }

        for (groupNumber in 1:numberOfGroups) {
            dataPerGroupAndStage <- arrayData
            if (length(arrayDataDim) > 2) {
                dataPerGroupAndStage <- arrayData[, variedParamNumber, groupNumber]
            }
            if (numberOfGroups > 1) {
                groupCaption <- .getSummaryGroupCaption(
                    designPlan,
                    parameterName, numberOfGroups, groupNumber
                )
            }
            summaryFactory$addParameter(designPlan,
                parameterName = parameterName,
                values = dataPerGroupAndStage,
                parameterCaption = groupCaption,
                roundDigits = digitsSampleSize,
                smoothedZeroFormat = smoothedZeroFormat,
                enforceFirstCase = TRUE
            )
        }
    }
}

.addSimulationMultiArmArrayParameter <- function(
        designPlan,
        parameterName,
        parameterCaption,
        summaryFactory,
        roundDigits,
        smoothedZeroFormat = FALSE) {
    arrayData <- designPlan[[parameterName]]
    if (is.array(arrayData) && length(dim(arrayData)) == 3) {
        totalNumberOfGroups <- dim(designPlan[[ifelse(grepl("Survival", .getClassName(designPlan)),
            "cumulativeEventsPerStage", "sampleSizes"
        )]])[3]
        if (is.null(totalNumberOfGroups) || is.na(totalNumberOfGroups)) {
            totalNumberOfGroups <- designPlan$activeArms
        }
        if (is.null(totalNumberOfGroups) || is.na(totalNumberOfGroups)) {
            warning("Unable to identify 'totalNumberOfGroups' from ",
                .pQuote(parameterName), "in ", .getClassName(designPlan),
                call = FALSE
            )
        }

        numberOfGroups <- dim(arrayData)[3]
        if (parameterName == "selectedArms" &&
                !grepl("Survival", .getClassName(designPlan))) {
            numberOfGroups <- numberOfGroups - 1 # remove control group
        }
        numberOfVariedParams <- dim(arrayData)[2]

        for (variedParamNumber in 1:numberOfVariedParams) {
            summaryGroup <- .getSummaryGroup(
                parameterCaption,
                numberOfVariedParams,
                variedParamNumber,
                designPlan
            )
            groupCaption <- summaryGroup$groupCaption
            legendEntry <- summaryGroup$legendEntry
            if (numberOfGroups > 1) {
                summaryFactory$addItem(groupCaption, "", legendEntry = legendEntry)
            }

            for (groupNumber in 1:numberOfGroups) {
                dataPerGroupAndStage <- arrayData[, variedParamNumber, groupNumber]
                if (numberOfGroups > 1) {
                    groupCaption <- .getSummaryGroupCaption(
                        designPlan,
                        parameterName,
                        totalNumberOfGroups,
                        groupNumber
                    )
                }
                summaryFactory$addParameter(designPlan,
                    parameterName = parameterName,
                    values = dataPerGroupAndStage,
                    parameterCaption = groupCaption,
                    roundDigits = roundDigits,
                    smoothedZeroFormat = smoothedZeroFormat,
                    enforceFirstCase = TRUE
                )
            }
        }
    } else {
        data <- designPlan[[parameterName]]
        numberOfGroups <- ncol(data)

        for (groupNumber in 1:numberOfGroups) {
            dataPerGroupAndStage <- data[, groupNumber]
            paramCaption <- ifelse(groupNumber == numberOfGroups,
                paste0(parameterCaption, ", control"),
                paste0(parameterCaption, ", treatment ", groupNumber)
            )
            summaryFactory$addParameter(designPlan,
                parameterName = parameterName,
                values = dataPerGroupAndStage,
                parameterCaption = paramCaption,
                roundDigits = roundDigits,
                smoothedZeroFormat = smoothedZeroFormat
            )
        }
    }
}
