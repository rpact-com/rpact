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

.createSummaryHeaderDesign <- function(design, designPlan, summaryFactory) {
    if (is.null(designPlan)) {
        if (design$kMax == 1) {
            header <- "Fixed"
        } else {
            if (.isTrialDesignFisher(design)) {
                designType <- C_TYPE_OF_FISHER_LIST[[design$method]]
            } else if (.isTrialDesignConditionalDunnett(design)) {
                designType <- "Conditional Dunnett test"
            } else {
                designType <- C_TYPE_OF_DESIGN_LIST[[design$typeOfDesign]]
            }
            header <- .firstCharacterToUpperCase(designType)
        }
        header <- paste0(header, " design")
        if (design$.isDelayedResponseDesign()) {
            header <- paste0(header, " with delayed response")
        }
        if (design$kMax > 1 && .isTrialDesignInverseNormalOrGroupSequential(design)) {
            if (design$typeOfDesign == C_TYPE_OF_DESIGN_WT) {
                header <- .concatenateSummaryText(header,
                    paste0("(deltaWT = ", round(design$deltaWT, 3), ")"),
                    sep = " "
                )
            } else if (design$typeOfDesign == C_TYPE_OF_DESIGN_WT_OPTIMUM) {
                header <- .concatenateSummaryText(header,
                    paste0("(", design$optimizationCriterion, ", deltaWT = ", round(design$deltaWT, 3), ")"),
                    sep = " "
                )
            } else if (design$typeOfDesign == C_TYPE_OF_DESIGN_PT) {
                header <- .concatenateSummaryText(header,
                    paste0("(deltaPT1 = ", round(design$deltaPT1, 3), ""),
                    sep = " "
                )
                header <- .concatenateSummaryText(header,
                    paste0("deltaPT0 = ", round(design$deltaPT0, 3), ")"),
                    sep = ", "
                )
            } else if (design$typeOfDesign == C_TYPE_OF_DESIGN_HP) {
                header <- .concatenateSummaryText(header,
                    paste0("(constant bounds = ", round(design$constantBoundsHP, 3), ")"),
                    sep = " "
                )
            } else if (design$typeOfDesign %in% c(C_TYPE_OF_DESIGN_AS_KD, C_TYPE_OF_DESIGN_AS_HSD)) {
                header <- .concatenateSummaryText(header,
                    paste0("(gammaA = ", round(design$gammaA, 3), ")"),
                    sep = " "
                )
            } else if (design$typeOfDesign == C_TYPE_OF_DESIGN_AS_USER) {
                header <- .concatenateSummaryText(header,
                    paste0("(", .arrayToString(design$userAlphaSpending, digits = 6), ")"),
                    sep = " "
                )
            }

            if (grepl("^as", design$typeOfDesign) && design$typeBetaSpending != C_TYPE_OF_DESIGN_BS_NONE) {
                typeBetaSpending <- C_TYPE_OF_DESIGN_BS_LIST[[design$typeBetaSpending]]
                header <- .concatenateSummaryText(header, typeBetaSpending, sep = " and ")
                if (design$typeBetaSpending %in% c(C_TYPE_OF_DESIGN_BS_KD, C_TYPE_OF_DESIGN_BS_HSD)) {
                    header <- .concatenateSummaryText(header,
                        paste0("(gammaB = ", round(design$gammaB, 3), ")"),
                        sep = " "
                    )
                } else if (design$typeBetaSpending == C_TYPE_OF_DESIGN_BS_USER) {
                    header <- .concatenateSummaryText(header,
                        paste0("(", .arrayToString(design$userBetaSpending, digits = 6), ")"),
                        sep = " "
                    )
                }
            }
        }
        if (!.isDelayedInformationEnabled(design = design) &&
                (.isTrialDesignWithValidFutilityBounds(design) ||
                    (.isTrialDesignFisher(design) && any(design$alpha0Vec < 1, na.rm = TRUE)))) {
            header <- .concatenateSummaryText(
                header,
                paste0(ifelse(design$bindingFutility, "binding", "non-binding"), " futility")
            )
        }

        if (.isTrialDesignInverseNormalOrGroupSequential(design) &&
                !all(is.na(design$efficacyStops)) && !all(design$efficacyStops, na.rm = TRUE)) {
            header <- .concatenateSummaryText(
                header,
                paste0("efficacy stops ", .arrayToString(design$efficacyStops, vectorLookAndFeelEnabled = TRUE))
            )
        }

        if (.isTrialDesignInverseNormalOrGroupSequential(design) &&
                !all(is.na(design$futilityStops)) && !all(design$futilityStops, na.rm = TRUE)) {
            header <- .concatenateSummaryText(
                header,
                paste0("futility stops ", .arrayToString(design$futilityStops, vectorLookAndFeelEnabled = TRUE))
            )
        }

        header <- .addAlphaAndBetaToHeader(header, design, designPlan)
        header <- .concatenateSummaryText(header, "undefined endpoint")

        designCharacteristics <- .getSummaryDesignCharacteristics(design)
        if (!is.null(designCharacteristics)) {
            header <- .concatenateSummaryText(
                header,
                paste0("inflation factor ", round(designCharacteristics$inflationFactor, 4))
            )
            outputSize <- .getEnvironmentVariable(
                "RPACT_SUMMARY_OUTPUT_SIZE",
                "rpact.summary.output.size",
                default = C_SUMMARY_OUTPUT_SIZE_DEFAULT,
                type = "character"
            )
            if (outputSize == "large") {
                header <- .concatenateSummaryText(
                    header,
                    paste0("ASN H1 ", round(designCharacteristics$averageSampleNumber1, 4))
                )
                header <- .concatenateSummaryText(
                    header,
                    paste0("ASN H01 ", round(designCharacteristics$averageSampleNumber01, 4))
                )
                header <- .concatenateSummaryText(
                    header,
                    paste0("ASN H0 ", round(designCharacteristics$averageSampleNumber0, 4))
                )
            }
        }

        header <- paste0(header, ".")
        return(header)
    }

    settings <- .getSummaryObjectSettings(designPlan)

    header <- ""
    if (design$kMax == 1) {
        header <- paste0(header, "Fixed sample analysis")
    } else {
        header <- paste0(header, "Sequential analysis with a maximum of ", design$kMax, " looks")
        prefix <- ifelse(design$.isDelayedResponseDesign(), "delayed response ", "")
        header <- .concatenateSummaryText(header,
            paste0("(", prefix, design$.toString(startWithUpperCase = FALSE), ")"),
            sep = " "
        )
    }
    header <- .addAlphaAndBetaToHeader(header, design, designPlan, endOfRecord = TRUE)
    header <- paste0(header, "\n")

    header <- paste0(header, "The results were ")
    header <- paste0(header, ifelse(inherits(designPlan, "SimulationResults"), "simulated", "calculated"))
    header <- paste0(header, " for a ")
    if (settings$meansEnabled) {
        if (settings$multiArmEnabled && settings$groups > 1) {
            header <- .concatenateSummaryText(header, "multi-arm comparisons for means", sep = "")
        } else if (settings$enrichmentEnabled && settings$populations > 1) {
            header <- .concatenateSummaryText(header, "population enrichment comparisons for means", sep = "")
        } else if (settings$groups == 1 && !settings$multiArmEnabled) {
            header <- .concatenateSummaryText(header, "one-sample t-test", sep = "")
        } else if (settings$groups == 2 || settings$multiArmEnabled) {
            header <- .concatenateSummaryText(header, "two-sample t-test", sep = "")
        }
    } else if (settings$ratesEnabled) {
        if (settings$multiArmEnabled && settings$groups > 1) {
            header <- .concatenateSummaryText(header, "multi-arm comparisons for rates", sep = "")
        } else if (settings$enrichmentEnabled && settings$populations > 1) {
            header <- .concatenateSummaryText(header, "population enrichment comparisons for rates", sep = "")
        } else if (settings$groups == 1 && !settings$multiArmEnabled) {
            header <- .concatenateSummaryText(header, "one-sample test for rates", sep = "")
        } else if (settings$groups == 2 || settings$multiArmEnabled) {
            header <- .concatenateSummaryText(header, "two-sample test for rates", sep = "")
        }
    } else if (settings$survivalEnabled) {
        if (settings$multiArmEnabled && settings$groups > 1) {
            header <- .concatenateSummaryText(header, "multi-arm logrank test", sep = "")
        } else if (settings$enrichmentEnabled && settings$populations > 1) {
            header <- .concatenateSummaryText(header, "population enrichment logrank test", sep = "")
        } else if (settings$groups == 2 || settings$multiArmEnabled) {
            header <- .concatenateSummaryText(header, "two-sample logrank test", sep = "")
        }
    } else if (settings$countDataEnabled) {
        header <- .concatenateSummaryText(header, "two-sample Wald-test for count data", sep = "")
    }

    part <- ""
    if (settings$multiArmEnabled && settings$groups > 1) {
        part <- .concatenateSummaryText(part, paste0(settings$groups, " treatments vs. control"))
    } else if (settings$enrichmentEnabled) {
        if (settings$groups == 2) {
            part <- .concatenateSummaryText(part, "treatment vs. control")
        } else if (settings$groups > 2) {
            part <- .concatenateSummaryText(part, paste0(settings$groups, " treatments vs. control"))
        }
        part <- .concatenateSummaryText(part, paste0(
            settings$populations, " population",
            ifelse(settings$populations == 1, "", "s")
        ))
    }
    if (!is.null(designPlan) && (.isTrialDesignPlan(designPlan) || inherits(designPlan, "SimulationResults")) &&
            !settings$multiArmEnabled && !settings$enrichmentEnabled && !settings$survivalEnabled) {
        if (settings$ratesEnabled) {
            if (settings$groups == 1) {
                part <- .concatenateSummaryText(part, ifelse(designPlan$normalApproximation,
                    "normal approximation", "exact test"
                ))
                if (.isTrialDesignPlan(designPlan) && !designPlan$normalApproximation) {
                    part <- .concatenateSummaryText(part, ifelse(designPlan$conservative,
                        "conservative solution", "non-conservative solution"
                    ))
                }
            } else {
                part <- .concatenateSummaryText(part, ifelse(designPlan$normalApproximation,
                    "normal approximation", "exact test of Fisher"
                ))
            }
        } else if (!is.null(designPlan[["normalApproximation"]]) && designPlan$normalApproximation) {
            part <- .concatenateSummaryText(part, "normal approximation")
        }
    }
    if (part != "") {
        header <- .concatenateSummaryText(header, paste0("(", part, ")"), sep = " ")
    }
    if (settings$countDataEnabled && (.isTrialDesignInverseNormalOrGroupSequentialOrFixed(design) ||
            inherits(designPlan, "SimulationResults"))) {
        header <- .concatenateSummaryText(header, .createSummaryHypothesisText(designPlan, summaryFactory))
        if (!is.null(designPlan[["theta"]]) && length(designPlan$theta) == 1) {
            effectText <- paste0("H1: effect = ", round(designPlan$theta, 3))
        } else {
            effectText <- "H1: effect as specified"
        }
        header <- .concatenateSummaryText(header, effectText)
        header <- .addAdditionalArgumentsToHeader(header, designPlan, settings)
    } else if (settings$meansEnabled && (.isTrialDesignInverseNormalOrGroupSequentialOrFixed(design) ||
            inherits(designPlan, "SimulationResults"))) {
        header <- .concatenateSummaryText(header, .createSummaryHypothesisText(designPlan, summaryFactory))
        if (!is.null(designPlan[["alternative"]]) && length(designPlan$alternative) == 1) {
            alternativeText <- paste0("H1: effect = ", round(designPlan$alternative, 3))
        } else if (!is.null(designPlan[["muMaxVector"]]) && length(designPlan$muMaxVector) == 1) {
            alternativeText <- paste0("H1: mu_max = ", round(designPlan$muMaxVector, 3))
        } else if (!is.null(designPlan[["effectList"]]) && !is.null(designPlan$effectList[["effects"]]) &&
                isTRUE(nrow(designPlan$effectList$effects) == 1)) {
            alternativeText <- paste0(
                "H1: effects = ",
                .arrayToString(designPlan$effectList$effects, mode = "vector")
            )
        } else {
            alternativeText <- "H1: effect as specified"
        }
        header <- .concatenateSummaryText(header, alternativeText)

        header <- .addEnrichmentEffectListToHeader(header, designPlan)

        if (grepl("SimulationResultsEnrichment", .getClassName(designPlan))) {
            stDevs <- designPlan$effectList$stDevs
            if (length(unique(stDevs)) == 1) {
                stDevs <- unique(stDevs)
            }
            s <- ifelse(length(stDevs) != 1, "s", "")
            stDevCaption <- ifelse(.isRatioComparisonEnabled(designPlan),
                paste0("coefficient", s, " of variation"),
                paste0("standard deviation", s)
            )
            header <- .concatenateSummaryText(header, paste0(
                stDevCaption, " = ",
                .arrayToString(round(stDevs, 3), vectorLookAndFeelEnabled = TRUE)
            ))
        } else {
            stDevCaption <- ifelse(.isRatioComparisonEnabled(designPlan),
                "coefficient of variation", "standard deviation"
            )
            stDev <- designPlan$stDev
            if (length(unique(stDev)) == 1) {
                stDev <- unique(stDev)
            }
            header <- .concatenateSummaryText(header, paste0(
                stDevCaption, " = ",
                .arrayToString(round(stDev, 3), vectorLookAndFeelEnabled = TRUE)
            ))
        }
        header <- .addAdditionalArgumentsToHeader(header, designPlan, settings)
    } else if (settings$ratesEnabled && (.isTrialDesignInverseNormalOrGroupSequentialOrFixed(design) ||
            inherits(designPlan, "SimulationResults"))) {
        if (settings$groups == 1) {
            if (!is.null(designPlan[["pi1"]]) && length(designPlan$pi1) == 1) {
                treatmentRateText <- paste0("H1: pi = ", round(designPlan$pi1, 3))
            } else {
                treatmentRateText <- "H1: pi as specified"
            }

            header <- paste0(header, ",\n", .createSummaryHypothesisText(designPlan, summaryFactory))
            header <- .concatenateSummaryText(header, treatmentRateText)
            header <- .addAdditionalArgumentsToHeader(header, designPlan, settings)
        } else {
            if (!is.null(designPlan[["pi1"]]) && length(designPlan$pi1) == 1) {
                treatmentRateText <- paste0("H1: pi(1) = ", round(designPlan$pi1, 3))
            } else if (!is.null(designPlan[["piMaxVector"]]) && length(designPlan$piMaxVector) == 1) {
                treatmentRateText <- paste0(
                    "H1: pi_max = ",
                    .arrayToString(round(designPlan$piMaxVector, 3), vectorLookAndFeelEnabled = TRUE)
                )
            } else if (settings$enrichmentEnabled && !is.null(designPlan[["effectList"]]) &&
                    !is.null(designPlan$effectList[["piTreatments"]])) {
                piTreatments <- designPlan$effectList[["piTreatments"]]
                if (is.matrix(piTreatments) && nrow(piTreatments) == 1) {
                    treatmentRateText <- paste0(
                        "H1: assumed pi(treatment) = ",
                        .arrayToString(round(designPlan$effectList$piTreatments, 3), vectorLookAndFeelEnabled = TRUE)
                    )
                } else {
                    treatmentRateText <- paste0("H1: assumed pi(treatment) as specified")
                }
            } else {
                treatmentRateText <- paste0(
                    "H1: pi",
                    ifelse(settings$multiArmEnabled, "_max", "(1)"), " as specified"
                )
            }

            controlRateText <- NA_character_
            if (settings$multiArmEnabled && !is.null(designPlan[["piControl"]])) {
                controlRateText <- paste0("control rate pi(control) = ", round(designPlan$piControl, 3))
            } else if (settings$enrichmentEnabled && !is.null(designPlan[["piControls"]])) {
                controlRateText <- paste0(
                    "control rates pi(control) = ",
                    .arrayToString(round(designPlan$piControls, 3), vectorLookAndFeelEnabled = TRUE)
                )
            } else if (settings$enrichmentEnabled && !is.null(designPlan[["effectList"]]) &&
                    !is.null(designPlan$effectList[["piControls"]])) {
                # controlRateText will be created in .addEnrichmentEffectListToHeader()
            } else if (!is.null(designPlan[["pi2"]])) {
                controlRateText <- paste0("control rate pi(2) = ", round(designPlan$pi2, 3))
            } else {
                stopRuntimeIssue("failed to identify case to build ", sQuote("controlRateText"),
                    functionName = ".createSummaryHeaderDesign",
                    parameter = "controlRateText"
                )
            }
            header <- paste0(header, ",\n", .createSummaryHypothesisText(designPlan, summaryFactory))
            header <- .concatenateSummaryText(header, treatmentRateText)
            if (!is.na(controlRateText)) {
                header <- .concatenateSummaryText(header, controlRateText)
            }
            header <- .addEnrichmentEffectListToHeader(header, designPlan)
            header <- .addAdditionalArgumentsToHeader(header, designPlan, settings)
        }
    } else if (settings$survivalEnabled && (.isTrialDesignInverseNormalOrGroupSequentialOrFixed(design) ||
            inherits(designPlan, "SimulationResults"))) {
        parameterNames <- designPlan$.getVisibleFieldNamesOrdered()
        numberOfVariants <- .getMultidimensionalNumberOfVariants(designPlan, parameterNames)

        if (grepl("SimulationResultsEnrichment", .getClassName(designPlan))) {
            userDefinedParam <- "hazardRatios"
            paramName <- "hazard ratios"
            paramValue <- designPlan$effectList$hazardRatios
        } else {
            userDefinedParam <- "pi1"
            for (param in c("pi1", "lambda1", "median1", "hazardRatio")) {
                if (designPlan$isUserDefinedParameter(param) &&
                        length(designPlan[[param]]) == numberOfVariants) {
                    userDefinedParam <- param
                }
            }
            paramValue <- designPlan[[userDefinedParam]]

            if (is.null(paramValue) || length(paramValue) == 0 || all(is.na(paramValue))) {
                userDefinedParam <- "hazardRatio"
            }
            paramName <- "treatment pi(1)"
            if (userDefinedParam == "lambda1") {
                paramName <- "treatment lambda(1)"
            } else if (userDefinedParam == "median1") {
                paramName <- "treatment median(1)"
            } else if (userDefinedParam == "hazardRatio") {
                paramName <- ifelse(grepl(
                    "SimulationResultsMultiArm",
                    .getClassName(designPlan)
                ), "omega_max", "hazard ratio")
            }
        }

        if (length(designPlan[[userDefinedParam]]) == 1) {
            treatmentRateText <- paste0("H1: ", paramName, " = ", round(designPlan[[userDefinedParam]], 3))
        } else if (!is.null(designPlan[["omegaMaxVector"]]) && length(designPlan$omegaMaxVector) == 1) {
            treatmentRateText <- paste0("H1: omega_max = ", round(designPlan$omegaMaxVector, 3))
        } else if (!is.null(designPlan[["hazardRatio"]]) && (length(designPlan$hazardRatio) == 1) ||
                (inherits(designPlan, "SimulationResults") && !is.null(designPlan[[".piecewiseSurvivalTime"]]) &&
                    designPlan$.piecewiseSurvivalTime$piecewiseSurvivalEnabled)) {
            treatmentRateText <- paste0(
                "H1: hazard ratio = ",
                .arrayToString(round(designPlan$hazardRatio, 3), vectorLookAndFeelEnabled = TRUE)
            )
        } else if (settings$enrichmentEnabled && !is.null(designPlan[["effectList"]]) &&
                !is.null(designPlan$effectList[["hazardRatios"]]) &&
                is.matrix(designPlan$effectList$hazardRatios) &&
                nrow(designPlan$effectList$hazardRatios) == 1) {
            treatmentRateText <- paste0(
                "H1: hazard ratios = ",
                .arrayToString(round(designPlan$effectList$hazardRatios, 3), vectorLookAndFeelEnabled = TRUE)
            )
        } else {
            treatmentRateText <- paste0("H1: ", paramName, " as specified")
        }
        if (userDefinedParam %in% c("hazardRatio", "pi1") &&
                (designPlan$isUserDefinedParameter("pi2") ||
                    designPlan$isDefaultParameter("pi2")) &&
                length(designPlan$pi2) == 1) {
            treatmentRateText <- paste0(treatmentRateText, ", control pi(2) = ", round(designPlan$pi2, 3))
        } else if (userDefinedParam %in% c("hazardRatio", "lambda1") &&
                (designPlan$isUserDefinedParameter("lambda2") ||
                    designPlan$isDefaultParameter("lambda2")) &&
                length(designPlan$lambda2) == 1) {
            treatmentRateText <- paste0(treatmentRateText, ", control lambda(2) = ", round(designPlan$lambda2, 3))
        } else if (userDefinedParam %in% c("hazardRatio", "median1") &&
                (designPlan$isUserDefinedParameter("median2") ||
                    designPlan$isGeneratedOrDerivedParameter("median2")) &&
                length(designPlan$median2) == 1) {
            treatmentRateText <- paste0(treatmentRateText, ", control median(2) = ", round(designPlan$median2, 3))
        } else if (!is.null(designPlan[[".piecewiseSurvivalTime"]]) &&
                designPlan$.piecewiseSurvivalTime$piecewiseSurvivalEnabled) {
            treatmentRateText <- paste0(treatmentRateText, ", piecewise survival distribution")
            treatmentRateText <- paste0(
                treatmentRateText, ", \n",
                "piecewise survival time = ", .arrayToString(round(designPlan$piecewiseSurvivalTime, 4),
                    digits = 4, vectorLookAndFeelEnabled = TRUE
                ), ", \n",
                "control lambda(2) = ", .arrayToString(round(designPlan$lambda2, 4),
                    digits = 4, vectorLookAndFeelEnabled = TRUE
                )
            )
        }
        header <- paste0(header, ", \n", .createSummaryHypothesisText(designPlan, summaryFactory))
        header <- .concatenateSummaryText(header, treatmentRateText)
        header <- .addEnrichmentEffectListToHeader(header, designPlan)
        header <- .addAdditionalArgumentsToHeader(header, designPlan, settings)
    }

    if (inherits(designPlan, "SimulationResults")) {
        header <- .concatenateSummaryText(
            header,
            paste0("simulation runs = ", designPlan$maxNumberOfIterations)
        )
        header <- .concatenateSummaryText(
            header,
            paste0("seed = ", designPlan$seed)
        )
    }
    header <- paste0(header, ".")

    if ("effectMatrix" %in% names(designPlan) && !is.null(designPlan$effectMatrix)) {
        effectMatrix <- designPlan$effectMatrix
        activeArms <- nrow(effectMatrix)
        if (activeArms == 1) {
            rownames(effectMatrix) <- NULL
        }
        effectMatrixLines <- capture.output(print(effectMatrix))
        if (activeArms == 1) {
            effectMatrixLines <- substring(effectMatrixLines, 6)
        }

        header <- paste0(
            header, "\n\n",
            "User defined effect shape:",
            "\n", paste(effectMatrixLines, collapse = "\n")
        )
    }

    return(header)
}

.createSummaryHeaderAnalysisResults <- function(design, analysisResults, summaryFactory, digits) {
    digitSettings <- .getSummaryDigits(digits)

    stageResults <- analysisResults$.stageResults
    dataInput <- analysisResults$.dataInput

    multiArmEnabled <- .isMultiArmAnalysisResults(analysisResults)
    enrichmentEnabled <- .isEnrichmentAnalysisResults(analysisResults)
    multiHypothesesEnabled <- .isMultiHypothesesAnalysisResults(analysisResults)

    header <- ""
    if (design$kMax == 1) {
        header <- paste0(header, "Fixed sample analysis")
    } else {
        header <- paste0(header, "Sequential analysis with ", design$kMax, " looks")
        header <- .concatenateSummaryText(header,
            paste0("(", design$.toString(startWithUpperCase = FALSE), ")"),
            sep = " "
        )
    }
    header <- .addAlphaAndBetaToHeader(header, design, designPlan = NULL, powerEnabled = FALSE, endOfRecord = TRUE)
    header <- paste0(header, "\n")

    header <- paste0(header, "The results were calculated using a ")
    if (stageResults$isDatasetMeans()) {
        if (dataInput$getNumberOfGroups() == 1) {
            header <- paste0(header, "one-sample t-test")
        } else if (dataInput$getNumberOfGroups() == 2) {
            header <- paste0(header, "two-sample t-test")
        } else {
            header <- paste0(header, "multi-arm t-test")
        }
    } else if (stageResults$isDatasetRates()) {
        if (dataInput$getNumberOfGroups() == 1) {
            header <- paste0(header, "one-sample test for rates")
        } else if (dataInput$getNumberOfGroups() == 2) {
            header <- paste0(header, "two-sample test for rates")
        } else {
            header <- paste0(header, "multi-arm test for rates")
        }
    } else if (stageResults$isDatasetSurvival()) {
        if (dataInput$getNumberOfGroups() == 2) {
            header <- paste0(header, "two-sample logrank test")
        } else {
            header <- paste0(header, "multi-arm logrank test")
        }
    }

    if (!.isTrialDesignConditionalDunnett(design) && multiHypothesesEnabled) {
        if (stageResults$intersectionTest == "Dunnett") {
            header <- .concatenateSummaryText(header, "Dunnett intersection test")
        } else if (stageResults$intersectionTest == "Bonferroni") {
            header <- .concatenateSummaryText(header, "Bonferroni intersection test")
        } else if (stageResults$intersectionTest == "Simes") {
            header <- .concatenateSummaryText(header, "Simes intersection test")
        } else if (stageResults$intersectionTest == "Sidak") {
            header <- .concatenateSummaryText(header, "Sidak intersection test")
        } else if (stageResults$intersectionTest == "Hierarchical") {
            header <- .concatenateSummaryText(header, "Hierarchical intersection test")
        } else if (stageResults$intersectionTest == "SpiessensDebois") {
            header <- .concatenateSummaryText(header, "Spiessens and Debois intersection test")
        }
    }

    if (!is.null(stageResults[["normalApproximation"]]) && stageResults$normalApproximation) {
        header <- .concatenateSummaryText(header, "normal approximation test")
    } else if (stageResults$isDatasetRates()) {
        if (dataInput$getNumberOfGroups() == 1) {
            header <- .concatenateSummaryText(header, "exact test")
        } else {
            header <- .concatenateSummaryText(header, "exact test of Fisher")
        }
    }

    if (stageResults$isDatasetMeans() && multiHypothesesEnabled) {
        if (stageResults$varianceOption == "overallPooled") {
            header <- .concatenateSummaryText(header, "overall pooled variances option")
        } else if (stageResults$varianceOption == "pairwisePooled") {
            header <- .concatenateSummaryText(header, "pairwise pooled variances option")
        } else if (stageResults$varianceOption == "pooledFromFull") {
            header <- .concatenateSummaryText(header, "pooled from full population variances option")
        } else if (stageResults$varianceOption == "pooled") {
            header <- .concatenateSummaryText(header, "pooled variances option")
        } else if (stageResults$varianceOption == "notPooled") {
            header <- .concatenateSummaryText(header, "not pooled variances option")
        }
    }

    if (inherits(stageResults, "StageResultsMeans") && (dataInput$getNumberOfGroups() == 2)) {
        if (stageResults$equalVariances) {
            header <- .concatenateSummaryText(header, "equal variances option")
        } else {
            header <- .concatenateSummaryText(header, "unequal variances option")
        }
    }

    if (.isTrialDesignConditionalDunnett(design)) {
        if (design$secondStageConditioning) {
            header <- .concatenateSummaryText(header, "conditional second stage p-values")
        } else {
            header <- .concatenateSummaryText(header, "unconditional second stage p-values")
        }
    }

    if (enrichmentEnabled) {
        header <- .concatenateSummaryText(header, paste0(
            ifelse(analysisResults$stratifiedAnalysis, "", "non-"), "stratified analysis"
        ))
    }

    header <- paste0(header, ".\n", .createSummaryHypothesisText(analysisResults, summaryFactory))

    if (stageResults$isDatasetMeans()) {
        header <- .getSummaryHeaderEntryAnalysisResults(header, analysisResults,
            paramName1 = "thetaH1",
            paramName2 = ifelse(multiHypothesesEnabled, "assumedStDevs", "assumedStDev"),
            paramCaption1 = "assumed effect",
            paramCaption2 = "assumed standard deviation",
            shortcut1 = "thetaH1",
            shortcut2 = "sd",
            digits1 = digitSettings$digitsGeneral,
            digits2 = digitSettings$digitsGeneral
        )
    } else if (stageResults$isDatasetRates()) {
        header <- .getSummaryHeaderEntryAnalysisResults(header, analysisResults,
            paramName1 = ifelse(enrichmentEnabled, "piTreatments", ifelse(multiArmEnabled, "piTreatments", "pi1")),
            paramName2 = ifelse(enrichmentEnabled, "piControls", ifelse(multiArmEnabled, "piControl", "pi2")),
            paramCaption1 = "assumed treatment rate",
            paramCaption2 = "assumed control rate",
            shortcut1 = "pi",
            shortcut2 = "pi"
        )
    } else if (stageResults$isDatasetSurvival()) {
        header <- .getSummaryHeaderEntryAnalysisResults(header, analysisResults,
            paramName1 = "thetaH1",
            paramCaption1 = "assumed effect",
            shortcut1 = "thetaH1",
            digits1 = digitSettings$digitsGeneral
        )
    }

    header <- paste0(header, ".")
    return(header)
}

.createSummaryHeaderObject <- function(object, summaryFactory, digits = NA_integer_) {
    if (inherits(object, "TrialDesignCharacteristics")) {
        return(.createSummaryHeaderDesign(object$.design, NULL, summaryFactory))
    }

    if (.isTrialDesignPlan(object) || inherits(object, "SimulationResults")) {
        return(.createSummaryHeaderDesign(object$.design, object, summaryFactory))
    }

    if (inherits(object, "AnalysisResults")) {
        return(.createSummaryHeaderAnalysisResults(object$.design, object, summaryFactory, digits))
    }

    if (.isTrialDesign(object)) {
        return(.createSummaryHeaderDesign(object, NULL, summaryFactory))
    }

    return("")
}

.getSummaryHeaderEntryAnalysisResults <- function(
        header,
        analysisResults,
        ...,
        paramName1,
        paramName2 = NA_character_,
        paramCaption1,
        paramCaption2 = NA_character_,
        shortcut1,
        shortcut2 = NA_character_,
        digits1 = 3,
        digits2 = 3) {
    if (analysisResults$.design$kMax == 1) {
        return(header)
    }

    if (length(analysisResults$nPlanned) == 0 || all(is.na(analysisResults$nPlanned))) {
        return(header)
    }

    paramValue1 <- analysisResults[[paramName1]]
    case1 <- analysisResults$isApplicableParameter(paramName1) && !all(is.na(paramValue1))
    if (!is.na(paramCaption1) && analysisResults$isGeneratedParameter(paramName1)) {
        paramCaption1 <- sub("assumed ", "overall ", paramCaption1)
    }

    case2 <- FALSE
    if (!is.na(paramName2)) {
        paramValue2 <- analysisResults[[paramName2]]
        case2 <- analysisResults$isApplicableParameter(paramName2) &&
            !all(is.na(paramValue2))
        if (!is.na(paramCaption2) && analysisResults$isGeneratedParameter(paramName2)) {
            paramCaption2 <- sub("assumed ", "overall ", paramCaption2)
        }
    }

    if (!case1 && !case2) {
        return(header)
    }

    if (.isTrialDesignFisher(analysisResults$.design) &&
            length(analysisResults$nPlanned[!is.na(analysisResults$nPlanned)]) > 1) {
        header <- .concatenateSummaryText(header, paste0(
            "The conditional power simulation with planned sample size and ",
            analysisResults$iterations, " iterations is based on"
        ), sep = ". ")
    } else {
        header <- .concatenateSummaryText(header,
            "The conditional power calculation with planned sample size is based on",
            sep = ". "
        )
    }

    header <- .addAllocationRatioToHeader(analysisResults, header, sep = " ")

    sepPrefix <- ifelse(length(analysisResults$allocationRatioPlanned) == 0 ||
        identical(unique(analysisResults$allocationRatioPlanned), 1), "", ",")

    if (case1) {
        if (!anyNA(paramValue1) && length(unique(paramValue1)) == 1) {
            paramValue1 <- paramValue1[1]
        }
        if (length(paramValue1) == 1) {
            header <- .concatenateSummaryText(header,
                paste0(paramCaption1, " = ", ifelse(is.na(paramValue1), paramValue1, round(paramValue1, digits1))),
                sep = paste0(sepPrefix, " ")
            )
        } else {
            header <- .concatenateSummaryText(header,
                paste0(paramCaption1, ": ", .getSummaryHeaderEntryValueAnalysisResults(
                    shortcut1, paramValue1, analysisResults
                )),
                sep = paste0(sepPrefix, " ")
            )
        }
    }

    if (case2) {
        if (length(paramValue2) == 1) {
            header <- .concatenateSummaryText(header,
                paste0(paramCaption2, " = ", ifelse(is.na(paramValue2), paramValue2, round(paramValue2, digits2))),
                sep = ifelse(case1, paste0(sepPrefix, " and "), " ")
            )
        } else {
            header <- .concatenateSummaryText(header,
                paste0(paramCaption2, ": ", .getSummaryHeaderEntryValueAnalysisResults(
                    shortcut2, paramValue2, analysisResults
                )),
                sep = ifelse(case1, paste0(sepPrefix, " and "), " ")
            )
        }
    }
    return(header)
}

.addAlphaAndBetaToHeader <- function(
        header,
        design,
        designPlan,
        ...,
        endOfRecord = FALSE,
        powerEnabled = NA,
        sep = ", ") {
    if (is.na(powerEnabled)) {
        powerEnabled <- .isTrialDesignGroupSequentialOrFixed(design) &&
            (is.null(designPlan) || (!.isSimulationResults(designPlan) &&
                !identical("power", designPlan[[".objectType"]])))
    }
    header <- .concatenateSummaryText(header,
        paste0(
            ifelse(design$sided == 1, "one-sided", "two-sided"),
            ifelse(design$kMax == 1, "", " overall"),
            " significance level ", round(100 * design$alpha, 2), "%",
            ifelse(!powerEnabled && endOfRecord, ".", "")
        ),
        sep = sep
    )
    if (powerEnabled) {
        header <- .concatenateSummaryText(
            header,
            paste0("power ", round(100 * (1 - design$beta), 1), "%", ifelse(endOfRecord, ".", ""))
        )
    }
    return(header)
}

.addEnrichmentEffectListToHeader <- function(header, designPlan) {
    if (!grepl("SimulationResultsEnrichment", .getClassName(designPlan)) ||
            is.null(designPlan[["effectList"]])) {
        return(header)
    }


    subGroups <- designPlan$effectList$subGroups
    header <- .concatenateSummaryText(header, paste0(
        "subgroup",
        ifelse(length(subGroups) != 1, "s", ""),
        " = ",
        .arrayToString(subGroups, vectorLookAndFeelEnabled = TRUE)
    ))

    prevalences <- designPlan$effectList$prevalences
    header <- .concatenateSummaryText(header, paste0(
        "prevalence",
        ifelse(length(prevalences) != 1, "s", ""),
        " = ",
        .arrayToString(round(prevalences, 3), vectorLookAndFeelEnabled = TRUE)
    ))

    if (!is.null(designPlan$effectList[["piControls"]])) {
        piControls <- designPlan$effectList$piControls
        if (length(piControls) > 0) {
            if (length(unique(piControls)) == 1) {
                piControls <- piControls[1]
            }
            controlRateText <- paste0(
                "control rate", ifelse(length(piControls) == 1, "", "s"), " pi(control) = ",
                .arrayToString(round(piControls, 3), vectorLookAndFeelEnabled = (length(unique(piControls)) > 1))
            )
            header <- .concatenateSummaryText(header, controlRateText)
        }
    }

    return(header)
}

.addAdditionalArgumentsToHeader <- function(header, designPlan, settings) {
    if (settings$countDataEnabled && !is.null(designPlan[["lambda1"]]) &&
            length(designPlan$lambda1) == 1 &&
            designPlan$isUserDefinedParameter("lambda1")) {
        header <- .concatenateSummaryText(header, paste0(
            "lambda(1) = ", designPlan$lambda1
        ))
    }

    if (settings$countDataEnabled && !is.null(designPlan[["lambda2"]]) &&
            designPlan$isUserDefinedParameter("lambda2")) {
        header <- .concatenateSummaryText(header, paste0(
            "lambda(2) = ", designPlan$lambda2[1]
        ))
    }

    if (settings$countDataEnabled && !is.null(designPlan[["lambda"]]) &&
            designPlan$isUserDefinedParameter("lambda")) {
        header <- .concatenateSummaryText(header, paste0(
            "lambda = ", designPlan$lambda
        ))
    }

    if (designPlan$.design$kMax > 1) {
        if (settings$survivalEnabled) {
            if (!is.null(designPlan[["plannedEvents"]])) {
                header <- .concatenateSummaryText(header, paste0(
                    "planned cumulative events = ",
                    .arrayToString(designPlan$plannedEvents,
                        vectorLookAndFeelEnabled = (length(designPlan$plannedEvents) > 1)
                    )
                ))
            }
        } else {
            if (!is.null(designPlan[["plannedSubjects"]])) {
                header <- .concatenateSummaryText(header, paste0(
                    "planned cumulative sample size = ",
                    .arrayToString(designPlan$plannedSubjects,
                        vectorLookAndFeelEnabled = (length(designPlan$plannedSubjects) > 1)
                    )
                ))
            }
        }

        if (!is.null(designPlan[["maxNumberOfSubjects"]]) &&
                designPlan$isUserDefinedParameter("maxNumberOfSubjects")) {
            header <- .concatenateSummaryText(header, paste0(
                "maximum number of subjects = ",
                ceiling(designPlan$maxNumberOfSubjects[1])
            ))
        }

        if (settings$survivalEnabled) {
            if (!is.null(designPlan[["maxNumberOfEvents"]]) &&
                    designPlan$isUserDefinedParameter("maxNumberOfEvents")) {
                header <- .concatenateSummaryText(header, paste0(
                    "maximum number of events = ",
                    ceiling(designPlan$maxNumberOfEvents[1])
                ))
            }
        }
    } else {
        if (settings$survivalEnabled) {
            if (!is.null(designPlan[["plannedEvents"]])) {
                header <- .concatenateSummaryText(header, paste0(
                    "planned events = ",
                    .arrayToString(designPlan$plannedEvents,
                        vectorLookAndFeelEnabled = (length(designPlan$plannedEvents) > 1)
                    )
                ))
            }
        } else {
            if (!is.null(designPlan[["plannedSubjects"]])) {
                header <- .concatenateSummaryText(header, paste0(
                    "planned sample size = ",
                    .arrayToString(designPlan$plannedSubjects,
                        vectorLookAndFeelEnabled = (length(designPlan$plannedSubjects) > 1)
                    )
                ))
            }
        }

        if (!is.null(designPlan[["maxNumberOfSubjects"]]) &&
                designPlan$isUserDefinedParameter("maxNumberOfSubjects")) {
            header <- .concatenateSummaryText(header, paste0(
                "number of subjects = ",
                ceiling(designPlan$maxNumberOfSubjects[1])
            ))
        }

        if (settings$survivalEnabled) {
            if (!is.null(designPlan[["maxNumberOfEvents"]]) &&
                    designPlan$isUserDefinedParameter("maxNumberOfEvents")) {
                header <- .concatenateSummaryText(header, paste0(
                    "number of events = ",
                    designPlan$maxNumberOfEvents[1]
                ))
            }
        }
    }

    header <- .addAllocationRatioToHeader(designPlan, header)

    if (settings$survivalEnabled || settings$countDataEnabled) {
        if (settings$survivalEnabled && !is.null(designPlan[["eventTime"]]) &&
                !all(is.na(designPlan[["eventTime"]]))) {
            header <- .concatenateSummaryText(header, paste0(
                "event time = ",
                .arrayToString(designPlan$eventTime,
                    vectorLookAndFeelEnabled = (length(designPlan$eventTime) > 1)
                )
            ))
        }
        if (settings$countDataEnabled && !is.null(designPlan[["overdispersion"]]) &&
                !is.na(designPlan[["overdispersion"]])) {
            header <- .concatenateSummaryText(header, paste0(
                "overdispersion = ", designPlan$overdispersion[1]
            ))
        }
        if (settings$countDataEnabled && !is.null(designPlan[["fixedExposureTime"]]) &&
                !is.na(designPlan[["fixedExposureTime"]])) {
            header <- .concatenateSummaryText(header, paste0(
                "fixed exposure time = ", designPlan$fixedExposureTime[1]
            ))
        }
        if (!is.null(designPlan[["plannedCalendarTime"]]) && !all(is.na(designPlan$plannedCalendarTime))) {
            header <- .concatenateSummaryText(header, paste0(
                "calendar time = ",
                .arrayToString(designPlan$plannedCalendarTime,
                    vectorLookAndFeelEnabled = (length(designPlan$plannedCalendarTime) > 1)
                )
            ))
        }
        if (!is.null(designPlan[["accrualTime"]]) && !all(is.na(designPlan$accrualTime))) {
            header <- .concatenateSummaryText(header, paste0(
                "accrual time = ",
                .arrayToString(designPlan$accrualTime,
                    vectorLookAndFeelEnabled = (length(designPlan$accrualTime) > 1)
                )
            ))
        }
        if (settings$countDataEnabled && !is.null(designPlan[["accrualTime"]]) &&
                !is.null(designPlan[["accrualIntensity"]]) && !all(is.na(designPlan$accrualIntensity))) {
            header <- .concatenateSummaryText(header, paste0(
                "accrual intensity = ",
                .arrayToString(designPlan$accrualIntensity,
                    digits = 1,
                    vectorLookAndFeelEnabled = (length(designPlan$accrualIntensity) > 1)
                )
            ))
        }
        if (settings$survivalEnabled && !is.null(designPlan[["accrualTime"]]) &&
                !is.null(designPlan[["accrualIntensity"]]) && !all(is.na(designPlan$accrualIntensity)) &&
                length(designPlan$accrualIntensity) == length(designPlan$accrualTime)) {
            header <- .concatenateSummaryText(header, paste0(
                "accrual intensity = ",
                .arrayToString(designPlan$accrualIntensity,
                    digits = 1,
                    vectorLookAndFeelEnabled = (length(designPlan$accrualIntensity) > 1)
                )
            ))
        }
        if (!is.null(designPlan[["followUpTime"]]) &&
                designPlan$.getParameterType("followUpTime") %in% c(C_PARAM_USER_DEFINED, C_PARAM_DEFAULT_VALUE) &&
                length(designPlan$followUpTime) == 1 &&
                !is.na(designPlan$followUpTime)) {
            header <- .concatenateSummaryText(header, paste0(
                "follow-up time = ", round(
                    designPlan$followUpTime[1],
                    .getEnvironmentVariable(
                        "RPACT_SUMMARY_DIGITS",
                        "rpact.summary.digits",
                        default = 3L,
                        type = "integer"
                    )
                )
            ))
        }
        if (settings$survivalEnabled &&
                !is.null(designPlan[["dropoutTime"]]) &&
                !is.na(designPlan$dropoutTime)) {
            if ((!is.na(designPlan$dropoutRate1) && designPlan$dropoutRate1 > 0) ||
                    (!is.na(designPlan$dropoutRate2) && designPlan$dropoutRate2 > 0)) {
                header <- .concatenateSummaryText(header, paste0(
                    "dropout rate(1) = ",
                    .arrayToString(designPlan$dropoutRate1,
                        vectorLookAndFeelEnabled = (length(designPlan$dropoutRate1) > 1)
                    )
                ))
                header <- .concatenateSummaryText(header, paste0(
                    "dropout rate(2) = ",
                    .arrayToString(designPlan$dropoutRate2,
                        vectorLookAndFeelEnabled = (length(designPlan$dropoutRate2) > 1)
                    )
                ))
                header <- .concatenateSummaryText(header, paste0(
                    "dropout time = ",
                    .arrayToString(designPlan$dropoutTime,
                        vectorLookAndFeelEnabled = (length(designPlan$dropoutTime) > 1)
                    )
                ))
            }
        }
    }

    if (settings$multiArmEnabled && designPlan$activeArms > 1) {
        header <- .addShapeToHeader(header, designPlan)
        header <- .addSelectionToHeader(header, designPlan)
    }

    if (settings$enrichmentEnabled && settings$populations > 1) {
        header <- .addSelectionToHeader(header, designPlan)
    }

    functionName <- ifelse(settings$survivalEnabled, "calcEventsFunction", "calcSubjectsFunction")
    userDefinedFunction <- !is.null(designPlan[[functionName]]) &&
        designPlan$isUserDefinedParameter(functionName)

    if (userDefinedFunction || (!is.null(designPlan[["conditionalPower"]]) &&
            !is.na(designPlan$conditionalPower))) {
        if (userDefinedFunction) {
            header <- .concatenateSummaryText(
                header,
                paste0("sample size reassessment: user defined ", .pQuote(functionName))
            )
            if ((!is.null(designPlan[["conditionalPower"]]) && !is.na(designPlan$conditionalPower))) {
                header <- .concatenateSummaryText(
                    header,
                    paste0("conditional power = ", designPlan$conditionalPower)
                )
            }
        } else {
            if ((!is.null(designPlan[["conditionalPower"]]) && !is.na(designPlan$conditionalPower))) {
                header <- .concatenateSummaryText(
                    header,
                    paste0("sample size reassessment: conditional power = ", designPlan$conditionalPower)
                )
            }
        }

        paramName1 <- ifelse(settings$survivalEnabled,
            "minNumberOfEventsPerStage", "minNumberOfSubjectsPerStage"
        )
        paramName2 <- ifelse(settings$survivalEnabled,
            "maxNumberOfEventsPerStage", "maxNumberOfSubjectsPerStage"
        )
        paramCaption <- ifelse(settings$survivalEnabled,
            "events", "subjects"
        )
        if (!is.null(designPlan[[paramName1]])) {
            header <- .concatenateSummaryText(header, paste0(
                "minimum ", paramCaption, " per stage = ",
                .arrayToString(designPlan[[paramName1]],
                    vectorLookAndFeelEnabled = (length(designPlan[[paramName1]]) > 1)
                )
            ))
        }
        if (!is.null(designPlan[[paramName2]])) {
            header <- .concatenateSummaryText(header, paste0(
                "maximum ", paramCaption, " per stage = ",
                .arrayToString(designPlan[[paramName2]],
                    vectorLookAndFeelEnabled = (length(designPlan[[paramName2]]) > 1)
                )
            ))
        }

        if (settings$meansEnabled) {
            if (!is.na(designPlan$thetaH1)) {
                header <- .concatenateSummaryText(
                    header,
                    paste0("theta H1 = ", round(designPlan$thetaH1, 3))
                )
            }
            if (!is.na(designPlan$stDevH1)) {
                header <- .concatenateSummaryText(
                    header,
                    paste0("standard deviation H1 = ", round(designPlan$stDevH1, 3))
                )
            }
        } else if (settings$ratesEnabled) {
            if (settings$multiArmEnabled || settings$enrichmentEnabled) {
                if (settings$multiArmEnabled && !is.na(designPlan$piTreatmentsH1)) {
                    header <- .concatenateSummaryText(
                        header,
                        paste0("pi(treatment)H1 = ", round(designPlan$piTreatmentsH1, 3))
                    )
                } else if (settings$enrichmentEnabled) {
                    piTreatmentH1 <- designPlan[["piTreatmentH1"]]
                    if (is.null(piTreatmentH1)) {
                        piTreatmentH1 <- designPlan[["piTreatmentsH1"]]
                    }
                    if (!is.null(piTreatmentH1) && !is.na(piTreatmentH1)) {
                        header <- .concatenateSummaryText(
                            header,
                            paste0("pi(treatment)H1 = ", round(piTreatmentH1, 3))
                        )
                    }
                }
                if (!is.na(designPlan$piControlH1)) {
                    header <- .concatenateSummaryText(
                        header,
                        paste0("pi(control)H1 = ", round(designPlan$piControlH1, 3))
                    )
                }
            } else {
                if (!is.na(designPlan$pi1H1)) {
                    header <- .concatenateSummaryText(
                        header,
                        paste0("pi(treatment)H1 = ", round(designPlan$pi1H1, 3))
                    )
                }
                if (!is.na(designPlan$pi2H1)) {
                    header <- .concatenateSummaryText(
                        header,
                        paste0("pi(control)H1 = ", round(designPlan$pi2H1, 3))
                    )
                }
            }
        }

        if (settings$survivalEnabled && !is.null(designPlan[["thetaH1"]]) &&
                !is.na(designPlan$thetaH1)) {
            header <- .concatenateSummaryText(
                header,
                paste0("thetaH1 = ", round(designPlan$thetaH1, 3))
            )
        }
    }

    return(header)
}

.addShapeToHeader <- function(header, designPlan) {
    header <- .concatenateSummaryText(
        header,
        paste0("effect shape = ", .formatCamelCase(designPlan$typeOfShape))
    )
    if (designPlan$typeOfShape == "sigmoidEmax") {
        header <- .concatenateSummaryText(header, paste0("slope = ", designPlan$slope))
        header <- .concatenateSummaryText(header, paste0("ED50 = ", designPlan$gED50))
    }
    if (!is.null(designPlan[["doseLevels"]]) &&
            designPlan$isUserDefinedParameter("doseLevels") &&
            !all(is.na(designPlan$doseLevels))) {
        header <- .concatenateSummaryText(header, paste0(
            "dose levels = ",
            .arrayToString(designPlan$doseLevels,
                vectorLookAndFeelEnabled = (length(designPlan$doseLevels) > 1)
            )
        ))
    }

    return(header)
}

.addSelectionToHeader <- function(header, designPlan) {
    header <- .concatenateSummaryText(header, paste0("intersection test = ", designPlan$intersectionTest))

    if (designPlan$.design$kMax > 1) {
        typeOfSelectionText <- paste0("selection = ", .formatCamelCase(designPlan$typeOfSelection))
        if (designPlan$typeOfSelection == "rBest") {
            typeOfSelectionText <- paste0(typeOfSelectionText, ", r = ", designPlan$rValue)
        } else if (designPlan$typeOfSelection == "epsilon") {
            typeOfSelectionText <- paste0(typeOfSelectionText, " rule, eps = ", designPlan$epsilonValue)
        }
        if (!is.null(designPlan$threshold) && length(designPlan$threshold) == 1 && designPlan$threshold > -Inf) {
            typeOfSelectionText <- paste0(typeOfSelectionText, ", threshold = ", designPlan$threshold)
        }
        header <- .concatenateSummaryText(header, typeOfSelectionText)

        header <- .concatenateSummaryText(
            header,
            paste0("effect measure based on ", .formatCamelCase(designPlan$effectMeasure))
        )
    }

    header <- .concatenateSummaryText(
        header,
        paste0("success criterion: ", .formatCamelCase(designPlan$successCriterion))
    )

    return(header)
}

.addAllocationRatioToHeader <- function(parameterSet, header, sep = ", ") {
    if (!.isTrialDesignPlanSurvival(parameterSet) &&
        !grepl("Simulation", .getClassName(parameterSet))) {
        numberOfGroups <- 1
        if (inherits(parameterSet, "TrialDesignPlan")) {
            numberOfGroups <- parameterSet$groups
        } else if (inherits(parameterSet, "AnalysisResults")) {
            numberOfGroups <- parameterSet$.dataInput$getNumberOfGroups()
        }
        if (numberOfGroups == 1) {
            return(header)
        }
    }

    prefix <- ""
    if (!is.null(parameterSet[["optimumAllocationRatio"]]) &&
        length(parameterSet$optimumAllocationRatio) == 1 &&
        parameterSet$optimumAllocationRatio) {
        if (length(unique(parameterSet$allocationRatioPlanned)) > 1) {
            return(.concatenateSummaryText(header, "optimum planned allocation ratio", sep = sep))
        }
        prefix <- "optimum "
    }

    allocationRatioPlanned <- round(unique(parameterSet$allocationRatioPlanned), 3)
    if (identical(allocationRatioPlanned, 1) && prefix == "") {
        return(header)
    }

    if (!all(is.na(allocationRatioPlanned))) {
        return(.concatenateSummaryText(header,
                paste0(
                    prefix, "planned allocation ratio = ",
                    .arrayToString(allocationRatioPlanned,
                        vectorLookAndFeelEnabled = length(allocationRatioPlanned) > 1
                    )
                ),
                sep = sep
            ))
    } else {
        return(header)
    }
}

.getSummaryHeaderEntryValueAnalysisResults <- function(shortcut, value, analysisResults) {
    if (is.matrix(value)) {
        stage <- analysisResults$.stageResults$stage
        if (stage <= ncol(value)) {
            value <- value[, stage]
        }
    }

    value[!is.na(value)] <- round(value[!is.na(value)], 2)

    if ((is.matrix(value) && nrow(value) > 1) || length(value) > 1) {
        treatmentNames <- seq_len(length(value))
        if (.isEnrichmentAnalysisResults(analysisResults)) {
            populations <- paste0("S", treatmentNames)
            gMax <- analysisResults$.stageResults$getGMax()
            populations[treatmentNames == gMax] <- "F"
            treatmentNames <- populations
        }
        value <- paste0(paste(paste0(shortcut, "(", treatmentNames, ") = ", value)), collapse = ", ")
    }
    return(value)
}


