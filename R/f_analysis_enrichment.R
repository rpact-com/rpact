## |
## |  *Analysis of enrichment designs with adaptive test*
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

#'
#' @title
#' Get Enrichment Analysis Results
#'
#' @description
#' Calculates and returns the analysis results for the specified design and data.
#'
#' @noRd
#'
.getAnalysisResultsEnrichment <- function(design,
        dataInput,
        ...,
        intersectionTest = C_INTERSECTION_TEST_ENRICHMENT_DEFAULT,
        directionUpper = NA,
        thetaH0 = NA_real_,
        nPlanned = NA_real_) {
    .assertIsTrialDesignInverseNormalOrFisher(design)
    .assertIsValidIntersectionTestEnrichment(design, intersectionTest)
    .assertIsOneSidedDesign(design, designType = "enrichment", engineType = "analysis")

    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design, showWarnings = TRUE)
    .assertIsSingleLogical(directionUpper, "directionUpper")
    .assertIsValidDataInput(dataInput = dataInput, design = design, stage = stage)
    on.exit(dataInput$.trim())
    .assertIsValidThetaH0DataInput(thetaH0, dataInput)
    .assertIsValidNPlanned(nPlanned, design$kMax, stage, required = FALSE)

    if (dataInput$isDatasetMeans()) {
        if (is.na(thetaH0)) {
            thetaH0 <- C_THETA_H0_MEANS_DEFAULT
        }
        return(.getAnalysisResultsMeansEnrichment(
            design = design,
            dataInput = dataInput, intersectionTest = intersectionTest,
            directionUpper = directionUpper, thetaH0 = thetaH0,
            nPlanned = nPlanned, stage = stage, ...
        ))
    }

    if (dataInput$isDatasetRates()) {
        if (is.na(thetaH0)) {
            thetaH0 <- C_THETA_H0_RATES_DEFAULT
        }
        return(.getAnalysisResultsRatesEnrichment(
            design = design,
            dataInput = dataInput, intersectionTest = intersectionTest,
            directionUpper = directionUpper, thetaH0 = thetaH0,
            nPlanned = nPlanned, stage = stage, ...
        ))
    }

    if (dataInput$isDatasetSurvival()) {
        if (is.na(thetaH0)) {
            thetaH0 <- C_THETA_H0_SURVIVAL_DEFAULT
        }
        return(.getAnalysisResultsSurvivalEnrichment(
            design = design,
            dataInput = dataInput, intersectionTest = intersectionTest,
            directionUpper = directionUpper, thetaH0 = thetaH0,
            nPlanned = nPlanned, stage = stage, ...
        ))
    }

    .fireDataInputNotSupportedException(dataInput)
}

#'
#' Get Stage Results
#'
#' Returns summary statistics and p-values for a 
#' given data set and a given enrichment design.
#'
#' @noRd
#'
.getStageResultsEnrichment <- function(
        design,
        dataInput,
        ...,
        directionUpper = C_DIRECTION_UPPER_DEFAULT) {
        
    .assertIsTrialDesignInverseNormalOrFisher(design)
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    .assertIsValidDataInput(dataInput = dataInput, design = design, stage = stage)
    on.exit(dataInput$.trim())

    if (dataInput$isDatasetMeans()) {
        return(.getStageResultsMeansEnrichment(
            design = design,
            dataInput = dataInput,
            directionUpper = directionUpper,
            userFunctionCallEnabled = TRUE,
            ...
        ))
    }

    if (dataInput$isDatasetRates()) {
        return(.getStageResultsRatesEnrichment(
            design = design,
            dataInput = dataInput,
            directionUpper = directionUpper,
            userFunctionCallEnabled = TRUE,
            ...
        ))
    }

    if (dataInput$isDatasetSurvival()) {
        return(.getStageResultsSurvivalEnrichment(
            design = design,
            dataInput = dataInput,
            directionUpper = directionUpper,
            userFunctionCallEnabled = TRUE,
            ...
        ))
    }

    .fireDataInputNotSupportedException(dataInput)
}

#'
#' Get Repeated Confidence Intervals for enrichment case
#'
#' Calculates and returns the lower and upper limit of the repeated 
#' confidence intervals of the trial for enrichment designs.
#'
#' @noRd
#'
.getRepeatedConfidenceIntervalsEnrichment <- function(
        design, 
        dataInput, 
        ...) {
    .assertIsTrialDesignInverseNormalOrFisher(design)
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    .assertIsValidDataInput(dataInput = dataInput, design = design, stage = stage)
    on.exit(dataInput$.trim())

    if (dataInput$isDatasetMeans()) {
        return(.getRepeatedConfidenceIntervalsMeansEnrichment(
            design = design, 
            dataInput = dataInput, 
            ...
        ))
    }

    if (dataInput$isDatasetRates()) {
        return(.getRepeatedConfidenceIntervalsRatesEnrichment(
            design = design, 
            dataInput = dataInput, 
            ...
        ))
    }

    if (dataInput$isDatasetSurvival()) {
        return(.getRepeatedConfidenceIntervalsSurvivalEnrichment(
            design = design, 
            dataInput = dataInput, 
            ...
        ))
    }

    .fireDataInputNotSupportedException(dataInput)
}

#'
#' Get Conditional Power for enrichment case
#'
#' Calculates and returns the conditional power for enrichment case.
#'
#' @noRd
#'
.getConditionalPowerEnrichment <- function(..., stageResults, nPlanned,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT) {
    .assertIsStageResults(stageResults)

    if (stageResults$isDatasetMeans()) {
        if ("assumedStDev" %in% names(list(...))) {
            warning("For enrichment analysis the argument for assumed standard deviation ",
                "is named 'assumedStDevs' and not 'assumedStDev'",
                call. = FALSE
            )
        }

        return(.getConditionalPowerMeansEnrichment(
            stageResults = stageResults,
            nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned, ...
        ))
    }

    if (stageResults$isDatasetRates()) {
        return(.getConditionalPowerRatesEnrichment(
            stageResults = stageResults,
            nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned, ...
        ))
    }

    if (stageResults$isDatasetSurvival()) {
        return(.getConditionalPowerSurvivalEnrichment(
            stageResults = stageResults,
            nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned, ...
        ))
    }

    .fireDataInputNotSupportedException(dataInput)
}

#'
#' Repeated p-values for enrichment designs
#'
#' @noRd
#'
.getRepeatedPValuesEnrichment <- function(stageResults, ..., tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .warnInCaseOfUnknownArguments(functionName = "getRepeatedPValuesEnrichment", ...)

    return(.getRepeatedPValuesMultiArm(stageResults = stageResults, tolerance = tolerance, ...))
}

#'
#' Calculation of conditional rejection probability (CRP)
#'
#' @noRd
#'
.getConditionalRejectionProbabilitiesEnrichment <- function(stageResults, ...,
        stage = stageResults$stage, iterations = C_ITERATIONS_DEFAULT, seed = NA_real_) {
    .assertIsValidStage(stage, stageResults$.design$kMax)
    gMax <- stageResults$getGMax()

    if (.isTrialDesignInverseNormal(stageResults$.design)) {
        return(.getConditionalRejectionProbabilitiesEnrichmentInverseNormal(
            stageResults = stageResults, stage = stage, ...
        ))
    } else if (.isTrialDesignFisher(stageResults$.design)) {
        return(.getConditionalRejectionProbabilitiesEnrichmentFisher(
            stageResults = stageResults, stage = stage, ...
        ))
    }

    stop(
        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
        "'design' must be an instance of TrialDesignInverseNormal or TrialDesignFisher"
    )
}

#'
#' Calculation of CRP based on inverse normal method
#'
#' @noRd
#'
.getConditionalRejectionProbabilitiesEnrichmentInverseNormal <- function(..., stageResults, stage) {
    design <- stageResults$.design
    .assertIsTrialDesignInverseNormal(design)
    .warnInCaseOfUnknownArguments(
        functionName = ".getConditionalRejectionProbabilitiesEnrichmentInverseNormal",
        ignore = c("stage", "design"), ...
    )

    kMax <- design$kMax
    if (kMax == 1) {
        return(as.matrix(NA_real_))
    }

    gMax <- stageResults$getGMax()
    conditionalRejectionProbabilities <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    weights <- .getWeightsInverseNormal(design)
    informationRates <- design$informationRates

    ctr <- .performClosedCombinationTest(stageResults = stageResults)
    criticalValues <- .getCriticalValues(design)
    for (stageIndex in (1:min(stage, kMax - 1))) {
        for (g in 1:gMax) {
            if (!is.na(ctr$separatePValues[g, stageIndex])) {
                # shifted decision region for use in getGroupSeqProbs
                # Inverse Normal Method
                shiftedDecisionRegionUpper <- criticalValues[(stageIndex + 1):kMax] *
                    sqrt(sum(weights[1:stageIndex]^2) + cumsum(weights[(stageIndex + 1):kMax]^2)) /
                    sqrt(cumsum(weights[(stageIndex + 1):kMax]^2)) -
                    min(ctr$overallAdjustedTestStatistics[ctr$indices[, g] == 1, stageIndex], na.rm = TRUE) *
                        sqrt(sum(weights[1:stageIndex]^2)) /
                        sqrt(cumsum(weights[(stageIndex + 1):kMax]^2))
                if (stageIndex == kMax - 1) {
                    shiftedFutilityBounds <- c()
                } else {
                    shiftedFutilityBounds <- design$futilityBounds[(stageIndex + 1):(kMax - 1)] *
                        sqrt(sum(weights[1:stageIndex]^2) + cumsum(weights[(stageIndex + 1):(kMax - 1)]^2)) /
                        sqrt(cumsum(weights[(stageIndex + 1):(kMax - 1)]^2)) -
                        min(ctr$overallAdjustedTestStatistics[ctr$indices[, g] == 1, stageIndex], na.rm = TRUE) *
                            sqrt(sum(weights[1:stageIndex]^2)) /
                            sqrt(cumsum(weights[(stageIndex + 1):(kMax - 1)]^2))
                }

                # scaled information for use in getGroupSeqProbs
                scaledInformation <- (informationRates[(stageIndex + 1):kMax] - informationRates[stageIndex]) /
                    (1 - informationRates[stageIndex])

                decisionMatrix <- matrix(c(
                    shiftedFutilityBounds, 
                    C_FUTILITY_BOUNDS_DEFAULT,
                    shiftedDecisionRegionUpper
                ), nrow = 2, byrow = TRUE)

                probs <- .getGroupSequentialProbabilities(
                    decisionMatrix = decisionMatrix,
                    informationRates = scaledInformation
                )
                conditionalRejectionProbabilities[g, stageIndex] <- sum(probs[3, ] - probs[2, ])
            }
        }
    }
    return(conditionalRejectionProbabilities)
}

#'
#' Calculation of conditional rejection probability based on Fisher's combination test
#'
#' @noRd
#'
.getConditionalRejectionProbabilitiesEnrichmentFisher <- function(..., stageResults, stage) {
    design <- stageResults$.design
    .assertIsTrialDesignFisher(design)
    .warnInCaseOfUnknownArguments(
        functionName = ".getConditionalRejectionProbabilitiesEnrichmentFisher",
        ignore = c("stage", "design"), ...
    )
    kMax <- design$kMax
    if (kMax == 1) {
        return(as.matrix(NA_real_))
    }
    gMax <- stageResults$getGMax()
    criticalValues <- .getCriticalValues(design)
    weights <- .getWeightsFisher(design)
    intersectionTest <- stageResults$intersectionTest

    conditionalRejectionProbabilities <- matrix(NA_real_, nrow = gMax, ncol = kMax)

    if (design$bindingFutility) {
        alpha0Vec <- design$alpha0Vec
    } else {
        alpha0Vec <- rep(1, kMax - 1)
    }

    for (g in 1:gMax) {
        for (stageIndex in (1:min(stage, kMax - 1))) {
            if (!is.na(stageResults$separatePValues[g, stageIndex])) {
                if (gMax == 1) {
                    pValues <- stageResults$separatePValues[1, 1:stageIndex]
                } else {
                    ctr <- .performClosedCombinationTest(
                        stageResults = stageResults,
                        design = design, intersectionTest = intersectionTest
                    )
                    pValues <- ctr$adjustedStageWisePValues[ctr$indices[, g] == 1, ][which.max(
                        ctr$overallAdjustedTestStatistics[ctr$indices[, g] == 1, stageIndex]
                    ), 1:stageIndex]
                }
                if (prod(pValues^weights[1:stageIndex]) <= criticalValues[stageIndex]) {
                    conditionalRejectionProbabilities[g, stageIndex] <- 1
                } else {
                    if (stageIndex < kMax - 1) {
                        conditionalRejectionProbabilities[g, stageIndex] <- .getFisherCombinationSize(
                            kMax - stageIndex,
                            alpha0Vec[(stageIndex + 1):(kMax - 1)], (criticalValues[(stageIndex + 1):kMax] /
                                prod(pValues^weights[1:stageIndex]))^(1 / weights[stageIndex + 1]),
                            weights[(stageIndex + 2):kMax] / weights[stageIndex + 1]
                        )
                    } else {
                        conditionalRejectionProbabilities[g, stageIndex] <- (criticalValues[kMax] /
                            prod(pValues^weights[1:stageIndex]))^(1 / weights[kMax])
                    }
                }
                if (design$bindingFutility) {
                    if (pValues[stageIndex] > alpha0Vec[stageIndex]) {
                        conditionalRejectionProbabilities[g, stageIndex:stage] <- 0
                        break
                    }
                }
            }
        }
    }

    conditionalRejectionProbabilities[conditionalRejectionProbabilities >= 1] <- 1
    conditionalRejectionProbabilities[conditionalRejectionProbabilities < 0] <- NA_real_

    return(conditionalRejectionProbabilities)
}

#'
#' Plotting conditional power and likelihood
#'
#' @noRd
#'
.getConditionalPowerPlotEnrichment <- function(stageResults, ...,
        nPlanned, allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        thetaRange = NA_real_, assumedStDevs = NA_real_,
        piTreatmentRange = NA_real_, piControls = NA_real_,
        iterations = C_ITERATIONS_DEFAULT, seed = NA_real_, showArms = NA_real_) {
    .stopInCaseOfIllegalStageDefinition2(...)

    kMax <- stageResults$.design$kMax
    stage <- stageResults$stage
    if (stage == kMax && length(nPlanned) > 0) {
        stage <- kMax - 1
    }
    if (stage < 1 || kMax == 1) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "cannot plot conditional power of a fixed design")
    }
    if (stage >= kMax) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "the conditional power plot is only available for subsequent stages. ",
            "Please specify a 'stage' (", stage, ") < 'kMax' (", kMax, ")"
        )
    }

    .assertIsValidNPlanned(nPlanned = nPlanned, kMax = kMax, stage = stage)
    .assertIsSingleNumber(allocationRatioPlanned, "allocationRatioPlanned")
    .assertIsInOpenInterval(allocationRatioPlanned, "allocationRatioPlanned", 
        lower = 0, upper = C_ALLOCATION_RATIO_MAXIMUM)

    if (stageResults$isDatasetMeans()) {
        .warnInCaseOfUnusedArgument(piTreatmentRange, "piTreatmentRange", NA_real_, "plot")
        .warnInCaseOfUnusedArgument(piControls, "piControls", NA_real_, "plot")
        return(.getConditionalPowerLikelihoodMeansEnrichment(
            stageResults = stageResults, stage = stage,
            nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned,
            thetaRange = thetaRange, assumedStDevs = assumedStDevs, iterations = iterations, seed = seed
        ))
    } else if (stageResults$isDatasetRates()) {
        .warnInCaseOfUnusedArgument(thetaRange, "thetaRange", NA_real_, "plot")
        .warnInCaseOfUnusedArgument(assumedStDevs, "assumedStDevs", NA_real_, "plot")
        return(.getConditionalPowerLikelihoodRatesEnrichment(
            stageResults = stageResults, stage = stage,
            nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned,
            piTreatmentRange = piTreatmentRange, piControls = piControls,
            iterations = iterations, seed = seed
        ))
    } else if (stageResults$isDatasetSurvival()) {
        .warnInCaseOfUnusedArgument(piTreatmentRange, "piTreatmentRange", NA_real_, "plot")
        .warnInCaseOfUnusedArgument(piControls, "piControls", NA_real_, "plot")
        .warnInCaseOfUnusedArgument(assumedStDevs, "assumedStDevs", NA_real_, "plot")
        return(.getConditionalPowerLikelihoodSurvivalEnrichment(
            stageResults = stageResults, stage = stage,
            nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned,
            thetaRange = thetaRange, iterations = iterations, seed = seed
        ))
    }

    .fireDataInputNotSupportedException(dataInput)
}
