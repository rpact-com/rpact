## |
## |  *Analysis of multi-arm designs with adaptive test*
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
## |  File version: $Revision: 7379 $
## |  Last changed: $Date: 2023-10-30 16:19:12 +0100 (Mo, 30 Okt 2023) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_core_utilities.R
NULL

#'
#' @title
#' Get Multi-Armed Analysis Results
#'
#' @description
#' Calculates and returns the analysis results for the specified design and data.
#'
#' @noRd
#'
.getAnalysisResultsMultiArm <- function(design, dataInput, ...,
        intersectionTest = C_INTERSECTION_TEST_MULTIARMED_DEFAULT,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        thetaH0 = NA_real_,
        nPlanned = NA_real_) {
    .assertIsTrialDesignInverseNormalOrFisherOrConditionalDunnett(design)
    .assertIsValidIntersectionTestMultiArm(design, intersectionTest)
    .assertIsOneSidedDesign(design, designType = "multi-arm", engineType = "analysis")

    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design, showWarnings = TRUE)
    .assertIsSingleLogical(directionUpper, "directionUpper")
    .assertIsValidDataInput(dataInput = dataInput, design = design, stage = stage)
    on.exit(dataInput$.trim())
    .assertIsValidThetaH0DataInput(thetaH0, dataInput)
    .assertIsValidNPlanned(nPlanned, design$kMax, stage, required = FALSE)

    intersectionTest <- .getCorrectedIntersectionTestMultiArmIfNecessary(design, intersectionTest)

    if (dataInput$isDatasetMeans()) {
        if (is.na(thetaH0)) {
            thetaH0 <- C_THETA_H0_MEANS_DEFAULT
        }
        return(.getAnalysisResultsMeansMultiArm(
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
        return(.getAnalysisResultsRatesMultiArm(
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
        return(.getAnalysisResultsSurvivalMultiArm(
            design = design,
            dataInput = dataInput, intersectionTest = intersectionTest,
            directionUpper = directionUpper, thetaH0 = thetaH0,
            nPlanned = nPlanned, stage = stage, ...
        ))
    }

    stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'dataInput' type '", .getClassName(dataInput), "' is not implemented yet")
}

#'
#' Get Stage Results
#' Returns summary statistics and p-values for a given data set and a given multi-arm design.
#'
#' @noRd
#'
.getStageResultsMultiArm <- function(design, dataInput, ...) {
    .assertIsTrialDesignInverseNormalOrFisherOrConditionalDunnett(design)
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    .assertIsValidDataInput(dataInput = dataInput, design = design, stage = stage)
    on.exit(dataInput$.trim())

    if (dataInput$isDatasetMeans()) {
        return(.getStageResultsMeansMultiArm(design = design, dataInput = dataInput, userFunctionCallEnabled = TRUE, ...))
    }

    if (dataInput$isDatasetRates()) {
        return(.getStageResultsRatesMultiArm(design = design, dataInput = dataInput, userFunctionCallEnabled = TRUE, ...))
    }

    if (dataInput$isDatasetSurvival()) {
        return(.getStageResultsSurvivalMultiArm(design = design, dataInput = dataInput, userFunctionCallEnabled = TRUE, ...))
    }

    stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'dataInput' type '", .getClassName(dataInput), "' is not supported")
}

#'
#' Get Repeated Confidence Intervals for multi-arm case
#' Calculates and returns the lower and upper limit of the repeated confidence intervals of the trial for multi-arm designs.
#'
#' @noRd
#'
.getRepeatedConfidenceIntervalsMultiArm <- function(design, dataInput, ...) {
    .assertIsTrialDesignInverseNormalOrFisherOrConditionalDunnett(design)
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    .assertIsValidDataInput(dataInput = dataInput, design = design, stage = stage)
    on.exit(dataInput$.trim())

    if (dataInput$isDatasetMeans()) {
        return(.getRepeatedConfidenceIntervalsMeansMultiArm(
            design = design, dataInput = dataInput, ...
        ))
    }

    if (dataInput$isDatasetRates()) {
        return(.getRepeatedConfidenceIntervalsRatesMultiArm(
            design = design, dataInput = dataInput, ...
        ))
    }

    if (dataInput$isDatasetSurvival()) {
        return(.getRepeatedConfidenceIntervalsSurvivalMultiArm(
            design = design, dataInput = dataInput, ...
        ))
    }

    stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'dataInput' type '", .getClassName(dataInput), "' is not implemented yet")
}

#'
#' Get Conditional Power for multi-arm case
#' Calculates and returns the conditional power for multi-arm case.
#'
#' @keywords internal
#'
#' @noRd
#'
.getConditionalPowerMultiArm <- function(..., stageResults, nPlanned,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT) {
    .assertIsStageResults(stageResults)

    if (stageResults$isDatasetMeans()) {
        if ("assumedStDev" %in% names(list(...))) {
            warning("For multi-arm analysis the argument for assumed standard deviation ",
                "is named 'assumedStDevs' and not 'assumedStDev'",
                call. = FALSE
            )
        }

        return(.getConditionalPowerMeansMultiArm(
            stageResults = stageResults,
            nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned, ...
        ))
    }

    if (stageResults$isDatasetRates()) {
        return(.getConditionalPowerRatesMultiArm(
            stageResults = stageResults,
            nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned, ...
        ))
    }

    if (stageResults$isDatasetSurvival()) {
        return(.getConditionalPowerSurvivalMultiArm(
            stageResults = stageResults,
            nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned, ...
        ))
    }

    stop(
        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'dataInput' type '",
        .getClassName(stageResults$.dataInput), "' is not implemented yet"
    )
}

.getIndicesOfClosedHypothesesSystem <- function(gMax) {
    indices <- as.data.frame(expand.grid(rep(list(1:0), gMax)))[1:(2^gMax - 1), ]
    if (gMax == 1) {
        return(as.matrix(indices))
    }

    y <- 10^(ncol(indices):1)
    indices$pos <- (as.matrix(indices) %*% y / 10)
    indices$sum <- as.numeric(rowSums(indices[, 1:gMax]))
    indices <- indices[order(indices$sum, indices$pos, decreasing = c(TRUE, TRUE)), ]
    indices <- indices[, 1:gMax]
    rownames(indices) <- as.character(1:nrow(indices))

    return(as.matrix(indices))
}


.performClosedCombinationTest <- function(..., stageResults, design = stageResults$.design,
        intersectionTest = stageResults$intersectionTest) {
    dataInput <- stageResults$.dataInput
    stage <- stageResults$stage
    gMax <- stageResults$getGMax()
    kMax <- design$kMax
    indices <- .getIndicesOfClosedHypothesesSystem(gMax = gMax)

    adjustedStageWisePValues <- matrix(NA_real_, nrow = 2^gMax - 1, ncol = kMax)
    adjustedOverallPValues <- matrix(NA_real_, nrow = 2^gMax - 1, ncol = kMax)

    overallAdjustedTestStatistics <- matrix(NA_real_, nrow = 2^gMax - 1, ncol = kMax)
    rejected <- matrix(NA, nrow = gMax, ncol = kMax)

    colnames(adjustedStageWisePValues) <- paste("stage ", (1:kMax), sep = "")
    colnames(overallAdjustedTestStatistics) <- paste("stage ", (1:kMax), sep = "")
    dimnames(rejected) <- list(paste("arm ", 1:gMax, sep = ""), paste("stage ", (1:kMax), sep = ""))

    rejectedIntersections <- matrix(rep(FALSE, stage * nrow(indices)), nrow(indices), stage)
    rejectedIntersectionsBefore <- matrix(rep(FALSE, nrow(indices)), nrow(indices), 1)

    if (.isTrialDesignFisher(design)) {
        weightsFisher <- .getWeightsFisher(design)
    } else {
        weightsInverseNormal <- .getWeightsInverseNormal(design)
    }

    for (k in 1:stage) {
        for (i in 1:(2^gMax - 1)) {
            if (!all(is.na(stageResults$separatePValues[indices[i, ] == 1, k]))) {
                if ((intersectionTest == "Dunnett") || (intersectionTest == "SpiessensDebois")) {
                    sigma <- 1
                    if (grepl("MultiArm", .getClassName(stageResults))) {
                        if (.isStageResultsMultiArmSurvival(stageResults)) {
                            allocationRatiosSelected <- as.numeric(na.omit(
                                dataInput$getAllocationRatios(stage = k, group = 1:gMax)[indices[i, ] == 1]
                            ))
                            sigma <- sqrt(allocationRatiosSelected / (1 + allocationRatiosSelected)) %*%
                                sqrt(t(allocationRatiosSelected / (1 + allocationRatiosSelected)))
                        } else {
                            sampleSizesSelected <- as.numeric(na.omit(
                                dataInput$getSampleSizes(stage = k, group = 1:gMax)[indices[i, ] == 1]
                            ))
                            sigma <- sqrt(sampleSizesSelected / (sampleSizesSelected +
                                dataInput$getSampleSizes(stage = k, group = gMax + 1))) %*%
                                sqrt(t(sampleSizesSelected / (sampleSizesSelected +
                                    dataInput$getSampleSizes(stage = k, group = gMax + 1))))
                        }
                    } else {
                        if (.isStageResultsEnrichmentSurvival(stageResults)) {
                            eventsSelected <- as.numeric(na.omit(
                                dataInput$getEvents(stage = k, group = 1)[indices[i, ] == 1]
                            ))
                            if (length(eventsSelected) == 2) {
                                if (dataInput$isStratified()) {
                                    sigma <- matrix(rep(sqrt(dataInput$getEvents(stage = k, subset = "S1") /
                                        sum(dataInput$getEvents(stage = k))), 4), nrow = 2)
                                } else {
                                    sigma <- matrix(rep(sqrt(dataInput$getEvents(stage = k, subset = "S1") /
                                        dataInput$getEvents(stage = k, subset = "F")), 4), nrow = 2)
                                }
                            }
                        } else {
                            sampleSizesSelected <- as.numeric(na.omit(
                                dataInput$getSampleSizes(stage = k, group = 1)[indices[i, ] == 1]
                            ))
                            if (length(sampleSizesSelected) == 2) {
                                if (dataInput$isStratified()) {
                                    sigma <- matrix(rep(sqrt(sum(dataInput$getSampleSizes(stage = k, subset = "S1")) /
                                        sum(dataInput$getSampleSizes(stage = k))), 4), nrow = 2)
                                } else {
                                    sigma <- matrix(rep(sqrt(sum(dataInput$getSampleSizes(stage = k, subset = "S1")) /
                                        sum(dataInput$getSampleSizes(stage = k, subset = "F"))), 4), nrow = 2)
                                }
                            }
                        }
                    }
                    if (is.matrix(sigma)) {
                        diag(sigma) <- 1
                    }

                    if (stageResults$directionUpper) {
                        maxTestStatistic <- max(stageResults$testStatistics[indices[i, ] == 1, k], na.rm = TRUE)
                    } else {
                        maxTestStatistic <- max(-stageResults$testStatistics[indices[i, ] == 1, k], na.rm = TRUE)
                    }

                    df <- NA_real_
                    if (.isStageResultsMultiArmMeans(stageResults)) {
                        if (!stageResults$normalApproximation) {
                            df <- sum(dataInput$getSampleSizes(stage = k) - 1, na.rm = TRUE)
                        }
                        adjustedStageWisePValues[i, k] <- 1 - .getMultivariateDistribution(
                            type = ifelse(stageResults$normalApproximation, "normal", "t"),
                            upper = maxTestStatistic, sigma = sigma, df = df
                        )
                    } else if (.isStageResultsEnrichmentMeans(stageResults)) {
                        if (length(sampleSizesSelected) == 1) {
                            adjustedStageWisePValues[i, k] <- stageResults$separatePValues[min(which(indices[i, ] == 1)), k]
                        } else {
                            if (!stageResults$normalApproximation) {
                                if (dataInput$isStratified()) {
                                    df <- sum(dataInput$getSampleSizes(stage = k) - 1, na.rm = TRUE)
                                } else {
                                    df <- sum(dataInput$getSampleSizes(stage = k, subset = "F") - 2, na.rm = TRUE)
                                }
                            }
                            adjustedStageWisePValues[i, k] <- 1 - .getMultivariateDistribution(
                                type = ifelse(stageResults$normalApproximation, "normal", "t"),
                                upper = maxTestStatistic, sigma = sigma, df = df
                            )
                        }
                    } else {
                        adjustedStageWisePValues[i, k] <- 1 - .getMultivariateDistribution(
                            type = "normal", upper = maxTestStatistic, sigma = sigma, df = df
                        )
                    }
                }

                #  Bonferroni adjusted p-values
                else if (intersectionTest == "Bonferroni") {
                    adjustedStageWisePValues[i, k] <- min(c(sum(indices[
                        i,
                        !is.na(stageResults$separatePValues[, k])
                    ]) *
                        min(stageResults$separatePValues[indices[i, ] == 1, k], na.rm = TRUE), 1))
                }

                #  Simes adjusted p-values
                else if (intersectionTest == "Simes") {
                    adjustedStageWisePValues[i, k] <- min(sum(indices[
                        i,
                        !is.na(stageResults$separatePValues[, k])
                    ]) /
                        (1:sum(indices[i, !is.na(stageResults$separatePValues[, k])])) *
                        sort(stageResults$separatePValues[indices[i, ] == 1, k]))
                }

                #  Sidak adjusted p-values
                else if (intersectionTest == "Sidak") {
                    adjustedStageWisePValues[i, k] <- 1 - (1 -
                        min(stageResults$separatePValues[indices[i, ] == 1, k], na.rm = TRUE))^
                        sum(indices[i, !is.na(stageResults$separatePValues[, k])])
                }

                #  Hierarchically ordered hypotheses
                else if (intersectionTest == "Hierarchical") {
                    separatePValues <- stageResults$separatePValues
                    separatePValues[is.na(separatePValues[, 1:stage])] <- 1
                    adjustedStageWisePValues[i, k] <- separatePValues[min(which(indices[i, ] == 1)), k]
                }

                if (.isTrialDesignFisher(design)) {
                    overallAdjustedTestStatistics[i, k] <-
                        prod(adjustedStageWisePValues[i, 1:k]^weightsFisher[1:k])
                } else {
                    overallAdjustedTestStatistics[i, k] <-
                        (weightsInverseNormal[1:k] %*% .getOneMinusQNorm(adjustedStageWisePValues[i, 1:k])) /
                            sqrt(sum(weightsInverseNormal[1:k]^2))
                }
            }
        }

        if (.isTrialDesignFisher(design)) {
            rejectedIntersections[, k] <- (overallAdjustedTestStatistics[, k] <= design$criticalValues[k])
        } else {
            rejectedIntersections[, k] <- (overallAdjustedTestStatistics[, k] >= design$criticalValues[k])
        }
        rejectedIntersections[is.na(rejectedIntersections[, k]), k] <- FALSE

        rejectedIntersections[, k] <- rejectedIntersections[, k] | rejectedIntersectionsBefore
        rejectedIntersectionsBefore <- matrix(rejectedIntersections[, k], ncol = 1)

        for (j in 1:gMax) {
            rejected[j, k] <- all(rejectedIntersections[indices[, j] == 1, k], na.rm = TRUE)
        }
    }

    return(list(
        .design = design,
        intersectionTest = intersectionTest,
        separatePValues = stageResults$separatePValues,
        indices = indices,
        adjustedStageWisePValues = adjustedStageWisePValues,
        overallAdjustedTestStatistics = overallAdjustedTestStatistics,
        rejected = rejected,
        rejectedIntersections = rejectedIntersections
    ))
}

#'
#' @title
#' Get Closed Combination Test Results
#'
#' @description
#' Calculates and returns the results from the closed combination test in multi-arm
#' and population enrichment designs.
#'
#' @inheritParams param_stageResults
#'
#' @family analysis functions
#'
#' @template return_object_closed_combination_test_results
#' @template how_to_get_help_for_generics
#'
#' @template examples_get_closed_combination_test_results
#'
#' @export
#'
getClosedCombinationTestResults <- function(stageResults) {
    .assertIsTrialDesignInverseNormalOrFisher(stageResults$.design)

    result <- .performClosedCombinationTest(stageResults = stageResults)
    return(ClosedCombinationTestResults$new(
        .design = result$.design,
        .enrichment = grepl("Enrichment", .getClassName(stageResults)),
        intersectionTest = result$intersectionTest,
        separatePValues = result$separatePValues,
        indices = result$indices,
        adjustedStageWisePValues = result$adjustedStageWisePValues,
        overallAdjustedTestStatistics = result$overallAdjustedTestStatistics,
        rejected = result$rejected,
        rejectedIntersections = result$rejectedIntersections
    ))
}

#'
#' Repeated p-values for multi-arm designs
#'
#' @noRd
#'
.getRepeatedPValuesMultiArm <- function(stageResults, ..., tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .warnInCaseOfUnknownArguments(functionName = "getRepeatedPValuesMultiArm", ...)

    design <- stageResults$.design
    gMax <- stageResults$getGMax()
    kMax <- design$kMax
    repeatedPValues <- matrix(NA_real_, nrow = gMax, ncol = kMax)

    if (.isTrialDesignInverseNormal(design)) {
        if (design$typeOfDesign == C_TYPE_OF_DESIGN_AS_USER) {
            warning("Repeated p-values not available for 'typeOfDesign' = '",
                C_TYPE_OF_DESIGN_AS_USER, "'",
                call. = FALSE
            )
            return(repeatedPValues)
        }

        if (design$typeOfDesign == C_TYPE_OF_DESIGN_WT_OPTIMUM) {
            warning("Repeated p-values not available for 'typeOfDesign' = '",
                C_TYPE_OF_DESIGN_WT_OPTIMUM, "'",
                call. = FALSE
            )
            return(repeatedPValues)
        }
    }

    if (.isTrialDesignFisher(design) && design$method == C_FISHER_METHOD_USER_DEFINED_ALPHA) {
        warning("Repeated p-values not available for 'method' = '",
            C_FISHER_METHOD_USER_DEFINED_ALPHA, "'",
            call. = FALSE
        )
        return(repeatedPValues)
    }

    startTime <- Sys.time()

    stage <- stageResults$stage

    if (.isTrialDesignConditionalDunnett(design)) {
        if (stage == 1 || stage > 2) {
            message("Repeated p-values can only be calculated for the second stage")
            return(repeatedPValues)
        }

        for (g in 1:gMax) {
            if (!is.na(stageResults$testStatistics[g, 2])) {
                prec <- 1
                lower <- tolerance
                upper <- 0.5
                maxSearchIterations <- 30
                while (prec > tolerance && maxSearchIterations >= 0) {
                    alpha <- (lower + upper) / 2
                    ctr <- .getClosedConditionalDunnettTestResults(
                        design = getDesignConditionalDunnett(
                            alpha = alpha, informationAtInterim = design$informationAtInterim,
                            secondStageConditioning = design$secondStageConditioning
                        ),
                        stageResults = stageResults, stage = stage
                    )
                    ifelse(ctr$rejected[g, 2], upper <- alpha, lower <- alpha)
                    prec <- upper - lower
                    maxSearchIterations <- maxSearchIterations - 1
                }
                repeatedPValues[g, 2] <- upper
            }
        }
        .logProgress("Repeated p-values for final stage calculated", startTime = startTime)
        return(repeatedPValues)
    }

    if (.isTrialDesignInverseNormal(design)) {
        typeOfDesign <- design$typeOfDesign
        deltaWT <- design$deltaWT
        typeBetaSpending <- design$typeBetaSpending

        if (!design$bindingFutility) {
            if (design$typeOfDesign == C_TYPE_OF_DESIGN_PT) {
                typeOfDesign <- C_TYPE_OF_DESIGN_WT
                deltaWT <- design$deltaPT1
            }
            if (design$typeBetaSpending != "none") {
                typeBetaSpending <- "none"
            }
        } else if (design$typeOfDesign == C_TYPE_OF_DESIGN_PT || design$typeBetaSpending != "none") {
            message("Calculation of repeated p-values might take a while for binding case, please wait...")
        }
    }

    intersectionTest <- stageResults$intersectionTest

    if (!.isTrialDesignFisher(design) && (design$typeOfDesign == C_TYPE_OF_DESIGN_HP)) {
        if (stage == kMax) {
            startTime <- Sys.time()
            for (g in 1:gMax) {
                if (!is.na(stageResults$testStatistics[g, kMax])) {
                    prec <- 1
                    lower <- .getDesignGroupSequential(
                        kMax = kMax,
                        sided = design$sided,
                        informationRates = design$informationRates,
                        typeOfDesign = C_TYPE_OF_DESIGN_HP,
                        futilityBounds = design$futilityBounds,
                        bindingFutility = design$bindingFutility
                    )$alphaSpent[kMax - 1] + tolerance
                    upper <- 0.5
                    maxSearchIterations <- 30
                    while (prec > tolerance && maxSearchIterations >= 0) {
                        alpha <- (lower + upper) / 2
                        designAlpha <- .getDesignInverseNormal(
                            kMax = kMax,
                            alpha = alpha, typeOfDesign = C_TYPE_OF_DESIGN_HP,
                            futilityBounds = design$futilityBounds,
                            sided = design$sided, bindingFutility = design$bindingFutility,
                            informationRates = design$informationRates
                        )
                        ctr <- .performClosedCombinationTest(
                            stageResults = stageResults,
                            design = designAlpha, intersectionTest = intersectionTest
                        )
                        ifelse(ctr$rejected[g, kMax], upper <- alpha, lower <- alpha)
                        prec <- upper - lower
                        maxSearchIterations <- maxSearchIterations - 1
                    }
                    repeatedPValues[g, kMax] <- upper
                }
            }
            .logProgress("Repeated p-values for final stage calculated", startTime = startTime)
        }
    } else if (kMax == 1) {
        startTime <- Sys.time()

        for (g in 1:gMax) {
            if (!is.na(stageResults$testStatistics[g, 1])) {
                prec <- 1
                lower <- tolerance
                upper <- 1 - tolerance
                maxSearchIterations <- 30
                while (prec > tolerance && maxSearchIterations >= 0) {
                    alpha <- (lower + upper) / 2
                    if (.isTrialDesignFisher(design)) {
                        designAlpha <- .getDesignFisher(kMax = 1, alpha = alpha)
                    } else {
                        designAlpha <- .getDesignInverseNormal(kMax = 1, alpha = alpha)
                    }
                    ctr <- .performClosedCombinationTest(
                        stageResults = stageResults,
                        design = designAlpha, intersectionTest = intersectionTest
                    )
                    ifelse(ctr$rejected[g, 1], upper <- alpha, lower <- alpha)
                    prec <- upper - lower
                    maxSearchIterations <- maxSearchIterations - 1
                }
                repeatedPValues[g, 1] <- upper
            }
        }
        .logProgress("Overall p-values calculated", startTime = startTime)
    } else {
        for (k in 1:stage) {
            startTime <- Sys.time()
            for (g in 1:gMax) {
                if (!is.na(stageResults$testStatistics[g, k])) {
                    prec <- 1
                    lower <- tolerance
                    upper <- 0.5
                    maxSearchIterations <- 30
                    while (prec > tolerance && maxSearchIterations >= 0) {
                        alpha <- (lower + upper) / 2
                        if (.isTrialDesignFisher(design)) {
                            designAlpha <- .getDesignFisher(
                                kMax = kMax, alpha = alpha,
                                method = design$method, alpha0Vec = design$alpha0Vec,
                                sided = design$sided, bindingFutility = design$bindingFutility,
                                informationRates = design$informationRates
                            )
                        } else {
                            designAlpha <- .getDesignInverseNormal(
                                kMax = kMax,
                                alpha = alpha, typeOfDesign = typeOfDesign, deltaWT = deltaWT,
                                typeBetaSpending = typeBetaSpending, gammaB = design$gammaB,
                                deltaPT0 = design$deltaPT0, deltaPT1 = design$deltaPT1, beta = design$beta,
                                gammaA = design$gammaA, futilityBounds = design$futilityBounds,
                                sided = design$sided, bindingFutility = design$bindingFutility,
                                informationRates = design$informationRates
                            )
                        }
                        ctr <- .performClosedCombinationTest(
                            stageResults = stageResults,
                            design = designAlpha, intersectionTest = intersectionTest
                        )
                        ifelse(ctr$rejected[g, k], upper <- alpha, lower <- alpha)
                        prec <- upper - lower
                        maxSearchIterations <- maxSearchIterations - 1
                    }
                    repeatedPValues[g, k] <- upper
                }
            }
            .logProgress("Repeated p-values for stage %s calculated", startTime = startTime, k)
        }
    }

    return(repeatedPValues)
}

#'
#' @title
#' Get Closed Conditional Dunnett Test Results
#'
#' @description
#' Calculates and returns the results from the closed conditional Dunnett test.
#'
#' @inheritParams param_stageResults
#' @inheritParams param_stage
#' @inheritParams param_three_dots
#'
#' @family analysis functions
#' @details
#' For performing the conditional Dunnett test the design must be defined through the function
#' \code{\link[=getDesignConditionalDunnett]{getDesignConditionalDunnett()}}.\cr
#' See Koenig et al. (2008) and Wassmer & Brannath (2016), chapter 11 for details of the test procedure.
#'
#' @template return_object_closed_combination_test_results
#' @template how_to_get_help_for_generics
#'
#' @template examples_get_closed_conditional_dunnett_test_results
#'
#' @export
#'
getClosedConditionalDunnettTestResults <- function(stageResults, ..., stage = stageResults$stage) {
    .assertIsStageResultsMultiArm(stageResults)
    .assertIsValidStage(stage, kMax = 2)
    .warnInCaseOfUnknownArguments(functionName = "getClosedConditionalDunnettTestResults", ignore = c("design"), ...)

    design <- stageResults$.design
    if (!is.null(list(...)[["design"]])) {
        design <- list(...)[["design"]]
    }
    .assertIsTrialDesignConditionalDunnett(design)

    result <- .getClosedConditionalDunnettTestResults(stageResults = stageResults, design = design, stage = stage)
    return(ClosedCombinationTestResults$new(
        .design = result$.design,
        .enrichment = grepl("Enrichment", .getClassName(stageResults)),
        intersectionTest = result$intersectionTest,
        indices = result$indices,
        separatePValues = result$separatePValues,
        conditionalErrorRate = result$conditionalErrorRate,
        secondStagePValues = result$secondStagePValues,
        rejected = result$rejected,
        rejectedIntersections = result$rejectedIntersections
    ))
}

.getClosedConditionalDunnettTestResults <- function(...,
        stageResults,
        design = stageResults$.design,
        stage = stageResults$stage) {
    gMax <- stageResults$getGMax()
    informationAtInterim <- design$informationAtInterim
    secondStageConditioning <- design$secondStageConditioning
    alpha <- design$alpha

    if (.isStageResultsMultiArmSurvival(stageResults)) {
        frac1 <- stageResults$.dataInput$allocationRatios[stageResults$.dataInput$stages == 1 &
            stageResults$.dataInput$groups <= gMax] /
            (stageResults$.dataInput$allocationRatios[stageResults$.dataInput$stages == 1 &
                stageResults$.dataInput$groups <= gMax] + 1)
        if (stage == 2) {
            frac2 <- stageResults$.dataInput$overallAllocationRatios[stageResults$.dataInput$stages == 2 &
                stageResults$.dataInput$groups <= gMax] /
                (stageResults$.dataInput$overallAllocationRatios[stageResults$.dataInput$stages == 2 &
                    stageResults$.dataInput$groups <= gMax] + 1)
        }
    } else {
        frac1 <- stageResults$.dataInput$sampleSizes[stageResults$.dataInput$stages == 1 &
            stageResults$.dataInput$groups <= gMax] /
            (stageResults$.dataInput$sampleSizes[stageResults$.dataInput$stages == 1 &
                stageResults$.dataInput$groups <= gMax] +
                stageResults$.dataInput$sampleSizes[stageResults$.dataInput$stages == 1 &
                    stageResults$.dataInput$groups == (gMax + 1)])
        if (stage == 2) {
            frac2 <- stageResults$.dataInput$sampleSizes[stageResults$.dataInput$stages == 2 &
                stageResults$.dataInput$groups <= gMax] /
                (stageResults$.dataInput$sampleSizes[stageResults$.dataInput$stages == 2 &
                    stageResults$.dataInput$groups <= gMax] +
                    stageResults$.dataInput$sampleSizes[stageResults$.dataInput$stages == 2 &
                        stageResults$.dataInput$groups == (gMax + 1)])
        }
    }

    indices <- .getIndicesOfClosedHypothesesSystem(gMax = gMax)

    conditionalErrorRate <- matrix(rep(NA_real_, 2 * (2^gMax - 1)), 2^gMax - 1, 2)
    secondStagePValues <- matrix(rep(NA_real_, 2 * (2^gMax - 1)), 2^gMax - 1, 2)
    rejected <- matrix(rep(FALSE, gMax * 2), gMax, 2)

    colnames(conditionalErrorRate) <- paste("stage ", (1:2), sep = "")
    colnames(secondStagePValues) <- paste("stage ", (1:2), sep = "")
    dimnames(rejected) <- list(paste("arm ", 1:gMax, sep = ""), paste("stage ", (1:2), sep = ""))
    rejectedIntersections <- matrix(rep(FALSE, stage * nrow(indices)), nrow(indices), stage)

    if (stageResults$directionUpper) {
        signedTestStatistics <- stageResults$testStatistics
        signedOverallTestStatistics <- stageResults$overallTestStatistics
        signedOverallTestStatistics[, 2] <- sqrt(informationAtInterim) *
            stageResults$testStatistics[, 1] + sqrt(1 - informationAtInterim) * stageResults$testStatistics[, 2]
    } else {
        signedTestStatistics <- -stageResults$testStatistics
        signedOverallTestStatistics <- -stageResults$overallTestStatistics
        signedOverallTestStatistics[, 2] <- -(sqrt(informationAtInterim) *
            stageResults$testStatistics[, 1] + sqrt(1 - informationAtInterim) * stageResults$testStatistics[, 2])
    }

    for (i in 1:(2^gMax - 1)) {
        tryCatch(
            {
                zeta <- sqrt(frac1[indices[i, ] == 1])
                sigma <- zeta %*% t(zeta)
                diag(sigma) <- 1
                crit <- .getMultivariateDistribution(
                    type = "quantile",
                    upper = NA_real_, sigma = sigma, alpha = alpha
                )

                integrandFunction <- function(x) {
                    innerProduct <- 1
                    for (g in (1:gMax)) {
                        if (indices[i, g] == 1) {
                            innerProduct <- innerProduct * stats::pnorm(((crit -
                                sqrt(informationAtInterim) * signedTestStatistics[g, 1] +
                                sqrt(1 - informationAtInterim) * sqrt(frac1[g]) * x)) /
                                sqrt((1 - informationAtInterim) * (1 - frac1[g])))
                        }
                    }
                    return(innerProduct * dnorm(x))
                }
                conditionalErrorRate[i, 1] <- 1 - stats::integrate(integrandFunction, lower = -Inf, upper = Inf)$value
            },
            error = function(e) {
                warning("Failed to calculate conditionalErrorRate[", i, ", 1]: ", e$message)
            }
        )

        tryCatch(
            {
                if (stage == 2) {
                    if (!all(is.na(stageResults$separatePValues[indices[i, ] == 1, 2]))) {
                        if (secondStageConditioning) {
                            maxOverallTestStatistic <- max(
                                signedOverallTestStatistics[indices[i, ] == 1, 2],
                                na.rm = TRUE
                            )
                            integrandFunctionStage2 <- function(x) {
                                innerProduct <- 1
                                for (g in (1:gMax)) {
                                    if ((indices[i, g] == 1) && !is.na(stageResults$overallTestStatistics[g, 2])) {
                                        innerProduct <- innerProduct * stats::pnorm(((maxOverallTestStatistic -
                                            sqrt(informationAtInterim) * signedTestStatistics[g, 1] +
                                            sqrt(1 - informationAtInterim) * sqrt(frac2[g]) * x)) /
                                            sqrt((1 - informationAtInterim) * (1 - frac2[g])))
                                    }
                                }
                                return(innerProduct * dnorm(x))
                            }
                        } else {
                            maxTestStatistic <- max(signedTestStatistics[indices[i, ] == 1, 2], na.rm = TRUE)
                            integrandFunctionStage2 <- function(x) {
                                innerProduct <- 1
                                for (g in (1:gMax)) {
                                    if ((indices[i, g] == 1) && !is.na(stageResults$separatePValues[g, 2])) {
                                        innerProduct <- innerProduct *
                                            stats::pnorm(((maxTestStatistic + sqrt(frac2[g]) * x)) / sqrt(1 - frac2[g]))
                                    }
                                }
                                return(innerProduct * dnorm(x))
                            }
                        }
                        secondStagePValues[i, 2] <- 1 - stats::integrate(integrandFunctionStage2, lower = -Inf, upper = Inf)$value
                    }
                }
            },
            error = function(e) {
                warning("Failed to calculate secondStagePValues[", i, ", 2]: ", e$message)
            }
        )
    }

    if (stage == 2) {
        rejectedIntersections[, 2] <- (secondStagePValues[, 2] <= conditionalErrorRate[, 1])
        rejectedIntersections[is.na(rejectedIntersections[, 2]), 2] <- FALSE
        for (j in 1:gMax) {
            rejected[j, 2] <- all(rejectedIntersections[indices[, j] == 1, 2], na.rm = TRUE)
        }
    }

    return(list(
        .design = design,
        intersectionTest = "Dunnett",
        indices = indices,
        separatePValues = stageResults$separatePValues,
        conditionalErrorRate = conditionalErrorRate,
        secondStagePValues = secondStagePValues,
        rejected = rejected,
        rejectedIntersections = rejectedIntersections
    ))
}


.getConditionalDunnettTestForCI <- function(..., design, stageResults, treatmentArm) {
    gMax <- stageResults$getGMax()
    informationAtInterim <- design$informationAtInterim
    secondStageConditioning <- design$secondStageConditioning
    alpha <- design$alpha

    if (.isStageResultsMultiArmSurvival(stageResults)) {
        frac1 <- stageResults$.dataInput$allocationRatios[stageResults$.dataInput$stages == 1 &
            stageResults$.dataInput$groups <= gMax] /
            (stageResults$.dataInput$allocationRatios[stageResults$.dataInput$stages == 1 &
                stageResults$.dataInput$groups <= gMax] + 1)
        frac2 <- stageResults$.dataInput$overallAllocationRatios[stageResults$.dataInput$stages == 2 &
            stageResults$.dataInput$groups <= gMax] /
            (stageResults$.dataInput$overallAllocationRatios[stageResults$.dataInput$stages == 2 &
                stageResults$.dataInput$groups <= gMax] + 1)
    } else {
        frac1 <- stageResults$.dataInput$sampleSizes[stageResults$.dataInput$stages == 1 &
            stageResults$.dataInput$groups <= gMax] /
            (stageResults$.dataInput$sampleSizes[stageResults$.dataInput$stages == 1 &
                stageResults$.dataInput$groups <= gMax] +
                stageResults$.dataInput$sampleSizes[stageResults$.dataInput$stages == 1 &
                    stageResults$.dataInput$groups == (gMax + 1)])
        frac2 <- stageResults$.dataInput$sampleSizes[stageResults$.dataInput$stages == 2 &
            stageResults$.dataInput$groups <= gMax] /
            (stageResults$.dataInput$sampleSizes[stageResults$.dataInput$stages == 2 &
                stageResults$.dataInput$groups <= gMax] +
                stageResults$.dataInput$sampleSizes[stageResults$.dataInput$stages == 2 &
                    stageResults$.dataInput$groups == (gMax + 1)])
    }

    if (stageResults$directionUpper) {
        signedTestStatistics <- stageResults$testStatistics
        signedOverallTestStatistics <- stageResults$overallTestStatistics
        signedOverallTestStatistics[, 2] <- sqrt(informationAtInterim) *
            stageResults$testStatistics[, 1] + sqrt(1 - informationAtInterim) * stageResults$testStatistics[, 2]
    } else {
        signedTestStatistics <- -stageResults$testStatistics
        signedOverallTestStatistics <- -stageResults$overallTestStatistics
        signedOverallTestStatistics[, 2] <- -(sqrt(informationAtInterim) *
            stageResults$testStatistics[, 1] + sqrt(1 - informationAtInterim) * stageResults$testStatistics[, 2])
    }

    zeta <- sqrt(frac1)
    sigma <- zeta %*% t(zeta)
    diag(sigma) <- 1
    crit <- .getMultivariateDistribution(type = "quantile", upper = NA_real_, sigma = sigma, alpha = alpha)

    integrand <- function(x) {
        innerProduct <- 1
        for (g in (1:gMax)) {
            innerProduct <- innerProduct * stats::pnorm(((crit -
                sqrt(informationAtInterim) * signedTestStatistics[g, 1] +
                sqrt(1 - informationAtInterim) * sqrt(frac1[g]) * x)) /
                sqrt((1 - informationAtInterim) * (1 - frac1[g])))
        }
        return(innerProduct * dnorm(x))
    }
    conditionalErrorRate <- 1 - integrate(integrand, lower = -Inf, upper = Inf)$value

    if (!is.na(stageResults$separatePValues[treatmentArm, 2])) {
        if (secondStageConditioning) {
            maxOverallTestStatistic <- signedOverallTestStatistics[treatmentArm, 2]
            integrand <- function(x) {
                innerProduct <- 1
                for (g in (1:gMax)) {
                    if (!is.na(stageResults$overallTestStatistics[g, 2])) {
                        innerProduct <- innerProduct *
                            stats::pnorm(((maxOverallTestStatistic -
                                sqrt(informationAtInterim) * signedTestStatistics[g, 1] +
                                sqrt(1 - informationAtInterim) * sqrt(frac2[g]) * x)) /
                                sqrt((1 - informationAtInterim) * (1 - frac2[g])))
                    }
                }
                return(innerProduct * dnorm(x))
            }
            secondStagePValues <- 1 - integrate(integrand, lower = -Inf, upper = Inf)$value
        } else {
            maxTestStatistic <- signedTestStatistics[treatmentArm, 2]
            integrand <- function(x) {
                innerProduct <- 1
                for (g in (1:gMax)) {
                    if (!is.na(stageResults$separatePValues[g, 2])) {
                        innerProduct <- innerProduct *
                            stats::pnorm(((maxTestStatistic + sqrt(frac2[g]) * x)) / sqrt(1 - frac2[g]))
                    }
                }
                return(innerProduct * dnorm(x))
            }
            secondStagePValues <- 1 - integrate(integrand, lower = -Inf, upper = Inf)$value
        }
    }

    return(secondStagePValues <= conditionalErrorRate)
}

#'
#' Calculation of conditional rejection probability (CRP)
#'
#' @noRd
#'
.getConditionalRejectionProbabilitiesMultiArm <- function(stageResults, ...,
        stage = stageResults$stage, iterations = C_ITERATIONS_DEFAULT, seed = NA_real_) {
    .assertIsValidStage(stage, stageResults$.design$kMax)
    gMax <- stageResults$getGMax()

    if (.isTrialDesignInverseNormal(stageResults$.design)) {
        return(.getConditionalRejectionProbabilitiesMultiArmInverseNormal(
            stageResults = stageResults, stage = stage, ...
        ))
    } else if (.isTrialDesignFisher(stageResults$.design)) {
        return(.getConditionalRejectionProbabilitiesMultiArmFisher(
            stageResults = stageResults, stage = stage, ...
        ))
    } else if (.isTrialDesignConditionalDunnett(stageResults$.design)) {
        return(.getConditionalRejectionProbabilitiesMultiArmConditionalDunnett(
            stageResults = stageResults, ...
        ))
    }

    stop(
        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
        "'design' must be an instance of TrialDesignInverseNormal, TrialDesignFisher, or TrialDesignDunnett"
    )
}

#'
#' Calculation of CRP based on inverse normal method
#'
#' @noRd
#'
.getConditionalRejectionProbabilitiesMultiArmInverseNormal <- function(..., stageResults, stage) {
    design <- stageResults$.design
    .assertIsTrialDesignInverseNormal(design)
    .warnInCaseOfUnknownArguments(
        functionName = ".getConditionalRejectionProbabilitiesMultiArmInverseNormal",
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
    criticalValues <- design$criticalValues

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
                    shiftedFutilityBounds, C_FUTILITY_BOUNDS_DEFAULT,
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
.getConditionalRejectionProbabilitiesMultiArmFisher <- function(..., stageResults, stage) {
    design <- stageResults$.design
    .assertIsTrialDesignFisher(design)
    .warnInCaseOfUnknownArguments(
        functionName = ".getConditionalRejectionProbabilitiesMultiArmFisher",
        ignore = c("stage", "design"), ...
    )
    kMax <- design$kMax
    if (kMax == 1) {
        return(as.matrix(NA_real_))
    }
    gMax <- stageResults$getGMax()
    criticalValues <- design$criticalValues
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
#' Calculation of CRP based on conditional Dunnett
#'
#' @noRd
#'
.getConditionalRejectionProbabilitiesMultiArmConditionalDunnett <- function(..., stageResults) {
    design <- stageResults$.design
    .assertIsTrialDesignConditionalDunnett(design)
    .warnInCaseOfUnknownArguments(
        functionName = ".getConditionalRejectionProbabilitiesMultiArmConditionalDunnett",
        ignore = c("stage", "intersectionTest", "design"), ...
    )

    kMax <- 2
    gMax <- stageResults$getGMax()
    conditionalRejectionProbabilities <- matrix(NA_real_, nrow = gMax, ncol = kMax)

    ctr <- getClosedConditionalDunnettTestResults(stageResults = stageResults, design = design)
    stage <- 1
    for (g in 1:gMax) {
        if (!is.na(ctr$separatePValues[g, stage])) {
            conditionalRejectionProbabilities[g, 2] <- 1 -
                stats::pnorm(.getOneMinusQNorm(min(ctr$conditionalErrorRate[
                    ctr$indices[, g] == 1,
                    stage
                ], na.rm = TRUE)))
        }
    }
    return(conditionalRejectionProbabilities)
}

#'
#' Plotting conditional power and likelihood
#'
#' @noRd
#'
.getConditionalPowerPlotMultiArm <- function(stageResults, ...,
        nPlanned, allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        thetaRange = NA_real_, assumedStDevs = NA_real_,
        piTreatmentRange = NA_real_, piControl = NA_real_,
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
    .assertIsInOpenInterval(allocationRatioPlanned, "allocationRatioPlanned", 0, C_ALLOCATION_RATIO_MAXIMUM)

    if (stageResults$isDatasetMeans()) {
        .warnInCaseOfUnusedArgument(piTreatmentRange, "piTreatmentRange", NA_real_, "plot")
        .warnInCaseOfUnusedArgument(piControl, "piControl", NA_real_, "plot")
        return(.getConditionalPowerLikelihoodMeansMultiArm(
            stageResults = stageResults, stage = stage,
            nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned,
            thetaRange = thetaRange, assumedStDevs = assumedStDevs, iterations = iterations, seed = seed
        ))
    } else if (stageResults$isDatasetRates()) {
        .warnInCaseOfUnusedArgument(thetaRange, "thetaRange", NA_real_, "plot")
        .warnInCaseOfUnusedArgument(assumedStDevs, "assumedStDevs", NA_real_, "plot")
        return(.getConditionalPowerLikelihoodRatesMultiArm(
            stageResults = stageResults, stage = stage,
            nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned,
            piTreatmentRange = piTreatmentRange, piControl = piControl,
            iterations = iterations, seed = seed
        ))
    } else if (stageResults$isDatasetSurvival()) {
        .warnInCaseOfUnusedArgument(piTreatmentRange, "piTreatmentRange", NA_real_, "plot")
        .warnInCaseOfUnusedArgument(piControl, "piControl", NA_real_, "plot")
        .warnInCaseOfUnusedArgument(assumedStDevs, "assumedStDevs", NA_real_, "plot")
        return(.getConditionalPowerLikelihoodSurvivalMultiArm(
            stageResults = stageResults, stage = stage,
            nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned,
            thetaRange = thetaRange, iterations = iterations, seed = seed
        ))
    }

    stop(
        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'dataInput' type '",
        .getClassName(stageResults$.dataInput), "' is not implemented yet"
    )
}
