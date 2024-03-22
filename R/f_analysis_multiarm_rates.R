## |
## |  *Analysis of rates in multi-arm designs with adaptive test*
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
## |  File version: $Revision: 7742 $
## |  Last changed: $Date: 2024-03-22 13:46:29 +0100 (Fr, 22 Mrz 2024) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_logger.R
NULL

#'
#' @title
#' Get Analysis Results Rates
#'
#' @description
#' Returns an analysis result object.
#'
#' @param design The trial design.
#'
#' @return Returns a \code{AnalysisResultsRates} object.
#'
#' @keywords internal
#'
#' @noRd
#'
.getAnalysisResultsRatesMultiArm <- function(..., design, dataInput) {
    if (.isTrialDesignInverseNormal(design)) {
        return(.getAnalysisResultsRatesInverseNormalMultiArm(
            design = design,
            dataInput = dataInput, ...
        ))
    }

    if (.isTrialDesignFisher(design)) {
        return(.getAnalysisResultsRatesFisherMultiArm(
            design = design,
            dataInput = dataInput, ...
        ))
    }

    if (.isTrialDesignConditionalDunnett(design)) {
        return(.getAnalysisResultsRatesConditionalDunnettMultiArm(
            design = design,
            dataInput = dataInput, ...
        ))
    }

    .stopWithWrongDesignMessage(design)
}

.getAnalysisResultsRatesInverseNormalMultiArm <- function(...,
        design, dataInput,
        intersectionTest = C_INTERSECTION_TEST_MULTIARMED_DEFAULT,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT,
        thetaH0 = C_THETA_H0_RATES_DEFAULT, piTreatments = NA_real_,
        piControl = NA_real_, nPlanned = NA_real_,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .assertIsTrialDesignInverseNormal(design)
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    .warnInCaseOfUnknownArguments(
        functionName = ".getAnalysisResultsRatesInverseNormalMultiArm",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), "stage"), ...
    )

    results <- AnalysisResultsMultiArmInverseNormal$new(design = design, dataInput = dataInput)

    results <- .getAnalysisResultsRatesMultiArmAll(
        results = results, design = design, dataInput = dataInput,
        intersectionTest = intersectionTest, stage = stage, directionUpper = directionUpper,
        normalApproximation = normalApproximation,
        thetaH0 = thetaH0, piTreatments = piTreatments, piControl = piControl, nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned,
        tolerance = tolerance
    )

    return(results)
}

.getAnalysisResultsRatesFisherMultiArm <- function(...,
        design, dataInput,
        intersectionTest = C_INTERSECTION_TEST_MULTIARMED_DEFAULT,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT,
        thetaH0 = C_THETA_H0_RATES_DEFAULT,
        piTreatments = NA_real_, piControl = NA_real_, nPlanned = NA_real_,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT,
        iterations = C_ITERATIONS_DEFAULT, seed = NA_real_) {
    .assertIsTrialDesignFisher(design)
    .assertIsValidIterationsAndSeed(iterations, seed, zeroIterationsAllowed = FALSE)
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    .warnInCaseOfUnknownArguments(
        functionName = ".getAnalysisResultsRatesFisherMultiArm",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), "stage"), ...
    )

    results <- AnalysisResultsMultiArmFisher$new(design = design, dataInput = dataInput)
    results <- .getAnalysisResultsRatesMultiArmAll(
        results = results, design = design, dataInput = dataInput,
        intersectionTest = intersectionTest, stage = stage, directionUpper = directionUpper,
        normalApproximation = normalApproximation,
        thetaH0 = thetaH0, piTreatments = piTreatments, piControl = piControl, nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned,
        tolerance = tolerance,
        iterations = iterations, seed = seed
    )

    return(results)
}

.getAnalysisResultsRatesConditionalDunnettMultiArm <- function(...,
        design, dataInput,
        intersectionTest = C_INTERSECTION_TEST_MULTIARMED_DEFAULT,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT,
        thetaH0 = C_THETA_H0_RATES_DEFAULT, piTreatments = NA_real_, piControl = NA_real_, nPlanned = NA_real_,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT,
        iterations = C_ITERATIONS_DEFAULT, seed = NA_real_) {
    .assertIsTrialDesignConditionalDunnett(design)
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    .warnInCaseOfUnknownArguments(
        functionName = ".getAnalysisResultsRatesConditionalDunnettMultiArm",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), "stage"), ...
    )

    results <- AnalysisResultsConditionalDunnett$new(design = design, dataInput = dataInput)

    results <- .getAnalysisResultsRatesMultiArmAll(
        results = results, design = design,
        dataInput = dataInput, intersectionTest = intersectionTest,
        stage = stage, directionUpper = directionUpper, normalApproximation = normalApproximation,
        thetaH0 = thetaH0, piTreatments = piTreatments, piControl = piControl, nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned,
        tolerance = tolerance,
        iterations = iterations, seed = seed
    )

    return(results)
}

.getAnalysisResultsRatesMultiArmAll <- function(..., results, design, dataInput, intersectionTest, stage,
        directionUpper, normalApproximation, thetaH0, piTreatments, piControl, nPlanned, allocationRatioPlanned,
        tolerance, iterations, seed) {
    startTime <- Sys.time()

    intersectionTest <- .getCorrectedIntersectionTestMultiArmIfNecessary(design, intersectionTest)

    stageResults <- .getStageResultsRatesMultiArm(
        design = design, dataInput = dataInput,
        intersectionTest = intersectionTest, stage = stage,
        thetaH0 = thetaH0, directionUpper = directionUpper,
        normalApproximation = normalApproximation
    )
    results$.setStageResults(stageResults)
    .logProgress("Stage results calculated", startTime = startTime)
    gMax <- stageResults$getGMax()

    piControl <- .assertIsValidPiControlForMultiArm(piControl, stageResults, stage, results = results)
    piTreatments <- .assertIsValidPiTreatmentsForMultiArm(piTreatments, stageResults, stage, results = results)

    .setValueAndParameterType(
        results, "intersectionTest",
        intersectionTest, C_INTERSECTION_TEST_MULTIARMED_DEFAULT
    )
    .setValueAndParameterType(results, "directionUpper", directionUpper, C_DIRECTION_UPPER_DEFAULT)
    .setValueAndParameterType(
        results, "normalApproximation",
        normalApproximation, C_NORMAL_APPROXIMATION_RATES_DEFAULT
    )
    .setValueAndParameterType(results, "thetaH0", thetaH0, C_THETA_H0_MEANS_DEFAULT)
    .setConditionalPowerArguments(results, dataInput, nPlanned, allocationRatioPlanned)
    .setNPlannedAndPi(results, nPlanned, "piControl", piControl, piTreatments)

    if (results$.getParameterType("piControl") %in% c(C_PARAM_TYPE_UNKNOWN, C_PARAM_NOT_APPLICABLE)) {
        .setValueAndParameterType(
            results, "piControl",
            matrix(piControl, ncol = 1), matrix(rep(NA_real_, gMax), ncol = 1)
        )
    } else {
        results$piControl <- matrix(piControl, ncol = 1)
    }
    if (results$.getParameterType("piTreatments") %in% c(C_PARAM_TYPE_UNKNOWN, C_PARAM_NOT_APPLICABLE)) {
        .setValueAndParameterType(
            results, "piTreatments",
            matrix(piTreatments, ncol = 1), matrix(rep(NA_real_, gMax), ncol = 1)
        )
    } else {
        results$piTreatments <- matrix(piTreatments, ncol = 1)
    }

    startTime <- Sys.time()
    if (!.isTrialDesignConditionalDunnett(design)) {
        results$.closedTestResults <- getClosedCombinationTestResults(stageResults = stageResults)
    } else {
        results$.closedTestResults <- getClosedConditionalDunnettTestResults(
            stageResults = stageResults, design = design, stage = stage
        )
    }
    .logProgress("Closed test calculated", startTime = startTime)

    if (design$kMax > 1) {
        # conditional power
        startTime <- Sys.time()
        if (.isTrialDesignFisher(design)) {
            results$.conditionalPowerResults <- .getConditionalPowerRatesMultiArm(
                stageResults = stageResults,
                stage = stage, nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned,
                piTreatments = piTreatments, piControl = piControl, iterations = iterations, seed = seed
            )
            .synchronizeIterationsAndSeed(results)
        } else {
            results$.conditionalPowerResults <- .getConditionalPowerRatesMultiArm(
                stageResults = stageResults,
                stage = stage, nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned,
                piTreatments = piTreatments, piControl = piControl
            )
            results$conditionalPower <- results$.conditionalPowerResults$conditionalPower
            results$.setParameterType("conditionalPower", C_PARAM_GENERATED)
        }
        .logProgress("Conditional power calculated", startTime = startTime)

        # CRP - conditional rejection probabilities
        startTime <- Sys.time()
        results$conditionalRejectionProbabilities <- .getConditionalRejectionProbabilitiesMultiArm(
            stageResults = stageResults, stage = stage
        )
        results$.setParameterType("conditionalRejectionProbabilities", C_PARAM_GENERATED)
        .logProgress("Conditional rejection probabilities (CRP) calculated", startTime = startTime)
    } else {
        results$.setParameterType("conditionalPower", C_PARAM_NOT_APPLICABLE)
        results$.setParameterType("conditionalPowerSimulated", C_PARAM_NOT_APPLICABLE)
        results$.setParameterType("conditionalRejectionProbabilities", C_PARAM_NOT_APPLICABLE)
    }

    # RCI - repeated confidence interval
    repeatedConfidenceIntervals <- .getRepeatedConfidenceIntervalsRatesMultiArm(
        design = design, dataInput = dataInput,
        intersectionTest = intersectionTest, stage = stage,
        normalApproximation = normalApproximation, tolerance = tolerance
    )
    results$repeatedConfidenceIntervalLowerBounds <-
        matrix(rep(NA_real_, gMax * design$kMax), nrow = gMax, ncol = design$kMax)
    results$repeatedConfidenceIntervalUpperBounds <- results$repeatedConfidenceIntervalLowerBounds
    for (k in 1:design$kMax) {
        for (treatmentArm in 1:gMax) {
            results$repeatedConfidenceIntervalLowerBounds[treatmentArm, k] <-
                repeatedConfidenceIntervals[treatmentArm, 1, k]
            results$repeatedConfidenceIntervalUpperBounds[treatmentArm, k] <-
                repeatedConfidenceIntervals[treatmentArm, 2, k]
        }
    }
    results$.setParameterType("repeatedConfidenceIntervalLowerBounds", C_PARAM_GENERATED)
    results$.setParameterType("repeatedConfidenceIntervalUpperBounds", C_PARAM_GENERATED)

    # repeated p-value
    results$repeatedPValues <- .getRepeatedPValuesMultiArm(stageResults = stageResults, tolerance = tolerance)
    results$.setParameterType("repeatedPValues", C_PARAM_GENERATED)

    return(results)
}

.getStageResultsRatesMultiArm <- function(..., design, dataInput,
        thetaH0 = C_THETA_H0_RATES_DEFAULT,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT,
        intersectionTest = C_INTERSECTION_TEST_MULTIARMED_DEFAULT,
        calculateSingleStepAdjusted = FALSE,
        userFunctionCallEnabled = FALSE) {
    .assertIsTrialDesign(design)
    .assertIsDatasetRates(dataInput)
    .assertIsValidThetaH0DataInput(thetaH0, dataInput)
    .assertIsValidDirectionUpper(directionUpper, design$sided)
    .assertIsSingleLogical(normalApproximation, "normalApproximation")
    .assertIsSingleLogical(calculateSingleStepAdjusted, "calculateSingleStepAdjusted")
    .warnInCaseOfUnknownArguments(
        functionName = ".getStageResultsRatesMultiArm",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design, powerCalculationEnabled = TRUE), "stage"), ...
    )

    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    gMax <- dataInput$getNumberOfGroups() - 1
    kMax <- design$kMax

    if (.isTrialDesignConditionalDunnett(design)) {
        if (!normalApproximation) {
            if (userFunctionCallEnabled) {
                warning("'normalApproximation' was set to TRUE ",
                    "because conditional Dunnett test was specified as design",
                    call. = FALSE
                )
            }
            normalApproximation <- TRUE
        }
    }
    intersectionTest <- .getCorrectedIntersectionTestMultiArmIfNecessary(
        design, intersectionTest, userFunctionCallEnabled
    )
    .assertIsValidIntersectionTestMultiArm(design, intersectionTest)

    if (intersectionTest == "Dunnett" && !normalApproximation) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "Dunnett test cannot be used with Fisher's exact test (normalApproximation = FALSE)",
            call. = FALSE
        )
    }

    stageResults <- StageResultsMultiArmRates$new(
        design = design,
        dataInput = dataInput,
        intersectionTest = intersectionTest,
        thetaH0 = thetaH0,
        direction = ifelse(directionUpper, C_DIRECTION_UPPER, C_DIRECTION_LOWER),
        normalApproximation = normalApproximation,
        directionUpper = directionUpper,
        stage = stage
    )

    piControl <- matrix(rep(NA_real_, kMax), 1, kMax)
    piTreatments <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    testStatistics <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    overallTestStatistics <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    separatePValues <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    overallPValues <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    dimnames(testStatistics) <- list(paste("arm ", 1:gMax, sep = ""), paste("stage ", (1:kMax), sep = ""))
    dimnames(overallTestStatistics) <- list(
        paste("arm ", 1:gMax, sep = ""),
        paste("stage ", (1:kMax), sep = "")
    )
    dimnames(separatePValues) <- list(paste("arm ", 1:gMax, sep = ""), paste("stage ", (1:kMax), sep = ""))
    dimnames(overallPValues) <- list(paste("arm ", 1:gMax, sep = ""), paste("stage ", (1:kMax), sep = ""))

    for (k in 1:stage) {
        piControl[1, k] <- dataInput$getOverallEvents(stage = k, group = gMax + 1) /
            dataInput$getOverallSampleSizes(stage = k, group = gMax + 1)

        for (treatmentArm in 1:gMax) {
            piTreatments[treatmentArm, k] <- dataInput$getOverallEvents(stage = k, group = treatmentArm) /
                dataInput$getOverallSampleSizes(stage = k, group = treatmentArm)

            actEv <- dataInput$getEvents(stage = k, group = treatmentArm)
            ctrEv <- dataInput$getEvents(stage = k, group = gMax + 1)
            actN <- dataInput$getSampleSize(stage = k, group = treatmentArm)
            ctrN <- dataInput$getSampleSize(stage = k, group = gMax + 1)

            if (normalApproximation) {
                if (thetaH0 == 0) {
                    if (!is.na(actEv)) {
                        if ((actEv + ctrEv == 0) ||
                                (actEv + ctrEv == actN + ctrN)) {
                            testStatistics[treatmentArm, k] <- 0
                        } else {
                            rateH0 <- (actEv + ctrEv) / (actN + ctrN)
                            testStatistics[treatmentArm, k] <- (actEv / actN - ctrEv / ctrN - thetaH0) /
                                sqrt(rateH0 * (1 - rateH0) * (1 / actN + 1 / ctrN))
                        }
                    }
                } else {
                    y <- .getFarringtonManningValues(
                        rate1 = actEv / actN,
                        rate2 = ctrEv / ctrN, theta = thetaH0, allocation = actN / ctrN, method = "diff"
                    )

                    testStatistics[treatmentArm, k] <-
                        (actEv / actN - ctrEv / ctrN - thetaH0) /
                            sqrt(y$ml1 * (1 - y$ml1) / actN + y$ml2 * (1 - y$ml2) / ctrN)
                }

                if (directionUpper) {
                    separatePValues[treatmentArm, k] <- 1 - stats::pnorm(testStatistics[treatmentArm, k])
                } else {
                    separatePValues[treatmentArm, k] <- stats::pnorm(testStatistics[treatmentArm, k])
                }
            } else {
                if (thetaH0 != 0) {
                    stop(
                        C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                        "'thetaH0' (", thetaH0, ") must be 0 to perform Fisher's exact test"
                    )
                }

                if (directionUpper) {
                    separatePValues[treatmentArm, k] <- stats::phyper(actEv - 1,
                        actEv + ctrEv,
                        actN + ctrN - actEv - ctrEv,
                        actN,
                        lower.tail = FALSE
                    )
                } else {
                    separatePValues[treatmentArm, k] <- stats::phyper(actEv,
                        actEv + ctrEv,
                        actN + ctrN - actEv - ctrEv,
                        actN,
                        lower.tail = TRUE
                    )
                }
                if (directionUpper) {
                    testStatistics <- .getOneMinusQNorm(separatePValues)
                } else {
                    testStatistics <- -.getOneMinusQNorm(separatePValues)
                }
            }

            # overall test statistics
            actEv <- dataInput$getOverallEvents(stage = k, group = treatmentArm)
            ctrEv <- dataInput$getOverallEvents(stage = k, group = gMax + 1)
            actN <- dataInput$getOverallSampleSize(stage = k, group = treatmentArm)
            ctrN <- dataInput$getOverallSampleSize(stage = k, group = gMax + 1)

            if (normalApproximation) {
                if (thetaH0 == 0) {
                    if (!is.na(actEv)) {
                        if ((actEv + ctrEv == 0) ||
                                (actEv + ctrEv == actN + ctrN)) {
                            overallTestStatistics[treatmentArm, k] <- 0
                        } else {
                            overallRateH0 <- (actEv + ctrEv) / (actN + ctrN)
                            overallTestStatistics[treatmentArm, k] <- (actEv / actN - ctrEv / ctrN - thetaH0) /
                                sqrt(overallRateH0 * (1 - overallRateH0) * (1 / actN + 1 / ctrN))
                        }
                    }
                } else {
                    y <- .getFarringtonManningValues(
                        rate1 = actEv / actN, rate2 = ctrEv / ctrN,
                        theta = thetaH0, allocation = actN / ctrN, method = "diff"
                    )

                    overallTestStatistics[treatmentArm, k] <-
                        (actEv / actN - ctrEv / ctrN - thetaH0) /
                            sqrt(y$ml1 * (1 - y$ml1) / actN + y$ml2 * (1 - y$ml2) / ctrN)
                }

                if (directionUpper) {
                    overallPValues[treatmentArm, k] <- 1 - stats::pnorm(overallTestStatistics[treatmentArm, k])
                } else {
                    overallPValues[treatmentArm, k] <- stats::pnorm(overallTestStatistics[treatmentArm, k])
                }
            } else {
                if (thetaH0 != 0) {
                    stop(
                        C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                        "'thetaH0' (", thetaH0, ") must be 0 to perform Fisher's exact test"
                    )
                }

                if (directionUpper) {
                    overallPValues[treatmentArm, k] <- stats::phyper(actEv - 1,
                        actEv + ctrEv,
                        actN + ctrN - actEv - ctrEv,
                        actN,
                        lower.tail = FALSE
                    )
                } else {
                    overallPValues[treatmentArm, k] <- stats::phyper(actEv,
                        actEv + ctrEv,
                        actN + ctrN - actEv - ctrEv,
                        actN,
                        lower.tail = TRUE
                    )
                }

                if (directionUpper) {
                    overallTestStatistics <- .getOneMinusQNorm(overallPValues)
                } else {
                    overallTestStatistics <- -.getOneMinusQNorm(overallPValues)
                }
            }
        }
    }

    stageResults$overallPiControl <- piControl
    stageResults$overallPiTreatments <- piTreatments
    stageResults$overallTestStatistics <- overallTestStatistics
    stageResults$overallPValues <- overallPValues
    stageResults$testStatistics <- testStatistics
    stageResults$separatePValues <- separatePValues

    effectSizes <- matrix(numeric(0), ncol = ncol(piControl))
    for (treatmentArm in 1:gMax) {
        effectSizes <- rbind(effectSizes, piTreatments[treatmentArm, ] - piControl)
    }
    stageResults$effectSizes <- effectSizes
    stageResults$.setParameterType("effectSizes", C_PARAM_GENERATED)

    .setWeightsToStageResults(design, stageResults)

    if (!calculateSingleStepAdjusted) {
        return(stageResults)
    }

    # Calculation of single stage adjusted p-Values and overall test statistics
    # for determination of RCIs
    singleStepAdjustedPValues <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    combInverseNormal <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    combFisher <- matrix(NA_real_, nrow = gMax, ncol = kMax)

    if (.isTrialDesignInverseNormal(design)) {
        weightsInverseNormal <- stageResults$weightsInverseNormal
    } else if (.isTrialDesignFisher(design)) {
        weightsFisher <- stageResults$weightsFisher
    }

    for (k in 1:stage) {
        selected <- sum(!is.na(separatePValues[, k]))
        sampleSizesSelected <- as.numeric(na.omit(
            dataInput$getSampleSizes(stage = k, group = -(gMax + 1))
        ))
        sigma <- sqrt(sampleSizesSelected /
            (sampleSizesSelected + dataInput$getSampleSize(k, gMax + 1))) %*%
            sqrt(t(sampleSizesSelected / (sampleSizesSelected +
                dataInput$getSampleSize(k, gMax + 1))))
        diag(sigma) <- 1
        for (treatmentArm in 1:gMax) {
            if ((intersectionTest == "Bonferroni") || (intersectionTest == "Simes")) {
                if (.isTrialDesignGroupSequential(design)) {
                    overallPValues[treatmentArm, k] <- min(1, overallPValues[treatmentArm, k] * selected)
                } else {
                    singleStepAdjustedPValues[treatmentArm, k] <- min(1, separatePValues[treatmentArm, k] * selected)
                }
            } else if (intersectionTest == "Sidak") {
                if (.isTrialDesignGroupSequential(design)) {
                    overallPValues[treatmentArm, k] <- 1 - (1 - overallPValues[treatmentArm, k])^selected
                } else {
                    singleStepAdjustedPValues[treatmentArm, k] <- 1 - (1 - separatePValues[treatmentArm, k])^selected
                }
            } else if (intersectionTest == "Dunnett") {
                if (!is.na(testStatistics[treatmentArm, k])) {
                    df <- NA_real_
                    singleStepAdjustedPValues[treatmentArm, k] <- 1 - .getMultivariateDistribution(
                        type = "normal",
                        upper = ifelse(directionUpper, testStatistics[treatmentArm, k], -testStatistics[treatmentArm, k]),
                        sigma = sigma, df = df
                    )
                }
            }
            if (.isTrialDesignInverseNormal(design)) {
                combInverseNormal[treatmentArm, k] <- (weightsInverseNormal[1:k] %*%
                    .getOneMinusQNorm(singleStepAdjustedPValues[treatmentArm, 1:k])) /
                    sqrt(sum(weightsInverseNormal[1:k]^2))
            } else if (.isTrialDesignFisher(design)) {
                combFisher[treatmentArm, k] <- prod(singleStepAdjustedPValues[treatmentArm, 1:k]^weightsFisher[1:k])
            }
        }
    }

    stageResults$singleStepAdjustedPValues <- singleStepAdjustedPValues
    stageResults$.setParameterType("singleStepAdjustedPValues", C_PARAM_GENERATED)

    if (.isTrialDesignFisher(design)) {
        stageResults$combFisher <- combFisher
        stageResults$.setParameterType("combFisher", C_PARAM_GENERATED)
    } else if (.isTrialDesignInverseNormal(design)) {
        stageResults$combInverseNormal <- combInverseNormal
        stageResults$.setParameterType("combInverseNormal", C_PARAM_GENERATED)
    }

    return(stageResults)
}


.getRootThetaRatesMultiArm <- function(..., design, dataInput, treatmentArm, stage,
        directionUpper, normalApproximation, intersectionTest,
        thetaLow, thetaUp, firstParameterName, secondValue, tolerance) {
    result <- .getOneDimensionalRoot(
        function(theta) {
            stageResults <- .getStageResultsRatesMultiArm(
                design = design, dataInput = dataInput,
                stage = stage, thetaH0 = theta, directionUpper = directionUpper,
                intersectionTest = intersectionTest, normalApproximation = normalApproximation,
                calculateSingleStepAdjusted = TRUE
            )
            firstValue <- stageResults[[firstParameterName]][treatmentArm, stage]
            if (.isTrialDesignGroupSequential(design)) {
                firstValue <- .getOneMinusQNorm(firstValue)
            }
            return(firstValue - secondValue)
        },
        lower = thetaLow, upper = thetaUp, tolerance = tolerance,
        callingFunctionInformation = ".getRootThetaRatesMultiArm"
    )
    return(result)
}

.getRepeatedConfidenceIntervalsRatesMultiArmAll <- function(...,
        design, dataInput,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT,
        intersectionTest = C_INTERSECTION_TEST_MULTIARMED_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT,
        firstParameterName) {
    .assertIsValidIntersectionTestMultiArm(design, intersectionTest)
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)

    stageResults <- .getStageResultsRatesMultiArm(
        design = design, dataInput = dataInput,
        stage = stage, thetaH0 = 0, directionUpper = directionUpper,
        intersectionTest = intersectionTest, normalApproximation = normalApproximation,
        calculateSingleStepAdjusted = FALSE
    )

    gMax <- dataInput$getNumberOfGroups() - 1
    repeatedConfidenceIntervals <- array(NA_real_, dim = c(gMax, 2, design$kMax))

    # Confidence interval for second stage when using conditional Dunnett test
    if (.isTrialDesignConditionalDunnett(design)) {
        startTime <- Sys.time()
        for (treatmentArm in 1:gMax) {
            if (!is.na(stageResults$testStatistics[treatmentArm, 2])) {
                thetaLow <- -1
                thetaUp <- 1

                iteration <- 50
                prec <- 1
                while (prec > tolerance) {
                    theta <- (thetaLow + thetaUp) / 2
                    stageResults <- .getStageResultsRatesMultiArm(
                        design = design, dataInput = dataInput,
                        stage = stage, thetaH0 = theta, directionUpper = TRUE,
                        intersectionTest = intersectionTest, normalApproximation = TRUE,
                        calculateSingleStepAdjusted = FALSE
                    )
                    conditionalDunnettSingleStepRejected <- .getConditionalDunnettTestForCI(
                        design = design, stageResults = stageResults, treatmentArm = treatmentArm
                    )
                    ifelse(conditionalDunnettSingleStepRejected, thetaLow <- theta, thetaUp <- theta)
                    ifelse(iteration > 0, prec <- thetaUp - thetaLow, prec <- 0)
                    iteration <- iteration - 1
                }
                repeatedConfidenceIntervals[treatmentArm, 1, 2] <- theta

                thetaLow <- -1
                thetaUp <- 1

                iteration <- 50
                prec <- 1
                while (prec > tolerance) {
                    theta <- (thetaLow + thetaUp) / 2
                    stageResults <- .getStageResultsRatesMultiArm(
                        design = design, dataInput = dataInput,
                        stage = stage, thetaH0 = theta, directionUpper = FALSE,
                        intersectionTest = intersectionTest, normalApproximation = TRUE,
                        calculateSingleStepAdjusted = FALSE
                    )
                    conditionalDunnettSingleStepRejected <- .getConditionalDunnettTestForCI(
                        design = design, stageResults = stageResults, treatmentArm = treatmentArm
                    )
                    ifelse(conditionalDunnettSingleStepRejected, thetaUp <- theta, thetaLow <- theta)
                    ifelse(iteration > 0, prec <- thetaUp - thetaLow, prec <- 0)
                    iteration <- iteration - 1
                }
                repeatedConfidenceIntervals[treatmentArm, 2, 2] <- theta

                if (!is.na(repeatedConfidenceIntervals[treatmentArm, 1, 2]) &&
                        !is.na(repeatedConfidenceIntervals[treatmentArm, 2, 2]) &&
                        repeatedConfidenceIntervals[treatmentArm, 1, 2] > repeatedConfidenceIntervals[treatmentArm, 2, 2]) {
                    repeatedConfidenceIntervals[treatmentArm, , 2] <- rep(NA_real_, 2)
                }
            }
        }
        .logProgress("Confidence intervals for final stage calculated", startTime = startTime)
    } else {
        # Repeated onfidence intervals when using combination tests

        if (intersectionTest == "Hierarchical") {
            warning("Repeated confidence intervals not available for ",
                "'intersectionTest' = \"Hierarchical\"",
                call. = FALSE
            )
            return(repeatedConfidenceIntervals)
        }

        if (.isTrialDesignFisher(design)) {
            bounds <- design$alpha0Vec
            border <- C_ALPHA_0_VEC_DEFAULT
            criticalValues <- design$criticalValues
            conditionFunction <- .isFirstValueSmallerThanSecondValue
        } else if (.isTrialDesignInverseNormal(design)) {
            bounds <- design$futilityBounds
            border <- C_FUTILITY_BOUNDS_DEFAULT
            criticalValues <- design$criticalValues
            criticalValues[is.infinite(criticalValues) & criticalValues > 0] <- C_QNORM_MAXIMUM
            criticalValues[is.infinite(criticalValues) & criticalValues < 0] <- C_QNORM_MINIMUM
            conditionFunction <- .isFirstValueGreaterThanSecondValue
        }

        # necessary for adjustment for binding futility boundaries
        futilityCorr <- rep(NA_real_, design$kMax)

        stages <- (1:stage)
        for (k in stages) {
            startTime <- Sys.time()
            for (treatmentArm in 1:gMax) {
                if (!is.na(stageResults$testStatistics[treatmentArm, k]) && criticalValues[k] < C_QNORM_MAXIMUM) {
                    thetaLow <- -1 + tolerance
                    thetaUp <- 1 - tolerance
                    # finding upper and lower RCI limits through root function
                    repeatedConfidenceIntervals[treatmentArm, 1, k] <- .getRootThetaRatesMultiArm(
                        design = design,
                        dataInput = dataInput, treatmentArm = treatmentArm, stage = k, directionUpper = TRUE,
                        normalApproximation = normalApproximation,
                        thetaLow = thetaLow, thetaUp = thetaUp,
                        intersectionTest = intersectionTest, firstParameterName = firstParameterName,
                        secondValue = criticalValues[k], tolerance = tolerance
                    )

                    repeatedConfidenceIntervals[treatmentArm, 2, k] <- .getRootThetaRatesMultiArm(
                        design = design,
                        dataInput = dataInput, treatmentArm = treatmentArm, stage = k, directionUpper = FALSE,
                        normalApproximation = normalApproximation,
                        thetaLow = thetaLow, thetaUp = thetaUp,
                        intersectionTest = intersectionTest, firstParameterName = firstParameterName,
                        secondValue = criticalValues[k], tolerance = tolerance
                    )

                    # adjustment for binding futility bounds
                    if (k > 1 && !is.na(bounds[k - 1]) && conditionFunction(bounds[k - 1], border) && design$bindingFutility) {
                        parameterName <- ifelse(.isTrialDesignFisher(design),
                            "singleStepAdjustedPValues", firstParameterName
                        )

                        futilityCorr[k] <- .getRootThetaRatesMultiArm(
                            design = design, dataInput = dataInput,
                            treatmentArm = treatmentArm, stage = k - 1, directionUpper = directionUpper,
                            normalApproximation = normalApproximation,
                            thetaLow = thetaLow, thetaUp = thetaUp,
                            intersectionTest = intersectionTest, firstParameterName = parameterName,
                            secondValue = bounds[k - 1], tolerance = tolerance
                        )

                        if (directionUpper) {
                            repeatedConfidenceIntervals[treatmentArm, 1, k] <- min(
                                min(futilityCorr[2:k]),
                                repeatedConfidenceIntervals[treatmentArm, 1, k]
                            )
                        } else {
                            repeatedConfidenceIntervals[treatmentArm, 2, k] <- max(
                                max(futilityCorr[2:k]),
                                repeatedConfidenceIntervals[treatmentArm, 2, k]
                            )
                        }
                    }

                    if (!is.na(repeatedConfidenceIntervals[treatmentArm, 1, k]) &&
                            !is.na(repeatedConfidenceIntervals[treatmentArm, 2, k]) &&
                            repeatedConfidenceIntervals[treatmentArm, 1, k] > repeatedConfidenceIntervals[treatmentArm, 2, k]) {
                        repeatedConfidenceIntervals[treatmentArm, , k] <- rep(NA_real_, 2)
                    }
                }
            }
            .logProgress("Repeated confidence intervals for stage %s calculated", startTime = startTime, k)
        }
    }

    return(repeatedConfidenceIntervals)
}

#'
#' RCIs based on inverse normal combination test
#'
#' @noRd
#'
.getRepeatedConfidenceIntervalsRatesMultiArmInverseNormal <- function(...,
        design, dataInput,
        normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        intersectionTest = C_INTERSECTION_TEST_MULTIARMED_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    if (!normalApproximation) {
        message("Repeated confidence intervals will be calculated under the normal approximation")
        normalApproximation <- TRUE
    }

    .warnInCaseOfUnknownArguments(
        functionName =
            ".getRepeatedConfidenceIntervalsRatesMultiArmInverseNormal",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design, powerCalculationEnabled = TRUE), "stage"), ...
    )

    return(.getRepeatedConfidenceIntervalsRatesMultiArmAll(
        design = design, dataInput = dataInput,
        normalApproximation = normalApproximation,
        directionUpper = directionUpper, intersectionTest = intersectionTest,
        tolerance = tolerance, firstParameterName = "combInverseNormal", ...
    ))
}

#'
#' RCIs based on Fisher's combination test
#'
#' @noRd
#'
.getRepeatedConfidenceIntervalsRatesMultiArmFisher <- function(...,
        design, dataInput,
        normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        intersectionTest = C_INTERSECTION_TEST_MULTIARMED_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    if (!normalApproximation) {
        message("Repeated confidence intervals will be calculated under the normal approximation")
        normalApproximation <- TRUE
    }

    .warnInCaseOfUnknownArguments(
        functionName =
            ".getRepeatedConfidenceIntervalsRatesMultiArmFisher",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), "stage"), ...
    )

    return(.getRepeatedConfidenceIntervalsRatesMultiArmAll(
        design = design, dataInput = dataInput,
        normalApproximation = normalApproximation,
        directionUpper = directionUpper, intersectionTest = intersectionTest,
        tolerance = tolerance, firstParameterName = "combFisher", ...
    ))
}

#'
#' CIs based on conditional Dunnett test
#'
#' @noRd
#'
.getRepeatedConfidenceIntervalsRatesMultiArmConditionalDunnett <- function(...,
        design, dataInput,
        normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        intersectionTest = C_INTERSECTION_TEST_MULTIARMED_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .warnInCaseOfUnknownArguments(
        functionName =
            ".getRepeatedConfidenceIntervalsRatesMultiArmConditionalDunnett",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design, powerCalculationEnabled = TRUE), "stage"), ...
    )

    return(.getRepeatedConfidenceIntervalsRatesMultiArmAll(
        design = design, dataInput = dataInput,
        normalApproximation = normalApproximation,
        directionUpper = directionUpper, intersectionTest = intersectionTest,
        tolerance = tolerance, firstParameterName = "condDunnett", ...
    ))
}

#'
#' Calculation of repeated confidence intervals (RCIs) for Rates
#'
#' @noRd
#'
.getRepeatedConfidenceIntervalsRatesMultiArm <- function(..., design) {
    if (.isTrialDesignInverseNormal(design)) {
        return(.getRepeatedConfidenceIntervalsRatesMultiArmInverseNormal(design = design, ...))
    }
    if (.isTrialDesignFisher(design)) {
        return(.getRepeatedConfidenceIntervalsRatesMultiArmFisher(design = design, ...))
    }
    if (.isTrialDesignConditionalDunnett(design)) {
        return(.getRepeatedConfidenceIntervalsRatesMultiArmConditionalDunnett(design = design, ...))
    }
    .stopWithWrongDesignMessage(design)
}

#'
#' Calculation of conditional power for Rates
#'
#' @noRd
#'
.getConditionalPowerRatesMultiArm <- function(..., stageResults, stage = stageResults$stage,
        nPlanned, allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        piTreatments = NA_real_, piControl = NA_real_, useAdjustment = TRUE,
        iterations = C_ITERATIONS_DEFAULT, seed = NA_real_) {
    design <- stageResults$.design
    gMax <- stageResults$getGMax()

    if (.isTrialDesignConditionalDunnett(design)) {
        kMax <- 2
    } else {
        kMax <- design$kMax
    }

    piTreatmentsH1 <- .getOptionalArgument("piTreatmentsH1", ...)
    if (!is.null(piTreatmentsH1) && !is.na(piTreatmentsH1)) {
        if (!is.na(piTreatments)) {
            warning(sQuote("piTreatments"), " will be ignored because ",
                sQuote("piTreatmentsH1"), " is defined",
                call. = FALSE
            )
        }
        piTreatments <- piTreatmentsH1
    }

    piControlH1 <- .getOptionalArgument("piControlH1", ...)
    if (!is.null(piControlH1) && !is.na(piControlH1)) {
        if (!is.na(piControl)) {
            warning(sQuote("piControl"), " will be ignored because ",
                sQuote("piControlH1"), " is defined",
                call. = FALSE
            )
        }
        piControl <- piControlH1
    }

    results <- ConditionalPowerResultsMultiArmRates$new(
        .design = design,
        .stageResults = stageResults,
        piControl = piControl,
        piTreatments = piTreatments,
        nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned
    )

    if (any(is.na(nPlanned))) {
        return(results)
    }

    .assertIsValidStage(stage, kMax)
    if (stage == kMax) {
        .logDebug(
            "Conditional power will be calculated only for subsequent stages ",
            "(stage = ", stage, ", kMax = ", kMax, ")"
        )
        return(results)
    }

    if (!.isValidNPlanned(nPlanned = nPlanned, kMax = kMax, stage = stage)) {
        return(results)
    }

    .assertIsValidNPlanned(nPlanned, kMax, stage)
    .assertIsSingleNumber(allocationRatioPlanned, "allocationRatioPlanned")
    .assertIsInOpenInterval(allocationRatioPlanned, "allocationRatioPlanned", 0, C_ALLOCATION_RATIO_MAXIMUM)
    .setValueAndParameterType(results, "allocationRatioPlanned", allocationRatioPlanned, C_ALLOCATION_RATIO_DEFAULT)
    piControl <- .assertIsValidPiControlForMultiArm(piControl, stageResults, stage, results = results)
    piTreatments <- .assertIsValidPiTreatmentsForMultiArm(piTreatments, stageResults, stage, results = results)
    results$.setParameterType("nPlanned", C_PARAM_USER_DEFINED)

    if ((length(piTreatments) != 1) && (length(piTreatments) != gMax)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            sprintf(paste0(
                "length of 'piTreatments' (%s) ",
                "must be equal to 'gMax' (%s) or 1"
            ), .arrayToString(piTreatments), gMax)
        )
    }

    if (length(piControl) != 1) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            sprintf(paste0("length of 'piControl' (%s) must be equal to 1"), .arrayToString(piControl))
        )
    }

    if (.isTrialDesignInverseNormal(design)) {
        return(.getConditionalPowerRatesMultiArmInverseNormal(
            results = results,
            design = design, stageResults = stageResults, stage = stage,
            nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned,
            piControl = piControl,
            piTreatments = piTreatments, ...
        ))
    } else if (.isTrialDesignFisher(design)) {
        return(.getConditionalPowerRatesMultiArmFisher(
            results = results,
            design = design, stageResults = stageResults, stage = stage,
            nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned,
            useAdjustment = useAdjustment,
            piControl = piControl,
            piTreatments = piTreatments,
            iterations = iterations, seed = seed, ...
        ))
    } else if (.isTrialDesignConditionalDunnett(design)) {
        return(.getConditionalPowerRatesMultiArmConditionalDunnett(
            results = results,
            design = design, stageResults = stageResults, stage = stage,
            nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned,
            piControl = piControl,
            piTreatments = piTreatments, ...
        ))
    }

    stop(
        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
        "'design' must be an instance of TrialDesignInverseNormal, TrialDesignFisher, ",
        "or TrialDesignConditionalDunnett"
    )
}

#'
#' Calculation of conditional power based on inverse normal method
#'
#' @noRd
#'
.getConditionalPowerRatesMultiArmInverseNormal <- function(..., results, design, stageResults, stage,
        allocationRatioPlanned, nPlanned, piTreatments, piControl) {
    .assertIsTrialDesignInverseNormal(design)
    .warnInCaseOfUnknownArguments(
        functionName = ".getConditionalPowerRatesMultiArmInverseNormal",
        ignore = c("piTreatmentsH1", "piControlH1"), ...
    )

    kMax <- design$kMax
    gMax <- stageResults$getGMax()
    weights <- .getWeightsInverseNormal(design)
    informationRates <- design$informationRates

    nPlanned <- c(rep(NA_real_, stage), nPlanned)

    condError <- .getConditionalRejectionProbabilitiesMultiArm(design = design, stageResults = stageResults)[, stage]
    ml <- (allocationRatioPlanned * piTreatments + piControl) / (1 + allocationRatioPlanned)
    adjustment <- .getOneMinusQNorm(condError) * (1 - sqrt(ml * (1 - ml) * (1 + allocationRatioPlanned)) /
        sqrt(piTreatments * (1 - piTreatments) + allocationRatioPlanned * piControl * (1 - piControl))) *
        (1 + allocationRatioPlanned) / sqrt(allocationRatioPlanned * sum(nPlanned[(stage + 1):kMax]))
    adjustment[condError < 1e-12] <- 0

    results$.setParameterType("piControl", C_PARAM_DEFAULT_VALUE)
    if (length(piTreatments) == 1) {
        piTreatments <- rep(piTreatments, gMax)
        results$.setParameterType("piTreatments", C_PARAM_GENERATED)
    } else {
        results$.setParameterType("piTreatments", C_PARAM_DEFAULT_VALUE)
    }

    if (stageResults$directionUpper) {
        standardizedEffect <- (piTreatments - piControl - stageResults$thetaH0) / sqrt(piTreatments * (1 - piTreatments) +
            allocationRatioPlanned * piControl * (1 - piControl)) * sqrt(1 + allocationRatioPlanned) + adjustment
    } else {
        standardizedEffect <- -(piTreatments - piControl - stageResults$thetaH0) / sqrt(piTreatments * (1 - piTreatments) +
            allocationRatioPlanned * piControl * (1 - piControl)) * sqrt(1 + allocationRatioPlanned) + adjustment
    }

    nPlanned <- allocationRatioPlanned / (1 + allocationRatioPlanned)^2 * nPlanned

    ctr <- .performClosedCombinationTest(stageResults = stageResults)
    criticalValues <- design$criticalValues

    for (treatmentArm in 1:gMax) {
        if (!is.na(ctr$separatePValues[treatmentArm, stage])) {
            # shifted decision region for use in getGroupSeqProbs
            # Inverse Normal Method
            shiftedDecisionRegionUpper <- criticalValues[(stage + 1):kMax] *
                sqrt(sum(weights[1:stage]^2) + cumsum(weights[(stage + 1):kMax]^2)) /
                sqrt(cumsum(weights[(stage + 1):kMax]^2)) -
                min(ctr$overallAdjustedTestStatistics[ctr$indices[, treatmentArm] == 1, stage], na.rm = TRUE) *
                    sqrt(sum(weights[1:stage]^2)) /
                    sqrt(cumsum(weights[(stage + 1):kMax]^2)) - standardizedEffect[treatmentArm] *
                    cumsum(sqrt(nPlanned[(stage + 1):kMax]) * weights[(stage + 1):kMax]) /
                    sqrt(cumsum(weights[(stage + 1):kMax]^2))
            if (stage == kMax - 1) {
                shiftedFutilityBounds <- c()
            } else {
                shiftedFutilityBounds <- design$futilityBounds[(stage + 1):(kMax - 1)] *
                    sqrt(sum(weights[1:stage]^2) + cumsum(weights[(stage + 1):(kMax - 1)]^2)) /
                    sqrt(cumsum(weights[(stage + 1):(kMax - 1)]^2)) -
                    min(ctr$overallAdjustedTestStatistics[ctr$indices[, treatmentArm] == 1, stage], na.rm = TRUE) *
                        sqrt(sum(weights[1:stage]^2)) /
                        sqrt(cumsum(weights[(stage + 1):(kMax - 1)]^2)) - standardizedEffect[treatmentArm] *
                        cumsum(sqrt(nPlanned[(stage + 1):(kMax - 1)]) * weights[(stage + 1):(kMax - 1)]) /
                        sqrt(cumsum(weights[(stage + 1):(kMax - 1)]^2))
            }

            # scaled information for use in getGroupSeqProbs
            scaledInformation <- (informationRates[(stage + 1):kMax] - informationRates[stage]) /
                (1 - informationRates[stage])

            decisionMatrix <- matrix(c(
                shiftedFutilityBounds, C_FUTILITY_BOUNDS_DEFAULT,
                shiftedDecisionRegionUpper
            ), nrow = 2, byrow = TRUE)

            probs <- .getGroupSequentialProbabilities(
                decisionMatrix = decisionMatrix,
                informationRates = scaledInformation
            )

            results$conditionalPower[treatmentArm, (stage + 1):kMax] <- cumsum(probs[3, ] - probs[2, ])
        }
    }
    nPlanned <- (1 + allocationRatioPlanned)^2 / allocationRatioPlanned * nPlanned
    results$nPlanned <- nPlanned
    results$.setParameterType("nPlanned", C_PARAM_GENERATED)

    results$.setParameterType("conditionalPower", C_PARAM_GENERATED)

    results$piTreatments <- piTreatments
    results$piControl <- piControl
    return(results)
}

#'
#' Calculation of conditional power based on Fisher's combination test
#'
#' @noRd
#'
.getConditionalPowerRatesMultiArmFisher <- function(..., results, design, stageResults, stage,
        allocationRatioPlanned, nPlanned, piTreatments, piControl, useAdjustment = TRUE,
        iterations, seed) {
    .assertIsTrialDesignFisher(design)
    .assertIsValidIterationsAndSeed(iterations, seed, zeroIterationsAllowed = FALSE)
    .warnInCaseOfUnknownArguments(
        functionName = ".getConditionalPowerRatesMultiArmFisher",
        ignore = c("piTreatmentsH1", "piControlH1"), ...
    )

    kMax <- design$kMax
    gMax <- stageResults$getGMax()
    criticalValues <- design$criticalValues
    weightsFisher <- .getWeightsFisher(design)

    results$iterations <- as.integer(iterations)
    results$.setParameterType("iterations", C_PARAM_USER_DEFINED)
    results$.setParameterType("seed", ifelse(is.na(seed), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))
    results$seed <- .setSeed(seed)
    results$simulated <- FALSE
    results$.setParameterType("simulated", C_PARAM_DEFAULT_VALUE)

    nPlanned <- c(rep(NA_real_, stage), nPlanned)

    if (useAdjustment) {
        condError <- .getConditionalRejectionProbabilitiesMultiArm(
            design = design, stageResults = stageResults,
            iterations = iterations, seed = seed
        )[, stage]

        ml <- (allocationRatioPlanned * piTreatments + piControl) / (1 + allocationRatioPlanned)
        adjustment <- .getOneMinusQNorm(condError) * (1 - sqrt(ml * (1 - ml) * (1 + allocationRatioPlanned)) /
            sqrt(piTreatments * (1 - piTreatments) + allocationRatioPlanned * piControl * (1 - piControl))) *
            (1 + allocationRatioPlanned) / sqrt(allocationRatioPlanned * sum(nPlanned[(stage + 1):kMax]))
        adjustment[condError < 1e-12] <- 0
    } else {
        adjustment <- 0
    }

    if (length(piTreatments) == 1) {
        piTreatments <- rep(piTreatments, gMax)
        results$.setParameterType("piTreatments", C_PARAM_GENERATED)
    } else {
        results$.setParameterType("piTreatments", C_PARAM_DEFAULT_VALUE)
    }

    if (stageResults$directionUpper) {
        standardizedEffect <- (piTreatments - piControl) / sqrt(piTreatments * (1 - piTreatments) +
            allocationRatioPlanned * piControl * (1 - piControl)) * sqrt(1 + allocationRatioPlanned) + adjustment
    } else {
        standardizedEffect <- -(piTreatments - piControl - stageResults$thetaH0) / sqrt(piTreatments * (1 - piTreatments) +
            allocationRatioPlanned * piControl * (1 - piControl)) * sqrt(1 + allocationRatioPlanned) + adjustment
    }

    nPlanned <- allocationRatioPlanned / (1 + allocationRatioPlanned)^2 * nPlanned

    ctr <- .performClosedCombinationTest(stageResults = stageResults)
    for (treatmentArm in 1:gMax) {
        if (!is.na(ctr$separatePValues[treatmentArm, stage])) {
            if (gMax == 1) {
                pValues <- ctr$adjustedStageWisePValues[ctr$indices[, treatmentArm] == 1, ][1:stage]
            } else {
                pValues <- ctr$adjustedStageWisePValues[ctr$indices[, treatmentArm] == 1, ][which.max(
                    ctr$overallAdjustedTestStatistics[ctr$indices[, treatmentArm] == 1, stage]
                ), 1:stage]
            }
            if (stage < kMax - 1) {
                for (k in (stage + 1):kMax) {
                    reject <- 0
                    for (i in 1:iterations) {
                        reject <- reject + .getRejectValueConditionalPowerFisher(
                            kMax = kMax, alpha0Vec = design$alpha0Vec,
                            criticalValues = criticalValues, weightsFisher = weightsFisher,
                            pValues = pValues, currentKMax = k, thetaH1 = standardizedEffect[treatmentArm],
                            stage = stage, nPlanned = nPlanned
                        )
                    }
                    results$conditionalPower[treatmentArm, k] <- reject / iterations
                }
                results$simulated <- TRUE
                results$.setParameterType("simulated", C_PARAM_GENERATED)
            } else if (stage == kMax - 1) {
                divisor <- prod(pValues[1:(kMax - 1)]^weightsFisher[1:(kMax - 1)])
                result <- 1 - (criticalValues[kMax] / divisor)^(1 / weightsFisher[kMax])

                if (result <= 0 || result >= 1) {
                    warning("Calculation not possible: could not calculate conditional power for stage ", kMax, call. = FALSE)
                    results$conditionalPower[treatmentArm, kMax] <- NA_real_
                } else {
                    results$conditionalPower[treatmentArm, kMax] <- 1 - stats::pnorm(.getQNorm(result) -
                        standardizedEffect[treatmentArm] * sqrt(nPlanned[kMax]))
                }
            }
        }
    }

    nPlanned <- (1 + allocationRatioPlanned)^2 / allocationRatioPlanned * nPlanned
    results$nPlanned <- nPlanned
    results$.setParameterType("nPlanned", C_PARAM_GENERATED)

    results$.setParameterType("conditionalPower", C_PARAM_GENERATED)

    results$piTreatments <- piTreatments
    results$piControl <- piControl

    if (!results$simulated) {
        results$iterations <- NA_integer_
        results$seed <- NA_real_
        results$.setParameterType("iterations", C_PARAM_NOT_APPLICABLE)
        results$.setParameterType("seed", C_PARAM_NOT_APPLICABLE)
    }

    return(results)
}

#'
#' Calculation of conditional power based on conditional Dunnett test
#'
#' @noRd
#'
.getConditionalPowerRatesMultiArmConditionalDunnett <- function(..., results, design, stageResults, stage,
        allocationRatioPlanned, nPlanned, piTreatments, piControl) {
    .assertIsTrialDesignConditionalDunnett(design)
    .warnInCaseOfUnknownArguments(
        functionName = ".getConditionalPowerRatesMultiArmConditionalDunnett",
        ignore = c("intersectionTest", "piTreatmentsH1", "piControlH1"), ...
    )

    if (stage > 1) {
        warning("Conditional power is only calculated for the first (interim) stage", call. = FALSE)
    }

    gMax <- stageResults$getGMax()
    nPlanned <- c(rep(NA_real_, stage), nPlanned)
    condError <- .getConditionalRejectionProbabilitiesMultiArm(design = design, stageResults = stageResults)[, 2]
    ml <- (allocationRatioPlanned * piTreatments + piControl) / (1 + allocationRatioPlanned)
    adjustment <- .getOneMinusQNorm(condError) * (1 - sqrt(ml * (1 - ml) * (1 + allocationRatioPlanned)) /
        sqrt(piTreatments * (1 - piTreatments) + allocationRatioPlanned * piControl * (1 - piControl))) *
        (1 + allocationRatioPlanned) / sqrt(allocationRatioPlanned * sum(nPlanned[(stage + 1):2]))
    adjustment[condError < 1e-12] <- 0

    if (length(piTreatments) == 1) {
        piTreatments <- rep(piTreatments, gMax)
        results$.setParameterType("piTreatments", C_PARAM_GENERATED)
    } else {
        results$.setParameterType("piTreatments", C_PARAM_DEFAULT_VALUE)
    }

    nPlanned <- allocationRatioPlanned / (1 + allocationRatioPlanned)^2 * nPlanned
    if (stageResults$directionUpper) {
        standardizedEffect <- (piTreatments - piControl - stageResults$thetaH0) / sqrt(piTreatments * (1 - piTreatments) +
            allocationRatioPlanned * piControl * (1 - piControl)) * sqrt(1 + allocationRatioPlanned) + adjustment
    } else {
        standardizedEffect <- -(piTreatments - piControl - stageResults$thetaH0) / sqrt(piTreatments * (1 - piTreatments) +
            allocationRatioPlanned * piControl * (1 - piControl)) * sqrt(1 + allocationRatioPlanned) + adjustment
    }

    ctr <- .getClosedConditionalDunnettTestResults(stageResults = stageResults, design = design, stage = stage)

    for (treatmentArm in 1:gMax) {
        if (!is.na(ctr$separatePValues[treatmentArm, stage])) {
            results$conditionalPower[treatmentArm, 2] <- 1 -
                stats::pnorm(.getOneMinusQNorm(min(ctr$conditionalErrorRate[
                    ctr$indices[, treatmentArm] == 1,
                    stage
                ], na.rm = TRUE)) - standardizedEffect[treatmentArm] * sqrt(nPlanned[2]))
        }
    }

    nPlanned <- (1 + allocationRatioPlanned)^2 / allocationRatioPlanned * nPlanned
    results$nPlanned <- nPlanned
    results$.setParameterType("nPlanned", C_PARAM_GENERATED)

    results$.setParameterType("conditionalPower", C_PARAM_GENERATED)

    results$piTreatments <- piTreatments
    results$piControl <- piControl
    return(results)
}

#'
#' Calculation of conditional power and likelihood values for plotting the graph
#'
#' @noRd
#'
.getConditionalPowerLikelihoodRatesMultiArm <- function(..., stageResults, stage,
        nPlanned, allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        piTreatmentRange, piControl = NA_real_,
        iterations = C_ITERATIONS_DEFAULT, seed = NA_real_) {
    .associatedArgumentsAreDefined(nPlanned = nPlanned, piTreatmentRange = piTreatmentRange)
    .assertIsSingleNumber(allocationRatioPlanned, "allocationRatioPlanned")
    .assertIsInOpenInterval(allocationRatioPlanned, "allocationRatioPlanned", 0, C_ALLOCATION_RATIO_MAXIMUM)

    design <- stageResults$.design
    kMax <- design$kMax
    gMax <- stageResults$getGMax()
    intersectionTest <- stageResults$intersectionTest

    piControl <- .assertIsValidPiControlForMultiArm(piControl, stageResults, stage)
    if (length(piControl) != 1) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "length of 'piControl' (", .arrayToString(piControl), ") must be equal to 1"
        )
    }

    piTreatmentRange <- .assertIsValidPiTreatmentRange(piTreatmentRange = piTreatmentRange)

    treatmentArms <- numeric(gMax * length(piTreatmentRange))
    effectValues <- numeric(gMax * length(piTreatmentRange))
    condPowerValues <- numeric(gMax * length(piTreatmentRange))
    likelihoodValues <- numeric(gMax * length(piTreatmentRange))

    stdErr <- sqrt(stageResults$overallPiTreatments[, stage] * (1 - stageResults$overallPiTreatments[, stage])) /
        sqrt(stageResults$.dataInput$getOverallSampleSizes(stage = stage, group = (1:gMax)))

    results <- ConditionalPowerResultsMultiArmRates$new(
        .design = design,
        .stageResults = stageResults,
        piControl = piControl,
        nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned
    )

    j <- 1
    for (i in seq(along = piTreatmentRange)) {
        for (treatmentArm in (1:gMax)) {
            treatmentArms[j] <- treatmentArm
            effectValues[j] <- piTreatmentRange[i]

            if (.isTrialDesignInverseNormal(design)) {
                condPowerValues[j] <- .getConditionalPowerRatesMultiArmInverseNormal(
                    results = results,
                    design = design, stageResults = stageResults, stage = stage, nPlanned = nPlanned,
                    allocationRatioPlanned = allocationRatioPlanned,
                    piControl = piControl,
                    piTreatments = piTreatmentRange[i]
                )$conditionalPower[treatmentArm, kMax]
            } else if (.isTrialDesignFisher(design)) {
                condPowerValues[j] <- .getConditionalPowerRatesMultiArmFisher(
                    results = results,
                    design = design, stageResults = stageResults, stage = stage, nPlanned = nPlanned,
                    allocationRatioPlanned = allocationRatioPlanned, useAdjustment = FALSE,
                    piControl = piControl,
                    piTreatments = piTreatmentRange[i],
                    iterations = iterations, seed = seed
                )$conditionalPower[treatmentArm, kMax]
            } else if (.isTrialDesignConditionalDunnett(design)) {
                condPowerValues[j] <- .getConditionalPowerRatesMultiArmConditionalDunnett(
                    results = results,
                    design = design, stageResults = stageResults, stage = stage, nPlanned = nPlanned,
                    allocationRatioPlanned = allocationRatioPlanned,
                    piControl = piControl,
                    piTreatments = piTreatmentRange[i]
                )$conditionalPower[treatmentArm, 2]
            }

            likelihoodValues[j] <- stats::dnorm(piTreatmentRange[i], stageResults$overallPiTreatments[treatmentArm, stage], stdErr[treatmentArm]) /
                stats::dnorm(0, 0, stdErr[treatmentArm])
            j <- j + 1
        }
    }

    subtitle <- paste0(
        "Intersection test = ", intersectionTest,
        ", stage = ", stage, ", # of remaining subjects = ",
        sum(nPlanned), ", control rate = ", .formatSubTitleValue(piControl, "piControl"),
        ", allocation ratio = ", .formatSubTitleValue(allocationRatioPlanned, "allocationRatioPlanned")
    )

    return(list(
        treatmentArms = treatmentArms,
        xValues = effectValues,
        condPowerValues = condPowerValues,
        likelihoodValues = likelihoodValues,
        main = C_PLOT_MAIN_CONDITIONAL_POWER_WITH_LIKELIHOOD,
        xlab = "Treatment rate",
        ylab = C_PLOT_YLAB_CONDITIONAL_POWER_WITH_LIKELIHOOD,
        sub = subtitle
    ))
}
