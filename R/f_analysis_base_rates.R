## |
## |  *Analysis of rates with group sequential and combination test*
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
## |  File version: $Revision: 8765 $
## |  Last changed: $Date: 2025-07-22 08:09:47 +0200 (Di, 22 Jul 2025) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_logger.R
NULL

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
.getAnalysisResultsRates <- function(..., design, dataInput) {
    if (.isTrialDesignGroupSequential(design)) {
        return(.getAnalysisResultsRatesGroupSequential(
            design = design,
            dataInput = dataInput,
            ...
        ))
    }

    if (.isTrialDesignInverseNormal(design)) {
        return(.getAnalysisResultsRatesInverseNormal(
            design = design,
            dataInput = dataInput,
            ...
        ))
    }

    if (.isTrialDesignFisher(design)) {
        return(.getAnalysisResultsRatesFisher(
            design = design,
            dataInput = dataInput,
            ...
        ))
    }

    .stopWithWrongDesignMessage(design, inclusiveConditionalDunnett = FALSE)
}

.getAnalysisResultsRatesInverseNormal <- function(
        ...,
        design,
        dataInput,
        directionUpper = NA,
        normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT,
        thetaH0 = C_THETA_H0_RATES_DEFAULT,
        pi1 = NA_real_,
        pi2 = NA_real_,
        nPlanned = NA_real_,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        stdErrorEstimate = NA_character_,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .assertIsTrialDesignInverseNormal(design)
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    .warnInCaseOfUnknownArguments(
        functionName = ".getAnalysisResultsRatesInverseNormal",
        ignore = c(
            .getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
                design,
                powerCalculationEnabled = TRUE
            ),
            "stage"
        ),
        ...
    )

    results <- AnalysisResultsInverseNormal$new(design = design, dataInput = dataInput) # R6$new

    .getAnalysisResultsRatesAll(
        results = results,
        design = design,
        dataInput = dataInput,
        stage = stage,
        directionUpper = directionUpper,
        normalApproximation = normalApproximation,
        thetaH0 = thetaH0,
        pi1 = pi1,
        pi2 = pi2,
        nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned,
        stdErrorEstimate = stdErrorEstimate,
        tolerance = tolerance
    )

    return(results)
}

.getAnalysisResultsRatesGroupSequential <- function(
        ...,
        design,
        dataInput,
        directionUpper = NA,
        normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT,
        thetaH0 = C_THETA_H0_RATES_DEFAULT,
        pi1 = NA_real_,
        pi2 = NA_real_,
        nPlanned = NA_real_,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        stdErrorEstimate = NA_character_,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .assertIsTrialDesignGroupSequential(design)
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    .warnInCaseOfUnknownArguments(
        functionName = ".getAnalysisResultsRatesGroupSequential",
        ignore = c(
            .getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
                design,
                powerCalculationEnabled = TRUE
            ),
            "stage"
        ),
        ...
    )

    results <- AnalysisResultsGroupSequential$new(design = design, dataInput = dataInput) # R6$new

    .getAnalysisResultsRatesAll(
        results = results,
        design = design,
        dataInput = dataInput,
        stage = stage,
        directionUpper = directionUpper,
        normalApproximation = normalApproximation,
        thetaH0 = thetaH0,
        pi1 = pi1,
        pi2 = pi2,
        nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned,
        stdErrorEstimate = stdErrorEstimate,
        tolerance = tolerance
    )

    return(results)
}

.getAnalysisResultsRatesFisher <- function(
        ...,
        design,
        dataInput,
        directionUpper = NA,
        normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT,
        thetaH0 = C_THETA_H0_RATES_DEFAULT,
        pi1 = NA_real_,
        pi2 = NA_real_,
        nPlanned = NA_real_,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT,
        iterations = C_ITERATIONS_DEFAULT,
        seed = NA_real_) {
    .assertIsTrialDesignFisher(design)
    .assertIsValidIterationsAndSeed(iterations, seed, zeroIterationsAllowed = FALSE)
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    .warnInCaseOfUnknownArguments(
        functionName = ".getAnalysisResultsRatesFisher",
        ignore = c(
            .getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
                design,
                powerCalculationEnabled = TRUE
            ),
            "stage"
        ),
        ...
    )

    results <- AnalysisResultsFisher$new(design = design, dataInput = dataInput) # R6$new
    .setValueAndParameterType(results, "iterations", as.integer(iterations), C_ITERATIONS_DEFAULT)
    .setValueAndParameterType(results, "seed", seed, NA_real_)

    .getAnalysisResultsRatesAll(
        results = results,
        design = design,
        dataInput = dataInput,
        stage = stage,
        directionUpper = directionUpper,
        normalApproximation = normalApproximation,
        thetaH0 = thetaH0,
        pi1 = pi1,
        pi2 = pi2,
        nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned,
        stdErrorEstimate = NA_character_,
        tolerance = tolerance,
        iterations = iterations,
        seed = seed
    )

    return(results)
}

#'
#' The following parameters will be taken from 'design':
#' stages, informationRates, criticalValues, futilityBounds, alphaSpent, stageLevels
#'
#' @noRd
#'
.getAnalysisResultsRatesAll <- function(
        ...,
        results,
        design,
        dataInput,
        stage,
        directionUpper,
        normalApproximation,
        thetaH0,
        pi1,
        pi2,
        nPlanned,
        allocationRatioPlanned,
        stdErrorEstimate,
        tolerance,
        iterations,
        seed) {
    startTime <- Sys.time()
    stageResults <- .getStageResultsRates(
        design = design,
        dataInput = dataInput,
        stage = stage,
        thetaH0 = thetaH0,
        directionUpper = directionUpper,
        normalApproximation = normalApproximation
    )
    results$.setStageResults(stageResults)
    .logProgress("Stage results calculated", startTime = startTime)

    pi1User <- pi1
    .assertIsSingleNumber(pi1, "pi1", naAllowed = TRUE)
    pi1 <- .assertIsValidPi1(pi1, stageResults, stage)
    if (identical(pi1, pi1User)) {
        .setValueAndParameterType(results, "pi1", pi1, NA_real_)
    } else {
        results$pi1 <- pi1
        results$.setParameterType("pi1", C_PARAM_GENERATED)
    }

    if (dataInput$getNumberOfGroups() == 2) {
        pi2User <- pi2
        .assertIsSingleNumber(pi2, "pi2", naAllowed = TRUE)
        pi2 <- .assertIsValidPi2(pi2, stageResults, stage)
        if (identical(pi2, pi2User)) {
            .setValueAndParameterType(results, "pi2", pi2, NA_real_)
        } else {
            results$pi2 <- pi2
            results$.setParameterType("pi2", C_PARAM_GENERATED)
        }
    } else {
        if (!all(is.na(pi2))) {
            warning(
                "'pi2' (",
                .arrayToString(pi2),
                ") will be ignored ",
                "because the specified data has only one group",
                call. = FALSE
            )
        }
        results$pi2 <- NA_real_
        results$.setParameterType("pi2", C_PARAM_NOT_APPLICABLE)
    }
    .warnInCaseOfUnusedConditionalPowerArgument(results, nPlanned, "pi1", pi1)
    .warnInCaseOfUnusedConditionalPowerArgument(results, nPlanned, "pi2", pi2)

    .setValueAndParameterType(results, "directionUpper", directionUpper, C_DIRECTION_UPPER_DEFAULT)
    .setValueAndParameterType(
        results,
        "normalApproximation",
        normalApproximation,
        C_NORMAL_APPROXIMATION_RATES_DEFAULT
    )
    .setValueAndParameterType(results, "thetaH0", thetaH0, C_THETA_H0_RATES_DEFAULT)
    .setConditionalPowerArguments(results, dataInput, nPlanned, allocationRatioPlanned)

    #  test actions
    results$testActions <- getTestActions(stageResults = stageResults)
    results$.setParameterType("testActions", C_PARAM_GENERATED)

    if (design$kMax > 1) {
        # conditional power
        startTime <- Sys.time()
        if (.isTrialDesignFisher(design)) {
            results$.conditionalPowerResults <- .getConditionalPowerRates(
                stageResults = stageResults,
                nPlanned = nPlanned,
                allocationRatioPlanned = allocationRatioPlanned,
                pi1 = pi1,
                pi2 = pi2,
                iterations = iterations,
                seed = seed
            )
            .synchronizeIterationsAndSeed(results)
        } else {
            results$.conditionalPowerResults <- .getConditionalPowerRates(
                stageResults = stageResults,
                nPlanned = nPlanned,
                allocationRatioPlanned = allocationRatioPlanned,
                pi1 = pi1,
                pi2 = pi2
            )
            results$conditionalPower <- results$.conditionalPowerResults$conditionalPower
            results$.setParameterType("conditionalPower", C_PARAM_GENERATED)
        }
        .logProgress("Conditional power calculated", startTime = startTime)

        # CRP - conditional rejection probabilities
        startTime <- Sys.time()
        if (.isTrialDesignFisher(design) && isTRUE(.getOptionalArgument("simulateCRP", ...))) {
            results$.setParameterType("seed", ifelse(is.na(seed), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))
            seed <- results$.conditionalPowerResults$seed
            crp <- getConditionalRejectionProbabilities(
                stageResults = stageResults,
                iterations = iterations,
                seed = seed
            )
            results$conditionalRejectionProbabilities <- crp$crpFisherSimulated
            paramTypeSeed <- results$.conditionalPowerResults$.getParameterType("seed")
            if (paramTypeSeed != C_PARAM_TYPE_UNKNOWN) {
                results$.setParameterType("seed", paramTypeSeed)
            }
            results$seed <- seed
        } else {
            results$conditionalRejectionProbabilities <-
                getConditionalRejectionProbabilities(stageResults = stageResults)
        }
        results$.setParameterType("conditionalRejectionProbabilities", C_PARAM_GENERATED)
        .logProgress("Conditional rejection probabilities (CRP) calculated", startTime = startTime)
    }

    # RCI - repeated confidence interval
    startTime <- Sys.time()
    repeatedConfidenceIntervals <- .getRepeatedConfidenceIntervalsRates(
        design = design,
        dataInput = dataInput,
        stage = stage,
        directionUpper = directionUpper,
        normalApproximation = normalApproximation,
        tolerance = tolerance
    )
    results$repeatedConfidenceIntervalLowerBounds <- repeatedConfidenceIntervals[1, ]
    results$repeatedConfidenceIntervalUpperBounds <- repeatedConfidenceIntervals[2, ]
    results$.setParameterType("repeatedConfidenceIntervalLowerBounds", C_PARAM_GENERATED)
    results$.setParameterType("repeatedConfidenceIntervalUpperBounds", C_PARAM_GENERATED)
    .logProgress("Repeated confidence interval calculated", startTime = startTime)

    # repeated p-value
    startTime <- Sys.time()
    results$repeatedPValues <- getRepeatedPValues(
        stageResults = stageResults,
        tolerance = tolerance
    )
    results$.setParameterType("repeatedPValues", C_PARAM_GENERATED)
    .logProgress("Repeated p-values calculated", startTime = startTime)

    if (design$kMax > 1) {
        # final p-value
        startTime <- Sys.time()
        finalPValue <- getFinalPValue(stageResults, showWarnings = FALSE)
        results$finalPValues <- .getVectorWithFinalValueAtFinalStage(
            kMax = design$kMax,
            finalValue = finalPValue$pFinal,
            finalStage = finalPValue$finalStage
        )
        results$.setParameterType("finalPValues", C_PARAM_GENERATED)
        results$finalStage <- finalPValue$finalStage
        results$.setParameterType("finalPValues", C_PARAM_GENERATED)
        results$.setParameterType("finalStage", C_PARAM_GENERATED)
        .logProgress("Final p-value calculated", startTime = startTime)

        # final confidence interval & median unbiased estimate
        startTime <- Sys.time()
        if (!.isTrialDesignFisher(design)) {
            stdErrorEstimate <- .assertIsValidStdErrorEstimateRates(stdErrorEstimate, dataInput)
        }
        finalConfidenceIntervals <- .getFinalConfidenceIntervalRates(
            design = design,
            dataInput = dataInput,
            thetaH0 = thetaH0,
            stage = stage,
            directionUpper = directionUpper,
            normalApproximation = normalApproximation,
            stdErrorEstimate = stdErrorEstimate,
            tolerance = tolerance
        )
        if (!is.null(finalConfidenceIntervals)) {
            finalStage <- finalConfidenceIntervals$finalStage
            results$finalConfidenceIntervalLowerBounds <- .getVectorWithFinalValueAtFinalStage(
                kMax = design$kMax,
                finalValue = finalConfidenceIntervals$finalConfidenceInterval[1],
                finalStage = finalStage
            )
            results$finalConfidenceIntervalUpperBounds <- .getVectorWithFinalValueAtFinalStage(
                kMax = design$kMax,
                finalValue = finalConfidenceIntervals$finalConfidenceInterval[2],
                finalStage = finalStage
            )
            results$medianUnbiasedEstimates <- .getVectorWithFinalValueAtFinalStage(
                kMax = design$kMax,
                finalValue = finalConfidenceIntervals$medianUnbiased,
                finalStage = finalStage
            )
            results$.setParameterType("finalConfidenceIntervalLowerBounds", C_PARAM_GENERATED)
            results$.setParameterType("finalConfidenceIntervalUpperBounds", C_PARAM_GENERATED)
            results$.setParameterType("medianUnbiasedEstimates", C_PARAM_GENERATED)

            if (!.isTrialDesignFisher(design) && !is.na(stdErrorEstimate)) {
                results$.setParameterType(
                    "stdErrorEstimate",
                    ifelse(
                        stdErrorEstimate == C_RATES_STD_ERROR_ESTIMATE_DEFAULT,
                        C_PARAM_DEFAULT_VALUE,
                        C_PARAM_USER_DEFINED
                    )
                )
                results$stdErrorEstimate <- stdErrorEstimate
            }

            .logProgress("Final confidence interval calculated", startTime = startTime)
        }
    }

    return(results)
}

#' @title
#' Get Stage Results Rates
#'
#' @description
#' Returns a stage results object.
#'
#' @param design the trial design.
#'
#' @return Returns a \code{StageResultsRates} object.
#'
#' @keywords internal
#'
#' @noRd
#'
.getStageResultsRates <- function(
        ...,
        design,
        dataInput,
        thetaH0 = NA_real_,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT,
        stage = NA_integer_,
        userFunctionCallEnabled = FALSE) {
    .assertIsDatasetRates(dataInput)
    .assertIsValidThetaH0DataInput(thetaH0, dataInput)
    .assertIsSingleLogical(normalApproximation, "normalApproximation")
    .warnInCaseOfUnknownArguments(
        functionName = "getStageResultsRates",
        ignore = .getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design, powerCalculationEnabled = TRUE),
        ...
    )
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design, stage = stage)

    effectSizes <- rep(NA_real_, design$kMax)

    if (dataInput$getNumberOfGroups() == 1) {
        if (is.na(thetaH0)) {
            stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'thetaH0' must be defined", call. = FALSE)
        }

        if (normalApproximation) {
            overallTestStatistics <- c(
                (dataInput$getOverallEventsUpTo(stage) /
                    dataInput$getOverallSampleSizesUpTo(stage) -
                    thetaH0) /
                    sqrt(thetaH0 * (1 - thetaH0)) *
                    sqrt(dataInput$getOverallSampleSizesUpTo(stage)),
                rep(NA_real_, design$kMax - stage)
            )
            overallPValues <- .applyDirectionOfAlternative(
                stats::pnorm(overallTestStatistics),
                directionUpper,
                type = "oneMinusValue",
                phase = "analysis"
            )
        } else {
            overallTestStatistics <- rep(NA_real_, design$kMax)
            overallPValues <- stats::pbinom(
                .applyDirectionOfAlternative(
                    dataInput$getOverallEventsUpTo(stage),
                    directionUpper,
                    type = "valueMinusOne",
                    phase = "analysis"
                ),
                dataInput$getOverallSampleSizesUpTo(stage),
                thetaH0,
                lower.tail = .applyDirectionOfAlternative(
                    FALSE,
                    directionUpper,
                    type = "negateIfLower",
                    phase = "analysis"
                )
            )
            overallTestStatistics <- .getOneMinusQNorm(overallPValues)
        }
        effectSizes[1:stage] <- dataInput$getOverallEventsUpTo(stage) /
            dataInput$getOverallSampleSizesUpTo(stage)
    }

    if (dataInput$getNumberOfGroups() == 2) {
        if (is.na(thetaH0)) {
            thetaH0 <- C_THETA_H0_RATES_DEFAULT
        }

        overallEvents1 <- dataInput$getOverallEvents(group = 1)
        overallEvents2 <- dataInput$getOverallEvents(group = 2)

        overallTestStatistics <- rep(NA_real_, design$kMax)
        overallPValues <- rep(NA_real_, design$kMax)

        for (k in 1:stage) {
            if (normalApproximation) {
                if (thetaH0 == 0) {
                    if (
                        (overallEvents1[k] + overallEvents2[k] == 0) ||
                            (overallEvents1[k] + overallEvents2[k] ==
                                sum(dataInput$getSampleSizesUpTo(k, 1)) +
                                    sum(dataInput$getSampleSizesUpTo(k, 2)))
                        ) {
                        overallTestStatistics[k] <- 0
                    } else {
                        overallRateH0 <- (overallEvents1[k] + overallEvents2[k]) /
                            (sum(dataInput$getSampleSizesUpTo(k, 1)) + sum(dataInput$getSampleSizesUpTo(k, 2)))
                        overallTestStatistics[k] <-
                            (overallEvents1[k] /
                                sum(dataInput$getSampleSizesUpTo(k, 1)) -
                                overallEvents2[k] / sum(dataInput$getSampleSizesUpTo(k, 2)) -
                                thetaH0) /
                                sqrt(
                                    overallRateH0 *
                                        (1 - overallRateH0) *
                                        (1 /
                                            sum(dataInput$getSampleSizesUpTo(k, 1)) +
                                            1 / sum(dataInput$getSampleSizesUpTo(k, 2)))
                                )
                    }
                } else {
                    y <- .getFarringtonManningValues(
                        rate1 = overallEvents1[k] / sum(dataInput$getSampleSizesUpTo(k, 1)),
                        rate2 = overallEvents2[k] / sum(dataInput$getSampleSizesUpTo(k, 2)),
                        theta = thetaH0,
                        allocation = sum(dataInput$getSampleSizesUpTo(k, 1)) /
                            sum(dataInput$getSampleSizesUpTo(k, 2)),
                        "diff"
                    )
                    overallTestStatistics[k] <-
                        (overallEvents1[k] /
                            sum(dataInput$getSampleSizesUpTo(k, 1)) -
                            overallEvents2[k] / sum(dataInput$getSampleSizesUpTo(k, 2)) -
                            thetaH0) /
                            sqrt(
                                y$ml1 *
                                    (1 - y$ml1) /
                                    sum(dataInput$getSampleSizesUpTo(k, 1)) +
                                    y$ml2 * (1 - y$ml2) / sum(dataInput$getSampleSizesUpTo(k, 2))
                            )
                }
                overallPValues[k] <- .applyDirectionOfAlternative(
                    stats::pnorm(overallTestStatistics[k]),
                    directionUpper,
                    type = "oneMinusValue",
                    phase = "analysis"
                )
            } else {
                if (thetaH0 != 0) {
                    stop(
                        C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                        "thetaH0 must be equal 0 for performing Fisher's exact test",
                        call. = FALSE
                    )
                }

                overallPValues[k] <- stats::phyper(
                    .applyDirectionOfAlternative(
                        overallEvents1[k],
                        directionUpper,
                        type = "valueMinusOne",
                        phase = "analysis"
                    ),
                    overallEvents1[k] + overallEvents2[k],
                    sum(dataInput$getSampleSizesUpTo(k, 1)) +
                        sum(dataInput$getSampleSizesUpTo(k, 2)) -
                        overallEvents1[k] -
                        overallEvents2[k],
                    sum(dataInput$getSampleSizesUpTo(k, 1)),
                    lower.tail = .applyDirectionOfAlternative(
                        FALSE,
                        directionUpper,
                        type = "negateIfLower",
                        phase = "analysis"
                    )
                )
                overallTestStatistics <- .getOneMinusQNorm(overallPValues)
            }
        }
        effectSizes[1:stage] <- overallEvents1[1:stage] /
            cumsum(dataInput$getSampleSizesUpTo(stage, 1)) -
            overallEvents2[1:stage] / cumsum(dataInput$getSampleSizesUpTo(stage, 2))
    }

    # calculation of stage-wise test statistics and combination tests
    testStatistics <- rep(NA_real_, design$kMax)
    pValues <- rep(NA_real_, design$kMax)
    combInverseNormal <- rep(NA_real_, design$kMax)
    combFisher <- rep(NA_real_, design$kMax)
    weightsInverseNormal <- .getWeightsInverseNormal(design)
    weightsFisher <- .getWeightsFisher(design)
    for (k in 1:stage) {
        if (dataInput$getNumberOfGroups() == 1) {
            if (normalApproximation) {
                # stage-wise test statistics
                testStatistics[k] <- (dataInput$getEvent(k) / dataInput$getSampleSize(k) - thetaH0) /
                    sqrt(thetaH0 * (1 - thetaH0)) *
                    sqrt(dataInput$getSampleSize(k))
                pValues[k] <- .applyDirectionOfAlternative(
                    stats::pnorm(testStatistics[k]),
                    directionUpper,
                    type = "oneMinusValue",
                    phase = "analysis"
                )
            } else {
                testStatistics[k] <- NA_real_
                pValues[k] <- stats::pbinom(
                    .applyDirectionOfAlternative(
                        dataInput$getEvent(k),
                        directionUpper,
                        type = "valueMinusOne",
                        phase = "analysis"
                    ),
                    dataInput$getSampleSize(k),
                    thetaH0,
                    lower.tail = .applyDirectionOfAlternative(
                        FALSE,
                        directionUpper,
                        type = "negateIfLower",
                        phase = "analysis"
                    )
                )
            }
        } else if (dataInput$getNumberOfGroups() == 2) {
            if (normalApproximation) {
                # stage-wise test statistics
                if (thetaH0 == 0) {
                    if (
                        (dataInput$getEvent(k, 1) + dataInput$getEvent(k, 2) == 0) ||
                            (dataInput$getEvent(k, 1) + dataInput$getEvent(k, 2) ==
                                dataInput$getSampleSize(k, 1) + dataInput$getSampleSize(k, 2))
                        ) {
                        testStatistics[k] <- 0
                    } else {
                        rateH0 <- (dataInput$getEvent(k, 1) + dataInput$getEvent(k, 2)) /
                            (dataInput$getSampleSize(k, 1) + dataInput$getSampleSize(k, 2))
                        testStatistics[k] <-
                            (dataInput$getEvent(k, 1) /
                                dataInput$getSampleSize(k, 1) -
                                dataInput$getEvent(k, 2) / dataInput$getSampleSize(k, 2) -
                                thetaH0) /
                                sqrt(
                                    rateH0 *
                                        (1 - rateH0) *
                                        (1 / dataInput$getSampleSize(k, 1) + 1 / dataInput$getSampleSize(k, 2))
                                )
                    }
                } else {
                    y <- .getFarringtonManningValues(
                        rate1 = dataInput$getEvent(k, 1) / dataInput$getSampleSize(k, 1),
                        rate2 = dataInput$getEvent(k, 2) / dataInput$getSampleSize(k, 2),
                        theta = thetaH0,
                        allocation = dataInput$getSampleSize(k, 1) / dataInput$getSampleSize(k, 2),
                        method = "diff"
                    )

                    testStatistics[k] <- (dataInput$getEvent(k, 1) /
                        dataInput$getSampleSize(k, 1) -
                        dataInput$getEvent(k, 2) / dataInput$getSampleSize(k, 2) -
                        thetaH0) /
                        sqrt(
                            y$ml1 *
                                (1 - y$ml1) /
                                dataInput$getSampleSize(k, 1) +
                                y$ml2 * (1 - y$ml2) / dataInput$getSampleSize(k, 2)
                        )
                }
                pValues[k] <- .applyDirectionOfAlternative(
                    stats::pnorm(testStatistics[k]),
                    directionUpper,
                    type = "oneMinusValue",
                    phase = "analysis"
                )
            } else {
                testStatistics[k] <- NA_real_

                pValues[k] <- stats::phyper(
                    .applyDirectionOfAlternative(
                        dataInput$getEvent(k, 1),
                        directionUpper,
                        type = "valueMinusOne",
                        phase = "analysis"
                    ),
                    dataInput$getEvent(k, 1) + dataInput$getEvent(k, 2),
                    dataInput$getSampleSize(k, 1) +
                        dataInput$getSampleSize(k, 2) -
                        dataInput$getEvent(k, 1) -
                        dataInput$getEvent(k, 2),
                    dataInput$getSampleSize(k, 1),
                    lower.tail = .applyDirectionOfAlternative(
                        FALSE,
                        directionUpper,
                        type = "negateIfLower",
                        phase = "analysis"
                    )
                )
            }
        }

        # inverse normal test
        combInverseNormal[k] <- (weightsInverseNormal[1:k] %*% .getOneMinusQNorm(pValues[1:k])) /
            sqrt(sum(weightsInverseNormal[1:k]^2))

        # Fisher combination test
        combFisher[k] <- prod(pValues[1:k]^weightsFisher[1:k])
    }

    direction <- ifelse(!isFALSE(directionUpper), C_DIRECTION_UPPER, C_DIRECTION_LOWER)

    stageResults <- StageResultsRates$new(
        # R6$new
        design = design,
        dataInput = dataInput,
        stage = as.integer(stage),
        overallTestStatistics = .fillWithNAs(overallTestStatistics, design$kMax),
        overallPValues = .fillWithNAs(overallPValues, design$kMax),
        effectSizes = effectSizes,
        overallEvents = .fillWithNAs(dataInput$getOverallEventsUpTo(stage, group = 1), design$kMax),
        overallSampleSizes = .fillWithNAs(dataInput$getOverallSampleSizesUpTo(stage, 1), design$kMax),
        testStatistics = testStatistics,
        pValues = pValues,
        combInverseNormal = combInverseNormal,
        combFisher = combFisher,
        weightsInverseNormal = weightsInverseNormal,
        weightsFisher = weightsFisher,
        thetaH0 = thetaH0,
        direction = ifelse(!isFALSE(directionUpper), C_DIRECTION_UPPER, C_DIRECTION_LOWER),
        normalApproximation = normalApproximation
    )

    if (dataInput$getNumberOfGroups() == 1) {
        stageResults$overallEvents <- .fillWithNAs(dataInput$getOverallEventsUpTo(stage, group = 1), design$kMax)
        stageResults$overallSampleSizes <- .fillWithNAs(dataInput$getOverallSampleSizesUpTo(stage, 1), design$kMax)
        stageResults$overallPi1 <- stageResults$overallEvents / stageResults$overallSampleSizes
        stageResults$.setParameterType("overallPi1", C_PARAM_GENERATED)
    } else if (dataInput$getNumberOfGroups() == 2) {
        stageResults$overallEvents1 <- .fillWithNAs(dataInput$getOverallEventsUpTo(stage, group <- 1), design$kMax)
        stageResults$overallEvents2 <- .fillWithNAs(dataInput$getOverallEventsUpTo(stage, group <- 2), design$kMax)
        stageResults$overallSampleSizes1 <- .fillWithNAs(dataInput$getOverallSampleSizesUpTo(stage, 1), design$kMax)
        stageResults$overallSampleSizes2 <- .fillWithNAs(dataInput$getOverallSampleSizesUpTo(stage, 2), design$kMax)
        stageResults$overallPi1 <- stageResults$overallEvents1 / stageResults$overallSampleSizes1
        stageResults$overallPi2 <- stageResults$overallEvents2 / stageResults$overallSampleSizes2
        stageResults$.setParameterType("overallPi1", C_PARAM_GENERATED)
        stageResults$.setParameterType("overallPi2", C_PARAM_GENERATED)
    }

    if (.isTrialDesignFisher(design)) {
        stageResults$.setParameterType("combFisher", C_PARAM_GENERATED)
        stageResults$.setParameterType("weightsFisher", C_PARAM_GENERATED)
    } else if (.isTrialDesignInverseNormal(design)) {
        stageResults$.setParameterType("combInverseNormal", C_PARAM_GENERATED)
        stageResults$.setParameterType("weightsInverseNormal", C_PARAM_GENERATED)
    }

    return(stageResults)
}

#'
#' Calculation of lower and upper limits of repeated confidence intervals (RCIs) for Rates
#'
#' @noRd
#'
.getRepeatedConfidenceIntervalsRates <- function(..., design) {
    if (.isTrialDesignGroupSequential(design)) {
        return(.getRepeatedConfidenceIntervalsRatesGroupSequential(design = design, ...))
    }

    if (.isTrialDesignInverseNormal(design)) {
        return(.getRepeatedConfidenceIntervalsRatesInverseNormal(design = design, ...))
    }

    if (.isTrialDesignFisher(design)) {
        return(.getRepeatedConfidenceIntervalsRatesFisher(design = design, ...))
    }

    .stopWithWrongDesignMessage(design, inclusiveConditionalDunnett = FALSE)
}

.getRootThetaRates <- function(
        ...,
        design,
        dataInput,
        stage,
        directionUpper,
        normalApproximation,
        firstParameterName,
        secondValue,
        tolerance,
        acceptResultsOutOfTolerance,
        callingFunctionInformation) {
    if (dataInput$getNumberOfGroups() == 2) {
        thetaLow <- -1 + tolerance
    } else {
        thetaLow <- tolerance
    }
    thetaUp <- 1 - tolerance

    if (dataInput$getNumberOfGroups() == 1 && !normalApproximation) {
        acceptResultsOutOfTolerance <- FALSE
    }

    result <- .getOneDimensionalRoot(
        function(theta) {
            stageResults <- .getStageResultsRates(
                design = design,
                dataInput = dataInput,
                stage = stage,
                thetaH0 = theta,
                directionUpper = directionUpper,
                normalApproximation = normalApproximation
            )
            firstValue <- stageResults[[firstParameterName]][stage]
            if (.isTrialDesignGroupSequential(design)) {
                firstValue <- .getOneMinusQNorm(firstValue)
            }
            return(firstValue - secondValue)
        },
        lower = thetaLow,
        upper = thetaUp,
        tolerance = tolerance,
        acceptResultsOutOfTolerance = acceptResultsOutOfTolerance,
        callingFunctionInformation = callingFunctionInformation
    )

    return(result)
}

.getRepeatedConfidenceIntervalsRatesAll <- function(
        ...,
        design,
        dataInput,
        normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT,
        directionUpper = NA,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT,
        firstParameterName) {
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)

    if (!normalApproximation && dataInput$getNumberOfGroups() == 2) {
        normalApproximation <- TRUE
        message("Repeated confidence intervals will be calculated under the normal approximation")
    }

    futilityCorr <- rep(NA_real_, design$kMax) # necessary for adjustment for binding futility boundaries

    criticalValues <- .getCriticalValues(design)

    if (.isTrialDesignFisher(design)) {
        bounds <- design$alpha0Vec
        border <- C_ALPHA_0_VEC_DEFAULT
        conditionFunction <- .isFirstValueSmallerThanSecondValue
    } else {
        criticalValues[is.infinite(criticalValues) & criticalValues > 0] <- C_QNORM_MAXIMUM
        criticalValues[is.infinite(criticalValues) & criticalValues < 0] <- C_QNORM_MINIMUM
        bounds <- design$futilityBounds
        border <- C_FUTILITY_BOUNDS_DEFAULT
        conditionFunction <- .isFirstValueGreaterThanSecondValue
    }

    repeatedConfidenceIntervals <- matrix(NA_real_, 2, design$kMax)
    for (k in (1:stage)) {
        startTime <- Sys.time()
        if (criticalValues[k] < C_QNORM_MAXIMUM) {
            # finding upper and lower RCI limits through root function
            if (dataInput$getNumberOfGroups() == 1) {
                if (dataInput$overallEvents[k] == 0) {
                    repeatedConfidenceIntervals[1, k] <- 0
                } else {
                    repeatedConfidenceIntervals[1, k] <- .getRootThetaRates(
                        design = design,
                        dataInput = dataInput,
                        stage = k,
                        directionUpper = TRUE,
                        normalApproximation = normalApproximation,
                        firstParameterName = firstParameterName,
                        secondValue = criticalValues[k],
                        tolerance = tolerance,
                        acceptResultsOutOfTolerance = TRUE,
                        callingFunctionInformation = paste0("Repeated confidence interval [1, ", k, "]")
                    )
                }

                if (dataInput$overallEvents[k] == dataInput$overallSampleSizes[k]) {
                    repeatedConfidenceIntervals[2, k] <- 1
                } else {
                    repeatedConfidenceIntervals[2, k] <- .getRootThetaRates(
                        design = design,
                        dataInput = dataInput,
                        stage = k,
                        directionUpper = FALSE,
                        normalApproximation = normalApproximation,
                        firstParameterName = firstParameterName,
                        secondValue = criticalValues[k],
                        tolerance = tolerance,
                        acceptResultsOutOfTolerance = TRUE,
                        callingFunctionInformation = paste0("Repeated confidence interval [2, ", k, "]")
                    )
                }
            } else if (dataInput$getNumberOfGroups() == 2) {
                repeatedConfidenceIntervals[1, k] <- .getRootThetaRates(
                    design = design,
                    dataInput = dataInput,
                    stage = k,
                    directionUpper = TRUE,
                    normalApproximation = normalApproximation,
                    firstParameterName = firstParameterName,
                    secondValue = criticalValues[k],
                    tolerance = tolerance,
                    acceptResultsOutOfTolerance = TRUE,
                    callingFunctionInformation = paste0("Repeated confidence interval [1, ", k, "]")
                )

                repeatedConfidenceIntervals[2, k] <- .getRootThetaRates(
                    design = design,
                    dataInput = dataInput,
                    stage = k,
                    directionUpper = FALSE,
                    normalApproximation = normalApproximation,
                    firstParameterName = firstParameterName,
                    secondValue = criticalValues[k],
                    tolerance = tolerance,
                    acceptResultsOutOfTolerance = TRUE,
                    callingFunctionInformation = paste0("Repeated confidence interval [1, ", k, "]")
                )
            }

            # adjustment for binding futility bounds
            if (k > 1 && !is.na(bounds[k - 1]) && conditionFunction(bounds[k - 1], border) && design$bindingFutility) {
                parameterName <- ifelse(.isTrialDesignFisher(design), "pValues", firstParameterName)

                futilityCorr[k] <- .getRootThetaRates(
                    design = design,
                    dataInput = dataInput,
                    stage = k - 1,
                    directionUpper = directionUpper,
                    normalApproximation = normalApproximation,
                    firstParameterName = parameterName,
                    secondValue = bounds[k - 1],
                    tolerance = tolerance,
                    acceptResultsOutOfTolerance = TRUE,
                    callingFunctionInformation = paste0("Repeated confidence interval, futility correction [", k, "]")
                )

                if (is.na(directionUpper) || isTRUE(directionUpper)) {
                    repeatedConfidenceIntervals[1, k] <- min(min(futilityCorr[2:k]), repeatedConfidenceIntervals[1, k])
                } else {
                    repeatedConfidenceIntervals[2, k] <- max(max(futilityCorr[2:k]), repeatedConfidenceIntervals[2, k])
                }
            }
            .logProgress("Repeated confidence interval of stage %s calculated", startTime = startTime, k)
        }

        if (
            !is.na(repeatedConfidenceIntervals[1, k]) &&
                !is.na(repeatedConfidenceIntervals[2, k]) &&
                repeatedConfidenceIntervals[1, k] > repeatedConfidenceIntervals[2, k]
            ) {
            repeatedConfidenceIntervals[, k] <- rep(NA_real_, 2)
        }
    }

    return(repeatedConfidenceIntervals)
}

#'
#' RCIs based on group sequential method
#'
#' @noRd
#'
.getRepeatedConfidenceIntervalsRatesGroupSequential <- function(
        ...,
        design,
        dataInput,
        normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT,
        directionUpper = NA,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .warnInCaseOfUnknownArguments(
        functionName = ".getRepeatedConfidenceIntervalsRatesGroupSequential",
        ignore = c(
            .getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
                design,
                powerCalculationEnabled = TRUE
            ),
            "stage"
        ),
        ...
    )

    return(.getRepeatedConfidenceIntervalsRatesAll(
        design = design,
        dataInput = dataInput,
        normalApproximation = normalApproximation,
        directionUpper = directionUpper,
        firstParameterName = "overallPValues",
        tolerance = tolerance,
        ...
    ))
}

#'
#' RCIs based on inverse normal combination test
#'
#' @noRd
#'
.getRepeatedConfidenceIntervalsRatesInverseNormal <- function(
        ...,
        design,
        dataInput,
        normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT,
        directionUpper = NA,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .warnInCaseOfUnknownArguments(
        functionName = ".getRepeatedConfidenceIntervalsRatesInverseNormal",
        ignore = c(
            .getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
                design,
                powerCalculationEnabled = TRUE
            ),
            "stage"
        ),
        ...
    )

    return(.getRepeatedConfidenceIntervalsRatesAll(
        design = design,
        dataInput = dataInput,
        normalApproximation = normalApproximation,
        directionUpper = directionUpper,
        firstParameterName = "combInverseNormal",
        tolerance = tolerance,
        ...
    ))
}

#'
#' RCIs based on Fisher's combination test
#'
#' @noRd
#'
.getRepeatedConfidenceIntervalsRatesFisher <- function(
        ...,
        design,
        dataInput,
        normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT,
        directionUpper = NA,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .warnInCaseOfUnknownArguments(
        functionName = ".getRepeatedConfidenceIntervalsRatesFisher",
        ignore = c(
            .getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
                design,
                powerCalculationEnabled = TRUE
            ),
            "stage"
        ),
        ...
    )

    return(.getRepeatedConfidenceIntervalsRatesAll(
        design = design,
        dataInput = dataInput,
        normalApproximation = normalApproximation,
        directionUpper = directionUpper,
        firstParameterName = "combFisher",
        tolerance = tolerance,
        ...
    ))
}

.calculateThetaH1 <- function(stageResults, pi1, pi2, stage, kMax, nPlanned, allocationRatioPlanned) {
    # Shifted decision region for use in getGroupSequentialProbabilities
    # Inverse normal method
    condError <- getConditionalRejectionProbabilities(stageResults = stageResults)[stage]

    if (stageResults$isOneSampleDataset()) {
        if (condError < 1e-12) {
            adjustment <- 0
        } else {
            adjustment <- .getOneMinusQNorm(condError) *
                (1 -
                    sqrt(stageResults$thetaH0 * (1 - stageResults$thetaH0)) /
                        sqrt(pi1 * (1 - pi1))) /
                sqrt(sum(nPlanned[(stage + 1):kMax]))
        }

        if (stageResults$direction == "upper") {
            thetaH1 <- (pi1 - stageResults$thetaH0) / sqrt(pi1 * (1 - pi1)) + adjustment
        } else {
            thetaH1 <- -(pi1 - stageResults$thetaH0) / sqrt(pi1 * (1 - pi1)) + adjustment
        }

        return(list(thetaH1 = thetaH1, nPlanned = nPlanned))
    }

    .assertIsSingleNumber(allocationRatioPlanned, "allocationRatioPlanned")
    .assertIsInOpenInterval(
        allocationRatioPlanned,
        "allocationRatioPlanned",
        lower = 0,
        upper = C_ALLOCATION_RATIO_MAXIMUM
    )

    x <- .getFarringtonManningValues(
        rate1 = pi1,
        rate2 = pi2,
        theta = stageResults$thetaH0,
        allocation = allocationRatioPlanned
    )

    if (condError < 1e-12) {
        adjustment <- 0
    } else {
        adjustment <- .getOneMinusQNorm(condError) *
            (1 -
                sqrt(x$ml1 * (1 - x$ml1) + allocationRatioPlanned * x$ml2 * (1 - x$ml2)) /
                    sqrt(pi1 * (1 - pi1) + allocationRatioPlanned * pi2 * (1 - pi2))) *
            (1 + allocationRatioPlanned) /
            sqrt(
                allocationRatioPlanned *
                    sum(nPlanned[(stage + 1):kMax])
            )
    }

    if (stageResults$direction == "upper") {
        thetaH1 <- (pi1 - pi2 - stageResults$thetaH0) /
            sqrt(pi1 * (1 - pi1) + allocationRatioPlanned * pi2 * (1 - pi2)) *
            sqrt(1 + allocationRatioPlanned) +
            adjustment
    } else {
        thetaH1 <- -(pi1 - pi2 - stageResults$thetaH0) /
            sqrt(pi1 * (1 - pi1) + allocationRatioPlanned * pi2 * (1 - pi2)) *
            sqrt(1 + allocationRatioPlanned) +
            adjustment
    }

    nPlanned <- allocationRatioPlanned / (1 + allocationRatioPlanned)^2 * nPlanned
    return(list(thetaH1 = thetaH1, nPlanned = nPlanned))
}

#'
#' Calculation of conditional power based on group sequential method
#'
#' @noRd
#'
.getConditionalPowerRatesGroupSequential <- function(
        ...,
        stageResults,
        stage = stageResults$stage,
        nPlanned,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        pi1,
        pi2) {
    design <- stageResults$.design
    .assertIsTrialDesignInverseNormalOrGroupSequential(design)
    .assertIsValidStage(stage, design$kMax)
    .warnInCaseOfUnknownArguments(
        functionName = ".getConditionalPowerRatesGroupSequential",
        ignore = c("design", "stageResultsName", "grid", "pi1H1", "pi2H1"),
        ...
    )

    kMax <- design$kMax
    conditionalPower <- rep(NA_real_, kMax)
    weights <- stageResults$weightsInverseNormal
    informationRates <- design$informationRates
    nPlanned <- c(rep(NA, stage), nPlanned)
    if (stage == kMax) {
        .logDebug(
            "Conditional power will be calculated only for subsequent stages ",
            "(stage = ",
            stage,
            ", kMax = ",
            design$kMax,
            ")"
        )
        return(list(
            nPlanned = nPlanned,
            conditionalPower = conditionalPower
        ))
    }

    criticalValuesGroupSequential <- .getCriticalValues(design)

    resultList <- .calculateThetaH1(stageResults, pi1, pi2, stage, kMax, nPlanned, allocationRatioPlanned)
    thetaH1 <- resultList$thetaH1
    nPlanned <- resultList$nPlanned

    shiftedDecisionRegionUpper <- criticalValuesGroupSequential[(stage + 1):kMax] *
        sqrt(sum(weights[1:stage]^2) + cumsum(weights[(stage + 1):kMax]^2)) /
        sqrt(cumsum(weights[(stage + 1):kMax]^2)) -
        .getOneMinusQNorm(stageResults$overallPValues[stage]) *
            sqrt(sum(weights[1:stage]^2)) /
            sqrt(cumsum(weights[(stage + 1):kMax]^2)) -
        thetaH1 *
            cumsum(sqrt(nPlanned[(stage + 1):kMax]) * weights[(stage + 1):kMax]) /
            sqrt(cumsum(weights[(stage + 1):kMax]^2))

    if (design$sided == 2) {
        shiftedDecisionRegionLower <- -criticalValuesGroupSequential[(stage + 1):kMax] *
            sqrt(sum(weights[1:stage]^2) + cumsum(weights[(stage + 1):kMax]^2)) /
            sqrt(cumsum(weights[(stage + 1):kMax]^2)) -
            .getOneMinusQNorm(stageResults$overallPValues[stage]) *
                sqrt(sum(weights[1:stage]^2)) /
                sqrt(cumsum(weights[(stage + 1):kMax]^2)) -
            thetaH1 *
                cumsum(sqrt(nPlanned[(stage + 1):kMax]) * weights[(stage + 1):kMax]) /
                sqrt(cumsum(weights[(stage + 1):kMax]^2))
    }
    if (stage == kMax - 1) {
        shiftedFutilityBounds <- c()
    } else {
        shiftedFutilityBounds <- design$futilityBounds[(stage + 1):(kMax - 1)] *
            sqrt(sum(weights[1:stage]^2) + cumsum(weights[(stage + 1):(kMax - 1)]^2)) /
            sqrt(cumsum(weights[(stage + 1):(kMax - 1)]^2)) -
            .getOneMinusQNorm(stageResults$overallPValues[stage]) *
                sqrt(sum(weights[1:stage]^2)) /
                sqrt(cumsum(weights[(stage + 1):(kMax - 1)]^2)) -
            thetaH1 *
                cumsum(sqrt(nPlanned[(stage + 1):(kMax - 1)]) * weights[(stage + 1):(kMax - 1)]) /
                sqrt(cumsum(weights[(stage + 1):(kMax - 1)]^2))
    }

    # Scaled information for use in getGroupSeqProbs
    scaledInformation <- (informationRates[(stage + 1):kMax] - informationRates[stage]) /
        (1 - informationRates[stage])

    if (design$sided == 2) {
        decisionMatrix <- matrix(
            c(
                shiftedDecisionRegionLower,
                shiftedDecisionRegionUpper
            ),
            nrow = 2,
            byrow = TRUE
        )
    } else {
        decisionMatrix <- matrix(
            c(
                shiftedFutilityBounds,
                C_FUTILITY_BOUNDS_DEFAULT,
                shiftedDecisionRegionUpper
            ),
            nrow = 2,
            byrow = TRUE
        )
    }
    probs <- .getGroupSequentialProbabilities(
        decisionMatrix = decisionMatrix,
        informationRates = scaledInformation
    )

    if (design$twoSidedPower) {
        conditionalPower[(stage + 1):kMax] <- cumsum(probs[3, ] - probs[2, ] + probs[1, ])
    } else {
        conditionalPower[(stage + 1):kMax] <- cumsum(probs[3, ] - probs[2, ])
    }

    if (stageResults$isTwoSampleDataset()) {
        nPlanned <- (1 + allocationRatioPlanned)^2 / allocationRatioPlanned * nPlanned
    }

    return(list(
        nPlanned = nPlanned,
        conditionalPower = conditionalPower
    ))
}

#'
#' Calculation of conditional power based on inverse normal method
#'
#' @noRd
#'
.getConditionalPowerRatesInverseNormal <- function(
        ...,
        stageResults,
        stage = stageResults$stage,
        nPlanned,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        pi1,
        pi2) {
    design <- stageResults$.design
    .assertIsTrialDesignInverseNormalOrGroupSequential(design)
    .assertIsValidStage(stage, design$kMax)
    .warnInCaseOfUnknownArguments(
        functionName = ".getConditionalPowerRatesInverseNormal",
        ignore = c("design", "stageResultsName", "grid", "pi1H1", "pi2H1"),
        ...
    )

    kMax <- design$kMax
    conditionalPower <- rep(NA_real_, kMax)
    weights <- stageResults$weightsInverseNormal
    informationRates <- design$informationRates
    nPlanned <- c(rep(NA, stage), nPlanned)
    if (stage == kMax) {
        .logDebug(
            "Conditional power will be calculated only for subsequent stages ",
            "(stage = ",
            stage,
            ", kMax = ",
            design$kMax,
            ")"
        )
        return(list(
            nPlanned = nPlanned,
            conditionalPower = conditionalPower
        ))
    }

    criticalValuesInverseNormal <- .getCriticalValues(design)

    resultList <- .calculateThetaH1(stageResults, pi1, pi2, stage, kMax, nPlanned, allocationRatioPlanned)
    thetaH1 <- resultList$thetaH1
    nPlanned <- resultList$nPlanned

    shiftedDecisionRegionUpper <- criticalValuesInverseNormal[(stage + 1):kMax] *
        sqrt(sum(weights[1:stage]^2) + cumsum(weights[(stage + 1):kMax]^2)) /
        sqrt(cumsum(weights[(stage + 1):kMax]^2)) -
        c(weights[1:stage] %*% .getOneMinusQNorm(stageResults$pValues[1:stage])) /
        sqrt(cumsum(weights[(stage + 1):kMax]^2)) -
        thetaH1 *
            cumsum(sqrt(nPlanned[(stage + 1):kMax]) * weights[(stage + 1):kMax]) /
            sqrt(cumsum(weights[(stage + 1):kMax]^2))

    if (design$sided == 2) {
        shiftedDecisionRegionLower <- -criticalValuesInverseNormal[(stage + 1):kMax] *
            sqrt(sum(weights[1:stage]^2) + cumsum(weights[(stage + 1):kMax]^2)) /
            sqrt(cumsum(weights[(stage + 1):kMax]^2)) -
            c(weights[1:stage] %*% .getOneMinusQNorm(stageResults$pValues[1:stage])) /
            sqrt(cumsum(weights[(stage + 1):kMax]^2)) -
            thetaH1 *
                cumsum(sqrt(nPlanned[(stage + 1):kMax]) * weights[(stage + 1):kMax]) /
                sqrt(cumsum(weights[(stage + 1):kMax]^2))
    }
    if (stage == kMax - 1) {
        shiftedFutilityBounds <- c()
    } else {
        shiftedFutilityBounds <- design$futilityBounds[(stage + 1):(kMax - 1)] *
            sqrt(sum(weights[1:stage]^2) + cumsum(weights[(stage + 1):(kMax - 1)]^2)) /
            sqrt(cumsum(weights[(stage + 1):(kMax - 1)]^2)) -
            c(weights[1:stage] %*% .getOneMinusQNorm(stageResults$pValues[1:stage])) /
            sqrt(cumsum(weights[(stage + 1):(kMax - 1)]^2)) -
            thetaH1 *
                cumsum(sqrt(nPlanned[(stage + 1):(kMax - 1)]) * weights[(stage + 1):(kMax - 1)]) /
                sqrt(cumsum(weights[(stage + 1):(kMax - 1)]^2))
    }

    # Scaled information for use in getGroupSeqProbs
    scaledInformation <- (informationRates[(stage + 1):kMax] - informationRates[stage]) /
        (1 - informationRates[stage])

    if (design$sided == 2) {
        decisionMatrix <- matrix(
            c(
                shiftedDecisionRegionLower,
                shiftedDecisionRegionUpper
            ),
            nrow = 2,
            byrow = TRUE
        )
    } else {
        decisionMatrix <- matrix(
            c(
                shiftedFutilityBounds,
                C_FUTILITY_BOUNDS_DEFAULT,
                shiftedDecisionRegionUpper
            ),
            nrow = 2,
            byrow = TRUE
        )
    }
    probs <- .getGroupSequentialProbabilities(
        decisionMatrix = decisionMatrix,
        informationRates = scaledInformation
    )

    if (design$twoSidedPower) {
        conditionalPower[(stage + 1):kMax] <- cumsum(probs[3, ] - probs[2, ] + probs[1, ])
    } else {
        conditionalPower[(stage + 1):kMax] <- cumsum(probs[3, ] - probs[2, ])
    }

    if (stageResults$isTwoSampleDataset()) {
        nPlanned <- (1 + allocationRatioPlanned)^2 / allocationRatioPlanned * nPlanned
    }

    return(list(
        nPlanned = nPlanned,
        conditionalPower = conditionalPower
    ))
}


#'
#' Calculation of conditional power based on Fisher combination test
#'
#' @noRd
#'
.getConditionalPowerRatesFisher <- function(
        ...,
        stageResults,
        stage = stageResults$stage,
        nPlanned,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        pi1,
        pi2,
        iterations = C_ITERATIONS_DEFAULT,
        seed = NA_real_) {
    design <- stageResults$.design
    .assertIsTrialDesignFisher(design)
    .assertIsValidStage(stage, design$kMax)
    .assertIsValidIterationsAndSeed(iterations, seed, zeroIterationsAllowed = FALSE)
    .warnInCaseOfUnknownArguments(
        functionName = ".getConditionalPowerRatesFisher",
        ignore = c("design", "stageResultsName", "grid", "pi1H1", "pi2H1"),
        ...
    )

    kMax <- design$kMax
    conditionalPower <- rep(NA_real_, kMax)
    seed <- .setSeed(seed)
    simulated <- FALSE
    nPlanned <- c(rep(NA, stage), nPlanned)

    resultList <- .calculateThetaH1(stageResults, pi1, pi2, stage, kMax, nPlanned, allocationRatioPlanned)
    thetaH1 <- resultList$thetaH1
    nPlanned <- resultList$nPlanned

    criticalValues <- .getCriticalValues(design)
    weightsFisher <- stageResults$weightsFisher
    pValues <- stageResults$pValues

    if (stage < kMax - 1) {
        for (k in (stage + 1):kMax) {
            reject <- 0
            for (i in 1:iterations) {
                reject <- reject +
                    .getRejectValueConditionalPowerFisher(
                        kMax = kMax,
                        alpha0Vec = design$alpha0Vec,
                        criticalValues = criticalValues,
                        weightsFisher = weightsFisher,
                        pValues = pValues,
                        currentKMax = k,
                        thetaH1 = thetaH1,
                        stage = stage,
                        nPlanned = nPlanned
                    )
            }
            conditionalPower[k] <- reject / iterations
        }
        simulated <- TRUE
    }

    if (stage == kMax - 1) {
        divisor <- prod(pValues[1:(kMax - 1)]^weightsFisher[1:(kMax - 1)])
        result <- 1 - (criticalValues[kMax] / divisor)^(1 / weightsFisher[kMax])
        if (result <= 0 || result >= 1) {
            warning("Calculation not possible: could not calculate conditional power for stage ", kMax, call. = FALSE)
            conditionalPower[kMax] <- NA_real_
        } else {
            conditionalPower[kMax] <- 1 - stats::pnorm(.getQNorm(result) - thetaH1 * sqrt(nPlanned[kMax]))
        }
    }

    if (stageResults$isTwoSampleDataset()) {
        nPlanned <- (1 + allocationRatioPlanned)^2 / allocationRatioPlanned * nPlanned
    }

    return(list(
        nPlanned = nPlanned,
        conditionalPower = conditionalPower,
        iterations = as.integer(iterations),
        seed = seed,
        simulated = simulated
    ))
}

.getConditionalPowerRates <- function(
        ...,
        stageResults,
        nPlanned,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        pi1 = NA_real_,
        pi2 = NA_real_) {
    pi1H1 <- .getOptionalArgument("pi1H1", ...)
    if (!is.null(pi1H1) && !is.na(pi1H1)) {
        if (!is.na(pi1)) {
            warning(sQuote("pi1"), " will be ignored because ", sQuote("pi1H1"), " is defined", call. = FALSE)
        }
        pi1 <- pi1H1
    }

    pi2H1 <- .getOptionalArgument("pi2H1", ...)
    if (!is.null(pi2H1) && !is.na(pi2H1)) {
        if (!is.na(pi2)) {
            warning(sQuote("pi2"), " will be ignored because ", sQuote("pi2H1"), " is defined", call. = FALSE)
        }
        pi2 <- pi2H1
    }

    stage <- stageResults$stage
    pi1 <- .assertIsValidPi1(pi1, stageResults, stage)

    if (!stageResults$isOneSampleDataset()) {
        pi2 <- .assertIsValidPi2(pi2, stageResults, stage)
    }

    results <- ConditionalPowerResultsRates$new(
        # R6$new
        .stageResults = stageResults,
        .design = stageResults$.design,
        nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned,
        pi1 = pi1,
        pi2 = pi2
    )

    if (any(is.na(nPlanned))) {
        return(results)
    }

    if (!.isValidNPlanned(nPlanned = nPlanned, kMax = stageResults$.design$kMax, stage = stage)) {
        return(results)
    }

    if (.isTrialDesignGroupSequential(stageResults$.design)) {
        cp <- .getConditionalPowerRatesInverseNormal(
            ...,
            stageResults = stageResults,
            nPlanned = nPlanned,
            allocationRatioPlanned = allocationRatioPlanned,
            pi1 = pi1,
            pi2 = pi2
        )
    } else if (.isTrialDesignInverseNormal(stageResults$.design)) {
        cp <- .getConditionalPowerRatesInverseNormal(
            ...,
            stageResults = stageResults,
            nPlanned = nPlanned,
            allocationRatioPlanned = allocationRatioPlanned,
            pi1 = pi1,
            pi2 = pi2
        )
    } else if (.isTrialDesignFisher(stageResults$.design)) {
        cp <- .getConditionalPowerRatesFisher(
            ...,
            stageResults = stageResults,
            nPlanned = nPlanned,
            allocationRatioPlanned = allocationRatioPlanned,
            pi1 = pi1,
            pi2 = pi2
        )
        results$iterations <- cp$iterations
        results$seed <- cp$seed
        results$simulated <- cp$simulated
        .updateParameterTypeOfIterationsAndSeed(results, ...)
    } else {
        .stopWithWrongDesignMessage(stageResults$.design, inclusiveConditionalDunnett = FALSE)
    }

    results$nPlanned <- cp$nPlanned
    results$conditionalPower <- cp$conditionalPower
    results$.setParameterType("nPlanned", C_PARAM_GENERATED)
    results$.setParameterType("conditionalPower", C_PARAM_GENERATED)
    results$.setParameterType(
        "allocationRatioPlanned",
        ifelse(allocationRatioPlanned == C_ALLOCATION_RATIO_DEFAULT, C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED)
    )
    results$.setParameterType("pi1", ifelse(is.na(pi1), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))
    results$.setParameterType("pi2", ifelse(is.na(pi2), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))

    return(results)
}

.getConditionalPowerPlotRates <- function(
        ...,
        stageResults,
        stage,
        nPlanned,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        piTreatmentRange,
        pi2) {
    if (stageResults$isOneSampleDataset()) {
        .associatedArgumentsAreDefined(
            nPlanned = nPlanned,
            piTreatmentRange = piTreatmentRange
        )
        pi2 <- NA_real_
    } else {
        .associatedArgumentsAreDefined(
            nPlanned = nPlanned,
            pi2 = pi2,
            piTreatmentRange = piTreatmentRange
        )
    }

    .assertIsValidAllocationRatioPlanned(
        allocationRatioPlanned,
        stageResults$getDataInput()$getNumberOfGroups()
    )
    .assertIsValidPi(pi2, "pi2")
    piTreatmentRange <- .assertIsValidPiTreatmentRange(piTreatmentRange = piTreatmentRange)

    .warnInCaseOfUnknownArguments(
        functionName = ".getConditionalPowerPlotRates",
        ignore = c("iterations", "seed", "stageResultsName", "grid"),
        ...
    )

    condPowerValues <- rep(NA, length(piTreatmentRange))
    likelihoodValues <- rep(NA, length(piTreatmentRange))

    design <- stageResults$.design

    warningMessages <- c()
    withCallingHandlers(
        if (stageResults$isOneSampleDataset()) {
            mu <- stageResults$effectSizes[stage]
            stdErr <- sqrt(
                stageResults$effectSizes[stage] *
                    (1 - stageResults$effectSizes[stage]) /
                    stageResults$overallSampleSizes[stage]
            )

            for (i in seq(along = piTreatmentRange)) {
                if (.isTrialDesignGroupSequential(design)) {
                    condPowerValues[i] <- .getConditionalPowerRatesGroupSequential(
                        stageResults = stageResults,
                        nPlanned = nPlanned,
                        allocationRatioPlanned = allocationRatioPlanned,
                        pi1 = piTreatmentRange[i],
                        pi2 = pi2
                    )$conditionalPower[design$kMax]
                } else if (.isTrialDesignInverseNormal(design)) {
                    condPowerValues[i] <- .getConditionalPowerRatesInverseNormal(
                        stageResults = stageResults,
                        nPlanned = nPlanned,
                        allocationRatioPlanned = allocationRatioPlanned,
                        pi1 = piTreatmentRange[i],
                        pi2 = pi2
                    )$conditionalPower[design$kMax]
                } else if (.isTrialDesignFisher(design)) {
                    condPowerValues[i] <- .getConditionalPowerRatesFisher(
                        stageResults = stageResults,
                        nPlanned = nPlanned,
                        allocationRatioPlanned = allocationRatioPlanned,
                        pi1 = piTreatmentRange[i],
                        pi2 = pi2
                    )$conditionalPower[design$kMax]
                }
                likelihoodValues[i] <- stats::dnorm(piTreatmentRange[i], mu, stdErr) / stats::dnorm(0, 0, stdErr)
            }
        },
        warning = function(w) {
            m <- w$message
            if (!(m %in% warningMessages)) {
                warningMessages <<- c(warningMessages, m)
            }
            invokeRestart("muffleWarning")
        },
        error = function(e) {
            e
        }
    )

    if (stageResults$isTwoSampleDataset()) {
        mu <- stageResults$overallEvents1[stage] / stageResults$overallSampleSizes1[stage]
        stdErr <- sqrt(
            stageResults$overallEvents1[stage] /
                stageResults$overallSampleSizes1[stage] *
                (1 - stageResults$overallEvents1[stage] / stageResults$overallSampleSizes1[stage]) /
                stageResults$overallSampleSizes1[stage]
        )

        withCallingHandlers(
            for (i in seq(along = piTreatmentRange)) {
                if (.isTrialDesignGroupSequential(design)) {
                    condPowerValues[i] <- .getConditionalPowerRatesGroupSequential(
                        stageResults = stageResults,
                        nPlanned = nPlanned,
                        allocationRatioPlanned = allocationRatioPlanned,
                        pi1 = piTreatmentRange[i],
                        pi2 = pi2
                    )$conditionalPower[design$kMax]
                } else if (.isTrialDesignInverseNormal(design)) {
                    condPowerValues[i] <- .getConditionalPowerRatesInverseNormal(
                        stageResults = stageResults,
                        nPlanned = nPlanned,
                        allocationRatioPlanned = allocationRatioPlanned,
                        pi1 = piTreatmentRange[i],
                        pi2 = pi2
                    )$conditionalPower[design$kMax]
                } else if (.isTrialDesignFisher(design)) {
                    condPowerValues[i] <- .getConditionalPowerRatesFisher(
                        stageResults = stageResults,
                        stage = stage,
                        nPlanned = nPlanned,
                        allocationRatioPlanned = allocationRatioPlanned,
                        pi1 = piTreatmentRange[i],
                        pi2 = pi2
                    )$conditionalPower[design$kMax]
                }
                likelihoodValues[i] <- stats::dnorm(piTreatmentRange[i], mu, stdErr) / stats::dnorm(0, 0, stdErr)
            },
            warning = function(w) {
                m <- w$message
                if (!(m %in% warningMessages)) {
                    warningMessages <<- c(warningMessages, m)
                }
                invokeRestart("muffleWarning")
            },
            error = function(e) {
                e
            }
        )
    }

    if (length(warningMessages) > 0) {
        for (m in warningMessages) {
            warning(m, call. = FALSE)
        }
    }

    if (stageResults$isOneSampleDataset()) {
        subtitle <- paste0("Stage = ", stage, ", # of remaining subjects = ", sum(nPlanned))
    } else {
        subtitle <- paste0(
            "Stage = ",
            stage,
            ", # of remaining subjects = ",
            sum(nPlanned),
            ", pi2 = ",
            .formatSubTitleValue(pi2, "pi2"),
            ", allocation ratio = ",
            .formatSubTitleValue(allocationRatioPlanned, "allocationRatioPlanned")
        )
    }

    return(list(
        xValues = piTreatmentRange,
        condPowerValues = condPowerValues,
        likelihoodValues = likelihoodValues,
        main = C_PLOT_MAIN_CONDITIONAL_POWER_WITH_LIKELIHOOD,
        xlab = "pi1",
        ylab = C_PLOT_YLAB_CONDITIONAL_POWER_WITH_LIKELIHOOD,
        sub = subtitle
    ))
}


.getFinalConfidenceIntervalRatesValues <- function(
        design,
        dataInput,
        stageResults,
        directionUpper,
        normalApproximation,
        thetaH0,
        stage,
        stdErrorEstimate,
        tolerance) {
    finalConfidenceIntervalGeneral <- rep(NA_real_, 2)
    medianUnbiasedGeneral <- NA_real_

    finalConfidenceInterval <- rep(NA_real_, 2)
    medianUnbiased <- NA_real_

    if (.isTrialDesignGroupSequential(design)) {
        designStage <- .getStageGroupSeq(design = design, stageResults = stageResults, stage = stage)
    } else {
        designStage <- .getStageInverseNormal(design = design, stageResults = stageResults, stage = stage)
    }

    finalStage <- min(designStage, design$kMax)

    firstParameterName <- ifelse(.isTrialDesignGroupSequential(design), "overallPValues", "combInverseNormal")

    # No early efficacy stopping design
    if (stage == design$kMax && .isNoEarlyEfficacy(design)) {
        if (.isTrialDesignGroupSequential(design)) {
            finalConfidenceInterval <- .getRepeatedConfidenceIntervalsRatesGroupSequential(
                design = design,
                dataInput = dataInput,
                normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT,
                directionUpper = directionUpper,
                tolerance = C_ANALYSIS_TOLERANCE_DEFAULT
            )[, design$kMax]
        } else {
            finalConfidenceInterval <- .getRepeatedConfidenceIntervalsRatesInverseNormal(
                design = design,
                dataInput = dataInput,
                normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT,
                directionUpper = directionUpper,
                tolerance = C_ANALYSIS_TOLERANCE_DEFAULT
            )[, design$kMax]
        }
        medianUnbiased <- (finalConfidenceInterval[1] + finalConfidenceInterval[2]) / 2
    } else if (designStage < design$kMax || stage == design$kMax) {
        # early stopping or at end of study
        if (designStage == 1) {
            finalConfidenceIntervalGeneral[1] <- stageResults$overallTestStatistics[1] -
                .getOneMinusQNorm(design$alpha / design$sided)
            finalConfidenceIntervalGeneral[2] <- stageResults$overallTestStatistics[1] +
                .getOneMinusQNorm(design$alpha / design$sided)
            medianUnbiasedGeneral <- stageResults$overallTestStatistics[1]

            if (dataInput$getNumberOfGroups() == 1) {
                finalConfidenceIntervalGeneral <- finalConfidenceIntervalGeneral /
                    sqrt(stageResults$overallSampleSizes[1])
                medianUnbiasedGeneral <- medianUnbiasedGeneral /
                    sqrt(stageResults$overallSampleSizes[1])
            } else {
                finalConfidenceIntervalGeneral <- finalConfidenceIntervalGeneral *
                    sqrt(
                        1 /
                            stageResults$overallSampleSizes1[finalStage] +
                            1 / stageResults$overallSampleSizes2[finalStage]
                    )
                medianUnbiasedGeneral <- medianUnbiasedGeneral *
                    sqrt(
                        1 /
                            stageResults$overallSampleSizes1[finalStage] +
                            1 / stageResults$overallSampleSizes2[finalStage]
                    )
            }
        } else {
            if (.isTrialDesignInverseNormal(design) && design$kMax > 2 && !.isNoEarlyEfficacy(design)) {
                message(
                    "Calculation of final confidence interval performed for kMax = ",
                    design$kMax,
                    " (for kMax > 2, it is theoretically shown that it is valid only ",
                    "if no sample size change was performed)"
                )
            }

            finalConfidenceIntervalGeneral[1] <- .getDecisionMatrixRoot(
                design = design,
                stage = finalStage,
                stageResults = stageResults,
                tolerance = tolerance,
                firstParameterName = firstParameterName,
                case = "finalConfidenceIntervalGeneralLower"
            )

            finalConfidenceIntervalGeneral[2] <- .getDecisionMatrixRoot(
                design = design,
                stage = finalStage,
                stageResults = stageResults,
                tolerance = tolerance,
                firstParameterName = firstParameterName,
                case = "finalConfidenceIntervalGeneralUpper"
            )

            medianUnbiasedGeneral <- .getDecisionMatrixRoot(
                design = design,
                stage = finalStage,
                stageResults = stageResults,
                tolerance = tolerance,
                firstParameterName = firstParameterName,
                case = "medianUnbiasedGeneral"
            )
        }

        if (any(is.na(finalConfidenceIntervalGeneral)) && (designStage > 1)) {
            finalStage <- NA_integer_
        }

        if (!is.na(finalStage)) {
            if (dataInput$getNumberOfGroups() == 1) {
                stErrRates <- sqrt(
                    stageResults$overallEvents[finalStage] /
                        stageResults$overallSampleSizes[finalStage] *
                        (1 - stageResults$overallEvents[finalStage] / stageResults$overallSampleSizes[finalStage])
                ) /
                    sqrt(stageResults$overallSampleSizes[finalStage])
            } else {
                if (identical(stdErrorEstimate, "unpooled")) {
                    stErrRates <- sqrt(
                        stageResults$overallEvents1[finalStage] /
                            stageResults$overallSampleSizes1[finalStage] *
                            (1 -
                                stageResults$overallEvents1[finalStage] /
                                    stageResults$overallSampleSizes1[finalStage]) /
                            stageResults$overallSampleSizes1[finalStage] +
                            stageResults$overallEvents2[finalStage] /
                                stageResults$overallSampleSizes2[finalStage] *
                                (1 -
                                    stageResults$overallEvents2[finalStage] /
                                        stageResults$overallSampleSizes2[finalStage]) /
                                stageResults$overallSampleSizes2[finalStage]
                    )
                } else {
                    piPooled <- (stageResults$overallEvents1[finalStage] + stageResults$overallEvents2[finalStage]) /
                        (stageResults$overallSampleSizes1[finalStage] + stageResults$overallSampleSizes2[finalStage])
                    stErrRates <- sqrt(
                        piPooled *
                            (1 - piPooled) *
                            (1 /
                                stageResults$overallSampleSizes1[finalStage] +
                                1 / stageResults$overallSampleSizes2[finalStage])
                    )
                }
            }

            finalConfidenceInterval <- rep(NA_real_, 2)
            medianUnbiased <- NA_real_
            if (designStage == 1) {
                args <- list(
                    design = design,
                    dataInput = dataInput,
                    stage = 1,
                    directionUpper = TRUE,
                    normalApproximation = normalApproximation,
                    firstParameterName = firstParameterName,
                    secondValue = .getOneMinusQNorm(design$alpha / design$sided),
                    tolerance = tolerance,
                    acceptResultsOutOfTolerance = TRUE,
                    callingFunctionInformation = "Final confidence interval [1]"
                )
                finalConfidenceInterval[1] <- do.call(.getRootThetaRates, args)
                args$directionUpper <- FALSE
                args$callingFunctionInformation <- "Final confidence interval [2]"
                finalConfidenceInterval[2] <- do.call(.getRootThetaRates, args)
                medianUnbiased <- stageResults$effectSizes[1]
            } else {
                directionUpperSign <- ifelse(!isFALSE(directionUpper), 1, -1)
                if (dataInput$getNumberOfGroups() == 1) {
                    finalConfidenceInterval[1] <- finalConfidenceIntervalGeneral[1] *
                        sqrt(stageResults$overallSampleSizes[finalStage]) *
                        stErrRates +
                        directionUpperSign * thetaH0
                    finalConfidenceInterval[2] <- finalConfidenceIntervalGeneral[2] *
                        sqrt(stageResults$overallSampleSizes[finalStage]) *
                        stErrRates +
                        directionUpperSign * thetaH0
                    medianUnbiased <- medianUnbiasedGeneral *
                        sqrt(stageResults$overallSampleSizes[finalStage]) *
                        stErrRates +
                        directionUpperSign * thetaH0
                } else {
                    finalConfidenceInterval[1] <- finalConfidenceIntervalGeneral[1] /
                        sqrt(
                            1 /
                                stageResults$overallSampleSizes1[finalStage] +
                                1 / stageResults$overallSampleSizes2[finalStage]
                        ) *
                        stErrRates +
                        directionUpperSign * thetaH0
                    finalConfidenceInterval[2] <- finalConfidenceIntervalGeneral[2] /
                        sqrt(
                            1 /
                                stageResults$overallSampleSizes1[finalStage] +
                                1 / stageResults$overallSampleSizes2[finalStage]
                        ) *
                        stErrRates +
                        directionUpperSign * thetaH0
                    medianUnbiased <- medianUnbiasedGeneral /
                        sqrt(
                            1 /
                                stageResults$overallSampleSizes1[finalStage] +
                                1 / stageResults$overallSampleSizes2[finalStage]
                        ) *
                        stErrRates +
                        directionUpperSign * thetaH0
                }
            }
        }

        if (isFALSE(directionUpper)) {
            medianUnbiasedGeneral <- -medianUnbiasedGeneral
            finalConfidenceIntervalGeneral <- -finalConfidenceIntervalGeneral
            if (designStage > 1) {
                medianUnbiased <- -medianUnbiased
                finalConfidenceInterval <- -finalConfidenceInterval
            }
        }
    }

    if (!any(is.na(finalConfidenceIntervalGeneral))) {
        finalConfidenceIntervalGeneral <- sort(finalConfidenceIntervalGeneral)
    }
    if (!any(is.na(finalConfidenceInterval))) {
        finalConfidenceInterval <- sort(finalConfidenceInterval)
    }

    if (dataInput$getNumberOfGroups() == 1) {
        finalConfidenceInterval[1] <- max(0, finalConfidenceInterval[1])
        finalConfidenceInterval[2] <- min(1, finalConfidenceInterval[2])
    } else {
        finalConfidenceInterval[1] <- max(-1, finalConfidenceInterval[1])
        finalConfidenceInterval[2] <- min(1, finalConfidenceInterval[2])
    }

    return(list(
        stage = stage,
        thetaH0 = thetaH0,
        directionUpper = directionUpper,
        normalApproximation = normalApproximation,
        tolerance = tolerance,
        stdErrorEstimate = stdErrorEstimate,
        finalStage = finalStage,
        medianUnbiasedGeneral = medianUnbiasedGeneral,
        finalConfidenceIntervalGeneral = finalConfidenceIntervalGeneral,
        medianUnbiased = medianUnbiased,
        finalConfidenceInterval = finalConfidenceInterval
    ))
}


#'
#' Calculation of final confidence interval
#' based on group sequential test without SSR (general case).
#'
#' @noRd
#'
.getFinalConfidenceIntervalRatesGroupSequential <- function(
        ...,
        design,
        dataInput,
        stage,
        thetaH0 = C_THETA_H0_RATES_DEFAULT,
        directionUpper = NA,
        normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT,
        stdErrorEstimate = NA_character_,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    stageResults <- .getStageResultsRates(
        design = design,
        dataInput = dataInput,
        stage = stage,
        thetaH0 = thetaH0,
        directionUpper = directionUpper,
        normalApproximation = normalApproximation
    )

    finalConfidenceIntervalRatesValues <- .getFinalConfidenceIntervalRatesValues(
        design,
        dataInput,
        stageResults,
        directionUpper,
        normalApproximation,
        thetaH0,
        stage,
        stdErrorEstimate,
        tolerance
    )

    return(list(
        stage = stage,
        thetaH0 = thetaH0,
        directionUpper = directionUpper,
        normalApproximation = normalApproximation,
        tolerance = tolerance,
        stdErrorEstimate = stdErrorEstimate,
        finalStage = finalConfidenceIntervalRatesValues$finalStage,
        medianUnbiasedGeneral = finalConfidenceIntervalRatesValues$medianUnbiasedGeneral,
        finalConfidenceIntervalGeneral = finalConfidenceIntervalRatesValues$finalConfidenceIntervalGeneral,
        medianUnbiased = finalConfidenceIntervalRatesValues$medianUnbiased,
        finalConfidenceInterval = finalConfidenceIntervalRatesValues$finalConfidenceInterval
    ))
}


#'
#' Calculation of final confidence interval
#' based on inverse normal method, only valid for kMax <= 2 or no SSR.
#'
#' @noRd
#'
.getFinalConfidenceIntervalRatesInverseNormal <- function(
        ...,
        design,
        dataInput,
        stage,
        thetaH0 = C_THETA_H0_RATES_DEFAULT,
        directionUpper = NA,
        normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT,
        stdErrorEstimate = NA_character_,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    stageResults <- .getStageResultsRates(
        design = design,
        dataInput = dataInput,
        stage = stage,
        thetaH0 = thetaH0,
        directionUpper = directionUpper,
        normalApproximation = normalApproximation
    )

    finalConfidenceIntervalRatesValues <- .getFinalConfidenceIntervalRatesValues(
        design,
        dataInput,
        stageResults,
        directionUpper,
        normalApproximation,
        thetaH0,
        stage,
        stdErrorEstimate,
        tolerance
    )

    return(list(
        stage = stage,
        thetaH0 = thetaH0,
        directionUpper = directionUpper,
        normalApproximation = normalApproximation,
        tolerance = tolerance,
        stdErrorEstimate = stdErrorEstimate,
        finalStage = finalConfidenceIntervalRatesValues$finalStage,
        medianUnbiasedGeneral = finalConfidenceIntervalRatesValues$medianUnbiasedGeneral,
        finalConfidenceIntervalGeneral = finalConfidenceIntervalRatesValues$finalConfidenceIntervalGeneral,
        medianUnbiased = finalConfidenceIntervalRatesValues$medianUnbiased,
        finalConfidenceInterval = finalConfidenceIntervalRatesValues$finalConfidenceInterval
    ))
}


#'
#' Calculation of final confidence interval
#' based on Fisher combination test, only valid for kMax <= 2.
#'
#' @noRd
#'
.getFinalConfidenceIntervalRatesFisher <- function(
        ...,
        design,
        dataInput,
        stage,
        thetaH0 = C_THETA_H0_RATES_DEFAULT,
        directionUpper = NA,
        normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT,
        stdErrorEstimate = NA_character_,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    stageResults <- .getStageResultsRates(
        design = design,
        dataInput = dataInput,
        stage = stage,
        thetaH0 = thetaH0,
        directionUpper = directionUpper,
        normalApproximation = normalApproximation
    )

    stageFisher <- .getStageFisher(
        design = design,
        stageResults = stageResults,
        stage = stage
    )

    finalStage <- min(stageFisher, design$kMax)

    message(
        "Calculation of final confidence interval for Fisher's ",
        "design not implemented yet"
    )

    return(list(
        stage = stage,
        thetaH0 = thetaH0,
        directionUpper = directionUpper,
        normalApproximation = normalApproximation,
        tolerance = tolerance,
        finalStage = finalStage,
        medianUnbiasedGeneral = NA_real_,
        finalConfidenceIntervalGeneral = rep(NA_real_, 2),
        medianUnbiased = NA_real_,
        finalConfidenceInterval = rep(NA_real_, 2),
        stdErrorEstimate = NA_real_
    ))
}

.getFinalConfidenceIntervalRates <- function(
        ...,
        design,
        dataInput,
        thetaH0 = NA_real_,
        directionUpper = NA,
        normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT,
        stdErrorEstimate = NA_character_,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)

    .assertIsValidThetaH0DataInput(thetaH0, dataInput)
    .warnInCaseOfUnknownArguments(
        functionName = "getFinalConfidenceIntervalRates",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design, powerCalculationEnabled = TRUE), "stage"),
        ...
    )

    if (design$kMax == 1) {
        return(list(
            finalStage = NA_integer_,
            medianUnbiasedGeneral = NA_real_,
            finalConfidenceIntervalGeneral = c(NA_real_, NA_real_),
            medianUnbiased = NA_real_,
            finalConfidenceInterval = c(NA_real_)
        ))
    }

    if (is.na(thetaH0)) {
        thetaH0 <- C_THETA_H0_RATES_DEFAULT
    }

    if (.isTrialDesignGroupSequential(design)) {
        return(.getFinalConfidenceIntervalRatesGroupSequential(
            design = design,
            dataInput = dataInput,
            stage = stage,
            thetaH0 = thetaH0,
            directionUpper = directionUpper,
            normalApproximation = normalApproximation,
            stdErrorEstimate = stdErrorEstimate,
            tolerance = tolerance
        ))
    }

    if (.isTrialDesignInverseNormal(design)) {
        return(.getFinalConfidenceIntervalRatesInverseNormal(
            design = design,
            dataInput = dataInput,
            stage = stage,
            thetaH0 = thetaH0,
            directionUpper = directionUpper,
            normalApproximation = normalApproximation,
            stdErrorEstimate = stdErrorEstimate,
            tolerance = tolerance
        ))
    }

    if (.isTrialDesignFisher(design)) {
        return(.getFinalConfidenceIntervalRatesFisher(
            design = design,
            dataInput = dataInput,
            stage = stage,
            thetaH0 = thetaH0,
            directionUpper = directionUpper,
            normalApproximation = normalApproximation,
            stdErrorEstimate = stdErrorEstimate,
            tolerance = tolerance
        ))
    }

    .stopWithWrongDesignMessage(design, inclusiveConditionalDunnett = FALSE)
}
