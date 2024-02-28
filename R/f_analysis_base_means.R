## |
## |  *Analysis of means with group sequential and combination test*
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
## |  File version: $Revision: 7147 $
## |  Last changed: $Date: 2023-07-03 08:10:31 +0200 (Mo, 03 Jul 2023) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_logger.R
NULL

.getAnalysisResultsMeans <- function(..., design, dataInput) {
    if (.isTrialDesignGroupSequential(design)) {
        return(.getAnalysisResultsMeansGroupSequential(
            design = design,
            dataInput = dataInput, ...
        ))
    }

    if (.isTrialDesignInverseNormal(design)) {
        return(.getAnalysisResultsMeansInverseNormal(
            design = design,
            dataInput = dataInput, ...
        ))
    }

    if (.isTrialDesignFisher(design)) {
        return(.getAnalysisResultsMeansFisher(
            design = design,
            dataInput = dataInput, ...
        ))
    }

    .stopWithWrongDesignMessage(design, inclusiveConditionalDunnett = FALSE)
}

.getAnalysisResultsMeansInverseNormal <- function(...,
        design, dataInput, directionUpper = C_DIRECTION_UPPER_DEFAULT,
        normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT,
        equalVariances = C_EQUAL_VARIANCES_DEFAULT,
        thetaH0 = C_THETA_H0_MEANS_DEFAULT, thetaH1 = NA_real_,
        nPlanned = NA_real_, assumedStDev = NA_real_,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .assertIsTrialDesignInverseNormal(design)
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    .warnInCaseOfUnknownArguments(
        functionName = ".getAnalysisResultsMeansInverseNormal",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), "stage"), ...
    )

    results <- AnalysisResultsInverseNormal$new(design = design, dataInput = dataInput)

    .getAnalysisResultsMeansAll(
        results = results, design = design, dataInput = dataInput,
        stage = stage, directionUpper = directionUpper, normalApproximation = normalApproximation,
        equalVariances = equalVariances, thetaH0 = thetaH0, thetaH1 = thetaH1, nPlanned = nPlanned,
        assumedStDev = assumedStDev, allocationRatioPlanned = allocationRatioPlanned,
        tolerance = tolerance
    )

    return(results)
}

.getAnalysisResultsMeansGroupSequential <- function(...,
        design, dataInput, directionUpper = C_DIRECTION_UPPER_DEFAULT,
        normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT, equalVariances = C_EQUAL_VARIANCES_DEFAULT,
        thetaH0 = C_THETA_H0_MEANS_DEFAULT, thetaH1 = NA_real_, nPlanned = NA_real_, assumedStDev = NA_real_,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .assertIsTrialDesignGroupSequential(design)
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    .warnInCaseOfUnknownArguments(
        functionName = ".getAnalysisResultsMeansGroupSequential",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), c("stage", "stDevH1")), ...
    )

    results <- AnalysisResultsGroupSequential$new(design = design, dataInput = dataInput)

    stDevH1 <- .getOptionalArgument("stDevH1", ...)
    if (!is.null(stDevH1)) {
        .assertIsSingleNumber(assumedStDev, "assumedStDev", naAllowed = TRUE)
        if (!is.na(assumedStDev)) {
            if (!identical(assumedStDev, stDevH1)) {
                stop(C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS, "either 'assumedStDev' or 'stDevH1' must be defined")
            }
        }
        assumedStDev <- stDevH1
    }

    .getAnalysisResultsMeansAll(
        results = results, design = design, dataInput = dataInput,
        stage = stage, directionUpper = directionUpper, normalApproximation = normalApproximation,
        equalVariances = equalVariances, thetaH0 = thetaH0, thetaH1 = thetaH1, nPlanned = nPlanned,
        assumedStDev = assumedStDev, allocationRatioPlanned = allocationRatioPlanned,
        tolerance = tolerance
    )

    return(results)
}

.getAnalysisResultsMeansFisher <- function(...,
        design, dataInput, directionUpper = C_DIRECTION_UPPER_DEFAULT,
        normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT, equalVariances = C_EQUAL_VARIANCES_DEFAULT,
        thetaH0 = C_THETA_H0_MEANS_DEFAULT, thetaH1 = NA_real_, nPlanned = NA_real_, assumedStDev = NA_real_,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT,
        iterations = C_ITERATIONS_DEFAULT, seed = NA_real_) {
    .assertIsTrialDesignFisher(design)
    .assertIsValidIterationsAndSeed(iterations, seed, zeroIterationsAllowed = FALSE)
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    .warnInCaseOfUnknownArguments(
        functionName = ".getAnalysisResultsMeansFisher",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), "stage"), ...
    )

    results <- AnalysisResultsFisher$new(design = design, dataInput = dataInput)
    .setValueAndParameterType(results, "iterations", as.integer(iterations), C_ITERATIONS_DEFAULT)
    .setValueAndParameterType(results, "seed", seed, NA_real_)

    .getAnalysisResultsMeansAll(
        results = results, design = design, dataInput = dataInput,
        stage = stage, directionUpper = directionUpper, normalApproximation = normalApproximation,
        equalVariances = equalVariances, thetaH0 = thetaH0, thetaH1 = thetaH1, nPlanned = nPlanned,
        assumedStDev = assumedStDev, allocationRatioPlanned = allocationRatioPlanned,
        tolerance = tolerance, iterations = iterations,
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
.getAnalysisResultsMeansAll <- function(..., results, design, dataInput, stage,
        directionUpper, normalApproximation = normalApproximation,
        equalVariances = equalVariances, thetaH0, thetaH1, assumedStDev,
        nPlanned, allocationRatioPlanned, tolerance,
        iterations, seed) {
    startTime <- Sys.time()
    .assertIsValidTolerance(tolerance)
    stageResults <- .getStageResultsMeans(
        design = design, dataInput = dataInput, stage = stage,
        thetaH0 = thetaH0, directionUpper = directionUpper,
        normalApproximation = normalApproximation, equalVariances = equalVariances
    )
    results$.setStageResults(stageResults)
    .logProgress("Stage results calculated", startTime = startTime)

    assumedStDev <- .assertIsValidAssumedStDev(assumedStDev, stageResults, stage, results = results)
    thetaH1 <- .assertIsValidThetaH1(thetaH1, stageResults, stage, results = results)

    .setValueAndParameterType(results, "thetaH0", thetaH0, C_THETA_H0_MEANS_DEFAULT)
    .setValueAndParameterType(results, "directionUpper", directionUpper, C_DIRECTION_UPPER_DEFAULT)
    .setValueAndParameterType(
        results, "normalApproximation",
        normalApproximation, C_NORMAL_APPROXIMATION_MEANS_DEFAULT
    )
    if (stageResults$isTwoSampleDataset()) {
        .setValueAndParameterType(results, "equalVariances", equalVariances, C_EQUAL_VARIANCES_DEFAULT)
    } else {
        results$.setParameterType("equalVariances", C_PARAM_NOT_APPLICABLE)
    }
    .setConditionalPowerArguments(results, dataInput, nPlanned, allocationRatioPlanned)
    .setNPlannedAndThetaH1AndAssumedStDev(results, nPlanned, thetaH1, assumedStDev)

    # test actions
    results$testActions <- getTestActions(stageResults = stageResults)
    results$.setParameterType("testActions", C_PARAM_GENERATED)

    if (design$kMax > 1) {
        # conditional power
        startTime <- Sys.time()
        if (.isTrialDesignFisher(design)) {
            results$.conditionalPowerResults <- .getConditionalPowerMeans(
                stageResults = stageResults,
                nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned,
                assumedStDev = assumedStDev, thetaH1 = thetaH1,
                iterations = iterations, seed = seed
            )
            .synchronizeIterationsAndSeed(results)
        } else {
            results$.conditionalPowerResults <- .getConditionalPowerMeans(
                stageResults = stageResults,
                nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned,
                assumedStDev = assumedStDev, thetaH1 = thetaH1
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
                stageResults = stageResults, iterations = iterations, seed = seed
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
    repeatedConfidenceIntervals <- .getRepeatedConfidenceIntervalsMeans(
        design = design, dataInput = dataInput, stage = stage,
        normalApproximation = normalApproximation, equalVariances = equalVariances,
        tolerance = tolerance
    )
    results$repeatedConfidenceIntervalLowerBounds <- repeatedConfidenceIntervals[1, ]
    results$repeatedConfidenceIntervalUpperBounds <- repeatedConfidenceIntervals[2, ]
    .logProgress("Repeated confidence interval calculated", startTime = startTime)

    # repeated p-value
    startTime <- Sys.time()
    results$repeatedPValues <- getRepeatedPValues(
        stageResults = stageResults, tolerance = tolerance
    )
    .logProgress("Repeated p-values calculated", startTime = startTime)

    results$.setParameterType("repeatedConfidenceIntervalLowerBounds", C_PARAM_GENERATED)
    results$.setParameterType("repeatedConfidenceIntervalUpperBounds", C_PARAM_GENERATED)
    results$.setParameterType("repeatedPValues", C_PARAM_GENERATED)

    if (design$kMax > 1) {
        startTime <- Sys.time()

        # final p-value
        finalPValue <- getFinalPValue(stageResults, showWarnings = FALSE)
        results$finalPValues <- .getVectorWithFinalValueAtFinalStage(
            kMax = design$kMax,
            finalValue = finalPValue$pFinal, finalStage = finalPValue$finalStage
        )
        results$finalStage <- finalPValue$finalStage
        results$.setParameterType("finalPValues", C_PARAM_GENERATED)
        results$.setParameterType("finalStage", C_PARAM_GENERATED)
        .logProgress("Final p-value calculated", startTime = startTime)

        # final confidence interval & median unbiased estimate
        startTime <- Sys.time()
        finalConfidenceIntervals <- .getFinalConfidenceIntervalMeans(
            design = design, dataInput = dataInput,
            thetaH0 = thetaH0, stage = stage, directionUpper = directionUpper,
            normalApproximation = normalApproximation,
            equalVariances = equalVariances, tolerance = tolerance
        )

        if (!is.null(finalConfidenceIntervals)) {
            finalStage <- finalConfidenceIntervals$finalStage
            results$finalConfidenceIntervalLowerBounds <- .getVectorWithFinalValueAtFinalStage(
                kMax = design$kMax,
                finalValue = finalConfidenceIntervals$finalConfidenceInterval[1], finalStage = finalStage
            )
            results$finalConfidenceIntervalUpperBounds <- .getVectorWithFinalValueAtFinalStage(
                kMax = design$kMax,
                finalValue = finalConfidenceIntervals$finalConfidenceInterval[2], finalStage = finalStage
            )
            results$medianUnbiasedEstimates <- .getVectorWithFinalValueAtFinalStage(
                kMax = design$kMax,
                finalValue = finalConfidenceIntervals$medianUnbiased, finalStage = finalStage
            )
            results$.setParameterType("finalConfidenceIntervalLowerBounds", C_PARAM_GENERATED)
            results$.setParameterType("finalConfidenceIntervalUpperBounds", C_PARAM_GENERATED)
            results$.setParameterType("medianUnbiasedEstimates", C_PARAM_GENERATED)
            .logProgress("Final confidence interval calculated", startTime = startTime)
        }
    }

    return(results)
}

.getStageResultsMeans <- function(..., design, dataInput,
        thetaH0 = C_THETA_H0_MEANS_DEFAULT,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT,
        equalVariances = C_EQUAL_VARIANCES_DEFAULT,
        stage = NA_integer_, userFunctionCallEnabled = FALSE) {
    .assertIsDatasetMeans(dataInput = dataInput)
    .assertIsValidThetaH0DataInput(thetaH0, dataInput)
    .assertIsValidDirectionUpper(directionUpper, design$sided,
        userFunctionCallEnabled = userFunctionCallEnabled
    )
    .assertIsSingleLogical(normalApproximation, "normalApproximation")
    .assertIsSingleLogical(equalVariances, "equalVariances")
    .warnInCaseOfUnknownArguments(
        functionName = "getStageResultsMeans",
        ignore = .getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), ...
    )
    stage <- .getStageFromOptionalArguments(...,
        dataInput = dataInput,
        design = design, stage = stage
    )

    effectSizes <- rep(NA_real_, design$kMax)

    if (dataInput$getNumberOfGroups() == 1) {
        overallTestStatistics <- c((dataInput$getOverallMeansUpTo(stage) - thetaH0) /
            dataInput$getOverallStDevsUpTo(stage) *
            sqrt(dataInput$getOverallSampleSizesUpTo(stage)), rep(NA_real_, design$kMax - stage))

        if (normalApproximation) {
            overallPValues <- 1 - stats::pnorm(overallTestStatistics)
        } else {
            overallPValues <- 1 - stats::pt(
                overallTestStatistics,
                dataInput$getOverallSampleSizesUpTo(stage) - 1
            )
        }
        effectSizes[1:stage] <- dataInput$getOverallMeansUpTo(stage)
    }

    if (dataInput$getNumberOfGroups() == 2) {
        # common variance
        overallStDevs <- rep(NA_real_, design$kMax)
        for (k in 1:stage) {
            overallStDevs[k] <- sqrt(((sum(dataInput$getSampleSizesUpTo(k, 1)) - 1) *
                dataInput$getOverallStDev(k)^2 +
                (sum(dataInput$getSampleSizesUpTo(k, 2)) - 1) * dataInput$getOverallStDev(k, 2)^2) /
                (sum(dataInput$getSampleSizesUpTo(k, 1)) + sum(dataInput$getSampleSizesUpTo(k, 2)) - 2))
        }

        overallSampleSizes1 <- dataInput$getOverallSampleSizesUpTo(stage)
        overallSampleSizes2 <- dataInput$getOverallSampleSizesUpTo(stage, 2)

        if (equalVariances) {
            overallTestStatistics <- c(
                (dataInput$getOverallMeansUpTo(stage) -
                    dataInput$getOverallMeansUpTo(stage, 2) - thetaH0) /
                    overallStDevs[1:stage] /
                    sqrt(1 / overallSampleSizes1 + 1 / overallSampleSizes2),
                rep(NA_real_, design$kMax - stage)
            )
        } else {
            overallTestStatistics <- c(
                (dataInput$getOverallMeansUpTo(stage) -
                    dataInput$getOverallMeansUpTo(stage, 2) - thetaH0) /
                    (sqrt(dataInput$getOverallStDevsUpTo(stage)^2 / overallSampleSizes1 +
                        dataInput$getOverallStDevsUpTo(stage, 2)^2 / overallSampleSizes2)),
                rep(NA_real_, design$kMax - stage)
            )
        }

        if (normalApproximation) {
            overallPValues <- 1 - stats::pnorm(overallTestStatistics)
        } else {
            if (equalVariances) {
                overallPValues <- 1 - stats::pt(
                    overallTestStatistics,
                    overallSampleSizes1 + overallSampleSizes2 - 2
                )
            } else {
                u <- dataInput$getOverallStDevsUpTo(stage)^2 / overallSampleSizes1 /
                    (dataInput$getOverallStDevsUpTo(stage)^2 / overallSampleSizes1 +
                        dataInput$getOverallStDevsUpTo(stage, 2)^2 / overallSampleSizes2)
                overallPValues <- 1 - stats::pt(
                    overallTestStatistics,
                    1 / (u^2 / (overallSampleSizes1 - 1) +
                        (1 - u)^2 / (overallSampleSizes2 - 1))
                )
            }
        }
        effectSizes[1:stage] <- dataInput$getOverallMeansUpTo(stage) - dataInput$getOverallMeansUpTo(stage, 2)
    }
    if (!directionUpper) {
        overallPValues <- 1 - overallPValues
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
            # stage-wise test statistics
            testStatistics[k] <- (dataInput$getMean(k) - thetaH0) /
                dataInput$getStDev(k) * sqrt(dataInput$getSampleSize(k))

            if (normalApproximation) {
                # stage-wise p-values
                pValues[k] <- 1 - stats::pnorm(testStatistics[k])
            } else {
                pValues[k] <- 1 - stats::pt(testStatistics[k], dataInput$getSampleSize(k) - 1)
            }
        }

        if (dataInput$getNumberOfGroups() == 2) {
            # stage-wise test statistics
            if (equalVariances) {
                testStatistics[k] <- (dataInput$getMean(k, 1) - dataInput$getMean(k, 2) - thetaH0) /
                    sqrt(((dataInput$getSampleSize(k, 1) - 1) * dataInput$getStDev(k, 1)^2 +
                        (dataInput$getSampleSize(k, 2) - 1) * dataInput$getStDev(k, 2)^2) /
                        (dataInput$getSampleSize(k, 1) + dataInput$getSampleSize(k, 2) - 2)) /
                    sqrt(1 / dataInput$getSampleSize(k, 1) + 1 / dataInput$getSampleSize(k, 2))
            } else {
                testStatistics[k] <- (dataInput$getMean(k, 1) - dataInput$getMean(k, 2) - thetaH0) /
                    sqrt(dataInput$getStDev(k, 1)^2 / dataInput$getSampleSize(k, 1) +
                        dataInput$getStDev(k, 2)^2 / dataInput$getSampleSize(k, 2))
            }

            if (normalApproximation) {
                # stage-wise p-values
                pValues[k] <- 1 - stats::pnorm(testStatistics[k])
            } else {
                if (equalVariances) {
                    pValues[k] <- 1 - stats::pt(
                        testStatistics[k],
                        dataInput$getSampleSize(k, 1) + dataInput$getSampleSize(k, 2) - 2
                    )
                } else {
                    u <- dataInput$getStDev(k, 1)^2 / dataInput$getSampleSize(k, 1) / (dataInput$getStDev(k, 1)^2 /
                        dataInput$getSampleSize(k, 1) + dataInput$getStDev(k, 2)^2 / dataInput$getSampleSize(k, 2))
                    pValues[k] <- 1 - stats::pt(
                        testStatistics[k],
                        1 / (u^2 / (dataInput$getSampleSize(k, 1) - 1) +
                            (1 - u)^2 / (dataInput$getSampleSize(k, 2) - 1))
                    )
                }
            }
        }
        if (!directionUpper) {
            pValues[k] <- 1 - pValues[k]
        }

        # inverse normal test
        combInverseNormal[k] <- (weightsInverseNormal[1:k] %*% .getOneMinusQNorm(pValues[1:k])) /
            sqrt(sum(weightsInverseNormal[1:k]^2))

        # Fisher combination test
        combFisher[k] <- prod(pValues[1:k]^weightsFisher[1:k])
    }

    if (dataInput$getNumberOfGroups() == 1) {
        stageResults <- StageResultsMeans$new(
            design = design,
            dataInput = dataInput,
            stage = as.integer(stage),
            overallTestStatistics = .fillWithNAs(overallTestStatistics, design$kMax),
            overallPValues = .fillWithNAs(overallPValues, design$kMax),
            overallMeans = .trimAnalysisMeansResultObjectAndFillWithNAs(
                dataInput$getOverallMeans(), design$kMax
            ),
            overallStDevs = .trimAnalysisMeansResultObjectAndFillWithNAs(
                dataInput$getOverallStDevs(), design$kMax
            ),
            overallSampleSizes = .fillWithNAs(dataInput$getOverallSampleSizesUpTo(stage), design$kMax),
            testStatistics = testStatistics,
            effectSizes = effectSizes,
            pValues = pValues,
            combInverseNormal = combInverseNormal,
            combFisher = combFisher,
            weightsFisher = weightsFisher,
            weightsInverseNormal = weightsInverseNormal,
            thetaH0 = thetaH0,
            direction = ifelse(directionUpper, C_DIRECTION_UPPER, C_DIRECTION_LOWER),
            normalApproximation = normalApproximation,
            equalVariances = equalVariances
        )
    } else if (dataInput$getNumberOfGroups() == 2) {
        stageResults <- StageResultsMeans$new(
            design = design,
            dataInput = dataInput,
            stage = as.integer(stage),
            overallTestStatistics = .fillWithNAs(overallTestStatistics, design$kMax),
            overallPValues = .fillWithNAs(overallPValues, design$kMax),
            overallMeans1 = .trimAnalysisMeansResultObjectAndFillWithNAs(
                dataInput$getOverallMeans(group = 1), design$kMax
            ),
            overallMeans2 = .trimAnalysisMeansResultObjectAndFillWithNAs(
                dataInput$getOverallMeans(group = 2), design$kMax
            ),
            overallStDevs1 = .trimAnalysisMeansResultObjectAndFillWithNAs(
                dataInput$getOverallStDevs(group = 1), design$kMax
            ),
            overallStDevs2 = .trimAnalysisMeansResultObjectAndFillWithNAs(
                dataInput$getOverallStDevs(group = 2), design$kMax
            ),
            overallStDevs = overallStDevs, # common variance
            overallSampleSizes1 = .fillWithNAs(dataInput$getOverallSampleSizesUpTo(stage), design$kMax),
            overallSampleSizes2 = .fillWithNAs(dataInput$getOverallSampleSizesUpTo(stage, 2), design$kMax),
            effectSizes = effectSizes,
            testStatistics = testStatistics,
            pValues = pValues,
            combInverseNormal = combInverseNormal,
            combFisher = combFisher,
            weightsFisher = weightsFisher,
            weightsInverseNormal = weightsInverseNormal,
            thetaH0 = thetaH0,
            direction = ifelse(directionUpper, C_DIRECTION_UPPER, C_DIRECTION_LOWER),
            normalApproximation = normalApproximation,
            equalVariances = equalVariances
        )
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

.trimAnalysisMeansResultObjectAndFillWithNAs <- function(x, kMax) {
    return(.fillWithNAs(.trimAnalysisMeansResultObject(x, kMax), kMax))
}

.trimAnalysisMeansResultObject <- function(x, kMax) {
    if (is.matrix(x)) {
        if (ncol(x) <= kMax) {
            return(x)
        }

        return(x[, 1:kMax])
    }

    if (length(x) <= kMax) {
        return(x)
    }

    return(x[1:kMax])
}

#'
#' Calculation of lower and upper limits of repeated confidence intervals (RCIs) for Means
#'
#' @noRd
#'
.getRepeatedConfidenceIntervalsMeans <- function(design, ...) {
    if (.isTrialDesignGroupSequential(design)) {
        return(.getRepeatedConfidenceIntervalsMeansGroupSequential(design = design, ...))
    }

    if (.isTrialDesignInverseNormal(design)) {
        return(.getRepeatedConfidenceIntervalsMeansInverseNormal(design = design, ...))
    }

    if (.isTrialDesignFisher(design)) {
        return(.getRepeatedConfidenceIntervalsMeansFisher(design = design, ...))
    }

    .stopWithWrongDesignMessage(design, inclusiveConditionalDunnett = FALSE)
}

.getRootThetaMeans <- function(..., design, dataInput, stage,
        directionUpper, normalApproximation = normalApproximation, equalVariances = equalVariances,
        thetaLow, thetaUp, firstParameterName, secondValue, tolerance,
        callingFunctionInformation = NA_character_) {
    result <- .getOneDimensionalRoot(
        function(theta) {
            stageResults <- .getStageResultsMeans(
                design = design, dataInput = dataInput,
                stage = stage, thetaH0 = theta, directionUpper = directionUpper,
                normalApproximation = normalApproximation, equalVariances = equalVariances
            )
            firstValue <- stageResults[[firstParameterName]][stage]
            if (.isTrialDesignGroupSequential(design)) {
                firstValue <- .getOneMinusQNorm(firstValue)
            }
            return(firstValue - secondValue)
        },
        lower = thetaLow, upper = thetaUp, tolerance = tolerance,
        callingFunctionInformation = callingFunctionInformation
    )
    return(result)
}

.getUpperLowerThetaMeans <- function(..., design, dataInput, theta, stage,
        directionUpper, normalApproximation = normalApproximation,
        equalVariances = equalVariances, conditionFunction,
        firstParameterName, secondValue) {
    stageResults <- .getStageResultsMeans(
        design = design, dataInput = dataInput,
        stage = stage, thetaH0 = theta, directionUpper = directionUpper,
        normalApproximation = normalApproximation, equalVariances = equalVariances
    )

    firstValue <- stageResults[[firstParameterName]][stage]
    if (.isTrialDesignGroupSequential(design)) {
        firstValue <- .getOneMinusQNorm(firstValue)
    }

    maxSearchIterations <- 50
    while (conditionFunction(secondValue, firstValue)) {
        theta <- 2 * theta

        stageResults <- .getStageResultsMeans(
            design = design, dataInput = dataInput,
            stage = stage, thetaH0 = theta, directionUpper = directionUpper,
            normalApproximation = normalApproximation, equalVariances = equalVariances
        )

        firstValue <- stageResults[[firstParameterName]][stage]
        if (.isTrialDesignGroupSequential(design)) {
            firstValue <- .getOneMinusQNorm(firstValue)
        }

        maxSearchIterations <- maxSearchIterations - 1
        if (maxSearchIterations < 0) {
            stop(sprintf(
                paste0(
                    "Failed to find theta (k = %s, firstValue = %s, ",
                    "secondValue = %s, levels(firstValue) = %s, theta = %s)"
                ),
                stage, stageResults[[firstParameterName]][stage], secondValue,
                firstValue, theta
            ))
        }
    }

    return(theta)
}

.getRepeatedConfidenceIntervalsMeansAll <- function(...,
        design, dataInput,
        normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT,
        equalVariances = C_EQUAL_VARIANCES_DEFAULT,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT,
        firstParameterName) {
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    futilityCorr <- rep(NA_real_, design$kMax) # necessary for adjustment for binding futility boundaries
    criticalValues <- design$criticalValues
    criticalValues[is.infinite(criticalValues) & criticalValues > 0] <- C_QNORM_MAXIMUM
    criticalValues[is.infinite(criticalValues) & criticalValues < 0] <- C_QNORM_MINIMUM

    if (.isTrialDesignFisher(design)) {
        bounds <- design$alpha0Vec
        border <- C_ALPHA_0_VEC_DEFAULT
        conditionFunction <- .isFirstValueSmallerThanSecondValue
    } else {
        bounds <- design$futilityBounds
        border <- C_FUTILITY_BOUNDS_DEFAULT
        conditionFunction <- .isFirstValueGreaterThanSecondValue
    }

    repeatedConfidenceIntervals <- matrix(NA_real_, nrow = 2, ncol = design$kMax)
    for (k in 1:stage) {
        startTime <- Sys.time()
        if (criticalValues[k] < C_QNORM_MAXIMUM) {
            # finding maximum upper and minimum lower bounds for RCIs
            thetaLow <- .getUpperLowerThetaMeans(
                design = design, dataInput = dataInput,
                theta = -1, stage = k, directionUpper = TRUE,
                normalApproximation = normalApproximation, equalVariances = equalVariances,
                conditionFunction = conditionFunction,
                firstParameterName = firstParameterName, secondValue = criticalValues[k]
            )

            thetaUp <- .getUpperLowerThetaMeans(
                design = design, dataInput = dataInput,
                theta = 1, stage = k, directionUpper = FALSE,
                normalApproximation = normalApproximation, equalVariances = equalVariances,
                conditionFunction = conditionFunction,
                firstParameterName = firstParameterName, secondValue = criticalValues[k]
            )

            # finding upper and lower RCI limits through root function
            repeatedConfidenceIntervals[1, k] <- .getRootThetaMeans(
                design = design, dataInput = dataInput, stage = k,
                directionUpper = TRUE, normalApproximation = normalApproximation,
                equalVariances = equalVariances, thetaLow = thetaLow, thetaUp = thetaUp,
                firstParameterName = firstParameterName, secondValue = criticalValues[k], tolerance = tolerance,
                callingFunctionInformation = paste0("Repeated confidence interval [1, ", k, "]")
            )

            repeatedConfidenceIntervals[2, k] <- .getRootThetaMeans(
                design = design, dataInput = dataInput, stage = k,
                directionUpper = FALSE, normalApproximation = normalApproximation,
                equalVariances = equalVariances, thetaLow = thetaLow, thetaUp = thetaUp,
                firstParameterName = firstParameterName, secondValue = criticalValues[k], tolerance = tolerance,
                callingFunctionInformation = paste0("Repeated confidence interval [2, ", k, "]")
            )

            # adjustment for binding futility bounds
            if (k > 1 && !is.na(bounds[k - 1]) && conditionFunction(bounds[k - 1], border) && design$bindingFutility) {
                parameterName <- ifelse(.isTrialDesignFisher(design), "pValues", firstParameterName)

                #  Calculate new lower and upper bounds
                if (directionUpper) {
                    thetaLow <- .getUpperLowerThetaMeans(
                        design = design, dataInput = dataInput, theta = -1, stage = k - 1,
                        directionUpper = TRUE, normalApproximation = normalApproximation,
                        equalVariances = equalVariances, conditionFunction = conditionFunction,
                        firstParameterName = parameterName, secondValue = bounds[k - 1]
                    )
                } else {
                    thetaUp <- .getUpperLowerThetaMeans(
                        design = design, dataInput = dataInput, theta = 1, stage = k - 1,
                        directionUpper = FALSE, normalApproximation = normalApproximation,
                        equalVariances = equalVariances, conditionFunction = conditionFunction,
                        firstParameterName = parameterName, secondValue = bounds[k - 1]
                    )
                }

                futilityCorr[k] <- .getRootThetaMeans(
                    design = design, dataInput = dataInput, stage = k - 1,
                    directionUpper = directionUpper, normalApproximation = normalApproximation,
                    equalVariances = equalVariances, thetaLow = thetaLow, thetaUp = thetaUp,
                    firstParameterName = parameterName, secondValue = bounds[k - 1],
                    tolerance = tolerance, callingFunctionInformation =
                        paste0("Repeated confidence interval, futility correction [", k, "]")
                )

                if (directionUpper) {
                    repeatedConfidenceIntervals[1, k] <- min(min(futilityCorr[2:k]), repeatedConfidenceIntervals[1, k])
                } else {
                    repeatedConfidenceIntervals[2, k] <- max(max(futilityCorr[2:k]), repeatedConfidenceIntervals[2, k])
                }
            }

            if (!is.na(repeatedConfidenceIntervals[1, k]) && !is.na(repeatedConfidenceIntervals[2, k]) &&
                    repeatedConfidenceIntervals[1, k] > repeatedConfidenceIntervals[2, k]) {
                repeatedConfidenceIntervals[, k] <- rep(NA_real_, 2)
            }
        }

        .logProgress("Repeated confidence interval of stage %s calculated", startTime = startTime, k)
    }

    return(repeatedConfidenceIntervals)
}

#'
#' RCIs based on group sequential combination test
#'
#' @noRd
#'
.getRepeatedConfidenceIntervalsMeansGroupSequential <- function(...,
        design, dataInput,
        normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT,
        equalVariances = C_EQUAL_VARIANCES_DEFAULT,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .warnInCaseOfUnknownArguments(
        functionName =
            ".getRepeatedConfidenceIntervalsMeansGroupSequential",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), "stage"), ...
    )

    return(.getRepeatedConfidenceIntervalsMeansAll(
        design = design, dataInput = dataInput,
        normalApproximation = normalApproximation, equalVariances = equalVariances,
        directionUpper = directionUpper, tolerance = tolerance, firstParameterName = "overallPValues", ...
    ))
}

#'
#' RCIs based on inverse normal combination test
#'
#' @noRd
#'
.getRepeatedConfidenceIntervalsMeansInverseNormal <- function(...,
        design, dataInput,
        normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT,
        equalVariances = C_EQUAL_VARIANCES_DEFAULT,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .warnInCaseOfUnknownArguments(
        functionName =
            ".getRepeatedConfidenceIntervalsMeansInverseNormal",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), "stage"), ...
    )

    return(.getRepeatedConfidenceIntervalsMeansAll(
        design = design, dataInput = dataInput,
        normalApproximation = normalApproximation, equalVariances = equalVariances,
        directionUpper = directionUpper, tolerance = tolerance,
        firstParameterName = "combInverseNormal", ...
    ))
}

#'
#' RCIs based on Fisher's combination test
#'
#' @noRd
#'
.getRepeatedConfidenceIntervalsMeansFisher <- function(...,
        design, dataInput,
        normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT,
        equalVariances = C_EQUAL_VARIANCES_DEFAULT,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .warnInCaseOfUnknownArguments(
        functionName =
            ".getRepeatedConfidenceIntervalsMeansFisher",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), "stage"), ...
    )

    return(.getRepeatedConfidenceIntervalsMeansAll(
        design = design, dataInput = dataInput,
        normalApproximation = normalApproximation, equalVariances = equalVariances,
        directionUpper = directionUpper, tolerance = tolerance, firstParameterName = "combFisher", ...
    ))
}

#'
#' Calculation of conditional power based on group sequential method
#'
#' @noRd
#'
.getConditionalPowerMeansGroupSequential <- function(..., stageResults, stage = stageResults$stage,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT, nPlanned = NA_real_,
        thetaH1 = NA_real_, assumedStDev = NA_real_) {
    design <- stageResults$.design
    .assertIsTrialDesignGroupSequential(design)
    .assertIsValidStage(stage, design$kMax)

    assumedStDev <- .assertIsValidAssumedStDev(assumedStDev, stageResults, stage)

    thetaH1 <- .assertIsValidThetaH1(thetaH1, stageResults, stage)

    .warnInCaseOfUnknownArguments(
        functionName = ".getConditionalPowerMeansGroupSequential",
        ignore = c("stage", "design", "stageResultsName", "grid", "stDevH1"), ...
    )

    kMax <- design$kMax
    conditionalPower <- rep(NA_real_, kMax)
    weights <- stageResults$weightsInverseNormal
    informationRates <- design$informationRates

    nPlanned <- c(rep(NA, stage), nPlanned)

    if (stage == kMax) {
        .logDebug(
            "Conditional power will be calculated only for subsequent stages ",
            "(stage = ", stage, ", kMax = ", design$kMax, ")"
        )
        return(list(
            nPlanned = nPlanned,
            conditionalPower = conditionalPower
        ))
    }

    criticalValues <- design$criticalValues

    if (stageResults$isTwoSampleDataset()) {
        .assertIsSingleNumber(allocationRatioPlanned, "allocationRatioPlanned")
        .assertIsInOpenInterval(allocationRatioPlanned, "allocationRatioPlanned", 0, C_ALLOCATION_RATIO_MAXIMUM)
        nPlanned <- allocationRatioPlanned / (1 + allocationRatioPlanned)^2 * nPlanned
    }

    if (stageResults$direction == "upper") {
        thetaH1 <- (thetaH1 - stageResults$thetaH0) / assumedStDev
    } else {
        thetaH1 <- -(thetaH1 - stageResults$thetaH0) / assumedStDev
    }

    # shifted decision region for use in getGroupSeqProbs
    # Group Sequential Method
    shiftedDecisionRegionUpper <- criticalValues[(stage + 1):kMax] *
        sqrt(sum(weights[1:stage]^2) + cumsum(weights[(stage + 1):kMax]^2)) /
        sqrt(cumsum(weights[(stage + 1):kMax]^2)) -
        .getOneMinusQNorm(stageResults$overallPValues[stage]) * sqrt(sum(weights[1:stage]^2)) /
            sqrt(cumsum(weights[(stage + 1):kMax]^2)) -
        thetaH1 * cumsum(sqrt(nPlanned[(stage + 1):kMax]) * weights[(stage + 1):kMax]) /
            sqrt(cumsum(weights[(stage + 1):kMax]^2))

    if (design$sided == 2) {
        shiftedDecisionRegionLower <- -criticalValues[(stage + 1):kMax] *
            sqrt(sum(weights[1:stage]^2) + cumsum(weights[(stage + 1):kMax]^2)) /
            sqrt(cumsum(weights[(stage + 1):kMax]^2)) -
            c(weights[1:stage] %*% .getOneMinusQNorm(stageResults$pValues[1:stage])) /
                sqrt(cumsum(weights[(stage + 1):kMax]^2)) -
            thetaH1 * cumsum(sqrt(nPlanned[(stage + 1):kMax]) * weights[(stage + 1):kMax]) /
                sqrt(cumsum(weights[(stage + 1):kMax]^2))
    }

    if (stage == kMax - 1) {
        shiftedFutilityBounds <- c()
    } else {
        shiftedFutilityBounds <- design$futilityBounds[(stage + 1):(kMax - 1)] *
            sqrt(sum(weights[1:stage]^2) + cumsum(weights[(stage + 1):(kMax - 1)]^2)) /
            sqrt(cumsum(weights[(stage + 1):(kMax - 1)]^2)) -
            .getOneMinusQNorm(stageResults$overallPValues[stage]) * sqrt(sum(weights[1:stage]^2)) /
                sqrt(cumsum(weights[(stage + 1):(kMax - 1)]^2)) -
            thetaH1 * cumsum(sqrt(nPlanned[(stage + 1):(kMax - 1)]) * weights[(stage + 1):(kMax - 1)]) /
                sqrt(cumsum(weights[(stage + 1):(kMax - 1)]^2))
    }

    # scaled information for use in getGroupSeqProbs
    scaledInformation <- (informationRates[(stage + 1):kMax] - informationRates[stage]) /
        (1 - informationRates[stage])

    if (design$sided == 2) {
        decisionMatrix <- matrix(c(shiftedDecisionRegionLower, shiftedDecisionRegionUpper), nrow = 2, byrow = TRUE)
    } else {
        decisionMatrix <- matrix(c(
            shiftedFutilityBounds, C_FUTILITY_BOUNDS_DEFAULT,
            shiftedDecisionRegionUpper
        ), nrow = 2, byrow = TRUE)
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
.getConditionalPowerMeansInverseNormal <- function(..., stageResults, stage = stageResults$stage,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT, nPlanned = NA_real_,
        thetaH1 = NA_real_, assumedStDev = NA_real_) {
    design <- stageResults$.design
    .assertIsTrialDesignInverseNormal(design)
    .assertIsValidStage(stage, design$kMax)

    assumedStDev <- .assertIsValidAssumedStDev(assumedStDev, stageResults, stage)

    thetaH1 <- .assertIsValidThetaH1(thetaH1, stageResults, stage)

    .warnInCaseOfUnknownArguments(
        functionName = ".getConditionalPowerMeansInverseNormal",
        ignore = c("stage", "design", "stageResultsName", "grid", "stDevH1"), ...
    )

    kMax <- design$kMax
    conditionalPower <- rep(NA_real_, kMax)
    weights <- stageResults$weightsInverseNormal
    informationRates <- design$informationRates

    nPlanned <- c(rep(NA_real_, stage), nPlanned)

    if (stage == kMax) {
        .logDebug(
            "Conditional power will be calculated only for subsequent stages ",
            "(stage = ", stage, ", kMax = ", design$kMax, ")"
        )
        return(list(
            nPlanned = nPlanned,
            conditionalPower = conditionalPower
        ))
    }

    criticalValuesInverseNormal <- design$criticalValues

    if (stageResults$isTwoSampleDataset()) {
        .assertIsSingleNumber(allocationRatioPlanned, "allocationRatioPlanned")
        .assertIsInOpenInterval(allocationRatioPlanned, "allocationRatioPlanned", 0, C_ALLOCATION_RATIO_MAXIMUM)
        nPlanned <- allocationRatioPlanned / (1 + allocationRatioPlanned)^2 * nPlanned
    }

    if (stageResults$direction == "upper") {
        thetaH1 <- (thetaH1 - stageResults$thetaH0) / assumedStDev
    } else {
        thetaH1 <- -(thetaH1 - stageResults$thetaH0) / assumedStDev
    }

    # shifted decision region for use in getGroupSeqProbs
    # Inverse Normal Method
    shiftedDecisionRegionUpper <- criticalValuesInverseNormal[(stage + 1):kMax] *
        sqrt(sum(weights[1:stage]^2) + cumsum(weights[(stage + 1):kMax]^2)) /
        sqrt(cumsum(weights[(stage + 1):kMax]^2)) -
        c(weights[1:stage] %*% .getOneMinusQNorm(stageResults$pValues[1:stage])) /
            sqrt(cumsum(weights[(stage + 1):kMax]^2)) -
        thetaH1 * cumsum(sqrt(nPlanned[(stage + 1):kMax]) * weights[(stage + 1):kMax]) /
            sqrt(cumsum(weights[(stage + 1):kMax]^2))
    if (design$sided == 2) {
        shiftedDecisionRegionLower <- -criticalValuesInverseNormal[(stage + 1):kMax] *
            sqrt(sum(weights[1:stage]^2) + cumsum(weights[(stage + 1):kMax]^2)) /
            sqrt(cumsum(weights[(stage + 1):kMax]^2)) -
            c(weights[1:stage] %*% .getOneMinusQNorm(stageResults$pValues[1:stage])) /
                sqrt(cumsum(weights[(stage + 1):kMax]^2)) -
            thetaH1 * cumsum(sqrt(nPlanned[(stage + 1):kMax]) * weights[(stage + 1):kMax]) /
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
            thetaH1 * cumsum(sqrt(nPlanned[(stage + 1):(kMax - 1)]) * weights[(stage + 1):(kMax - 1)]) /
                sqrt(cumsum(weights[(stage + 1):(kMax - 1)]^2))
    }

    # scaled information for use in getGroupSeqProbs
    scaledInformation <- (informationRates[(stage + 1):kMax] - informationRates[stage]) /
        (1 - informationRates[stage])

    if (design$sided == 2) {
        decisionMatrix <- matrix(c(shiftedDecisionRegionLower, shiftedDecisionRegionUpper), nrow = 2, byrow = TRUE)
    } else {
        decisionMatrix <- matrix(c(
            shiftedFutilityBounds, C_FUTILITY_BOUNDS_DEFAULT,
            shiftedDecisionRegionUpper
        ), nrow = 2, byrow = TRUE)
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
.getConditionalPowerMeansFisher <- function(..., stageResults, stage = stageResults$stage,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT, nPlanned = NA_real_,
        thetaH1 = NA_real_, assumedStDev = NA_real_,
        iterations = C_ITERATIONS_DEFAULT, seed = NA_real_) {
    design <- stageResults$.design
    .assertIsTrialDesignFisher(design)
    .assertIsValidStage(stage, design$kMax)
    .assertIsValidIterationsAndSeed(iterations, seed, zeroIterationsAllowed = FALSE)

    assumedStDev <- .assertIsValidAssumedStDev(assumedStDev, stageResults, stage)

    thetaH1 <- .assertIsValidThetaH1(thetaH1, stageResults, stage)

    .warnInCaseOfUnknownArguments(
        functionName = ".getConditionalPowerMeansFisher",
        ignore = c("stage", "design", "stageResultsName", "grid", "stDevH1"), ...
    )

    kMax <- design$kMax
    conditionalPower <- rep(NA_real_, kMax)
    seed <- .setSeed(seed)
    simulated <- FALSE

    .assertIsValidNPlanned(nPlanned, kMax, stage)

    nPlanned <- c(rep(NA_real_, stage), nPlanned)

    if (stageResults$isTwoSampleDataset()) {
        .assertIsSingleNumber(allocationRatioPlanned, "allocationRatioPlanned")
        .assertIsInOpenInterval(
            allocationRatioPlanned,
            "allocationRatioPlanned", 0, C_ALLOCATION_RATIO_MAXIMUM
        )
        nPlanned <- allocationRatioPlanned / (1 + allocationRatioPlanned)^2 * nPlanned
    }

    if (stageResults$direction == "upper") {
        thetaH1 <- (thetaH1 - stageResults$thetaH0) / assumedStDev
    } else {
        thetaH1 <- -(thetaH1 - stageResults$thetaH0) / assumedStDev
    }

    criticalValues <- design$criticalValues
    weightsFisher <- stageResults$weightsFisher
    pValues <- stageResults$pValues

    if (stage < kMax - 1) {
        for (k in (stage + 1):kMax) {
            reject <- 0
            for (i in 1:iterations) {
                reject <- reject + .getRejectValueConditionalPowerFisher(
                    kMax = kMax, alpha0Vec = design$alpha0Vec,
                    criticalValues = criticalValues, weightsFisher = weightsFisher,
                    pValues = pValues, currentKMax = k, thetaH1 = thetaH1,
                    stage = stage, nPlanned = nPlanned
                )
            }
            conditionalPower[k] <- reject / iterations
        }
        simulated <- TRUE
    } else if (stage == kMax - 1) {
        divisor <- prod(pValues[1:(kMax - 1)]^weightsFisher[1:(kMax - 1)])
        result <- 1 - (criticalValues[kMax] / divisor)^(1 / weightsFisher[kMax])
        if (result <= 0 || result >= 1) {
            warning("Calculation not possible: could not calculate ",
                "conditional power for stage ", kMax,
                call. = FALSE
            )
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

.getConditionalPowerMeans <- function(..., stageResults, nPlanned,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT, thetaH1 = NA_real_, assumedStDev = NA_real_) {
    stDevH1 <- .getOptionalArgument("stDevH1", ...)
    if (!is.null(stDevH1) && !is.na(stDevH1)) {
        if (!is.na(assumedStDev)) {
            warning(sQuote("assumedStDev"), " will be ignored because ",
                sQuote("stDevH1"), " is defined",
                call. = FALSE
            )
        }
        assumedStDev <- stDevH1
    }

    .assertIsSingleNumber(thetaH1, "thetaH1", naAllowed = TRUE)
    .assertIsSingleNumber(assumedStDev, "assumedStDev", naAllowed = TRUE)

    design <- stageResults$.design

    results <- ConditionalPowerResultsMeans$new(
        .stageResults = stageResults, .design = design,
        nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned,
        thetaH1 = thetaH1, assumedStDev = assumedStDev
    )

    if (any(is.na(nPlanned))) {
        return(results)
    }

    if (!.isValidNPlanned(nPlanned = nPlanned, kMax = design$kMax, stage = stageResults$stage)) {
        return(results)
    }

    if (.isTrialDesignGroupSequential(design)) {
        cp <- .getConditionalPowerMeansGroupSequential(
            stageResults = stageResults,
            nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned,
            thetaH1 = thetaH1, assumedStDev = assumedStDev, ...
        )
    } else if (.isTrialDesignInverseNormal(design)) {
        cp <- .getConditionalPowerMeansInverseNormal(
            stageResults = stageResults,
            nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned,
            thetaH1 = thetaH1, assumedStDev = assumedStDev, ...
        )
    } else if (.isTrialDesignFisher(design)) {
        cp <- .getConditionalPowerMeansFisher(
            stageResults = stageResults,
            nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned,
            thetaH1 = thetaH1, assumedStDev = assumedStDev, ...
        )
        results$iterations <- cp$iterations
        results$seed <- cp$seed
        results$simulated <- cp$simulated
        .updateParameterTypeOfIterationsAndSeed(results, ...)
    } else {
        .stopWithWrongDesignMessage(design, inclusiveConditionalDunnett = FALSE)
    }

    results$nPlanned <- cp$nPlanned
    results$conditionalPower <- cp$conditionalPower
    results$.setParameterType("nPlanned", C_PARAM_GENERATED)
    results$.setParameterType("conditionalPower", C_PARAM_GENERATED)
    results$.setParameterType(
        "allocationRatioPlanned",
        ifelse(allocationRatioPlanned == C_ALLOCATION_RATIO_DEFAULT, C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED)
    )
    results$.setParameterType("thetaH1", ifelse(is.na(thetaH1), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))
    results$.setParameterType("assumedStDev", ifelse(is.na(assumedStDev), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))
    return(results)
}

.getConditionalPowerPlotMeans <- function(..., stageResults, stage,
        nPlanned, allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        thetaRange, assumedStDev = NA_real_) {
    .associatedArgumentsAreDefined(nPlanned = nPlanned, thetaRange = thetaRange)
    .assertIsValidAllocationRatioPlanned(
        allocationRatioPlanned,
        stageResults$getDataInput()$getNumberOfGroups()
    )
    .warnInCaseOfUnknownArguments(
        functionName = ".getConditionalPowerPlotMeans",
        ignore = c("iterations", "seed", "stageResultsName", "grid"), ...
    )

    assumedStDev <- .assertIsValidAssumedStDev(assumedStDev, stageResults, stage)

    thetaRange <- .assertIsValidThetaRange(thetaRange = thetaRange)

    condPowerValues <- rep(NA, length(thetaRange))
    likelihoodValues <- rep(NA, length(thetaRange))

    if (stageResults$isOneSampleDataset()) {
        stdErr <- stageResults$overallStDevs[stage] / sqrt(stageResults$overallSampleSizes[stage])
    } else if (stageResults$isTwoSampleDataset()) {
        stdErr <- stageResults$overallStDevs[stage] * sqrt(1 / stageResults$overallSampleSizes1[stage] + 1 /
            stageResults$overallSampleSizes2[stage])
    }

    design <- stageResults$.design

    warningMessages <- c()
    withCallingHandlers(
        for (i in seq(along.with = thetaRange)) {
            if (.isTrialDesignGroupSequential(design)) {
                condPowerValues[i] <- .getConditionalPowerMeansGroupSequential(
                    stageResults = stageResults, stage = stage, nPlanned = nPlanned,
                    allocationRatioPlanned = allocationRatioPlanned, thetaH1 = thetaRange[i],
                    assumedStDev = assumedStDev
                )$conditionalPower[design$kMax]
            } else if (.isTrialDesignInverseNormal(design)) {
                condPowerValues[i] <- .getConditionalPowerMeansInverseNormal(
                    stageResults = stageResults, stage = stage, nPlanned = nPlanned,
                    allocationRatioPlanned = allocationRatioPlanned, thetaH1 = thetaRange[i],
                    assumedStDev = assumedStDev
                )$conditionalPower[design$kMax]
            } else if (.isTrialDesignFisher(design)) {
                condPowerValues[i] <- .getConditionalPowerMeansFisher(
                    stageResults = stageResults, stage = stage, nPlanned = nPlanned,
                    allocationRatioPlanned = allocationRatioPlanned, thetaH1 = thetaRange[i],
                    assumedStDev = assumedStDev
                )$conditionalPower[design$kMax]
            }

            likelihoodValues[i] <- stats::dnorm(
                thetaRange[i],
                stageResults$effectSizes[stage], stdErr
            ) / stats::dnorm(0, 0, stdErr)
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
    if (length(warningMessages) > 0) {
        for (m in warningMessages) {
            warning(m, call. = FALSE)
        }
    }

    if (stageResults$isOneSampleDataset()) {
        subtitle <- paste0(
            "Stage = ", stage, ", # of remaining subjects = ",
            sum(nPlanned), ", sd = ", .formatSubTitleValue(assumedStDev, "assumedStDev")
        )
    } else {
        subtitle <- paste0(
            "Stage = ", stage, ", # of remaining subjects = ",
            sum(nPlanned), ", sd = ", .formatSubTitleValue(assumedStDev, "assumedStDev"),
            ", allocation ratio = ", .formatSubTitleValue(allocationRatioPlanned, "allocationRatioPlanned")
        )
    }

    return(list(
        xValues = thetaRange,
        condPowerValues = condPowerValues,
        likelihoodValues = likelihoodValues,
        main = C_PLOT_MAIN_CONDITIONAL_POWER_WITH_LIKELIHOOD,
        xlab = "Effect size",
        ylab = C_PLOT_YLAB_CONDITIONAL_POWER_WITH_LIKELIHOOD,
        sub = subtitle
    ))
}

#'
#' Calculation of final confidence interval
#' based on group sequential test without SSR (general case).
#'
#' @noRd
#'
.getFinalConfidenceIntervalMeansGroupSequential <- function(..., design, dataInput, stage,
        thetaH0 = C_THETA_H0_MEANS_DEFAULT, directionUpper = C_DIRECTION_UPPER_DEFAULT,
        normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT,
        equalVariances = C_EQUAL_VARIANCES_DEFAULT, tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    stageResults <- .getStageResultsMeans(
        design = design, dataInput = dataInput, stage = stage,
        thetaH0 = thetaH0, directionUpper = directionUpper, normalApproximation = normalApproximation,
        equalVariances = equalVariances
    )

    finalConfidenceIntervalMeansValues <- .getFinalConfidenceIntervalMeansValues(
        design, dataInput, stageResults, directionUpper, thetaH0, stage, tolerance
    )

    return(list(
        stage = stage,
        thetaH0 = thetaH0,
        directionUpper = directionUpper,
        normalApproximation = normalApproximation,
        equalVariances = equalVariances,
        tolerance = tolerance,
        finalStage = finalConfidenceIntervalMeansValues$finalStage,
        medianUnbiasedGeneral = finalConfidenceIntervalMeansValues$medianUnbiasedGeneral,
        finalConfidenceIntervalGeneral = finalConfidenceIntervalMeansValues$finalConfidenceIntervalGeneral,
        medianUnbiased = finalConfidenceIntervalMeansValues$medianUnbiased,
        finalConfidenceInterval = finalConfidenceIntervalMeansValues$finalConfidenceInterval
    ))
}

.getFinalConfidenceIntervalMeansValues <- function(design, dataInput,
        stageResults, directionUpper, thetaH0, stage, tolerance) {
    finalConfidenceIntervalGeneral <- rep(NA_real_, 2)
    medianUnbiasedGeneral <- NA_real_

    if (.isTrialDesignGroupSequential(design)) {
        designStage <- .getStageGroupSeq(design = design, stageResults = stageResults, stage = stage)
    } else {
        designStage <- .getStageInverseNormal(design = design, stageResults = stageResults, stage = stage)
    }

    finalStage <- min(designStage, design$kMax)

    # early stopping or at end of study
    if (designStage < design$kMax || stage == design$kMax) {
        if (designStage == 1) {
            if (.isTrialDesignGroupSequential(design)) {
                medianUnbiasedGeneral <- .getOneMinusQNorm(stageResults$overallPValues[1])
            } else {
                medianUnbiasedGeneral <- stageResults$combInverseNormal[1]
            }

            finalConfidenceIntervalGeneral[1] <- medianUnbiasedGeneral -
                .getOneMinusQNorm(design$alpha / design$sided)
            finalConfidenceIntervalGeneral[2] <- medianUnbiasedGeneral +
                .getOneMinusQNorm(design$alpha / design$sided)

            if (dataInput$getNumberOfGroups() == 1) {
                finalConfidenceIntervalGeneral <- finalConfidenceIntervalGeneral /
                    sqrt(stageResults$overallSampleSizes[1])
                medianUnbiasedGeneral <- medianUnbiasedGeneral /
                    sqrt(stageResults$overallSampleSizes[1])
            } else {
                finalConfidenceIntervalGeneral <- finalConfidenceIntervalGeneral *
                    sqrt(1 / stageResults$overallSampleSizes1[finalStage] +
                        1 / stageResults$overallSampleSizes2[finalStage])
                medianUnbiasedGeneral <- medianUnbiasedGeneral *
                    sqrt(1 / stageResults$overallSampleSizes1[finalStage] +
                        1 / stageResults$overallSampleSizes2[finalStage])
            }
        } else {
            if (.isTrialDesignInverseNormal(design) && design$kMax > 2 && !.isNoEarlyEfficacy(design)) {
                message(
                    "Calculation of final confidence interval performed for kMax = ", design$kMax,
                    " (for kMax > 2, it is theoretically shown that it is valid only ",
                    "if no sample size change was performed)"
                )
            }

            firstParameterName <- ifelse(.isTrialDesignGroupSequential(design),
                "overallPValues", "combInverseNormal"
            )

            finalConfidenceIntervalGeneral[1] <- .getDecisionMatrixRoot(
                design = design,
                stage = finalStage, stageResults = stageResults, tolerance = tolerance,
                firstParameterName = firstParameterName,
                case = "finalConfidenceIntervalGeneralLower"
            )

            finalConfidenceIntervalGeneral[2] <- .getDecisionMatrixRoot(
                design = design,
                stage = finalStage, stageResults = stageResults, tolerance = tolerance,
                firstParameterName = firstParameterName,
                case = "finalConfidenceIntervalGeneralUpper"
            )

            medianUnbiasedGeneral <- .getDecisionMatrixRoot(
                design = design,
                stage = finalStage, stageResults = stageResults, tolerance = tolerance,
                firstParameterName = firstParameterName,
                case = "medianUnbiasedGeneral"
            )
        }
    }

    if (designStage > 1 && is.na(finalConfidenceIntervalGeneral[1])) {
        finalStage <- NA_integer_
    }

    finalConfidenceInterval <- rep(NA_real_, 2)
    medianUnbiased <- NA_real_

    if (!is.na(finalStage)) {
        if (designStage == 1) {
            # retransformation
            if (dataInput$getNumberOfGroups() == 1) {
                stdErr <- stageResults$overallStDevs[finalStage] /
                    sqrt(stageResults$overallSampleSizes[finalStage])
            } else {
                stdErr <- stageResults$overallStDevs[finalStage] *
                    sqrt(1 / stageResults$overallSampleSizes1[finalStage] +
                        1 / stageResults$overallSampleSizes2[finalStage])
            }

            value <- .getOneMinusQNorm(design$alpha / design$sided) * stdErr
            medianUnbiased <- stageResults$effectSizes[1]
            finalConfidenceInterval[1] <- medianUnbiased - value
            finalConfidenceInterval[2] <- medianUnbiased + value
        } else {
            directionUpperSign <- ifelse(directionUpper, 1, -1)
            finalConfidenceInterval <- finalConfidenceIntervalGeneral *
                stageResults$overallStDevs[finalStage] + directionUpperSign * thetaH0
            medianUnbiased <- medianUnbiasedGeneral *
                stageResults$overallStDevs[finalStage] + directionUpperSign * thetaH0
        }
    }

    if (!directionUpper) {
        medianUnbiasedGeneral <- -medianUnbiasedGeneral
        finalConfidenceIntervalGeneral <- -finalConfidenceIntervalGeneral
        if (designStage > 1) {
            medianUnbiased <- -medianUnbiased
            finalConfidenceInterval <- -finalConfidenceInterval
        }
    }

    if (!any(is.na(finalConfidenceIntervalGeneral))) {
        finalConfidenceIntervalGeneral <- sort(finalConfidenceIntervalGeneral)
    }
    if (!any(is.na(finalConfidenceInterval))) {
        finalConfidenceInterval <- sort(finalConfidenceInterval)
    }

    return(list(
        finalStage = finalStage,
        medianUnbiasedGeneral = medianUnbiasedGeneral,
        finalConfidenceIntervalGeneral = finalConfidenceIntervalGeneral,
        medianUnbiased = medianUnbiased,
        finalConfidenceInterval = finalConfidenceInterval
    ))
}

#'
#' Calculation of final confidence interval
#' based on inverse normal method, only theoretically shown to be valid for kMax <= 2 or no SSR.
#'
#' @noRd
#'
.getFinalConfidenceIntervalMeansInverseNormal <- function(..., design, dataInput, stage,
        thetaH0 = C_THETA_H0_MEANS_DEFAULT, directionUpper = C_DIRECTION_UPPER_DEFAULT,
        normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT, equalVariances = C_EQUAL_VARIANCES_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    stageResults <- .getStageResultsMeans(
        design = design, dataInput = dataInput, stage = stage,
        thetaH0 = thetaH0, directionUpper = directionUpper, normalApproximation = normalApproximation,
        equalVariances = equalVariances
    )

    finalConfidenceIntervalMeansValues <- .getFinalConfidenceIntervalMeansValues(
        design, dataInput, stageResults, directionUpper, thetaH0, stage, tolerance
    )

    return(list(
        stage = stage,
        thetaH0 = thetaH0,
        directionUpper = directionUpper,
        normalApproximation = normalApproximation,
        equalVariances = equalVariances,
        tolerance = tolerance,
        finalStage = finalConfidenceIntervalMeansValues$finalStage,
        medianUnbiasedGeneral = finalConfidenceIntervalMeansValues$medianUnbiasedGeneral,
        finalConfidenceIntervalGeneral = finalConfidenceIntervalMeansValues$finalConfidenceIntervalGeneral,
        medianUnbiased = finalConfidenceIntervalMeansValues$medianUnbiased,
        finalConfidenceInterval = finalConfidenceIntervalMeansValues$finalConfidenceInterval
    ))
}

.getQFunctionResultBasedOnDataInput <- function(..., design, dataInput, theta, stage, infRate,
        directionUpper, normalApproximation, equalVariances) {
    if (dataInput$getNumberOfGroups() == 1) {
        stageResults <- .getStageResultsMeans(
            design = design, dataInput = dataInput, stage = stage,
            thetaH0 = theta, directionUpper = directionUpper, normalApproximation = normalApproximation
        )
    }

    if (dataInput$getNumberOfGroups() == 2) {
        stageResults <- .getStageResultsMeans(
            design = design, dataInput = dataInput, stage = stage,
            thetaH0 = theta, directionUpper = directionUpper, normalApproximation = normalApproximation,
            equalVariances = equalVariances
        )
    }

    return(.getQFunctionResult(
        design = design, stageResults = stageResults,
        theta = theta, infRate = infRate
    ))
}

#'
#' Calculation of final confidence interval
#' based on Fisher combination test, only valid for kMax <= 2.
#'
#' @noRd
#'
.getFinalConfidenceIntervalMeansFisher <- function(..., design, dataInput, stage,
        thetaH0 = C_THETA_H0_MEANS_DEFAULT, directionUpper = C_DIRECTION_UPPER_DEFAULT,
        normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT, equalVariances = C_EQUAL_VARIANCES_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    stageResults <- .getStageResultsMeans(
        design = design, dataInput = dataInput, stage = stage,
        thetaH0 = thetaH0, directionUpper = directionUpper, normalApproximation = normalApproximation,
        equalVariances = equalVariances
    )

    stageFisher <- .getStageFisher(design = design, stageResults = stageResults, stage = stage)

    finalStage <- min(stageFisher, design$kMax)

    finalConfidenceInterval <- rep(NA_real_, 2)
    medianUnbiased <- NA_real_

    # early stopping or at end of study
    if (stageFisher < design$kMax || stage == design$kMax) {
        if (dataInput$getNumberOfGroups() == 1) {
            infRate <- sqrt(stageResults$overallSampleSizes[1])
            stderr <- stageResults$overallStDevs[finalStage] /
                sqrt(stageResults$overallSampleSizes[finalStage])
        } else {
            infRate <- 1 / sqrt(1 / stageResults$overallSampleSizes1[1] +
                1 / stageResults$overallSampleSizes2[1])
            stderr <- stageResults$overallStDevs[finalStage] *
                sqrt(1 / stageResults$overallSampleSizes1[finalStage] +
                    1 / stageResults$overallSampleSizes2[finalStage])
        }

        if (stageFisher == 1) {
            finalConfidenceInterval[1] <- stageResults$effectSizes[1] -
                .getOneMinusQNorm(design$alpha / design$sided) * stderr
            finalConfidenceInterval[2] <- stageResults$effectSizes[1] +
                .getOneMinusQNorm(design$alpha / design$sided) * stderr
            medianUnbiased <- stageResults$effectSizes[1]
        } else {
            maxSearchIterations <- 50

            if (design$kMax >= 1) {
                message(
                    "Calculation of final confidence interval for Fisher's ",
                    "design not implemented yet"
                )
                return(list(
                    finalStage = NA_integer_, medianUnbiased = NA_real_,
                    finalConfidenceInterval = rep(NA_real_, design$kMax)
                ))
            }

            thetaLow <- -1
            .getQFunctionResult(
                design = design, stageResults = stageResults,
                theta = thetaLow, infRate = infRate
            )
            iteration <- 0
            while (iteration <= maxSearchIterations &&
                .getQFunctionResultBasedOnDataInput(
                    design = design, dataInput = dataInput,
                    theta = thetaLow, stage = finalStage,
                    infRate = infRate, directionUpper = directionUpper,
                    normalApproximation = normalApproximation,
                    equalVariances = equalVariances
                ) > design$alpha / design$sided) {
                thetaLow <- 2 * thetaLow
                iteration <- iteration + 1
                if (iteration == maxSearchIterations) {
                    thetaLow <- -1
                }
            }

            thetaUp <- 1
            iteration <- 0
            while (iteration <= maxSearchIterations &&
                .getQFunctionResultBasedOnDataInput(
                    design = design, dataInput = dataInput,
                    theta = thetaUp, stage = finalStage,
                    infRate = infRate, directionUpper = directionUpper,
                    normalApproximation = normalApproximation,
                    equalVariances = equalVariances
                ) < 1 - design$alpha / design$sided) {
                thetaUp <- 2 * thetaUp
                iteration <- iteration + 1
                if (iteration == maxSearchIterations) {
                    thetaUp <- 1
                }
            }

            finalConfidenceInterval[1] <- .getOneDimensionalRoot(
                function(theta) {
                    return(.getQFunctionResultBasedOnDataInput(
                        design = design, dataInput = dataInput,
                        theta = theta, stage = finalStage,
                        infRate = infRate, directionUpper = directionUpper,
                        normalApproximation = normalApproximation,
                        equalVariances = equalVariances
                    ) - design$alpha / design$sided)
                },
                lower = thetaLow, upper = thetaUp, tolerance = tolerance,
                callingFunctionInformation = "Final confidence interval Fisher [1]"
            )

            finalConfidenceInterval[2] <- .getOneDimensionalRoot(
                function(theta) {
                    return(.getQFunctionResultBasedOnDataInput(
                        design = design, dataInput = dataInput,
                        theta = theta, stage = finalStage,
                        infRate = infRate, directionUpper = directionUpper,
                        normalApproximation = normalApproximation,
                        equalVariances = equalVariances
                    ) - 1 + design$alpha / design$sided)
                },
                lower = thetaLow, upper = thetaUp, tolerance = tolerance,
                callingFunctionInformation = "Final confidence interval Fisher [2]"
            )

            medianUnbiased <- .getOneDimensionalRoot(
                function(theta) {
                    return(.getQFunctionResultBasedOnDataInput(
                        design = design, dataInput = dataInput,
                        theta = theta, stage = finalStage,
                        infRate = infRate, directionUpper = directionUpper,
                        normalApproximation = normalApproximation, equalVariances = equalVariances
                    ) - 0.5)
                },
                lower = thetaLow, upper = thetaUp, tolerance = tolerance,
                callingFunctionInformation = "Final confidence interval Fisher, median unbiased"
            )
        }

        if (is.na(finalConfidenceInterval[1])) {
            finalStage <- NA_integer_
        }
    }

    return(list(
        stage = stage,
        thetaH0 = thetaH0,
        directionUpper = directionUpper,
        normalApproximation = normalApproximation,
        equalVariances = equalVariances,
        tolerance = tolerance,
        finalStage = finalStage,
        medianUnbiased = medianUnbiased,
        finalConfidenceInterval = finalConfidenceInterval
    ))
}

.getFinalConfidenceIntervalMeans <- function(..., design, dataInput,
        thetaH0 = NA_real_, directionUpper = C_DIRECTION_UPPER_DEFAULT,
        normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT, equalVariances = C_EQUAL_VARIANCES_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    .assertIsValidThetaH0DataInput(thetaH0, dataInput)
    .warnInCaseOfUnknownArguments(
        functionName = "getFinalConfidenceIntervalMeans",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design, powerCalculationEnabled = TRUE), "stage"), ...
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
        thetaH0 <- C_THETA_H0_MEANS_DEFAULT
    }

    if (.isTrialDesignGroupSequential(design)) {
        return(.getFinalConfidenceIntervalMeansGroupSequential(
            design = design, dataInput = dataInput, stage = stage, thetaH0 = thetaH0,
            directionUpper = directionUpper, normalApproximation = normalApproximation,
            equalVariances = equalVariances, tolerance = tolerance
        ))
    }

    if (.isTrialDesignInverseNormal(design)) {
        return(.getFinalConfidenceIntervalMeansInverseNormal(
            design = design, dataInput = dataInput, stage = stage, thetaH0 = thetaH0,
            directionUpper = directionUpper, normalApproximation = normalApproximation,
            equalVariances = equalVariances, tolerance = tolerance
        ))
    }

    if (.isTrialDesignFisher(design)) {
        return(.getFinalConfidenceIntervalMeansFisher(
            design = design, dataInput = dataInput, stage = stage, thetaH0 = thetaH0,
            directionUpper = directionUpper, normalApproximation = normalApproximation,
            equalVariances = equalVariances, tolerance = tolerance
        ))
    }

    .stopWithWrongDesignMessage(design, inclusiveConditionalDunnett = FALSE)
}
