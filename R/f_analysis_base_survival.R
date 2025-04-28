## |
## |  *Analysis of survival data with group sequential and combination test*
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
## |  File version: $Revision: 8695 $
## |  Last changed: $Date: 2025-04-24 14:37:00 +0200 (Do, 24 Apr 2025) $
## |  Last changed by: $Author: wassmer $
## |

#' @include f_logger.R
NULL

.getAnalysisResultsSurvival <- function(..., design, dataInput) {
    if (.isTrialDesignGroupSequential(design)) {
        return(.getAnalysisResultsSurvivalGroupSequential(
            design = design,
            dataInput = dataInput, ...
        ))
    }

    if (.isTrialDesignInverseNormal(design)) {
        return(.getAnalysisResultsSurvivalInverseNormal(
            design = design,
            dataInput = dataInput, ...
        ))
    }

    if (.isTrialDesignFisher(design)) {
        return(.getAnalysisResultsSurvivalFisher(
            design = design,
            dataInput = dataInput, ...
        ))
    }

    .stopWithWrongDesignMessage(design, inclusiveConditionalDunnett = FALSE)
}

.getAnalysisResultsSurvivalInverseNormal <- function(...,
        design,
        dataInput,
        directionUpper = NA,
        thetaH0 = C_THETA_H0_SURVIVAL_DEFAULT,
        thetaH1 = NA_real_,
        nPlanned = NA_real_,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .assertIsTrialDesignInverseNormal(design)
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    .warnInCaseOfUnknownArguments(
        functionName = ".getAnalysisResultsSurvivalInverseNormal",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), "stage"), ...
    )

    results <- AnalysisResultsInverseNormal$new(design = design, dataInput = dataInput)

    .getAnalysisResultsSurvivalAll(
        results = results,
        design = design,
        dataInput = dataInput,
        stage = stage,
        directionUpper = directionUpper,
        thetaH0 = thetaH0,
        thetaH1 = thetaH1,
        nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned,
        tolerance = tolerance
    )

    return(results)
}

.getAnalysisResultsSurvivalGroupSequential <- function(...,
        design,
        dataInput,
        directionUpper = NA,
        thetaH0 = C_THETA_H0_SURVIVAL_DEFAULT,
        thetaH1 = NA_real_,
        nPlanned = NA_real_,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .assertIsTrialDesignGroupSequential(design)
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    .warnInCaseOfUnknownArguments(
        functionName = ".getAnalysisResultsSurvivalGroupSequential",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), "stage"), ...
    )

    results <- AnalysisResultsGroupSequential$new(design = design, dataInput = dataInput)

    .getAnalysisResultsSurvivalAll(
        results = results,
        design = design,
        dataInput = dataInput,
        stage = stage,
        directionUpper = directionUpper,
        thetaH0 = thetaH0,
        thetaH1 = thetaH1,
        nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned,
        tolerance = tolerance
    )

    return(results)
}

.getAnalysisResultsSurvivalFisher <- function(...,
        design,
        dataInput,
        directionUpper = NA,
        thetaH0 = C_THETA_H0_SURVIVAL_DEFAULT,
        thetaH1 = NA_real_,
        nPlanned = NA_real_,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT,
        iterations = C_ITERATIONS_DEFAULT,
        seed = NA_real_) {
    .assertIsTrialDesignFisher(design)
    .assertIsValidIterationsAndSeed(iterations, seed, zeroIterationsAllowed = FALSE)
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    .warnInCaseOfUnknownArguments(
        functionName = ".getAnalysisResultsSurvivalFisher",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), "stage"), ...
    )

    results <- AnalysisResultsFisher$new(design = design, dataInput = dataInput)
    .setValueAndParameterType(results, "iterations", as.integer(iterations), C_ITERATIONS_DEFAULT)
    .setValueAndParameterType(results, "seed", seed, NA_real_)

    .getAnalysisResultsSurvivalAll(
        results = results,
        design = design,
        dataInput = dataInput,
        stage = stage,
        directionUpper = directionUpper,
        thetaH0 = thetaH0,
        thetaH1 = thetaH1,
        nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned,
        tolerance = tolerance,
        iterations = iterations,
        seed = seed
    )

    return(results)
}

#'
#' The following parameters will be taken from 'design':
#' stages, informationRate, criticalValues, futilityBounds, alphaSpent, stageLevels
#'
#' @noRd
#'
.getAnalysisResultsSurvivalAll <- function(...,
        results,
        design,
        dataInput,
        stage,
        directionUpper,
        thetaH0,
        thetaH1,
        nPlanned,
        allocationRatioPlanned,
        tolerance,
        iterations,
        seed) {
    startTime <- Sys.time()
    stageResults <- .getStageResultsSurvival(
        design = design,
        dataInput = dataInput,
        stage = stage,
        thetaH0 = thetaH0,
        directionUpper = directionUpper
    )
    results$.setStageResults(stageResults)
    .logProgress("Stage results calculated", startTime = startTime)

    thetaH1User <- thetaH1
    thetaH1 <- .assertIsValidThetaH1(thetaH1, stageResults, stage)
    .assertIsInOpenInterval(thetaH1, "thetaH1", 0, Inf)
    if (identical(thetaH1, thetaH1User)) {
        .setValueAndParameterType(results, "thetaH1", thetaH1, NA_real_)
    } else {
        results$thetaH1 <- thetaH1
        results$.setParameterType("thetaH1", C_PARAM_GENERATED)
    }
    .warnInCaseOfUnusedConditionalPowerArgument(results, nPlanned, "thetaH1", thetaH1)

    .setValueAndParameterType(results, "directionUpper", directionUpper, C_DIRECTION_UPPER_DEFAULT)
    .setValueAndParameterType(results, "normalApproximation", TRUE, TRUE)
    .setValueAndParameterType(results, "thetaH0", thetaH0, C_THETA_H0_SURVIVAL_DEFAULT)
    .setConditionalPowerArguments(results, dataInput, nPlanned, allocationRatioPlanned)

    # test actions
    results$testActions <- getTestActions(stageResults = stageResults)
    results$.setParameterType("testActions", C_PARAM_GENERATED)

    if (design$kMax > 1) {
        # conditional power
        startTime <- Sys.time()
        if (.isTrialDesignFisher(design)) {
            results$.conditionalPowerResults <- .getConditionalPowerSurvival(
                stageResults = stageResults,
                nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned,
                thetaH1 = thetaH1, iterations = iterations, seed = seed
            )
            .synchronizeIterationsAndSeed(results)
        } else {
            results$.conditionalPowerResults <- .getConditionalPowerSurvival(
                stageResults = stageResults,
                nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned,
                thetaH1 = thetaH1
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
    repeatedConfidenceIntervals <- .getRepeatedConfidenceIntervalsSurvival(
        design = design,
        dataInput = dataInput,
        stage = stage,
        directionUpper = directionUpper,
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
            finalValue = finalPValue$pFinal, finalStage = finalPValue$finalStage
        )
        results$finalStage <- finalPValue$finalStage
        results$.setParameterType("finalPValues", C_PARAM_GENERATED)
        results$.setParameterType("finalStage", C_PARAM_GENERATED)
        .logProgress("Final p-value calculated", startTime = startTime)

        # final confidence interval & median unbiased estimate
        startTime <- Sys.time()
        finalConfidenceIntervals <- .getFinalConfidenceIntervalSurvival(
            design = design, dataInput = dataInput, thetaH0 = thetaH0, stage = stage,
            directionUpper = directionUpper, tolerance = tolerance
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

#' @title
#' Get Stage Results Survival
#'
#' @description
#' Returns a stage results object
#'
#' @param design the trial design.
#'
#' @return Returns a \code{StageResultsSurvival} object.
#'
#' @keywords internal
#'
#' @noRd
#'
.getStageResultsSurvival <- function(...,
        design,
        dataInput,
        thetaH0 = C_THETA_H0_SURVIVAL_DEFAULT,
        directionUpper = NA,
        stage = NA_integer_,
        userFunctionCallEnabled = FALSE) {
    .assertIsDatasetSurvival(dataInput)
    .assertIsValidThetaH0DataInput(thetaH0, dataInput)
    .warnInCaseOfUnknownArguments(
        functionName = "getStageResultsSurvival",
        ignore = .getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design, powerCalculationEnabled = TRUE), ...
    )
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design, stage = stage)

    overallEvents <- dataInput$getOverallEventsUpTo(stage, group = 1)
    overallAllocationRatios <- dataInput$getOverallAllocationRatiosUpTo(stage, group = 1)

    # Calculation of overall log-ranks for specified hypothesis
    overallLogRankTestStatistics <- dataInput$getOverallLogRanksUpTo(stage, group = 1) -
        sqrt(overallEvents) * sqrt(overallAllocationRatios) / (1 + overallAllocationRatios) * log(thetaH0)

    effectSizes <- exp(dataInput$getOverallLogRanksUpTo(stage, group = 1) * (1 + overallAllocationRatios[1:stage]) /
        sqrt(overallAllocationRatios[1:stage] * overallEvents[1:stage]))

    events <- dataInput$getEventsUpTo(stage, group = 1)
    allocationRatios <- dataInput$getAllocationRatiosUpTo(stage, group = 1)

    # Calculation of log-ranks for specified hypothesis
    logRankTestStatistics <- dataInput$getLogRanksUpTo(stage, group = 1) -
        sqrt(events) * sqrt(allocationRatios) / (1 + allocationRatios) * log(thetaH0)

    # Calculation of stage-wise test statistics and combination tests
    pValues <- rep(NA_real_, design$kMax)
    combInverseNormal <- rep(NA_real_, design$kMax)
    combFisher <- rep(NA_real_, design$kMax)
    weightsInverseNormal <- .getWeightsInverseNormal(design)
    weightsFisher <- .getWeightsFisher(design)
    pValues <- .applyDirectionOfAlternative(stats::pnorm(logRankTestStatistics),
        directionUpper,
        type = "oneMinusValue", phase = "analysis"
    )
    overallPValues <- .applyDirectionOfAlternative(stats::pnorm(overallLogRankTestStatistics),
        directionUpper,
        type = "oneMinusValue", phase = "analysis"
    )

    for (k in 1:stage) {
        # Inverse normal test
        combInverseNormal[k] <- (weightsInverseNormal[1:k] %*% .getOneMinusQNorm(pValues[1:k])) /
            sqrt(sum(weightsInverseNormal[1:k]^2))

        # Fisher combination test
        combFisher[k] <- prod(pValues[1:k]^weightsFisher[1:k])
    }

    stageResults <- StageResultsSurvival$new(
        design = design,
        dataInput = dataInput,
        stage = as.integer(stage),
        overallTestStatistics = .fillWithNAs(overallLogRankTestStatistics, design$kMax),
        overallPValues = .fillWithNAs(overallPValues, design$kMax),
        overallEvents = .fillWithNAs(overallEvents, design$kMax),
        overallAllocationRatios = .fillWithNAs(overallAllocationRatios, design$kMax),
        events = .fillWithNAs(events, design$kMax),
        allocationRatios = .fillWithNAs(allocationRatios, design$kMax),
        testStatistics = .fillWithNAs(logRankTestStatistics, design$kMax),
        pValues = .fillWithNAs(pValues, design$kMax),
        effectSizes = .fillWithNAs(effectSizes, design$kMax),
        combInverseNormal = combInverseNormal,
        combFisher = combFisher,
        weightsFisher = weightsFisher,
        weightsInverseNormal = weightsInverseNormal,
        thetaH0 = thetaH0,
        direction = ifelse(!isFALSE(directionUpper), C_DIRECTION_UPPER, C_DIRECTION_LOWER)
    )

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
#' Calculation of lower and upper limits of repeated confidence intervals (RCIs) for Survival
#'
#' @noRd
#'
.getRepeatedConfidenceIntervalsSurvival <- function(..., design) {
    if (.isTrialDesignGroupSequential(design)) {
        return(.getRepeatedConfidenceIntervalsSurvivalGroupSequential(design = design, ...))
    }

    if (.isTrialDesignInverseNormal(design)) {
        return(.getRepeatedConfidenceIntervalsSurvivalInverseNormal(design = design, ...))
    }

    if (.isTrialDesignFisher(design)) {
        return(.getRepeatedConfidenceIntervalsSurvivalFisher(design = design, ...))
    }

    .stopWithWrongDesignMessage(design, inclusiveConditionalDunnett = FALSE)
}

.getRootThetaSurvival <- function(...,
        design,
        dataInput,
        stage,
        directionUpper,
        thetaLow,
        thetaUp,
        firstParameterName,
        secondValue,
        tolerance,
        callingFunctionInformation) {
    result <- .getOneDimensionalRoot(
        function(theta) {
            stageResults <- .getStageResultsSurvival(
                design = design, dataInput = dataInput,
                stage = stage, thetaH0 = theta, directionUpper = directionUpper
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

.getUpperLowerThetaSurvival <- function(...,
        design,
        dataInput,
        theta,
        stage,
        directionUpper,
        conditionFunction,
        firstParameterName,
        secondValue) {
    stageResults <- .getStageResultsSurvival(
        design = design, dataInput = dataInput,
        stage = stage, thetaH0 = exp(theta), directionUpper = directionUpper
    )

    firstValue <- stageResults[[firstParameterName]][stage]

    if (.isTrialDesignGroupSequential(design)) {
        firstValue <- .getOneMinusQNorm(firstValue)
    }

    maxSearchIterations <- 30
    while (conditionFunction(secondValue, firstValue)) {
        theta <- 2 * theta

        stageResults <- .getStageResultsSurvival(
            design = design, dataInput = dataInput,
            stage = stage, thetaH0 = exp(theta), directionUpper = directionUpper
        )

        firstValue <- stageResults[[firstParameterName]][stage]
        if (.isTrialDesignGroupSequential(design)) {
            firstValue <- .getOneMinusQNorm(firstValue)
        }

        maxSearchIterations <- maxSearchIterations - 1
        if (maxSearchIterations < 0) {
            stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE,
                sprintf(
                    paste0(
                        "failed to find theta (k = %s, firstValue = %s, ",
                        "secondValue = %s, levels(firstValue) = %s, theta = %s)"
                    ),
                    stage, stageResults[[firstParameterName]][stage], secondValue,
                    firstValue, theta
                ),
                call. = FALSE
            )
        }
    }

    return(theta)
}

.getRepeatedConfidenceIntervalsSurvivalAll <- function(...,
        design,
        dataInput,
        directionUpper = NA,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT,
        firstParameterName) {
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)

    # necessary for adjustment for binding futility boundaries
    futilityCorr <- rep(NA_real_, design$kMax)

    criticalValues <- .getCriticalValues(design)

    if (.isTrialDesignFisher(design)) {
        bounds <- design$alpha0Vec
        border <- C_ALPHA_0_VEC_DEFAULT
        conditionFunction <- .isFirstValueSmallerThanSecondValue
    } else {
        bounds <- design$futilityBounds
        criticalValues[is.infinite(criticalValues) & criticalValues > 0] <- C_QNORM_MAXIMUM
        criticalValues[is.infinite(criticalValues) & criticalValues < 0] <- C_QNORM_MINIMUM
        border <- C_FUTILITY_BOUNDS_DEFAULT
        conditionFunction <- .isFirstValueGreaterThanSecondValue
    }

    repeatedConfidenceIntervals <- matrix(NA_real_, 2, design$kMax)
    for (k in (1:stage)) {
        startTime <- Sys.time()
        if (criticalValues[k] < C_QNORM_MAXIMUM) {
            # Finding maximum upper and minimum lower bounds for RCIs
            thetaLow <- exp(.getUpperLowerThetaSurvival(
                design = design, dataInput = dataInput,
                theta = -1, stage = k, directionUpper = TRUE,
                conditionFunction = conditionFunction, firstParameterName = firstParameterName,
                secondValue = criticalValues[k]
            ))

            thetaUp <- exp(.getUpperLowerThetaSurvival(
                design = design, dataInput = dataInput,
                theta = 1, stage = k, directionUpper = FALSE,
                conditionFunction = conditionFunction, firstParameterName = firstParameterName,
                secondValue = criticalValues[k]
            ))

            # Finding upper and lower RCI limits through root function
            repeatedConfidenceIntervals[1, k] <- .getRootThetaSurvival(
                design = design,
                dataInput = dataInput,
                stage = k,
                directionUpper = TRUE,
                thetaLow = thetaLow,
                thetaUp = thetaUp,
                firstParameterName = firstParameterName,
                secondValue = criticalValues[k],
                tolerance = tolerance,
                callingFunctionInformation = paste0("Repeated confidence interval [1, ", k, "]")
            )

            repeatedConfidenceIntervals[2, k] <- .getRootThetaSurvival(
                design = design,
                dataInput = dataInput,
                stage = k,
                directionUpper = FALSE,
                thetaLow = thetaLow,
                thetaUp = thetaUp,
                firstParameterName = firstParameterName,
                secondValue = criticalValues[k],
                tolerance = tolerance,
                callingFunctionInformation = paste0("Repeated confidence interval [2, ", k, "]")
            )

            # Adjustment for binding futility bounds
            if (k > 1 && !is.na(bounds[k - 1]) && conditionFunction(bounds[k - 1], border) && design$bindingFutility) {
                parameterName <- ifelse(.isTrialDesignFisher(design), "pValues", firstParameterName)

                futilityCorr[k] <- .getRootThetaSurvival(
                    design = design, dataInput = dataInput, stage = k - 1, directionUpper = directionUpper,
                    thetaLow = thetaLow, thetaUp = thetaUp,
                    firstParameterName = parameterName, secondValue = bounds[k - 1], tolerance = tolerance,
                    callingFunctionInformation = paste0("Repeated confidence interval, futility correction [", k, "]")
                )

                if (is.na(directionUpper) || isTRUE(directionUpper)) {
                    repeatedConfidenceIntervals[1, k] <- min(min(futilityCorr[2:k]), repeatedConfidenceIntervals[1, k])
                } else {
                    repeatedConfidenceIntervals[2, k] <- max(max(futilityCorr[2:k]), repeatedConfidenceIntervals[2, k])
                }
            }
            .logProgress("Repeated confidence interval of stage %s calculated", startTime = startTime, k)

            if (!is.na(repeatedConfidenceIntervals[1, k]) && !is.na(repeatedConfidenceIntervals[2, k]) &&
                    repeatedConfidenceIntervals[1, k] > repeatedConfidenceIntervals[2, k]) {
                repeatedConfidenceIntervals[, k] <- rep(NA_real_, 2)
            }
        }
    }

    return(repeatedConfidenceIntervals)
}

#'
#' RCIs based on group sequential method
#'
#' @noRd
#'
.getRepeatedConfidenceIntervalsSurvivalGroupSequential <- function(...,
        design,
        dataInput,
        directionUpper = NA,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .warnInCaseOfUnknownArguments(
        functionName =
            ".getRepeatedConfidenceIntervalsSurvivalGroupSequential",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design, powerCalculationEnabled = TRUE), "stage"), ...
    )

    return(.getRepeatedConfidenceIntervalsSurvivalAll(
        design = design,
        dataInput = dataInput,
        firstParameterName = "overallPValues",
        directionUpper = directionUpper,
        tolerance = tolerance,
        ...
    ))
}

#'
#' RCIs based on inverse normal combination test
#'
#' @noRd
#'
.getRepeatedConfidenceIntervalsSurvivalInverseNormal <- function(...,
        design,
        dataInput,
        directionUpper = NA,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .warnInCaseOfUnknownArguments(
        functionName =
            ".getRepeatedConfidenceIntervalsSurvivalInverseNormal",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design, powerCalculationEnabled = TRUE), "stage"), ...
    )

    return(.getRepeatedConfidenceIntervalsSurvivalAll(
        design = design,
        dataInput = dataInput,
        firstParameterName = "combInverseNormal",
        directionUpper = directionUpper,
        tolerance = tolerance,
        ...
    ))
}

#'
#' RCIs based on Fisher's combination test
#'
#' @noRd
#'
.getRepeatedConfidenceIntervalsSurvivalFisher <- function(...,
        design,
        dataInput,
        directionUpper = NA,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .warnInCaseOfUnknownArguments(
        functionName =
            ".getRepeatedConfidenceIntervalsSurvivalFisher",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design, powerCalculationEnabled = TRUE), "stage"), ...
    )

    return(.getRepeatedConfidenceIntervalsSurvivalAll(
        design = design,
        dataInput = dataInput,
        firstParameterName = "combFisher",
        directionUpper = directionUpper,
        tolerance = tolerance,
        ...
    ))
}

#'
#' Calculation of conditional power based on group sequential method
#'
#' @noRd
#'
.getConditionalPowerSurvivalGroupSequential <- function(...,
        stageResults,
        stage = stageResults$stage,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        nPlanned = NA_real_,
        thetaH1 = NA_real_) {
    design <- stageResults$.design
    .assertIsTrialDesignGroupSequential(design)
    .assertIsValidStage(stage, design$kMax)
    .warnInCaseOfUnknownArguments(
        functionName = ".getConditionalPowerSurvivalGroupSequential",
        ignore = c("design", "stageResultsName"), ...
    )

    kMax <- design$kMax
    conditionalPower <- rep(NA_real_, kMax)
    weights <- stageResults$weightsInverseNormal
    informationRates <- design$informationRates

    nPlanned <- c(rep(NA, stageResults$stage), nPlanned)

    if (stageResults$stage == kMax) {
        .logDebug(
            "Conditional power will be calculated only for subsequent stages ",
            "(stage = ", stageResults$stage, ", kMax = ", design$kMax, ")"
        )
        return(list(
            nPlanned = nPlanned,
            conditionalPower = conditionalPower
        ))
    }

    criticalValuesInverseNormal <- .getCriticalValues(design)

    .assertIsSingleNumber(allocationRatioPlanned, "allocationRatioPlanned")
    .assertIsInOpenInterval(
        allocationRatioPlanned,
        "allocationRatioPlanned", 0, C_ALLOCATION_RATIO_MAXIMUM
    )
    nPlanned <- allocationRatioPlanned / (1 + allocationRatioPlanned)^2 * nPlanned

    if (stageResults$direction == "upper") {
        thetaH1 <- log(thetaH1 / stageResults$thetaH0)
    } else {
        thetaH1 <- -log(thetaH1 / stageResults$thetaH0)
    }

    # Shifted decision region for use in getGroupSeqProbs
    # Group sequential method
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
            .getOneMinusQNorm(stageResults$overallPValues[stage]) * sqrt(sum(weights[1:stage]^2)) /
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

    # Scaled information for use in getGroupSeqProbs
    scaledInformation <- (informationRates[(stage + 1):kMax] - informationRates[stage]) /
        (1 - informationRates[stage])

    if (design$sided == 2) {
        decisionMatrix <- matrix(c(
            shiftedDecisionRegionLower,
            shiftedDecisionRegionUpper
        ), nrow = 2, byrow = TRUE)
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

    nPlanned <- (1 + allocationRatioPlanned)^2 / allocationRatioPlanned * nPlanned

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
.getConditionalPowerSurvivalInverseNormal <- function(...,
        stageResults,
        stage = stageResults$stage,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        nPlanned = NA_real_,
        thetaH1 = NA_real_) {
    design <- stageResults$.design
    .assertIsTrialDesignInverseNormal(design)
    .assertIsValidStage(stage, design$kMax)
    .warnInCaseOfUnknownArguments(
        functionName = ".getConditionalPowerSurvivalInverseNormal",
        ignore = c("design", "stageResultsName"), ...
    )

    kMax <- design$kMax
    conditionalPower <- rep(NA_real_, kMax)
    weights <- stageResults$weightsInverseNormal
    informationRates <- design$informationRates

    nPlanned <- c(rep(NA, stageResults$stage), nPlanned)

    if (stageResults$stage == kMax) {
        .logDebug(
            "Conditional power will be calculated only for subsequent stages ",
            "(stage = ", stageResults$stage, ", kMax = ", design$kMax, ")"
        )
        return(list(
            nPlanned = nPlanned,
            conditionalPower = conditionalPower
        ))
    }

    criticalValuesInverseNormal <- .getCriticalValues(design)

    .assertIsSingleNumber(allocationRatioPlanned, "allocationRatioPlanned")
    .assertIsInOpenInterval(
        allocationRatioPlanned,
        "allocationRatioPlanned", 0, C_ALLOCATION_RATIO_MAXIMUM
    )
    nPlanned <- allocationRatioPlanned / (1 + allocationRatioPlanned)^2 * nPlanned

    if (stageResults$direction == "upper") {
        thetaH1 <- log(thetaH1 / stageResults$thetaH0)
    } else {
        thetaH1 <- -log(thetaH1 / stageResults$thetaH0)
    }

    # Shifted decision region for use in getGroupSeqProbs
    # Inverse normal method
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
            .getOneMinusQNorm(stageResults$overallPValues[stage]) * sqrt(sum(weights[1:stage]^2)) /
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

    # Scaled information for use in getGroupSeqProbs
    scaledInformation <- (informationRates[(stage + 1):kMax] - informationRates[stage]) /
        (1 - informationRates[stage])

    if (design$sided == 2) {
        decisionMatrix <- matrix(c(
            shiftedDecisionRegionLower,
            shiftedDecisionRegionUpper
        ), nrow = 2, byrow = TRUE)
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

    nPlanned <- (1 + allocationRatioPlanned)^2 / allocationRatioPlanned * nPlanned

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
.getConditionalPowerSurvivalFisher <- function(...,
        stageResults,
        stage = stageResults$stage,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        nPlanned = NA_real_,
        thetaH1 = NA_real_,
        iterations = C_ITERATIONS_DEFAULT,
        seed = NA_real_) {
    design <- stageResults$.design
    .assertIsTrialDesignFisher(design)
    .assertIsValidStage(stage, design$kMax)
    .assertIsValidIterationsAndSeed(iterations, seed, zeroIterationsAllowed = FALSE)
    .warnInCaseOfUnknownArguments(
        functionName = ".getConditionalPowerSurvivalFisher",
        ignore = c("design", "piTreatmentRange", "stageResultsName"), ...
    )

    kMax <- design$kMax
    conditionalPower <- rep(NA_real_, kMax)
    seed <- .setSeed(seed)
    simulated <- FALSE

    nPlanned <- c(rep(NA, stageResults$stage), nPlanned)

    .assertIsSingleNumber(allocationRatioPlanned, "allocationRatioPlanned")
    .assertIsInOpenInterval(allocationRatioPlanned, "allocationRatioPlanned", 0, C_ALLOCATION_RATIO_MAXIMUM)
    nPlanned <- allocationRatioPlanned / (1 + allocationRatioPlanned)^2 * nPlanned

    if (stageResults$direction == "upper") {
        thetaH1 <- log(thetaH1 / stageResults$thetaH0)
    } else {
        thetaH1 <- -log(thetaH1 / stageResults$thetaH0)
    }

    criticalValues <- .getCriticalValues(design)
    weightsFisher <- stageResults$weightsFisher
    pValues <- stageResults$pValues

    if (stageResults$stage < kMax - 1) {
        for (k in (stageResults$stage + 1):kMax) {
            reject <- 0
            for (i in 1:iterations) {
                reject <- reject + .getRejectValueConditionalPowerFisher(
                    kMax = kMax, alpha0Vec = design$alpha0Vec,
                    criticalValues = criticalValues, weightsFisher = weightsFisher,
                    pValues = pValues, currentKMax = k, thetaH1 = thetaH1,
                    stage = stageResults$stage, nPlanned = nPlanned
                )
            }
            conditionalPower[k] <- reject / iterations
        }
        simulated <- TRUE
    }

    if (stageResults$stage == kMax - 1) {
        divisor <- prod(pValues[1:(kMax - 1)]^weightsFisher[1:(kMax - 1)])
        result <- 1 - (criticalValues[kMax] / divisor)^(1 / weightsFisher[kMax])
        if (result <= 0 || result >= 1) {
            warning("Calculation not possible: could not calculate conditional power for stage ", kMax, call. = FALSE)
            conditionalPower[kMax] <- NA_real_
        } else {
            conditionalPower[kMax] <- 1 - stats::pnorm(.getQNorm(result) - thetaH1 * sqrt(nPlanned[kMax]))
        }
    }

    nPlanned <- (1 + allocationRatioPlanned)^2 / allocationRatioPlanned * nPlanned

    return(list(
        nPlanned = nPlanned,
        conditionalPower = conditionalPower,
        iterations = as.integer(iterations),
        seed = seed,
        simulated = simulated
    ))
}

.getConditionalPowerSurvival <- function(...,
        stageResults, nPlanned = NA_real_,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        thetaH1 = NA_real_) {
    results <- ConditionalPowerResultsSurvival$new(
        .stageResults = stageResults,
        .design = stageResults$.design, nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned, thetaH1 = thetaH1
    )

    if (any(is.na(nPlanned))) {
        return(results)
    }

    stage <- stageResults$stage
    thetaH1 <- .assertIsValidThetaH1(thetaH1, stageResults, stage)
    .assertIsInOpenInterval(thetaH1, "thetaH1", 0, Inf)

    if (!.isValidNPlanned(nPlanned = nPlanned, kMax = stageResults$.design$kMax, stage = stage)) {
        return(results)
    }

    if (.isTrialDesignGroupSequential(stageResults$.design)) {
        cp <- .getConditionalPowerSurvivalGroupSequential(
            stageResults = stageResults, nPlanned = nPlanned,
            allocationRatioPlanned = allocationRatioPlanned, thetaH1 = thetaH1, ...
        )
    } else if (.isTrialDesignInverseNormal(stageResults$.design)) {
        cp <- .getConditionalPowerSurvivalInverseNormal(
            stageResults = stageResults, nPlanned = nPlanned,
            allocationRatioPlanned = allocationRatioPlanned, thetaH1 = thetaH1, ...
        )
    } else if (.isTrialDesignFisher(stageResults$.design)) {
        cp <- .getConditionalPowerSurvivalFisher(
            stageResults = stageResults, nPlanned = nPlanned,
            allocationRatioPlanned = allocationRatioPlanned, thetaH1 = thetaH1, ...
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
    results$.setParameterType("thetaH1", ifelse(is.na(thetaH1), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))

    return(results)
}

.getConditionalPowerPlotSurvival <- function(...,
        stageResults,
        stage,
        nPlanned,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        thetaRange) {
    .assertIsValidAllocationRatioPlanned(allocationRatioPlanned, 2)
    .associatedArgumentsAreDefined(nPlanned = nPlanned, thetaRange = thetaRange)
    .warnInCaseOfUnknownArguments(
        functionName = ".getConditionalPowerPlotSurvival",
        ignore = c("iterations", "seed", "stageResultsName", "grid"), ...
    )

    design <- stageResults$.design
    if (!.isValidNPlanned(nPlanned = nPlanned, kMax = design$kMax, stage = stage)) {
        return(list(
            xValues = 0,
            condPowerValues = 0,
            likelihoodValues = 0,
            main = C_PLOT_MAIN_CONDITIONAL_POWER_WITH_LIKELIHOOD,
            xlab = "Hazard ratio",
            ylab = C_PLOT_YLAB_CONDITIONAL_POWER_WITH_LIKELIHOOD,
            sub = ""
        ))
    }

    thetaRange <- .assertIsValidThetaRange(thetaRange = thetaRange, survivalDataEnabled = TRUE)

    condPowerValues <- rep(NA, length(thetaRange))
    likelihoodValues <- rep(NA, length(thetaRange))

    warningMessages <- c()
    withCallingHandlers(
        for (i in seq(along = thetaRange)) {
            if (.isTrialDesignGroupSequential(design)) {
                condPowerValues[i] <- .getConditionalPowerSurvivalGroupSequential(
                    stageResults = stageResults, nPlanned = nPlanned,
                    allocationRatioPlanned = allocationRatioPlanned,
                    thetaH1 = thetaRange[i]
                )$conditionalPower[design$kMax]
            }

            if (.isTrialDesignInverseNormal(design)) {
                condPowerValues[i] <- .getConditionalPowerSurvivalInverseNormal(
                    stageResults = stageResults, nPlanned = nPlanned,
                    allocationRatioPlanned = allocationRatioPlanned,
                    thetaH1 = thetaRange[i]
                )$conditionalPower[design$kMax]
            }

            if (.isTrialDesignFisher(design)) {
                condPowerValues[i] <- .getConditionalPowerSurvivalFisher(
                    stageResults = stageResults, nPlanned = nPlanned,
                    allocationRatioPlanned = allocationRatioPlanned,
                    thetaH1 = thetaRange[i]
                )$conditionalPower[design$kMax]
            }

            likelihoodValues[i] <- stats::dnorm(
                log(thetaRange[i]), log(stageResults$effectSizes[stage]),
                2 / sqrt(stageResults$overallEvents[stage])
            ) /
                stats::dnorm(0, 0, 2 / sqrt(stageResults$overallEvents[stage]))
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

    subtitle <- paste0(
        "Stage = ", stage, ", maximum number of remaining events = ",
        sum(nPlanned), ", allocation ratio = ", .formatSubTitleValue(allocationRatioPlanned, "allocationRatioPlanned")
    )

    return(list(
        xValues = thetaRange,
        condPowerValues = condPowerValues,
        likelihoodValues = likelihoodValues,
        main = C_PLOT_MAIN_CONDITIONAL_POWER_WITH_LIKELIHOOD,
        xlab = "Hazard ratio",
        ylab = C_PLOT_YLAB_CONDITIONAL_POWER_WITH_LIKELIHOOD,
        sub = subtitle
    ))
}

.getFinalConfidenceIntervalSurvivalValues <- function(
        design,
        dataInput,
        stageResults,
        directionUpper,
        thetaH0,
        stage,
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

    # No early efficacy stopping design
    if (stage == design$kMax && .isNoEarlyEfficacy(design)) {
        if (.isTrialDesignGroupSequential(design)) {
            finalConfidenceInterval <- .getRepeatedConfidenceIntervalsSurvivalGroupSequential(
                design = design,
                dataInput = dataInput,
                directionUpper = directionUpper,
                tolerance = C_ANALYSIS_TOLERANCE_DEFAULT
            )[, design$kMax]
        } else {
            finalConfidenceInterval <- .getRepeatedConfidenceIntervalsSurvivalInverseNormal(
                design = design,
                dataInput = dataInput,
                directionUpper = directionUpper,
                tolerance = C_ANALYSIS_TOLERANCE_DEFAULT
            )[, design$kMax]
        }
        medianUnbiased <- (finalConfidenceInterval[1] + finalConfidenceInterval[2]) / 2
    }
    # Early stopping or at end of study
    else if (designStage < design$kMax || stage == design$kMax) {
        if (designStage == 1) {
            finalConfidenceIntervalGeneral[1] <- stageResults$testStatistics[1] -
                .getOneMinusQNorm(design$alpha / design$sided)
            finalConfidenceIntervalGeneral[2] <- stageResults$testStatistics[1] +
                .getOneMinusQNorm(design$alpha / design$sided)
            medianUnbiasedGeneral <- stageResults$testStatistics[1]
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

        if (is.na(finalConfidenceIntervalGeneral[1]) && (designStage > 1)) {
            finalStage <- NA_integer_
        }

        if (!is.na(finalStage)) {
            # Retransformation
            y <- .getStageResultsSurvival(
                design = design,
                dataInput = dataInput,
                stage = finalStage,
                thetaH0 = thetaH0,
                directionUpper = directionUpper
            )

            stderr <- (1 + y$overallAllocationRatios[finalStage]) / sqrt(y$overallAllocationRatios[finalStage]) /
                sqrt(stageResults$overallEvents[finalStage])

            directionUpperSign <- ifelse(!isFALSE(directionUpper), 1, -1)
            if (designStage == 1) {
                finalConfidenceInterval <- exp(stderr * finalConfidenceIntervalGeneral + log(thetaH0))
                medianUnbiased <- exp(stderr * medianUnbiasedGeneral + log(thetaH0))
            } else {
                finalConfidenceInterval[1] <- exp(finalConfidenceIntervalGeneral[1] *
                    (1 + y$overallAllocationRatios[finalStage]) /
                    sqrt(y$overallAllocationRatios[finalStage]) +
                    directionUpperSign * log(thetaH0))
                finalConfidenceInterval[2] <- exp(finalConfidenceIntervalGeneral[2] *
                    (1 + y$overallAllocationRatios[finalStage]) /
                    sqrt(y$overallAllocationRatios[finalStage]) +
                    directionUpperSign * log(thetaH0))
                medianUnbiased <- exp(medianUnbiasedGeneral *
                    (1 + y$overallAllocationRatios[finalStage]) /
                    sqrt(y$overallAllocationRatios[finalStage]) +
                    directionUpperSign * log(thetaH0))
            }
        }

        if (isFALSE(directionUpper)) {
            medianUnbiasedGeneral <- 1 / medianUnbiasedGeneral
            finalConfidenceIntervalGeneral <- 1 / finalConfidenceIntervalGeneral
            if (designStage > 1) {
                medianUnbiased <- 1 / medianUnbiased
                finalConfidenceInterval <- 1 / finalConfidenceInterval
            }
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
#' based on group sequential test without SSR (general case).
#'
#' @noRd
#'
.getFinalConfidenceIntervalSurvivalGroupSequential <- function(...,
        design,
        dataInput,
        stage,
        thetaH0 = C_THETA_H0_SURVIVAL_DEFAULT,
        directionUpper = NA,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    stageResults <- .getStageResultsSurvival(
        design = design, dataInput = dataInput, stage = stage,
        thetaH0 = thetaH0, directionUpper = directionUpper
    )

    finalConfidenceIntervalSurvivalValues <- .getFinalConfidenceIntervalSurvivalValues(
        design,
        dataInput,
        stageResults,
        directionUpper,
        thetaH0,
        stage,
        tolerance
    )

    return(list(
        stage = stage,
        thetaH0 = thetaH0,
        directionUpper = directionUpper,
        tolerance = tolerance,
        finalStage = finalConfidenceIntervalSurvivalValues$finalStage,
        medianUnbiasedGeneral = finalConfidenceIntervalSurvivalValues$medianUnbiasedGeneral,
        finalConfidenceIntervalGeneral = finalConfidenceIntervalSurvivalValues$finalConfidenceIntervalGeneral,
        medianUnbiased = finalConfidenceIntervalSurvivalValues$medianUnbiased,
        finalConfidenceInterval = finalConfidenceIntervalSurvivalValues$finalConfidenceInterval
    ))
}

#'
#' Calculation of final confidence interval
#' based on inverse normal method, only valid for kMax <= 2 or no SSR.
#'
#' @noRd
#'
.getFinalConfidenceIntervalSurvivalInverseNormal <- function(...,
        design,
        dataInput,
        stage,
        thetaH0 = C_THETA_H0_SURVIVAL_DEFAULT,
        directionUpper = NA,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    stageResults <- .getStageResultsSurvival(
        design = design,
        dataInput = dataInput,
        stage = stage,
        thetaH0 = thetaH0,
        directionUpper = directionUpper
    )

    finalConfidenceIntervalSurvivalValues <- .getFinalConfidenceIntervalSurvivalValues(
        design,
        dataInput,
        stageResults,
        directionUpper,
        thetaH0,
        stage,
        tolerance
    )

    return(list(
        stage = stage,
        thetaH0 = thetaH0,
        directionUpper = directionUpper,
        tolerance = tolerance,
        finalStage = finalConfidenceIntervalSurvivalValues$finalStage,
        medianUnbiasedGeneral = finalConfidenceIntervalSurvivalValues$medianUnbiasedGeneral,
        finalConfidenceIntervalGeneral = finalConfidenceIntervalSurvivalValues$finalConfidenceIntervalGeneral,
        medianUnbiased = finalConfidenceIntervalSurvivalValues$medianUnbiased,
        finalConfidenceInterval = finalConfidenceIntervalSurvivalValues$finalConfidenceInterval
    ))
}

#'
#' Calculation of final confidence interval
#' based on Fisher combination test, only valid for kMax <= 2.
#'
#' @noRd
#'
.getFinalConfidenceIntervalSurvivalFisher <- function(...,
        design,
        dataInput,
        stage,
        thetaH0 = C_THETA_H0_SURVIVAL_DEFAULT,
        directionUpper = NA,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    stageResults <- .getStageResultsSurvival(
        design = design,
        dataInput = dataInput,
        stage = stage,
        thetaH0 = thetaH0,
        directionUpper = directionUpper
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
        tolerance = tolerance,
        finalStage = finalStage,
        medianUnbiasedGeneral = NA_real_,
        finalConfidenceIntervalGeneral = rep(NA_real_, 2),
        medianUnbiased = NA_real_,
        finalConfidenceInterval = rep(NA_real_, 2)
    ))
}

.getFinalConfidenceIntervalSurvival <- function(...,
        design,
        dataInput,
        thetaH0 = NA_real_,
        directionUpper = NA,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)

    .assertIsValidThetaH0DataInput(thetaH0, dataInput)
    .warnInCaseOfUnknownArguments(
        functionName = "getFinalConfidenceIntervalSurvival",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), "stage"), ...
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
        thetaH0 <- C_THETA_H0_SURVIVAL_DEFAULT
    }

    if (.isTrialDesignGroupSequential(design)) {
        return(.getFinalConfidenceIntervalSurvivalGroupSequential(
            design = design,
            dataInput = dataInput,
            stage = stage,
            thetaH0 = thetaH0,
            directionUpper = directionUpper,
            tolerance = tolerance
        ))
    }

    if (.isTrialDesignInverseNormal(design)) {
        return(.getFinalConfidenceIntervalSurvivalInverseNormal(
            design = design,
            dataInput = dataInput,
            stage = stage,
            thetaH0 = thetaH0,
            directionUpper = directionUpper,
            tolerance = tolerance
        ))
    }

    if (.isTrialDesignFisher(design)) {
        return(.getFinalConfidenceIntervalSurvivalFisher(
            design = design,
            dataInput = dataInput,
            stage = stage,
            thetaH0 = thetaH0,
            directionUpper = directionUpper,
            tolerance = tolerance
        ))
    }

    .stopWithWrongDesignMessage(design, inclusiveConditionalDunnett = FALSE)
}
