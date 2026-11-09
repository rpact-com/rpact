## |
## |  rpact: Confirmatory Adaptive Clinical Trial Design and Analysis
## |  This file is part of the rpact R package.
## |
## |  RPACT — Statistical Tools for Drug Development
## |
## |  Authors: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
## |  SPDX-FileCopyrightText: RPACT GmbH, Germany <info@rpact.com>
## |  SPDX-License-Identifier: LGPL-3.0-only
## |
## |  Licensed under the GNU Lesser General Public License, version 3.
## |  License text: https://www.gnu.org/licenses/lgpl-3.0.html
## |
## |  Package documentation: https://www.rpact.org
## |  Resources and customer portal: https://connect.rpact.app
## |  Consulting and services: https://www.rpact.com
## |  Contact: info@rpact.com
## |

#'
#' @title
#' Get Performance Score
#'
#' @description
#' Calculates the conditional performance score, its sub-scores and components according to
#' (Herrmann et al. (2020), \doi{10.1002/sim.8534}) and
#' (Bokelmann et al. (2024), \doi{10.1186/s12874-024-02150-4}) for a given
#' simulation result from a two-stage design with continuous or binary endpoint.
#' Larger (sub-)score and component values refer to a better performance.
#'
#' @param simulationResult A simulation result.
#'
#' @details
#' The conditional performance score consists of two sub-scores, one for the sample size
#' (subscoreSampleSize) and one for the conditional power (subscoreConditionalPower).
#' Each of those are composed of a location (locationSampleSize, locationConditionalPower)
#' and variation component (variationSampleSize, variationConditionalPower).
#' The term conditional refers to an evaluation perspective where the interim results
#' suggest a trial continuation with a second stage.
#' The score can take values between 0 and 1. More details on the performance score
#' can be found in Herrmann et al. (2020), \doi{10.1002/sim.8534} and
#' Bokelmann et al. (2024) \doi{10.1186/s12874-024-02150-4}.
#'
#' @template examples_get_performance_score
#'
#' @author Stephen Schueuerhuis
#'
#' @export
#'
getPerformanceScore <- function(simulationResult) {
    .assertIsSimulationResults(simulationResult)

    design <- simulationResult$.design

    if (!inherits(simulationResult, "SimulationResultsMeans") && !inherits(simulationResult, "SimulationResultsRates")) {
        stopIllegalArgument("performance score so far implemented only for single comparisons with continuous and binary endpoints",
            functionName = "getPerformanceScore",
            diagnosticId = "simulation.performance_score_unsupported"
        )
    }

    if (design$kMax != 2) {
        stopIllegalArgument("performance score so far implemented only for two-stage designs",
            functionName = "getPerformanceScore",
            diagnosticId = "simulation.performance_score_unsupported"
        )
    }

    # initialize necessary sample size values
    plannedSubjects <- simulationResult$plannedSubjects
    maxAdditionalNumberOfSubjects <- ifelse(is.na(simulationResult$conditionalPower),
        plannedSubjects[2] - plannedSubjects[1],
        simulationResult$maxNumberOfSubjectsPerStage[2]
    )

    # number of iterations
    iterations <- simulationResult$maxNumberOfIterations

    # target CP
    targetConditionalPower <- ifelse(is.na(simulationResult$conditionalPower),
        1 - design$beta,
        simulationResult$conditionalPower
    )
    args <- list(
        design = getDesignGroupSequential(
            kMax = 1,
            alpha = design$alpha,
            beta = design$beta
        ),
        thetaH0 = 0,
        normalApproximation = TRUE,
        groups = simulationResult$groups
    )

    alternativeParamName <- NA_character_
    referenceValue <- NA_real_

    # simulated alternative values
    if (methods::is(simulationResult, "SimulationResultsMeans")) {
        alternativeParamName <- "alternative"
        referenceValue <- 0
    } else if (methods::is(simulationResult, "SimulationResultsRates")) {
        alternativeParamName <- "pi1"
        referenceValue <- simulationResult$pi2
        args$pi2 <- referenceValue
    } else {
        stopIllegalArgument("performance score is not available for class ",
            .getClassName(simulationResult),
            functionName = "getPerformanceScore",
            diagnosticId = "simulation.performance_score_unsupported"
        )
    }
    alternativeValues <- simulationResult[[alternativeParamName]]

    simData <- simulationResult$.data
    resultMatrix <- sapply(alternativeValues, FUN = function(alternativeValue) {
        args[[alternativeParamName]] <- alternativeValue

        if (alternativeValue == referenceValue) {
            singleStageSampleSize <- plannedSubjects[2] - plannedSubjects[1]
        } else if (methods::is(simulationResult, "SimulationResultsMeans")) {
            singleStageSampleSize <- do.call(getSampleSizeMeans, args)$numberOfSubjects
        } else if (methods::is(simulationResult, "SimulationResultsRates")) {
            singleStageSampleSize <- do.call(getSampleSizeRates, args)$numberOfSubjects
        }

        # iterations in which the trial has proceed to stage two
        secondStageIterations <- simData[
            simData$stageNumber == 2 & simData[[alternativeParamName]] == alternativeValue,
        ]

        # mean and variance estimates for sample size and conditional power
        meanSampleSize <- mean(secondStageIterations$numberOfCumulatedSubjects, na.rm = TRUE)
        varSampleSize <- stats::var(secondStageIterations$numberOfCumulatedSubjects, na.rm = TRUE)

        meanConditionalPower <- mean(secondStageIterations$conditionalPowerAchieved, na.rm = TRUE)
        varConditionalPower <- stats::var(secondStageIterations$conditionalPowerAchieved, na.rm = TRUE)

        # target sample size: single stage sample size if it doesn't exceed maximum admissible
        # sample size, otherwise only first stage sample size
        targetSampleSize <- ifelse(singleStageSampleSize <= (maxAdditionalNumberOfSubjects + plannedSubjects[1]),
            singleStageSampleSize, plannedSubjects[2] - plannedSubjects[1]
        )

        # sample size components
        locationSampleSize <- 1 - abs(meanSampleSize - targetSampleSize) / maxAdditionalNumberOfSubjects
        maxVariationSampleSize <- (maxAdditionalNumberOfSubjects / 2)^2 * iterations / (iterations - 1)
        variationSampleSize <- 1 - sqrt(varSampleSize / maxVariationSampleSize)
        subscoreSampleSize <- mean(c(locationSampleSize, variationSampleSize), na.rm = TRUE)

        # conditional power components
        locationConditionalPower <- 1 - abs(meanConditionalPower - targetConditionalPower) / (1 - design$alpha)
        maxVariationConditionalPower <- (1 / 2)^2 * iterations / (iterations - 1)
        variationConditionalPower <- 1 - sqrt(varConditionalPower / maxVariationConditionalPower)
        subscoreConditionalPower <- mean(c(locationConditionalPower, variationConditionalPower), na.rm = TRUE)

        # performance score calculation
        performanceScore <- mean(c(subscoreSampleSize, subscoreConditionalPower), na.rm = TRUE)

        return(c(
            locationSampleSize = locationSampleSize,
            variationSampleSize = variationSampleSize,
            subscoreSampleSize = subscoreSampleSize,
            locationConditionalPower = locationConditionalPower,
            variationConditionalPower = variationConditionalPower,
            subscoreConditionalPower = subscoreConditionalPower,
            performanceScore = performanceScore
        ))
    })

    performanceScore <- PerformanceScore$new(simulationResult)
    performanceScore$.alternative <- alternativeValues
    paramNames <- rownames(resultMatrix)
    for (k in 1:nrow(resultMatrix)) {
        paramName <- paramNames[k]
        performanceScore[[paramName]] <- resultMatrix[k, ]
        performanceScore$.setParameterType(paramName, C_PARAM_GENERATED)
    }

    if (inherits(simulationResult, "SimulationResultsRates")) {
        warnNotValidated("The performance score function is experimental and hence not fully validated ",
            "(see www.rpact.com/experimental)",
            diagnosticId = "validation.experimental_feature"
        )
    }

    return(performanceScore)
}
