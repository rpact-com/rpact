## |
## |  *Performance score functions*
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
## |  File version: $Revision: 7644 $
## |  Last changed: $Date: 2024-02-16 10:36:28 +0100 (Fr, 16 Feb 2024) $
## |  Last changed by: $Author: pahlke $
## |

#'
#' @title
#' Get Performance Score
#'
#' @description
#' Calculates the conditional performance score, its sub-scores and components according to
#' Herrmann et al. (2020) for a given simulation result from a two-stage design.
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
#' can be found in Herrmann et al. (2020).
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

    if (!inherits(simulationResult, "SimulationResultsMeans")) { 
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "performance score so far implemented only for single comparisons with continuous endpoints"
        )
    }

    if (design$kMax != 2) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "performance score so far implemented only for two-stage designs"
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
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "performance score is not available for class ",
            class(simulationResult)[1]
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

    performanceScore <- PerformanceScore(simulationResult)
    performanceScore$.alternative <- alternativeValues
    paramNames <- rownames(resultMatrix)
    for (k in 1:nrow(resultMatrix)) {
        paramName <- paramNames[k]
        performanceScore[[paramName]] <- resultMatrix[k, ]
        performanceScore$.setParameterType(paramName, C_PARAM_GENERATED)
    }

    warning("The performance score function is experimental and hence not fully validated ",
        "(see www.rpact.com/experimental)",
        call. = FALSE
    )

    return(performanceScore)
}
