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
## |  File version: $Revision: 6585 $
## |  Last changed: $Date: 2022-09-23 14:23:08 +0200 (Fr, 23 Sep 2022) $
## |  Last changed by: $Author: pahlke $
## |

#'
#' @title 
#' Get Performance Score
#' 
#' @description
#' Calculates the performance score for a given simulation result.
#' 
#' @param simulationResult A simulation result.
#'
#' @author Stephen Schüürhuis
#' 
#' @keywords internal
#'
#' @export 
#' 
getPerformanceScore <- function(simulationResult) {
    .assertIsSimulationResults(simulationResult)

    design <- simulationResult$.design

    if (!inherits(simulationResult, "SimulationResultsMeans")) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
            "performance score so far only implemented for continuous endpoints")
    }
    if (!design$bindingFutility) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
            "performance score so far only implemented for binding futility boundaries")
    }
    if (design$kMax != 2) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
            "performance score so far only implemented for two-stage designs")
    }

    alternative <- simulationResult$alternative

    # initialize necessary sample size values
    plannedSubjects <- simulationResult$plannedSubjects
    maxAdditionalNumberOfSubjects <- ifelse(is.na(simulationResult$conditionalPower),
        plannedSubjects[2] - plannedSubjects[1],
        simulationResult$maxNumberOfSubjectsPerStage[2]
    )

    iterations <- simulationResult$maxNumberOfIterations

    targetConditionalPower <- ifelse(is.na(simulationResult$conditionalPower),
        1 - design$beta,
        simulationResult$conditionalPower
    )

    fixedDesign <- getDesignGroupSequential(
        kMax = 1,
        alpha = design$alpha,
        beta = design$beta
    )
    simulatedData <- simulationResult$.data
    resultMatrix <- sapply(1:length(alternative), FUN = function(k) {
        data <- simulatedData[which(simulatedData$alternative == alternative[k]), ]

        # compute sample size necessary for an analogously planned single stage design
        fixedSampleSizeResult <- getSampleSizeMeans(
            design = fixedDesign,
            normalApproximation = simulationResult$normalApproximation,
            groups = simulationResult$groups,
            thetaH0 = 0,
            alternative = alternative[k],
            stDev = simulationResult$stDev
        )
        fixedSampleSize <- as.numeric(fixedSampleSizeResult$numberOfSubjects)

        # iterations in which the trial has proceed to stage two
        secondStageIterations <- data[which(data$stageNumber == 2), ]

        # mean and variance estimates for sample size and conditional power
        meanSampleSize <- mean(secondStageIterations$numberOfCumulatedSubjects)
        varSampleSize <- var(secondStageIterations$numberOfCumulatedSubjects)

        meanConditionalPower <- mean(secondStageIterations$conditionalPowerAchieved)
        varConditionalPower <- var(secondStageIterations$conditionalPowerAchieved)

        # target sample size: single stage sample size if it doesn't exceed maximum 
        # admissible sample size, otherwise only first stage sample size
        targetSampleSize <- ifelse(fixedSampleSize <= maxAdditionalNumberOfSubjects + plannedSubjects[1],
            fixedSampleSize, plannedSubjects[1]
        )

        # sample size components
        locationSampleSize <- 1 - abs(meanSampleSize - targetSampleSize) / (maxAdditionalNumberOfSubjects)
        maxVariationSampleSize <- (maxAdditionalNumberOfSubjects / 2)^2 * iterations / (iterations - 1)
        variationSampleSize <- 1 - sqrt(varSampleSize / maxVariationSampleSize)
        subscoreSampleSize <- mean(c(locationSampleSize, variationSampleSize))

        # conditional power components
        locationConditionalPower <- 1 - abs(meanConditionalPower - targetConditionalPower) / (1 - design$alpha)
        maxVariationConditionalPower <- (1 / 2)^2 * iterations / (iterations - 1)
        variationConditionalPower <- 1 - sqrt(varConditionalPower / maxVariationConditionalPower)
        subscoreConditionalPower <- mean(c(locationConditionalPower, variationConditionalPower))

        # performance score calculation
        performanceScore <- mean(c(subscoreSampleSize, subscoreConditionalPower))

        return(c(
            alternative = alternative[k],
            locationSampleSize = locationSampleSize,
            variationSampleSize = variationSampleSize,
            subscoreSampleSize = subscoreSampleSize,
            locationConditionalPower = locationConditionalPower,
            variationConditionalPower = variationConditionalPower,
            subscoreConditionalPower = subscoreConditionalPower,
            performanceScore = performanceScore
        ))
    })

    resultList <- list()
    for (k in 1:nrow(resultMatrix)) {
        resultList[[rownames(resultMatrix)[k]]] <- resultMatrix[k, ]
    }

    warning("The performance score function is experimental and hence not fully validated ",
        "(see www.rpact.com/experimental)", call. = FALSE)
    
    return(resultList)
}


