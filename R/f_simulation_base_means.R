## |
## |  *Simulation of continuous data with group sequential and combination test*
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

.getTestStatisticsMeans <- function(..., designNumber, informationRates, groups, normalApproximation,
        meanRatio, thetaH0, allocationRatioPlanned, sampleSizesPerStage, testStatisticsPerStage) {
    stage <- length(sampleSizesPerStage)

    # This is an estimate of the overall test statistic disregarding the fact that the variance can be estimated
    # from the overall data (might have influence on the Type I error rate and power when choosing the conventional
    # group sequential design with unknown variance):
    overallTestStatistic <- sqrt(sampleSizesPerStage) %*% testStatisticsPerStage /
        sqrt(sum(sampleSizesPerStage))

    if (normalApproximation) {
        pValuesSeparate <- 1 - stats::pnorm(testStatisticsPerStage)
    } else {
        pValuesSeparate <- 1 - stats::pt(testStatisticsPerStage, sampleSizesPerStage - groups)
    }

    if (designNumber == 1L) {
        if (normalApproximation) {
            value <- overallTestStatistic
        } else {
            value <- .getQNorm(stats::pt(overallTestStatistic, sum(sampleSizesPerStage) - groups))
        }
    } else if (designNumber == 2L) {
        if (stage == 1) {
            if (normalApproximation) {
                value <- testStatisticsPerStage[1]
            } else {
                value <- .getQNorm(stats::pt(testStatisticsPerStage[1], sampleSizesPerStage[1] - groups))
            }
        } else {
            if (normalApproximation) {
                value <- (sqrt(informationRates[1]) * testStatisticsPerStage[1] +
                    sqrt(informationRates[2:stage] - informationRates[1:(stage - 1)]) %*%
                    testStatisticsPerStage[2:stage]) / sqrt(informationRates[stage])
            } else {
                value <- (sqrt(informationRates[1]) *
                    .getQNorm(stats::pt(testStatisticsPerStage[1], sampleSizesPerStage[1] - groups)) +
                    sqrt(informationRates[2:stage] - informationRates[1:(stage - 1)]) %*%
                    .getQNorm(stats::pt(
                        testStatisticsPerStage[2:stage],
                        sampleSizesPerStage[2:stage] - groups
                    ))) / sqrt(informationRates[stage])
            }
        }
    } else if (designNumber == 3L) {
        weightsFisher <- rep(NA_real_, stage)
        weightsFisher[1] <- 1
        if (stage > 1) {
            weightsFisher[2:stage] <- sqrt(informationRates[2:stage] -
                informationRates[1:(stage - 1)]) / sqrt(informationRates[1])
        }
        if (normalApproximation) {
            value <- prod((1 - stats::pnorm(testStatisticsPerStage[1:stage]))^weightsFisher[1:stage])
        } else {
            value <- prod((1 - stats::pt(
                testStatisticsPerStage[1:stage],
                sampleSizesPerStage[1:stage] - groups
            ))^weightsFisher[1:stage])
        }
    }

    if (groups == 1) {
        standardizedEffectEstimate <- overallTestStatistic / sqrt(sum(sampleSizesPerStage))
    } else {
        if (!meanRatio) {
            standardizedEffectEstimate <- overallTestStatistic /
                sqrt(allocationRatioPlanned * sum(sampleSizesPerStage)) *
                (1 + allocationRatioPlanned)
        } else {
            standardizedEffectEstimate <- overallTestStatistic /
                sqrt(allocationRatioPlanned * sum(sampleSizesPerStage)) *
                sqrt((1 + allocationRatioPlanned) *
                    (1 + thetaH0^2 * allocationRatioPlanned))
        }
    }

    return(list(
        value = value,
        overallTestStatistic = overallTestStatistic,
        standardizedEffectEstimate = standardizedEffectEstimate,
        pValuesSeparate = pValuesSeparate
    ))
}

.getSimulationMeansStageSubjects <- function(..., stage,
        meanRatio, thetaH0, groups, plannedSubjects,
        allocationRatioPlanned,
        minNumberOfSubjectsPerStage,
        maxNumberOfSubjectsPerStage,
        sampleSizesPerStage,
        thetaH1,
        stDevH1,
        conditionalPower,
        conditionalCriticalValue) {
    if (is.na(conditionalPower)) {
        return(plannedSubjects[stage] - plannedSubjects[stage - 1])
    }

    thetaStandardized <- thetaH1 / stDevH1

    mult <- 1
    if (groups == 2) {
        thetaH0 <- ifelse(meanRatio, thetaH0, 1)
        mult <- 1 + 1 / allocationRatioPlanned + thetaH0^2 * (1 + allocationRatioPlanned)
    }

    stageSubjects <- (max(0, conditionalCriticalValue + .getQNorm(conditionalPower)))^2 * mult /
        (max(1e-12, thetaStandardized))^2

    stageSubjects <- min(
        max(minNumberOfSubjectsPerStage[stage], stageSubjects),
        maxNumberOfSubjectsPerStage[stage]
    )

    return(stageSubjects)
}

.getSimulationStepMeans <- function(...,
        k,
        kMax,
        designNumber,
        informationRates,
        futilityBounds,
        alpha0Vec,
        criticalValues,
        meanRatio,
        thetaH0,
        alternative,
        stDev,
        groups,
        normalApproximation,
        plannedSubjects,
        directionUpper,
        allocationRatioPlanned,
        minNumberOfSubjectsPerStage,
        maxNumberOfSubjectsPerStage,
        conditionalPower,
        thetaH1,
        stDevH1,
        effectEstimate,
        sampleSizesPerStage,
        testStatisticsPerStage,
        testStatistic,
        calcSubjectsFunction) {
    stageSubjects <- plannedSubjects[1]

    # perform sample size size recalculation for stages 2, ..., kMax
    simulatedConditionalPower <- 0
    if (k > 1) {

        # used effect size is either estimated from test statistic or pre-fixed
        if (is.na(thetaH1)) {
            thetaH1 <- effectEstimate
        } else {
            thetaH1 <- thetaH1 - thetaH0
        }
        thetaStandardized <- thetaH1 / stDevH1

        if (!directionUpper) {
            thetaH1 <- -thetaH1
            thetaStandardized <- -thetaStandardized
        }

        # conditional critical value to reject the null hypotheses at the next stage of the trial
        if (designNumber == 3L) {
            conditionalCriticalValue <- .getOneMinusQNorm((criticalValues[k] /
                testStatistic$value)^(1 / sqrt((informationRates[k] -
                informationRates[k - 1]) / informationRates[1])))
        } else {
            conditionalCriticalValue <- (criticalValues[k] *
                sqrt(informationRates[k]) - testStatistic$value * sqrt(informationRates[k - 1])) /
                sqrt(informationRates[k] - informationRates[k - 1])
        }

        stageSubjects <- calcSubjectsFunction(
            stage = k,
            meanRatio = meanRatio,
            thetaH0 = thetaH0,
            groups = groups,
            plannedSubjects = plannedSubjects,
            sampleSizesPerStage = sampleSizesPerStage,
            allocationRatioPlanned = allocationRatioPlanned,
            minNumberOfSubjectsPerStage = minNumberOfSubjectsPerStage,
            maxNumberOfSubjectsPerStage = maxNumberOfSubjectsPerStage,
            conditionalPower = conditionalPower,
            thetaH1 = thetaH1,
            stDevH1 = stDevH1,
            conditionalCriticalValue = conditionalCriticalValue
        )

        if (is.null(stageSubjects) || length(stageSubjects) != 1 || !is.numeric(stageSubjects) || is.na(stageSubjects)) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'calcSubjectsFunction' returned an illegal or undefined result (", stageSubjects, "); ",
                "the output must be a single numeric value"
            )
        }

        # calculate conditional power for computed stageSubjects
        if (groups == 1) {
            thetaStandardized <- thetaStandardized
        } else {
            if (!meanRatio) {
                thetaStandardized <- thetaStandardized * sqrt(allocationRatioPlanned) / (1 + allocationRatioPlanned)
            } else {
                thetaStandardized <- thetaStandardized * sqrt(allocationRatioPlanned) /
                    sqrt((1 + allocationRatioPlanned) * (1 + thetaH0 * allocationRatioPlanned))
            }
        }
        simulatedConditionalPower <-
            1 - stats::pnorm(conditionalCriticalValue - thetaStandardized * sqrt(stageSubjects))
    }

    if (groups == 1) {
        nz <- (alternative - thetaH0) / stDev * sqrt(stageSubjects)
        if (normalApproximation) {
            testResult <- (2 * directionUpper - 1) * stats::rnorm(1, nz, 1)
        } else {
            testResult <- (2 * directionUpper - 1) * stats::rt(1, stageSubjects - 1, nz)
        }
    } else {
        if (!meanRatio) {
            nz <- (alternative - thetaH0) / stDev *
                sqrt(allocationRatioPlanned * stageSubjects) / (1 + allocationRatioPlanned)
        } else {
            nz <- (alternative - thetaH0) / stDev *
                sqrt(allocationRatioPlanned * stageSubjects) /
                sqrt((1 + allocationRatioPlanned) * (1 + thetaH0^2 * allocationRatioPlanned))
        }
        if (normalApproximation) {
            testResult <- (2 * directionUpper - 1) * stats::rnorm(1, nz, 1)
        } else {
            testResult <- (2 * directionUpper - 1) * stats::rt(1, stageSubjects - 2, nz)
        }
    }
    
    sampleSizesPerStage <- c(sampleSizesPerStage, stageSubjects)
    testStatisticsPerStage <- c(testStatisticsPerStage, testResult)

    testStatistic <- .getTestStatisticsMeans(
        designNumber = designNumber,
        informationRates = informationRates,
        groups = groups, normalApproximation = normalApproximation,
        meanRatio = meanRatio, thetaH0 = thetaH0,
        allocationRatioPlanned = allocationRatioPlanned,
        sampleSizesPerStage = sampleSizesPerStage,
        testStatisticsPerStage = testStatisticsPerStage
    )

    effectEstimate <- testStatistic$standardizedEffectEstimate * stDev

    simulatedRejections <- 0
    simulatedFutilityStop <- 0
    trialStop <- FALSE
    if (k == kMax) {
        trialStop <- TRUE
    }
    if (designNumber <= 2) {
        if (!is.na(testStatistic$value) && !is.na(criticalValues[k]) &&
                testStatistic$value >= criticalValues[k]) {
            simulatedRejections <- 1
            trialStop <- TRUE
        }
        if (!is.na(testStatistic$value) && !is.na(futilityBounds[k]) &&
                k < kMax && testStatistic$value <= futilityBounds[k]) {
            simulatedFutilityStop <- 1
            trialStop <- TRUE
        }
    } else {
        if (!is.na(testStatistic$value) && !is.na(criticalValues[k]) &&
                testStatistic$value <= criticalValues[k]) {
            simulatedRejections <- 1
            trialStop <- TRUE
        }
        if (!is.na(testStatistic$pValuesSeparate[k]) && !is.na(alpha0Vec[k]) &&
                k < kMax && testStatistic$pValuesSeparate[k] >= alpha0Vec[k]) {
            simulatedFutilityStop <- 1
            trialStop <- TRUE
        }
    }

    if (!directionUpper) {
        effectEstimate <- -effectEstimate
    }

    return(list(
        trialStop = trialStop,
        sampleSizesPerStage = sampleSizesPerStage,
        testStatisticsPerStage = testStatisticsPerStage,
        testStatistic = testStatistic,
        effectEstimate = effectEstimate,
        simulatedSubjects = stageSubjects,
        simulatedRejections = simulatedRejections,
        simulatedFutilityStop = simulatedFutilityStop,
        simulatedConditionalPower = simulatedConditionalPower
    ))
}

.getSimulationMeansLoop <- function(alternative,
        kMax,
        maxNumberOfIterations,
        designNumber,
        informationRates,
        futilityBounds,
        alpha0Vec,
        criticalValues,
        meanRatio,
        thetaH0,
        stDev,
        groups,
        normalApproximation,
        plannedSubjects,
        directionUpper,
        allocationRatioPlanned,
        minNumberOfSubjectsPerStage,
        maxNumberOfSubjectsPerStage,
        conditionalPower,
        thetaH1,
        stDevH1,
        calcSubjectsFunction) {
    len <- length(alternative) * maxNumberOfIterations * kMax
    dataIterationNumber <- rep(NA_real_, len)
    dataStageNumber <- rep(NA_real_, len)
    dataAlternative <- rep(NA_real_, len)
    dataEffect <- rep(NA_real_, len)
    dataNumberOfSubjects <- rep(NA_real_, len)
    dataNumberOfCumulatedSubjects <- rep(NA_real_, len)
    dataRejectPerStage <- rep(NA_real_, len)
    dataFutilityPerStage <- rep(NA_real_, len)
    dataTestStatisticsPerStage <- rep(NA_real_, len)
    dataTestStatistic <- rep(NA_real_, len)
    dataTrialStop <- rep(NA, len)
    dataConditionalPowerAchieved <- rep(NA_real_, len)
    dataEffectEstimate <- rep(NA_real_, len)
    dataPValuesSeparate <- rep(NA_real_, len)

    cols <- length(alternative)
    sampleSizes <- matrix(0, nrow = kMax, ncol = cols)
    rejectPerStage <- matrix(0, nrow = kMax, ncol = cols)
    overallReject <- rep(0, cols)
    futilityPerStage <- matrix(0, kMax - 1, cols)
    futilityStop <- rep(0, cols)
    iterations <- matrix(0, nrow = kMax, ncol = cols)
    expectedNumberOfSubjects <- rep(0, cols)
    conditionalPowerAchieved <- matrix(NA_real_, nrow = kMax, ncol = cols)

    index <- 1
    for (i in 1:length(alternative)) {
        simulatedSubjects <- rep(0, kMax)
        simulatedOverallSubjects <- rep(0, kMax)
        simulatedRejections <- rep(0, kMax)
        simulatedFutilityStop <- rep(0, kMax - 1)
        simulatedOverallSubjects <- 0
        simulatedConditionalPower <- rep(0, kMax)

        for (j in 1:maxNumberOfIterations) {
            trialStop <- FALSE
            sampleSizesPerStage <- c()
            testStatisticsPerStage <- c()
            testStatistic <- NULL
            effectEstimate <- NULL

            for (k in 1:kMax) {
                if (!trialStop) {
                    stepResult <- .getSimulationStepMeans(
                        k = k,
                        kMax = kMax,
                        designNumber = designNumber,
                        informationRates = informationRates,
                        futilityBounds = futilityBounds,
                        alpha0Vec = alpha0Vec,
                        criticalValues = criticalValues,
                        meanRatio = meanRatio,
                        thetaH0 = thetaH0,
                        alternative = alternative[i],
                        stDev = stDev,
                        groups = groups,
                        normalApproximation = normalApproximation,
                        plannedSubjects = plannedSubjects,
                        directionUpper = directionUpper,
                        allocationRatioPlanned = allocationRatioPlanned,
                        minNumberOfSubjectsPerStage = minNumberOfSubjectsPerStage,
                        maxNumberOfSubjectsPerStage = maxNumberOfSubjectsPerStage,
                        conditionalPower = conditionalPower,
                        thetaH1 = thetaH1,
                        stDevH1 = stDevH1,
                        effectEstimate = effectEstimate,
                        sampleSizesPerStage = sampleSizesPerStage,
                        testStatisticsPerStage = testStatisticsPerStage,
                        testStatistic = testStatistic,
                        calcSubjectsFunction = calcSubjectsFunction
                    )

                    trialStop <- stepResult$trialStop
                    sampleSizesPerStage <- stepResult$sampleSizesPerStage
                    testStatisticsPerStage <- stepResult$testStatisticsPerStage
                    testStatistic <- stepResult$testStatistic
                    simulatedSubjectsStep <- stepResult$simulatedSubjects
                    simulatedRejectionsStep <- stepResult$simulatedRejections
                    simulatedFutilityStopStep <- stepResult$simulatedFutilityStop
                    effectEstimate <- stepResult$effectEstimate
                    simulatedConditionalPowerStep <- NA_real_
                    if (k > 1) {
                        simulatedConditionalPowerStep <- stepResult$simulatedConditionalPower
                    }
                    iterations[k, i] <- iterations[k, i] + 1
                    simulatedSubjects[k] <- simulatedSubjects[k] + simulatedSubjectsStep
                    simulatedRejections[k] <- simulatedRejections[k] + simulatedRejectionsStep
                    if (k < kMax) {
                        simulatedFutilityStop[k] <- simulatedFutilityStop[k] + simulatedFutilityStopStep
                    }
                    simulatedConditionalPower[k] <-
                        simulatedConditionalPower[k] + simulatedConditionalPowerStep

                    dataIterationNumber[index] <- j
                    dataStageNumber[index] <- k
                    dataAlternative[index] <- alternative[i]
                    dataNumberOfSubjects[index] <- simulatedSubjectsStep
                    dataNumberOfCumulatedSubjects[index] <- sum(sampleSizesPerStage)
                    dataRejectPerStage[index] <- simulatedRejectionsStep
                    dataFutilityPerStage[index] <- simulatedFutilityStopStep
                    dataTestStatistic[index] <- testStatistic$value
                    dataTestStatisticsPerStage[index] <- testStatisticsPerStage[k]
                    dataTrialStop[index] <- trialStop
                    dataConditionalPowerAchieved[index] <- simulatedConditionalPowerStep
                    dataEffectEstimate[index] <- effectEstimate
                    if (designNumber == 3L) {
                        dataPValuesSeparate[index] <- testStatistic$pValuesSeparate[k]
                    }
                    index <- index + 1
                }
            }
        }

        simulatedOverallSubjects <- sum(simulatedSubjects[1:k])
        sampleSizes[, i] <- simulatedSubjects / iterations[, i]
        rejectPerStage[, i] <- simulatedRejections / maxNumberOfIterations
        overallReject[i] <- sum(simulatedRejections / maxNumberOfIterations)
        futilityPerStage[, i] <- simulatedFutilityStop / maxNumberOfIterations
        futilityStop[i] <- sum(simulatedFutilityStop / maxNumberOfIterations)
        expectedNumberOfSubjects[i] <- simulatedOverallSubjects / maxNumberOfIterations
        if (kMax > 1) {
            conditionalPowerAchieved[2:kMax, i] <-
                simulatedConditionalPower[2:kMax] / iterations[2:kMax, i]
        }
    }
    
    data <- data.frame(
        iterationNumber = dataIterationNumber,
        stageNumber = dataStageNumber,
        alternative = dataAlternative,
        numberOfSubjects = dataNumberOfSubjects,
        numberOfCumulatedSubjects = dataNumberOfCumulatedSubjects,
        rejectPerStage = dataRejectPerStage,
        futilityPerStage = dataFutilityPerStage,
        testStatistic = dataTestStatistic,
        testStatisticsPerStage = dataTestStatisticsPerStage,
        effectEstimate = dataEffectEstimate,
        trialStop = dataTrialStop,
        conditionalPowerAchieved = round(dataConditionalPowerAchieved, 6)
    )
    if (designNumber == 3L) {
        data$pValue <- dataPValuesSeparate
    }
    data <- data[!is.na(data$alternative), ]

    return(list(
        sampleSizes = sampleSizes,
        iterations = iterations,
        rejectPerStage = rejectPerStage,
        overallReject = overallReject,
        futilityPerStage = futilityPerStage,
        futilityStop = futilityStop,
        expectedNumberOfSubjects = expectedNumberOfSubjects,
        conditionalPowerAchieved = conditionalPowerAchieved,
        data = data
    ))
}

#' @title
#' Get Simulation Means
#'
#' @description
#' Returns the simulated power, stopping probabilities, conditional power, and expected sample size
#' for testing means in a one or two treatment groups testing situation.
#'
#' @inheritParams param_design_with_default
#' @inheritParams param_groups
#' @param normalApproximation The type of computation of the p-values. Default is \code{TRUE},
#'        i.e., normally distributed test statistics are generated.
#'        If \code{FALSE}, the t test is used for calculating the p-values,
#'        i.e., t distributed test statistics are generated.
#' @param meanRatio If \code{TRUE}, the design characteristics for
#'        one-sided testing of H0: \code{mu1 / mu2 = thetaH0} are simulated, default is \code{FALSE}.
#' @inheritParams param_thetaH0
#' @inheritParams param_alternative_simulation
#' @inheritParams param_stDevSimulation
#' @inheritParams param_directionUpper
#' @inheritParams param_allocationRatioPlanned
#' @inheritParams param_plannedSubjects
#' @inheritParams param_minNumberOfSubjectsPerStage
#' @inheritParams param_maxNumberOfSubjectsPerStage
#' @inheritParams param_conditionalPowerSimulation
#' @inheritParams param_thetaH1
#' @inheritParams param_stDevH1
#' @inheritParams param_maxNumberOfIterations
#' @inheritParams param_calcSubjectsFunction
#' @inheritParams param_seed
#' @inheritParams param_three_dots
#' @inheritParams param_showStatistics
#'
#' @details
#' At given design the function simulates the power, stopping probabilities, conditional power, and expected
#' sample size at given number of subjects and parameter configuration.
#' Additionally, an allocation ratio = n1/n2 can be specified where n1 and n2 are the number
#' of subjects in the two treatment groups.
#'
#' The definition of \code{thetaH1} makes only sense if \code{kMax} > 1
#' and if \code{conditionalPower}, \code{minNumberOfSubjectsPerStage}, and
#' \code{maxNumberOfSubjectsPerStage} (or \code{calcSubjectsFunction}) are defined.
#'
#' \code{calcSubjectsFunction}\cr
#' This function returns the number of subjects at given conditional power and conditional critical value for specified
#' testing situation. The function might depend on variables
#' \code{stage},
#' \code{meanRatio},
#' \code{thetaH0},
#' \code{groups},
#' \code{plannedSubjects},
#' \code{sampleSizesPerStage},
#' \code{directionUpper},
#' \code{allocationRatioPlanned},
#' \code{minNumberOfSubjectsPerStage},
#' \code{maxNumberOfSubjectsPerStage},
#' \code{conditionalPower},
#' \code{conditionalCriticalValue},
#' \code{thetaH1}, and
#' \code{stDevH1}.
#' The function has to contain the three-dots argument '...' (see examples).
#'
#' @section Simulation Data:
#' The summary statistics "Simulated data" contains the following parameters: median [range]; mean +/-sd\cr
#'
#' \code{$show(showStatistics = FALSE)} or \code{$setShowStatistics(FALSE)} can be used to disable
#' the output of the aggregated simulated data.\cr
#'
#' Example 1: \cr
#' \code{simulationResults <- getSimulationMeans(plannedSubjects = 40)} \cr
#' \code{simulationResults$show(showStatistics = FALSE)}\cr
#'
#' Example 2: \cr
#' \code{simulationResults <- getSimulationMeans(plannedSubjects = 40)} \cr
#' \code{simulationResults$setShowStatistics(FALSE)}\cr
#' \code{simulationResults}\cr
#'
#' \code{\link[=getData]{getData()}} can be used to get the aggregated simulated data from the
#' object as \code{\link[base]{data.frame}}. The data frame contains the following columns:
#' \enumerate{
#'   \item \code{iterationNumber}: The number of the simulation iteration.
#'   \item \code{stageNumber}: The stage.
#'   \item \code{alternative}: The alternative hypothesis value.
#'   \item \code{numberOfSubjects}: The number of subjects under consideration when the
#'         (interim) analysis takes place.
#'   \item \code{rejectPerStage}: 1 if null hypothesis can be rejected, 0 otherwise.
#'   \item \code{futilityPerStage}: 1 if study should be stopped for futility, 0 otherwise.
#'   \item \code{testStatistic}: The test statistic that is used for the test decision,
#'         depends on which design was chosen (group sequential, inverse normal, or Fisher's combination test).
#'   \item \code{testStatisticsPerStage}: The test statistic for each stage if only data from
#'         the considered stage is taken into account.
#'   \item \code{effectEstimate}: Overall simulated standardized effect estimate.
#'   \item \code{trialStop}: \code{TRUE} if study should be stopped for efficacy or futility or final stage, \code{FALSE} otherwise.
#'   \item \code{conditionalPowerAchieved}: The conditional power for the subsequent stage of the trial for
#'         selected sample size and effect. The effect is either estimated from the data or can be
#'         user defined with \code{thetaH1}.
#' }
#'
#' @template return_object_simulation_results
#' @template how_to_get_help_for_generics
#'
#' @template examples_get_simulation_means
#'
#' @export
#'
getSimulationMeans <- function(design = NULL, ...,
        groups = 2L,
        normalApproximation = TRUE,
        meanRatio = FALSE,
        thetaH0 = ifelse(meanRatio, 1, 0),
        alternative = seq(0, 1, 0.2), # C_ALTERNATIVE_POWER_SIMULATION_DEFAULT
        stDev = 1, # C_STDEV_DEFAULT
        plannedSubjects = NA_real_,
        directionUpper = TRUE, # C_DIRECTION_UPPER_DEFAULT
        allocationRatioPlanned = NA_real_,
        minNumberOfSubjectsPerStage = NA_real_,
        maxNumberOfSubjectsPerStage = NA_real_,
        conditionalPower = NA_real_,
        thetaH1 = NA_real_,
        stDevH1 = NA_real_,
        maxNumberOfIterations = 1000L, # C_MAX_SIMULATION_ITERATIONS_DEFAULT
        seed = NA_real_,
        calcSubjectsFunction = NULL,
        showStatistics = FALSE) {
    if (is.null(design)) {
        design <- .getDefaultDesign(..., type = "simulation")
        .warnInCaseOfUnknownArguments(
            functionName = "getSimulationMeans",
            ignore = c(
                .getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design, powerCalculationEnabled = TRUE),
                "showStatistics", "cppEnabled"
            ), ...
        )
    } else {
        .assertIsTrialDesign(design)
        .warnInCaseOfUnknownArguments(functionName = "getSimulationMeans", ignore = c("showStatistics", "cppEnabled"), ...)
        .warnInCaseOfTwoSidedPowerArgument(...)
    }
    .assertIsSingleLogical(directionUpper, "directionUpper")
    .assertIsSingleNumber(thetaH0, "thetaH0")
    if (meanRatio) {
        .assertIsInOpenInterval(thetaH0, "thetaH0", 0, NULL, naAllowed = TRUE)
        .assertIsInOpenInterval(thetaH1, "thetaH1", 0, NULL, naAllowed = TRUE)
        if (identical(alternative, C_ALTERNATIVE_POWER_SIMULATION_DEFAULT)) {
            alternative <- C_ALTERNATIVE_POWER_SIMULATION_MEAN_RATIO_DEFAULT
        }
        .assertIsInOpenInterval(alternative, "alternative", 0, NULL, naAllowed = TRUE)
    }
    .assertIsValidGroupsParameter(groups)
    .assertIsNumericVector(alternative, "alternative", naAllowed = FALSE)
    .assertIsNumericVector(minNumberOfSubjectsPerStage, "minNumberOfSubjectsPerStage", naAllowed = TRUE)
    .assertIsNumericVector(maxNumberOfSubjectsPerStage, "maxNumberOfSubjectsPerStage", naAllowed = TRUE)
    .assertIsSingleNumber(conditionalPower, "conditionalPower", naAllowed = TRUE)
    .assertIsInOpenInterval(conditionalPower, "conditionalPower", 0, 1, naAllowed = TRUE)
    .assertIsSingleNumber(thetaH1, "thetaH1", naAllowed = TRUE)
    .assertIsSingleNumber(stDevH1, "stDevH1", naAllowed = TRUE)
    .assertIsInOpenInterval(stDevH1, "stDevH1", 0, NULL, naAllowed = TRUE)
    .assertIsSingleNumber(allocationRatioPlanned, "allocationRatioPlanned", naAllowed = TRUE)
    .assertIsInOpenInterval(allocationRatioPlanned, "allocationRatioPlanned", 0, C_ALLOCATION_RATIO_MAXIMUM, naAllowed = TRUE)
    .assertIsSinglePositiveInteger(maxNumberOfIterations, "maxNumberOfIterations", validateType = FALSE)
    .assertIsSingleNumber(seed, "seed", naAllowed = TRUE)
    .assertIsValidStandardDeviation(stDev)
    .assertIsSingleLogical(showStatistics, "showStatistics", naAllowed = FALSE)
    .assertIsSingleLogical(normalApproximation, "normalApproximation", naAllowed = FALSE)

    if (design$sided == 2) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "only one-sided case is implemented for the simulation design"
        )
    }

    if (groups == 1L) {
        if (isTRUE(meanRatio)) {
            warning("'meanRatio' (", meanRatio, ") will be ignored ",
                "because it is not applicable for 'groups' = 1",
                call. = FALSE
            )
        }
        if (!is.na(allocationRatioPlanned)) {
            warning("'allocationRatioPlanned' (", allocationRatioPlanned,
                ") will be ignored because it is not applicable for 'groups' = 1",
                call. = FALSE
            )
            allocationRatioPlanned <- NA_real_
        }
    } else if (is.na(allocationRatioPlanned)) {
        allocationRatioPlanned <- C_ALLOCATION_RATIO_DEFAULT
    }

    simulationResults <- SimulationResultsMeans(design, showStatistics = showStatistics)

    thetaH1 <- .ignoreParameterIfNotUsed(
        "thetaH1", thetaH1, design$kMax > 1,
        "design is fixed ('kMax' = 1)", "Assumed effect"
    )
    stDevH1 <- .ignoreParameterIfNotUsed(
        "stDevH1", stDevH1, design$kMax > 1,
        "design is fixed ('kMax' = 1)", "Assumed effect"
    )
    if (is.na(conditionalPower) && is.null(calcSubjectsFunction) && !is.na(stDevH1)) {
        warning("'stDevH1' will be ignored because neither 'conditionalPower' nor ",
            "'calcSubjectsFunction' is defined",
            call. = FALSE
        )
    }
    conditionalPower <- .ignoreParameterIfNotUsed(
        "conditionalPower",
        conditionalPower, design$kMax > 1, "design is fixed ('kMax' = 1)"
    )
    minNumberOfSubjectsPerStage <- .ignoreParameterIfNotUsed(
        "minNumberOfSubjectsPerStage",
        minNumberOfSubjectsPerStage, design$kMax > 1, "design is fixed ('kMax' = 1)"
    )
    maxNumberOfSubjectsPerStage <- .ignoreParameterIfNotUsed(
        "maxNumberOfSubjectsPerStage",
        maxNumberOfSubjectsPerStage, design$kMax > 1, "design is fixed ('kMax' = 1)"
    )
    minNumberOfSubjectsPerStage <- .assertIsValidNumberOfSubjectsPerStage(minNumberOfSubjectsPerStage,
        "minNumberOfSubjectsPerStage", plannedSubjects, conditionalPower, calcSubjectsFunction, design$kMax,
        endpoint = "means"
    )
    maxNumberOfSubjectsPerStage <- .assertIsValidNumberOfSubjectsPerStage(maxNumberOfSubjectsPerStage,
        "maxNumberOfSubjectsPerStage", plannedSubjects, conditionalPower, calcSubjectsFunction, design$kMax,
        endpoint = "means"
    )

    if (design$kMax > 1) {
        if (!normalApproximation) {
            if (!all(is.na(minNumberOfSubjectsPerStage)) && (any(minNumberOfSubjectsPerStage < groups * 2))) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "minNumberOfSubjectsPerStage not correctly specified"
                )
            }
        }
        if (any(maxNumberOfSubjectsPerStage - minNumberOfSubjectsPerStage < 0) &&
                !all(is.na(maxNumberOfSubjectsPerStage - minNumberOfSubjectsPerStage))) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'maxNumberOfSubjectsPerStage' (",
                .arrayToString(maxNumberOfSubjectsPerStage),
                ") must be not smaller than minNumberOfSubjectsPerStage' (",
                .arrayToString(minNumberOfSubjectsPerStage), ")"
            )
        }
        .setValueAndParameterType(
            simulationResults, "minNumberOfSubjectsPerStage",
            minNumberOfSubjectsPerStage, NA_real_
        )
        .setValueAndParameterType(
            simulationResults, "maxNumberOfSubjectsPerStage",
            maxNumberOfSubjectsPerStage, NA_real_
        )
    }
    if (!is.na(conditionalPower) && (design$kMax == 1)) {
        warning("'conditionalPower' will be ignored for fixed sample design", call. = FALSE)
    }
    if (!is.null(calcSubjectsFunction) && (design$kMax == 1)) {
        warning("'calcSubjectsFunction' will be ignored for fixed sample design", call. = FALSE)
    }

    if (is.na(conditionalPower) && is.null(calcSubjectsFunction)) {
        if (length(minNumberOfSubjectsPerStage) != 1 ||
                !is.na(minNumberOfSubjectsPerStage)) {
            warning("'minNumberOfSubjectsPerStage' (",
                .arrayToString(minNumberOfSubjectsPerStage), ") ",
                "will be ignored because neither 'conditionalPower' nor ",
                "'calcSubjectsFunction' is defined",
                call. = FALSE
            )
            simulationResults$minNumberOfSubjectsPerStage <- NA_real_
        }
        if (length(maxNumberOfSubjectsPerStage) != 1 ||
                !is.na(maxNumberOfSubjectsPerStage)) {
            warning("'maxNumberOfSubjectsPerStage' (",
                .arrayToString(maxNumberOfSubjectsPerStage), ") ",
                "will be ignored because neither 'conditionalPower' nor ",
                "'calcSubjectsFunction' is defined",
                call. = FALSE
            )
            simulationResults$maxNumberOfSubjectsPerStage <- NA_real_
        }
    }

    simulationResults$.setParameterType(
        "calcSubjectsFunction",
        ifelse(design$kMax == 1, C_PARAM_NOT_APPLICABLE,
            ifelse(!is.null(calcSubjectsFunction) && design$kMax > 1,
                C_PARAM_USER_DEFINED, C_PARAM_DEFAULT_VALUE
            )
        )
    )
    if (is.null(calcSubjectsFunction)) {
        calcSubjectsFunction <- .getSimulationMeansStageSubjects
    }
    .assertIsValidFunction(
        fun = calcSubjectsFunction,
        funArgName = "calcSubjectsFunction",
        expectedFunction = .getSimulationMeansStageSubjects
    )
    simulationResults$calcSubjectsFunction <- calcSubjectsFunction

    .assertIsIntegerVector(plannedSubjects, "plannedSubjects", validateType = FALSE)
    if (length(plannedSubjects) != design$kMax) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'plannedSubjects' (", .arrayToString(plannedSubjects),
            ") must have length ", design$kMax
        )
    }
    .assertIsInClosedInterval(plannedSubjects, "plannedSubjects", lower = 1, upper = NULL)
    .assertValuesAreStrictlyIncreasing(plannedSubjects, "plannedSubjects")

    effect <- alternative - thetaH0
    simulationResults$effect <- effect
    simulationResults$.setParameterType(
        "effect",
        ifelse(thetaH0 == 0, C_PARAM_NOT_APPLICABLE, C_PARAM_GENERATED)
    )

    .setValueAndParameterType(simulationResults, "normalApproximation", normalApproximation, TRUE)
    .setValueAndParameterType(simulationResults, "meanRatio", meanRatio, FALSE)
    .setValueAndParameterType(simulationResults, "thetaH0", thetaH0, ifelse(meanRatio, 1, 0))
    .setValueAndParameterType(
        simulationResults, "alternative",
        alternative, C_ALTERNATIVE_POWER_SIMULATION_DEFAULT
    )
    .setValueAndParameterType(simulationResults, "stDev", stDev, C_STDEV_DEFAULT)
    .setValueAndParameterType(simulationResults, "groups", as.integer(groups), 2L)
    .setValueAndParameterType(
        simulationResults, "allocationRatioPlanned",
        allocationRatioPlanned, C_ALLOCATION_RATIO_DEFAULT
    )
    if (groups == 1L) {
        simulationResults$.setParameterType("allocationRatioPlanned", C_PARAM_NOT_APPLICABLE)
    }
    .setValueAndParameterType(
        simulationResults, "plannedSubjects",
        plannedSubjects, NA_real_
    )
    .setValueAndParameterType(
        simulationResults, "directionUpper",
        directionUpper, C_DIRECTION_UPPER_DEFAULT
    )
    .setValueAndParameterType(simulationResults, "minNumberOfSubjectsPerStage",
        minNumberOfSubjectsPerStage, NA_real_,
        notApplicableIfNA = TRUE
    )
    .setValueAndParameterType(simulationResults, "maxNumberOfSubjectsPerStage",
        maxNumberOfSubjectsPerStage, NA_real_,
        notApplicableIfNA = TRUE
    )
    .setValueAndParameterType(simulationResults, "conditionalPower",
        conditionalPower, NA_real_,
        notApplicableIfNA = TRUE
    )
    .setValueAndParameterType(simulationResults, "thetaH1",
        thetaH1, NA_real_,
        notApplicableIfNA = TRUE
    )
    .setValueAndParameterType(simulationResults, "stDevH1",
        stDevH1, NA_real_,
        notApplicableIfNA = TRUE
    )
    .setValueAndParameterType(
        simulationResults, "maxNumberOfIterations",
        as.integer(maxNumberOfIterations), C_MAX_SIMULATION_ITERATIONS_DEFAULT
    )
    simulationResults$.setParameterType("seed", ifelse(is.na(seed),
        C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
    ))
    simulationResults$seed <- .setSeed(seed)

    if (.isTrialDesignGroupSequential(design)) {
        designNumber <- 1L
    } else if (.isTrialDesignInverseNormal(design)) {
        designNumber <- 2L
    } else if (.isTrialDesignFisher(design)) {
        designNumber <- 3L
    }

    if (.isTrialDesignFisher(design)) {
        alpha0Vec <- design$alpha0Vec
        futilityBounds <- rep(NA_real_, design$kMax - 1)
    } else {
        alpha0Vec <- rep(NA_real_, design$kMax - 1)
        futilityBounds <- design$futilityBounds
    }

    if (is.na(stDevH1)) {
        stDevH1 <- stDev
    }

    cppEnabled <- .getOptionalArgument("cppEnabled", ..., optionalArgumentDefaultValue = TRUE)
    fun <- if (cppEnabled && simulationResults$.getParameterType("calcSubjectsFunction") != C_PARAM_USER_DEFINED) 
        getSimulationMeansLoopCpp else .getSimulationMeansLoop
    cppResult <- fun(
        alternative = alternative,
        kMax = design$kMax,
        maxNumberOfIterations = maxNumberOfIterations,
        designNumber = designNumber,
        informationRates = design$informationRates,
        futilityBounds = futilityBounds,
        alpha0Vec = alpha0Vec,
        criticalValues = design$criticalValues,
        meanRatio = meanRatio,
        thetaH0 = thetaH0,
        stDev = stDev,
        groups = groups,
        normalApproximation = normalApproximation,
        plannedSubjects = plannedSubjects,
        directionUpper = directionUpper,
        allocationRatioPlanned = allocationRatioPlanned,
        minNumberOfSubjectsPerStage = minNumberOfSubjectsPerStage,
        maxNumberOfSubjectsPerStage = maxNumberOfSubjectsPerStage,
        conditionalPower = conditionalPower,
        thetaH1 = thetaH1,
        stDevH1 = stDevH1,
        calcSubjectsFunction = calcSubjectsFunction
    )

    sampleSizes <- cppResult$sampleSizes
    sampleSizes[is.na(sampleSizes)] <- 0

    simulationResults$iterations <- cppResult$iterations
    simulationResults$sampleSizes <- sampleSizes
    simulationResults$rejectPerStage <- cppResult$rejectPerStage
    simulationResults$overallReject <- cppResult$overallReject
    simulationResults$futilityPerStage <- cppResult$futilityPerStage
    simulationResults$futilityStop <- cppResult$futilityStop
    if (design$kMax > 1) {
        if (length(alternative) == 1) {
            simulationResults$earlyStop <- sum(cppResult$futilityPerStage) + sum(cppResult$rejectPerStage[1:(design$kMax - 1)])
        } else {
            if (design$kMax > 2) {
                rejectPerStageColSum <- colSums(cppResult$rejectPerStage[1:(design$kMax - 1), ])
            } else {
                rejectPerStageColSum <- cppResult$rejectPerStage[1, ]
            }
            simulationResults$earlyStop <- colSums(cppResult$futilityPerStage) + rejectPerStageColSum
        }
    } else {
        simulationResults$earlyStop <- rep(0, length(alternative))
    }

    simulationResults$expectedNumberOfSubjects <- cppResult$expectedNumberOfSubjects
    simulationResults$conditionalPowerAchieved <- cppResult$conditionalPowerAchieved

    if (!all(is.na(simulationResults$conditionalPowerAchieved))) {
        simulationResults$.setParameterType("conditionalPowerAchieved", C_PARAM_GENERATED)
    }
    
    data <- cppResult$data
    data <- data[!is.na(data$alternative), ]
    if (designNumber != 3L) {
        data <- data[, colnames(data) != "pValue"]
    }
    data$trialStop <- as.logical(data$trialStop)
    simulationResults$.data <- data

    return(simulationResults)
}
