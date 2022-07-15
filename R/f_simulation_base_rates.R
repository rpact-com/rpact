## |
## |  *Simulation of binary data with group sequential and combination test*
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
## |  File version: $Revision: 5615 $
## |  Last changed: $Date: 2021-12-06 09:29:15 +0100 (Mo, 06 Dez 2021) $
## |  Last changed by: $Author: wassmer $
## |

.getTestStatisticsRates <- function(..., designNumber, informationRates, groups, normalApproximation,
        riskRatio, thetaH0, directionUpper, eventsPerStage, sampleSizesPerStage,
        testStatisticsPerStage) {
    stage <- ncol(sampleSizesPerStage)

    if (groups == 1) {
        stagewiseRates <- eventsPerStage[, stage] / sampleSizesPerStage[, stage]
        overallRate <- sum(eventsPerStage[, 1:stage]) / sum(sampleSizesPerStage[, 1:stage])
    } else {
        stagewiseRates <- eventsPerStage[, stage] / sampleSizesPerStage[, stage]
        if (stage == 1) {
            overallRate <- eventsPerStage[, 1] / sampleSizesPerStage[, 1]
        } else {
            overallRate <- rowSums(eventsPerStage[, 1:stage]) / rowSums(sampleSizesPerStage[, 1:stage])
        }
    }

    if (designNumber == 1L) {
        n1 <- sum(sampleSizesPerStage[1, ])
        e1 <- sum(eventsPerStage[1, ])
        r1 <- e1 / n1
        if (groups == 1) {
            if (!normalApproximation) {
                if (directionUpper) {
                    value <- .getOneMinusQNorm(stats::pbinom(e1 - 1, n1, thetaH0, lower.tail = FALSE))
                } else {
                    value <- .getOneMinusQNorm(stats::pbinom(e1, n1, thetaH0, lower.tail = TRUE))
                }
            } else {
                value <- (r1 - thetaH0) / sqrt(thetaH0 * (1 - thetaH0)) * sqrt(n1)
            }
        } else {
            n2 <- sum(sampleSizesPerStage[2, ])
            e2 <- sum(eventsPerStage[2, ])
            r2 <- e2 / n2
            if (!normalApproximation) {
                if (directionUpper) {
                    value <- .getOneMinusQNorm(stats::phyper(e1 - 1, e1 + e2, n1 + n2 - e1 - e2, n1, lower.tail = FALSE))
                } else {
                    value <- .getOneMinusQNorm(stats::phyper(e1, e1 + e2, n1 + n2 - e1 - e2, n1, lower.tail = TRUE))
                }
            } else {
                if (!riskRatio) {
                    if (r1 - r2 - thetaH0 == 0) {
                        value <- 0
                    } else {
                        fm <- .getFarringtonManningValuesDiff(rate1 = r1, rate2 = r2, 
                            theta = thetaH0, allocation = n1 / n2)
                        value <- (r1 - r2 - thetaH0) /
                            sqrt(fm[1] * (1 - fm[1]) / n1 + fm[2] * (1 - fm[2]) / n2)
                    }
                } else {
                    if (r1 - r2 * thetaH0 == 0) {
                        value <- 0
                    } else {
                        fm <- .getFarringtonManningValuesRatio(rate1 = r1, rate2 = r2, 
                            theta = thetaH0, allocation = n1 / n2)
                        value <- (r1 - r2 * thetaH0) /
                            sqrt(fm[1] * (1 - fm[1]) / n1 + thetaH0^2 * fm[2] * (1 - fm[2]) / n2)
                    }
                }
                value <- (2 * directionUpper - 1) * value
            }
        }

        pValuesSeparate <- NA_real_
        testStatisticsPerStage <- NA_real_
    } else {
        if (stage == 1) {
            n1 <- sampleSizesPerStage[1, 1]
            e1 <- eventsPerStage[1, 1]
            r1 <- e1 / n1
            if (groups == 1) {
                if (!normalApproximation) {
                    if (directionUpper) {
                        testStatisticsPerStage <- .getOneMinusQNorm(
                            stats::pbinom(e1 - 1, n1, thetaH0, lower.tail = FALSE))
                    } else {
                        testStatisticsPerStage <- .getOneMinusQNorm(
                            stats::pbinom(e1, n1, thetaH0, lower.tail = TRUE))
                    }
                } else {
                    testStatisticsPerStage <- (2 * directionUpper - 1) * (r1 - thetaH0) / sqrt(thetaH0 * (1 - thetaH0)) * sqrt(n1)
                }
            } else {
                n2 <- sampleSizesPerStage[2, 1]
                e2 <- eventsPerStage[2, 1]
                r2 <- e2 / n2
                if (!normalApproximation) {
                    if (directionUpper) {
                        testStatisticsPerStage <- .getOneMinusQNorm(stats::phyper(
                            e1 - 1, e1 + e2, n1 + n2 - e1 - e2, n1, lower.tail = FALSE))
                    } else {
                        testStatisticsPerStage <- .getOneMinusQNorm(stats::phyper(
                            e1, e1 + e2, n1 + n2 - e1 - e2, n1, lower.tail = TRUE))
                    }
                } else {
                    if (!riskRatio) {
                        if (r1 - r2 - thetaH0 == 0) {
                            testStatisticsPerStage <- 0
                        } else {
                            fm <- .getFarringtonManningValuesDiff(
                                rate1 = r1, rate2 = r2, theta = thetaH0, allocation = n1 / n2)
                            testStatisticsPerStage <- (2 * directionUpper - 1) *
                                (r1 - r2 - thetaH0) / sqrt(fm[1] * (1 - fm[1]) / n1 + fm[2] * (1 - fm[2]) / n2)
                        }
                    } else {
                        if (r1 - r2 * thetaH0 == 0) {
                            testStatisticsPerStage <- 0
                        } else {
                            fm <- .getFarringtonManningValuesRatio(rate1 = r1, 
                                rate2 = r2, theta = thetaH0, allocation = n1 / n2)
                            testStatisticsPerStage <- (2 * directionUpper - 1) *
                                (r1 - r2 * thetaH0) / sqrt(fm[1] * 
                                (1 - fm[1]) / n1 + thetaH0^2 * fm[2] * (1 - fm[2]) / n2)
                        }
                    }
                }
            }
        } else {
            n1 <- sampleSizesPerStage[1, stage]
            e1 <- eventsPerStage[1, stage]
            r1 <- e1 / n1
            if (groups == 1) {
                if (!normalApproximation) {
                    if (directionUpper) {
                        testStatisticsPerStage <- c(
                            testStatisticsPerStage,
                            .getOneMinusQNorm(stats::pbinom(e1 - 1, n1, thetaH0, lower.tail = FALSE))
                        )
                    } else {
                        testStatisticsPerStage <- c(
                            testStatisticsPerStage,
                            .getOneMinusQNorm(stats::pbinom(e1, n1, thetaH0, lower.tail = TRUE))
                        )
                    }
                } else {
                    testStatisticsPerStage <- c(
                        testStatisticsPerStage,
                        (2 * directionUpper - 1) * (r1 - thetaH0) / sqrt(thetaH0 * (1 - thetaH0)) * sqrt(n1)
                    )
                }
            } else {
                n2 <- sampleSizesPerStage[2, stage]
                e2 <- eventsPerStage[2, stage]
                r2 <- e2 / n2
                if (!normalApproximation) {
                    if (directionUpper) {
                        testStatisticsPerStage <- c(
                            testStatisticsPerStage,
                            .getOneMinusQNorm(stats::phyper(e1 - 1, e1 + e2, n1 + n2 - e1 - e2, n1, lower.tail = FALSE))
                        )
                    } else {
                        testStatisticsPerStage <- c(
                            testStatisticsPerStage,
                            .getOneMinusQNorm(stats::phyper(e1, e1 + e2, n1 + n2 - e1 - e2, n1, lower.tail = TRUE))
                        )
                    }
                } else {
                    if (!riskRatio) {
                        if (r1 - r2 - thetaH0 == 0) {
                            testStatisticsPerStage <- c(testStatisticsPerStage, 0)
                        } else {
                            fm <- .getFarringtonManningValuesDiff(rate1 = r1, rate2 = r2, theta = thetaH0, allocation = n1 / n2)
                            testStatisticsPerStage <- c(testStatisticsPerStage, (2 * directionUpper - 1) *
                                (r1 - r2 - thetaH0) / sqrt(fm[1] * (1 - fm[1]) / n1 + fm[2] * (1 - fm[2]) / n2))
                        }
                    } else {
                        if (r1 - r2 * thetaH0 == 0) {
                            testStatisticsPerStage <- c(testStatisticsPerStage, 0)
                        } else {
                            fm <- .getFarringtonManningValuesRatio(rate1 = r1, rate2 = r2, theta = thetaH0, allocation = n1 / n2)
                            testStatisticsPerStage <- c(testStatisticsPerStage, (2 * directionUpper - 1) *
                                (r1 - r2 * thetaH0) / sqrt(fm[1] * (1 - fm[1]) / n1 + thetaH0^2 * fm[2] * (1 - fm[2]) / n2))
                        }
                    }
                }
            }
        }

        if (designNumber == 2L) {
            if (stage == 1) {
                value <- testStatisticsPerStage
            } else {
                value <- (sqrt(informationRates[1]) * testStatisticsPerStage[1] +
                    sqrt(informationRates[2:stage] - informationRates[1:(stage - 1)]) %*%
                    testStatisticsPerStage[2:stage]) / sqrt(informationRates[stage])
            }
        } else if (designNumber == 3L) {
            if (stage == 1) {
                value <- 1 - pnorm(testStatisticsPerStage)
            } else {
                weightsFisher <- rep(NA_real_, stage)
                weightsFisher[1] <- 1
                if (stage > 1) {
                    weightsFisher[2:stage] <- sqrt(informationRates[2:stage] -
                        informationRates[1:(stage - 1)]) / sqrt(informationRates[1])
                }
                value <- prod((1 - pnorm(testStatisticsPerStage[1:stage]))^weightsFisher[1:stage])
            }
        }

        pValuesSeparate <- 1 - pnorm(testStatisticsPerStage)
    }

    return(list(
        value = value,
        stagewiseRates = stagewiseRates,
        overallRate = overallRate,
        sampleSizesPerStage = sampleSizesPerStage,
        testStatisticsPerStage = testStatisticsPerStage,
        pValuesSeparate = pValuesSeparate
    ))
}

.getSimulationRatesStageSubjects <- function(..., stage,
        riskRatio,
        thetaH0,
        groups,
        plannedSubjects,
        directionUpper,
        allocationRatioPlanned,
        minNumberOfSubjectsPerStage,
        maxNumberOfSubjectsPerStage,
        sampleSizesPerStage,
        conditionalPower,
        conditionalCriticalValue,
        overallRate,
        farringtonManningValue1,
        farringtonManningValue2) {
    if (is.na(conditionalPower)) {
        return(plannedSubjects[stage] - plannedSubjects[stage - 1])
    }

    if (groups == 1) {
        stageSubjects <-
            (max(0, conditionalCriticalValue * sqrt(thetaH0 * (1 - thetaH0)) +
                .getQNorm(conditionalPower) * sqrt(overallRate[1] * (1 - overallRate[1]))))^2 /
                (max(1e-12, (2 * directionUpper - 1) * (overallRate[1] - thetaH0)))^2
    } else {
        mult <- 1
        corr <- thetaH0
        if (riskRatio) {
            mult <- thetaH0
            corr <- 0
        }
        stageSubjects <- (1 + 1 / allocationRatioPlanned) * (max(0, conditionalCriticalValue *
            sqrt(farringtonManningValue1 * (1 - farringtonManningValue1) +
                farringtonManningValue2 * (1 - farringtonManningValue2) * allocationRatioPlanned * mult^2) +
            .getQNorm(conditionalPower) * sqrt(overallRate[1] * (1 - overallRate[1]) + overallRate[2] *
                (1 - overallRate[2]) * allocationRatioPlanned * mult^2)))^2 /
            (max(1e-12, (2 * directionUpper - 1) * (overallRate[1] - mult * overallRate[2] - corr)))^2
    }
    stageSubjects <- ceiling(min(max(minNumberOfSubjectsPerStage[stage], stageSubjects), maxNumberOfSubjectsPerStage[stage]))

    return(stageSubjects)
}

.getSimulationStepRates <- function(...,
        k,
        kMax,
        designNumber,
        informationRates,
        futilityBounds,
        alpha0Vec,
        criticalValues,
        riskRatio,
        thetaH0,
        pi1,
        pi2,
        groups,
        normalApproximation,
        plannedSubjects,
        directionUpper,
        allocationRatioPlanned,
        minNumberOfSubjectsPerStage,
        maxNumberOfSubjectsPerStage,
        conditionalPower,
        pi1H1,
        pi2H1,
        sampleSizesPerStage,
        eventsPerStage,
        testStatisticsPerStage,
        testStatistic,
        calcSubjectsFunction) {
    stageSubjects <- plannedSubjects[k]

    # perform event size recalculation for stages 2, ..., kMax
    simulatedConditionalPower <- 0
    if (k > 1) {

        # used effect size is either estimated from test statistic or pre-fixed
        overallRate <- testStatistic$overallRate
        if (!is.na(pi1H1)) {
            overallRate[1] <- pi1H1
        }
        if (groups == 2 && !is.na(pi2H1)) {
            overallRate[2] <- pi2H1
        }

        # conditional critical value to reject the null hypotheses at the next stage of the trial
        if (designNumber == 3L) {
            conditionalCriticalValue <- .getOneMinusQNorm((criticalValues[k] /
                testStatistic$value)^(1 / sqrt((informationRates[k] -
                informationRates[k - 1]) / informationRates[1])))
        } else {
            if (criticalValues[k] >= 6) {
                conditionalCriticalValue <- Inf
            } else {
                conditionalCriticalValue <- (criticalValues[k] *
                    sqrt(informationRates[k]) - testStatistic$value * sqrt(informationRates[k - 1])) /
                    sqrt(informationRates[k] - informationRates[k - 1])
            }
        }

        if (groups == 2) {
            if (!riskRatio) {
                fm <- .getFarringtonManningValuesDiff(
                    rate1 = overallRate[1], rate2 = overallRate[2],
                    theta = thetaH0, allocation = allocationRatioPlanned
                )
            } else {
                fm <- .getFarringtonManningValuesRatio(
                    rate1 = overallRate[1], rate2 = overallRate[2],
                    theta = thetaH0, allocation = allocationRatioPlanned
                )
            }
        }

        stageSubjects <- calcSubjectsFunction(
            stage = k,
            riskRatio = riskRatio,
            thetaH0 = thetaH0,
            groups = groups,
            plannedSubjects = plannedSubjects,
            directionUpper = directionUpper,
            allocationRatioPlanned = allocationRatioPlanned,
            minNumberOfSubjectsPerStage = minNumberOfSubjectsPerStage,
            maxNumberOfSubjectsPerStage = maxNumberOfSubjectsPerStage,
            sampleSizesPerStage = sampleSizesPerStage,
            conditionalPower = conditionalPower,
            overallRate = overallRate,
            conditionalCriticalValue = conditionalCriticalValue,
            farringtonManningValue1 = fm[1],
            farringtonManningValue2 = fm[2]
        )

        # calculate conditional power for selected stageSubjects
        if (groups == 1) {
            if (overallRate[1] * (1 - overallRate[1]) == 0) {
                theta <- 0
            } else {
                theta <- (overallRate[1] - thetaH0) / sqrt(overallRate[1] * (1 - overallRate[1])) +
                    sign(overallRate[1] - thetaH0) * conditionalCriticalValue *
                        (1 - sqrt(thetaH0 * (1 - thetaH0) / (overallRate[1] * (1 - overallRate[1])))) /
                        sqrt(stageSubjects)
            }
        } else {
            if (overallRate[1] * (1 - overallRate[1]) + overallRate[2] * (1 - overallRate[2]) == 0) {
                theta <- 0
            } else {
                if (!riskRatio) {
                    theta <- sqrt(allocationRatioPlanned) / (1 + allocationRatioPlanned) * (
                        (overallRate[1] - overallRate[2] - thetaH0) * sqrt(1 + allocationRatioPlanned) /
                            sqrt(overallRate[1] * (1 - overallRate[1]) + allocationRatioPlanned *
                                overallRate[2] * (1 - overallRate[2]))
                            + sign(overallRate[1] - overallRate[2] - thetaH0) * conditionalCriticalValue *
                                (1 - sqrt(fm[1] * (1 - fm[1]) + allocationRatioPlanned *
                                    fm[2] * (1 - fm[2])) / sqrt(overallRate[1] * (1 - overallRate[1]) +
                                    allocationRatioPlanned * overallRate[2] * (1 - overallRate[2]))) *
                                (1 + allocationRatioPlanned) / sqrt(allocationRatioPlanned * stageSubjects)
                    )
                } else {
                    theta <- sqrt(allocationRatioPlanned) / (1 + allocationRatioPlanned) * (
                        (overallRate[1] - thetaH0 * overallRate[2]) * sqrt(1 + allocationRatioPlanned) /
                            sqrt(overallRate[1] * (1 - overallRate[1]) + allocationRatioPlanned *
                                thetaH0^2 * overallRate[2] * (1 - overallRate[2])) +
                            sign(overallRate[1] - thetaH0 * overallRate[2]) * conditionalCriticalValue *
                                (1 - sqrt(fm[1] * (1 - fm[1]) + allocationRatioPlanned * thetaH0^2 *
                                    fm[2] * (1 - fm[2])) / sqrt(overallRate[1] * (1 - overallRate[1]) +
                                    allocationRatioPlanned * thetaH0^2 * overallRate[2] * (1 - overallRate[2]))) *
                                (1 + allocationRatioPlanned) / sqrt(allocationRatioPlanned * stageSubjects))
                }
            }
        }
        if (!directionUpper) {
            theta <- -theta
        }
        simulatedConditionalPower <-
            1 - stats::pnorm(conditionalCriticalValue - theta * sqrt(stageSubjects))
    }

    #   Simulate events with achieved sample size
    if (groups == 1) {
        n1 <- stageSubjects
        eventsPerStage <- cbind(eventsPerStage, matrix(c(stats::rbinom(1, n1, pi1)), nrow = 1))
        sampleSizesPerStage <- cbind(sampleSizesPerStage, matrix(n1, nrow = 1))
    } else {
        n1 <- ceiling(allocationRatioPlanned * stageSubjects / (1 + allocationRatioPlanned))
        n2 <- stageSubjects - n1
        eventsPerStage <- cbind(
            eventsPerStage,
            matrix(c(stats::rbinom(1, n1, pi1), stats::rbinom(1, n2, pi2)), nrow = 2)
        )
        sampleSizesPerStage <- cbind(sampleSizesPerStage, matrix(c(n1, n2), nrow = 2))
    }

    testStatistic <- .getTestStatisticsRates(
        designNumber = designNumber,
        informationRates = informationRates,
        groups = groups, normalApproximation = normalApproximation,
        riskRatio = riskRatio, thetaH0 = thetaH0,
        directionUpper = directionUpper, eventsPerStage = eventsPerStage,
        sampleSizesPerStage = sampleSizesPerStage,
        testStatisticsPerStage = testStatisticsPerStage
    )

    testStatisticsPerStage <- c(testStatisticsPerStage, testStatistic$testStatisticsPerStage[k])

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
        # add small number to avoid ties
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

    return(list(
        trialStop = trialStop,
        sampleSizesPerStage = sampleSizesPerStage,
        eventsPerStage = eventsPerStage,
        testStatisticsPerStage = testStatisticsPerStage,
        testStatistic = testStatistic,
        simulatedSubjects = stageSubjects,
        simulatedRejections = simulatedRejections,
        simulatedFutilityStop = simulatedFutilityStop,
        simulatedConditionalPower = simulatedConditionalPower
    ))
}

#' @title
#' Get Simulation Rates
#'
#' @description
#' Returns the simulated power, stopping probabilities, conditional power, and expected sample size for
#' testing rates in a one or two treatment groups testing situation.
#'
#' @inheritParams param_design_with_default
#' @inheritParams param_groups
#' @inheritParams param_normalApproximation
#' @param riskRatio If \code{TRUE}, the design characteristics for
#'        one-sided testing of H0: \code{pi1 / pi2 = thetaH0} are simulated, default is \code{FALSE}.
#' @inheritParams param_thetaH0
#' @inheritParams param_pi1_rates
#' @inheritParams param_pi2_rates
#' @inheritParams param_directionUpper
#' @inheritParams param_allocationRatioPlanned
#' @inheritParams param_plannedSubjects
#' @inheritParams param_minNumberOfSubjectsPerStage
#' @inheritParams param_maxNumberOfSubjectsPerStage
#' @inheritParams param_conditionalPowerSimulation
#' @param pi1H1 If specified, the assumed probability in the active treatment group if two treatment groups
#'        are considered, or the assumed probability for a one treatment group design, for which the conditional
#'        power was calculated.
#' @param pi2H1 If specified, the assumed probability in the reference group if two treatment groups
#'        are considered, for which the conditional power was calculated.
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
#' The definition of \code{pi1H1} and/or \code{pi2H1} makes only sense if \code{kMax} > 1
#' and if \code{conditionalPower}, \code{minNumberOfSubjectsPerStage}, and
#' \code{maxNumberOfSubjectsPerStage} (or \code{calcSubjectsFunction}) are defined.
#'
#' \code{calcSubjectsFunction}\cr
#' This function returns the number of subjects at given conditional power and conditional critical value for specified
#' testing situation. The function might depend on variables
#' \code{stage},
#' \code{riskRatio},
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
#' \code{overallRate},
#' \code{farringtonManningValue1}, and \code{farringtonManningValue2}.
#' The function has to contain the three-dots argument '...' (see examples).
#'
#' @section Simulation Data:
#' The summary statistics "Simulated data" contains the following parameters: median [range]; mean +/-sd\cr
#'
#' \code{$show(showStatistics = FALSE)} or \code{$setShowStatistics(FALSE)} can be used to disable
#' the output of the aggregated simulated data.\cr
#'
#' Example 1: \cr
#' \code{simulationResults <- getSimulationRates(plannedSubjects = 40)} \cr
#' \code{simulationResults$show(showStatistics = FALSE)}\cr
#'
#' Example 2: \cr
#' \code{simulationResults <- getSimulationRates(plannedSubjects = 40)} \cr
#' \code{simulationResults$setShowStatistics(FALSE)}\cr
#' \code{simulationResults}\cr
#'
#' \code{\link{getData}} can be used to get the aggregated simulated data from the
#' object as \code{\link[base]{data.frame}}. The data frame contains the following columns:
#' \enumerate{
#'   \item \code{iterationNumber}: The number of the simulation iteration.
#'   \item \code{stageNumber}: The stage.
#'   \item \code{pi1}: The assumed or derived event rate in the treatment group (if available).
#'   \item \code{pi2}: The assumed or derived event rate in the control group (if available).
#'   \item \code{numberOfSubjects}: The number of subjects under consideration when the
#'         (interim) analysis takes place.
#'   \item \code{rejectPerStage}: 1 if null hypothesis can be rejected, 0 otherwise.
#'   \item \code{futilityPerStage}: 1 if study should be stopped for futility, 0 otherwise.
#'   \item \code{testStatistic}: The test statistic that is used for the test decision,
#'         depends on which design was chosen (group sequential, inverse normal,
#'         or Fisher combination test)'
#'   \item \code{testStatisticsPerStage}: The test statistic for each stage if only data from
#'         the considered stage is taken into account.
#'   \item \code{overallRate1}: The cumulative rate in treatment group 1.
#'   \item \code{overallRate2}: The cumulative rate in treatment group 2.
#'   \item \code{stagewiseRates1}: The stage-wise rate in treatment group 1.
#'   \item \code{stagewiseRates2}: The stage-wise rate in treatment group 2.
#'   \item \code{sampleSizesPerStage1}: The stage-wise sample size in treatment group 1.
#'   \item \code{sampleSizesPerStage2}: The stage-wise sample size in treatment group 2.
#'   \item \code{trialStop}: \code{TRUE} if study should be stopped for efficacy or futility or final stage, \code{FALSE} otherwise.
#'   \item \code{conditionalPowerAchieved}: The conditional power for the subsequent stage of the trial for
#'         selected sample size and effect. The effect is either estimated from the data or can be
#'         user defined with \code{pi1H1} and \code{pi2H1}.
#' }
#'
#' @template return_object_simulation_results
#' @template how_to_get_help_for_generics
#'
#' @template examples_get_simulation_rates
#'
#' @export
#'
getSimulationRates <- function(design = NULL, ...,
        groups = 2L,
        normalApproximation = TRUE,
        riskRatio = FALSE,
        thetaH0 = ifelse(riskRatio, 1, 0),
        pi1 = seq(0.2, 0.5, 0.1), # C_PI_1_DEFAULT
        pi2 = NA_real_,
        plannedSubjects = NA_real_,
        directionUpper = TRUE, # C_DIRECTION_UPPER_DEFAULT
        allocationRatioPlanned = NA_real_,
        minNumberOfSubjectsPerStage = NA_real_,
        maxNumberOfSubjectsPerStage = NA_real_,
        conditionalPower = NA_real_,
        pi1H1 = NA_real_,
        pi2H1 = NA_real_,
        maxNumberOfIterations = 1000L, # C_MAX_SIMULATION_ITERATIONS_DEFAULT
        seed = NA_real_,
        calcSubjectsFunction = NULL,
        showStatistics = FALSE) {
    if (is.null(design)) {
        design <- .getDefaultDesign(..., type = "simulation")
        .warnInCaseOfUnknownArguments(
            functionName = "getSimulationRates",
            ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design, powerCalculationEnabled = TRUE), "showStatistics"), ...
        )
    } else {
        .assertIsTrialDesign(design)
        .warnInCaseOfUnknownArguments(functionName = "getSimulationRates", ignore = "showStatistics", ...)
        .warnInCaseOfTwoSidedPowerArgument(...)
    }
    .assertIsSingleLogical(directionUpper, "directionUpper")
    .assertIsSingleNumber(thetaH0, "thetaH0")
    .assertIsValidGroupsParameter(groups)
    .assertIsSingleLogical(normalApproximation, "normalApproximation")
    .assertIsSingleLogical(riskRatio, "riskRatio")
    if (groups == 1L) {
        .assertIsInOpenInterval(thetaH0, "thetaH0", 0, 1, naAllowed = FALSE)
    } else {
        if (riskRatio) {
            .assertIsInOpenInterval(thetaH0, "thetaH0", 0, NULL, naAllowed = TRUE)
        } else {
            .assertIsInOpenInterval(thetaH0, "thetaH0", -1, 1, naAllowed = TRUE)
        }
    }
    .assertIsNumericVector(pi1, "pi1", naAllowed = FALSE)
    .assertIsInOpenInterval(pi1, "pi1", 0, 1, naAllowed = FALSE)
    .assertIsNumericVector(pi2, "pi2", naAllowed = TRUE)
    .assertIsInOpenInterval(pi2, "pi2", 0, 1, naAllowed = TRUE)
    .assertIsNumericVector(minNumberOfSubjectsPerStage,
        "minNumberOfSubjectsPerStage",
        naAllowed = TRUE
    )
    .assertIsNumericVector(maxNumberOfSubjectsPerStage,
        "maxNumberOfSubjectsPerStage",
        naAllowed = TRUE
    )
    .assertIsSingleNumber(conditionalPower, "conditionalPower", naAllowed = TRUE)
    .assertIsInOpenInterval(conditionalPower, "conditionalPower", 0, 1, naAllowed = TRUE)
    .assertIsSingleNumber(pi1H1, "pi1H1", naAllowed = TRUE)
    .assertIsInOpenInterval(pi1H1, "pi1H1", 0, 1, naAllowed = TRUE)
    .assertIsSingleNumber(pi2H1, "pi2H1", naAllowed = TRUE)
    .assertIsInOpenInterval(pi2H1, "pi2H1", 0, 1, naAllowed = TRUE)
    .assertIsSingleNumber(allocationRatioPlanned, "allocationRatioPlanned", naAllowed = TRUE)
    .assertIsInOpenInterval(allocationRatioPlanned, "allocationRatioPlanned",
        0, C_ALLOCATION_RATIO_MAXIMUM,
        naAllowed = TRUE
    )
    .assertIsSinglePositiveInteger(maxNumberOfIterations,
        "maxNumberOfIterations",
        validateType = FALSE
    )
    .assertIsSingleNumber(seed, "seed", naAllowed = TRUE)
    .assertIsSingleLogical(showStatistics, "showStatistics", naAllowed = FALSE)

    if (design$sided == 2) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "only one-sided case is implemented for the simulation design"
        )
    }

    if (!normalApproximation && (groups == 2) && (riskRatio || (thetaH0 != 0))) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "in the two-sample case, exact test is implemented only for testing H0: pi1 - pi2 = 0"
        )
    }

    simulationResults <- SimulationResultsRates(design, showStatistics = showStatistics)

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
        endpoint = "rates"
    )
    maxNumberOfSubjectsPerStage <- .assertIsValidNumberOfSubjectsPerStage(maxNumberOfSubjectsPerStage,
        "maxNumberOfSubjectsPerStage", plannedSubjects, conditionalPower, calcSubjectsFunction, design$kMax,
        endpoint = "rates"
    )

    if (design$kMax > 1) {
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
    } else if (!is.null(calcSubjectsFunction) && is.function(calcSubjectsFunction)) {
        simulationResults$calcSubjectsFunction <- calcSubjectsFunction
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

    pi1H1 <- .ignoreParameterIfNotUsed(
        "pi1H1", pi1H1, design$kMax > 1,
        "design is fixed ('kMax' = 1)"
    )
    pi2H1 <- .ignoreParameterIfNotUsed(
        "pi2H1", pi2H1, design$kMax > 1,
        "design is fixed ('kMax' = 1)"
    )
    pi1H1 <- .ignoreParameterIfNotUsed("pi1H1", pi1H1, groups == 2, "'groups' = 1")
    pi2H1 <- .ignoreParameterIfNotUsed("pi2H1", pi2H1, groups == 2, "'groups' = 1")

    if (is.null(calcSubjectsFunction)) {
        calcSubjectsFunction <- .getSimulationRatesStageSubjects
    }
    .assertIsValidFunction(
        fun = calcSubjectsFunction,
        funArgName = "calcSubjectsFunction",
        expectedFunction = .getSimulationRatesStageSubjects
    )

    .setValueAndParameterType(simulationResults, "pi2", pi2, NA_real_)
    .setValueAndParameterType(
        simulationResults, "allocationRatioPlanned",
        allocationRatioPlanned, NA_real_
    )
    if (groups == 1) {
        if (isTRUE(riskRatio)) {
            warning("'riskRatio' (", riskRatio, ") will be ignored ",
                "because it is not applicable for 'groups' = 1",
                call. = FALSE
            )
        }

        if (!is.na(allocationRatioPlanned)) {
            warning("'allocationRatioPlanned' (", allocationRatioPlanned,
                ") will be ignored because it is not applicable for 'groups' = 1",
                call. = FALSE
            )
            simulationResults$allocationRatioPlanned <- NA_real_
        }
        simulationResults$.setParameterType("allocationRatioPlanned", C_PARAM_NOT_APPLICABLE)

        if (!is.na(pi2)) {
            warning("'pi2' (", pi2,
                ") will be ignored because it is not applicable for 'groups' = 1",
                call. = FALSE
            )
            simulationResults$pi2 <- NA_real_
        }
        simulationResults$.setParameterType("pi2", C_PARAM_NOT_APPLICABLE)
    } else {
        if (is.na(allocationRatioPlanned)) {
            allocationRatioPlanned <- C_ALLOCATION_RATIO_DEFAULT
            simulationResults$allocationRatioPlanned <- allocationRatioPlanned
            simulationResults$.setParameterType("allocationRatioPlanned", C_PARAM_DEFAULT_VALUE)
        }
        if (is.na(pi2)) {
            pi2 <- C_PI_2_DEFAULT
            simulationResults$pi2 <- pi2
            simulationResults$.setParameterType("pi2", C_PARAM_DEFAULT_VALUE)
        }
    }

    if (groups == 1) {
        effect <- pi1 - thetaH0
    } else {
        if (riskRatio) {
            effect <- pi1 / pi2 - thetaH0
        } else {
            effect <- pi1 - pi2 - thetaH0
        }
    }
    simulationResults$effect <- effect
    simulationResults$.setParameterType(
        "effect",
        ifelse(groups == 1 && thetaH0 == 0, C_PARAM_NOT_APPLICABLE, C_PARAM_GENERATED)
    )

    .assertIsIntegerVector(plannedSubjects, "plannedSubjects", validateType = FALSE)
    if (length(plannedSubjects) != design$kMax) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'plannedSubjects' (", .arrayToString(plannedSubjects), ") must have length ", design$kMax
        )
    }
    .assertIsInClosedInterval(plannedSubjects, "plannedSubjects", lower = 1, upper = NULL)
    .assertValuesAreStrictlyIncreasing(plannedSubjects, "plannedSubjects")

    .setValueAndParameterType(simulationResults, "normalApproximation", normalApproximation, TRUE)
    .setValueAndParameterType(simulationResults, "riskRatio", riskRatio, FALSE)
    .setValueAndParameterType(simulationResults, "thetaH0", thetaH0, ifelse(riskRatio, 1, 0))
    .setValueAndParameterType(simulationResults, "pi1", pi1, C_PI_1_DEFAULT)
    .setValueAndParameterType(simulationResults, "groups", as.integer(groups), 2L)
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
    .setValueAndParameterType(simulationResults, "pi1H1",
        pi1H1, NA_real_,
        notApplicableIfNA = TRUE
    )
    .setValueAndParameterType(simulationResults, "pi2H1", pi2H1, 0.2, notApplicableIfNA = TRUE)
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

    informationRates <- design$informationRates
    criticalValues <- design$criticalValues
    kMax <- design$kMax
    cols <- length(pi1)
    sampleSizes <- matrix(0, nrow = kMax, ncol = cols)
    rejectPerStage <- matrix(0, nrow = kMax, ncol = cols)
    overallReject <- rep(0, cols)
    futilityPerStage <- matrix(0, kMax - 1, cols)
    futilityStop <- rep(0, cols)
    iterations <- matrix(0, nrow = kMax, ncol = cols)
    expectedNumberOfSubjects <- rep(0, cols)
    conditionalPowerAchieved <- matrix(NA_real_, nrow = kMax, ncol = cols)

    len <- length(pi1) * maxNumberOfIterations * kMax
    dataIterationNumber <- rep(NA_real_, len)
    dataStageNumber <- rep(NA_real_, len)
    dataPi1 <- rep(NA_real_, len)
    dataPi2 <- rep(pi2, len)
    dataNumberOfSubjects <- rep(NA_real_, len)
    dataNumberOfCumulatedSubjects <- rep(NA_real_, len)
    dataRejectPerStage <- rep(NA_real_, len)
    dataFutilityPerStage <- rep(NA_real_, len)
    dataTestStatistic <- rep(NA_real_, len)
    dataTestStatisticsPerStage <- rep(NA_real_, len)
    dataOverallRate1 <- rep(NA_real_, len)
    dataOverallRate2 <- rep(NA_real_, len)
    dataStagewiseRates1 <- rep(NA_real_, len)
    dataStagewiseRates2 <- rep(NA_real_, len)
    dataSampleSizesPerStage1 <- rep(NA_real_, len)
    dataSampleSizesPerStage2 <- rep(NA_real_, len)
    dataTrialStop <- rep(NA, len)
    dataConditionalPowerAchieved <- rep(NA_real_, len)
    if (designNumber != 1L) {
        dataPValuesSeparate <- rep(NA_real_, len)
    }

    index <- 1
    for (i in 1:length(pi1)) {
        simulatedSubjects <- rep(0, kMax)
        simulatedOverallSubjects <- rep(0, kMax)
        simulatedRejections <- rep(0, kMax)
        simulatedFutilityStop <- rep(0, kMax - 1)
        simulatedOverallSubjects <- 0
        simulatedConditionalPower <- rep(0, kMax)

        for (j in 1:maxNumberOfIterations) {
            trialStop <- FALSE
            sampleSizesPerStage <- matrix(rep(numeric(0), 2), nrow = groups)
            eventsPerStage <- matrix(rep(numeric(0), 2), nrow = groups)
            testStatisticsPerStage <- c()
            testStatistic <- NULL

            for (k in 1:kMax) {
                if (!trialStop) {
                    stepResult <- .getSimulationStepRates(
                        k = k,
                        kMax = kMax,
                        designNumber = designNumber,
                        informationRates = informationRates,
                        futilityBounds = futilityBounds,
                        alpha0Vec = alpha0Vec,
                        criticalValues = criticalValues,
                        riskRatio = riskRatio,
                        thetaH0 = thetaH0,
                        pi1 = pi1[i],
                        pi2 = pi2,
                        groups = groups,
                        normalApproximation = normalApproximation,
                        plannedSubjects = plannedSubjects,
                        directionUpper = directionUpper,
                        allocationRatioPlanned = allocationRatioPlanned,
                        minNumberOfSubjectsPerStage = minNumberOfSubjectsPerStage,
                        maxNumberOfSubjectsPerStage = maxNumberOfSubjectsPerStage,
                        conditionalPower = conditionalPower,
                        pi1H1 = pi1H1,
                        pi2H1 = pi2H1,
                        sampleSizesPerStage = sampleSizesPerStage,
                        eventsPerStage = eventsPerStage,
                        testStatisticsPerStage = testStatisticsPerStage,
                        testStatistic = testStatistic,
                        calcSubjectsFunction = calcSubjectsFunction
                    )

                    trialStop <- stepResult$trialStop
                    sampleSizesPerStage <- stepResult$sampleSizesPerStage
                    eventsPerStage <- stepResult$eventsPerStage
                    testStatisticsPerStage <- stepResult$testStatisticsPerStage
                    testStatistic <- stepResult$testStatistic

                    simulatedSubjectsStep <- stepResult$simulatedSubjects
                    simulatedRejectionsStep <- stepResult$simulatedRejections
                    simulatedFutilityStopStep <- stepResult$simulatedFutilityStop
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
                    simulatedConditionalPower[k] <- simulatedConditionalPower[k] +
                        simulatedConditionalPowerStep

                    dataIterationNumber[index] <- j
                    dataStageNumber[index] <- k
                    dataPi1[index] <- pi1[i]
                    dataNumberOfSubjects[index] <- simulatedSubjectsStep
                    dataNumberOfCumulatedSubjects[index] <- sum(sampleSizesPerStage[, ])
                    dataRejectPerStage[index] <- simulatedRejectionsStep
                    dataFutilityPerStage[index] <- simulatedFutilityStopStep
                    dataTestStatistic[index] <- testStatistic$value
                    dataTestStatisticsPerStage[index] <- testStatisticsPerStage[k]
                    dataOverallRate1[index] <- testStatistic$overallRate[1]
                    dataStagewiseRates1[index] <- testStatistic$stagewiseRates[1]
                    dataSampleSizesPerStage1[index] <- testStatistic$sampleSizesPerStage[1, k]
                    if (length(testStatistic$stagewiseRates) > 1) {
                        dataOverallRate2[index] <- testStatistic$overallRate[2]
                        dataStagewiseRates2[index] <- testStatistic$stagewiseRates[2]
                        dataSampleSizesPerStage2[index] <- testStatistic$sampleSizesPerStage[2, k]
                    } else {
                        dataStagewiseRates2[index] <- NA_real_
                        dataOverallRate2[index] <- NA_real_
                        dataSampleSizesPerStage2[index] <- NA_real_
                    }
                    dataTrialStop[index] <- trialStop
                    dataConditionalPowerAchieved[index] <- simulatedConditionalPowerStep
                    if (designNumber != 1L) {
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

    sampleSizes[is.na(sampleSizes)] <- 0

    simulationResults$iterations <- iterations
    simulationResults$sampleSizes <- sampleSizes
    simulationResults$rejectPerStage <- rejectPerStage
    simulationResults$overallReject <- overallReject
    simulationResults$futilityPerStage <- futilityPerStage
    simulationResults$futilityStop <- futilityStop
    if (kMax > 1) {
        if (length(pi1) == 1) {
            simulationResults$earlyStop <- sum(futilityPerStage) + sum(rejectPerStage[1:(kMax - 1)])
        } else {
            if (kMax > 2) {
                rejectPerStageColSum <- colSums(rejectPerStage[1:(kMax - 1), ])
            } else {
                rejectPerStageColSum <- rejectPerStage[1, ]
            }
            simulationResults$earlyStop <- colSums(futilityPerStage) + rejectPerStageColSum
        }
    } else {
        simulationResults$earlyStop <- rep(0, length(pi1))
    }
    simulationResults$expectedNumberOfSubjects <- expectedNumberOfSubjects
    simulationResults$conditionalPowerAchieved <- conditionalPowerAchieved

    if (!all(is.na(simulationResults$conditionalPowerAchieved))) {
        simulationResults$.setParameterType("conditionalPowerAchieved", C_PARAM_GENERATED)
    }

    data <- data.frame(
        iterationNumber = dataIterationNumber,
        stageNumber = dataStageNumber,
        pi1 = dataPi1,
        pi2 = dataPi2,
        numberOfSubjects = dataNumberOfSubjects,
        numberOfCumulatedSubjects = dataNumberOfCumulatedSubjects,
        rejectPerStage = dataRejectPerStage,
        futilityPerStage = dataFutilityPerStage,
        testStatistic = dataTestStatistic,
        testStatisticsPerStage = dataTestStatisticsPerStage,
        overallRate1 = dataOverallRate1,
        overallRate2 = dataOverallRate2,
        stagewiseRates1 = dataStagewiseRates1,
        stagewiseRates2 = dataStagewiseRates2,
        sampleSizesPerStage1 = dataSampleSizesPerStage1,
        sampleSizesPerStage2 = dataSampleSizesPerStage2,
        trialStop = dataTrialStop,
        conditionalPowerAchieved = round(dataConditionalPowerAchieved, 6)
    )
    if (designNumber == 3L) {
        data$pValue <- dataPValuesSeparate
    }
    data <- data[!is.na(data$pi1), ]

    simulationResults$.data <- data

    return(simulationResults)
}
