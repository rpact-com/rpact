## |
## |  *Simulation of enrichment design with time to event data*
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
## |  File version: $Revision: 8349 $
## |  Last changed: $Date: 2024-11-01 14:50:21 +0100 (Fr, 01 Nov 2024) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_simulation_enrichment.R
NULL

.logRankHelper <- function(survivalDataSetSelected, time, thetaH0) {
    res <- .logRankTestCpp(
        accrualTime = survivalDataSetSelected$accrualTime,
        survivalTime = survivalDataSetSelected$survivalTime,
        dropoutTime = survivalDataSetSelected$dropoutTime,
        treatmentGroup = as.integer(survivalDataSetSelected$treatmentArm),
        time = time,
        directionUpper = TRUE,
        thetaH0 = thetaH0,
        returnRawData = FALSE
    )
    res <- res$result
    list(
        numerator = res[5],
        denominator = res[6],
        subjectNumber = res[2],
        events1 = res[3],
        events2 = res[4]
    )
}

#   Calculates the stratified logrank test statistic for the enrichment survival
#   data set and a specified population at given time.
#' @noRd
#'
.logRankTestEnrichment <- function(gMax,
                                   survivalDataSet,
                                   time,
                                   subPopulation,
                                   stratifiedAnalysis,
                                   directionUpper = TRUE,
                                   thetaH0 = 1) {
    subGroups <- .createSubGroupsFromPopulationCpp(gMax, subPopulation)

    if (stratifiedAnalysis) {
        stratifiedNumerator <- 0
        stratifiedDenominator <- 0
        stratifiedEvents1 <- 0
        stratifiedEvents2 <- 0
        stratifiedSubjectNumber <- 0
        for (subGroup in subGroups) {
            survivalDataSetSelected <- survivalDataSet[survivalDataSet$subGroup == subGroup, ]
            logRankHelperResult <- .logRankHelper(survivalDataSetSelected, time, thetaH0)
            stratifiedNumerator <- stratifiedNumerator + logRankHelperResult$numerator
            stratifiedDenominator <- stratifiedDenominator + logRankHelperResult$denominator
            stratifiedEvents1 <- stratifiedEvents1 + logRankHelperResult$events1
            stratifiedEvents2 <- stratifiedEvents2 + logRankHelperResult$events2
            stratifiedSubjectNumber <- stratifiedSubjectNumber + logRankHelperResult$subjectNumber
        }
        if (directionUpper && stratifiedDenominator > 0) {
            logRank <- -stratifiedNumerator / sqrt(stratifiedDenominator)
        } else if (!directionUpper && stratifiedDenominator > 0) {
            logRank <- stratifiedNumerator / sqrt(stratifiedDenominator)
        } else {
            logRank <- -Inf
        }
        events1 <- stratifiedEvents1
        events2 <- stratifiedEvents2
        subjectNumber <- stratifiedSubjectNumber
    } else {
        survivalDataSetSelected <- survivalDataSet[survivalDataSet$subGroup %in% subGroups, ]
        logRankHelperResult <- .logRankHelper(survivalDataSetSelected, time, thetaH0)
        if (directionUpper && logRankHelperResult$denominator > 0) {
            logRank <- -logRankHelperResult$numerator / sqrt(logRankHelperResult$denominator)
        } else if (!directionUpper && logRankHelperResult$denominator > 0) {
            logRank <- logRankHelperResult$numerator / sqrt(logRankHelperResult$denominator)
        } else {
            logRank <- -Inf
        }
        events1 <- logRankHelperResult$events1
        events2 <- logRankHelperResult$events2
        subjectNumber <- logRankHelperResult$subjectNumber
    }

    list(
        logRank = logRank,
        thetaH0 = thetaH0,
        directionUpper = directionUpper,
        subjectNumber = subjectNumber,
        events = c(events1, events2)
    )
}

#'
#' Calculates stage events for specified conditional power
#'
#' @noRd
#'
.getSimulationSurvivalEnrichmentStageEvents <- function(...,
                                                        stage,
                                                        directionUpper,
                                                        conditionalPower,
                                                        conditionalCriticalValue,
                                                        plannedEvents,
                                                        allocationRatioPlanned,
                                                        selectedPopulations,
                                                        thetaH1,
                                                        overallEffects,
                                                        minNumberOfEventsPerStage,
                                                        maxNumberOfEventsPerStage) {
    stage <- stage - 1 # to be consistent with non-enrichment situation
    gMax <- nrow(overallEffects)

    if (!is.na(conditionalPower)) {
        if (any(selectedPopulations[1:gMax, stage + 1], na.rm = TRUE)) {
            if (is.na(thetaH1)) {
                if (is.na(directionUpper) || isTRUE(directionUpper)) {
                    thetaStandardized <- log(max(
                        min(
                            overallEffects[selectedPopulations[1:gMax, stage + 1], stage],
                            na.rm = TRUE
                        ),
                        1 + 1e-07
                    ))
                } else {
                    thetaStandardized <- log(min(
                        max(
                            overallEffects[selectedPopulations[1:gMax, stage + 1], stage],
                            na.rm = TRUE
                        ),
                        1 - 1e-07
                    ))
                }
            } else {
                thetaStandardized <- log(min(
                    thetaH1,
                    1 +
                        ifelse(
                            is.na(directionUpper) ||
                                isTRUE(directionUpper),
                            1e-07,
                            -1e-07
                        )
                ))
            }

            if (conditionalCriticalValue[stage] > 8) {
                newEvents <- maxNumberOfEventsPerStage[stage + 1]
            } else {
                newEvents <- (1 + allocationRatioPlanned[stage])^2 /
                    allocationRatioPlanned[stage] *
                    (max(
                        0,
                        conditionalCriticalValue[stage] +
                            .getQNorm(conditionalPower),
                        na.rm = TRUE
                    ))^2 /
                    thetaStandardized^2
                newEvents <- min(
                    max(minNumberOfEventsPerStage[stage + 1], newEvents),
                    maxNumberOfEventsPerStage[stage + 1]
                )
            }
        } else {
            newEvents <- 0
        }
    } else {
        newEvents <- plannedEvents[stage + 1] - plannedEvents[stage]
    }
    return(newEvents)
}

#'
#' Calculates stage results for each simulation iteration step
#'
#' @noRd
#'
.getSimulatedStageResultsSurvivalEnrichmentSubjectsBased <- function(...,
                                                                     design,
                                                                     subGroups,
                                                                     prevalences,
                                                                     piControls,
                                                                     kappa,
                                                                     phi,
                                                                     eventTime,
                                                                     hazardRatios,
                                                                     directionUpper,
                                                                     stratifiedAnalysis,
                                                                     plannedEvents,
                                                                     recruitmentTimes,
                                                                     allocationFraction,
                                                                     typeOfSelection,
                                                                     effectMeasure,
                                                                     adaptations,
                                                                     epsilonValue,
                                                                     rValue,
                                                                     threshold,
                                                                     minNumberOfEventsPerStage,
                                                                     maxNumberOfEventsPerStage,
                                                                     conditionalPower,
                                                                     thetaH1,
                                                                     calcEventsFunction,
                                                                     calcEventsFunctionIsUserDefined,
                                                                     selectPopulationsFunction) {
    kMax <- length(plannedEvents)
    pMax <- length(hazardRatios)
    gMax <- log(length(hazardRatios), 2) + 1

    maxNumberOfSubjects <- length(recruitmentTimes)

    simLogRanks <- matrix(NA_real_, nrow = pMax, ncol = kMax)
    populationEventsPerStage <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    overallEffects <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    testStatistics <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    logRankStatistics <- matrix(NA_real_, nrow = pMax, ncol = kMax)
    overallTestStatistics <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    separatePValues <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    conditionalCriticalValue <- rep(NA_real_, kMax - 1)
    conditionalPowerPerStage <- rep(NA_real_, kMax)
    selectedPopulations <- matrix(FALSE, nrow = gMax, ncol = kMax)
    selectedPopulations[, 1] <- TRUE
    selectedsubGroupsIndices <- matrix(FALSE, nrow = pMax, ncol = kMax)
    selectedsubGroupsIndices[, 1] <- TRUE
    adjustedPValues <- rep(NA_real_, kMax)
    populationHazardRatios <- rep(NA_real_, gMax)
    analysisTime <- rep(NA_real_, kMax)
    numberOfSubjects <- rep(NA_real_, kMax)
    eventsNotAchieved <- rep(FALSE, kMax)

    if (.isTrialDesignFisher(design)) {
        weights <- .getWeightsFisher(design)
    } else if (.isTrialDesignInverseNormal(design)) {
        weights <- .getWeightsInverseNormal(design)
    }

    ##  Create data set for first stage  ##
    treatments <- c()
    subGroupVector <- c()
    while (length(treatments) < maxNumberOfSubjects) {
        if (allocationFraction[1] > allocationFraction[2]) {
            treatments <- c(
                treatments,
                rep(c(1, 2), allocationFraction[2]),
                rep(1, allocationFraction[1] - allocationFraction[2])
            )
        } else {
            treatments <- c(
                treatments,
                rep(c(1, 2), allocationFraction[1]),
                rep(2, allocationFraction[2] - allocationFraction[1])
            )
        }
        subGroup <- subGroups[which(rmultinom(1, 1, prevalences) == 1)]
        subGroupVector <- c(subGroupVector, rep(subGroup, allocationFraction[1] + allocationFraction[2]))
    }
    treatments <- treatments[1:maxNumberOfSubjects]
    subGroupVector <- subGroupVector[1:maxNumberOfSubjects]

    survivalDataSet <- data.frame(
        accrualTime = recruitmentTimes,
        subGroup = subGroupVector,
        treatmentArm = treatments
    )

    lambdaControl <- getLambdaByPi(piControls, eventTime, kappa)
    lambdaActive <- hazardRatios * lambdaControl

    for (i in 1:maxNumberOfSubjects) {
        if (survivalDataSet$treatmentArm[i] == 1) {
            survivalDataSet$survivalTime[i] <- (-log(1 - runif(1, 0, 1)))^(1 / kappa) /
                lambdaActive[which(survivalDataSet$subGroup[i] == subGroups)]
        } else {
            survivalDataSet$survivalTime[i] <- (-log(1 - runif(1, 0, 1)))^(1 / kappa) /
                lambdaControl[which(survivalDataSet$subGroup[i] == subGroups)]
        }
        if (any(phi > 0)) {
            if (phi[1] > 0) {
                if (survivalDataSet$treatmentArm[i] == 1) {
                    survivalDataSet$dropoutTime[i] <- -log(1 - runif(1, 0, 1)) / phi[1]
                }
            }
            if (phi[2] > 0) {
                if (survivalDataSet$treatmentArm[i] == 2) {
                    survivalDataSet$dropoutTime[i] <- -log(1 - runif(1, 0, 1)) / phi[2]
                }
            }
        } else {
            survivalDataSet$dropoutTime[i] <- NA_real_
        }
    }

    for (k in 1:kMax) {
        if (k == 1) {
            analysisTime[k] <- .findObservationTimeCpp(
                accrualTime = survivalDataSet$accrualTime,
                survivalTime = survivalDataSet$survivalTime,
                dropoutTime = survivalDataSet$dropoutTime,
                requiredStageEvents = plannedEvents[k]
            )
            if (is.na(analysisTime[k])) {
                eventsNotAchieved[k] <- TRUE
                break
            } else {
                numberOfSubjects[k] <- sum(survivalDataSet$accrualTime <= analysisTime[k])

                for (g in 1:gMax) {
                    if (selectedPopulations[g, k]) {
                        logRank <- .logRankTestEnrichment(
                            gMax = gMax,
                            survivalDataSet = survivalDataSet,
                            time = analysisTime[1],
                            subPopulation = g,
                            stratifiedAnalysis = stratifiedAnalysis,
                            directionUpper = directionUpper
                        )
                        logRank2 <- .logRankTestEnrichmentCpp(
                            gMax = gMax,
                            survivalDataSet = survivalDataSet,
                            time = analysisTime[1],
                            subPopulation = g,
                            stratifiedAnalysis = stratifiedAnalysis,
                            directionUpper = directionUpper
                        )
                        stopifnot(isTRUE(all.equal(logRank, logRank2)))
                    }
                    testStatistics[g, k] <- logRank$logRank
                    overallTestStatistics[g, k] <- logRank$logRank
                    populationEventsPerStage[g, k] <- sum(logRank$events)
                }
            }
        } else {
            selectedsubGroupsIndices[, k] <- .createSelectedSubsets(selectedPopulations[, k])

            if (analysisTime[k - 1] < max(survivalDataSet$accrualTime)) {
                #  create new survival and dropout times for selected populations
                if (!all(selectedPopulations[, k] & selectedPopulations[, k - 1])) {
                    prevSelected <- prevalences / sum(prevalences[selectedsubGroupsIndices[, k]])
                    prevSelected[!selectedsubGroupsIndices[, k]] <- 0

                    subGroupVector <- subGroupVector[1:numberOfSubjects[k - 1]]

                    while (length(subGroupVector) < maxNumberOfSubjects) {
                        subGroup <- subGroups[which(rmultinom(1, 1, prevSelected) == 1)]
                        subGroupVector <- c(
                            subGroupVector,
                            rep(subGroup, allocationFraction[1] + allocationFraction[2])
                        )
                    }
                    survivalDataSet$subGroup <- subGroupVector[1:maxNumberOfSubjects]

                    for (i in numberOfSubjects[k - 1]:maxNumberOfSubjects) {
                        if (survivalDataSet$treatmentArm[i] == 1) {
                            survivalDataSet$survivalTime[i] <- (-log(1 - runif(1, 0, 1)))^(1 / kappa) /
                                lambdaActive[which(survivalDataSet$subGroup[i] == subGroups)]
                        } else {
                            survivalDataSet$survivalTime[i] <- (-log(1 - runif(1, 0, 1)))^(1 / kappa) /
                                lambdaControl[which(survivalDataSet$subGroup[i] == subGroups)]
                        }
                        if (any(phi > 0)) {
                            if (phi[1] > 0) {
                                if (survivalDataSet$treatmentArm[i] == 1) {
                                    survivalDataSet$dropoutTime[i] <- -log(1 - runif(1, 0, 1)) / phi[1]
                                }
                            }
                            if (phi[2] > 0) {
                                if (survivalDataSet$treatmentArm[i] == 2) {
                                    survivalDataSet$dropoutTime[i] <- -log(1 - runif(1, 0, 1)) / phi[2]
                                }
                            }
                        } else {
                            survivalDataSet$dropoutTime[i] <- NA_real_
                        }
                    }
                }
            }

            selectedsubGroups <- .createSubGroupsCpp(gMax)[selectedsubGroupsIndices[, k]]

            survivalDatasetSelected <- survivalDataSet[survivalDataSet$subGroup %in% selectedsubGroups, ]

            analysisTime[k] <- .findObservationTimeCpp(
                accrualTime = survivalDatasetSelected$accrualTime,
                survivalTime = survivalDatasetSelected$survivalTime,
                dropoutTime = survivalDatasetSelected$dropoutTime,
                requiredStageEvents = plannedEvents[k]
            )
            if (is.na(analysisTime[k])) {
                eventsNotAchieved[k] <- TRUE
                break
            } else {
                numberOfSubjects[k] <- sum(survivalDataSet$accrualTime <= analysisTime[k])
                for (g in 1:gMax) {
                    if (selectedPopulations[g, k]) {
                        logRank <- .logRankTestEnrichment(
                            gMax = gMax,
                            survivalDataSet = survivalDatasetSelected,
                            time = analysisTime[k],
                            subPopulation = g,
                            stratifiedAnalysis = stratifiedAnalysis,
                            directionUpper = directionUpper
                        )
                        logRank2 <- .logRankTestEnrichmentCpp(
                            gMax = gMax,
                            survivalDataSet = survivalDataSet,
                            time = analysisTime[k],
                            subPopulation = g,
                            stratifiedAnalysis = stratifiedAnalysis,
                            directionUpper = directionUpper
                        )
                        stopifnot(isTRUE(all.equal(logRank, logRank2)))

                        overallTestStatistics[g, k] <- logRank$logRank
                        populationEventsPerStage[g, k] <- sum(logRank$events)
                        if (populationEventsPerStage[g, k] - populationEventsPerStage[g, k - 1] > 0) {
                            testStatistics[g, k] <- (sqrt(populationEventsPerStage[g, k]) *
                                overallTestStatistics[g, k] -
                                sqrt(populationEventsPerStage[g, k - 1]) *
                                    overallTestStatistics[g, k - 1]) /
                                sqrt(populationEventsPerStage[g, k] - populationEventsPerStage[g, k - 1])
                        }
                    }
                }
            }
            testStatistics[!selectedPopulations[, k], k] <- NA_real_
            overallEffects[!selectedPopulations[, k], k] <- NA_real_
            overallTestStatistics[!selectedPopulations[, k], k] <- NA_real_
        }

        separatePValues[, k] <- 1 - stats::pnorm(testStatistics[, k])

        overallEffects[, k] <- exp(
            (2 * directionUpper - 1) *
                overallTestStatistics[, k] *
                (1 + allocationFraction[1] / allocationFraction[2]) /
                sqrt(allocationFraction[1] / allocationFraction[2]) /
                sqrt(populationEventsPerStage[, k])
        )

        allocationRatioPlanned <- allocationFraction[1] / allocationFraction[2]

        if (k < kMax) {
            if (colSums(selectedPopulations)[k] == 0) {
                break
            }

            # Bonferroni adjustment
            adjustedPValues[k] <- min(
                min(separatePValues[, k], na.rm = TRUE) * (colSums(selectedPopulations)[k]),
                1 - 1e-07
            )

            # conditional critical value to reject the null hypotheses at the next stage of the trial
            criticalValues <- .getCriticalValues(design)
            if (.isTrialDesignFisher(design)) {
                conditionalCriticalValue[k] <- .getOneMinusQNorm(min(
                    (criticalValues[k + 1] /
                        prod(adjustedPValues[1:k]^weights[1:k]))^(1 / weights[k + 1]),
                    1 - 1e-07
                ))
            } else {
                conditionalCriticalValue[k] <- (criticalValues[k + 1] *
                    sqrt(design$informationRates[k + 1]) -
                    .getOneMinusQNorm(adjustedPValues[1:k]) %*% weights[1:k]) /
                    sqrt(design$informationRates[k + 1] - design$informationRates[k])
            }

            if (adaptations[k]) {
                selectPopulationsFunctionArgs <- list(
                    effectVector = NULL,
                    stage = k,
                    directionUpper = directionUpper,
                    conditionalPower = conditionalPower,
                    conditionalCriticalValue = conditionalCriticalValue,
                    plannedEvents = plannedEvents,
                    allocationRatioPlanned = allocationRatioPlanned,
                    selectedPopulations = selectedPopulations,
                    thetaH1 = thetaH1,
                    overallEffects = overallEffects
                )

                args <- list(
                    typeOfSelection = typeOfSelection,
                    epsilonValue = epsilonValue,
                    rValue = rValue,
                    threshold = threshold,
                    selectPopulationsFunction = selectPopulationsFunction
                )

                if (effectMeasure == "testStatistic") {
                    selectPopulationsFunctionArgs$effectVector <- overallTestStatistics[, k]
                } else if (effectMeasure == "effectEstimate") {
                    if (is.na(directionUpper) || isTRUE(directionUpper)) {
                        selectPopulationsFunctionArgs$effectVector <- overallEffects[, k]
                    } else {
                        selectPopulationsFunctionArgs$effectVector <- 1 / overallEffects[, k]
                        args$threshold <- 1 / threshold
                    }
                }

                args$selectPopulationsFunctionArgs <- selectPopulationsFunctionArgs

                selectedPopulations[, k + 1] <- (selectedPopulations[, k] & do.call(.selectPopulations, args))

                newEvents <- calcEventsFunction(
                    stage = k + 1, # to be consistent with non-enrichment situation, cf. line 38
                    directionUpper = directionUpper,
                    conditionalPower = conditionalPower,
                    conditionalCriticalValue = conditionalCriticalValue,
                    plannedEvents = plannedEvents,
                    allocationRatioPlanned = allocationRatioPlanned,
                    selectedPopulations = selectedPopulations,
                    thetaH1 = thetaH1,
                    overallEffects = overallEffects,
                    minNumberOfEventsPerStage = minNumberOfEventsPerStage,
                    maxNumberOfEventsPerStage = maxNumberOfEventsPerStage
                )

                if (is.null(newEvents) || length(newEvents) != 1 || !is.numeric(newEvents) || is.na(newEvents)) {
                    stop(
                        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                        "'calcEventsFunction' returned an illegal or undefined result (",
                        newEvents,
                        "); ",
                        "the output must be a single numeric value"
                    )
                }

                if (!is.na(conditionalPower) || calcEventsFunctionIsUserDefined) {
                    plannedEvents[(k + 1):kMax] <- plannedEvents[k] + cumsum(rep(newEvents, kMax - k))
                }
            } else {
                selectedPopulations[, k + 1] <- selectedPopulations[, k]
            }

            if (is.na(thetaH1)) {
                thetaStandardized <- log(.applyDirectionOfAlternative(
                    overallEffects[selectedPopulations[1:gMax, k], k],
                    directionUpper,
                    type = "minMax",
                    phase = "planning"
                ))
            } else {
                thetaStandardized <- log(thetaH1)
            }
            thetaStandardized <- (2 * directionUpper - 1) * thetaStandardized

            conditionalPowerPerStage[k] <- 1 -
                stats::pnorm(
                    conditionalCriticalValue[k] -
                        thetaStandardized *
                            sqrt(plannedEvents[k + 1] - plannedEvents[k]) *
                            sqrt(allocationFraction[1] / allocationFraction[2]) /
                            (1 + allocationFraction[1] / allocationFraction[2])
                )
        }
    }

    return(list(
        eventsNotAchieved = eventsNotAchieved,
        populationEventsPerStage = populationEventsPerStage,
        plannedEvents = plannedEvents,
        analysisTime = analysisTime,
        numberOfSubjects = numberOfSubjects,
        testStatistics = testStatistics,
        overallEffects = overallEffects,
        overallTestStatistics = overallTestStatistics,
        separatePValues = separatePValues,
        conditionalCriticalValue = conditionalCriticalValue,
        conditionalPowerPerStage = conditionalPowerPerStage,
        selectedPopulations = selectedPopulations
    ))
}


#'
#' @title
#' Get Simulation Enrichment Survival
#'
#' @description
#' Returns the simulated power, stopping and selection probabilities, conditional power,
#' and expected sample size for testing hazard ratios in an enrichment design testing situation.
#' In contrast to \code{getSimulationSurvival()} (where survival times are simulated), normally
#' distributed logrank test statistics are simulated.
#'
#' @inheritParams param_intersectionTest_Enrichment
#' @inheritParams param_typeOfSelection
#' @inheritParams param_effectMeasure
#' @inheritParams param_adaptations
#' @inheritParams param_threshold
#' @inheritParams param_effectList
#' @inheritParams param_successCriterion
#' @inheritParams param_typeOfSelection
#' @inheritParams param_design_with_default
#' @inheritParams param_directionUpper
#' @inheritParams param_allocationRatioPlanned
#' @inheritParams param_kappa
#' @inheritParams param_eventTime
#' @inheritParams param_accrualTime
#' @inheritParams param_accrualIntensity
#' @inheritParams param_accrualIntensityType
#' @inheritParams param_dropoutRate1
#' @inheritParams param_dropoutRate2
#' @inheritParams param_dropoutTime
#' @inheritParams param_maxNumberOfSubjects_survival
#' @inheritParams param_minNumberOfEventsPerStage
#' @inheritParams param_maxNumberOfEventsPerStage
#' @inheritParams param_conditionalPowerSimulation
#' @inheritParams param_thetaH1
#' @inheritParams param_plannedEvents
#' @inheritParams param_maxNumberOfIterations
#' @inheritParams param_calcEventsFunction
#' @inheritParams param_selectPopulationsFunction
#' @inheritParams param_rValue
#' @inheritParams param_epsilonValue
#' @inheritParams param_seed
#' @inheritParams param_three_dots
#' @inheritParams param_showStatistics
#' @inheritParams param_stratifiedAnalysis
#'
#'
#' @details
#' At given design the function simulates the power, stopping probabilities,
#' selection probabilities, and expected event number at given number of events,
#' parameter configuration, and population selection rule in the enrichment situation.
#' An allocation ratio can be specified referring to the ratio of number of subjects
#' in the active treatment group as compared to the control group.
#'
#' The definition of \code{thetaH1} makes only sense if \code{kMax} > 1
#' and if \code{conditionalPower}, \code{minNumberOfEventsPerStage}, and
#' \code{maxNumberOfEventsPerStage} (or \code{calcEventsFunction}) are defined.
#'
#' \code{calcEventsFunction}\cr
#' This function returns the number of events at given conditional power
#' and conditional critical value for specified testing situation.
#' The function might depend on the variables
#' \code{stage},
#' \code{selectedPopulations},
#' \code{plannedEvents},
#' \code{directionUpper},
#' \code{allocationRatioPlanned},
#' \code{minNumberOfEventsPerStage},
#' \code{maxNumberOfEventsPerStage},
#' \code{conditionalPower},
#' \code{conditionalCriticalValue}, and
#' \code{overallEffects}.
#' The function has to contain the three-dots argument '...' (see examples).
#'
#' @template return_object_simulation_results
#' @template how_to_get_help_for_generics
#'
#' @template examples_get_simulation_enrichment_survival
#'
#' @export
#'
getSimulationEnrichmentSurvival <- function(design = NULL,
                                            ...,
                                            effectList = NULL,
                                            kappa = 1,
                                            eventTime = 12, # C_EVENT_TIME_DEFAULT
                                            accrualTime = c(0, 12), # C_ACCRUAL_TIME_DEFAULT
                                            accrualIntensity = 0.1, # C_ACCRUAL_INTENSITY_DEFAULT
                                            accrualIntensityType = c("auto", "absolute", "relative"),
                                            dropoutRate1 = 0, # C_DROP_OUT_RATE_DEFAULT
                                            dropoutRate2 = 0, # C_DROP_OUT_RATE_DEFAULT
                                            dropoutTime = 12, # C_DROP_OUT_TIME_DEFAULT
                                            maxNumberOfSubjects = NA_real_,
                                            intersectionTest = c("Simes", "SpiessensDebois", "Bonferroni", "Sidak"), # C_INTERSECTION_TEST_ENRICHMENT_DEFAULT
                                            stratifiedAnalysis = TRUE, # C_STRATIFIED_ANALYSIS_DEFAULT
                                            directionUpper = NA, # C_DIRECTION_UPPER_DEFAULT
                                            adaptations = NA,
                                            typeOfSelection = c("best", "rBest", "epsilon", "all", "userDefined"), # C_TYPE_OF_SELECTION_DEFAULT
                                            effectMeasure = c("effectEstimate", "testStatistic"), # C_EFFECT_MEASURE_DEFAULT
                                            successCriterion = c("all", "atLeastOne"), # C_SUCCESS_CRITERION_DEFAULT
                                            epsilonValue = NA_real_,
                                            rValue = NA_real_,
                                            threshold = -Inf,
                                            plannedEvents = NA_real_,
                                            allocationRatioPlanned = NA_real_,
                                            minNumberOfEventsPerStage = NA_real_,
                                            maxNumberOfEventsPerStage = NA_real_,
                                            conditionalPower = NA_real_,
                                            thetaH1 = NA_real_,
                                            maxNumberOfIterations = 1000L, # C_MAX_SIMULATION_ITERATIONS_DEFAULT
                                            seed = NA_real_,
                                            calcEventsFunction = NULL,
                                            selectPopulationsFunction = NULL,
                                            showStatistics = FALSE) {
    if (is.null(design)) {
        design <- .getDefaultDesign(..., type = "simulation")
        .warnInCaseOfUnknownArguments(
            functionName = "getSimulationEnrichmentSurvival",
            ignore = c(
                .getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
                    design,
                    powerCalculationEnabled = TRUE
                ),
                "showStatistics"
            ),
            ...
        )
    } else {
        .assertIsTrialDesignInverseNormalOrFisher(design)
        .warnInCaseOfUnknownArguments(
            functionName = "getSimulationEnrichmentSurvival",
            ignore = "showStatistics",
            ...
        )
        .warnInCaseOfTwoSidedPowerArgument(...)
    }

    .assertIsOneSidedDesign(
        design,
        designType = "enrichment",
        engineType = "simulation"
    )
    .assertIsValidMaxNumberOfSubjects(
        maxNumberOfSubjects,
        naAllowed = TRUE
    )

    calcEventsFunctionIsUserDefined <- !is.null(calcEventsFunction)

    directionUpper <- .assertIsValidDirectionUpper(
        directionUpper,
        design,
        objectType = "power",
        userFunctionCallEnabled = TRUE
    )

    if (length(allocationRatioPlanned) != 1) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'allocationRatioPlanned' (",
            .arrayToString(allocationRatioPlanned),
            ") ",
            "must have length 1"
        )
    }

    simulationResults <- .createSimulationResultsEnrichmentObject(
        design = design,
        effectList = effectList,
        eventTime = eventTime,
        kappa = kappa,
        accrualTime = accrualTime,
        accrualIntensity = accrualIntensity,
        dropoutRate1 = dropoutRate1,
        dropoutRate2 = dropoutRate2,
        dropoutTime = dropoutTime,
        intersectionTest = intersectionTest,
        stratifiedAnalysis = stratifiedAnalysis,
        directionUpper = directionUpper, # rates + survival only
        adaptations = adaptations,
        typeOfSelection = typeOfSelection,
        effectMeasure = effectMeasure,
        successCriterion = successCriterion,
        epsilonValue = epsilonValue,
        rValue = rValue,
        threshold = threshold,
        plannedEvents = plannedEvents, # survival only
        maxNumberOfSubjects = maxNumberOfSubjects,
        allocationRatioPlanned = allocationRatioPlanned,
        minNumberOfEventsPerStage = minNumberOfEventsPerStage, # survival only
        maxNumberOfEventsPerStage = maxNumberOfEventsPerStage, # survival only
        conditionalPower = conditionalPower,
        thetaH1 = thetaH1, # means + survival only
        maxNumberOfIterations = maxNumberOfIterations,
        seed = seed,
        calcEventsFunction = calcEventsFunction, # survival only
        selectPopulationsFunction = selectPopulationsFunction,
        showStatistics = showStatistics,
        endpoint = "survival"
    )

    design <- simulationResults$.design
    effectList <- simulationResults$effectList
    successCriterion <- simulationResults$successCriterion
    effectMeasure <- simulationResults$effectMeasure
    adaptations <- simulationResults$adaptations
    gMax <- simulationResults$populations
    kMax <- simulationResults$.design$kMax
    intersectionTest <- simulationResults$intersectionTest
    typeOfSelection <- simulationResults$typeOfSelection
    thetaH1 <- simulationResults$thetaH1 # means + survival only
    plannedEvents <- simulationResults$plannedEvents # survival only
    conditionalPower <- simulationResults$conditionalPower
    minNumberOfEventsPerStage <- simulationResults$minNumberOfEventsPerStage # survival only
    maxNumberOfEventsPerStage <- simulationResults$maxNumberOfEventsPerStage # survival only
    allocationRatioPlanned <- simulationResults$allocationRatioPlanned
    calcEventsFunction <- simulationResults$calcEventsFunction

    indices <- .getIndicesOfClosedHypothesesSystemForSimulation(gMax = gMax)

    cols <- nrow(effectList$hazardRatios)

    simulatedNumberEventsNotAchieved <- matrix(0, nrow = kMax, ncol = cols)
    simulatedAnalysisTime <- matrix(0, nrow = kMax, ncol = cols)
    simulatedNumberOfSubjects <- matrix(0, nrow = kMax, ncol = cols)
    simulatedSelections <- array(0, dim = c(kMax, cols, gMax))
    simulatedRejections <- array(0, dim = c(kMax, cols, gMax))
    simulatedNumberOfPopulations <- matrix(0, nrow = kMax, ncol = cols)
    simulatedPopulationEventsPerStage <- array(0, dim = c(kMax, cols, gMax))
    simulatedNumberOfEvents <- matrix(0, nrow = kMax, ncol = cols)
    simulatedSuccessStopping <- matrix(0, nrow = kMax, ncol = cols)
    simulatedFutilityStopping <- matrix(0, nrow = kMax - 1, ncol = cols)
    simulatedConditionalPower <- matrix(0, nrow = kMax, ncol = cols)
    simulatedRejectAtLeastOne <- rep(0, cols)
    expectedNumberOfEvents <- rep(0, cols)
    expectedNumberOfSubjects <- rep(0, cols)
    expectedStudyDuration <- rep(0, cols)
    iterations <- matrix(0, nrow = kMax, ncol = cols)

    len <- maxNumberOfIterations * kMax * gMax * cols

    dataIterationNumber <- rep(NA_real_, len)
    dataStageNumber <- rep(NA_real_, len)
    dataArmNumber <- rep(NA_real_, len)
    dataAlternative <- rep(NA_real_, len)
    dataEffect <- rep(NA_real_, len)
    dataAnalysisTime <- rep(NA_real_, len)
    dataNumberOfSubjects <- rep(NA_real_, len)
    dataNumberOfEvents <- rep(NA_real_, len)
    dataRejectPerStage <- rep(NA, len)
    dataFutilityStop <- rep(NA_real_, len)
    dataSuccessStop <- rep(NA, len)
    dataFutilityStop <- rep(NA, len)
    dataTestStatistics <- rep(NA_real_, len)
    dataConditionalCriticalValue <- rep(NA_real_, len)
    dataConditionalPowerAchieved <- rep(NA_real_, len)
    dataEffectEstimate <- rep(NA_real_, len)
    dataPValuesSeparate <- rep(NA_real_, len)

    accrualSetup <- getAccrualTime(
        accrualTime = accrualTime,
        accrualIntensity = accrualIntensity,
        accrualIntensityType = accrualIntensityType,
        maxNumberOfSubjects = maxNumberOfSubjects
    )
    if (is.na(accrualSetup$maxNumberOfSubjects)) {
        if (identical(accrualIntensity, 1L)) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "choose a 'accrualIntensity' > 1 or define 'maxNumberOfSubjects'"
            )
        }
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'maxNumberOfSubjects' must be defined"
        )
    }
    simulationResults$maxNumberOfSubjects <- accrualSetup$maxNumberOfSubjects
    simulationResults$.setParameterType("maxNumberOfSubjects", accrualSetup$.getParameterType("maxNumberOfSubjects"))

    .setValueAndParameterType(simulationResults, "kappa", kappa, 1)

    allocationFraction <- .getFraction(allocationRatioPlanned)
    .warnInCaseOfExtremeAllocationRatios(allocationFraction[1], allocationFraction[2])

    accrualTime <- accrualSetup$.getAccrualTimeWithoutLeadingZero()
    recruitmentTimes <- .generateRecruitmentTimes(
        allocationRatioPlanned,
        accrualTime,
        accrualSetup$accrualIntensity
    )$recruit

    recruitmentTimes <- recruitmentTimes[1:accrualSetup$maxNumberOfSubjects]

    phi <- c(-log(1 - dropoutRate1), -log(1 - dropoutRate2)) / dropoutTime

    # to force last value to be last accrualTime
    recruitmentTimes[length(recruitmentTimes)] <- accrualTime[length(accrualTime)]

    index <- 1
    for (i in seq_len(cols)) {
        for (j in seq_len(maxNumberOfIterations)) {
            stageResults <- .getSimulatedStageResultsSurvivalEnrichmentSubjectsBased(
                design = design,
                subGroups = effectList$subGroups,
                prevalences = effectList$prevalences,
                piControls = effectList$piControls,
                hazardRatios = effectList$hazardRatios[i, ],
                kappa = kappa,
                phi = phi,
                eventTime = eventTime,
                recruitmentTimes = recruitmentTimes,
                allocationFraction = allocationFraction,
                directionUpper = directionUpper,
                stratifiedAnalysis = stratifiedAnalysis,
                plannedEvents = plannedEvents,
                typeOfSelection = typeOfSelection,
                effectMeasure = effectMeasure,
                adaptations = adaptations,
                epsilonValue = epsilonValue,
                rValue = rValue,
                threshold = threshold,
                minNumberOfEventsPerStage = minNumberOfEventsPerStage,
                maxNumberOfEventsPerStage = maxNumberOfEventsPerStage,
                conditionalPower = conditionalPower,
                thetaH1 = thetaH1,
                calcEventsFunction = calcEventsFunction,
                calcEventsFunctionIsUserDefined = calcEventsFunctionIsUserDefined,
                selectPopulationsFunction = selectPopulationsFunction
            )

            closedTest <- .performClosedCombinationTestForSimulationEnrichment(
                stageResults = stageResults,
                design = design,
                indices = indices,
                intersectionTest = intersectionTest,
                successCriterion = successCriterion
            )

            rejectAtSomeStage <- FALSE
            rejectedPopulationsBefore <- rep(FALSE, gMax)

            for (k in 1:kMax) {
                if (stageResults$eventsNotAchieved[k]) {
                    simulatedNumberEventsNotAchieved[k, i] <- simulatedNumberEventsNotAchieved[k, i] + 1
                } else {
                    simulatedAnalysisTime[k, i] <- simulatedAnalysisTime[k, i] + stageResults$analysisTime[k]
                    simulatedNumberOfSubjects[k, i] <- simulatedNumberOfSubjects[k, i] +
                        stageResults$numberOfSubjects[k]

                    simulatedRejections[k, i, ] <- simulatedRejections[k, i, ] +
                        (closedTest$rejected[, k] &
                            closedTest$selectedPopulations[1:gMax, k] |
                            rejectedPopulationsBefore)

                    simulatedSelections[k, i, ] <- simulatedSelections[k, i, ] + closedTest$selectedPopulations[, k]

                    for (g in 1:gMax) {
                        if (!is.na(stageResults$populationEventsPerStage[g, k])) {
                            simulatedPopulationEventsPerStage[k, i, g] <- simulatedPopulationEventsPerStage[k, i, g] +
                                stageResults$populationEventsPerStage[g, k]
                        }
                    }
                    simulatedNumberOfPopulations[k, i] <- simulatedNumberOfPopulations[k, i] +
                        sum(closedTest$selectedPopulations[, k])

                    if (!any(is.na(closedTest$successStop))) {
                        simulatedSuccessStopping[k, i] <- simulatedSuccessStopping[k, i] + closedTest$successStop[k]
                    }

                    if ((kMax > 1) && (k < kMax)) {
                        if (!any(is.na(closedTest$futilityStop))) {
                            simulatedFutilityStopping[k, i] <- simulatedFutilityStopping[k, i] +
                                (closedTest$futilityStop[k] && !closedTest$successStop[k])
                        }
                        if (!closedTest$successStop[k] && !closedTest$futilityStop[k]) {
                            simulatedConditionalPower[k + 1, i] <- simulatedConditionalPower[k + 1, i] +
                                stageResults$conditionalPowerPerStage[k]
                        }
                    }

                    iterations[k, i] <- iterations[k, i] + 1

                    if (k == 1) {
                        simulatedNumberOfEvents[k, i] <- simulatedNumberOfEvents[k, i] +
                            stageResults$plannedEvents[k]
                    } else {
                        simulatedNumberOfEvents[k, i] <- simulatedNumberOfEvents[k, i] +
                            stageResults$plannedEvents[k]
                    }

                    for (g in 1:gMax) {
                        dataIterationNumber[index] <- j
                        dataStageNumber[index] <- k
                        dataArmNumber[index] <- g
                        dataAlternative[index] <- i
                        dataEffect[index] <- effectList$hazardRatios[i, g]
                        dataAnalysisTime[index] <- stageResults$analysisTime[k]
                        dataNumberOfSubjects[index] <- stageResults$numberOfSubjects[k]
                        dataNumberOfEvents[index] <- round(stageResults$populationEventsPerStage[g, k], 1)
                        dataRejectPerStage[index] <- closedTest$rejected[g, k]
                        dataTestStatistics[index] <- stageResults$testStatistics[g, k]
                        dataSuccessStop[index] <- closedTest$successStop[k]
                        if (k < kMax) {
                            dataFutilityStop[index] <- closedTest$futilityStop[k]
                            dataConditionalCriticalValue[index] <- stageResults$conditionalCriticalValue[k]
                            dataConditionalPowerAchieved[index + 1] <- stageResults$conditionalPowerPerStage[k]
                        }
                        dataEffectEstimate[index] <- stageResults$overallEffects[g, k]
                        dataPValuesSeparate[index] <- closedTest$separatePValues[g, k]
                        index <- index + 1
                    }

                    if (
                        !rejectAtSomeStage &&
                            any(
                                closedTest$rejected[, k] &
                                    closedTest$selectedPopulations[1:gMax, k] |
                                    rejectedPopulationsBefore
                            )
                    ) {
                        simulatedRejectAtLeastOne[i] <- simulatedRejectAtLeastOne[i] + 1
                        rejectAtSomeStage <- TRUE
                    }

                    if ((k < kMax) && (closedTest$successStop[k] || closedTest$futilityStop[k])) {
                        # rejected hypotheses remain rejected also in case of early stopping
                        simulatedRejections[(k + 1):kMax, i, ] <- simulatedRejections[(k + 1):kMax, i, ] +
                            matrix(
                                (closedTest$rejected[, k] &
                                    closedTest$selectedPopulations[1:gMax, k] |
                                    rejectedPopulationsBefore),
                                kMax - k,
                                gMax,
                                byrow = TRUE
                            )
                        break
                    }

                    rejectedPopulationsBefore <- closedTest$rejected[, k] &
                        closedTest$selectedPopulations[1:gMax, k] |
                        rejectedPopulationsBefore
                }
            }
        }

        for (g in 1:gMax) {
            simulatedPopulationEventsPerStage[, i, g] <- round(
                simulatedPopulationEventsPerStage[, i, g] / iterations[, i],
                1
            )
        }
        simulatedNumberOfEvents[, i] <- simulatedNumberOfEvents[, i] / iterations[, i]
        simulatedNumberOfSubjects[, i] <- simulatedNumberOfSubjects[, i] / iterations[, i]
        simulatedAnalysisTime[, i] <- simulatedAnalysisTime[, i] / iterations[, i]

        if (kMax > 1) {
            simulatedRejections[2:kMax, i, ] <- simulatedRejections[2:kMax, i, ] -
                simulatedRejections[1:(kMax - 1), i, ]
            stopping <- cumsum(simulatedSuccessStopping[1:(kMax - 1), i] + simulatedFutilityStopping[, i]) /
                maxNumberOfIterations
            expectedNumberOfEvents[i] <- simulatedNumberOfEvents[1, i] +
                t(1 - stopping) %*%
                (simulatedNumberOfEvents[2:kMax, i] - simulatedNumberOfEvents[1:(kMax - 1), i])
            expectedNumberOfSubjects[i] <- simulatedNumberOfSubjects[1, i] +
                t(1 - stopping) %*%
                (simulatedNumberOfSubjects[2:kMax, i] - simulatedNumberOfSubjects[1:(kMax - 1), i])
            expectedStudyDuration[i] <- simulatedAnalysisTime[1, i] +
                t(1 - stopping) %*%
                (simulatedAnalysisTime[2:kMax, i] - simulatedAnalysisTime[1:(kMax - 1), i])
        } else {
            expectedNumberOfEvents[i] <- simulatedNumberOfEvents[1, i]
            expectedNumberOfSubjects[i] <- simulatedNumberOfSubjects[1, i]
            expectedStudyDuration[i] <- simulatedAnalysisTime[1, i]
        }
    }

    simulatedConditionalPower[1, ] <- NA_real_
    if (kMax > 1) {
        for (k in 2:kMax) {
            simulatedConditionalPower[k, ] <- simulatedConditionalPower[k, ] /
                (iterations[k, ] +
                    simulatedNumberEventsNotAchieved[k, ])
        }
    }

    simulationResults$numberOfPopulations <- simulatedNumberOfPopulations / iterations
    simulationResults$numberOfSubjects <- simulatedNumberOfSubjects
    simulationResults$populationEventsPerStage <- simulatedPopulationEventsPerStage
    simulationResults$analysisTime <- simulatedAnalysisTime
    simulationResults$eventsNotAchieved <- simulatedNumberEventsNotAchieved / maxNumberOfIterations
    simulationResults$rejectAtLeastOne <- simulatedRejectAtLeastOne / maxNumberOfIterations
    simulationResults$selectedPopulations <- simulatedSelections / maxNumberOfIterations
    simulationResults$rejectedPopulationsPerStage <- simulatedRejections / maxNumberOfIterations
    simulationResults$successPerStage <- simulatedSuccessStopping / maxNumberOfIterations
    simulationResults$futilityPerStage <- simulatedFutilityStopping / maxNumberOfIterations
    simulationResults$futilityStop <- base::colSums(simulatedFutilityStopping / maxNumberOfIterations)
    simulationResults$expectedNumberOfEvents <- expectedNumberOfEvents
    simulationResults$expectedNumberOfSubjects <- expectedNumberOfSubjects
    simulationResults$studyDuration <- expectedStudyDuration
    simulationResults$cumulativeEventsPerStage <- simulatedNumberOfEvents
    simulationResults$iterations <- iterations
    if (kMax > 1) {
        simulationResults$earlyStop <- simulationResults$futilityPerStage +
            simulationResults$successPerStage[1:(kMax - 1), ]
        simulationResults$conditionalPowerAchieved <- simulatedConditionalPower
    }

    ## set parameter types in simulationResults
    if (kMax > 1) {
        simulationResults$.setParameterType("expectedNumberOfSubjects", C_PARAM_GENERATED)
        simulationResults$.setParameterType("expectedNumberOfEvents", C_PARAM_GENERATED)
        simulationResults$.setParameterType("studyDuration", C_PARAM_GENERATED)
    }
    simulationResults$.setParameterType(
        "selectedPopulations",
        ifelse(gMax == 1, C_PARAM_NOT_APPLICABLE, C_PARAM_GENERATED)
    )

    simulationResults$.setParameterType("numberOfSubjects", C_PARAM_GENERATED)
    simulationResults$.setParameterType("analysisTime", C_PARAM_GENERATED)
    if (!all(is.na(simulationResults$conditionalPowerAchieved))) {
        simulationResults$.setParameterType("conditionalPowerAchieved", C_PARAM_GENERATED)
    }

    if (any(simulationResults$eventsNotAchieved > 0)) {
        warning(
            "Presumably due to small number of subjects in selected arms, ",
            "required number of events were not achieved for at least one situation. ",
            "Increase the maximum number of subjects (",
            accrualSetup$maxNumberOfSubjects,
            ") ",
            "to avoid this situation",
            call. = FALSE
        )
    }

    if (any(simulationResults$rejectedPopulationsPerStage < 0)) {
        stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "internal error, simulation not possible due to numerical overflow")
    }

    data <- data.frame(
        iterationNumber = dataIterationNumber,
        stageNumber = dataStageNumber,
        populationNumber = dataArmNumber,
        omegaMax = dataAlternative,
        effect = dataEffect,
        numberOfEvents = dataNumberOfEvents,
        analysisTime = dataAnalysisTime,
        numberOfSubjects = dataNumberOfSubjects,
        effectEstimate = dataEffectEstimate,
        testStatistics = dataTestStatistics,
        pValue = dataPValuesSeparate,
        conditionalCriticalValue = round(dataConditionalCriticalValue, 6),
        conditionalPowerAchieved = round(dataConditionalPowerAchieved, 6),
        rejectPerStage = dataRejectPerStage,
        successStop = dataSuccessStop,
        futilityPerStage = dataFutilityStop
    )

    data <- data[!is.na(data$effectEstimate), ]
    simulationResults$.data <- data

    return(simulationResults)
}
