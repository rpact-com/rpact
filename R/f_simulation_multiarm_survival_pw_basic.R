## |
## |  *Simulation of multi-arm design with time to event data*
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
## |  File version: $Revision: 8449 $
## |  Last changed: $Date: 2024-12-10 09:39:04 +0100 (Di, 10 Dez 2024) $
## |  Last changed by: $Author: wassmer $
## |

#' @include f_simulation_multiarm.R
NULL

#'
#' Calculates the logrank test statistic for the multi-armed survival data set at given time
#' and specified treatment arm comparison
#'
#' @noRd
#'
.logRankTestMultiArm <- function(survivalDataSet, time, treatmentArms, directionUpper = TRUE, thetaH0 = 1) {
    subjectsT1 <- 0
    subjectsT2 <- 0

    survivalDataSetSelectedArms <- survivalDataSet[survivalDataSet$treatmentArm %in% treatmentArms, ]
    numberOfSubjects <- length(survivalDataSetSelectedArms$accrualTime)

    for (i in (1:numberOfSubjects)) {
        if (survivalDataSetSelectedArms$accrualTime[i] > time) {
            survivalDataSetSelectedArms$treatmentArm[i] <- 0
            survivalDataSetSelectedArms$event[i] <- FALSE
            survivalDataSetSelectedArms$dropoutEvent[i] <- FALSE
        }

        if (
            (survivalDataSetSelectedArms$treatmentArm[i] == treatmentArms[1]) &&
                (survivalDataSetSelectedArms$accrualTime[i] <= time)
            ) {
            subjectsT1 <- subjectsT1 + 1
        }
        if (
            (survivalDataSetSelectedArms$treatmentArm[i] == treatmentArms[2]) &&
                (survivalDataSetSelectedArms$accrualTime[i] <= time)
            ) {
            subjectsT2 <- subjectsT2 + 1
        }

        if (
            (survivalDataSetSelectedArms$accrualTime[i] + survivalDataSetSelectedArms$survivalTime[i] < time) &&
                (survivalDataSetSelectedArms$treatmentArm[i] > 0) &&
                ((survivalDataSetSelectedArms$dropoutTime[i] > survivalDataSetSelectedArms$survivalTime[i]) ||
                    is.na(survivalDataSetSelectedArms$dropoutTime[i]))
            ) {
            survivalDataSetSelectedArms$event[i] <- TRUE
        } else {
            survivalDataSetSelectedArms$event[i] <- FALSE
        }

        if (
            (survivalDataSetSelectedArms$accrualTime[i] + survivalDataSetSelectedArms$dropoutTime[i] < time) &&
                (survivalDataSetSelectedArms$treatmentArm[i] > 0) &&
                (survivalDataSetSelectedArms$dropoutTime[i] < survivalDataSetSelectedArms$survivalTime[i]) &&
                !is.na(survivalDataSetSelectedArms$dropoutTime[i])
            ) {
            survivalDataSetSelectedArms$dropoutEvent[i] <- TRUE
        } else {
            survivalDataSetSelectedArms$dropoutEvent[i] <- FALSE
        }

        if (survivalDataSetSelectedArms$event[i]) {
            survivalDataSetSelectedArms$timeUnderObservation[i] <- survivalDataSetSelectedArms$survivalTime[i]
        } else if (survivalDataSetSelectedArms$dropoutEvent[i]) {
            survivalDataSetSelectedArms$timeUnderObservation[i] <- survivalDataSetSelectedArms$dropoutTime[i]
        } else {
            survivalDataSetSelectedArms$timeUnderObservation[i] <- time - survivalDataSetSelectedArms$accrualTime[i]
        }
    }
    subjectNumber <- subjectsT1 + subjectsT2

    sortedIndex <- order(survivalDataSetSelectedArms$timeUnderObservation, decreasing = FALSE)
    survivalDataSetSelectedArmsSorted <- survivalDataSetSelectedArms[sortedIndex, ]
    numerator <- 0
    denominator <- 0
    events1 <- 0
    events2 <- 0
    for (i in (1:numberOfSubjects)) {
        if (survivalDataSetSelectedArmsSorted$event[i]) {
            if (survivalDataSet$treatmentArm[i] == 1) {
                if (subjectsT1 + subjectsT2 > 0) {
                    numerator <- numerator + 1 - thetaH0 * subjectsT1 / (thetaH0 * subjectsT1 + subjectsT2)
                }
                events1 <- events1 + 1
            } else if (survivalDataSet$treatmentArm[i] == 2) {
                if (subjectsT1 + subjectsT2 > 0) {
                    numerator <- numerator - thetaH0 * subjectsT1 / (thetaH0 * subjectsT1 + subjectsT2)
                }
                events2 <- events2 + 1
            }
            if (subjectsT1 + subjectsT2 > 0) {
                denominator <- denominator +
                    thetaH0 * subjectsT1 * subjectsT2 / (thetaH0 * subjectsT1 + subjectsT2)^2
            }
        }
        if (survivalDataSetSelectedArmsSorted$treatmentArm[i] == treatmentArms[1]) {
            subjectsT1 <- subjectsT1 - 1
        }
        if (survivalDataSetSelectedArmsSorted$treatmentArm[i] == treatmentArms[2]) {
            subjectsT2 <- subjectsT2 - 1
        }
    }

    if (directionUpper && denominator > 0) {
        logRank <- numerator / sqrt(denominator)
    } else if (!directionUpper && denominator > 0) {
        logRank <- -numerator / sqrt(denominator)
    } else {
        logRank <- 0
    }

    return(list(
        logRank = logRank,
        thetaH0 = thetaH0,
        directionUpper = directionUpper,
        subjectNumber = subjectNumber,
        events = c(events1, events2)
    ))
}

#'
#' Calculates the time until required events were observed.
#' If number of subjects (taking into account drop outs) are too small, eventsAchieved = FALSE
#' is returned (same function as in f_simulation_enrichment_survival_new.R)
#'
#' @noRd
#'
.findObservationTime <- function(
        survivalDataSet,
        requiredStageEvents
        ) {
    numberOfSubjects <- length(survivalDataSet$accrualTime)
    upperBound <- 1
    repeat {
        numberOfEvents <- 0
        for (i in (1:numberOfSubjects)) {
            if (
                (survivalDataSet$accrualTime[i] + survivalDataSet$survivalTime[i] < upperBound) &&
                    ((survivalDataSet$dropoutTime[i] > survivalDataSet$survivalTime[i]) ||
                        is.na(survivalDataSet$dropoutTime[i]))
                ) {
                numberOfEvents <- numberOfEvents + 1
            }
        }
        upperBound <- 2 * upperBound
        if ((numberOfEvents >= requiredStageEvents) || (upperBound > 1E20)) break
    }

    if (upperBound > 1E20) {
        return(list(
            time = NA,
            eventsAchieved = FALSE
        ))
    } else {
        lower <- 0
        upper <- upperBound
        repeat {
            time <- (lower + upper) / 2
            numberOfEvents <- 0
            for (i in (1:numberOfSubjects)) {
                if (
                    (survivalDataSet$accrualTime[i] + survivalDataSet$survivalTime[i] <= time) &&
                        ((survivalDataSet$dropoutTime[i] > survivalDataSet$survivalTime[i]) ||
                            is.na(survivalDataSet$dropoutTime[i]))
                    ) {
                    numberOfEvents <- numberOfEvents + 1
                }
            }
            ifelse(numberOfEvents >= requiredStageEvents, upper <- time, lower <- time)
            if (upper - lower < 1E-5) break
        }
        if (numberOfEvents > requiredStageEvents) time <- time - 1E-5
        if (numberOfEvents < requiredStageEvents) time <- time + 1E-5
        return(list(
            time = time,
            eventsAchieved = TRUE
        ))
    }
}

.getTreatmentsMultiArm <- function(gMax, maxNumberOfSubjects, allocationFraction) {
    treatments <- c()
    while (length(treatments) < maxNumberOfSubjects) {
        if (allocationFraction[1] > allocationFraction[2]) {
            treatments <- c(
                treatments,
                rep(1:(gMax + 1), allocationFraction[2]),
                rep(1:gMax, allocationFraction[1] - allocationFraction[2])
            )
        } else {
            treatments <- c(
                treatments,
                rep(1:(gMax + 1), allocationFraction[1]),
                rep(gMax + 1, allocationFraction[2] - allocationFraction[1])
            )
        }
    }
    treatments[1:maxNumberOfSubjects]
}

.updateTreatmentsVector <- function(
        k,
        gMax,
        maxNumberOfSubjects,
        numberOfSubjects,
        treatments,
        selectedArms,
        allocationFraction
        ) {
    treatments <- treatments[1:numberOfSubjects[k - 1]]
    while (length(treatments) < maxNumberOfSubjects) {
        if (allocationFraction[1] > allocationFraction[2]) {
            treatments <- c(
                treatments,
                rep(c(which(selectedArms[, k]), gMax + 1), allocationFraction[2]),
                rep(which(selectedArms[, k]), allocationFraction[1] - allocationFraction[2])
            )
        } else {
            treatments <- c(
                treatments,
                rep(c(which(selectedArms[, k]), gMax + 1), allocationFraction[1]),
                rep(gMax + 1, allocationFraction[2] - allocationFraction[1])
            )
        }
    }
    treatments
}

#'
#' Calculates stage results for each simulation iteration step (R Based)
#'
#' @noRd
#'
.getSimulatedStageResultsSurvivalMultiArmPatientWise <- function(
        ...,
        design,
        directionUpper,
        omegaVector,
        piControl,
        kappa,
        phi,
        eventTime,
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
        selectArmsFunction
        ) {
    kMax <- length(plannedEvents)
    gMax <- length(omegaVector)
    maxNumberOfSubjects <- length(recruitmentTimes)

    singleEventsPerStage <- matrix(NA_integer_, nrow = gMax + 1, ncol = kMax)
    cumulativeEventsPerStage <- matrix(NA_integer_, nrow = gMax, ncol = kMax)
    simSurvival <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    overallEffects <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    testStatistics <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    overallTestStatistics <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    separatePValues <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    conditionalCriticalValue <- rep(NA_real_, kMax - 1)
    conditionalPowerPerStage <- rep(NA_real_, kMax)
    selectedArms <- matrix(FALSE, nrow = gMax, ncol = kMax)
    selectedArms[, 1] <- TRUE
    adjustedPValues <- rep(NA_real_, kMax)
    analysisTime <- rep(NA_real_, kMax)
    numberOfSubjects <- rep(NA_real_, kMax)
    eventsNotAchieved <- rep(FALSE, kMax)

    if (.isTrialDesignFisher(design)) {
        weights <- .getWeightsFisher(design)
    } else if (.isTrialDesignInverseNormal(design)) {
        weights <- .getWeightsInverseNormal(design)
    }

    ##  Create data set for first stage  ##
    treatments <- .getTreatmentsMultiArm(gMax, maxNumberOfSubjects, allocationFraction)

    survivalDataSet <- data.frame(
        accrualTime = recruitmentTimes,
        treatmentArm = treatments
    )

    lambdaControl <- getLambdaByPi(piControl, eventTime, kappa)
    lambdaVector <- c(omegaVector * lambdaControl, lambdaControl)

    for (i in 1:maxNumberOfSubjects) {
        for (g in 1:(gMax + 1)) {
            if (survivalDataSet$treatmentArm[i] == g) {
                survivalDataSet$survivalTime[i] <- (-log(1 - runif(1, 0, 1)))^(1 / kappa) / lambdaVector[g]
            }
        }
        if (any(phi > 0)) {
            if (phi[1] > 0) {
                for (g in 1:gMax) {
                    if (survivalDataSet$treatmentArm[i] == g) {
                        survivalDataSet$dropoutTime[i] <- -log(1 - runif(1, 0, 1)) / phi[1]
                    }
                }
            }
            if (phi[2] > 0) {
                if (survivalDataSet$treatmentArm[i] == gMax + 1) {
                    survivalDataSet$dropoutTime[i] <- -log(1 - runif(1, 0, 1)) / phi[2]
                }
            }
        } else {
            survivalDataSet$dropoutTime[i] <- NA_real_
        }
    }

    ##   Perform test results over stages  ##
    for (k in 1:kMax) {
        if (k == 1) {
            analysisTime[k] <- .findObservationTime(
                survivalDataSet,
                plannedEvents[k]
            )$time
            if (is.na(analysisTime[k])) {
                eventsNotAchieved[k] <- TRUE
                break
            } else {
                numberOfSubjects[k] <- sum(survivalDataSet$accrualTime <= analysisTime[k])
                for (g in 1:gMax) {
                    logRank <- .logRankTestMultiArm(
                        survivalDataSet = survivalDataSet,
                        time = analysisTime[1],
                        treatmentArms = c(g, gMax + 1),
                        directionUpper = directionUpper
                    )

                    testStatistics[g, k] <- logRank$logRank
                    overallTestStatistics[g, k] <- logRank$logRank
                    cumulativeEventsPerStage[g, k] <- sum(logRank$events)
                    singleEventsPerStage[g, k] <- logRank$events[1]
                }
                singleEventsPerStage[gMax + 1, k] <- logRank$events[2]
            }
        } else {
            if (analysisTime[k - 1] < max(survivalDataSet$accrualTime)) {
                #  create new survival and dropout times for selected treatment arms
                if (!all(selectedArms[, k] & selectedArms[, k - 1])) {
                    treatments <- .updateTreatmentsVector(
                        k,
                        gMax,
                        maxNumberOfSubjects,
                        numberOfSubjects,
                        treatments,
                        selectedArms,
                        allocationFraction
                    )

                    survivalDataSet$treatmentArm <- treatments[1:maxNumberOfSubjects]

                    for (i in numberOfSubjects[k - 1]:maxNumberOfSubjects) {
                        for (g in 1:gMax) {
                            if (survivalDataSet$treatmentArm[i] == g && selectedArms[g, k]) {
                                survivalDataSet$survivalTime[i] <- (-log(1 - runif(1, 0, 1)))^(1 / kappa) /
                                    lambdaVector[g]
                            }
                        }
                        if (any(phi > 0)) {
                            if (phi[1] > 0) {
                                for (g in 1:gMax) {
                                    if (survivalDataSet$treatmentArm[i] == g && selectedArms[g, k]) {
                                        survivalDataSet$dropoutTime[i] <- -log(1 - runif(1, 0, 1)) / phi[1]
                                    }
                                }
                                if (survivalDataSet$treatmentArm[i] == gMax + 1) {
                                    survivalDataSet$dropoutTime[i] <- -log(1 - runif(1, 0, 1)) / phi[2]
                                }
                            }
                        } else {
                            survivalDataSet$dropoutTime[i] <- NA_real_
                        }
                    }
                }
            }

            survivalDataSetSelected <- survivalDataSet[survivalDataSet$treatmentArm %in% c(which(selectedArms[, k]), gMax + 1), ]

            analysisTime[k] <- .findObservationTime(
                survivalDataSetSelected,
                plannedEvents[k]
            )$time

            if (is.na(analysisTime[k])) {
                eventsNotAchieved[k] <- TRUE
                break
            } else {
                numberOfSubjects[k] <- sum(survivalDataSet$accrualTime <= analysisTime[k])
                for (g in 1:gMax) {
                    if (selectedArms[g, k]) {
                        logRank <- .logRankTestMultiArm(
                            survivalDataSet = survivalDataSet,
                            time = analysisTime[k],
                            treatmentArms = c(g, gMax + 1),
                            directionUpper = directionUpper
                        )

                        overallTestStatistics[g, k] <- logRank$logRank
                        singleEventsPerStage[g, k] <- logRank$events[1]
                        cumulativeEventsPerStage[g, k] <- sum(logRank$events)
                        testStatistics[g, k] <- (sqrt(cumulativeEventsPerStage[g, k]) *
                            overallTestStatistics[g, k] -
                            sqrt(cumulativeEventsPerStage[g, k - 1]) * overallTestStatistics[g, k - 1]) /
                            sqrt(cumulativeEventsPerStage[g, k] - cumulativeEventsPerStage[g, k - 1])
                    }
                }
                singleEventsPerStage[gMax + 1, k] <- logRank$events[2]
            }
        }
        separatePValues[, k] <- 1 - stats::pnorm(testStatistics[, k])
        overallEffects[, k] <- exp(
            (2 * directionUpper - 1) *
                overallTestStatistics[, k] *
                (1 + allocationFraction[1] / allocationFraction[2]) /
                sqrt(allocationFraction[1] / allocationFraction[2]) /
                sqrt(cumulativeEventsPerStage[, k])
        )

        if (k < kMax) {
            if (base::colSums(selectedArms)[k] == 0) {
                break
            }
            # Bonferroni adjustment
            adjustedPValues[k] <- min(
                min(separatePValues[, k], na.rm = TRUE) * (base::colSums(selectedArms)[k]),
                1 - 1e-07
            )
            # conditional critical value to reject the null hypotheses at the next stage of the trial
            if (.isTrialDesignConditionalDunnett(design)) {
                conditionalCriticalValue[k] <- (.getOneMinusQNorm(design$alpha) -
                    .getOneMinusQNorm(adjustedPValues[k]) *
                        sqrt(design$informationAtInterim)) /
                    sqrt(1 - design$informationAtInterim)
            } else {
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
            }
            if (adaptations[k]) {
                selectArmsFunctionArgs <- list(
                    effectVector = NULL,
                    stage = k,
                    directionUpper = directionUpper,
                    conditionalPower = conditionalPower,
                    conditionalCriticalValue = conditionalCriticalValue,
                    plannedEvents = plannedEvents,
                    allocationRatioPlanned = allocationFraction[1] / allocationFraction[2],
                    selectedArms = selectedArms,
                    thetaH1 = thetaH1,
                    overallEffects = overallEffects
                )
                args <- list(
                    typeOfSelection = typeOfSelection,
                    epsilonValue = epsilonValue,
                    rValue = rValue,
                    threshold = threshold,
                    selectArmsFunction = selectArmsFunction,
                    selectArmsFunctionArgs = NULL,
                    survival = TRUE
                )

                if (effectMeasure == "testStatistic") {
                    selectArmsFunctionArgs$effectVector <- overallTestStatistics[, k]
                } else if (effectMeasure == "effectEstimate") {
                    if (is.na(directionUpper) || isTRUE(directionUpper)) {
                        selectArmsFunctionArgs$effectVector <- overallEffects[, k]
                    } else {
                        selectArmsFunctionArgs$effectVector <- 1 / overallEffects[, k]
                        args$threshold <- 1 / threshold
                    }
                }

                args$selectArmsFunctionArgs <- selectArmsFunctionArgs

                selectedArms[, k + 1] <- (selectedArms[, k] & do.call(.selectTreatmentArms, args))

                newEvents <- calcEventsFunction(
                    stage = k + 1, # to be consistent with non-multiarm situation, cf. line 38
                    directionUpper = directionUpper,
                    conditionalPower = conditionalPower,
                    conditionalCriticalValue = conditionalCriticalValue,
                    plannedEvents = plannedEvents,
                    # necessary for use in .getSimulationSurvivalMultiArmStageEventsBasic():
                    allocationRatioPlanned = rep(allocationFraction[1] / allocationFraction[2], k + 1),
                    selectedArms = selectedArms,
                    estimatedTheta = thetaH1,
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
                    plannedEvents[(k + 1):kMax] <- plannedEvents[k] + cumsum(rep(ceiling(newEvents), kMax - k))
                }
            } else {
                selectedArms[, k + 1] <- selectedArms[, k]
            }

            if (is.na(thetaH1)) {
                thetaStandardized <- log(min(overallEffects[selectedArms[1:gMax, k], k], na.rm = TRUE))
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
        singleEventsPerStage = singleEventsPerStage,
        cumulativeEventsPerStage = cumulativeEventsPerStage,
        plannedEvents = plannedEvents,
        analysisTime = analysisTime,
        numberOfSubjects = numberOfSubjects,
        ##  necessary for use in .performClosedCombinationTestForSimulationMultiArm():
        allocationRatioPlanned = rep(allocationFraction[1] / allocationFraction[2], kMax),
        testStatistics = testStatistics,
        overallEffects = overallEffects,
        overallTestStatistics = overallTestStatistics,
        separatePValues = separatePValues,
        conditionalCriticalValue = conditionalCriticalValue,
        conditionalPowerPerStage = conditionalPowerPerStage,
        selectedArms = selectedArms
    ))
}

.performSimulationMultiArmSurvivalLoop <- function(
        cols,
        maxNumberOfIterations,
        design,
        directionUpper,
        effectMatrix,
        omegaMaxVector,
        piControl,
        kappa,
        phi,
        eventTime,
        plannedEvents,
        recruitmentTimes,
        typeOfSelection,
        effectMeasure,
        adaptations,
        epsilonValue,
        rValue,
        threshold,
        allocationFraction,
        minNumberOfEventsPerStage,
        maxNumberOfEventsPerStage,
        conditionalPower,
        thetaH1,
        calcEventsFunction,
        calcEventsFunctionIsUserDefined,
        selectArmsFunction,
        indices,
        intersectionTest,
        criticalValuesDunnett,
        successCriterion,
        gMax,
        kMax
        ) {
    # Initialize simulation result matrices
    simulatedNumberEventsNotAchieved <- matrix(0, nrow = kMax, ncol = cols)
    simulatedAnalysisTime <- matrix(0, nrow = kMax, ncol = cols)
    simulatedNumberOfSubjects <- matrix(0, nrow = kMax, ncol = cols)
    simulatedSelections <- array(0, dim = c(kMax, cols, gMax))
    simulatedRejections <- array(0, dim = c(kMax, cols, gMax))
    simulatedNumberOfActiveArms <- matrix(0, nrow = kMax, ncol = cols)
    simulatedSingleEventsPerStage <- array(0, dim = c(kMax, cols, gMax + 1))
    simulatedPlannedEvents <- matrix(0, nrow = kMax, ncol = cols)
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

    index <- 1
    for (i in seq_len(cols)) {
        for (j in seq_len(maxNumberOfIterations)) {
            stageResults <- .getSimulatedStageResultsSurvivalMultiArmPatientWise(
                design = design,
                directionUpper = directionUpper,
                omegaVector = effectMatrix[i, ],
                piControl = piControl,
                kappa = kappa,
                phi = phi,
                eventTime = eventTime,
                plannedEvents = plannedEvents,
                recruitmentTimes = recruitmentTimes,
                typeOfSelection = typeOfSelection,
                effectMeasure = effectMeasure,
                adaptations = adaptations,
                epsilonValue = epsilonValue,
                rValue = rValue,
                threshold = threshold,
                allocationFraction = allocationFraction,
                minNumberOfEventsPerStage = minNumberOfEventsPerStage,
                maxNumberOfEventsPerStage = maxNumberOfEventsPerStage,
                conditionalPower = conditionalPower,
                thetaH1 = thetaH1,
                calcEventsFunction = calcEventsFunction,
                calcEventsFunctionIsUserDefined = calcEventsFunctionIsUserDefined,
                selectArmsFunction = selectArmsFunction
            )
            closedTest <- if (.isTrialDesignConditionalDunnett(design)) {
                .performClosedConditionalDunnettTestForSimulation(
                    stageResults = stageResults,
                    design = design,
                    indices = indices,
                    criticalValuesDunnett = criticalValuesDunnett,
                    successCriterion = successCriterion
                )
            } else {
                .performClosedCombinationTestForSimulationMultiArm(
                    stageResults = stageResults,
                    design = design,
                    indices = indices,
                    intersectionTest = intersectionTest,
                    successCriterion = successCriterion
                )
            }

            rejectAtSomeStage <- FALSE
            rejectedArmsBefore <- rep(FALSE, gMax)

            for (k in 1:kMax) {
                if (stageResults$eventsNotAchieved[k]) {
                    simulatedNumberEventsNotAchieved[k, i] <- simulatedNumberEventsNotAchieved[k, i] + 1
                } else {
                    simulatedAnalysisTime[k, i] <- simulatedAnalysisTime[k, i] + stageResults$analysisTime[k]
                    simulatedNumberOfSubjects[k, i] <- simulatedNumberOfSubjects[k, i] +
                        stageResults$numberOfSubjects[k]
                    simulatedRejections[k, i, ] <- simulatedRejections[k, i, ] +
                        (closedTest$rejected[, k] & closedTest$selectedArms[1:gMax, k] | rejectedArmsBefore)
                    simulatedSelections[k, i, ] <- simulatedSelections[k, i, ] + closedTest$selectedArms[, k]
                    simulatedNumberOfActiveArms[k, i] <- simulatedNumberOfActiveArms[k, i] +
                        sum(closedTest$selectedArms[, k])
                    if (!anyNA(closedTest$successStop)) {
                        simulatedSuccessStopping[k, i] <- simulatedSuccessStopping[k, i] + closedTest$successStop[k]
                    }
                    if ((kMax > 1) && (k < kMax)) {
                        if (!anyNA(closedTest$futilityStop)) {
                            simulatedFutilityStopping[k, i] <- simulatedFutilityStopping[k, i] +
                                (closedTest$futilityStop[k] && !closedTest$successStop[k])
                        }
                        if (!closedTest$successStop[k] && !closedTest$futilityStop[k]) {
                            simulatedConditionalPower[k + 1, i] <- simulatedConditionalPower[k + 1, i] +
                                stageResults$conditionalPowerPerStage[k]
                        }
                    }
                    for (g in (1:gMax)) {
                        if (!is.na(stageResults$cumulativeEventsPerStage[g, k])) {
                            simulatedSingleEventsPerStage[k, i, g] <- simulatedSingleEventsPerStage[k, i, g] +
                                stageResults$singleEventsPerStage[g, k]
                        }
                    }
                    simulatedSingleEventsPerStage[k, i, gMax + 1] <- simulatedSingleEventsPerStage[k, i, gMax + 1] +
                        stageResults$singleEventsPerStage[gMax + 1, k]

                    simulatedPlannedEvents[k, i] <- simulatedPlannedEvents[k, i] +
                        sum(stageResults$singleEventsPerStage[, k], na.rm = TRUE)

                    iterations[k, i] <- iterations[k, i] + 1

                    for (g in 1:gMax) {
                        dataIterationNumber[index] <- j
                        dataStageNumber[index] <- k
                        dataArmNumber[index] <- g
                        dataAlternative[index] <- omegaMaxVector[i]
                        dataEffect[index] <- effectMatrix[i, g]
                        dataAnalysisTime[index] <- stageResults$analysisTime[k]
                        dataNumberOfSubjects[index] <- stageResults$numberOfSubjects[k]
                        dataNumberOfEvents[index] <- round(stageResults$cumulativeEventsPerStage[g, k], 1)
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
                            any(closedTest$rejected[, k] & closedTest$selectedArms[1:gMax, k] | rejectedArmsBefore)
                        ) {
                        simulatedRejectAtLeastOne[i] <- simulatedRejectAtLeastOne[i] + 1
                        rejectAtSomeStage <- TRUE
                    }

                    if ((k < kMax) && (closedTest$successStop[k] || closedTest$futilityStop[k])) {
                        # rejected hypotheses remain rejected also in case of early stopping
                        simulatedRejections[(k + 1):kMax, i, ] <- simulatedRejections[(k + 1):kMax, i, ] +
                            matrix(
                                (closedTest$rejected[, k] & closedTest$selectedArms[1:gMax, k] | rejectedArmsBefore),
                                kMax - k,
                                gMax,
                                byrow = TRUE
                            )
                        break
                    }

                    rejectedArmsBefore <- closedTest$rejected[, k] &
                        closedTest$selectedArms[1:gMax, k] |
                        rejectedArmsBefore
                }
            }
        }

        for (g in 1:(gMax + 1)) {
            simulatedSingleEventsPerStage[, i, g] <- simulatedSingleEventsPerStage[, i, g] / pmax(iterations[, i], 1)
        }
        simulatedPlannedEvents[, i] <- simulatedPlannedEvents[, i] / pmax(iterations[, i], 1)
        simulatedNumberOfSubjects[, i] <- simulatedNumberOfSubjects[, i] / pmax(iterations[, i], 1)
        simulatedAnalysisTime[, i] <- simulatedAnalysisTime[, i] / pmax(iterations[, i], 1)
        simulatedNumberOfActiveArms[, i] <- simulatedNumberOfActiveArms[, i] / pmax(iterations[, i], 1)

        if (kMax > 1) {
            simulatedRejections[2:kMax, i, ] <- simulatedRejections[2:kMax, i, ] -
                simulatedRejections[1:(kMax - 1), i, ]

            stopping <- cumsum(simulatedSuccessStopping[1:(kMax - 1), i] + simulatedFutilityStopping[, i]) /
                maxNumberOfIterations

            expectedNumberOfEvents[i] <- simulatedPlannedEvents[1, i] +
                t(1 - stopping) %*%
                (simulatedPlannedEvents[2:kMax, i] - simulatedPlannedEvents[1:(kMax - 1), i])

            expectedNumberOfSubjects[i] <- simulatedNumberOfSubjects[1, i] +
                t(1 - stopping) %*%
                (simulatedNumberOfSubjects[2:kMax, i] - simulatedNumberOfSubjects[1:(kMax - 1), i])

            expectedStudyDuration[i] <- simulatedAnalysisTime[1, i] +
                t(1 - stopping) %*%
                (simulatedAnalysisTime[2:kMax, i] - simulatedAnalysisTime[1:(kMax - 1), i])
        } else {
            expectedNumberOfEvents[i] <- simulatedPlannedEvents[1, i]
            expectedNumberOfSubjects[i] <- simulatedNumberOfSubjects[1, i]
            expectedStudyDuration[i] <- simulatedAnalysisTime[1, i]
        }
    }

    simulatedConditionalPower[1, ] <- NA_real_
    if (kMax > 1) {
        for (k in 2:kMax) {
            simulatedConditionalPower[k, ] <- simulatedConditionalPower[k, ] /
                (iterations[k, ] + simulatedNumberEventsNotAchieved[k, ])
        }
    }

    # Create data frame for detailed results
    data <- data.frame(
        iterationNumber = dataIterationNumber,
        stageNumber = dataStageNumber,
        armNumber = dataArmNumber,
        omegaMax = dataAlternative,
        effect = dataEffect,
        analysisTime = dataAnalysisTime,
        numberOfSubjects = dataNumberOfSubjects,
        numberOfEvents = dataNumberOfEvents,
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

    list(
        simulatedNumberEventsNotAchieved = simulatedNumberEventsNotAchieved,
        simulatedAnalysisTime = simulatedAnalysisTime,
        simulatedNumberOfSubjects = simulatedNumberOfSubjects,
        simulatedSelections = simulatedSelections,
        simulatedRejections = simulatedRejections,
        simulatedNumberOfActiveArms = simulatedNumberOfActiveArms,
        simulatedSingleEventsPerStage = simulatedSingleEventsPerStage,
        simulatedPlannedEvents = simulatedPlannedEvents,
        simulatedSuccessStopping = simulatedSuccessStopping,
        simulatedFutilityStopping = simulatedFutilityStopping,
        simulatedConditionalPower = simulatedConditionalPower,
        simulatedRejectAtLeastOne = simulatedRejectAtLeastOne,
        expectedNumberOfEvents = expectedNumberOfEvents,
        expectedNumberOfSubjects = expectedNumberOfSubjects,
        expectedStudyDuration = expectedStudyDuration,
        iterations = iterations,
        data = data
    )
}
