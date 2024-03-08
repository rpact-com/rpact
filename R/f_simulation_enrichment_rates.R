## |
## |  *Simulation of enrichment design with binary data*
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
## |  File version: $Revision: 7679 $
## |  Last changed: $Date: 2024-03-04 15:00:35 +0100 (Mo, 04 Mrz 2024) $
## |  Last changed by: $Author: wassmer $
## |

#' @include f_simulation_enrichment.R
NULL

.getSimulationRatesEnrichmentStageSubjects <- function(...,
        stage,
        directionUpper,
        conditionalPower,
        conditionalCriticalValue,
        plannedSubjects,
        allocationRatioPlanned,
        selectedPopulations,
        piTreatmentH1,
        piControlH1,
        overallRatesTreatment,
        overallRatesControl,
        minNumberOfSubjectsPerStage,
        maxNumberOfSubjectsPerStage) {
    stage <- stage - 1 # to be consistent with non-enrichment situation
    gMax <- nrow(overallRatesTreatment)

    if (!is.na(conditionalPower)) {
        if (any(selectedPopulations[1:gMax, stage + 1], na.rm = TRUE)) {
            if (is.na(piControlH1)) {
                pi2H1 <- overallRatesControl[selectedPopulations[1:gMax, stage + 1], stage]
            } else {
                pi2H1 <- piControlH1
            }

            if (is.na(piTreatmentH1)) {
                pi1H1 <- overallRatesTreatment[selectedPopulations[1:gMax, stage + 1], stage]
            } else {
                pi1H1 <- piTreatmentH1
            }

            pim <- (allocationRatioPlanned[stage] * pi1H1 + pi2H1) / (1 + allocationRatioPlanned[stage])

            if (conditionalCriticalValue[stage] > 8) {
                newSubjects <- maxNumberOfSubjectsPerStage[stage + 1]
            } else {
                newSubjects <- (1 + 1 / allocationRatioPlanned[stage]) * (max(0, conditionalCriticalValue[stage] *
                    sqrt(pim * (1 - pim) * (1 + allocationRatioPlanned[stage])) +
                    .getQNorm(conditionalPower) * sqrt(pi1H1 * (1 - pi1H1) +
                        pi2H1 * (1 - pi2H1) * allocationRatioPlanned[stage]), na.rm = TRUE))^2 /
                    (max(1e-07, (2 * directionUpper - 1) * (pi1H1 - pi2H1), na.rm = TRUE))^2

                newSubjects <- min(
                    max(minNumberOfSubjectsPerStage[stage + 1], newSubjects),
                    maxNumberOfSubjectsPerStage[stage + 1]
                )
            }
        } else {
            newSubjects <- 0
        }
    } else {
        newSubjects <- plannedSubjects[stage + 1] - plannedSubjects[stage]
    }
    return(newSubjects)
}

.getSimulatedStageRatesEnrichment <- function(...,
        design,
        subsets,
        prevalences,
        directionUpper,
        piTreatments,
        piControls,
        stratifiedAnalysis,
        plannedSubjects,
        typeOfSelection,
        effectMeasure,
        adaptations,
        epsilonValue,
        rValue,
        threshold,
        allocationRatioPlanned,
        minNumberOfSubjectsPerStage,
        maxNumberOfSubjectsPerStage,
        conditionalPower,
        piTreatmentH1,
        piControlH1,
        calcSubjectsFunction,
        calcSubjectsFunctionIsUserDefined,
        selectPopulationsFunction) {
    kMax <- length(plannedSubjects)
    pMax <- length(piTreatments)
    gMax <- log(length(piTreatments), 2) + 1

    subjectsPerStage <- matrix(NA_real_, nrow = pMax, ncol = kMax)
    simEventsTreatment <- matrix(NA_real_, nrow = pMax, ncol = kMax)
    simEventsControl <- matrix(NA_real_, nrow = pMax, ncol = kMax)
    populationSubjectsPerStage <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    testStatistics <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    overallTestStatistics <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    separatePValues <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    conditionalCriticalValue <- rep(NA_real_, kMax - 1)
    conditionalPowerPerStage <- rep(NA_real_, kMax)
    selectedPopulations <- matrix(FALSE, nrow = gMax, ncol = kMax)
    selectedSubsets <- matrix(FALSE, nrow = pMax, ncol = kMax)
    selectedPopulations[, 1] <- TRUE
    selectedSubsets[, 1] <- TRUE
    adjustedPValues <- rep(NA_real_, kMax)
    overallRatesTreatment <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    overallRatesControl <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    overallEffectSizes <- matrix(NA_real_, nrow = gMax, ncol = kMax)

    if (.isTrialDesignFisher(design)) {
        weights <- .getWeightsFisher(design)
    } else if (.isTrialDesignInverseNormal(design)) {
        weights <- .getWeightsInverseNormal(design)
    }

    for (k in 1:kMax) {
        const <- allocationRatioPlanned[k]

        selectedSubsets[, k] <- .createSelectedSubsets(k, selectedPopulations)

        if (k == 1) {
            # subjectsPerStage[, k] <- stats::rmultinom(1, plannedSubjects[k], prevalences)
            subjectsPerStage[, k] <- plannedSubjects[k] * prevalences
        } else {
            prevSelected <- prevalences / sum(prevalences[selectedSubsets[, k]])
            prevSelected[!selectedSubsets[, k]] <- 0
            if (sum(prevSelected, na.rm = TRUE) > 0) {
                # subjectsPerStage[, k] <- stats::rmultinom(1, plannedSubjects[k] - plannedSubjects[k - 1], prevSelected)
                subjectsPerStage[, k] <- (plannedSubjects[k] - plannedSubjects[k - 1]) * prevSelected
            } else {
                break
            }
        }

        selsubs <- !is.na(subjectsPerStage[, k]) & subjectsPerStage[, k] > 0

        if (any(round(subjectsPerStage[selsubs, k] * const / (1 + const)) < 1) ||
                any(round(subjectsPerStage[selsubs, k] / (1 + const)) < 1)) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "at least one sample size specification too small to create simulation results, ",
                "e.g., due to small prevalences of subsets"
            )
        }

        simEventsTreatment[selsubs, k] <- stats::rbinom(
            rep(1, sum(selsubs)),
            round(subjectsPerStage[selsubs, k] * const / (1 + const)), piTreatments[selsubs]
        )

        simEventsControl[selsubs, k] <- stats::rbinom(
            rep(1, sum(selsubs)),
            round(subjectsPerStage[selsubs, k] / (1 + const)), piControls[selsubs]
        )


        if (gMax == 1) {
            rm <- (simEventsControl[1, k] + simEventsTreatment[1, k]) / subjectsPerStage[1, k]
            if (rm <= 0 || rm >= 1) {
                testStatistics[1, k] <- 0
            } else {
                testStatistics[1, k] <- (2 * directionUpper - 1) *
                    (simEventsTreatment[1, k] * (1 + const) / const - simEventsControl[1, k] *
                        (1 + const)) / subjectsPerStage[1, k] /
                    sqrt(rm * (1 - rm)) * sqrt(subjectsPerStage[1, k] * const / (1 + const)^2)
            }
            populationSubjectsPerStage[1, k] <- subjectsPerStage[1, k]
            overallRatesTreatment[1, k] <- sum(simEventsTreatment[1, 1:k]) /
                round(const / (1 + const) * sum(subjectsPerStage[1, 1:k]))
            overallRatesControl[1, k] <- sum(simEventsControl[1, 1:k]) /
                round(1 / (1 + const) * sum(subjectsPerStage[1, 1:k]))
            overallEffectSizes[1, k] <-
                (2 * directionUpper - 1) * (overallRatesTreatment[1, k] - overallRatesControl[1, k])
            rm <- sum(simEventsControl[1, 1:k] + simEventsTreatment[1, 1:k]) / sum(subjectsPerStage[1, 1:k])
            if (rm == 0 || rm == 1) {
                overallTestStatistics[1, k] <- 0
            } else {
                overallTestStatistics[1, k] <- overallEffectSizes[1, k] /
                    sqrt(rm * (1 - rm)) * sqrt(sum(subjectsPerStage[1, 1:k]) * const / (1 + const)^2)
            }
        } else if (gMax == 2) {
            # Population S1
            rm <- (simEventsControl[1, k] + simEventsTreatment[1, k]) / subjectsPerStage[1, k]
            if (!is.na(rm)) {
                if (rm <= 0 || rm >= 1) {
                    testStatistics[1, k] <- 0
                } else {
                    testStatistics[1, k] <- (2 * directionUpper - 1) *
                        (simEventsTreatment[1, k] * (1 + const) / const - simEventsControl[1, k] *
                            (1 + const)) / subjectsPerStage[1, k] /
                        sqrt(rm * (1 - rm)) * sqrt(subjectsPerStage[1, k] * const / (1 + const)^2)
                }
            }
            populationSubjectsPerStage[1, k] <- subjectsPerStage[1, k]
            overallRatesTreatment[1, k] <- sum(simEventsTreatment[1, 1:k]) /
                round(const / (1 + const) * sum(subjectsPerStage[1, 1:k]))
            overallRatesControl[1, k] <- sum(simEventsControl[1, 1:k]) /
                round(1 / (1 + const) * sum(subjectsPerStage[1, 1:k]))
            overallEffectSizes[1, k] <-
                (2 * directionUpper - 1) * (overallRatesTreatment[1, k] - overallRatesControl[1, k])
            rm <- sum(simEventsControl[1, 1:k] + simEventsTreatment[1, 1:k]) / sum(subjectsPerStage[1, 1:k])
            if (!is.na(rm)) {
                if (rm <= 0 || rm >= 1) {
                    overallTestStatistics[1, k] <- 0
                } else {
                    overallTestStatistics[1, k] <- overallEffectSizes[1, k] /
                        sqrt(rm * (1 - rm)) * sqrt(sum(subjectsPerStage[1, 1:k]) * const / (1 + const)^2)
                }
            }
            # Full population
            if (stratifiedAnalysis) {
                rm <- (simEventsControl[1:2, k] + simEventsTreatment[1:2, k]) / subjectsPerStage[1:2, k]
                rm[!is.na(rm) & (rm <= 0 | rm >= 1)] <- 0
                if (!all(is.na(rm))) {
                    if (all(na.omit(rm) == 0) || all(na.omit(rm) == 1)) {
                        testStatistics[2, k] <- 0
                    } else {
                        testStatistics[2, k] <- sqrt(const) / (1 + const) * (2 * directionUpper - 1) *
                            sum(subjectsPerStage[1:2, k] * (simEventsTreatment[1:2, k] *
                                (1 + const) / const - simEventsControl[1:2, k] * (1 + const)) /
                                subjectsPerStage[1:2, k], na.rm = TRUE) / sqrt(sum(rm * (1 - rm) *
                                subjectsPerStage[1:2, k], na.rm = TRUE))
                    }
                }
            } else {
                rm <- sum(simEventsControl[1:2, k] + simEventsTreatment[1:2, k], na.rm = TRUE) /
                    sum(subjectsPerStage[1:2, k], na.rm = TRUE)
                if (!is.na(rm)) {
                    if (rm <= 0 || rm >= 1) {
                        testStatistics[2, k] <- 0
                    } else {
                        testStatistics[2, k] <- (2 * directionUpper - 1) *
                            sum(simEventsTreatment[1:2, k] * (1 + const) / const - simEventsControl[1:2, k] *
                                (1 + const), na.rm = TRUE) /
                            sum(subjectsPerStage[1:2, k], na.rm = TRUE) /
                            sqrt(rm * (1 - rm)) * sqrt(sum(subjectsPerStage[1:2, k], na.rm = TRUE) * const / (1 + const)^2)
                    }
                }
            }
            populationSubjectsPerStage[2, k] <- sum(subjectsPerStage[1:2, k], na.rm = TRUE)
            overallRatesTreatment[2, k] <- sum(simEventsTreatment[1:2, 1:k], na.rm = TRUE) /
                round(const / (1 + const) * sum(subjectsPerStage[1:2, 1:k], na.rm = TRUE))
            overallRatesControl[2, k] <- sum(simEventsControl[1:2, 1:k], na.rm = TRUE) /
                round(1 / (1 + const) * sum(subjectsPerStage[1:2, 1:k], na.rm = TRUE))
            overallEffectSizes[2, k] <-
                (2 * directionUpper - 1) * (overallRatesTreatment[2, k] - overallRatesControl[2, k])
            rm <- sum(simEventsControl[1:2, 1:k] + simEventsTreatment[1:2, 1:k], na.rm = TRUE) /
                sum(subjectsPerStage[1:2, 1:k], na.rm = TRUE)
            if (!is.na(rm)) {
                if (rm <= 0 || rm >= 1) {
                    overallTestStatistics[2, k] <- 0
                } else {
                    overallTestStatistics[2, k] <- overallEffectSizes[2, k] /
                        sqrt(rm * (1 - rm)) * sqrt(sum(subjectsPerStage[1:2, 1:k], na.rm = TRUE) * const / (1 + const)^2)
                }
            }
        } else if (gMax == 3) {
            # Population S1
            if (stratifiedAnalysis) {
                rm <- (simEventsControl[c(1, 3), k] + simEventsTreatment[c(1, 3), k]) / subjectsPerStage[c(1, 3), k]
                rm[!is.na(rm) & (rm <= 0 | rm >= 1)] <- 0
                if (!all(is.na(rm))) {
                    if (all(na.omit(rm) == 0) || all(na.omit(rm) == 1)) {
                        testStatistics[1, k] <- 0
                    } else {
                        testStatistics[1, k] <- sqrt(const) / (1 + const) * (2 * directionUpper - 1) *
                            sum(subjectsPerStage[c(1, 3), k] * (simEventsTreatment[c(1, 3), k] *
                                (1 + const) / const - simEventsControl[c(1, 3), k] * (1 + const)) /
                                subjectsPerStage[c(1, 3), k], na.rm = TRUE) / sqrt(sum(rm * (1 - rm) *
                                subjectsPerStage[c(1, 3), k], na.rm = TRUE))
                    }
                }
            } else {
                rm <- sum(simEventsControl[c(1, 3), k] + simEventsTreatment[c(1, 3), k], na.rm = TRUE) /
                    sum(subjectsPerStage[c(1, 3), k], na.rm = TRUE)
                if (!is.na(rm)) {
                    if (rm <= 0 || rm >= 1) {
                        testStatistics[1, k] <- 0
                    } else {
                        testStatistics[1, k] <- (2 * directionUpper - 1) *
                            sum(simEventsTreatment[c(1, 3), k] * (1 + const) / const - simEventsControl[c(1, 3), k] *
                                (1 + const), na.rm = TRUE) /
                            sum(subjectsPerStage[c(1, 3), k], na.rm = TRUE) /
                            sqrt(rm * (1 - rm)) * sqrt(sum(subjectsPerStage[c(1, 3), k], na.rm = TRUE) * const / (1 + const)^2)
                    }
                }
            }
            populationSubjectsPerStage[1, k] <- sum(subjectsPerStage[c(1, 3), k], na.rm = TRUE)
            overallRatesTreatment[1, k] <- sum(simEventsTreatment[c(1, 3), 1:k], na.rm = TRUE) /
                round(const / (1 + const) * sum(subjectsPerStage[c(1, 3), 1:k], na.rm = TRUE))
            overallRatesControl[1, k] <- sum(simEventsControl[c(1, 3), 1:k], na.rm = TRUE) /
                round(1 / (1 + const) * sum(subjectsPerStage[c(1, 3), 1:k], na.rm = TRUE))
            overallEffectSizes[1, k] <-
                (2 * directionUpper - 1) * (overallRatesTreatment[1, k] - overallRatesControl[1, k])
            rm <- sum(simEventsControl[c(1, 3), 1:k] + simEventsTreatment[c(1, 3), 1:k], na.rm = TRUE) /
                sum(subjectsPerStage[c(1, 3), 1:k], na.rm = TRUE)
            if (!is.na(rm)) {
                if (rm <= 0 || rm >= 1) {
                    overallTestStatistics[1, k] <- 0
                } else {
                    overallTestStatistics[1, k] <- overallEffectSizes[1, k] /
                        sqrt(rm * (1 - rm)) * sqrt(sum(subjectsPerStage[c(1, 3), 1:k], na.rm = TRUE) * const / (1 + const)^2)
                }
            }
            # Population S2
            if (stratifiedAnalysis) {
                rm <- (simEventsControl[c(2, 3), k] + simEventsTreatment[c(2, 3), k]) / subjectsPerStage[c(2, 3), k]
                rm[!is.na(rm) & (rm <= 0 | rm >= 1)] <- 0
                if (!all(is.na(rm))) {
                    if (all(na.omit(rm) == 0) || all(na.omit(rm) == 1)) {
                        testStatistics[2, k] <- 0
                    } else {
                        testStatistics[2, k] <- sqrt(const) / (1 + const) * (2 * directionUpper - 1) *
                            sum(subjectsPerStage[c(2, 3), k] * (simEventsTreatment[c(2, 3), k] *
                                (1 + const) / const - simEventsControl[c(2, 3), k] * (1 + const)) /
                                subjectsPerStage[c(2, 3), k], na.rm = TRUE) / sqrt(sum(rm * (1 - rm) *
                                subjectsPerStage[c(2, 3), k], na.rm = TRUE))
                    }
                }
            } else {
                rm <- sum(simEventsControl[c(2, 3), k] + simEventsTreatment[c(2, 3), k], na.rm = TRUE) /
                    sum(subjectsPerStage[c(2, 3), k], na.rm = TRUE)
                if (!is.na(rm)) {
                    if (rm <= 0 || rm >= 1) {
                        testStatistics[2, k] <- 0
                    } else {
                        testStatistics[2, k] <- (2 * directionUpper - 1) *
                            sum(simEventsTreatment[c(2, 3), k] * (1 + const) / const - simEventsControl[c(2, 3), k] *
                                (1 + const), na.rm = TRUE) /
                            sum(subjectsPerStage[c(2, 3), k], na.rm = TRUE) /
                            sqrt(rm * (1 - rm)) * sqrt(sum(subjectsPerStage[c(2, 3), k], na.rm = TRUE) * const / (1 + const)^2)
                    }
                }
            }
            populationSubjectsPerStage[2, k] <- sum(subjectsPerStage[c(2, 3), k], na.rm = TRUE)
            overallRatesTreatment[2, k] <- sum(simEventsTreatment[c(2, 3), 1:k], na.rm = TRUE) /
                round(const / (1 + const) * sum(subjectsPerStage[c(2, 3), 1:k], na.rm = TRUE))
            overallRatesControl[2, k] <- sum(simEventsControl[c(2, 3), 1:k], na.rm = TRUE) /
                round(1 / (1 + const) * sum(subjectsPerStage[c(2, 3), 1:k], na.rm = TRUE))
            overallEffectSizes[2, k] <-
                (2 * directionUpper - 1) * (overallRatesTreatment[2, k] - overallRatesControl[2, k])
            rm <- sum(simEventsControl[c(2, 3), 1:k] + simEventsTreatment[c(2, 3), 1:k], na.rm = TRUE) /
                sum(subjectsPerStage[c(2, 3), 1:k], na.rm = TRUE)
            if (!is.na(rm)) {
                if (rm <= 0 || rm >= 1) {
                    overallTestStatistics[2, k] <- 0
                } else {
                    overallTestStatistics[2, k] <- overallEffectSizes[2, k] /
                        sqrt(rm * (1 - rm)) * sqrt(sum(subjectsPerStage[c(2, 3), 1:k], na.rm = TRUE) * const / (1 + const)^2)
                }
            }
            # Full population
            if (stratifiedAnalysis) {
                rm <- (simEventsControl[1:4, k] + simEventsTreatment[1:4, k]) / subjectsPerStage[1:4, k]
                rm[!is.na(rm) & (rm <= 0 | rm >= 1)] <- 0
                if (!all(is.na(rm))) {
                    if (all(na.omit(rm) == 0) || all(na.omit(rm) == 1)) {
                        testStatistics[3, k] <- 0
                    } else {
                        testStatistics[3, k] <- sqrt(const) / (1 + const) * (2 * directionUpper - 1) *
                            sum(subjectsPerStage[1:4, k] * (simEventsTreatment[1:4, k] *
                                (1 + const) / const - simEventsControl[1:4, k] * (1 + const)) /
                                subjectsPerStage[1:4, k], na.rm = TRUE) / sqrt(sum(rm * (1 - rm) *
                                subjectsPerStage[1:4, k], na.rm = TRUE))
                    }
                }
            } else {
                rm <- sum(simEventsControl[1:4, k] + simEventsTreatment[1:4, k], na.rm = TRUE) /
                    sum(subjectsPerStage[1:4, k], na.rm = TRUE)
                if (!is.na(rm)) {
                    if (rm <= 0 || rm >= 1) {
                        testStatistics[3, k] <- 0
                    } else {
                        testStatistics[3, k] <- (2 * directionUpper - 1) *
                            sum(simEventsTreatment[1:4, k] * (1 + const) / const - simEventsControl[1:4, k] *
                                (1 + const), na.rm = TRUE) /
                            sum(subjectsPerStage[1:4, k], na.rm = TRUE) /
                            sqrt(rm * (1 - rm)) * sqrt(sum(subjectsPerStage[1:4, k], na.rm = TRUE) * const / (1 + const)^2)
                    }
                }
            }
            populationSubjectsPerStage[3, k] <- sum(subjectsPerStage[1:4, k], na.rm = TRUE)
            overallRatesTreatment[3, k] <- sum(simEventsTreatment[1:4, 1:k], na.rm = TRUE) /
                round(const / (1 + const) * sum(subjectsPerStage[1:4, 1:k], na.rm = TRUE))
            overallRatesControl[3, k] <- sum(simEventsControl[1:4, 1:k], na.rm = TRUE) /
                round(1 / (1 + const) * sum(subjectsPerStage[1:4, 1:k], na.rm = TRUE))
            overallEffectSizes[3, k] <-
                (2 * directionUpper - 1) * (overallRatesTreatment[3, k] - overallRatesControl[3, k])
            rm <- sum(simEventsControl[1:4, 1:k] + simEventsTreatment[1:4, 1:k], na.rm = TRUE) /
                sum(subjectsPerStage[1:4, 1:k], na.rm = TRUE)
            if (!is.na(rm)) {
                if (rm <= 0 || rm >= 1) {
                    overallTestStatistics[3, k] <- 0
                } else {
                    overallTestStatistics[3, k] <- overallEffectSizes[3, k] /
                        sqrt(rm * (1 - rm)) * sqrt(sum(subjectsPerStage[1:4, 1:k], na.rm = TRUE) * const / (1 + const)^2)
                }
            }
        } else if (gMax == 4) {
            # Population S1
            if (stratifiedAnalysis) {
                rm <- (simEventsControl[c(1, 4, 5, 7), k] + simEventsTreatment[c(1, 4, 5, 7), k]) / subjectsPerStage[c(1, 4, 5, 7), k]
                rm[!is.na(rm) & (rm <= 0 | rm >= 1)] <- 0
                if (!all(is.na(rm))) {
                    if (all(na.omit(rm) == 0) || all(na.omit(rm) == 1)) {
                        testStatistics[1, k] <- 0
                    } else {
                        testStatistics[1, k] <- sqrt(const) / (1 + const) * (2 * directionUpper - 1) *
                            sum(subjectsPerStage[c(1, 4, 5, 7), k] * (simEventsTreatment[c(1, 4, 5, 7), k] *
                                (1 + const) / const - simEventsControl[c(1, 4, 5, 7), k] * (1 + const)) /
                                subjectsPerStage[c(1, 4, 5, 7), k], na.rm = TRUE) / sqrt(sum(rm * (1 - rm) *
                                subjectsPerStage[c(1, 4, 5, 7), k], na.rm = TRUE))
                    }
                }
            } else {
                rm <- sum(simEventsControl[c(1, 4, 5, 7), k] + simEventsTreatment[c(1, 4, 5, 7), k], na.rm = TRUE) /
                    sum(subjectsPerStage[c(1, 4, 5, 7), k], na.rm = TRUE)
                if (!is.na(rm)) {
                    if (rm <= 0 || rm >= 1) {
                        testStatistics[1, k] <- 0
                    } else {
                        testStatistics[1, k] <- (2 * directionUpper - 1) *
                            sum(simEventsTreatment[c(1, 4, 5, 7), k] * (1 + const) / const -
                                simEventsControl[c(1, 4, 5, 7), k] * (1 + const), na.rm = TRUE) /
                            sum(subjectsPerStage[c(1, 4, 5, 7), k], na.rm = TRUE) /
                            sqrt(rm * (1 - rm)) * sqrt(sum(subjectsPerStage[c(1, 4, 5, 7), k], na.rm = TRUE) * const / (1 + const)^2)
                    }
                }
            }
            populationSubjectsPerStage[1, k] <- sum(subjectsPerStage[c(1, 4, 5, 7), k], na.rm = TRUE)
            overallRatesTreatment[1, k] <- sum(simEventsTreatment[c(1, 4, 5, 7), 1:k], na.rm = TRUE) /
                round(const / (1 + const) * sum(subjectsPerStage[c(1, 4, 5, 7), 1:k], na.rm = TRUE))
            overallRatesControl[1, k] <- sum(simEventsControl[c(1, 4, 5, 7), 1:k], na.rm = TRUE) /
                round(1 / (1 + const) * sum(subjectsPerStage[c(1, 4, 5, 7), 1:k], na.rm = TRUE))
            overallEffectSizes[1, k] <-
                (2 * directionUpper - 1) * (overallRatesTreatment[1, k] - overallRatesControl[1, k])
            rm <- sum(simEventsControl[c(1, 4, 5, 7), 1:k] + simEventsTreatment[c(1, 4, 5, 7), 1:k], na.rm = TRUE) /
                sum(subjectsPerStage[c(1, 4, 5, 7), 1:k], na.rm = TRUE)
            if (!is.na(rm)) {
                if (rm <= 0 || rm >= 1) {
                    overallTestStatistics[1, k] <- 0
                } else {
                    overallTestStatistics[1, k] <- overallEffectSizes[1, k] /
                        sqrt(rm * (1 - rm)) * sqrt(sum(subjectsPerStage[c(1, 4, 5, 7), 1:k], na.rm = TRUE) * const / (1 + const)^2)
                }
            }
            # Population S2
            if (stratifiedAnalysis) {
                rm <- (simEventsControl[c(2, 4, 6, 7), k] + simEventsTreatment[c(2, 4, 6, 7), k]) / subjectsPerStage[c(2, 4, 6, 7), k]
                rm[!is.na(rm) & (rm <= 0 | rm >= 1)] <- 0
                if (!all(is.na(rm))) {
                    if (all(na.omit(rm) == 0) || all(na.omit(rm) == 1)) {
                        testStatistics[2, k] <- 0
                    } else {
                        testStatistics[2, k] <- sqrt(const) / (1 + const) * (2 * directionUpper - 1) *
                            sum(subjectsPerStage[c(2, 4, 6, 7), k] * (simEventsTreatment[c(2, 4, 6, 7), k] *
                                (1 + const) / const - simEventsControl[c(2, 4, 6, 7), k] * (1 + const)) /
                                subjectsPerStage[c(2, 4, 6, 7), k], na.rm = TRUE) / sqrt(sum(rm * (1 - rm) *
                                subjectsPerStage[c(2, 4, 6, 7), k], na.rm = TRUE))
                    }
                }
            } else {
                rm <- sum(simEventsControl[c(2, 4, 6, 7), k] + simEventsTreatment[c(2, 4, 6, 7), k], na.rm = TRUE) /
                    sum(subjectsPerStage[c(2, 4, 6, 7), k], na.rm = TRUE)
                if (!is.na(rm)) {
                    if (rm <= 0 || rm >= 1) {
                        testStatistics[2, k] <- 0
                    } else {
                        testStatistics[2, k] <- (2 * directionUpper - 1) *
                            sum(simEventsTreatment[c(2, 4, 6, 7), k] * (1 + const) / const - simEventsControl[c(2, 4, 6, 7), k] *
                                (1 + const), na.rm = TRUE) /
                            sum(subjectsPerStage[c(2, 4, 6, 7), k], na.rm = TRUE) / sqrt(rm * (1 - rm)) *
                            sqrt(sum(subjectsPerStage[c(2, 4, 6, 7), k], na.rm = TRUE) * const / (1 + const)^2)
                    }
                }
            }
            populationSubjectsPerStage[2, k] <- sum(subjectsPerStage[c(2, 4, 6, 7), k], na.rm = TRUE)
            overallRatesTreatment[2, k] <- sum(simEventsTreatment[c(2, 4, 6, 7), 1:k], na.rm = TRUE) /
                round(const / (1 + const) * sum(subjectsPerStage[c(2, 4, 6, 7), 1:k], na.rm = TRUE))
            overallRatesControl[2, k] <- sum(simEventsControl[c(2, 4, 6, 7), 1:k], na.rm = TRUE) /
                round(1 / (1 + const) * sum(subjectsPerStage[c(2, 4, 6, 7), 1:k], na.rm = TRUE))
            overallEffectSizes[2, k] <-
                (2 * directionUpper - 1) * (overallRatesTreatment[2, k] - overallRatesControl[2, k])
            rm <- sum(simEventsControl[c(2, 4, 6, 7), 1:k] + simEventsTreatment[c(2, 4, 6, 7), 1:k], na.rm = TRUE) /
                sum(subjectsPerStage[c(2, 4, 6, 7), 1:k], na.rm = TRUE)
            if (!is.na(rm)) {
                if (rm <= 0 || rm >= 1) {
                    overallTestStatistics[2, k] <- 0
                } else {
                    overallTestStatistics[2, k] <- overallEffectSizes[2, k] /
                        sqrt(rm * (1 - rm)) * sqrt(sum(subjectsPerStage[c(2, 4, 6, 7), 1:k], na.rm = TRUE) * const / (1 + const)^2)
                }
            }
            # Population S3
            if (stratifiedAnalysis) {
                rm <- (simEventsControl[c(3, 5, 6, 7), k] + simEventsTreatment[c(3, 5, 6, 7), k]) / subjectsPerStage[c(3, 5, 6, 7), k]
                rm[!is.na(rm) & (rm <= 0 | rm >= 1)] <- 0
                if (!all(is.na(rm))) {
                    if (all(na.omit(rm) == 0) || all(na.omit(rm) == 1)) {
                        testStatistics[3, k] <- 0
                    } else {
                        testStatistics[3, k] <- sqrt(const) / (1 + const) * (2 * directionUpper - 1) *
                            sum(subjectsPerStage[c(3, 5, 6, 7), k] * (simEventsTreatment[c(3, 5, 6, 7), k] *
                                (1 + const) / const - simEventsControl[c(3, 5, 6, 7), k] * (1 + const)) /
                                subjectsPerStage[c(3, 5, 6, 7), k], na.rm = TRUE) / sqrt(sum(rm * (1 - rm) *
                                subjectsPerStage[c(3, 5, 6, 7), k], na.rm = TRUE))
                    }
                }
            } else {
                rm <- sum(simEventsControl[c(3, 5, 6, 7), k] + simEventsTreatment[c(3, 5, 6, 7), k], na.rm = TRUE) /
                    sum(subjectsPerStage[c(3, 5, 6, 7), k], na.rm = TRUE)
                if (!is.na(rm)) {
                    if (rm <= 0 || rm >= 1) {
                        testStatistics[3, k] <- 0
                    } else {
                        testStatistics[3, k] <- (2 * directionUpper - 1) *
                            sum(simEventsTreatment[c(3, 5, 6, 7), k] * (1 + const) / const -
                                simEventsControl[c(3, 5, 6, 7), k] * (1 + const), na.rm = TRUE) /
                            sum(subjectsPerStage[c(3, 5, 6, 7), k], na.rm = TRUE) / sqrt(rm * (1 - rm)) *
                            sqrt(sum(subjectsPerStage[c(3, 5, 6, 7), k], na.rm = TRUE) * const / (1 + const)^2)
                    }
                }
            }
            populationSubjectsPerStage[3, k] <- sum(subjectsPerStage[c(3, 5, 6, 7), k], na.rm = TRUE)
            overallRatesTreatment[3, k] <- sum(simEventsTreatment[c(3, 5, 6, 7), 1:k], na.rm = TRUE) /
                round(const / (1 + const) * sum(subjectsPerStage[c(3, 5, 6, 7), 1:k], na.rm = TRUE))
            overallRatesControl[3, k] <- sum(simEventsControl[c(3, 5, 6, 7), 1:k], na.rm = TRUE) /
                round(1 / (1 + const) * sum(subjectsPerStage[c(3, 5, 6, 7), 1:k], na.rm = TRUE))
            overallEffectSizes[3, k] <-
                (2 * directionUpper - 1) * (overallRatesTreatment[3, k] - overallRatesControl[3, k])
            rm <- sum(simEventsControl[c(3, 5, 6, 7), 1:k] + simEventsTreatment[c(3, 5, 6, 7), 1:k], na.rm = TRUE) /
                sum(subjectsPerStage[c(3, 5, 6, 7), 1:k], na.rm = TRUE)
            if (!is.na(rm)) {
                if (rm <= 0 || rm >= 1) {
                    overallTestStatistics[3, k] <- 0
                } else {
                    overallTestStatistics[3, k] <- overallEffectSizes[3, k] /
                        sqrt(rm * (1 - rm)) * sqrt(sum(subjectsPerStage[c(3, 5, 6, 7), 1:k], na.rm = TRUE) * const / (1 + const)^2)
                }
            }
            # Full population
            if (stratifiedAnalysis) {
                rm <- (simEventsControl[1:8, k] + simEventsTreatment[1:8, k]) / subjectsPerStage[1:8, k]
                rm[!is.na(rm) & (rm <= 0 | rm >= 1)] <- 0
                if (!all(is.na(rm))) {
                    if (all(na.omit(rm) == 0) || all(na.omit(rm) == 1)) {
                        testStatistics[4, k] <- 0
                    } else {
                        testStatistics[4, k] <- sqrt(const) / (1 + const) * (2 * directionUpper - 1) *
                            sum(subjectsPerStage[1:8, k] * (simEventsTreatment[1:8, k] *
                                (1 + const) / const - simEventsControl[1:8, k] * (1 + const)) /
                                subjectsPerStage[1:8, k], na.rm = TRUE) / sqrt(sum(rm * (1 - rm) *
                                subjectsPerStage[1:8, k], na.rm = TRUE))
                    }
                }
            } else {
                rm <- sum(simEventsControl[1:8, k] + simEventsTreatment[1:8, k], na.rm = TRUE) /
                    sum(subjectsPerStage[1:8, k], na.rm = TRUE)
                if (!is.na(rm)) {
                    if (rm <= 0 || rm >= 1) {
                        testStatistics[4, k] <- 0
                    } else {
                        testStatistics[4, k] <- (2 * directionUpper - 1) *
                            sum(simEventsTreatment[1:8, k] * (1 + const) / const - simEventsControl[1:8, k] *
                                (1 + const), na.rm = TRUE) /
                            sum(subjectsPerStage[1:8, k], na.rm = TRUE) / sqrt(rm * (1 - rm)) *
                            sqrt(sum(subjectsPerStage[1:8, k], na.rm = TRUE) * const / (1 + const)^2)
                    }
                }
            }
            populationSubjectsPerStage[4, k] <- sum(subjectsPerStage[1:8, k], na.rm = TRUE)
            overallRatesTreatment[4, k] <- sum(simEventsTreatment[1:8, 1:k], na.rm = TRUE) /
                round(const / (1 + const) * sum(subjectsPerStage[1:8, 1:k], na.rm = TRUE))
            overallRatesControl[4, k] <- sum(simEventsControl[1:8, 1:k], na.rm = TRUE) /
                round(1 / (1 + const) * sum(subjectsPerStage[1:8, 1:k], na.rm = TRUE))
            overallEffectSizes[4, k] <-
                (2 * directionUpper - 1) * (overallRatesTreatment[4, k] - overallRatesControl[4, k])
            rm <- sum(simEventsControl[1:8, 1:k] + simEventsTreatment[1:8, 1:k], na.rm = TRUE) /
                sum(subjectsPerStage[1:8, 1:k], na.rm = TRUE)
            if (!is.na(rm)) {
                if (rm <= 0 || rm >= 1) {
                    overallTestStatistics[4, k] <- 0
                } else {
                    overallTestStatistics[4, k] <- overallEffectSizes[4, k] /
                        sqrt(rm * (1 - rm)) * sqrt(sum(subjectsPerStage[1:8, 1:k], na.rm = TRUE) * const / (1 + const)^2)
                }
            }
        }

        testStatistics[!selectedPopulations[, k], k] <- NA_real_
        overallEffectSizes[!selectedPopulations[, k], k] <- NA_real_
        overallTestStatistics[!selectedPopulations[, k], k] <- NA_real_

        separatePValues[, k] <- 1 - stats::pnorm(testStatistics[, k])

        if (k < kMax) {
            if (colSums(selectedPopulations)[k] == 0) {
                break
            }

            # Bonferroni adjustment
            adjustedPValues[k] <- min(min(separatePValues[, k], na.rm = TRUE) *
                colSums(selectedPopulations)[k], 1 - 1e-12)

            # conditional critical value to reject the null hypotheses at the next stage of the trial
            if (.isTrialDesignFisher(design)) {
                conditionalCriticalValue[k] <- .getOneMinusQNorm(min((design$criticalValues[k + 1] /
                    prod(adjustedPValues[1:k]^weights[1:k]))^(1 / weights[k + 1]), 1 - 1e-07))
            } else {
                if (design$criticalValues[k + 1] >= 6) {
                    conditionalCriticalValue[k] <- Inf
                } else {
                    conditionalCriticalValue[k] <- (design$criticalValues[k + 1] * sqrt(design$informationRates[k + 1]) -
                        .getOneMinusQNorm(adjustedPValues[1:k]) %*% weights[1:k]) /
                        sqrt(design$informationRates[k + 1] - design$informationRates[k])
                }
            }

            if (adaptations[k]) {
                if (effectMeasure == "testStatistic") {
                    selectedPopulations[, k + 1] <- (selectedPopulations[, k] &
                        .selectPopulations(
                            k, overallTestStatistics[, k] + runif(gMax, -1e-05, 1e-05),
                            typeOfSelection, epsilonValue, rValue, threshold, selectPopulationsFunction
                        ))
                } else if (effectMeasure == "effectEstimate") {
                    selectedPopulations[, k + 1] <- (selectedPopulations[, k] &
                        .selectPopulations(
                            k, overallEffectSizes[, k] + runif(gMax, -1e-05, 1e-05),
                            typeOfSelection, epsilonValue, rValue, threshold, selectPopulationsFunction
                        ))
                }

                newSubjects <- calcSubjectsFunction(
                    stage = k + 1, # to be consistent with non-enrichment situation, cf. line 40
                    directionUpper = directionUpper,
                    conditionalPower = conditionalPower,
                    conditionalCriticalValue = conditionalCriticalValue,
                    plannedSubjects = plannedSubjects,
                    allocationRatioPlanned = allocationRatioPlanned,
                    selectedPopulations = selectedPopulations,
                    piTreatmentH1 = piTreatmentH1,
                    piControlH1 = piControlH1,
                    overallRatesTreatment = overallRatesTreatment,
                    overallRatesControl = overallRatesControl,
                    minNumberOfSubjectsPerStage = minNumberOfSubjectsPerStage,
                    maxNumberOfSubjectsPerStage = maxNumberOfSubjectsPerStage
                )

                if (is.null(newSubjects) || length(newSubjects) != 1 || !is.numeric(newSubjects) || is.na(newSubjects)) {
                    stop(
                        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                        "'calcSubjectsFunction' returned an illegal or undefined result (", newSubjects, "); ",
                        "the output must be a single numeric value"
                    )
                }

                if (!is.na(conditionalPower) || calcSubjectsFunctionIsUserDefined) {
                    plannedSubjects[(k + 1):kMax] <- plannedSubjects[k] + cumsum(rep(newSubjects, kMax - k))
                }
            } else {
                selectedPopulations[, k + 1] <- selectedPopulations[, k]
            }

            if (is.na(piControlH1)) {
                pi2H1 <- overallRatesControl[, k]
            } else {
                pi2H1 <- piControlH1
            }

            if (is.na(piTreatmentH1)) {
                pi1H1 <- overallRatesTreatment[, k]
            } else {
                pi1H1 <- piTreatmentH1
            }

            pim <- (allocationRatioPlanned[k] * pi1H1 + pi2H1) / (1 + allocationRatioPlanned[k])

            if (any(pi1H1 * (1 - pi1H1) + pi2H1 * (1 - pi2H1) == 0)) {
                thetaStandardized <- 0
            } else {
                thetaStandardized <- sqrt(allocationRatioPlanned[k]) / (1 + allocationRatioPlanned[k]) * (
                    (pi1H1 - pi2H1) * sqrt(1 + allocationRatioPlanned[k]) /
                        sqrt(pi1H1 * (1 - pi1H1) + allocationRatioPlanned[k] * pi2H1 * (1 - pi2H1)) +
                        sign(pi1H1 - pi2H1) * conditionalCriticalValue[k] *
                            (1 - sqrt(pim * (1 - pim) + allocationRatioPlanned[k] * pim * (1 - pim)) /
                                sqrt(pi1H1 * (1 - pi1H1) + allocationRatioPlanned[k] * pi2H1 * (1 - pi2H1))) *
                            (1 + allocationRatioPlanned[k]) / sqrt(allocationRatioPlanned[k] * (plannedSubjects[k + 1] - plannedSubjects[k]))
                )
            }

            thetaStandardized <- (2 * directionUpper - 1) * thetaStandardized

            if (any(!is.na(thetaStandardized))){
                thetaStandardized <- min(thetaStandardized, na.rm = TRUE)
                conditionalPowerPerStage[k] <- 1 - stats::pnorm(conditionalCriticalValue[k] -
                    thetaStandardized * sqrt(plannedSubjects[k + 1] - plannedSubjects[k]))
            } else {
                conditionalPowerPerStage[k] <- 0
            }
        }
    }
    return(list(
        subjectsPerStage = subjectsPerStage,
        populationSubjectsPerStage = populationSubjectsPerStage,
        allocationRatioPlanned = allocationRatioPlanned,
        overallEffectSizes = overallEffectSizes,
        testStatistics = testStatistics,
        directionUpper = directionUpper,
        overallTestStatistics = overallTestStatistics,
        overallRatesControl = overallRatesControl,
        overallRatesTreatment = overallRatesTreatment,
        separatePValues = separatePValues,
        conditionalCriticalValue = conditionalCriticalValue,
        conditionalPowerPerStage = conditionalPowerPerStage,
        selectedPopulations = selectedPopulations
    ))
}

#'
#' @title
#' Get Simulation Enrichment Rates
#'
#' @description
#' Returns the simulated power, stopping and selection probabilities, conditional power,
#' and expected sample size for testing rates in an enrichment design testing situation.
#'
#' @param piControlH1 If specified, the assumed probabilities in the control arm
#'        under which the sample size recalculation was performed
#'        and the conditional power was calculated.
#' @param piTreatmentH1 If specified, the assumed probabilities in the active arm
#'        under which the sample size recalculation was performed
#'        and the conditional power was calculated.
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
#' @inheritParams param_plannedSubjects
#' @inheritParams param_minNumberOfSubjectsPerStage
#' @inheritParams param_maxNumberOfSubjectsPerStage
#' @inheritParams param_conditionalPowerSimulation
#' @inheritParams param_maxNumberOfIterations
#' @inheritParams param_calcSubjectsFunction
#' @inheritParams param_selectPopulationsFunction
#' @inheritParams param_rValue
#' @inheritParams param_epsilonValue
#' @inheritParams param_seed
#' @inheritParams param_three_dots
#' @inheritParams param_showStatistics
#' @inheritParams param_stratifiedAnalysis
#'
#' @details
#' At given design the function simulates the power, stopping probabilities,
#' selection probabilities, and expected sample size at given number of subjects,
#' parameter configuration, and treatment arm selection rule in the enrichment situation.
#' An allocation ratio can be specified referring to the ratio of number of
#' subjects in the active treatment groups as compared to the control group.
#'
#' The definition of \code{piTreatmentH1} and/or \code{piControlH1} makes only sense if \code{kMax} > 1
#' and if \code{conditionalPower}, \code{minNumberOfSubjectsPerStage}, and
#' \code{maxNumberOfSubjectsPerStage} (or \code{calcSubjectsFunction}) are defined.
#'
#' \code{calcSubjectsFunction}\cr
#' This function returns the number of subjects at given conditional power and
#' conditional critical value for specified testing situation.
#' The function might depend on the variables
#' \code{stage},
#' \code{selectedPopulations},
#' \code{directionUpper},
#' \code{plannedSubjects},
#' \code{allocationRatioPlanned},
#' \code{minNumberOfSubjectsPerStage},
#' \code{maxNumberOfSubjectsPerStage},
#' \code{conditionalPower},
#' \code{conditionalCriticalValue},
#' \code{overallRatesTreatment},
#' \code{overallRatesControl},
#' \code{piTreatmentH1}, and
#' \code{piControlH1}.
#' The function has to contain the three-dots argument '...' (see examples).
#'
#' @template return_object_simulation_results
#' @template how_to_get_help_for_generics
#'
#' @template examples_get_simulation_enrichment_rates
#'
#' @export
#'
getSimulationEnrichmentRates <- function(design = NULL, ...,
        effectList = NULL,
        intersectionTest = c("Simes", "SpiessensDebois", "Bonferroni", "Sidak"), # C_INTERSECTION_TEST_ENRICHMENT_DEFAULT
        stratifiedAnalysis = TRUE, # C_STRATIFIED_ANALYSIS_DEFAULT,
        directionUpper = TRUE, # C_DIRECTION_UPPER_DEFAULT
        adaptations = NA,
        typeOfSelection = c("best", "rBest", "epsilon", "all", "userDefined"), # C_TYPE_OF_SELECTION_DEFAULT
        effectMeasure = c("effectEstimate", "testStatistic"), # C_EFFECT_MEASURE_DEFAULT
        successCriterion = c("all", "atLeastOne"), # C_SUCCESS_CRITERION_DEFAULT
        epsilonValue = NA_real_,
        rValue = NA_real_,
        threshold = -Inf,
        plannedSubjects = NA_real_,
        allocationRatioPlanned = NA_real_,
        minNumberOfSubjectsPerStage = NA_real_,
        maxNumberOfSubjectsPerStage = NA_real_,
        conditionalPower = NA_real_,
        piTreatmentH1 = NA_real_,
        piControlH1 = NA_real_,
        maxNumberOfIterations = 1000L, # C_MAX_SIMULATION_ITERATIONS_DEFAULT
        seed = NA_real_,
        calcSubjectsFunction = NULL,
        selectPopulationsFunction = NULL,
        showStatistics = FALSE) {
    if (is.null(design)) {
        design <- .getDefaultDesign(..., type = "simulation")
        .warnInCaseOfUnknownArguments(
            functionName = "getSimulationEnrichmentRates",
            ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
                design,
                powerCalculationEnabled = TRUE
            ), "showStatistics"), ...
        )
    } else {
        .assertIsTrialDesignInverseNormalOrFisher(design)
        .warnInCaseOfUnknownArguments(functionName = "getSimulationEnrichmentRates", ignore = "showStatistics", ...)
        .warnInCaseOfTwoSidedPowerArgument(...)
    }

    .assertIsOneSidedDesign(design, designType = "enrichment", engineType = "simulation")

    calcSubjectsFunctionIsUserDefined <- !is.null(calcSubjectsFunction)

    simulationResults <- .createSimulationResultsEnrichmentObject(
        design = design,
        effectList = effectList,
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
        plannedSubjects = plannedSubjects, # means + rates only
        allocationRatioPlanned = allocationRatioPlanned,
        minNumberOfSubjectsPerStage = minNumberOfSubjectsPerStage, # means + rates only
        maxNumberOfSubjectsPerStage = maxNumberOfSubjectsPerStage, # means + rates only
        conditionalPower = conditionalPower,
        piTreatmentH1 = piTreatmentH1, # rates only
        piControlH1 = piControlH1, # rates only
        maxNumberOfIterations = maxNumberOfIterations,
        seed = seed,
        calcSubjectsFunction = calcSubjectsFunction, # means + rates only
        selectPopulationsFunction = selectPopulationsFunction,
        showStatistics = showStatistics,
        endpoint = "rates"
    )

    design <- simulationResults$.design
    successCriterion <- simulationResults$successCriterion
    effectMeasure <- simulationResults$effectMeasure
    adaptations <- simulationResults$adaptations
    gMax <- simulationResults$populations
    kMax <- simulationResults$.design$kMax
    intersectionTest <- simulationResults$intersectionTest
    typeOfSelection <- simulationResults$typeOfSelection
    effectList <- simulationResults$effectList
    piTreatmentH1 <- simulationResults$piTreatmentH1 # rates only
    piControlH1 <- simulationResults$piControlH1 # rates only
    conditionalPower <- simulationResults$conditionalPower
    minNumberOfSubjectsPerStage <- simulationResults$minNumberOfSubjectsPerStage
    maxNumberOfSubjectsPerStage <- simulationResults$maxNumberOfSubjectsPerStage
    allocationRatioPlanned <- simulationResults$allocationRatioPlanned
    calcSubjectsFunction <- simulationResults$calcSubjectsFunction

    if (length(allocationRatioPlanned) == 1) {
        allocationRatioPlanned <- rep(allocationRatioPlanned, kMax)
    }

    indices <- .getIndicesOfClosedHypothesesSystemForSimulation(gMax = gMax)

    cols <- nrow(effectList$piTreatments)

    simulatedSelections <- array(0, dim = c(kMax, cols, gMax))
    simulatedRejections <- array(0, dim = c(kMax, cols, gMax))
    simulatedNumberOfPopulations <- matrix(0, nrow = kMax, ncol = cols)
    simulatedSubjectsPerStage <- array(0, dim = c(kMax, cols, 2^(gMax - 1)))
    simulatedSuccessStopping <- matrix(0, nrow = kMax, ncol = cols)
    simulatedFutilityStopping <- matrix(0, nrow = kMax - 1, ncol = cols)
    simulatedConditionalPower <- matrix(0, nrow = kMax, ncol = cols)
    simulatedRejectAtLeastOne <- rep(0, cols)
    expectedNumberOfSubjects <- rep(0, cols)
    iterations <- matrix(0, nrow = kMax, ncol = cols)

    len <- maxNumberOfIterations * kMax * gMax * cols

    dataIterationNumber <- rep(NA_real_, len)
    dataStageNumber <- rep(NA_real_, len)
    dataPopulationNumber <- rep(NA_real_, len)
    dataEffect <- rep(NA_real_, len)
    dataSubjectsPopulation <- rep(NA_real_, len)
    dataSubjectsActivePopulation <- rep(NA_real_, len)
    dataNumberOfSubjects <- rep(NA_real_, len)
    dataNumberOfCumulatedSubjects <- rep(NA_real_, len)
    dataRejectPerStage <- rep(NA, len)
    dataFutilityStop <- rep(NA_real_, len)
    dataSuccessStop <- rep(NA, len)
    dataFutilityStop <- rep(NA, len)
    dataTestStatistics <- rep(NA_real_, len)
    dataConditionalCriticalValue <- rep(NA_real_, len)
    dataConditionalPowerAchieved <- rep(NA_real_, len)
    dataEffectEstimate <- rep(NA_real_, len)
    dataPValuesSeparate <- rep(NA_real_, len)

    piControls <- effectList$piControls
    if (length(piControls) == 1) {
        piControls <- rep(piControls, ncol(effectList$piTreatments))
    }

    index <- 1

    for (i in 1:cols) {
        for (j in 1:maxNumberOfIterations) {
            stageResults <- .getSimulatedStageRatesEnrichment(
                design = design,
                subsets = effectList$subsets,
                prevalences = effectList$prevalences,
                piTreatments = effectList$piTreatments[i, ],
                piControls = piControls,
                directionUpper = directionUpper,
                stratifiedAnalysis = stratifiedAnalysis,
                plannedSubjects = plannedSubjects,
                typeOfSelection = typeOfSelection,
                effectMeasure = effectMeasure,
                adaptations = adaptations,
                epsilonValue = epsilonValue,
                rValue = rValue,
                threshold = threshold,
                allocationRatioPlanned = allocationRatioPlanned,
                minNumberOfSubjectsPerStage = minNumberOfSubjectsPerStage,
                maxNumberOfSubjectsPerStage = maxNumberOfSubjectsPerStage,
                conditionalPower = conditionalPower,
                piTreatmentH1 = piTreatmentH1,
                piControlH1 = piControlH1,
                calcSubjectsFunction = calcSubjectsFunction,
                calcSubjectsFunctionIsUserDefined = calcSubjectsFunctionIsUserDefined,
                selectPopulationsFunction = selectPopulationsFunction
            )

            closedTest <- .performClosedCombinationTestForSimulationEnrichment(
                stageResults = stageResults,
                design = design, indices = indices,
                intersectionTest = intersectionTest, successCriterion = successCriterion
            )

            rejectAtSomeStage <- FALSE
            rejectedPopulationsBefore <- rep(FALSE, gMax)

            for (k in 1:kMax) {
                simulatedRejections[k, i, ] <- simulatedRejections[k, i, ] +
                    (closedTest$rejected[, k] & closedTest$selectedPopulations[1:gMax, k] | rejectedPopulationsBefore)
                simulatedSelections[k, i, ] <- simulatedSelections[k, i, ] + closedTest$selectedPopulations[, k]

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

                for (p in 1:2^(gMax - 1)) {
                    if (!is.na(stageResults$subjectsPerStage[p, k])) {
                        simulatedSubjectsPerStage[k, i, p] <- simulatedSubjectsPerStage[k, i, p] +
                            stageResults$subjectsPerStage[p, k]
                    }
                }

                for (g in 1:gMax) {
                    dataIterationNumber[index] <- j
                    dataStageNumber[index] <- k
                    dataPopulationNumber[index] <- g
                    dataEffect[index] <- i
                    dataSubjectsPopulation[index] <- stageResults$populationSubjectsPerStage[g, k]
                    dataNumberOfSubjects[index] <- round(sum(stageResults$subjectsPerStage[, k], na.rm = TRUE), 1)
                    dataNumberOfCumulatedSubjects[index] <- sum(stageResults$subjectsPerStage[, 1:k], na.rm = TRUE)
                    dataRejectPerStage[index] <- closedTest$rejected[g, k]
                    dataTestStatistics[index] <- stageResults$testStatistics[g, k]
                    dataSuccessStop[index] <- closedTest$successStop[k]
                    if (k < kMax) {
                        dataFutilityStop[index] <- closedTest$futilityStop[k]
                        dataConditionalCriticalValue[index] <- stageResults$conditionalCriticalValue[k]
                        dataConditionalPowerAchieved[index + 1] <- stageResults$conditionalPowerPerStage[k]
                    }
                    dataEffectEstimate[index] <- stageResults$overallEffectSizes[g, k]
                    dataPValuesSeparate[index] <- closedTest$separatePValues[g, k]

                    index <- index + 1
                }

                if (!rejectAtSomeStage && any(closedTest$rejected[, k] &
                        closedTest$selectedPopulations[1:gMax, k] | rejectedPopulationsBefore)) {
                    simulatedRejectAtLeastOne[i] <- simulatedRejectAtLeastOne[i] + 1
                    rejectAtSomeStage <- TRUE
                }

                if ((k < kMax) && (closedTest$successStop[k] || closedTest$futilityStop[k])) {
                    # rejected hypotheses remain rejected also in case of early stopping
                    simulatedRejections[(k + 1):kMax, i, ] <- simulatedRejections[(k + 1):kMax, i, ] +
                        matrix(
                            (closedTest$rejected[, k] &
                                closedTest$selectedPopulations[1:gMax, k] | rejectedPopulationsBefore),
                            kMax - k, gMax,
                            byrow = TRUE
                        )
                    break
                }

                rejectedPopulationsBefore <- closedTest$rejected[, k] &
                    closedTest$selectedPopulations[1:gMax, k] | rejectedPopulationsBefore
            }
        }

        simulatedSubjectsPerStage[is.na(simulatedSubjectsPerStage)] <- 0

        simulatedSubjectsPerStage[, i, ] <- simulatedSubjectsPerStage[, i, ] / iterations[, i]

        if (kMax > 1) {
            simulatedRejections[2:kMax, i, ] <- simulatedRejections[2:kMax, i, ] - simulatedRejections[1:(kMax - 1), i, ]
            stopping <- cumsum(simulatedSuccessStopping[1:(kMax - 1), i] +
                simulatedFutilityStopping[, i]) / maxNumberOfIterations
            expectedNumberOfSubjects[i] <- sum(simulatedSubjectsPerStage[1, i, ] + t(1 - stopping) %*%
                simulatedSubjectsPerStage[2:kMax, i, ])
        } else {
            expectedNumberOfSubjects[i] <- sum(simulatedSubjectsPerStage[1, i, ])
        }
    }

    simulatedConditionalPower[1, ] <- NA_real_
    if (kMax > 1) {
        simulatedConditionalPower[2:kMax, ] <- as.matrix(simulatedConditionalPower[2:kMax, ] / iterations[2:kMax, ])
    }
    simulationResults$rejectAtLeastOne <- simulatedRejectAtLeastOne / maxNumberOfIterations
    simulationResults$numberOfPopulations <- simulatedNumberOfPopulations / iterations

    simulationResults$selectedPopulations <- simulatedSelections / maxNumberOfIterations
    simulationResults$rejectedPopulationsPerStage <- simulatedRejections / maxNumberOfIterations
    simulationResults$successPerStage <- simulatedSuccessStopping / maxNumberOfIterations
    simulationResults$futilityPerStage <- simulatedFutilityStopping / maxNumberOfIterations
    simulationResults$futilityStop <- base::colSums(simulatedFutilityStopping / maxNumberOfIterations)
    if (kMax > 1) {
        simulationResults$earlyStop <- simulationResults$futilityPerStage +
            simulationResults$successPerStage[1:(kMax - 1), ]
        simulationResults$conditionalPowerAchieved <- simulatedConditionalPower
    }
    simulationResults$sampleSizes <- simulatedSubjectsPerStage
    simulationResults$expectedNumberOfSubjects <- expectedNumberOfSubjects
    simulationResults$iterations <- iterations

    if (!all(is.na(simulationResults$conditionalPowerAchieved))) {
        simulationResults$.setParameterType("conditionalPowerAchieved", C_PARAM_GENERATED)
    }

    if (any(simulationResults$rejectedPopulationsPerStage < 0)) {
        stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "internal error, simulation not possible due to numerical overflow")
    }

    data <- data.frame(
        iterationNumber = dataIterationNumber,
        stageNumber = dataStageNumber,
        populationNumber = dataPopulationNumber,
        effect = dataEffect,
        numberOfSubjects = dataNumberOfSubjects,
        numberOfCumulatedSubjects = dataNumberOfCumulatedSubjects,
        subjectsPopulation = dataSubjectsPopulation,
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
