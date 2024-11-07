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
                    thetaStandardized <- log(max(min(
                        overallEffects[selectedPopulations[1:gMax, stage + 1], stage],
                        na.rm = TRUE
                    ), 1 + 1e-07))
                } else {
                    thetaStandardized <- log(min(max(
                        overallEffects[selectedPopulations[1:gMax, stage + 1], stage],
                        na.rm = TRUE
                    ), 1 - 1e-07))
                }
            } else {
                thetaStandardized <- log(min(thetaH1, 1 + ifelse(is.na(directionUpper) ||
                    isTRUE(directionUpper), 1e-07, -1e-07)))
            }

            if (conditionalCriticalValue[stage] > 8) {
                newEvents <- maxNumberOfEventsPerStage[stage + 1]
            } else {
                newEvents <- (1 + allocationRatioPlanned[stage])^2 / allocationRatioPlanned[stage] *
                    (max(0, conditionalCriticalValue[stage] +
                        .getQNorm(conditionalPower), na.rm = TRUE))^2 / thetaStandardized^2
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

.getSimulatedStageSurvivalEnrichment <- function(...,
        design,
        subsets,
        prevalences,
        piControls,
        hazardRatios,
        directionUpper,
        stratifiedAnalysis,
        plannedEvents,
        typeOfSelection,
        effectMeasure,
        adaptations,
        epsilonValue,
        rValue,
        threshold,
        allocationRatioPlanned,
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

    simLogRanks <- matrix(NA_real_, nrow = pMax, ncol = kMax)
    cumulativeEventsPerStage <- matrix(NA_real_, nrow = pMax, ncol = kMax)

    populationEventsPerStage <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    overallEffects <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    testStatistics <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    logRankStatistics <- matrix(NA_real_, nrow = pMax, ncol = kMax)
    overallTestStatistics <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    separatePValues <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    conditionalCriticalValue <- rep(NA_real_, kMax - 1)
    conditionalPowerPerStage <- rep(NA_real_, kMax)
    selectedPopulations <- matrix(FALSE, nrow = gMax, ncol = kMax)
    selectedSubsets <- matrix(FALSE, nrow = pMax, ncol = kMax)
    selectedPopulations[, 1] <- TRUE
    selectedSubsets[, 1] <- TRUE
    adjustedPValues <- rep(NA_real_, kMax)
    populationHazardRatios <- rep(NA_real_, gMax)

    if (.isTrialDesignFisher(design)) {
        weights <- .getWeightsFisher(design)
    } else if (.isTrialDesignInverseNormal(design)) {
        weights <- .getWeightsInverseNormal(design)
    }

    for (k in seq_len(kMax)) {
        const <- allocationRatioPlanned[k] / (1 + allocationRatioPlanned[k])^2

        selectedSubsets[, k] <- .createSelectedSubsets(k, selectedPopulations)
        if (is.null(piControls) || length(piControls) == 0) {
            if (k == 1) {
                cumulativeEventsPerStage[, k] <- prevalences * (1 + allocationRatioPlanned[k] * hazardRatios) /
                    sum(prevalences * (1 + allocationRatioPlanned[k] * hazardRatios), na.rm = TRUE) *
                    plannedEvents[k]
            } else {
                prevSelected <- prevalences / sum(prevalences[selectedSubsets[, k]])
                prevSelected[!selectedSubsets[, k]] <- 0
                if (sum(prevSelected, na.rm = TRUE) > 0) {
                    cumulativeEventsPerStage[, k] <- prevSelected * (1 + allocationRatioPlanned[k] * hazardRatios) /
                        sum(prevSelected * (1 + allocationRatioPlanned[k] * hazardRatios), na.rm = TRUE) *
                        (plannedEvents[k] - plannedEvents[k - 1])
                } else {
                    break
                }
            }
        } else {
            rho <- (allocationRatioPlanned[k] * (1 - (1 - piControls)^hazardRatios) + piControls) /
                (1 + allocationRatioPlanned[k])
            if (k == 1) {
                cumulativeEventsPerStage[, k] <- prevalences * rho / sum(prevalences * rho, na.rm = TRUE) *
                    plannedEvents[k]
            } else {
                prevSelected <- prevalences / sum(prevalences[selectedSubsets[, k]])
                prevSelected[!selectedSubsets[, k]] <- 0
                if (sum(prevSelected, na.rm = TRUE) > 0) {
                    cumulativeEventsPerStage[, k] <- prevSelected * rho / sum(prevSelected * rho, na.rm = TRUE) *
                        (plannedEvents[k] - plannedEvents[k - 1])
                } else {
                    break
                }
            }
        }

        logRankStatistics[, k] <- (2 * directionUpper - 1) * stats::rnorm(pMax, log(hazardRatios) *
            sqrt(const * cumulativeEventsPerStage[, k]), 1)
        if (gMax == 1) {
            testStatistics[1, k] <- logRankStatistics[1, k]
            populationEventsPerStage[1, k] <- cumulativeEventsPerStage[1, k]
            overallTestStatistics[1, k] <- sum(sqrt(cumulativeEventsPerStage[1, 1:k]) * testStatistics[1, 1:k], na.rm = TRUE) /
                sqrt(sum(cumulativeEventsPerStage[1, 1:k], na.rm = TRUE))
            overallEffects[1, k] <- exp((2 * directionUpper - 1) * overallTestStatistics[1, k] /
                sqrt(const) / sqrt(sum(cumulativeEventsPerStage[1, 1:k], na.rm = TRUE)))
        } else if (gMax == 2) {
            # Population S1
            testStatistics[1, k] <- logRankStatistics[1, k]
            populationEventsPerStage[1, k] <- cumulativeEventsPerStage[1, k]
            overallTestStatistics[1, k] <- sum(sqrt(cumulativeEventsPerStage[1, 1:k]) * testStatistics[1, 1:k], na.rm = TRUE) /
                sqrt(sum(cumulativeEventsPerStage[1, 1:k], na.rm = TRUE))
            overallEffects[1, k] <- exp((2 * directionUpper - 1) * overallTestStatistics[1, k] /
                sqrt(const) / sqrt(sum(cumulativeEventsPerStage[1, 1:k], na.rm = TRUE)))
            # Full population
            testStatistics[2, k] <- sum(sqrt(cumulativeEventsPerStage[1:2, k]) * logRankStatistics[1:2, k], na.rm = TRUE) /
                sqrt(sum(cumulativeEventsPerStage[1:2, k], na.rm = TRUE))

            populationEventsPerStage[2, k] <- sum(cumulativeEventsPerStage[1:2, k], na.rm = TRUE)
            overallTestStatistics[2, k] <- sum(sqrt(populationEventsPerStage[2, 1:k]) * testStatistics[2, 1:k], na.rm = TRUE) /
                sqrt(sum(populationEventsPerStage[2, 1:k], na.rm = TRUE))
            overallEffects[2, k] <- exp((2 * directionUpper - 1) * overallTestStatistics[2, k] /
                sqrt(const) / sqrt(sum(populationEventsPerStage[2, 1:k], na.rm = TRUE)))
        } else if (gMax == 3) {
            # Population S1
            testStatistics[1, k] <- sum(sqrt(cumulativeEventsPerStage[c(1, 3), k]) * logRankStatistics[c(1, 3), k], na.rm = TRUE) /
                sqrt(sum(cumulativeEventsPerStage[c(1, 3), k], na.rm = TRUE))
            populationEventsPerStage[1, k] <- sum(cumulativeEventsPerStage[c(1, 3), k], na.rm = TRUE)
            overallTestStatistics[1, k] <- sum(sqrt(populationEventsPerStage[1, 1:k]) * testStatistics[1, 1:k], na.rm = TRUE) /
                sqrt(sum(populationEventsPerStage[1, 1:k], na.rm = TRUE))
            overallEffects[1, k] <- exp((2 * directionUpper - 1) * overallTestStatistics[1, k] /
                sqrt(const) / sqrt(sum(populationEventsPerStage[1, 1:k], na.rm = TRUE)))
            # Population S2
            testStatistics[2, k] <- sum(sqrt(cumulativeEventsPerStage[c(2, 3), k]) * logRankStatistics[c(2, 3), k], na.rm = TRUE) /
                sqrt(sum(cumulativeEventsPerStage[c(2, 3), k], na.rm = TRUE))
            populationEventsPerStage[2, k] <- sum(cumulativeEventsPerStage[c(2, 3), k], na.rm = TRUE)
            overallTestStatistics[2, k] <- sum(sqrt(populationEventsPerStage[2, 1:k]) * testStatistics[2, 1:k], na.rm = TRUE) /
                sqrt(sum(populationEventsPerStage[2, 1:k], na.rm = TRUE))
            overallEffects[2, k] <- exp((2 * directionUpper - 1) * overallTestStatistics[2, k] /
                sqrt(const) / sqrt(sum(populationEventsPerStage[2, 1:k], na.rm = TRUE)))
            # Full population
            testStatistics[3, k] <- sum(sqrt(cumulativeEventsPerStage[1:4, k]) * logRankStatistics[1:4, k], na.rm = TRUE) /
                sqrt(sum(cumulativeEventsPerStage[1:4, k], na.rm = TRUE))
            populationEventsPerStage[3, k] <- sum(cumulativeEventsPerStage[1:4, k], na.rm = TRUE)
            overallTestStatistics[3, k] <- sum(sqrt(populationEventsPerStage[3, 1:k]) * testStatistics[3, 1:k], na.rm = TRUE) /
                sqrt(sum(populationEventsPerStage[3, 1:k], na.rm = TRUE))
            overallEffects[3, k] <- exp((2 * directionUpper - 1) * overallTestStatistics[3, k] /
                sqrt(const) / sqrt(sum(populationEventsPerStage[3, 1:k], na.rm = TRUE)))
        } else if (gMax == 4) {
            # Population S1
            testStatistics[1, k] <- sum(sqrt(cumulativeEventsPerStage[c(1, 4, 5, 7), k]) * logRankStatistics[c(1, 4, 5, 7), k], na.rm = TRUE) /
                sqrt(sum(cumulativeEventsPerStage[c(1, 4, 5, 7), k], na.rm = TRUE))
            populationEventsPerStage[1, k] <- sum(cumulativeEventsPerStage[c(1, 4, 5, 7), k], na.rm = TRUE)
            overallTestStatistics[1, k] <- sum(sqrt(populationEventsPerStage[1, 1:k]) * testStatistics[1, 1:k], na.rm = TRUE) /
                sqrt(sum(populationEventsPerStage[1, 1:k], na.rm = TRUE))
            overallEffects[1, k] <- exp((2 * directionUpper - 1) * overallTestStatistics[1, k] /
                sqrt(const) / sqrt(sum(populationEventsPerStage[1, 1:k], na.rm = TRUE)))
            # Population S2
            testStatistics[2, k] <- sum(sqrt(cumulativeEventsPerStage[c(2, 4, 6, 7), k]) * logRankStatistics[c(2, 4, 6, 7), k], na.rm = TRUE) /
                sqrt(sum(cumulativeEventsPerStage[c(2, 4, 6, 7), k], na.rm = TRUE))
            populationEventsPerStage[2, k] <- sum(cumulativeEventsPerStage[c(2, 4, 6, 7), k], na.rm = TRUE)
            overallTestStatistics[2, k] <- sum(sqrt(populationEventsPerStage[2, 1:k]) * testStatistics[2, 1:k], na.rm = TRUE) /
                sqrt(sum(populationEventsPerStage[2, 1:k], na.rm = TRUE))
            overallEffects[2, k] <- exp((2 * directionUpper - 1) * overallTestStatistics[2, k] /
                sqrt(const) / sqrt(sum(populationEventsPerStage[2, 1:k], na.rm = TRUE)))
            # Population S3
            testStatistics[3, k] <- sum(sqrt(cumulativeEventsPerStage[c(3, 5, 6, 7), k]) * logRankStatistics[c(3, 5, 6, 7), k], na.rm = TRUE) /
                sqrt(sum(cumulativeEventsPerStage[c(3, 5, 6, 7), k], na.rm = TRUE))
            populationEventsPerStage[3, k] <- sum(cumulativeEventsPerStage[c(3, 5, 6, 7), k], na.rm = TRUE)
            overallTestStatistics[3, k] <- sum(sqrt(populationEventsPerStage[3, 1:k]) * testStatistics[3, 1:k], na.rm = TRUE) /
                sqrt(sum(populationEventsPerStage[3, 1:k], na.rm = TRUE))
            overallEffects[3, k] <- exp((2 * directionUpper - 1) * overallTestStatistics[3, k] /
                sqrt(const) / sqrt(sum(populationEventsPerStage[3, 1:k], na.rm = TRUE)))
            # Full population
            testStatistics[4, k] <- sum(sqrt(cumulativeEventsPerStage[1:8, k]) * logRankStatistics[1:8, k], na.rm = TRUE) /
                sqrt(sum(cumulativeEventsPerStage[1:8, k], na.rm = TRUE))
            populationEventsPerStage[4, k] <- sum(cumulativeEventsPerStage[1:8, k], na.rm = TRUE)
            overallTestStatistics[4, k] <- sum(sqrt(populationEventsPerStage[4, 1:k]) * testStatistics[4, 1:k], na.rm = TRUE) /
                sqrt(sum(populationEventsPerStage[4, 1:k], na.rm = TRUE))
            overallEffects[4, k] <- exp((2 * directionUpper - 1) * overallTestStatistics[4, k] /
                sqrt(const) / sqrt(sum(populationEventsPerStage[4, 1:k], na.rm = TRUE)))
        }

        testStatistics[!selectedPopulations[, k], k] <- NA_real_
        overallEffects[!selectedPopulations[, k], k] <- NA_real_
        overallTestStatistics[!selectedPopulations[, k], k] <- NA_real_

        separatePValues[, k] <- 1 - stats::pnorm(testStatistics[, k])

        if (k < kMax) {
            if (colSums(selectedPopulations)[k] == 0) {
                break
            }

            # Bonferroni adjustment
            adjustedPValues[k] <- min(min(separatePValues[, k], na.rm = TRUE) * (colSums(selectedPopulations)[k]), 1 - 1e-07)

            # conditional critical value to reject the null hypotheses at the next stage of the trial
            criticalValues <- .getCriticalValues(design)
            if (.isTrialDesignFisher(design)) {
                conditionalCriticalValue[k] <- .getOneMinusQNorm(min((criticalValues[k + 1] /
                    prod(adjustedPValues[1:k]^weights[1:k]))^(1 / weights[k + 1]), 1 - 1e-07))
            } else {
                conditionalCriticalValue[k] <- (criticalValues[k + 1] * sqrt(design$informationRates[k + 1]) -
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
                        "'calcEventsFunction' returned an illegal or undefined result (", newEvents, "); ",
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
                    type = "minMax", phase = "planning"
                ))
            } else {
                thetaStandardized <- log(thetaH1)
            }
            thetaStandardized <- (2 * directionUpper - 1) * thetaStandardized

            conditionalPowerPerStage[k] <- 1 - stats::pnorm(conditionalCriticalValue[k] -
                thetaStandardized * sqrt(plannedEvents[k + 1] - plannedEvents[k]) * sqrt(const))
        }
    }

    return(list(
        cumulativeEventsPerStage = cumulativeEventsPerStage,
        plannedEvents = plannedEvents,
        allocationRatioPlanned = allocationRatioPlanned,
        overallEffects = overallEffects,
        testStatistics = testStatistics,
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
getSimulationEnrichmentSurvival <- function(design = NULL, ...,
        effectList = NULL,
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
            ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
                design,
                powerCalculationEnabled = TRUE
            ), "showStatistics"), ...
        )
    } else {
        .assertIsTrialDesignInverseNormalOrFisher(design)
        .warnInCaseOfUnknownArguments(
            functionName = "getSimulationEnrichmentSurvival", 
            ignore = "showStatistics", ...)
        .warnInCaseOfTwoSidedPowerArgument(...)
    }

    .assertIsOneSidedDesign(design, designType = "enrichment", engineType = "simulation")

    calcEventsFunctionIsUserDefined <- !is.null(calcEventsFunction)

    directionUpper <- .assertIsValidDirectionUpper(directionUpper,
        design,
        objectType = "power", userFunctionCallEnabled = TRUE
    )

    simulationResults <- .createSimulationResultsEnrichmentObject(
        design                      = design,
        effectList                  = effectList,
        intersectionTest            = intersectionTest,
        stratifiedAnalysis          = stratifiedAnalysis,
        directionUpper              = directionUpper, # rates + survival only
        adaptations                 = adaptations,
        typeOfSelection             = typeOfSelection,
        effectMeasure               = effectMeasure,
        successCriterion            = successCriterion,
        epsilonValue                = epsilonValue,
        rValue                      = rValue,
        threshold                   = threshold,
        plannedEvents               = plannedEvents, # survival only
        allocationRatioPlanned      = allocationRatioPlanned,
        minNumberOfEventsPerStage   = minNumberOfEventsPerStage, # survival only
        maxNumberOfEventsPerStage   = maxNumberOfEventsPerStage, # survival only
        conditionalPower            = conditionalPower,
        thetaH1                     = thetaH1, # means + survival only
        maxNumberOfIterations       = maxNumberOfIterations,
        seed                        = seed,
        calcEventsFunction          = calcEventsFunction, # survival only
        selectPopulationsFunction   = selectPopulationsFunction,
        showStatistics              = showStatistics,
        endpoint                    = "survival"
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
    thetaH1 <- simulationResults$thetaH1 # means + survival only
    plannedEvents <- simulationResults$plannedEvents # survival only
    conditionalPower <- simulationResults$conditionalPower
    minNumberOfEventsPerStage <- simulationResults$minNumberOfEventsPerStage # survival only
    maxNumberOfEventsPerStage <- simulationResults$maxNumberOfEventsPerStage # survival only
    allocationRatioPlanned <- simulationResults$allocationRatioPlanned
    calcEventsFunction <- simulationResults$calcEventsFunction

    if (length(allocationRatioPlanned) == 1) {
        allocationRatioPlanned <- rep(allocationRatioPlanned, kMax)
    }

    indices <- .getIndicesOfClosedHypothesesSystemForSimulation(gMax = gMax)

    cols <- nrow(effectList$hazardRatios)

    simulatedSelections <- array(0, dim = c(kMax, cols, gMax))
    simulatedRejections <- array(0, dim = c(kMax, cols, gMax))
    simulatedNumberOfPopulations <- matrix(0, nrow = kMax, ncol = cols)
    simulatedSingleEventsPerStage <- array(0, dim = c(kMax, cols, 2^(gMax - 1)))
    simulatedOverallEventsPerStage <- matrix(0, nrow = kMax, ncol = cols)
    simulatedSuccessStopping <- matrix(0, nrow = kMax, ncol = cols)
    simulatedFutilityStopping <- matrix(0, nrow = kMax - 1, ncol = cols)
    simulatedConditionalPower <- matrix(0, nrow = kMax, ncol = cols)
    simulatedRejectAtLeastOne <- rep(0, cols)
    expectedNumberOfEvents <- rep(0, cols)
    iterations <- matrix(0, nrow = kMax, ncol = cols)

    len <- maxNumberOfIterations * kMax * gMax * cols

    dataIterationNumber <- rep(NA_real_, len)
    dataStageNumber <- rep(NA_real_, len)
    dataArmNumber <- rep(NA_real_, len)
    dataAlternative <- rep(NA_real_, len)
    dataEffect <- rep(NA_real_, len)
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
            stageResults <- .getSimulatedStageSurvivalEnrichment(
                design = design,
                subsets = effectList$subsets,
                prevalences = effectList$prevalences,
                piControls = effectList$piControls,
                hazardRatios = effectList$hazardRatios[i, ],
                directionUpper = directionUpper,
                stratifiedAnalysis = stratifiedAnalysis,
                plannedEvents = plannedEvents,
                typeOfSelection = typeOfSelection,
                effectMeasure = effectMeasure,
                adaptations = adaptations,
                epsilonValue = epsilonValue,
                rValue = rValue,
                threshold = threshold,
                allocationRatioPlanned = allocationRatioPlanned,
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
                design = design, indices = indices,
                intersectionTest = intersectionTest,
                successCriterion = successCriterion
            )

            rejectAtSomeStage <- FALSE
            rejectedPopulationsBefore <- rep(FALSE, gMax)

            for (k in seq_len(kMax)) {
                simulatedRejections[k, i, ] <- simulatedRejections[k, i, ] +
                    (closedTest$rejected[, k] & closedTest$selectedPopulations[1:gMax, k] | rejectedPopulationsBefore)
                simulatedSelections[k, i, ] <- simulatedSelections[k, i, ] + closedTest$selectedPopulations[, k]

                simulatedSingleEventsPerStage[k, i, ] <- simulatedSingleEventsPerStage[k, i, ] + stageResults$cumulativeEventsPerStage[, k]

                simulatedNumberOfPopulations[k, i] <- simulatedNumberOfPopulations[k, i] + sum(closedTest$selectedPopulations[, k])

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
                    simulatedOverallEventsPerStage[k, i] <- simulatedOverallEventsPerStage[k, i] +
                        stageResults$plannedEvents[k]
                } else {
                    simulatedOverallEventsPerStage[k, i] <- simulatedOverallEventsPerStage[k, i] +
                        stageResults$plannedEvents[k] - stageResults$plannedEvents[k - 1]
                }

                for (g in seq_len(gMax)) {
                    dataIterationNumber[index] <- j
                    dataStageNumber[index] <- k
                    dataArmNumber[index] <- g
                    dataAlternative[index] <- i
                    dataEffect[index] <- effectList$hazardRatios[i, g]
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

                if (!rejectAtSomeStage && any(closedTest$rejected[, k] &
                        closedTest$selectedPopulations[1:gMax, k] | rejectedPopulationsBefore)) {
                    simulatedRejectAtLeastOne[i] <- simulatedRejectAtLeastOne[i] + 1
                    rejectAtSomeStage <- TRUE
                }

                if ((k < kMax) && (closedTest$successStop[k] || closedTest$futilityStop[k])) {
                    # rejected hypotheses remain rejected also in case of early stopping
                    simulatedRejections[(k + 1):kMax, i, ] <- simulatedRejections[(k + 1):kMax, i, ] +
                        matrix((closedTest$rejected[, k] & closedTest$selectedPopulations[1:gMax, k] | rejectedPopulationsBefore),
                            kMax - k, gMax,
                            byrow = TRUE
                        )
                    break
                }

                rejectedPopulationsBefore <- closedTest$rejected[, k] &
                    closedTest$selectedPopulations[1:gMax, k] | rejectedPopulationsBefore
            }
        }

        simulatedSingleEventsPerStage[, i, ] <- simulatedSingleEventsPerStage[, i, ] / iterations[, i]

        simulatedOverallEventsPerStage[, i] <- simulatedOverallEventsPerStage[, i] / iterations[, i]

        if (kMax > 1) {
            simulatedRejections[2:kMax, i, ] <- simulatedRejections[2:kMax, i, ] - simulatedRejections[1:(kMax - 1), i, ]

            stopping <- cumsum(simulatedSuccessStopping[1:(kMax - 1), i] +
                simulatedFutilityStopping[, i]) / maxNumberOfIterations

            expectedNumberOfEvents[i] <- simulatedOverallEventsPerStage[1, i] + t(1 - stopping) %*%
                simulatedOverallEventsPerStage[2:kMax, i]
        } else {
            expectedNumberOfEvents[i] <- simulatedOverallEventsPerStage[1, i]
        }
    }

    simulatedConditionalPower[1, ] <- NA_real_
    if (kMax > 1) {
        simulatedConditionalPower[2:kMax, ] <- simulatedConditionalPower[2:kMax, ] / iterations[2:kMax, ]
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

    simulationResults$singleEventsPerSubsetAndStage <- simulatedSingleEventsPerStage
    simulationResults$.setParameterType("singleEventsPerSubsetAndStage", C_PARAM_GENERATED)
    .addDeprecatedFieldValues(simulationResults, "singleNumberOfEventsPerStage", simulatedSingleEventsPerStage)

    simulationResults$expectedNumberOfEvents <- expectedNumberOfEvents

    simulationResults$iterations <- iterations

    if (!all(is.na(simulationResults$conditionalPowerAchieved))) {
        simulationResults$.setParameterType("conditionalPowerAchieved", C_PARAM_GENERATED)
    }

    if (any(simulationResults$rejectedPopulationsPerStage < 0)) {
        stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, 
            "internal error, simulation not possible due to numerical overflow")
    }

    data <- data.frame(
        iterationNumber = dataIterationNumber,
        stageNumber = dataStageNumber,
        populationNumber = dataArmNumber,
        omegaMax = dataAlternative,
        effect = dataEffect,
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
    simulationResults$.data <- data

    return(simulationResults)
}
