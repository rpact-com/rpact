## |
## |  *Simulation of enrichment design with continuous data*
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
## |  File version: $Revision: 8225 $
## |  Last changed: $Date: 2024-09-18 09:38:40 +0200 (Mi, 18 Sep 2024) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_simulation_enrichment.R
NULL

.getSimulationMeansEnrichmentStageSubjects <- function(..., stage,
        conditionalPower,
        conditionalCriticalValue,
        plannedSubjects,
        allocationRatioPlanned,
        selectedPopulations,
        thetaH1,
        overallEffects,
        stDevH1,
        minNumberOfSubjectsPerStage,
        maxNumberOfSubjectsPerStage) {
    stage <- stage - 1 # to be consistent with non-enrichment situation
    gMax <- nrow(overallEffects)

    if (!is.na(conditionalPower)) {
        if (any(selectedPopulations[1:gMax, stage + 1], na.rm = TRUE)) {
            if (is.na(thetaH1)) {
                thetaStandardized <- max(min(overallEffects[
                    selectedPopulations[1:gMax, stage + 1], stage
                ] / stDevH1, na.rm = TRUE), 1e-07)
            } else {
                max(thetaStandardized <- thetaH1 / stDevH1, 1e-07)
            }

            if (conditionalCriticalValue[stage] > 8) {
                newSubjects <- maxNumberOfSubjectsPerStage[stage + 1]
            } else {
                newSubjects <- (1 + allocationRatioPlanned[stage])^2 / allocationRatioPlanned[stage] *
                    (max(0, conditionalCriticalValue[stage] +
                        .getQNorm(conditionalPower)))^2 / thetaStandardized^2
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

.getSimulatedStageMeansEnrichment <- function(...,
        design,
        subsets,
        prevalences,
        effects,
        stDevs,
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
        thetaH1,
        stDevH1,
        calcSubjectsFunction,
        calcSubjectsFunctionIsUserDefined,
        selectPopulationsFunction) {
    kMax <- length(plannedSubjects)
    pMax <- length(effects)
    gMax <- log(length(effects), 2) + 1

    subjectsPerStage <- matrix(NA_real_, nrow = pMax, ncol = kMax)
    simEffects <- matrix(NA_real_, nrow = pMax, ncol = kMax)

    populationSubjectsPerStage <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    overallEffects <- matrix(NA_real_, nrow = gMax, ncol = kMax)
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

    if (.isTrialDesignFisher(design)) {
        weights <- .getWeightsFisher(design)
    } else if (.isTrialDesignInverseNormal(design)) {
        weights <- .getWeightsInverseNormal(design)
    }


    for (k in 1:kMax) {
        const <- allocationRatioPlanned[k] / (1 + allocationRatioPlanned[k])^2

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
        simEffects[selsubs, k] <- stats::rnorm(rep(1, sum(selsubs)), effects[selsubs], stDevs[selsubs] /
            sqrt(subjectsPerStage[selsubs, k] * const))

        if (gMax == 1) {
            testStatistics[1, k] <- simEffects[1, k] / stDevs[1] * sqrt(subjectsPerStage[1, k] * const)
            populationSubjectsPerStage[1, k] <- subjectsPerStage[1, k]
            overallEffects[1, k] <-
                sum(subjectsPerStage[1, 1:k] * simEffects[1, 1:k]) / sum(subjectsPerStage[1, 1:k])
            overallTestStatistics[1, k] <- overallEffects[1, k] /
                (stDevs[1] / sqrt(sum(subjectsPerStage[1, 1:k]) * const))
        } else if (gMax == 2) {
            # Population S1
            testStatistics[1, k] <- simEffects[1, k] / stDevs[1] * sqrt(subjectsPerStage[1, k] * const)
            populationSubjectsPerStage[1, k] <- subjectsPerStage[1, k]
            overallEffects[1, k] <-
                sum(subjectsPerStage[1, 1:k] * simEffects[1, 1:k]) / sum(subjectsPerStage[1, 1:k])
            overallTestStatistics[1, k] <- overallEffects[1, k] /
                (stDevs[1] / sqrt(sum(subjectsPerStage[1, 1:k]) * const))
            # Full population
            testStatistics[2, k] <- sum(subjectsPerStage[1:2, k] * simEffects[1:2, k], na.rm = TRUE) * sqrt(const) /
                sqrt(sum(subjectsPerStage[1:2, k] * stDevs[1:2]^2, na.rm = TRUE))
            populationSubjectsPerStage[2, k] <- sum(subjectsPerStage[1:2, k], na.rm = TRUE)

            overallEffects[2, k] <- sum(subjectsPerStage[1:2, 1:k] * simEffects[1:2, 1:k], na.rm = TRUE) /
                sum(subjectsPerStage[1:2, 1:k], na.rm = TRUE)
            sd <- sqrt(sum(subjectsPerStage[1:2, 1:k] * stDevs[1:2]^2, na.rm = TRUE) /
                sum(subjectsPerStage[1:2, 1:k], na.rm = TRUE))
            overallTestStatistics[2, k] <- overallEffects[2, k] /
                sd * sqrt(sum(subjectsPerStage[1:2, 1:k], na.rm = TRUE) * const)
        } else if (gMax == 3) {
            # Population S1
            testStatistics[1, k] <- sum(subjectsPerStage[c(1, 3), k] * simEffects[c(1, 3), k], na.rm = TRUE) * sqrt(const) /
                sqrt(sum(subjectsPerStage[c(1, 3), k] * stDevs[c(1, 3)]^2, na.rm = TRUE))
            populationSubjectsPerStage[1, k] <- sum(subjectsPerStage[c(1, 3), k], na.rm = TRUE)
            overallEffects[1, k] <-
                sum(subjectsPerStage[c(1, 3), 1:k] * simEffects[c(1, 3), 1:k], na.rm = TRUE) /
                    sum(subjectsPerStage[c(1, 3), 1:k], na.rm = TRUE)
            sd <- sqrt(sum(subjectsPerStage[c(1, 3), 1:k] * stDevs[c(1, 3)]^2, na.rm = TRUE) /
                sum(subjectsPerStage[c(1, 3), 1:k], na.rm = TRUE))
            overallTestStatistics[1, k] <- overallEffects[1, k] /
                sd * sqrt(sum(subjectsPerStage[c(1, 3), 1:k], na.rm = TRUE) * const)
            # Population S2
            testStatistics[2, k] <- sum(subjectsPerStage[c(2, 3), k] * simEffects[c(2, 3), k], na.rm = TRUE) * sqrt(const) /
                sqrt(sum(subjectsPerStage[c(2, 3), k] * stDevs[c(2, 3)]^2, na.rm = TRUE))
            populationSubjectsPerStage[2, k] <- sum(subjectsPerStage[c(2, 3), k])
            overallEffects[2, k] <-
                sum(subjectsPerStage[c(2, 3), 1:k] * simEffects[c(2, 3), 1:k], na.rm = TRUE) /
                    sum(subjectsPerStage[c(2, 3), 1:k], na.rm = TRUE)
            sd <- sqrt(sum(subjectsPerStage[c(2, 3), 1:k] * stDevs[c(2, 3)]^2, na.rm = TRUE) /
                sum(subjectsPerStage[c(2, 3), 1:k], na.rm = TRUE))
            overallTestStatistics[2, k] <- overallEffects[2, k] /
                sd * sqrt(sum(subjectsPerStage[c(2, 3), 1:k], na.rm = TRUE) * const)
            # Full population
            testStatistics[3, k] <- sum(subjectsPerStage[1:4, k] * simEffects[1:4, k], na.rm = TRUE) * sqrt(const) /
                sqrt(sum(subjectsPerStage[1:4, k] * stDevs[1:4]^2, na.rm = TRUE))
            populationSubjectsPerStage[3, k] <- sum(subjectsPerStage[1:4, k])
            overallEffects[3, k] <-
                sum(subjectsPerStage[1:4, 1:k] * simEffects[1:4, 1:k], na.rm = TRUE) /
                    sum(subjectsPerStage[1:4, 1:k], na.rm = TRUE)
            sd <- sqrt(sum(subjectsPerStage[1:4, 1:k] * stDevs[1:4]^2, na.rm = TRUE) /
                sum(subjectsPerStage[1:4, 1:k], na.rm = TRUE))
            overallTestStatistics[3, k] <- overallEffects[3, k] /
                sd * sqrt(sum(subjectsPerStage[1:4, 1:k], na.rm = TRUE) * const)
        } else if (gMax == 4) {
            # Population S1
            testStatistics[1, k] <- sum(subjectsPerStage[c(1, 4, 5, 7), k] * simEffects[c(1, 4, 5, 7), k], na.rm = TRUE) * sqrt(const) /
                sqrt(sum(subjectsPerStage[c(1, 4, 5, 7), k] * stDevs[c(1, 4, 5, 7)]^2, na.rm = TRUE))
            populationSubjectsPerStage[1, k] <- sum(subjectsPerStage[c(1, 4, 5, 7), k], na.rm = TRUE)
            overallEffects[1, k] <-
                sum(subjectsPerStage[c(1, 4, 5, 7), 1:k] * simEffects[c(1, 4, 5, 7), 1:k], na.rm = TRUE) /
                    sum(subjectsPerStage[c(1, 4, 5, 7), 1:k], na.rm = TRUE)
            sd <- sqrt(sum(subjectsPerStage[c(1, 4, 5, 7), 1:k] * stDevs[c(1, 4, 5, 7)]^2, na.rm = TRUE) /
                sum(subjectsPerStage[c(1, 4, 5, 7), 1:k], na.rm = TRUE))
            overallTestStatistics[1, k] <- overallEffects[1, k] /
                sd * sqrt(sum(subjectsPerStage[c(1, 4, 5, 7), 1:k], na.rm = TRUE) * const)
            # Population S2
            testStatistics[2, k] <- sum(subjectsPerStage[c(2, 4, 6, 7), k] * simEffects[c(2, 4, 6, 7), k], na.rm = TRUE) * sqrt(const) /
                sqrt(sum(subjectsPerStage[c(2, 4, 6, 7), k] * stDevs[c(2, 4, 6, 7)]^2, na.rm = TRUE))
            populationSubjectsPerStage[2, k] <- sum(subjectsPerStage[c(2, 4, 6, 7), k])
            overallEffects[2, k] <-
                sum(subjectsPerStage[c(2, 4, 6, 7), 1:k] * simEffects[c(2, 4, 6, 7), 1:k], na.rm = TRUE) /
                    sum(subjectsPerStage[c(2, 4, 6, 7), 1:k], na.rm = TRUE)
            sd <- sqrt(sum(subjectsPerStage[c(2, 4, 6, 7), 1:k] * stDevs[c(2, 4, 6, 7)]^2, na.rm = TRUE) /
                sum(subjectsPerStage[c(2, 4, 6, 7), 1:k], na.rm = TRUE))
            overallTestStatistics[2, k] <- overallEffects[2, k] /
                sd * sqrt(sum(subjectsPerStage[c(2, 4, 6, 7), 1:k], na.rm = TRUE) * const)
            # Population S3
            testStatistics[3, k] <- sum(subjectsPerStage[c(3, 5, 6, 7), k] * simEffects[c(3, 5, 6, 7), k], na.rm = TRUE) * sqrt(const) /
                sqrt(sum(subjectsPerStage[c(3, 5, 6, 7), k] * stDevs[c(3, 5, 6, 7)]^2, na.rm = TRUE))
            populationSubjectsPerStage[3, k] <- sum(subjectsPerStage[c(3, 5, 6, 7), k])
            overallEffects[3, k] <-
                sum(subjectsPerStage[c(3, 5, 6, 7), 1:k] * simEffects[c(3, 5, 6, 7), 1:k], na.rm = TRUE) /
                    sum(subjectsPerStage[c(3, 5, 6, 7), 1:k], na.rm = TRUE)
            sd <- sqrt(sum(subjectsPerStage[c(3, 5, 6, 7), 1:k] * stDevs[c(3, 5, 6, 7)]^2, na.rm = TRUE) /
                sum(subjectsPerStage[c(3, 5, 6, 7), 1:k], na.rm = TRUE))
            overallTestStatistics[3, k] <- overallEffects[3, k] /
                sd * sqrt(sum(subjectsPerStage[c(3, 5, 6, 7), 1:k], na.rm = TRUE) * const)
            # Full population
            testStatistics[4, k] <- sum(subjectsPerStage[1:8, k] * simEffects[1:8, k], na.rm = TRUE) * sqrt(const) /
                sqrt(sum(subjectsPerStage[1:8, k] * stDevs[1:8]^2, na.rm = TRUE))
            populationSubjectsPerStage[4, k] <- sum(subjectsPerStage[1:8, k])
            overallEffects[4, k] <-
                sum(subjectsPerStage[1:8, 1:k] * simEffects[1:8, 1:k], na.rm = TRUE) /
                    sum(subjectsPerStage[1:8, 1:k], na.rm = TRUE)
            sd <- sqrt(sum(subjectsPerStage[1:8, 1:k] * stDevs[1:8]^2, na.rm = TRUE) /
                sum(subjectsPerStage[1:8, 1:k], na.rm = TRUE))
            overallTestStatistics[4, k] <- overallEffects[4, k] /
                sd * sqrt(sum(subjectsPerStage[1:8, 1:k], na.rm = TRUE) * const)
        }

        testStatistics[!selectedPopulations[, k], k] <- NA_real_
        overallEffects[!selectedPopulations[, k], k] <- NA_real_
        overallTestStatistics[!selectedPopulations[, k], k] <- NA_real_

        separatePValues[, k] <- 1 - stats::pnorm(testStatistics[, k])

        criticalValues <- .getCriticalValues(design)
        if (k < kMax) {
            if (colSums(selectedPopulations)[k] == 0) {
                break
            }

            # Bonferroni adjustment
            adjustedPValues[k] <- min(min(separatePValues[, k], na.rm = TRUE) *
                colSums(selectedPopulations)[k], 1 - 1e-07)

            # conditional critical value to reject the null hypotheses at the next stage of the trial
            if (.isTrialDesignFisher(design)) {
                conditionalCriticalValue[k] <- .getOneMinusQNorm(min((criticalValues[k + 1] /
                    prod(adjustedPValues[1:k]^weights[1:k]))^(1 / weights[k + 1]), 1 - 1e-07))
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
                    conditionalPower = conditionalPower,
                    conditionalCriticalValue = conditionalCriticalValue,
                    plannedSubjects = plannedSubjects,
                    allocationRatioPlanned = allocationRatioPlanned,
                    selectedPopulations = selectedPopulations,
                    thetaH1 = thetaH1,
                    stDevH1 = stDevH1,
                    overallEffects = overallEffects
                )
                if (effectMeasure == "testStatistic") {
                    selectPopulationsFunctionArgs$effectVector <- overallTestStatistics[, k]
                } else if (effectMeasure == "effectEstimate") {
                    selectPopulationsFunctionArgs$effectVector <- overallEffects[, k]
                }

                args <- list(
                    typeOfSelection = typeOfSelection,
                    epsilonValue = epsilonValue,
                    rValue = rValue,
                    threshold = threshold,
                    selectPopulationsFunction = selectPopulationsFunction,
                    selectPopulationsFunctionArgs = selectPopulationsFunctionArgs
                )

                selectedPopulations[, k + 1] <- (selectedPopulations[, k] & do.call(.selectPopulations, args))

                newSubjects <- calcSubjectsFunction(
                    stage = k + 1, # to be consistent with non-enrichment situation, cf. line 36
                    conditionalPower = conditionalPower,
                    conditionalCriticalValue = conditionalCriticalValue,
                    plannedSubjects = plannedSubjects,
                    allocationRatioPlanned = allocationRatioPlanned,
                    selectedPopulations = selectedPopulations,
                    thetaH1 = thetaH1,
                    stDevH1 = stDevH1,
                    overallEffects = overallEffects,
                    minNumberOfSubjectsPerStage = minNumberOfSubjectsPerStage,
                    maxNumberOfSubjectsPerStage = maxNumberOfSubjectsPerStage
                )

                if (is.null(newSubjects) || length(newSubjects) != 1 ||
                        !is.numeric(newSubjects) || is.na(newSubjects) || newSubjects < 0) {
                    stop(
                        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                        "'calcSubjectsFunction' returned an illegal or undefined result (", newSubjects, "); ",
                        "the output must be a single numeric value >= 0"
                    )
                }
                if (!is.na(conditionalPower) || calcSubjectsFunctionIsUserDefined) {
                    plannedSubjects[(k + 1):kMax] <- plannedSubjects[k] + cumsum(rep(newSubjects, kMax - k))
                }
            } else {
                selectedPopulations[, k + 1] <- selectedPopulations[, k]
            }

            if (is.na(thetaH1)) {
                thetaStandardized <- min(overallEffects[selectedPopulations[1:gMax, k], k] / stDevH1, na.rm = TRUE)
            } else {
                thetaStandardized <- thetaH1 / stDevH1
            }

            conditionalPowerPerStage[k] <- 1 - stats::pnorm(conditionalCriticalValue[k] -
                thetaStandardized * sqrt(plannedSubjects[k + 1] - plannedSubjects[k]) *
                    sqrt(allocationRatioPlanned[k]) / (1 + allocationRatioPlanned[k]))
        }
    }

    return(list(
        subjectsPerStage = subjectsPerStage,
        populationSubjectsPerStage = populationSubjectsPerStage,
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
#' Get Simulation Enrichment Means
#'
#' @description
#' Returns the simulated power, stopping and selection probabilities, conditional power,
#' and expected sample size or testing means in an enrichment design testing situation.
#'
#' @inheritParams param_intersectionTest_Enrichment
#' @inheritParams param_typeOfSelection
#' @inheritParams param_effectMeasure
#' @inheritParams param_adaptations
#' @inheritParams param_threshold
#' @inheritParams param_effectList
#' @inheritParams param_stDevSimulation
#' @inheritParams param_successCriterion
#' @inheritParams param_typeOfSelection
#' @inheritParams param_design_with_default
#' @inheritParams param_allocationRatioPlanned
#' @inheritParams param_plannedSubjects
#' @inheritParams param_minNumberOfSubjectsPerStage
#' @inheritParams param_maxNumberOfSubjectsPerStage
#' @inheritParams param_conditionalPowerSimulation
#' @inheritParams param_thetaH1
#' @inheritParams param_stDevH1
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
#' At given design the function simulates the power, stopping probabilities, selection probabilities,
#' and expected sample size at given number of subjects, parameter configuration, and population
#' selection rule in the enrichment situation.
#' An allocation ratio can be specified referring to the ratio of number of subjects in the active
#' treatment groups as compared to the control group.
#'
#' The definition of \code{thetaH1} and/or \code{stDevH1} makes only sense if \code{kMax} > 1
#' and if \code{conditionalPower}, \code{minNumberOfSubjectsPerStage}, and
#' \code{maxNumberOfSubjectsPerStage} (or \code{calcSubjectsFunction}) are defined.
#'
#' \code{calcSubjectsFunction}\cr
#' This function returns the number of subjects at given conditional power and conditional
#' critical value for specified testing situation. The function might depend on the variables
#' \code{stage},
#' \code{selectedPopulations},
#' \code{plannedSubjects},
#' \code{allocationRatioPlanned},
#' \code{minNumberOfSubjectsPerStage},
#' \code{maxNumberOfSubjectsPerStage},
#' \code{conditionalPower},
#' \code{conditionalCriticalValue},
#' \code{overallEffects}, and
#' \code{stDevH1}.
#' The function has to contain the three-dots argument '...' (see examples).
#'
#' @template return_object_simulation_results
#' @template how_to_get_help_for_generics
#'
#' @template examples_get_simulation_enrichment_means
#'
#' @export
#'
getSimulationEnrichmentMeans <- function(design = NULL, ...,
        effectList = NULL,
        intersectionTest = c("Simes", "SpiessensDebois", "Bonferroni", "Sidak"), # C_INTERSECTION_TEST_ENRICHMENT_DEFAULT
        stratifiedAnalysis = TRUE, # C_STRATIFIED_ANALYSIS_DEFAULT,
        adaptations = NA,
        typeOfSelection = c("best", "rBest", "epsilon", "all", "userDefined"), # C_TYPE_OF_SELECTION_DEFAULT
        effectMeasure = c("effectEstimate", "testStatistic"), # C_EFFECT_MEASURE_DEFAULT
        successCriterion = c("all", "atLeastOne"), # C_SUCCESS_CRITERION_DEFAULT
        epsilonValue = NA_real_,
        rValue = NA_real_,
        threshold = -Inf,
        plannedSubjects = NA_integer_,
        allocationRatioPlanned = NA_real_,
        minNumberOfSubjectsPerStage = NA_real_,
        maxNumberOfSubjectsPerStage = NA_real_,
        conditionalPower = NA_real_,
        thetaH1 = NA_real_,
        stDevH1 = NA_real_,
        maxNumberOfIterations = 1000L, # C_MAX_SIMULATION_ITERATIONS_DEFAULT
        seed = NA_real_,
        calcSubjectsFunction = NULL,
        selectPopulationsFunction = NULL,
        showStatistics = FALSE) {
    if (is.null(design)) {
        design <- .getDefaultDesign(..., type = "simulation")
        .warnInCaseOfUnknownArguments(
            functionName = "getSimulationEnrichmentMeans",
            ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
                design,
                powerCalculationEnabled = TRUE
            ), "showStatistics"), ...
        )
    } else {
        .assertIsTrialDesignInverseNormalOrFisher(design)
        .warnInCaseOfUnknownArguments(functionName = "getSimulationEnrichmentMeans", ignore = "showStatistics", ...)
        .warnInCaseOfTwoSidedPowerArgument(...)
    }

    .assertIsOneSidedDesign(design, designType = "enrichment", engineType = "simulation")

    calcSubjectsFunctionIsUserDefined <- !is.null(calcSubjectsFunction)

    simulationResults <- .createSimulationResultsEnrichmentObject(
        design                      = design,
        effectList                  = effectList,
        intersectionTest            = intersectionTest,
        stratifiedAnalysis          = stratifiedAnalysis,
        adaptations                 = adaptations,
        typeOfSelection             = typeOfSelection,
        effectMeasure               = effectMeasure,
        successCriterion            = successCriterion,
        epsilonValue                = epsilonValue,
        rValue                      = rValue,
        threshold                   = threshold,
        plannedSubjects             = plannedSubjects, # means + rates only
        allocationRatioPlanned      = allocationRatioPlanned,
        minNumberOfSubjectsPerStage = minNumberOfSubjectsPerStage, # means + rates only
        maxNumberOfSubjectsPerStage = maxNumberOfSubjectsPerStage, # means + rates only
        conditionalPower            = conditionalPower,
        thetaH1                     = thetaH1, # means + survival only
        stDevH1                     = stDevH1, # means only
        maxNumberOfIterations       = maxNumberOfIterations,
        seed                        = seed,
        calcSubjectsFunction        = calcSubjectsFunction, # means + rates only
        selectPopulationsFunction   = selectPopulationsFunction,
        showStatistics              = showStatistics,
        endpoint                    = "means"
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
    stDevH1 <- simulationResults$stDevH1 # means only
    conditionalPower <- simulationResults$conditionalPower
    minNumberOfSubjectsPerStage <- simulationResults$minNumberOfSubjectsPerStage
    maxNumberOfSubjectsPerStage <- simulationResults$maxNumberOfSubjectsPerStage
    allocationRatioPlanned <- simulationResults$allocationRatioPlanned
    calcSubjectsFunction <- simulationResults$calcSubjectsFunction

    if (length(allocationRatioPlanned) == 1) {
        allocationRatioPlanned <- rep(allocationRatioPlanned, kMax)
    }

    indices <- .getIndicesOfClosedHypothesesSystemForSimulation(gMax = gMax)

    cols <- nrow(effectList$effects)

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

    stDevs <- effectList$stDevs
    if (length(stDevs) == 1) {
        stDevs <- rep(stDevs, ncol(effectList$effects))
    }

    if (is.na(stDevH1)) {
        stDevH1 <- max(stDevs, na.rm = TRUE)
    }

    index <- 1
    for (i in 1:cols) {
        for (j in 1:maxNumberOfIterations) {
            stageResults <- .getSimulatedStageMeansEnrichment(
                design = design,
                subsets = effectList$subsets,
                prevalences = effectList$prevalences,
                effects = effectList$effects[i, ],
                stDevs = stDevs,
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
                thetaH1 = thetaH1,
                stDevH1 = stDevH1,
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
                    dataSubjectsPopulation[index] <- round(stageResults$populationSubjectsPerStage[g, k], 1)
                    dataSubjectsActivePopulation[index] <- round(stageResults$populationSubjectsPerStage[g, k], 1)
                    dataNumberOfSubjects[index] <- round(sum(stageResults$populationSubjectsPerStage[, k], na.rm = TRUE), 1)
                    dataNumberOfCumulatedSubjects[index] <- round(sum(
                        stageResults$populationSubjectsPerStage[, 1:k],
                        na.rm = TRUE
                    ), 1)
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
            stopping <- cumsum(simulatedSuccessStopping[1:(kMax - 1), i] + simulatedFutilityStopping[, i]) / maxNumberOfIterations
            expectedNumberOfSubjects[i] <- sum(simulatedSubjectsPerStage[1, i, ] + t(1 - stopping) %*%
                simulatedSubjectsPerStage[2:kMax, i, ])
        } else {
            expectedNumberOfSubjects[i] <- sum(simulatedSubjectsPerStage[1, i, ])
        }
    }

    simulatedConditionalPower[1, ] <- NA_real_
    if (kMax > 1) {
        simulatedConditionalPower[2:kMax, ] <- simulatedConditionalPower[2:kMax, ] / iterations[2:kMax, ]
    }
    simulationResults$numberOfPopulations <- simulatedNumberOfPopulations / iterations

    simulationResults$rejectAtLeastOne <- simulatedRejectAtLeastOne / maxNumberOfIterations
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
        testStatistic = dataTestStatistics,
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
