## |
## |  *Simulation of multi-arm design with binary data*
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
## |  File version: $Revision: 7126 $
## |  Last changed: $Date: 2023-06-23 14:26:39 +0200 (Fr, 23 Jun 2023) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_simulation_multiarm.R
NULL

.getSimulationRatesMultiArmStageSubjects <- function(...,
        stage,
        directionUpper,
        conditionalPower,
        conditionalCriticalValue,
        plannedSubjects,
        allocationRatioPlanned,
        selectedArms,
        piTreatmentsH1,
        piControlH1,
        overallRates,
        overallRatesControl,
        minNumberOfSubjectsPerStage,
        maxNumberOfSubjectsPerStage) {
    stage <- stage - 1 # to be consistent with non-multiarm situation
    gMax <- nrow(overallRates)

    if (!is.na(conditionalPower)) {
        if (any(selectedArms[1:gMax, stage + 1], na.rm = TRUE)) {
            if (is.na(piControlH1)) {
                piAssumedControlH1 <- overallRatesControl[stage]
            } else {
                piAssumedControlH1 <- piControlH1
            }
            if (is.na(piTreatmentsH1)) {
                if (directionUpper) {
                    piAssumedH1 <- min(overallRates[selectedArms[1:gMax, stage + 1], stage], na.rm = TRUE)
                } else {
                    piAssumedH1 <- max(overallRates[selectedArms[1:gMax, stage + 1], stage], na.rm = TRUE)
                }
            } else {
                piAssumedH1 <- piTreatmentsH1
            }
            pim <- (allocationRatioPlanned[stage] * piAssumedH1 + piAssumedControlH1) / (1 + allocationRatioPlanned[stage])

            if (conditionalCriticalValue[stage] > 8) {
                newSubjects <- maxNumberOfSubjectsPerStage[stage + 1]
            } else {
                newSubjects <- (max(0, conditionalCriticalValue[stage] *
                    sqrt(pim * (1 - pim) * (1 + allocationRatioPlanned[stage])) +
                    .getQNorm(conditionalPower) * sqrt(piAssumedH1 * (1 - piAssumedH1) +
                        piAssumedControlH1 * (1 - piAssumedControlH1) * allocationRatioPlanned[stage])))^2 /
                    (max(1e-7, (2 * directionUpper - 1) * (piAssumedH1 - piAssumedControlH1)))^2
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

.getSimulatedStageRatesMultiArm <- function(design, directionUpper, piVector, piControl,
        plannedSubjects, typeOfSelection, effectMeasure, adaptations, epsilonValue, rValue,
        threshold, allocationRatioPlanned, minNumberOfSubjectsPerStage,
        maxNumberOfSubjectsPerStage, conditionalPower, piTreatmentsH1, piControlH1,
        calcSubjectsFunction, calcSubjectsFunctionIsUserDefined, selectArmsFunction) {
    kMax <- length(plannedSubjects)
    gMax <- length(piVector)
    simRates <- matrix(NA_real_, nrow = gMax + 1, ncol = kMax)
    overallEffectSizes <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    subjectsPerStage <- matrix(NA_real_, nrow = gMax + 1, ncol = kMax)
    testStatistics <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    overallTestStatistics <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    separatePValues <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    conditionalCriticalValue <- rep(NA_real_, kMax - 1)
    conditionalPowerPerStage <- rep(NA_real_, kMax)
    selectedArms <- matrix(FALSE, nrow = gMax + 1, ncol = kMax)
    selectedArms[, 1] <- TRUE
    adjustedPValues <- rep(NA_real_, kMax)
    overallRates <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    overallRatesControl <- rep(NA_real_, kMax)

    if (.isTrialDesignFisher(design)) {
        weights <- .getWeightsFisher(design)
    } else if (.isTrialDesignInverseNormal(design)) {
        weights <- .getWeightsInverseNormal(design)
    }

    for (k in (1:kMax)) {
        if (k == 1) {
            subjectsPerStage[gMax + 1, k] <- trunc(plannedSubjects[k] / allocationRatioPlanned[k])
        } else {
            subjectsPerStage[gMax + 1, k] <- trunc((plannedSubjects[k] - plannedSubjects[k - 1]) / allocationRatioPlanned[k])
        }
        simRates[gMax + 1, k] <- stats::rbinom(1, subjectsPerStage[gMax + 1, k], piControl) / subjectsPerStage[gMax + 1, k]

        for (treatmentArm in (1:gMax)) {
            if (selectedArms[treatmentArm, k]) {
                if (k == 1) {
                    subjectsPerStage[treatmentArm, k] <- plannedSubjects[k]
                } else {
                    subjectsPerStage[treatmentArm, k] <- plannedSubjects[k] - plannedSubjects[k - 1]
                }

                simRates[treatmentArm, k] <- stats::rbinom(1, subjectsPerStage[treatmentArm, k], piVector[treatmentArm]) /
                    subjectsPerStage[treatmentArm, k]

                rm <- (subjectsPerStage[treatmentArm, k] * simRates[treatmentArm, k] +
                    subjectsPerStage[gMax + 1, k] * simRates[gMax + 1, k]) /
                    (subjectsPerStage[treatmentArm, k] + subjectsPerStage[gMax + 1, k])

                if (simRates[treatmentArm, k] - simRates[gMax + 1, k] == 0) {
                    testStatistics[treatmentArm, k] <- 0
                } else {
                    testStatistics[treatmentArm, k] <- (2 * directionUpper - 1) *
                        (simRates[treatmentArm, k] - simRates[gMax + 1, k]) /
                        sqrt(rm * (1 - rm) * (1 / subjectsPerStage[treatmentArm, k] + 1 / subjectsPerStage[gMax + 1, k]))
                }

                separatePValues[treatmentArm, k] <- 1 - stats::pnorm(testStatistics[treatmentArm, k])

                overallRates[treatmentArm, k] <- subjectsPerStage[treatmentArm, 1:k] %*%
                    simRates[treatmentArm, 1:k] / sum(subjectsPerStage[treatmentArm, 1:k])

                overallRatesControl[k] <- subjectsPerStage[gMax + 1, 1:k] %*%
                    simRates[gMax + 1, 1:k] / sum(subjectsPerStage[gMax + 1, 1:k])

                overallEffectSizes[treatmentArm, k] <- (2 * directionUpper - 1) *
                    (overallRates[treatmentArm, k] - overallRatesControl[k])

                rmOverall <- (allocationRatioPlanned[k] * overallRates[treatmentArm, k] +
                    overallRatesControl[k]) / (allocationRatioPlanned[k] + 1)

                if (overallEffectSizes[treatmentArm, k] == 0) {
                    overallTestStatistics[treatmentArm, k] <- 0
                } else {
                    overallTestStatistics[treatmentArm, k] <- overallEffectSizes[treatmentArm, k] /
                        sqrt(rmOverall * (1 - rmOverall) * sqrt(1 / sum(subjectsPerStage[treatmentArm, 1:k]) +
                            1 / sum(subjectsPerStage[gMax + 1, 1:k])))
                }
            }
        }

        if (k < kMax) {
            if (colSums(selectedArms)[k] == 1) {
                break
            }

            # Bonferroni adjustment
            adjustedPValues[k] <- min(min(separatePValues[, k], na.rm = TRUE) *
                (colSums(selectedArms)[k] - 1), 1 - 1e-12)

            # conditional critical value to reject the null hypotheses at the next stage of the trial
            if (.isTrialDesignConditionalDunnett(design)) {
                conditionalCriticalValue[k] <- (.getOneMinusQNorm(design$alpha) -
                    .getOneMinusQNorm(adjustedPValues[k]) * sqrt(design$informationAtInterim)) /
                    sqrt(1 - design$informationAtInterim)
            } else {
                if (.isTrialDesignFisher(design)) {
                    conditionalCriticalValue[k] <- .getOneMinusQNorm(min((design$criticalValues[k + 1] /
                        prod(adjustedPValues[1:k]^weights[1:k]))^(1 / weights[k + 1]), 1 - 1e-7))
                } else {
                    if (design$criticalValues[k + 1] >= 6) {
                        conditionalCriticalValue[k] <- Inf
                    } else {
                        conditionalCriticalValue[k] <- (design$criticalValues[k + 1] * sqrt(design$informationRates[k + 1]) -
                            .getOneMinusQNorm(adjustedPValues[1:k]) %*% weights[1:k]) /
                            sqrt(design$informationRates[k + 1] - design$informationRates[k])
                    }
                }
            }

            if (adaptations[k]) {
                if (effectMeasure == "testStatistic") {
                    selectedArms[, k + 1] <- (selectedArms[, k] & .selectTreatmentArms(
                        k, overallTestStatistics[, k] + runif(gMax, -1e-05, 1e-05),
                        typeOfSelection, epsilonValue, rValue, threshold, selectArmsFunction
                    ))
                } else if (effectMeasure == "effectEstimate") {
                    selectedArms[, k + 1] <- (selectedArms[, k] & .selectTreatmentArms(
                        k, overallEffectSizes[, k] + runif(gMax, -1e-05, 1e-05),
                        typeOfSelection, epsilonValue, rValue, threshold, selectArmsFunction
                    ))
                }

                newSubjects <- calcSubjectsFunction(
                    stage = k + 1, # to be consistent with non-multiarm situation, cf. line 39
                    directionUpper = directionUpper,
                    conditionalPower = conditionalPower,
                    conditionalCriticalValue = conditionalCriticalValue,
                    plannedSubjects = plannedSubjects,
                    allocationRatioPlanned = allocationRatioPlanned,
                    selectedArms = selectedArms,
                    piTreatmentsH1 = piTreatmentsH1,
                    piControlH1 = piControlH1,
                    overallRates = overallRates,
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
                    plannedSubjects[(k + 1):kMax] <- ceiling(sum(subjectsPerStage[gMax + 1, 1:k] *
                        allocationRatioPlanned[1:k]) + cumsum(rep(newSubjects, kMax - k)))
                }
            } else {
                selectedArms[, k + 1] <- selectedArms[, k]
            }

            if (is.na(piControlH1)) {
                piAssumedControlH1 <- overallRatesControl[k]
            } else {
                piAssumedControlH1 <- piControlH1
            }

            if (is.na(piTreatmentsH1)) {
                if (directionUpper) {
                    piAssumedH1 <- min(overallRates[selectedArms[1:gMax, k], k], na.rm = TRUE)
                } else {
                    piAssumedH1 <- max(overallRates[selectedArms[1:gMax, k], k], na.rm = TRUE)
                }
            } else {
                piAssumedH1 <- piTreatmentsH1
            }

            pim <- (allocationRatioPlanned[k] * piAssumedH1 + piAssumedControlH1) / (1 + allocationRatioPlanned[k])

            if (piAssumedH1 * (1 - piAssumedH1) + piAssumedControlH1 * (1 - piAssumedControlH1) == 0) {
                thetaStandardized <- 0
            } else {
                thetaStandardized <- sqrt(allocationRatioPlanned[k]) / (1 + allocationRatioPlanned[k]) *
                    ((piAssumedH1 - piAssumedControlH1) * sqrt(1 + allocationRatioPlanned[k]) /
                        sqrt(piAssumedH1 * (1 - piAssumedH1) + allocationRatioPlanned[k] *
                            piAssumedControlH1 * (1 - piAssumedControlH1)) +
                        sign(piAssumedH1 - piAssumedControlH1) * conditionalCriticalValue[k] *
                            (1 - sqrt(pim * (1 - pim) + allocationRatioPlanned[k] * pim * (1 - pim)) /
                                sqrt(piAssumedH1 * (1 - piAssumedH1) + allocationRatioPlanned[k] *
                                    piAssumedControlH1 * (1 - piAssumedControlH1))) *
                            sqrt((1 + allocationRatioPlanned[k]) / (plannedSubjects[k + 1] - plannedSubjects[k]))
                    )
            }

            thetaStandardized <- (2 * directionUpper - 1) * thetaStandardized

            conditionalPowerPerStage[k] <- 1 - stats::pnorm(conditionalCriticalValue[k] -
                thetaStandardized * sqrt((1 + allocationRatioPlanned[k]) / allocationRatioPlanned[k]) *
                    sqrt(plannedSubjects[k + 1] - plannedSubjects[k]))
        }
    }
    return(list(
        subjectsPerStage = subjectsPerStage,
        allocationRatioPlanned = allocationRatioPlanned,
        overallEffectSizes = overallEffectSizes,
        testStatistics = testStatistics,
        directionUpper = directionUpper,
        overallTestStatistics = overallTestStatistics,
        overallRatesControl = overallRatesControl,
        overallRates = overallRates,
        separatePValues = separatePValues,
        conditionalCriticalValue = conditionalCriticalValue,
        conditionalPowerPerStage = conditionalPowerPerStage,
        selectedArms = selectedArms
    ))
}

#'
#' @title
#' Get Simulation Multi-Arm Rates
#'
#' @description
#' Returns the simulated power, stopping and selection probabilities, conditional power,
#' and expected sample size for testing rates in a multi-arm treatment groups testing situation.
#'
#' @param piMaxVector Range of assumed probabilities for the treatment group with
#'        highest response for \code{"linear"} and \code{"sigmoidEmax"} model,
#'        default is \code{seq(0, 1, 0.2)}.
#' @param piControl If specified, the assumed probability in the control arm
#'        for simulation and under which the sample size recalculation is performed.
#' @param piTreatmentsH1 If specified, the assumed probability in the active treatment arm(s)
#'        under which the sample size recalculation is performed.
#' @param piControlH1 If specified, the assumed probability in the reference group
#'        (if different from \code{piControl}) for which the conditional power was calculated.
#' @inheritParams param_intersectionTest_MultiArm
#' @inheritParams param_typeOfSelection
#' @inheritParams param_effectMeasure
#' @inheritParams param_adaptations
#' @inheritParams param_threshold
#' @inheritParams param_effectMatrix
#' @inheritParams param_activeArms
#' @inheritParams param_successCriterion
#' @inheritParams param_typeOfShape
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
#' @inheritParams param_selectArmsFunction
#' @inheritParams param_rValue
#' @inheritParams param_epsilonValue
#' @inheritParams param_gED50
#' @inheritParams param_slope
#' @inheritParams param_seed
#' @inheritParams param_three_dots
#' @inheritParams param_showStatistics
#'
#' @details
#' At given design the function simulates the power, stopping probabilities,
#' selection probabilities, and expected sample size at given number of subjects,
#' parameter configuration, and treatment arm selection rule in the multi-arm situation.
#' An allocation ratio can be specified referring to the ratio of number of
#' subjects in the active treatment groups as compared to the control group.
#'
#' The definition of \code{pi1H1} and/or \code{piControl} makes only sense if \code{kMax} > 1
#' and if \code{conditionalPower}, \code{minNumberOfSubjectsPerStage}, and
#' \code{maxNumberOfSubjectsPerStage} (or \code{calcSubjectsFunction}) are defined.
#'
#' \code{calcSubjectsFunction}\cr
#' This function returns the number of subjects at given conditional power and
#' conditional critical value for specified testing situation.
#' The function might depend on the variables
#' \code{stage},
#' \code{selectedArms},
#' \code{directionUpper},
#' \code{plannedSubjects},
#' \code{allocationRatioPlanned},
#' \code{minNumberOfSubjectsPerStage},
#' \code{maxNumberOfSubjectsPerStage},
#' \code{conditionalPower},
#' \code{conditionalCriticalValue},
#' \code{overallRates},
#' \code{overallRatesControl},
#' \code{piTreatmentsH1}, and
#' \code{piControlH1}.
#' The function has to contain the three-dots argument '...' (see examples).
#'
#' @template return_object_simulation_results
#' @template how_to_get_help_for_generics
#'
#' @template examples_get_simulation_multiarm_rates
#'
#' @export
#'
getSimulationMultiArmRates <- function(design = NULL, ...,
        activeArms = 3L, # C_ACTIVE_ARMS_DEFAULT
        effectMatrix = NULL,
        typeOfShape = c("linear", "sigmoidEmax", "userDefined"), # C_TYPE_OF_SHAPE_DEFAULT
        piMaxVector = seq(0.2, 0.5, 0.1), # C_PI_1_DEFAULT
        piControl = 0.2, # C_PI_2_DEFAULT
        gED50 = NA_real_,
        slope = 1,
        intersectionTest = c("Dunnett", "Bonferroni", "Simes", "Sidak", "Hierarchical"), # C_INTERSECTION_TEST_MULTIARMED_DEFAULT
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
        piTreatmentsH1 = NA_real_,
        piControlH1 = NA_real_,
        maxNumberOfIterations = 1000L, # C_MAX_SIMULATION_ITERATIONS_DEFAULT
        seed = NA_real_,
        calcSubjectsFunction = NULL,
        selectArmsFunction = NULL,
        showStatistics = FALSE) {
    if (is.null(design)) {
        design <- .getDefaultDesign(..., type = "simulation")
        .warnInCaseOfUnknownArguments(
            functionName = "getSimulationMultiArmRates",
            ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
                design,
                powerCalculationEnabled = TRUE
            ), "showStatistics"), ...
        )
    } else {
        .assertIsTrialDesignInverseNormalOrFisherOrConditionalDunnett(design)
        .warnInCaseOfUnknownArguments(
            functionName = "getSimulationMultiArmRates",
            ignore = "showStatistics", ...
        )
        .warnInCaseOfTwoSidedPowerArgument(...)
    }

    .assertIsOneSidedDesign(design, designType = "multi-arm", engineType = "simulation")

    calcSubjectsFunctionIsUserDefined <- !is.null(calcSubjectsFunction)

    simulationResults <- .createSimulationResultsMultiArmObject(
        design                      = design,
        activeArms                  = activeArms,
        effectMatrix                = effectMatrix,
        typeOfShape                 = typeOfShape,
        piMaxVector                 = piMaxVector, # rates only
        piControl                   = piControl, # rates only
        gED50                       = gED50,
        slope                       = slope,
        intersectionTest            = intersectionTest,
        directionUpper              = directionUpper, # rates + survival only
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
        piTreatmentsH1              = piTreatmentsH1, # rates only
        piControlH1                 = piControlH1, # rates only
        maxNumberOfIterations       = maxNumberOfIterations,
        seed                        = seed,
        calcSubjectsFunction        = calcSubjectsFunction, # means + rates only
        selectArmsFunction          = selectArmsFunction,
        showStatistics              = showStatistics,
        endpoint                    = "rates"
    )

    design <- simulationResults$.design
    successCriterion <- simulationResults$successCriterion
    effectMeasure <- simulationResults$effectMeasure
    adaptations <- simulationResults$adaptations
    gMax <- activeArms
    kMax <- simulationResults$.design$kMax
    intersectionTest <- simulationResults$intersectionTest
    typeOfSelection <- simulationResults$typeOfSelection
    effectMatrix <- t(simulationResults$effectMatrix)
    piMaxVector <- simulationResults$piMaxVector # rates only
    piControl <- simulationResults$piControl # rates only
    piTreatmentsH1 <- simulationResults$piTreatmentsH1 # rates only
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

    if (.isTrialDesignConditionalDunnett(design)) {
        criticalValuesDunnett <- .getCriticalValuesDunnettForSimulation(
            alpha = design$alpha, indices = indices,
            allocationRatioPlanned = allocationRatioPlanned
        )
    }

    cols <- length(piMaxVector)

    simulatedSelections <- array(0, dim = c(kMax, cols, gMax + 1))
    simulatedRejections <- array(0, dim = c(kMax, cols, gMax))
    simulatedNumberOfActiveArms <- matrix(0, nrow = kMax, ncol = cols)
    simulatedSubjectsPerStage <- array(0, dim = c(kMax, cols, gMax + 1))
    simulatedSuccessStopping <- matrix(0, nrow = kMax, ncol = cols)
    simulatedFutilityStopping <- matrix(0, nrow = kMax - 1, ncol = cols)
    simulatedConditionalPower <- matrix(0, nrow = kMax, ncol = cols)
    simulatedRejectAtLeastOne <- rep(0, cols)
    expectedNumberOfSubjects <- rep(0, cols)
    iterations <- matrix(0, nrow = kMax, ncol = cols)

    len <- maxNumberOfIterations * kMax * gMax * cols

    dataIterationNumber <- rep(NA_real_, len)
    dataStageNumber <- rep(NA_real_, len)
    dataArmNumber <- rep(NA_real_, len)
    dataAlternative <- rep(NA_real_, len)
    dataEffect <- rep(NA_real_, len)
    dataSubjectsControlArm <- rep(NA_real_, len)
    dataSubjectsActiveArm <- rep(NA_real_, len)
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

    index <- 1

    for (i in 1:cols) {
        for (j in 1:maxNumberOfIterations) {
            stageResults <- .getSimulatedStageRatesMultiArm(
                design = design,
                directionUpper = directionUpper,
                piVector = effectMatrix[i, ],
                piControl = piControl,
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
                piTreatmentsH1 = piTreatmentsH1,
                piControlH1 = piControlH1,
                calcSubjectsFunction = calcSubjectsFunction,
                calcSubjectsFunctionIsUserDefined = calcSubjectsFunctionIsUserDefined,
                selectArmsFunction = selectArmsFunction
            )

            if (.isTrialDesignConditionalDunnett(design)) {
                closedTest <- .performClosedConditionalDunnettTestForSimulation(
                    stageResults = stageResults,
                    design = design, indices = indices,
                    criticalValuesDunnett = criticalValuesDunnett, successCriterion = successCriterion
                )
            } else {
                closedTest <- .performClosedCombinationTestForSimulationMultiArm(
                    stageResults = stageResults,
                    design = design, indices = indices,
                    intersectionTest = intersectionTest, successCriterion = successCriterion
                )
            }

            rejectAtSomeStage <- FALSE
            rejectedArmsBefore <- rep(FALSE, gMax)

            for (k in 1:kMax) {
                simulatedRejections[k, i, ] <- simulatedRejections[k, i, ] +
                    (closedTest$rejected[, k] & closedTest$selectedArms[1:gMax, k] | rejectedArmsBefore)
                simulatedSelections[k, i, ] <- simulatedSelections[k, i, ] + closedTest$selectedArms[, k]

                simulatedNumberOfActiveArms[k, i] <- simulatedNumberOfActiveArms[k, i] + sum(closedTest$selectedArms[, k])

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

                for (g in (1:(gMax + 1))) {
                    if (!is.na(stageResults$subjectsPerStage[g, k])) {
                        simulatedSubjectsPerStage[k, i, g] <- simulatedSubjectsPerStage[k, i, g] +
                            stageResults$subjectsPerStage[g, k]
                    }
                }

                for (g in 1:gMax) {
                    dataIterationNumber[index] <- j
                    dataStageNumber[index] <- k
                    dataArmNumber[index] <- g
                    dataAlternative[index] <- piMaxVector[i]
                    dataEffect[index] <- effectMatrix[i, g]
                    dataSubjectsControlArm[index] <- round(stageResults$subjectsPerStage[gMax + 1, k], 1)
                    dataSubjectsActiveArm[index] <- round(stageResults$subjectsPerStage[g, k], 1)
                    dataNumberOfSubjects[index] <- round(sum(stageResults$subjectsPerStage[, k], na.rm = TRUE), 1)
                    dataNumberOfCumulatedSubjects[index] <- round(sum(stageResults$subjectsPerStage[, 1:k], na.rm = TRUE), 1)
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
                        closedTest$selectedArms[1:gMax, k] | rejectedArmsBefore)) {
                    simulatedRejectAtLeastOne[i] <- simulatedRejectAtLeastOne[i] + 1
                    rejectAtSomeStage <- TRUE
                }

                if ((k < kMax) && (closedTest$successStop[k] || closedTest$futilityStop[k])) {
                    # rejected hypotheses remain rejected also in case of early stopping
                    simulatedRejections[(k + 1):kMax, i, ] <- simulatedRejections[(k + 1):kMax, i, ] +
                        matrix((closedTest$rejected[, k] & closedTest$selectedArms[1:gMax, k] | rejectedArmsBefore),
                            kMax - k, gMax,
                            byrow = TRUE
                        )
                    break
                }

                rejectedArmsBefore <- closedTest$rejected[, k] & closedTest$selectedArms[1:gMax, k] | rejectedArmsBefore
            }
        }

        simulatedSubjectsPerStage[is.na(simulatedSubjectsPerStage)] <- 0

        simulatedSubjectsPerStage[, i, ] <- simulatedSubjectsPerStage[, i, ] / iterations[, i]

        if (kMax > 1) {
            simulatedRejections[2:kMax, i, ] <- simulatedRejections[2:kMax, i, ] -
                simulatedRejections[1:(kMax - 1), i, ]

            stopping <- cumsum(simulatedSuccessStopping[1:(kMax - 1), i] +
                simulatedFutilityStopping[, i]) / maxNumberOfIterations

            expectedNumberOfSubjects[i] <- sum(simulatedSubjectsPerStage[1, i, ] +
                t(1 - stopping) %*% simulatedSubjectsPerStage[2:kMax, i, ])
        } else {
            expectedNumberOfSubjects[i] <- sum(simulatedSubjectsPerStage[1, i, ])
        }
    }

    simulatedConditionalPower[1, ] <- NA_real_
    if (kMax > 1) {
        simulatedConditionalPower[2:kMax, ] <- as.matrix(simulatedConditionalPower[2:kMax, ] / iterations[2:kMax, ])
    }
    simulationResults$rejectAtLeastOne <- simulatedRejectAtLeastOne / maxNumberOfIterations
    simulationResults$numberOfActiveArms <- simulatedNumberOfActiveArms / iterations - 1

    simulationResults$selectedArms <- simulatedSelections / maxNumberOfIterations
    simulationResults$rejectedArmsPerStage <- simulatedRejections / maxNumberOfIterations
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

    if (any(simulationResults$rejectedArmsPerStage < 0)) {
        stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "internal error, simulation not possible due to numerical overflow")
    }

    data <- data.frame(
        iterationNumber = dataIterationNumber,
        stageNumber = dataStageNumber,
        armNumber = dataArmNumber,
        piMax = dataAlternative,
        effect = dataEffect,
        numberOfSubjects = dataNumberOfSubjects,
        numberOfCumulatedSubjects = dataNumberOfCumulatedSubjects,
        subjectsControlArm = dataSubjectsControlArm,
        subjectsActiveArm = dataSubjectsActiveArm,
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
