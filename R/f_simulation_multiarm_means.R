## |
## |  *Simulation of multi-arm design with continuous data*
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
## |  Last changed: $Date: 2024-12-10 09:39:04 +0100 (Tue, 10 Dec 2024) $
## |  Last changed by: $Author: wassmer $
## |

#' @include f_simulation_multiarm.R
NULL

.getSimulationMeansMultiArmStageSubjects <- function(..., stage,
        conditionalPower,
        conditionalCriticalValue,
        plannedSubjects,
        allocationRatioPlanned,
        selectedArms,
        thetaH1,
        overallEffects,
        stDevH1,
        minNumberOfSubjectsPerStage,
        maxNumberOfSubjectsPerStage) {
    stage <- stage - 1 # to be consistent with non-multiarm situation
    gMax <- nrow(overallEffects)

    if (!is.na(conditionalPower)) {
        if (any(selectedArms[1:gMax, stage + 1], na.rm = TRUE)) {
            if (is.na(thetaH1)) {
                thetaStandardized <- max(min(overallEffects[
                    selectedArms[1:gMax, stage + 1], stage
                ] / stDevH1, na.rm = TRUE), 1e-07)
            } else {
                thetaStandardized <- max(thetaH1 / stDevH1, 1e-07)
            }

            if (conditionalCriticalValue[stage] > 8) {
                newSubjects <- maxNumberOfSubjectsPerStage[stage + 1]
            } else {
                newSubjects <- (1 + allocationRatioPlanned[stage]) *
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

.getSimulatedStageMeansMultiArm <- function(...,
        design, muVector, stDev, plannedSubjects, typeOfSelection, effectMeasure,
        adaptations, epsilonValue, rValue, threshold, allocationRatioPlanned,
        minNumberOfSubjectsPerStage, maxNumberOfSubjectsPerStage, conditionalPower,
        thetaH1, stDevH1, calcSubjectsFunction, calcSubjectsFunctionIsUserDefined, selectArmsFunction) {
    kMax <- length(plannedSubjects)
    gMax <- length(muVector)
    simMeans <- matrix(NA_real_, nrow = gMax + 1, ncol = kMax)
    overallEffects <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    subjectsPerStage <- matrix(NA_real_, nrow = gMax + 1, ncol = kMax)
    testStatistics <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    overallTestStatistics <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    separatePValues <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    conditionalCriticalValue <- rep(NA_real_, kMax - 1)
    conditionalPowerPerStage <- rep(NA_real_, kMax)
    selectedArms <- matrix(FALSE, nrow = gMax + 1, ncol = kMax)
    selectedArms[, 1] <- TRUE
    adjustedPValues <- rep(NA_real_, kMax)

    if (.isTrialDesignFisher(design)) {
        weights <- .getWeightsFisher(design)
    } else if (.isTrialDesignInverseNormal(design)) {
        weights <- .getWeightsInverseNormal(design)
    }

    for (k in 1:kMax) {
        if (k == 1) {
            subjectsPerStage[gMax + 1, k] <- plannedSubjects[k] / allocationRatioPlanned[k]
        } else {
            subjectsPerStage[gMax + 1, k] <- (plannedSubjects[k] - plannedSubjects[k - 1]) / allocationRatioPlanned[k]
        }

        if (subjectsPerStage[gMax + 1, k] > 0) {
            simMeans[gMax + 1, k] <- stats::rnorm(1, 0, stDev / sqrt(subjectsPerStage[gMax + 1, k]))
        }

        for (treatmentArm in 1:gMax) {
            if (selectedArms[treatmentArm, k]) {
                if (k == 1) {
                    subjectsPerStage[treatmentArm, k] <- plannedSubjects[k]
                } else {
                    subjectsPerStage[treatmentArm, k] <- plannedSubjects[k] - plannedSubjects[k - 1]
                }

                if (subjectsPerStage[treatmentArm, k] > 0) {
                    simMeans[treatmentArm, k] <- stats::rnorm(
                        1, muVector[treatmentArm],
                        stDev / sqrt(subjectsPerStage[treatmentArm, k])
                    )
                    testStatistics[treatmentArm, k] <- (simMeans[treatmentArm, k] - simMeans[gMax + 1, k]) /
                        (stDev * sqrt(1 / subjectsPerStage[treatmentArm, k] + 1 / subjectsPerStage[gMax + 1, k]))
                }

                overallEffects[treatmentArm, k] <-
                    subjectsPerStage[treatmentArm, 1:k] %*% simMeans[treatmentArm, 1:k] /
                    sum(subjectsPerStage[treatmentArm, 1:k]) -
                    subjectsPerStage[gMax + 1, 1:k] %*% simMeans[gMax + 1, 1:k] / sum(subjectsPerStage[gMax + 1, 1:k])

                overallTestStatistics[treatmentArm, k] <- overallEffects[treatmentArm, k] /
                    (stDev * sqrt(1 / sum(subjectsPerStage[treatmentArm, 1:k]) + 1 / sum(subjectsPerStage[gMax + 1, 1:k])))

                separatePValues[treatmentArm, k] <- 1 - stats::pnorm(testStatistics[treatmentArm, k])
            }
        }

        if (k < kMax) {
            if (colSums(selectedArms)[k] == 1) {
                break
            }

            # Bonferroni adjustment
            adjustedPValues[k] <- min(min(separatePValues[, k], na.rm = TRUE) *
                (colSums(selectedArms)[k] - 1), 1 - 1e-07)

            # conditional critical value to reject the null hypotheses at the next stage of the trial
            if (.isTrialDesignConditionalDunnett(design)) {
                conditionalCriticalValue[k] <- (.getOneMinusQNorm(design$alpha) -
                    .getOneMinusQNorm(adjustedPValues[k]) * sqrt(design$informationAtInterim)) /
                    sqrt(1 - design$informationAtInterim)
            } else {
                criticalValues <- .getCriticalValues(design)
                if (.isTrialDesignFisher(design)) {
                    conditionalCriticalValue[k] <- .getOneMinusQNorm(min((criticalValues[k + 1] /
                        prod(adjustedPValues[1:k]^weights[1:k]))^(1 / weights[k + 1]), 1 - 1e-07))
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
                    conditionalPower = conditionalPower,
                    conditionalCriticalValue = conditionalCriticalValue,
                    plannedSubjects = plannedSubjects,
                    allocationRatioPlanned = allocationRatioPlanned,
                    selectedArms = selectedArms,
                    thetaH1 = thetaH1,
                    stDevH1 = stDevH1,
                    overallEffects = overallEffects
                )

                if (effectMeasure == "testStatistic") {
                    selectArmsFunctionArgs$effectVector <- overallTestStatistics[, k]
                } else if (effectMeasure == "effectEstimate") {
                    selectArmsFunctionArgs$effectVector <- overallEffects[, k]
                }

                args <- list(
                    typeOfSelection = typeOfSelection,
                    epsilonValue = epsilonValue,
                    rValue = rValue,
                    threshold = threshold,
                    selectArmsFunction = selectArmsFunction,
                    selectArmsFunctionArgs = selectArmsFunctionArgs,
                    survival = FALSE
                )

                selectedArms[, k + 1] <- (selectedArms[, k] & do.call(.selectTreatmentArms, args))

                newSubjects <- calcSubjectsFunction(
                    stage = k + 1, # to be consistent with non-multiarm situation, cf. line 37
                    conditionalPower = conditionalPower,
                    conditionalCriticalValue = conditionalCriticalValue,
                    plannedSubjects = plannedSubjects,
                    allocationRatioPlanned = allocationRatioPlanned,
                    selectedArms = selectedArms,
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
                    plannedSubjects[(k + 1):kMax] <- sum(subjectsPerStage[gMax + 1, 1:k] *
                        allocationRatioPlanned[1:k]) + cumsum(rep(newSubjects, kMax - k))
                }
            } else {
                selectedArms[, k + 1] <- selectedArms[, k]
            }

            if (is.na(thetaH1)) {
                thetaStandardized <- max(min(overallEffects[selectedArms[1:gMax, k], k] / stDevH1, na.rm = TRUE), 1e-12)
            } else {
                thetaStandardized <- thetaH1 / stDevH1
            }

            conditionalPowerPerStage[k] <- 1 - stats::pnorm(conditionalCriticalValue[k] -
                thetaStandardized * sqrt(plannedSubjects[k + 1] - plannedSubjects[k]) *
                    sqrt(1 / (1 + allocationRatioPlanned[k])))
        }
    }

    return(list(
        subjectsPerStage = subjectsPerStage,
        allocationRatioPlanned = allocationRatioPlanned,
        overallEffects = overallEffects,
        testStatistics = testStatistics,
        overallTestStatistics = overallTestStatistics,
        separatePValues = separatePValues,
        conditionalCriticalValue = conditionalCriticalValue,
        conditionalPowerPerStage = conditionalPowerPerStage,
        selectedArms = selectedArms
    ))
}

#'
#' @title
#' Get Simulation Multi-Arm Means
#'
#' @description
#' Returns the simulated power, stopping and selection probabilities, conditional power,
#' and expected sample size for testing means in a multi-arm treatment groups testing situation.
#'
#' @param muMaxVector Range of effect sizes for the treatment group with highest response
#'        for \code{"linear"} and \code{"sigmoidEmax"} model, default is \code{seq(0, 1, 0.2)}.
#' @inheritParams param_intersectionTest_MultiArm
#' @inheritParams param_typeOfSelection
#' @inheritParams param_effectMeasure
#' @inheritParams param_adaptations
#' @inheritParams param_threshold
#' @inheritParams param_effectMatrix
#' @inheritParams param_stDevSimulation
#' @inheritParams param_activeArms
#' @inheritParams param_successCriterion
#' @inheritParams param_typeOfShapeMeans
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
#' @inheritParams param_selectArmsFunction
#' @inheritParams param_rValue
#' @inheritParams param_epsilonValue
#' @inheritParams param_gED50
#' @inheritParams param_slope
#' @inheritParams param_doseLevels
#' @inheritParams param_seed
#' @inheritParams param_three_dots
#' @inheritParams param_showStatistics
#'
#' @details
#' At given design the function simulates the power, stopping probabilities, selection probabilities,
#' and expected sample size at given number of subjects, parameter configuration, and treatment arm
#' selection rule in the multi-arm situation.
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
#' \code{selectedArms},
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
#' @template examples_get_simulation_multiarm_means
#'
#' @export
#'
getSimulationMultiArmMeans <- function(design = NULL, ...,
        activeArms = 3L, # C_ACTIVE_ARMS_DEFAULT
        effectMatrix = NULL,
        typeOfShape = c("linear", "sigmoidEmax", "userDefined"), # C_TYPE_OF_SHAPE_DEFAULT
        muMaxVector = seq(0, 1, 0.2), # C_ALTERNATIVE_POWER_SIMULATION_DEFAULT
        gED50 = NA_real_,
        slope = 1,
        doseLevels = NA_real_,
        intersectionTest = c("Dunnett", "Bonferroni", "Simes", "Sidak", "Hierarchical"), # C_INTERSECTION_TEST_MULTIARMED_DEFAULT
        stDev = 1, # C_STDEV_DEFAULT
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
        selectArmsFunction = NULL,
        showStatistics = FALSE) {
    if (is.null(design)) {
        design <- .getDefaultDesign(..., type = "simulation")
        .warnInCaseOfUnknownArguments(
            functionName = "getSimulationMultiArmMeans",
            ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
                design,
                powerCalculationEnabled = TRUE
            ), "showStatistics"), ...
        )
    } else {
        .assertIsTrialDesignInverseNormalOrFisherOrConditionalDunnett(design)
        .warnInCaseOfUnknownArguments(
            functionName = "getSimulationMultiArmMeans",
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
        muMaxVector                 = muMaxVector, # means only
        gED50                       = gED50,
        slope                       = slope,
        doseLevels                  = doseLevels,
        intersectionTest            = intersectionTest,
        stDev                       = stDev, # means only
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
        selectArmsFunction          = selectArmsFunction,
        showStatistics              = showStatistics,
        endpoint                    = "means"
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
    muMaxVector <- simulationResults$muMaxVector # means only
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

    if (.isTrialDesignConditionalDunnett(design)) {
        criticalValuesDunnett <- .getCriticalValuesDunnettForSimulation(
            alpha = design$alpha, indices = indices, allocationRatioPlanned = allocationRatioPlanned
        )
    }

    cols <- length(muMaxVector)

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
    dataSuccessStop <- rep(NA, len)
    dataFutilityStop <- rep(NA, len)
    dataTestStatistics <- rep(NA_real_, len)
    dataConditionalCriticalValue <- rep(NA_real_, len)
    dataConditionalPowerAchieved <- rep(NA_real_, len)
    dataEffectEstimate <- rep(NA_real_, len)
    dataPValuesSeparate <- rep(NA_real_, len)

    if (is.na(stDevH1)) {
        stDevH1 <- stDev
    }

    index <- 1
    for (i in 1:cols) {
        for (j in 1:maxNumberOfIterations) {
            stageResults <- .getSimulatedStageMeansMultiArm(
                design = design,
                muVector = effectMatrix[i, ],
                stDev = stDev,
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
                    dataAlternative[index] <- muMaxVector[i]
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
                    dataEffectEstimate[index] <- stageResults$overallEffects[g, k]
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
    simulationResults$numberOfActiveArms <- simulatedNumberOfActiveArms / iterations - 1

    simulationResults$rejectAtLeastOne <- simulatedRejectAtLeastOne / maxNumberOfIterations
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
        stop(
            C_EXCEPTION_TYPE_RUNTIME_ISSUE,
            "internal error, simulation not possible due to numerical overflow"
        )
    }

    data <- data.frame(
        iterationNumber = dataIterationNumber,
        stageNumber = dataStageNumber,
        armNumber = dataArmNumber,
        muMax = dataAlternative,
        effect = dataEffect,
        numberOfSubjects = dataNumberOfSubjects,
        numberOfCumulatedSubjects = dataNumberOfCumulatedSubjects,
        subjectsControlArm = dataSubjectsControlArm,
        subjectsActiveArm = dataSubjectsActiveArm,
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
