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
## |  File version: $Revision: 6275 $
## |  Last changed: $Date: 2022-06-09 13:35:36 +0200 (Thu, 09 Jun 2022) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_simulation_multiarm.R
NULL

.getSimulationSurvivalMultiArmStageEvents <- function(...,
        stage,
        directionUpper,
        conditionalPower,
        conditionalCriticalValue,
        plannedEvents,
        allocationRatioPlanned,
        selectedArms,
        thetaH1,
        overallEffects,
        minNumberOfEventsPerStage,
        maxNumberOfEventsPerStage) {
    stage <- stage - 1 # to be consistent with non-multiarm situation
    gMax <- nrow(overallEffects)

    if (!is.na(conditionalPower)) {
        if (any(selectedArms[1:gMax, stage + 1], na.rm = TRUE)) {
            if (is.na(thetaH1)) {
                if (directionUpper) {
                    thetaStandardized <- log(max(min(
                        overallEffects[selectedArms[1:gMax, stage + 1], stage],
                        na.rm = TRUE
                    ), 1 + 1e-07))
                } else {
                    thetaStandardized <- log(min(max(
                        overallEffects[selectedArms[1:gMax, stage + 1], stage],
                        na.rm = TRUE
                    ), 1 - 1e-07))
                }
            } else {
                if (directionUpper) {
                    thetaStandardized <- log(max(thetaH1, 1 + 1e-07))
                } else {
                    thetaStandardized <- log(min(thetaH1, 1 - 1e-07))
                }
            }
            if (conditionalCriticalValue[stage] > 8) {
                newEvents <- maxNumberOfEventsPerStage[stage + 1]
            } else {
                newEvents <- (1 + allocationRatioPlanned)^2 / allocationRatioPlanned *
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

# Correlation matrix according to Deng et al. (2019) accounting for alternative:
.getCholeskyDecomposition <- function(allocationRatioPlanned,
        selectedArms,
        k,
        omegaVector) {
    selectedArmsVec <- selectedArms[, k]
    probabilityVector <- allocationRatioPlanned * omegaVector[selectedArmsVec] /
        (1 + allocationRatioPlanned * sum(omegaVector[selectedArmsVec]))
    armsSelected <- sum(selectedArmsVec)
    p0 <- 1 / (1 + allocationRatioPlanned * sum(omegaVector[selectedArmsVec]))
    covMatrix <- matrix(rep(1 / p0, armsSelected^2), ncol = armsSelected, nrow = armsSelected)
    diag(covMatrix) <- 1 / p0 + 1 / probabilityVector
    corrMatrix <- cov2cor(covMatrix)
    choleskyDecomposition <- chol(corrMatrix)

    return(choleskyDecomposition)
}

.getSimulatedStageSurvivalMultiArm <- function(...,
        design,
        directionUpper,
        omegaVector,
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
        selectArmsFunction,
        choleskyDecompositionList,
        choleskyDecomposition = NULL) {
    kMax <- length(plannedEvents)
    gMax <- length(omegaVector)
    simSurvival <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    overallEffects <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    eventsPerStage <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    singleEventsPerStage <- matrix(NA_real_, nrow = gMax + 1, ncol = kMax)
    testStatistics <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    overallTestStatistics <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    separatePValues <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    conditionalCriticalValue <- rep(NA_real_, kMax - 1)
    conditionalPowerPerStage <- rep(NA_real_, kMax)
    selectedArms <- matrix(FALSE, nrow = gMax, ncol = kMax)
    selectedArms[, 1] <- TRUE
    adjustedPValues <- rep(NA_real_, kMax)

    if (.isTrialDesignFisher(design)) {
        weights <- .getWeightsFisher(design)
    } else if (.isTrialDesignInverseNormal(design)) {
        weights <- .getWeightsInverseNormal(design)
    }

    for (k in 1:kMax) {
        for (treatmentArm in 1:gMax) {
            if (selectedArms[treatmentArm, k]) {
                if (k == 1) {
                    eventsPerStage[treatmentArm, k] <- plannedEvents[k] *
                        (allocationRatioPlanned * omegaVector[treatmentArm] + 1) /
                        (allocationRatioPlanned * sum(omegaVector) + 1)
                } else {
                    eventsPerStage[treatmentArm, k] <- (plannedEvents[k] - plannedEvents[k - 1]) *
                        (allocationRatioPlanned * omegaVector[treatmentArm] + 1) /
                        (allocationRatioPlanned * sum(omegaVector[selectedArms[, k]]) + 1)
                }
                if (eventsPerStage[treatmentArm, k] > 0) {
                    testStatistics[treatmentArm, k] <- stats::rnorm(1, 0, 1)
                }
            }
        }

        if (is.null(choleskyDecomposition)) {
            key <- paste0(selectedArms[, k], collapse = "")
            choleskyDecomposition <- choleskyDecompositionList[[key]]
            if (is.null(choleskyDecomposition)) {
                choleskyDecomposition <- .getCholeskyDecomposition(allocationRatioPlanned, selectedArms, k, omegaVector)
                choleskyDecompositionList[[key]] <- choleskyDecomposition
            }

            testStatistics[!is.na(testStatistics[, k]), k] <-
                t(choleskyDecomposition) %*% testStatistics[!is.na(testStatistics[, k]), k]
        } else {
            testStatistics[!is.na(testStatistics[, k]), k] <-
                t(choleskyDecomposition[1:sum(selectedArms[, k]), 1:sum(selectedArms[, k])]) %*%
                testStatistics[!is.na(testStatistics[, k]), k]
        }

        for (treatmentArm in 1:gMax) {
            if (selectedArms[treatmentArm, k]) {
                testStatistics[treatmentArm, k] <- testStatistics[treatmentArm, k] +
                    (2 * directionUpper - 1) * log(omegaVector[treatmentArm]) * sqrt(eventsPerStage[treatmentArm, k]) *
                        sqrt(allocationRatioPlanned) / (1 + allocationRatioPlanned)

                separatePValues[treatmentArm, k] <- 1 - stats::pnorm(testStatistics[treatmentArm, k])

                overallTestStatistics[treatmentArm, k] <- sqrt(eventsPerStage[treatmentArm, 1:k]) %*%
                    testStatistics[treatmentArm, 1:k] / sqrt(sum(eventsPerStage[treatmentArm, 1:k]))

                overallEffects[treatmentArm, k] <- exp((2 * directionUpper - 1) * overallTestStatistics[treatmentArm, k] *
                    (1 + allocationRatioPlanned) / sqrt(allocationRatioPlanned) /
                    sqrt(sum(eventsPerStage[treatmentArm, 1:k])))
            }
        }

        if (k < kMax) {
            if (colSums(selectedArms)[k] == 0) {
                break
            }

            # Bonferroni adjustment
            adjustedPValues[k] <- min(min(separatePValues[, k], na.rm = TRUE) * (colSums(selectedArms)[k]), 1 - 1e-7)

            # conditional critical value to reject the null hypotheses at the next stage of the trial
            if (.isTrialDesignConditionalDunnett(design)) {
                conditionalCriticalValue[k] <- (.getOneMinusQNorm(design$alpha) - .getOneMinusQNorm(adjustedPValues[k]) *
                    sqrt(design$informationAtInterim)) / sqrt(1 - design$informationAtInterim)
            } else {
                if (.isTrialDesignFisher(design)) {
                    conditionalCriticalValue[k] <- .getOneMinusQNorm(min((design$criticalValues[k + 1] /
                        prod(adjustedPValues[1:k]^weights[1:k]))^(1 / weights[k + 1]), 1 - 1e-7))
                } else {
                    conditionalCriticalValue[k] <- (design$criticalValues[k + 1] * sqrt(design$informationRates[k + 1]) -
                        .getOneMinusQNorm(adjustedPValues[1:k]) %*% weights[1:k]) /
                        sqrt(design$informationRates[k + 1] - design$informationRates[k])
                }
            }

            if (adaptations[k]) {
                if (effectMeasure == "testStatistic") {
                    selectedArms[, k + 1] <- (selectedArms[, k] & .selectTreatmentArms(k, overallTestStatistics[, k],
                        typeOfSelection, epsilonValue, rValue, threshold, selectArmsFunction,
                        survival = TRUE
                    ))
                } else if (effectMeasure == "effectEstimate") {
                    if (directionUpper) {
                        selectedArms[, k + 1] <- (selectedArms[, k] & .selectTreatmentArms(k, overallEffects[, k],
                            typeOfSelection, epsilonValue, rValue, threshold, selectArmsFunction,
                            survival = TRUE
                        ))
                    } else {
                        selectedArms[, k + 1] <- (selectedArms[, k] & .selectTreatmentArms(k, 1 / overallEffects[, k],
                            typeOfSelection, epsilonValue, rValue, 1 / threshold, selectArmsFunction,
                            survival = TRUE
                        ))
                    }
                }

                newEvents <- calcEventsFunction(
                    stage = k + 1, # to be consistent with non-multiarm situation, cf. line 38
                    directionUpper = directionUpper,
                    conditionalPower = conditionalPower,
                    conditionalCriticalValue = conditionalCriticalValue,
                    plannedEvents = plannedEvents,
                    allocationRatioPlanned = allocationRatioPlanned,
                    selectedArms = selectedArms,
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
                selectedArms[, k + 1] <- selectedArms[, k]
            }

            if (is.na(thetaH1)) {
                thetaStandardized <- log(min(overallEffects[selectedArms[1:gMax, k], k], na.rm = TRUE))
            } else {
                thetaStandardized <- log(thetaH1)
            }
            thetaStandardized <- (2 * directionUpper - 1) * thetaStandardized

            conditionalPowerPerStage[k] <- 1 - stats::pnorm(conditionalCriticalValue[k] -
                thetaStandardized * sqrt(plannedEvents[k + 1] - plannedEvents[k]) *
                    sqrt(allocationRatioPlanned) / (1 + allocationRatioPlanned))
        }
    }

    return(list(
        eventsPerStage = eventsPerStage,
        plannedEvents = plannedEvents,
        allocationRatioPlanned = allocationRatioPlanned,
        overallEffects = overallEffects,
        testStatistics = testStatistics,
        overallTestStatistics = overallTestStatistics,
        separatePValues = separatePValues,
        conditionalCriticalValue = conditionalCriticalValue,
        conditionalPowerPerStage = conditionalPowerPerStage,
        selectedArms = selectedArms,
        choleskyDecompositionList = choleskyDecompositionList
    ))
}

#'
#' @title
#' Get Simulation Multi-Arm Survival
#'
#' @description
#' Returns the simulated power, stopping and selection probabilities, conditional power, and
#' expected sample size for testing hazard ratios in a multi-arm treatment groups testing situation.
#' In contrast to \code{getSimulationSurvival()} (where survival times are simulated), normally
#' distributed logrank test statistics are simulated.
#'
#' @param omegaMaxVector Range of hazard ratios with highest response for \code{"linear"} and
#'        \code{"sigmoidEmax"} model, default is \code{seq(1, 2.6, 0.4)}.
#' @inheritParams param_intersectionTest_MultiArm
#' @inheritParams param_typeOfSelection
#' @inheritParams param_effectMeasure
#' @inheritParams param_adaptations
#' @inheritParams param_threshold
#' @inheritParams param_effectMatrix
#' @inheritParams param_activeArms
#' @inheritParams param_successCriterion
#' @param correlationComputation If \code{correlationComputation = "alternative"},
#'        for simulating log-rank statistics in the many-to-one design, a correlation
#'        matrix according to Deng et al. (Biometrics, 2019) accounting for the
#'        respective alternative is used;
#'        if \code{correlationComputation = "null"}, a constant correlation matrix valid
#'        under the null, i.e., not accounting for the alternative is used,
#'         default is \code{"alternative"}.
#' @inheritParams param_typeOfShape
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
#' An allocation ratio can be specified referring to the ratio of number of subjects
#' in the active treatment groups as compared to the control group.
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
#' \code{selectedArms},
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
#' @template examples_get_simulation_multiarm_survival
#'
#' @export
#'
getSimulationMultiArmSurvival <- function(design = NULL, ...,
        activeArms = 3L, # C_ACTIVE_ARMS_DEFAULT
        effectMatrix = NULL,
        typeOfShape = c("linear", "sigmoidEmax", "userDefined"), # C_TYPE_OF_SHAPE_DEFAULT
        omegaMaxVector = seq(1, 2.6, 0.4), # C_RANGE_OF_HAZARD_RATIOS_DEFAULT
        gED50 = NA_real_,
        slope = 1,
        intersectionTest = c("Dunnett", "Bonferroni", "Simes", "Sidak", "Hierarchical"), # C_INTERSECTION_TEST_MULTIARMED_DEFAULT
        directionUpper = TRUE, # C_DIRECTION_UPPER_DEFAULT
        adaptations = NA,
        typeOfSelection = c("best", "rBest", "epsilon", "all", "userDefined"), # C_TYPE_OF_SELECTION_DEFAULT
        effectMeasure = c("effectEstimate", "testStatistic"), # C_EFFECT_MEASURE_DEFAULT
        successCriterion = c("all", "atLeastOne"), # C_SUCCESS_CRITERION_DEFAULT
        correlationComputation = c("alternative", "null"),
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
        selectArmsFunction = NULL,
        showStatistics = FALSE) {
    if (is.null(design)) {
        design <- .getDefaultDesign(..., type = "simulation")
        .warnInCaseOfUnknownArguments(
            functionName = "getSimulationMultiArmSurvival",
            ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design,
                powerCalculationEnabled = TRUE
            ), "showStatistics"), ...
        )
    } else {
        .assertIsTrialDesignInverseNormalOrFisherOrConditionalDunnett(design)
        .warnInCaseOfUnknownArguments(functionName = "getSimulationMultiArmSurvival", ignore = "showStatistics", ...)
        .warnInCaseOfTwoSidedPowerArgument(...)
    }

    .assertIsOneSidedDesign(design, designType = "multi-arm", engineType = "simulation")

    correlationComputation <- match.arg(correlationComputation)

    calcEventsFunctionIsUserDefined <- !is.null(calcEventsFunction)

    simulationResults <- .createSimulationResultsMultiArmObject(
        design                      = design,
        activeArms                  = activeArms,
        effectMatrix                = effectMatrix,
        typeOfShape                 = typeOfShape,
        omegaMaxVector              = omegaMaxVector, # survival only
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
        plannedEvents               = plannedEvents, # survival only
        allocationRatioPlanned      = allocationRatioPlanned,
        minNumberOfEventsPerStage   = minNumberOfEventsPerStage, # survival only
        maxNumberOfEventsPerStage   = maxNumberOfEventsPerStage, # survival only
        conditionalPower            = conditionalPower,
        thetaH1                     = thetaH1, # means + survival only
        maxNumberOfIterations       = maxNumberOfIterations,
        seed                        = seed,
        calcEventsFunction          = calcEventsFunction, # survival only
        selectArmsFunction          = selectArmsFunction,
        showStatistics              = showStatistics,
        endpoint                    = "survival"
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
    omegaMaxVector <- simulationResults$omegaMaxVector # survival only
    thetaH1 <- simulationResults$thetaH1 # means + survival only
    plannedEvents <- simulationResults$plannedEvents # survival only
    conditionalPower <- simulationResults$conditionalPower
    minNumberOfEventsPerStage <- simulationResults$minNumberOfEventsPerStage # survival only
    maxNumberOfEventsPerStage <- simulationResults$maxNumberOfEventsPerStage # survival only
    allocationRatioPlanned <- simulationResults$allocationRatioPlanned
    calcEventsFunction <- simulationResults$calcEventsFunction

    simulationResults$correlationComputation <- correlationComputation
    if (correlationComputation != "alternative") {
        simulationResults$.setParameterType("correlationComputation", C_PARAM_USER_DEFINED)
    }

    indices <- .getIndicesOfClosedHypothesesSystemForSimulation(gMax = gMax)

    if (.isTrialDesignConditionalDunnett(design)) {
        criticalValuesDunnett <- .getCriticalValuesDunnettForSimulation(
            alpha = design$alpha, indices = indices,
            allocationRatioPlanned = allocationRatioPlanned
        )
    }

    cols <- length(omegaMaxVector)

    simulatedSelections <- array(0, dim = c(kMax, cols, gMax))
    simulatedRejections <- array(0, dim = c(kMax, cols, gMax))
    simulatedNumberOfActiveArms <- matrix(0, nrow = kMax, ncol = cols)
    simulatedSingleEventsPerStage <- array(0, dim = c(kMax, cols, gMax + 1))
    simulatedOverallEventsPerStage <- matrix(0, nrow = kMax, ncol = cols)
    simulatedSuccessStopping <- matrix(0, nrow = kMax, ncol = cols)
    simulatedFutilityStopping <- matrix(0, nrow = kMax - 1, ncol = cols)
    simulatedConditionalPower <- matrix(0, nrow = kMax, ncol = cols)
    simulatedRejectAtLeastOne <- rep(0, cols)
    expectedNumberOfEvents <- rep(0, cols)
    iterations <- matrix(0, nrow = kMax, ncol = cols)
    probabilityVector <- rep(NA_real_, cols)

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

    choleskyDecomposition <- NULL

    if (correlationComputation == "null") {
        # not accounting for alternative
        corrMatrix <- matrix(rep(allocationRatioPlanned / (1 + allocationRatioPlanned), gMax^2), ncol = gMax, nrow = gMax)
        diag(corrMatrix) <- 1
        choleskyDecomposition <- chol(corrMatrix)
    }

    index <- 1
    for (i in 1:cols) {
        choleskyDecompositionList <- list()

        for (j in 1:maxNumberOfIterations) {
            stageResults <- .getSimulatedStageSurvivalMultiArm(
                design = design,
                directionUpper = directionUpper,
                omegaVector = effectMatrix[i, ],
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
                selectArmsFunction = selectArmsFunction,
                choleskyDecompositionList = choleskyDecompositionList,
                choleskyDecomposition = choleskyDecomposition
            )

            choleskyDecompositionList <- stageResults$choleskyDecompositionList

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

                if (k == 1) {
                    simulatedOverallEventsPerStage[k, i] <- simulatedOverallEventsPerStage[k, i] +
                        stageResults$plannedEvents[k]
                    for (g in 1:gMax) {
                        if (closedTest$selectedArms[g, k]) {
                            simulatedSingleEventsPerStage[k, i, g] <- simulatedSingleEventsPerStage[k, i, g] +
                                stageResults$plannedEvents[k] *
                                    allocationRatioPlanned * effectMatrix[i, g] / (1 + allocationRatioPlanned *
                                        sum(effectMatrix[i, closedTest$selectedArms[, k]]))
                        }
                    }
                    simulatedSingleEventsPerStage[k, i, gMax + 1] <- simulatedSingleEventsPerStage[k, i, gMax + 1] +
                        stageResults$plannedEvents[k] /
                            (1 + allocationRatioPlanned * sum(effectMatrix[i, closedTest$selectedArms[, k]]))
                } else {
                    simulatedOverallEventsPerStage[k, i] <- simulatedOverallEventsPerStage[k, i] +
                        stageResults$plannedEvents[k] - stageResults$plannedEvents[k - 1]
                    for (g in 1:gMax) {
                        if (closedTest$selectedArms[g, k]) {
                            simulatedSingleEventsPerStage[k, i, g] <- simulatedSingleEventsPerStage[k, i, g] +
                                (stageResults$plannedEvents[k] - stageResults$plannedEvents[k - 1]) *
                                    allocationRatioPlanned * effectMatrix[i, g] / (1 + allocationRatioPlanned *
                                        sum(effectMatrix[i, closedTest$selectedArms[, k]]))
                        }
                    }
                    simulatedSingleEventsPerStage[k, i, gMax + 1] <- simulatedSingleEventsPerStage[k, i, gMax + 1] +
                        (stageResults$plannedEvents[k] - stageResults$plannedEvents[k - 1]) /
                            (1 + allocationRatioPlanned * sum(effectMatrix[i, closedTest$selectedArms[, k]]))
                }

                for (g in 1:gMax) {
                    dataIterationNumber[index] <- j
                    dataStageNumber[index] <- k
                    dataArmNumber[index] <- g
                    dataAlternative[index] <- omegaMaxVector[i]
                    dataEffect[index] <- effectMatrix[i, g]
                    dataNumberOfEvents[index] <- round(stageResults$eventsPerStage[g, k], 1)
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

                if (!rejectAtSomeStage && any(closedTest$rejected[, k] & closedTest$selectedArms[1:gMax, k] | rejectedArmsBefore)) {
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

        simulatedOverallEventsPerStage[, i] <- simulatedOverallEventsPerStage[, i] / iterations[, i]
        simulatedSingleEventsPerStage[, i, ] <- simulatedSingleEventsPerStage[, i, ] / iterations[, i]

        if (kMax > 1) {
            simulatedRejections[2:kMax, i, ] <- simulatedRejections[2:kMax, i, ] - simulatedRejections[1:(kMax - 1), i, ]

            stopping <- cumsum(simulatedSuccessStopping[1:(kMax - 1), i] + simulatedFutilityStopping[, i]) / maxNumberOfIterations

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
    simulationResults$numberOfActiveArms <- simulatedNumberOfActiveArms / iterations

    simulationResults$selectedArms <- simulatedSelections / maxNumberOfIterations
    simulationResults$rejectedArmsPerStage <- simulatedRejections / maxNumberOfIterations
    simulationResults$successPerStage <- simulatedSuccessStopping / maxNumberOfIterations
    simulationResults$futilityPerStage <- simulatedFutilityStopping / maxNumberOfIterations
    simulationResults$futilityStop <- base::colSums(simulatedFutilityStopping / maxNumberOfIterations)
    if (kMax > 1) {
        simulationResults$earlyStop <- simulationResults$futilityPerStage + simulationResults$successPerStage[1:(kMax - 1), ]
        simulationResults$conditionalPowerAchieved <- simulatedConditionalPower
    }

    simulationResults$eventsPerStage <- .convertStageWiseToOverallValues(simulatedSingleEventsPerStage)
    for (g in (1:gMax)) {
        simulationResults$eventsPerStage[, , g] <- simulationResults$eventsPerStage[, , g] +
            simulationResults$eventsPerStage[, , gMax + 1]
    }
    simulationResults$eventsPerStage <- .removeLastEntryFromArray(simulationResults$eventsPerStage)

    simulationResults$singleNumberOfEventsPerStage <- simulatedSingleEventsPerStage
    simulationResults$.setParameterType("singleNumberOfEventsPerStage", C_PARAM_GENERATED)

    simulationResults$expectedNumberOfEvents <- expectedNumberOfEvents

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
