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
#' @title
#' Get Simulation Multi-Arm Survival
#'
#' @description
#' Returns the simulated power, stopping and selection probabilities, conditional power, and
#' expected sample size for testing hazard ratios in a multi-arm treatment groups testing situation.
#'
#' Depending on \code{simulationType}, either a patient-wise survival simulation is performed,
#' a patient-wise basic approximation is used, or normally distributed log-rank test statistics
#' are simulated. The default \code{simulationType = "auto"} chooses the simulation approach
#' automatically based on the explicitly specified arguments.
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
#' @inheritParams param_correlationComputation
#' @inheritParams param_typeOfShapeSurvival
#' @inheritParams param_design_with_default
#' @inheritParams param_directionUpper
#' @inheritParams param_allocationRatioPlanned
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
#' @inheritParams param_estimatedTheta
#' @inheritParams param_thetaH1
#' @inheritParams param_plannedEvents
#' @inheritParams param_maxNumberOfIterations
#' @inheritParams param_calcEventsFunction
#' @inheritParams param_selectArmsFunction
#' @inheritParams param_rValue
#' @inheritParams param_epsilonValue
#' @inheritParams param_gED50
#' @inheritParams param_slope
#' @inheritParams param_kappa
#' @inheritParams param_doseLevels
#' @inheritParams param_seed
#' @inheritParams param_three_dots
#' @inheritParams param_showStatistics
#' @inheritParams param_simulationType_multiarm_survival
#' @param piControl The assumed probability in the control arm, default is \code{0.2}.
#'
#' @details
#' At given design the function simulates the analysis times, power, stopping
#' probabilities, expected sample size and events, and selection probabilities,
#' at given number of subjects, events, parameter configuration, and treatment
#' arm selection rule in the multi-arm survival design situation. An allocation
#' ratio can be specified referring to the ratio of number of subjects in the
#' active treatment groups as compared to the control group.
#'
#' If \code{simulationType = "patientWise"}, patient-wise survival data are simulated based on
#' the specified accrual, event-time, and dropout assumptions. If
#' \code{simulationType = "patientWiseBasic"}, a simplified patient-wise survival approach is
#' used. If \code{simulationType = "testStatisticBased"}, normally distributed log-rank test
#' statistics are simulated instead. The default \code{simulationType = "auto"} selects the
#' simulation approach automatically based on the explicitly specified arguments and uses the
#' legacy test-statistic-based approach for backward compatibility if no patient-wise-specific
#' arguments are provided.
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
getSimulationMultiArmSurvival <- function(
        design = NULL,
        ...,
        simulationType = c("auto", "patientWise", "testStatisticBased", "patientWiseBasic"),
        activeArms = NA_integer_,
        piControl = 0.2,
        effectMatrix = NULL,
        typeOfShape = c("linear", "sigmoidEmax", "userDefined"),
        omegaMaxVector = seq(1, 2.6, 0.4),
        kappa = 1,
        gED50 = NA_real_,
        slope = 1,
        doseLevels = NA_real_,
        eventTime = 12,
        accrualTime = c(0, 12),
        accrualIntensity = 0.1,
        accrualIntensityType = c("auto", "absolute", "relative"),
        dropoutRate1 = 0,
        dropoutRate2 = 0,
        dropoutTime = 12,
        maxNumberOfSubjects = NA_real_,
        intersectionTest = c("Dunnett", "Bonferroni", "Simes", "Sidak", "Hierarchical"),
        directionUpper = NA,
        adaptations = NA,
        typeOfSelection = c("best", "rBest", "epsilon", "all", "userDefined"),
        effectMeasure = c("effectEstimate", "testStatistic"),
        successCriterion = c("all", "atLeastOne"),
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
        maxNumberOfIterations = 1000L,
        seed = NA_real_,
        calcEventsFunction = NULL,
        selectArmsFunction = NULL,
        showStatistics = FALSE) {
    simulationType <- match.arg(simulationType)

    callArgs <- names(as.list(match.call(expand.dots = FALSE)))
    hasArg <- function(x) x %in% callArgs

    usesBasicOnlyArgs <- hasArg("correlationComputation")
    usesPatientWiseOnlyArgs <- any(vapply(
        c(
            "piControl", "eventTime", "accrualTime", "accrualIntensity",
            "accrualIntensityType", "dropoutRate1", "dropoutRate2",
            "dropoutTime", "maxNumberOfSubjects", "kappa"
        ),
        hasArg,
        FALSE
    ))

    if (simulationType == "auto") {
        if (usesBasicOnlyArgs && usesPatientWiseOnlyArgs) {
            stopConflictingArguments("arguments from both 'testStatisticBased' and 'patientWise' simulation types were specified",
                functionName = "getSimulationMultiArmSurvival", parameter = "testStatisticBased", relatedParameter = "patientWise"
            )
        }
        if (usesBasicOnlyArgs) {
            simulationType <- "testStatisticBased"
        } else if (usesPatientWiseOnlyArgs) {
            simulationType <- "patientWise"
        } else {
            simulationType <- "testStatisticBased"
        }
    }

    if (simulationType == "testStatisticBased") {
        if (usesPatientWiseOnlyArgs) {
            stopIllegalArgument("patient-wise simulation arguments cannot be specified if 'simulationType' = \"testStatisticBased\"",
                functionName = "getSimulationMultiArmSurvival", parameter = "simulationType", value = simulationType
            )
        }

        message(
            "Note: 'simulationType' = \"testStatisticBased\" simulates normally distributed log-rank test statistics instead of patient-wise survival data. ",
            "To simulate patient-wise survival data, specify 'simulationType' = \"patientWise\" and the corresponding arguments."
        )

        return(getSimulationMultiArmSurvivalBasic(
            design = design,
            ...,
            activeArms = activeArms,
            effectMatrix = effectMatrix,
            typeOfShape = typeOfShape,
            omegaMaxVector = omegaMaxVector,
            gED50 = gED50,
            slope = slope,
            doseLevels = doseLevels,
            intersectionTest = intersectionTest,
            directionUpper = directionUpper,
            adaptations = adaptations,
            typeOfSelection = typeOfSelection,
            effectMeasure = effectMeasure,
            successCriterion = successCriterion,
            correlationComputation = correlationComputation,
            epsilonValue = epsilonValue,
            rValue = rValue,
            threshold = threshold,
            plannedEvents = plannedEvents,
            allocationRatioPlanned = allocationRatioPlanned,
            minNumberOfEventsPerStage = minNumberOfEventsPerStage,
            maxNumberOfEventsPerStage = maxNumberOfEventsPerStage,
            conditionalPower = conditionalPower,
            thetaH1 = thetaH1,
            maxNumberOfIterations = maxNumberOfIterations,
            seed = seed,
            calcEventsFunction = calcEventsFunction,
            selectArmsFunction = selectArmsFunction,
            showStatistics = showStatistics
        ))
    }

    if (simulationType %in% c("patientWise", "patientWiseBasic")) {
        if (usesBasicOnlyArgs) {
            stopIllegalArgument("'correlationComputation' cannot be specified if 'simulationType' = \"patientWise\" or \"patientWiseBasic\"",
                functionName = "getSimulationMultiArmSurvival", parameter = "correlationComputation", relatedParameter = "simulationType",
                value = correlationComputation
            )
        }
        return(getSimulationMultiArmSurvivalPatientWise(
            design = design,
            ...,
            activeArms = activeArms,
            piControl = piControl,
            effectMatrix = effectMatrix,
            typeOfShape = typeOfShape,
            omegaMaxVector = omegaMaxVector,
            kappa = kappa,
            gED50 = gED50,
            slope = slope,
            doseLevels = doseLevels,
            eventTime = eventTime,
            accrualTime = accrualTime,
            accrualIntensity = accrualIntensity,
            accrualIntensityType = accrualIntensityType,
            dropoutRate1 = dropoutRate1,
            dropoutRate2 = dropoutRate2,
            dropoutTime = dropoutTime,
            maxNumberOfSubjects = maxNumberOfSubjects,
            intersectionTest = intersectionTest,
            directionUpper = directionUpper,
            adaptations = adaptations,
            typeOfSelection = typeOfSelection,
            effectMeasure = effectMeasure,
            successCriterion = successCriterion,
            epsilonValue = epsilonValue,
            rValue = rValue,
            threshold = threshold,
            plannedEvents = plannedEvents,
            allocationRatioPlanned = allocationRatioPlanned,
            minNumberOfEventsPerStage = minNumberOfEventsPerStage,
            maxNumberOfEventsPerStage = maxNumberOfEventsPerStage,
            conditionalPower = conditionalPower,
            thetaH1 = thetaH1,
            maxNumberOfIterations = maxNumberOfIterations,
            seed = seed,
            calcEventsFunction = calcEventsFunction,
            selectArmsFunction = selectArmsFunction,
            showStatistics = showStatistics,
            cppEnabled = ifelse(identical(simulationType, "patientWiseBasic"), FALSE, TRUE)
        ))
    }
}

#'
#' @title
#' Get Simulation Multi-Arm Survival
#'
#' @description
#' Returns the simulated power, stopping and selection probabilities, conditional power, and
#' expected sample size for testing hazard ratios in a multi-arm treatment groups testing situation.
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
#' @inheritParams param_typeOfShapeSurvival
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
#' @inheritParams param_selectArmsFunction
#' @inheritParams param_rValue
#' @inheritParams param_epsilonValue
#' @inheritParams param_gED50
#' @inheritParams param_slope
#' @inheritParams param_kappa
#' @inheritParams param_doseLevels
#' @inheritParams param_seed
#' @inheritParams param_three_dots
#' @inheritParams param_showStatistics
#'
#' @details
#' At given design the function simulates the analysis times, power, stopping
#' probabilities, expected sample size and events, and selection probabilities,
#' at given number of subjects, events, parameter configuration, and treatment
#' arm selection rule in the multi-arm survival design situation. An allocation
#' ratio can be specified referring to the ratio of number of subjects in the
#' active treatment groups as compared to the control group.
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
#' @keywords internal
#'
getSimulationMultiArmSurvivalPatientWise <- function(
        design = NULL,
        ...,
        activeArms = NA_integer_,
        # C_ACTIVE_ARMS_DEFAULT = 3L
        piControl = NA_real_,
        effectMatrix = NULL,
        typeOfShape = c("linear", "sigmoidEmax", "userDefined"),
        # C_TYPE_OF_SHAPE_DEFAULT
        omegaMaxVector = seq(1, 2.6, 0.4),
        # C_RANGE_OF_HAZARD_RATIOS_DEFAULT
        kappa = 1,
        gED50 = NA_real_,
        slope = 1,
        doseLevels = NA_real_,
        eventTime = 12,
        # C_EVENT_TIME_DEFAULT
        accrualTime = c(0, 12),
        # C_ACCRUAL_TIME_DEFAULT
        accrualIntensity = 0.1,
        # C_ACCRUAL_INTENSITY_DEFAULT
        accrualIntensityType = c("auto", "absolute", "relative"),
        dropoutRate1 = 0,
        # C_DROP_OUT_RATE_DEFAULT
        dropoutRate2 = 0,
        # C_DROP_OUT_RATE_DEFAULT
        dropoutTime = 12,
        # C_DROP_OUT_TIME_DEFAULT
        maxNumberOfSubjects = NA_real_,
        intersectionTest = c("Dunnett", "Bonferroni", "Simes", "Sidak", "Hierarchical"),
        # C_INTERSECTION_TEST_MULTIARMED_DEFAULT
        directionUpper = NA,
        # C_DIRECTION_UPPER_DEFAULT
        adaptations = NA,
        typeOfSelection = c("best", "rBest", "epsilon", "all", "userDefined"),
        # C_TYPE_OF_SELECTION_DEFAULT
        effectMeasure = c("effectEstimate", "testStatistic"),
        # C_EFFECT_MEASURE_DEFAULT
        successCriterion = c("all", "atLeastOne"),
        # C_SUCCESS_CRITERION_DEFAULT
        epsilonValue = NA_real_,
        rValue = NA_real_,
        threshold = -Inf,
        plannedEvents = NA_real_,
        allocationRatioPlanned = NA_real_,
        minNumberOfEventsPerStage = NA_real_,
        maxNumberOfEventsPerStage = NA_real_,
        conditionalPower = NA_real_,
        thetaH1 = NA_real_,
        maxNumberOfIterations = 1000L,
        # C_MAX_SIMULATION_ITERATIONS_DEFAULT
        seed = NA_real_,
        calcEventsFunction = NULL,
        selectArmsFunction = NULL,
        showStatistics = FALSE,
        cppEnabled = TRUE) {
    if (is.null(design)) {
        design <- .getDefaultDesign(directionUpper = directionUpper, type = "simulation", ...)
        .warnInCaseOfUnknownArguments(
            functionName = "getSimulationMultiArmSurvival",
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
        .assertIsTrialDesignInverseNormalOrFisherOrConditionalDunnettOrFixed(design)
        .warnInCaseOfUnknownArguments(
            functionName = "getSimulationMultiArmSurvival",
            ignore = "showStatistics",
            ...
        )
        .warnInCaseOfTwoSidedPowerArgument(...)
    }
    .assertIsOneSidedDesign(
        design,
        designType = "multi-arm",
        engineType = "simulation"
    )
    .assertIsValidMaxNumberOfSubjects(
        maxNumberOfSubjects,
        naAllowed = TRUE
    )

    if (isFALSE(cppEnabled)) {
        message(
            "Note: 'simulationType' = \"patientWiseBasic\" simulates patient-wise survival data using R code instead of C++ code. ",
            "This approach is less efficient and should only be used for testing purposes. ",
            "To perform a more efficient patient-wise simulation, specify 'simulationType' = \"patientWise\"."
        )
    }

    calcEventsFunctionIsUserDefined <- !is.null(calcEventsFunction)

    directionUpper <- .assertIsValidDirectionUpper(
        directionUpper,
        design,
        objectType = "power",
        userFunctionCallEnabled = TRUE
    )

    if (length(allocationRatioPlanned) != 1) {
        stopIllegalArgument("'allocationRatioPlanned' (", .arrayToString(allocationRatioPlanned), ") ", "must have length 1",
            functionName = "getSimulationMultiArmSurvivalPatientWise", parameter = "allocationRatioPlanned",
            value = allocationRatioPlanned
        )
    }

    simulationResults <- .createSimulationResultsMultiArmObject(
        design = design,
        activeArms = activeArms,
        effectMatrix = effectMatrix,
        typeOfShape = typeOfShape,
        omegaMaxVector = omegaMaxVector,
        piControl = piControl,
        eventTime = eventTime,
        kappa = kappa,
        accrualTime = accrualTime,
        accrualIntensity = accrualIntensity,
        dropoutRate1 = dropoutRate1,
        dropoutRate2 = dropoutRate2,
        dropoutTime = dropoutTime,
        gED50 = gED50,
        slope = slope,
        doseLevels = doseLevels,
        intersectionTest = intersectionTest,
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
        selectArmsFunction = selectArmsFunction,
        showStatistics = showStatistics,
        endpoint = "survival",
        simulationType = "patientWise"
    )

    design <- simulationResults$.design
    successCriterion <- simulationResults$successCriterion
    effectMeasure <- simulationResults$effectMeasure
    adaptations <- simulationResults$adaptations
    gMax <- simulationResults$activeArms
    kMax <- simulationResults$.design$kMax
    intersectionTest <- simulationResults$intersectionTest
    typeOfSelection <- simulationResults$typeOfSelection
    effectMatrix <- t(simulationResults$effectMatrix)
    omegaMaxVector <- simulationResults$omegaMaxVector # survival only
    piControl <- simulationResults$piControl # rates + survival only
    thetaH1 <- simulationResults$thetaH1 # means + survival only
    plannedEvents <- simulationResults$plannedEvents # survival only
    maxNumberOfSubjects <- simulationResults$maxNumberOfSubjects # survival only
    conditionalPower <- simulationResults$conditionalPower
    minNumberOfEventsPerStage <- simulationResults$minNumberOfEventsPerStage # survival only
    maxNumberOfEventsPerStage <- simulationResults$maxNumberOfEventsPerStage # survival only
    allocationRatioPlanned <- simulationResults$allocationRatioPlanned
    calcEventsFunction <- simulationResults$calcEventsFunction

    indices <- .getIndicesOfClosedHypothesesSystemForSimulation(gMax = gMax)

    if (.isTrialDesignConditionalDunnett(design)) {
        criticalValuesDunnett <- .getCriticalValuesDunnettForSimulation(
            alpha = design$alpha,
            indices = indices,
            allocationRatioPlanned = allocationRatioPlanned
        )
    }

    cols <- length(omegaMaxVector)

    accrualSetup <- getAccrualTime(
        accrualTime = accrualTime,
        accrualIntensity = accrualIntensity,
        accrualIntensityType = accrualIntensityType,
        maxNumberOfSubjects = maxNumberOfSubjects
    )
    if (is.na(accrualSetup$maxNumberOfSubjects)) {
        if (accrualIntensity < 1L) {
            stopIllegalArgument("choose a 'accrualIntensity' > 1 or define 'maxNumberOfSubjects'",
                functionName = "getSimulationMultiArmSurvivalPatientWise",
                parameter = "accrualIntensity", relatedParameter = "maxNumberOfSubjects", value = accrualIntensity
            )
        }
        stopIllegalArgument("'maxNumberOfSubjects' must be defined",
            functionName = "getSimulationMultiArmSurvivalPatientWise", parameter = "maxNumberOfSubjects",
            value = maxNumberOfSubjects
        )
    }
    simulationResults$.accrualTime <- accrualSetup

    simulationResults$maxNumberOfSubjects <- accrualSetup$maxNumberOfSubjects
    simulationResults$.setParameterType("maxNumberOfSubjects", accrualSetup$.getParameterType("maxNumberOfSubjects"))

    allocationFraction <- .getFraction(allocationRatioPlanned)
    .warnInCaseOfExtremeAllocationRatios(allocationFraction[1], allocationFraction[2])

    simulationResults$accrualTime <- accrualSetup$.getAccrualTimeWithoutLeadingZero()
    simulationResults$.setParameterType("accrualTime", accrualSetup$.getParameterType("accrualTime"))

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

    loopResult <- if (isTRUE(cppEnabled)) {
        weights <- if (.isTrialDesignFixed(design) || .isTrialDesignInverseNormal(design)) {
            .getWeightsInverseNormal(design)
        } else if (.isTrialDesignFisher(design)) {
            .getWeightsFisher(design)
        } else if (.isTrialDesignConditionalDunnett(design)) {
            numeric(0) # not used
        }

        .performSimulationMultiArmSurvivalLoopCpp(
            cols = cols,
            maxNumberOfIterations = maxNumberOfIterations,
            design = design,
            weights = weights,
            directionUpper = directionUpper,
            effectMatrix = effectMatrix,
            omegaMaxVector = omegaMaxVector,
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
            selectArmsFunction = selectArmsFunction,
            indices = indices,
            intersectionTest = intersectionTest,
            criticalValuesDunnett = if (.isTrialDesignConditionalDunnett(design)) {
                criticalValuesDunnett
            } else {
                NULL
            },
            successCriterion = successCriterion,
            gMax = gMax,
            kMax = kMax
        )
    } else {
        .performSimulationMultiArmSurvivalLoop(
            cols = cols,
            maxNumberOfIterations = maxNumberOfIterations,
            design = design,
            directionUpper = directionUpper,
            effectMatrix = effectMatrix,
            omegaMaxVector = omegaMaxVector,
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
            selectArmsFunction = selectArmsFunction,
            indices = indices,
            intersectionTest = intersectionTest,
            criticalValuesDunnett = if (.isTrialDesignConditionalDunnett(design)) {
                criticalValuesDunnett
            } else {
                NULL
            },
            successCriterion = successCriterion,
            gMax = gMax,
            kMax = kMax
        )
    }

    # Extract results from the simulation
    simulatedNumberEventsNotAchieved <- loopResult$simulatedNumberEventsNotAchieved
    simulatedAnalysisTime <- loopResult$simulatedAnalysisTime
    simulatedNumberOfSubjects <- loopResult$simulatedNumberOfSubjects
    simulatedSelections <- loopResult$simulatedSelections
    simulatedRejections <- loopResult$simulatedRejections
    simulatedNumberOfActiveArms <- loopResult$simulatedNumberOfActiveArms
    simulatedSingleEventsPerStage <- loopResult$simulatedSingleEventsPerStage
    simulatedPlannedEvents <- loopResult$simulatedPlannedEvents
    simulatedSuccessStopping <- loopResult$simulatedSuccessStopping
    simulatedFutilityStopping <- loopResult$simulatedFutilityStopping
    simulatedConditionalPower <- loopResult$simulatedConditionalPower
    simulatedRejectAtLeastOne <- loopResult$simulatedRejectAtLeastOne
    expectedNumberOfEvents <- loopResult$expectedNumberOfEvents
    expectedNumberOfSubjects <- loopResult$expectedNumberOfSubjects
    expectedStudyDuration <- loopResult$expectedStudyDuration
    iterations <- loopResult$iterations

    simulationResults$numberOfSelectedArms <- simulatedNumberOfActiveArms
    .addDeprecatedFieldValues(
        simulationResults, "numberOfActiveArms",
        simulationResults$numberOfSelectedArms, "2026-07-13"
    )
    simulationResults$numberOfSubjects <- simulatedNumberOfSubjects
    simulationResults$analysisTime <- simulatedAnalysisTime
    simulationResults$eventsNotAchieved <- simulatedNumberEventsNotAchieved / maxNumberOfIterations
    simulationResults$rejectAtLeastOne <- simulatedRejectAtLeastOne / maxNumberOfIterations
    simulationResults$selectedArms <- simulatedSelections / maxNumberOfIterations
    simulationResults$rejectedArmsPerStage <- simulatedRejections / maxNumberOfIterations
    simulationResults$successPerStage <- simulatedSuccessStopping / maxNumberOfIterations
    simulationResults$futilityPerStage <- simulatedFutilityStopping / maxNumberOfIterations
    simulationResults$futilityStop <- base::colSums(simulatedFutilityStopping / maxNumberOfIterations)
    simulationResults$singleEventsPerArmAndStage <- simulatedSingleEventsPerStage
    simulationResults$cumulativeEventsPerStage <- simulatedPlannedEvents
    simulationResults$expectedNumberOfEvents <- expectedNumberOfEvents
    simulationResults$expectedNumberOfSubjects <- expectedNumberOfSubjects
    simulationResults$studyDuration <- expectedStudyDuration
    simulationResults$iterations <- iterations
    if (kMax > 1) {
        simulationResults$earlyStop <- simulationResults$futilityPerStage +
            simulationResults$successPerStage[1:(kMax - 1), ]
        simulationResults$conditionalPowerAchieved <- simulatedConditionalPower
    }
    if (gMax == 1) {
        simulationResults$.setParameterType("successPerStage", C_PARAM_NOT_APPLICABLE)
    }

    ## set parameter types in simulationResults
    if (kMax > 1) {
        simulationResults$.setParameterType("expectedNumberOfSubjects", C_PARAM_GENERATED)
        simulationResults$.setParameterType("expectedNumberOfEvents", C_PARAM_GENERATED)
        simulationResults$.setParameterType("studyDuration", C_PARAM_GENERATED)
    }
    simulationResults$.setParameterType(
        "selectedArms",
        ifelse(gMax == 1, C_PARAM_NOT_APPLICABLE, C_PARAM_GENERATED)
    )
    simulationResults$.setParameterType(
        "numberOfActiveArms",
        ifelse(gMax == 1, C_PARAM_NOT_APPLICABLE, C_PARAM_GENERATED)
    )
    simulationResults$.setParameterType("numberOfSubjects", C_PARAM_GENERATED)
    simulationResults$.setParameterType("analysisTime", C_PARAM_GENERATED)
    simulationResults$.setParameterType("singleEventsPerArmAndStage", C_PARAM_GENERATED)
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

    if (any(simulationResults$rejectedArmsPerStage < 0)) {
        stopRuntimeIssue("internal error, simulation not possible due to numerical overflow", functionName = "getSimulationMultiArmSurvivalPatientWise")
    }

    simulationResults$.data <- loopResult$data

    return(simulationResults)
}
