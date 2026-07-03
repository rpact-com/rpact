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
#' @keywords internal
#'
getSimulationEnrichmentSurvivalPatientWise <- function(design = NULL,
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
        .assertIsTrialDesignInverseNormalOrFisherOrFixed(design)
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
        endpoint = "survival",
        simulationType = "patientWise"
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

    if (.isTrialDesignFisher(design)) {
        weights <- .getWeightsFisher(design)
    } else if (.isTrialDesignFixed(design) || .isTrialDesignInverseNormal(design)) {
        weights <- .getWeightsInverseNormal(design)
    }

    # Perform the main simulation
    loopResult <- .performSimulationEnrichmentSurvivalLoopCpp(
        cols = cols,
        maxNumberOfIterations = maxNumberOfIterations,
        design = design,
        weights = weights,
        effectList = effectList,
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
        selectPopulationsFunction = selectPopulationsFunction,
        indices = indices,
        intersectionTest = intersectionTest,
        successCriterion = successCriterion,
        gMax = gMax,
        kMax = kMax
    )

    # Extract results from the simulation
    simulatedNumberEventsNotAchieved <- loopResult$simulatedNumberEventsNotAchieved
    simulatedAnalysisTime <- loopResult$simulatedAnalysisTime
    simulatedNumberOfSubjects <- loopResult$simulatedNumberOfSubjects
    simulatedSelections <- loopResult$simulatedSelections
    simulatedRejections <- loopResult$simulatedRejections
    simulatedNumberOfPopulations <- loopResult$simulatedNumberOfPopulations
    simulatedPopulationEventsPerStage <- loopResult$simulatedPopulationEventsPerStage
    simulatedNumberOfEvents <- loopResult$simulatedNumberOfEvents
    simulatedSuccessStopping <- loopResult$simulatedSuccessStopping
    simulatedFutilityStopping <- loopResult$simulatedFutilityStopping
    simulatedConditionalPower <- loopResult$simulatedConditionalPower
    simulatedRejectAtLeastOne <- loopResult$simulatedRejectAtLeastOne
    expectedNumberOfEvents <- loopResult$expectedNumberOfEvents
    expectedNumberOfSubjects <- loopResult$expectedNumberOfSubjects
    expectedStudyDuration <- loopResult$expectedStudyDuration
    iterations <- loopResult$iterations

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

    simulationResults$.data <- loopResult$data
    return(simulationResults)
}

#'
#' @title
#' Get Simulation Enrichment Survival
#'
#' @description
#' Returns the simulated power, stopping and selection probabilities, conditional power,
#' and expected sample size for testing hazard ratios in an enrichment design testing situation.
#'
#' Depending on \code{simulationType}, either a patient-wise survival simulation is performed
#' or normally distributed log-rank test statistics are simulated. The default
#' \code{simulationType = "auto"} chooses the simulation approach automatically based on
#' the explicitly specified arguments.
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
#' @inheritParams param_simulationType_enrichment_survival
#'
#' @details
#' At given design the function simulates the power, stopping probabilities,
#' selection probabilities, and expected event number at given number of events,
#' parameter configuration, and population selection rule in the enrichment situation.
#' An allocation ratio can be specified referring to the ratio of number of subjects
#' in the active treatment group as compared to the control group.
#'
#' If \code{simulationType = "patientWise"}, patient-wise survival data are simulated based on
#' the specified accrual, event-time, and dropout assumptions. If
#' \code{simulationType = "testStatisticBased"}, normally distributed log-rank test statistics
#' are simulated instead. The default \code{simulationType = "auto"} selects the patient-wise
#' approach if patient-wise-specific arguments were explicitly specified; otherwise the
#' test-statistic-based approach is used for backward compatibility.
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
getSimulationEnrichmentSurvival <- function(
        design = NULL,
        ...,
        simulationType = c("auto", "patientWise", "testStatisticBased", "patientWiseBasic"),
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
        showStatistics = FALSE
        ) {
    simulationType <- match.arg(simulationType)

    callArgs <- names(as.list(match.call(expand.dots = FALSE)))
    hasArg <- function(arg) {
        arg %in% callArgs
    }

    patientWiseOnlyArgs <- c(
        "kappa",
        "eventTime",
        "accrualTime",
        "accrualIntensity",
        "accrualIntensityType",
        "dropoutRate1",
        "dropoutRate2",
        "dropoutTime",
        "maxNumberOfSubjects"
    )

    usesPatientWiseOnlyArgs <- any(vapply(
        patientWiseOnlyArgs,
        hasArg,
        logical(1)
    ))

    if (simulationType == "auto") {
        if (usesPatientWiseOnlyArgs) {
            simulationType <- "patientWise"
        } else {
            simulationType <- "testStatisticBased"
        }
    }

    if (identical(simulationType, "testStatisticBased")) {
        if (usesPatientWiseOnlyArgs) {
            stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "patient-wise simulation arguments cannot be specified if 'simulationType' = \"testStatisticBased\"",
                call. = FALSE
            )
        }

        message("Note: 'simulationType' = \"testStatisticBased\" simulates normally distributed log-rank test statistics instead of patient-wise survival data. ",
            "To simulate patient-wise survival data, specify 'simulationType' = \"patientWise\" and the corresponding arguments."
        )
        
        return(getSimulationEnrichmentSurvivalBasic(
            design = design,
            effectList = effectList,
            intersectionTest = intersectionTest,
            stratifiedAnalysis = stratifiedAnalysis,
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
            selectPopulationsFunction = selectPopulationsFunction,
            showStatistics = showStatistics,
            ...
        ))
    }

    if (identical(simulationType, "patientWise")) {
        return(getSimulationEnrichmentSurvivalPatientWise(
            design = design,
            effectList = effectList,
            kappa = kappa,
            eventTime = eventTime,
            accrualTime = accrualTime,
            accrualIntensity = accrualIntensity,
            accrualIntensityType = accrualIntensityType,
            dropoutRate1 = dropoutRate1,
            dropoutRate2 = dropoutRate2,
            dropoutTime = dropoutTime,
            maxNumberOfSubjects = maxNumberOfSubjects,
            intersectionTest = intersectionTest,
            stratifiedAnalysis = stratifiedAnalysis,
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
            selectPopulationsFunction = selectPopulationsFunction,
            showStatistics = showStatistics,
            ...
        ))
    }

    if (identical(simulationType, "patientWiseBasic")) {
        
        message("Note: 'simulationType' = \"patientWiseBasic\" simulates patient-wise survival data using R code instead of C++ code. ",
            "This approach is less efficient and should only be used for testing purposes. ",
            "To perform a more efficient patient-wise simulation, specify 'simulationType' = \"patientWise\".")
        
        return(.getSimulationEnrichmentSurvivalPatientWiseBasic(
            design = design,
            effectList = effectList,
            kappa = kappa,
            eventTime = eventTime,
            accrualTime = accrualTime,
            accrualIntensity = accrualIntensity,
            accrualIntensityType = accrualIntensityType,
            dropoutRate1 = dropoutRate1,
            dropoutRate2 = dropoutRate2,
            dropoutTime = dropoutTime,
            maxNumberOfSubjects = maxNumberOfSubjects,
            intersectionTest = intersectionTest,
            stratifiedAnalysis = stratifiedAnalysis,
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
            selectPopulationsFunction = selectPopulationsFunction,
            showStatistics = showStatistics,
            ...
        ))
    }

    stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "unknown simulation type: ", dQuote(simulationType), call. = FALSE)
}
