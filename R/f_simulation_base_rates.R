## |
## |  *Simulation of binary data with group sequential and combination test*
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
## |  File version: $Revision: 7408 $
## |  Last changed: $Date: 2023-11-09 10:36:19 +0100 (Do, 09 Nov 2023) $
## |  Last changed by: $Author: pahlke $
## |

.getSimulationRatesStageSubjects <- function(
        ...,
        stage,
        riskRatio,
        thetaH0,
        groups,
        plannedSubjects,
        directionUpper,
        allocationRatioPlanned,
        minNumberOfSubjectsPerStage,
        maxNumberOfSubjectsPerStage,
        sampleSizesPerStage,
        conditionalPower,
        conditionalCriticalValue,
        overallRate,
        farringtonManningValue1,
        farringtonManningValue2) {
    if (is.na(conditionalPower)) {
        return(plannedSubjects[stage] - plannedSubjects[stage - 1])
    }

    if (groups == 1) {
        stageSubjects <-
            (max(0, conditionalCriticalValue * sqrt(thetaH0 * (1 - thetaH0)) +
                .getQNorm(conditionalPower) * sqrt(overallRate[1] * (1 - overallRate[1]))))^2 /
                (max(1e-12, (2 * directionUpper - 1) * (overallRate[1] - thetaH0)))^2
    } else {
        mult <- 1
        corr <- thetaH0
        if (riskRatio) {
            mult <- thetaH0
            corr <- 0
        }
        stageSubjects <- (1 + 1 / allocationRatioPlanned[stage]) * (max(0, conditionalCriticalValue *
            sqrt(farringtonManningValue1 * (1 - farringtonManningValue1) +
                farringtonManningValue2 * (1 - farringtonManningValue2) * allocationRatioPlanned[stage] * mult^2) +
            .getQNorm(conditionalPower) * sqrt(overallRate[1] * (1 - overallRate[1]) + overallRate[2] *
                (1 - overallRate[2]) * allocationRatioPlanned[stage] * mult^2)))^2 /
            (max(1e-12, (2 * directionUpper - 1) * (overallRate[1] - mult * overallRate[2] - corr)))^2
    }
    stageSubjects <- ceiling(min(max(minNumberOfSubjectsPerStage[stage], stageSubjects), maxNumberOfSubjectsPerStage[stage]))

    return(stageSubjects)
}

#' @title
#' Get Simulation Rates
#'
#' @description
#' Returns the simulated power, stopping probabilities, conditional power, and expected sample size for
#' testing rates in a one or two treatment groups testing situation.
#'
#' @inheritParams param_design_with_default
#' @inheritParams param_groups
#' @inheritParams param_normalApproximation
#' @param riskRatio If \code{TRUE}, the design characteristics for
#'        one-sided testing of H0: \code{pi1 / pi2 = thetaH0} are simulated, default is \code{FALSE}.
#' @inheritParams param_thetaH0
#' @inheritParams param_pi1_rates
#' @inheritParams param_pi2_rates
#' @inheritParams param_directionUpper
#' @inheritParams param_allocationRatioPlanned
#' @inheritParams param_plannedSubjects
#' @inheritParams param_minNumberOfSubjectsPerStage
#' @inheritParams param_maxNumberOfSubjectsPerStage
#' @inheritParams param_conditionalPowerSimulation
#' @param pi1H1 If specified, the assumed probability in the active treatment group if two treatment groups
#'        are considered, or the assumed probability for a one treatment group design, for which the conditional
#'        power was calculated.
#' @param pi2H1 If specified, the assumed probability in the reference group if two treatment groups
#'        are considered, for which the conditional power was calculated.
#' @inheritParams param_maxNumberOfIterations
#' @inheritParams param_calcSubjectsFunction
#' @inheritParams param_seed
#' @inheritParams param_three_dots
#' @inheritParams param_showStatistics

#'
#' @details
#' At given design the function simulates the power, stopping probabilities, conditional power, and expected
#' sample size at given number of subjects and parameter configuration.
#' Additionally, an allocation ratio = n1/n2 can be specified where n1 and n2 are the number
#' of subjects in the two treatment groups.
#'
#' The definition of \code{pi1H1} and/or \code{pi2H1} makes only sense if \code{kMax} > 1
#' and if \code{conditionalPower}, \code{minNumberOfSubjectsPerStage}, and
#' \code{maxNumberOfSubjectsPerStage} (or \code{calcSubjectsFunction}) are defined.
#'
#' \code{calcSubjectsFunction}\cr
#' This function returns the number of subjects at given conditional power and conditional critical value for specified
#' testing situation. The function might depend on variables
#' \code{stage},
#' \code{riskRatio},
#' \code{thetaH0},
#' \code{groups},
#' \code{plannedSubjects},
#' \code{sampleSizesPerStage},
#' \code{directionUpper},
#' \code{allocationRatioPlanned},
#' \code{minNumberOfSubjectsPerStage},
#' \code{maxNumberOfSubjectsPerStage},
#' \code{conditionalPower},
#' \code{conditionalCriticalValue},
#' \code{overallRate},
#' \code{farringtonManningValue1}, and \code{farringtonManningValue2}.
#' The function has to contain the three-dots argument '...' (see examples).
#'
#' @section Simulation Data:
#' The summary statistics "Simulated data" contains the following parameters: median [range]; mean +/-sd\cr
#'
#' \code{$show(showStatistics = FALSE)} or \code{$setShowStatistics(FALSE)} can be used to disable
#' the output of the aggregated simulated data.\cr
#'
#' Example 1: \cr
#' \code{simulationResults <- getSimulationRates(plannedSubjects = 40)} \cr
#' \code{simulationResults$show(showStatistics = FALSE)}\cr
#'
#' Example 2: \cr
#' \code{simulationResults <- getSimulationRates(plannedSubjects = 40)} \cr
#' \code{simulationResults$setShowStatistics(FALSE)}\cr
#' \code{simulationResults}\cr
#'
#' \code{\link[=getData]{getData()}} can be used to get the aggregated simulated data from the
#' object as \code{\link[base]{data.frame}}. The data frame contains the following columns:
#' \enumerate{
#'   \item \code{iterationNumber}: The number of the simulation iteration.
#'   \item \code{stageNumber}: The stage.
#'   \item \code{pi1}: The assumed or derived event rate in the treatment group (if available).
#'   \item \code{pi2}: The assumed or derived event rate in the control group (if available).
#'   \item \code{numberOfSubjects}: The number of subjects under consideration when the
#'         (interim) analysis takes place.
#'   \item \code{rejectPerStage}: 1 if null hypothesis can be rejected, 0 otherwise.
#'   \item \code{futilityPerStage}: 1 if study should be stopped for futility, 0 otherwise.
#'   \item \code{testStatistic}: The test statistic that is used for the test decision,
#'         depends on which design was chosen (group sequential, inverse normal,
#'         or Fisher combination test)'
#'   \item \code{testStatisticsPerStage}: The test statistic for each stage if only data from
#'         the considered stage is taken into account.
#'   \item \code{overallRate1}: The cumulative rate in treatment group 1.
#'   \item \code{overallRate2}: The cumulative rate in treatment group 2.
#'   \item \code{stagewiseRates1}: The stage-wise rate in treatment group 1.
#'   \item \code{stagewiseRates2}: The stage-wise rate in treatment group 2.
#'   \item \code{sampleSizesPerStage1}: The stage-wise sample size in treatment group 1.
#'   \item \code{sampleSizesPerStage2}: The stage-wise sample size in treatment group 2.
#'   \item \code{trialStop}: \code{TRUE} if study should be stopped for efficacy or futility or final stage, \code{FALSE} otherwise.
#'   \item \code{conditionalPowerAchieved}: The conditional power for the subsequent stage of the trial for
#'         selected sample size and effect. The effect is either estimated from the data or can be
#'         user defined with \code{pi1H1} and \code{pi2H1}.
#' }
#'
#' @template return_object_simulation_results
#' @template how_to_get_help_for_generics
#'
#' @template examples_get_simulation_rates
#'
#' @export
#'
getSimulationRates <- function(design = NULL, ...,
        groups = 2L,
        normalApproximation = TRUE,
        riskRatio = FALSE,
        thetaH0 = ifelse(riskRatio, 1, 0),
        pi1 = seq(0.2, 0.5, 0.1), # C_PI_1_DEFAULT
        pi2 = NA_real_,
        plannedSubjects = NA_real_,
        directionUpper = TRUE, # C_DIRECTION_UPPER_DEFAULT
        allocationRatioPlanned = NA_real_,
        minNumberOfSubjectsPerStage = NA_real_,
        maxNumberOfSubjectsPerStage = NA_real_,
        conditionalPower = NA_real_,
        pi1H1 = NA_real_,
        pi2H1 = NA_real_,
        maxNumberOfIterations = 1000L, # C_MAX_SIMULATION_ITERATIONS_DEFAULT
        seed = NA_real_,
        calcSubjectsFunction = NULL,
        showStatistics = FALSE) {
    if (is.null(design)) {
        design <- .getDefaultDesign(..., type = "simulation")
        .warnInCaseOfUnknownArguments(
            functionName = "getSimulationRates",
            ignore = c(
                .getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design, powerCalculationEnabled = TRUE),
                "showStatistics"
            ), ...
        )
    } else {
        .assertIsTrialDesign(design)
        .warnInCaseOfUnknownArguments(
            functionName = "getSimulationRates",
            ignore = c("showStatistics"), ...
        )
        .warnInCaseOfTwoSidedPowerArgument(...)
    }
    .assertIsSingleLogical(directionUpper, "directionUpper")
    .assertIsSingleNumber(thetaH0, "thetaH0")
    .assertIsValidGroupsParameter(groups)
    .assertIsSingleLogical(normalApproximation, "normalApproximation")
    .assertIsSingleLogical(riskRatio, "riskRatio")
    if (groups == 1L) {
        .assertIsInOpenInterval(thetaH0, "thetaH0", 0, 1, naAllowed = FALSE)
    } else {
        if (riskRatio) {
            .assertIsInOpenInterval(thetaH0, "thetaH0", 0, NULL, naAllowed = TRUE)
        } else {
            .assertIsInOpenInterval(thetaH0, "thetaH0", -1, 1, naAllowed = TRUE)
        }
    }
    .assertIsNumericVector(pi1, "pi1", naAllowed = FALSE)
    .assertIsInOpenInterval(pi1, "pi1", 0, 1, naAllowed = FALSE)
    .assertIsNumericVector(pi2, "pi2", naAllowed = TRUE)
    .assertIsInOpenInterval(pi2, "pi2", 0, 1, naAllowed = TRUE)
    .assertIsNumericVector(minNumberOfSubjectsPerStage, "minNumberOfSubjectsPerStage", naAllowed = TRUE)
    .assertIsNumericVector(maxNumberOfSubjectsPerStage, "maxNumberOfSubjectsPerStage", naAllowed = TRUE)
    .assertIsSingleNumber(conditionalPower, "conditionalPower", naAllowed = TRUE)
    .assertIsInOpenInterval(conditionalPower, "conditionalPower", 0, 1, naAllowed = TRUE)
    .assertIsSingleNumber(pi1H1, "pi1H1", naAllowed = TRUE)
    .assertIsInOpenInterval(pi1H1, "pi1H1", 0, 1, naAllowed = TRUE)
    .assertIsSingleNumber(pi2H1, "pi2H1", naAllowed = TRUE)
    .assertIsInOpenInterval(pi2H1, "pi2H1", 0, 1, naAllowed = TRUE)
    .assertIsNumericVector(allocationRatioPlanned, "allocationRatioPlanned", naAllowed = TRUE)
    .assertIsInOpenInterval(allocationRatioPlanned, "allocationRatioPlanned", 0, C_ALLOCATION_RATIO_MAXIMUM, naAllowed = TRUE)
    .assertIsSinglePositiveInteger(maxNumberOfIterations, "maxNumberOfIterations", validateType = FALSE)
    .assertIsSingleNumber(seed, "seed", naAllowed = TRUE)
    .assertIsSingleLogical(showStatistics, "showStatistics", naAllowed = FALSE)
    .assertIsValidPlannedSubjectsOrEvents(design, plannedSubjects, parameterName = "plannedSubjects")

    if (design$sided == 2) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "only one-sided case is implemented for the simulation design"
        )
    }

    if (!normalApproximation && (groups == 2) && (riskRatio || (thetaH0 != 0))) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "in the two-sample case, exact test is implemented only for testing H0: pi1 - pi2 = 0"
        )
    }

    simulationResults <- SimulationResultsRates$new(design, showStatistics = showStatistics)

    conditionalPower <- .ignoreParameterIfNotUsed(
        "conditionalPower",
        conditionalPower, design$kMax > 1, "design is fixed ('kMax' = 1)"
    )
    minNumberOfSubjectsPerStage <- .ignoreParameterIfNotUsed(
        "minNumberOfSubjectsPerStage",
        minNumberOfSubjectsPerStage, design$kMax > 1, "design is fixed ('kMax' = 1)"
    )
    maxNumberOfSubjectsPerStage <- .ignoreParameterIfNotUsed(
        "maxNumberOfSubjectsPerStage",
        maxNumberOfSubjectsPerStage, design$kMax > 1, "design is fixed ('kMax' = 1)"
    )
    minNumberOfSubjectsPerStage <- .assertIsValidNumberOfSubjectsPerStage(minNumberOfSubjectsPerStage,
        "minNumberOfSubjectsPerStage", plannedSubjects, conditionalPower, calcSubjectsFunction, design$kMax,
        endpoint = "rates"
    )
    maxNumberOfSubjectsPerStage <- .assertIsValidNumberOfSubjectsPerStage(maxNumberOfSubjectsPerStage,
        "maxNumberOfSubjectsPerStage", plannedSubjects, conditionalPower, calcSubjectsFunction, design$kMax,
        endpoint = "rates"
    )
    if (design$kMax > 1) {
        if (any(maxNumberOfSubjectsPerStage - minNumberOfSubjectsPerStage < 0) &&
                !all(is.na(maxNumberOfSubjectsPerStage - minNumberOfSubjectsPerStage))) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'maxNumberOfSubjectsPerStage' (",
                .arrayToString(maxNumberOfSubjectsPerStage),
                ") must be not smaller than minNumberOfSubjectsPerStage' (",
                .arrayToString(minNumberOfSubjectsPerStage), ")"
            )
        }
        .setValueAndParameterType(
            simulationResults, "minNumberOfSubjectsPerStage",
            minNumberOfSubjectsPerStage, NA_real_
        )
        .setValueAndParameterType(
            simulationResults, "maxNumberOfSubjectsPerStage",
            maxNumberOfSubjectsPerStage, NA_real_
        )
    }
    if (design$kMax == 1 && !is.na(conditionalPower)) {
        warning("'conditionalPower' will be ignored for fixed sample design", call. = FALSE)
    }
    if (design$kMax > 1 && is.na(conditionalPower) && is.null(calcSubjectsFunction)) {
        if (length(minNumberOfSubjectsPerStage) != 1 ||
                !is.na(minNumberOfSubjectsPerStage)) {
            warning("'minNumberOfSubjectsPerStage' (",
                .arrayToString(minNumberOfSubjectsPerStage), ") ",
                "will be ignored because neither 'conditionalPower' nor ",
                "'calcSubjectsFunction' is defined",
                call. = FALSE
            )
            simulationResults$minNumberOfSubjectsPerStage <- NA_real_
        }
        if (length(maxNumberOfSubjectsPerStage) != 1 ||
                !is.na(maxNumberOfSubjectsPerStage)) {
            warning("'maxNumberOfSubjectsPerStage' (",
                .arrayToString(maxNumberOfSubjectsPerStage), ") ",
                "will be ignored because neither 'conditionalPower' nor ",
                "'calcSubjectsFunction' is defined",
                call. = FALSE
            )
            simulationResults$maxNumberOfSubjectsPerStage <- NA_real_
        }
    }

    pi1H1 <- .ignoreParameterIfNotUsed(
        "pi1H1", pi1H1, design$kMax > 1,
        "design is fixed ('kMax' = 1)"
    )
    pi2H1 <- .ignoreParameterIfNotUsed(
        "pi2H1", pi2H1, design$kMax > 1,
        "design is fixed ('kMax' = 1)"
    )
    pi1H1 <- .ignoreParameterIfNotUsed("pi1H1", pi1H1, groups == 2, "'groups' = 1")
    pi2H1 <- .ignoreParameterIfNotUsed("pi2H1", pi2H1, groups == 2, "'groups' = 1")

    .setValueAndParameterType(simulationResults, "pi2", pi2, NA_real_)

    if (groups == 1L) {
        if (isTRUE(riskRatio)) {
            warning("'riskRatio' (", riskRatio, ") will be ignored ",
                "because it is not applicable for 'groups' = 1",
                call. = FALSE
            )
        }

        if (!is.na(allocationRatioPlanned)) {
            warning("'allocationRatioPlanned' (", allocationRatioPlanned,
                ") will be ignored because it is not applicable for 'groups' = 1",
                call. = FALSE
            )
            simulationResults$allocationRatioPlanned <- NA_real_
        }
        simulationResults$.setParameterType("allocationRatioPlanned", C_PARAM_NOT_APPLICABLE)

        if (!is.na(pi2)) {
            warning("'pi2' (", pi2,
                ") will be ignored because it is not applicable for 'groups' = 1",
                call. = FALSE
            )
            simulationResults$pi2 <- NA_real_
        }
        simulationResults$.setParameterType("pi2", C_PARAM_NOT_APPLICABLE)
    } else {
        if (any(is.na(allocationRatioPlanned))) {
            allocationRatioPlanned <- C_ALLOCATION_RATIO_DEFAULT
        }
        if (is.na(pi2)) {
            pi2 <- C_PI_2_DEFAULT
            simulationResults$pi2 <- pi2
            simulationResults$.setParameterType("pi2", C_PARAM_DEFAULT_VALUE)
        }
        if (length(allocationRatioPlanned) == 1) {
            allocationRatioPlanned <- rep(allocationRatioPlanned, design$kMax)
        } else if (length(allocationRatioPlanned) != design$kMax) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'allocationRatioPlanned' (", .arrayToString(allocationRatioPlanned), ") ",
                "must have length 1 or ", design$kMax, " (kMax)"
            )
        }

        if (length(unique(allocationRatioPlanned)) == 1) {
            .setValueAndParameterType(
                simulationResults, "allocationRatioPlanned",
                allocationRatioPlanned[1],
                defaultValue = 1
            )
        } else {
            .setValueAndParameterType(
                simulationResults, "allocationRatioPlanned",
                allocationRatioPlanned,
                defaultValue = rep(1, design$kMax)
            )
        }
    }

    if (groups == 1) {
        effect <- pi1 - thetaH0
    } else {
        if (riskRatio) {
            effect <- pi1 / pi2 - thetaH0
        } else {
            effect <- pi1 - pi2 - thetaH0
        }
    }
    simulationResults$effect <- effect
    simulationResults$.setParameterType(
        "effect",
        ifelse(groups == 1 && thetaH0 == 0, C_PARAM_NOT_APPLICABLE, C_PARAM_GENERATED)
    )

    .setValueAndParameterType(simulationResults, "normalApproximation", normalApproximation, TRUE)
    .setValueAndParameterType(simulationResults, "riskRatio", riskRatio, FALSE)
    .setValueAndParameterType(simulationResults, "thetaH0", thetaH0, ifelse(riskRatio, 1, 0))
    .setValueAndParameterType(simulationResults, "pi1", pi1, C_PI_1_DEFAULT)
    .setValueAndParameterType(simulationResults, "groups", as.integer(groups), 2L)
    .setValueAndParameterType(
        simulationResults, "plannedSubjects",
        plannedSubjects, NA_real_
    )
    .setValueAndParameterType(
        simulationResults, "directionUpper",
        directionUpper, C_DIRECTION_UPPER_DEFAULT
    )
    .setValueAndParameterType(simulationResults, "minNumberOfSubjectsPerStage",
        minNumberOfSubjectsPerStage, NA_real_,
        notApplicableIfNA = TRUE
    )
    .setValueAndParameterType(simulationResults, "maxNumberOfSubjectsPerStage",
        maxNumberOfSubjectsPerStage, NA_real_,
        notApplicableIfNA = TRUE
    )
    .setValueAndParameterType(simulationResults, "conditionalPower",
        conditionalPower, NA_real_,
        notApplicableIfNA = TRUE
    )
    .setValueAndParameterType(simulationResults, "pi1H1",
        pi1H1, NA_real_,
        notApplicableIfNA = TRUE
    )
    .setValueAndParameterType(simulationResults, "pi2H1", pi2H1, 0.2, notApplicableIfNA = TRUE)
    .setValueAndParameterType(
        simulationResults, "maxNumberOfIterations",
        as.integer(maxNumberOfIterations), C_MAX_SIMULATION_ITERATIONS_DEFAULT
    )
    simulationResults$.setParameterType("seed", ifelse(is.na(seed),
        C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
    ))
    simulationResults$seed <- .setSeed(seed)

    if (.isTrialDesignGroupSequential(design)) {
        designNumber <- 1L
    } else if (.isTrialDesignInverseNormal(design)) {
        designNumber <- 2L
    } else if (.isTrialDesignFisher(design)) {
        designNumber <- 3L
    }

    if (.isTrialDesignFisher(design)) {
        alpha0Vec <- design$alpha0Vec
        futilityBounds <- rep(NA_real_, design$kMax - 1)
    } else {
        alpha0Vec <- rep(NA_real_, design$kMax - 1)
        futilityBounds <- design$futilityBounds
    }

    calcSubjectsFunctionList <- .getCalcSubjectsFunction(
        design = design,
        simulationResults = simulationResults,
        calcFunction = calcSubjectsFunction,
        expectedFunction = .getSimulationRatesStageSubjects
    )
    calcSubjectsFunctionType <- calcSubjectsFunctionList$calcSubjectsFunctionType
    calcSubjectsFunctionR <- calcSubjectsFunctionList$calcSubjectsFunctionR
    calcSubjectsFunctionCpp <- calcSubjectsFunctionList$calcSubjectsFunctionCpp

    cppResult <- .getSimulationRatesCpp(
        kMax                        = design$kMax,
        informationRates            = design$informationRates,
        criticalValues              = design$criticalValues,
        pi1                         = pi1,
        pi2                         = pi2,
        maxNumberOfIterations       = maxNumberOfIterations,
        designNumber                = designNumber,
        groups                      = groups,
        futilityBounds              = futilityBounds,
        alpha0Vec                   = alpha0Vec,
        minNumberOfSubjectsPerStage = minNumberOfSubjectsPerStage,
        maxNumberOfSubjectsPerStage = maxNumberOfSubjectsPerStage,
        conditionalPower            = conditionalPower,
        pi1H1                       = pi1H1,
        pi2H1                       = pi2H1,
        normalApproximation         = normalApproximation,
        plannedSubjects             = plannedSubjects,
        directionUpper              = directionUpper,
        allocationRatioPlanned      = allocationRatioPlanned,
        riskRatio                   = riskRatio,
        thetaH0                     = thetaH0,
        calcSubjectsFunctionType    = calcSubjectsFunctionType,
        calcSubjectsFunctionR       = calcSubjectsFunctionR,
        calcSubjectsFunctionCpp     = calcSubjectsFunctionCpp
    )

    data <- cppResult$data
    data <- data[!is.na(data$pi1), ]
    simulationResults$.data <- data

    simulationResults$iterations <- cppResult$iterations
    simulationResults$sampleSizes <- cppResult$sampleSizes
    simulationResults$rejectPerStage <- cppResult$rejectPerStage
    simulationResults$overallReject <- cppResult$overallReject
    simulationResults$futilityPerStage <- cppResult$futilityPerStage
    simulationResults$futilityStop <- cppResult$futilityStop
    simulationResults$earlyStop <- cppResult$earlyStop
    simulationResults$expectedNumberOfSubjects <- cppResult$expectedNumberOfSubjects
    simulationResults$conditionalPowerAchieved <- cppResult$conditionalPowerAchieved

    if (!all(is.na(simulationResults$conditionalPowerAchieved))) {
        simulationResults$.setParameterType("conditionalPowerAchieved", C_PARAM_GENERATED)
    }

    return(simulationResults)
}
