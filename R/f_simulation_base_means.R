## |
## |  *Simulation of continuous data with group sequential and combination test*
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

.getSimulationMeansStageSubjects <- function(..., stage,
        meanRatio,
        thetaH0,
        groups,
        plannedSubjects,
        allocationRatioPlanned,
        minNumberOfSubjectsPerStage,
        maxNumberOfSubjectsPerStage,
        sampleSizesPerStage,
        thetaH1,
        stDevH1,
        conditionalPower,
        conditionalCriticalValue) {
    if (is.na(conditionalPower)) {
        return(plannedSubjects[stage] - plannedSubjects[stage - 1])
    }

    thetaStandardized <- thetaH1 / stDevH1

    mult <- 1
    if (groups == 2) {
        thetaH0 <- ifelse(meanRatio, thetaH0, 1)
        mult <- 1 + 1 / allocationRatioPlanned[stage] + thetaH0^2 * (1 + allocationRatioPlanned[stage])
    }

    stageSubjects <- (max(0, conditionalCriticalValue + .getQNorm(conditionalPower)))^2 * mult /
        (max(1e-12, thetaStandardized))^2

    stageSubjects <- min(
        max(minNumberOfSubjectsPerStage[stage], stageSubjects), maxNumberOfSubjectsPerStage[stage]
    )

    return(stageSubjects)
}

#' @title
#' Get Simulation Means
#'
#' @description
#' Returns the simulated power, stopping probabilities, conditional power, and expected sample size
#' for testing means in a one or two treatment groups testing situation.
#'
#' @inheritParams param_design_with_default
#' @inheritParams param_groups
#' @param normalApproximation The type of computation of the p-values. Default is \code{TRUE},
#'        i.e., normally distributed test statistics are generated.
#'        If \code{FALSE}, the t test is used for calculating the p-values,
#'        i.e., t distributed test statistics are generated.
#' @param meanRatio If \code{TRUE}, the design characteristics for
#'        one-sided testing of H0: \code{mu1 / mu2 = thetaH0} are simulated, default is \code{FALSE}.
#' @inheritParams param_thetaH0
#' @inheritParams param_alternative_simulation
#' @inheritParams param_stDevSimulation
#' @inheritParams param_directionUpper
#' @inheritParams param_allocationRatioPlanned
#' @inheritParams param_plannedSubjects
#' @inheritParams param_minNumberOfSubjectsPerStage
#' @inheritParams param_maxNumberOfSubjectsPerStage
#' @inheritParams param_conditionalPowerSimulation
#' @inheritParams param_thetaH1
#' @inheritParams param_stDevH1
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
#' The definition of \code{thetaH1} makes only sense if \code{kMax} > 1
#' and if \code{conditionalPower}, \code{minNumberOfSubjectsPerStage}, and
#' \code{maxNumberOfSubjectsPerStage} (or \code{calcSubjectsFunction}) are defined.
#'
#' \code{calcSubjectsFunction}\cr
#' This function returns the number of subjects at given conditional power and conditional critical value for specified
#' testing situation. The function might depend on variables
#' \code{stage},
#' \code{meanRatio},
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
#' \code{thetaH1}, and
#' \code{stDevH1}.
#' The function has to contain the three-dots argument '...' (see examples).
#'
#' @section Simulation Data:
#' The summary statistics "Simulated data" contains the following parameters: median [range]; mean +/-sd\cr
#'
#' \code{$show(showStatistics = FALSE)} or \code{$setShowStatistics(FALSE)} can be used to disable
#' the output of the aggregated simulated data.\cr
#'
#' Example 1: \cr
#' \code{simulationResults <- getSimulationMeans(plannedSubjects = 40)} \cr
#' \code{simulationResults$show(showStatistics = FALSE)}\cr
#'
#' Example 2: \cr
#' \code{simulationResults <- getSimulationMeans(plannedSubjects = 40)} \cr
#' \code{simulationResults$setShowStatistics(FALSE)}\cr
#' \code{simulationResults}\cr
#'
#' \code{\link[=getData]{getData()}} can be used to get the aggregated simulated data from the
#' object as \code{\link[base]{data.frame}}. The data frame contains the following columns:
#' \enumerate{
#'   \item \code{iterationNumber}: The number of the simulation iteration.
#'   \item \code{stageNumber}: The stage.
#'   \item \code{alternative}: The alternative hypothesis value.
#'   \item \code{numberOfSubjects}: The number of subjects under consideration when the
#'         (interim) analysis takes place.
#'   \item \code{rejectPerStage}: 1 if null hypothesis can be rejected, 0 otherwise.
#'   \item \code{futilityPerStage}: 1 if study should be stopped for futility, 0 otherwise.
#'   \item \code{testStatistic}: The test statistic that is used for the test decision,
#'         depends on which design was chosen (group sequential, inverse normal, or Fisher's combination test).
#'   \item \code{testStatisticsPerStage}: The test statistic for each stage if only data from
#'         the considered stage is taken into account.
#'   \item \code{effectEstimate}: Overall simulated standardized effect estimate.
#'   \item \code{trialStop}: \code{TRUE} if study should be stopped for efficacy or futility or final stage, \code{FALSE} otherwise.
#'   \item \code{conditionalPowerAchieved}: The conditional power for the subsequent stage of the trial for
#'         selected sample size and effect. The effect is either estimated from the data or can be
#'         user defined with \code{thetaH1}.
#' }
#'
#' @template return_object_simulation_results
#' @template how_to_get_help_for_generics
#'
#' @template examples_get_simulation_means
#'
#' @export
#'
getSimulationMeans <- function(design = NULL, ...,
        groups = 2L,
        normalApproximation = TRUE,
        meanRatio = FALSE,
        thetaH0 = ifelse(meanRatio, 1, 0),
        alternative = seq(0, 1, 0.2), # C_ALTERNATIVE_POWER_SIMULATION_DEFAULT
        stDev = 1, # C_STDEV_DEFAULT
        plannedSubjects = NA_real_,
        directionUpper = TRUE, # C_DIRECTION_UPPER_DEFAULT
        allocationRatioPlanned = NA_real_,
        minNumberOfSubjectsPerStage = NA_real_,
        maxNumberOfSubjectsPerStage = NA_real_,
        conditionalPower = NA_real_,
        thetaH1 = NA_real_,
        stDevH1 = NA_real_,
        maxNumberOfIterations = 1000L, # C_MAX_SIMULATION_ITERATIONS_DEFAULT
        seed = NA_real_,
        calcSubjectsFunction = NULL,
        showStatistics = FALSE) {
    if (is.null(design)) {
        design <- .getDefaultDesign(..., type = "simulation")
        .warnInCaseOfUnknownArguments(
            functionName = "getSimulationMeans",
            ignore = c(
                .getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design, powerCalculationEnabled = TRUE),
                "showStatistics"
            ), ...
        )
    } else {
        .assertIsTrialDesign(design)
        .warnInCaseOfUnknownArguments(functionName = "getSimulationMeans", ignore = c("showStatistics"), ...)
        .warnInCaseOfTwoSidedPowerArgument(...)
    }
    .assertIsSingleLogical(directionUpper, "directionUpper")
    .assertIsSingleNumber(thetaH0, "thetaH0")
    if (meanRatio) {
        .assertIsInOpenInterval(thetaH0, "thetaH0", 0, NULL, naAllowed = TRUE)
        .assertIsInOpenInterval(thetaH1, "thetaH1", 0, NULL, naAllowed = TRUE)
        if (identical(alternative, C_ALTERNATIVE_POWER_SIMULATION_DEFAULT)) {
            alternative <- C_ALTERNATIVE_POWER_SIMULATION_MEAN_RATIO_DEFAULT
        }
        .assertIsInOpenInterval(alternative, "alternative", 0, NULL, naAllowed = TRUE)
    }
    .assertIsValidGroupsParameter(groups)
    .assertIsNumericVector(alternative, "alternative", naAllowed = FALSE)
    .assertIsNumericVector(minNumberOfSubjectsPerStage, "minNumberOfSubjectsPerStage", naAllowed = TRUE)
    .assertIsNumericVector(maxNumberOfSubjectsPerStage, "maxNumberOfSubjectsPerStage", naAllowed = TRUE)
    .assertIsSingleNumber(conditionalPower, "conditionalPower", naAllowed = TRUE)
    .assertIsInOpenInterval(conditionalPower, "conditionalPower", 0, 1, naAllowed = TRUE)
    .assertIsSingleNumber(thetaH1, "thetaH1", naAllowed = TRUE)
    .assertIsSingleNumber(stDevH1, "stDevH1", naAllowed = TRUE)
    .assertIsInOpenInterval(stDevH1, "stDevH1", 0, NULL, naAllowed = TRUE)
    .assertIsNumericVector(allocationRatioPlanned, "allocationRatioPlanned", naAllowed = TRUE)
    .assertIsInOpenInterval(allocationRatioPlanned, "allocationRatioPlanned", 0, C_ALLOCATION_RATIO_MAXIMUM, naAllowed = TRUE)
    .assertIsSinglePositiveInteger(maxNumberOfIterations, "maxNumberOfIterations", validateType = FALSE)
    .assertIsSingleNumber(seed, "seed", naAllowed = TRUE)
    .assertIsValidStandardDeviation(stDev)
    .assertIsSingleLogical(showStatistics, "showStatistics", naAllowed = FALSE)
    .assertIsSingleLogical(normalApproximation, "normalApproximation", naAllowed = FALSE)
    .assertIsValidPlannedSubjectsOrEvents(design, plannedSubjects, parameterName = "plannedSubjects")

    simulationResults <- SimulationResultsMeans(design, showStatistics = showStatistics)

    if (design$sided == 2) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "only one-sided case is implemented for the simulation design"
        )
    }

    if (groups == 1L) {
        if (isTRUE(meanRatio)) {
            warning("'meanRatio' (", meanRatio, ") will be ignored ",
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
    } else {
        if (any(is.na(allocationRatioPlanned))) {
            allocationRatioPlanned <- C_ALLOCATION_RATIO_DEFAULT
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

    thetaH1 <- .ignoreParameterIfNotUsed(
        "thetaH1", thetaH1, design$kMax > 1,
        "design is fixed ('kMax' = 1)", "Assumed effect"
    )
    stDevH1 <- .ignoreParameterIfNotUsed(
        "stDevH1", stDevH1, design$kMax > 1,
        "design is fixed ('kMax' = 1)", "Assumed effect"
    )
    if (is.na(conditionalPower) && is.null(calcSubjectsFunction) && !is.na(stDevH1)) {
        warning("'stDevH1' will be ignored because neither 'conditionalPower' nor ",
            "'calcSubjectsFunction' is defined",
            call. = FALSE
        )
    }
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
        endpoint = "means"
    )
    maxNumberOfSubjectsPerStage <- .assertIsValidNumberOfSubjectsPerStage(maxNumberOfSubjectsPerStage,
        "maxNumberOfSubjectsPerStage", plannedSubjects, conditionalPower, calcSubjectsFunction, design$kMax,
        endpoint = "means"
    )

    if (design$kMax > 1) {
        if (!normalApproximation) {
            if (!all(is.na(minNumberOfSubjectsPerStage)) && (any(minNumberOfSubjectsPerStage < groups * 2))) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "minNumberOfSubjectsPerStage not correctly specified"
                )
            }
        }
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
    if (!is.na(conditionalPower) && design$kMax == 1) {
        warning("'conditionalPower' will be ignored for fixed sample design", call. = FALSE)
    }
    if (!is.null(calcSubjectsFunction) && design$kMax == 1) {
        warning("'calcSubjectsFunction' will be ignored for fixed sample design", call. = FALSE)
    }

    if (is.na(conditionalPower) && is.null(calcSubjectsFunction)) {
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

    simulationResults$.setParameterType(
        "calcSubjectsFunction",
        ifelse(design$kMax == 1, C_PARAM_NOT_APPLICABLE,
            ifelse(!is.null(calcSubjectsFunction) && design$kMax > 1,
                C_PARAM_USER_DEFINED, C_PARAM_DEFAULT_VALUE
            )
        )
    )

    effect <- alternative - thetaH0
    simulationResults$effect <- effect
    simulationResults$.setParameterType(
        "effect",
        ifelse(thetaH0 == 0, C_PARAM_NOT_APPLICABLE, C_PARAM_GENERATED)
    )

    .setValueAndParameterType(simulationResults, "normalApproximation", normalApproximation, TRUE)
    .setValueAndParameterType(simulationResults, "meanRatio", meanRatio, FALSE)
    .setValueAndParameterType(simulationResults, "thetaH0", thetaH0, ifelse(meanRatio, 1, 0))
    .setValueAndParameterType(
        simulationResults, "alternative",
        alternative, C_ALTERNATIVE_POWER_SIMULATION_DEFAULT
    )
    .setValueAndParameterType(simulationResults, "stDev", stDev, C_STDEV_DEFAULT)
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
    .setValueAndParameterType(simulationResults, "thetaH1",
        thetaH1, NA_real_,
        notApplicableIfNA = TRUE
    )
    .setValueAndParameterType(simulationResults, "stDevH1",
        stDevH1, NA_real_,
        notApplicableIfNA = TRUE
    )
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
    if (is.na(stDevH1)) {
        stDevH1 <- stDev
    }

    calcSubjectsFunctionList <- .getCalcSubjectsFunction(
        design = design,
        simulationResults = simulationResults,
        calcFunction = calcSubjectsFunction,
        expectedFunction = .getSimulationMeansStageSubjects
    )
    calcSubjectsFunctionType <- calcSubjectsFunctionList$calcSubjectsFunctionType
    calcSubjectsFunctionR <- calcSubjectsFunctionList$calcSubjectsFunctionR
    calcSubjectsFunctionCpp <- calcSubjectsFunctionList$calcSubjectsFunctionCpp

    cppResult <- .getSimulationMeansLoopCpp(
        alternative = alternative,
        kMax = design$kMax,
        maxNumberOfIterations = maxNumberOfIterations,
        designNumber = designNumber,
        informationRates = design$informationRates,
        futilityBounds = futilityBounds,
        alpha0Vec = alpha0Vec,
        criticalValues = design$criticalValues,
        meanRatio = meanRatio,
        thetaH0 = thetaH0,
        stDev = stDev,
        groups = groups,
        normalApproximation = normalApproximation,
        plannedSubjects = plannedSubjects,
        directionUpper = directionUpper,
        allocationRatioPlanned = allocationRatioPlanned,
        minNumberOfSubjectsPerStage = minNumberOfSubjectsPerStage,
        maxNumberOfSubjectsPerStage = maxNumberOfSubjectsPerStage,
        conditionalPower = conditionalPower,
        thetaH1 = thetaH1,
        stDevH1 = stDevH1,
        calcSubjectsFunctionType = calcSubjectsFunctionType,
        calcSubjectsFunctionR = calcSubjectsFunctionR,
        calcSubjectsFunctionCpp = calcSubjectsFunctionCpp
    )

    sampleSizes <- cppResult$sampleSizes
    sampleSizes[is.na(sampleSizes)] <- 0

    simulationResults$iterations <- cppResult$iterations
    simulationResults$sampleSizes <- sampleSizes
    simulationResults$rejectPerStage <- cppResult$rejectPerStage
    simulationResults$overallReject <- cppResult$overallReject
    simulationResults$futilityPerStage <- cppResult$futilityPerStage
    simulationResults$futilityStop <- cppResult$futilityStop
    if (design$kMax > 1) {
        if (length(alternative) == 1) {
            simulationResults$earlyStop <- sum(cppResult$futilityPerStage) + sum(cppResult$rejectPerStage[1:(design$kMax - 1)])
        } else {
            if (design$kMax > 2) {
                rejectPerStageColSum <- colSums(cppResult$rejectPerStage[1:(design$kMax - 1), ])
            } else {
                rejectPerStageColSum <- cppResult$rejectPerStage[1, ]
            }
            simulationResults$earlyStop <- colSums(cppResult$futilityPerStage) + rejectPerStageColSum
        }
    } else {
        simulationResults$earlyStop <- rep(0, length(alternative))
    }

    simulationResults$expectedNumberOfSubjects <- cppResult$expectedNumberOfSubjects
    simulationResults$conditionalPowerAchieved <- cppResult$conditionalPowerAchieved

    if (!all(is.na(simulationResults$conditionalPowerAchieved))) {
        simulationResults$.setParameterType("conditionalPowerAchieved", C_PARAM_GENERATED)
    }

    data <- cppResult$data
    data <- data[!is.na(data$alternative), ]
    if (designNumber != 3L) {
        data <- data[, colnames(data) != "pValue"]
    }
    data$trialStop <- as.logical(data$trialStop)
    simulationResults$.data <- data

    return(simulationResults)
}
