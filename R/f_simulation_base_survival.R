## |
## |  *Simulation of survival data with group sequential and combination test*
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
## |  File version: $Revision: 8023 $
## |  Last changed: $Date: 2024-07-01 08:50:30 +0200 (Mo, 01 Jul 2024) $
## |  Last changed by: $Author: pahlke $
## |

#' @include class_simulation_results.R
#' @include f_core_utilities.R
NULL

.warnInCaseOfDefinedPiValue <- function(designPlan, piValueName) {
    piValue <- designPlan[[piValueName]]
    if (!is.null(piValue) && !is.na(piValue) && length(piValue) > 0) {
        designPlan$.setParameterType(piValueName, C_PARAM_NOT_APPLICABLE)
        warning("'pi2' (", .arrayToString(piValue), ") will be ignored ",
            "because piecewise exponential survival function is enabled",
            call. = FALSE
        )
        designPlan[[piValueName]] <- NA_real_
    }
}

.isLambdaBasedSimulationEnabled <- function(pwsTimeObject) {
    if (!pwsTimeObject$.isLambdaBased()) {
        return(FALSE)
    }

    if (pwsTimeObject$delayedResponseEnabled) {
        return(TRUE)
    }

    if (pwsTimeObject$piecewiseSurvivalEnabled) {
        return(TRUE)
    }

    if (pwsTimeObject$kappa != 1) {
        if (length(pwsTimeObject$lambda1) != 1) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "if 'kappa' != 1 then 'lambda1' (",
                .arrayToString(pwsTimeObject$lambda1), ") must be a single numeric value"
            )
        }
        if (length(pwsTimeObject$lambda2) != 1) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "if 'kappa' != 1 then 'lambda2' (",
                .arrayToString(pwsTimeObject$lambda2), ") must be a single numeric value"
            )
        }

        return(TRUE)
    }

    if (pwsTimeObject$.getParameterType("hazardRatio") == C_PARAM_USER_DEFINED &&
            !all(is.na(pwsTimeObject$hazardRatio))) {
        if (pwsTimeObject$.getParameterType("lambda1") == C_PARAM_USER_DEFINED &&
                length(pwsTimeObject$lambda1) == length(pwsTimeObject$hazardRatio) &&
                !all(is.na(pwsTimeObject$lambda1))) {
            return(TRUE)
        }
        if (pwsTimeObject$.getParameterType("lambda2") == C_PARAM_USER_DEFINED &&
                length(pwsTimeObject$lambda2) == length(pwsTimeObject$hazardRatio) &&
                !all(is.na(pwsTimeObject$lambda2))) {
            return(TRUE)
        }
    }

    return(FALSE)
}

#' @title
#' Get Simulation Survival
#'
#' @description
#' Returns the analysis times, power, stopping probabilities, conditional power, and expected sample size
#' for testing the hazard ratio in a two treatment groups survival design.
#'
#' @inheritParams param_design_with_default
#' @inheritParams param_thetaH0
#' @inheritParams param_directionUpper
#' @inheritParams param_pi1_survival
#' @inheritParams param_pi2_survival
#' @inheritParams param_lambda1
#' @inheritParams param_lambda2
#' @inheritParams param_median1
#' @inheritParams param_median2
#' @inheritParams param_hazardRatio
#' @inheritParams param_piecewiseSurvivalTime
#' @inheritParams param_kappa
#' @param allocation1 The number how many subjects are assigned to treatment 1 in a
#'        subsequent order, default is \code{1}
#' @param allocation2 The number how many subjects are assigned to treatment 2 in a
#'        subsequent order, default is \code{1}
#' @inheritParams param_eventTime
#' @inheritParams param_accrualTime
#' @inheritParams param_accrualIntensity
#' @inheritParams param_accrualIntensityType
#' @inheritParams param_dropoutRate1
#' @inheritParams param_dropoutRate2
#' @inheritParams param_dropoutTime
#' @inheritParams param_maxNumberOfSubjects_survival
#' @inheritParams param_plannedEvents
#' @inheritParams param_minNumberOfEventsPerStage
#' @inheritParams param_maxNumberOfEventsPerStage
#' @inheritParams param_conditionalPowerSimulation
#' @inheritParams param_thetaH1
#' @inheritParams param_maxNumberOfIterations
#' @inheritParams param_calcEventsFunction
#' @inheritParams param_showStatistics
#' @param maxNumberOfRawDatasetsPerStage The number of raw datasets per stage that shall
#'        be extracted and saved as \code{\link[base]{data.frame}}, default is \code{0}.
#'        \code{\link[=getRawData]{getRawData()}} can be used to get the extracted raw data from the object.
#' @param longTimeSimulationAllowed Logical that indicates whether long time simulations
#'        that consumes more than 30 seconds are allowed or not, default is \code{FALSE}.
#' @inheritParams param_seed
#' @inheritParams param_three_dots
#'
#' @details
#' At given design the function simulates the power, stopping probabilities, conditional power, and expected
#' sample size at given number of events, number of subjects, and parameter configuration.
#' It also simulates the time when the required events are expected under the given
#' assumptions (exponentially, piecewise exponentially, or Weibull distributed survival times
#' and constant or non-constant piecewise accrual).
#' Additionally, integers \code{allocation1} and \code{allocation2} can be specified that determine the number allocated
#' to treatment group 1 and treatment group 2, respectively.
#' More precisely, unequal randomization ratios must be specified via the two integer arguments \code{allocation1} and
#' \code{allocation2} which describe how many subjects are consecutively enrolled in each group, respectively, before a
#' subject is assigned to the other group. For example, the arguments \code{allocation1 = 2}, \code{allocation2 = 1},
#' \code{maxNumberOfSubjects = 300} specify 2:1 randomization with 200 subjects randomized to intervention and 100 to
#' control. (Caveat: Do not use \code{allocation1 = 200}, \code{allocation2 = 100}, \code{maxNumberOfSubjects = 300}
#' as this would imply that the 200 intervention subjects are enrolled prior to enrollment of any control subjects.)
#'
#' \code{conditionalPower}\cr
#' The definition of \code{thetaH1} makes only sense if \code{kMax} > 1
#' and if \code{conditionalPower}, \code{minNumberOfEventsPerStage}, and
#' \code{maxNumberOfEventsPerStage} are defined.
#'
#' Note that \code{numberOfSubjects}, \code{numberOfSubjects1}, and \code{numberOfSubjects2} in the output
#' are the expected number of subjects.
#'
#' \code{calcEventsFunction}\cr
#' This function returns the number of events at given conditional power and conditional critical value for specified
#' testing situation. The function might depend on variables
#' \code{stage},
#' \code{conditionalPower},
#' \code{thetaH0},
#' \code{plannedEvents},
#' \code{singleEventsPerStage},
#' \code{minNumberOfEventsPerStage},
#' \code{maxNumberOfEventsPerStage},
#' \code{allocationRatioPlanned},
#' \code{conditionalCriticalValue},
#' The function has to contain the three-dots argument '...' (see examples).
#'
#' @template details_piecewise_survival
#'
#' @template details_piecewise_accrual
#'
#' @section Simulation Data:
#' The summary statistics "Simulated data" contains the following parameters: median [range]; mean +/-sd\cr
#'
#' \code{$show(showStatistics = FALSE)} or \code{$setShowStatistics(FALSE)} can be used to disable
#' the output of the aggregated simulated data.\cr
#'
#' Example 1: \cr
#' \code{simulationResults <- getSimulationSurvival(maxNumberOfSubjects = 100, plannedEvents = 30)} \cr
#' \code{simulationResults$show(showStatistics = FALSE)}\cr
#'
#' Example 2: \cr
#' \code{simulationResults <- getSimulationSurvival(maxNumberOfSubjects = 100, plannedEvents = 30)} \cr
#' \code{simulationResults$setShowStatistics(FALSE)}\cr
#' \code{simulationResults}\cr
#'
#' \code{\link[=getData]{getData()}} can be used to get the aggregated simulated data from the
#' object as \code{\link[base]{data.frame}}. The data frame contains the following columns:
#' \enumerate{
#'   \item \code{iterationNumber}: The number of the simulation iteration.
#'   \item \code{stageNumber}: The stage.
#'   \item \code{pi1}: The assumed or derived event rate in the treatment group.
#'   \item \code{pi2}: The assumed or derived event rate in the control group.
#'   \item \code{hazardRatio}: The hazard ratio under consideration (if available).
#'   \item \code{analysisTime}: The analysis time.
#'   \item \code{numberOfSubjects}: The number of subjects under consideration when the
#'         (interim) analysis takes place.
#'   \item \code{eventsPerStage1}: The observed number of events per stage
#'         in treatment group 1.
#'   \item \code{eventsPerStage2}: The observed number of events per stage
#'         in treatment group 2.
#'   \item \code{singleEventsPerStage}: The observed number of events per stage
#'         in both treatment groups.
#'   \item \code{rejectPerStage}: 1 if null hypothesis can be rejected, 0 otherwise.
#'   \item \code{futilityPerStage}: 1 if study should be stopped for futility, 0 otherwise.
#'   \item \code{eventsNotAchieved}: 1 if number of events could not be reached with
#'         observed number of subjects, 0 otherwise.
#'   \item \code{testStatistic}: The test statistic that is used for the test decision,
#'         depends on which design was chosen (group sequential, inverse normal,
#'         or Fisher combination test)'
#'   \item \code{logRankStatistic}: Z-score statistic which corresponds to a one-sided
#'         log-rank test at considered stage.
#'   \item \code{hazardRatioEstimateLR}: The estimated hazard ratio, derived from the
#'         log-rank statistic.
#'   \item \code{trialStop}: \code{TRUE} if study should be stopped for efficacy or futility or final stage, \code{FALSE} otherwise.
#'   \item \code{conditionalPowerAchieved}: The conditional power for the subsequent stage of the trial for
#'         selected sample size and effect. The effect is either estimated from the data or can be
#'         user defined with \code{thetaH1}.
#' }
#'
#' @section Raw Data:
#' \code{\link[=getRawData]{getRawData()}} can be used to get the simulated raw data from the
#' object as \code{\link[base]{data.frame}}. Note that \code{getSimulationSurvival()}
#' must called before with \code{maxNumberOfRawDatasetsPerStage} > 0.
#'
#' @template return_object_simulation_results
#' @template how_to_get_help_for_generics
#'
#' @template examples_get_simulation_survival
#'
#' @export
#'
getSimulationSurvival <- function(design = NULL, ...,
        thetaH0 = 1, # C_THETA_H0_SURVIVAL_DEFAULT
        directionUpper = TRUE, # C_DIRECTION_UPPER_DEFAULT
        pi1 = NA_real_,
        pi2 = NA_real_,
        lambda1 = NA_real_,
        lambda2 = NA_real_,
        median1 = NA_real_,
        median2 = NA_real_,
        hazardRatio = NA_real_,
        kappa = 1,
        piecewiseSurvivalTime = NA_real_,
        allocation1 = 1, # C_ALLOCATION_1_DEFAULT
        allocation2 = 1, # C_ALLOCATION_2_DEFAULT
        eventTime = 12, # C_EVENT_TIME_DEFAULT
        accrualTime = c(0, 12), # C_ACCRUAL_TIME_DEFAULT
        accrualIntensity = 0.1, # C_ACCRUAL_INTENSITY_DEFAULT
        accrualIntensityType = c("auto", "absolute", "relative"),
        dropoutRate1 = 0, # C_DROP_OUT_RATE_1_DEFAULT
        dropoutRate2 = 0, # C_DROP_OUT_RATE_2_DEFAULT
        dropoutTime = 12, # C_DROP_OUT_TIME_DEFAULT
        maxNumberOfSubjects = NA_real_,
        plannedEvents = NA_real_,
        minNumberOfEventsPerStage = NA_real_,
        maxNumberOfEventsPerStage = NA_real_,
        conditionalPower = NA_real_,
        thetaH1 = NA_real_,
        maxNumberOfIterations = 1000L, #  C_MAX_SIMULATION_ITERATIONS_DEFAULT
        maxNumberOfRawDatasetsPerStage = 0,
        longTimeSimulationAllowed = FALSE,
        seed = NA_real_,
        calcEventsFunction = NULL,
        showStatistics = FALSE) {
    .assertRcppIsInstalled()

    if (is.null(design)) {
        design <- .getDefaultDesign(..., type = "simulation")
        .warnInCaseOfUnknownArguments(
            functionName = "getSimulationSurvival",
            ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
                design,
                powerCalculationEnabled = TRUE
            ), "showStatistics"), ...
        )
    } else {
        .assertIsTrialDesign(design)
        .warnInCaseOfUnknownArguments(
            functionName = "getSimulationSurvival",
            ignore = "showStatistics", ...
        )
        .warnInCaseOfTwoSidedPowerArgument(...)
        design <- .resetPipeOperatorQueue(design)
    }

    .assertIsSingleLogical(directionUpper, "directionUpper")
    .assertIsSingleNumber(thetaH0, "thetaH0")
    .assertIsInOpenInterval(thetaH0, "thetaH0", 0, NULL, naAllowed = TRUE)
    .assertIsNumericVector(minNumberOfEventsPerStage, "minNumberOfEventsPerStage", naAllowed = TRUE)
    .assertIsNumericVector(maxNumberOfEventsPerStage, "maxNumberOfEventsPerStage", naAllowed = TRUE)
    .assertIsSingleNumber(conditionalPower, "conditionalPower", naAllowed = TRUE)
    .assertIsInOpenInterval(conditionalPower, "conditionalPower", 0, 1, naAllowed = TRUE)
    .assertIsSingleNumber(thetaH1, "thetaH1", naAllowed = TRUE)
    .assertIsInOpenInterval(thetaH1, "thetaH1", 0, NULL, naAllowed = TRUE)
    .assertIsSinglePositiveInteger(maxNumberOfIterations, "maxNumberOfIterations", validateType = FALSE)
    .assertIsSingleNumber(seed, "seed", naAllowed = TRUE)
    .assertIsNumericVector(lambda1, "lambda1", naAllowed = TRUE)
    .assertIsNumericVector(lambda2, "lambda2", naAllowed = TRUE)
    .assertIsValidMaxNumberOfSubjects(maxNumberOfSubjects, naAllowed = TRUE)
    .assertIsIntegerVector(allocation1, "allocation1", validateType = FALSE)
    .assertIsIntegerVector(allocation2, "allocation2", validateType = FALSE)
    .assertIsInClosedInterval(allocation1, "allocation1", lower = 1L, upper = NULL)
    .assertIsInClosedInterval(allocation2, "allocation2", lower = 1L, upper = NULL)
    .assertIsSingleNumber(dropoutTime, "dropoutTime", naAllowed = TRUE)
    .assertIsSingleNumber(dropoutRate1, "dropoutRate1", naAllowed = TRUE)
    .assertIsSingleNumber(dropoutRate2, "dropoutRate2", naAllowed = TRUE)
    .assertIsSingleLogical(longTimeSimulationAllowed, "longTimeSimulationAllowed")
    .assertIsSingleLogical(showStatistics, "showStatistics", naAllowed = FALSE)
    .assertIsValidPlannedSubjectsOrEvents(design, plannedEvents, parameterName = "plannedEvents")

    if (!is.na(dropoutTime) && dropoutTime <= 0) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'dropoutTime' (", dropoutTime, ") must be > 0", call. = FALSE)
    }

    if (dropoutRate1 < 0 || dropoutRate1 >= 1) {
        stop(
            C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS,
            "'dropoutRate1' (", dropoutRate1, ") is out of bounds [0; 1)"
        )
    }

    if (dropoutRate2 < 0 || dropoutRate2 >= 1) {
        stop(
            C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS,
            "'dropoutRate2' (", dropoutRate2, ") is out of bounds [0; 1)"
        )
    }

    if (design$sided == 2) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "Only one-sided case is implemented for the survival simulation design"
        )
    }

    if (!all(is.na(lambda2)) && !all(is.na(lambda1)) &&
            length(lambda2) != length(lambda1) && length(lambda2) > 1) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "length of 'lambda2' (", length(lambda2),
            ") must be equal to length of 'lambda1' (", length(lambda1), ")"
        )
    }

    if (all(is.na(lambda2)) && !all(is.na(lambda1))) {
        warning("'lambda1' (", .arrayToString(lambda1), ") will be ignored ",
            "because 'lambda2' (", .arrayToString(lambda2), ") is undefined",
            call. = FALSE
        )
        lambda1 <- NA_real_
    }

    if (!all(is.na(lambda2)) && is.list(piecewiseSurvivalTime)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'piecewiseSurvivalTime' needs to be a numeric vector and not a list ",
            "because 'lambda2' (", .arrayToString(lambda2), ") is defined separately"
        )
    }

    thetaH1 <- .ignoreParameterIfNotUsed(
        "thetaH1", thetaH1, design$kMax > 1,
        "design is fixed ('kMax' = 1)", "Assumed effect"
    )
    if (is.na(conditionalPower) && !is.na(thetaH1)) {
        warning("'thetaH1' will be ignored because 'conditionalPower' is not defined", call. = FALSE)
    }
    conditionalPower <- .ignoreParameterIfNotUsed(
        "conditionalPower",
        conditionalPower, design$kMax > 1, "design is fixed ('kMax' = 1)"
    )
    minNumberOfEventsPerStage <- .ignoreParameterIfNotUsed(
        "minNumberOfEventsPerStage",
        minNumberOfEventsPerStage, design$kMax > 1, "design is fixed ('kMax' = 1)"
    )
    maxNumberOfEventsPerStage <- .ignoreParameterIfNotUsed(
        "maxNumberOfEventsPerStage",
        maxNumberOfEventsPerStage, design$kMax > 1, "design is fixed ('kMax' = 1)"
    )
    minNumberOfEventsPerStage <- .assertIsValidNumberOfSubjectsPerStage(minNumberOfEventsPerStage,
        "minNumberOfEventsPerStage", plannedEvents, conditionalPower, NULL, design$kMax,
        endpoint = "survival", calcSubjectsFunctionEnabled = FALSE
    )
    maxNumberOfEventsPerStage <- .assertIsValidNumberOfSubjectsPerStage(maxNumberOfEventsPerStage,
        "maxNumberOfEventsPerStage", plannedEvents, conditionalPower, NULL, design$kMax,
        endpoint = "survival", calcSubjectsFunctionEnabled = FALSE
    )

    simulationResults <- SimulationResultsSurvival$new(design, showStatistics = showStatistics)
    if (!is.na(conditionalPower)) {
        if (design$kMax > 1) {
            if (any(maxNumberOfEventsPerStage - minNumberOfEventsPerStage < 0) &&
                    !all(is.na(maxNumberOfEventsPerStage - minNumberOfEventsPerStage))) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'maxNumberOfEventsPerStage' (",
                    .arrayToString(maxNumberOfEventsPerStage),
                    ") must be not smaller than minNumberOfEventsPerStage' (",
                    .arrayToString(minNumberOfEventsPerStage), ")"
                )
            }
            .setValueAndParameterType(
                simulationResults, "minNumberOfEventsPerStage",
                minNumberOfEventsPerStage, NA_real_
            )
            .setValueAndParameterType(
                simulationResults, "maxNumberOfEventsPerStage",
                maxNumberOfEventsPerStage, NA_real_
            )
        } else {
            warning("'conditionalPower' will be ignored for fixed sample design", call. = FALSE)
        }
    } else {
        simulationResults$minNumberOfEventsPerStage <- NA_real_
        simulationResults$maxNumberOfEventsPerStage <- NA_real_
        simulationResults$.setParameterType("minNumberOfEventsPerStage", C_PARAM_NOT_APPLICABLE)
        simulationResults$.setParameterType("maxNumberOfEventsPerStage", C_PARAM_NOT_APPLICABLE)
        simulationResults$.setParameterType("conditionalPower", C_PARAM_NOT_APPLICABLE)
    }
    if (!is.na(conditionalPower) && (design$kMax == 1)) {
        warning("'conditionalPower' will be ignored for fixed sample design", call. = FALSE)
    }

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

    simulationResults$.setParameterType("seed", ifelse(is.na(seed),
        C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
    ))
    simulationResults$seed <- .setSeed(seed)

    simulationResults$.accrualTime <- accrualSetup

    accrualTime <- accrualSetup$.getAccrualTimeWithoutLeadingZero()
    simulationResults$maxNumberOfSubjects <- accrualSetup$maxNumberOfSubjects
    simulationResults$.setParameterType(
        "maxNumberOfSubjects",
        accrualSetup$.getParameterType("maxNumberOfSubjects")
    )

    simulationResults$accrualTime <- accrualSetup$.getAccrualTimeWithoutLeadingZero()
    simulationResults$.setParameterType("accrualTime", accrualSetup$.getParameterType("accrualTime"))

    simulationResults$accrualIntensity <- accrualSetup$accrualIntensity
    simulationResults$.setParameterType(
        "accrualIntensity",
        accrualSetup$.getParameterType("accrualIntensity")
    )

    simulationResults$plannedEvents <- plannedEvents
    simulationResults$.setParameterType("plannedEvents", C_PARAM_USER_DEFINED)

    pwsTimeObject <- getPiecewiseSurvivalTime(
        piecewiseSurvivalTime = piecewiseSurvivalTime,
        lambda2 = lambda2, lambda1 = lambda1, median1 = median1, median2 = median2,
        hazardRatio = hazardRatio, pi1 = pi1, pi2 = pi2, eventTime = eventTime, kappa = kappa,
        delayedResponseAllowed = TRUE, .pi1Default = C_PI_1_DEFAULT
    )

    simulationResults$.piecewiseSurvivalTime <- pwsTimeObject
    simulationResults$hazardRatio <- pwsTimeObject$hazardRatio
    simulationResults$.setParameterType("hazardRatio", pwsTimeObject$.getParameterType("hazardRatio"))
    simulationResults$.setParameterType("eventTime", pwsTimeObject$.getParameterType("eventTime"))
    simulationResults$eventTime <- pwsTimeObject$eventTime

    if (.isLambdaBasedSimulationEnabled(pwsTimeObject)) {
        simulationResults$piecewiseSurvivalTime <- pwsTimeObject$piecewiseSurvivalTime
        simulationResults$.setParameterType("piecewiseSurvivalTime", C_PARAM_USER_DEFINED)

        simulationResults$lambda2 <- pwsTimeObject$lambda2
        simulationResults$.setParameterType("lambda2", pwsTimeObject$.getParameterType("lambda2"))
        lambdaVec2 <- simulationResults$lambda2

        simulationResults$lambda1 <- pwsTimeObject$lambda1
        simulationResults$.setParameterType("lambda1", pwsTimeObject$.getParameterType("lambda1"))

        if (any(is.na(pwsTimeObject$lambda1))) {
            .assertIsValidHazardRatioVector(pwsTimeObject$hazardRatio)
            .setValueAndParameterType(
                simulationResults, "hazardRatio",
                pwsTimeObject$hazardRatio, NA_real_
            )
            numberOfResults <- length(simulationResults$hazardRatio)
            lambdaVec1 <- simulationResults$lambda2 * pwsTimeObject$hazardRatio
        } else {
            numberOfResults <- 1
            lambdaVec1 <- pwsTimeObject$lambda1
        }

        .warnInCaseOfDefinedPiValue(simulationResults, "pi1")
        .warnInCaseOfDefinedPiValue(simulationResults, "pi2")
        simulationResults$pi1 <- pwsTimeObject$pi1
        simulationResults$pi2 <- pwsTimeObject$pi2
        simulationResults$.setParameterType("pi1", pwsTimeObject$.getParameterType("pi1"))
        simulationResults$.setParameterType("pi2", pwsTimeObject$.getParameterType("pi2"))

        simulationResults$median1 <- pwsTimeObject$median1
        simulationResults$median2 <- pwsTimeObject$median2
        simulationResults$.setParameterType("median1", pwsTimeObject$.getParameterType("median1"))
        simulationResults$.setParameterType("median2", pwsTimeObject$.getParameterType("median2"))

        cdfValues1 <- .getPiecewiseExponentialDistribution(
            pwsTimeObject$piecewiseSurvivalTime, lambdaVec1,
            pwsTimeObject$piecewiseSurvivalTime,
            kappa = kappa
        )
        cdfValues2 <- .getPiecewiseExponentialDistribution(
            pwsTimeObject$piecewiseSurvivalTime, lambdaVec2,
            pwsTimeObject$piecewiseSurvivalTime,
            kappa = kappa
        )

        if (length(cdfValues1) == 1) {
            cdfValues1 <- NA_real_
            cdfValues2 <- NA_real_
        } else {
            cdfValues1 <- cdfValues1[2:length(cdfValues1)] # use values without a leading 0
            cdfValues2 <- cdfValues2[2:length(cdfValues2)]
        }

        pi1 <- NA_real_
        pi2 <- NA_real_
    } else {
        numberOfResults <- .initDesignPlanSurvivalByPiecewiseSurvivalTimeObject(
            simulationResults, pwsTimeObject
        )
        pi1 <- simulationResults$pi1
        if (all(is.na(pi1))) {
            pi1 <- getPiByLambda(simulationResults$lambda1, eventTime = eventTime, kappa = kappa)
            simulationResults$pi1 <- pi1
            simulationResults$.setParameterType("pi1", C_PARAM_GENERATED)
        }

        pi2 <- simulationResults$pi2
        if (all(is.na(pi2))) {
            pi2 <- getPiByLambda(simulationResults$lambda2, eventTime = eventTime, kappa = kappa)
            simulationResults$pi2 <- pi2
            simulationResults$.setParameterType("pi2", C_PARAM_GENERATED)
        }
        simulationResults$piecewiseSurvivalTime <- NA_real_
        lambdaVec1 <- NA_real_
        lambdaVec2 <- NA_real_
        cdfValues1 <- NA_real_
        cdfValues2 <- NA_real_
    }

    numberOfSimStepsTotal <- numberOfResults * maxNumberOfIterations *
        accrualSetup$maxNumberOfSubjects
    maxNumberOfSimStepsTotal <- 10 * 100000 * 100
    if (numberOfSimStepsTotal > maxNumberOfSimStepsTotal) {
        if (!longTimeSimulationAllowed) {
            stop(
                "Simulation stopped because long time simulation is disabled ",
                "and the defined number of single simulation steps (", numberOfSimStepsTotal,
                ") is larger than the threshold ", maxNumberOfSimStepsTotal, ". ",
                "Set 'longTimeSimulationAllowed = TRUE' to enable simulations ",
                "that take a long time (> 30 sec)"
            )
        }

        message(
            "Note that the simulation may take a long time because ",
            sprintf("%.0f", numberOfSimStepsTotal),
            " single simulation steps must be calculated"
        )
    }

    .setValueAndParameterType(simulationResults, "directionUpper", directionUpper, C_DIRECTION_UPPER_DEFAULT)
    .setValueAndParameterType(simulationResults, "dropoutRate1", dropoutRate1, C_DROP_OUT_RATE_1_DEFAULT)
    .setValueAndParameterType(simulationResults, "dropoutRate2", dropoutRate2, C_DROP_OUT_RATE_2_DEFAULT)
    .setValueAndParameterType(simulationResults, "dropoutTime", dropoutTime, C_DROP_OUT_TIME_DEFAULT)
    .setValueAndParameterType(simulationResults, "thetaH0", thetaH0, C_THETA_H0_SURVIVAL_DEFAULT)

    allocationFraction <- getFraction(allocation1 / allocation2)
    if (allocationFraction[1] != allocation1 || allocationFraction[2] != allocation2) {
        warning(sprintf(
            "allocation1 = %s and allocation2 = %s was replaced by allocation1 = %s and allocation2 = %s",
            allocation1, allocation2, allocationFraction[1], allocationFraction[2]
        ), call. = FALSE)
        allocation1 <- allocationFraction[1]
        allocation2 <- allocationFraction[2]
    }

    .setValueAndParameterType(simulationResults, "allocation1", allocation1, C_ALLOCATION_1_DEFAULT)
    .setValueAndParameterType(simulationResults, "allocation2", allocation2, C_ALLOCATION_2_DEFAULT)

    allocationRatioPlanned <- allocation1 / allocation2
    .setValueAndParameterType(
        simulationResults, "allocationRatioPlanned",
        allocationRatioPlanned, C_ALLOCATION_RATIO_DEFAULT
    )
    .setValueAndParameterType(simulationResults, "conditionalPower", conditionalPower, NA_real_)
    if (!is.na(thetaH0) && !is.na(thetaH1) && thetaH0 != 1) {
        thetaH1 <- thetaH1 / thetaH0
        .setValueAndParameterType(simulationResults, "thetaH1", thetaH1, NA_real_)
        simulationResults$.setParameterType("thetaH1", C_PARAM_GENERATED)
    } else {
        .setValueAndParameterType(simulationResults, "thetaH1", thetaH1, NA_real_)
    }
    if (is.na(conditionalPower)) {
        simulationResults$.setParameterType("thetaH1", C_PARAM_NOT_APPLICABLE)
    }
    .setValueAndParameterType(simulationResults, "kappa", kappa, 1)
    .setValueAndParameterType(
        simulationResults, "maxNumberOfIterations",
        as.integer(maxNumberOfIterations), C_MAX_SIMULATION_ITERATIONS_DEFAULT
    )

    phi <- -c(log(1 - dropoutRate1), log(1 - dropoutRate2)) / dropoutTime

    densityIntervals <- accrualTime
    if (length(accrualTime) > 1) {
        densityIntervals[2:length(accrualTime)] <-
            accrualTime[2:length(accrualTime)] - accrualTime[1:(length(accrualTime) - 1)]
    }
    densityVector <- accrualSetup$accrualIntensity / sum(densityIntervals * accrualSetup$accrualIntensity)

    intensityReplications <- round(densityVector * densityIntervals * accrualSetup$maxNumberOfSubjects)

    if (all(intensityReplications > 0)) {
        accrualTimeValue <- cumsum(rep(
            1 / (densityVector * accrualSetup$maxNumberOfSubjects), intensityReplications
        ))
    } else {
        accrualTimeValue <- cumsum(rep(
            1 / (densityVector[1] * accrualSetup$maxNumberOfSubjects),
            intensityReplications[1]
        ))
        if (length(accrualIntensity) > 1 && length(intensityReplications) > 1) {
            for (i in 2:min(length(accrualIntensity), length(intensityReplications))) {
                if (intensityReplications[i] > 0) {
                    accrualTimeValue <- c(
                        accrualTimeValue,
                        accrualTime[i - 1] +
                            cumsum(rep(
                                1 / (densityVector[i] * accrualSetup$maxNumberOfSubjects),
                                intensityReplications[i]
                            ))
                    )
                }
            }
        }
    }

    accrualTimeValue <- accrualTimeValue[1:accrualSetup$maxNumberOfSubjects]

    # to avoid last value to be NA_real_
    i <- accrualSetup$maxNumberOfSubjects
    while (is.na(accrualTimeValue[i])) {
        accrualTimeValue[i] <- accrualTime[length(accrualTime)]
        i <- i - 1
    }

    treatmentGroup <- rep(
        c(rep(1, allocation1), rep(2, allocation2)),
        ceiling(accrualSetup$maxNumberOfSubjects /
            (allocation1 + allocation2))
    )[1:accrualSetup$maxNumberOfSubjects]

    if (.isTrialDesignFisher(design)) {
        alpha0Vec <- design$alpha0Vec
        futilityBounds <- rep(NA_real_, design$kMax - 1)
    } else {
        alpha0Vec <- rep(NA_real_, design$kMax - 1)
        futilityBounds <- design$futilityBounds
    }

    if (.isTrialDesignGroupSequential(design)) {
        designNumber <- 1L
    } else if (.isTrialDesignInverseNormal(design)) {
        designNumber <- 2L
    } else if (.isTrialDesignFisher(design)) {
        designNumber <- 3L
    }

    calcSubjectsFunctionList <- .getCalcSubjectsFunction(
        design = design,
        simulationResults = simulationResults,
        calcFunction = calcEventsFunction,
        expectedFunction = function(stage,
                conditionalPower,
                thetaH0,
                estimatedTheta,
                plannedEvents,
                eventsOverStages,
                minNumberOfEventsPerStage,
                maxNumberOfEventsPerStage,
                allocationRatioPlanned,
                conditionalCriticalValue) {
            NULL
        }
    )
    calcEventsFunctionType <- calcSubjectsFunctionList$calcSubjectsFunctionType
    calcEventsFunctionR <- calcSubjectsFunctionList$calcSubjectsFunctionR
    calcEventsFunctionCpp <- calcSubjectsFunctionList$calcSubjectsFunctionCpp

    resultData <- .getSimulationSurvivalCpp(
        designNumber                   = designNumber,
        kMax                           = design$kMax,
        sided                          = design$sided,
        criticalValues                 = design$criticalValues,
        informationRates               = design$informationRates,
        conditionalPower               = conditionalPower,
        plannedEvents                  = plannedEvents,
        thetaH1                        = thetaH1,
        minNumberOfEventsPerStage      = minNumberOfEventsPerStage,
        maxNumberOfEventsPerStage      = maxNumberOfEventsPerStage,
        directionUpper                 = directionUpper,
        allocationRatioPlanned         = allocationRatioPlanned,
        accrualTime                    = accrualTimeValue,
        treatmentGroup                 = treatmentGroup,
        thetaH0                        = thetaH0,
        futilityBounds                 = futilityBounds,
        alpha0Vec                      = alpha0Vec,
        pi1Vec                         = pi1,
        pi2                            = pi2,
        eventTime                      = eventTime,
        piecewiseSurvivalTime          = .getPiecewiseExpStartTimesWithoutLeadingZero(pwsTimeObject$piecewiseSurvivalTime),
        cdfValues1                     = cdfValues1,
        cdfValues2                     = cdfValues2,
        lambdaVec1                     = lambdaVec1,
        lambdaVec2                     = lambdaVec2,
        phi                            = phi,
        maxNumberOfSubjects            = accrualSetup$maxNumberOfSubjects,
        maxNumberOfIterations          = maxNumberOfIterations,
        maxNumberOfRawDatasetsPerStage = maxNumberOfRawDatasetsPerStage,
        kappa                          = kappa,
        calcEventsFunctionType         = calcEventsFunctionType,
        calcEventsFunctionR            = calcEventsFunctionR,
        calcEventsFunctionCpp          = calcEventsFunctionCpp
    )

    overview <- resultData$overview
    if (length(overview) == 0 || nrow(overview) == 0) {
        stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "no simulation results calculated")
    }

    n <- nrow(overview)
    overview <- cbind(
        design = rep(sub("^TrialDesign", "", .getClassName(design)), n),
        overview
    )

    if (pwsTimeObject$.isPiBased() &&
            pwsTimeObject$.getParameterType("hazardRatio") != C_PARAM_USER_DEFINED) {
        simulationResults$hazardRatio <- matrix(overview$hazardRatio, nrow = design$kMax)[1, ]
    }
    simulationResults$iterations <- matrix(as.integer(overview$iterations), nrow = design$kMax)
    if (!is.null(overview$eventsPerStage)) {
        simulationResults$singleEventsPerStage <- matrix(overview$eventsPerStage, nrow = design$kMax)
        .addDeprecatedFieldValues(simulationResults, "eventsPerStage", simulationResults$singleEventsPerStage)
    }
    simulationResults$eventsNotAchieved <- matrix(overview$eventsNotAchieved, nrow = design$kMax)
    if (any(simulationResults$eventsNotAchieved > 0)) {
        warning("Presumably due to drop-outs, required number of events ",
            "were not achieved for at least one situation. ",
            "Increase the maximum number of subjects (",
            accrualSetup$maxNumberOfSubjects, ") ",
            "to avoid this situation",
            call. = FALSE
        )
    }

    simulationResults$numberOfSubjects <- matrix(overview$numberOfSubjects, nrow = design$kMax)

    simulationResults$numberOfSubjects1 <-
        .getNumberOfSubjects1(simulationResults$numberOfSubjects, allocationRatioPlanned)
    simulationResults$numberOfSubjects2 <-
        .getNumberOfSubjects2(simulationResults$numberOfSubjects, allocationRatioPlanned)
    if (any(allocationRatioPlanned != 1)) {
        simulationResults$.setParameterType("numberOfSubjects1", C_PARAM_GENERATED)
        simulationResults$.setParameterType("numberOfSubjects2", C_PARAM_GENERATED)
    }

    simulationResults$overallReject <- matrix(overview$overallReject, nrow = design$kMax)[1, ]
    if (design$kMax > 1) {
        simulationResults$rejectPerStage <- matrix(overview$rejectPerStage, nrow = design$kMax)
    } else {
        simulationResults$rejectPerStage <- matrix(simulationResults$overallReject, nrow = 1)
    }

    if (!all(is.na(overview$conditionalPowerAchieved))) {
        simulationResults$conditionalPowerAchieved <- matrix(
            overview$conditionalPowerAchieved,
            nrow = design$kMax
        )
        simulationResults$.setParameterType("conditionalPowerAchieved", C_PARAM_GENERATED)
    }

    if (design$kMax == 1) {
        simulationResults$.setParameterType("numberOfSubjects", C_PARAM_NOT_APPLICABLE)
        simulationResults$.setParameterType("singleEventsPerStage", C_PARAM_NOT_APPLICABLE)
    }

    if (design$kMax > 1) {
        if (numberOfResults == 1) {
            simulationResults$futilityPerStage <- matrix(
                overview$futilityPerStage[1:(design$kMax - 1)],
                nrow = design$kMax - 1
            )
        } else {
            simulationResults$futilityPerStage <- matrix(
                matrix(
                    overview$futilityPerStage,
                    nrow = design$kMax
                )[1:(design$kMax - 1), ],
                nrow = design$kMax - 1
            )
        }
    }
    if (design$kMax > 1) {
        simulationResults$futilityStop <- matrix(overview$futilityStop, nrow = design$kMax)[1, ]
        simulationResults$earlyStop <- simulationResults$futilityStop +
            simulationResults$overallReject - simulationResults$rejectPerStage[design$kMax, ]
    } else {
        simulationResults$futilityStop <- rep(0, numberOfResults)
        simulationResults$earlyStop <- rep(0, numberOfResults)
    }

    simulationResults$analysisTime <- matrix(overview$analysisTime, nrow = design$kMax)
    simulationResults$studyDuration <- matrix(overview$studyDuration, nrow = design$kMax)[1, ]

    if (design$kMax > 1) {
        subData <- simulationResults$rejectPerStage[1:(design$kMax - 1), ] +
            simulationResults$futilityPerStage
        pStop <- rbind(subData, 1 - colSums(subData))

        numberOfSubjects <- simulationResults$numberOfSubjects
        numberOfSubjects[is.na(numberOfSubjects)] <- 0
        simulationResults$expectedNumberOfSubjects <- diag(t(numberOfSubjects) %*% pStop)

        if (!is.null(simulationResults$singleEventsPerStage) &&
                nrow(simulationResults$singleEventsPerStage) > 0 &&
                ncol(simulationResults$singleEventsPerStage) > 0) {
            simulationResults$cumulativeEventsPerStage <- .convertStageWiseToOverallValues(
                simulationResults$singleEventsPerStage
            )
            simulationResults$.setParameterType("cumulativeEventsPerStage", C_PARAM_GENERATED)
            .addDeprecatedFieldValues(
                simulationResults,
                "overallEventsPerStage", simulationResults$cumulativeEventsPerStage
            )
            simulationResults$expectedNumberOfEvents <-
                diag(t(simulationResults$cumulativeEventsPerStage) %*% pStop)
        }
    } else {
        simulationResults$expectedNumberOfSubjects <-
            as.numeric(simulationResults$numberOfSubjects)
        if (!is.null(simulationResults$singleEventsPerStage) &&
                nrow(simulationResults$singleEventsPerStage) > 0 &&
                ncol(simulationResults$singleEventsPerStage) > 0) {
            simulationResults$cumulativeEventsPerStage <- simulationResults$singleEventsPerStage
            .addDeprecatedFieldValues(
                simulationResults,
                "overallEventsPerStage", simulationResults$cumulativeEventsPerStage
            )
            simulationResults$expectedNumberOfEvents <-
                as.numeric(simulationResults$cumulativeEventsPerStage)
        }
    }
    if (is.null(simulationResults$expectedNumberOfEvents) ||
            length(simulationResults$expectedNumberOfEvents) == 0) {
        warning("Failed to calculate expected number of events", call. = FALSE)
    }

    simulationResults$.data <- resultData$data[!is.na(resultData$data$iterationNumber), ]

    stages <- 1:design$kMax
    rawData <- resultData$rawData
    if (!is.null(rawData) && nrow(rawData) > 0 && ncol(rawData) > 0) {
        rawData <- rawData[!is.na(rawData$iterationNumber), ]
    }
    if (!is.null(rawData) && nrow(rawData) > 0 && ncol(rawData) > 0) {
        rawData <- rawData[order(rawData$iterationNumber, rawData$subjectId), ]
        rownames(rawData) <- NULL

        stopStageNumbers <- rawData$stopStage
        missingStageNumbers <- c()
        if (length(stopStageNumbers) > 0) {
            stopStageNumbers <- order(unique(stopStageNumbers))
            missingStageNumbers <- stages[!which(stages %in% stopStageNumbers)]
        } else {
            missingStageNumbers <- stages
        }
        if (length(missingStageNumbers) > 0) {
            warning("Could not get rawData (individual results) for stages ",
                .arrayToString(missingStageNumbers),
                call. = FALSE
            )
        }
    } else {
        rawData <- data.frame(
            iterationNumber = numeric(0),
            stopStage = numeric(0),
            pi1 = numeric(0),
            pi2 = numeric(0),
            subjectId = numeric(0),
            accrualTime = numeric(0),
            treatmentGroup = numeric(0),
            survivalTime = numeric(0),
            dropoutTime = numeric(0),
            observationTime = numeric(0),
            timeUnderObservation = numeric(0),
            event = logical(0),
            dropoutEvent = logical(0),
            censorIndicator = numeric(0)
        )
        if (maxNumberOfRawDatasetsPerStage > 0) {
            warning("Could not get rawData (individual results) for stages ",
                .arrayToString(stages),
                call. = FALSE
            )
        }
    }

    if (pwsTimeObject$.isLambdaBased() || length(pi1) < 2) {
        rawData <- rawData[, !(colnames(rawData) %in% c("pi1", "pi2"))]
    }

    # remove censorIndicator because it will not be calculated yet
    rawData <- rawData[, colnames(rawData) != "censorIndicator"]

    simulationResults$.rawData <- rawData

    return(simulationResults)
}
