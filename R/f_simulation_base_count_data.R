## |
## |  *Simulation of count data*
## |
## |  This file is part of the R package rpact:
## |  Confirmatory Adaptive Clinical Trial Design and Analysis
## |
## |  Author: Gernot Wassmer, PhD, Tobias Muetze, PhD, and Friedrich Pahlke, PhD
## |  Licensed under "GNU Lesser General Public License" version 3
## |  License text can be found here: https://www.r-project.org/Licenses/LGPL-3
## |
## |  RPACT company website: https://www.rpact.com
## |  rpact package website: https://www.rpact.org
## |
## |  Contact us for information about our services: info@rpact.com
## |
## |  File version: $Revision: 7744 $
## |  Last changed: $Date: 2024-03-22 17:38:03 +0100 (Fr, 22 Mrz 2024) $
## |  Last changed by: $Author: pahlke $
## |

.getInformationCountData <- function(lambda1,
        lambda2,
        overdispersion,
        recruit1,
        recruit2) {
    sumLambda1 <- sum(recruit1 * lambda1 /
        (1 + overdispersion * recruit1 * lambda1))
    sumLambda2 <- sum(recruit2 * lambda2 /
        (1 + overdispersion * recruit2 * lambda2))
    return(1 / (1 / sumLambda1 + 1 / sumLambda2))
}

.getGeneratedEventTimesCountData <- function(recruit1, recruit2, accrualTime, followUpTime,
        lambda1, lambda2, overdispersion, fixedFollowUp = FALSE) {
    n1 <- length(recruit1)
    n2 <- length(recruit2)
    totalRecruitment <- c(recruit1, recruit2)

    if (fixedFollowUp) {
        followUp <- rep(followUpTime, times = n1 + n2)
    } else {
        followUp <- accrualTime + followUpTime - totalRecruitment
    }

    # generate number of events by subject
    nEvents <- rnbinom(
        n = n1 + n2,
        mu = c(lambda1 * followUp[1:n1], lambda2 * followUp[(n1 + 1):(n1 + n2)]),
        size = 1 / overdispersion
    )

    # generate event times through a homogeneous Poisson process
    output <- matrix(NA, nrow = sum(nEvents) + n1 + n2, ncol = 8)
    colnames(output) <- c(
        "id", "recruitTime", "startEvent", "stopEvent",
        "startCalendar", "stopCalendar", "status", "group"
    )
    index <- 1
    for (i in 1:(n1 + n2)) {
        if (nEvents[i] == 0) {
            output[index, c("id", "recruitTime", "startEvent", "stopEvent")] <-
                c(i, totalRecruitment[i], 0, followUp[i])
            output[index, "status"] <- c(0)
            output[index, "group"] <- if (i <= n1) 1 else 2
            index <- index + 1
        } else if (nEvents[i] != 0) {
            eventTime <- sort(runif(nEvents[i], min = 0, max = followUp[i]))
            indices <- index:(index + nEvents[i])
            output[indices, "id"] <- i
            output[indices, "recruitTime"] <- totalRecruitment[i]
            output[indices, "startEvent"] <- c(0, eventTime)
            output[indices, "stopEvent"] <- c(eventTime, followUp[i])
            output[indices, "status"] <- c(rep(1, times = nEvents[i]), 0)
            output[indices, "group"] <- if (i <= n1) 1 else 2
            index <- index + nEvents[i] + 1
        }
    }

    # calculate the calendar times
    output[, "startCalendar"] <- output[, "recruitTime"] + output[, "startEvent"]
    output[, "stopCalendar"] <- output[, "recruitTime"] + output[, "stopEvent"]

    return(list(output = output, nEvents = nEvents))
}

#' @title
#' Get Simulation Counts
#'
#' @description
#' Returns the simulated power, stopping probabilities, conditional power, and expected sample size for
#' testing means rates for negative binomial distributed event numbers in the two treatment groups testing situation.
#'
#' @inheritParams param_design_with_default
#' @inheritParams param_plannedCalendarTime
#' @inheritParams param_thetaH0
#' @inheritParams param_lambda_counts
#' @inheritParams param_lambda1_counts
#' @inheritParams param_lambda2_counts
#' @inheritParams param_theta_counts
#' @inheritParams param_fixedExposureTime_counts
#' @inheritParams param_accrualTime_counts
#' @inheritParams param_accrualIntensity_counts
#' @inheritParams param_followUpTime_counts
#' @inheritParams param_plannedMaxSubjects
#' @inheritParams param_overdispersion_counts
#' @inheritParams param_directionUpper
#' @inheritParams param_allocationRatioPlanned
#' @inheritParams param_plannedSubjects
#' @inheritParams param_minNumberOfSubjectsPerStage
#' @inheritParams param_plannedMaxSubjectsPerStage
#' @inheritParams param_conditionalPowerSimulation
#' @inheritParams param_maxNumberOfIterations
#' @inheritParams param_calcSubjectsFunction
#' @inheritParams param_seed
#' @inheritParams param_three_dots
#' @inheritParams param_showStatistics
#'
#' @details
#' At given design the function simulates the power, stopping probabilities, conditional power, and expected
#' sample size at given number of subjects and parameter configuration.
#' Additionally, an allocation ratio = n1/n2 and a null hypothesis value thetaH0 can be specified.
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
#'   \item \code{lambda1}: The assumed or derived event rate in the treatment group (if available).
#'   \item \code{lambda2}: The assumed or derived event rate in the control group (if available).
#'   \item \code{numberOfSubjects}: The number of subjects under consideration when the
#'         (interim) analysis takes place.
#'   \item \code{rejectPerStage}: 1 if null hypothesis can be rejected, 0 otherwise.
#'   \item \code{futilityPerStage}: 1 if study should be stopped for futility, 0 otherwise.
#'   \item \code{testStatistic}: The test statistic that is used for the test decision,
#'         depends on which design was chosen (group sequential, inverse normal,
#'         or Fisher combination test)'
#'   \item \code{testStatisticsPerStage}: The test statistic for each stage if only data from
#'         the considered stage is taken into account.
#'   \item \code{overallLambda1}: The cumulative rate in treatment group 1.
#'   \item \code{overallLambda2}: The cumulative rate in treatment group 2.
#'   \item \code{stagewiseLambda1}: The stage-wise rate in treatment group 1.
#'   \item \code{stagewiseLambda2}: The stage-wise rate in treatment group 2.
#'   \item \code{sampleSizesPerStage1}: The stage-wise sample size in treatment group 1.
#'   \item \code{sampleSizesPerStage2}: The stage-wise sample size in treatment group 2.
#'   \item \code{trialStop}: \code{TRUE} if study should be stopped for efficacy or futility or final stage, \code{FALSE} otherwise.
#'   \item \code{conditionalPowerAchieved}: The conditional power for the subsequent stage of the trial for
#'         selected sample size and effect. The effect is either estimated from the data or can be
#'         user defined with \code{lambda1H1} and \code{lambda2H1}.
#' }
#'
#' @template return_object_simulation_results
#' @template how_to_get_help_for_generics
#'
#' @template examples_get_simulation_count_data
#'
#' @export
#'
getSimulationCounts <- function(design = NULL,
        ...,
        plannedCalendarTime,
        plannedMaxSubjects = NA_real_,
        lambda1 = NA_real_,
        lambda2 = NA_real_,
        lambda = NA_real_,
        theta = NA_real_,
        directionUpper = TRUE, # C_DIRECTION_UPPER_DEFAULT
        thetaH0 = 1,
        overdispersion = 0,
        fixedExposureTime = NA_real_,
        accrualTime = NA_real_,
        accrualIntensity = NA_real_,
        followUpTime = NA_real_,
        allocationRatioPlanned = NA_real_,
        maxNumberOfIterations = 1000L, # C_MAX_SIMULATION_ITERATIONS_DEFAULT
        seed = NA_real_,
        calcSubjectsFunction = NULL,
        showStatistics = FALSE) {
    if (is.na(directionUpper)) {
        directionUpper <- TRUE
    }
    if (is.null(design)) {
        design <- .getDefaultDesign(..., type = "simulation")
        .warnInCaseOfUnknownArguments(
            functionName = "getSimulationCounts",
            ignore = .getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
                design,
                powerCalculationEnabled = TRUE
            )
        )
    } else {
        .assertIsTrialDesign(design)
        .warnInCaseOfUnknownArguments(
            functionName = "getSimulationCounts",
            ignore = c("showStatistics"), ...
        )
        .warnInCaseOfTwoSidedPowerArgument(...)
        .warnInCaseOfTwoSidedPowerIsDisabled(design)
    }
    if (!any(is.na(theta))) {
        totalCases <- length(theta)
        lambda1 <- rep(NA_real_, totalCases)
    } else if (!any(is.na(lambda1))) {
        totalCases <- length(lambda1)
    } else {
        totalCases <- 1
    }

    kMax <- design$kMax
    alpha <- design$alpha
    sided <- design$sided
    sampleSizeEnabled <- FALSE

    allocationRatioPlanned <- .assertIsValidAllocationRatioPlannedSampleSize(allocationRatioPlanned, plannedMaxSubjects)
    .assertIsValidEffectCountData(
        sampleSizeEnabled, sided, lambda1, lambda2, lambda, theta,
        thetaH0, overdispersion
    )
    if (!is.na(lambda2) && !any(is.na(theta))) {
        lambda1 <- lambda2 * theta
    } else if (!any(is.na(lambda1)) && !any(is.na(theta))) {
        lambda2 <- lambda1 / theta
    }
    .assertIsValidParametersCountData(
        sampleSizeEnabled = sampleSizeEnabled,
        simulationEnabled = TRUE,
        fixedExposureTime = fixedExposureTime,
        followUpTime = followUpTime,
        accrualTime = accrualTime,
        accrualIntensity = accrualIntensity,
        maxNumberOfSubjects = plannedMaxSubjects
    )
    .assertAreValidCalendarTimes(plannedCalendarTime, kMax)
    if (any(is.na(accrualTime))) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'accrualTime' needs to be specified for simulating count data"
        )
    }

    simulationResults <- SimulationResultsBaseCountData$new(design = design)

    if ((length(accrualTime) > 1) && (accrualTime[1] == 0)) {
        accrualTime <- accrualTime[-1]
    }

    .assertIsSinglePositiveInteger(maxNumberOfIterations, "maxNumberOfIterations", validateType = FALSE)
    .assertIsSingleNumber(seed, "seed", naAllowed = TRUE)
    .assertIsSingleLogical(showStatistics, "showStatistics", naAllowed = FALSE)
    if (!is.null(calcSubjectsFunction) && design$kMax == 1) {
        warning("'calcSubjectsFunction' will be ignored for fixed sample design", call. = FALSE)
    }

    .setValueAndParameterType(simulationResults, "plannedCalendarTime", plannedCalendarTime, NA_real_)
    .setValueAndParameterType(simulationResults, "plannedMaxSubjects", plannedMaxSubjects, NA_real_, notApplicableIfNA = TRUE)
    .setValueAndParameterType(simulationResults, "lambda1", lambda1, NA_real_, notApplicableIfNA = TRUE)
    .setValueAndParameterType(simulationResults, "lambda2", lambda2, NA_real_, notApplicableIfNA = TRUE)
    .setValueAndParameterType(simulationResults, "lambda", lambda, NA_real_, notApplicableIfNA = TRUE)
    .setValueAndParameterType(simulationResults, "theta", theta, NA_real_, notApplicableIfNA = TRUE)
    .setValueAndParameterType(simulationResults, "thetaH0", thetaH0, 1, notApplicableIfNA = TRUE)
    .setValueAndParameterType(simulationResults, "directionUpper", directionUpper, C_DIRECTION_UPPER_DEFAULT)
    .setValueAndParameterType(simulationResults, "overdispersion", overdispersion, 0)
    .setValueAndParameterType(simulationResults, "fixedExposureTime", fixedExposureTime, NA_real_, notApplicableIfNA = TRUE)
    .setValueAndParameterType(simulationResults, "accrualTime", accrualTime, NA_real_, notApplicableIfNA = TRUE)
    .setValueAndParameterType(simulationResults, "accrualIntensity", accrualIntensity, NA_real_, notApplicableIfNA = TRUE)
    .setValueAndParameterType(simulationResults, "followUpTime", followUpTime, NA_real_, notApplicableIfNA = TRUE)
    .setValueAndParameterType(simulationResults, "maxNumberOfIterations", as.integer(maxNumberOfIterations), C_MAX_SIMULATION_ITERATIONS_DEFAULT)
    .setValueAndParameterType(simulationResults, "allocationRatioPlanned", allocationRatioPlanned, C_ALLOCATION_RATIO_DEFAULT)
    simulationResults$.setParameterType(
        "calcSubjectsFunction",
        ifelse(design$kMax == 1, C_PARAM_NOT_APPLICABLE,
            ifelse(!is.null(calcSubjectsFunction) && design$kMax > 1,
                C_PARAM_USER_DEFINED, C_PARAM_DEFAULT_VALUE
            )
        )
    )
    simulationResults$.setParameterType("seed", ifelse(is.na(seed), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))
    simulationResults$seed <- .setSeed(seed)

    if (!is.na(lambda2) && !any(is.na(theta))) {
        lambda1 <- lambda2 * theta
        simulationResults$lambda1 <- lambda1
        simulationResults$.setParameterType("lambda1", C_PARAM_GENERATED)
    } else if (!any(is.na(lambda1)) && !any(is.na(theta))) {
        lambda2 <- lambda1 / theta
        simulationResults$lambda2 <- lambda2
        simulationResults$.setParameterType("lambda2", C_PARAM_GENERATED)
    } else if (!is.na(lambda) && !any(is.na(theta))) {
        simulationResults$.setParameterType("lambda1", C_PARAM_GENERATED)
        simulationResults$.setParameterType("lambda2", C_PARAM_GENERATED)
    }

    if (kMax == 1) {
        futilityPerStage <- NULL
        rejectPerStage <- NULL
        earlyStop <- NULL
    } else {
        futilityPerStage <- matrix(NA_real_, kMax - 1, totalCases)
        rejectPerStage <- matrix(NA_real_, kMax, totalCases)
        earlyStop <- matrix(NA_real_, 1, totalCases)
    }
    overallReject <- rep(NA_real_, totalCases)
    iterations <- matrix(0, kMax, totalCases)

    for (iCase in 1:totalCases) {
        if (!(is.na(lambda)) && !any(is.na(theta))) {
            lambda2 <- (1 + allocationRatioPlanned) * lambda / (1 + allocationRatioPlanned * theta[iCase])
            lambda1[iCase] <- lambda2 * theta[iCase]
        }
        if (!any(is.na(accrualIntensity))) {
            const <- allocationRatioPlanned / (1 + allocationRatioPlanned)
            if (length(unique(accrualIntensity)) == 1) {
                recruit1 <- seq(0, accrualTime[length(accrualIntensity)],
                    length.out = accrualTime[length(accrualIntensity)] * accrualIntensity[1] * const
                )
                recruit2 <- seq(0, accrualTime[length(accrualIntensity)],
                    length.out = accrualTime[length(accrualIntensity)] * accrualIntensity[1] * (1 - const)
                )
            } else {
                recruit1 <- seq(0, accrualTime[1], length.out = accrualTime[1] * accrualIntensity[1] * const)
                recruit2 <- seq(0, accrualTime[1], length.out = accrualTime[1] * accrualIntensity[1] * (1 - const))
                for (i in 2:length(accrualIntensity)) {
                    recruit1 <- c(recruit1, seq(accrualTime[i - 1] + 1 / accrualIntensity[i],
                        accrualTime[i],
                        length.out = (accrualTime[i] - accrualTime[i - 1]) *
                            accrualIntensity[i] * const
                    ))
                    recruit2 <- c(recruit2, seq(accrualTime[i - 1] + 1 / accrualIntensity[i],
                        accrualTime[i],
                        length.out = (accrualTime[i] - accrualTime[i - 1]) *
                            accrualIntensity[i] * (1 - const)
                    ))
                }
            }
            n1 <- length(recruit1)
            n2 <- length(recruit2)
            nTotal <- n1 + n2
        } else {
            n2 <- plannedMaxSubjects / (1 + allocationRatioPlanned)
            n1 <- allocationRatioPlanned * n2
            nTotal <- n1 + n2
            recruit1 <- seq(0, accrualTime, length.out = n1)
            recruit2 <- seq(0, accrualTime, length.out = n2)
        }

        reject <- rep(0, kMax)
        futility <- rep(0, kMax - 1)
        if (!is.na(fixedExposureTime)) {
            followUpTime <- fixedExposureTime
        }
        for (i in 1:maxNumberOfIterations) {
            if (kMax == 1) {
                recruit1 <- seq(0, accrualTime, length.out = n1)
                recruit2 <- seq(0, accrualTime, length.out = n2)
                if (is.na(fixedExposureTime)) {
                    timeUnderObservation1 <- pmax(accrualTime + followUpTime - recruit1, 0)
                    timeUnderObservation2 <- pmax(accrualTime + followUpTime - recruit2, 0)
                } else {
                    timeUnderObservation1 <- pmax(pmin(
                        accrualTime + followUpTime - recruit1,
                        fixedExposureTime
                    ), 0)
                    timeUnderObservation2 <- pmax(pmin(
                        accrualTime + followUpTime - recruit2,
                        fixedExposureTime
                    ), 0)
                }
                counts1 <- rnbinom(
                    n = n1, mu = lambda1[iCase] * timeUnderObservation1,
                    size = 1 / overdispersion
                )
                counts2 <- rnbinom(
                    n = n2, mu = lambda2 * timeUnderObservation2,
                    size = 1 / overdispersion
                )
                nb <- .getNegativeBinomialEstimates(
                    counts1 = counts1, counts2 = counts2,
                    t1 = timeUnderObservation1, t2 = timeUnderObservation2
                )
                info_Analysis <- .getInformationCountData(
                    lambda1 = nb[1],
                    lambda2 = nb[2],
                    overdispersion = nb[3],
                    recruit1 = timeUnderObservation1,
                    recruit2 = timeUnderObservation2
                )
                z <- (2 * directionUpper - 1) * (log(nb[1]) - log(nb[2]) - log(thetaH0)) * sqrt(info_Analysis)
                if (!is.na(z) && z > design$criticalValues[1]) {
                    reject[1] <- reject[1] + 1
                }
                iterations[1, iCase] <- iterations[1, iCase] + 1
            } else {
                counts <- rep(0, length(recruit1) + length(recruit2))
                dfStartStop <- .getGeneratedEventTimesCountData(
                    recruit1 = recruit1,
                    recruit2 = recruit2,
                    accrualTime = accrualTime,
                    followUpTime = followUpTime,
                    lambda1 = lambda1[iCase],
                    lambda2 = lambda2,
                    overdispersion = overdispersion,
                    fixedFollowUp = !is.na(fixedExposureTime)
                )
                for (k in 1:kMax) {
                    if (is.na(fixedExposureTime)) {
                        timeUnderObservation1 <- (plannedCalendarTime[k] -
                            recruit1)[plannedCalendarTime[k] - recruit1 >= 0]
                        timeUnderObservation2 <- (plannedCalendarTime[k] -
                            recruit2)[plannedCalendarTime[k] - recruit2 >= 0]
                    } else {
                        timeUnderObservation1 <- pmin(
                            plannedCalendarTime[k] - recruit1,
                            fixedExposureTime
                        )[plannedCalendarTime[k] - recruit1 >= 0]
                        timeUnderObservation2 <- pmin(
                            plannedCalendarTime[k] - recruit2,
                            fixedExposureTime
                        )[plannedCalendarTime[k] - recruit2 >= 0]
                    }
                    if (k < kMax) {
                        kthStageWithEvents <- dfStartStop$output[
                            dfStartStop$output[, "status"] == 1 &
                                dfStartStop$output[, "recruitTime"] <= plannedCalendarTime[k] &
                                dfStartStop$output[, "stopCalendar"] <= plannedCalendarTime[k],
                        ]
                        if (length(kthStageWithEvents) > 0 && nrow(kthStageWithEvents) > 0) {
                            tab <- table(kthStageWithEvents[, "id"])
                            idx <- as.integer(names(tab))
                            counts[idx] <- as.vector(tab)
                        }
                        counts1 <- counts[1:length(timeUnderObservation1)]
                        counts2 <- counts[(length(recruit1) + 1):(length(recruit1) + length(timeUnderObservation2))]
                    } else {
                        counts1 <- dfStartStop$nEvents[1:n1]
                        counts2 <- dfStartStop$nEvents[(n1 + 1):(n1 + n2)]
                    }
                    nb <- .getNegativeBinomialEstimates(
                        counts1 = counts1, counts2 = counts2,
                        t1 = timeUnderObservation1, t2 = timeUnderObservation2
                    )
                    info_Analysis <- .getInformationCountData(
                        lambda1 = nb[1],
                        lambda2 = nb[2],
                        overdispersion = nb[3],
                        recruit1 = timeUnderObservation1,
                        recruit2 = timeUnderObservation2
                    )
                    z <- (2 * directionUpper - 1) * (log(nb[1]) - log(nb[2]) - log(thetaH0)) * sqrt(info_Analysis)
                    iterations[k, iCase] <- iterations[k, iCase] + 1
                    if (!is.na(z)) {
                        if (z > design$criticalValues[k]) {
                            reject[k] <- reject[k] + 1
                            break
                        }
                        if (z < design$futilityBounds[k] && k < kMax) {
                            futility[k] <- futility[k] + 1
                            break
                        }
                    }
                }
            }
        }
        if (kMax > 1) {
            futilityPerStage[, iCase] <- futility / i
            rejectPerStage[, iCase] <- reject / i
            earlyStop[1, iCase] <- sum(reject[1:(kMax - 1)] + futility) / i
        }
        overallReject[iCase] <- cumsum(reject / i)[kMax]
    }

    if (design$kMax > 1) {
        simulationResults$futilityPerStage <- futilityPerStage
        simulationResults$.setParameterType(
            "futilityPerStage",
            ifelse(!all(is.na(futilityPerStage)) &&
                any(futilityPerStage > 1e-06, na.rm = TRUE),
            C_PARAM_GENERATED, C_PARAM_NOT_APPLICABLE
            )
        )

        simulationResults$futilityStop <- base::colSums(futilityPerStage, na.rm = TRUE)
        simulationResults$.setParameterType(
            "futilityStop",
            ifelse(!all(is.na(simulationResults$futilityStop)) &&
                any(simulationResults$futilityStop > 1e-06, na.rm = TRUE),
            C_PARAM_GENERATED, C_PARAM_NOT_APPLICABLE
            )
        )

        simulationResults$rejectPerStage <- rejectPerStage
        simulationResults$.setParameterType("rejectPerStage", C_PARAM_GENERATED)

        simulationResults$earlyStop <- earlyStop
        simulationResults$.setParameterType(
            "earlyStop", ifelse(!all(is.na(earlyStop)), C_PARAM_GENERATED, C_PARAM_NOT_APPLICABLE)
        )
    }

    simulationResults$iterations <- iterations
    simulationResults$.setParameterType("iterations", C_PARAM_GENERATED)

    simulationResults$numberOfSubjects <- n1 + n2
    simulationResults$.setParameterType(
        "numberOfSubjects",
        ifelse(any(is.na(accrualIntensity)), C_PARAM_USER_DEFINED, C_PARAM_GENERATED)
    )

    simulationResults$numberOfSubjects1 <- n1
    simulationResults$.setParameterType(
        "numberOfSubjects1",
        ifelse(allocationRatioPlanned == 1, C_PARAM_NOT_APPLICABLE, C_PARAM_GENERATED)
    )
    simulationResults$numberOfSubjects2 <- n2
    simulationResults$.setParameterType(
        "numberOfSubjects2",
        ifelse(allocationRatioPlanned == 1, C_PARAM_NOT_APPLICABLE, C_PARAM_GENERATED)
    )

    simulationResults$overallReject <- overallReject
    simulationResults$.setParameterType("overallReject", C_PARAM_GENERATED)

    if (all(is.na(theta))) {
        simulationResults$theta <- lambda1 / lambda2
        simulationResults$.setParameterType("theta", C_PARAM_GENERATED)
    }
    
    warning("The simulation count data feature is experimental and ",
        "hence not fully validated (see www.rpact.com/experimental)",
        call. = FALSE
    )

    return(simulationResults)
}
