## |
## |  *Count data*
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
## |  File version: $Revision: 7620 $
## |  Last changed: $Date: 2024-02-09 12:57:37 +0100 (Fr, 09 Feb 2024) $
## |  Last changed by: $Author: pahlke $
## |

.getCalendarTime <- function(
        n1, 
        n2,
        information,
        shift,
        accrualTime,
        recruit1,
        recruit2,
        fixedExposureTime,
        lambda1,
        lambda2,
        thetaH0,
        overdispersion) {
    if (any(is.na(recruit1))) {
        recruit1 <- seq(0, accrualTime, length.out = n1)
    }
    if (any(is.na(recruit2))) {
        recruit2 <- seq(0, accrualTime, length.out = n2)
    }
    tryCatch(
        {
            return(stats::uniroot(
                function(x) {
                    if (!is.na(fixedExposureTime)) {
                        timeUnderObservation1 <-
                            pmax(pmin(x - recruit1, fixedExposureTime), 0)
                        timeUnderObservation2 <-
                            pmax(pmin(x - recruit2, fixedExposureTime), 0)
                    } else {
                        timeUnderObservation1 <- pmax(x - recruit1, 0)
                        timeUnderObservation2 <- pmax(x - recruit2, 0)
                    }
                    sumLambda1 <- sum(timeUnderObservation1 * lambda1 /
                        (1 + overdispersion * timeUnderObservation1 * lambda1))
                    sumLambda2 <- sum(timeUnderObservation2 * lambda2 /
                        (1 + overdispersion * timeUnderObservation2 * lambda2))
                    return(1 / (1 / sumLambda1 + 1 / sumLambda2) -
                        information * shift / log(lambda1 / lambda2 / thetaH0)^2)
                },
                interval = c(0, 10 * max(1, max(accrualTime))),
                extendInt = "yes",
                tol = 1e-07
            )$root)
        },
        error = function(e) {
            stop("Failed to calculate the calendar time. ",
                "Fisher information might be bounded, e.g., due to overdispersion > 0",
                call. = FALSE
            )
        }
    )
}

.getMaximumSampleSizeTwoGroups <- function(allocationRatioPlanned,
        shift,
        accrualTime,
        followUpTime,
        lambda1,
        lambda2,
        thetaH0,
        overdispersion) {
    n2 <- stats::uniroot(function(y) {
        n2 <- y
        n1 <- allocationRatioPlanned * n2
        timeUnderObservation1 <-
            pmax(accrualTime + followUpTime - seq(0, accrualTime, length.out = n1), 0)
        timeUnderObservation2 <-
            pmax(accrualTime + followUpTime - seq(0, accrualTime, length.out = n2), 0)
        sumLambda1 <- sum(timeUnderObservation1 * lambda1 /
            (1 + overdispersion * timeUnderObservation1 * lambda1))
        sumLambda2 <- sum(timeUnderObservation2 * lambda2 /
            (1 + overdispersion * timeUnderObservation2 * lambda2))
        return(1 / (1 / sumLambda1 + 1 / sumLambda2) -
            shift / log(lambda1 / lambda2 / thetaH0)^2)
    }, interval = c(0, 10^4), extendInt = "yes", tol = 1e-02)$root
    n1 <- ceiling(allocationRatioPlanned * n2)
    n2 <- ceiling(n2)

    # ensure that information is reached (necessary, gscounts does not check!)
    timeUnderObservation1 <- pmax(accrualTime + followUpTime - seq(0, accrualTime, length.out = n1), 0)
    timeUnderObservation2 <- pmax(accrualTime + followUpTime - seq(0, accrualTime, length.out = n2), 0)
    sumLambda1 <- sum(timeUnderObservation1 * lambda1 / (1 + overdispersion * timeUnderObservation1 * lambda1))
    sumLambda2 <- sum(timeUnderObservation2 * lambda2 / (1 + overdispersion * timeUnderObservation2 * lambda2))
    if (1 / (1 / sumLambda1 + 1 / sumLambda2) < shift / log(lambda1 / lambda2 / thetaH0)^2) {
        n2 <- n2 + 1
        n1 <- n1 + 1
    }
    return(list(
        n1 = n1,
        n2 = n2
    ))
}

.getDesignPlanCountData <- function(design,
        designCharacteristics,
        objectType,
        sided,
        lambda1,
        lambda2,
        lambda,
        theta,
        thetaH0,
        overdispersion,
        fixedExposureTime,
        accrualTime,
        accrualIntensity,
        followUpTime,
        maxNumberOfSubjects,
        allocationRatioPlanned) {
    designPlan <- TrialDesignPlanCountData(design = design, designCharacteristics = designCharacteristics)
    designPlan$.setObjectType(objectType)
    sampleSizeEnabled <- identical(objectType, "sampleSize")

    if (sampleSizeEnabled || design$kMax == 1) {
        designPlan$.setParameterType("overallReject", C_PARAM_NOT_APPLICABLE)
        designPlan$.setParameterType("rejectPerStage", C_PARAM_NOT_APPLICABLE)
        designPlan$.setParameterType("futilityPerStage", C_PARAM_NOT_APPLICABLE)
        designPlan$.setParameterType("futilityStop", C_PARAM_NOT_APPLICABLE)
        designPlan$.setParameterType("earlyStop", C_PARAM_NOT_APPLICABLE)
    }

    if (!sampleSizeEnabled) {
        designPlan$.setParameterType("power", C_PARAM_NOT_APPLICABLE)
        designPlan$.setParameterType("studyTime", C_PARAM_NOT_APPLICABLE)
        designPlan$.setParameterType("numberOfSubjects", C_PARAM_NOT_APPLICABLE)
        designPlan$.setParameterType("calendarTime", C_PARAM_NOT_APPLICABLE)
        designPlan$.setParameterType("expectedStudyDurationH1", C_PARAM_NOT_APPLICABLE)
        designPlan$.setParameterType("expectedNumberOfSubjectsH1", C_PARAM_NOT_APPLICABLE)
        designPlan$.setParameterType("informationOverStages", C_PARAM_NOT_APPLICABLE)
        designPlan$.setParameterType("maxInformation", C_PARAM_NOT_APPLICABLE)
        designPlan$.setParameterType("expectedInformationH0", C_PARAM_NOT_APPLICABLE)
        designPlan$.setParameterType("expectedInformationH01", C_PARAM_NOT_APPLICABLE)
        designPlan$.setParameterType("expectedInformationH1", C_PARAM_NOT_APPLICABLE)
    }

    if (any(is.na(allocationRatioPlanned))) {
        allocationRatioPlanned <- C_ALLOCATION_RATIO_DEFAULT
    }

    .assertIsValidAllocationRatioPlannedSampleSize(allocationRatioPlanned, maxNumberOfSubjects)
    .assertIsValidEffectCountData(
        sampleSizeEnabled = sampleSizeEnabled,
        sided = sided,
        lambda1 = lambda1,
        lambda2 = lambda2,
        lambda = lambda,
        theta = theta,
        thetaH0 = thetaH0,
        overdispersion = overdispersion
    )

    if (design$sided == 2 && thetaH0 != 1) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "two-sided case is implemented for superiority testing only (i.e., thetaH0 = 1)"
        )
    }

    if (!is.na(lambda2) && !any(is.na(theta))) {
        totalCases <- length(theta)
    } else if (!any(is.na(lambda1))) {
        totalCases <- length(lambda1)
    } else {
        totalCases <- 1
    }

    .setValueAndParameterType(designPlan, "lambda1", lambda1, NA_real_, notApplicableIfNA = TRUE)
    .setValueAndParameterType(designPlan, "lambda2", lambda2, NA_real_, notApplicableIfNA = TRUE)
    .setValueAndParameterType(designPlan, "lambda", lambda, NA_real_, notApplicableIfNA = TRUE)
    .setValueAndParameterType(designPlan, "theta", theta, NA_real_, notApplicableIfNA = TRUE)
    .setValueAndParameterType(designPlan, "thetaH0", thetaH0, 1, notApplicableIfNA = TRUE)
    .setValueAndParameterType(designPlan, "overdispersion", overdispersion, 0)
    .setValueAndParameterType(designPlan, "fixedExposureTime", fixedExposureTime, NA_real_, notApplicableIfNA = TRUE)
    .setValueAndParameterType(designPlan, "accrualTime", accrualTime, NA_real_, notApplicableIfNA = TRUE)
    .setValueAndParameterType(designPlan, "accrualIntensity", accrualIntensity, NA_real_, notApplicableIfNA = TRUE)
    .setValueAndParameterType(designPlan, "followUpTime", followUpTime, NA_real_, notApplicableIfNA = TRUE)
    .setValueAndParameterType(designPlan, "maxNumberOfSubjects", 
        as.integer(maxNumberOfSubjects), NA_integer_, notApplicableIfNA = TRUE)
    .setValueAndParameterType(
        designPlan, "allocationRatioPlanned",
        allocationRatioPlanned, C_ALLOCATION_RATIO_DEFAULT
    )

    if (identical(designPlan$allocationRatioPlanned, 0)) {
        designPlan$optimumAllocationRatio <- TRUE
        designPlan$.setParameterType("optimumAllocationRatio", C_PARAM_USER_DEFINED)
    }

    if (!is.na(lambda2) && !any(is.na(theta))) {
        lambda1 <- lambda2 * theta
        designPlan$lambda1 <- lambda1
        designPlan$.setParameterType("lambda1", C_PARAM_GENERATED)
    } else if (!any(is.na(lambda1)) && !any(is.na(theta))) {
        lambda2 <- lambda1 / theta
        designPlan$lambda2 <- lambda2
        designPlan$.setParameterType("lambda2", C_PARAM_GENERATED)
    } else if (!is.na(lambda) && !any(is.na(theta))) {
        designPlan$.setParameterType("lambda1", C_PARAM_GENERATED)
        designPlan$.setParameterType("lambda2", C_PARAM_GENERATED)
    }

    if (length(accrualTime) == 2 && accrualTime[1] == 0) {
        accrualTime <- accrualTime[-1]
        designPlan$accrualTime <- accrualTime
    }

    .assertIsValidParametersCountData(
        sampleSizeEnabled = sampleSizeEnabled,
        simulationEnabled = FALSE,
        fixedExposureTime = fixedExposureTime,
        followUpTime = followUpTime,
        accrualTime = accrualTime,
        accrualIntensity = accrualIntensity,
        maxNumberOfSubjects = maxNumberOfSubjects
    )

    designPlan$.setParameterType(
        "criticalValuesPValueScale",
        ifelse(design$kMax > 1 || design$sided == 2,
            C_PARAM_GENERATED, C_PARAM_NOT_APPLICABLE
        )
    )

    if (any(design$futilityBounds > C_FUTILITY_BOUNDS_DEFAULT)) {
        designPlan$futilityBoundsPValueScale <- matrix(1 - stats::pnorm(design$futilityBounds), ncol = 1)
        designPlan$.setParameterType("futilityBoundsPValueScale", C_PARAM_GENERATED)
    }

    return(list(
        designPlan = designPlan,
        totalCases = totalCases
    ))
}

#' @title
#' Get Sample Size Counts
#'
#' @description
#' Returns the sample size for testing the ratio of mean rates
#' of negative binomial distributed event numbers in two samples at given effect.
#'
#'
#' @inheritParams param_design_with_default
#' @inheritParams param_thetaH0
#' @inheritParams param_lambda_counts
#' @inheritParams param_lambda1_counts
#' @inheritParams param_lambda2_counts
#' @inheritParams param_theta_counts
#' @inheritParams param_fixedExposureTime_counts
#' @inheritParams param_accrualTime_counts
#' @inheritParams param_accrualIntensity_counts
#' @inheritParams param_followUpTime_counts
#' @inheritParams param_maxNumberOfSubjects
#' @inheritParams param_overdispersion_counts
#' @inheritParams param_allocationRatioPlanned_sampleSize
#' @inheritParams param_three_dots
#'
#' @details
#' At given design the function calculates the information, and stage-wise and maximum sample size for testing mean rates
#' of negative binomial distributed event numbers in two samples at given effect.
#' The sample size calculation is performed either for a fixed exposure time or a variable exposure time with fixed follow-up.
#' For the variable exposure time case, at given maximum sample size the necessary follow-up time is calculated.
#' The planned calendar time of interim stages is calculated if an accrual time is defined.
#' Additionally, an allocation ratio = \code{n1 / n2} can be specified where \code{n1} and \code{n2} are the number
#' of subjects in the two treatment groups. A null hypothesis value \code{thetaH0} can also be specified.
#'
#' @template return_object_trial_design_plan
#' @template how_to_get_help_for_generics
#'
#' @family sample size functions
#'
#' @template examples_get_sample_size_counts
#'
#' @export
#'
getSampleSizeCounts <- function(design = NULL, ...,
        lambda1 = NA_real_,
        lambda2 = NA_real_,
        lambda = NA_real_,
        theta = NA_real_,
        thetaH0 = 1,
        overdispersion = 0,
        fixedExposureTime = NA_real_,
        accrualTime = NA_real_,
        accrualIntensity = NA_real_,
        followUpTime = NA_real_,
        maxNumberOfSubjects = NA_integer_,
        allocationRatioPlanned = NA_real_) {
    if (is.null(design)) {
        design <- .getDefaultDesign(..., type = "sampleSize")
        .warnInCaseOfUnknownArguments(
            functionName = "getSampleSizeCounts",
            ignore = .getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design,
                powerCalculationEnabled = FALSE
            ), ...
        )
    } else {
        .assertIsTrialDesign(design)
        .warnInCaseOfUnknownArguments(functionName = "getSampleSizeCounts", ...)
        .warnInCaseOfTwoSidedPowerArgument(...)
        .warnInCaseOfTwoSidedPowerIsDisabled(design)
    }

    kMax <- design$kMax
    informationRates <- design$informationRates
    alpha <- design$alpha
    sided <- design$sided
    beta <- design$beta
    designCharacteristics <- getDesignCharacteristics(design)
    shift <- designCharacteristics$shift

    result <- .getDesignPlanCountData(
        design,
        designCharacteristics,
        objectType = "sampleSize",
        sided,
        lambda1,
        lambda2,
        lambda,
        theta,
        thetaH0,
        overdispersion,
        fixedExposureTime,
        accrualTime,
        accrualIntensity,
        followUpTime,
        maxNumberOfSubjects,
        allocationRatioPlanned
    )

    designPlan <- result$designPlan

    totalCases <- result$totalCases
    allocationRatioPlanned <- designPlan$allocationRatioPlanned
    lambda1 <- designPlan$lambda1
    lambda2 <- designPlan$lambda2
    accrualTime <- designPlan$accrualTime
    if (length(accrualTime) > 1) {
        accrualTime <- accrualTime[-1]
    }
    
    if (design$kMax > 1) {
        designPlan$rejectPerStage <- matrix(designCharacteristics$rejectionProbabilities, ncol = 1)
        designPlan$.setParameterType("rejectPerStage", C_PARAM_GENERATED)

        designPlan$futilityPerStage <- matrix(designCharacteristics$futilityProbabilities, ncol = 1)
        designPlan$.setParameterType(
            "futilityPerStage",
            ifelse(!all(is.na(designPlan$futilityPerStage)) &&
                any(designPlan$futilityPerStage > 1e-06, na.rm = TRUE),
            C_PARAM_GENERATED, C_PARAM_NOT_APPLICABLE
            )
        )

        designPlan$futilityStop <- sum(designCharacteristics$futilityProbabilities)
        designPlan$.setParameterType(
            "futilityStop",
            ifelse(!all(is.na(designPlan$futilityStop)) &&
                any(designPlan$futilityStop > 1e-06, na.rm = TRUE),
            C_PARAM_GENERATED, C_PARAM_NOT_APPLICABLE
            )
        )

        designPlan$earlyStop <- sum(c(
            designCharacteristics$rejectionProbabilities[1:(design$kMax - 1)], designPlan$futilityStop))
        designPlan$.setParameterType("earlyStop", C_PARAM_GENERATED)

    } else {
        designPlan$.setParameterType("nFixed", C_PARAM_GENERATED)
    }

    calendarTime <- matrix(NA_real_, kMax, totalCases)
    numberOfSubjects <- matrix(NA_real_, kMax, totalCases)
    informationOverStages <- matrix(NA_real_, kMax, totalCases)
    if (is.na(followUpTime)) {
        studyTime <- rep(NA_real_, totalCases)
    } else {
        studyTime <- accrualTime + followUpTime
    }
    if (length(allocationRatioPlanned) == 1) {
        allocationRatioPlanned <- rep(allocationRatioPlanned, totalCases)
    }
    n1 <- rep(NA_real_, totalCases)
    n2 <- rep(NA_real_, totalCases)
    expectedStudyDurationH1 <- rep(NA_real_, totalCases)
    expectedNumberOfSubjectsH1 <- rep(NA_real_, totalCases)

    for (iCase in 1:totalCases) {
        if (!is.na(fixedExposureTime)) {
            if (allocationRatioPlanned[iCase] == 0) {
                designPlan$.setParameterType("allocationRatioPlanned", C_PARAM_GENERATED)
                # find optimum allocation ratio
                allocationRatioPlanned[iCase] <-
                    stats::optimize(function(x) {
                        if (!is.na(lambda) && !any(is.na(theta))) {
                            lambda2 <- (1 + x) * lambda / (1 + x * theta[iCase])
                            lambda1[iCase] <- lambda2 * theta[iCase]
                        }
                        varianceEstimate <- 1 / fixedExposureTime *
                            (1 / lambda2 + 1 / (lambda1[iCase] * x)) +
                            overdispersion * (1 + 1 / x)
                        n2[iCase] <- (qnorm(1 - alpha / sided) + qnorm(1 - beta))^2 *
                            varianceEstimate / log(lambda1[iCase] / lambda2 / thetaH0)^2
                        return(x * n2[iCase] + n2[iCase])
                    }, interval = c(0, 5), tol = 1e-05)$minimum
            }

            if (!is.na(lambda) && !any(is.na(theta))) {
                lambda2 <- (1 + allocationRatioPlanned[iCase]) * lambda /
                    (1 + allocationRatioPlanned[iCase] * theta[iCase])
                lambda1[iCase] <- lambda2 * theta[iCase]
            }

            # method 2 of Zhu & Lakkis (2013), coincides with Friede & Schmidli (2010)
            varianceEstimate <- 1 / fixedExposureTime *
                (1 / lambda2 + 1 / (lambda1[iCase] * allocationRatioPlanned[iCase])) +
                overdispersion * (1 + 1 / allocationRatioPlanned[iCase])
            n2[iCase] <- designCharacteristics$inflationFactor *
                (qnorm(1 - alpha / sided) + qnorm(1 - beta))^2 * varianceEstimate /
                log(lambda1[iCase] / lambda2 / thetaH0)^2

            n1[iCase] <- allocationRatioPlanned[iCase] * n2[iCase]
            n2[iCase] <- ceiling(n2[iCase])
            n1[iCase] <- ceiling(n1[iCase])

            if (!any(is.na(accrualTime))) {
                recruit1 <- seq(0, accrualTime, length.out = n1[iCase])
                recruit2 <- seq(0, accrualTime, length.out = n2[iCase])
                if (kMax > 1) {
                    for (k in 1:(kMax - 1)) {
                        calendarTime[k, iCase] <- .getCalendarTime(
                            n1[iCase], n2[iCase], informationRates[k], shift,
                            accrualTime, NA_real_, NA_real_, fixedExposureTime,
                            lambda1[iCase], lambda2, thetaH0, overdispersion
                        )
                    }
                }
                calendarTime[kMax, iCase] <- accrualTime + fixedExposureTime
            }
            studyTime[iCase] <- calendarTime[kMax, iCase]
        } else if (!is.na(maxNumberOfSubjects) || !any(is.na(accrualIntensity))) {
            if (!is.na(lambda) && !any(is.na(theta))) {
                lambda2 <- (1 + allocationRatioPlanned[iCase]) * lambda /
                    (1 + allocationRatioPlanned[iCase] * theta[iCase])
                lambda1[iCase] <- lambda2 * theta[iCase]
            }

            if (!any(is.na(accrualIntensity))) {
                const <- allocationRatioPlanned[iCase] / (1 + allocationRatioPlanned[iCase])
                if (length(accrualIntensity) == 1) {
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
                            length.out = (accrualTime[i] - accrualTime[i - 1]) * accrualIntensity[i] * const
                        ))
                        recruit2 <- c(recruit2, seq(accrualTime[i - 1] + 1 / accrualIntensity[i],
                            accrualTime[i],
                            length.out = (accrualTime[i] - accrualTime[i - 1]) * accrualIntensity[i] * (1 - const)
                        ))
                    }
                }
                n1[iCase] <- length(recruit1)
                n2[iCase] <- length(recruit2)
                studyTime[iCase] <- .getCalendarTime(
                    n1[iCase], n2[iCase], informationRates[kMax], shift,
                    accrualTime, recruit1, recruit2, NA_real_,
                    lambda1[iCase], lambda2, thetaH0, overdispersion
                )

                if (kMax > 1) {
                    for (k in 1:(kMax - 1)) {
                        calendarTime[k, iCase] <- .getCalendarTime(
                            n1[iCase], n2[iCase], informationRates[k], shift,
                            accrualTime, recruit1, recruit2, NA_real_,
                            lambda1[iCase], lambda2, thetaH0, overdispersion
                        )
                    }
                }
            } else {
                n2[iCase] <- ceiling(maxNumberOfSubjects / (1 + allocationRatioPlanned[iCase]))
                n1[iCase] <- ceiling(allocationRatioPlanned[iCase] * n2[iCase])
                recruit1 <- seq(0, accrualTime, length.out = n1[iCase])
                recruit2 <- seq(0, accrualTime, length.out = n2[iCase])

                studyTime[iCase] <- .getCalendarTime(
                    n1[iCase], n2[iCase], informationRates[kMax], shift,
                    accrualTime, NA_real_, NA_real_, NA_real_,
                    lambda1[iCase], lambda2, thetaH0, overdispersion
                )

                if (kMax > 1) {
                    for (k in 1:(kMax - 1)) {
                        calendarTime[k, iCase] <- .getCalendarTime(
                            n1[iCase], n2[iCase], informationRates[k], shift,
                            accrualTime, NA_real_, NA_real_, NA_real_,
                            lambda1[iCase], lambda2, thetaH0, overdispersion
                        )
                    }
                }
            }
            calendarTime[kMax, iCase] <- studyTime[iCase]
        } else {
            if (allocationRatioPlanned[iCase] == 0) {
                designPlan$.setParameterType("allocationRatioPlanned", C_PARAM_GENERATED)
                allocationRatioPlanned[iCase] <- stats::optimize(function(x) {
                    if (!is.na(lambda) && !any(is.na(theta))) {
                        lambda2 <- (1 + x) * lambda / (1 + x * theta[iCase])
                        lambda1[iCase] <- lambda2 * theta[iCase]
                    }
                    n2[iCase] <- .getMaximumSampleSizeTwoGroups(
                        x, shift, accrualTime, followUpTime,
                        lambda1[iCase], lambda2, thetaH0, overdispersion
                    )$n2
                    return(x * n2[iCase] + n2[iCase])
                }, interval = c(0, 5), tol = 1e-05)$minimum
            }

            if (!(is.na(lambda)) && !any(is.na(theta))) {
                lambda2 <- (1 + allocationRatioPlanned[iCase]) * lambda /
                    (1 + allocationRatioPlanned[iCase] * theta[iCase])
                lambda1[iCase] <- lambda2 * theta[iCase]
            }

            sampleSizes <- .getMaximumSampleSizeTwoGroups(
                allocationRatioPlanned[iCase], shift, accrualTime, followUpTime,
                lambda1[iCase], lambda2, thetaH0, overdispersion
            )
            n1[iCase] <- sampleSizes$n1
            n2[iCase] <- sampleSizes$n2
            if (!any(is.na(accrualTime))) {
                recruit1 <- seq(0, accrualTime, length.out = n1[iCase])
                recruit2 <- seq(0, accrualTime, length.out = n2[iCase])
            }
            if (kMax > 1) {
                for (k in 1:(kMax - 1)) {
                    calendarTime[k, iCase] <- .getCalendarTime(
                        n1[iCase], n2[iCase], informationRates[k], shift,
                        accrualTime, NA_real_, NA_real_, NA_real_,
                        lambda1[iCase], lambda2, thetaH0, overdispersion
                    )
                }
            }
            calendarTime[kMax, iCase] <- accrualTime + followUpTime
        }

        if (!any(is.na(calendarTime[, iCase]))) {
            expectedStudyDurationH1[iCase] <- calendarTime[kMax, iCase]
            if (kMax > 1) {
                expectedStudyDurationH1[iCase] <- calendarTime[kMax, iCase] -
                    sum((designCharacteristics$rejectionProbabilities[1:(kMax - 1)] +
                        designCharacteristics$futilityProbabilities[1:(kMax - 1)]) *
                        (calendarTime[kMax, iCase] - calendarTime[1:(kMax - 1), iCase]))
            }
            for (k in 1:kMax) {
                numberOfSubjects[k, iCase] <- length(recruit1[recruit1 <= calendarTime[k, iCase]]) +
                    length(recruit2[recruit2 <= calendarTime[k, iCase]])
            }
            expectedNumberOfSubjectsH1[iCase] <- numberOfSubjects[kMax, iCase]
            if (kMax > 1) {
                expectedNumberOfSubjectsH1[iCase] <- expectedNumberOfSubjectsH1[iCase] -
                    sum((designCharacteristics$rejectionProbabilities[1:(kMax - 1)] +
                        designCharacteristics$futilityProbabilities[1:(kMax - 1)]) *
                        (numberOfSubjects[kMax, iCase] - numberOfSubjects[1:(kMax - 1), iCase]))
            }
        }
        informationOverStages[, iCase] <-
            designCharacteristics$shift / log(lambda1[iCase] / lambda2 / thetaH0)^2 *
                informationRates
    }
    if (length(unique(allocationRatioPlanned)) == 1) {
        allocationRatioPlanned <- allocationRatioPlanned[1]
    }

    designPlan$lambda1 <- lambda1
    designPlan$lambda2 <- lambda2
    designPlan$allocationRatioPlanned <- allocationRatioPlanned

    designPlan$directionUpper <- (lambda1 / lambda2 > thetaH0)
    designPlan$.setParameterType("directionUpper", C_PARAM_GENERATED)

    designPlan$calendarTime <- calendarTime
    designPlan$.setParameterType(
        "calendarTime",
        ifelse(!all(is.na(calendarTime)), C_PARAM_GENERATED, C_PARAM_NOT_APPLICABLE)
    )

    designPlan$studyTime <- studyTime
    designPlan$.setParameterType(
        "studyTime",
        ifelse(all(is.na(calendarTime)) && !all(is.na(studyTime)),
            C_PARAM_GENERATED, C_PARAM_NOT_APPLICABLE
        )
    )

    designPlan$numberOfSubjects <- numberOfSubjects
    designPlan$.setParameterType(
        "numberOfSubjects",
        ifelse((kMax > 1) && !all(is.na(calendarTime)) && !all(is.na(accrualTime)),
            C_PARAM_GENERATED, C_PARAM_NOT_APPLICABLE
        )
    )

    designPlan$expectedStudyDurationH1 <- expectedStudyDurationH1
    designPlan$.setParameterType(
        "expectedStudyDurationH1",
        ifelse(!all(is.na(calendarTime)),
            C_PARAM_GENERATED, C_PARAM_NOT_APPLICABLE
        )
    )

    designPlan$expectedNumberOfSubjectsH1 <- expectedNumberOfSubjectsH1
    designPlan$.setParameterType(
        "expectedNumberOfSubjectsH1",
        ifelse(!all(is.na(calendarTime)),
            C_PARAM_GENERATED, C_PARAM_NOT_APPLICABLE
        )
    )
    
    if (!is.na(maxNumberOfSubjects)) {
        designPlan$.setParameterType("maxNumberOfSubjects", C_PARAM_USER_DEFINED)
    } else {
        designPlan$maxNumberOfSubjects <- n1 + n2
        designPlan$.setParameterType("maxNumberOfSubjects", C_PARAM_GENERATED)
    }
  
    designPlan$maxNumberOfSubjects1 <- n1
    designPlan$.setParameterType("maxNumberOfSubjects1", C_PARAM_GENERATED)
    
    designPlan$maxNumberOfSubjects2 <- n2
    designPlan$.setParameterType("maxNumberOfSubjects2", C_PARAM_GENERATED)
 
    if (design$kMax > 1) {

        designPlan$informationOverStages <- informationOverStages
        designPlan$.setParameterType("informationOverStages", C_PARAM_GENERATED)

        designPlan$maxInformation <- designCharacteristics$shift / log(lambda1 / lambda2 / thetaH0)^2
        designPlan$.setParameterType("maxInformation", C_PARAM_GENERATED)

        designPlan$expectedInformationH0 <- designCharacteristics$averageSampleNumber0 *
            designCharacteristics$nFixed / log(lambda1 / lambda2 / thetaH0)^2
        designPlan$.setParameterType(
            "expectedInformationH0",
            ifelse(!all(is.na(designPlan$expectedInformationH0)),
                C_PARAM_GENERATED, C_PARAM_NOT_APPLICABLE
            )
        )

        designPlan$expectedInformationH01 <- designCharacteristics$averageSampleNumber01 *
            designCharacteristics$nFixed / log(lambda1 / lambda2 / thetaH0)^2
        designPlan$.setParameterType(
            "expectedInformationH01",
            ifelse(!all(is.na(designPlan$expectedInformationH01)),
                C_PARAM_GENERATED, C_PARAM_NOT_APPLICABLE
            )
        )

        designPlan$expectedInformationH1 <- designCharacteristics$averageSampleNumber1 *
            designCharacteristics$nFixed / log(lambda1 / lambda2 / thetaH0)^2
        designPlan$.setParameterType(
            "expectedInformationH1",
            ifelse(!all(is.na(designPlan$expectedInformationH1)),
                C_PARAM_GENERATED, C_PARAM_NOT_APPLICABLE
            )
        )

        designPlan$.setParameterType("nFixed1", C_PARAM_NOT_APPLICABLE)
        designPlan$.setParameterType("nFixed2", C_PARAM_NOT_APPLICABLE)
        designPlan$.setParameterType("nFixed", C_PARAM_NOT_APPLICABLE)
    } else {
        designPlan$nFixed <- n1 + n2
        designPlan$nFixed1 <- n1
        designPlan$.setParameterType("nFixed1", C_PARAM_GENERATED)
        designPlan$nFixed2 <- n2
        designPlan$.setParameterType("nFixed2", C_PARAM_GENERATED)

        designPlan$maxNumberOfSubjects1 <- n1
        designPlan$.setParameterType("maxNumberOfSubjects1", C_PARAM_NOT_APPLICABLE)
        designPlan$maxNumberOfSubjects2 <- n2
        designPlan$.setParameterType("maxNumberOfSubjects2", C_PARAM_NOT_APPLICABLE)

        designPlan$maxInformation <- designCharacteristics$nFixed / log(lambda1 / lambda2 / thetaH0)^2
        designPlan$.setParameterType("maxInformation", C_PARAM_GENERATED)
        designPlan$.setParameterType("informationOverStages", C_PARAM_NOT_APPLICABLE)
        designPlan$.setParameterType("expectedInformationH0", C_PARAM_NOT_APPLICABLE)
        designPlan$.setParameterType("expectedInformationH01", C_PARAM_NOT_APPLICABLE)
        designPlan$.setParameterType("expectedInformationH1", C_PARAM_NOT_APPLICABLE)
    }

    if (all(is.na(theta))) {
        designPlan$theta <- lambda1 / lambda2
        designPlan$.setParameterType("theta", C_PARAM_GENERATED)
    }

    return(designPlan)
}

#' @title
#' Get Power Counts
#'
#' @description
#' Returns the power, stopping probabilities, and expected sample size for testing mean rates
#' for negative binomial distributed event numbers in two samples at given sample sizes.
#'
#' @inheritParams param_design_with_default
#' @inheritParams param_thetaH0
#' @inheritParams param_lambda_counts
#' @inheritParams param_lambda1_counts
#' @inheritParams param_lambda2_counts
#' @inheritParams param_theta_counts
#' @inheritParams param_fixedExposureTime_counts
#' @inheritParams param_accrualTime_counts
#' @inheritParams param_accrualIntensity_counts
#' @inheritParams param_followUpTime_counts
#' @inheritParams param_maxNumberOfSubjects
#' @inheritParams param_overdispersion_counts
#' @inheritParams param_directionUpper
#' @inheritParams param_maxNumberOfSubjects
#' @inheritParams param_allocationRatioPlanned
#' @inheritParams param_three_dots
#'
#' @details
#' At given design the function calculates the power, stopping probabilities, and expected sample size
#' for testing the ratio of two mean rates of negative binomial distributed event numbers in two samples
#' at given maximum sample size and effect.
#' The power calculation is performed either for a fixed exposure time or a variable exposure time with fixed follow-up
#' where the information over the stages is calculated according to the specified information rate in the design.
#' Additionally, an allocation ratio = \code{n1 / n2} can be specified where \code{n1} and \code{n2} are the number
#' of subjects in the two treatment groups. A null hypothesis value \code{thetaH0} can also be specified.
#'
#' @template return_object_trial_design_plan
#' @template how_to_get_help_for_generics
#'
#' @family power functions
#'
#' @template examples_get_power_counts
#'
#' @export
#'
getPowerCounts <- function(
        design = NULL, ...,
        directionUpper = NA,
        maxNumberOfSubjects = NA_real_,
        lambda1 = NA_real_,
        lambda2 = NA_real_,
        lambda = NA_real_,
        theta = NA_real_,
        thetaH0 = 1,
        overdispersion = 0,
        fixedExposureTime = NA_real_,
        accrualTime = NA_real_,
        accrualIntensity = NA_real_,
        followUpTime = NA_real_,
        allocationRatioPlanned = NA_real_) {
    if (is.null(design)) {
        design <- .getDefaultDesign(..., type = "power")
        .warnInCaseOfUnknownArguments(
            functionName = "getPowerCounts",
            ignore = .getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
                design,
                powerCalculationEnabled = TRUE
            ), ...
        )
    } else {
        .assertIsTrialDesign(design)
        .warnInCaseOfUnknownArguments(functionName = "getPowerCounts", ...)
        .warnInCaseOfTwoSidedPowerArgument(...)
        .warnInCaseOfTwoSidedPowerIsDisabled(design)
    }

    kMax <- design$kMax
    informationRates <- design$informationRates
    alpha <- design$alpha
    sided <- design$sided
    designCharacteristics <- getDesignCharacteristics(design)
    shift <- designCharacteristics$shift

    result <- .getDesignPlanCountData(
        design,
        designCharacteristics,
        objectType = "power",
        sided,
        lambda1,
        lambda2,
        lambda,
        theta,
        thetaH0,
        overdispersion,
        fixedExposureTime,
        accrualTime,
        accrualIntensity,
        followUpTime,
        maxNumberOfSubjects,
        allocationRatioPlanned
    )
    designPlan <- result$designPlan
    totalCases <- result$totalCases
    allocationRatioPlanned <- designPlan$allocationRatioPlanned
    lambda1 <- designPlan$lambda1
    lambda2 <- designPlan$lambda2
    accrualTime <- designPlan$accrualTime
    if (length(accrualTime) > 1) {
        accrualTime <- accrualTime[-1]
    }
    directionUpper <- .assertIsValidDirectionUpper(directionUpper,
        sided = design$sided,
        objectType = "power",
        userFunctionCallEnabled = TRUE
    )
    .setValueAndParameterType(designPlan, "directionUpper", directionUpper, TRUE)

    futilityPerStage <- matrix(NA_real_, kMax - 1, totalCases)
    rejectPerStage <- matrix(NA_real_, kMax, totalCases)
    earlyStop <- rep(NA_real_, totalCases)
    overallReject <- rep(NA_real_, totalCases)
    for (iCase in 1:totalCases) {
        if (!is.na(lambda) && !any(is.na(theta))) {
            lambda2 <- (1 + allocationRatioPlanned) * lambda / (1 + allocationRatioPlanned * theta[iCase])
            lambda1[iCase] <- lambda2 * theta[iCase]
        }
        if (!any(is.na(accrualIntensity))) {
            const <- allocationRatioPlanned / (1 + allocationRatioPlanned)
            if (length(accrualIntensity) == 1) {
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
                        length.out = (accrualTime[i] - accrualTime[i - 1]) * accrualIntensity[i] * const
                    ))
                    recruit2 <- c(recruit2, seq(accrualTime[i - 1] + 1 / accrualIntensity[i],
                        accrualTime[i],
                        length.out = (accrualTime[i] - accrualTime[i - 1]) * accrualIntensity[i] * (1 - const)
                    ))
                }
            }
            n1 <- length(recruit1)
            n2 <- length(recruit2)
            nTotal <- n1 + n2
        } else {
            n2 <- maxNumberOfSubjects / (1 + allocationRatioPlanned)
            n1 <- allocationRatioPlanned * n2
            nTotal <- n1 + n2
            if (!any(is.na(accrualTime))) {
                recruit1 <- seq(0, accrualTime, length.out = n1)
                recruit2 <- seq(0, accrualTime, length.out = n2)
            }
        }

        if (!is.na(fixedExposureTime)) {
            varianceEstimate <- (1 + allocationRatioPlanned) * (1 / fixedExposureTime *
                (1 / lambda2 + 1 / (lambda1[iCase] * allocationRatioPlanned)) +
                overdispersion * (1 + 1 / allocationRatioPlanned))
        } else {
            timeUnderObservation1 <-
                pmax(accrualTime[length(accrualTime)] + followUpTime - recruit1, 0)
            timeUnderObservation2 <-
                pmax(accrualTime[length(accrualTime)] + followUpTime - recruit2, 0)
            sumLambda1 <- sum(timeUnderObservation1 * lambda1[iCase] /
                (1 + overdispersion * timeUnderObservation1 * lambda1[iCase]))
            sumLambda2 <- sum(timeUnderObservation2 * lambda2 /
                (1 + overdispersion * timeUnderObservation2 * lambda2))
            varianceEstimate <- nTotal * (1 / sumLambda1 + 1 / sumLambda2)
        }

        oneSidedFactor <- ifelse(sided == 1, 2 * directionUpper - 1, 1)
        powerAndAverageSampleNumber <- getPowerAndAverageSampleNumber(
            design = design,
            theta = oneSidedFactor * log(lambda1[iCase] / lambda2 / thetaH0) /
                sqrt(varianceEstimate),
            nMax = nTotal
        )
        futilityPerStage[, iCase] <- powerAndAverageSampleNumber$futilityPerStage
        rejectPerStage[, iCase] <- powerAndAverageSampleNumber$rejectPerStage
        overallReject[iCase] <- powerAndAverageSampleNumber$overallReject
        earlyStop[iCase] <- sum(powerAndAverageSampleNumber$earlyStop[1:(design$kMax - 1), ], na.rm = TRUE)
    }

    designPlan$maxNumberOfSubjects <- n1 + n2
    designPlan$.setParameterType(
        "maxNumberOfSubjects",
        ifelse(any(is.na(accrualIntensity)), C_PARAM_USER_DEFINED, C_PARAM_GENERATED)
    )
    designPlan$maxNumberOfSubjects1 <- n1
    designPlan$.setParameterType(
        "maxNumberOfSubjects1",
        ifelse(allocationRatioPlanned == 1, C_PARAM_NOT_APPLICABLE, C_PARAM_GENERATED)
    )

    designPlan$maxNumberOfSubjects2 <- n2
    designPlan$.setParameterType(
        "maxNumberOfSubjects2",
        ifelse(allocationRatioPlanned == 1, C_PARAM_NOT_APPLICABLE, C_PARAM_GENERATED)
    )

    if (design$kMax > 1) {
        designPlan$futilityPerStage <- futilityPerStage
        designPlan$.setParameterType(
            "futilityPerStage",
            ifelse(!all(is.na(futilityPerStage)) && any(futilityPerStage > 1e-06, na.rm = TRUE),
                C_PARAM_GENERATED, C_PARAM_NOT_APPLICABLE
            )
        )

        designPlan$futilityStop <- base::colSums(futilityPerStage, na.rm = TRUE)
        designPlan$.setParameterType(
            "futilityStop",
            ifelse(!all(is.na(designPlan$futilityStop)) &&
                any(designPlan$futilityStop > 1e-06, na.rm = TRUE),
            C_PARAM_GENERATED, C_PARAM_NOT_APPLICABLE
            )
        )

        designPlan$rejectPerStage <- rejectPerStage
        designPlan$.setParameterType("rejectPerStage", C_PARAM_GENERATED)

        designPlan$earlyStop <- earlyStop
        designPlan$.setParameterType(
            "earlyStop", ifelse(!all(is.na(earlyStop)), C_PARAM_GENERATED, C_PARAM_NOT_APPLICABLE)
        )

        designPlan$.setParameterType("nFixed1", C_PARAM_NOT_APPLICABLE)
        designPlan$.setParameterType("nFixed2", C_PARAM_NOT_APPLICABLE)
        designPlan$.setParameterType("nFixed", C_PARAM_NOT_APPLICABLE)
    } else {
        designPlan$nFixed <- n1 + n2
        designPlan$.setParameterType(
            "nFixed",
            ifelse(any(is.na(accrualIntensity)), C_PARAM_NOT_APPLICABLE, C_PARAM_GENERATED)
        )

        designPlan$nFixed1 <- n1
        designPlan$.setParameterType(
            "nFixed1",
            ifelse(allocationRatioPlanned == 1, C_PARAM_NOT_APPLICABLE, C_PARAM_GENERATED)
        )
        designPlan$nFixed2 <- n2
        designPlan$.setParameterType(
            "nFixed2",
            ifelse(allocationRatioPlanned == 1, C_PARAM_NOT_APPLICABLE, C_PARAM_GENERATED)
        )
        designPlan$.setParameterType("maxNumberOfSubjects1", C_PARAM_NOT_APPLICABLE)
        designPlan$.setParameterType("maxNumberOfSubjects2", C_PARAM_NOT_APPLICABLE)
        if (is.na(designPlan$maxNumberOfSubjects)) {
            designPlan$maxNumberOfSubjects <- designPlan$nFixed
        }
    }

    designPlan$overallReject <- overallReject
    designPlan$.setParameterType("overallReject", C_PARAM_GENERATED)

    if (all(is.na(theta))) {
        designPlan$theta <- lambda1 / lambda2
        designPlan$.setParameterType("theta", C_PARAM_GENERATED)
    }

    return(designPlan)
}
