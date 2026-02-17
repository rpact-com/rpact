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

#'
#' Warn in Case of Extreme Allocation Ratios
#'
#' @description
#' This function generates a warning if the difference between two allocation ratios
#' exceeds a predefined threshold, indicating that the allocation scheme might yield
#' unreliable results.
#'
#' @param allocation1 A numeric value representing the allocation ratio for group 1.
#' @param allocation2 A numeric value representing the allocation ratio for group 2.
#'
#' @details
#' The function checks the absolute difference between `allocation1` and `allocation2`.
#' If the difference is greater than 4, a warning is issued to inform the user that
#' the chosen allocation scheme might lead to unreliable results.
#'
#' @return
#' This function does not return a value. It is used for its side effect of generating a warning.
#'
#' @examples
#' \dontrun{
#' .warnInCaseOfExtremeAllocationRatios(6, 1) # Generates a warning
#' .warnInCaseOfExtremeAllocationRatios(3, 2) # No warning
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
.warnInCaseOfExtremeAllocationRatios <- function(allocation1, allocation2) {
    if (abs(allocation1 - allocation2) > 4) {
        warning(
            "Choice of allocation scheme ('allocation1' = ",
            allocation1,
            ", 'allocation2' = ",
            allocation2,
            ") might yield unreliable results",
            call. = FALSE
        )
    }
}

#'
#' Generate Single Recruitment Time
#'
#' @description
#' This function calculates the recruitment time for a single subject based on the given fraction,
#' accrual time, and subject fractions.
#'
#' @param fraction A numeric value representing the fraction of subjects recruited.
#' @param accrualTime A numeric vector representing the accrual time intervals.
#' @param subjectFractions A numeric vector representing the cumulative fractions of subjects recruited
#' at each accrual time interval.
#'
#' @details
#' The function determines the recruitment time for a subject by comparing the given fraction to the
#' cumulative subject fractions. If the fraction falls within a specific interval, the recruitment time
#' is calculated proportionally within that interval.
#'
#' @return
#' A numeric value representing the recruitment time for the subject. Returns \code{NA_real_} if the
#' fraction does not fall within the defined intervals.
#'
#' @examples
#' \dontrun{
#' accrualTime <- c(0, 5, 10)
#' subjectFractions <- c(0.2, 0.7, 1.0)
#' .generateSingleRecruitmentTimes(0.5, accrualTime, subjectFractions)
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
.generateSingleRecruitmentTimes <- function(fraction, accrualTime, subjectFractions) {
    if (fraction <= subjectFractions[1]) {
        return(accrualTime[1] / subjectFractions[1] * fraction)
    }

    for (i in 2:length(accrualTime)) {
        if (fraction <= subjectFractions[i]) {
            return(
                accrualTime[i] +
                    (accrualTime[i] - accrualTime[i - 1]) /
                        (subjectFractions[i] - subjectFractions[i - 1]) *
                        (fraction - subjectFractions[i])
            )
        }
    }

    return(NA_real_)
}


#'
#' Generate Recruitment Times
#'
#' @description
#' This function generates recruitment times for subjects based on the given allocation ratio,
#' accrual time, and accrual intensity. It also assigns treatments to subjects based on the allocation ratio.
#'
#' @param allocationRatio A numeric value representing the planned allocation ratio between two groups.
#' @param accrualTime A numeric vector representing the accrual time intervals.
#' @param accrualIntensity A numeric vector representing the accrual intensity for each accrual time interval.
#'
#' @details
#' The function calculates the recruitment times for subjects by dividing the accrual time into intervals
#' and using the accrual intensity to determine the number of subjects recruited in each interval.
#' It also assigns treatments to subjects based on the allocation ratio, ensuring that the allocation
#' scheme is followed. A warning is generated if the allocation ratio is extreme.
#'
#' @return
#' A data frame containing:
#' \itemize{
#'   \item \code{treatments}: A numeric vector indicating the treatment group for each subject.
#'   \item \code{recruit}: A numeric vector representing the recruitment time for each subject.
#' }
#'
#' @examples
#' \dontrun{
#' allocationRatio <- 2
#' accrualTime <- c(0, 5, 10)
#' accrualIntensity <- c(10, 20, 30)
#' recruitmentData <- .generateRecruitmentTimes(allocationRatio, accrualTime, accrualIntensity)
#' print(recruitmentData)
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
.generateRecruitmentTimes <- function(allocationRatio, accrualTime, accrualIntensity) {
    .assertIsSingleNumber(allocationRatio, "allocationRatio")
    .assertIsNumericVector(accrualTime, "accrualTime")
    .assertIsNumericVector(accrualIntensity, "accrualIntensity")
    
    if (length(accrualTime) != length(accrualIntensity)) {
        stop(
            C_EXCEPTION_TYPE_RUNTIME_ISSUE,
            "length of accrualTime (",
            length(accrualTime),
            ") and ",
            "accrualIntensity (",
            length(accrualIntensity),
            ") must be identical"
        )
    }

    densityIntervals <- accrualTime
    if (length(densityIntervals) > 1) {
        densityIntervals[2:length(densityIntervals)] <-
            densityIntervals[2:length(densityIntervals)] -
            densityIntervals[1:(length(densityIntervals) - 1)]
        maxNumberOfSubjects <- ceiling(sum(densityIntervals * accrualIntensity))
    } else {
        maxNumberOfSubjects <- ceiling(accrualTime * accrualIntensity)
    }
    
    subjectFractions <- cumsum(densityIntervals * accrualIntensity) / maxNumberOfSubjects
    subjectFractions[length(accrualTime)] <- 1

    recruit <- numeric()
    for (fraction in (1:maxNumberOfSubjects) / maxNumberOfSubjects) {
        recruit <- c(recruit, .generateSingleRecruitmentTimes(fraction, accrualTime, subjectFractions))
    }

    # to force last value to be last accrualTime
    recruit[length(recruit)] <- accrualTime[length(accrualTime)]

    allocationFraction <- .getFraction(allocationRatio)
    allocation1 <- allocationFraction[1]
    allocation2 <- allocationFraction[2]
    .warnInCaseOfExtremeAllocationRatios(allocation1, allocation2)

    treatments <- c()
    while (length(treatments) < maxNumberOfSubjects) {
        if (allocation1 > allocation2) {
            treatments <- c(treatments, rep(c(1, 2), allocation2), rep(1, allocation1 - allocation2))
        } else {
            treatments <- c(treatments, rep(c(1, 2), allocation1), rep(2, allocation2 - allocation1))
        }
    }
    treatments <- treatments[1:maxNumberOfSubjects]

    return(data.frame(
        treatments = treatments,
        recruit = recruit
    ))
}


#'
#' Calculate Variance Estimate
#'
#' @description
#' This function calculates the variance estimate for count data based on the provided
#' event rates, allocation ratio, overdispersion, and recruitment times for two groups.
#'
#' @param lambda1 A numeric vector representing the event rates for group 1.
#' @param lambda2 A numeric vector representing the event rates for group 2.
#' @param allocation A numeric value representing the allocation ratio between the two groups.
#' @param overdispersion A numeric value representing the overdispersion parameter.
#' @param accrualTime A numeric vector representing the accrual time intervals.
#' @param followUpTime A numeric value representing the follow-up time.
#' @param fixedExposureTime A numeric value representing the fixed exposure time. Default is \code{NA_real_}.
#' @param recruit1 A numeric vector representing the recruitment times for group 1.
#' @param recruit2 A numeric vector representing the recruitment times for group 2.
#'
#' @details
#' The function calculates the variance estimate for two groups. If a fixed exposure time is provided,
#' the variance is calculated using the fixed exposure time and overdispersion. Otherwise, it uses
#' the follow-up time and recruitment times to compute the variance based on the observed event rates.
#'
#' @return
#' A numeric value representing the variance estimate.
#'
#' @examples
#' \dontrun{
#' lambda1 <- c(0.1, 0.2, 0.3)
#' lambda2 <- c(0.15, 0.25, 0.35)
#' allocation <- 1
#' overdispersion <- 0.1
#' accrualTime <- c(0, 5, 10)
#' followUpTime <- 5
#' recruit1 <- c(1, 2, 3)
#' recruit2 <- c(2, 3, 4)
#' variance <- .getVarianceEstimate(
#'     lambda1, lambda2, allocation, overdispersion,
#'     accrualTime, followUpTime, NA_real_, recruit1, recruit2
#' )
#' print(variance)
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
.getVarianceEstimate <- function(
        lambda1,
        lambda2,
        allocation,
        overdispersion,
        accrualTime,
        followUpTime,
        fixedExposureTime,
        recruit1,
        recruit2
        ) {
    if (!is.na(fixedExposureTime)) {
        varianceEstimate <- (1 + allocation) *
            (1 / fixedExposureTime * (1 / lambda2 + 1 / (lambda1 * allocation)) + overdispersion * (1 + 1 / allocation))
    } else {
        if (is.na(followUpTime)) {
            stop(
                C_EXCEPTION_TYPE_RUNTIME_ISSUE,
                "Cannot calculated variance estimate because follow-up time is NA"
            )
        }
        timeUnderObservation1 <-
            pmax(accrualTime[length(accrualTime)] + followUpTime - recruit1, 0)
        timeUnderObservation2 <-
            pmax(accrualTime[length(accrualTime)] + followUpTime - recruit2, 0)
        sumLambda1 <- sum(timeUnderObservation1 * lambda1 / (1 + overdispersion * timeUnderObservation1 * lambda1))
        sumLambda2 <- sum(timeUnderObservation2 * lambda2 / (1 + overdispersion * timeUnderObservation2 * lambda2))
        nTotal <- length(recruit1) + length(recruit2)
        varianceEstimate <- nTotal * (1 / sumLambda1 + 1 / sumLambda2)
    }
    return(varianceEstimate)
}


#' Find Theta Using Uniroot
#'
#' @description
#' This function finds the value of \code{theta} that satisfies a given boundary condition
#' using the \code{uniroot} method. It calculates the variance estimate and solves for
#' \code{theta} based on the provided parameters.
#'
#' @param boundary A numeric value representing the boundary condition to be satisfied.
#' @param lambda2 A numeric vector representing the event rates for group 2.
#' @param thetaH0 A numeric value representing the null hypothesis value of \code{theta}.
#' @param directionUpper A numeric value (1 or 0) indicating the direction of the test.
#' @param allocationRatio A numeric value representing the allocation ratio between the two groups.
#' @param overdispersion A numeric value representing the overdispersion parameter.
#' @param accrualTime A numeric vector representing the accrual time intervals.
#' @param followUpTime A numeric value representing the follow-up time.
#' @param fixedExposureTime A numeric value representing the fixed exposure time. Default is \code{NA_real_}.
#' @param numberOfSubjects A numeric value representing the total number of subjects.
#' @param recruit1 A numeric vector representing the recruitment times for group 1.
#' @param recruit2 A numeric vector representing the recruitment times for group 2.
#'
#' @details
#' The function uses the \code{uniroot} method to find the root of the equation that satisfies
#' the boundary condition. It calculates the variance estimate for the given parameters and
#' uses it to determine the value of \code{theta}.
#'
#' @return
#' A numeric value representing the value of \code{theta} that satisfies the boundary condition.
#' Returns \code{NA_real_} if the root cannot be found.
#'
#' @keywords internal
#'
#' @noRd
#'
.findThetaUniRoot <- function(
        boundary,
        informationRate,
        lambda2,
        thetaH0,
        directionUpper,
        allocationRatio,
        overdispersion,
        accrualTime,
        followUpTime,
        fixedExposureTime,
        numberOfSubjects,
        recruit1,
        recruit2
        ) {
    tryCatch(
        {
            if ((2 * directionUpper - 1) * boundary < 0) {
                lowerBound <- stats::optimize(
                    function(x) {
                        vHat <- .getVarianceEstimate(
                            lambda1 = x * lambda2,
                            lambda2 = lambda2,
                            allocation = allocationRatio,
                            overdispersion = overdispersion,
                            accrualTime = accrualTime,
                            followUpTime = followUpTime,
                            fixedExposureTime = fixedExposureTime,
                            recruit1 = recruit1[1:(allocationRatio * numberOfSubjects / (1 + allocationRatio))],
                            recruit2[1:(numberOfSubjects / (1 + allocationRatio))]
                        )

                        if (is.null(vHat) || length(vHat) == 0 || is.na(vHat) || is.nan(vHat)) {
                            stop(
                                C_EXCEPTION_TYPE_RUNTIME_ISSUE,
                                "Cannot find theta. ",
                                "The calculated variance estimate is invalid: ",
                                vHat
                            )
                        }
                        (log(x) - log(thetaH0)) / sqrt(vHat) * sqrt(informationRate * numberOfSubjects)
                    },
                    lower = 1e-05,
                    upper = 1
                )$minimum
            } else {
                lowerBound <- min(thetaH0, 1 / thetaH0)
            }

            if (is.na(lowerBound)) {
                return(NA_real_)
            }

            effectRatio <- stats::uniroot(
                function(x) {
                    vHat <- .getVarianceEstimate(
                        lambda1 = x * lambda2,
                        lambda2 = lambda2,
                        allocation = allocationRatio,
                        overdispersion = overdispersion,
                        accrualTime = accrualTime,
                        followUpTime = followUpTime,
                        fixedExposureTime = fixedExposureTime,
                        recruit1 = recruit1[1:(allocationRatio * numberOfSubjects / (1 + allocationRatio))],
                        recruit2 = recruit2[1:(numberOfSubjects / (1 + allocationRatio))]
                    )

                    if (is.null(vHat) || length(vHat) == 0 || is.na(vHat) || is.nan(vHat)) {
                        stop(
                            C_EXCEPTION_TYPE_RUNTIME_ISSUE,
                            "cannot find theta. ",
                            "The calculated variance estimate is invalid: ",
                            vHat
                        )
                    }

                    (log(x) - log(thetaH0)) /
                        sqrt(vHat) *
                        sqrt(informationRate * numberOfSubjects) -
                        (2 * directionUpper - 1) * boundary
                },
                lower = lowerBound,
                upper = 10,
                tol = .Machine$double.eps^0.5,
                extendInt = "yes"
            )$root
        },
        warning = function(w) {
            effectRatio <<- NA_real_
        },
        error = function(e) {
            effectRatio <<- NA_real_
        }
    )
    return(effectRatio)
}

.getEffectScaleBoundaryCountData <- function(designPlan) {
    design <- designPlan$.design
    thetaH0 <- designPlan$thetaH0
    lambda1 <- designPlan$lambda1
    lambda2 <- designPlan$lambda2
    overdispersion <- designPlan$overdispersion
    fixedExposureTime <- designPlan$fixedExposureTime
    followUpTime <- designPlan$followUpTime
    studyTime <- designPlan$studyTime
    accrualTime <- designPlan$accrualTime
    if (length(accrualTime) > 1) {
        accrualTime <- accrualTime[-1]
    }
    accrualIntensity <- designPlan$accrualIntensity
    maxNumberOfSubjects <- designPlan$maxNumberOfSubjects
    numberOfSubjects <- designPlan$numberOfSubjects
    allocationRatioPlanned <- designPlan$allocationRatioPlanned
    directionUpper <- designPlan$directionUpper

    if (is.na(followUpTime) && is.na(fixedExposureTime)) {
        followUpTime <- studyTime - max(accrualTime)
    }

    if (designPlan$.objectType == "power") {
        nParameters <- 1
    } else {
        nParameters <- length(lambda1)
    }

    criticalValuesEffectScaleUpper <- matrix(, nrow = design$kMax, ncol = nParameters)
    criticalValuesEffectScaleLower <- matrix(, nrow = design$kMax, ncol = nParameters)
    futilityBoundsEffectScaleUpper <- matrix(, nrow = design$kMax - 1, ncol = nParameters)
    futilityBoundsEffectScaleLower <- matrix(, nrow = design$kMax - 1, ncol = nParameters)

    if (length(allocationRatioPlanned) == 1) {
        allocationRatioPlanned <- rep(allocationRatioPlanned, nParameters)
    }
    if (length(followUpTime) == 1) {
        followUpTime <- rep(followUpTime, nParameters)
    }
    if (length(maxNumberOfSubjects) == 1) {
        maxNumberOfSubjects <- rep(maxNumberOfSubjects, nParameters)
    }

    directionUpper[is.na(directionUpper)] <- C_DIRECTION_UPPER_DEFAULT

    if (length(directionUpper) == 1) {
        directionUpper <- rep(directionUpper, nParameters)
    }

    informationRates <- design$informationRates
    criticalValues <- .getCriticalValues(design)
    futilityBounds <- design$futilityBounds
    futilityBounds[!is.na(futilityBounds) & futilityBounds <= C_FUTILITY_BOUNDS_DEFAULT] <- NA_real_

    for (iCase in 1:nParameters) {
        allocationRatio <- allocationRatioPlanned[iCase]

        if (!anyNA(accrualIntensity)) {
            # build up general recruitment times
            recruitmentTimes <- .generateRecruitmentTimes(
                allocationRatio,
                accrualTime,
                accrualIntensity
            )
            recruit1 <- recruitmentTimes$recruit[recruitmentTimes$treatments == 1]
            recruit2 <- recruitmentTimes$recruit[recruitmentTimes$treatments == 2]
        } else if (!anyNA(accrualTime)) {
            recruit1 <- seq(
                0,
                accrualTime,
                length.out = maxNumberOfSubjects[iCase] * allocationRatio / (1 + allocationRatio)
            )
            recruit2 <- seq(0, accrualTime, length.out = maxNumberOfSubjects[iCase] / (1 + allocationRatio))
        } else {
            recruit1 <- NA_real_
            recruit2 <- NA_real_
        }

        # calculate theta that solves (ln(theta) - ln(thetaH0) sqrt(FisherInformation_k) = boundary
        for (j in seq_len(length(criticalValues))) {
            if (all(is.na(numberOfSubjects[, iCase]))) {
                numberOfSubjectsPerStage <- maxNumberOfSubjects[iCase]
            } else {
                numberOfSubjectsPerStage <- numberOfSubjects[j, iCase]
            }
            criticalValuesEffectScaleUpper[j, iCase] <- .findThetaUniRoot(
                criticalValues[j],
                informationRates[j],
                lambda2,
                thetaH0,
                directionUpper[iCase],
                allocationRatio,
                overdispersion,
                accrualTime,
                followUpTime[iCase],
                fixedExposureTime,
                numberOfSubjectsPerStage,
                recruit1[1:(allocationRatio * numberOfSubjectsPerStage / (1 + allocationRatio))],
                recruit2[1:(numberOfSubjectsPerStage / (1 + allocationRatio))]
            )

            if (design$sided == 2) {
                criticalValuesEffectScaleLower[j, iCase] <- .findThetaUniRoot(
                    -criticalValues[j],
                    informationRates[j],
                    lambda2,
                    thetaH0,
                    directionUpper[iCase],
                    allocationRatio,
                    overdispersion,
                    accrualTime,
                    followUpTime[iCase],
                    fixedExposureTime,
                    numberOfSubjectsPerStage,
                    recruit1[1:(allocationRatio * numberOfSubjectsPerStage / (1 + allocationRatio))],
                    recruit2[1:(numberOfSubjectsPerStage / (1 + allocationRatio))]
                )
            }
        }

        if (!all(is.na(futilityBounds))) {
            for (j in seq_len(length(futilityBounds))) {
                if (all(is.na(numberOfSubjects[, iCase]))) {
                    numberOfSubjectsPerStage <- maxNumberOfSubjects[iCase]
                } else {
                    numberOfSubjectsPerStage <- numberOfSubjects[j, iCase]
                }
                futilityBoundsEffectScaleUpper[j, iCase] <- .findThetaUniRoot(
                    futilityBounds[j],
                    informationRates[j],
                    lambda2,
                    thetaH0,
                    directionUpper[iCase],
                    allocationRatio,
                    overdispersion,
                    accrualTime,
                    followUpTime[iCase],
                    fixedExposureTime,
                    numberOfSubjectsPerStage,
                    recruit1[1:(allocationRatio * numberOfSubjectsPerStage / (1 + allocationRatio))],
                    recruit2[1:(numberOfSubjectsPerStage / (1 + allocationRatio))]
                )
                if (
                    design$sided == 2 &&
                        design$kMax > 1 &&
                        (design$typeOfDesign == C_TYPE_OF_DESIGN_PT ||
                            !is.null(design$typeBetaSpending) && design$typeBetaSpending != "none")
                    ) {
                    futilityBoundsEffectScaleLower[j, iCase] <- .findThetaUniRoot(
                        -futilityBounds[j],
                        informationRates[j],
                        lambda2,
                        thetaH0,
                        directionUpper[iCase],
                        allocationRatio,
                        overdispersion,
                        accrualTime,
                        followUpTime[iCase],
                        fixedExposureTime,
                        numberOfSubjectsPerStage,
                        recruit1[1:(allocationRatio * numberOfSubjectsPerStage / (1 + allocationRatio))],
                        recruit2[1:(numberOfSubjectsPerStage / (1 + allocationRatio))]
                    )
                }
            }
        }
    }

    criticalValuesEffectScaleUpper[
        !is.na(criticalValuesEffectScaleUpper) &
            criticalValuesEffectScaleUpper <= 0
    ] <- NA_real_
    criticalValuesEffectScaleLower[
        !is.na(criticalValuesEffectScaleLower) &
            criticalValuesEffectScaleLower <= 0
    ] <- NA_real_
    futilityBoundsEffectScaleUpper[
        !is.na(futilityBoundsEffectScaleUpper) &
            futilityBoundsEffectScaleUpper <= 0
    ] <- NA_real_
    futilityBoundsEffectScaleLower[
        !is.na(futilityBoundsEffectScaleLower) &
            futilityBoundsEffectScaleLower <= 0
    ] <- NA_real_

    return(list(
        criticalValuesEffectScaleUpper = matrix(criticalValuesEffectScaleUpper, nrow = design$kMax),
        criticalValuesEffectScaleLower = matrix(criticalValuesEffectScaleLower, nrow = design$kMax),
        futilityBoundsEffectScaleUpper = matrix(futilityBoundsEffectScaleUpper, nrow = design$kMax - 1),
        futilityBoundsEffectScaleLower = matrix(futilityBoundsEffectScaleLower, nrow = design$kMax - 1)
    ))
}

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
        overdispersion
        ) {
    if (anyNA(recruit1)) {
        recruit1 <- seq(0, accrualTime, length.out = n1)
    }
    if (anyNA(recruit2)) {
        recruit2 <- seq(0, accrualTime, length.out = n2)
    }
    tryCatch(
        {
            return(
                stats::uniroot(
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
                        sumLambda1 <- sum(
                            timeUnderObservation1 * lambda1 / (1 + overdispersion * timeUnderObservation1 * lambda1)
                        )
                        sumLambda2 <- sum(
                            timeUnderObservation2 * lambda2 / (1 + overdispersion * timeUnderObservation2 * lambda2)
                        )
                        return(
                            1 /
                                (1 / sumLambda1 + 1 / sumLambda2) -
                                information * shift / log(lambda1 / lambda2 / thetaH0)^2
                        )
                    },
                    interval = c(0, 10 * max(1, max(accrualTime))),
                    extendInt = "yes",
                    tol = 1e-07
                )$root
            )
        },
        error = function(e) {
            warning(
                "Failed to calculate the calendar time. ",
                "Fisher information might be bounded, e.g., due to overdispersion > 0",
                call. = FALSE
            )
        }
    )
    return(NA_real_)
}

.getMaximumSampleSizeTwoGroups <- function(allocationRatioPlanned,
        shift,
        accrualTime,
        followUpTime,
        lambda1,
        lambda2,
        thetaH0,
        overdispersion) {
    tryCatch(
        {
            n2 <- stats::uniroot(
                function(y) {
                    n2 <- y
                    n1 <- allocationRatioPlanned * n2
                    timeUnderObservation1 <-
                        pmax(accrualTime + followUpTime - seq(0, accrualTime, length.out = n1), 0)
                    timeUnderObservation2 <-
                        pmax(accrualTime + followUpTime - seq(0, accrualTime, length.out = n2), 0)
                    sumLambda1 <- sum(
                        timeUnderObservation1 * lambda1 / (1 + overdispersion * timeUnderObservation1 * lambda1)
                    )
                    sumLambda2 <- sum(
                        timeUnderObservation2 * lambda2 / (1 + overdispersion * timeUnderObservation2 * lambda2)
                    )
                    return(1 / (1 / sumLambda1 + 1 / sumLambda2) - shift / log(lambda1 / lambda2 / thetaH0)^2)
                },
                interval = c(0, 10^5),
                extendInt = "yes",
                tol = 1e-02
            )$root
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
        },
        error = function(e) {
            return(list(
                n1 = NA_real_,
                n2 = NA_real_
            ))
        }
    )
}

.getDesignPlanCountData <- function(
        design,
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
        allocationRatioPlanned
        ) {
            
    designPlan <- TrialDesignPlanCountData$new(
        design = design,
        designCharacteristics = designCharacteristics
    )
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

    if (anyNA(allocationRatioPlanned)) {
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

    if (!is.na(lambda2) && !anyNA(theta)) {
        totalCases <- length(theta)
    } else if (!anyNA(lambda1)) {
        totalCases <- length(lambda1)
    } else {
        totalCases <- 1
    }

    .setValueAndParameterType(designPlan, "lambda1", lambda1, NA_real_, notApplicableIfNA = TRUE)
    .setValueAndParameterType(designPlan, "lambda2", lambda2, NA_real_, notApplicableIfNA = TRUE)
    .setValueAndParameterType(designPlan, "lambda", lambda, NA_real_, notApplicableIfNA = TRUE)
    .setValueAndParameterType(designPlan, "theta", theta, NA_real_, notApplicableIfNA = TRUE)
    .setValueAndParameterType(designPlan, "thetaH0", thetaH0, C_THETA_H0_COUNTS_DEFAULT, notApplicableIfNA = TRUE)
    .setValueAndParameterType(designPlan, "overdispersion", overdispersion, 0)
    .setValueAndParameterType(designPlan, "fixedExposureTime", fixedExposureTime, NA_real_, notApplicableIfNA = TRUE)
    .setValueAndParameterType(designPlan, "accrualTime", accrualTime, NA_real_, notApplicableIfNA = TRUE)
    .setValueAndParameterType(designPlan, "accrualIntensity", accrualIntensity, NA_real_, notApplicableIfNA = TRUE)
    .setValueAndParameterType(designPlan, "followUpTime", followUpTime, NA_real_, notApplicableIfNA = TRUE)
    .setValueAndParameterType(
        designPlan,
        "maxNumberOfSubjects",
        as.integer(maxNumberOfSubjects),
        NA_integer_,
        notApplicableIfNA = TRUE
    )
    .setValueAndParameterType(
        designPlan,
        "allocationRatioPlanned",
        allocationRatioPlanned,
        C_ALLOCATION_RATIO_DEFAULT
    )

    if (identical(designPlan$allocationRatioPlanned, 0)) {
        designPlan$optimumAllocationRatio <- TRUE
        designPlan$.setParameterType("optimumAllocationRatio", C_PARAM_USER_DEFINED)
    }

    if (!is.na(lambda2) && !anyNA(theta)) {
        lambda1 <- lambda2 * theta
        designPlan$lambda1 <- lambda1
        designPlan$.setParameterType("lambda1", C_PARAM_GENERATED)
    } else if (!anyNA(lambda1) && !anyNA(theta)) {
        lambda2 <- lambda1 / theta
        designPlan$lambda2 <- lambda2
        designPlan$.setParameterType("lambda2", C_PARAM_GENERATED)
    } else if (!is.na(lambda) && !anyNA(theta)) {
        designPlan$.setParameterType("lambda1", C_PARAM_GENERATED)
        designPlan$.setParameterType("lambda2", C_PARAM_GENERATED)
    }

    if (length(accrualTime) == 2 && accrualTime[1] == 0) {
        accrualTime <- accrualTime[-1]
        designPlan$accrualTime <- accrualTime
    }

    .assertAreValidParametersCountData(
        sampleSizeEnabled = sampleSizeEnabled,
        simulationEnabled = FALSE,
        fixedExposureTime = fixedExposureTime,
        followUpTime = followUpTime,
        accrualTime = accrualTime,
        accrualIntensity = accrualIntensity,
        maxNumberOfSubjects = maxNumberOfSubjects
    )

    designPlan$criticalValuesPValueScale <- matrix(design$stageLevels, ncol = 1)
    if (design$sided == 2) {
        designPlan$criticalValuesPValueScale <- designPlan$criticalValuesPValueScale * 2
    }
    designPlan$.setParameterType("criticalValuesPValueScale", C_PARAM_NOT_APPLICABLE)

    if (.hasApplicableFutilityBounds(design)) {
        designPlan$futilityBoundsPValueScale <-
            matrix(1 - stats::pnorm(design$futilityBounds), ncol = 1)
        designPlan$.setParameterType("futilityBoundsPValueScale", C_PARAM_GENERATED)
    }

    attr(designPlan, "totalCases") <- totalCases
    return(designPlan)
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
getSampleSizeCounts <- function(
        design = NULL,
        ...,
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
        allocationRatioPlanned = NA_real_
        ) {
    if (is.null(design)) {
        design <- .getDefaultDesign(..., type = "sampleSize")
        .warnInCaseOfUnknownArguments(
            functionName = "getSampleSizeCounts",
            ignore = .getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design, powerCalculationEnabled = FALSE),
            ...
        )
    } else {
        .assertIsTrialDesignGroupSequential(design)
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

    designPlan <- .getDesignPlanCountData(
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
    totalCases <- attr(designPlan, "totalCases")
    attr(designPlan, "totalCases") <- NULL
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
            ifelse(
                !all(is.na(designPlan$futilityPerStage)) &&
                    any(designPlan$futilityPerStage > 1e-06, na.rm = TRUE),
                C_PARAM_GENERATED,
                C_PARAM_NOT_APPLICABLE
            )
        )

        designPlan$futilityStop <- sum(designCharacteristics$futilityProbabilities)
        designPlan$.setParameterType(
            "futilityStop",
            ifelse(
                !all(is.na(designPlan$futilityStop)) &&
                    any(designPlan$futilityStop > 1e-06, na.rm = TRUE),
                C_PARAM_GENERATED,
                C_PARAM_NOT_APPLICABLE
            )
        )

        designPlan$earlyStop <- sum(c(
            designCharacteristics$rejectionProbabilities[1:(design$kMax - 1)],
            designPlan$futilityStop
        ))
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
                    stats::optimize(
                        function(x) {
                            if (!is.na(lambda) && !anyNA(theta)) {
                                lambda2 <- (1 + x) * lambda / (1 + x * theta[iCase])
                                lambda1[iCase] <- lambda2 * theta[iCase]
                            }
                            varianceEstimate <- 1 /
                                fixedExposureTime *
                                (1 / lambda2 + 1 / (lambda1[iCase] * x)) +
                                overdispersion * (1 + 1 / x)
                            n2[iCase] <- (qnorm(1 - alpha / sided) + qnorm(1 - beta))^2 *
                                varianceEstimate /
                                log(lambda1[iCase] / lambda2 / thetaH0)^2
                            return(x * n2[iCase] + n2[iCase])
                        },
                        interval = c(0, 5),
                        tol = 1e-05
                    )$minimum
            }

            if (!is.na(lambda) && !anyNA(theta)) {
                lambda2 <- (1 + allocationRatioPlanned[iCase]) *
                    lambda /
                    (1 + allocationRatioPlanned[iCase] * theta[iCase])
                lambda1[iCase] <- lambda2 * theta[iCase]
            }
            else if (!is.na(lambda) && !anyNA(lambda1)) {
                lambda2 <- ((1 + allocationRatioPlanned[iCase]) * lambda) / (allocationRatioPlanned[iCase] * lambda1[iCase])
            }
            else if (!is.na(lambda) && !anyNA(lambda2)) {
                lambda1 <- ((1 + allocationRatioPlanned[iCase]) * lambda - lambda2) / allocationRatioPlanned[iCase]
            }

            # method 2 of Zhu & Lakkis (2013), coincides with Friede & Schmidli (2010)
            varianceEstimate <- 1 /
                fixedExposureTime *
                (1 / lambda2 + 1 / (lambda1[iCase] * allocationRatioPlanned[iCase])) +
                overdispersion * (1 + 1 / allocationRatioPlanned[iCase])
            n2[iCase] <- designCharacteristics$inflationFactor *
                (qnorm(1 - alpha / sided) + qnorm(1 - beta))^2 *
                varianceEstimate /
                log(lambda1[iCase] / lambda2 / thetaH0)^2

            n1[iCase] <- allocationRatioPlanned[iCase] * n2[iCase]
            n2[iCase] <- ceiling(n2[iCase])
            n1[iCase] <- ceiling(n1[iCase])
            
            if (!anyNA(accrualTime)) {
                recruit1 <- seq(0, accrualTime, length.out = n1[iCase])
                recruit2 <- seq(0, accrualTime, length.out = n2[iCase])
                if (kMax > 1) {
                    for (k in 1:(kMax - 1)) {
                        calendarTime[k, iCase] <- .getCalendarTime(
                            n1[iCase],
                            n2[iCase],
                            informationRates[k],
                            shift,
                            accrualTime,
                            NA_real_,
                            NA_real_,
                            fixedExposureTime,
                            lambda1[iCase],
                            lambda2,
                            thetaH0,
                            overdispersion
                        )
                    }
                }
                calendarTime[kMax, iCase] <- accrualTime + fixedExposureTime
            }
            studyTime[iCase] <- calendarTime[kMax, iCase]
        } else if (!is.na(maxNumberOfSubjects) || !anyNA(accrualIntensity)) {
            if (!is.na(lambda) && !anyNA(theta)) {
                lambda2 <- (1 + allocationRatioPlanned[iCase]) *
                    lambda /
                    (1 + allocationRatioPlanned[iCase] * theta[iCase])
                lambda1[iCase] <- lambda2 * theta[iCase]
            }

            if (!anyNA(accrualIntensity)) {
                # build up general recruitment times
                recruitmentTimes <- .generateRecruitmentTimes(
                    allocationRatioPlanned[iCase],
                    accrualTime,
                    accrualIntensity
                )
                recruit1 <- recruitmentTimes$recruit[recruitmentTimes$treatments == 1]
                recruit2 <- recruitmentTimes$recruit[recruitmentTimes$treatments == 2]

                n1[iCase] <- length(recruit1)
                n2[iCase] <- length(recruit2)
                studyTime[iCase] <- .getCalendarTime(
                    n1[iCase],
                    n2[iCase],
                    informationRates[kMax],
                    shift,
                    accrualTime,
                    recruit1,
                    recruit2,
                    NA_real_,
                    lambda1[iCase],
                    lambda2,
                    thetaH0,
                    overdispersion
                )

                if (kMax > 1) {
                    for (k in 1:(kMax - 1)) {
                        calendarTime[k, iCase] <- .getCalendarTime(
                            n1[iCase],
                            n2[iCase],
                            informationRates[k],
                            shift,
                            accrualTime,
                            recruit1,
                            recruit2,
                            NA_real_,
                            lambda1[iCase],
                            lambda2,
                            thetaH0,
                            overdispersion
                        )
                    }
                }
            } else {
                n2[iCase] <- ceiling(maxNumberOfSubjects / (1 + allocationRatioPlanned[iCase]))
                n1[iCase] <- ceiling(allocationRatioPlanned[iCase] * n2[iCase])
                recruit1 <- seq(0, accrualTime, length.out = n1[iCase])
                recruit2 <- seq(0, accrualTime, length.out = n2[iCase])

                studyTime[iCase] <- .getCalendarTime(
                    n1[iCase],
                    n2[iCase],
                    informationRates[kMax],
                    shift,
                    accrualTime,
                    NA_real_,
                    NA_real_,
                    NA_real_,
                    lambda1[iCase],
                    lambda2,
                    thetaH0,
                    overdispersion
                )

                if (kMax > 1) {
                    for (k in 1:(kMax - 1)) {
                        calendarTime[k, iCase] <- .getCalendarTime(
                            n1[iCase],
                            n2[iCase],
                            informationRates[k],
                            shift,
                            accrualTime,
                            NA_real_,
                            NA_real_,
                            NA_real_,
                            lambda1[iCase],
                            lambda2,
                            thetaH0,
                            overdispersion
                        )
                    }
                }
            }
            calendarTime[kMax, iCase] <- studyTime[iCase]
        } else {
            if (allocationRatioPlanned[iCase] == 0) {
                designPlan$.setParameterType("allocationRatioPlanned", C_PARAM_GENERATED)
                allocationRatioPlanned[iCase] <- stats::optimize(
                    function(x) {
                        if (!is.na(lambda) && !anyNA(theta)) {
                            lambda2 <- (1 + x) * lambda / (1 + x * theta[iCase])
                            lambda1[iCase] <- lambda2 * theta[iCase]
                        }
                        n2[iCase] <- .getMaximumSampleSizeTwoGroups(
                            x,
                            shift,
                            accrualTime,
                            followUpTime,
                            lambda1[iCase],
                            lambda2,
                            thetaH0,
                            overdispersion
                        )$n2
                        return(x * n2[iCase] + n2[iCase])
                    },
                    interval = c(0, 5),
                    tol = 1e-05
                )$minimum
            }

            if (!(is.na(lambda)) && !anyNA(theta)) {
                lambda2 <- (1 + allocationRatioPlanned[iCase]) *
                    lambda /
                    (1 + allocationRatioPlanned[iCase] * theta[iCase])
                lambda1[iCase] <- lambda2 * theta[iCase]
            }

            sampleSizes <- .getMaximumSampleSizeTwoGroups(
                allocationRatioPlanned[iCase],
                shift,
                accrualTime,
                followUpTime,
                lambda1[iCase],
                lambda2,
                thetaH0,
                overdispersion
            )
            n1[iCase] <- sampleSizes$n1
            n2[iCase] <- sampleSizes$n2
            if (!anyNA(accrualTime)) {
                recruit1 <- seq(0, accrualTime, length.out = n1[iCase])
                recruit2 <- seq(0, accrualTime, length.out = n2[iCase])
            }
            if (kMax > 1) {
                for (k in 1:(kMax - 1)) {
                    calendarTime[k, iCase] <- .getCalendarTime(
                        n1[iCase],
                        n2[iCase],
                        informationRates[k],
                        shift,
                        accrualTime,
                        NA_real_,
                        NA_real_,
                        NA_real_,
                        lambda1[iCase],
                        lambda2,
                        thetaH0,
                        overdispersion
                    )
                }
            }
            calendarTime[kMax, iCase] <- accrualTime + followUpTime
        }

        if (!anyNA(calendarTime[, iCase])) {
            expectedStudyDurationH1[iCase] <- calendarTime[kMax, iCase]
            if (kMax > 1) {
                expectedStudyDurationH1[iCase] <- calendarTime[kMax, iCase] -
                    sum(
                        (designCharacteristics$rejectionProbabilities[1:(kMax - 1)] +
                            designCharacteristics$futilityProbabilities[1:(kMax - 1)]) *
                            (calendarTime[kMax, iCase] - calendarTime[1:(kMax - 1), iCase])
                    )
            }
            for (k in 1:kMax) {
                numberOfSubjects[k, iCase] <- length(recruit1[recruit1 <= calendarTime[k, iCase]]) +
                    length(recruit2[recruit2 <= calendarTime[k, iCase]])
            }
            expectedNumberOfSubjectsH1[iCase] <- numberOfSubjects[kMax, iCase]
            if (kMax > 1) {
                expectedNumberOfSubjectsH1[iCase] <- expectedNumberOfSubjectsH1[iCase] -
                    sum(
                        (designCharacteristics$rejectionProbabilities[1:(kMax - 1)] +
                            designCharacteristics$futilityProbabilities[1:(kMax - 1)]) *
                            (numberOfSubjects[kMax, iCase] - numberOfSubjects[1:(kMax - 1), iCase])
                    )
            }
        }
        informationOverStages[, iCase] <-
            designCharacteristics$shift / log(lambda1[iCase] / lambda2 / thetaH0)^2 * informationRates
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
    if (!is.na(fixedExposureTime) && anyNA(accrualTime) && all(is.na(calendarTime))) {
        .logDebug("The calendar time was not calculated due to missing accrual time")
    }

    designPlan$studyTime <- studyTime
    designPlan$.setParameterType(
        "studyTime",
        ifelse(all(is.na(calendarTime)) && !all(is.na(studyTime)), C_PARAM_GENERATED, C_PARAM_NOT_APPLICABLE)
    )

    designPlan$numberOfSubjects <- numberOfSubjects
    designPlan$.setParameterType(
        "numberOfSubjects",
        ifelse(
            (kMax > 1) && !all(is.na(calendarTime)) && !all(is.na(accrualTime)),
            C_PARAM_GENERATED,
            C_PARAM_NOT_APPLICABLE
        )
    )

    designPlan$expectedStudyDurationH1 <- expectedStudyDurationH1
    designPlan$.setParameterType(
        "expectedStudyDurationH1",
        ifelse(!all(is.na(calendarTime)), C_PARAM_GENERATED, C_PARAM_NOT_APPLICABLE)
    )

    designPlan$expectedNumberOfSubjectsH1 <- expectedNumberOfSubjectsH1
    designPlan$.setParameterType(
        "expectedNumberOfSubjectsH1",
        ifelse(!all(is.na(calendarTime)), C_PARAM_GENERATED, C_PARAM_NOT_APPLICABLE)
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
            designCharacteristics$nFixed /
            log(lambda1 / lambda2 / thetaH0)^2
        designPlan$.setParameterType(
            "expectedInformationH0",
            ifelse(!all(is.na(designPlan$expectedInformationH0)), C_PARAM_GENERATED, C_PARAM_NOT_APPLICABLE)
        )

        designPlan$expectedInformationH01 <- designCharacteristics$averageSampleNumber01 *
            designCharacteristics$nFixed /
            log(lambda1 / lambda2 / thetaH0)^2
        designPlan$.setParameterType(
            "expectedInformationH01",
            ifelse(!all(is.na(designPlan$expectedInformationH01)), C_PARAM_GENERATED, C_PARAM_NOT_APPLICABLE)
        )

        designPlan$expectedInformationH1 <- designCharacteristics$averageSampleNumber1 *
            designCharacteristics$nFixed /
            log(lambda1 / lambda2 / thetaH0)^2
        designPlan$.setParameterType(
            "expectedInformationH1",
            ifelse(!all(is.na(designPlan$expectedInformationH1)), C_PARAM_GENERATED, C_PARAM_NOT_APPLICABLE)
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

    .addEffectScaleBoundaryDataToDesignPlan(designPlan)

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
        design = NULL,
        ...,
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
        allocationRatioPlanned = NA_real_
        ) {
    if (is.null(design)) {
        design <- .getDefaultDesign(..., type = "power")
        .warnInCaseOfUnknownArguments(
            functionName = "getPowerCounts",
            ignore = .getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
                design,
                powerCalculationEnabled = TRUE
            ),
            ...
        )
    } else {
        .assertIsTrialDesignGroupSequential(design)
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

    designPlan <- .getDesignPlanCountData(
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
    totalCases <- attr(designPlan, "totalCases")
    attr(designPlan, "totalCases") <- NULL
    allocationRatioPlanned <- designPlan$allocationRatioPlanned
    lambda1 <- designPlan$lambda1
    lambda2 <- designPlan$lambda2
    accrualTime <- designPlan$accrualTime
    if (length(accrualTime) > 1) {
        accrualTime <- accrualTime[-1]
    }

    directionUpper <- .assertIsValidDirectionUpper(
        directionUpper,
        design,
        objectType = "power",
        userFunctionCallEnabled = TRUE
    )
    .setValueAndParameterType(designPlan, "directionUpper", directionUpper, C_DIRECTION_UPPER_DEFAULT)

    futilityPerStage <- matrix(NA_real_, kMax - 1, totalCases)
    rejectPerStage <- matrix(NA_real_, kMax, totalCases)
    earlyStop <- rep(NA_real_, totalCases)
    overallReject <- rep(NA_real_, totalCases)

    if (!anyNA(accrualIntensity)) {
        # build up general recruitment times
        recruitmentTimes <- .generateRecruitmentTimes(
            allocationRatioPlanned,
            accrualTime,
            accrualIntensity
        )
        recruit1 <- recruitmentTimes$recruit[recruitmentTimes$treatments == 1]
        recruit2 <- recruitmentTimes$recruit[recruitmentTimes$treatments == 2]
        n1 <- length(recruit1)
        n2 <- length(recruit2)
        nTotal <- n1 + n2
    } else {
        n2 <- maxNumberOfSubjects / (1 + allocationRatioPlanned)
        n1 <- allocationRatioPlanned * n2
        nTotal <- n1 + n2
        if (!anyNA(accrualTime)) {
            recruit1 <- seq(0, accrualTime, length.out = n1)
            recruit2 <- seq(0, accrualTime, length.out = n2)
        }
    }

    for (iCase in 1:totalCases) {
        if (!is.na(lambda) && !anyNA(theta)) {
            lambda2 <- (1 + allocationRatioPlanned) * lambda / (1 + allocationRatioPlanned * theta[iCase])
            lambda1[iCase] <- lambda2 * theta[iCase]
        }
        if (!is.na(fixedExposureTime)) {
            varianceEstimate <- (1 + allocationRatioPlanned) *
                (1 /
                    fixedExposureTime *
                    (1 / lambda2 + 1 / (lambda1[iCase] * allocationRatioPlanned)) +
                    overdispersion * (1 + 1 / allocationRatioPlanned))
        } else {
            timeUnderObservation1 <-
                pmax(accrualTime[length(accrualTime)] + followUpTime - recruit1, 0)
            timeUnderObservation2 <-
                pmax(accrualTime[length(accrualTime)] + followUpTime - recruit2, 0)
            sumLambda1 <- sum(
                timeUnderObservation1 * lambda1[iCase] / (1 + overdispersion * timeUnderObservation1 * lambda1[iCase])
            )
            sumLambda2 <- sum(timeUnderObservation2 * lambda2 / (1 + overdispersion * timeUnderObservation2 * lambda2))
            varianceEstimate <- nTotal * (1 / sumLambda1 + 1 / sumLambda2)
        }
        oneSidedFactor <- ifelse(sided == 1, 2 * directionUpper - 1, 1)
        
        powerAndAverageSampleNumber <- getPowerAndAverageSampleNumber(
            design = design,
            theta = oneSidedFactor * log(lambda1[iCase] / lambda2 / thetaH0) / sqrt(varianceEstimate),
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
        ifelse(anyNA(accrualIntensity), C_PARAM_USER_DEFINED, C_PARAM_GENERATED)
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
            ifelse(
                !all(is.na(futilityPerStage)) && any(futilityPerStage > 1e-06, na.rm = TRUE),
                C_PARAM_GENERATED,
                C_PARAM_NOT_APPLICABLE
            )
        )

        designPlan$futilityStop <- base::colSums(futilityPerStage, na.rm = TRUE)
        designPlan$.setParameterType(
            "futilityStop",
            ifelse(
                !all(is.na(designPlan$futilityStop)) &&
                    any(designPlan$futilityStop > 1e-06, na.rm = TRUE),
                C_PARAM_GENERATED,
                C_PARAM_NOT_APPLICABLE
            )
        )

        designPlan$rejectPerStage <- rejectPerStage
        designPlan$.setParameterType("rejectPerStage", C_PARAM_GENERATED)

        designPlan$earlyStop <- earlyStop
        designPlan$.setParameterType(
            "earlyStop",
            ifelse(!all(is.na(earlyStop)), C_PARAM_GENERATED, C_PARAM_NOT_APPLICABLE)
        )

        designPlan$.setParameterType("nFixed1", C_PARAM_NOT_APPLICABLE)
        designPlan$.setParameterType("nFixed2", C_PARAM_NOT_APPLICABLE)
        designPlan$.setParameterType("nFixed", C_PARAM_NOT_APPLICABLE)
    } else {
        designPlan$nFixed <- n1 + n2
        designPlan$.setParameterType(
            "nFixed",
            ifelse(anyNA(accrualIntensity), C_PARAM_NOT_APPLICABLE, C_PARAM_GENERATED)
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

    .addEffectScaleBoundaryDataToDesignPlan(designPlan)

    return(designPlan)
}
