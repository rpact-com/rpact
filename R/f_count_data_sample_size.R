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
## |  File version: $Revision: 7428 $
## |  Last changed: $Date: 2023-11-13 10:42:22 +0100 (Mo, 13 Nov 2023) $
## |  Last changed by: $Author: pahlke $
## |

.getCalendarTime <- function(
        n1, n2,
        information,
        shift,
        accrualTime,
        recruit1,
        recruit2,
        fixedExposureTime,
        lambda1,
        lambda2,
        thetaH0,
        overDispersion) {
    if (any(is.na(recruit1))) {
        recruit1 <- seq(0, accrualTime, length.out = n1)
    }
    if (any(is.na(recruit2))) {
        recruit2 <- seq(0, accrualTime, length.out = n2)
    }
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
            sumLambda1 <- sum(timeUnderObservation1 * lambda1 /
                (1 + overDispersion * timeUnderObservation1 * lambda1))
            sumLambda2 <- sum(timeUnderObservation2 * lambda2 /
                (1 + overDispersion * timeUnderObservation2 * lambda2))
            return(1 / (1 / sumLambda1 + 1 / sumLambda2) -
                information * shift / log(lambda1 / lambda2 / thetaH0)^2)
        },
        interval = c(0, 10 * min(1, accrualTime)),
        extendInt = "yes",
        tol = 1e-07
    )$root
}

.getMaximumSampleSizeTwoGroups <- function(
        allocationRatioPlanned,
        shift,
        accrualTime,
        followUpTime,
        lambda1,
        lambda2,
        thetaH0,
        overDispersion) {
    n2 <- stats::uniroot(function(y) {
        n2 <- y
        n1 <- allocationRatioPlanned * n2
        timeUnderObservation1 <-
            pmax(accrualTime + followUpTime - seq(0, accrualTime, length.out = n1), 0)
        timeUnderObservation2 <-
            pmax(accrualTime + followUpTime - seq(0, accrualTime, length.out = n2), 0)
        sumLambda1 <- sum(timeUnderObservation1 * lambda1 /
            (1 + overDispersion * timeUnderObservation1 * lambda1))
        sumLambda2 <- sum(timeUnderObservation2 * lambda2 /
            (1 + overDispersion * timeUnderObservation2 * lambda2))
        return(1 / (1 / sumLambda1 + 1 / sumLambda2) -
            shift / log(lambda1 / lambda2 / thetaH0)^2)
    }, interval = c(0, 10^4), extendInt = "yes", tol = 1e-02)$root
    n1 <- ceiling(allocationRatioPlanned * n2)
    n2 <- ceiling(n2)

    # ensure that information is reached (necessary, gscounts does not check!)
    timeUnderObservation1 <- pmax(accrualTime + followUpTime - seq(0, accrualTime, length.out = n1), 0)
    timeUnderObservation2 <- pmax(accrualTime + followUpTime - seq(0, accrualTime, length.out = n2), 0)
    sumLambda1 <- sum(timeUnderObservation1 * lambda1 / (1 + overDispersion * timeUnderObservation1 * lambda1))
    sumLambda2 <- sum(timeUnderObservation2 * lambda2 / (1 + overDispersion * timeUnderObservation2 * lambda2))
    if (1 / (1 / sumLambda1 + 1 / sumLambda2) < shift / log(lambda1 / lambda2 / thetaH0)^2) {
        n2 <- n2 + 1
        n1 <- n1 + 1
    }
    return(list(
        n1 = n1,
        n2 = n2
    ))
}

#'
#' TODO @Gernot: document
#'
#' @export
#'
getSampleSizeCounts <- function(
        design = NULL, ...,
        lambda1 = NA_real_,
        lambda2 = NA_real_,
        lambda = NA_real_,
        theta = NA_real_,
        thetaH0 = 1,
        overDispersion = 0,
        fixedExposureTime = NA_real_,
        accrualTime = NA_real_,
        accrualIntensity = NA_real_,
        followUpTime = NA_real_,
        maxNumberOfSubjects = NA_real_,
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
    sampleSizeEnabled <- TRUE

    if (!is.na(lambda2) && !any(is.na(theta))) {
        totalCases <- length(theta)
        lambda1 <- rep(NA_real_, totalCases)
    } else if (!any(is.na(lambda1))) {
        totalCases <- length(lambda1)
    } else {
        totalCases <- 1
    }
    if (any(is.na(allocationRatioPlanned))) {
        allocationRatioPlanned <- C_ALLOCATION_RATIO_DEFAULT
    }
    .assertIsValidAllocationRatioPlannedSampleSize(allocationRatioPlanned, maxNumberOfSubjects)
    .assertIsValidEffectCountData(
        sampleSizeEnabled, sided, lambda1, lambda2, lambda, theta,
        thetaH0, overDispersion
    )
    if (!is.na(lambda2) && !any(is.na(theta))) {
        lambda1 <- lambda2 * theta
    } else if (!any(is.na(lambda1)) && !any(is.na(theta))) {
        lambda2 <- lambda1 / theta
    }
    .assertIsValidParametersCountData(
        sampleSizeEnabled, fixedExposureTime, followUpTime,
        accrualTime, accrualIntensity, maxNumberOfSubjects
    )
    if ((length(accrualTime) > 1) && (accrualTime[1] == 0)) {
        accrualTime <- accrualTime[-1]
    }
    calendarTime <- matrix(NA_real_, kMax, totalCases)
    studySubjects <- matrix(NA_real_, kMax, totalCases)
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

    for (iCase in (1:totalCases)) {
        if (!is.na(fixedExposureTime)) {
            if (allocationRatioPlanned[iCase] == 0) {
                # find optimum allocation ratio
                allocationRatioPlanned[iCase] <-
                    stats::optimize(function(x) {
                        if (!is.na(lambda) && !any(is.na(theta))) {
                            lambda2 <- (1 + x) * lambda / (1 + x * theta[iCase])
                            lambda1[iCase] <- lambda2 * theta[iCase]
                        }
                        varianceEstimate <- 1 / fixedExposureTime *
                            (1 / lambda2 + 1 / (lambda1[iCase] * x)) +
                            overDispersion * (1 + 1 / x)
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
                overDispersion * (1 + 1 / allocationRatioPlanned[iCase])
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
                    for (k in (1:(kMax - 1))) {
                        calendarTime[k, iCase] <- .getCalendarTime(
                            n1[iCase], n2[iCase], informationRates[k], shift,
                            accrualTime, NA_real_, NA_real_, fixedExposureTime,
                            lambda1[iCase], lambda2, thetaH0, overDispersion
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
                    lambda1[iCase], lambda2, thetaH0, overDispersion
                )

                if (kMax > 1) {
                    for (k in (1:(kMax - 1))) {
                        calendarTime[k, iCase] <- .getCalendarTime(
                            n1[iCase], n2[iCase], informationRates[k], shift,
                            accrualTime, recruit1, recruit2, NA_real_,
                            lambda1[iCase], lambda2, thetaH0, overDispersion
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
                    lambda1[iCase], lambda2, thetaH0, overDispersion
                )

                if (kMax > 1) {
                    for (k in (1:(kMax - 1))) {
                        calendarTime[k, iCase] <- .getCalendarTime(
                            n1[iCase], n2[iCase], informationRates[k], shift,
                            accrualTime, NA_real_, NA_real_, NA_real_,
                            lambda1[iCase], lambda2, thetaH0, overDispersion
                        )
                    }
                }
            }
            calendarTime[kMax, iCase] <- studyTime[iCase]
        } else {
            if (allocationRatioPlanned[iCase] == 0) {
                allocationRatioPlanned[iCase] <- stats::optimize(function(x) {
                    if (!is.na(lambda) && !any(is.na(theta))) {
                        lambda2 <- (1 + x) * lambda / (1 + x * theta[iCase])
                        lambda1[iCase] <- lambda2 * theta[iCase]
                    }
                    n2[iCase] <- .getMaximumSampleSizeTwoGroups(
                        x, shift, accrualTime, followUpTime,
                        lambda1[iCase], lambda2, thetaH0, overDispersion
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
                lambda1[iCase], lambda2, thetaH0, overDispersion
            )
            n1[iCase] <- sampleSizes$n1
            n2[iCase] <- sampleSizes$n2
            recruit1 <- seq(0, accrualTime, length.out = n1[iCase])
            recruit2 <- seq(0, accrualTime, length.out = n2[iCase])

            if (kMax > 1) {
                for (k in (1:(kMax - 1))) {
                    calendarTime[k, iCase] <- .getCalendarTime(
                        n1[iCase], n2[iCase], informationRates[k], shift,
                        accrualTime, NA_real_, NA_real_, NA_real_,
                        lambda1[iCase], lambda2, thetaH0, overDispersion
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
            for (k in (1:kMax)) {
                studySubjects[k, iCase] <- length(recruit1[recruit1 <= calendarTime[k, iCase]]) +
                    length(recruit2[recruit2 <= calendarTime[k, iCase]])
            }
            expectedNumberOfSubjectsH1[iCase] <- studySubjects[kMax, iCase]
            if (kMax > 1) {
                expectedNumberOfSubjectsH1[iCase] <- expectedNumberOfSubjectsH1[iCase] -
                    sum((designCharacteristics$rejectionProbabilities[1:(kMax - 1)] +
                        designCharacteristics$futilityProbabilities[1:(kMax - 1)]) *
                        (studySubjects[kMax, iCase] - studySubjects[1:(kMax - 1), iCase]))
            }
        }
        informationOverStages[, iCase] <-
            designCharacteristics$shift / log(lambda1[iCase] / lambda2 / thetaH0)^2 *
                informationRates
    }
    if (length(unique(allocationRatioPlanned)) == 1) {
        allocationRatioPlanned <- allocationRatioPlanned[1]
    }

    return(list(
        design = design,
        designCharacteristics = designCharacteristics,
        power = 1 - beta,
        allocationRatioPlanned = allocationRatioPlanned,
        lambda1 = lambda1,
        lambda2 = lambda2,
        lambda = lambda,
        theta = lambda1 / lambda2,
        thetaH0 = thetaH0,
        overDispersion = overDispersion,
        fixedExposureTime = fixedExposureTime,
        accrualTime = accrualTime,
        followUpTime = followUpTime,
        accrualIntensity = accrualIntensity,
        studyTime = studyTime,
        calendarTime = calendarTime,
        studySubjects = studySubjects,
        expectedStudyDurationH1 = expectedStudyDurationH1,
        expectedNumberOfSubjectsH1 = expectedNumberOfSubjectsH1,
        n1 = n1,
        n2 = n2,
        nTotal = n1 + n2,
        informationOverStages = informationOverStages,
        maximumInformation = designCharacteristics$shift / log(lambda1 / lambda2 / thetaH0)^2,
        expectedInformationH0 = designCharacteristics$averageSampleNumber0 *
            designCharacteristics$nFixed / log(lambda1 / lambda2 / thetaH0)^2,
        expectedInformationH01 = designCharacteristics$averageSampleNumber01 *
            designCharacteristics$nFixed / log(lambda1 / lambda2 / thetaH0)^2,
        expectedInformationH1 = designCharacteristics$averageSampleNumber1 *
            designCharacteristics$nFixed / log(lambda1 / lambda2 / thetaH0)^2
    ))
}

#'
#' TODO @Gernot: document
#'
#' @export
#'
getPowerCounts <- function(
        design = NULL, ...,
        directionUpper = TRUE, # C_DIRECTION_UPPER_DEFAULT
        maxNumberOfSubjects = NA_real_,
        lambda1 = NA_real_,
        lambda2 = NA_real_,
        lambda = NA_real_,
        theta = NA_real_,
        thetaH0 = 1,
        overDispersion = 0,
        fixedExposureTime = NA_real_,
        accrualTime = NA_real_,
        accrualIntensity = NA_real_,
        followUpTime = NA_real_,
        allocationRatioPlanned = NA_real_) {
    if (is.na(directionUpper)) {
        directionUpper <- TRUE
    }
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
    if (!any(is.na(theta))) {
        totalCases <- length(theta)
        lambda1 <- rep(NA_real_, totalCases)
    } else if (!any(is.na(lambda1))) {
        totalCases <- length(lambda1)
    } else {
        totalCases <- 1
    }

    kMax <- design$kMax
    informationRates <- design$informationRates
    alpha <- design$alpha
    sided <- design$sided
    designCharacteristics <- getDesignCharacteristics(design)
    shift <- designCharacteristics$shift
    sampleSizeEnabled <- FALSE

    if (any(is.na(allocationRatioPlanned))) {
        allocationRatioPlanned <- C_ALLOCATION_RATIO_DEFAULT
    }
    .assertIsValidAllocationRatioPlannedSampleSize(allocationRatioPlanned, maxNumberOfSubjects)
    .assertIsValidEffectCountData(
        sampleSizeEnabled, sided, lambda1, lambda2, lambda, theta,
        thetaH0, overDispersion
    )
    if (!is.na(lambda2) && !any(is.na(theta))) {
        lambda1 <- lambda2 * theta
    } else if (!any(is.na(lambda1)) && !any(is.na(theta))) {
        lambda2 <- lambda1 / theta
    }
    .assertIsValidParametersCountData(
        sampleSizeEnabled, fixedExposureTime, followUpTime,
        accrualTime, accrualIntensity, maxNumberOfSubjects
    )
    if ((length(accrualTime) > 1) && (accrualTime[1] == 0)) {
        accrualTime <- accrualTime[-1]
    }
    futilityPerStage <- matrix(NA_real_, kMax - 1, totalCases)
    rejectPerStage <- matrix(NA_real_, kMax, totalCases)
    earlyStop <- matrix(NA_real_, kMax, totalCases)
    overallReject <- rep(NA_real_, totalCases)
    for (iCase in (1:totalCases)) {
        if (!(is.na(lambda)) && !any(is.na(theta))) {
            lambda2 <- (1 + allocationRatioPlanned) * lambda / (1 + allocationRatioPlanned * theta[iCase])
            lambda1[iCase] <- lambda2 * theta[iCase]
        }
        if (!any(is.na(accrualIntensity))) {
            const <- allocationRatioPlanned / (1 + allocationRatioPlanned)
            if (sd(accrualIntensity) < 1E-10) {
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
            n2 <- maxNumberOfSubjects / (1 + allocationRatioPlanned)
            n1 <- allocationRatioPlanned * n2
            nTotal <- n1 + n2
            recruit1 <- seq(0, accrualTime, length.out = n1)
            recruit2 <- seq(0, accrualTime, length.out = n2)
        }

        if (!is.na(fixedExposureTime)) {
            varianceEstimate <- (1 + allocationRatioPlanned) * (1 / fixedExposureTime *
                (1 / lambda2 + 1 / (lambda1[iCase] * allocationRatioPlanned)) +
                overDispersion * (1 + 1 / allocationRatioPlanned))
        } else {
            timeUnderObservation1 <-
                pmax(accrualTime + followUpTime - recruit1, 0)
            timeUnderObservation2 <-
                pmax(accrualTime + followUpTime - recruit2, 0)
            sumLambda1 <- sum(timeUnderObservation1 * lambda1[iCase] /
                (1 + overDispersion * timeUnderObservation1 * lambda1[iCase]))
            sumLambda2 <- sum(timeUnderObservation2 * lambda2 /
                (1 + overDispersion * timeUnderObservation2 * lambda2))
            varianceEstimate <- nTotal * (1 / sumLambda1 + 1 / sumLambda2)
        }
        powerAndAverageSampleNumber <- getPowerAndAverageSampleNumber(
            design = design,
            (2 * directionUpper - 1) * log(lambda1[iCase] / lambda2 / thetaH0) /
                sqrt(varianceEstimate),
            nMax = nTotal
        )
        futilityPerStage[, iCase] <- powerAndAverageSampleNumber$futilityPerStage
        rejectPerStage[, iCase] <- powerAndAverageSampleNumber$rejectPerStage
        earlyStop[, iCase] <- powerAndAverageSampleNumber$earlyStop
        overallReject[iCase] <- powerAndAverageSampleNumber$overallReject
    }

    return(list(
        design = design,
        directionUpper = directionUpper,
        designCharacteristics = designCharacteristics,
        allocationRatioPlanned = allocationRatioPlanned,
        lambda1 = lambda1,
        lambda2 = lambda2,
        lambda = lambda,
        theta = theta,
        thetaH0 = thetaH0,
        overDispersion = overDispersion,
        fixedExposureTime = fixedExposureTime,
        accrualTime = accrualTime,
        accrualIntensity = accrualIntensity,
        followUpTime = followUpTime,
        n1 = n1,
        n2 = n2,
        nTotal = nTotal,
        futilityPerStage = futilityPerStage,
        rejectPerStage = rejectPerStage,
        earlyStop = earlyStop,
        overallReject = overallReject
    ))
}
