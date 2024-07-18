## |
## |  *Sample size and power survival*
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
## |  File version: $Revision: 8049 $
## |  Last changed: $Date: 2024-07-17 17:03:09 +0200 (Mi, 17 Jul 2024) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_core_utilities.R
NULL

.addStudyDurationToDesignPlan <- function(designPlan) {
    if (!designPlan$accountForObservationTimes) {
        return(invisible())
    }

    kMax <- designPlan$.design$kMax
    if (kMax == 1) {
        designPlan$studyDuration <- designPlan$analysisTime[1, ]
        designPlan$.setParameterType("studyDuration", C_PARAM_GENERATED)
        designPlan$maxStudyDuration <- designPlan$studyDuration
    } else {
        designPlan$maxStudyDuration <- designPlan$analysisTime[kMax, ]
        designPlan$.setParameterType("maxStudyDuration", C_PARAM_GENERATED)
    }
}

.getLambda <- function(..., groupNumber, lambda2, lambda1, hazardRatio, kappa) {
    if (groupNumber == 1) {
        if (!any(is.na(lambda1))) {
            return(lambda1)
        }

        lambda2 <- lambda2 * hazardRatio^(1 / kappa)
    }
    return(lambda2)
}

.getEventProbabilityFunction <- function(..., time, piecewiseLambda, piecewiseSurvivalTime, phi, kappa) {
    if (length(piecewiseLambda) == 1) {
        if (kappa != 1 && phi > 0) {
            stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "Weibull distribution cannot ",
                "be used together with specified dropout rate (use simulation instead)",
                call. = FALSE
            )
        }

        return(piecewiseLambda / (piecewiseLambda + phi) *
            pweibull(time, shape = kappa, scale = 1 / (piecewiseLambda + phi), lower.tail = TRUE, log.p = FALSE))
    }

    if (length(piecewiseSurvivalTime) != length(piecewiseLambda)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "length of 'piecewiseSurvivalTime' (", .arrayToString(piecewiseSurvivalTime),
            ") must be equal to length of 'piecewiseLambda' (", .arrayToString(piecewiseLambda), ")"
        )
    }

    piecewiseSurvivalTime <- .getPiecewiseExpStartTimesWithoutLeadingZero(piecewiseSurvivalTime)

    if (kappa != 1) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "Weibull distribution cannot be used for piecewise survival definition",
            call. = FALSE
        )
    }
    len <- length(piecewiseSurvivalTime)
    for (i in 1:len) {
        if (i == 1) {
            if (time <= piecewiseSurvivalTime[1]) {
                return(piecewiseLambda[1] / (piecewiseLambda[1] + phi) *
                    (1 - exp(-((piecewiseLambda[1] + phi) * time))))
            }
        } else if (i == 2) {
            cdfPart <- piecewiseLambda[1] / (piecewiseLambda[1] + phi) *
                (1 - exp(-((piecewiseLambda[1] + phi) * piecewiseSurvivalTime[1])))
            if (time <= piecewiseSurvivalTime[2]) {
                cdfFactor <- piecewiseLambda[1] * piecewiseSurvivalTime[1]
                cdf <- cdfPart + piecewiseLambda[2] / (piecewiseLambda[2] + phi) * exp(-cdfFactor) * (
                    exp(-phi * piecewiseSurvivalTime[1]) - exp(-piecewiseLambda[2] *
                        (time - piecewiseSurvivalTime[1]) - phi * time))
                return(cdf)
            }
        } else if (i == 3) {
            cdfPart <- cdfPart + piecewiseLambda[2] / (piecewiseLambda[2] + phi) *
                exp(-piecewiseLambda[1] * piecewiseSurvivalTime[1]) * (
                    exp(-phi * piecewiseSurvivalTime[1]) - exp(-piecewiseLambda[2] *
                        (piecewiseSurvivalTime[2] - piecewiseSurvivalTime[1]) - phi * piecewiseSurvivalTime[2]))
            if (time <= piecewiseSurvivalTime[3]) {
                cdfFactor <- piecewiseLambda[1] * piecewiseSurvivalTime[1] +
                    piecewiseLambda[2] * (piecewiseSurvivalTime[2] - piecewiseSurvivalTime[1])
                cdf <- cdfPart + piecewiseLambda[3] / (piecewiseLambda[3] + phi) * exp(-cdfFactor) * (
                    exp(-phi * piecewiseSurvivalTime[2]) - exp(-piecewiseLambda[3] *
                        (time - piecewiseSurvivalTime[2]) - phi * time))
                return(cdf)
            }
        } else if (i > 3) {
            cdfFactor <- piecewiseLambda[1] * piecewiseSurvivalTime[1] +
                sum(piecewiseLambda[2:(i - 2)] * (piecewiseSurvivalTime[2:(i - 2)] -
                    piecewiseSurvivalTime[1:(i - 3)]))
            cdfPart <- cdfPart + piecewiseLambda[i - 1] / (piecewiseLambda[i - 1] + phi) * exp(-cdfFactor) * (
                exp(-phi * piecewiseSurvivalTime[i - 2]) - exp(-piecewiseLambda[i - 1] *
                    (piecewiseSurvivalTime[i - 1] - piecewiseSurvivalTime[i - 2]) - phi * piecewiseSurvivalTime[i - 1]))
            if (time <= piecewiseSurvivalTime[i]) {
                cdfFactor <- piecewiseLambda[1] * piecewiseSurvivalTime[1] +
                    sum(piecewiseLambda[2:(i - 1)] * (piecewiseSurvivalTime[2:(i - 1)] - piecewiseSurvivalTime[1:(i - 2)]))
                cdf <- cdfPart + piecewiseLambda[i] / (piecewiseLambda[i] + phi) * exp(-cdfFactor) * (
                    exp(-phi * piecewiseSurvivalTime[i - 1]) - exp(-piecewiseLambda[i] *
                        (time - piecewiseSurvivalTime[i - 1]) - phi * time))
                return(cdf)
            }
        }
    }

    if (len == 1) {
        cdfPart <- piecewiseLambda[1] / (piecewiseLambda[1] + phi) *
            (1 - exp(-((piecewiseLambda[1] + phi) * piecewiseSurvivalTime[1])))
    } else if (len == 2) {
        cdfFactor <- piecewiseLambda[1] * piecewiseSurvivalTime[1]
        cdfPart <- cdfPart + piecewiseLambda[len] / (piecewiseLambda[len] + phi) * exp(-cdfFactor) * (
            exp(-phi * piecewiseSurvivalTime[len - 1]) - exp(-piecewiseLambda[len] *
                (piecewiseSurvivalTime[len] - piecewiseSurvivalTime[len - 1]) - phi * piecewiseSurvivalTime[len]))
    } else {
        cdfFactor <- piecewiseLambda[1] * piecewiseSurvivalTime[1] +
            sum(piecewiseLambda[2:(len - 1)] * (piecewiseSurvivalTime[2:(len - 1)] - piecewiseSurvivalTime[1:(len - 2)]))
        cdfPart <- cdfPart + piecewiseLambda[len] / (piecewiseLambda[len] + phi) * exp(-cdfFactor) * (
            exp(-phi * piecewiseSurvivalTime[len - 1]) - exp(-piecewiseLambda[len] *
                (piecewiseSurvivalTime[len] - piecewiseSurvivalTime[len - 1]) - phi * piecewiseSurvivalTime[len]))
    }

    if (len == 1) {
        cdfFactor <- piecewiseLambda[1] * piecewiseSurvivalTime[1]
    } else {
        cdfFactor <- cdfFactor + piecewiseLambda[len] * (piecewiseSurvivalTime[len] - piecewiseSurvivalTime[len - 1])
    }

    cdf <- cdfPart + piecewiseLambda[len + 1] / (piecewiseLambda[len + 1] + phi) * exp(-cdfFactor) * (
        exp(-phi * piecewiseSurvivalTime[len]) - exp(-piecewiseLambda[len + 1] *
            (time - piecewiseSurvivalTime[len]) - phi * time))

    return(cdf)
}

.getEventProbabilityFunctionVec <- function(..., timeVector, piecewiseLambda, piecewiseSurvivalTime, phi, kappa) {
    result <- c()
    for (time in timeVector) {
        result <- c(result, .getEventProbabilityFunction(
            time = time, piecewiseLambda = piecewiseLambda,
            piecewiseSurvivalTime = piecewiseSurvivalTime, phi = phi, kappa = kappa
        ))
    }
    return(result)
}

.getEventProbabilitiesGroupwise <- function(..., time, accrualTimeVector, accrualIntensity, lambda2,
        lambda1, piecewiseSurvivalTime, phi, kappa, allocationRatioPlanned, hazardRatio) {
    .assertIsSingleNumber(time, "time")

    if (length(accrualTimeVector) > 1 && accrualTimeVector[1] == 0) {
        accrualTimeVector <- accrualTimeVector[2:length(accrualTimeVector)]
    }

    accrualTimeVectorLength <- length(accrualTimeVector)
    densityIntervals <- accrualTimeVector
    if (accrualTimeVectorLength > 1) {
        densityIntervals[2:accrualTimeVectorLength] <-
            accrualTimeVector[2:accrualTimeVectorLength] -
            accrualTimeVector[1:(accrualTimeVectorLength - 1)]
    }

    if (length(densityIntervals) > 1 && length(accrualIntensity) > 1 &&
            length(densityIntervals) != length(accrualIntensity)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'densityIntervals' (", .arrayToString(densityIntervals),
            ") and 'accrualIntensity' (", .arrayToString(accrualIntensity), ") must have same length"
        )
    }

    densityVector <- accrualIntensity / sum(densityIntervals * accrualIntensity)

    eventProbs <- rep(NA_real_, 2)

    for (k in 1:accrualTimeVectorLength) {
        if (time <= accrualTimeVector[k]) {
            for (groupNumber in c(1, 2)) { # two groups: 1 = treatment, 2 = control

                lambdaTemp <- .getLambda(
                    groupNumber           = groupNumber,
                    lambda2               = lambda2,
                    lambda1               = lambda1,
                    hazardRatio           = hazardRatio,
                    kappa                 = kappa
                )

                inner <- function(x) {
                    .getEventProbabilityFunctionVec(
                        timeVector = x, piecewiseLambda = lambdaTemp,
                        piecewiseSurvivalTime = piecewiseSurvivalTime, phi = phi[groupNumber], kappa = kappa
                    )
                }
                timeValue1 <- 0
                if (k > 1) {
                    timeValue1 <- time - accrualTimeVector[1]
                }

                eventProbs[groupNumber] <- densityVector[1] * stats::integrate(inner, timeValue1, time)$value

                if (k > 2) {
                    for (j in 2:(k - 1)) {
                        eventProbs[groupNumber] <- eventProbs[groupNumber] +
                            densityVector[j] * stats::integrate(
                                inner, time - accrualTimeVector[j],
                                time - accrualTimeVector[j - 1]
                            )$value
                    }
                }
                if (k > 1) {
                    eventProbs[groupNumber] <- eventProbs[groupNumber] +
                        densityVector[k] * stats::integrate(inner, 0, time - accrualTimeVector[k - 1])$value
                }
            }

            return(eventProbs)
        }
    }

    for (groupNumber in c(1, 2)) {
        lambdaTemp <- .getLambda(
            groupNumber           = groupNumber,
            lambda2               = lambda2,
            lambda1               = lambda1,
            hazardRatio           = hazardRatio,
            kappa                 = kappa
        )

        inner <- function(x) {
            .getEventProbabilityFunctionVec(
                timeVector = x, piecewiseLambda = lambdaTemp,
                piecewiseSurvivalTime = piecewiseSurvivalTime, phi = phi[groupNumber], kappa = kappa
            )
        }

        eventProbs[groupNumber] <- densityVector[1] *
            stats::integrate(inner, time - accrualTimeVector[1], time)$value
        if (accrualTimeVectorLength > 1) {
            for (j in (2:accrualTimeVectorLength)) {
                eventProbs[groupNumber] <- eventProbs[groupNumber] +
                    densityVector[j] * stats::integrate(
                        inner, time - accrualTimeVector[j],
                        time - accrualTimeVector[j - 1]
                    )$value
            }
        }
    }

    return(eventProbs)
}

.getEventProbabilitiesOverall <- function(eventProbs, allocationRatioPlanned) {
    return((allocationRatioPlanned * eventProbs[1] + eventProbs[2]) / (1 + allocationRatioPlanned))
}

.isUserDefinedMaxNumberOfSubjects <- function(designPlan) {
    if (!is.null(designPlan) && length(designPlan$.getParameterType("maxNumberOfSubjects")) > 0) {
        if (designPlan$.getParameterType("maxNumberOfSubjects") == C_PARAM_USER_DEFINED) {
            return(TRUE)
        }
    }

    return(!is.null(designPlan$maxNumberOfSubjects) && length(designPlan$maxNumberOfSubjects) == 1 &&
        !is.na(designPlan$maxNumberOfSubjects) && designPlan$maxNumberOfSubjects > 0)
}

.getEventsFixed <- function(..., typeOfComputation = c("Schoenfeld", "Freedman", "HsiehFreedman"),
        twoSidedPower, alpha, beta, sided, hazardRatio, thetaH0, allocationRatioPlanned) {
    typeOfComputation <- match.arg(typeOfComputation)

    if (typeOfComputation == "Schoenfeld") {
        eventsFixed <- (.getOneMinusQNorm(alpha / sided) + .getOneMinusQNorm(beta))^2 /
            (log(hazardRatio) - log(thetaH0))^2 *
            (1 + allocationRatioPlanned)^2 / allocationRatioPlanned
        if (twoSidedPower && (sided == 2)) {
            up <- 2 * eventsFixed
            eventsFixed <- .getOneDimensionalRoot(
                function(n) {
                    return(stats::pnorm(.getOneMinusQNorm(alpha / 2) - sqrt(n) *
                        (log(hazardRatio) - log(thetaH0)) * sqrt(allocationRatioPlanned) /
                        (1 + allocationRatioPlanned)) -
                        stats::pnorm(-.getOneMinusQNorm(alpha / 2) - sqrt(n) *
                            (log(hazardRatio) - log(thetaH0)) * sqrt(allocationRatioPlanned) /
                            (1 + allocationRatioPlanned)) - beta)
                },
                lower = 0.001, upper = up, tolerance = 1e-04,
                callingFunctionInformation = ".getEventsFixed"
            )
        }
        return(eventsFixed)
    }

    if (typeOfComputation == "Freedman") {
        eventsFixed <- (.getOneMinusQNorm(alpha / sided) + .getOneMinusQNorm(beta))^2 *
            (1 + hazardRatio * allocationRatioPlanned)^2 / (1 - hazardRatio)^2 /
            allocationRatioPlanned
        if (twoSidedPower && (sided == 2)) {
            up <- 2 * eventsFixed
            eventsFixed <- .getOneDimensionalRoot(
                function(n) {
                    return(stats::pnorm(.getOneMinusQNorm(alpha / 2) - sqrt(n) *
                        sqrt(allocationRatioPlanned) * (1 - hazardRatio) /
                        (1 + allocationRatioPlanned * hazardRatio)) -
                        stats::pnorm(-.getOneMinusQNorm(alpha / 2) - sqrt(n) *
                            sqrt(allocationRatioPlanned) * (1 - hazardRatio) /
                            (1 + allocationRatioPlanned * hazardRatio)) - beta)
                },
                lower = 0.001, upper = up, tolerance = 1e-04,
                callingFunctionInformation = ".getEventsFixed"
            )
        }
        return(eventsFixed)
    }

    if (typeOfComputation == "HsiehFreedman") {
        eventsFixed <- (.getOneMinusQNorm(alpha / sided) + .getOneMinusQNorm(beta))^2 *
            (1 + hazardRatio)^2 / (1 - hazardRatio)^2 *
            (1 + allocationRatioPlanned)^2 / (4 * allocationRatioPlanned)
        if (twoSidedPower && sided == 2) {
            up <- 2 * eventsFixed
            eventsFixed <- .getOneDimensionalRoot(
                function(n) {
                    return(stats::pnorm(.getOneMinusQNorm(alpha / 2) - sqrt(n) *
                        2 * sqrt(allocationRatioPlanned) / (1 + allocationRatioPlanned) *
                        (1 - hazardRatio) / (1 + hazardRatio)) -
                        stats::pnorm(-.getOneMinusQNorm(alpha / 2) - sqrt(n) *
                            2 * sqrt(allocationRatioPlanned) / (1 + allocationRatioPlanned) *
                            (1 - hazardRatio) / (1 + hazardRatio)) - beta)
                },
                lower = 0.001, upper = up, tolerance = 1e-04,
                callingFunctionInformation = ".getEventsFixed"
            )
        }
        return(eventsFixed)
    }
}

.getEventProbabilities <- function(..., time, accrualTimeVector, accrualIntensity, lambda2,
        lambda1, piecewiseSurvivalTime, phi, kappa, allocationRatioPlanned, hazardRatio) {
    eventProbs <- .getEventProbabilitiesGroupwise(
        time = time, accrualTimeVector = accrualTimeVector,
        accrualIntensity = accrualIntensity, lambda2 = lambda2,
        lambda1 = lambda1, piecewiseSurvivalTime = piecewiseSurvivalTime, phi = phi,
        kappa = kappa, allocationRatioPlanned = allocationRatioPlanned, hazardRatio = hazardRatio
    )

    return(.getEventProbabilitiesOverall(eventProbs, allocationRatioPlanned))
}

.getEffectScaleBoundaryDataSurvival <- function(designPlan) {
    design <- designPlan$.design
    thetaH0 <- designPlan$thetaH0
    cumulativeEventsPerStage <- designPlan$cumulativeEventsPerStage
    allocationRatioPlanned <- designPlan$allocationRatioPlanned
    directionUpper <- designPlan$directionUpper

    if (design$kMax == 1) {
        nParameters <- length(cumulativeEventsPerStage)
    } else {
        nParameters <- ncol(cumulativeEventsPerStage)
    }

    directionUpper[is.na(directionUpper)] <- TRUE

    if (length(allocationRatioPlanned) == 1) {
        allocationRatioPlanned <- rep(allocationRatioPlanned, nParameters)
    }

    futilityBounds <- design$futilityBounds
    futilityBounds[!is.na(futilityBounds) & futilityBounds <= C_FUTILITY_BOUNDS_DEFAULT] <- NA_real_

    criticalValues <- design$criticalValues

    criticalValuesEffectScaleUpper <- matrix(, nrow = design$kMax, ncol = nParameters)
    criticalValuesEffectScaleLower <- matrix(, nrow = design$kMax, ncol = nParameters)
    futilityBoundsEffectScaleUpper <- matrix(, nrow = design$kMax - 1, ncol = nParameters)
    futilityBoundsEffectScaleLower <- matrix(, nrow = design$kMax - 1, ncol = nParameters)

    for (j in (1:nParameters)) {
        if (design$sided == 1) {
            criticalValuesEffectScaleUpper[, j] <- thetaH0 * (exp((2 * directionUpper[j] - 1) * criticalValues *
                (1 + allocationRatioPlanned[j]) / sqrt(allocationRatioPlanned[j] *
                    cumulativeEventsPerStage[, j])))
        } else {
            criticalValuesEffectScaleUpper[, j] <- thetaH0 * (exp((2 * directionUpper[j] - 1) * criticalValues *
                (1 + allocationRatioPlanned[j]) / sqrt(allocationRatioPlanned[j] *
                    cumulativeEventsPerStage[, j])))
            criticalValuesEffectScaleLower[, j] <- thetaH0 * (exp(-(2 * directionUpper[j] - 1) * criticalValues *
                (1 + allocationRatioPlanned[j]) / sqrt(allocationRatioPlanned[j] *
                    cumulativeEventsPerStage[, j])))
        }

        if (!.isTrialDesignFisher(design) && !all(is.na(futilityBounds))) {
            futilityBoundsEffectScaleUpper[, j] <- thetaH0 * (exp((2 * directionUpper[j] - 1) * futilityBounds *
                (1 + allocationRatioPlanned[j]) / sqrt(allocationRatioPlanned[j] *
                    cumulativeEventsPerStage[1:(design$kMax - 1), j])))
        }
        if (!.isTrialDesignFisher(design) && design$sided == 2 && design$kMax > 1 &&
                (design$typeOfDesign == C_TYPE_OF_DESIGN_PT || !is.null(design$typeBetaSpending) && design$typeBetaSpending != "none")) {
            futilityBoundsEffectScaleLower[, j] <- thetaH0 * (exp(-(2 * directionUpper[j] - 1) * futilityBounds *
                (1 + allocationRatioPlanned[j]) / sqrt(allocationRatioPlanned[j] *
                    cumulativeEventsPerStage[1:(design$kMax - 1), j])))
        }
    }

    return(list(
        criticalValuesEffectScaleUpper = matrix(criticalValuesEffectScaleUpper, nrow = design$kMax),
        criticalValuesEffectScaleLower = matrix(criticalValuesEffectScaleLower, nrow = design$kMax),
        futilityBoundsEffectScaleUpper = matrix(futilityBoundsEffectScaleUpper, nrow = design$kMax - 1),
        futilityBoundsEffectScaleLower = matrix(futilityBoundsEffectScaleLower, nrow = design$kMax - 1)
    ))
}

.calculateSampleSizeSurvival <- function(designPlan) {
    # Fixed
    designPlan <- .getSampleSizeFixedSurvival(designPlan)

    # Sequential
    if (designPlan$.design$kMax > 1) {
        designCharacteristics <- getDesignCharacteristics(designPlan$.design)
        designPlan <- .getSampleSizeSequentialSurvival(designPlan, designCharacteristics)
    }

    if (designPlan$accountForObservationTimes && !any(is.na(designPlan$followUpTime)) &&
            all(designPlan$followUpTime == C_FOLLOW_UP_TIME_DEFAULT)) {
        designPlan$.setParameterType("followUpTime", C_PARAM_DEFAULT_VALUE)
    }

    .addEffectScaleBoundaryDataToDesignPlan(designPlan)
    designPlan$singleEventsPerStage <- .getSingleEventsPerStage(designPlan$cumulativeEventsPerStage)
    .addDeprecatedFieldValues(designPlan, "eventsPerStage", designPlan$cumulativeEventsPerStage)

    paramName <- ifelse(designPlan$.design$kMax == 1, "nFixed", "maxNumberOfSubjects")
    if (designPlan$.getParameterType(paramName) == C_PARAM_GENERATED &&
            designPlan$.accrualTime$.getParameterType("maxNumberOfSubjects") != C_PARAM_GENERATED &&
            all(designPlan$accrualIntensity < 1)) {
        numberOfDefinedAccrualIntensities <- length(designPlan$accrualIntensity)

        accrualTime <- designPlan$accrualTime
        if (length(accrualTime) > 0 && accrualTime[1] != 0) {
            accrualTime <- c(0, accrualTime)
        }

        if (any(designPlan$accrualIntensity < 0)) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'accrualIntensityRelative' (",
                .arrayToString(designPlan$accrualIntensity), ") must be >= 0"
            )
        }

        designPlan$accrualIntensityRelative <- designPlan$accrualIntensity
        if (identical(designPlan$accrualIntensityRelative, C_ACCRUAL_INTENSITY_DEFAULT)) {
            designPlan$.setParameterType("accrualIntensityRelative", C_PARAM_NOT_APPLICABLE)
        } else {
            designPlan$.setParameterType(
                "accrualIntensityRelative",
                designPlan$.getParameterType("accrualIntensity")
            )
        }

        accrualIntensityAbsolute <- numeric()
        for (maxNumberOfSubjects in designPlan[[paramName]]) {
            accrualSetup <- getAccrualTime(
                accrualTime = accrualTime,
                accrualIntensity = designPlan$accrualIntensityRelative,
                accrualIntensityType = "relative",
                maxNumberOfSubjects = maxNumberOfSubjects
            )
            accrualIntensityAbsolute <- c(accrualIntensityAbsolute, accrualSetup$accrualIntensity)
        }
        designPlan$accrualIntensity <- accrualIntensityAbsolute
        designPlan$.setParameterType("accrualIntensity", C_PARAM_GENERATED)

        if (numberOfDefinedAccrualIntensities > 1) {
            paramName <- NULL
            if (designPlan$.getParameterType("pi1") == C_PARAM_USER_DEFINED ||
                    designPlan$.getParameterType("pi1") == C_PARAM_DEFAULT_VALUE ||
                    designPlan$.getParameterType("pi2") == C_PARAM_USER_DEFINED) {
                paramName <- "pi1"
            } else if (designPlan$.getParameterType("median1") == C_PARAM_USER_DEFINED ||
                    designPlan$.getParameterType("median2") == C_PARAM_USER_DEFINED) {
                paramName <- "median1"
            }
            if (!is.null(paramName)) {
                paramValue <- designPlan[[paramName]]
                if (!is.null(paramValue) && length(paramValue) > 1) {
                    stop(
                        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                        "the definition of relative accrual intensities ",
                        "(all 'accrualIntensity' values < 1) ",
                        "is only available for a single value ",
                        "(", paramName, " = ", .arrayToString(
                            paramValue,
                            vectorLookAndFeelEnabled = TRUE
                        ), ")"
                    )
                }
            }
        }
    }

    designPlan$maxNumberOfEvents <- designPlan$cumulativeEventsPerStage[designPlan$.design$kMax, ]
    designPlan$.setParameterType("maxNumberOfEvents", C_PARAM_GENERATED)

    if (!any(is.na(designPlan$followUpTime))) {
        if (any(designPlan$followUpTime < -1e-02)) {
            warning("Accrual duration longer than maximal study ",
                "duration (time to maximal number of events); followUpTime = ",
                .arrayToString(designPlan$followUpTime),
                call. = FALSE
            )
        }
    } else {
        warning("Follow-up time could not be calculated for hazardRatio = ",
            .arrayToString(designPlan$hazardRatio),
            call. = FALSE
        )
    }

    if (designPlan$.getParameterType("accountForObservationTimes") != C_PARAM_USER_DEFINED) {
        designPlan$.setParameterType("accountForObservationTimes", C_PARAM_NOT_APPLICABLE)
    }
    designPlan$.setParameterType("chi", C_PARAM_NOT_APPLICABLE)

    .addStudyDurationToDesignPlan(designPlan)

    return(designPlan)
}

# Hidden parameter:
# @param accountForObservationTimes If \code{accountForObservationTimes = TRUE}, the number of
#        subjects is calculated assuming specific accrual and follow-up time, default is \code{TRUE}
#        (see details).
# If \code{accountForObservationTimes = FALSE}, only the event rates are used for the calculation
# of the maximum number of subjects.
# \code{accountForObservationTimes} can be selected as \code{FALSE}. In this case,
# the number of subjects is calculated from the event probabilities only.
# This kind of computation does not account for the specific accrual pattern and survival distribution.
.getSampleSizeSurvival <- function(...,
        design = NULL,
        typeOfComputation = c("Schoenfeld", "Freedman", "HsiehFreedman"),
        thetaH0 = 1,
        pi2 = NA_real_,
        pi1 = NA_real_,
        allocationRatioPlanned = NA_real_,
        accountForObservationTimes = TRUE,
        eventTime = C_EVENT_TIME_DEFAULT,
        accrualTime = C_ACCRUAL_TIME_DEFAULT,
        accrualIntensity = C_ACCRUAL_INTENSITY_DEFAULT,
        accrualIntensityType = c("auto", "absolute", "relative"),
        kappa = 1,
        piecewiseSurvivalTime = NA_real_,
        lambda2 = NA_real_,
        lambda1 = NA_real_,
        median1 = NA_real_,
        median2 = NA_real_,
        followUpTime = NA_real_,
        maxNumberOfSubjects = NA_real_,
        dropoutRate1 = 0,
        dropoutRate2 = dropoutRate1,
        dropoutTime = NA_real_,
        hazardRatio = NA_real_,
        userFunctionCallEnabled = TRUE) {
    designPlan <- .createDesignPlanSurvival(
        objectType = "sampleSize",
        design = design,
        typeOfComputation = typeOfComputation,
        thetaH0 = thetaH0,
        pi2 = pi2,
        pi1 = pi1,
        allocationRatioPlanned = allocationRatioPlanned,
        accountForObservationTimes = accountForObservationTimes,
        eventTime = eventTime,
        accrualTime = accrualTime,
        accrualIntensity = accrualIntensity,
        accrualIntensityType = accrualIntensityType,
        kappa = kappa,
        piecewiseSurvivalTime = piecewiseSurvivalTime,
        lambda2 = lambda2,
        lambda1 = lambda1,
        median1 = median1,
        median2 = median2,
        followUpTime = followUpTime,
        maxNumberOfSubjects = maxNumberOfSubjects,
        dropoutRate1 = dropoutRate1,
        dropoutRate2 = dropoutRate2,
        dropoutTime = dropoutTime,
        hazardRatio = hazardRatio,
        userFunctionCallEnabled = userFunctionCallEnabled
    )
    return(.calculateSampleSizeSurvival(designPlan))
}

.createDesignPlanSurvival <- function(..., objectType = c("sampleSize", "power"),
        design,
        typeOfComputation = c("Schoenfeld", "Freedman", "HsiehFreedman"),
        thetaH0, pi2, pi1,
        allocationRatioPlanned,
        accountForObservationTimes,
        eventTime,
        accrualTime,
        accrualIntensity,
        accrualIntensityType,
        kappa,
        piecewiseSurvivalTime,
        lambda2,
        lambda1,
        median1,
        median2,
        followUpTime = NA_real_,
        directionUpper = NA,
        maxNumberOfEvents = NA_real_,
        maxNumberOfSubjects,
        dropoutRate1,
        dropoutRate2,
        dropoutTime,
        hazardRatio,
        userFunctionCallEnabled = TRUE) {
    objectType <- match.arg(objectType)
    typeOfComputation <- .matchArgument(typeOfComputation, "Schoenfeld")

    if (userFunctionCallEnabled) {
        .assertIsTrialDesignInverseNormalOrGroupSequential(design)
        .assertIsValidAlphaAndBeta(design$alpha, design$beta)
        .assertIsValidSidedParameter(design$sided)
        .assertIsSingleLogical(accountForObservationTimes, "accountForObservationTimes", naAllowed = TRUE)
        .assertIsSingleNumber(thetaH0, "thetaH0")
        .assertIsValidThetaH0(thetaH0, endpoint = "survival", groups = 2)
        .assertIsValidKappa(kappa)
        directionUpper <- .assertIsValidDirectionUpper(directionUpper,
            design$sided, objectType,
            userFunctionCallEnabled = TRUE
        )

        if (objectType == "power") {
            .assertIsSingleNumber(maxNumberOfEvents, "maxNumberOfEvents")
            .assertIsInClosedInterval(maxNumberOfEvents, "maxNumberOfEvents",
                lower = 1, upper = maxNumberOfSubjects
            )
        }

        if (!any(is.na(pi1)) && (any(pi1 <= 0) || any(pi1 >= 1))) {
            stop(
                C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS,
                "event rate 'pi1' (", .arrayToString(pi1), ") is out of bounds (0; 1)"
            )
        }

        if (!any(is.na(pi2)) && (any(pi2 <= 0) || any(pi2 >= 1))) {
            stop(
                C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS,
                "event rate 'pi2' (", .arrayToString(pi2), ") is out of bounds (0; 1)"
            )
        }

        if (design$sided == 2 && thetaH0 != 1) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "two-sided case is implemented for superiority testing only (i.e., thetaH0 = 1)"
            )
        }

        if (thetaH0 <= 0) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "null hypothesis hazard ratio is not allowed be negative or zero"
            )
        }

        if (!(typeOfComputation %in% c("Schoenfeld", "Freedman", "HsiehFreedman"))) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "computation type ('", typeOfComputation, "') must be one of the following: ",
                "'Schoenfeld', 'Freedman', or 'HsiehFreedman' "
            )
        }

        if (typeOfComputation != "Schoenfeld" && thetaH0 != 1) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "Freedman test calculation is possible only for superiority testing (thetaH0 != 1)"
            )
        }
    }

    if (is.numeric(accrualTime) && all(is.na(accrualTime))) {
        accrualTime <- C_ACCRUAL_TIME_DEFAULT
    }
    if (all(is.na(accrualIntensity))) {
        accrualIntensity <- C_ACCRUAL_INTENSITY_DEFAULT
    }

    accrualSetup <- getAccrualTime(
        accrualTime = accrualTime,
        accrualIntensity = accrualIntensity,
        accrualIntensityType = accrualIntensityType,
        maxNumberOfSubjects = maxNumberOfSubjects
    )
    accrualSetup$.validate()

    if (is.na(allocationRatioPlanned)) {
        allocationRatioPlanned <- C_ALLOCATION_RATIO_DEFAULT
    }

    if (userFunctionCallEnabled) {
        .assertIsValidAllocationRatioPlannedSampleSize(allocationRatioPlanned, maxNumberOfSubjects)
    }

    designPlan <- TrialDesignPlanSurvival$new(
        design = design,
        typeOfComputation = typeOfComputation,
        thetaH0 = thetaH0,
        allocationRatioPlanned = allocationRatioPlanned,
        accountForObservationTimes = accountForObservationTimes,
        eventTime = eventTime,
        accrualTime = accrualSetup$.getAccrualTimeWithoutLeadingZero(),
        accrualIntensity = accrualSetup$accrualIntensity,
        kappa = kappa,
        followUpTime = followUpTime,
        maxNumberOfSubjects = maxNumberOfSubjects,
        dropoutRate1 = dropoutRate1,
        dropoutRate2 = dropoutRate2,
        dropoutTime = dropoutTime,
        hazardRatio = hazardRatio
    )

    .setValueAndParameterType(
        designPlan, "allocationRatioPlanned",
        allocationRatioPlanned, C_ALLOCATION_RATIO_DEFAULT
    )
    .setValueAndParameterType(designPlan, "dropoutRate1", dropoutRate1, C_DROP_OUT_RATE_1_DEFAULT)
    .setValueAndParameterType(designPlan, "dropoutRate2", dropoutRate2, C_DROP_OUT_RATE_2_DEFAULT)
    .setValueAndParameterType(designPlan, "dropoutTime", dropoutTime, C_DROP_OUT_TIME_DEFAULT)
    .setValueAndParameterType(designPlan, "kappa", kappa, 1)

    designPlan$.setObjectType(objectType)

    designPlan$criticalValuesPValueScale <- matrix(design$stageLevels, ncol = 1)
    if (design$sided == 2) {
        designPlan$criticalValuesPValueScale <- designPlan$criticalValuesPValueScale * 2
        designPlan$.setParameterType("criticalValuesPValueScale", C_PARAM_GENERATED)
    }

    if (any(design$futilityBounds > C_FUTILITY_BOUNDS_DEFAULT)) {
        designPlan$futilityBoundsPValueScale <- matrix(1 - stats::pnorm(design$futilityBounds), ncol = 1)
        designPlan$.setParameterType("futilityBoundsPValueScale", C_PARAM_GENERATED)
    }

    designPlan$.accrualTime <- accrualSetup

    designPlan$totalAccrualTime <- accrualSetup$accrualTime[length(accrualSetup$accrualTime)]
    if (length(accrualSetup$accrualTime) > 2) {
        designPlan$.setParameterType("totalAccrualTime", C_PARAM_GENERATED)
    } else {
        designPlan$.setParameterType("totalAccrualTime", C_PARAM_NOT_APPLICABLE)
    }

    if (is.na(maxNumberOfSubjects)) {
        if (!is.na(accrualSetup$maxNumberOfSubjects)) {
            designPlan$maxNumberOfSubjects <- accrualSetup$maxNumberOfSubjects
            designPlan$.setParameterType(
                "maxNumberOfSubjects",
                accrualSetup$.getParameterType("maxNumberOfSubjects")
            )
        }
    } else if (maxNumberOfSubjects == 0) {
        designPlan$.setParameterType("maxNumberOfSubjects", C_PARAM_GENERATED)
    } else {
        designPlan$.setParameterType("maxNumberOfSubjects", C_PARAM_USER_DEFINED)
    }

    if (isTRUE(all.equal(accrualSetup$accrualTime, C_ACCRUAL_TIME_DEFAULT)) ||
            isTRUE(all.equal(
                as.integer(c(0L, accrualSetup$.getAccrualTimeWithoutLeadingZero())),
                C_ACCRUAL_TIME_DEFAULT))
            ) {
        designPlan$.setParameterType("accrualTime", C_PARAM_DEFAULT_VALUE)
    } else {
        designPlan$.setParameterType("accrualTime", accrualSetup$.getParameterType("accrualTime"))
    }

    if (length(designPlan$accrualIntensity) == 1 &&
            designPlan$accrualIntensity == C_ACCRUAL_INTENSITY_DEFAULT) {
        designPlan$.setParameterType("accrualIntensity", C_PARAM_DEFAULT_VALUE)
    } else {
        designPlan$.setParameterType(
            "accrualIntensity",
            accrualSetup$.getParameterType("accrualIntensity")
        )
    }

    .assertIsSingleNumber(designPlan$eventTime, "eventTime")
    .assertIsSingleNumber(designPlan$allocationRatioPlanned, "allocationRatioPlanned")
    .assertIsSingleNumber(designPlan$kappa, "kappa")
    if (objectType == "power") {
        .assertIsValidMaxNumberOfSubjects(designPlan$maxNumberOfSubjects)
    }
    .assertIsSingleNumber(designPlan$dropoutRate1, "dropoutRate1")
    .assertIsSingleNumber(designPlan$dropoutRate2, "dropoutRate2")
    .assertIsSingleNumber(designPlan$dropoutTime, "dropoutTime")

    if (objectType == "power") {
        pi1Default <- C_PI_1_DEFAULT
    } else {
        pi1Default <- C_PI_1_SAMPLE_SIZE_DEFAULT
    }
    designPlan$.piecewiseSurvivalTime <- getPiecewiseSurvivalTime(
        piecewiseSurvivalTime = piecewiseSurvivalTime, lambda2 = lambda2, lambda1 = lambda1,
        median1 = median1, median2 = median2,
        hazardRatio = hazardRatio, pi1 = pi1, pi2 = pi2, eventTime = eventTime, kappa = kappa,
        .pi1Default = pi1Default
    )
    designPlan$.setParameterType("kappa", designPlan$.piecewiseSurvivalTime$.getParameterType("kappa"))

    if (designPlan$.piecewiseSurvivalTime$.getParameterType("pi1") == C_PARAM_DEFAULT_VALUE &&
            length(designPlan$.piecewiseSurvivalTime$pi1) > 1 &&
            length(accrualSetup$accrualIntensity) > 1 && all(accrualSetup$accrualIntensity < 1)) {
        designPlan$.piecewiseSurvivalTime$pi1 <- designPlan$.piecewiseSurvivalTime$pi1[1]
        warning("Only the first default 'pi1' (", designPlan$.piecewiseSurvivalTime$pi1, ") was used ",
            "because the accrual intensities (", .arrayToString(accrualSetup$accrualIntensity), ") ",
            "were defined relative (all accrual intensities are < 1)",
            call. = FALSE
        )
    }

    .initDesignPlanSurvival(designPlan)

    designPlan$.setParameterType("followUpTime", C_PARAM_NOT_APPLICABLE)
    if (designPlan$accountForObservationTimes) {
        .assertIsSingleNumber(dropoutRate1, "dropoutRate1")
        .assertIsSingleNumber(dropoutRate2, "dropoutRate2")
        .assertIsSingleNumber(dropoutTime, "dropoutTime")

        if (!is.na(dropoutTime) && dropoutTime <= 0) {
            stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'dropoutTime' (", dropoutTime, ") must be > 0")
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

        if (!is.na(eventTime) && eventTime <= 0) {
            stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'eventTime' (", eventTime, ") must be > 0")
        }

        .assertIsValidAccrualTime(accrualSetup$accrualTime)

        .assertIsValidFollowUpTime(followUpTime)
        .setValueAndParameterType(designPlan, "followUpTime", followUpTime, C_FOLLOW_UP_TIME_DEFAULT)

        if (.isUserDefinedMaxNumberOfSubjects(designPlan) && !is.null(followUpTime) &&
                length(followUpTime) == 1 && !is.na(followUpTime)) {
            warning("Follow-up time will be calculated, value entered (",
                followUpTime, ") is not taken into account",
                call. = FALSE
            )
        } else if (is.na(followUpTime)) {
            designPlan$followUpTime <- C_FOLLOW_UP_TIME_DEFAULT
            designPlan$.setParameterType("followUpTime", C_PARAM_DEFAULT_VALUE)
        }

        if (objectType == "power") {
            designPlan$followUpTime <- NA_real_
            designPlan$.setParameterType("followUpTime", C_PARAM_NOT_APPLICABLE)
        }
    } else {
        for (p in c(
            "accrualTime", "accrualIntensity",
            "eventTime", "dropoutRate1", "dropoutRate2", "dropoutTime", "followUpTime",
            "analysisTime", "studyDuration"
        )) {
            designPlan$.setParameterType(p, C_PARAM_NOT_APPLICABLE)
        }
        if (designPlan$.getParameterType("accrualTime") == C_PARAM_USER_DEFINED ||
                !isTRUE(all.equal(accrualTime, C_ACCRUAL_TIME_DEFAULT))) {
            designPlan$.warnInCaseArgumentExists(accrualSetup$accrualTime, "accrualTime")
        }
        designPlan$.warnInCaseArgumentExists(dropoutRate1, "dropoutRate1")
        designPlan$.warnInCaseArgumentExists(dropoutRate2, "dropoutRate2")
        if (!identical(dropoutTime, C_DROP_OUT_TIME_DEFAULT)) {
            designPlan$.warnInCaseArgumentExists(dropoutTime, "dropoutTime")
        }
        designPlan$.warnInCaseArgumentExists(maxNumberOfSubjects, "maxNumberOfSubjects")
        if (!identical(followUpTime, C_FOLLOW_UP_TIME_DEFAULT)) {
            designPlan$.warnInCaseArgumentExists(followUpTime, "followUpTime")
        }
    }

    .setValueAndParameterType(designPlan, "directionUpper", directionUpper, TRUE)
    if (objectType == "power") {
        .setValueAndParameterType(designPlan, "maxNumberOfEvents", maxNumberOfEvents, NA_real_)
        designPlan$.setParameterType("accountForObservationTimes", C_PARAM_NOT_APPLICABLE)
    }

    return(designPlan)
}

.initDesignPlanSurvivalByPiecewiseSurvivalTimeObject <- function(designPlan, pwstSetup) {
    designPlan$pi1 <- pwstSetup$pi1
    designPlan$.setParameterType("pi1", pwstSetup$.getParameterType("pi1"))

    designPlan$pi2 <- pwstSetup$pi2
    designPlan$.setParameterType("pi2", pwstSetup$.getParameterType("pi2"))

    designPlan$hazardRatio <- pwstSetup$hazardRatio
    designPlan$.setParameterType("hazardRatio", pwstSetup$.getParameterType("hazardRatio"))

    designPlan$lambda1 <- pwstSetup$lambda1
    designPlan$.setParameterType("lambda1", pwstSetup$.getParameterType("lambda1"))

    designPlan$lambda2 <- pwstSetup$lambda2
    designPlan$.setParameterType("lambda2", pwstSetup$.getParameterType("lambda2"))

    designPlan$median1 <- pwstSetup$median1
    designPlan$.setParameterType("median1", pwstSetup$.getParameterType("median1"))

    designPlan$median2 <- pwstSetup$median2
    designPlan$.setParameterType("median2", pwstSetup$.getParameterType("median2"))

    designPlan$piecewiseSurvivalTime <- pwstSetup$piecewiseSurvivalTime
    designPlan$.setParameterType(
        "piecewiseSurvivalTime",
        pwstSetup$.getParameterType("piecewiseSurvivalTime")
    )

    designPlan$eventTime <- pwstSetup$eventTime
    designPlan$.setParameterType("eventTime", pwstSetup$.getParameterType("eventTime"))

    if (pwstSetup$.isLambdaBased()) {
        return(length(designPlan$hazardRatio))
    }

    return(length(designPlan$pi1))
}

.initDesignPlanSurvival <- function(designPlan) {
    numberOfResults <- .initDesignPlanSurvivalByPiecewiseSurvivalTimeObject(
        designPlan, designPlan$.piecewiseSurvivalTime
    )

    if (designPlan$.piecewiseSurvivalTime$.isLambdaBased()) {
        if (length(designPlan$accountForObservationTimes) == 0 ||
                is.na(designPlan$accountForObservationTimes) ||
                !designPlan$accountForObservationTimes) {
            designPlan$accountForObservationTimes <- TRUE
            designPlan$.setParameterType("accountForObservationTimes", C_PARAM_DEFAULT_VALUE)
        }

        if (!designPlan$accountForObservationTimes) {
            designPlan$accountForObservationTimes <- TRUE
            warning("'accountForObservationTimes' was set to TRUE ",
                "because piecewise exponential survival function is enabled",
                call. = FALSE
            )
        }
    } else {
        if (.isUserDefinedMaxNumberOfSubjects(designPlan)) {
            if (length(designPlan$accountForObservationTimes) != 0 &&
                    !is.na(designPlan$accountForObservationTimes) &&
                    !designPlan$accountForObservationTimes) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'accountForObservationTimes' must be TRUE because 'maxNumberOfSubjects' is > 0"
                )
            }

            designPlan$.setParameterType("accountForObservationTimes", C_PARAM_GENERATED)
            designPlan$accountForObservationTimes <- TRUE
        } else {
            if (length(designPlan$accountForObservationTimes) == 0 ||
                    is.na(designPlan$accountForObservationTimes)) {
                designPlan$accountForObservationTimes <- FALSE
                designPlan$.setParameterType("accountForObservationTimes", C_PARAM_DEFAULT_VALUE)
            } else {
                designPlan$.setParameterType(
                    "accountForObservationTimes",
                    ifelse(designPlan$accountForObservationTimes,
                        C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
                    )
                )
            }
        }
    }

    designPlan$.setParameterType("chi", C_PARAM_NOT_APPLICABLE)

    if (designPlan$.isSampleSizeObject()) {
        designPlan$.setParameterType("directionUpper", C_PARAM_NOT_APPLICABLE)
        designPlan$.setParameterType("maxNumberOfEvents", C_PARAM_NOT_APPLICABLE)
    }

    return(numberOfResults = numberOfResults)
}

.getSampleSizeFixedSurvival <- function(designPlan) {
    alpha <- designPlan$getAlpha()
    beta <- designPlan$getBeta()
    sided <- designPlan$getSided()
    twoSidedPower <- designPlan$getTwoSidedPower()
    typeOfComputation <- designPlan$typeOfComputation
    thetaH0 <- designPlan$thetaH0
    pi1 <- designPlan$pi1
    pi2 <- designPlan$pi2
    allocationRatioPlanned <- designPlan$allocationRatioPlanned
    accountForObservationTimes <- designPlan$accountForObservationTimes
    accrualTime <- designPlan$accrualTime
    kappa <- designPlan$kappa
    piecewiseSurvivalTime <- designPlan$piecewiseSurvivalTime
    maxNumberOfSubjects <- designPlan$maxNumberOfSubjects
    hazardRatio <- designPlan$hazardRatio

    .assertIsValidHazardRatio(hazardRatio, thetaH0)

    if (designPlan$.piecewiseSurvivalTime$.isLambdaBased()) {
        numberOfResults <- length(hazardRatio)
    } else {
        numberOfResults <- length(pi1)
    }

    designPlan$eventsFixed <- rep(NA_real_, numberOfResults) # number of events
    designPlan$nFixed <- rep(NA_real_, numberOfResults) # number of subjects
    designPlan$chi <- rep(NA_real_, numberOfResults) # probability of an event

    calculateAllocationRatioPlanned <- FALSE
    if (allocationRatioPlanned == 0) {
        allocationRatioPlannedVec <- rep(NA_real_, numberOfResults)
        calculateAllocationRatioPlanned <- TRUE
        designPlan$optimumAllocationRatio <- TRUE
        designPlan$.setParameterType("optimumAllocationRatio", C_PARAM_USER_DEFINED)
    }

    userDefinedMaxNumberOfSubjects <- .isUserDefinedMaxNumberOfSubjects(designPlan)
    if (userDefinedMaxNumberOfSubjects && allocationRatioPlanned == 0) {
        stop(
            C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
            "determination of optimum allocation ('allocationRatioPlanned' = 0) not possible ",
            "for given 'maxNumberOfSubjects' (", designPlan$maxNumberOfSubjects, ")"
        )
    }

    if (userDefinedMaxNumberOfSubjects) {
        timeVector <- rep(NA_real_, numberOfResults)
    }

    designPlan$.calculateFollowUpTime <- FALSE

    lambda1 <- designPlan$lambda1
    if (designPlan$.piecewiseSurvivalTime$piecewiseSurvivalEnabled) {
        lambda1 <- rep(NA_real_, numberOfResults)
    }

    for (i in 1:numberOfResults) {
        phi <- -c(
            log(1 - designPlan$dropoutRate1),
            log(1 - designPlan$dropoutRate2)
        ) / designPlan$dropoutTime

        if (!userDefinedMaxNumberOfSubjects) {
            if (calculateAllocationRatioPlanned) {
                # allocationRatioPlanned = 0 provides optimum sample size
                allocationRatioPlanned <- stats::optimize(function(x) {
                    numberEvents <- .getEventsFixed(
                        typeOfComputation = typeOfComputation, twoSidedPower = twoSidedPower,
                        alpha = alpha, beta = beta, sided = sided, hazardRatio = hazardRatio[i],
                        thetaH0 = thetaH0, allocationRatioPlanned = x
                    )

                    if (!accountForObservationTimes) {
                        probEvent <- (x * pi1[i] + pi2) / (1 + x)
                    } else {
                        probEvent <- .getEventProbabilities(
                            time = accrualTime[length(accrualTime)] + designPlan$followUpTime,
                            accrualTimeVector = accrualTime,
                            accrualIntensity = designPlan$accrualIntensity,
                            lambda2 = designPlan$lambda2, lambda1 = lambda1[i],
                            piecewiseSurvivalTime = piecewiseSurvivalTime,
                            phi = phi, kappa = kappa, allocationRatioPlanned = x,
                            hazardRatio = hazardRatio[i]
                        )
                    }
                    return(numberEvents / probEvent)
                }, interval = c(0, 5), tol = 0.0001)$minimum
                allocationRatioPlannedVec[i] <- allocationRatioPlanned
            }

            designPlan$eventsFixed[i] <- .getEventsFixed(
                typeOfComputation = typeOfComputation, twoSidedPower = twoSidedPower,
                alpha = alpha, beta = beta, sided = sided, hazardRatio = hazardRatio[i],
                thetaH0 = thetaH0, allocationRatioPlanned = allocationRatioPlanned
            )

            if (!accountForObservationTimes) {
                designPlan$chi[i] <- (allocationRatioPlanned * pi1[i] + pi2) /
                    (1 + allocationRatioPlanned)
            } else {
                designPlan$chi[i] <- .getEventProbabilities(
                    time = accrualTime[length(accrualTime)] + designPlan$followUpTime,
                    accrualTimeVector = accrualTime,
                    accrualIntensity = designPlan$accrualIntensity,
                    lambda2 = designPlan$lambda2,
                    lambda1 = lambda1[i],
                    piecewiseSurvivalTime = piecewiseSurvivalTime, phi = phi, kappa = kappa,
                    allocationRatioPlanned = allocationRatioPlanned, hazardRatio = hazardRatio[i]
                )
            }
            designPlan$.setParameterType("chi", C_PARAM_GENERATED)

            designPlan$nFixed[i] <- designPlan$eventsFixed[i] / designPlan$chi[i]
        } else {
            if (length(maxNumberOfSubjects) > 1) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "length of user defined 'maxNumberOfSubjects' (",
                    .arrayToString(maxNumberOfSubjects), ") must be 1"
                )
            }

            designPlan$.calculateFollowUpTime <- TRUE

            designPlan$eventsFixed[i] <- .getEventsFixed(
                typeOfComputation = typeOfComputation, twoSidedPower = twoSidedPower,
                alpha = alpha, beta = beta, sided = sided, hazardRatio = hazardRatio[i],
                thetaH0 = thetaH0, allocationRatioPlanned = allocationRatioPlanned
            )

            designPlan$nFixed[i] <- maxNumberOfSubjects
            if (designPlan$eventsFixed[i] > maxNumberOfSubjects) {
                if (length(hazardRatio) > 1) {
                    stop(
                        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                        sprintf(
                            paste0(
                                "'maxNumberOfSubjects' (%s) is smaller than the number ",
                                "of events (%.3f) at index %s (hazard ratio = %.3f)"
                            ),
                            maxNumberOfSubjects, designPlan$eventsFixed[i], i, hazardRatio[i]
                        )
                    )
                } else {
                    stop(
                        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                        sprintf(
                            paste0(
                                "'maxNumberOfSubjects' (%s) is smaller than the number ",
                                "of events (%.3f)"
                            ),
                            maxNumberOfSubjects, designPlan$eventsFixed[i]
                        )
                    )
                }
            }

            up <- 2
            iterate <- 1
            while (designPlan$eventsFixed[i] / .getEventProbabilities(
                time = up, accrualTimeVector = accrualTime, accrualIntensity = designPlan$accrualIntensity,
                lambda2 = designPlan$lambda2, lambda1 = lambda1[i],
                piecewiseSurvivalTime = piecewiseSurvivalTime, phi = phi, kappa = kappa,
                allocationRatioPlanned = allocationRatioPlanned,
                hazardRatio = hazardRatio[i]
            ) > maxNumberOfSubjects) {
                up <- 2 * up
                iterate <- iterate + 1
                if (iterate > 50) {
                    stop(
                        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                        "the number of subjects is too small to reach maximum number of events ",
                        "(presumably due to drop-out rates), search algorithm failed"
                    )
                }
            }

            timeVector[i] <- .getOneDimensionalRoot(
                function(x) {
                    designPlan$eventsFixed[i] / .getEventProbabilities(
                        time = x, accrualTimeVector = accrualTime,
                        accrualIntensity = designPlan$accrualIntensity, lambda2 = designPlan$lambda2,
                        lambda1 = lambda1[i],
                        piecewiseSurvivalTime = piecewiseSurvivalTime, phi = phi, kappa = kappa,
                        allocationRatioPlanned = allocationRatioPlanned,
                        hazardRatio = hazardRatio[i]
                    ) - maxNumberOfSubjects
                },
                lower = 0, upper = up, tolerance = 1e-06, acceptResultsOutOfTolerance = TRUE,
                callingFunctionInformation = ".getSampleSizeSequentialSurvival"
            )

            if (!is.na(timeVector[i])) {
                designPlan$chi[i] <- .getEventProbabilities(
                    time = timeVector[i],
                    accrualTimeVector = accrualTime,
                    accrualIntensity = designPlan$accrualIntensity, lambda2 = designPlan$lambda2,
                    lambda1 = lambda1[i],
                    piecewiseSurvivalTime = piecewiseSurvivalTime, phi = phi, kappa = kappa,
                    allocationRatioPlanned = allocationRatioPlanned, hazardRatio = hazardRatio[i]
                )
                designPlan$.setParameterType("chi", C_PARAM_GENERATED)
            }
        }
    }

    if (calculateAllocationRatioPlanned) {
        allocationRatioPlanned <- allocationRatioPlannedVec
        designPlan$allocationRatioPlanned <- allocationRatioPlanned
        designPlan$.setParameterType("allocationRatioPlanned", C_PARAM_GENERATED)
    }

    if (userDefinedMaxNumberOfSubjects) {
        designPlan$followUpTime <- timeVector - accrualTime[length(accrualTime)]
        designPlan$.setParameterType("followUpTime", C_PARAM_GENERATED)
    }

    designPlan$nFixed2 <- designPlan$nFixed / (1 + allocationRatioPlanned)
    designPlan$nFixed1 <- designPlan$nFixed2 * allocationRatioPlanned

    designPlan$numberOfSubjects1 <- matrix(designPlan$nFixed1, nrow = 1)
    designPlan$numberOfSubjects2 <- matrix(designPlan$nFixed2, nrow = 1)

    if (!designPlan$.piecewiseSurvivalTime$piecewiseSurvivalEnabled) {
        eventRatio <- allocationRatioPlanned * pi1 / pi2
    } else {
        eventRatio <- NA_real_
    }

    # Fixed
    designPlan$hazardRatio <- hazardRatio
    designPlan$expectedEventsH1 <- designPlan$eventsFixed
    designPlan$maxNumberOfSubjects <- designPlan$nFixed
    designPlan$numberOfSubjects <- matrix(designPlan$nFixed, nrow = 1)

    designPlan$.setParameterType("eventsFixed", C_PARAM_GENERATED)
    designPlan$.setParameterType("nFixed1", C_PARAM_GENERATED)
    designPlan$.setParameterType("nFixed2", C_PARAM_GENERATED)
    designPlan$.setParameterType("nFixed", C_PARAM_GENERATED)

    if (designPlan$accountForObservationTimes) {
        designPlan$analysisTime <- matrix(accrualTime[length(accrualTime)] +
            designPlan$followUpTime, nrow = 1)
        designPlan$.setParameterType("analysisTime", C_PARAM_GENERATED)
    }
    return(designPlan)
}

# note that fixed sample size must be calculated before on 'designPlan'
.getSampleSizeSequentialSurvival <- function(designPlan, designCharacteristics) {
    if (designPlan$.piecewiseSurvivalTime$.isLambdaBased()) {
        numberOfResults <- length(designPlan$hazardRatio)
    } else {
        numberOfResults <- length(designPlan$pi1)
    }

    kMax <- designCharacteristics$.design$kMax
    designPlan$cumulativeEventsPerStage <- matrix(NA_real_, kMax, numberOfResults)
    analysisTime <- matrix(NA_real_, kMax, numberOfResults)
    numberOfSubjects <- matrix(NA_real_, kMax, numberOfResults)
    designPlan$expectedEventsH0 <- rep(NA_real_, numberOfResults)
    designPlan$expectedEventsH01 <- rep(NA_real_, numberOfResults)
    designPlan$expectedEventsH1 <- rep(NA_real_, numberOfResults)
    expectedNumberOfSubjectsH1 <- rep(NA_real_, numberOfResults)
    studyDuration <- rep(NA_real_, numberOfResults)
    designPlan$chi <- rep(NA_real_, numberOfResults)

    informationRates <- designCharacteristics$information / designCharacteristics$shift

    lambda1 <- designPlan$lambda1
    if (designPlan$.piecewiseSurvivalTime$piecewiseSurvivalEnabled) {
        lambda1 <- rep(NA_real_, numberOfResults)
    }

    if (designPlan$accountForObservationTimes && designPlan$.calculateFollowUpTime) {
        designPlan$followUpTime <- rep(NA_real_, numberOfResults)
    }

    for (i in 1:numberOfResults) {
        designPlan$cumulativeEventsPerStage[, i] <- designPlan$eventsFixed[i] * informationRates *
            designCharacteristics$inflationFactor

        if (!designPlan$accountForObservationTimes) {
            if (length(designPlan$allocationRatioPlanned) > 1) {
                allocationRatioPlanned <- designPlan$allocationRatioPlanned[i]
            } else {
                allocationRatioPlanned <- designPlan$allocationRatioPlanned
            }
            designPlan$chi[i] <- (allocationRatioPlanned * designPlan$pi1[i] + designPlan$pi2) /
                (1 + allocationRatioPlanned)
            designPlan$.setParameterType("chi", C_PARAM_GENERATED)
            numberOfSubjects[kMax, i] <- designPlan$cumulativeEventsPerStage[kMax, i] / designPlan$chi[i]
        } else {
            phi <- -c(log(1 - designPlan$dropoutRate1), log(1 - designPlan$dropoutRate2)) /
                designPlan$dropoutTime

            if (designPlan$.calculateFollowUpTime) {
                if (designPlan$cumulativeEventsPerStage[kMax, i] > designPlan$maxNumberOfSubjects[i]) {
                    stop(
                        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                        sprintf(
                            paste0(
                                "the number of subjects (%s) is smaller than the number ",
                                "of events (%s) at stage %s"
                            ),
                            designPlan$maxNumberOfSubjects[i],
                            designPlan$cumulativeEventsPerStage[kMax, i], i
                        )
                    )
                }

                up <- 2
                iterate <- 1
                while (designPlan$cumulativeEventsPerStage[kMax, i] / .getEventProbabilities(
                    time = up,
                    accrualTimeVector = designPlan$accrualTime,
                    accrualIntensity = designPlan$accrualIntensity,
                    lambda2 = designPlan$lambda2,
                    lambda1 = lambda1[i],
                    piecewiseSurvivalTime = designPlan$piecewiseSurvivalTime,
                    phi = phi, kappa = designPlan$kappa,
                    allocationRatioPlanned = designPlan$allocationRatioPlanned,
                    hazardRatio = designPlan$hazardRatio[i]
                ) > designPlan$maxNumberOfSubjects[i]) {
                    up <- 2 * up
                    iterate <- iterate + 1
                    if (iterate > 50) {
                        stop(
                            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                            "the number of subjects is too small to reach maximum number of events ",
                            "(presumably due to drop-out rates)"
                        )
                    }
                }

                totalTime <- .getOneDimensionalRoot(
                    function(x) {
                        designPlan$cumulativeEventsPerStage[kMax, i] / designPlan$maxNumberOfSubjects[i] -
                            .getEventProbabilities(
                                time = x, accrualTimeVector = designPlan$accrualTime,
                                accrualIntensity = designPlan$accrualIntensity,
                                lambda2 = designPlan$lambda2,
                                lambda1 = lambda1[i],
                                piecewiseSurvivalTime = designPlan$piecewiseSurvivalTime,
                                phi = phi, kappa = designPlan$kappa,
                                allocationRatioPlanned = designPlan$allocationRatioPlanned,
                                hazardRatio = designPlan$hazardRatio[i]
                            )
                    },
                    lower = 0, upper = up, tolerance = 1e-06,
                    callingFunctionInformation = ".getSampleSizeSequentialSurvival"
                )

                # analysis times
                for (j in 1:kMax) {
                    analysisTime[j, i] <- .getOneDimensionalRoot(
                        function(x) {
                            designPlan$cumulativeEventsPerStage[j, i] / designPlan$maxNumberOfSubjects[i] -
                                .getEventProbabilities(
                                    time = x, accrualTimeVector = designPlan$accrualTime,
                                    accrualIntensity = designPlan$accrualIntensity,
                                    lambda2 = designPlan$lambda2,
                                    lambda1 = lambda1[i],
                                    piecewiseSurvivalTime = designPlan$piecewiseSurvivalTime,
                                    phi = phi, kappa = designPlan$kappa,
                                    allocationRatioPlanned = designPlan$allocationRatioPlanned,
                                    hazardRatio = designPlan$hazardRatio[i]
                                )
                        },
                        lower = 0, upper = totalTime, tolerance = 1e-06, acceptResultsOutOfTolerance = TRUE,
                        callingFunctionInformation = ".getSampleSizeSequentialSurvival"
                    )
                }
                analysisTime[kMax, i] <- totalTime

                designPlan$followUpTime[i] <- totalTime -
                    designPlan$accrualTime[length(designPlan$accrualTime)]

                numberOfSubjects[, i] <- .getNumberOfSubjects(
                    time = analysisTime[, i],
                    accrualTime = designPlan$accrualTime,
                    accrualIntensity = designPlan$accrualIntensity,
                    maxNumberOfSubjects = designPlan$maxNumberOfSubjects[i]
                )
            } else {
                if (length(designPlan$allocationRatioPlanned) > 1) {
                    allocationRatioPlanned <- designPlan$allocationRatioPlanned[i]
                } else {
                    allocationRatioPlanned <- designPlan$allocationRatioPlanned
                }

                if (is.na(designPlan$followUpTime)) {
                    stop(
                        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                        "'followUpTime' must be defined because 'designPlan$.calculateFollowUpTime' = FALSE"
                    )
                }

                designPlan$chi[i] <- .getEventProbabilities(
                    time = designPlan$accrualTime[length(designPlan$accrualTime)] + designPlan$followUpTime,
                    accrualTimeVector = designPlan$accrualTime,
                    accrualIntensity = designPlan$accrualIntensity,
                    lambda2 = designPlan$lambda2,
                    lambda1 = lambda1[i],
                    piecewiseSurvivalTime = designPlan$piecewiseSurvivalTime,
                    phi = phi, kappa = designPlan$kappa,
                    allocationRatioPlanned = allocationRatioPlanned,
                    hazardRatio = designPlan$hazardRatio[i]
                )
                designPlan$.setParameterType("chi", C_PARAM_GENERATED)
                numberOfSubjects[kMax, i] <- designPlan$cumulativeEventsPerStage[kMax, i] / designPlan$chi[i]

                #   Analysis times
                for (j in 1:(kMax - 1)) {
                    analysisTime[j, i] <- .getOneDimensionalRoot(
                        function(x) {
                            designPlan$cumulativeEventsPerStage[j, i] / numberOfSubjects[kMax, i] -
                                .getEventProbabilities(
                                    time = x, accrualTimeVector = designPlan$accrualTime,
                                    accrualIntensity = designPlan$accrualIntensity,
                                    lambda2 = designPlan$lambda2,
                                    lambda1 = lambda1[i],
                                    piecewiseSurvivalTime = designPlan$piecewiseSurvivalTime,
                                    phi = phi, kappa = designPlan$kappa,
                                    allocationRatioPlanned = allocationRatioPlanned,
                                    hazardRatio = designPlan$hazardRatio[i]
                                )
                        },
                        lower = 0, upper = designPlan$accrualTime[length(designPlan$accrualTime)] +
                            designPlan$followUpTime, tolerance = 1e-06,
                        callingFunctionInformation = ".getSampleSizeSequentialSurvival"
                    )
                }
                analysisTime[kMax, i] <- designPlan$accrualTime[length(designPlan$accrualTime)] +
                    designPlan$followUpTime

                numberOfSubjects[, i] <- .getNumberOfSubjects(
                    time = analysisTime[, i],
                    accrualTime = designPlan$accrualTime, accrualIntensity = designPlan$accrualIntensity,
                    maxNumberOfSubjects = numberOfSubjects[kMax, i]
                )
            }

            stoppingProbs <- designCharacteristics$rejectionProbabilities +
                c(designCharacteristics$futilityProbabilities, 0)

            if (all(is.na(designCharacteristics$futilityProbabilities))) {
                warning("Expected number of subjects H1 and study duration H1 ",
                    "cannot be calculated because the futility probabilities ",
                    "are not applicable for the specified design",
                    call. = FALSE
                )
            }

            stoppingProbs[kMax] <- 1 - sum(stoppingProbs[1:(kMax - 1)])
            
            studyDuration[i] <- analysisTime[, i] %*% stoppingProbs

            expectedNumberOfSubjectsH1[i] <- numberOfSubjects[, i] %*% stoppingProbs
        }

        designPlan$expectedEventsH0[i] <- designCharacteristics$averageSampleNumber0 *
            designPlan$eventsFixed[i]
        designPlan$expectedEventsH01[i] <- designCharacteristics$averageSampleNumber01 *
            designPlan$eventsFixed[i]
        designPlan$expectedEventsH1[i] <- designCharacteristics$averageSampleNumber1 *
            designPlan$eventsFixed[i]
        designPlan$.setParameterType("expectedEventsH0", C_PARAM_GENERATED)
        designPlan$.setParameterType("expectedEventsH01", C_PARAM_GENERATED)
        designPlan$.setParameterType("expectedEventsH1", C_PARAM_GENERATED)

        designPlan$numberOfSubjects2 <- numberOfSubjects / (1 + designPlan$allocationRatioPlanned)
        designPlan$numberOfSubjects1 <- designPlan$numberOfSubjects2 *
            designPlan$allocationRatioPlanned
        designPlan$.setParameterType("numberOfSubjects1", C_PARAM_GENERATED)
        designPlan$.setParameterType("numberOfSubjects2", C_PARAM_GENERATED)
    }

    if (!is.null(designCharacteristics$rejectionProbabilities)) {
        designPlan$rejectPerStage <- matrix(designCharacteristics$rejectionProbabilities,
            nrow = designPlan$.design$kMax
        )
        designPlan$.setParameterType("rejectPerStage", C_PARAM_GENERATED)

        designPlan$earlyStop <- sum(designPlan$rejectPerStage[1:(designPlan$.design$kMax - 1), ])
        designPlan$.setParameterType("earlyStop", C_PARAM_GENERATED)
    }

    if (!is.null(designCharacteristics$futilityProbabilities) &&
            any(designPlan$.design$futilityBounds != C_FUTILITY_BOUNDS_DEFAULT)) {
        designPlan$futilityPerStage <- matrix(designCharacteristics$futilityProbabilities,
            nrow = designPlan$.design$kMax - 1
        )
        designPlan$.setParameterType("futilityPerStage", C_PARAM_GENERATED)

        designPlan$futilityStop <- sum(designPlan$futilityPerStage)
        designPlan$.setParameterType("futilityStop", C_PARAM_GENERATED)

        designPlan$earlyStop <- designPlan$earlyStop + sum(designPlan$futilityPerStage)
    }

    designPlan$informationRates <- matrix(informationRates, ncol = 1)
    if (!is.matrix(numberOfSubjects)) {
        designPlan$numberOfSubjects <- matrix(numberOfSubjects[kMax, ], nrow = 1)
    } else {
        designPlan$numberOfSubjects <- numberOfSubjects
    }

    designPlan$maxNumberOfSubjects <- designPlan$numberOfSubjects[kMax, ]
    if (designPlan$.getParameterType("maxNumberOfSubjects") == C_PARAM_NOT_APPLICABLE ||
            length(designPlan$maxNumberOfSubjects) > 1) {
        designPlan$.setParameterType("maxNumberOfSubjects", C_PARAM_GENERATED)
    }

    designPlan$maxNumberOfSubjects1 <- .getNumberOfSubjects1(
        designPlan$maxNumberOfSubjects, designPlan$allocationRatioPlanned
    )
    designPlan$maxNumberOfSubjects2 <- .getNumberOfSubjects2(
        designPlan$maxNumberOfSubjects, designPlan$allocationRatioPlanned
    )
    designPlan$.setParameterType("maxNumberOfSubjects1", C_PARAM_GENERATED)
    designPlan$.setParameterType("maxNumberOfSubjects2", C_PARAM_GENERATED)

    if (ncol(designPlan$informationRates) == 1 &&
            identical(designPlan$informationRates[, 1], designPlan$.design$informationRates)) {
        designPlan$.setParameterType("informationRates", C_PARAM_NOT_APPLICABLE)
    } else {
        designPlan$.setParameterType("informationRates", C_PARAM_GENERATED)
    }
    designPlan$.setParameterType("numberOfSubjects", C_PARAM_GENERATED)
    designPlan$.setParameterType("cumulativeEventsPerStage", C_PARAM_GENERATED)

    if (designPlan$accountForObservationTimes) {
        designPlan$analysisTime <- analysisTime
        designPlan$expectedNumberOfSubjectsH1 <- expectedNumberOfSubjectsH1
        designPlan$studyDuration <- studyDuration
        designPlan$studyDurationH1 <- studyDuration # deprecated

        designPlan$.setParameterType("analysisTime", C_PARAM_GENERATED)
        designPlan$.setParameterType("expectedNumberOfSubjectsH1", C_PARAM_GENERATED)
        designPlan$.setParameterType("studyDuration", C_PARAM_GENERATED)
    }

    designPlan$.setParameterType("eventsFixed", C_PARAM_NOT_APPLICABLE)
    designPlan$.setParameterType("nFixed1", C_PARAM_NOT_APPLICABLE)
    designPlan$.setParameterType("nFixed2", C_PARAM_NOT_APPLICABLE)
    designPlan$.setParameterType("nFixed", C_PARAM_NOT_APPLICABLE)

    if (all(designPlan$allocationRatioPlanned == 1)) {
        designPlan$.setParameterType("numberOfSubjects1", C_PARAM_NOT_APPLICABLE)
        designPlan$.setParameterType("numberOfSubjects2", C_PARAM_NOT_APPLICABLE)
    }

    designPlan$.calculateFollowUpTime <- NA

    return(designPlan)
}

.getNumberOfSubjects <- function(..., time, accrualTime, accrualIntensity, maxNumberOfSubjects) {
    subjectNumbers <- c()
    for (timeValue in time) {
        if (is.na(timeValue)) {
            return(NA_real_)
        }

        subjectNumbers <- c(
            subjectNumbers,
            .getNumberOfSubjectsInner(
                timeValue = timeValue, accrualTime = accrualTime,
                accrualIntensity = accrualIntensity, maxNumberOfSubjects = maxNumberOfSubjects
            )
        )
    }
    return(subjectNumbers)
}

#' @title
#' Get Event Probabilities
#'
#' @description
#' Returns the event probabilities for specified parameters at given time vector.
#'
#' @param time A numeric vector with time values.
#' @inheritParams param_lambda1
#' @inheritParams param_lambda2
#' @inheritParams param_piecewiseSurvivalTime
#' @inheritParams param_hazardRatio
#' @inheritParams param_kappa
#' @inheritParams param_allocationRatioPlanned_sampleSize
#' @inheritParams param_accrualTime
#' @inheritParams param_accrualIntensity
#' @inheritParams param_accrualIntensityType
#' @inheritParams param_dropoutRate1
#' @inheritParams param_dropoutRate2
#' @inheritParams param_dropoutTime
#' @param maxNumberOfSubjects If \code{maxNumberOfSubjects > 0} is specified,
#'        the end of accrual at specified \code{accrualIntensity} for the specified
#'        number of subjects is determined or \code{accrualIntensity} is calculated
#'        at fixed end of accrual.
#' @inheritParams param_three_dots
#'
#' @details
#' The function computes the overall event probabilities in a two treatment groups design.
#' For details of the parameters see \code{\link[=getSampleSizeSurvival]{getSampleSizeSurvival()}}.
#'
#' @return Returns a \code{\link{EventProbabilities}} object.
#' The following generics (R generic functions) are available for this result object:
#' \itemize{
#'   \item \code{\link[=names.FieldSet]{names()}} to obtain the field names,
#'   \item \code{\link[=print.FieldSet]{print()}} to print the object,
#'   \item \code{\link[=summary.ParameterSet]{summary()}} to display a summary of the object,
#'   \item \code{\link[=plot.EventProbabilities]{plot()}} to plot the object,
#'   \item \code{\link[=as.data.frame.ParameterSet]{as.data.frame()}} to coerce the object to a \code{\link[base]{data.frame}},
#'   \item \code{\link[=as.matrix.FieldSet]{as.matrix()}} to coerce the object to a \code{\link[base]{matrix}}.
#' }
#' @template how_to_get_help_for_generics
#'
#' @template examples_get_event_probabilities
#'
#' @export
#'
getEventProbabilities <- function(time, ...,
        accrualTime = c(0, 12), # C_ACCRUAL_TIME_DEFAULT
        accrualIntensity = 0.1, # C_ACCRUAL_INTENSITY_DEFAULT
        accrualIntensityType = c("auto", "absolute", "relative"),
        kappa = 1,
        piecewiseSurvivalTime = NA_real_,
        lambda2 = NA_real_,
        lambda1 = NA_real_,
        allocationRatioPlanned = 1,
        hazardRatio = NA_real_,
        dropoutRate1 = 0, # C_DROP_OUT_RATE_1_DEFAULT
        dropoutRate2 = 0, # C_DROP_OUT_RATE_2_DEFAULT
        dropoutTime = 12, # C_DROP_OUT_TIME_DEFAULT
        maxNumberOfSubjects = NA_real_) {
    .warnInCaseOfUnknownArguments(functionName = "getEventProbabilities", ...)

    .assertIsNumericVector(time, "time")
    .assertIsValidMaxNumberOfSubjects(maxNumberOfSubjects, naAllowed = TRUE)
    .assertIsValidAllocationRatioPlannedSampleSize(allocationRatioPlanned, maxNumberOfSubjects)
    .assertIsValidKappa(kappa)
    .assertIsSingleNumber(hazardRatio, "hazardRatio", naAllowed = TRUE)

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

    accrualSetup <- getAccrualTime(
        accrualTime = accrualTime,
        accrualIntensity = accrualIntensity,
        accrualIntensityType = accrualIntensityType,
        maxNumberOfSubjects = maxNumberOfSubjects
    )
    accrualTime <- accrualSetup$.getAccrualTimeWithoutLeadingZero()
    accrualIntensity <- accrualSetup$accrualIntensity
    maxNumberOfSubjects <- accrualSetup$maxNumberOfSubjects

    setting <- getPiecewiseSurvivalTime(
        piecewiseSurvivalTime = piecewiseSurvivalTime,
        lambda2 = lambda2, lambda1 = lambda1,
        hazardRatio = hazardRatio, kappa = kappa,
        delayedResponseAllowed = TRUE,
        .lambdaBased = TRUE
    )

    if (!setting$delayedResponseEnabled && length(setting$lambda1) > 1 &&
            setting$.getParameterType("lambda1") == C_PARAM_USER_DEFINED) {
        warning("Only the first 'lambda1' (", lambda1[1], ") was used to calculate event probabilities", call. = FALSE)
        setting <- getPiecewiseSurvivalTime(
            piecewiseSurvivalTime = piecewiseSurvivalTime,
            lambda2 = lambda2, lambda1 = lambda1[1],
            hazardRatio = hazardRatio, kappa = kappa,
            delayedResponseAllowed = TRUE,
            .lambdaBased = TRUE
        )
    }

    piecewiseSurvivalTime <- setting$piecewiseSurvivalTime
    lambda2 <- setting$lambda2
    lambda1 <- setting$lambda1
    hazardRatio <- setting$hazardRatio

    phi <- -log(1 - c(dropoutRate1, dropoutRate2)) / dropoutTime

    if (length(accrualTime) != length(accrualIntensity)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "length of 'accrualTime' (", (length(accrualTime) + 1),
            ") must be equal to length of 'accrualIntensity' (", length(accrualIntensity), ")"
        )
    }

    if (any(accrualIntensity <= 0)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all values of 'accrualIntensity' must be > 0")
    }

    if (any(accrualTime <= 0)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all values of 'accrualTime' must be > 0")
    }

    if (kappa != 1 && any(phi > 0)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "for Weibull distribution (kappa != 1) drop-out rates (phi) cannot be specified"
        )
    }

    if (any(phi < 0)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all drop-out rates (phi) must be >= 0")
    }

    .assertIsNumericVector(lambda2, "lambda2")
    if (any(lambda2 <= 0)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all rates (lambda2) must be > 0")
    }

    eventProbabilities <- EventProbabilities$new(
        .piecewiseSurvivalTime = setting,
        .accrualTime           = accrualSetup,
        time                   = time,
        accrualTime            = accrualTime,
        accrualIntensity       = accrualIntensity,
        kappa                  = kappa,
        piecewiseSurvivalTime  = piecewiseSurvivalTime,
        lambda1                = lambda1,
        lambda2                = lambda2,
        allocationRatioPlanned = allocationRatioPlanned,
        hazardRatio            = hazardRatio,
        dropoutRate1           = dropoutRate1,
        dropoutRate2           = dropoutRate2,
        dropoutTime            = dropoutTime,
        maxNumberOfSubjects    = maxNumberOfSubjects
    )

    eventProbabilities$.setParameterType("time", C_PARAM_USER_DEFINED)
    eventProbabilities$.setParameterType(
        "accrualTime",
        accrualSetup$.getParameterType("accrualTime")
    )
    eventProbabilities$.setParameterType(
        "accrualIntensity",
        accrualSetup$.getParameterType("accrualIntensity")
    )
    eventProbabilities$.setParameterType("kappa", setting$.getParameterType("kappa"))
    eventProbabilities$.setParameterType(
        "piecewiseSurvivalTime",
        setting$.getParameterType("piecewiseSurvivalTime")
    )
    eventProbabilities$.setParameterType("lambda1", setting$.getParameterType("lambda1"))
    eventProbabilities$.setParameterType("lambda2", setting$.getParameterType("lambda2"))
    .setValueAndParameterType(eventProbabilities, "allocationRatioPlanned", allocationRatioPlanned, 1)
    eventProbabilities$.setParameterType("hazardRatio", setting$.getParameterType("hazardRatio"))
    .setValueAndParameterType(eventProbabilities, "dropoutRate1", dropoutRate1, C_DROP_OUT_RATE_1_DEFAULT)
    .setValueAndParameterType(eventProbabilities, "dropoutRate2", dropoutRate2, C_DROP_OUT_RATE_2_DEFAULT)
    .setValueAndParameterType(eventProbabilities, "dropoutTime", dropoutTime, C_DROP_OUT_TIME_DEFAULT)
    eventProbabilities$.setParameterType(
        "maxNumberOfSubjects",
        accrualSetup$.getParameterType("maxNumberOfSubjects")
    )

    eventProbabilities$cumulativeEventProbabilities <- numeric(0)
    eventProbabilities$overallEventProbabilities <- numeric(0) # deprecated
    eventProbabilities$eventProbabilities1 <- numeric(0)
    eventProbabilities$eventProbabilities2 <- numeric(0)

    for (timeValue in time) {
        eventProbs <- .getEventProbabilitiesGroupwise(
            time = timeValue,
            accrualTimeVector = accrualSetup$.getAccrualTimeWithoutLeadingZero(),
            accrualIntensity = accrualSetup$accrualIntensity, lambda2 = lambda2,
            lambda1 = lambda1, piecewiseSurvivalTime = piecewiseSurvivalTime, phi = phi,
            kappa = kappa, allocationRatioPlanned = allocationRatioPlanned, hazardRatio = hazardRatio
        )

        eventProbabilities$cumulativeEventProbabilities <- c(
            eventProbabilities$cumulativeEventProbabilities,
            .getEventProbabilitiesOverall(eventProbs, allocationRatioPlanned)
        )

        eventProbabilities$overallEventProbabilities <-
            eventProbabilities$cumulativeEventProbabilities # deprecated

        eventProbabilities$eventProbabilities1 <- c(
            eventProbabilities$eventProbabilities1,
            eventProbs[1]
        )
        eventProbabilities$eventProbabilities2 <- c(
            eventProbabilities$eventProbabilities2,
            eventProbs[2]
        )
    }

    eventProbabilities$.setParameterType("cumulativeEventProbabilities", C_PARAM_GENERATED)
    eventProbabilities$.setParameterType("eventProbabilities1", C_PARAM_GENERATED)
    eventProbabilities$.setParameterType("eventProbabilities2", C_PARAM_GENERATED)

    return(eventProbabilities)
}

#' @title
#' Get Number Of Subjects
#'
#' @description
#' Returns the number of recruited subjects at given time vector.
#'
#' @param time A numeric vector with time values.
#' @inheritParams param_accrualTime
#' @inheritParams param_accrualIntensity
#' @inheritParams param_accrualIntensityType
#' @param maxNumberOfSubjects If \code{maxNumberOfSubjects > 0} is specified,
#'        the end of accrual at specified \code{accrualIntensity} for the specified number of
#'        subjects is determined or \code{accrualIntensity} is calculated at fixed end of accrual.
#' @inheritParams param_three_dots
#'
#' @details
#' Calculate number of subjects over time range at given accrual time vector
#' and accrual intensity. Intensity can either be defined in absolute or
#' relative terms (for the latter, \code{maxNumberOfSubjects} needs to be defined)\cr
#' The function is used by \code{\link[=getSampleSizeSurvival]{getSampleSizeSurvival()}}.
#'
#' @return Returns a \code{\link{NumberOfSubjects}} object.
#' The following generics (R generic functions) are available for this result object:
#' \itemize{
#'   \item \code{\link[=names.FieldSet]{names()}} to obtain the field names,
#'   \item \code{\link[=print.FieldSet]{print()}} to print the object,
#'   \item \code{\link[=summary.ParameterSet]{summary()}} to display a summary of the object,
#'   \item \code{\link[=plot.NumberOfSubjects]{plot()}} to plot the object,
#'   \item \code{\link[=as.data.frame.ParameterSet]{as.data.frame()}} to coerce the object to a \code{\link[base]{data.frame}},
#'   \item \code{\link[=as.matrix.FieldSet]{as.matrix()}} to coerce the object to a \code{\link[base]{matrix}}.
#' }
#' @template how_to_get_help_for_generics
#'
#' @seealso \code{\link{AccrualTime}} for defining the accrual time.
#'
#' @template examples_get_number_of_subjects
#'
#' @export
#'
getNumberOfSubjects <- function(time, ...,
        accrualTime = c(0, 12), # C_ACCRUAL_TIME_DEFAULT
        accrualIntensity = 0.1, # C_ACCRUAL_INTENSITY_DEFAULT
        accrualIntensityType = c("auto", "absolute", "relative"),
        maxNumberOfSubjects = NA_real_) {
    .warnInCaseOfUnknownArguments(functionName = "getNumberOfSubjects", ...)

    .assertIsNumericVector(time, "time")

    accrualSetup <- getAccrualTime(
        accrualTime = accrualTime,
        accrualIntensity = accrualIntensity,
        accrualIntensityType = accrualIntensityType,
        maxNumberOfSubjects = maxNumberOfSubjects
    )
    accrualTime <- accrualSetup$.getAccrualTimeWithoutLeadingZero()
    accrualIntensity <- accrualSetup$accrualIntensity
    maxNumberOfSubjects <- accrualSetup$maxNumberOfSubjects

    if (length(accrualTime) != length(accrualIntensity)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "length of 'accrualTime' (", length(accrualTime),
            ") must be equal to length of 'accrualIntensity' (", length(accrualIntensity), ")"
        )
    }

    if (any(accrualIntensity < 0)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all values of 'accrualIntensity' must be >= 0")
    }

    if (all(accrualIntensity < 1)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "at least one value of 'accrualIntensity' must be >= 1")
    }

    if (any(accrualTime <= 0)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all values of 'accrualTime' must be > 0")
    }

    numberOfSubjects <- .getNumberOfSubjects(
        time = time, accrualTime = accrualTime,
        accrualIntensity = accrualIntensity, maxNumberOfSubjects = maxNumberOfSubjects
    )

    result <- NumberOfSubjects$new(
        .accrualTime = accrualSetup,
        time = time,
        accrualTime = accrualTime,
        accrualIntensity = accrualIntensity,
        maxNumberOfSubjects = maxNumberOfSubjects,
        numberOfSubjects = numberOfSubjects
    )

    result$.setParameterType("time", C_PARAM_USER_DEFINED)
    result$.setParameterType("accrualTime", accrualSetup$.getParameterType("accrualTime"))
    result$.setParameterType("accrualIntensity", accrualSetup$.getParameterType("accrualIntensity"))
    result$.setParameterType("maxNumberOfSubjects", accrualSetup$.getParameterType("maxNumberOfSubjects"))
    result$.setParameterType("numberOfSubjects", C_PARAM_GENERATED)

    return(result)
}

#' @title
#' Get Sample Size Survival
#'
#' @description
#' Returns the sample size for testing the hazard ratio in a two treatment groups survival design.
#'
#' @inheritParams param_design_with_default
#' @inheritParams param_typeOfComputation
#' @inheritParams param_allocationRatioPlanned_sampleSize
#' @inheritParams param_thetaH0
#' @inheritParams param_lambda1
#' @inheritParams param_lambda2
#' @inheritParams param_pi1_survival
#' @inheritParams param_pi2_survival
#' @inheritParams param_median1
#' @inheritParams param_median2
#' @inheritParams param_piecewiseSurvivalTime
#' @inheritParams param_accrualTime
#' @inheritParams param_accrualIntensity
#' @inheritParams param_accrualIntensityType
#' @inheritParams param_eventTime
#' @inheritParams param_hazardRatio
#' @inheritParams param_kappa
#' @inheritParams param_dropoutRate1
#' @inheritParams param_dropoutRate2
#' @inheritParams param_dropoutTime
#' @param followUpTime The assumed (additional) follow-up time for the study, default is \code{6}.
#'        The total study duration is \code{accrualTime + followUpTime}.
#' @param maxNumberOfSubjects If \code{maxNumberOfSubjects > 0} is specified,
#'        the follow-up time for the required number of events is determined.
#' @inheritParams param_three_dots
#'
#' @details
#' At given design the function calculates the number of events and an estimate for the
#' necessary number of subjects for testing the hazard ratio in a survival design.
#' It also calculates the time when the required events are expected under the given
#' assumptions (exponentially, piecewise exponentially, or Weibull distributed survival times
#' and constant or non-constant piecewise accrual).
#' Additionally, an allocation ratio = \code{n1 / n2} can be specified where \code{n1} and \code{n2} are the number
#' of subjects in the two treatment groups.
#'
#' Optional argument \code{accountForObservationTimes}: if \code{accountForObservationTimes = TRUE}, the number of
#' subjects is calculated assuming specific accrual and follow-up time, default is \code{TRUE}.
#'
#' The formula of Kim & Tsiatis (Biometrics, 1990)
#' is used to calculate the expected number of events under the alternative
#' (see also Lakatos & Lan, Statistics in Medicine, 1992). These formulas are generalized
#' to piecewise survival times and non-constant piecewise accrual over time.\cr
#'
#' Optional argument \code{accountForObservationTimes}: if \code{accountForObservationTimes = FALSE},
#' only the event rates are used for the calculation of the maximum number of subjects.
#'
#' @template details_piecewise_survival
#'
#' @template details_piecewise_accrual
#'
#' @template return_object_trial_design_plan
#' @template how_to_get_help_for_generics
#'
#' @family sample size functions
#'
#' @template examples_get_sample_size_survival
#'
#' @export
#'
getSampleSizeSurvival <- function(design = NULL, ...,
        typeOfComputation = c("Schoenfeld", "Freedman", "HsiehFreedman"),
        thetaH0 = 1, # C_THETA_H0_SURVIVAL_DEFAULT
        pi1 = NA_real_,
        pi2 = NA_real_,
        lambda1 = NA_real_,
        lambda2 = NA_real_,
        median1 = NA_real_,
        median2 = NA_real_,
        kappa = 1,
        hazardRatio = NA_real_,
        piecewiseSurvivalTime = NA_real_,
        allocationRatioPlanned = NA_real_, # C_ALLOCATION_RATIO_DEFAULT
        eventTime = 12, # C_EVENT_TIME_DEFAULT
        accrualTime = c(0, 12), # C_ACCRUAL_TIME_DEFAULT
        accrualIntensity = 0.1, # C_ACCRUAL_INTENSITY_DEFAULT
        accrualIntensityType = c("auto", "absolute", "relative"),
        followUpTime = NA_real_,
        maxNumberOfSubjects = NA_real_,
        dropoutRate1 = 0, # C_DROP_OUT_RATE_1_DEFAULT
        dropoutRate2 = 0, # C_DROP_OUT_RATE_2_DEFAULT
        dropoutTime = 12 # C_DROP_OUT_TIME_DEFAULT
        ) {
    if (is.null(design)) {
        design <- .getDefaultDesign(..., type = "sampleSize", ignore = c("accountForObservationTimes"))
        .warnInCaseOfUnknownArguments(
            functionName = "getSampleSizeSurvival",
            ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
                design,
                powerCalculationEnabled = FALSE
            ), "accountForObservationTimes"), ...
        )
    } else {
        .assertIsTrialDesign(design)
        .warnInCaseOfUnknownArguments(
            functionName = "getSampleSizeSurvival", ...,
            ignore = c("accountForObservationTimes")
        )
        .warnInCaseOfTwoSidedPowerArgument(...)
        design <- .resetPipeOperatorQueue(design)
    }

    if (!is.na(maxNumberOfSubjects) && maxNumberOfSubjects == 0) {
        maxNumberOfSubjects <- NA_real_
    }

    # identify accrual time case
    accrualSetup <- getAccrualTime(
        accrualTime = accrualTime,
        accrualIntensity = accrualIntensity,
        accrualIntensityType = accrualIntensityType,
        maxNumberOfSubjects = maxNumberOfSubjects,
        showWarnings = FALSE
    )
    accrualSetup$.validate()

    accountForObservationTimes <- .getOptionalArgument("accountForObservationTimes", ...)
    if (is.null(accountForObservationTimes)) {
        accountForObservationTimes <- TRUE
    }

    if (!accrualSetup$maxNumberOfSubjectsCanBeCalculatedDirectly &&
            accrualSetup$followUpTimeMustBeUserDefined) {
        if (is.na(followUpTime)) {
            if (accrualSetup$piecewiseAccrualEnabled && !accrualSetup$endOfAccrualIsUserDefined) {
                if (length(accrualIntensity) + 1 < length(accrualTime)) {
                  stop(
                    C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                    "length of 'accrualTime' (", length(accrualTime), ") must be greater ",
                    "than length of 'accrualIntensity' (", length(accrualIntensity), ") + 1"
                  )
                }             
                stop(
                    C_EXCEPTION_TYPE_MISSING_ARGUMENT,
                    "'followUpTime', 'maxNumberOfSubjects' or end of accrual must be defined"
                )
            }

            stop(
                C_EXCEPTION_TYPE_MISSING_ARGUMENT,
                "'followUpTime' or 'maxNumberOfSubjects' must be defined"
            )
        }

        if (followUpTime == Inf) {
            followUpTime <- 1e12
        }

        if (!any(is.na(hazardRatio)) && !is.na(thetaH0)) {
            .assertIsValidHazardRatio(hazardRatio, thetaH0)
        }

        pwst <- getPiecewiseSurvivalTime(
            piecewiseSurvivalTime = piecewiseSurvivalTime,
            lambda1 = lambda1, lambda2 = lambda2,
            pi1 = pi1, pi2 = pi2,
            median1 = median1, median2 = median2,
            hazardRatio = hazardRatio, eventTime = eventTime, kappa = kappa,
            .silent = TRUE
        )
        paramName <- NULL
        if (!pwst$piecewiseSurvivalEnabled) {
            if (pwst$.getParameterType("pi1") == C_PARAM_USER_DEFINED ||
                    pwst$.getParameterType("pi1") == C_PARAM_DEFAULT_VALUE ||
                    pwst$.getParameterType("pi2") == C_PARAM_USER_DEFINED) {
                paramName <- "pi1"
            } else if (pwst$.getParameterType("lambda1") == C_PARAM_USER_DEFINED ||
                    pwst$.getParameterType("lambda2") == C_PARAM_USER_DEFINED) {
                paramName <- "lambda1"
            } else if (pwst$.getParameterType("hazardRatio") == C_PARAM_USER_DEFINED) {
                paramName <- "hazardRatio"
            } else if (pwst$.getParameterType("median1") == C_PARAM_USER_DEFINED ||
                    pwst$.getParameterType("median2") == C_PARAM_USER_DEFINED) {
                paramName <- "median1"
            }
        } else if (pwst$.getParameterType("hazardRatio") == C_PARAM_USER_DEFINED) {
            paramName <- "hazardRatio"
        }
        if (!is.null(paramName)) {
            paramValue <- pwst[[paramName]]
            if (!is.null(paramValue) && length(paramValue) > 1) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "the calculation of 'maxNumberOfSubjects' for given 'followUpTime' ",
                    "is only available for a single '", paramName, "'; ",
                    paramName, " = ", .arrayToString(
                        paramValue,
                        vectorLookAndFeelEnabled = TRUE
                    )
                )
            }
        }

        hr <- hazardRatio
        if (all(is.na(hazardRatio))) {
            hr <- pwst$hazardRatio
        }
        if (all(is.na(hazardRatio))) {
            .assertIsValidHazardRatio(hr, thetaH0)
        }

        maxNumberOfSubjectsTarget <- NA_real_
        withCallingHandlers(
            {
                # search for accrual time that provides a result
                at <- accrualSetup$accrualTime
                additionalAccrual <- 1
                searchAccrualTimeEnabled <- TRUE
                maxSearchIterations <- 50
                maxNumberOfSubjectsLower <- NA_real_
                maxNumberOfSubjectsLowerBefore <- 0
                sampleSize <- NULL
                expectionMessage <- NA_character_

                while (searchAccrualTimeEnabled && maxSearchIterations >= 0 &&
                    (is.na(maxNumberOfSubjectsLower) ||
                        maxNumberOfSubjectsLower < maxNumberOfSubjectsLowerBefore ||
                        maxNumberOfSubjectsLower < 1e8)) {
                    tryCatch(
                        {
                            maxNumberOfSubjectsLowerBefore <- ifelse(is.na(maxNumberOfSubjectsLower),
                                0, maxNumberOfSubjectsLower
                            )
                            maxNumberOfSubjectsLower <- getAccrualTime(
                                accrualTime = c(at, at[length(at)] + additionalAccrual),
                                accrualIntensity = accrualSetup$accrualIntensity,
                                accrualIntensityType = accrualIntensityType
                            )$maxNumberOfSubjects
                            additionalAccrual <- 2 * additionalAccrual
                            sampleSize <- .getSampleSizeSurvival(
                                design = design,
                                typeOfComputation = typeOfComputation,
                                thetaH0 = thetaH0, pi2 = pi2, pi1 = pi1,
                                allocationRatioPlanned = allocationRatioPlanned,
                                accountForObservationTimes = accountForObservationTimes,
                                eventTime = eventTime, accrualTime = accrualSetup$accrualTime,
                                accrualIntensity = accrualSetup$accrualIntensity, kappa = kappa,
                                piecewiseSurvivalTime = piecewiseSurvivalTime,
                                lambda2 = lambda2, lambda1 = lambda1, median1 = median1, median2 = median2,
                                followUpTime = NA_real_, maxNumberOfSubjects = maxNumberOfSubjectsLower,
                                dropoutRate1 = dropoutRate1, dropoutRate2 = dropoutRate2, dropoutTime = dropoutTime,
                                hazardRatio = hazardRatio
                            )
                            searchAccrualTimeEnabled <- FALSE
                        },
                        error = function(e) {
                            expectionMessage <<- e$message
                        }
                    )
                    maxSearchIterations <- maxSearchIterations - 1
                }

                if (is.null(sampleSize) || is.na(sampleSize$followUpTime)) {
                    if (!is.na(expectionMessage) && grepl("'allocationRatioPlanned' > 0", expectionMessage)) {
                        stop(expectionMessage, call. = FALSE)
                    }
                    stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE,
                        "'additionalAccrual' could not be found, change accrual time specification",
                        call. = FALSE
                    )
                }

                # define lower bound for maxNumberOfSubjects
                maxNumberOfSubjectsLower <- ceiling(max(na.omit(c(
                    sampleSize$eventsFixed,
                    as.vector(sampleSize$cumulativeEventsPerStage)
                ))))
                if (is.na(maxNumberOfSubjectsLower)) {
                    stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                        "'maxNumberOfSubjectsLower' could not be found",
                        call. = FALSE
                    )
                }

                # check whether accrual time already fulfills requirement
                # (followUpTime < given value) or need to be increased,
                # then define upper bound for maxNumberOfSubjects
                maxSearchIterations <- 50
                maxNumberOfSubjectsUpper <- NA_real_
                fut <- sampleSize$followUpTime
                iterations <- 1
                while (fut <= followUpTime) {
                    fut <- 2 * abs(fut)
                    iterations <- iterations + 1
                    if (iterations > 50) {
                        stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE,
                            "search algorithm failed to end",
                            call. = FALSE
                        )
                    }
                }
                while (!is.na(fut) && fut > followUpTime && maxSearchIterations >= 0) {
                    maxNumberOfSubjectsUpper <- getAccrualTime(
                        accrualTime = c(at, at[length(at)] + additionalAccrual),
                        accrualIntensity = accrualSetup$accrualIntensity,
                        accrualIntensityType = accrualIntensityType
                    )$maxNumberOfSubjects
                    additionalAccrual <- 2 * additionalAccrual
                    fut <- .getSampleSizeSurvival(
                        design = design,
                        typeOfComputation = typeOfComputation,
                        thetaH0 = thetaH0, pi2 = pi2, pi1 = pi1,
                        allocationRatioPlanned = allocationRatioPlanned,
                        accountForObservationTimes = accountForObservationTimes,
                        eventTime = eventTime, accrualTime = accrualSetup$accrualTime,
                        accrualIntensity = accrualSetup$accrualIntensity, kappa = kappa,
                        piecewiseSurvivalTime = piecewiseSurvivalTime,
                        lambda2 = lambda2, lambda1 = lambda1, median1 = median1, median2 = median2,
                        followUpTime = NA_real_, maxNumberOfSubjects = maxNumberOfSubjectsUpper,
                        dropoutRate1 = dropoutRate1, dropoutRate2 = dropoutRate2, dropoutTime = dropoutTime,
                        hazardRatio = hazardRatio
                    )$followUpTime
                    maxSearchIterations <- maxSearchIterations - 1
                }
                if (is.na(maxNumberOfSubjectsUpper)) {
                    stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE,
                        "'maxNumberOfSubjectsUpper' could not be found ",
                        "(fut = ", fut, ", followUpTime = ", followUpTime, ")",
                        call. = FALSE
                    )
                }

                # use maxNumberOfSubjectsLower and maxNumberOfSubjectsUpper to find end of accrual
                if (dropoutRate1 != 0 || dropoutRate2 != 0) {
                    #  Adjust lower bound for given dropouts assuming exponential distribution
                    if (is.na(allocationRatioPlanned)) {
                        allocationRatioPlanned <- C_ALLOCATION_RATIO_DEFAULT
                    }

                    maxNumberOfSubjectsLower <- maxNumberOfSubjectsLower /
                        ((allocationRatioPlanned * (1 - dropoutRate1)^(
                            accrualSetup$accrualTime[length(accrualSetup$accrualTime)] / dropoutTime) +
                            (1 - dropoutRate2)^(accrualSetup$accrualTime[length(accrualSetup$accrualTime)] / dropoutTime)) /
                            (allocationRatioPlanned + 1))

                    prec <- 1
                    maxSearchIterations <- 50
                    while (prec > 1e-04 && maxSearchIterations >= 0) {
                        maxNumberOfSubjectsTarget <- (maxNumberOfSubjectsLower + maxNumberOfSubjectsUpper) / 2
                        fut <- .getSampleSizeSurvival(
                            design = design,
                            typeOfComputation = typeOfComputation,
                            thetaH0 = thetaH0, pi2 = pi2, pi1 = pi1,
                            allocationRatioPlanned = allocationRatioPlanned,
                            accountForObservationTimes = accountForObservationTimes,
                            eventTime = eventTime, accrualTime = accrualSetup$accrualTime,
                            accrualIntensity = accrualSetup$accrualIntensity, kappa = kappa,
                            piecewiseSurvivalTime = piecewiseSurvivalTime,
                            lambda2 = lambda2, lambda1 = lambda1,
                            median1 = median1, median2 = median2,
                            followUpTime = NA_real_, maxNumberOfSubjects = maxNumberOfSubjectsTarget,
                            dropoutRate1 = dropoutRate1, dropoutRate2 = dropoutRate2,
                            dropoutTime = dropoutTime,
                            hazardRatio = hazardRatio,
                            userFunctionCallEnabled = FALSE
                        )$followUpTime
                        ifelse(fut <= followUpTime,
                            maxNumberOfSubjectsUpper <- maxNumberOfSubjectsTarget,
                            maxNumberOfSubjectsLower <- maxNumberOfSubjectsTarget
                        )
                        prec <- maxNumberOfSubjectsUpper - maxNumberOfSubjectsLower
                        maxSearchIterations <- maxSearchIterations - 1
                    }
                } else {
                    maxNumberOfSubjectsTarget <- .getOneDimensionalRootBisectionMethod(
                        fun = function(x) {
                            fut <- .getSampleSizeSurvival(
                                design = design,
                                typeOfComputation = typeOfComputation,
                                thetaH0 = thetaH0, pi2 = pi2, pi1 = pi1,
                                allocationRatioPlanned = allocationRatioPlanned,
                                accountForObservationTimes = accountForObservationTimes,
                                eventTime = eventTime, accrualTime = accrualSetup$accrualTime,
                                accrualIntensity = accrualSetup$accrualIntensity, kappa = kappa,
                                piecewiseSurvivalTime = piecewiseSurvivalTime,
                                lambda2 = lambda2, lambda1 = lambda1,
                                median1 = median1, median2 = median2,
                                followUpTime = NA_real_, maxNumberOfSubjects = x,
                                dropoutRate1 = dropoutRate1, dropoutRate2 = dropoutRate2,
                                dropoutTime = dropoutTime,
                                hazardRatio = hazardRatio,
                                userFunctionCallEnabled = FALSE
                            )$followUpTime
                            return(followUpTime - fut)
                        },
                        lower = maxNumberOfSubjectsLower,
                        upper = maxNumberOfSubjectsUpper,
                        tolerance = 1e-04,
                        acceptResultsOutOfTolerance = TRUE,
                        maxSearchIterations = 50,
                        direction = 0,
                        suppressWarnings = FALSE,
                        callingFunctionInformation = "getSampleSizeSurvival"
                    )
                }
            },
            warning = function(w) {
                invokeRestart("muffleWarning")
            }
        )

        if (is.na(maxNumberOfSubjectsTarget)) {
            stop(
                C_EXCEPTION_TYPE_RUNTIME_ISSUE,
                "failed to calculate 'maxNumberOfSubjects' by given 'followUpTime' ",
                "(lower = ", maxNumberOfSubjectsLower, ", upper = ", maxNumberOfSubjectsUpper, ")"
            )
        }

        sampleSizeSurvival <- .getSampleSizeSurvival(
            design = design,
            typeOfComputation = typeOfComputation,
            thetaH0 = thetaH0, pi2 = pi2, pi1 = pi1,
            allocationRatioPlanned = allocationRatioPlanned,
            accountForObservationTimes = accountForObservationTimes,
            eventTime = eventTime, accrualTime = accrualSetup$accrualTime,
            accrualIntensity = accrualSetup$accrualIntensity, kappa = kappa,
            piecewiseSurvivalTime = piecewiseSurvivalTime, lambda2 = lambda2, lambda1 = lambda1,
            median1 = median1, median2 = median2,
            followUpTime = NA_real_, maxNumberOfSubjects = maxNumberOfSubjectsTarget,
            dropoutRate1 = dropoutRate1, dropoutRate2 = dropoutRate2,
            dropoutTime = dropoutTime,
            hazardRatio = hazardRatio,
            userFunctionCallEnabled = FALSE
        )
        sampleSizeSurvival$.setParameterType("followUpTime", C_PARAM_USER_DEFINED)
        sampleSizeSurvival$.accrualTime <- accrualSetup

        if (!is.na(sampleSizeSurvival$followUpTime)) {
            if (followUpTime == 1e12) {
                followUpTime <- Inf
            }

            if (sampleSizeSurvival$followUpTime >= -1e-02 && sampleSizeSurvival$followUpTime <= 1e-02) {
                sampleSizeSurvival$followUpTime <- 0
            }

            if (sampleSizeSurvival$followUpTime < followUpTime - 1e-02 ||
                    sampleSizeSurvival$followUpTime > followUpTime + 1e-02) {
                sampleSizeSurvival$.setParameterType("followUpTime", C_PARAM_GENERATED)
                warning("User defined 'followUpTime' (", followUpTime, ") ignored because ",
                    "follow-up time is ", round(sampleSizeSurvival$followUpTime, 4),
                    call. = FALSE
                )
            }
        }

        sampleSizeSurvival$.setParameterType("maxNumberOfSubjects", C_PARAM_GENERATED)
        sampleSizeSurvival$.setParameterType("accrualTime", C_PARAM_GENERATED)

        return(sampleSizeSurvival)
    }

    return(.getSampleSizeSurvival(
        design = design,
        typeOfComputation = typeOfComputation,
        thetaH0 = thetaH0, pi2 = pi2, pi1 = pi1,
        allocationRatioPlanned = allocationRatioPlanned,
        accountForObservationTimes = accountForObservationTimes,
        eventTime = eventTime, accrualTime = accrualTime,
        accrualIntensity = accrualIntensity, accrualIntensityType = accrualIntensityType,
        kappa = kappa, piecewiseSurvivalTime = piecewiseSurvivalTime,
        lambda2 = lambda2, lambda1 = lambda1,
        median1 = median1, median2 = median2,
        followUpTime = followUpTime, maxNumberOfSubjects = maxNumberOfSubjects,
        dropoutRate1 = dropoutRate1, dropoutRate2 = dropoutRate2,
        dropoutTime = dropoutTime,
        hazardRatio = hazardRatio
    ))
}

#' @title
#' Get Power Survival
#'
#' @description
#' Returns the power, stopping probabilities, and expected sample size for testing
#' the hazard ratio in a two treatment groups survival design.
#'
#' @inheritParams param_design_with_default
#' @inheritParams param_typeOfComputation
#' @inheritParams param_allocationRatioPlanned
#' @inheritParams param_thetaH0
#' @inheritParams param_lambda1
#' @inheritParams param_lambda2
#' @inheritParams param_pi1_survival
#' @inheritParams param_pi2_survival
#' @inheritParams param_median1
#' @inheritParams param_median2
#' @inheritParams param_piecewiseSurvivalTime
#' @inheritParams param_directionUpper
#' @inheritParams param_accrualTime
#' @inheritParams param_accrualIntensity
#' @inheritParams param_accrualIntensityType
#' @inheritParams param_eventTime
#' @inheritParams param_hazardRatio
#' @inheritParams param_kappa
#' @inheritParams param_dropoutRate1
#' @inheritParams param_dropoutRate2
#' @inheritParams param_dropoutTime
#' @param maxNumberOfEvents \code{maxNumberOfEvents > 0} is the maximum number of events, it determines
#'        the power of the test and needs to be specified.
#' @inheritParams param_maxNumberOfSubjects_survival
#' @inheritParams param_three_dots
#'
#' @details
#' At given design the function calculates the power, stopping probabilities, and expected
#' sample size at given number of events and number of subjects.
#' It also calculates the time when the required events are expected under the given
#' assumptions (exponentially, piecewise exponentially, or Weibull distributed survival times
#' and constant or non-constant piecewise accrual).
#' Additionally, an allocation ratio = \code{n1 / n2} can be specified where \code{n1} and \code{n2} are the number
#' of subjects in the two treatment groups.
#'
#' The formula of Kim & Tsiatis (Biometrics, 1990)
#' is used to calculate the expected number of events under the alternative
#' (see also Lakatos & Lan, Statistics in Medicine, 1992). These formulas are generalized to piecewise survival times and
#' non-constant piecewise accrual over time.\cr
#'
#' @template details_piecewise_survival
#'
#' @template details_piecewise_accrual
#'
#' @template return_object_trial_design_plan
#' @template how_to_get_help_for_generics
#'
#' @family power functions
#'
#' @template examples_get_power_survival
#'
#' @export
#'
getPowerSurvival <- function(design = NULL, ...,
        typeOfComputation = c("Schoenfeld", "Freedman", "HsiehFreedman"),
        thetaH0 = 1, # C_THETA_H0_SURVIVAL_DEFAULT
        directionUpper = NA,
        pi1 = NA_real_,
        pi2 = NA_real_,
        lambda1 = NA_real_,
        lambda2 = NA_real_,
        median1 = NA_real_,
        median2 = NA_real_,
        kappa = 1,
        hazardRatio = NA_real_,
        piecewiseSurvivalTime = NA_real_,
        allocationRatioPlanned = 1, # C_ALLOCATION_RATIO_DEFAULT
        eventTime = 12, # C_EVENT_TIME_DEFAULT
        accrualTime = c(0, 12), # C_ACCRUAL_TIME_DEFAULT
        accrualIntensity = 0.1, # C_ACCRUAL_INTENSITY_DEFAULT
        accrualIntensityType = c("auto", "absolute", "relative"),
        maxNumberOfSubjects = NA_real_,
        maxNumberOfEvents = NA_real_,
        dropoutRate1 = 0, # C_DROP_OUT_RATE_1_DEFAULT
        dropoutRate2 = 0, # C_DROP_OUT_RATE_2_DEFAULT
        dropoutTime = 12 # C_DROP_OUT_TIME_DEFAULT
        ) {
    if (is.null(design)) {
        design <- .getDefaultDesign(..., type = "power")
        .warnInCaseOfUnknownArguments(
            functionName = "getPowerSurvival",
            ignore = .getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design, powerCalculationEnabled = TRUE), ...
        )
    } else {
        .assertIsTrialDesign(design)
        .warnInCaseOfUnknownArguments(functionName = "getPowerSurvival", ...)
        .warnInCaseOfTwoSidedPowerArgument(...)
        .warnInCaseOfTwoSidedPowerIsDisabled(design)
        design <- .resetPipeOperatorQueue(design)
    }

    designPlan <- .createDesignPlanSurvival(
        objectType = "power",
        design = design,
        typeOfComputation = typeOfComputation,
        thetaH0 = thetaH0,
        pi2 = pi2,
        pi1 = pi1,
        allocationRatioPlanned = allocationRatioPlanned,
        accountForObservationTimes = TRUE,
        eventTime = eventTime,
        accrualTime = accrualTime,
        accrualIntensity = accrualIntensity,
        accrualIntensityType = accrualIntensityType,
        kappa = kappa,
        piecewiseSurvivalTime = piecewiseSurvivalTime,
        lambda2 = lambda2,
        lambda1 = lambda1,
        median1 = median1,
        median2 = median2,
        directionUpper = directionUpper,
        maxNumberOfEvents = maxNumberOfEvents,
        maxNumberOfSubjects = maxNumberOfSubjects,
        dropoutRate1 = dropoutRate1,
        dropoutRate2 = dropoutRate2,
        dropoutTime = dropoutTime,
        hazardRatio = hazardRatio
    )

    .assertIsSingleNumber(allocationRatioPlanned, "allocationRatioPlanned")
    .assertIsInOpenInterval(allocationRatioPlanned, "allocationRatioPlanned", 0, C_ALLOCATION_RATIO_MAXIMUM)

    if (designPlan$typeOfComputation == "Schoenfeld") {
        theta <- sqrt(allocationRatioPlanned) / (1 + allocationRatioPlanned) *
            (log(designPlan$hazardRatio / thetaH0))
    } else if (designPlan$typeOfComputation == "Freedman") {
        theta <- sqrt(allocationRatioPlanned) * (designPlan$hazardRatio - 1) /
            (allocationRatioPlanned * designPlan$hazardRatio + 1)
    } else if (designPlan$typeOfComputation == "HsiehFreedman") {
        theta <- sqrt(4 * allocationRatioPlanned) / (1 + allocationRatioPlanned) *
            (designPlan$hazardRatio - 1) / (designPlan$hazardRatio + 1)
    }

    if (!is.na(designPlan$directionUpper) && !designPlan$directionUpper) {
        theta <- -theta
    }

    powerAndAverageSampleNumber <- getPowerAndAverageSampleNumber(
        design = design, theta = theta, nMax = maxNumberOfEvents
    )

    kMax <- design$kMax
    sided <- design$sided

    if (designPlan$.piecewiseSurvivalTime$.isLambdaBased()) {
        numberOfResults <- length(designPlan$hazardRatio)
    } else {
        numberOfResults <- length(designPlan$pi1)
    }

    stoppingProbs <- matrix(NA_real_, kMax, numberOfResults)
    designPlan$analysisTime <- matrix(NA_real_, kMax, numberOfResults)
    designPlan$numberOfSubjects <- matrix(NA_real_, kMax, numberOfResults)
    designPlan$studyDuration <- rep(NA_real_, numberOfResults)
    designPlan$expectedNumberOfSubjects <- rep(NA_real_, numberOfResults)
    cumulativeEventsPerStage <- maxNumberOfEvents * design$informationRates
    parameterNames <- c(
        "analysisTime",
        "numberOfSubjects",
        "studyDuration",
        "expectedNumberOfSubjects",
        "cumulativeEventsPerStage"
    )

    phi <- -c(log(1 - dropoutRate1), log(1 - dropoutRate2)) / dropoutTime

    lambda1 <- designPlan$lambda1
    if (designPlan$.piecewiseSurvivalTime$piecewiseSurvivalEnabled) {
        lambda1 <- rep(NA_real_, numberOfResults)
    }

    studyDuration <- rep(NA_real_, numberOfResults)
    for (i in 1:numberOfResults) {
        # Analysis times
        up <- 2
        iterate <- 1
        while (cumulativeEventsPerStage[kMax] / designPlan$maxNumberOfSubjects > .getEventProbabilities(
            time = up, accrualTimeVector = designPlan$accrualTime,
            accrualIntensity = designPlan$accrualIntensity,
            lambda2 = designPlan$lambda2,
            lambda1 = lambda1[i], phi = phi,
            piecewiseSurvivalTime = designPlan$piecewiseSurvivalTime,
            kappa = kappa, allocationRatioPlanned = allocationRatioPlanned,
            hazardRatio = designPlan$hazardRatio[i]
        )) {
            up <- 2 * up
            iterate <- iterate + 1
            if (iterate > 50) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'maxNumberOfSubjects' (", designPlan$maxNumberOfSubjects, ") ",
                    "is too small to reach maximum number of events ",
                    "(presumably due to drop-out rates)"
                )
            }
        }
        for (j in 1:kMax) {
            designPlan$analysisTime[j, i] <- .getOneDimensionalRoot(
                function(x) {
                    cumulativeEventsPerStage[j] / designPlan$maxNumberOfSubjects -
                        .getEventProbabilities(
                            time = x,
                            accrualTimeVector = designPlan$accrualTime,
                            accrualIntensity = designPlan$accrualIntensity,
                            lambda2 = designPlan$lambda2,
                            lambda1 = lambda1[i], phi = phi,
                            piecewiseSurvivalTime = designPlan$piecewiseSurvivalTime,
                            kappa = kappa, allocationRatioPlanned = allocationRatioPlanned,
                            hazardRatio = designPlan$hazardRatio[i]
                        )
                },
                lower = 0, upper = up, tolerance = 1e-06,
                callingFunctionInformation = "getPowerSurvival"
            )
            if (is.na(designPlan$analysisTime[j, i])) {
                warning("Cannot calculate analysis time at stage ", j, ": ",
                    "'maxNumberOfSubjects' (", designPlan$maxNumberOfSubjects, ") is too ",
                    "small to reach maximum number of events",
                    call. = FALSE
                )
            }
        }
        if (kMax > 1) {
            designPlan$numberOfSubjects[, i] <- .getNumberOfSubjects(
                time = designPlan$analysisTime[, i],
                accrualTime = designPlan$accrualTime,
                accrualIntensity = designPlan$accrualIntensity,
                maxNumberOfSubjects = designPlan$maxNumberOfSubjects
            )

            powerAndAverageSampleNumber$futilityPerStage[is.na(
                powerAndAverageSampleNumber$futilityPerStage[, i]
            ), i] <- 0
        
            stoppingProbs[, i] <- powerAndAverageSampleNumber$rejectPerStage[, i] +
                c(powerAndAverageSampleNumber$futilityPerStage[, i], 0)
            
            stoppingProbs[kMax, i] <- 1 - sum(stoppingProbs[1:(kMax - 1), i])
            designPlan$studyDuration[i] <- designPlan$analysisTime[, i] %*% stoppingProbs[, i]
            designPlan$.setParameterType("studyDuration", C_PARAM_GENERATED)
            designPlan$expectedNumberOfSubjects[i] <-
                designPlan$numberOfSubjects[, i] %*% stoppingProbs[, i]
        }
    }

    if (kMax == 1) {
        designPlan$expectedNumberOfSubjects <- .getNumberOfSubjects(
            time = designPlan$analysisTime[1, ],
            accrualTime = designPlan$accrualTime,
            accrualIntensity = designPlan$accrualIntensity,
            maxNumberOfSubjects = designPlan$maxNumberOfSubjects
        )

        designPlan$numberOfSubjects <- matrix(designPlan$expectedNumberOfSubjects, nrow = 1)
    }

    designPlan$cumulativeEventsPerStage <- matrix(cumulativeEventsPerStage, ncol = 1)
    designPlan$.setParameterType("cumulativeEventsPerStage", C_PARAM_GENERATED)
    .addDeprecatedFieldValues(designPlan, "eventsPerStage", designPlan$cumulativeEventsPerStage)

    designPlan$expectedNumberOfEvents <- powerAndAverageSampleNumber$averageSampleNumber
    designPlan$overallReject <- powerAndAverageSampleNumber$overallReject
    designPlan$rejectPerStage <- powerAndAverageSampleNumber$rejectPerStage
    designPlan$futilityStop <- powerAndAverageSampleNumber$overallFutility
    designPlan$futilityPerStage <- powerAndAverageSampleNumber$futilityPerStage
    designPlan$earlyStop <- powerAndAverageSampleNumber$overallEarlyStop

    parameterNames <- c(
        parameterNames,
        "expectedNumberOfEvents",
        "overallReject",
        "rejectPerStage",
        "futilityStop",
        "futilityPerStage",
        "earlyStop"
    )

    for (parameterName in parameterNames) {
        designPlan$.setParameterType(parameterName, C_PARAM_GENERATED)
    }

    if (kMax == 1L) {
        designPlan$.setParameterType("numberOfSubjects", C_PARAM_NOT_APPLICABLE)
        designPlan$.setParameterType("cumulativeEventsPerStage", C_PARAM_NOT_APPLICABLE)

        designPlan$.setParameterType("futilityStop", C_PARAM_NOT_APPLICABLE)
        designPlan$.setParameterType("earlyStop", C_PARAM_NOT_APPLICABLE)
        designPlan$.setParameterType("rejectPerStage", C_PARAM_NOT_APPLICABLE)
    }

    if (!any(is.na(designPlan$analysisTime)) && !any(is.na(designPlan$accrualTime))) {
        designPlan$followUpTime <- designPlan$analysisTime[kMax, ] -
            designPlan$accrualTime[length(designPlan$accrualTime)]
        designPlan$.setParameterType("followUpTime", C_PARAM_GENERATED)
    }

    designPlan$singleEventsPerStage <- .getSingleEventsPerStage(designPlan$cumulativeEventsPerStage)

    .addEffectScaleBoundaryDataToDesignPlan(designPlan)
    .addStudyDurationToDesignPlan(designPlan)
    .hideFutilityStopsIfNotApplicable(designPlan)

    return(designPlan)
}
