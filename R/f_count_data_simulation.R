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
## |  File version: $Revision: 7436 $
## |  Last changed: $Date: 2023-11-13 17:08:37 +0100 (Mo, 13 Nov 2023) $
## |  Last changed by: $Author: wassmer $
## |

#'
#' TODO @Gernot: document
#'
#' @export
#'
getSimulationCounts <- function(design = NULL, ...,
        plannedMaxSubjects = NA_real_,
        plannedCalendarTime = NA_real_,
        lambda1 = NA_real_,
        lambda2 = NA_real_,
        lambda = NA_real_,
        theta = NA_real_,
		directionUpper = TRUE, # C_DIRECTION_UPPER_DEFAULT		
        thetaH0 = 1,
        overDispersion = 0,
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
            ignore = c(
                .getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design,
                    powerCalculationEnabled = TRUE
                ),
                "showStatistics"
            ), ...
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

    if (any(is.na(allocationRatioPlanned))) {
        allocationRatioPlanned <- C_ALLOCATION_RATIO_DEFAULT
    }
    .assertIsValidAllocationRatioPlannedSampleSize(allocationRatioPlanned, plannedMaxSubjects)
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
        accrualTime, accrualIntensity, plannedMaxSubjects
    )
    if ((length(accrualTime) > 1) && (accrualTime[1] == 0)) {
        accrualTime <- accrualTime[-1]
    }
    if (kMax == 1) {
        futilityPerStage <- NULL
        rejectPerStage <- NULL
        earlyStop <- NULL
    } else {
        futilityPerStage <- matrix(NA_real_, kMax - 1, totalCases)
        rejectPerStage <- matrix(NA_real_, kMax, totalCases)
        earlyStop <- matrix(NA_real_, kMax, totalCases)
    }
    overallReject <- rep(NA_real_, totalCases)
    for (iCase in 1:totalCases) {
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
					timeUnderObservation1 <- pmax(pmin(accrualTime + followUpTime - recruit1, 
									fixedExposureTime), 0)
					timeUnderObservation2 <- pmax(pmin(accrualTime + followUpTime - recruit2, 
									fixedExposureTime), 0)
                }
                counts1 <- rnbinom(n = n1, mu = lambda1[iCase] * timeUnderObservation1, 
									size = 1 / overDispersion)
                counts2 <- rnbinom(n = n2, mu = lambda2 * timeUnderObservation2, 
									size = 1 / overDispersion)
                nb <- .getNegativeBinomialEstimates(
                    counts1 = counts1, counts2 = counts2,
                    t1 = timeUnderObservation1, t2 = timeUnderObservation2
                )
                info_interim <- .getInformation(
                    lambda1 = nb[1],
                    lambda2 = nb[2],
                    overDispersion = nb[3],
                    recruit1 = timeUnderObservation1,
                    recruit2 = timeUnderObservation2
                )
                z <- (2 * directionUpper - 1) * (log(nb[1]) - log(nb[2]) - log(thetaH0)) * sqrt(info_interim)
                if (z > design$criticalValues[1]) {
                    reject[1] <- reject[1] + 1
                }
            } else {
                counts <- rep(0, length(recruit1) + length(recruit2))
                dfStartStop <- .generateEventTimes(
                    recruit1 = recruit1,
                    recruit2 = recruit2,
                    accrualTime = accrualTime,
                    followUpTime = followUpTime,
                    lambda1 = lambda1[iCase],
                    lambda2 = lambda2,
                    overDispersion = overDispersion,
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
                    info_interim <- .getInformation(
                        lambda1 = nb[1],
                        lambda2 = nb[2],
                        overDispersion = nb[3],
                        recruit1 = timeUnderObservation1,
                        recruit2 = timeUnderObservation2
                    )
                    z <- (2 * directionUpper - 1) * (log(nb[1]) - log(nb[2]) - log(thetaH0)) * sqrt(info_interim)
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
        if (kMax > 1) {
            futilityPerStage[, iCase] <- futility / i
            rejectPerStage[, iCase] <- reject / i
            earlyStop[1:(kMax - 1), iCase] <- (reject[1:(kMax - 1)] + futility) / i
        }
        overallReject[iCase] <- cumsum(reject / i)[kMax]
    }
    return(list(
        design = design,
        directionUpper = directionUpper,
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
        plannedMaxSubjects = nTotal,
        plannedCalendarTime = plannedCalendarTime,
        futilityPerStage = futilityPerStage,
        rejectPerStage = rejectPerStage,
        earlyStop = earlyStop,
        overallReject = overallReject
    ))
}
