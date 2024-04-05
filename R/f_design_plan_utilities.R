## |
## |  *Sample size and power utilities*
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
## |  File version: $Revision: 7742 $
## |  Last changed: $Date: 2024-03-22 13:46:29 +0100 (Fr, 22 Mrz 2024) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_core_utilities.R
NULL

.hideFutilityStopsIfNotApplicable <- function(designPlan) {
    if (all(designPlan$.design$futilityBounds == C_FUTILITY_BOUNDS_DEFAULT)) {
        designPlan$.setParameterType("futilityStop", C_PARAM_NOT_APPLICABLE)
        designPlan$.setParameterType("futilityPerStage", C_PARAM_NOT_APPLICABLE)
    }
}

.getSingleEventsPerStage <- function(cumulativeEventsPerStage) {
    singleEventsPerStage <- matrix(nrow = nrow(cumulativeEventsPerStage), ncol = ncol(cumulativeEventsPerStage))
    if (nrow(cumulativeEventsPerStage) > 1) {
        for (i in nrow(cumulativeEventsPerStage):2) {
            singleEventsPerStage[i, ] <- cumulativeEventsPerStage[i, ] - cumulativeEventsPerStage[i - 1, ]
        }
    }
    if (nrow(cumulativeEventsPerStage) > 0) {
        singleEventsPerStage[1, ] <- cumulativeEventsPerStage[1, ]
    }
    return(singleEventsPerStage)
}

.addEffectScaleBoundaryDataToDesignPlan <- function(designPlan) {
    .assertIsTrialDesignPlan(designPlan)

    design <- designPlan$.design
    if (.isTrialDesignPlanMeans(designPlan)) {
        if (design$kMax == 1 && designPlan$.isSampleSizeObject()) {
            designPlan$maxNumberOfSubjects <- designPlan$nFixed
        }

        boundaries <- .getEffectScaleBoundaryDataMeans(designPlan)
    } else if (.isTrialDesignPlanRates(designPlan)) {
        if (designPlan$.isSampleSizeObject()) { # comes from getSampleSize
            if (designPlan$groups == 1) {
                designPlan$directionUpper <- (designPlan$pi1 > designPlan$thetaH0)
            } else {
                if (designPlan$riskRatio) {
                    designPlan$directionUpper <- (designPlan$pi1 / designPlan$pi2 > designPlan$thetaH0)
                } else {
                    designPlan$directionUpper <- (designPlan$pi1 - designPlan$pi2 > designPlan$thetaH0)
                }
            }
            designPlan$.setParameterType("directionUpper", C_PARAM_GENERATED)
        }
        if (design$kMax == 1 && designPlan$.isSampleSizeObject()) {
            designPlan$maxNumberOfSubjects <- designPlan$nFixed
        }
        boundaries <- .getEffectScaleBoundaryDataRates(designPlan)
    } else if (.isTrialDesignPlanSurvival(designPlan)) {
        if (designPlan$.isSampleSizeObject()) { # comes from getSampleSize
            designPlan$directionUpper <- (designPlan$hazardRatio > designPlan$thetaH0)
            designPlan$.setParameterType("directionUpper", C_PARAM_GENERATED)
        }

        if (design$kMax == 1 && designPlan$.isSampleSizeObject()) {
            designPlan$cumulativeEventsPerStage <- matrix(designPlan$eventsFixed, nrow = 1)
        }
        boundaries <- .getEffectScaleBoundaryDataSurvival(designPlan)
    }

    if (designPlan$.design$sided == 1) {
        designPlan$criticalValuesEffectScale <- boundaries$criticalValuesEffectScaleUpper
        designPlan$.setParameterType("criticalValuesEffectScale", C_PARAM_GENERATED)
    } else {
        if (all(boundaries$criticalValuesEffectScaleLower < boundaries$criticalValuesEffectScaleUpper, na.rm = TRUE)) {
            designPlan$criticalValuesEffectScaleLower <- boundaries$criticalValuesEffectScaleLower
            designPlan$criticalValuesEffectScaleUpper <- boundaries$criticalValuesEffectScaleUpper
        } else {
            designPlan$criticalValuesEffectScaleLower <- boundaries$criticalValuesEffectScaleUpper
            designPlan$criticalValuesEffectScaleUpper <- boundaries$criticalValuesEffectScaleLower
        }
        designPlan$.setParameterType("criticalValuesEffectScaleUpper", C_PARAM_GENERATED)
        designPlan$.setParameterType("criticalValuesEffectScaleLower", C_PARAM_GENERATED)
    }

    if (!.isTrialDesignFisher(design) && any(design$futilityBounds > C_FUTILITY_BOUNDS_DEFAULT)) {
        if (design$sided == 1) {
            designPlan$futilityBoundsEffectScale <- round(boundaries$futilityBoundsEffectScaleUpper, 8)
            designPlan$.setParameterType("futilityBoundsEffectScale", C_PARAM_GENERATED)
        } else {
            if (all(designPlan$futilityBoundsEffectScaleLower < designPlan$futilityBoundsEffectScaleUpper, na.rm = TRUE)) {
                designPlan$futilityBoundsEffectScaleLower <- round(boundaries$futilityBoundsEffectScaleLower, 8)
                designPlan$futilityBoundsEffectScaleUpper <- round(boundaries$futilityBoundsEffectScaleUpper, 8)
            } else {
                designPlan$futilityBoundsEffectScaleLower <- round(boundaries$futilityBoundsEffectScaleUpper, 8)
                designPlan$futilityBoundsEffectScaleUpper <- round(boundaries$futilityBoundsEffectScaleLower, 8)
            }
            designPlan$.setParameterType("futilityBoundsEffectScaleLower", C_PARAM_GENERATED)
            designPlan$.setParameterType("futilityBoundsEffectScaleUpper", C_PARAM_GENERATED)
        }
    }
}

.calculateSampleSizeMeansAndRates <- function(designPlan) {
    if (identical(designPlan$allocationRatioPlanned, 0)) {
        designPlan$optimumAllocationRatio <- TRUE
        designPlan$.setParameterType("optimumAllocationRatio", C_PARAM_USER_DEFINED)
    }

    if (.isTrialDesignPlanMeans(designPlan)) {
        sampleSizeFixed <- .getSampleSizeFixedMeans(
            alpha = designPlan$getAlpha(),
            beta = designPlan$getBeta(),
            sided = designPlan$getSided(),
            twoSidedPower = designPlan$getTwoSidedPower(),
            normalApproximation = designPlan$normalApproximation,
            meanRatio = designPlan$meanRatio,
            thetaH0 = designPlan$thetaH0,
            alternative = designPlan$alternative,
            stDev = designPlan$stDev,
            groups = designPlan$groups,
            allocationRatioPlanned = designPlan$allocationRatioPlanned
        )
    } else {
        sampleSizeFixed <- .getSampleSizeFixedRates(
            alpha = designPlan$getAlpha(),
            beta = designPlan$getBeta(),
            sided = designPlan$getSided(),
            normalApproximation = designPlan$normalApproximation,
            riskRatio = designPlan$riskRatio,
            thetaH0 = designPlan$thetaH0,
            pi1 = designPlan$pi1,
            pi2 = designPlan$pi2,
            groups = designPlan$groups,
            allocationRatioPlanned = designPlan$allocationRatioPlanned
        )
    }

    # Fixed
    designPlan$nFixed <- sampleSizeFixed$nFixed
    designPlan$.setParameterType("nFixed", C_PARAM_GENERATED)
    if (designPlan$groups == 2) {
        designPlan$nFixed1 <- sampleSizeFixed$n1Fixed
        designPlan$nFixed2 <- sampleSizeFixed$n2Fixed
        designPlan$.setParameterType("nFixed1", C_PARAM_GENERATED)
        designPlan$.setParameterType("nFixed2", C_PARAM_GENERATED)
        designPlan$numberOfSubjects1 <- matrix(designPlan$nFixed1, nrow = 1)
        designPlan$numberOfSubjects2 <- matrix(designPlan$nFixed2, nrow = 1)
    }
    designPlan$numberOfSubjects <- matrix(designPlan$nFixed, nrow = 1)

    if (!is.null(sampleSizeFixed$allocationRatioPlanned) &&
            (length(designPlan$allocationRatioPlanned) !=
                length(sampleSizeFixed$allocationRatioPlanned) ||
                sum(designPlan$allocationRatioPlanned == sampleSizeFixed$allocationRatioPlanned) !=
                    length(designPlan$allocationRatioPlanned))) {
        designPlan$allocationRatioPlanned <- sampleSizeFixed$allocationRatioPlanned
        designPlan$.setParameterType("allocationRatioPlanned", C_PARAM_GENERATED)
    }

    # Sequential
    if (designPlan$.design$kMax > 1) {
        designCharacteristics <- getDesignCharacteristics(designPlan$.design)
        if (.isTrialDesignPlanMeans(designPlan)) {
            sampleSizeSequential <- .getSampleSizeSequentialMeans(
                sampleSizeFixed, designCharacteristics
            )
        } else {
            sampleSizeSequential <- .getSampleSizeSequentialRates(
                sampleSizeFixed, designCharacteristics
            )
        }

        designPlan$informationRates <- sampleSizeSequential$informationRates
        if (ncol(designPlan$informationRates) == 1 &&
                identical(designPlan$informationRates[, 1], designPlan$.design$informationRates)) {
            designPlan$.setParameterType("informationRates", C_PARAM_NOT_APPLICABLE)
        } else {
            designPlan$.setParameterType("informationRates", C_PARAM_GENERATED)
        }

        designPlan$maxNumberOfSubjects <- sampleSizeSequential$maxNumberOfSubjects
        designPlan$.setParameterType("maxNumberOfSubjects", C_PARAM_GENERATED)
        if (designPlan$groups == 2) {
            designPlan$maxNumberOfSubjects1 <- .getNumberOfSubjects1(
                designPlan$maxNumberOfSubjects, designPlan$allocationRatioPlanned
            )
            designPlan$maxNumberOfSubjects2 <- .getNumberOfSubjects2(
                designPlan$maxNumberOfSubjects, designPlan$allocationRatioPlanned
            )
            designPlan$.setParameterType("maxNumberOfSubjects1", C_PARAM_GENERATED)
            designPlan$.setParameterType("maxNumberOfSubjects2", C_PARAM_GENERATED)
        }

        designPlan$numberOfSubjects <- sampleSizeSequential$numberOfSubjects
        designPlan$.setParameterType("numberOfSubjects", C_PARAM_GENERATED)

        if (designPlan$groups == 2) {
            designPlan$numberOfSubjects1 <- sampleSizeSequential$numberOfSubjects1
            designPlan$numberOfSubjects2 <- sampleSizeSequential$numberOfSubjects2
            designPlan$.setParameterType("numberOfSubjects1", C_PARAM_GENERATED)
            designPlan$.setParameterType("numberOfSubjects2", C_PARAM_GENERATED)
        }

        designPlan$expectedNumberOfSubjectsH0 <- sampleSizeSequential$expectedNumberOfSubjectsH0
        designPlan$expectedNumberOfSubjectsH01 <- sampleSizeSequential$expectedNumberOfSubjectsH01
        designPlan$expectedNumberOfSubjectsH1 <- sampleSizeSequential$expectedNumberOfSubjectsH1
        designPlan$.setParameterType("expectedNumberOfSubjectsH0", C_PARAM_GENERATED)
        designPlan$.setParameterType("expectedNumberOfSubjectsH01", C_PARAM_GENERATED)
        designPlan$.setParameterType("expectedNumberOfSubjectsH1", C_PARAM_GENERATED)

        designPlan$.setParameterType("eventsFixed", C_PARAM_NOT_APPLICABLE)
        designPlan$.setParameterType("nFixed1", C_PARAM_NOT_APPLICABLE)
        designPlan$.setParameterType("nFixed2", C_PARAM_NOT_APPLICABLE)
        designPlan$.setParameterType("nFixed", C_PARAM_NOT_APPLICABLE)

        if (all(designPlan$allocationRatioPlanned == 1)) {
            designPlan$.setParameterType("numberOfSubjects1", C_PARAM_NOT_APPLICABLE)
            designPlan$.setParameterType("numberOfSubjects2", C_PARAM_NOT_APPLICABLE)
        }

        if (!is.null(sampleSizeSequential$rejectPerStage)) {
            designPlan$rejectPerStage <- matrix(sampleSizeSequential$rejectPerStage,
                nrow = designPlan$.design$kMax
            )
            designPlan$.setParameterType("rejectPerStage", C_PARAM_GENERATED)

            designPlan$earlyStop <- sum(designPlan$rejectPerStage[1:(designPlan$.design$kMax - 1), ])
            designPlan$.setParameterType("earlyStop", C_PARAM_GENERATED)
        }
        if (!is.null(sampleSizeSequential$futilityPerStage) &&
                any(designPlan$.design$futilityBounds != C_FUTILITY_BOUNDS_DEFAULT)) {
            designPlan$futilityPerStage <- matrix(sampleSizeSequential$futilityPerStage,
                nrow = designPlan$.design$kMax - 1
            )
            designPlan$.setParameterType("futilityPerStage", C_PARAM_GENERATED)

            designPlan$futilityStop <- sum(designPlan$futilityPerStage)
            designPlan$.setParameterType("futilityStop", C_PARAM_GENERATED)

            designPlan$earlyStop <- designPlan$earlyStop + sum(designPlan$futilityPerStage)
        }
    }

    .addEffectScaleBoundaryDataToDesignPlan(designPlan)

    return(designPlan)
}

.getColumnCumSum <- function(x) {
    if (is.matrix(x)) {
        result <- x
        for (i in 1:ncol(x)) {
            result[, i] <- cumsum(x[, i])
        }
        return(result)
    }

    return(cumsum(x))
}

.getFarringtonManningValuesDiff <- function(..., rate1, rate2, theta, allocation) {
    if (theta == 0) {
        ml1 <- (allocation * rate1 + rate2) / (1 + allocation)
        ml2 <- ml1
        return(c(ml1, ml2))
    }

    a <- 1 + 1 / allocation
    b <- -(1 + 1 / allocation + rate1 + rate2 / allocation + theta * (1 / allocation + 2))
    c <- theta^2 + theta * (2 * rate1 + 1 / allocation + 1) + rate1 + rate2 / allocation
    d <- -theta * (1 + theta) * rate1

    v <- b^3 / (3 * a)^3 - b * c / (6 * a^2) + d / (2 * a)
    if (!is.na(v) && (v == 0)) {
        u <- sqrt(b^2 / (3 * a)^2 - c / (3 * a))
        w <- acos(-1) / 2
    } else {
        u <- sign(v) * sqrt(b^2 / (3 * a)^2 - c / (3 * a))
        w <- 1 / 3 * (acos(-1) + acos(v / u^3))
    }
    ml1 <- min(max(0, 2 * u * cos(w) - b / (3 * a)), 1)
    ml2 <- min(max(0, ml1 - theta), 1)

    return(c(ml1, ml2))
}

.getFarringtonManningValuesRatio <- function(..., rate1, rate2, theta, allocation) {
    if (theta == 1) {
        ml1 <- (allocation * rate1 + rate2) / (1 + allocation)
        ml2 <- ml1
        return(c(ml1, ml2))
    }

    a <- 1 + 1 / allocation
    b <- -((1 + rate2 / allocation) * theta + 1 / allocation + rate1)
    c <- (rate1 + rate2 / allocation) * theta
    ml1 <- (-b - sqrt(b^2 - 4 * a * c)) / (2 * a)
    ml2 <- ml1 / theta

    return(c(ml1, ml2))
}

#'
#' @title
#' Get Farrington Manning Values
#'
#' @description
#' Calculates and returns the maximum likelihood estimates under H0.
#'
#' @details
#' Calculation of maximum likelihood estimates under
#' H0: pi1 - pi2 = theta or H0: pi1 / pi2 = theta
#'
#' @references
#' Farrington & Manning (1990)
#' Wassmer (2003)
#'
#' @keywords internal
#'
#' @noRd
#'
.getFarringtonManningValues <- function(rate1, rate2, theta, allocation, method = c("diff", "ratio")) {
    method <- match.arg(method)
    if (method == "diff") {
        ml <- .getFarringtonManningValuesDiff(rate1 = rate1, rate2 = rate2, theta = theta, allocation = allocation)
    } else {
        ml <- .getFarringtonManningValuesRatio(rate1 = rate1, rate2 = rate2, theta = theta, allocation = allocation)
    }
    return(list(theta = theta, method = method, ml1 = ml[1], ml2 = ml[2]))
}

.getPiecewiseExpStartTimesWithoutLeadingZero <- function(piecewiseSurvivalTime) {
    if (is.null(piecewiseSurvivalTime) || length(piecewiseSurvivalTime) == 0 ||
            all(is.na(piecewiseSurvivalTime))) {
        return(NA_real_)
    }

    if (piecewiseSurvivalTime[1] != 0) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "the first value of 'piecewiseSurvivalTime' (",
            .arrayToString(piecewiseSurvivalTime), ") must be 0",
            call. = FALSE
        )
    }

    if (length(piecewiseSurvivalTime) == 1) {
        return(numeric(0))
    }

    if (length(piecewiseSurvivalTime) < 2) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "length of 'piecewiseSurvivalTime' (",
            length(piecewiseSurvivalTime), ") must be > 1"
        )
    }

    return(piecewiseSurvivalTime[2:length(piecewiseSurvivalTime)])
}

.getNumberOfSubjectsInner <- function(..., timeValue, accrualTime, accrualIntensity, maxNumberOfSubjects) {
    .assertIsSingleNumber(timeValue, "timeValue")
    if (length(accrualTime) != length(accrualIntensity)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "length of 'accrualTime' (", length(accrualIntensity), ") ",
            "must be equel to length of 'accrualIntensity' (", length(accrualIntensity), ")"
        )
    }

    densityIntervals <- accrualTime
    if (length(accrualTime) > 1) {
        densityIntervals[2:length(accrualTime)] <- accrualTime[2:length(accrualTime)] -
            accrualTime[1:(length(accrualTime) - 1)]
    }
    densityVector <- accrualIntensity / sum(densityIntervals * accrualIntensity)
    for (l in 1:length(densityVector)) {
        if (timeValue <= accrualTime[l]) {
            if (l == 1) {
                return(timeValue * densityVector[l] * maxNumberOfSubjects)
            } else {
                return((sum(densityVector[1:(l - 1)] * densityIntervals[1:(l - 1)]) +
                    (timeValue - accrualTime[l - 1]) * densityVector[l]) * maxNumberOfSubjects)
            }
        }
    }
    return(maxNumberOfSubjects)
}

.addNumberOfSubjectsToPowerResult <- function(designPlan) {
    design <- designPlan$.design

    designPlan$numberOfSubjects <- matrix(rep(NA_real_, design$kMax), ncol = 1)
    designPlan$numberOfSubjects[1, 1] <- design$informationRates[1] * designPlan$maxNumberOfSubjects
    if (design$kMax > 1) {
        designPlan$numberOfSubjects[2:design$kMax, 1] <- (design$informationRates[2:design$kMax] -
            design$informationRates[1:(design$kMax - 1)]) * designPlan$maxNumberOfSubjects
    }

    designPlan$numberOfSubjects <- .getColumnCumSum(designPlan$numberOfSubjects)

    designPlan$numberOfSubjects1 <- .getNumberOfSubjects1(
        designPlan$numberOfSubjects, designPlan$allocationRatioPlanned
    )
    designPlan$numberOfSubjects2 <- .getNumberOfSubjects2(
        designPlan$numberOfSubjects, designPlan$allocationRatioPlanned
    )

    if (designPlan$.design$kMax == 1) {
        designPlan$nFixed <- as.numeric(designPlan$numberOfSubjects)
        designPlan$.setParameterType("nFixed", C_PARAM_GENERATED)
        if (designPlan$groups == 2) {
            designPlan$nFixed1 <- as.numeric(designPlan$numberOfSubjects1)
            designPlan$nFixed2 <- as.numeric(designPlan$numberOfSubjects2)
            designPlan$.setParameterType("nFixed1", C_PARAM_GENERATED)
            designPlan$.setParameterType("nFixed2", C_PARAM_GENERATED)
        }
    } else {
        designPlan$.setParameterType("numberOfSubjects", C_PARAM_GENERATED)

        if (designPlan$groups == 1 || all(designPlan$allocationRatioPlanned == 1)) {
            designPlan$.setParameterType("numberOfSubjects1", C_PARAM_NOT_APPLICABLE)
            designPlan$.setParameterType("numberOfSubjects2", C_PARAM_NOT_APPLICABLE)
        } else {
            designPlan$.setParameterType("numberOfSubjects1", C_PARAM_GENERATED)
            designPlan$.setParameterType("numberOfSubjects2", C_PARAM_GENERATED)
        }
    }
}
