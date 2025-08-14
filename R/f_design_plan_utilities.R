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

#' @include f_core_utilities.R
NULL

#'
#' Get Futility Bounds
#'
#' @description
#' This function converts futility bounds between different scales such as
#' z-value, p-value, conditional power, predictive power, reverse conditional
#' power, and effect estimate.
#'
#' @param sourceValue A numeric vector representing the
#' futility bounds in the source scale.
#' @param sourceScale Character. The scale of the input futility bounds.
#' Must be one of \code{"zValue"}, \code{"pValue"},
#' \code{"conditionalPower"}, "condPowerAtObserved", \code{"predictivePower"},
#' \code{"reverseCondPower"}, or \code{"effectEstimate"}.
#' @param targetScale Character. The scale to which the futility bounds should
#' be converted. Must be one of \code{"zValue"}, \code{"pValue"},
#' \code{"conditionalPower"}, "condPowerAtObserved", \code{"predictivePower"},
#' \code{"reverseCondPower"}, or \code{"effectEstimate"}.
#' @param design
#' @param theta
#' @param information1
#' @param information2
#' @inheritParams param_three_dots
#'
#' @details
#' If the \code{sourceScale} and \code{targetScale} are the same, the function
#' returns the input \code{sourceValue} without modification.
#' Otherwise, the function is designed to convert between the specified scales.
#'
#' @return
#' A numeric vector representing the futility bounds in the target scale, or
#' \code{NULL} if the conversion is not implemented or yields no result.
#'
#' @examples
#' \dontrun{
#' # Example with identical source and target scales
#' getFutilityBounds(
#'     sourceValue = c(0, 0.5),
#'     sourceScale = "zValue",
#'     targetScale = "zValue"
#' )
#'
#' # Example with different scales
#' getFutilityBounds(
#'     design = getDesignGroupSequential(kMax = 2, typeOfDesign = "noEarlyEfficacy", alpha = 0.05),
#'     information1 = 10,
#'     information2 = 10,
#'     sourceValue = 0.5,
#'     sourceScale = "condPowerAtObserved",
#'     targetScale = "pValue"
#' )
#' }
#' @export
getFutilityBounds <- function(
        sourceValue,
        ...,
        design = NULL,
        sourceScale = c(
            "zValue",
            "pValue",
            "conditionalPower",
            "condPowerAtObserved",
            "predictivePower",
            "reverseCondPower",
            "effectEstimate"
        ),
        targetScale = c(
            "zValue",
            "pValue",
            "conditionalPower",
            "condPowerAtObserved",
            "predictivePower",
            "reverseCondPower",
            "effectEstimate"
        ),
        theta = NA_real_,
        information1 = NA_real_,
        information2 = NA_real_) {
    sourceScale <- match.arg(sourceScale)
    targetScale <- match.arg(targetScale)
    if (sourceScale == targetScale) {
        return(sourceValue)
    }
    
    # TODO test and activate the following line instead of lines 107 - 223 because messages are more precise
    #.assertAreValidFutilityBoundsScaleArguments(design, sourceScale, targetScale, theta, information1, information2)
    
    if (!((sourceScale == "conditionalPower") ||
            (targetScale == "conditionalPower") ||
            (sourceScale == "condPowerAtObserved") ||
            (targetScale == "condPowerAtObserved") ||
            (sourceScale == "predictivePower") ||
            (targetScale == "predictivePower") ||
            (sourceScale == "reverseCondPower") ||
            (targetScale == "reverseCondPower")) &&
            !is.null(design)
        ) {
        warning(
            "'design' need not to be specified for 'sourceScale' = ",
            sQuote(sourceScale),
            " and 'targetScale' = ",
            sQuote(targetScale),
            call. = FALSE
        )
    }
    if (
        !((sourceScale == "conditionalPower") ||
            (targetScale == "conditionalPower") ||
            (sourceScale == "condPowerAtObserved") ||
            (targetScale == "condPowerAtObserved") ||
            (sourceScale == "predictivePower") ||
            (targetScale == "predictivePower") ||
            (sourceScale == "effectEstimate") ||
            (targetScale == "effectEstimate")) &&
            !is.na(information1)
        ) {
        warning(
            "'information1' need not to be specified for 'sourceScale' = ",
            sourceScale,
            " and 'targetScale' = ",
            targetScale,
            call. = FALSE
        )
    }
    if (
        !((sourceScale == "conditionalPower") ||
            (targetScale == "conditionalPower") ||
            (sourceScale == "condPowerAtObserved") ||
            (targetScale == "condPowerAtObserved") ||
            (sourceScale == "predictivePower") ||
            (targetScale == "predictivePower")) &&
            !is.na(information2)
        ) {
        warning(
            "'information2' need not to be specified for 'sourceScale' = ",
            sourceScale,
            " and 'targetScale' = ",
            targetScale,
            call. = FALSE
        )
    }

    if (
        !((sourceScale == "conditionalPower") ||
            (targetScale == "conditionalPower")) &&
            !is.na(theta)
        ) {
        warning(
            "'theta' need not to be specified for 'sourceScale' = ",
            sourceScale,
            " and 'targetScale' = ",
            targetScale,
            call. = FALSE
        )
    }

    if ((sourceScale == "effectEstimate") || (targetScale == "effectEstimate")) {
        if (is.na(information1)) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'information1' needs to be specified for 'sourceScale' or 'targetScale'  = 'effectEstimate'",
                call. = FALSE
            )
        }
    }
    if (
        (sourceScale == "conditionalPower") ||
            (targetScale == "conditionalPower")
        ) {
        if (is.null(design) || is.na(theta) || is.na(information2)) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'design', 'theta' and 'information2' need to be specified for
                        'sourceScale' or 'targetScale' = 'conditionalPower'",
                call. = FALSE
            )
        }
    }
    if (
        (sourceScale == "condPowerAtObserved") ||
            (targetScale == "condPowerAtObserved")
        ) {
        if (is.null(design) || is.na(information1) || is.na(information2)) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'design', 'information1', and 'information2' need to be specified for
                        'sourceScale' or 'targetScale' = 'condPowerAtObserved'",
                call. = FALSE
            )
        }
    }
    if (
        (sourceScale == "predictivePower") ||
            (targetScale == "predictivePower")
        ) {
        if (is.null(design) || is.na(information1) || is.na(information2)) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'design', 'information1', and 'information2' need to be specified for
                        'sourceScale' or 'targetScale' = 'predictivePower'",
                call. = FALSE
            )
        }
    }
    if (
        (sourceScale == "reverseCondPower") ||
            (targetScale == "reverseCondPower")
        ) {
        if (is.null(design)) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'design' need to be specified for
                        'sourceScale' or 'targetScale' = 'predictivePower'",
                call. = FALSE
            )
        } else {
            if (!.isTrialDesignGroupSequential(design)) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "design not correctly specified, needs to be one-sided two-stage group sequential",
                    call. = FALSE
                )
            }
        }
    }

    if (!is.null(design)) {
        if (.isTrialDesignInverseNormalOrGroupSequential(design)) {
            gsWeights <- c(sqrt(design$informationRates[1]), sqrt(1 - design$informationRates[1]))
        } else if (.isTrialDesignFisher(design)) {
            gsWeights <- c(1, sqrt((1 - design$informationRates[1]) / design$informationRates[1]))
        } else {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "design not correctly specified",
                call. = FALSE
            )
        }
        criticalValue <- design$criticalValues[2]
        if ((design$kMax != 2) || (design$sided != 1)) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "Futility bounds conversion is available only for one-sided two-stage designs",
                call. = FALSE
            )
        }
    }
    if (!is.na(information1)) {
        .assertIsSingleNumber(information1, "information1")
        .assertIsInOpenInterval(
            information1,
            "information1",
            lower = 0,
            upper = Inf
        )
    }
    if (!is.na(information2)) {
        .assertIsSingleNumber(information2, "information2")
        .assertIsInOpenInterval(
            information2,
            "information2",
            lower = 0,
            upper = Inf
        )
    }

    if (!is.na(theta)) {
        .assertIsSingleNumber(theta, "theta")
    }

    if (
        sourceScale %in% c("conditionalPower", "condPowerAtObserved", "predictivePower", "reverseCondPower", "pValue")
        ) {
        .assertIsInClosedInterval(
            sourceValue,
            "sourceValue",
            lower = 0,
            upper = 1,
            naAllowed = TRUE
        )
    }

    if (sourceScale == "zValue") {
        usedSource <- sourceValue
    } else if (sourceScale == "pValue") {
        usedSource <- qnorm(1 - sourceValue)
    } else if (sourceScale == "effectEstimate") {
        usedSource <- sourceValue * sqrt(information1)
    } else if (.isTrialDesignInverseNormalOrGroupSequential(design)) {
        if (sourceScale == "conditionalPower") {
            usedSource <- (criticalValue - gsWeights[2] * (qnorm(1 - sourceValue) + theta * sqrt(information2))) /
                gsWeights[1]
        } else if (sourceScale == "condPowerAtObserved") {
            usedSource <- (criticalValue /
                gsWeights[2] -
                qnorm(1 - sourceValue)) /
                (gsWeights[1] / gsWeights[2] + sqrt(information2 / information1))
        } else if (sourceScale == "predictivePower") {
            usedSource <- (criticalValue /
                gsWeights[2] -
                sqrt((information1 + information2) / information1) * qnorm(1 - sourceValue)) /
                (gsWeights[1] / gsWeights[2] + sqrt(information2 / information1))
        } else if (sourceScale == "reverseCondPower") {
            usedSource <- sqrt(1 - design$informationRates[1]) *
                qnorm(sourceValue) +
                sqrt(design$informationRates[1]) * criticalValue
        }
    } else if (.isTrialDesignFisher(design)) {
        if (sourceScale == "conditionalPower") {
            usedSource <- qnorm(
                1 - criticalValue /
                    pnorm((qnorm(sourceValue) - theta * sqrt(information2)))^gsWeights[2]
            )
        } else if (sourceScale == "condPowerAtObserved") {
            usedSource <- c()
            for (y in sourceValue) {
                result <- NA
                tryCatch(
                    {
                        result <- stats::uniroot(
                            function(x) {
                                pmin(
                                    1,
                                    pnorm(
                                        qnorm((criticalValue / (1 - pnorm(x)))^(1 / gsWeights[2])) +
                                            x * sqrt(information2 / information1)
                                    )
                                ) - y
                            },
                            lower = -2,
                            upper = qnorm(1 - criticalValue),
                            tol = .Machine$double.eps^0.5
                        )$root
                    },
                    error = function(e) {
                        warning("Failed to calculate sourceValue", e$message)
                    }
                )
                usedSource <- c(
                    usedSource,
                    result
                )
            }
        } else if (sourceScale == "predictivePower") {
            usedSource <- c()
            for (y in sourceValue) {
                result <- NA
                tryCatch(
                    {
                        result <- stats::uniroot(
                            function(x) {
                                pmin(
                                    1,
                                    pnorm(
                                        sqrt(information1 / (information1 + information2)) *
                                            (qnorm((criticalValue / (1 - pnorm(x)))^(1 / gsWeights[2])) +
                                                x * sqrt(information2 / information1))
                                    )
                                ) - y
                            },
                            lower = -2,
                            upper = qnorm(1 - criticalValue),
                            tol = .Machine$double.eps^0.5
                        )$root
                    },
                    error = function(e) {
                        warning("Failed to calculate sourceValue", e$message)
                        result <- NA
                    }
                )
                usedSource <- c(
                    usedSource,
                    result
                )
            }
        }
    }

    if (targetScale == "zValue") {
        return(usedSource)
    } else if (targetScale == "pValue") {
        return(1 - pnorm(usedSource))
    } else if (targetScale == "effectEstimate") {
        return(usedSource / sqrt(information1))
    } else if (.isTrialDesignInverseNormalOrGroupSequential(design)) {
        if (targetScale == "conditionalPower") {
            return(pmax(
                1e-20,
                1 - pnorm(
                    (criticalValue - gsWeights[1] * usedSource) /
                        gsWeights[2] -
                        theta * sqrt(information2)
                )
            ))
        } else if (targetScale == "condPowerAtObserved") {
            return(pmax(
                1e-20,
                1 - pnorm(
                    (criticalValue - gsWeights[1] * usedSource) /
                        gsWeights[2] -
                        usedSource * sqrt(information2 / information1)
                )
            ))
        } else if (targetScale == "predictivePower") {
            return(pmax(
                1e-20,
                1 - pnorm(
                    sqrt(information1 / (information1 + information2)) *
                        ((criticalValue - gsWeights[1] * usedSource) /
                            gsWeights[2] -
                            usedSource * sqrt(information2 / information1))
                )
            ))
        } else if (targetScale == "reverseCondPower") {
            return(pmax(
                1e-20,
                pnorm(
                    (usedSource - sqrt(design$informationRates[1]) * criticalValue) /
                        sqrt(1 - design$informationRates[1])
                )
            ))
        }
    } else if (.isTrialDesignFisher(design)) {
        if (targetScale == "conditionalPower") {
            return(pmax(
                1e-20,
                pnorm(
                    qnorm((criticalValue / (1 - pnorm(usedSource)))^(1 / gsWeights[2])) +
                        theta * sqrt(information2)
                )
            ))
        } else if (targetScale == "condPowerAtObserved") {
            return(pmax(
                1e-20,
                pnorm(
                    qnorm((criticalValue / (1 - pnorm(usedSource)))^(1 / gsWeights[2])) +
                        usedSource * sqrt(information2 / information1)
                )
            ))
        } else if (targetScale == "predictivePower") {
            return(pmax(
                1e-20,
                pnorm(
                    sqrt(information1 / (information1 + information2)) *
                        (qnorm((criticalValue / (1 - pnorm(usedSource)))^(1 / gsWeights[2])) +
                            usedSource * sqrt(information2 / information1))
                )
            ))
        }
    }
    return(NULL)
}

.hideFutilityStopsIfNotApplicable <- function(designPlan) {
    if (all(designPlan$.design$futilityBounds == C_FUTILITY_BOUNDS_DEFAULT)) {
        designPlan$.setParameterType("futilityStop", C_PARAM_NOT_APPLICABLE)
        designPlan$.setParameterType("futilityPerStage", C_PARAM_NOT_APPLICABLE)
    }
}

.getSingleEventsPerStage <- function(cumulativeEventsPerStage) {
    singleEventsPerStage <- matrix(
        nrow = nrow(cumulativeEventsPerStage),
        ncol = ncol(cumulativeEventsPerStage)
    )
    if (nrow(cumulativeEventsPerStage) > 1) {
        for (i in nrow(cumulativeEventsPerStage):2) {
            singleEventsPerStage[i, ] <- cumulativeEventsPerStage[i, ] -
                cumulativeEventsPerStage[i - 1, ]
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
        if (designPlan$.isSampleSizeObject()) {
            # comes from getSampleSize
            if (designPlan$groups == 1) {
                designPlan$directionUpper <- (designPlan$pi1 > designPlan$thetaH0)
            } else {
                if (designPlan$riskRatio) {
                    designPlan$directionUpper <-
                        (designPlan$pi1 / designPlan$pi2 > designPlan$thetaH0)
                } else {
                    designPlan$directionUpper <-
                        (designPlan$pi1 - designPlan$pi2 > designPlan$thetaH0)
                }
            }
            designPlan$.setParameterType("directionUpper", C_PARAM_GENERATED)
        }
        if (design$kMax == 1 && designPlan$.isSampleSizeObject()) {
            designPlan$maxNumberOfSubjects <- designPlan$nFixed
        }
        boundaries <- .getEffectScaleBoundaryDataRates(designPlan)
    } else if (.isTrialDesignPlanSurvival(designPlan)) {
        if (designPlan$.isSampleSizeObject()) {
            # comes from getSampleSize
            designPlan$directionUpper <- (designPlan$hazardRatio > designPlan$thetaH0)
            designPlan$.setParameterType("directionUpper", C_PARAM_GENERATED)
        }

        if (design$kMax == 1 && designPlan$.isSampleSizeObject()) {
            designPlan$cumulativeEventsPerStage <- matrix(designPlan$eventsFixed, nrow = 1)
        }
        boundaries <- .getEffectScaleBoundaryDataSurvival(designPlan)
    } else if (.isTrialDesignPlanCountData(designPlan)) {
        boundaries <- .getEffectScaleBoundaryDataCounts(designPlan)
    }

    if (designPlan$.design$sided == 1) {
        designPlan$criticalValuesEffectScale <- boundaries$criticalValuesEffectScaleUpper
        designPlan$.setParameterType("criticalValuesEffectScale", C_PARAM_GENERATED)
    } else {
        if (all(boundaries$criticalValuesEffectScaleLower <
                boundaries$criticalValuesEffectScaleUpper, na.rm = TRUE)) {
            designPlan$criticalValuesEffectScaleLower <- boundaries$criticalValuesEffectScaleLower
            designPlan$criticalValuesEffectScaleUpper <- boundaries$criticalValuesEffectScaleUpper
        } else {
            designPlan$criticalValuesEffectScaleLower <- boundaries$criticalValuesEffectScaleUpper
            designPlan$criticalValuesEffectScaleUpper <- boundaries$criticalValuesEffectScaleLower
        }
        designPlan$.setParameterType("criticalValuesEffectScaleUpper", C_PARAM_GENERATED)
        designPlan$.setParameterType("criticalValuesEffectScaleLower", C_PARAM_GENERATED)
    }

    if (.hasApplicableFutilityBounds(design)) {
        if (design$sided == 1) {
            designPlan$futilityBoundsEffectScale <- 
                round(boundaries$futilityBoundsEffectScaleUpper, 8)
            designPlan$.setParameterType("futilityBoundsEffectScale", C_PARAM_GENERATED)
        } else {
            if (all(designPlan$futilityBoundsEffectScaleLower <
                    designPlan$futilityBoundsEffectScaleUpper, na.rm = TRUE)) {
                designPlan$futilityBoundsEffectScaleLower <- 
                    round(boundaries$futilityBoundsEffectScaleLower, 8)
                designPlan$futilityBoundsEffectScaleUpper <- 
                    round(boundaries$futilityBoundsEffectScaleUpper, 8)
            } else {
                designPlan$futilityBoundsEffectScaleLower <- 
                    round(boundaries$futilityBoundsEffectScaleUpper, 8)
                designPlan$futilityBoundsEffectScaleUpper <- 
                    round(boundaries$futilityBoundsEffectScaleLower, 8)
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
            conservative = designPlan$conservative,
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

    if (
        !is.null(sampleSizeFixed$allocationRatioPlanned) &&
            (length(designPlan$allocationRatioPlanned) != length(sampleSizeFixed$allocationRatioPlanned) ||
                sum(designPlan$allocationRatioPlanned == sampleSizeFixed$allocationRatioPlanned) !=
                    length(designPlan$allocationRatioPlanned))
        ) {
        designPlan$allocationRatioPlanned <- sampleSizeFixed$allocationRatioPlanned
        designPlan$.setParameterType("allocationRatioPlanned", C_PARAM_GENERATED)
    }

    # Sequential
    if (designPlan$.design$kMax > 1) {
        designCharacteristics <- getDesignCharacteristics(designPlan$.design)
        if (.isTrialDesignPlanMeans(designPlan)) {
            sampleSizeSequential <- .getSampleSizeSequentialMeans(
                sampleSizeFixed,
                designCharacteristics
            )
        } else {
            sampleSizeSequential <- .getSampleSizeSequentialRates(
                sampleSizeFixed,
                designCharacteristics
            )
        }

        designPlan$informationRates <- sampleSizeSequential$informationRates
        if (
            ncol(designPlan$informationRates) == 1 &&
                identical(designPlan$informationRates[, 1], designPlan$.design$informationRates)
            ) {
            designPlan$.setParameterType("informationRates", C_PARAM_NOT_APPLICABLE)
        } else {
            designPlan$.setParameterType("informationRates", C_PARAM_GENERATED)
        }

        designPlan$maxNumberOfSubjects <- sampleSizeSequential$maxNumberOfSubjects
        designPlan$.setParameterType("maxNumberOfSubjects", C_PARAM_GENERATED)
        if (designPlan$groups == 2) {
            designPlan$maxNumberOfSubjects1 <- .getNumberOfSubjects1(
                designPlan$maxNumberOfSubjects,
                designPlan$allocationRatioPlanned
            )
            designPlan$maxNumberOfSubjects2 <- .getNumberOfSubjects2(
                designPlan$maxNumberOfSubjects,
                designPlan$allocationRatioPlanned
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
                nrow = designPlan$.design$kMax)
            designPlan$.setParameterType("rejectPerStage", C_PARAM_GENERATED)

            designPlan$earlyStop <- sum(designPlan$rejectPerStage[1:(designPlan$.design$kMax - 1), ])
            designPlan$.setParameterType("earlyStop", C_PARAM_GENERATED)
        }
        if (
            !is.null(sampleSizeSequential$futilityPerStage) &&
                any(designPlan$.design$futilityBounds != C_FUTILITY_BOUNDS_DEFAULT, na.rm = TRUE)
            ) {
            designPlan$futilityPerStage <- matrix(
                sampleSizeSequential$futilityPerStage,
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
        ml <- .getFarringtonManningValuesDiff(
            rate1 = rate1, rate2 = rate2, theta = theta, allocation = allocation)
    } else {
        ml <- .getFarringtonManningValuesRatio(
            rate1 = rate1, rate2 = rate2, theta = theta, allocation = allocation)
    }
    return(list(theta = theta, method = method, ml1 = ml[1], ml2 = ml[2]))
}

.getPiecewiseExpStartTimesWithoutLeadingZero <- function(piecewiseSurvivalTime) {
    if (is.null(piecewiseSurvivalTime) || length(piecewiseSurvivalTime) == 0 || all(is.na(piecewiseSurvivalTime))) {
        return(NA_real_)
    }

    if (piecewiseSurvivalTime[1] != 0) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "the first value of 'piecewiseSurvivalTime' (",
            .arrayToString(piecewiseSurvivalTime),
            ") must be 0",
            call. = FALSE
        )
    }

    if (length(piecewiseSurvivalTime) == 1) {
        return(numeric(0))
    }

    if (length(piecewiseSurvivalTime) < 2) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "length of 'piecewiseSurvivalTime' (",
            length(piecewiseSurvivalTime),
            ") must be > 1"
        )
    }

    return(piecewiseSurvivalTime[2:length(piecewiseSurvivalTime)])
}

.getNumberOfSubjectsInner <- function(..., 
        timeValue, accrualTime, accrualIntensity, maxNumberOfSubjects) {
    .assertIsSingleNumber(timeValue, "timeValue")
    if (length(accrualTime) != length(accrualIntensity)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "length of 'accrualTime' (",
            length(accrualIntensity),
            ") ",
            "must be equel to length of 'accrualIntensity' (",
            length(accrualIntensity),
            ")"
        )
    }

    densityIntervals <- accrualTime
    if (length(accrualTime) > 1) {
        densityIntervals[2:length(accrualTime)] <- accrualTime[2:length(accrualTime)] -
            accrualTime[1:(length(accrualTime) - 1)]
    }
    densityVector <- accrualIntensity / sum(densityIntervals * accrualIntensity)
    for (l in seq_len(length(densityVector))) {
        if (timeValue <= accrualTime[l]) {
            if (l == 1) {
                return(timeValue * densityVector[l] * maxNumberOfSubjects)
            } else {
                return(
                    (sum(densityVector[1:(l - 1)] * densityIntervals[1:(l - 1)]) +
                        (timeValue - accrualTime[l - 1]) * densityVector[l]) *
                        maxNumberOfSubjects
                )
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
            design$informationRates[1:(design$kMax - 1)]) *
            designPlan$maxNumberOfSubjects
    }

    designPlan$numberOfSubjects <- .getColumnCumSum(designPlan$numberOfSubjects)

    designPlan$numberOfSubjects1 <- .getNumberOfSubjects1(
        designPlan$numberOfSubjects,
        designPlan$allocationRatioPlanned
    )
    designPlan$numberOfSubjects2 <- .getNumberOfSubjects2(
        designPlan$numberOfSubjects,
        designPlan$allocationRatioPlanned
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
