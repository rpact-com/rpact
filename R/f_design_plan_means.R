## |
## |  *Sample size and power means*
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
## |  File version: $Revision: 8674 $
## |  Last changed: $Date: 2025-04-10 15:45:44 +0200 (Do, 10 Apr 2025) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_core_utilities.R
NULL

.getEffectScaleBoundaryDataMeans <- function(designPlan) {
    design <- designPlan$.design
    thetaH0 <- designPlan$thetaH0
    stDev <- designPlan$stDev
    maxNumberOfSubjects <- designPlan$maxNumberOfSubjects
    allocationRatioPlanned <- designPlan$allocationRatioPlanned
    directionUpper <- designPlan$directionUpper

    # initialize effect scale matrix
    futilityBoundsEffectScaleUpper <- rep(NA_real_, design$kMax - 1)
    futilityBoundsEffectScaleLower <- rep(NA_real_, design$kMax - 1)

    if (designPlan$normalApproximation) {
        criticalValues <- .getCriticalValues(design)
        futilityBounds <- design$futilityBounds
    } else {
        criticalValues <- stats::qt(
            1 - design$stageLevels,
            pmax(design$informationRates %*% t(maxNumberOfSubjects) - designPlan$groups, 1e-04)
        )

        # outside validated range
        criticalValues[criticalValues > 50] <- NA_real_
        if (design$typeOfDesign == "noEarlyEfficacy") {
            if ((is.matrix(criticalValues) && any(is.na(criticalValues[design$kMax, ]))) ||
                    (is.vector(criticalValues) && is.na(criticalValues[design$kMax]))) {
                warning("The computation of last stage efficacy boundary on treatment ",
                    "effect scale not performed presumably ",
                    "due to too small degrees of freedom",
                    call. = FALSE
                )
            }
        } else {
            numberOfNAs <- sum(is.na(criticalValues))
            if (numberOfNAs > 0) {
                warning("The computation of ", .integerToWrittenNumber(numberOfNAs), " ",
                    "efficacy boundar", ifelse(numberOfNAs == 1, "y", "ies"), " ",
                    "on treatment effect scale not performed presumably ",
                    "due to too small degrees of freedom",
                    call. = FALSE
                )
            }
        }

        if (any(design$futilityBounds > C_FUTILITY_BOUNDS_DEFAULT, na.rm = TRUE)) {
            futilityBounds <- stats::qt(
                stats::pnorm(design$futilityBounds),
                pmax(design$informationRates[1:(design$kMax - 1)] %*%
                    t(maxNumberOfSubjects) - designPlan$groups, 1e-04)
            )

            # outside validated range
            futilityBounds[abs(futilityBounds) > 50] <- NA_real_
            if (any(is.na(futilityBounds))) {
                warning("The computation of futility boundaries on ",
                    "treatment effect scale not performed presumably ",
                    "due to too small degrees of freedom",
                    call. = FALSE
                )
            }
        } else {
            futilityBounds <- design$futilityBounds
        }
    }
    futilityBounds[!is.na(futilityBounds) & futilityBounds <= C_FUTILITY_BOUNDS_DEFAULT] <- NA_real_

    if ((length(stDev) == 1) && (designPlan$groups == 2)) {
        stDev <- rep(stDev, 2)
    }

    if (designPlan$groups == 1) {
        criticalValuesEffectScaleUpper <- thetaH0 + criticalValues * stDev /
            sqrt(design$informationRates %*% t(maxNumberOfSubjects))
        criticalValuesEffectScaleLower <- thetaH0 - criticalValues * stDev /
            sqrt(design$informationRates %*% t(maxNumberOfSubjects))
        if (!.isTrialDesignFisher(design) && !all(is.na(futilityBounds))) {
            futilityBoundsEffectScaleUpper <- thetaH0 + futilityBounds * stDev /
                sqrt(design$informationRates[1:(design$kMax - 1)] %*% t(maxNumberOfSubjects))
        }
        if (!.isTrialDesignFisher(design) && design$sided == 2 && design$kMax > 1 &&
                (design$typeOfDesign == C_TYPE_OF_DESIGN_PT ||
                    !is.null(design$typeBetaSpending) && design$typeBetaSpending != "none")) {
            futilityBoundsEffectScaleLower <- thetaH0 - futilityBounds * stDev /
                sqrt(design$informationRates[1:(design$kMax - 1)] %*% t(maxNumberOfSubjects))
        }
    } else if (!designPlan$meanRatio) {
        criticalValuesEffectScaleUpper <- thetaH0 + criticalValues *
            sqrt(1 + allocationRatioPlanned) *
            sqrt(stDev[1]^2 + allocationRatioPlanned * stDev[2]^2) /
            (sqrt(allocationRatioPlanned *
                design$informationRates %*% t(maxNumberOfSubjects)))
        criticalValuesEffectScaleLower <- thetaH0 - criticalValues *
            sqrt(1 + allocationRatioPlanned) *
            sqrt(stDev[1]^2 + allocationRatioPlanned * stDev[2]^2) /
            (sqrt(allocationRatioPlanned *
                design$informationRates %*% t(maxNumberOfSubjects)))
        if (!.isTrialDesignFisher(design) && !all(is.na(futilityBounds))) {
            futilityBoundsEffectScaleUpper <- thetaH0 + futilityBounds *
                sqrt(1 + allocationRatioPlanned) *
                sqrt(stDev[1]^2 + allocationRatioPlanned * stDev[2]^2) /
                (sqrt(allocationRatioPlanned *
                    design$informationRates[1:(design$kMax - 1)] %*% t(maxNumberOfSubjects)))
        }
        if (!.isTrialDesignFisher(design) && design$sided == 2 && design$kMax > 1 &&
                (design$typeOfDesign == C_TYPE_OF_DESIGN_PT ||
                    !is.null(design$typeBetaSpending) && design$typeBetaSpending != "none")) {
            futilityBoundsEffectScaleLower <- thetaH0 - futilityBounds *
                stDev * (1 + allocationRatioPlanned) /
                (sqrt(allocationRatioPlanned *
                    design$informationRates[1:(design$kMax - 1)] %*% t(maxNumberOfSubjects)))
        }
    } else {
        criticalValuesEffectScaleUpper <- thetaH0 + criticalValues *
            sqrt((1 + allocationRatioPlanned) / allocationRatioPlanned) *
            sqrt(stDev[1]^2 + thetaH0^2 * allocationRatioPlanned * stDev[2]^2) /
            (sqrt(design$informationRates %*% t(maxNumberOfSubjects)))
        criticalValuesEffectScaleLower <- thetaH0 - criticalValues *
            sqrt((1 + allocationRatioPlanned) / allocationRatioPlanned) *
            sqrt(stDev[1]^2 + thetaH0^2 * allocationRatioPlanned * stDev[2]^2) /
            (sqrt(design$informationRates %*% t(maxNumberOfSubjects)))

        if (!.isTrialDesignFisher(design) && !all(is.na(futilityBounds))) {
            futilityBoundsEffectScaleUpper <- thetaH0 + futilityBounds *
                sqrt((1 + allocationRatioPlanned) / allocationRatioPlanned) *
                sqrt(stDev[1]^2 + thetaH0^2 * allocationRatioPlanned * stDev[2]^2) /
                (sqrt(design$informationRates[1:(design$kMax - 1)] %*% t(maxNumberOfSubjects)))
        }
        if (!.isTrialDesignFisher(design) && design$sided == 2 && design$kMax > 1 &&
                (design$typeOfDesign == C_TYPE_OF_DESIGN_PT ||
                    !is.null(design$typeBetaSpending) && design$typeBetaSpending != "none")) {
            futilityBoundsEffectScaleLower <- thetaH0 - futilityBounds *
                sqrt((1 + allocationRatioPlanned) / allocationRatioPlanned) *
                sqrt(stDev[1]^2 + thetaH0^2 * allocationRatioPlanned * stDev[2]^2) /
                (sqrt(design$informationRates[1:(design$kMax - 1)] %*% t(maxNumberOfSubjects)))
        }
    }

    directionUpper[is.na(directionUpper)] <- TRUE
    if (length(directionUpper) > 0 && all(!directionUpper)) {
        criticalValuesEffectScaleUpper <- -criticalValuesEffectScaleUpper + 2 * thetaH0
        criticalValuesEffectScaleLower <- -criticalValuesEffectScaleLower + 2 * thetaH0
        if (!all(is.na(futilityBoundsEffectScaleUpper))) {
            futilityBoundsEffectScaleUpper <- -futilityBoundsEffectScaleUpper + 2 * thetaH0
            futilityBoundsEffectScaleLower <- -futilityBoundsEffectScaleLower + 2 * thetaH0
        }
    }
    if (designPlan$meanRatio) {
        criticalValuesEffectScaleUpper[!is.na(criticalValuesEffectScaleUpper) &
            criticalValuesEffectScaleUpper <= 0] <- NA_real_
        criticalValuesEffectScaleLower[!is.na(criticalValuesEffectScaleLower) &
            criticalValuesEffectScaleLower <= 0] <- NA_real_
        futilityBoundsEffectScaleUpper[!is.na(futilityBoundsEffectScaleUpper) &
            futilityBoundsEffectScaleUpper <= 0] <- NA_real_
        futilityBoundsEffectScaleLower[!is.na(futilityBoundsEffectScaleLower) &
            futilityBoundsEffectScaleLower <= 0] <- NA_real_
    }

    return(list(
        criticalValuesEffectScaleUpper = matrix(
            criticalValuesEffectScaleUpper,
            nrow = design$kMax
        ),
        criticalValuesEffectScaleLower = matrix(
            criticalValuesEffectScaleLower,
            nrow = design$kMax
        ),
        futilityBoundsEffectScaleUpper = matrix(
            futilityBoundsEffectScaleUpper,
            nrow = design$kMax - 1
        ),
        futilityBoundsEffectScaleLower = matrix(
            futilityBoundsEffectScaleLower,
            nrow = design$kMax - 1
        )
    ))
}

.getSampleSizeFixedMeans <- function(...,
        alpha = 0.025,
        beta = 0.2,
        sided = 1,
        twoSidedPower = C_TWO_SIDED_POWER_DEFAULT,
        normalApproximation = FALSE,
        meanRatio = FALSE,
        thetaH0 = 0,
        alternative = C_ALTERNATIVE_DEFAULT,
        stDev = C_STDEV_DEFAULT,
        groups = 2,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT) {
    nFixed <- rep(NA_real_, length(alternative))

    if ((length(stDev) == 1) && (groups == 2)) {
        stDev <- rep(stDev, 2)
    }

    for (i in seq_len(length(alternative))) {
        theta <- alternative[i]

        if (groups == 1) {
            if (sided == 1 || !twoSidedPower) {
                if (!normalApproximation) {
                    up <- 2
                    while (stats::pt(
                        stats::qt(1 - alpha / sided, up - 1), max(0.001, up - 1),
                        sqrt(up) * abs(theta - thetaH0) / stDev
                    ) > beta) {
                        up <- 2 * up
                    }
                    nFixed[i] <- .getOneDimensionalRoot(
                        function(n) {
                            return(stats::pt(
                                stats::qt(1 - alpha / sided, max(0.001, n - 1)),
                                max(0.001, n - 1), sqrt(n) * abs(theta - thetaH0) / stDev
                            ) - beta)
                        },
                        lower = 0.001, upper = up, tolerance = 1e-04,
                        callingFunctionInformation = ".getSampleSizeFixedMeans"
                    )
                } else {
                    nFixed[i] <- (.getOneMinusQNorm(alpha / sided) +
                        .getOneMinusQNorm(beta))^2 / ((theta - thetaH0) / stDev)^2
                }
            } else {
                up <- 2
                while (stats::pt(
                    stats::qt(1 - alpha / 2, max(0.001, up - 1)), max(0.001, up - 1),
                    sqrt(up) * (theta - thetaH0) / stDev
                ) -
                    stats::pt(
                        -stats::qt(1 - alpha / 2, max(0.001, up - 1)),
                        max(0.001, up - 1), sqrt(up) * (theta - thetaH0) / stDev
                    ) > beta) {
                    up <- 2 * up
                }
                if (!normalApproximation) {
                    nFixed[i] <- .getOneDimensionalRoot(
                        function(n) {
                            return(stats::pt(
                                stats::qt(1 - alpha / 2, max(0.001, n - 1)), max(0.001, n - 1),
                                sqrt(n) * (theta - thetaH0) / stDev
                            ) - stats::pt(
                                -stats::qt(1 - alpha / 2, max(0.001, n - 1)),
                                max(0.001, n - 1), sqrt(n) * (theta - thetaH0) / stDev
                            ) - beta)
                        },
                        lower = 0.001, upper = up, tolerance = 1e-04,
                        callingFunctionInformation = ".getSampleSizeFixedMeans"
                    )
                } else {
                    nFixed[i] <- .getOneDimensionalRoot(
                        function(n) {
                            return(stats::pnorm(.getOneMinusQNorm(alpha / 2) -
                                sqrt(n) * (theta - thetaH0) / stDev) -
                                stats::pnorm(-.getOneMinusQNorm(alpha / 2) - sqrt(n) * (theta - thetaH0) / stDev) - beta)
                        },
                        lower = 0.001, upper = up, tolerance = 1e-04,
                        callingFunctionInformation = ".getSampleSizeFixedMeans"
                    )
                }
            }
        } else if (groups == 2) {
            if (sided == 1 || !twoSidedPower) {
                if (!meanRatio) {
                    # allocationRatioPlanned = 0 provides optimum sample size
                    if (allocationRatioPlanned == 0) {
                        allocationRatioPlanned <- stDev[1] / stDev[2]
                    }
                    if (!normalApproximation) {
                        up <- 2
                        while (stats::pt(
                            stats::qt(1 - alpha / sided, up *
                                (1 + allocationRatioPlanned) - 2),
                            up * (1 + allocationRatioPlanned) - 2,
                            sqrt(up) *
                                abs(theta - thetaH0) /
                                sqrt(stDev[1]^2 / allocationRatioPlanned + stDev[2]^2)
                        ) > beta) {
                            up <- 2 * up
                        }
                        n2Fixed <- .getOneDimensionalRoot(
                            function(n2) {
                                if (stDev[1] == stDev[2]) {
                                    df <- max(0.001, n2 * (1 + allocationRatioPlanned) - 2)
                                } else {
                                    n1 <- allocationRatioPlanned * n2
                                    u <- stDev[1]^2 / n1 / (stDev[1]^2 / n1 + stDev[2]^2 / n2)
                                    df <- max(0.001, 1 / (u^2 / (n1 - 1) + (1 - u)^2 / (n2 - 1)))
                                }
                                return(stats::pt(
                                    stats::qt(1 - alpha / sided, df), df,
                                    sqrt(n2) * abs(theta - thetaH0) /
                                        sqrt(stDev[1]^2 / allocationRatioPlanned + stDev[2]^2)
                                ) - beta)
                            },
                            lower = 0.001, upper = up, tolerance = 1e-04,
                            callingFunctionInformation = ".getSampleSizeFixedMeans"
                        )
                        nFixed[i] <- n2Fixed * (1 + allocationRatioPlanned)
                    } else {
                        nFixed[i] <- (1 + allocationRatioPlanned) *
                            (stDev[1]^2 + allocationRatioPlanned * stDev[2]^2) / allocationRatioPlanned *
                            (.getOneMinusQNorm(alpha / sided) + .getOneMinusQNorm(beta))^2 /
                            (theta - thetaH0)^2
                    }
                } else {
                    # allocationRatioPlanned = 0 provides optimum sample size
                    if (allocationRatioPlanned == 0) {
                        allocationRatioPlanned <- 1 / thetaH0 * stDev[1] / stDev[2]
                    }
                    if (!normalApproximation) {
                        up <- 2
                        while (stats::pt(
                            stats::qt(
                                1 - alpha / sided,
                                up * (1 + allocationRatioPlanned) - 2
                            ),
                            up * (1 + allocationRatioPlanned) - 2,
                            sqrt(up) * abs(theta - thetaH0) /
                                sqrt(stDev[1]^2 / allocationRatioPlanned + stDev[2]^2 * thetaH0^2)
                        ) > beta) {
                            up <- 2 * up
                        }
                        n2Fixed <- .getOneDimensionalRoot(
                            function(n2) {
                                if (stDev[1] == stDev[2]) {
                                    df <- max(0.001, n2 * (1 + allocationRatioPlanned) - 2)
                                } else {
                                    n1 <- allocationRatioPlanned * n2
                                    u <- stDev[1]^2 / n1 / (stDev[1]^2 / n1 + stDev[2]^2 / n2)
                                    df <- max(0.001, 1 / (u^2 / (n1 - 1) + (1 - u)^2 / (n2 - 1)))
                                }
                                return(stats::pt(
                                    stats::qt(1 - alpha / sided, df), df,
                                    sqrt(n2) * abs(theta - thetaH0) /
                                        sqrt(stDev[1]^2 / allocationRatioPlanned + stDev[2]^2 * thetaH0^2)
                                ) - beta)
                            },
                            lower = 0.001, upper = up, tolerance = 1e-04,
                            callingFunctionInformation = ".getSampleSizeFixedMeans"
                        )
                        nFixed[i] <- n2Fixed * (1 + allocationRatioPlanned)
                    } else {
                        nFixed[i] <- (1 + allocationRatioPlanned) *
                            (stDev[1]^2 / allocationRatioPlanned + thetaH0^2 * stDev[2]^2) *
                            (.getOneMinusQNorm(alpha / sided) + .getOneMinusQNorm(beta))^2 /
                            (theta - thetaH0)^2
                    }
                }
            } else {
                if (!normalApproximation) {
                    if (allocationRatioPlanned == 0) {
                        allocationRatioPlanned <- stDev[1] / stDev[2]
                    }
                    up <- 2
                    while (stats::pt(
                        stats::qt(
                            1 - alpha / 2,
                            up * (1 + allocationRatioPlanned) - 2
                        ),
                        up * (1 + allocationRatioPlanned) - 2,
                        sqrt(up) * (theta - thetaH0) /
                            sqrt(stDev[1]^2 / allocationRatioPlanned + stDev[2]^2)
                    ) - stats::pt(
                        -stats::qt(
                            1 - alpha / 2,
                            up * (1 + allocationRatioPlanned) - 2
                        ),
                        up * (1 + allocationRatioPlanned) - 2,
                        sqrt(up) * (theta - thetaH0) /
                            sqrt(stDev[1]^2 / allocationRatioPlanned + stDev[2]^2)
                    ) > beta) {
                        up <- 2 * up
                    }
                    n2Fixed <- .getOneDimensionalRoot(
                        function(n2) {
                            if (stDev[1] == stDev[2]) {
                                df <- max(0.001, n2 * (1 + allocationRatioPlanned) - 2)
                            } else {
                                n1 <- allocationRatioPlanned * n2
                                u <- stDev[1]^2 / n1 / (stDev[1]^2 / n1 + stDev[2]^2 / n2)
                                df <- max(0.001, 1 / (u^2 / (n1 - 1) + (1 - u)^2 / (n2 - 1)))
                            }
                            return(stats::pt(
                                stats::qt(1 - alpha / 2, df), df,
                                sqrt(n2) * (theta - thetaH0) /
                                    sqrt(stDev[1]^2 / allocationRatioPlanned + stDev[2]^2)
                            ) - stats::pt(
                                -stats::qt(
                                    1 - alpha / 2, df
                                ), df,
                                sqrt(n2) * (theta - thetaH0) /
                                    sqrt(stDev[1]^2 / allocationRatioPlanned + stDev[2]^2)
                            ) - beta)
                        },
                        lower = 0.001, upper = up, tolerance = 1e-04,
                        callingFunctionInformation = ".getSampleSizeFixedMeans"
                    )
                    nFixed[i] <- n2Fixed * (1 + allocationRatioPlanned)
                } else {
                    up <- 2
                    while (stats::pnorm(.getOneMinusQNorm(alpha / 2) -
                        sqrt(up) * (theta - thetaH0) /
                            sqrt(stDev[1]^2 / allocationRatioPlanned + stDev[2]^2)) -
                        stats::pnorm(-.getOneMinusQNorm(alpha / 2) -
                            sqrt(up) * (theta - thetaH0) /
                                sqrt(stDev[1]^2 / allocationRatioPlanned + stDev[2]^2))
                    > beta) {
                        up <- 2 * up
                    }
                    n2Fixed <-
                        .getOneDimensionalRoot(
                            function(n2) {
                                return(stats::pnorm(.getOneMinusQNorm(alpha / 2) -
                                    sqrt(n2) * (theta - thetaH0) /
                                        sqrt(stDev[1]^2 / allocationRatioPlanned + stDev[2]^2)) -
                                    stats::pnorm(-.getOneMinusQNorm(alpha / 2) -
                                        sqrt(n2) * (theta - thetaH0) /
                                            sqrt(stDev[1]^2 / allocationRatioPlanned + stDev[2]^2))
                                    - beta)
                            },
                            lower = 0.001, upper = up, tolerance = 1e-04,
                            callingFunctionInformation = ".getSampleSizeFixedMeans"
                        )
                    nFixed[i] <- n2Fixed * (1 + allocationRatioPlanned)
                }
            }
        }
    }

    if (groups == 1) {
        return(list(
            alpha = alpha,
            beta = beta,
            sided = sided,
            groups = groups,
            thetaH0 = thetaH0,
            alternative = alternative,
            stDev = stDev,
            normalApproximation = normalApproximation,
            nFixed = nFixed
        ))
    } else if (groups == 2) {
        n1Fixed <- nFixed * allocationRatioPlanned / (1 + allocationRatioPlanned)
        n2Fixed <- n1Fixed / allocationRatioPlanned
        return(list(
            alpha = alpha,
            beta = beta,
            sided = sided,
            groups = groups,
            allocationRatioPlanned = allocationRatioPlanned,
            thetaH0 = thetaH0,
            meanRatio = meanRatio,
            alternative = alternative,
            stDev = stDev,
            normalApproximation = normalApproximation,
            n1Fixed = n1Fixed,
            n2Fixed = n2Fixed,
            nFixed = nFixed
        ))
    }
}

.getSampleSizeSequentialMeans <- function(fixedSampleSize, designCharacteristics) {
    kMax <- designCharacteristics$.design$kMax
    numberOfSubjects <- matrix(NA_real_, kMax, length(fixedSampleSize$alternative))
    numberOfSubjects1 <- matrix(NA_real_, kMax, length(fixedSampleSize$alternative))
    numberOfSubjects2 <- matrix(NA_real_, kMax, length(fixedSampleSize$alternative))
    maxNumberOfSubjects <- rep(NA_real_, length(fixedSampleSize$alternative))
    expectedNumberOfSubjectsH0 <- rep(NA_real_, length(fixedSampleSize$alternative))
    expectedNumberOfSubjectsH01 <- rep(NA_real_, length(fixedSampleSize$alternative))
    expectedNumberOfSubjectsH1 <- rep(NA_real_, length(fixedSampleSize$alternative))

    informationRates <- designCharacteristics$information / designCharacteristics$shift

    for (i in seq_len(length(fixedSampleSize$alternative))) {
        maxNumberOfSubjects[i] <- fixedSampleSize$nFixed[i] * designCharacteristics$inflationFactor

        numberOfSubjects[, i] <- maxNumberOfSubjects[i] *
            c(informationRates[1], (informationRates[2:kMax] - informationRates[1:(kMax - 1)]))

        expectedNumberOfSubjectsH0[i] <- designCharacteristics$averageSampleNumber0 *
            fixedSampleSize$nFixed[i]
        expectedNumberOfSubjectsH01[i] <- designCharacteristics$averageSampleNumber01 *
            fixedSampleSize$nFixed[i]
        expectedNumberOfSubjectsH1[i] <- designCharacteristics$averageSampleNumber1 *
            fixedSampleSize$nFixed[i]

        if (fixedSampleSize$groups == 2) {
            if (length(fixedSampleSize$allocationRatioPlanned) > 1) {
                allocationRatioPlanned <- fixedSampleSize$allocationRatioPlanned[i]
            } else {
                allocationRatioPlanned <- fixedSampleSize$allocationRatioPlanned
            }
            numberOfSubjects1[, i] <- numberOfSubjects[, i] * allocationRatioPlanned /
                (1 + allocationRatioPlanned)
            numberOfSubjects2[, i] <- numberOfSubjects[, i] / (1 + allocationRatioPlanned)
        }
    }

    if (fixedSampleSize$groups == 1) {
        return(list(
            alpha = fixedSampleSize$alpha,
            beta = fixedSampleSize$beta,
            sided = fixedSampleSize$sided,
            groups = fixedSampleSize$groups,
            thetaH0 = fixedSampleSize$thetaH0,
            alternative = fixedSampleSize$alternative,
            stDev = fixedSampleSize$stDev,
            normalApproximation = fixedSampleSize$normalApproximation,
            informationRates = matrix(informationRates, ncol = 1),
            maxNumberOfSubjects = maxNumberOfSubjects,
            numberOfSubjects = .getColumnCumSum(numberOfSubjects),
            expectedNumberOfSubjectsH0 = expectedNumberOfSubjectsH0,
            expectedNumberOfSubjectsH01 = expectedNumberOfSubjectsH01,
            expectedNumberOfSubjectsH1 = expectedNumberOfSubjectsH1,
            rejectPerStage = designCharacteristics$rejectionProbabilities,
            futilityPerStage = designCharacteristics$futilityProbabilities
        ))
    } else {
        return(list(
            alpha = fixedSampleSize$alpha,
            beta = fixedSampleSize$beta,
            sided = fixedSampleSize$sided,
            groups = fixedSampleSize$groups,
            allocationRatioPlanned = fixedSampleSize$allocationRatioPlanned,
            thetaH0 = fixedSampleSize$thetaH0,
            alternative = fixedSampleSize$alternative,
            stDev = fixedSampleSize$stDev,
            normalApproximation = fixedSampleSize$normalApproximation,
            meanRatio = fixedSampleSize$meanRatio,
            informationRates = matrix(informationRates, ncol = 1),
            maxNumberOfSubjects = maxNumberOfSubjects,
            numberOfSubjects = .getColumnCumSum(numberOfSubjects),
            numberOfSubjects1 = .getColumnCumSum(numberOfSubjects1),
            numberOfSubjects2 = .getColumnCumSum(numberOfSubjects2),
            expectedNumberOfSubjectsH0 = expectedNumberOfSubjectsH0,
            expectedNumberOfSubjectsH01 = expectedNumberOfSubjectsH01,
            expectedNumberOfSubjectsH1 = expectedNumberOfSubjectsH1,
            rejectPerStage = designCharacteristics$rejectionProbabilities,
            futilityPerStage = designCharacteristics$futilityProbabilities
        ))
    }
}

# Note that 'directionUpper' and 'maxNumberOfSubjects' are only applicable
# for 'objectType' = "power"
.createDesignPlanMeans <- function(...,
        objectType = c("sampleSize", "power"),
        design,
        normalApproximation = FALSE,
        meanRatio = FALSE,
        thetaH0 = ifelse(meanRatio, 1, 0),
        alternative = NA_real_,
        stDev = C_STDEV_DEFAULT,
        directionUpper = NA,
        maxNumberOfSubjects = NA_real_,
        groups = 2,
        allocationRatioPlanned = NA_real_) {
    objectType <- match.arg(objectType)

    .assertIsTrialDesignInverseNormalOrGroupSequential(design)
    .assertIsValidAlphaAndBeta(design$alpha, design$beta)
    .assertIsValidSidedParameter(design$sided)
    .assertIsValidGroupsParameter(groups)
    .assertIsValidStandardDeviation(stDev, groups = groups)
    .assertIsSingleNumber(thetaH0, "thetaH0")
    .assertIsSingleLogical(meanRatio, "meanRatio")
    .assertIsValidThetaH0(thetaH0, endpoint = "means", groups = groups, ratioEnabled = meanRatio)
    .assertIsSingleLogical(normalApproximation, "normalApproximation")

    if (meanRatio) {
        if (identical(alternative, C_ALTERNATIVE_POWER_SIMULATION_DEFAULT)) {
            alternative <- C_ALTERNATIVE_POWER_SIMULATION_MEAN_RATIO_DEFAULT
        }
        .assertIsInOpenInterval(alternative, "alternative", 0, NULL, naAllowed = TRUE)
    }

    directionUpper <- .assertIsValidDirectionUpper(directionUpper,
        design,
        objectType = objectType, userFunctionCallEnabled = TRUE
    )

    if (objectType == "sampleSize" && !any(is.na(alternative))) {
        if (design$sided == 1 && any(alternative - thetaH0 <= 0)) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "any 'alternative' (", .arrayToString(alternative),
                ") must be > 'thetaH0' (", thetaH0, ")"
            )
        }

        if (any(alternative - thetaH0 == 0)) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "any 'alternative' (", .arrayToString(alternative),
                ") must be != 'thetaH0' (", thetaH0, ")"
            )
        }
    }

    designPlan <- TrialDesignPlanMeans$new(design = design, meanRatio = meanRatio)
    designPlan$.setObjectType(objectType)

    designPlan$criticalValuesPValueScale <- matrix(design$stageLevels, ncol = 1)
    if (design$sided == 2) {
        designPlan$criticalValuesPValueScale <- designPlan$criticalValuesPValueScale * 2
    }
    designPlan$.setParameterType("criticalValuesPValueScale", C_PARAM_NOT_APPLICABLE)

    if (.hasApplicableFutilityBounds(design)) {
        designPlan$futilityBoundsPValueScale <- matrix(1 - stats::pnorm(design$futilityBounds), ncol = 1)
        designPlan$.setParameterType("futilityBoundsPValueScale", C_PARAM_GENERATED)
    }

    if (groups == 2) {
        if (design$sided == 2 && ((thetaH0 != 0 && !meanRatio) ||
                (thetaH0 != 1 && meanRatio))) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "two-sided case is implemented only for superiority testing ",
                "(i.e., thetaH0 = ", ifelse(meanRatio, 1, 0), ")"
            )
        }

        if (is.na(allocationRatioPlanned)) {
            allocationRatioPlanned <- C_ALLOCATION_RATIO_DEFAULT
        }

        if (allocationRatioPlanned < 0) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'allocationRatioPlanned' (", allocationRatioPlanned, ") must be >= 0"
            )
        }

        .setValueAndParameterType(
            designPlan, "allocationRatioPlanned",
            allocationRatioPlanned, C_ALLOCATION_RATIO_DEFAULT
        )

        if (meanRatio && thetaH0 <= 0) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "null hypothesis mean ratio is not allowed be negative or zero, ",
                "i.e., 'thetaH0' must be > 0 if 'meanRatio' = TRUE"
            )
        }
    }

    .setValueAndParameterType(
        designPlan, "normalApproximation",
        normalApproximation, FALSE
    )
    .setValueAndParameterType(designPlan, "meanRatio", meanRatio, FALSE)
    .setValueAndParameterType(designPlan, "thetaH0", thetaH0, 0)
    if (objectType == "power") {
        .setValueAndParameterType(
            designPlan, "alternative", alternative,
            C_ALTERNATIVE_POWER_SIMULATION_DEFAULT
        )
    } else {
        .setValueAndParameterType(
            designPlan, "alternative",
            alternative, C_ALTERNATIVE_DEFAULT
        )
    }
    .setValueAndParameterType(designPlan, "stDev", stDev, C_STDEV_DEFAULT)
    if (objectType == "power") {
        .assertIsValidMaxNumberOfSubjects(maxNumberOfSubjects)
        .setValueAndParameterType(
            designPlan, "maxNumberOfSubjects",
            maxNumberOfSubjects, NA_real_
        )
        .setValueAndParameterType(designPlan, "directionUpper", directionUpper, TRUE)

        designPlan$.setParameterType("effect", C_PARAM_GENERATED)
    }
    .setValueAndParameterType(designPlan, "groups", groups, 2)
    if (groups == 1) {
        if (isTRUE(meanRatio)) {
            warning("'meanRatio' (", meanRatio, ") will be ignored ",
                "because it is not applicable for 'groups' = 1",
                call. = FALSE
            )
        }
        designPlan$.setParameterType("meanRatio", C_PARAM_NOT_APPLICABLE)

        if (length(allocationRatioPlanned) == 1 && !is.na(allocationRatioPlanned)) {
            warning("'allocationRatioPlanned' (", allocationRatioPlanned,
                ") will be ignored because it is not applicable for 'groups' = 1",
                call. = FALSE
            )
        }
        designPlan$.setParameterType("allocationRatioPlanned", C_PARAM_NOT_APPLICABLE)
    }

    return(designPlan)
}

#' @title
#' Get Sample Size Means
#'
#' @description
#' Returns the sample size for testing means in one or two samples.
#'
#' @inheritParams param_design_with_default
#' @inheritParams param_groups
#' @param normalApproximation The type of computation of the p-values. If \code{TRUE}, the variance is
#'        assumed to be known, default is \code{FALSE}, i.e., the calculations are performed
#'        with the t distribution.
#' @param meanRatio If \code{TRUE}, the sample size for
#'        one-sided testing of H0: \code{mu1 / mu2 = thetaH0} is calculated, default is \code{FALSE}.
#' @inheritParams param_thetaH0
#' @inheritParams param_alternative
#' @inheritParams param_stDev
#' @inheritParams param_allocationRatioPlanned_sampleSize
#' @inheritParams param_three_dots
#'
#' @details
#' At given design the function calculates the stage-wise and maximum sample size for testing means.
#' In a two treatment groups design, additionally, an allocation ratio = \code{n1 / n2}
#' can be specified where \code{n1} and \code{n2} are the number of subjects in the two treatment groups.
#' A null hypothesis value thetaH0 != 0 for testing the difference of two means or
#' thetaH0 != 1 for testing the ratio of two means can be specified.
#' Critical bounds and stopping for futility bounds are provided at the effect scale
#' (mean, mean difference, or mean ratio, respectively) for each sample size calculation separately.
#'
#' @template return_object_trial_design_plan
#' @template how_to_get_help_for_generics
#'
#' @family sample size functions
#'
#' @template examples_get_sample_size_means
#'
#' @export
#'
getSampleSizeMeans <- function(design = NULL, ...,
        groups = 2L,
        normalApproximation = FALSE,
        meanRatio = FALSE,
        thetaH0 = ifelse(meanRatio, 1, 0),
        alternative = seq(0.2, 1, 0.2), # C_ALTERNATIVE_DEFAULT
        stDev = 1, # C_STDEV_DEFAULT
        allocationRatioPlanned = NA_real_ # C_ALLOCATION_RATIO_DEFAULT
        ) {
    if (is.null(design)) {
        design <- .getDefaultDesign(..., type = "sampleSize")
        .warnInCaseOfUnknownArguments(
            functionName = "getSampleSizeMeans",
            ignore = .getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
                design,
                powerCalculationEnabled = FALSE
            ), ...
        )
    } else {
        .assertIsTrialDesign(design)
        .warnInCaseOfUnknownArguments(functionName = "getSampleSizeMeans", ...)
        .warnInCaseOfTwoSidedPowerArgument(...)
        design <- .resetPipeOperatorQueue(design)
    }

    designPlan <- .createDesignPlanMeans(
        objectType = "sampleSize",
        design = design,
        normalApproximation = normalApproximation,
        meanRatio = meanRatio,
        thetaH0 = thetaH0,
        alternative = alternative,
        stDev = stDev,
        groups = groups,
        allocationRatioPlanned = allocationRatioPlanned,
        ...
    )

    return(.calculateSampleSizeMeansAndRates(designPlan))
}

#' @title
#' Get Power Means
#'
#' @description
#' Returns the power, stopping probabilities, and expected sample size for
#' testing means in one or two samples at given maximum sample size.
#'
#' @inheritParams param_design_with_default
#' @inheritParams param_groups
#' @param normalApproximation The type of computation of the p-values. If \code{TRUE}, the variance is
#'        assumed to be known, default is \code{FALSE}, i.e., the calculations are performed
#'        with the t distribution.
#' @param meanRatio If \code{TRUE}, the sample size for
#'        one-sided testing of H0: \code{mu1 / mu2 = thetaH0} is calculated, default is \code{FALSE}.
#' @inheritParams param_thetaH0
#' @inheritParams param_alternative
#' @inheritParams param_stDev
#' @inheritParams param_allocationRatioPlanned
#' @inheritParams param_directionUpper
#' @inheritParams param_maxNumberOfSubjects
#' @inheritParams param_three_dots
#'
#' @details
#' At given design the function calculates the power, stopping probabilities,
#' and expected sample size for testing means at given sample size.
#' In a two treatment groups design, additionally, an allocation ratio = \code{n1 / n2}
#' can be specified where \code{n1} and \code{n2} are the number
#' of subjects in the two treatment groups.
#' A null hypothesis value thetaH0 != 0 for testing the difference of two means
#' or \code{thetaH0 != 1} for testing the ratio of two means can be specified.
#' For the specified sample size, critical bounds and stopping for futility
#' bounds are provided at the effect scale (mean, mean difference, or
#' mean ratio, respectively)
#'
#' @template return_object_trial_design_plan
#' @template how_to_get_help_for_generics
#'
#' @family power functions
#'
#' @template examples_get_power_means
#'
#' @export
#'
getPowerMeans <- function(design = NULL, ...,
        groups = 2L,
        normalApproximation = FALSE,
        meanRatio = FALSE,
        thetaH0 = ifelse(meanRatio, 1, 0),
        alternative = seq(0, 1, 0.2), # C_ALTERNATIVE_POWER_SIMULATION_DEFAULT
        stDev = 1, # C_STDEV_DEFAULT
        directionUpper = NA,
        maxNumberOfSubjects = NA_real_,
        allocationRatioPlanned = NA_real_ # C_ALLOCATION_RATIO_DEFAULT
        ) {
    .assertIsValidMaxNumberOfSubjects(maxNumberOfSubjects)
    
    if ((length(stDev) == 1) && (groups == 2)) {
        stDev <- rep(stDev, 2)
    }

    if (is.null(design)) {
        design <- .getDefaultDesign(..., type = "power")
        .warnInCaseOfUnknownArguments(
            functionName = "getPowerMeans",
            ignore = .getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
                design,
                powerCalculationEnabled = TRUE
            ), ...
        )
    } else {
        .warnInCaseOfUnknownArguments(functionName = "getPowerMeans", ...)
        .assertIsTrialDesign(design)
        .warnInCaseOfTwoSidedPowerArgument(...)
        .warnInCaseOfTwoSidedPowerIsDisabled(design)
        design <- .resetPipeOperatorQueue(design)
    }

    designPlan <- .createDesignPlanMeans(
        objectType = "power",
        design = design,
        normalApproximation = normalApproximation,
        meanRatio = meanRatio,
        thetaH0 = thetaH0,
        alternative = alternative,
        stDev = stDev,
        directionUpper = directionUpper,
        maxNumberOfSubjects = maxNumberOfSubjects,
        groups = groups,
        allocationRatioPlanned = allocationRatioPlanned,
        ...
    )

    if (designPlan$groups == 1) {
        theta <- (designPlan$alternative - designPlan$thetaH0) / designPlan$stDev
        if (!is.na(designPlan$directionUpper) && !designPlan$directionUpper) {
            theta <- -theta
        }
        if (designPlan$normalApproximation) {
            powerAndAverageSampleNumber <- getPowerAndAverageSampleNumber(
                design, theta, maxNumberOfSubjects
            )
        } else {
            thetaAdj <- (sign(theta) * .getOneMinusQNorm(design$alpha / design$sided) -
                .getQNorm(stats::pt(
                    sign(theta) * stats::qt(
                        1 - design$alpha / design$sided,
                        maxNumberOfSubjects - 1
                    ),
                    maxNumberOfSubjects - 1,
                    theta * sqrt(maxNumberOfSubjects)
                ))) / sqrt(maxNumberOfSubjects)
            powerAndAverageSampleNumber <- getPowerAndAverageSampleNumber(
                design, thetaAdj, maxNumberOfSubjects
            )
        }
    } else {
        if (!designPlan$meanRatio) {
            theta <- sqrt(designPlan$allocationRatioPlanned) /
                sqrt(1 + designPlan$allocationRatioPlanned) /
                sqrt(designPlan$stDev[1]^2 +
                    designPlan$allocationRatioPlanned * designPlan$stDev[2]^2) *
                (designPlan$alternative - designPlan$thetaH0)
        } else {
            theta <- sqrt(designPlan$allocationRatioPlanned) /
                sqrt((designPlan$stDev[1]^2 + designPlan$allocationRatioPlanned *
                    designPlan$stDev[2]^2 * designPlan$thetaH0^2) *
                    (1 + designPlan$allocationRatioPlanned)) *
                (designPlan$alternative - designPlan$thetaH0)
        }
        if (!is.na(designPlan$directionUpper) && !designPlan$directionUpper) {
            theta <- -theta
        }
        if (designPlan$normalApproximation) {
            powerAndAverageSampleNumber <- getPowerAndAverageSampleNumber(
                design, theta, maxNumberOfSubjects
            )
        } else {
            if (designPlan$stDev[1] == designPlan$stDev[2]) {
                df <- maxNumberOfSubjects - 2
            } else {
                n2 <- maxNumberOfSubjects / (1 + designPlan$allocationRatioPlanned)
                n1 <- designPlan$allocationRatioPlanned * n2
                u <- designPlan$stDev[1]^2 / n1 /
                    (designPlan$stDev[1]^2 / n1 + designPlan$stDev[2]^2 / n2)
                df <- 1 / (u^2 / (n1 - 1) + (1 - u)^2 / (n2 - 1))
            }
            thetaAdj <- (sign(theta) * .getOneMinusQNorm(design$alpha / design$sided) -
                .getQNorm(stats::pt(
                    sign(theta) * stats::qt(
                        1 - design$alpha / design$sided,
                        df
                    ), df,
                    theta * sqrt(maxNumberOfSubjects)
                ))) / sqrt(maxNumberOfSubjects)
            powerAndAverageSampleNumber <- getPowerAndAverageSampleNumber(
                design, thetaAdj, maxNumberOfSubjects
            )
        }
    }

    designPlan$effect <- designPlan$alternative - designPlan$thetaH0

    designPlan$expectedNumberOfSubjects <- powerAndAverageSampleNumber$averageSampleNumber
    designPlan$overallReject <- powerAndAverageSampleNumber$overallReject
    designPlan$rejectPerStage <- powerAndAverageSampleNumber$rejectPerStage
    designPlan$futilityStop <- powerAndAverageSampleNumber$overallFutility
    designPlan$futilityPerStage <- powerAndAverageSampleNumber$futilityPerStage
    designPlan$earlyStop <- powerAndAverageSampleNumber$overallEarlyStop

    parameterNames <- c("overallReject")
    if (design$kMax > 1) {
        parameterNames <- c(
            parameterNames,
            "expectedNumberOfSubjects",
            "rejectPerStage",
            "futilityStop",
            "futilityPerStage",
            "earlyStop"
        )
    }
    for (parameterName in parameterNames) {
        designPlan$.setParameterType(parameterName, C_PARAM_GENERATED)
    }

    .addNumberOfSubjectsToPowerResult(designPlan)
    .addEffectScaleBoundaryDataToDesignPlan(designPlan)
    .hideFutilityStopsIfNotApplicable(designPlan)

    return(designPlan)
}
