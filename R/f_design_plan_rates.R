## |
## |  *Sample size and power rates*
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
## |  File version: $Revision: 7543 $
## |  Last changed: $Date: 2024-01-09 11:57:18 +0100 (Di, 09 Jan 2024) $
## |  Last changed by: $Author: wassmer $
## |

#' @include f_core_utilities.R
NULL

.getEffectScaleBoundaryDataRates <- function(designPlan) {
    design <- designPlan$.design
    thetaH0 <- designPlan$thetaH0
    pi2 <- designPlan$pi2
    maxNumberOfSubjects <- designPlan$maxNumberOfSubjects
    allocationRatioPlanned <- designPlan$allocationRatioPlanned
    directionUpper <- designPlan$directionUpper

    nParameters <- length(maxNumberOfSubjects)

    directionUpper[is.na(directionUpper)] <- TRUE

    criticalValuesEffectScaleUpper <- matrix(, nrow = design$kMax, ncol = nParameters)
    criticalValuesEffectScaleLower <- matrix(, nrow = design$kMax, ncol = nParameters)
    futilityBoundsEffectScaleUpper <- matrix(, nrow = design$kMax - 1, ncol = nParameters)
    futilityBoundsEffectScaleLower <- matrix(, nrow = design$kMax - 1, ncol = nParameters)
    if (length(allocationRatioPlanned) == 1) {
        allocationRatioPlanned <- rep(allocationRatioPlanned, nParameters)
    }
    futilityBounds <- design$futilityBounds
    futilityBounds[!is.na(futilityBounds) & futilityBounds <= C_FUTILITY_BOUNDS_DEFAULT] <- NA_real_

    if (designPlan$groups == 1) {
        n1 <- design$informationRates %*% t(maxNumberOfSubjects)
        for (j in (1:nParameters)) {
            criticalValuesEffectScaleUpper[, j] <- thetaH0 + (2 * directionUpper[j] - 1) *
                design$criticalValues * sqrt(thetaH0 * (1 - thetaH0)) / sqrt(n1[, j])
            if (design$sided == 2) {
                criticalValuesEffectScaleLower[, j] <- thetaH0 - (2 * directionUpper[j] - 1) *
                    design$criticalValues * sqrt(thetaH0 * (1 - thetaH0)) / sqrt(n1[, j])
            }
            if (!.isTrialDesignFisher(design) && !all(is.na(futilityBounds))) {
                futilityBoundsEffectScaleUpper[, j] <- thetaH0 + (2 * directionUpper[j] - 1) *
                    futilityBounds * sqrt(thetaH0 * (1 - thetaH0)) /
                    sqrt(n1[1:(design$kMax - 1), j])
            }
            if (!.isTrialDesignFisher(design) && design$sided == 2 && design$kMax > 1 &&
                    (design$typeOfDesign == C_TYPE_OF_DESIGN_PT || !is.null(design$typeBetaSpending) && design$typeBetaSpending != "none")) {
                futilityBoundsEffectScaleLower[, j] <- thetaH0 - (2 * directionUpper[j] - 1) *
                    futilityBounds * sqrt(thetaH0 * (1 - thetaH0)) /
                    sqrt(n1[1:(design$kMax - 1), j])
            }
        }
    } else if (!designPlan$riskRatio) {
        boundaries <- design$criticalValues

        # calculate pi1 that solves (pi1 - pi2 - thetaH0) / SE(pi1 - pi2 - thetaH0)
        # = crit by using Farrington & Manning approach
        for (j in (1:nParameters)) {
            n1 <- allocationRatioPlanned[j] * design$informationRates *
                maxNumberOfSubjects[j] / (1 + allocationRatioPlanned[j])
            n2 <- n1 / allocationRatioPlanned[j]

            for (i in (1:length(boundaries))) {
                tryCatch(
                    {
                        pi1Bound <- uniroot(
                            function(x) {
                                fm <- .getFarringtonManningValues(
                                    rate1 = x, rate2 = pi2, theta = thetaH0,
                                    allocation = allocationRatioPlanned[j], method = "diff"
                                )
                                (x - pi2 - thetaH0) / sqrt(fm$ml1 * (1 - fm$ml1) /
                                    n1[i] + fm$ml2 * (1 - fm$ml2) / n2[i]) -
                                    (2 * directionUpper[j] - 1) * boundaries[i]
                            },
                            lower = 0, upper = 1, tol = .Machine$double.eps^0.5
                        )$root
                    },
                    error = function(e) {
                        pi1Bound <<- NA_real_
                    }
                )

                # difference to pi2
                criticalValuesEffectScaleUpper[i, j] <- pi1Bound - pi2
            }
            if (design$sided == 2) {
                for (i in (1:length(boundaries))) {
                    tryCatch(
                        {
                            pi1Bound <- uniroot(
                                function(x) {
                                    fm <- .getFarringtonManningValues(
                                        rate1 = x, rate2 = pi2, theta = thetaH0,
                                        allocation = allocationRatioPlanned[j], method = "diff"
                                    )
                                    (x - pi2 - thetaH0) / sqrt(fm$ml1 * (1 - fm$ml1) /
                                        n1[i] + fm$ml2 * (1 - fm$ml2) / n2[i]) +
                                        (2 * directionUpper[j] - 1) * boundaries[i]
                                },
                                lower = 0, upper = 1, tol = .Machine$double.eps^0.5
                            )$root
                        },
                        error = function(e) {
                            pi1Bound <<- NA_real_
                        }
                    )

                    # difference to pi2
                    criticalValuesEffectScaleLower[i, j] <- pi1Bound - pi2
                }
            }
        }
        if (!.isTrialDesignFisher(design) && !all(is.na(futilityBounds))) {
            boundaries <- futilityBounds
            for (j in (1:nParameters)) {
                n1 <- allocationRatioPlanned[j] * design$informationRates *
                    maxNumberOfSubjects[j] / (1 + allocationRatioPlanned[j])
                n2 <- n1 / allocationRatioPlanned[j]
                for (i in (1:length(boundaries))) {
                    tryCatch(
                        {
                            pi1Bound <- uniroot(
                                function(x) {
                                    fm <- .getFarringtonManningValues(
                                        rate1 = x, rate2 = pi2, theta = thetaH0,
                                        allocation = allocationRatioPlanned[j], method = "diff"
                                    )
                                    (x - pi2 - thetaH0) / sqrt(fm$ml1 * (1 - fm$ml1) / n1[i] +
                                        fm$ml2 * (1 - fm$ml2) / n2[i]) -
                                        (2 * directionUpper[j] - 1) * boundaries[i]
                                },
                                lower = 0, upper = 1, tol = .Machine$double.eps^0.5
                            )$root
                        },
                        error = function(e) {
                            pi1Bound <<- NA_real_
                        }
                    )

                    # difference to pi2
                    futilityBoundsEffectScaleUpper[i, j] <- pi1Bound - pi2
                }
            }
        }

        if (!.isTrialDesignFisher(design) && design$sided == 2 && design$kMax > 1 &&
                (design$typeOfDesign == C_TYPE_OF_DESIGN_PT || !is.null(design$typeBetaSpending) && design$typeBetaSpending != "none")) {
            boundaries <- -futilityBounds
            for (j in (1:nParameters)) {
                n1 <- allocationRatioPlanned[j] * design$informationRates *
                    maxNumberOfSubjects[j] / (1 + allocationRatioPlanned[j])
                n2 <- n1 / allocationRatioPlanned[j]
                for (i in (1:length(boundaries))) {
                    tryCatch(
                        {
                            pi1Bound <- uniroot(
                                function(x) {
                                    fm <- .getFarringtonManningValues(
                                        rate1 = x, rate2 = pi2, theta = thetaH0,
                                        allocation = allocationRatioPlanned[j], method = "diff"
                                    )
                                    (x - pi2 - thetaH0) / sqrt(fm$ml1 * (1 - fm$ml1) / n1[i] +
                                        fm$ml2 * (1 - fm$ml2) / n2[i]) -
                                        (2 * directionUpper[j] - 1) * boundaries[i]
                                },
                                lower = 0, upper = 1, tol = .Machine$double.eps^0.5
                            )$root
                        },
                        error = function(e) {
                            pi1Bound <<- NA_real_
                        }
                    )
                    futilityBoundsEffectScaleLower[i, j] <- pi1Bound - pi2 # difference to pi2
                }
            }
        }
    } else {
        boundaries <- design$criticalValues
        # calculate pi1 that solves (pi1 - thetaH0 * pi2) / SE(pi1 - thetaH0 * pi2)
        # = crit by using Farrington & Manning approach
        for (j in (1:nParameters)) {
            n1 <- allocationRatioPlanned[j] * design$informationRates * maxNumberOfSubjects[j] /
                (1 + allocationRatioPlanned[j])
            n2 <- n1 / allocationRatioPlanned[j]
            for (i in (1:length(boundaries))) {
                tryCatch(
                    {
                        pi1Bound <- uniroot(
                            function(x) {
                                fm <- .getFarringtonManningValues(
                                    rate1 = x, rate2 = pi2, theta = thetaH0,
                                    allocation = allocationRatioPlanned[j], method = "ratio"
                                )
                                (x - thetaH0 * pi2) / sqrt(fm$ml1 * (1 - fm$ml1) / n1[i] +
                                    thetaH0^2 * fm$ml2 * (1 - fm$ml2) / n2[i]) -
                                    (2 * directionUpper[j] - 1) * boundaries[i]
                            },
                            lower = 0, upper = 1, tol = .Machine$double.eps^0.5
                        )$root
                    },
                    error = function(e) {
                        pi1Bound <<- NA_real_
                    }
                )

                # ratio to pi2
                criticalValuesEffectScaleUpper[i, j] <- pi1Bound / pi2
            }
            if (design$sided == 2) {
                for (i in (1:length(boundaries))) {
                    tryCatch(
                        {
                            pi1Bound <- uniroot(
                                function(x) {
                                    fm <- .getFarringtonManningValues(
                                        rate1 = x, rate2 = pi2, theta = thetaH0,
                                        allocation = allocationRatioPlanned[j], method = "ratio"
                                    )
                                    (x - thetaH0 * pi2) / sqrt(fm$ml1 * (1 - fm$ml1) / n1[i] +
                                        thetaH0^2 * fm$ml2 * (1 - fm$ml2) / n2[i]) +
                                        (2 * directionUpper[j] - 1) * boundaries[i]
                                },
                                lower = 0, upper = 1, tol = .Machine$double.eps^0.5
                            )$root
                        },
                        error = function(e) {
                            pi1Bound <<- NA_real_
                        }
                    )

                    # ratio to pi2
                    criticalValuesEffectScaleLower[i, j] <- pi1Bound / pi2
                }
            }
        }
        if (!.isTrialDesignFisher(design) && !all(is.na(futilityBounds))) {
            boundaries <- futilityBounds
            for (j in (1:nParameters)) {
                n1 <- allocationRatioPlanned[j] * design$informationRates * maxNumberOfSubjects[j] /
                    (1 + allocationRatioPlanned[j])
                n2 <- n1 / allocationRatioPlanned[j]
                for (i in (1:length(boundaries))) {
                    tryCatch(
                        {
                            pi1Bound <- uniroot(
                                function(x) {
                                    fm <- .getFarringtonManningValues(
                                        rate1 = x, rate2 = pi2, theta = thetaH0,
                                        allocation = allocationRatioPlanned[j], method = "ratio"
                                    )
                                    (x - thetaH0 * pi2) / sqrt(fm$ml1 * (1 - fm$ml1) / n1[i] +
                                        thetaH0^2 * fm$ml2 * (1 - fm$ml2) / n2[i]) -
                                        (2 * directionUpper[j] - 1) * boundaries[i]
                                },
                                lower = 0, upper = 1, tol = .Machine$double.eps^0.5
                            )$root
                        },
                        error = function(e) {
                            pi1Bound <<- NA_real_
                        }
                    )

                    # ratio to pi2
                    futilityBoundsEffectScaleUpper[i, j] <- pi1Bound / pi2
                }
            }
        }
        if (!.isTrialDesignFisher(design) && design$sided == 2 && design$kMax > 1 &&
                (design$typeOfDesign == C_TYPE_OF_DESIGN_PT || !is.null(design$typeBetaSpending) && design$typeBetaSpending != "none")) {
            boundaries <- -futilityBounds
            for (j in (1:nParameters)) {
                n1 <- allocationRatioPlanned[j] * design$informationRates * maxNumberOfSubjects[j] /
                    (1 + allocationRatioPlanned[j])
                n2 <- n1 / allocationRatioPlanned[j]
                for (i in (1:length(boundaries))) {
                    tryCatch(
                        {
                            pi1Bound <- uniroot(
                                function(x) {
                                    fm <- .getFarringtonManningValues(
                                        rate1 = x, rate2 = pi2, theta = thetaH0,
                                        allocation = allocationRatioPlanned[j], method = "ratio"
                                    )
                                    (x - thetaH0 * pi2) / sqrt(fm$ml1 * (1 - fm$ml1) / n1[i] +
                                        thetaH0^2 * fm$ml2 * (1 - fm$ml2) / n2[i]) -
                                        (2 * directionUpper[j] - 1) * boundaries[i]
                                },
                                lower = 0, upper = 1, tol = .Machine$double.eps^0.5
                            )$root
                        },
                        error = function(e) {
                            pi1Bound <<- NA_real_
                        }
                    )

                    # ratio to pi2
                    futilityBoundsEffectScaleLower[i, j] <- pi1Bound / pi2
                }
            }
        }
    }
    return(list(
        criticalValuesEffectScaleUpper = matrix(criticalValuesEffectScaleUpper, nrow = design$kMax),
        criticalValuesEffectScaleLower = matrix(criticalValuesEffectScaleLower, nrow = design$kMax),
        futilityBoundsEffectScaleUpper = matrix(futilityBoundsEffectScaleUpper, nrow = design$kMax - 1),
        futilityBoundsEffectScaleLower = matrix(futilityBoundsEffectScaleLower, nrow = design$kMax - 1)
    ))
}

.getSampleSizeFixedRates <- function(..., alpha = 0.025, beta = 0.2, sided = 1,
        normalApproximation = TRUE, riskRatio = FALSE,
        thetaH0 = 0, pi1 = seq(0.4, 0.6, 0.1), pi2 = 0.2,
        groups = 2, allocationRatioPlanned = 1) {
    if (groups == 1) {
        nFixed <- rep(NA_real_, length(pi1))

        for (i in 1:length(pi1)) {
            if (normalApproximation) {
                nFixed[i] <- (.getOneMinusQNorm(alpha / sided) * sqrt(thetaH0 * (1 - thetaH0)) +
                    .getOneMinusQNorm(beta) * sqrt(pi1[i] * (1 - pi1[i])))^2 /
                    (pi1[i] - thetaH0)^2
            } else {
                ifelse(pi1[i] > thetaH0, lower.tail <- FALSE, lower.tail <- TRUE)
                iterations <- 1
                if (lower.tail) {
                    nup <- 2
                    while ((stats::pbinom(stats::qbinom(alpha, nup, thetaH0, lower.tail = lower.tail) - 1,
                        nup, pi1[i],
                        lower.tail = lower.tail
                    ) < 1 - beta) && (iterations <= 50)) {
                        nup <- 2 * nup
                        iterations <- iterations + 1
                    }
                    if (iterations > 50) {
                        nFixed[i] <- Inf
                    } else {
                        prec <- 2
                        nlow <- 2
                        while (prec > 1) {
                            nFixed[i] <- round((nlow + nup) / 2)
                            ifelse(stats::pbinom(stats::qbinom(alpha, nFixed[i], thetaH0, lower.tail = lower.tail) - 1,
                                nFixed[i], pi1[i],
                                lower.tail = lower.tail
                            ) < 1 - beta,
                            nlow <- nFixed[i], nup <- nFixed[i]
                            )
                            prec <- nup - nlow
                        }
                        if (stats::pbinom(stats::qbinom(alpha, nFixed[i], thetaH0, lower.tail = lower.tail) - 1,
                                nFixed[i], pi1[i],
                                lower.tail = lower.tail
                            ) < 1 - beta) {
                            nFixed[i] <- nFixed[i] + 1
                        }
                    }
                } else {
                    nup <- 2
                    while ((stats::pbinom(stats::qbinom(alpha, nup, thetaH0, lower.tail = lower.tail),
                        nup, pi1[i],
                        lower.tail = lower.tail
                    ) < 1 - beta) && (iterations <= 50)) {
                        nup <- 2 * nup
                        iterations <- iterations + 1
                    }
                    if (iterations > 50) {
                        nFixed[i] <- Inf
                    } else {
                        prec <- 2
                        nlow <- 2
                        while (prec > 1) {
                            nFixed[i] <- round((nlow + nup) / 2)
                            ifelse(stats::pbinom(stats::qbinom(alpha, nFixed[i], thetaH0, lower.tail = lower.tail),
                                nFixed[i], pi1[i],
                                lower.tail = lower.tail
                            ) < 1 - beta,
                            nlow <- nFixed[i], nup <- nFixed[i]
                            )
                            prec <- nup - nlow
                        }
                        if (stats::pbinom(stats::qbinom(alpha, nFixed[i], thetaH0, lower.tail = lower.tail),
                                nFixed[i], pi1[i],
                                lower.tail = lower.tail
                            ) < 1 - beta) {
                            nFixed[i] <- nFixed[i] + 1
                        }
                    }
                }
            }
        }

        return(list(
            alpha = alpha,
            beta = beta,
            sided = sided,
            groups = groups,
            thetaH0 = thetaH0,
            pi1 = pi1,
            normalApproximation = normalApproximation,
            nFixed = nFixed
        ))
    }

    if (groups == 2) {
        n1Fixed <- rep(NA_real_, length(pi1))
        n2Fixed <- rep(NA_real_, length(pi1))
        nFixed <- rep(NA_real_, length(pi1))
        if (allocationRatioPlanned == 0) {
            allocationRatioPlannedVec <- rep(NA_real_, length(pi1))
        }

        for (i in 1:length(pi1)) {
            if (!riskRatio) {
                # allocationRatioPlanned = 0 provides optimum sample size
                if (allocationRatioPlanned == 0) {
                    allocationRatioPlannedVec[i] <- stats::optimize(function(x) {
                        fm <- .getFarringtonManningValues(
                            rate1 = pi1[i], rate2 = pi2,
                            theta = thetaH0, allocation = x, method = "diff"
                        )
                        n1 <- (.getOneMinusQNorm(alpha / sided) * sqrt(fm$ml1 * (1 - fm$ml1) + fm$ml2 * (1 - fm$ml2) * x) +
                            .getOneMinusQNorm(beta) * sqrt(pi1[i] * (1 - pi1[i]) + pi2 * (1 - pi2) * x))^2 /
                            (pi1[i] - pi2 - thetaH0)^2
                        return((1 + x) / x * n1)
                    }, interval = c(0, 5), tol = 0.0001)$minimum
                    fm <- .getFarringtonManningValues(
                        rate1 = pi1[i], rate2 = pi2, theta = thetaH0,
                        allocation = allocationRatioPlannedVec[i], method = "diff"
                    )
                    n1Fixed[i] <- (.getOneMinusQNorm(alpha / sided) * sqrt(fm$ml1 * (1 - fm$ml1) +
                        fm$ml2 * (1 - fm$ml2) * allocationRatioPlannedVec[i]) +
                        .getOneMinusQNorm(beta) * sqrt(pi1[i] * (1 - pi1[i]) + pi2 * (1 - pi2) *
                            allocationRatioPlannedVec[i]))^2 / (pi1[i] - pi2 - thetaH0)^2
                } else {
                    fm <- .getFarringtonManningValues(
                        rate1 = pi1[i], rate2 = pi2,
                        theta = thetaH0, allocation = allocationRatioPlanned, method = "diff"
                    )
                    n1Fixed[i] <- (.getOneMinusQNorm(alpha / sided) * sqrt(fm$ml1 * (1 - fm$ml1) +
                        fm$ml2 * (1 - fm$ml2) * allocationRatioPlanned) +
                        .getOneMinusQNorm(beta) * sqrt(pi1[i] * (1 - pi1[i]) + pi2 * (1 - pi2) *
                            allocationRatioPlanned))^2 / (pi1[i] - pi2 - thetaH0)^2
                }
            } else {
                if (allocationRatioPlanned == 0) {
                    # allocationRatioPlanned = 0 provides optimum sample size
                    allocationRatioPlannedVec[i] <- stats::optimize(function(x) {
                        fm <- .getFarringtonManningValues(
                            rate1 = pi1[i], rate2 = pi2,
                            theta = thetaH0, allocation = x, method = "ratio"
                        )
                        n1 <- (.getOneMinusQNorm(alpha / sided) * sqrt(fm$ml1 * (1 - fm$ml1) +
                            fm$ml2 * (1 - fm$ml2) * x * thetaH0^2) +
                            .getOneMinusQNorm(beta) * sqrt(pi1[i] * (1 - pi1[i]) + pi2 *
                                (1 - pi2) * x * thetaH0^2))^2 / (pi1[i] - thetaH0 * pi2)^2
                        return((1 + x) / x * n1)
                    }, interval = c(0, 5), tol = 0.0001)$minimum
                    fm <- .getFarringtonManningValues(
                        rate1 = pi1[i], rate2 = pi2, theta = thetaH0,
                        allocation = allocationRatioPlannedVec[i], method = "ratio"
                    )
                    n1Fixed[i] <- (.getOneMinusQNorm(alpha / sided) * sqrt(fm$ml1 * (1 - fm$ml1) +
                        fm$ml2 * (1 - fm$ml2) * allocationRatioPlannedVec[i] * thetaH0^2) +
                        .getOneMinusQNorm(beta) * sqrt(pi1[i] * (1 - pi1[i]) + pi2 * (1 - pi2) *
                            allocationRatioPlannedVec[i] * thetaH0^2))^2 / (pi1[i] - thetaH0 * pi2)^2
                } else {
                    fm <- .getFarringtonManningValues(
                        rate1 = pi1[i], rate2 = pi2,
                        theta = thetaH0, allocation = allocationRatioPlanned, method = "ratio"
                    )
                    n1Fixed[i] <- (.getOneMinusQNorm(alpha / sided) * sqrt(fm$ml1 * (1 - fm$ml1) +
                        fm$ml2 * (1 - fm$ml2) * allocationRatioPlanned * thetaH0^2) +
                        .getOneMinusQNorm(beta) * sqrt(pi1[i] * (1 - pi1[i]) + pi2 * (1 - pi2) *
                            allocationRatioPlanned * thetaH0^2))^2 / (pi1[i] - thetaH0 * pi2)^2
                }
            }
        }
        if (allocationRatioPlanned == 0) {
            allocationRatioPlanned <- allocationRatioPlannedVec
        }

        n2Fixed <- n1Fixed / allocationRatioPlanned
        nFixed <- n1Fixed + n2Fixed

        return(list(
            alpha = alpha,
            beta = beta,
            sided = sided,
            groups = groups,
            allocationRatioPlanned = allocationRatioPlanned,
            thetaH0 = thetaH0,
            pi1 = pi1,
            pi2 = pi2,
            normalApproximation = normalApproximation,
            riskRatio = riskRatio,
            n1Fixed = n1Fixed,
            n2Fixed = n2Fixed,
            nFixed = nFixed
        ))
    }
}

.getSampleSizeSequentialRates <- function(fixedSampleSize, designCharacteristics) {
    kMax <- designCharacteristics$.design$kMax
    numberOfSubjects <- matrix(NA_real_, kMax, length(fixedSampleSize$pi1))
    numberOfSubjects1 <- matrix(NA_real_, kMax, length(fixedSampleSize$pi1))
    numberOfSubjects2 <- matrix(NA_real_, kMax, length(fixedSampleSize$pi1))
    maxNumberOfSubjects <- rep(NA_real_, length(fixedSampleSize$pi1))
    expectedNumberOfSubjectsH0 <- rep(NA_real_, length(fixedSampleSize$pi1))
    expectedNumberOfSubjectsH01 <- rep(NA_real_, length(fixedSampleSize$pi1))
    expectedNumberOfSubjectsH1 <- rep(NA_real_, length(fixedSampleSize$pi1))

    informationRates <- designCharacteristics$information / designCharacteristics$shift

    for (i in 1:length(fixedSampleSize$pi1)) {
        maxNumberOfSubjects[i] <- fixedSampleSize$nFixed[i] * designCharacteristics$inflationFactor

        numberOfSubjects[, i] <- maxNumberOfSubjects[i] * c(
            informationRates[1],
            (informationRates[2:kMax] - informationRates[1:(kMax - 1)])
        )

        expectedNumberOfSubjectsH0[i] <- designCharacteristics$averageSampleNumber0 * fixedSampleSize$nFixed[i]
        expectedNumberOfSubjectsH01[i] <- designCharacteristics$averageSampleNumber01 * fixedSampleSize$nFixed[i]
        expectedNumberOfSubjectsH1[i] <- designCharacteristics$averageSampleNumber1 * fixedSampleSize$nFixed[i]

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
            pi1 = fixedSampleSize$pi1,
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
            pi1 = fixedSampleSize$pi1,
            pi2 = fixedSampleSize$pi2,
            normalApproximation = fixedSampleSize$normalApproximation,
            riskRatio = fixedSampleSize$riskRatio,
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

#
# note that 'directionUpper' and 'maxNumberOfSubjects' are
# only applicable for 'objectType' = "power"
#
.createDesignPlanRates <- function(..., objectType = c("sampleSize", "power"),
        design, normalApproximation = TRUE, riskRatio = FALSE,
        thetaH0 = ifelse(riskRatio, 1, 0), pi1 = C_PI_1_SAMPLE_SIZE_DEFAULT,
        pi2 = C_PI_2_DEFAULT, directionUpper = NA,
        maxNumberOfSubjects = NA_real_, groups = 2, allocationRatioPlanned = NA_real_) {
    objectType <- match.arg(objectType)

    .assertIsTrialDesignInverseNormalOrGroupSequential(design)
    .assertIsValidAlphaAndBeta(design$alpha, design$beta)
    .assertIsValidSidedParameter(design$sided)
    .assertIsValidGroupsParameter(groups)
    .assertIsSingleLogical(normalApproximation, "normalApproximation")
    .assertIsSingleLogical(riskRatio, "riskRatio")
    directionUpper <- .assertIsValidDirectionUpper(directionUpper, design$sided, objectType, userFunctionCallEnabled = TRUE)

    if (groups == 1) {
        if (!any(is.na(pi1)) && any(pi1 == thetaH0) && (objectType == "sampleSize")) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "any 'pi1' (", .arrayToString(pi1), ") must be != 'thetaH0' (", thetaH0, ")"
            )
        }

        if (any(is.na(pi1)) || any(pi1 <= 0) || any(pi1 >= 1)) {
            stop(
                C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS,
                "probability 'pi1' (", .arrayToString(pi1), ") is out of bounds (0; 1)"
            )
        }

        if (thetaH0 >= 1 || thetaH0 <= 0) {
            stop(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS, "'thetaH0' (", thetaH0, ") is out of bounds (0; 1)")
        }

        if (!normalApproximation && design$sided == 2 && (objectType == "sampleSize")) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "exact sample size calculation not available for two-sided testing"
            )
        }
    } else if (groups == 2) {
        if (!any(is.na(c(pi1, pi2))) && any(abs(pi1 - pi2 - thetaH0) < 1E-12) &&
                (objectType == "sampleSize") && !riskRatio) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "any 'pi1 - pi2' (", .arrayToString(pi1 - pi2), ") must be != 'thetaH0' (", thetaH0, ")"
            )
        }

        if (!any(is.na(c(pi1, pi2))) && any(abs(pi1 / pi2 - thetaH0) < 1E-12) &&
                (objectType == "sampleSize") && riskRatio) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "any 'pi1 / pi2' (", .arrayToString(pi1 / pi2), ") must be != 'thetaH0' (", thetaH0, ")"
            )
        }

        if (any(is.na(pi1)) || any(pi1 <= 0) || any(pi1 >= 1)) {
            stop(
                C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS,
                "probability 'pi1' (", .arrayToString(pi1), ") is out of bounds (0; 1)"
            )
        }

        if (any(is.na(pi2)) || any(pi2 <= 0) || any(pi2 >= 1)) {
            stop(
                C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS,
                "probability 'pi2' (", .arrayToString(pi2), ") is out of bounds (0; 1)"
            )
        }

        if (design$sided == 2 && ((thetaH0 != 0 && !riskRatio) || (thetaH0 != 1 && riskRatio))) {
            stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "two-sided case is implemented only for superiority testing")
        }

        if (!normalApproximation) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "only normal approximation case is implemented for two groups"
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

        if (riskRatio && thetaH0 <= 0) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "null hypothesis risk ratio is not allowed be negative or zero, ",
                "i.e., 'thetaH0' must be > 0 if 'riskRatio' = TRUE"
            )
        }
    }

    designPlan <- TrialDesignPlanRates(design = design)
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

    if (objectType == "power") {
        .assertIsValidMaxNumberOfSubjects(maxNumberOfSubjects)
        .setValueAndParameterType(designPlan, "maxNumberOfSubjects", maxNumberOfSubjects, NA_real_)
        .setValueAndParameterType(designPlan, "directionUpper", directionUpper, TRUE)

        designPlan$.setParameterType("effect", C_PARAM_GENERATED)
    }

    .setValueAndParameterType(designPlan, "normalApproximation", normalApproximation, TRUE)
    .setValueAndParameterType(designPlan, "thetaH0", thetaH0, ifelse(riskRatio, 1, 0))
    .assertIsValidThetaH0(thetaH0, endpoint = "rates", groups = groups, ratioEnabled = riskRatio)
    if (objectType == "power") {
        .setValueAndParameterType(designPlan, "pi1", pi1, C_PI_1_DEFAULT)
    } else {
        .setValueAndParameterType(designPlan, "pi1", pi1, C_PI_1_SAMPLE_SIZE_DEFAULT)
    }
    .setValueAndParameterType(designPlan, "pi2", pi2, 0.2, notApplicableIfNA = TRUE)
    if (groups == 1) {
        if (designPlan$.getParameterType("pi2") == C_PARAM_USER_DEFINED) {
            warning("'pi2' (", pi2, ") will be ignored ",
                "because it is not applicable for 'groups' = 1",
                call. = FALSE
            )
        }
        designPlan$.setParameterType("pi2", C_PARAM_NOT_APPLICABLE)

        if (isTRUE(riskRatio)) {
            warning("'riskRatio' (", riskRatio, ") will be ignored ",
                "because it is not applicable for 'groups' = 1",
                call. = FALSE
            )
        }
        designPlan$.setParameterType("riskRatio", C_PARAM_NOT_APPLICABLE)

        if (length(allocationRatioPlanned) == 1 && !is.na(allocationRatioPlanned)) {
            warning("'allocationRatioPlanned' (", allocationRatioPlanned,
                ") will be ignored because it is not applicable for 'groups' = 1",
                call. = FALSE
            )
        }
        designPlan$.setParameterType("allocationRatioPlanned", C_PARAM_NOT_APPLICABLE)
    } else {
        .setValueAndParameterType(designPlan, "riskRatio", riskRatio, FALSE)
        .setValueAndParameterType(
            designPlan, "allocationRatioPlanned",
            allocationRatioPlanned, C_ALLOCATION_RATIO_DEFAULT
        )
    }
    .setValueAndParameterType(designPlan, "groups", groups, 2)

    return(designPlan)
}

#' @title
#' Get Power Rates
#'
#' @description
#' Returns the power, stopping probabilities, and expected sample size for testing rates
#' in one or two samples at given maximum sample size.
#'
#' @inheritParams param_design_with_default
#' @inheritParams param_groups
#' @param riskRatio If \code{TRUE}, the power for one-sided
#'        testing of H0: \code{pi1 / pi2 = thetaH0} is calculated, default is \code{FALSE}.
#' @inheritParams param_thetaH0
#' @inheritParams param_pi1_rates
#' @inheritParams param_pi2_rates
#' @inheritParams param_directionUpper
#' @inheritParams param_maxNumberOfSubjects
#' @inheritParams param_allocationRatioPlanned
#' @inheritParams param_three_dots
#'
#' @details
#' At given design the function calculates the power, stopping probabilities, and expected sample size
#' for testing rates at given maximum sample size.
#' The sample sizes over the stages are calculated according to the specified information rate in the design.
#' In a two treatment groups design, additionally, an allocation ratio = \code{n1 / n2} can be specified 
#' where \code{n1} and \code{n2} are the number of subjects in the two treatment groups.
#' If a null hypothesis value thetaH0 != 0 for testing the difference of two rates
#' or \code{thetaH0 != 1} for testing the risk ratio is specified, the
#' formulas according to Farrington & Manning (Statistics in Medicine, 1990) are used (only one-sided testing).
#' Critical bounds and stopping for futility bounds are provided at the effect scale
#' (rate, rate difference, or rate ratio, respectively).
#' For the two-sample case, the calculation here is performed at fixed pi2 as given as argument in the function.
#' Note that the power calculation for rates is always based on the normal approximation.
#'
#' @template return_object_trial_design_plan
#' @template how_to_get_help_for_generics
#'
#' @family power functions
#'
#' @template examples_get_power_rates
#'
#' @export
#'
getPowerRates <- function(design = NULL, ...,
        groups = 2L,
        riskRatio = FALSE,
        thetaH0 = ifelse(riskRatio, 1, 0),
        pi1 = seq(0.2, 0.5, 0.1), # C_PI_1_DEFAULT
        pi2 = 0.2, # C_PI_2_DEFAULT
        directionUpper = NA,
        maxNumberOfSubjects = NA_real_,
        allocationRatioPlanned = NA_real_ # C_ALLOCATION_RATIO_DEFAULT
        ) {
    if (is.null(design)) {
        design <- .getDefaultDesign(..., type = "power")
        .warnInCaseOfUnknownArguments(
            functionName = "getPowerRates",
            ignore = .getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design, powerCalculationEnabled = TRUE), ...
        )
    } else {
        .warnInCaseOfUnknownArguments(functionName = "getPowerRates", ...)
        .assertIsTrialDesign(design)
        .warnInCaseOfTwoSidedPowerArgument(...)
        .warnInCaseOfTwoSidedPowerIsDisabled(design)
    }

    designPlan <- .createDesignPlanRates(
        objectType = "power",
        design = design, riskRatio = riskRatio,
        thetaH0 = thetaH0, pi1 = pi1, pi2 = pi2, directionUpper = directionUpper,
        maxNumberOfSubjects = maxNumberOfSubjects, groups = groups,
        allocationRatioPlanned = allocationRatioPlanned, ...
    )

    if (!is.na(allocationRatioPlanned) && allocationRatioPlanned <= 0) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "allocation ratio must be > 0")
    }

    allocationRatioPlanned <- designPlan$allocationRatioPlanned

    theta <- rep(NA_real_, length(pi1))
    if (groups == 1) {
        designPlan$effect <- pi1 - thetaH0
        theta <- (pi1 - thetaH0) / sqrt(pi1 * (1 - pi1)) + sign(pi1 - thetaH0) *
            .getOneMinusQNorm(design$alpha / design$sided) *
            (1 - sqrt(thetaH0 * (1 - thetaH0) / (pi1 * (1 - pi1)))) / sqrt(maxNumberOfSubjects)
    } else {
        if (!riskRatio) {
            designPlan$effect <- pi1 - pi2 - thetaH0
            for (i in (1:length(pi1))) {
                fm <- .getFarringtonManningValues(
                    rate1 = pi1[i], rate2 = pi2,
                    theta = thetaH0, allocation = allocationRatioPlanned, method = "diff"
                )
                theta[i] <- sqrt(allocationRatioPlanned) / (1 + allocationRatioPlanned) *
                    (pi1[i] - pi2 - thetaH0) * sqrt(1 + allocationRatioPlanned) /
                    sqrt(pi1[i] * (1 - pi1[i]) + allocationRatioPlanned * pi2 * (1 - pi2)) +
                    sign(pi1[i] - pi2 - thetaH0) * .getOneMinusQNorm(design$alpha / design$sided) *
                        (1 - sqrt(fm$ml1 * (1 - fm$ml1) + allocationRatioPlanned * fm$ml2 * (1 - fm$ml2)) /
                            sqrt(pi1[i] * (1 - pi1[i]) + allocationRatioPlanned * pi2 * (1 - pi2))) /
                        sqrt(maxNumberOfSubjects)
            }
        } else {
            designPlan$effect <- pi1 / pi2 - thetaH0
            for (i in (1:length(pi1))) {
                fm <- .getFarringtonManningValues(
                    rate1 = pi1[i], rate2 = pi2,
                    theta = thetaH0, allocation = allocationRatioPlanned, method = "ratio"
                )
                theta[i] <- sqrt(allocationRatioPlanned) / (1 + allocationRatioPlanned) *
                    (pi1[i] - thetaH0 * pi2) * sqrt(1 + allocationRatioPlanned) /
                    sqrt(pi1[i] * (1 - pi1[i]) + allocationRatioPlanned * thetaH0^2 * pi2 * (1 - pi2)) +
                    sign(pi1[i] - thetaH0 * pi2) * .getOneMinusQNorm(design$alpha / design$sided) *
                        (1 - sqrt(fm$ml1 * (1 - fm$ml1) + allocationRatioPlanned * thetaH0^2 *
                            fm$ml2 * (1 - fm$ml2)) / sqrt(pi1[i] * (1 - pi1[i]) + allocationRatioPlanned *
                            thetaH0^2 * pi2 * (1 - pi2))) /
                        sqrt(maxNumberOfSubjects)
            }
        }
    }

    if (!is.na(designPlan$directionUpper) && !designPlan$directionUpper) {
        theta <- -theta
    }

    powerAndAverageSampleNumber <- getPowerAndAverageSampleNumber(design, theta, maxNumberOfSubjects)

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

#' @title
#' Get Sample Size Rates
#'
#' @description
#' Returns the sample size for testing rates in one or two samples.
#'
#' @inheritParams param_design_with_default
#' @inheritParams param_groups
#' @param normalApproximation If \code{FALSE}, the sample size
#'        for the case of one treatment group is calculated exactly using the binomial distribution,
#'        default is \code{TRUE}.
#' @param riskRatio If \code{TRUE}, the sample size for one-sided
#'        testing of H0: \code{pi1 / pi2 = thetaH0} is calculated, default is \code{FALSE}.
#' @inheritParams param_thetaH0
#' @inheritParams param_pi1_rates
#' @inheritParams param_pi2_rates
#' @inheritParams param_allocationRatioPlanned_sampleSize
#' @inheritParams param_three_dots
#'
#' @details
#' At given design the function calculates the stage-wise and maximum sample size for testing rates.
#' In a two treatment groups design, additionally, an allocation ratio = \code{n1 / n2} can be specified 
#' where \code{n1} and \code{n2} are the number of subjects in the two treatment groups.
#' If a null hypothesis value thetaH0 != 0 for testing the difference of two rates or
#' thetaH0 != 1 for testing the risk ratio is specified, the sample size
#' formula according to Farrington & Manning (Statistics in Medicine, 1990) is used.
#' Critical bounds and stopping for futility bounds are provided at the effect scale
#' (rate, rate difference, or rate ratio, respectively) for each sample size calculation separately.
#' For the two-sample case, the calculation here is performed at fixed pi2 as given as argument
#' in the function.
#'
#' @template return_object_trial_design_plan
#' @template how_to_get_help_for_generics
#'
#' @family sample size functions
#'
#' @template examples_get_sample_size_rates
#'
#' @export
#'
getSampleSizeRates <- function(
        design = NULL, ...,
        groups = 2,
        normalApproximation = TRUE,
        riskRatio = FALSE,
        thetaH0 = ifelse(riskRatio, 1, 0),
        pi1 = c(0.4, 0.5, 0.6), # C_PI_1_SAMPLE_SIZE_DEFAULT
        pi2 = 0.2, # C_PI_2_DEFAULT
        allocationRatioPlanned = NA_real_ # C_ALLOCATION_RATIO_DEFAULT
        ) {
    if (is.null(design)) {
        design <- .getDefaultDesign(..., type = "sampleSize")
        .warnInCaseOfUnknownArguments(
            functionName = "getSampleSizeRates",
            ignore = .getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design, powerCalculationEnabled = FALSE), ...
        )
    } else {
        .assertIsTrialDesign(design)
        .warnInCaseOfUnknownArguments(functionName = "getSampleSizeRates", ...)
        .warnInCaseOfTwoSidedPowerArgument(...)
    }

    designPlan <- .createDesignPlanRates(
        objectType = "sampleSize",
        design = design, normalApproximation = normalApproximation, riskRatio = riskRatio,
        thetaH0 = thetaH0, pi1 = pi1, pi2 = pi2, groups = groups,
        allocationRatioPlanned = allocationRatioPlanned, ...
    )

    return(.getSampleSize(designPlan))
}
