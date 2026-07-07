## |
## |  *Effect scale boundary data*
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

.getDirectionUpper <- function(designPlan, nParameters = 1) {
    design <- designPlan$.design
    directionUpper <- design$directionUpper
    if (is.null(directionUpper) || length(directionUpper) == 0 || all(is.na(directionUpper))) {
        directionUpper <- designPlan$directionUpper
    }
    directionUpper[is.na(directionUpper)] <- C_DIRECTION_UPPER_DEFAULT
    if (length(directionUpper) == 1 && nParameters > 1) {
        directionUpper <- rep(directionUpper, nParameters)
    }
    return(directionUpper)
}

#
# Effect scale boundary data: means
#
.getEffectScaleBoundaryDataMeans_OLD <- function(designPlan) {
    design <- designPlan$.design
    thetaH0 <- designPlan$thetaH0
    stDev <- designPlan$stDev
    maxNumberOfSubjects <- designPlan$maxNumberOfSubjects
    allocationRatioPlanned <- designPlan$allocationRatioPlanned
    directionUpper <- .getDirectionUpper(designPlan)

    # initialize effect scale matrix
    futilityBoundsEffectScaleUpper <- rep(NA_real_, design$kMax - 1)
    futilityBoundsEffectScaleLower <- rep(NA_real_, design$kMax - 1)

    if (designPlan$normalApproximation) {
        criticalValues <- .getCriticalValues(design)
    } else {
        criticalValues <- stats::qt(
            1 - design$stageLevels,
            pmax(design$informationRates %*% t(maxNumberOfSubjects) - designPlan$groups, 1e-04)
        )

        # outside validated range
        criticalValues[criticalValues > 50] <- NA_real_
        if (design$kMax > 1 && identical(design$typeOfDesign, "noEarlyEfficacy")) {
            if ((is.matrix(criticalValues) && anyNA(criticalValues[design$kMax, ])) ||
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
    }

    if (design$kMax > 1) {
        futilityBounds <- .getFutilityBounds(design)
        if (!designPlan$normalApproximation && .hasApplicableFutilityBounds(design)) {
            futilityBounds <- stats::qt(
                stats::pnorm(futilityBounds),
                pmax(design$informationRates[1:(design$kMax - 1)] %*%
                    t(maxNumberOfSubjects) - designPlan$groups, 1e-04)
            )

            # outside validated range
            futilityBounds[abs(futilityBounds) > 50] <- NA_real_
            if (anyNA(futilityBounds)) {
                warning("The computation of futility boundaries on ",
                    "treatment effect scale not performed presumably ",
                    "due to too small degrees of freedom",
                    call. = FALSE
                )
            }
        }
        futilityBounds[!is.na(futilityBounds) & futilityBounds <= C_FUTILITY_BOUNDS_DEFAULT] <- NA_real_
    }

    if ((length(stDev) == 1) && (designPlan$groups == 2)) {
        stDev <- rep(stDev, 2)
    }

    if (designPlan$groups == 1) {
        criticalValuesEffectScaleUpper <- thetaH0 + criticalValues * stDev /
            sqrt(design$informationRates %*% t(maxNumberOfSubjects))
        criticalValuesEffectScaleLower <- thetaH0 - criticalValues * stDev /
            sqrt(design$informationRates %*% t(maxNumberOfSubjects))
        if (design$kMax > 1 && !.isTrialDesignFisher(design) && !all(is.na(futilityBounds))) {
            futilityBoundsEffectScaleUpper <- thetaH0 + futilityBounds * stDev /
                sqrt(design$informationRates[1:(design$kMax - 1)] %*% t(maxNumberOfSubjects))
        }
        if (design$kMax > 1 && !.isTrialDesignFisher(design) && design$sided == 2 && design$kMax > 1 &&
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
        if (design$kMax > 1 && !.isTrialDesignFisher(design) && !all(is.na(futilityBounds))) {
            futilityBoundsEffectScaleUpper <- thetaH0 + futilityBounds *
                sqrt(1 + allocationRatioPlanned) *
                sqrt(stDev[1]^2 + allocationRatioPlanned * stDev[2]^2) /
                (sqrt(allocationRatioPlanned *
                    design$informationRates[1:(design$kMax - 1)] %*% t(maxNumberOfSubjects)))
        }
        if (design$kMax > 1 && !.isTrialDesignFisher(design) && design$sided == 2 && design$kMax > 1 &&
                (design$typeOfDesign == C_TYPE_OF_DESIGN_PT ||
                    !is.null(design$typeBetaSpending) && design$typeBetaSpending != "none")) {
            futilityBoundsEffectScaleLower <- thetaH0 - futilityBounds *
                sqrt(1 + allocationRatioPlanned) *
                sqrt(stDev[1]^2 + allocationRatioPlanned * stDev[2]^2) /
                # stDev * (1 + allocationRatioPlanned) /
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

        if (design$kMax > 1 && !.isTrialDesignFisher(design) && !all(is.na(futilityBounds))) {
            futilityBoundsEffectScaleUpper <- thetaH0 + futilityBounds *
                sqrt((1 + allocationRatioPlanned) / allocationRatioPlanned) *
                sqrt(stDev[1]^2 + thetaH0^2 * allocationRatioPlanned * stDev[2]^2) /
                (sqrt(design$informationRates[1:(design$kMax - 1)] %*% t(maxNumberOfSubjects)))
        }
        if (design$kMax > 1 && !.isTrialDesignFisher(design) && design$sided == 2 && design$kMax > 1 &&
                (design$typeOfDesign == C_TYPE_OF_DESIGN_PT ||
                    !is.null(design$typeBetaSpending) && design$typeBetaSpending != "none")) {
            futilityBoundsEffectScaleLower <- thetaH0 - futilityBounds *
                sqrt((1 + allocationRatioPlanned) / allocationRatioPlanned) *
                sqrt(stDev[1]^2 + thetaH0^2 * allocationRatioPlanned * stDev[2]^2) /
                (sqrt(design$informationRates[1:(design$kMax - 1)] %*% t(maxNumberOfSubjects)))
        }
    }

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

# New solution
.getEffectScaleBoundaryDataMeans <- function(designPlan) {
    design <- designPlan$.design
    directionUpper <- .getDirectionUpper(designPlan)
    thetaH0 <- designPlan$thetaH0
    stDev <- designPlan$stDev
    maxNumberOfSubjects <- designPlan$maxNumberOfSubjects
    allocationRatioPlanned <- designPlan$allocationRatioPlanned

    # initialize effect scale matrix
    futilityBoundsEffectScaleUpper <- rep(NA_real_, design$kMax - 1)
    futilityBoundsEffectScaleLower <- rep(NA_real_, design$kMax - 1)

    if (designPlan$normalApproximation) {
        criticalValues <- .getCriticalValues(design)
    } else {
        criticalValues <- stats::qt(
            1 - design$stageLevels,
            pmax(design$informationRates %*% t(maxNumberOfSubjects) - designPlan$groups, 1e-04)
        )

        # outside validated range
        criticalValues[criticalValues > 50] <- NA_real_
        if (design$kMax > 1 && identical(design$typeOfDesign, "noEarlyEfficacy")) {
            if ((is.matrix(criticalValues) && anyNA(criticalValues[design$kMax, ])) ||
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
        
#        criticalValues <- .applyDirectionOfAlternative(criticalValues, directionUpper,
#            type = "negateIfLower", phase = "planning"
#        )
    }

    if (design$kMax > 1) {
        futilityBounds <- .getFutilityBounds(design)
        if (!designPlan$normalApproximation && .hasApplicableFutilityBounds(design)) {
            futilityBounds <- stats::qt(
                stats::pnorm(futilityBounds),
                pmax(design$informationRates[1:(design$kMax - 1)] %*%
                    t(maxNumberOfSubjects) - designPlan$groups, 1e-04)
            )

            # outside validated range
            futilityBounds[abs(futilityBounds) > 50] <- NA_real_
            if (anyNA(futilityBounds)) {
                warning("The computation of futility boundaries on ",
                    "treatment effect scale not performed presumably ",
                    "due to too small degrees of freedom",
                    call. = FALSE
                )
            }
        }
        futilityBounds[!is.na(futilityBounds) & futilityBounds <= C_FUTILITY_BOUNDS_DEFAULT] <- NA_real_
        
        futilityBounds <- .applyDirectionOfAlternative(futilityBounds, directionUpper,
            type = "negateIfLower", phase = "planning"
        )
    }

    if ((length(stDev) == 1) && (designPlan$groups == 2)) {
        stDev <- rep(stDev, 2)
    }

    if (designPlan$groups == 1) {
        criticalValuesEffectScaleUpper <- thetaH0 + criticalValues * stDev /
            sqrt(design$informationRates %*% t(maxNumberOfSubjects))
        criticalValuesEffectScaleLower <- thetaH0 - criticalValues * stDev /
            sqrt(design$informationRates %*% t(maxNumberOfSubjects))
        if (design$kMax > 1 && !.isTrialDesignFisher(design) && !all(is.na(futilityBounds))) {
            futilityBoundsEffectScaleUpper <- thetaH0 + futilityBounds * stDev /
                sqrt(design$informationRates[1:(design$kMax - 1)] %*% t(maxNumberOfSubjects))
        }
        if (design$kMax > 1 && !.isTrialDesignFisher(design) && design$sided == 2 && design$kMax > 1 &&
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
        if (design$kMax > 1 && !.isTrialDesignFisher(design) && !all(is.na(futilityBounds))) {
            futilityBoundsEffectScaleUpper <- thetaH0 + futilityBounds *
                sqrt(1 + allocationRatioPlanned) *
                sqrt(stDev[1]^2 + allocationRatioPlanned * stDev[2]^2) /
                (sqrt(allocationRatioPlanned *
                    design$informationRates[1:(design$kMax - 1)] %*% t(maxNumberOfSubjects)))
        }
        if (design$kMax > 1 && !.isTrialDesignFisher(design) && design$sided == 2 && design$kMax > 1 &&
                (design$typeOfDesign == C_TYPE_OF_DESIGN_PT ||
                    !is.null(design$typeBetaSpending) && design$typeBetaSpending != "none")) {
            futilityBoundsEffectScaleLower <- thetaH0 - futilityBounds *
                sqrt(1 + allocationRatioPlanned) *
                sqrt(stDev[1]^2 + allocationRatioPlanned * stDev[2]^2) /
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

        if (design$kMax > 1 && !.isTrialDesignFisher(design) && !all(is.na(futilityBounds))) {
            futilityBoundsEffectScaleUpper <- thetaH0 + futilityBounds *
                sqrt((1 + allocationRatioPlanned) / allocationRatioPlanned) *
                sqrt(stDev[1]^2 + thetaH0^2 * allocationRatioPlanned * stDev[2]^2) /
                (sqrt(design$informationRates[1:(design$kMax - 1)] %*% t(maxNumberOfSubjects)))
        }
        if (design$kMax > 1 && !.isTrialDesignFisher(design) && design$sided == 2 && design$kMax > 1 &&
                (design$typeOfDesign == C_TYPE_OF_DESIGN_PT ||
                    !is.null(design$typeBetaSpending) && design$typeBetaSpending != "none")) {
            futilityBoundsEffectScaleLower <- thetaH0 - futilityBounds *
                sqrt((1 + allocationRatioPlanned) / allocationRatioPlanned) *
                sqrt(stDev[1]^2 + thetaH0^2 * allocationRatioPlanned * stDev[2]^2) /
                (sqrt(design$informationRates[1:(design$kMax - 1)] %*% t(maxNumberOfSubjects)))
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

#
# Effect scale boundary data: rates
#

.getEffectScaleBoundaryDataRatesCorrected <- function(boundaries) {
    boundaries[!is.na(boundaries) & (boundaries < 0 | boundaries > 1)] <- NA_real_
    return(boundaries)
}

.getEffectScaleBoundaryDataRatesPi <- function(boundary, pi2, thetaH0, n1, n2, allocationRatio, directionUpper, method) {
    tryCatch(
        {
            pi1Bound <- stats::uniroot(
                function(x) {
                    fm <- .getFarringtonManningValues(
                        rate1 = x,
                        rate2 = pi2,
                        theta = thetaH0,
                        allocation = allocationRatio,
                        method = method
                    )
                    if (method == "diff") {
                        (x - pi2 - thetaH0) /
                            sqrt(fm$ml1 * (1 - fm$ml1) / n1 + fm$ml2 * (1 - fm$ml2) / n2) -
                            (2 * directionUpper - 1) * boundary
                    } else {
                        (x - thetaH0 * pi2) /
                            sqrt(fm$ml1 * (1 - fm$ml1) / n1 + thetaH0^2 * fm$ml2 * (1 - fm$ml2) / n2) -
                            (2 * directionUpper - 1) * boundary
                    }
                },
                lower = 0,
                upper = 1,
                tol = .Machine$double.eps^0.5
            )$root
        },
        error = function(e) {
            pi1Bound <<- NA_real_
        }
    )
    return(pi1Bound)
}

.getEffectScaleBoundaryDataRates <- function(designPlan) {
    design <- designPlan$.design
    thetaH0 <- designPlan$thetaH0
    pi2 <- designPlan$pi2
    maxNumberOfSubjects <- designPlan$maxNumberOfSubjects
    allocationRatioPlanned <- designPlan$allocationRatioPlanned
    nParameters <- length(maxNumberOfSubjects)
    directionUpper <- .getDirectionUpper(designPlan, nParameters)

    criticalValuesEffectScaleUpper <- matrix(, nrow = design$kMax, ncol = nParameters)
    criticalValuesEffectScaleLower <- matrix(, nrow = design$kMax, ncol = nParameters)
    futilityBoundsEffectScaleUpper <- matrix(, nrow = design$kMax - 1, ncol = nParameters)
    futilityBoundsEffectScaleLower <- matrix(, nrow = design$kMax - 1, ncol = nParameters)
    if (length(allocationRatioPlanned) == 1) {
        allocationRatioPlanned <- rep(allocationRatioPlanned, nParameters)
    }
    futilityBounds <- .getFutilityBounds(design)
    futilityBounds[!is.na(futilityBounds) & futilityBounds <= C_FUTILITY_BOUNDS_DEFAULT] <- NA_real_

    if (designPlan$groups == 1) {
        criticalValues <- .getCriticalValues(design)
        n1 <- design$informationRates %*% t(maxNumberOfSubjects)
        for (j in 1:nParameters) {
            criticalValuesEffectScaleUpper[, j] <- thetaH0 +
                (2 * directionUpper[j] - 1) *
                    criticalValues *
                    sqrt(thetaH0 * (1 - thetaH0)) /
                    sqrt(n1[, j])
            if (design$sided == 2) {
                criticalValuesEffectScaleLower[, j] <- thetaH0 -
                    (2 * directionUpper[j] - 1) *
                        criticalValues *
                        sqrt(thetaH0 * (1 - thetaH0)) /
                        sqrt(n1[, j])
            }
            if (!.isTrialDesignFisher(design) && !all(is.na(futilityBounds))) {
                futilityBoundsEffectScaleUpper[, j] <- thetaH0 +
                    (2 * directionUpper[j] - 1) *
                        futilityBounds *
                        sqrt(thetaH0 * (1 - thetaH0)) /
                        sqrt(n1[1:(design$kMax - 1), j])
            }
            if (
                !.isTrialDesignFisher(design) &&
                    design$sided == 2 &&
                    design$kMax > 1 &&
                    (design$typeOfDesign == C_TYPE_OF_DESIGN_PT ||
                        !is.null(design$typeBetaSpending) &&
                            design$typeBetaSpending != "none")
                ) {
                futilityBoundsEffectScaleLower[, j] <- thetaH0 -
                    (2 * directionUpper[j] - 1) *
                        futilityBounds *
                        sqrt(thetaH0 * (1 - thetaH0)) /
                        sqrt(n1[1:(design$kMax - 1), j])
            }
        }

        criticalValuesEffectScaleLower <- .getEffectScaleBoundaryDataRatesCorrected(criticalValuesEffectScaleLower)
        criticalValuesEffectScaleUpper <- .getEffectScaleBoundaryDataRatesCorrected(criticalValuesEffectScaleUpper)
        futilityBoundsEffectScaleLower <- .getEffectScaleBoundaryDataRatesCorrected(futilityBoundsEffectScaleLower)
        futilityBoundsEffectScaleUpper <- .getEffectScaleBoundaryDataRatesCorrected(futilityBoundsEffectScaleUpper)
    } else if (!designPlan$riskRatio) {
        boundaries <- .getCriticalValues(design)
        # calculate pi1 that solves (pi1 - pi2 - thetaH0) / SE(pi1 - pi2 - thetaH0)
        # = crit by using Farrington & Manning approach
        for (j in 1:nParameters) {
            n1 <- allocationRatioPlanned[j] *
                design$informationRates *
                maxNumberOfSubjects[j] /
                (1 + allocationRatioPlanned[j])
            n2 <- n1 / allocationRatioPlanned[j]

            for (i in seq_len(length(boundaries))) {
                criticalValuesEffectScaleUpper[i, j] <- .getEffectScaleBoundaryDataRatesPi(
                    boundaries[i],
                    pi2,
                    thetaH0,
                    n1[i],
                    n2[i],
                    allocationRatioPlanned[j],
                    directionUpper[j],
                    "diff"
                ) -
                    pi2
            }
            if (design$sided == 2) {
                for (i in seq_len(length(boundaries))) {
                    criticalValuesEffectScaleLower[i, j] <- .getEffectScaleBoundaryDataRatesPi(
                        -boundaries[i],
                        pi2,
                        thetaH0,
                        n1[i],
                        n2[i],
                        allocationRatioPlanned[j],
                        directionUpper[j],
                        "diff"
                    ) -
                        pi2
                }
            }
        }
        if (!.isTrialDesignFisher(design) && !all(is.na(futilityBounds))) {
            boundaries <- futilityBounds
            for (j in 1:nParameters) {
                n1 <- allocationRatioPlanned[j] *
                    design$informationRates *
                    maxNumberOfSubjects[j] /
                    (1 + allocationRatioPlanned[j])
                n2 <- n1 / allocationRatioPlanned[j]
                for (i in seq_len(length(boundaries))) {
                    futilityBoundsEffectScaleUpper[i, j] <- .getEffectScaleBoundaryDataRatesPi(
                        boundaries[i],
                        pi2,
                        thetaH0,
                        n1[i],
                        n2[i],
                        allocationRatioPlanned[j],
                        directionUpper[j],
                        "diff"
                    ) -
                        pi2
                }

                if (
                    !.isTrialDesignFisher(design) &&
                        design$sided == 2 &&
                        design$kMax > 1 &&
                        (design$typeOfDesign == C_TYPE_OF_DESIGN_PT ||
                            !is.null(design$typeBetaSpending) &&
                                design$typeBetaSpending != "none")
                    ) {
                    for (i in seq_len(length(boundaries))) {
                        futilityBoundsEffectScaleLower[i, j] <- .getEffectScaleBoundaryDataRatesPi(
                            -boundaries[i],
                            pi2,
                            thetaH0,
                            n1[i],
                            n2[i],
                            allocationRatioPlanned[j],
                            directionUpper[j],
                            "diff"
                        ) -
                            pi2
                    }
                }
            }
        }
    } else {
        boundaries <- .getCriticalValues(design)
        # calculate pi1 that solves (pi1 - thetaH0 * pi2) / SE(pi1 - thetaH0 * pi2)
        # = crit by using Farrington & Manning approach
        for (j in 1:nParameters) {
            n1 <- allocationRatioPlanned[j] *
                design$informationRates *
                maxNumberOfSubjects[j] /
                (1 + allocationRatioPlanned[j])
            n2 <- n1 / allocationRatioPlanned[j]

            for (i in seq_len(length(boundaries))) {
                criticalValuesEffectScaleUpper[i, j] <- .getEffectScaleBoundaryDataRatesPi(
                    boundaries[i],
                    pi2,
                    thetaH0,
                    n1[i],
                    n2[i],
                    allocationRatioPlanned[j],
                    directionUpper[j],
                    "ratio"
                ) /
                    pi2
            }
            if (design$sided == 2) {
                for (i in seq_len(length(boundaries))) {
                    criticalValuesEffectScaleLower[i, j] <- .getEffectScaleBoundaryDataRatesPi(
                        -boundaries[i],
                        pi2,
                        thetaH0,
                        n1[i],
                        n2[i],
                        allocationRatioPlanned[j],
                        directionUpper[j],
                        "ratio"
                    ) /
                        pi2
                }
            }
        }
        if (!.isTrialDesignFisher(design) && !all(is.na(futilityBounds))) {
            boundaries <- futilityBounds
            for (j in (1:nParameters)) {
                n1 <- allocationRatioPlanned[j] *
                    design$informationRates *
                    maxNumberOfSubjects[j] /
                    (1 + allocationRatioPlanned[j])
                n2 <- n1 / allocationRatioPlanned[j]
                for (i in seq_len(length(boundaries))) {
                    futilityBoundsEffectScaleUpper[i, j] <- .getEffectScaleBoundaryDataRatesPi(
                        boundaries[i],
                        pi2,
                        thetaH0,
                        n1[i],
                        n2[i],
                        allocationRatioPlanned[j],
                        directionUpper[j],
                        "ratio"
                    ) /
                        pi2
                }

                if (
                    !.isTrialDesignFisher(design) &&
                        design$sided == 2 &&
                        design$kMax > 1 &&
                        (design$typeOfDesign == C_TYPE_OF_DESIGN_PT ||
                            !is.null(design$typeBetaSpending) &&
                                design$typeBetaSpending != "none")
                    ) {
                    for (i in seq_len(length(boundaries))) {
                        futilityBoundsEffectScaleLower[i, j] <- .getEffectScaleBoundaryDataRatesPi(
                            -boundaries[i],
                            pi2,
                            thetaH0,
                            n1[i],
                            n2[i],
                            allocationRatioPlanned[j],
                            directionUpper[j],
                            "ratio"
                        ) /
                            pi2
                    }
                }
            }
        }
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

#
# Effect scale boundary data: survival
#

.getEffectScaleBoundaryDataSurvival <- function(designPlan) {
    design <- designPlan$.design
    thetaH0 <- designPlan$thetaH0
    cumulativeEventsPerStage <- designPlan$cumulativeEventsPerStage
    allocationRatioPlanned <- designPlan$allocationRatioPlanned
    nParameters <- ifelse(design$kMax == 1, length(cumulativeEventsPerStage), ncol(cumulativeEventsPerStage))
    directionUpper <- .getDirectionUpper(designPlan, nParameters)
    if (length(allocationRatioPlanned) == 1) {
        allocationRatioPlanned <- rep(allocationRatioPlanned, nParameters)
    }

    futilityBounds <- .getFutilityBounds(design)
    futilityBounds[!is.na(futilityBounds) & futilityBounds <= C_FUTILITY_BOUNDS_DEFAULT] <- NA_real_

    criticalValuesEffectScaleUpper <- matrix(, nrow = design$kMax, ncol = nParameters)
    criticalValuesEffectScaleLower <- matrix(, nrow = design$kMax, ncol = nParameters)
    futilityBoundsEffectScaleUpper <- matrix(, nrow = design$kMax - 1, ncol = nParameters)
    futilityBoundsEffectScaleLower <- matrix(, nrow = design$kMax - 1, ncol = nParameters)

    criticalValues <- .getCriticalValues(design)
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

#
# Effect scale boundary data: count data
#

#' Find Count Data Theta Using Uniroot
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
.getEffectScaleBoundaryCountDataTheta <- function(
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

    if (is.na(followUpTime) && is.na(fixedExposureTime)) {
        followUpTime <- studyTime - max(accrualTime)
    }

    nParameters <- ifelse(identical(designPlan$.objectType, "power"), 1, length(lambda1))

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

    directionUpper <- .getDirectionUpper(designPlan, nParameters)

    informationRates <- design$informationRates
    criticalValues <- .getCriticalValues(design)
    futilityBounds <- .getFutilityBounds(design)
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

        # calculate theta that solves (ln(theta) - ln(thetaH0)) sqrt(FisherInformation_k) = boundary
        for (j in seq_len(length(criticalValues))) {
            if (all(is.na(numberOfSubjects[, iCase]))) {
                numberOfSubjectsPerStage <- maxNumberOfSubjects[iCase]
            } else {
                numberOfSubjectsPerStage <- numberOfSubjects[j, iCase]
            }
            criticalValuesEffectScaleUpper[j, iCase] <- .getEffectScaleBoundaryCountDataTheta(
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
                criticalValuesEffectScaleLower[j, iCase] <- .getEffectScaleBoundaryCountDataTheta(
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
                futilityBoundsEffectScaleUpper[j, iCase] <- .getEffectScaleBoundaryCountDataTheta(
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
                    futilityBoundsEffectScaleLower[j, iCase] <- .getEffectScaleBoundaryCountDataTheta(
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
