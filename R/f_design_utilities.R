## |
## |  *Design utilities*
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
## |  File version: $Revision: 6631 $
## |  Last changed: $Date: 2022-10-25 13:49:41 +0200 (Tue, 25 Oct 2022) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_core_assertions.R
#' @include f_core_utilities.R
NULL

.getInformationRatesDefault <- function(kMax) {
    return(c(1:kMax) / kMax)
}

.getDefaultDesign <- function(...,
        type = c("sampleSize", "power", "simulation", "analysis"),
        ignore = c()) {
    type <- match.arg(type)

    alpha <- .getOptionalArgument("alpha", ...)
    if (is.null(alpha)) {
        alpha <- NA_real_
    } else {
        ignore <- c(ignore, "alpha")
    }

    beta <- .getOptionalArgument("beta", ...)
    if (is.null(beta)) {
        beta <- NA_real_
    } else {
        ignore <- c(ignore, "beta")
    }

    sided <- .getOptionalArgument("sided", ...)
    if (is.null(sided)) {
        sided <- 1L
    } else {
        ignore <- c(ignore, "sided")
    }

    twoSidedPower <- .getOptionalArgument("twoSidedPower", ...)
    if (is.null(twoSidedPower)) {
        if (type %in% c("power", "simulation") && sided == 2) {
            twoSidedPower <- TRUE
        } else {
            twoSidedPower <- C_TWO_SIDED_POWER_DEFAULT
        }
    } else {
        ignore <- c(ignore, "twoSidedPower")
    }
    if (type %in% c("analysis", "simulation")) {
        design <- getDesignInverseNormal(
            kMax = 1, alpha = alpha, beta = beta,
            sided = sided, twoSidedPower = twoSidedPower
        )
    } else {
        design <- getDesignGroupSequential(
            kMax = 1, alpha = alpha, beta = beta,
            sided = sided, twoSidedPower = twoSidedPower
        )
    }
    return(design)
}

.getDesignArgumentsToIgnoreAtUnknownArgumentCheck <- function(design, powerCalculationEnabled = FALSE) {
    baseArgsToIgnore <- c("showObservedInformationRatesMessage", "showWarnings")

    if (design$kMax > 1) {
        return(baseArgsToIgnore)
    }

    if (powerCalculationEnabled) {
        return(c(baseArgsToIgnore, "alpha", "sided"))
    }

    return(c(baseArgsToIgnore, "alpha", "beta", "sided", "twoSidedPower"))
}


.getValidatedFutilityBounds <- function(design, kMaxLowerBound = 1,
        writeToDesign = TRUE, twoSidedWarningForDefaultValues = TRUE) {
    .assertIsTrialDesignInverseNormalOrGroupSequential(design)
    return(.getValidatedFutilityBoundsOrAlpha0Vec(
        design = design, parameterName = "futilityBounds",
        defaultValue = C_FUTILITY_BOUNDS_DEFAULT, kMaxLowerBound = kMaxLowerBound,
        writeToDesign = writeToDesign, twoSidedWarningForDefaultValues = twoSidedWarningForDefaultValues
    ))
}

.getValidatedAlpha0Vec <- function(design, kMaxLowerBound = 1,
        writeToDesign = TRUE, twoSidedWarningForDefaultValues = TRUE) {
    .assertIsTrialDesignFisher(design)
    return(.getValidatedFutilityBoundsOrAlpha0Vec(
        design = design, parameterName = "alpha0Vec",
        defaultValue = C_ALPHA_0_VEC_DEFAULT, kMaxLowerBound = kMaxLowerBound,
        writeToDesign = writeToDesign, twoSidedWarningForDefaultValues = twoSidedWarningForDefaultValues
    ))
}

.getValidatedFutilityBoundsOrAlpha0Vec <- function(design, parameterName, defaultValue,
        kMaxLowerBound, writeToDesign, twoSidedWarningForDefaultValues = TRUE) {
    parameterValues <- design[[parameterName]]

    if (length(parameterValues) > 1) {
        .assertIsNumericVector(parameterValues, parameterName, naAllowed = TRUE)
    }

    kMaxUpperBound <- ifelse(.isTrialDesignFisher(design), C_KMAX_UPPER_BOUND_FISHER, C_KMAX_UPPER_BOUND)
    if (.isDefinedArgument(parameterValues) && .isDefinedArgument(design$kMax)) {
        if (.isTrialDesignFisher(design)) {
            .assertIsValidAlpha0Vec(parameterValues,
                kMax = design$kMax,
                kMaxLowerBound = kMaxLowerBound, kMaxUpperBound = kMaxUpperBound
            )
        } else {
            .assertAreValidFutilityBounds(parameterValues,
                kMax = design$kMax,
                kMaxLowerBound = kMaxLowerBound, kMaxUpperBound = kMaxUpperBound
            )
        }
    }

    if (design$sided == 2 && .isDefinedArgument(parameterValues) &&
            (!.isTrialDesignInverseNormalOrGroupSequential(design) ||
                (design$typeOfDesign != C_TYPE_OF_DESIGN_PT) && !.isBetaSpendingDesignType(design$typeBetaSpending)
            ) &&
            (twoSidedWarningForDefaultValues && !all(is.na(parameterValues)) ||
                (!twoSidedWarningForDefaultValues && any(na.omit(parameterValues) != defaultValue)))) {
        warning("'", parameterName, "' (", .arrayToString(parameterValues),
            ") will be ignored because the design is two-sided",
            call. = FALSE
        )
        parameterValues <- rep(defaultValue, design$kMax - 1)
    }

    if (writeToDesign) {
        .setParameterType(design, parameterName, C_PARAM_USER_DEFINED)
    }

    if (.isUndefinedArgument(design$informationRates) && .isUndefinedArgument(parameterValues)) {
        if (writeToDesign) {
            if (.setKMaxToDefaultIfUndefined(design, writeToDesign) || design$kMax == C_KMAX_DEFAULT) {
                .setParameterType(design, parameterName, C_PARAM_DEFAULT_VALUE)
            } else {
                .setParameterType(design, parameterName, C_PARAM_DERIVED)
            }
        }

        return(rep(defaultValue, design$kMax - 1))
    }

    if (.isDefinedArgument(design$informationRates) && .isUndefinedArgument(parameterValues)) {
        if (writeToDesign) {
            if (.isUndefinedArgument(design$kMax)) {
                .setKMax(design, kMax = length(design$informationRates))
            }
            .setParameterType(design, parameterName, ifelse(design$kMax == C_KMAX_DEFAULT,
                C_PARAM_DEFAULT_VALUE, C_PARAM_DERIVED
            ))
        }
        return(rep(defaultValue, design$kMax - 1))
    }

    if (.isUndefinedArgument(design$informationRates) &&
            .isDefinedArgument(parameterValues, argumentExistsValidationEnabled = FALSE)) {
        if (writeToDesign) {
            .setKMax(design, kMax = length(parameterValues) + 1)
            if (.isDefaultVector(parameterValues, rep(defaultValue, design$kMax - 1))) {
                .setParameterType(design, parameterName, C_PARAM_DEFAULT_VALUE)
            }
        }

        if (.isBetaSpendingOrPampallonaTsiatisDesignWithDefinedFutilityBounds(design, parameterName, writeToDesign)) {
            return(rep(defaultValue, design$kMax - 1))
        }

        return(parameterValues)
    }

    if (writeToDesign) {
        .setKMax(design, kMax = length(parameterValues) + 1)
        if (.isDefaultVector(parameterValues, rep(defaultValue, design$kMax - 1))) {
            .setParameterType(design, parameterName, C_PARAM_DEFAULT_VALUE)
        }
    }

    if (.isTrialDesignFisher(design)) {
        .assertIsValidAlpha0Vec(parameterValues,
            kMax = design$kMax,
            kMaxLowerBound = kMaxLowerBound, kMaxUpperBound = kMaxUpperBound
        )
    } else {
        .assertAreValidFutilityBounds(parameterValues,
            kMax = design$kMax,
            kMaxLowerBound = kMaxLowerBound, kMaxUpperBound = kMaxUpperBound
        )
    }

    if (.isBetaSpendingOrPampallonaTsiatisDesignWithDefinedFutilityBounds(design, parameterName, writeToDesign)) {
        return(rep(defaultValue, design$kMax - 1))
    }

    return(parameterValues)
}

# Check whether design is a beta spending or Pampallona Tsiatis design
.isBetaSpendingOrPampallonaTsiatisDesignWithDefinedFutilityBounds <- function(design, parameterName, writeToDesign) {
    if (.isTrialDesignFisher(design)) {
        return(FALSE)
    }

    if (!.isBetaSpendingDesignType(design$typeBetaSpending) && design$typeOfDesign != C_TYPE_OF_DESIGN_PT) {
        return(FALSE)
    }

    if (design$.getParameterType(parameterName) == C_PARAM_USER_DEFINED) {
        warning("'", parameterName, "' (", .arrayToString(design[[parameterName]]),
            ") will be ignored because it will be calculated",
            call. = FALSE
        )
    } else if (design$.getParameterType(parameterName) == C_PARAM_GENERATED) {
        return(FALSE)
    }

    if (writeToDesign) {
        .setParameterType(design, parameterName, C_PARAM_DEFAULT_VALUE)
    }
    return(TRUE)
}

.setKMax <- function(design, kMax) {
    design$kMax <- as.integer(kMax)
    .setParameterType(design, "kMax", C_PARAM_DERIVED)
    invisible(kMax)
}

.getValidatedInformationRates <- function(design, kMaxLowerBound = 1L, writeToDesign = TRUE) {
    kMaxUpperBound <- ifelse(.isTrialDesignFisher(design), C_KMAX_UPPER_BOUND_FISHER, C_KMAX_UPPER_BOUND)
    if (.isDefinedArgument(design$informationRates) && .isDefinedArgument(design$kMax)) {
        .assertAreValidInformationRates(
            informationRates = design$informationRates,
            kMax = design$kMax, kMaxLowerBound = kMaxLowerBound, kMaxUpperBound = kMaxUpperBound
        )
    }

    .setParameterType(design, "informationRates", C_PARAM_USER_DEFINED)

    if (.isTrialDesignFisher(design)) {
        futilityBounds <- design$alpha0Vec
    } else {
        futilityBounds <- design$futilityBounds
    }

    if (.isUndefinedArgument(design$informationRates) && .isUndefinedArgument(futilityBounds)) {
        if (writeToDesign) {
            if (.setKMaxToDefaultIfUndefined(design, writeToDesign) || design$kMax == C_KMAX_DEFAULT) {
                .setParameterType(design, "informationRates", C_PARAM_DEFAULT_VALUE)
            } else {
                .setParameterType(design, "informationRates", C_PARAM_DERIVED)
            }
        }
        return((1:design$kMax) / design$kMax)
    }

    if (.isDefinedArgument(design$informationRates) && .isUndefinedArgument(futilityBounds)) {
        if (writeToDesign) {
            .setKMax(design, kMax = length(design$informationRates))
            if (.isDefaultVector(design$informationRates, (1:design$kMax) / design$kMax)) {
                .setParameterType(design, "informationRates", C_PARAM_DEFAULT_VALUE)
            }
        }
        .assertAreValidInformationRates(
            informationRates = design$informationRates,
            kMax = design$kMax, kMaxLowerBound = kMaxLowerBound, kMaxUpperBound = kMaxUpperBound
        )
        return(design$informationRates)
    }

    if (.isUndefinedArgument(design$informationRates) &&
            .isDefinedArgument(futilityBounds, argumentExistsValidationEnabled = FALSE)) {
        if (writeToDesign) {
            if (.isUndefinedArgument(design$kMax)) {
                .setKMax(design, kMax = length(futilityBounds) + 1)
            }
            .setParameterType(design, "informationRates", ifelse(design$kMax == C_KMAX_DEFAULT,
                C_PARAM_DEFAULT_VALUE, C_PARAM_DERIVED
            ))
        }
        return((1:design$kMax) / design$kMax)
    }

    if (writeToDesign) {
        .setKMax(design, kMax = length(design$informationRates))
        if (.isDefaultVector(design$informationRates, (1:design$kMax) / design$kMax)) {
            .setParameterType(design, "informationRates", C_PARAM_DEFAULT_VALUE)
        }
    }

    .assertAreValidInformationRates(
        informationRates = design$informationRates,
        kMax = design$kMax, kMaxLowerBound = kMaxLowerBound, kMaxUpperBound = kMaxUpperBound
    )

    return(design$informationRates)
}

.setKMaxToDefaultIfUndefined <- function(design, writeToDesign = TRUE) {
    if (writeToDesign && .isUndefinedArgument(design$kMax)) {
        design$kMax <- C_KMAX_DEFAULT
        design$.setParameterType("kMax", C_PARAM_DEFAULT_VALUE)
        return(TRUE)
    }
    return(FALSE)
}

.validateAlphaAndBeta <- function(design) {
    .assertDesignParameterExists(design, "alpha", C_ALPHA_DEFAULT)
    .assertDesignParameterExists(design, "beta", C_BETA_DEFAULT)
    .assertIsValidAlphaAndBeta(alpha = design$alpha, beta = design$beta)
}

.validateUserAlphaSpending <- function(design) {
    .assertIsTrialDesign(design)
    .assertDesignParameterExists(design, "userAlphaSpending", NA_real_)

    if ((design$isUserDefinedParameter("informationRates") ||
            (design$isDefaultParameter("informationRates") && !design$isUserDefinedParameter("kMax"))) &&
            length(design$informationRates) != length(design$userAlphaSpending)) {
        stop(sprintf(
            paste0(
                C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                "length of 'userAlphaSpending' (%s) must be equal to length of 'informationRates' (%s)"
            ),
            length(design$userAlphaSpending), length(design$informationRates)
        ))
    }

    if (length(design$userAlphaSpending) != design$kMax) {
        stop(sprintf(
            paste0(
                C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                "length of 'userAlphaSpending' (%s) must be equal to 'kMax' (%s)"
            ),
            length(design$userAlphaSpending), design$kMax
        ))
    }

    .validateUserAlphaSpendingLength(design)

    if (.isUndefinedArgument(design$alpha)) {
        design$alpha <- design$userAlphaSpending[design$kMax]
        design$.setParameterType("alpha", ifelse(design$alpha == C_ALPHA_DEFAULT,
            C_PARAM_DEFAULT_VALUE, C_PARAM_DERIVED
        ))
    }

    .assertIsValidAlpha(design$alpha)

    if (design$kMax > 1 && (design$userAlphaSpending[1] < 0 || design$userAlphaSpending[design$kMax] > design$alpha ||
            any(design$userAlphaSpending[2:design$kMax] - design$userAlphaSpending[1:(design$kMax - 1)] < 0))) {
        stop(sprintf(
            paste0(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'userAlphaSpending' = %s must be a vector that satisfies the following condition: ",
                "0 <= alpha_1 <= .. <= alpha_%s <= alpha = %s"
            ),
            .arrayToString(design$userAlphaSpending, vectorLookAndFeelEnabled = TRUE),
            design$kMax, design$alpha
        ))
    }
}

.validateUserBetaSpending <- function(design) {
    .assertIsTrialDesign(design)
    .assertDesignParameterExists(design, "userBetaSpending", NA_real_)

    if ((design$isUserDefinedParameter("informationRates") ||
            (design$isDefaultParameter("informationRates") && !design$isUserDefinedParameter("kMax"))) &&
            length(design$informationRates) != length(design$userBetaSpending)) {
        stop(sprintf(
            paste0(
                C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                "length of 'userBetaSpending' (%s) must be equal to length of 'informationRates' (%s)"
            ),
            length(design$userBetaSpending), length(design$informationRates)
        ))
    }

    if (length(design$userBetaSpending) != design$kMax) {
        stop(sprintf(
            paste0(
                C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                "length of 'userBetaSpending' (%s) must be equal to 'kMax' (%s)"
            ),
            length(design$userBetaSpending), design$kMax
        ))
    }

    if (length(design$userBetaSpending) < 2 || length(design$userBetaSpending) > C_KMAX_UPPER_BOUND) {
        stop(sprintf(
            paste0(
                C_EXCEPTION_TYPE_ARGUMENT_LENGTH_OUT_OF_BOUNDS,
                "length of 'userBetaSpending' (%s) is out of bounds [2; %s]"
            ),
            length(design$userBetaSpending), C_KMAX_UPPER_BOUND
        ))
    }

    if (.isUndefinedArgument(design$beta)) {
        design$beta <- design$userBetaSpending[design$kMax]
        design$.setParameterType("beta", ifelse(design$beta == C_BETA_DEFAULT,
            C_PARAM_DEFAULT_VALUE, C_PARAM_DERIVED
        ))
    }

    .assertIsValidBeta(beta = design$beta, alpha = design$alpha)

    if (design$kMax > 1 && (design$userBetaSpending[1] < 0 || design$userBetaSpending[design$kMax] > design$beta ||
            any(design$userBetaSpending[2:design$kMax] - design$userBetaSpending[1:(design$kMax - 1)] < 0))) {
        stop(sprintf(
            paste0(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'userBetaSpending' = %s must be a vector that satisfies the following condition: ",
                "0 <= beta_1 <= .. <= beta_%s <= beta = %s"
            ),
            .arrayToString(design$userBetaSpending, vectorLookAndFeelEnabled = TRUE),
            design$kMax, design$beta
        ))
    }
}

.validateUserAlphaSpendingLength <- function(design) {
    if (length(design$userAlphaSpending) < 1 || length(design$userAlphaSpending) > C_KMAX_UPPER_BOUND) {
        stop(sprintf(
            paste0(
                C_EXCEPTION_TYPE_ARGUMENT_LENGTH_OUT_OF_BOUNDS,
                "length of 'userAlphaSpending' (%s) is out of bounds [1; %s]"
            ),
            length(design$userAlphaSpending), C_KMAX_UPPER_BOUND
        ))
    }
}

.setKmaxBasedOnAlphaSpendingDefintion <- function(design) {
    if (.isTrialDesignFisher(design)) {
        if (design$method != C_FISHER_METHOD_USER_DEFINED_ALPHA) {
            return(invisible())
        }
    } else {
        if (design$typeOfDesign != C_TYPE_OF_DESIGN_AS_USER) {
            return(invisible())
        }
    }

    if (.isDefinedArgument(design$kMax)) {
        return(invisible())
    }

    if (.isUndefinedArgument(design$userAlphaSpending)) {
        return(invisible())
    }

    if (.isDefinedArgument(design$informationRates)) {
        return(invisible())
    }

    if (.isTrialDesignFisher(design)) {
        if (.isDefinedArgument(design$alpha0Vec)) {
            return(invisible())
        }
    } else {
        if (.isDefinedArgument(design$futilityBounds)) {
            return(invisible())
        }
    }

    .validateUserAlphaSpendingLength(design)

    .setKMax(design, kMax = length(design$userAlphaSpending))
}

# This function generates the piecewise exponential survival function or (if kappa != 1) a Weibull cdf
.getPiecewiseExponentialDistributionSingleTime <- function(time, piecewiseLambda, piecewiseSurvivalTime = NA_real_, kappa) {
    if (length(piecewiseLambda) == 1) {
        if (kappa <= 0) {
            stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'kappa' (", kappa, ") must be > 0")
        }

        return(pweibull(time, kappa, scale = 1 / piecewiseLambda, lower.tail = TRUE, log.p = FALSE))
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
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "Weibull distribution cannot be used for piecewise survival definition"
        )
    }

    len <- length(piecewiseSurvivalTime)
    for (i in 1:len) {
        if (time <= piecewiseSurvivalTime[i]) {
            if (i == 1) {
                return(1 - exp(-(piecewiseLambda[1] * time)))
            }
            y <- piecewiseLambda[1] * piecewiseSurvivalTime[1]
            if (i > 2) {
                y <- y + sum(piecewiseLambda[2:(i - 1)] *
                    (piecewiseSurvivalTime[2:(i - 1)] - piecewiseSurvivalTime[1:(i - 2)]))
            }
            y <- y + piecewiseLambda[i] * (time - piecewiseSurvivalTime[i - 1])
            return(1 - exp(-y))
        }
    }
    if (len == 1) {
        y <- piecewiseLambda[1] * piecewiseSurvivalTime[1] +
            piecewiseLambda[len + 1] * (time - piecewiseSurvivalTime[len])
    } else {
        y <- piecewiseLambda[1] * piecewiseSurvivalTime[1] +
            sum(piecewiseLambda[2:len] * (piecewiseSurvivalTime[2:len] -
                piecewiseSurvivalTime[1:(len - 1)])) +
            piecewiseLambda[len + 1] * (time - piecewiseSurvivalTime[len])
    }
    return(1 - exp(-y))
}

.getPiecewiseExponentialSingleQuantile <- function(quantile, piecewiseLambda, piecewiseSurvivalTime, kappa) {
    if (length(piecewiseLambda) == 1) {
        if (kappa <= 0) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "kappa needs to a positive number"
            )
        }
        return((-log(1 - quantile))^(1 / kappa) / piecewiseLambda[1])
    }

    if (kappa != 1) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "Weibull distribution cannot be used for piecewise survival definition"
        )
    }

    cdfValues <- .getPiecewiseExponentialDistribution(piecewiseSurvivalTime,
        piecewiseSurvivalTime = piecewiseSurvivalTime, piecewiseLambda = piecewiseLambda, kappa = 1
    )
    cdfValues <- cdfValues[2:length(cdfValues)] # use values without a leading 0

    piecewiseSurvivalTime <- .getPiecewiseExpStartTimesWithoutLeadingZero(piecewiseSurvivalTime)

    len <- length(piecewiseSurvivalTime)
    for (i in 1:len) {
        if (quantile <= cdfValues[i]) {
            if (i == 1) {
                return(-log(1 - quantile) / piecewiseLambda[1])
            }
            y <- piecewiseLambda[1] * piecewiseSurvivalTime[1]
            if (i > 2) {
                y <- y + sum(piecewiseLambda[2:(i - 1)] *
                    (piecewiseSurvivalTime[2:(i - 1)] - piecewiseSurvivalTime[1:(i - 2)]))
            }
            return(piecewiseSurvivalTime[i - 1] - (log(1 - quantile) + y) / piecewiseLambda[i])
        }
    }

    if (len == 1) {
        return(piecewiseSurvivalTime[1] - (log(1 - quantile) + piecewiseLambda[1] *
            piecewiseSurvivalTime[1]) / piecewiseLambda[2])
    }

    y <- piecewiseLambda[1] * piecewiseSurvivalTime[1] +
        sum(piecewiseLambda[2:len] * (piecewiseSurvivalTime[2:len] -
            piecewiseSurvivalTime[1:(len - 1)]))

    return(piecewiseSurvivalTime[len] - (log(1 - quantile) + y) / piecewiseLambda[len + 1])
}

.getPiecewiseExponentialDistribution <- function(time, piecewiseLambda, piecewiseSurvivalTime, kappa) {
    if (length(time) == 1 && length(piecewiseSurvivalTime) == 1 &&
            identical(time, piecewiseSurvivalTime) && length(piecewiseLambda) > 1) {
        result <- c()
        for (lambda in piecewiseLambda) {
            result <- c(result, .getPiecewiseExponentialDistributionSingleTime(
                time, lambda, piecewiseSurvivalTime, kappa
            ))
        }
        return(result)
    }

    result <- c()
    for (timeValue in time) {
        result <- c(result, .getPiecewiseExponentialDistributionSingleTime(
            timeValue, piecewiseLambda, piecewiseSurvivalTime, kappa
        ))
    }
    return(result)
}

.getPiecewiseExponentialSettings <- function(..., piecewiseSurvivalTime = NA_real_,
        piecewiseLambda = NA_real_, kappa = 1) {
    if (!all(is.na(piecewiseLambda)) && is.list(piecewiseSurvivalTime)) {
        stop(
            C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
            "'piecewiseSurvivalTime' needs to be a numeric vector and not a list ",
            "because 'piecewiseLambda' (", piecewiseLambda, ") is defined separately"
        )
    }

    if (any(is.na(piecewiseSurvivalTime))) {
        .assertIsSingleNumber(kappa, "kappa")
    }

    if (length(piecewiseLambda) == 1 && !is.na(piecewiseLambda) &&
            length(piecewiseSurvivalTime) > 0 && !all(is.na(piecewiseSurvivalTime))) {
        warning("Argument 'piecewiseSurvivalTime' will be ignored because ",
            "length of 'piecewiseLambda' is 1",
            call. = FALSE
        )
    }

    setting <- PiecewiseSurvivalTime(
        piecewiseSurvivalTime = piecewiseSurvivalTime,
        lambda2 = piecewiseLambda,
        hazardRatio = 1, kappa = kappa,
        delayedResponseAllowed = FALSE
    )

    return(list(
        piecewiseSurvivalTime = setting$piecewiseSurvivalTime,
        piecewiseLambda = setting$lambda2
    ))
}

#'
#' @title
#' The Piecewise Exponential Distribution
#'
#' @description
#' Distribution function, quantile function and random number generation for the
#' piecewise exponential distribution.
#'
#' @param t,time Vector of time values.
#' @param q,quantile Vector of quantiles.
#' @param n Number of observations.
#' @param s,piecewiseSurvivalTime Vector of start times defining the "time pieces".
#' @param lambda,piecewiseLambda Vector of lambda values (hazard rates) corresponding to the start times.
#' @inheritParams param_kappa
#' @inheritParams param_three_dots
#'
#' @details
#' \code{getPiecewiseExponentialDistribution()} (short: \code{ppwexp()}),
#' \code{getPiecewiseExponentialQuantile()} (short: \code{qpwexp()}), and
#' \code{getPiecewiseExponentialRandomNumbers()} (short: \code{rpwexp()}) provide
#' probabilities, quantiles, and random numbers according to a piecewise
#' exponential or a Weibull distribution.
#' The piecewise definition is performed through a vector of
#' starting times (\code{piecewiseSurvivalTime}) and a vector of hazard rates (\code{piecewiseLambda}).
#' You can also use a list that defines the starting times and piecewise
#' lambdas together and define piecewiseSurvivalTime as this list.
#' The list needs to have the form, e.g.,
#' piecewiseSurvivalTime <- list(
#'     "0 - <6"   = 0.025,
#'     "6 - <9"   = 0.04,
#'     "9 - <15"  = 0.015,
#'     ">=15"      = 0.007) .
#' For the Weibull case, you can also specify a shape parameter kappa in order to
#' calculate probabilities, quantiles, or random numbers.
#' In this case, no piecewise definition is possible, i.e., only piecewiseLambda
#' (as a single value) and kappa need to be specified.
#'
#' @return A \code{\link[base]{numeric}} value or vector will be returned.
#'
#' @examples
#' # Calculate probabilties for a range of time values for a
#' # piecewise exponential distribution with hazard rates
#' # 0.025, 0.04, 0.015, and 0.007 in the intervals
#' # [0, 6), [6, 9), [9, 15), [15, Inf), respectively,
#' # and re-return the time values:
#' piecewiseSurvivalTime <- list(
#'     "0 - <6"   = 0.025,
#'     "6 - <9"   = 0.04,
#'     "9 - <15"  = 0.015,
#'     ">=15"     = 0.01
#' )
#' y <- getPiecewiseExponentialDistribution(seq(0, 150, 15),
#'     piecewiseSurvivalTime = piecewiseSurvivalTime
#' )
#' getPiecewiseExponentialQuantile(y,
#'     piecewiseSurvivalTime = piecewiseSurvivalTime
#' )
#'
#' @name utilitiesForPiecewiseExponentialDistribution
#'
NULL

#' @rdname utilitiesForPiecewiseExponentialDistribution
#' @export
getPiecewiseExponentialDistribution <- function(time, ...,
        piecewiseSurvivalTime = NA_real_, piecewiseLambda = NA_real_, kappa = 1) {
    .warnInCaseOfUnknownArguments(functionName = "getPiecewiseExponentialDistribution", ...)
    .assertIsNumericVector(time, "time")
    if (any(time < 0)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "time needs to be a non-negative number"
        )
    }

    settings <- .getPiecewiseExponentialSettings(
        piecewiseSurvivalTime = piecewiseSurvivalTime,
        piecewiseLambda = piecewiseLambda, kappa = kappa
    )

    return(.getPiecewiseExponentialDistribution(
        time = time,
        piecewiseSurvivalTime = settings$piecewiseSurvivalTime,
        piecewiseLambda = settings$piecewiseLambda, kappa = kappa
    ))
}

#' @rdname utilitiesForPiecewiseExponentialDistribution
#' @export
ppwexp <- function(t, ..., s = NA_real_, lambda = NA_real_, kappa = 1) {
    getPiecewiseExponentialDistribution(
        time = t,
        piecewiseSurvivalTime = s, piecewiseLambda = lambda, kappa = kappa, ...
    )
}

#' @rdname utilitiesForPiecewiseExponentialDistribution
#' @export
getPiecewiseExponentialQuantile <- function(quantile, ...,
        piecewiseSurvivalTime = NA_real_, piecewiseLambda = NA_real_, kappa = 1) {
    .warnInCaseOfUnknownArguments(functionName = "getPiecewiseExponentialQuantile", ...)
    .assertIsNumericVector(quantile, "quantile")
    if (any(quantile < 0) || any(quantile > 1)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "quantile needs to be within [0; 1]"
        )
    }

    settings <- .getPiecewiseExponentialSettings(
        piecewiseSurvivalTime = piecewiseSurvivalTime,
        piecewiseLambda = piecewiseLambda, kappa = kappa
    )

    result <- c()
    for (quantileValue in quantile) {
        result <- c(result, .getPiecewiseExponentialSingleQuantile(quantileValue,
            piecewiseSurvivalTime = settings$piecewiseSurvivalTime,
            piecewiseLambda = settings$piecewiseLambda, kappa
        ))
    }
    return(result)
}

#' @rdname utilitiesForPiecewiseExponentialDistribution
#' @export
qpwexp <- function(q, ..., s = NA_real_, lambda = NA_real_, kappa = 1) {
    getPiecewiseExponentialQuantile(
        quantile = q,
        piecewiseSurvivalTime = s, piecewiseLambda = lambda, kappa = kappa, ...
    )
}

.getPiecewiseExponentialRandomNumbersFast <- function(n, piecewiseSurvivalTime, piecewiseLambda) {
    result <- rexp(n, rate = piecewiseLambda[1])
    if (length(piecewiseSurvivalTime) > 1) {
        for (i in 2:length(piecewiseSurvivalTime)) {
            result <- ifelse(result < piecewiseSurvivalTime[i],
                result, piecewiseSurvivalTime[i] + rexp(n, rate = piecewiseLambda[i])
            )
        }
    }
    result
}

#' @rdname utilitiesForPiecewiseExponentialDistribution
#' @export
getPiecewiseExponentialRandomNumbers <- function(n, ...,
        piecewiseSurvivalTime = NA_real_, piecewiseLambda = NA_real_, kappa = 1) {
    .warnInCaseOfUnknownArguments(functionName = "getPiecewiseExponentialRandomNumbers", ...)
    .assertIsSingleInteger(n, "n", validateType = FALSE)
    .assertIsInClosedInterval(n, "n", lower = 1, upper = NULL)

    settings <- .getPiecewiseExponentialSettings(
        piecewiseSurvivalTime = piecewiseSurvivalTime,
        piecewiseLambda = piecewiseLambda, kappa = kappa
    )

    if (kappa == 1) {
        return(.getPiecewiseExponentialRandomNumbersFast(n,
            piecewiseSurvivalTime = settings$piecewiseSurvivalTime,
            piecewiseLambda = settings$piecewiseLambda
        ))
    }

    randomQuantiles <- runif(n, 0, 1)
    result <- c()
    for (quantile in randomQuantiles) {
        result <- c(result, .getPiecewiseExponentialSingleQuantile(quantile,
            piecewiseSurvivalTime = settings$piecewiseSurvivalTime,
            piecewiseLambda = settings$piecewiseLambda, kappa = kappa
        ))
    }
    return(result)
}

#' @rdname utilitiesForPiecewiseExponentialDistribution
#' @export
rpwexp <- function(n, ..., s = NA_real_, lambda = NA_real_, kappa = 1) {
    getPiecewiseExponentialRandomNumbers(
        n = n,
        piecewiseSurvivalTime = s, piecewiseLambda = lambda, kappa = kappa, ...
    )
}

#'
#' @title
#' Survival Helper Functions for Conversion of Pi, Lambda, Median
#'
#' @description
#' Functions to convert pi, lambda and median values into each other.
#'
#' @param piValue,pi1,pi2,lambda,median Value that shall be converted.
#' @inheritParams param_eventTime
#' @inheritParams param_kappa
#'
#' @details
#' Can be used, e.g., to convert median values into pi or lambda values for usage in
#' \code{\link[=getSampleSizeSurvival]{getSampleSizeSurvival()}} or \code{\link[=getPowerSurvival]{getPowerSurvival()}}.
#'
#' @return Returns a \code{\link[base]{numeric}} value or vector will be returned.
#'
#' @name utilitiesForSurvivalTrials
#'
NULL

#' @rdname utilitiesForSurvivalTrials
#' @export
getLambdaByPi <- function(piValue,
        eventTime = 12, # C_EVENT_TIME_DEFAULT
        kappa = 1) {
    .assertIsValidPi(piValue, "pi")
    .assertIsValidKappa(kappa)
    .assertIsSingleNumber(eventTime, "eventTime")
    .assertIsInOpenInterval(eventTime, "eventTime", lower = 0, upper = NULL)
    for (value in piValue) {
        if (value > 1 - 1e-16 && value < 1 + 1e-16) {
            stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'pi' must be != 1")
        }
    }
    return((-log(1 - piValue))^(1 / kappa) / eventTime)
}

#' @rdname utilitiesForSurvivalTrials
#' @export
getLambdaByMedian <- function(median, kappa = 1) {
    .assertIsNumericVector(median, "median")
    .assertIsValidKappa(kappa)
    return(log(2)^(1 / kappa) / median)
}

#' @rdname utilitiesForSurvivalTrials
#' @export
getHazardRatioByPi <- function(pi1, pi2,
        eventTime = 12, # C_EVENT_TIME_DEFAULT
        kappa = 1) {
    .assertIsValidPi(pi1, "pi1")
    .assertIsValidPi(pi2, "pi2")
    .assertIsValidKappa(kappa)
    .assertIsSingleNumber(eventTime, "eventTime")
    .assertIsInOpenInterval(eventTime, "eventTime", lower = 0, upper = NULL)
    return((getLambdaByPi(pi1, eventTime, kappa) / getLambdaByPi(pi2, eventTime, kappa))^kappa)
}

#' @rdname utilitiesForSurvivalTrials
#' @export
getPiByLambda <- function(lambda,
        eventTime = 12, # C_EVENT_TIME_DEFAULT
        kappa = 1) {
    .assertIsValidLambda(lambda)
    .assertIsValidKappa(kappa)
    .assertIsSingleNumber(eventTime, "eventTime")
    .assertIsInOpenInterval(eventTime, "eventTime", lower = 0, upper = NULL)
    x <- exp(-(lambda * eventTime)^kappa)
    if (any(x < 1e-15)) {
        warning("Calculation of pi (1) by lambda (", .arrayToString(round(lambda, 4)),
            ") results in a possible loss of precision because pi = 1 was returned but pi is not exactly 1",
            call. = FALSE
        )
    }
    return(1 - x)
}

# alternative: return(1 - exp(-(log(2)^(1 / kappa) / median * eventTime)^kappa))
#' @rdname utilitiesForSurvivalTrials
#' @export
getPiByMedian <- function(median,
        eventTime = 12, # C_EVENT_TIME_DEFAULT
        kappa = 1) {
    .assertIsNumericVector(median, "median")
    .assertIsValidKappa(kappa)
    .assertIsSingleNumber(eventTime, "eventTime")
    .assertIsInOpenInterval(eventTime, "eventTime", lower = 0, upper = NULL)
    return(1 - 2^(-(eventTime / median)^kappa))
}

#' @rdname utilitiesForSurvivalTrials
#' @export
getMedianByLambda <- function(lambda, kappa = 1) {
    .assertIsValidLambda(lambda)
    .assertIsValidKappa(kappa)
    return(log(2)^(1 / kappa) / lambda)
}

#' @rdname utilitiesForSurvivalTrials
#' @export
getMedianByPi <- function(piValue,
        eventTime = 12, # C_EVENT_TIME_DEFAULT
        kappa = 1) {
    .assertIsValidPi(piValue, "piValue")
    .assertIsSingleNumber(eventTime, "eventTime")
    .assertIsInOpenInterval(eventTime, "eventTime", lower = 0, upper = NULL)
    .assertIsValidKappa(kappa)
    getMedianByLambda(getLambdaByPi(piValue, eventTime, kappa), kappa)
}

.convertStageWiseToOverallValuesInner <- function(valuesPerStage) {
    eventsOverStages <- matrix(valuesPerStage, nrow = nrow(as.matrix(valuesPerStage)))
    eventsOverStages[is.na(eventsOverStages)] <- 0
    for (i in 1:ncol(as.matrix(valuesPerStage))) {
        eventsOverStages[, i] <- cumsum(eventsOverStages[, i])
    }
    return(eventsOverStages)
}

# example: .convertStageWiseToOverallValues(array(1:4, c(3, 4)))
.convertStageWiseToOverallValues <- function(valuesPerStage) {
    if (is.array(valuesPerStage) && length(dim(valuesPerStage)) == 3) {
        eventsOverStages <- array(dim = dim(valuesPerStage))
        for (g in 1:dim(valuesPerStage)[3]) {
            eventsTemp <- matrix(valuesPerStage[, , g], nrow = dim(valuesPerStage)[1])
            eventsOverStages[, , g] <- .convertStageWiseToOverallValuesInner(eventsTemp)
        }
        return(eventsOverStages)
    }

    return(.convertStageWiseToOverallValuesInner(valuesPerStage))
}

.getDesignParametersToShow <- function(paramaterSet) {
    if (is.null(paramaterSet[[".design"]])) {
        stop(
            C_EXCEPTION_TYPE_RUNTIME_ISSUE,
            "'paramaterSet' (", .getClassName(paramaterSet), ") does not contain '.design' field"
        )
    }

    designParametersToShow <- c(".design$stages")
    if (grepl("Dunnett", .getClassName(paramaterSet))) {
        designParametersToShow <- c(
            designParametersToShow,
            ".design$alpha",
            ".design$informationAtInterim",
            ".design$secondStageConditioning",
            ".design$sided"
        )
    } else {
        design <- paramaterSet$.design
        designParametersToShow <- c()
        if (design$kMax > 1) {
            if (is.null(paramaterSet[[".stageResults"]]) || .isTrialDesignGroupSequential(design)) {
                designParametersToShow <- c(designParametersToShow, ".design$informationRates")
            } else if (.isTrialDesignInverseNormal(design)) {
                designParametersToShow <- c(designParametersToShow, ".stageResults$weightsInverseNormal")
            } else if (.isTrialDesignFisher(design)) {
                designParametersToShow <- c(designParametersToShow, ".stageResults$weightsFisher")
            }
            if (design$.isDelayedResponseDesign()) {
                designParametersToShow <- c(designParametersToShow, ".design$delayedInformation")
            }
        }
        designParametersToShow <- c(designParametersToShow, ".design$criticalValues")
        if (design$.isDelayedResponseDesign()) {
            designParametersToShow <- c(designParametersToShow, ".design$decisionCriticalValues")
        }
        if (design$kMax > 1) {
            if (.isTrialDesignFisher(design)) {
                designParametersToShow <- c(designParametersToShow, ".design$alpha0Vec")
            } else {
                designParametersToShow <- c(designParametersToShow, ".design$futilityBounds")
            }
            designParametersToShow <- c(designParametersToShow, ".design$alphaSpent")
            designParametersToShow <- c(designParametersToShow, ".design$stageLevels")
        }
        if (design$sided == 2 && !grepl("Analysis|Simulation", .getClassName(paramaterSet)) &&
                (!inherits(paramaterSet, "TrialDesignPlan") || paramaterSet$.isSampleSizeObject())) {
            designParametersToShow <- c(designParametersToShow, ".design$twoSidedPower")
        }
        designParametersToShow <- c(designParametersToShow, ".design$alpha")
        if (!grepl("Analysis|Simulation", .getClassName(paramaterSet)) &&
                (!inherits(paramaterSet, "TrialDesignPlan") || paramaterSet$.isSampleSizeObject())) {
            designParametersToShow <- c(designParametersToShow, ".design$beta")
        }

        designParametersToShow <- c(designParametersToShow, ".design$sided")
    }
    return(designParametersToShow)
}

.isNoEarlyEfficacy <- function(design) {
    .assertIsTrialDesignInverseNormalOrGroupSequential(design)

    if (design$kMax == 1) {
        return(FALSE)
    }

    if (design$typeOfDesign == C_TYPE_OF_DESIGN_NO_EARLY_EFFICACY) {
        return(TRUE)
    }

    if (design$typeOfDesign != C_TYPE_OF_DESIGN_AS_USER) {
        return(FALSE)
    }

    indices <- design$userAlphaSpending == 0
    return(all(indices[1:(length(indices) - 1)]))
}

.addDelayedInformationRates <- function(dataFrame) {
    if (all(c("informationRates", "delayedInformation", "kMax", "stages") %in% colnames(dataFrame))) {
        kMax <- max(dataFrame$kMax)
        if (kMax > 1) {
            dataFrame$delayedInformationRates <- dataFrame$informationRates + dataFrame$delayedInformation
            dataFrame$delayedInformationRates[dataFrame$stages == kMax] <- NA_real_
        }
    }
    return(dataFrame)
}

.getNoEarlyEfficacyZeroCorrectedValues <- function(design, values) {
    if (design$kMax == 1) {
        return(values)
    }
    if (design$typeOfDesign == "noEarlyEfficacy") {
        values[1:(design$kMax - 1)] <- 0
    } else if (design$typeOfDesign == "asUser") {
        for (k in 1:(design$kMax - 1)) {
            if (!is.na(design$userAlphaSpending[k]) && 
                    abs(design$userAlphaSpending[k]) < 1e-16) {
                values[k] <- 0
            }
        }
    }
    return(values)
}

