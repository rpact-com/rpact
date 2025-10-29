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

.getFutilityBoundInformations <- function(..., information, sourceScale, targetScale) {
    if (!is.null(information) && !all(is.na(information)) && length(information) > 0) {
        len <- .getFutilityBoundVectorLength(sourceScale, targetScale)
        .assertIsNumericVector(information, "information", naAllowed = TRUE, len = len)
        return(list(
            information1 = information[1],
            information2 = information[2],
            vectorInput = TRUE,
            paramNames = c("information[1]", "information[2]")
        ))
    }

    information1 <- NA_real_
    information2 <- NA_real_
    if (length(list(...)) > 0) {
        information1 <- .getOptionalArgument("information1", optionalArgumentDefaultValue = NA_real_, ...)
        information2 <- .getOptionalArgument("information2", optionalArgumentDefaultValue = NA_real_, ...)
    }
    return(list(
        information1 = information1,
        information2 = information2,
        vectorInput = FALSE,
        paramNames = c("information1", "information2")
    ))
}

.addFutilityBoundParameterTypes <- function(result, args) {
    if (!is.null(args) && length(args) > 0) {
        for (argName in names(args)) {
            attr(result, argName) <- args[[argName]]
        }
    }
    class(result) <- c("FutilityBounds", class(result))
    return(result)
}

#'
#' @title
#' Print Futility Bounds
#'
#' @description
#' S3 print method for objects of class \code{FutilityBounds}. Prints the futility bounds as a numeric vector.
#'
#' @param x An object of class \code{FutilityBounds}.
#' @param ... Additional arguments passed to \code{print.default}.
#'
#' @keywords internal
#'
#' @export
#'
print.FutilityBounds <- function(x, ...) {
    print.default(as.numeric(x))
}

#'
#' @title
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
#' @param design The trial design. Required if either the \code{sourceScale} or
#' \code{targetScale} is \code{"reverseCondPower"} or if the conversion
#' involves conditional or predictive power in a group sequential or Fisher design.
#' Must be a one-sided two-stage group sequential design or Fisher's combination test design.
#' @param theta Numeric. The assumed effect size under the alternative hypothesis.
#' @param information Numeric vector of length 2. The information levels at the two stages.
#' @param naAllowed Logical. Indicates if \code{NA} `sourceValue` are permitted. Default is \code{FALSE}.
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
#'     information = c(10, 10),
#'     sourceValue = 0.5,
#'     sourceScale = "condPowerAtObserved",
#'     targetScale = "pValue"
#' )
#' }
#'
#' @export
#'
getFutilityBounds <- function(
        sourceValue,
        ...,
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
        design = NULL,
        theta = NA_real_,
        information = NA_real_,
        naAllowed = FALSE) {
    sourceScale <- match.arg(sourceScale)
    targetScale <- match.arg(targetScale)

    .assertIsNumericVector(sourceValue, "sourceValue", naAllowed = naAllowed)
    if (is(sourceValue, "FutilityBounds")) {
        sourceValue <- as.numeric(sourceValue)
    }

    infos <- .getFutilityBoundInformations(
        information = information,
        sourceScale = sourceScale,
        targetScale = targetScale,
        ...
    )
    information1 <- infos$information1
    information2 <- infos$information2
    .assertAreValidFutilityBoundsScaleArguments(
        design = design,
        sourceScale = sourceScale,
        targetScale = targetScale,
        theta = theta,
        information = information,
        ...
    )

    args <- list(
        `sourceValue` = list(
            `value` = sourceValue,
            `type` = C_PARAM_USER_DEFINED
        ),
        `sourceScale` = list(
            `value` = sourceScale,
            `type` = ifelse(sourceScale == "zValue", C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED)
        ),
        `targetScale` = list(
            `value` = targetScale,
            `type` = ifelse(targetScale == "zValue", C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED)
        ),
        `theta` = list(
            `value` = theta,
            `type` = ifelse(is.na(theta), C_PARAM_NOT_APPLICABLE, C_PARAM_USER_DEFINED)
        ),
        `information` = list(
            `value` = information,
            `type` = ifelse(all(is.na(information)), C_PARAM_NOT_APPLICABLE, C_PARAM_USER_DEFINED)
        )
    )

    if (sourceScale == targetScale) {
        return(.addFutilityBoundParameterTypes(sourceValue, args))
    }

    if ((sourceScale == "reverseCondPower" ||
            targetScale == "reverseCondPower") &&
            (is.null(design) || !.isTrialDesignInverseNormalOrGroupSequential(design))) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "design not correctly specified, needs to be one-sided two-stage group sequential",
            call. = FALSE
        )
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
        if (design$kMax != 2 || design$sided != 1) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "Futility bounds conversion is available only for one-sided two-stage designs",
                call. = FALSE
            )
        }
    }

    .assertIsSingleNumber(information1, infos$paramNames[1], naAllowed = TRUE)
    .assertIsInOpenInterval(
        information1,
        infos$paramNames[1],
        lower = 0,
        upper = Inf,
        naAllowed = TRUE
    )

    .assertIsSingleNumber(information2, infos$paramNames[2], naAllowed = TRUE)
    .assertIsInOpenInterval(
        information2,
        infos$paramNames[2],
        lower = 0,
        upper = Inf,
        naAllowed = TRUE
    )

    .assertIsSingleNumber(theta, "theta", naAllowed = TRUE)

    if (sourceScale %in% c(
            "conditionalPower", "condPowerAtObserved",
            "predictivePower", "reverseCondPower", "pValue"
        )) {
        .assertIsInClosedInterval(
            sourceValue,
            "sourceValue",
            lower = 0,
            upper = 1,
            naAllowed = TRUE
        )
    }

    sourceValues <- numeric()
    if (sourceScale == "zValue") {
        sourceValues <- sourceValue
    } else if (sourceScale == "pValue") {
        sourceValues <- qnorm(1 - sourceValue)
    } else if (sourceScale == "effectEstimate") {
        sourceValues <- sourceValue * sqrt(information1)
    } else if (.isTrialDesignInverseNormalOrGroupSequential(design)) {
        if (sourceScale == "conditionalPower") {
            sourceValues <- (criticalValue - gsWeights[2] * (qnorm(1 - sourceValue) + theta * sqrt(information2))) /
                gsWeights[1]
        } else if (sourceScale == "condPowerAtObserved") {
            sourceValues <- (criticalValue /
                gsWeights[2] -
                qnorm(1 - sourceValue)) /
                (gsWeights[1] / gsWeights[2] + sqrt(information2 / information1))
        } else if (sourceScale == "predictivePower") {
            sourceValues <- (criticalValue /
                gsWeights[2] -
                sqrt((information1 + information2) / information1) * qnorm(1 - sourceValue)) /
                (gsWeights[1] / gsWeights[2] + sqrt(information2 / information1))
        } else if (sourceScale == "reverseCondPower") {
            sourceValues <- sqrt(1 - design$informationRates[1]) *
                qnorm(sourceValue) +
                sqrt(design$informationRates[1]) * criticalValue
        }
    } else if (.isTrialDesignFisher(design)) {
        sourceValues <- .getFutilityBoundSourceValuesFisher(
            sourceValue,
            criticalValue,
            gsWeights,
            sourceScale,
            theta,
            information1,
            information2
        )
    }

    if (targetScale == "zValue") {
        return(.addFutilityBoundParameterTypes(sourceValues, args))
    } else if (targetScale == "pValue") {
        return(.addFutilityBoundParameterTypes(1 - pnorm(sourceValues), args))
    } else if (targetScale == "effectEstimate") {
        return(.addFutilityBoundParameterTypes(sourceValues / sqrt(information1), args))
    } else if (.isTrialDesignInverseNormalOrGroupSequential(design)) {
        result <- .getFutilityBoundGroupSequential(
            sourceValues,
            criticalValue,
            gsWeights,
            targetScale,
            theta,
            information1,
            information2,
            design
        )
        return(.addFutilityBoundParameterTypes(result, args))
    } else if (.isTrialDesignFisher(design)) {
        result <- numeric()
        for (x in sourceValues) {
            result <- c(result, .getFutilityBoundFisher(
                x,
                criticalValue,
                gsWeights,
                targetScale,
                theta,
                information1,
                information2
            ))
        }
        .showWarningIfCalculatedFutiltyBoundsOutsideAcceptableRange(result, upperBound = NULL)
        return(.addFutilityBoundParameterTypes(result, args))
    }

    stop(
        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
        "conversion from '", sourceScale, "' to '", targetScale, "' not implemented",
        call. = FALSE
    )
}

.getFutilityBoundSourceValueFisher <- function(
        sourceValue,
        criticalValue,
        gsWeights,
        sourceScale,
        theta,
        information1,
        information2) {
    if (is.na(sourceValue) || sourceValue >= 1 - 1e-07) {
        return(NA_real_)
    }

    tryCatch(
        {
            if (sourceScale == "conditionalPower") {
                return(qnorm(1 - criticalValue /
                    pnorm((qnorm(sourceValue) - theta * sqrt(information2)))^gsWeights[2]))
            } else if (sourceScale == "condPowerAtObserved") {
                return(stats::uniroot(
                    function(x) {
                        pmin(1, pnorm(
                            qnorm((criticalValue / (1 - pnorm(x)))^(1 / gsWeights[2])) +
                                x * sqrt(information2 / information1)
                        )) - sourceValue
                    },
                    lower = -3,
                    upper = qnorm(1 - criticalValue) - 1e-07,
                    tol = .Machine$double.eps^0.5
                )$root)
            } else if (sourceScale == "predictivePower") {
                return(stats::uniroot(
                    function(x) {
                        pmin(1, pnorm(
                            sqrt(information1 / (information1 + information2)) *
                                (qnorm((criticalValue / (1 - pnorm(x)))^(1 / gsWeights[2])) +
                                    x * sqrt(information2 / information1))
                        )) - sourceValue
                    },
                    lower = -3,
                    upper = qnorm(1 - criticalValue) - 1e-07,
                    tol = .Machine$double.eps^0.5
                )$root)
            }
        },
        warning = function(w) {
            warning("Failed to calculate ", sQuote(sourceScale), " source value from ", sourceValue, ": ", w$message, call. = FALSE)
        },
        error = function(e) {
            warning("Failed to calculate ", sQuote(sourceScale), " source value from ", sourceValue, ": ", e$message, call. = FALSE)
        }
    )
    
    return(NA_real_)
}

.getFutilityBoundSourceValuesFisher <- function(
        sourceValues,
        criticalValue,
        gsWeights,
        sourceScale,
        theta,
        information1,
        information2) {
    sourceValuesCalculated <- numeric()
    for (sourceValue in sourceValues) {
        sourceValuesCalculated <- c(sourceValuesCalculated, .getFutilityBoundSourceValueFisher(
            sourceValue,
            criticalValue,
            gsWeights,
            sourceScale,
            theta,
            information1,
            information2
        ))
    }
    return(sourceValuesCalculated)
}

.getFutilityBoundGroupSequential <- function(sourceValues,
        criticalValue,
        gsWeights,
        targetScale,
        theta,
        information1,
        information2,
        design) {
    result <- NA_real_
    if (targetScale == "conditionalPower") {
        result <- 1 - pnorm(
            (criticalValue - gsWeights[1] * sourceValues) /
                gsWeights[2] -
                theta * sqrt(information2)
        )
    } else if (targetScale == "condPowerAtObserved") {
        result <- 1 - pnorm(
            (criticalValue - gsWeights[1] * sourceValues) /
                gsWeights[2] -
                sourceValues * sqrt(information2 / information1)
        )
    } else if (targetScale == "predictivePower") {
        result <- 1 - pnorm(
            sqrt(information1 / (information1 + information2)) *
                ((criticalValue - gsWeights[1] * sourceValues) /
                    gsWeights[2] -
                    sourceValues * sqrt(information2 / information1))
        )
    } else if (targetScale == "reverseCondPower") {
        result <- pnorm((sourceValues - sqrt(design$informationRates[1]) * criticalValue) /
            sqrt(1 - design$informationRates[1]))
    }
    .showWarningIfCalculatedFutiltyBoundsOutsideAcceptableRange(result)
    return(result)
}

.getFutilityBoundFisher <- function(sourceValue,
        criticalValue,
        gsWeights,
        targetScale,
        theta,
        information1,
        information2) {
    if (is.na(sourceValue)) {
        return(NA_real_)
    }

    if (1 - pnorm(sourceValue) <= criticalValue) {
        return(1)
    }

    if (targetScale == "conditionalPower") {
        return(pnorm(
            qnorm((criticalValue / (1 - pnorm(sourceValue)))^(1 / gsWeights[2])) +
                theta * sqrt(information2)
        ))
    }

    if (targetScale == "condPowerAtObserved") {
        return(pnorm(
            qnorm((criticalValue / (1 - pnorm(sourceValue)))^(1 / gsWeights[2])) +
                sourceValue * sqrt(information2 / information1)
        ))
    }

    if (targetScale == "predictivePower") {
        return(pnorm(
            sqrt(information1 / (information1 + information2)) *
                (qnorm((criticalValue / (1 - pnorm(sourceValue)))^(1 / gsWeights[2])) +
                    sourceValue * sqrt(information2 / information1))
        ))
    }

    return(NA_real_)
}
