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
#'
#' @param design
#'
#' @param theta
#'
#' @param information1
#'
#' @param information2
#'
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
#' 
#' @export
#' 
getFutilityBounds <- function(design = NULL,
        sourceValue,
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
    .assertAreValidFutilityBoundsScaleArguments(
        design,
        sourceScale,
        targetScale,
        theta,
        information1,
        information2
    )

    sourceScale <- match.arg(sourceScale)
    targetScale <- match.arg(targetScale)
    if (sourceScale == targetScale) {
        return(sourceValue)
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

    .assertIsSingleNumber(information1, "information1", naAllowed = TRUE)
    .assertIsInOpenInterval(
        information1,
        "information1",
        lower = 0,
        upper = Inf,
        naAllowed = TRUE
    )

    .assertIsSingleNumber(information2, "information2", naAllowed = TRUE)
    .assertIsInOpenInterval(
        information2,
        "information2",
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

    sourceValues <- c()
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
        sourceValues <- .getFutilityBoundSourceValueFisher(
            sourceValue,
            criticalValue,
            gsWeights,
            sourceScale,
            theta,
            information1,
            information2)
    }

    if (targetScale == "zValue") {
        return(sourceValues)
    } else if (targetScale == "pValue") {
        return(1 - pnorm(sourceValues))
    } else if (targetScale == "effectEstimate") {
        return(sourceValues / sqrt(information1))
    } else if (.isTrialDesignInverseNormalOrGroupSequential(design)) {
        return(.getFutilityBoundGroupSequential(
            sourceValues,
            criticalValue,
            gsWeights,
            targetScale,
            theta,
            information1,
            information2,
            design
        ))
    } else if (.isTrialDesignFisher(design)) {
        result <- c()
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
        return(result)
    }

    return(NA_real_)
}

.getFutilityBoundSourceValueFisher <- function(sourceValue,
        criticalValue,
        gsWeights,
        sourceScale,
        theta,
        information1,
        information2) {
    sourceValues <- c()
    if (sourceScale == "conditionalPower") {
        for (y in sourceValue) {
            result <- NA_real_
            if (!is.na(y) && y < 1 - 1e-07) {
                result <- qnorm(1 - criticalValue /
                    pnorm((qnorm(y) - theta * sqrt(information2)))^gsWeights[2])
            }
            sourceValues <- c(sourceValues, result)
        }
    } else if (sourceScale == "condPowerAtObserved") {
        for (y in sourceValue) {
            result <- NA_real_
            if (!is.na(y) && y < 1 - 1e-07) {
                tryCatch(
                    {
                        result <- stats::uniroot(
                            function(x) {
                                pmin(1, pnorm(
                                    qnorm((criticalValue / (1 - pnorm(x)))^(1 / gsWeights[2])) +
                                        x * sqrt(information2 / information1)
                                )) - y
                            },
                            lower = -3,
                            upper = qnorm(1 - criticalValue) - 1e-07,
                            tol = .Machine$double.eps^0.5
                        )$root
                    },
                    error = function(e) {
                        warning("Failed to calculate 'sourceValue': ", e$message)
                    }
                )
            }
            sourceValues <- c(sourceValues, result)
        }
    } else if (sourceScale == "predictivePower") {
        for (y in sourceValue) {
            result <- NA_real_
            if (!is.na(y) && y < 1 - 1e-07) {
                tryCatch(
                    {
                        result <- stats::uniroot(
                            function(x) {
                                pmin(1, pnorm(
                                    sqrt(information1 / (information1 + information2)) *
                                        (qnorm((criticalValue / (1 - pnorm(x)))^(1 / gsWeights[2])) +
                                            x * sqrt(information2 / information1))
                                )) - y
                            },
                            lower = -3,
                            upper = qnorm(1 - criticalValue) - 1e-07,
                            tol = .Machine$double.eps^0.5
                        )$root
                    },
                    error = function(e) {
                        warning("Failed to calculate 'sourceValue': ", e$message)
                    }
                )
            }
            sourceValues <- c(sourceValues, result)
        }
    }
    return(sourceValues)
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
