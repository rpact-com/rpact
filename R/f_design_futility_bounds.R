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

.getFutilityBoundInformations <- function(..., information, sourceScale, targetScale, design, showWarnings = TRUE) {
    args <- list(...)
    separateInformationArguments <- length(args) > 0 && 
        !is.null(names(args)) && 
        any(c("information1", "information2") %in% names(args))
    
    if (!separateInformationArguments && !is.null(information) && 
            !all(is.na(information)) && length(information) > 0) {
        len <- .getFutilityBoundVectorLength(sourceScale, targetScale)
        .assertIsNumericVector(information, "information", naAllowed = TRUE, len = len)
        return(list(
            information1 = information[1],
            information2 = information[2],
            information = information,
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
    
    
    if (any(is.na(c(information1, information2))) && 
            !is.null(design) && .isTrialDesignInverseNormalOrGroupSequential(design) && 
            (sourceScale %in% c("predictivePower", "condPowerAtObserved") ||
            targetScale %in% c("predictivePower", "condPowerAtObserved"))) {
        
        .assertIsValidDesignForFutilityBoundsConversion(design, sourceScale, targetScale)
        
        if (isTRUE(showWarnings) && !is.na(information1)) {
            warning(
                "'information1' (", information1, ") will be ignored ", 
                "because it will only be taken into account if the information is provided for both stages",
                call. = FALSE
            )
        }
        if (isTRUE(showWarnings) && !is.na(information2)) {
            warning(
                "'information2' (", information2, ") will be ignored ", 
                "because it will only be taken into account if the information is provided for both stages",
                call. = FALSE
            )
        }
        
        information1 <- sqrt(design$informationRates[1])
        information2 <- sqrt(1 - design$informationRates[1]) 
    }
    
    information <- c(information1, information2)
    if (all(is.na(information))) {
        information <- NA_real_
    }
    return(list(
        information1 = information1,
        information2 = information2,
        information = information,
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

.getFutilityBoundsFromThreeDots <- function(...) {
    args <- list(...)
    if (length(args) == 0) {
        return(NA_real_)
    }
    
    for (arg in args) {
        if (is(arg, "FutilityBounds")) {
            .assertIsNumericVector(as.numeric(arg), "futilityBounds", naAllowed = TRUE)
            return(arg)
        }
    }
    
    return(NA_real_)
}

.getDesignFromThreeDots <- function(design, ...) {
    if (!is.null(design)) {
        return(design)
    }
    
    args <- list(...)
    for (arg in args) {
        if (.isTrialDesign(arg)) {
            return(arg)
        }
    }
    
    return(NULL)
}

.getFutilityBoundsFromArgs <- function(..., futilityBounds, futilityBoundsScale, functionName, design) {
    futilityBoundsFromArgs <- .getFutilityBoundsFromThreeDots(...)
    if (!all(is.na(futilityBoundsFromArgs))) {
        
        if (is(futilityBoundsFromArgs, "FutilityBounds") && 
                !identical(attr(futilityBoundsFromArgs, "targetScale")$value, "zValue")) {
            stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
                "'futilityBounds' (", .arrayToString(futilityBoundsFromArgs), ") ",
                "must be on 'zValue' scale or converted to 'zValue' scale",
                call. = FALSE)
        }
        
        futilityBoundsOld <- futilityBounds
        futilityBounds <- futilityBoundsFromArgs
        if (!all(is.na(futilityBoundsOld))) {
            .warnInCaseOfUnusedArgument(
                futilityBoundsOld, 
                "futilityBounds", 
                defaultValue = NA_real_,
                functionName = functionName)
        }
        
    } else {
        .assertIsNumericVector(futilityBounds, "futilityBounds", naAllowed = TRUE)
        if (futilityBoundsScale != "zValue") {
            if (!all(is.na(futilityBounds))) {
                futilityBounds <- getFutilityBounds(
                    sourceValue = futilityBounds,
                    sourceScale = futilityBoundsScale,
                    targetScale = "zValue",
                    design = design
                )
            } else {
                .warnInCaseOfUnusedArgument(
                    futilityBoundsScale, 
                    "futilityBoundsScale", 
                    defaultValue = "zValue",
                    functionName = functionName)
            }
        }
    }
    return(futilityBounds)
}

.assertIsValidDesignForFutilityBoundsConversion <- function(design, sourceScale, targetScale) {
    if (design$sided == 1 && design$kMax == 2) {
        return(invisible())
    }
    
    msg <- paste0(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
        "Futility bounds conversion ", sQuote(sourceScale), " -> ", sQuote(targetScale), 
        " is available only for one-sided two-stage designs ")
    if (design$sided != 1) {
        stop(msg, "(sided = ", design$sided, ")", call. = FALSE)
    }
    
    if (design$kMax != 2) {
        stop(msg, "(kMax = ", design$kMax, ")", call. = FALSE)
    }
}

.futilityBoundsCalculationRequiresDesign <- function(scale) {
    if (is.null(scale) || length(scale) != 1 || is.na(scale)) {
        return(FALSE)
    }
    
    return(scale %in% c(
        "conditionalPower",
        "condPowerAtObserved",
        "predictivePower",
        "reverseCondPower"
    ))
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
#' @param naAllowed Logical. Indicates if \code{NA} \code{sourceValue} are permitted. Default is \code{FALSE}.
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
    
    design <- .getDesignFromThreeDots(design, ...)

    infos <- .getFutilityBoundInformations(
        information = information,
        sourceScale = sourceScale,
        targetScale = targetScale,
        design = design,
        ...
    )
    information1 <- infos$information1
    information2 <- infos$information2
    information <- infos$information
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
        ),
        `design` = list(
            `value` = design,
            `type` = ifelse(is.null(design), C_PARAM_NOT_APPLICABLE, C_PARAM_USER_DEFINED)
        )
    )

    if (sourceScale == targetScale) {
        return(.addFutilityBoundParameterTypes(sourceValue, args))
    }

    if (!is.null(design)) {
        if (sourceScale == "reverseCondPower" || targetScale == "reverseCondPower") {
            .assertIsTrialDesignInverseNormalOrGroupSequential(design)
        } else {
            .assertIsTrialDesignInverseNormalOrGroupSequentialOrFisher(design)            
        }
        .assertIsValidDesignForFutilityBoundsConversion(design, sourceScale, targetScale)
        if (.isTrialDesignInverseNormalOrGroupSequential(design)) {
            gsWeights <- c(sqrt(design$informationRates[1]), sqrt(1 - design$informationRates[1]))
        } else if (.isTrialDesignFisher(design)) {
            gsWeights <- c(1, sqrt((1 - design$informationRates[1]) / design$informationRates[1]))
        }
        criticalValue <- design$criticalValues[2]
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
            "conditionalPower", 
            "condPowerAtObserved",
            "predictivePower", 
            "reverseCondPower", 
            "pValue"
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
            } else {
                warning(
                    "Source scale '", sourceScale, "' not implemented for Fisher's combination test design",
                    call. = FALSE
                )
                return(NA_real_)
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
