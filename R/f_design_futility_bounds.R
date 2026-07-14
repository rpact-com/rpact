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
        indices <- .getValidFutilityBoundVectorIndices(sourceScale, targetScale)
        if (length(information) > 1) {
            information <- .assertIsNumericVector(information, "information", naAllowed = TRUE, len = 2L)

            information1 <- information[1]
            information2 <- information[2]
            if (!any(indices == 1) && !is.na(information1)) {
                if (isTRUE(showWarnings)) {
                    warning(
                        "'information[1]' (", information1, ") will be ignored ",
                        "because it is not required for the conversion from '",
                        sourceScale, "' to '", targetScale, "'",
                        call. = FALSE
                    )
                }
                information1 <- NA_real_
            }
            if (!any(indices == 2) && !is.na(information2)) {
                if (isTRUE(showWarnings)) {
                    warning(
                        "'information[2]' (", information2, ") will be ignored ",
                        "because it is not required for the conversion from '",
                        sourceScale, "' to '", targetScale, "'",
                        call. = FALSE
                    )
                }
                information2 <- NA_real_
            }
        } else {
            information1 <- information[1]
            information2 <- information[1]
        }

        return(list(
            information1 = information1,
            information2 = information2,
            information = information,
            informationDerived = FALSE,
            vectorInput = TRUE,
            paramNames = c("information[1]", "information[2]")
        ))
    }

    vectorInput <- FALSE
    information1 <- NA_real_
    information2 <- NA_real_
    informationDerived <- FALSE
    if (length(list(...)) > 0) {
        information1 <- .getOptionalArgument("information1", optionalArgumentDefaultValue = NA_real_, ...)
        information2 <- .getOptionalArgument("information2", optionalArgumentDefaultValue = NA_real_, ...)
    }

    if (anyNA(c(information1, information2)) &&
            !is.null(design) && .isTrialDesignInverseNormalOrGroupSequentialOrFisher(design) &&
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

        information1 <- design$informationRates[1]
        information2 <- 1 - design$informationRates[1]
        informationDerived <- TRUE
        vectorInput <- TRUE
    }

    information <- c(information1, information2)
    if (all(is.na(information))) {
        information <- NA_real_
        informationDerived <- NA
        vectorInput <- TRUE
    }
    return(list(
        information1 = information1,
        information2 = information2,
        information = information,
        informationDerived = informationDerived,
        vectorInput = vectorInput,
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
#' Summarize Futility Bounds
#'
#' @description
#' S3 summary method for objects of class \code{FutilityBounds}.
#'
#' @param object An object of class \code{FutilityBounds}.
#' @param ... Additional arguments (currently not used).
#'
#' @details
#' Prints a categorized summary of futility bound parameters,
#' including user-defined, derived, default, and generated values.
#'
#' @examples
#' \dontrun{
#' futilityBounds <- getFutilityBounds(
#'     design = getDesignInverseNormal(kMax = 2),
#'     sourceValue = 0.5,
#'     sourceScale = "condPowerAtObserved",
#'     targetScale = "zValue"
#' )
#' summary(futilityBounds)
#' }
#'
#' @keywords internal
#'
#' @export
#'
summary.FutilityBounds <- function(object, ...) {
    objAttr <- attributes(object)
    objAttr$targetValue <- list(
        value = as.numeric(object),
        type = C_PARAM_GENERATED
    )

    if (!is.null(objAttr$design) && !is.null(objAttr$design$value)) {
        objAttr$design$value <- objAttr$design$value$.toString(TRUE)
    }

    userDefinedParams <- character()
    derivedDefinedParams <- character()
    defaultParams <- character()
    generatedParams <- character()

    for (paramName in names(objAttr)) {
        entry <- objAttr[[paramName]]
        if (!is.list(entry) || !all(c("value", "type") %in% names(entry))) {
            next
        }
        paramValue <- entry$value
        paramType <- entry$type
        paramSummary <- paste0(
            paramName, ": ",
            .arrayToString(paramValue, encapsulate = is.character(paramValue))
        )
        if (paramType == C_PARAM_USER_DEFINED) {
            userDefinedParams <- c(userDefinedParams, paramSummary)
        } else if (paramType == C_PARAM_DERIVED) {
            derivedDefinedParams <- c(derivedDefinedParams, paramSummary)
        } else if (paramType == C_PARAM_DEFAULT_VALUE) {
            defaultParams <- c(defaultParams, paramSummary)
        } else if (paramType == C_PARAM_GENERATED) {
            generatedParams <- c(generatedParams, paramSummary)
        }
    }

    cat("Futility bounds summary:\n\n")
    if (length(userDefinedParams) > 0) {
        cat("User-defined parameters:\n")
        for (param in userDefinedParams) {
            cat("  ", param, "\n", sep = "")
        }
        cat("\n")
    }
    if (length(derivedDefinedParams) > 0) {
        cat("Derived parameters:\n")
        for (param in derivedDefinedParams) {
            cat("  ", param, "\n", sep = "")
        }
        cat("\n")
    }
    if (length(defaultParams) > 0) {
        cat("Default parameters:\n")
        for (param in defaultParams) {
            cat("  ", param, "\n", sep = "")
        }
        cat("\n")
    }
    if (length(generatedParams) > 0) {
        cat("Output:\n")
        for (param in generatedParams) {
            cat("  ", param, "\n", sep = "")
        }
        cat("\n")
    }

    return(invisible(object))
}

.getFutilityBoundsFromThreeDots <- function(...) {
    args <- list(...)
    if (length(args) == 0) {
        return(NA_real_)
    }

    for (arg in args) {
        if (is(arg, "FutilityBounds")) {
            arg <- .assertIsNumericVector(as.numeric(arg), "futilityBounds", naAllowed = TRUE)
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

.getFutilityBoundsFromArgs <- function(..., futilityBounds, futilityBoundsScale, functionName, design, fisherDesign = FALSE) {
    futilityBoundsFromArgs <- .getFutilityBoundsFromThreeDots(...)

    futilityBoundsName <- ifelse(fisherDesign, "alpha0Vec", "futilityBounds")
    futilityBoundsScaleName <- ifelse(fisherDesign, "alpha0Scale", "futilityBoundsScale")
    targetScale <- ifelse(fisherDesign, "pValue", "zValue")

    if (!all(is.na(futilityBoundsFromArgs))) {
        if (is(futilityBoundsFromArgs, "FutilityBounds") &&
                !identical(attr(futilityBoundsFromArgs, "targetScale")$value, targetScale)) {
            stopIllegalArgument("'", futilityBoundsName, "' (", .arrayToString(futilityBoundsFromArgs), ") ", "must be on '",
                targetScale, "' scale or converted to '", targetScale, "' scale",
                functionName = ".getFutilityBoundsFromArgs",
                parameter = futilityBoundsName, value = futilityBoundsFromArgs, relatedParameter = futilityBoundsScaleName,
                relatedValue = targetScale
            )
        }

        futilityBoundsOld <- futilityBounds
        futilityBounds <- futilityBoundsFromArgs
        if (!all(is.na(futilityBoundsOld))) {
            .warnInCaseOfUnusedArgument(
                futilityBoundsOld,
                futilityBoundsName,
                defaultValue = NA_real_,
                functionName = functionName
            )
        }
    } else {
        futilityBounds <- .assertIsNumericVector(futilityBounds, futilityBoundsName, naAllowed = TRUE)
        if (futilityBoundsScale != targetScale) {
            if (!all(is.na(futilityBounds))) {
                futilityBounds <- getFutilityBounds(
                    sourceValue = futilityBounds,
                    sourceScale = futilityBoundsScale,
                    targetScale = targetScale,
                    design = design
                )
            } else {
                .warnInCaseOfUnusedArgument(
                    futilityBoundsScale,
                    futilityBoundsScaleName,
                    defaultValue = targetScale,
                    functionName = functionName
                )
            }
        }
    }
    return(futilityBounds)
}

.assertIsValidDesignForFutilityBoundsConversion <- function(design, sourceScale, targetScale) {
    if (design$sided == 1 && design$kMax == 2) {
        return(invisible())
    }

    msg <- paste0(
        "Futility bounds conversion ", sQuote(sourceScale), " -> ", sQuote(targetScale),
        " is available only for one-sided two-stage designs; invalid user input: "
    )

    invalidInputs <- character()
    if (design$sided != 1) {
        invalidInputs <- c(invalidInputs, paste0("sided = ", design$sided))
    }
    if (design$kMax != 2) {
        invalidInputs <- c(invalidInputs, paste0("kMax = ", design$kMax))
    }
    stopIllegalArgument(msg, paste(invalidInputs, collapse = ", "),
        parameter = c("sided", "kMax"), value = list(
            sided = design$sided,
            kMax = design$kMax
        ),
        constraint = "one-sided two-stage design",
        functionName = ".assertIsValidDesignForFutilityBoundsConversion"
    )
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
#' @param theta Numeric. The assumed effect size under the alternative hypothesis on the scale of the
#'   test statistic. For example, in a survival design, this would be the on the log hazard ratio scale.
#' @param information Numeric vector of length 1 or 2. The stage-wise information levels
#'   (i.e. this is *not* the cumulative information). Note that depending on which conversion is performed,
#'   either the first or second stage information may not be needed. In that case, a warning will be issued
#'   if the unused information is provided. The warning can be avoided by either passing a single number,
#'   or setting the unused information to \code{NA}.
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
#' @seealso \code{\link[=getDesignGroupSequential]{getDesignGroupSequential()}},
#'     \code{\link[=getDesignInverseNormal]{getDesignInverseNormal()}},
#'     \code{\link[=getDesignFisher]{getDesignFisher()}} for direct
#'     specification of futility bounds on different scales using the
#'     argument `futilityBoundsScale`.
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

    .warnInCaseOfUnknownArguments(
        functionName = "getFutilityBounds",
        ignore = c("information1", "information2"),
        numberOfAllowedUnnamedParameters = 1,
        exceptionEnabled = FALSE,
        ...
    )

    sourceValue <- .assertIsNumericVector(sourceValue, "sourceValue", naAllowed = naAllowed)
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
            `type` = ifelse(all(is.na(information)),
                C_PARAM_NOT_APPLICABLE,
                ifelse(isTRUE(infos$informationDerived), C_PARAM_DERIVED, C_PARAM_USER_DEFINED)
            )
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

    stopIllegalArgument("conversion from '", sourceScale, "' to '", targetScale, "' not implemented",
        functionName = "getFutilityBounds",
        parameter = "sourceScale", value = sourceScale, relatedParameter = "targetScale", relatedValue = targetScale
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

.getFutilityBoundGroupSequential <- function(
        sourceValues,
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

.getFutilityBoundFisher <- function(
        sourceValue,
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

#'
#' @title
#' Get Fisher Information From a Design Plan
#'
#' @description
#' Calculates the Fisher information at the first planned analysis stage for
#' a design plan for means, rates, or survival endpoints.
#'
#' @param designPlan A trial design plan object as returned by functions such as
#' \code{\link[=getSampleSizeMeans]{getSampleSizeMeans()}},
#' \code{\link[=getPowerMeans]{getPowerMeans()}},
#' \code{\link[=getSampleSizeRates]{getSampleSizeRates()}},
#' \code{\link[=getPowerRates]{getPowerRates()}},
#' \code{\link[=getSampleSizeSurvival]{getSampleSizeSurvival()}}, or
#' \code{\link[=getPowerSurvival]{getPowerSurvival()}}.
#'
#' @details
#' The returned information is the information used at the first stage of the
#' design plan. For group sequential designs, information at later stages can be
#' obtained by multiplying this value by the ratio of the corresponding
#' information rate to the first information rate.
#'
#' For means, the information is based on the planned sample size, standard
#' deviations, allocation ratio, and, if applicable, the mean-ratio null value.
#' For rates, it is based on the planned sample size and the binomial variance
#' under the corresponding planning assumptions. For survival endpoints, it is
#' based on the planned number of events and the allocation ratio.
#'
#' @return
#' A numeric value or numeric vector containing the first-stage Fisher
#' information. A vector is returned if the design plan contains several
#' planning alternatives or sample size values. \code{NA_real_} is returned if
#' the endpoint type is not supported by this helper.
#'
#' @examples
#' \dontrun{
#' designPlan <- getSampleSizeMeans(alternative = 0.4)
#' getFisherInformation(designPlan)
#'
#' design <- getDesignGroupSequential(kMax = 3)
#' designPlan <- getPowerMeans(design,
#'     alternative = c(0.3, 0.4), maxNumberOfSubjects = 100
#' )
#' getFisherInformation(designPlan)
#' }
#'
#' @seealso \code{\link[=getFutilityBounds]{getFutilityBounds()}}
#'
#' @export
getFisherInformation <- function(designPlan) {
    fisherInformation <- NA_real_
    if (.isTrialDesignPlanMeans(designPlan)) {
        if (designPlan$.isSampleSizeObject()) {
            numberOfSubjects <- designPlan$numberOfSubjects[1, ]
        } else {
            numberOfSubjects <- designPlan$maxNumberOfSubjects * designPlan$.design$informationRates[1]
        }
        stDev <- designPlan$stDev
        if (length(stDev) == 1 && designPlan$groups == 2) {
            stDev <- rep(stDev, 2)
        }
        if (designPlan$groups == 1) {
            fisherInformation <- numberOfSubjects / stDev[1]^2
        } else {
            allocationRatio <- designPlan$allocationRatioPlanned[1]
            if (designPlan$meanRatio) {
                fisherInformation <- allocationRatio * numberOfSubjects /
                    ((1 + allocationRatio) *
                        (stDev[1]^2 + designPlan$thetaH0^2 * allocationRatio * stDev[2]^2))
            } else {
                fisherInformation <- allocationRatio * numberOfSubjects /
                    ((1 + allocationRatio) *
                        (stDev[1]^2 + allocationRatio * stDev[2]^2))
            }
        }
    } else if (.isTrialDesignPlanRates(designPlan)) {
        if (designPlan$.isSampleSizeObject()) {
            n1 <- designPlan$numberOfSubjects[1, ]
        } else {
            n1 <- designPlan$maxNumberOfSubjects * designPlan$.design$informationRates[1]
        }
        if (designPlan$groups == 1) {
            pi0 <- designPlan$thetaH0
            fisherInformation <- n1 / (pi0 * (1 - pi0))
        } else if (designPlan$groups == 2) {
            pi1 <- designPlan$pi1
            pi2 <- designPlan$pi2
            allocationRatio <- designPlan$allocationRatioPlanned[1]
            fisherInformation <- allocationRatio / (pi1 * (1 - pi1) + allocationRatio * pi2 * (1 - pi2)) * n1 / (1 + allocationRatio)
        }
    } else if (.isTrialDesignPlanSurvival(designPlan)) {
        if (!is.null(designPlan$cumulativeEventsPerStage) && !all(is.na(designPlan$cumulativeEventsPerStage))) {
            cumulativeEvents <- designPlan$cumulativeEventsPerStage[1, ]
        } else if (designPlan$.isSampleSizeObject()) {
            cumulativeEvents <- designPlan$eventsFixed * designPlan$.design$informationRates[1]
        } else {
            cumulativeEvents <- designPlan$maxNumberOfEvents * designPlan$.design$informationRates[1]
        }
        allocationRatio <- designPlan$allocationRatioPlanned[1]
        fisherInformation <- allocationRatio / (1 + allocationRatio)^2 * cumulativeEvents
    }
    return(fisherInformation)
}

.getFutilityBoundsTreatmentEffectScaleRatesTwoGroups <- function(designPlan, boundary, nStages) {
    design <- designPlan$.design
    maxNumberOfSubjects <- designPlan$maxNumberOfSubjects
    allocationRatioPlanned <- designPlan$allocationRatioPlanned
    nParameters <- length(maxNumberOfSubjects)

    if (length(allocationRatioPlanned) == 1) {
        allocationRatioPlanned <- rep(allocationRatioPlanned, nParameters)
    }

    result <- matrix(NA_real_, nrow = nStages, ncol = nParameters)
    if (nStages == 0 || !.hasApplicableFutilityBounds(design)) {
        return(result)
    }

    futilityBounds <- .getFutilityBounds(design)
    if (length(futilityBounds) == 0) {
        return(result)
    }
    futilityBounds[.getInvalidFutilityBoundsIndices(design)] <- NA_real_
    futilityBounds <- futilityBounds[seq_len(nStages)]

    directionUpper <- .getDirectionUpper(designPlan, nParameters)
    method <- ifelse(designPlan$riskRatio, "ratio", "diff")

    for (index in seq_len(nParameters)) {
        n1 <- allocationRatioPlanned[index] *
            design$informationRates[seq_len(nStages)] *
            maxNumberOfSubjects[index] /
            (1 + allocationRatioPlanned[index])
        n2 <- n1 / allocationRatioPlanned[index]

        for (stage in seq_len(nStages)) {
            if (is.na(futilityBounds[stage])) {
                next
            }

            futilityBound <- futilityBounds[stage]
            directionUpperBound <- directionUpper[index]
            if (identical(boundary, "upper")) {
                directionUpperBound <- TRUE
            } else if (identical(boundary, "lower")) {
                futilityBound <- -futilityBound
                directionUpperBound <- TRUE
            }

            pi1Bound <- .getEffectScaleBoundaryDataRatesPi(
                futilityBound,
                designPlan$pi2,
                designPlan$thetaH0,
                n1[stage],
                n2[stage],
                allocationRatioPlanned[index],
                directionUpperBound,
                method
            )
            if (designPlan$riskRatio) {
                result[stage, index] <- pi1Bound / designPlan$pi2
            } else {
                result[stage, index] <- pi1Bound - designPlan$pi2
            }
        }
    }

    if (designPlan$riskRatio) {
        result[!is.na(result) & result <= 0] <- NA_real_
    }

    return(result)
}

.getFutilityBoundsTreatmentEffectScale <- function(designPlan, boundary = c("directed", "upper", "lower")) {
    .assertIsTrialDesignPlan(designPlan)
    boundary <- match.arg(boundary)

    design <- designPlan$.design
    nStages <- max(design$kMax - 1, 0)
    if (.isTrialDesignPlanRates(designPlan) && designPlan$groups == 2) {
        return(.getFutilityBoundsTreatmentEffectScaleRatesTwoGroups(designPlan, boundary, nStages))
    }

    fisherInformation <- getFisherInformation(designPlan)
    nParameters <- max(length(fisherInformation), 1)

    if (nStages == 0) {
        return(matrix(numeric(0), nrow = 0, ncol = nParameters))
    }

    result <- matrix(NA_real_, nrow = nStages, ncol = nParameters)
    if (!.hasApplicableFutilityBounds(design) || all(is.na(fisherInformation))) {
        return(result)
    }

    futilityBounds <- .getFutilityBounds(design)
    if (length(futilityBounds) == 0) {
        return(result)
    }
    futilityBounds[.getInvalidFutilityBoundsIndices(design)] <- NA_real_
    futilityBounds <- futilityBounds[seq_len(nStages)]

    informationRates <- design$informationRates[seq_len(nStages)]
    stageInformation <- (informationRates / design$informationRates[1]) %*% t(fisherInformation)
    standardizedFutilityBounds <- matrix(futilityBounds, nrow = nStages, ncol = nParameters)
    if (.isTrialDesignPlanMeans(designPlan) && !designPlan$normalApproximation) {
        degreesOfFreedom <- pmax(
            informationRates %*% t(designPlan$maxNumberOfSubjects) - designPlan$groups,
            1e-04
        )
        standardizedFutilityBounds <- matrix(
            stats::qt(stats::pnorm(futilityBounds), degreesOfFreedom),
            nrow = nStages,
            ncol = nParameters
        )
        standardizedFutilityBounds[abs(standardizedFutilityBounds) > 50] <- NA_real_
    }
    if (identical(boundary, "directed")) {
        directionUpper <- .getDirectionUpper(designPlan, nParameters)
        directionSign <- ifelse(directionUpper, 1, -1)
    } else {
        directionSign <- rep(ifelse(identical(boundary, "upper"), 1, -1), nParameters)
    }

    for (stage in seq_len(nStages)) {
        for (index in seq_len(nParameters)) {
            if (is.na(standardizedFutilityBounds[stage, index]) || is.na(stageInformation[stage, index])) {
                next
            }
            effectEstimate <- as.numeric(getFutilityBounds(
                sourceValue = standardizedFutilityBounds[stage, index],
                sourceScale = "zValue",
                targetScale = "effectEstimate",
                information1 = stageInformation[stage, index],
                naAllowed = TRUE
            ))
            if (.isTrialDesignPlanSurvival(designPlan)) {
                result[stage, index] <- designPlan$thetaH0 * exp(directionSign[index] * effectEstimate)
            } else {
                result[stage, index] <- designPlan$thetaH0 + directionSign[index] * effectEstimate
            }
        }
    }

    if (.isTrialDesignPlanRates(designPlan) && designPlan$groups == 1) {
        result[!is.na(result) & (result < 0 | result > 1)] <- NA_real_
    }
    if ((.isTrialDesignPlanMeans(designPlan) && designPlan$meanRatio) ||
            (.isTrialDesignPlanRates(designPlan) && designPlan$riskRatio)) {
        result[!is.na(result) & result <= 0] <- NA_real_
    }

    return(result)
}
