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

.getRequiredFutilityBoundsInformationType <- function(sourceScale, targetScale) {
    scales <- c(sourceScale, targetScale)
    if (any(scales %in% c("conditionalPower", "condPowerAtObserved", "predictivePower"))) {
        return("stageWise")
    }
    if (any(scales == "effectEstimate")) {
        return("cumulative")
    }
    return(NULL)
}

.assertIsValidFutilityBoundsInformationType <- function(information, sourceScale, targetScale) {
    informationType <- attr(information, "type", exact = TRUE)
    if (is.null(informationType)) {
        return(invisible())
    }

    validTypes <- c("cumulative", "stageWise")
    if (!is.character(informationType) || length(informationType) != 1L ||
            is.na(informationType) || !informationType %in% validTypes) {
        stopIllegalArgument(
            "attribute ", .sQuote("type"), " of ", .pQuote("information"), " (",
            .arrayToString(informationType), ") must be ", .arrayToString(validTypes, mode = "or"),
            functionName = ".assertIsValidFutilityBoundsInformationType",
            parameter = "information",
            value = information
        )
    }

    requiredType <- .getRequiredFutilityBoundsInformationType(sourceScale, targetScale)
    if (!is.null(requiredType) && informationType != requiredType) {
        stopIllegalArgument(
            .pQuote("information"), " has type ", .vQuote(informationType),
            ", but conversion from ", .vQuote(sourceScale), " to ", .vQuote(targetScale),
            " requires information of type ", .vQuote(requiredType),
            functionName = ".assertIsValidFutilityBoundsInformationType",
            parameter = "information",
            value = information,
            relatedParameter = "type",
            relatedValue = informationType
        )
    }

    return(invisible())
}

.getFutilityBoundInformations <- function(
        ...,
        information,
        sourceScale,
        targetScale,
        design,
        showWarnings = TRUE) {
    .assertIsValidFutilityBoundsInformationType(information, sourceScale, targetScale)

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
                        "because it is not required for the conversion from ",
                        .vQuote(sourceScale), " to ", .vQuote(targetScale),
                        call. = FALSE
                    )
                }
                information1 <- NA_real_
            }
            if (!any(indices == 2) && !is.na(information2)) {
                if (isTRUE(showWarnings)) {
                    warning(
                        "'information[2]' (", information2, ") will be ignored ",
                        "because it is not required for the conversion from ",
                        .vQuote(sourceScale), " to ", .vQuote(targetScale),
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
#' S3 print method for objects of class \code{FutilityBounds}. Prints the
#' futility bounds together with their target scale and, if available, the
#' analysis stages, planning situations, and Fisher information type used for
#' their calculation.
#'
#' @param x An object of class \code{FutilityBounds}.
#' @param ... Additional arguments passed to the underlying print method.
#'
#' @keywords internal
#'
#' @export
#'
print.FutilityBounds <- function(x, ...) {
    stage <- attr(x, "stage", exact = TRUE)
    situations <- attr(x, "situations", exact = TRUE)
    informationType <- attr(x, "informationType", exact = TRUE)
    targetScale <- attr(x, "targetScale", exact = TRUE)
    if (is.list(targetScale)) {
        targetScale <- targetScale$value
    }
    values <- .getFutilityBoundsValuesForPrinting(x)

    scaleDescription <- "target"
    if (!is.null(targetScale) && length(targetScale) == 1L && !is.na(targetScale)) {
        scaleDescription <- .formatCamelCaseSingleWord(targetScale, sep = "-")
    }
    heading <- paste0("Futility bounds on the ", scaleDescription, " scale")
    if (!is.null(informationType) && length(informationType) == 1L && !is.na(informationType)) {
        informationDescription <- .formatCamelCaseSingleWord(informationType, sep = "-")
        heading <- paste0(heading, " using ", informationDescription, " Fisher information")
    }

    if (!is.null(stage) && length(stage) == 1L) {
        heading <- paste0(heading, " at stage ", stage)
        if (!is.null(situations) && length(situations) == 1L) {
            heading <- paste0(heading, " (", situations, ")")
        } else if (!is.null(situations) && length(situations) > 1L) {
            heading <- paste0(heading, " by situation")
        }
    } else if (!is.null(stage) && length(stage) > 1L) {
        heading <- paste0(heading, " by stage")
        if (!is.null(situations) && length(situations) == 1L) {
            heading <- paste0(heading, " (", situations, ")")
        } else if (!is.null(situations) && length(situations) > 1L) {
            heading <- paste0(heading, " and situation")
        }
    }
    cat(heading, ":\n", sep = "")

    if (!is.null(stage) && length(stage) == 1L &&
            is.null(dim(values)) && !is.null(situations) &&
            length(situations) == length(values) && length(values) > 1L) {
        output <- data.frame(
            Situation = situations,
            `Futility bound` = as.numeric(values),
            check.names = FALSE
        )
        print(output, row.names = FALSE, ...)
    } else if (!is.null(stage) && length(stage) > 1L && is.null(dim(values))) {
        output <- data.frame(
            Stage = stage,
            `Futility bound` = as.numeric(values),
            check.names = FALSE
        )
        print(output, row.names = FALSE, ...)
    } else {
        if (!is.null(dim(values))) {
            if (!is.null(stage) && length(stage) == nrow(values)) {
                rownames(values) <- paste0("Stage ", stage)
            }
            if (!is.null(situations) && length(situations) == ncol(values)) {
                colnames(values) <- situations
            }
        }
        print.default(values, ...)
    }

    return(invisible(x))
}

.getFutilityBoundsValuesForPrinting <- function(x) {
    parameterAttributes <- c(
        "sourceValue", "sourceScale", "targetScale", "theta", "information", "design",
        "stage", "situations", "designPlan", "informationType"
    )
    for (attributeName in parameterAttributes) {
        attr(x, attributeName) <- NULL
    }
    class(x) <- setdiff(class(x), "FutilityBounds")
    return(x)
}

.getFisherInformationValuesForPrinting <- function(x) {
    attr(x, "type") <- NULL
    attr(x, "stage") <- NULL
    attr(x, "situations") <- NULL
    attr(x, "designPlan") <- NULL
    class(x) <- setdiff(class(x), "FisherInformation")
    return(x)
}

#'
#' @title
#' Print Fisher Information
#'
#' @description
#' S3 print method for objects of class \code{FisherInformation}.
#' Prints the Fisher information together with its type, analysis stage, and,
#' if available, the situations to which the values apply.
#'
#' @param x An object of class \code{FisherInformation}.
#' @param ... Additional arguments passed to the underlying print method.
#'
#' @keywords internal
#'
#' @export
#'
print.FisherInformation <- function(x, ...) {
    type <- attr(x, "type", exact = TRUE)
    stage <- attr(x, "stage", exact = TRUE)
    situations <- attr(x, "situations", exact = TRUE)
    values <- .getFisherInformationValuesForPrinting(x)

    typeDescription <- "Fisher information"
    if (!is.null(type) && length(type) == 1L && !is.na(type)) {
        typeDescription <- paste(
            .firstCharacterToUpperCase(.formatCamelCaseSingleWord(type, sep = "-")),
            typeDescription
        )
    }

    if (!is.null(stage) && length(stage) == 1L) {
        heading <- paste0(typeDescription, " at stage ", stage)
        if (!is.null(situations) && length(situations) == 1L) {
            heading <- paste0(heading, " (", situations, ")")
        }
        if (!is.null(situations) && length(situations) > 1L) {
            heading <- paste0(heading, " by situation")
        }
    } else if (!is.null(stage) && length(stage) > 1L) {
        heading <- paste0(typeDescription, " by stage")
        if (!is.null(situations) && length(situations) == 1L) {
            heading <- paste0(heading, " (", situations, ")")
        }
        if (!is.null(situations) && length(situations) > 1L) {
            heading <- paste0(heading, " and situation")
        }
    } else {
        heading <- typeDescription
    }
    cat(heading, ":\n", sep = "")

    if (!is.null(stage) && length(stage) == 1L &&
            is.null(dim(values)) && !is.null(situations) &&
            length(situations) == length(values) && length(values) > 1L) {
        output <- data.frame(
            Situation = situations,
            information = as.numeric(values),
            check.names = FALSE
        )
        names(output)[2] <- "Fisher information"
        print(output, row.names = FALSE, ...)
    } else if (!is.null(stage) && length(stage) > 1L && is.null(dim(values))) {
        output <- data.frame(
            Stage = stage,
            information = as.numeric(values),
            check.names = FALSE
        )
        names(output)[2] <- "Fisher information"
        print(output, row.names = FALSE, ...)
    } else {
        if (!is.null(dim(values))) {
            if (!is.null(stage) && length(stage) == nrow(values)) {
                rownames(values) <- paste0("Stage ", stage)
            }
            if (!is.null(situations) && length(situations) == ncol(values)) {
                colnames(values) <- situations
            }
        }
        print.default(values, ...)
    }

    return(invisible(x))
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

.getFutilityBoundsFromArgs <- function(
        ...,
        futilityBounds,
        futilityBoundsScale,
        functionName,
        design,
        directionUpper = NA,
        fisherDesign = FALSE) {
    futilityBoundsFromArgs <- .getFutilityBoundsFromThreeDots(...)

    futilityBoundsName <- ifelse(fisherDesign, "alpha0Vec", "futilityBounds")
    futilityBoundsScaleName <- ifelse(fisherDesign, "alpha0Scale", "futilityBoundsScale")
    targetScale <- ifelse(fisherDesign, "pValue", "zValue")

    if (!all(is.na(futilityBoundsFromArgs))) {
        if (is(futilityBoundsFromArgs, "FutilityBounds") &&
                !identical(attr(futilityBoundsFromArgs, "targetScale")$value, targetScale)) {
            stopIllegalArgument(.pQuote(futilityBoundsName), " (", .arrayToString(futilityBoundsFromArgs), ") ",
                "must be on ", .vQuote(targetScale), " scale or converted to ", .vQuote(targetScale), " scale",
                functionName = ".getFutilityBoundsFromArgs",
                parameter = futilityBoundsName, value = futilityBoundsFromArgs,
                relatedParameter = futilityBoundsScaleName,
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
                    design = design,
                    directionUpper = directionUpper
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

.getFisherInformationAsMatrix <- function(fisherInformation) {
    stage <- attr(fisherInformation, "stage", exact = TRUE)
    situations <- attr(fisherInformation, "situations", exact = TRUE)
    designPlan <- attr(fisherInformation, "designPlan", exact = TRUE)
    informationType <- attr(fisherInformation, "type", exact = TRUE)
    values <- .getFisherInformationValuesForPrinting(fisherInformation)

    if (is.null(stage) || length(stage) == 0L) {
        stopIllegalArgument(
            .pQuote("information"), " does not contain the required ", sQuote("stage"), " attribute",
            functionName = ".getFisherInformationAsMatrix",
            parameter = "information",
            value = fisherInformation
        )
    }

    if (!is.null(dim(values))) {
        if (nrow(values) != length(stage)) {
            stopIllegalArgument(
                "the number of rows in ", .pQuote("information"), " must equal the number of stages",
                functionName = ".getFisherInformationAsMatrix",
                parameter = "information",
                value = fisherInformation,
                relatedParameter = "stage",
                relatedValue = stage
            )
        }
        informationMatrix <- matrix(
            as.numeric(values),
            nrow = nrow(values),
            ncol = ncol(values),
            dimnames = dimnames(values)
        )
    } else if (length(stage) == 1L) {
        informationMatrix <- matrix(as.numeric(values), nrow = 1L)
    } else {
        if (length(values) != length(stage)) {
            stopIllegalArgument(
                "the length of ", .pQuote("information"), " must equal the number of stages",
                functionName = ".getFisherInformationAsMatrix",
                parameter = "information",
                value = fisherInformation,
                relatedParameter = "stage",
                relatedValue = stage
            )
        }
        informationMatrix <- matrix(as.numeric(values), ncol = 1L)
    }

    nSituations <- ncol(informationMatrix)
    if (is.null(situations) || length(situations) != nSituations) {
        situations <- if (nSituations == 1L) NULL else paste0("situation ", seq_len(nSituations))
    }

    return(list(
        values = informationMatrix,
        stage = stage,
        situations = situations,
        type = informationType,
        designPlan = designPlan
    ))
}

.getFutilityBoundsFromFisherInformation <- function(
        fisherInformation,
        targetScale,
        directionUpper,
        theta,
        naAllowed) {
    informationData <- .getFisherInformationAsMatrix(fisherInformation)
    designPlan <- informationData$designPlan
    if (is.null(designPlan)) {
        stopIllegalArgument(
            .pQuote("information"), " does not contain the design plan required for pipe-based conversion",
            functionName = ".getFutilityBoundsFromFisherInformation",
            parameter = "information",
            value = fisherInformation
        )
    }

    design <- designPlan$.design
    .assertIsValidFutilityBoundsInformationType(
        fisherInformation,
        sourceScale = "zValue",
        targetScale = targetScale
    )

    interimStages <- informationData$stage[informationData$stage < design$kMax]
    if (length(interimStages) == 0L) {
        stopIllegalArgument(
            .pQuote("information"), " must contain at least one interim analysis stage",
            functionName = ".getFutilityBoundsFromFisherInformation",
            parameter = "information",
            value = fisherInformation,
            relatedParameter = "stage",
            relatedValue = informationData$stage
        )
    }

    stageIndices <- match(interimStages, informationData$stage)
    sourceValues <- .getFutilityBounds(design, interimStages)
    nSituations <- ncol(informationData$values)
    result <- matrix(
        NA_real_,
        nrow = length(interimStages),
        ncol = nSituations
    )

    directionUpperCalculated <- directionUpper
    if (is.na(directionUpperCalculated)) {
        directionUpperCalculated <- if (is.na(design$directionUpper)) TRUE else design$directionUpper
    }

    powerScales <- c("conditionalPower", "condPowerAtObserved", "predictivePower")
    if (targetScale %in% powerScales) {
        requiredStages <- seq_len(design$kMax)
        requiredStageIndices <- match(requiredStages, informationData$stage)
        if (anyNA(requiredStageIndices)) {
            stopIllegalArgument(
                .pQuote("information"), " must contain all design stages for conversion to ",
                .vQuote(targetScale),
                functionName = ".getFutilityBoundsFromFisherInformation",
                parameter = "information",
                value = fisherInformation,
                relatedParameter = "stage",
                relatedValue = informationData$stage
            )
        }
        for (situationIndex in seq_len(nSituations)) {
            information <- informationData$values[requiredStageIndices, situationIndex]
            if (targetScale == "conditionalPower") {
                information[1] <- NA_real_
            }
            attr(information, "type") <- informationData$type
            result[, situationIndex] <- as.numeric(getFutilityBounds(
                sourceValue = sourceValues,
                sourceScale = "zValue",
                targetScale = targetScale,
                design = design,
                directionUpper = directionUpperCalculated,
                theta = theta,
                information = information,
                naAllowed = naAllowed
            ))
        }
    } else {
        for (stageIndex in seq_along(interimStages)) {
            for (situationIndex in seq_len(nSituations)) {
                information <- NA_real_
                if (targetScale == "effectEstimate") {
                    information <- informationData$values[stageIndices[stageIndex], situationIndex]
                    attr(information, "type") <- informationData$type
                }
                calculationDesign <- if (targetScale == "reverseCondPower") design else NULL
                result[stageIndex, situationIndex] <- as.numeric(getFutilityBounds(
                    sourceValue = sourceValues[stageIndex],
                    sourceScale = "zValue",
                    targetScale = targetScale,
                    design = calculationDesign,
                    directionUpper = directionUpperCalculated,
                    information = information,
                    naAllowed = naAllowed
                ))
            }
        }
    }

    dimnames(result) <- list(
        paste0("stage ", interimStages),
        informationData$situations
    )

    if (nrow(result) == 1L) {
        result <- as.numeric(result[1, ])
    } else if (ncol(result) == 1L) {
        result <- as.numeric(result[, 1])
    }

    args <- list(
        sourceValue = list(value = sourceValues, type = C_PARAM_DERIVED),
        sourceScale = list(value = "zValue", type = C_PARAM_DERIVED),
        targetScale = list(value = targetScale, type = C_PARAM_USER_DEFINED),
        theta = list(
            value = theta,
            type = ifelse(is.na(theta), C_PARAM_NOT_APPLICABLE, C_PARAM_USER_DEFINED)
        ),
        information = list(value = fisherInformation, type = C_PARAM_USER_DEFINED),
        design = list(value = design, type = C_PARAM_DERIVED)
    )
    result <- .addFutilityBoundParameterTypes(result, args)
    attr(result, "stage") <- interimStages
    attr(result, "situations") <- informationData$situations
    attr(result, "designPlan") <- designPlan
    attr(result, "informationType") <- informationData$type
    return(result)
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
#' @param sourceValue A numeric vector representing the futility bounds in the
#' source scale. Alternatively, a \code{FisherInformation} object returned by
#' \code{\link[=getFisherInformation]{getFisherInformation()}} can be piped in;
#' see Details.
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
#' @inheritParams param_directionUpper
#' @param theta Numeric. The assumed effect size under the alternative hypothesis on the scale of the
#'   test statistic. For example, in a survival design, this would be the on the log hazard ratio scale.
#' @param information Numeric vector of length 1 or 2 specifying the information
#'   used in the conversion. In general, \code{information[1]} is the cumulative
#'   information available at the analysis to which the futility bound refers,
#'   whereas \code{information[2]} is the additional information planned after
#'   that analysis. The exact requirements depend on \code{sourceScale} and
#'   \code{targetScale}. If present, the \code{"type"} attribute must be
#'   consistent with the requested conversion; see Details.
#' @param naAllowed Logical. Indicates if \code{NA} \code{sourceValue} are permitted. Default is \code{FALSE}.
#' @inheritParams param_three_dots
#'
#' @details
#' If the \code{sourceScale} and \code{targetScale} are the same, the function
#' returns the input \code{sourceValue} without modification.
#' Otherwise, the function is designed to convert between the specified scales.
#'
#' \strong{Piping Fisher information into getFutilityBounds}
#'
#' A complete result of
#' \code{\link[=getFisherInformation]{getFisherInformation()}} can be supplied
#' as \code{sourceValue}, most conveniently with the base R pipe. In this form,
#' \code{getFutilityBounds()} obtains the design, its z-value futility bounds,
#' the requested stages, the information type, and the planning-situation labels
#' from the attributes of the \code{FisherInformation} object. If
#' \code{targetScale} is omitted, the bounds are converted to
#' \code{"effectEstimate"}; another target scale can be requested explicitly.
#'
#' The conversion is performed separately for every planning situation and for
#' every supplied interim analysis. Information for the final analysis is used
#' where a conversion requires future information, but no final-stage futility
#' bound is returned. Consequently, cumulative information is appropriate for
#' the default conversion to the effect-estimate scale. Stage-wise information
#' is required for conversions involving conditional or predictive power. An
#' informative error is issued if the \code{"type"} attribute is incompatible
#' with the requested target scale.
#'
#' \strong{Interpretation of information}
#'
#' The elements of \code{information} have different interpretations:
#' \itemize{
#'   \item \code{information[1]} is the cumulative Fisher information underlying
#'   the test statistic or effect estimate at the analysis where the futility
#'   bound is evaluated. In a two-stage design this is also the information
#'   contributed by the first stage.
#'   \item \code{information[2]} is the additional, non-cumulative Fisher
#'   information to be collected after that analysis. In a two-stage design this
#'   is the information contributed by the second stage, not the cumulative
#'   information at the second analysis.
#' }
#'
#' Consequently, if \eqn{I_1} and \eqn{I_2} denote the cumulative information at
#' the first and second analyses, respectively, specify
#' \code{information = c(I1, I2 - I1)}. A single value is used for both elements.
#'
#' \strong{Information required by conversion type}
#'
#' The required elements are determined by all scales involved in the conversion:
#' \describe{
#'   \item{\code{"zValue"} and \code{"pValue"}}{Conversions between these two
#'   scales do not require information.}
#'   \item{\code{"effectEstimate"}}{Requires \code{information[1]}. The
#'   standardized statistic and effect estimate are related by
#'   \eqn{z = \widehat{\theta}\sqrt{I_1}}. Thus the cumulative information at the
#'   analysis represented by the z-value or effect estimate must be supplied.}
#'   \item{\code{"conditionalPower"}}{Requires \code{information[2]} together
#'   with \code{theta}. Here \code{information[2]} is the additional information
#'   available for the future stage over which conditional power is calculated.}
#'   \item{\code{"condPowerAtObserved"}}{Requires both elements. The current
#'   effect is estimated using the cumulative information \code{information[1]}
#'   and projected over the additional future information
#'   \code{information[2]}.}
#'   \item{\code{"predictivePower"}}{Requires both elements. Predictive power
#'   combines uncertainty based on the cumulative information already observed
#'   with the additional information planned for the future stage.}
#'   \item{\code{"reverseCondPower"}}{Does not require an explicit
#'   \code{information} value; the required information fractions are taken from
#'   the specified \code{design}.}
#' }
#'
#' If \code{condPowerAtObserved} or \code{predictivePower} is involved and no
#' complete information vector is supplied, the relative first- and second-stage
#' information can be derived from an eligible two-stage \code{design} as
#' \code{c(design$informationRates[1], 1 - design$informationRates[1])}. This
#' normalization is sufficient for conversions that depend only on information
#' ratios. An explicitly supplied vector is needed when absolute information is
#' required, for example when conditional power is calculated under a specified
#' value of \code{theta}.
#'
#' A warning is issued if a two-element vector contains an information value not
#' needed for the requested conversion. Set the unused element to \code{NA}, or
#' pass a single value when using the same value for both elements is intended.
#' Information returned by
#' \code{\link[=getFisherInformation]{getFisherInformation()}} has a
#' \code{"type"} attribute. If that attribute is present,
#' \code{getFutilityBounds()} verifies that it is either \code{"cumulative"} for
#' an effect-estimate conversion or \code{"stageWise"} for a conditional- or
#' predictive-power conversion, and stops with an error if the types do not
#' match. Numeric input without this attribute remains supported for backward
#' compatibility.
#'
#' \strong{Obtaining information from a design plan}
#'
#' Use \code{\link[=getFisherInformation]{getFisherInformation()}} to calculate
#' cumulative information from a design plan or simulation results object. For
#' an analysis at stage \code{j}, its result can be passed directly as
#' \code{information[1]} when converting between an effect estimate and a
#' standardized statistic. To construct the information vector for a two-stage
#' conditional- or predictive-power conversion, calculate the cumulative
#' information at both analyses and use:
#' \preformatted{
#' informationStage1 <- getFisherInformation(designPlan, stage = 1)
#' informationCumulative2 <- getFisherInformation(designPlan, stage = 2)
#' information <- c(
#'     informationStage1,
#'     informationCumulative2 - informationStage1
#' )
#' }
#'
#' @return
#' A numeric vector or matrix of class \code{FutilityBounds} representing the
#' futility bounds in the target scale, or \code{NULL} if the conversion is not
#' implemented or yields no result. For pipe-based input with multiple stages
#' and situations, rows represent interim stages and columns represent planning
#' situations. The corresponding labels and calculation context are retained as
#' attributes and displayed by \code{print.FutilityBounds()}.
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
#'
#' # Pipe all planning situations into an effect-estimate-scale conversion
#' getDesignGroupSequential(
#'     informationRates = c(0.2, 0.7, 1),
#'     futilityBounds = c(-0.5, 0)
#' ) |>
#'     getSampleSizeRates() |>
#'     getFisherInformation() |>
#'     getFutilityBounds()
#' }
#'
#' @seealso \code{\link[=getFisherInformation]{getFisherInformation()}} for
#'     calculating the value of the \code{information} argument;
#'     \code{\link[=getDesignGroupSequential]{getDesignGroupSequential()}},
#'     \code{\link[=getDesignInverseNormal]{getDesignInverseNormal()}},
#'     \code{\link[=getDesignFisher]{getDesignFisher()}} for direct
#'     specification of futility bounds on different scales using the
#'     argument \code{futilityBoundsScale}.
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
        directionUpper = NA,
        theta = NA_real_,
        information = NA_real_,
        naAllowed = FALSE) {
    targetScaleMissing <- missing(targetScale)
    sourceScale <- match.arg(sourceScale)
    targetScale <- match.arg(targetScale)

    .warnInCaseOfUnknownArguments(
        functionName = "getFutilityBounds",
        ignore = c("information1", "information2"),
        numberOfAllowedUnnamedParameters = 1,
        exceptionEnabled = FALSE,
        ...
    )
    
    if (is(sourceValue, "FisherInformation")) {
        if (targetScaleMissing) {
            targetScale <- "effectEstimate"
        }
        return(.getFutilityBoundsFromFisherInformation(
            fisherInformation = sourceValue,
            targetScale = targetScale,
            directionUpper = directionUpper,
            theta = theta,
            naAllowed = naAllowed
        ))
    }
    
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

        if (is.na(directionUpper)) {
            directionUpper <- if (is.na(design$directionUpper)) TRUE else design$directionUpper
        } else {
            if (!is.na(design$directionUpper) &&
                    !identical(directionUpper, design$directionUpper)) {
                stopConflictingArguments("in the design directionUpper = ",
                    design$directionUpper, " is defined. ",
                    "In getFutilityBounds() the same direction must be specified, but it is ",
                    directionUpper,
                    functionName = ".assertIsValidDirectionUpper",
                    parameter = "directionUpper",
                    value = design$directionUpper
                )
            } else if (is.na(design$directionUpper) && isFALSE(directionUpper) &&
                    .isTrialDesignInverseNormalOrGroupSequential(design)) {
                criticalValue <- -criticalValue
            }
        }
    } else {
        if (is.na(directionUpper)) {
            directionUpper <- TRUE
        }
    }
    .assertIsSingleLogical(directionUpper, "directionUpper", naAllowed = FALSE)

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
        normQuantile <- if (directionUpper) qnorm(1 - sourceValue) else qnorm(sourceValue)
    }

    sourceValues <- if (sourceScale == "zValue") {
        sourceValue
    } else if (sourceScale == "pValue") {
        normQuantile
    } else if (sourceScale == "effectEstimate") {
        sourceValue * sqrt(information1)
    } else if (.isTrialDesignInverseNormalOrGroupSequential(design)) {
        if (sourceScale == "conditionalPower") {
            (criticalValue - gsWeights[2] * (normQuantile + theta * sqrt(information2))) /
                gsWeights[1]
        } else if (sourceScale == "condPowerAtObserved") {
            (criticalValue /
                gsWeights[2] - normQuantile) /
                (gsWeights[1] / gsWeights[2] + sqrt(information2 / information1))
        } else if (sourceScale == "predictivePower") {
            (criticalValue /
                gsWeights[2] -
                sqrt((information1 + information2) / information1) * normQuantile) /
                (gsWeights[1] / gsWeights[2] + sqrt(information2 / information1))
        } else if (sourceScale == "reverseCondPower") {
            sqrt(design$informationRates[1]) * criticalValue -
                sqrt(1 - design$informationRates[1]) * normQuantile
        }
    } else if (.isTrialDesignFisher(design)) {
        .getFutilityBoundSourceValuesFisher(
            sourceValue,
            criticalValue,
            gsWeights,
            sourceScale,
            theta,
            information1,
            information2,
            directionUpper
        )
    }

    if (targetScale == "zValue") {
        return(.addFutilityBoundParameterTypes(sourceValues, args))
    } else if (targetScale == "pValue") {
        pval <- if (directionUpper) 1 - pnorm(sourceValues) else pnorm(sourceValues)
        return(.addFutilityBoundParameterTypes(pval, args))
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
            directionUpper,
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
                information2,
                directionUpper
            ))
        }
        .showWarningIfCalculatedFutiltyBoundsOutsideAcceptableRange(result, upperBound = NULL)
        return(.addFutilityBoundParameterTypes(result, args))
    }

    stopIllegalArgument("conversion from ", .vQuote(sourceScale), " to ",
        .vQuote(targetScale), " not implemented",
        functionName = "getFutilityBounds",
        parameter = "sourceScale", value = sourceScale,
        relatedParameter = "targetScale",
        relatedValue = targetScale
    )
}

.getFutilityBoundSourceValueFisher <- function(
        sourceValue,
        criticalValue,
        gsWeights,
        sourceScale,
        theta,
        information1,
        information2,
        directionUpper) {
    if (is.na(sourceValue) || sourceValue >= 1 - 1e-07) {
        return(NA_real_)
    }

    quantileSign <- if (directionUpper) +1 else -1

    tryCatch(
        {
            if (sourceScale == "conditionalPower") {
                return(quantileSign * qnorm(1 - criticalValue /
                    pnorm((qnorm(sourceValue) - quantileSign * theta * sqrt(information2)))^gsWeights[2]))
            } else if (sourceScale == "condPowerAtObserved") {
                return(quantileSign * stats::uniroot(
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
                return(quantileSign * stats::uniroot(
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
                    "Source scale ", .vQuote(sourceScale), " not implemented for Fisher's combination test design",
                    call. = FALSE
                )
                return(NA_real_)
            }
        },
        warning = function(w) {
            warning("Failed to calculate ", sQuote(sourceScale), " source value from ",
                sourceValue, ": ", w$message,
                call. = FALSE
            )
        },
        error = function(e) {
            warning("Failed to calculate ", sQuote(sourceScale), " source value from ",
                sourceValue, ": ", e$message,
                call. = FALSE
            )
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
        information2,
        directionUpper) {
    sourceValuesCalculated <- numeric()
    for (sourceValue in sourceValues) {
        sourceValuesCalculated <- c(sourceValuesCalculated, .getFutilityBoundSourceValueFisher(
            sourceValue,
            criticalValue,
            gsWeights,
            sourceScale,
            theta,
            information1,
            information2,
            directionUpper
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
        directionUpper,
        design) {
    normQuantile <- if (targetScale == "conditionalPower") {
        (criticalValue - gsWeights[1] * sourceValues) /
            gsWeights[2] - theta * sqrt(information2)
    } else if (targetScale == "condPowerAtObserved") {
        (criticalValue - gsWeights[1] * sourceValues) /
            gsWeights[2] -
            sourceValues * sqrt(information2 / information1)
    } else if (targetScale == "predictivePower") {
        sqrt(information1 / (information1 + information2)) *
            ((criticalValue - gsWeights[1] * sourceValues) /
                gsWeights[2] -
                sourceValues * sqrt(information2 / information1))
    } else if (targetScale == "reverseCondPower") {
        (sqrt(design$informationRates[1]) * criticalValue - sourceValues) /
            sqrt(1 - design$informationRates[1])
    }

    result <- if (directionUpper) 1 - pnorm(normQuantile) else pnorm(normQuantile)

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
        information2,
        directionUpper) {
    if (is.na(sourceValue)) {
        return(NA_real_)
    }

    normProbability <- if (directionUpper) 1 - pnorm(sourceValue) else pnorm(sourceValue)
    if (normProbability <= criticalValue) {
        return(1)
    }

    quantileSign <- if (directionUpper) +1 else -1

    if (targetScale == "conditionalPower") {
        return(pnorm(
            qnorm((criticalValue / normProbability)^(1 / gsWeights[2])) +
                quantileSign * theta * sqrt(information2)
        ))
    }

    if (targetScale == "condPowerAtObserved") {
        return(pnorm(
            qnorm((criticalValue / normProbability)^(1 / gsWeights[2])) +
                quantileSign * sourceValue * sqrt(information2 / information1)
        ))
    }

    if (targetScale == "predictivePower") {
        return(pnorm(
            sqrt(information1 / (information1 + information2)) *
                (qnorm((criticalValue / normProbability)^(1 / gsWeights[2])) +
                    quantileSign * sourceValue * sqrt(information2 / information1))
        ))
    }

    return(NA_real_)
}

.getNumberOfSubjects <- function(designPlan, stage = NA_integer_) {
    numberOfSubjects <- NA_real_
    matrixInput <- FALSE
    if (.isTrialDesignPlanMeans(designPlan) || .isTrialDesignPlanRates(designPlan)) {
        if (designPlan$.isSampleSizeObject()) {
            numberOfSubjects <- designPlan$numberOfSubjects
            matrixInput <- TRUE
        } else {
            numberOfSubjects <- designPlan$maxNumberOfSubjects * designPlan$.design$informationRates
        }
    } else if (.isTrialDesignPlanSurvival(designPlan)) {
        if (!is.null(designPlan$cumulativeEventsPerStage) && !all(is.na(designPlan$cumulativeEventsPerStage))) {
            numberOfSubjects <- designPlan$cumulativeEventsPerStage
            matrixInput <- TRUE
        } else if (designPlan$.isSampleSizeObject()) {
            numberOfSubjects <- designPlan$eventsFixed * designPlan$.design$informationRates
        } else {
            numberOfSubjects <- designPlan$maxNumberOfEvents * designPlan$.design$informationRates
        }
    } else if (
        is(designPlan, "SimulationResultsMeans") ||
            is(designPlan, "SimulationResultsMultiArmMeans") ||
            is(designPlan, "SimulationResultsRates") ||
            is(designPlan, "SimulationResultsMultiArmRates")) {
        numberOfSubjects <- designPlan$plannedSubjects
    } else if (is(designPlan, "SimulationResultsSurvival") || is(designPlan, "SimulationResultsMultiArmSurvival")) {
        numberOfSubjects <- designPlan$plannedEvents
    }
    if (!is.na(stage)) {
        if (matrixInput && is.matrix(numberOfSubjects)) {
            numberOfSubjects <- numberOfSubjects[stage, ]
        } else {
            numberOfSubjects <- numberOfSubjects[stage]
        }
    }
    return(numberOfSubjects)
}

.getNumberOfSubjectsTwoSample <- function(nTotal, allocationRatio) {
    n1 <- allocationRatio * nTotal / (1 + allocationRatio) # treatment arm
    n2 <- nTotal / (1 + allocationRatio) # control arm
    return(list(n1 = n1, n2 = n2))
}

.getFisherInformationMeansTwoSample <- function(stDev, n1, n2, thetaMult = 1) {
    return(1 / (stDev[1]^2 / n1 + thetaMult * stDev[2]^2 / n2))
}

.getFisherInformationMeans <- function(designPlan, stage = NA_integer_) {
    nTotal <- .getNumberOfSubjects(designPlan, stage)
    stDev <- designPlan$stDev

    # one group case
    if ((.isTrialDesignPlanMeans(designPlan) || is(designPlan, "SimulationResultsMeans")) && designPlan$groups == 1) {
        return(nTotal / stDev[1]^2)
    }

    allocationRatio <- .getAllocationRatioByStage(designPlan, stage)

    # multi-arm case
    if (is(designPlan, "SimulationResultsMultiArmMeans")) {
        return(nTotal / (designPlan$stDev[1]^2 * (1 + allocationRatio)))
    }

    # two group case
    if (length(stDev) == 1 && designPlan$groups == 2) {
        stDev <- rep(stDev, 2)
    }

    n <- .getNumberOfSubjectsTwoSample(nTotal, allocationRatio)
    thetaMult <- ifelse(isTRUE(designPlan$meanRatio), designPlan$thetaH0^2, 1)
    return(.getFisherInformationMeansTwoSample(stDev, n$n1, n$n2, thetaMult))
}

.getFisherInformationRatesTwoSample <- function(pi1, pi2, n1, n2) {
    return(1 / (pi1 * (1 - pi1) / n1 + pi2 * (1 - pi2) / n2))
}

.getFisherInformationRates <- function(designPlan, stage = NA_integer_) {
    nTotal <- .getNumberOfSubjects(designPlan, stage)

    # one group case
    if ((.isTrialDesignPlanRates(designPlan) || is(designPlan, "SimulationResultsRates")) && designPlan$groups == 1) {
        pi0 <- designPlan$thetaH0
        return(nTotal / (pi0 * (1 - pi0)))
    }

    allocationRatio <- .getAllocationRatioByStage(designPlan, stage)

    # multi-arm case
    if (is(designPlan, "SimulationResultsMultiArmRates")) {
        n1 <- nTotal # active arm
        n2 <- nTotal / allocationRatio
        pi1 <- designPlan$effectMatrix
        pi2 <- designPlan$piControl
        return(.getFisherInformationRatesTwoSample(pi1, pi2, n1, n2))
    }

    # two group case
    n <- .getNumberOfSubjectsTwoSample(nTotal, allocationRatio)
    return(.getFisherInformationRatesTwoSample(designPlan$pi1, designPlan$pi2, n$n1, n$n2))
}

.getAllocationRatioByStage <- function(designPlan, stage = NA_integer_) {
    allocationRatioPlanned <- designPlan$allocationRatioPlanned
    if (is.na(stage) || length(allocationRatioPlanned) == 1) {
        return(allocationRatioPlanned)
    }

    return(allocationRatioPlanned[stage])
}

.getFisherInformationSurvival <- function(designPlan, stage = NA_integer_) {
    cumulativeEvents <- .getNumberOfSubjects(designPlan, stage)
    allocationRatio <- .getAllocationRatioByStage(designPlan, stage)

    # multi-arm case
    if (is(designPlan, "SimulationResultsMultiArmSurvival")) {
        omega <- designPlan$effectMatrix

        cumulativeEventsPerComparison <- cumulativeEvents *
            sweep(
                1 + allocationRatio * omega,
                2,
                1 + allocationRatio * colSums(omega),
                FUN = "/"
            )

        return(
            allocationRatio / (1 + allocationRatio)^2 *
                cumulativeEventsPerComparison
        )
    }

    # two group case
    return(allocationRatio / (1 + allocationRatio)^2 * cumulativeEvents)
}

.getFisherInformationByStage <- function(information, stage = NA_integer_) {
    if (is.na(stage)) {
        return(information)
    }
    if (is.matrix(information)) {
        return(information[stage, ])
    }
    return(information[stage])
}

.getCountDataVectorByParameter <- function(x, nParameters) {
    if (is.null(x) || length(x) == 0) {
        return(rep(NA_real_, nParameters))
    }
    if (length(x) == 1) {
        return(rep(x, nParameters))
    }
    return(x)
}

.getCountDataPlanningRates <- function(designPlan, nParameters, allocationRatio) {
    lambda1 <- .getCountDataVectorByParameter(designPlan$lambda1, nParameters)
    lambda2 <- .getCountDataVectorByParameter(designPlan$lambda2, nParameters)
    theta <- .getCountDataVectorByParameter(designPlan$theta, nParameters)
    lambda <- .getCountDataVectorByParameter(designPlan$lambda, nParameters)

    if (!all(is.na(lambda)) && !all(is.na(theta))) {
        lambda2 <- (1 + allocationRatio) * lambda / (1 + allocationRatio * theta)
        lambda1 <- lambda2 * theta
    } else if (!all(is.na(lambda2)) && !all(is.na(theta))) {
        lambda1 <- lambda2 * theta
    } else if (!all(is.na(lambda1)) && !all(is.na(theta))) {
        lambda2 <- lambda1 / theta
    }

    return(list(lambda1 = lambda1, lambda2 = lambda2))
}

.getCountDataAccrualTime <- function(designPlan) {
    accrualTime <- designPlan$accrualTime
    if (length(accrualTime) > 1 && equals(accrualTime[1], 0, tolerance = 0)) {
        accrualTime <- accrualTime[-1]
    }
    return(accrualTime)
}

.getCountDataRecruitmentTimes <- function(designPlan, allocationRatio, maxNumberOfSubjects) {
    accrualTime <- .getCountDataAccrualTime(designPlan)
    accrualIntensity <- designPlan$accrualIntensity

    if (!anyNA(accrualIntensity)) {
        recruitmentTimes <- .generateRecruitmentTimes(
            allocationRatio,
            accrualTime,
            accrualIntensity
        )
        return(list(
            recruit1 = recruitmentTimes$recruit[recruitmentTimes$treatments == 1],
            recruit2 = recruitmentTimes$recruit[recruitmentTimes$treatments == 2]
        ))
    }

    if (length(accrualTime) > 1) {
        stopIllegalArgument(
            "if no 'accrualIntensity' is specified, 'accrualTime' (", .arrayToString(accrualTime), ") ",
            "must be a single number or a vector of length 2 starting with 0, ",
            "but it is of length ", length(accrualTime),
            functionName = ".getCountDataRecruitmentTimes",
            parameter = "accrualTime",
            value = accrualTime,
            relatedParameter = "accrualIntensity",
            relatedValue = accrualIntensity
        )
    }

    n <- .getNumberOfSubjectsTwoSample(maxNumberOfSubjects, allocationRatio)
    return(list(
        recruit1 = seq(0, accrualTime, length.out = n$n1),
        recruit2 = seq(0, accrualTime, length.out = n$n2)
    ))
}

.getFisherInformationCountDataFixedExposure <- function(
        lambda1,
        lambda2,
        overdispersion,
        fixedExposureTime,
        n1,
        n2) {
    sumLambda1 <- n1 * fixedExposureTime * lambda1 /
        (1 + overdispersion * fixedExposureTime * lambda1)
    sumLambda2 <- n2 * fixedExposureTime * lambda2 /
        (1 + overdispersion * fixedExposureTime * lambda2)
    return(1 / (1 / sumLambda1 + 1 / sumLambda2))
}

.getFisherInformationCountDataAtAnalysisTime <- function(
        lambda1,
        lambda2,
        overdispersion,
        fixedExposureTime,
        analysisTime,
        recruit1,
        recruit2) {
    timeUnderObservation1 <- pmax(analysisTime - recruit1, 0)
    timeUnderObservation2 <- pmax(analysisTime - recruit2, 0)
    if (!is.na(fixedExposureTime)) {
        timeUnderObservation1 <- pmin(timeUnderObservation1, fixedExposureTime)
        timeUnderObservation2 <- pmin(timeUnderObservation2, fixedExposureTime)
    }
    return(.getInformationCountData(
        lambda1 = lambda1,
        lambda2 = lambda2,
        overdispersion = overdispersion,
        exposure1 = timeUnderObservation1,
        exposure2 = timeUnderObservation2
    ))
}

.getFisherInformationCountDataFinal <- function(
        designPlan,
        lambda1,
        lambda2,
        allocationRatio,
        maxNumberOfSubjects) {
    overdispersion <- designPlan$overdispersion
    fixedExposureTime <- designPlan$fixedExposureTime
    if (!is.na(fixedExposureTime)) {
        n <- .getNumberOfSubjectsTwoSample(maxNumberOfSubjects, allocationRatio)
        return(.getFisherInformationCountDataFixedExposure(
            lambda1,
            lambda2,
            overdispersion,
            fixedExposureTime,
            n$n1,
            n$n2
        ))
    }

    accrualTime <- .getCountDataAccrualTime(designPlan)
    recruitmentTimes <- .getCountDataRecruitmentTimes(
        designPlan,
        allocationRatio,
        maxNumberOfSubjects
    )
    return(.getFisherInformationCountDataAtAnalysisTime(
        lambda1 = lambda1,
        lambda2 = lambda2,
        overdispersion = overdispersion,
        fixedExposureTime = fixedExposureTime,
        analysisTime = max(accrualTime) + designPlan$followUpTime,
        recruit1 = recruitmentTimes$recruit1,
        recruit2 = recruitmentTimes$recruit2
    ))
}

.getFisherInformationCountDataPower <- function(designPlan) {
    design <- designPlan$.design
    nParameters <- max(
        length(designPlan$lambda1),
        length(designPlan$lambda2),
        length(designPlan$lambda),
        length(designPlan$theta),
        length(designPlan$maxNumberOfSubjects),
        length(designPlan$allocationRatioPlanned),
        1
    )
    allocationRatio <- .getCountDataVectorByParameter(designPlan$allocationRatioPlanned, nParameters)
    maxNumberOfSubjects <- .getCountDataVectorByParameter(designPlan$maxNumberOfSubjects, nParameters)
    rates <- .getCountDataPlanningRates(designPlan, nParameters, allocationRatio)

    maxInformation <- rep(NA_real_, nParameters)
    for (index in seq_len(nParameters)) {
        maxInformation[index] <- .getFisherInformationCountDataFinal(
            designPlan = designPlan,
            lambda1 = rates$lambda1[index],
            lambda2 = rates$lambda2[index],
            allocationRatio = allocationRatio[index],
            maxNumberOfSubjects = maxNumberOfSubjects[index]
        )
    }

    return(design$informationRates %*% t(maxInformation))
}

.getFisherInformationCountDataSimulation <- function(designPlan) {
    design <- designPlan$.design
    nParameters <- max(
        length(designPlan$lambda1),
        length(designPlan$lambda2),
        length(designPlan$lambda),
        length(designPlan$theta),
        length(designPlan$maxNumberOfSubjects),
        length(designPlan$allocationRatioPlanned),
        1
    )
    allocationRatio <- .getCountDataVectorByParameter(designPlan$allocationRatioPlanned, nParameters)
    maxNumberOfSubjects <- .getCountDataVectorByParameter(designPlan$maxNumberOfSubjects, nParameters)
    rates <- .getCountDataPlanningRates(designPlan, nParameters, allocationRatio)

    analysisTime <- designPlan$plannedCalendarTime
    if (design$kMax == 1) {
        accrualTime <- .getCountDataAccrualTime(designPlan)
        followUpTime <- designPlan$followUpTime
        if (!is.na(designPlan$fixedExposureTime)) {
            followUpTime <- designPlan$fixedExposureTime
        }
        analysisTime <- max(accrualTime) + followUpTime
    }

    result <- matrix(NA_real_, nrow = design$kMax, ncol = nParameters)
    for (index in seq_len(nParameters)) {
        recruitmentTimes <- .getCountDataRecruitmentTimes(
            designPlan,
            allocationRatio[index],
            maxNumberOfSubjects[index]
        )
        for (stageIndex in seq_len(design$kMax)) {
            result[stageIndex, index] <- .getFisherInformationCountDataAtAnalysisTime(
                lambda1 = rates$lambda1[index],
                lambda2 = rates$lambda2[index],
                overdispersion = designPlan$overdispersion,
                fixedExposureTime = designPlan$fixedExposureTime,
                analysisTime = analysisTime[stageIndex],
                recruit1 = recruitmentTimes$recruit1,
                recruit2 = recruitmentTimes$recruit2
            )
        }
    }

    return(result)
}

.getFisherInformationCountData <- function(designPlan, stage = NA_integer_) {
    if (.isTrialDesignPlanCountData(designPlan) && designPlan$.isSampleSizeObject()) {
        informationOverStages <- designPlan$informationOverStages
        if (!is.null(informationOverStages) && length(informationOverStages) > 0 &&
                !all(is.na(informationOverStages))) {
            return(.getFisherInformationByStage(informationOverStages, stage))
        }

        maxInformation <- designPlan$maxInformation
        if (!is.null(maxInformation) && length(maxInformation) > 0 && !all(is.na(maxInformation))) {
            informationOverStages <- designPlan$.design$informationRates %*% t(maxInformation)
            return(.getFisherInformationByStage(informationOverStages, stage))
        }

        return(NA_real_)
    }

    if (is(designPlan, "SimulationResultsCountData")) {
        informationOverStages <- .getFisherInformationCountDataSimulation(designPlan)
    } else {
        informationOverStages <- .getFisherInformationCountDataPower(designPlan)
    }

    return(.getFisherInformationByStage(informationOverStages, stage))
}

.getFisherInformationCumulative <- function(designPlan, stage) {
    className <- .getClassName(designPlan)
    if (grepl("Means", className)) {
        return(.getFisherInformationMeans(designPlan, stage = stage))
    } else if (grepl("Rates", className)) {
        return(.getFisherInformationRates(designPlan, stage = stage))
    } else if (grepl("Survival", className)) {
        return(.getFisherInformationSurvival(designPlan, stage = stage))
    } else if (grepl("CountData", className)) {
        return(.getFisherInformationCountData(designPlan, stage = stage))
    }

    return(NA_real_)
}

.combineFisherInformationStages <- function(informationByStage) {
    if (length(informationByStage) == 1L) {
        return(informationByStage[[1]])
    }

    resultLengths <- vapply(informationByStage, length, integer(1))
    if (all(resultLengths == 1L)) {
        return(unlist(informationByStage, use.names = FALSE))
    }

    return(do.call(rbind, lapply(informationByStage, as.vector)))
}

.getFisherInformationSituationParameterNames <- function(designPlan) {
    className <- .getClassName(designPlan)
    if (grepl("Means", className)) {
        return(c("alternative"))
    } else if (grepl("Rates", className)) {
        return(c("pi1", "theta"))
    } else if (grepl("Survival", className)) {
        return(c("hazardRatio", "pi1", "lambda1"))
    } else if (grepl("CountData", className)) {
        return(c("lambda1", "theta", "lambda", "lambda2"))
    }
    return(character())
}

.getFisherInformationSituationLabels <- function(designPlan, nSituations) {
    parameterNames <- .getFisherInformationSituationParameterNames(designPlan)
    parameterNames <- parameterNames[parameterNames %in% names(designPlan)]
    if (length(parameterNames) == 0L) {
        if (nSituations > 1L) {
            return(paste0("situation ", seq_len(nSituations)))
        }
        return(NULL)
    }

    parameterValues <- lapply(parameterNames, function(parameterName) {
        designPlan[[parameterName]]
    })
    valid <- vapply(parameterValues, function(values) {
        !is.null(values) && length(values) > 0L && !all(is.na(values))
    }, logical(1))
    parameterNames <- parameterNames[valid]
    parameterValues <- parameterValues[valid]
    if (length(parameterNames) == 0L) {
        if (nSituations > 1L) {
            return(paste0("situation ", seq_len(nSituations)))
        }
        return(NULL)
    }

    matchingIndex <- which(vapply(parameterValues, length, integer(1)) == nSituations)[1]
    if (!is.na(matchingIndex)) {
        parameterName <- parameterNames[matchingIndex]
        values <- as.vector(parameterValues[[matchingIndex]])
        valueLabels <- format(values, trim = TRUE, digits = getOption("digits"))
        return(paste0(parameterName, " = ", valueLabels))
    }

    if (nSituations == 1L) {
        parameterName <- parameterNames[1]
        values <- parameterValues[[1]]
        if (length(values) == 1L) {
            valueLabel <- format(values, trim = TRUE, digits = getOption("digits"))
            return(paste0(parameterName, " = ", valueLabel))
        }
        return(paste0("common to all ", parameterName, " values"))
    }

    return(paste0("situation ", seq_len(nSituations)))
}

.getNumberOfFisherInformationSituations <- function(fisherInformation, stage) {
    if (!is.null(dim(fisherInformation))) {
        return(ncol(fisherInformation))
    }
    if (length(stage) > 1L) {
        return(1L)
    }
    return(length(fisherInformation))
}

#'
#' @title
#' Get Fisher Information From a Design Plan or Simulation Results
#'
#' @description
#' Calculates cumulative or stage-wise Fisher information at planned analyses for
#' a design plan or simulation results object for means, rates, survival, or
#' count data endpoints. This is particularly useful for calculating the
#' \code{information} argument of
#' \code{\link[=getFutilityBounds]{getFutilityBounds()}}.
#'
#' @param designPlan A trial design plan or simulation results object as returned by functions such as
#' \code{\link[=getSampleSizeMeans]{getSampleSizeMeans()}},
#' \code{\link[=getPowerMeans]{getPowerMeans()}},
#' \code{\link[=getSampleSizeRates]{getSampleSizeRates()}},
#' \code{\link[=getPowerRates]{getPowerRates()}},
#' \code{\link[=getSampleSizeSurvival]{getSampleSizeSurvival()}},
#' \code{\link[=getPowerSurvival]{getPowerSurvival()}},
#' \code{\link[=getSampleSizeCounts]{getSampleSizeCounts()}},
#' \code{\link[=getPowerCounts]{getPowerCounts()}},
#' \code{\link[=getSimulationMeans]{getSimulationMeans()}},
#' \code{\link[=getSimulationRates]{getSimulationRates()}},
#' \code{\link[=getSimulationSurvival]{getSimulationSurvival()}},
#' \code{\link[=getSimulationCounts]{getSimulationCounts()}}, or the
#' corresponding multi-arm simulation functions.
#' @param stage Integer vector. The analysis stage or stages for which Fisher
#'        information is requested. If \code{NA} (default), all stages of the
#'        design are used.
#' @param type Character. Defines whether cumulative information through each
#'        requested analysis (\code{"cumulative"}, the default) or the
#'        information increment contributed by each requested stage
#'        (\code{"stageWise"}) is returned.
#'
#' @details
#' \strong{Cumulative information}
#'
#' With \code{type = "cumulative"}, the function returns the total Fisher
#' information available at each requested analysis, including information
#' accumulated during all preceding stages. With \code{type = "stageWise"}, it
#' returns the increment contributed by each requested stage. The stage-wise
#' value for stage \code{j > 1} is calculated by subtracting the cumulative
#' information at stage \code{j - 1} from that at stage \code{j}, even if stage
#' \code{j - 1} was not included in \code{stage}. At the first analysis,
#' cumulative and stage-wise information are identical.
#'
#' To obtain stage-wise increments from cumulative values \eqn{I_1,\ldots,I_k},
#' use \eqn{I_1, I_2-I_1,\ldots,I_k-I_{k-1}}. For example:
#' \preformatted{
#' informationCumulative <- c(
#'     getFisherInformation(designPlan, stage = 1),
#'     getFisherInformation(designPlan, stage = 2)
#' )
#' informationStageWise <- c(
#'     informationCumulative[1],
#'     diff(informationCumulative)
#' )
#' }
#' If the result contains several planning alternatives or treatment comparisons,
#' apply the differences separately to each corresponding series.
#' The function performs this calculation directly when
#' \code{type = "stageWise"}; the explicit calculation above illustrates its
#' definition.
#'
#' \strong{Calculation by endpoint}
#'
#' \describe{
#'   \item{Means}{For a one-sample comparison, information is the cumulative
#'   sample size divided by the variance. For a two-sample comparison, it is the
#'   inverse variance of the estimated treatment difference, based on cumulative
#'   group sizes, standard deviations, and the planned allocation ratio. For a
#'   mean-ratio analysis, the null value is additionally taken into account.}
#'   \item{Rates}{Information is the inverse binomial variance of the estimated
#'   rate or rate difference under the planning assumptions. It uses cumulative
#'   planned sample sizes, event probabilities, and, for multiple groups, the
#'   planned allocation ratio.}
#'   \item{Survival}{Information is based on the cumulative number of events at
#'   the requested analysis and the planned allocation ratio. For multi-arm
#'   designs, the available events are apportioned to the relevant treatment
#'   comparisons using the planning assumptions.}
#'   \item{Count data}{Information is based on the negative binomial model and
#'   includes all exposure accumulated up to the requested analysis time. It
#'   accounts for recruitment, exposure or follow-up time, event rates,
#'   allocation ratio, and overdispersion. If the design plan already contains
#'   information by analysis, those stored cumulative values are used.}
#' }
#'
#' \strong{Use with getFutilityBounds}
#'
#' For a conversion involving \code{"effectEstimate"}, pass the cumulative
#' information returned for the relevant analysis as \code{information[1]}. For
#' conditional- or predictive-power conversions in a two-stage setting,
#' \code{getFutilityBounds()} interprets \code{information[1]} as the cumulative
#' information at the first analysis and \code{information[2]} as the additional
#' information after that analysis. It can therefore be populated directly with:
#' \preformatted{
#' information <- getFisherInformation(
#'     designPlan,
#'     stage = 1:2,
#'     type = "stageWise"
#' )
#' }
#' For an effect-estimate conversion at analysis \code{j}, use:
#' \preformatted{
#' information <- getFisherInformation(
#'     designPlan,
#'     stage = j,
#'     type = "cumulative"
#' )
#' }
#' The \code{"type"} attribute added to every result enables
#' \code{getFutilityBounds()} to detect incompatible use.
#' The complete result can also be piped into \code{getFutilityBounds()}. In
#' that case, its stage and situation attributes are used to convert every
#' applicable futility bound separately. With the default cumulative type, an
#' omitted target scale means conversion to the effect-estimate scale:
#' \preformatted{
#' designPlan |>
#'     getFisherInformation() |>
#'     getFutilityBounds()
#' }
#'
#' @return
#' A numeric value, vector, or matrix containing cumulative or stage-wise Fisher
#' information, as selected by \code{type}. If multiple stages are requested,
#' a vector is returned for a single planning scenario and a matrix with stages
#' in rows is returned for multiple planning alternatives or comparisons. A
#' vector or matrix can also be returned for a single stage if the object
#' contains several planning alternatives, arms, or sample size values.
#' \code{NA_real_} is returned if the endpoint type is not supported. The result
#' has class \code{FisherInformation} and always includes the attributes
#' \code{"type"} and \code{"stage"}. If the values refer to distinguishable
#' planning situations, the \code{"situations"} attribute contains descriptive
#' labels such as the corresponding alternatives, event probabilities, hazard
#' ratios, or count-data rates.
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
#' getFisherInformation(designPlan, stage = 2)
#' getFisherInformation(designPlan, stage = 1:3, type = "stageWise")
#'
#' simulationResults <- getSimulationMeans(design,
#'     plannedSubjects = c(20, 40, 60), alternative = 0.4,
#'     maxNumberOfIterations = 10
#' )
#' getFisherInformation(simulationResults)
#' }
#'
#' @seealso \code{\link[=getFutilityBounds]{getFutilityBounds()}} for converting
#'     futility bounds using the calculated information.
#'
#' @export
#'
getFisherInformation <- function(
        designPlan,
        stage = NA_integer_,
        type = c("cumulative", "stageWise")) {
    .assertIsTrialDesignPlanOrSimulationResults(designPlan)
    type <- match.arg(type)

    if (length(stage) == 1L && is.na(stage)) {
        stage <- 1:designPlan$.design$kMax
    } else {
        stage <- .assertIsIntegerVector(stage, "stage", validateType = FALSE)
    }
    .assertIsInClosedInterval(
        stage,
        "stage",
        lower = 1L,
        upper = designPlan$.design$kMax
    )

    informationCache <- new.env(parent = emptyenv())
    getCumulativeInformation <- function(stageIndex) {
        cacheKey <- as.character(stageIndex)
        if (!exists(cacheKey, envir = informationCache, inherits = FALSE)) {
            assign(
                cacheKey,
                .getFisherInformationCumulative(designPlan, stageIndex),
                envir = informationCache
            )
        }
        return(get(cacheKey, envir = informationCache, inherits = FALSE))
    }

    informationByStage <- lapply(stage, function(stageIndex) {
        information <- getCumulativeInformation(stageIndex)
        if (type == "stageWise" && stageIndex > 1L) {
            informationPreviousStage <- getCumulativeInformation(stageIndex - 1L)
            information <- information - informationPreviousStage
        }
        return(information)
    })

    fisherInformation <- .combineFisherInformationStages(informationByStage)
    attr(fisherInformation, "type") <- type
    attr(fisherInformation, "stage") <- stage
    if (length(stage) > 1L && !is.null(dim(fisherInformation))) {
        rownames(fisherInformation) <- paste0("stage ", stage)
    }

    nSituations <- .getNumberOfFisherInformationSituations(fisherInformation, stage)
    attr(fisherInformation, "situations") <- .getFisherInformationSituationLabels(
        designPlan,
        nSituations
    )
    attr(fisherInformation, "designPlan") <- designPlan
    class(fisherInformation) <- c("FisherInformation", class(fisherInformation))

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

    fisherInformation <- getFisherInformation(designPlan, stage = 1)
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
    if (grepl("CountData", .getClassName(designPlan))) {
        stageInformation <- NULL
        for (stage in seq_len(nStages)) {
            stageInformation <- rbind(stageInformation, getFisherInformation(designPlan, stage = stage))
        }
    } else {
        stageInformation <- (informationRates / design$informationRates[1]) %*% t(fisherInformation)
    }

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
