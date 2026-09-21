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
#' @include class_fisher_information.R
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
    informationType <- if (is(information, "FisherInformation")) {
        information$type
    } else {
        attr(information, "type", exact = TRUE)
    }
    if (is.null(informationType)) {
        return(invisible())
    }

    validTypes <- c("cumulative", "stageWise")
    if (!is.character(informationType) || length(informationType) != 1L ||
            is.na(informationType) || !informationType %in% validTypes) {
        stopIllegalArgument(
            .sQuote("type"), " metadata of ", .pQuote("information"), " (",
            .arrayToString(informationType), ") must be ", .arrayToString(validTypes, mode = "or"),
            functionName = ".assertIsValidFutilityBoundsInformationType",
            parameter = "information",
            value = if (is(information, "FisherInformation")) information$information else information
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
            value = if (is(information, "FisherInformation")) information$information else information,
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

    if (is(information, "FisherInformation")) {
        if (ncol(as.matrix(information)) > 1L) {
            stopIllegalArgument(
                .pQuote("information"), " contains multiple planning situations; pipe the object into ",
                .pQuote("getFutilityBounds()"), " as ", .pQuote("sourceValue"),
                " to convert every situation separately, or supply the numeric values for one situation",
                functionName = ".getFutilityBoundInformations",
                parameter = "information",
                value = information$information
            )
        }
        information <- as.numeric(information)
    }

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
                    warnArgumentIgnored(
                        "'information[1]' (", information1, ") will be ignored ",
                        "because it is not required for the conversion from ",
                        .vQuote(sourceScale), " to ", .vQuote(targetScale),
                        call. = FALSE,
                        userInstructions = paste0(
                            "Remove the unneeded conversion input only after verifying sourceScale and ",
                            "targetScale; choose the intended scales if they are incorrect."
                        )
                    )
                }
                information1 <- NA_real_
            }
            if (!any(indices == 2) && !is.na(information2)) {
                if (isTRUE(showWarnings)) {
                    warnArgumentIgnored(
                        "'information[2]' (", information2, ") will be ignored ",
                        "because it is not required for the conversion from ",
                        .vQuote(sourceScale), " to ", .vQuote(targetScale),
                        call. = FALSE,
                        userInstructions = paste0(
                            "Remove the unneeded conversion input only after verifying sourceScale and ",
                            "targetScale; choose the intended scales if they are incorrect."
                        )
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
            warnArgumentIgnored(
                "'information1' (", information1, ") will be ignored ",
                "because it will only be taken into account if the information is provided for both stages",
                call. = FALSE,
                parameter = "information1",
                value = information1,
                userInstructions = paste0(
                    "Provide information for both stages, or omit both information inputs if information-based ",
                    "conversion is not intended."
                )
            )
        }
        if (isTRUE(showWarnings) && !is.na(information2)) {
            warnArgumentIgnored(
                "'information2' (", information2, ") will be ignored ",
                "because it will only be taken into account if the information is provided for both stages",
                call. = FALSE,
                parameter = "information2",
                value = information2,
                userInstructions = paste0(
                    "Provide information for both stages, or omit both information inputs if information-based ",
                    "conversion is not intended."
                )
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
#' analysis stages, planning situations, Fisher information type, and
#' design-specific conversion method used for their calculation.
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
    informationContext <- attr(x, "informationContext", exact = TRUE)
    conversionDescription <- attr(x, "conversionDescription", exact = TRUE)
    targetScale <- attr(x, "targetScale", exact = TRUE)
    if (is.list(targetScale)) {
        targetScale <- targetScale$value
    }
    values <- .getFutilityBoundsValuesForPrinting(x)

    scaleDescription <- "target"
    if (!is.null(targetScale) && length(targetScale) == 1L && !is.na(targetScale)) {
        if (targetScale == "effectEstimate") {
            scaleDescription <- "null-centered Wald effect-estimate"
        } else {
            scaleDescription <- gsub("ondP", "onditionalP", targetScale)
            scaleDescription <- gsub("AtObserved$", "AtObservedEffect", scaleDescription)
            scaleDescription <- .formatCamelCaseSingleWord(scaleDescription, sep = "-")
        }
    }
    heading <- paste0("Futility bounds on the ", scaleDescription, " scale")

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
    if (!is.null(conversionDescription) && length(conversionDescription) == 1L) {
        heading <- paste0(heading, "\nusing ", conversionDescription)
    } else if (!is.null(informationType) && length(informationType) == 1L && !is.na(informationType)) {
        informationDescription <- .formatCamelCaseSingleWord(informationType, sep = "-")
        heading <- paste0(heading, "\nusing ", informationDescription, " Fisher information")
        if (identical(targetScale, "effectEstimate") && identical(informationContext, "planningAssumptions")) {
            heading <- paste0(heading, " evaluated under the planning assumptions")
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
        "stage", "situations", "designPlan", "informationType", "informationContext",
        "conversionDescription"
    )
    for (attributeName in parameterAttributes) {
        attr(x, attributeName) <- NULL
    }
    class(x) <- setdiff(class(x), "FutilityBounds")
    return(x)
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
    if (design$sided != 1 || design$kMax != 2) {
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

    return(invisible())
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

.getFutilityBoundsFromDesignPlanWithoutFisherInformation <- function(
        designPlan,
        targetScale,
        directionUpper,
        theta,
        naAllowed,
        stage = NULL) {
    design <- designPlan$.design
    if (is.null(stage)) {
        interimStages <- seq_len(max(design$kMax - 1L, 0L))
    } else {
        interimStages <- stage[stage < design$kMax]
    }
    if (length(interimStages) == 0L) {
        stopIllegalArgument(
            .pQuote("sourceValue"), " must contain a design with at least one interim analysis stage",
            functionName = ".getFutilityBoundsFromDesignPlanWithoutFisherInformation",
            parameter = "sourceValue",
            value = design$kMax
        )
    }

    directionUpperCalculated <- directionUpper
    if (is.na(directionUpperCalculated)) {
        directionUpperCalculated <- if (is.na(design$directionUpper)) TRUE else design$directionUpper
    }
    calculationDesign <- if (targetScale == "reverseCondPower") design else NULL
    calculateBounds <- function() {
        getFutilityBounds(
            sourceValue = .getFutilityBounds(design, interimStages),
            sourceScale = "zValue",
            targetScale = targetScale,
            design = calculationDesign,
            directionUpper = directionUpperCalculated,
            theta = theta,
            information = NA_real_,
            naAllowed = naAllowed
        )
    }
    defaultFutilityBounds <- targetScale == "reverseCondPower" &&
        .isTrialDesignInverseNormalOrGroupSequential(design) &&
        !.anyFutilityBoundsAreInvalid(design$futilityBounds, design$directionUpper)
    if (defaultFutilityBounds) {
        result <- withCallingHandlers(
            calculateBounds(),
            warning = function(warningCondition) {
                if (identical(
                        conditionMessage(warningCondition),
                        "At least one calculated futility bound outside acceptable range")) {
                    invokeRestart("muffleWarning")
                }
            }
        )
        result[] <- 0
    } else {
        result <- calculateBounds()
    }
    attr(result, "stage") <- interimStages
    attr(result, "designPlan") <- designPlan
    return(result)
}

.getFisherInformationAsMatrix <- function(fisherInformation) {
    stage <- fisherInformation$stage
    situations <- fisherInformation$situations
    designPlan <- fisherInformation$.getDesignPlan()
    informationType <- fisherInformation$type

    if (is.null(stage) || length(stage) == 0L) {
        stopIllegalArgument(
            .pQuote("information"), " does not contain the required ", sQuote("stage"), " field",
            functionName = ".getFisherInformationAsMatrix",
            parameter = "information",
            value = fisherInformation$information
        )
    }

    informationMatrix <- as.matrix(fisherInformation)

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

.getFisherInformationForTreatmentEffectScale <- function(
        fisherInformation,
        designPlan,
        stages) {
    if (is.null(fisherInformation)) {
        return(NULL)
    }
    if (!is(fisherInformation, "FisherInformation")) {
        stopIllegalArgument(
            .pQuote("fisherInformation"), " must be a FisherInformation object",
            functionName = ".getFisherInformationForTreatmentEffectScale",
            parameter = "fisherInformation",
            value = fisherInformation
        )
    }
    if (!identical(fisherInformation$type, "cumulative")) {
        stopIllegalArgument(
            .pQuote("fisherInformation"), " must have type ", .vQuote("cumulative"),
            " for conversion to the treatment-effect scale",
            functionName = ".getFisherInformationForTreatmentEffectScale",
            parameter = "fisherInformation",
            value = fisherInformation$information,
            relatedParameter = "type",
            relatedValue = fisherInformation$type
        )
    }

    informationDesignPlan <- fisherInformation$.getDesignPlan()
    if (is.null(informationDesignPlan)) {
        stopIllegalArgument(
            .pQuote("fisherInformation"), " does not contain its originating design plan",
            functionName = ".getFisherInformationForTreatmentEffectScale",
            parameter = "fisherInformation",
            value = fisherInformation$information
        )
    }
    if (!identical(informationDesignPlan, designPlan)) {
        stopConflictingArguments(
            .pQuote("fisherInformation"), " and ", .pQuote("designPlan"),
            " must refer to the same trial design plan",
            functionName = ".getFisherInformationForTreatmentEffectScale",
            parameter = "fisherInformation",
            value = fisherInformation$information
        )
    }

    informationData <- .getFisherInformationAsMatrix(fisherInformation)
    stageIndices <- match(stages, informationData$stage)
    if (anyNA(stageIndices)) {
        stopIllegalArgument(
            .pQuote("fisherInformation"), " must contain cumulative information for ",
            ifelse(length(stages) == 1L, "stage ", "stages "),
            .arrayToString(stages),
            functionName = ".getFisherInformationForTreatmentEffectScale",
            parameter = "fisherInformation",
            value = fisherInformation$information,
            relatedParameter = "stage",
            relatedValue = informationData$stage
        )
    }

    result <- informationData$values[stageIndices, , drop = FALSE]
    invalidValues <- !is.na(result) & (!is.finite(result) | result <= 0)
    if (any(invalidValues)) {
        stopIllegalArgument(
            .pQuote("fisherInformation"), " must contain positive finite values or ", .vQuote("NA"),
            functionName = ".getFisherInformationForTreatmentEffectScale",
            parameter = "fisherInformation",
            value = fisherInformation$information
        )
    }
    return(result)
}

.getFutilityBoundsOnTreatmentEffectScale <- function(
        designPlan,
        stages = NULL,
        zValues = NULL,
        fisherInformation = NULL) {
    .assertIsTrialDesignPlan(designPlan)
    design <- designPlan$.design
    if (is.null(stages)) {
        stages <- seq_len(max(design$kMax - 1L, 0L))
    }
    stages <- stages[stages < design$kMax]
    if (length(stages) == 0L) {
        stopIllegalArgument(
            .pQuote("sourceValue"), " must refer to at least one interim analysis stage",
            functionName = ".getFutilityBoundsOnTreatmentEffectScale",
            parameter = "stage",
            value = stages
        )
    }

    sourceValues <- if (is.null(zValues)) .getFutilityBounds(design, stages) else zValues
    result <- round(.getFutilityBoundsTreatmentEffectMatrix(
        designPlan,
        stages = stages,
        zValues = zValues,
        fisherInformation = fisherInformation
    ), 8L)
    nSituations <- ncol(result)
    situations <- .getFisherInformationSituationLabels(designPlan, nSituations)
    dimnames(result) <- list(paste0("stage ", stages), situations)

    if (nrow(result) == 1L) {
        result <- as.numeric(result[1L, ])
    } else if (ncol(result) == 1L) {
        result <- as.numeric(result[, 1L])
    }
    args <- list(
        sourceValue = list(value = sourceValues, type = C_PARAM_DERIVED),
        sourceScale = list(value = "zValue", type = C_PARAM_DERIVED),
        targetScale = list(value = "treatmentEffect", type = C_PARAM_USER_DEFINED),
        theta = list(value = NA_real_, type = C_PARAM_NOT_APPLICABLE),
        information = list(
            value = if (is.null(fisherInformation)) NA_real_ else fisherInformation$information,
            type = if (is.null(fisherInformation)) C_PARAM_NOT_APPLICABLE else C_PARAM_USER_DEFINED
        ),
        design = list(value = design, type = C_PARAM_DERIVED)
    )
    result <- .addFutilityBoundParameterTypes(result, args)
    attr(result, "stage") <- stages
    attr(result, "situations") <- situations
    attr(result, "designPlan") <- designPlan
    if (!is.null(fisherInformation)) {
        attr(result, "informationType") <- fisherInformation$type
    }
    attr(result, "conversionDescription") <-
        .getFutilityBoundsTreatmentEffectConversionDescription(designPlan)
    return(result)
}

.getFutilityBoundsFromFisherInformation <- function(
        fisherInformation,
        targetScale,
        directionUpper,
        theta,
        naAllowed) {
    designPlan <- fisherInformation$.getDesignPlan()
    if (is.null(designPlan)) {
        stopIllegalArgument(
            .pQuote("information"), " does not contain the design plan required for pipe-based conversion",
            functionName = ".getFutilityBoundsFromFisherInformation",
            parameter = "information",
            value = fisherInformation$information
        )
    }

    design <- designPlan$.design
    informationRequired <- !is.null(.getRequiredFutilityBoundsInformationType(
        sourceScale = "zValue",
        targetScale = targetScale
    ))
    if (!informationRequired) {
        warnArgumentIgnored(
            "Fisher information is not required for conversion from ",
            .vQuote("zValue"), " to ", .vQuote(targetScale), " and will be ignored",
            call. = FALSE,
            userInstructions = paste0(
                "Remove the unneeded conversion input only after verifying sourceScale and targetScale; choose ",
                "the intended scales if they are incorrect."
            )
        )
        return(.getFutilityBoundsFromDesignPlanWithoutFisherInformation(
            designPlan = designPlan,
            targetScale = targetScale,
            directionUpper = directionUpper,
            theta = theta,
            naAllowed = naAllowed,
            stage = fisherInformation$stage
        ))
    }
    informationData <- .getFisherInformationAsMatrix(fisherInformation)
    .assertIsValidFutilityBoundsInformationType(
        fisherInformation,
        sourceScale = "zValue",
        targetScale = targetScale
    )
    if (.futilityBoundsCalculationRequiresDesign(targetScale)) {
        .assertIsValidDesignForFutilityBoundsConversion(
            design,
            sourceScale = "zValue",
            targetScale = targetScale
        )
    }

    interimStages <- informationData$stage[informationData$stage < design$kMax]
    if (length(interimStages) == 0L) {
        stopIllegalArgument(
            .pQuote("information"), " must contain at least one interim analysis stage",
            functionName = ".getFutilityBoundsFromFisherInformation",
            parameter = "information",
            value = fisherInformation$information,
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
    probabilityScalesRequiringDesign <- c(powerScales, "reverseCondPower")
    defaultFutilityBounds <-
        .isTrialDesignInverseNormalOrGroupSequential(design) &&
        !.anyFutilityBoundsAreInvalid(design$futilityBounds, design$directionUpper)

    if (defaultFutilityBounds && targetScale %in% probabilityScalesRequiringDesign) {
        result[,] <- 0
    } else if (targetScale %in% powerScales) {
        requiredStages <- seq_len(design$kMax)
        requiredStageIndices <- match(requiredStages, informationData$stage)
        if (anyNA(requiredStageIndices)) {
            stopIllegalArgument(
                .pQuote("information"), " must contain all design stages for conversion to ",
                .vQuote(targetScale),
                functionName = ".getFutilityBoundsFromFisherInformation",
                parameter = "information",
                value = fisherInformation$information,
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
        information = list(value = informationData$values, type = C_PARAM_USER_DEFINED),
        design = list(value = design, type = C_PARAM_DERIVED)
    )
    result <- .addFutilityBoundParameterTypes(result, args)
    attr(result, "stage") <- interimStages
    attr(result, "situations") <- informationData$situations
    attr(result, "designPlan") <- designPlan
    attr(result, "informationType") <- informationData$type
    if (targetScale == "effectEstimate") {
        attr(result, "informationContext") <- "planningAssumptions"
    }
    return(result)
}

#'
#' @title
#' Get Futility Bounds
#'
#' @description
#' This function converts futility bounds between different scales such as
#' z-value, p-value, conditional power, predictive power, reverse conditional
#' power, Wald effect estimate, and design-specific treatment effect.
#'
#' @param sourceValue A numeric vector or matrix representing the futility bounds in the
#' source scale. Alternatively, a trial design plan, simulation results, or a
#' \code{FisherInformation} object returned by
#' \code{\link[=getFisherInformation]{getFisherInformation()}} can be piped in;
#' see Details.
#' @param sourceScale Character. The scale of the input futility bounds.
#' Must be one of \code{"zValue"}, \code{"pValue"},
#' \code{"conditionalPower"}, \code{"condPowerAtObserved"}, \code{"predictivePower"},
#' \code{"reverseCondPower"}, \code{"effectEstimate"}, or
#' \code{"treatmentEffect"}.
#' @param targetScale Character. The scale to which the futility bounds should
#' be converted. Must be one of \code{"zValue"}, \code{"pValue"},
#' \code{"conditionalPower"}, \code{"condPowerAtObserved"}, \code{"predictivePower"},
#' \code{"reverseCondPower"}, \code{"effectEstimate"}, or
#' \code{"treatmentEffect"}.
#' @param design The trial design. Required if either the \code{sourceScale} or
#' \code{targetScale} is \code{"reverseCondPower"} or if the conversion
#' involves conditional or predictive power in a group sequential or Fisher
#' design; these conversions require a supported one-sided two-stage design.
#' Conversion from or to \code{"treatmentEffect"} requires a trial design plan,
#' rather than only its contained trial design, because endpoint-specific
#' planning parameters are needed.
#' @inheritParams param_directionUpper
#' @param theta Numeric. The assumed treatment effect under the alternative
#'   hypothesis on the unstandardized analysis scale. For example, in a
#'   survival design this is specified on the log hazard-ratio scale.
#' @param information Numeric vector of length 1 or 2 specifying the information
#'   used in the conversion. In general, \code{information[1]} is the cumulative
#'   information available at the analysis to which the futility bound refers,
#'   whereas \code{information[2]} is the additional information planned after
#'   that analysis. The exact requirements depend on \code{sourceScale} and
#'   \code{targetScale}. A \code{FisherInformation} object can also be supplied;
#'   its \code{type} field must be consistent with the requested conversion;
#'   it must represent a single planning situation when used as this argument.
#'   See Details.
#' @param stage Integer vector identifying the analysis stages represented by
#'   numeric \code{sourceValue} when converting from or to the
#'   \code{"treatmentEffect"} scale. It is inferred automatically from piped
#'   \code{FutilityBounds} or \code{FisherInformation} objects. If omitted
#'   otherwise, all interim stages are used.
#' @param naAllowed Logical. Indicates if \code{NA} \code{sourceValue} are permitted. Default is \code{FALSE}.
#' @inheritParams param_three_dots
#'
#' @details
#' If the \code{sourceScale} and \code{targetScale} are the same, the function
#' returns the input \code{sourceValue} without modification.
#' Otherwise, the function is designed to convert between the specified scales.
#'
#' \strong{Available scales}
#'
#' Each scale expresses the same interim futility threshold from a different
#' perspective:
#' \describe{
#'   \item{\code{"zValue"}}{The standardized interim test statistic. For
#'   \code{directionUpper = TRUE}, larger values favor the alternative;
#'   \code{directionUpper = FALSE} reverses this direction. Conversion between
#'   this scale and \code{"pValue"} does not require Fisher information.}
#'   \item{\code{"pValue"}}{The one-sided p-value corresponding to the
#'   z-value and the selected direction. Smaller values indicate stronger
#'   evidence in favor of the alternative. Conversion between this scale and
#'   \code{"zValue"} does not require Fisher information.}
#'   \item{\code{"effectEstimate"}}{The unstandardized, null-centered effect
#'   estimate \eqn{\widehat{\delta}} on the Wald analysis scale, related to the
#'   z-value by \eqn{z = \widehat{\delta}\sqrt{I_1}}. Depending on the endpoint,
#'   this can be a mean or rate difference, or an effect on a transformed scale
#'   such as the log hazard-ratio scale. Adding the null value or applying a
#'   back-transformation may be necessary to obtain the endpoint's usual
#'   presentation scale. Fisher information is evaluated under the planning
#'   assumptions.}
#'   \item{\code{"treatmentEffect"}}{The endpoint's natural treatment-effect
#'   scale, calculated with the same endpoint- and test-specific transformation
#'   used for \code{futilityBoundsEffectScale} in the design plan. For two-group
#'   binary rates this inverts the Farrington--Manning score statistic; for
#'   count data it uses the validated negative-binomial inversion with a
#'   candidate-dependent variance estimate; for survival data it returns the
#'   hazard-ratio scale; and other endpoints retain their existing
#'   design-specific transformations. This scale requires a
#'   trial design plan, either supplied as
#'   \code{design} or retained in a piped result. Cumulative Fisher information
#'   is calculated internally where the endpoint-specific transformation needs
#'   it, or taken from a piped \code{FisherInformation} object after validation.
#'   The Count Data inversion additionally uses the stage-specific sample size,
#'   exposure, recruitment, allocation, and overdispersion from the design plan.}
#'   \item{\code{"conditionalPower"}}{The probability of rejecting the null
#'   hypothesis at the final analysis, conditional on the interim result and
#'   assuming the user-specified treatment effect \code{theta} for the future
#'   observations. It requires \code{theta} and the additional second-stage
#'   information.}
#'   \item{\code{"condPowerAtObserved"}}{Conditional power calculated by
#'   using the interim effect estimate in place of a separately assumed effect.
#'   It treats the observed estimate as fixed when projecting the future data
#'   and, in the supported two-stage setting, depends on the information through
#'   the ratio of second-stage to first-stage information.}
#'   \item{\code{"predictivePower"}}{The Bayesian predictive probability of
#'   rejecting the null hypothesis at the final analysis under a flat
#'   (improper) prior for the treatment effect. Unlike conditional power at the
#'   observed effect, it integrates uncertainty about that effect. In the
#'   supported two-stage setting, it also depends only on the information
#'   ratio.}
#'   \item{\code{"reverseCondPower"}}{Reverse conditional power (also called
#'   reverse stochastic curtailment): the conditional probability that the
#'   interim result would be at least as unfavorable as the observed result,
#'   given that the final combined test statistic is at its critical boundary.
#'   It is independent of an assumed treatment effect and, for the supported
#'   inverse normal or group sequential setting, coincides with the predictive
#'   power based on a flat prior. Conversion between this scale and
#'   \code{"zValue"} or \code{"pValue"} uses the design's information rates,
#'   but does not require endpoint-specific Fisher information.}
#' }
#' The four power-based scales take values between \code{0} and \code{1}. Their
#' values describe a futility threshold at an interim analysis; they should not
#' be confused with the unconditional power of the trial design.
#'
#' \strong{Piping design plans or Fisher information into getFutilityBounds}
#'
#' A trial design plan or simulation results object can be supplied directly.
#' If the requested conversion needs Fisher information,
#' \code{getFisherInformation()} is called internally with the required type:
#' \code{"cumulative"} for an effect-estimate conversion and \code{"stageWise"}
#' for conversions involving conditional or predictive power. It is not called
#' for conversion from the design's z-value bounds to \code{"zValue"},
#' \code{"pValue"}, or \code{"reverseCondPower"}; the latter uses information
#' rates from the design itself. This provides the short form
#' \preformatted{
#' designPlan |>
#'     getFutilityBounds(targetScale = "condPowerAtObserved")
#' }
#' instead of explicitly inserting
#' \code{getFisherInformation(type = "stageWise")} into the pipe.
#'
#' A complete result of
#' \code{\link[=getFisherInformation]{getFisherInformation()}} can be supplied
#' as \code{sourceValue}, most conveniently with the base R pipe. In this form,
#' \code{getFutilityBounds()} obtains the design, its z-value futility bounds,
#' the requested stages, the information type, and the planning-situation labels
#' from the fields of the \code{FisherInformation} object. If
#' \code{targetScale} is omitted, the bounds are converted to
#' \code{"effectEstimate"}; another target scale can be requested explicitly.
#' If Fisher information is not needed for that scale, the object remains a
#' valid source of the design and its futility bounds, but its information
#' values are ignored and a warning is issued. The printed result then does not
#' claim that Fisher information was used. An exception is
#' \code{targetScale = "treatmentEffect"}: the cumulative information object is
#' passed to and validated by the endpoint-specific transformation instead of
#' being discarded. For Count Data, its originating design and requested stages
#' are reused without recalculating Fisher information; the actual nonlinear
#' inversion uses the corresponding design-specific sample sizes and a variance
#' estimate evaluated at each candidate rate ratio.
#'
#' Conversion to \code{"treatmentEffect"} uses
#' \code{.getFutilityBoundsTreatmentEffectScale()} and therefore reproduces its
#' validated endpoint-specific values exactly (up to the eight-digit rounding
#' used for \code{futilityBoundsEffectScale}). This includes the nonlinear
#' Count Data calculation and reproduces the corresponding design-plan field.
#' Conversely, a result
#' on this scale can be piped back into \code{getFutilityBounds()}; its retained
#' design plan and stage metadata are used to recover the corresponding
#' standardized bounds before conversion to the requested target scale.
#' For two-group rates the output explicitly identifies the design-specific
#' Farrington--Manning inversion. The distinct \code{"effectEstimate"} scale
#' continues to report the null-centered Wald transformation based on cumulative
#' Fisher information under the planning assumptions.
#'
#' The conversion is performed separately for every planning situation and for
#' every supplied interim analysis. Information for the final analysis is used
#' where a conversion requires future information, but no final-stage futility
#' bound is returned. Consequently, cumulative information is appropriate for
#' the default conversion to the effect-estimate scale. Stage-wise information
#' is required for conversions involving conditional or predictive power. An
#' informative error is issued if its \code{type} field is incompatible
#' with the requested target scale.
#' Conversions to conditional power, conditional power at the observed effect,
#' predictive power, or reverse conditional power remain restricted to
#' one-sided two-stage designs. For a design with more than two stages, the
#' conditional probability would additionally require a precise definition of
#' how all remaining analyses and their stopping boundaries are handled.
#' If a group sequential or inverse normal design contains only the default
#' futility bound, that bound represents the absence of futility stopping. Its
#' conversion to a conditional-power, predictive-power, or reverse-conditional-
#' power scale is therefore returned as the exact boundary value \code{0},
#' without a numerical-range warning. This applies in both directions of the
#' alternative.
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
#'   \item{\code{"treatmentEffect"}}{Does not use the separately supplied
#'   numeric \code{information} argument. Instead, it requires a trial design
#'   plan and applies the endpoint- and test-specific transformation used by
#'   \code{futilityBoundsEffectScale}. Cumulative information is calculated
#'   internally when needed. Alternatively, a piped \code{FisherInformation}
#'   object of type \code{"cumulative"} is validated and used. For two-group
#'   rates, the validated Farrington--Manning calculation is based directly on
#'   the planned group sizes and null-restricted rates rather than on a generic
#'   Wald-information substitution. For Count Data, the validated
#'   negative-binomial inversion evaluates the variance at each candidate rate
#'   ratio and uses the planned stage sample size, recruitment, exposure,
#'   allocation ratio, and overdispersion. A piped cumulative
#'   \code{FisherInformation} object supplies and validates the design/stage
#'   context, but its stored numeric values do not replace that
#'   candidate-dependent variance calculation.}
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
#' A \code{FisherInformation} object returned by
#' \code{\link[=getFisherInformation]{getFisherInformation()}} has a
#' \code{type} field. \code{getFutilityBounds()} verifies that it is
#' \code{"cumulative"} for
#' an effect-estimate conversion or \code{"stageWise"} for a conditional- or
#' predictive-power conversion, and stops with an error if the types do not
#' match. Plain numeric input without type metadata remains supported for
#' backward compatibility.
#' If a \code{FisherInformation} object contains multiple planning situations,
#' pipe it into \code{getFutilityBounds()} as \code{sourceValue}; the conversion
#' is then performed separately for every situation. The \code{information}
#' argument itself accepts such an object only when it represents one situation.
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
#' informationStage1 <- as.numeric(getFisherInformation(designPlan, stage = 1))
#' informationCumulative2 <- as.numeric(getFisherInformation(designPlan, stage = 2))
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
#'
#' # Fisher information is calculated internally with type = "stageWise"
#' getDesignGroupSequential(kMax = 2, futilityBounds = 0.3) |>
#'     getSampleSizeRates() |>
#'     getFutilityBounds(targetScale = "condPowerAtObserved")
#'
#' # Reproduce the design-specific futility bounds on the treatment-effect scale
#' treatmentEffectBounds <- getDesignGroupSequential(
#'     informationRates = c(0.2, 0.7, 1),
#'     futilityBounds = c(0.3, 0.2)
#' ) |>
#'     getSampleSizeRates() |>
#'     getFutilityBounds(targetScale = "treatmentEffect")
#'
#' # Use treatmentEffect as a source scale; sourceScale is inferred here
#' treatmentEffectBounds |>
#'     getFutilityBounds(targetScale = "pValue")
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
            "effectEstimate",
            "treatmentEffect"
        ),
        targetScale = c(
            "zValue",
            "pValue",
            "conditionalPower",
            "condPowerAtObserved",
            "predictivePower",
            "reverseCondPower",
            "effectEstimate",
            "treatmentEffect"
        ),
        design = NULL,
        directionUpper = NA,
        theta = NA_real_,
        information = NA_real_,
        stage = NA_integer_,
        naAllowed = FALSE) {
    sourceScaleMissing <- missing(sourceScale)
    sourceScale <- match.arg(sourceScale)
    targetScale <- match.arg(targetScale)

    .warnInCaseOfUnknownArguments(
        functionName = "getFutilityBounds",
        ignore = c("information1", "information2"),
        numberOfAllowedUnnamedParameters = 1,
        exceptionEnabled = FALSE,
        ...
    )

    if (is(sourceValue, "TrialDesignPlan") || is(sourceValue, "SimulationResults")) {
        if (targetScale == "treatmentEffect") {
            return(.getFutilityBoundsOnTreatmentEffectScale(
                designPlan = sourceValue
            ))
        }
        requiredInformationType <- .getRequiredFutilityBoundsInformationType(
            sourceScale = "zValue",
            targetScale = targetScale
        )
        if (is.null(requiredInformationType)) {
            return(.getFutilityBoundsFromDesignPlanWithoutFisherInformation(
                designPlan = sourceValue,
                targetScale = targetScale,
                directionUpper = directionUpper,
                theta = theta,
                naAllowed = naAllowed
            ))
        }
        sourceValue <- getFisherInformation(
            sourceValue,
            type = requiredInformationType
        )
    }
    
    if (is(sourceValue, "FisherInformation")) {
        if (targetScale == "treatmentEffect") {
            return(.getFutilityBoundsOnTreatmentEffectScale(
                designPlan = sourceValue$.getDesignPlan(),
                stages = sourceValue$stage,
                fisherInformation = sourceValue
            ))
        }
        return(.getFutilityBoundsFromFisherInformation(
            fisherInformation = sourceValue,
            targetScale = targetScale,
            directionUpper = directionUpper,
            theta = theta,
            naAllowed = naAllowed
        ))
    }

    sourceDesignPlan <- NULL
    sourceStages <- NULL
    if (is(sourceValue, "FutilityBounds")) {
        sourceDesignPlan <- attr(sourceValue, "designPlan", exact = TRUE)
        sourceStages <- attr(sourceValue, "stage", exact = TRUE)
        if (sourceScaleMissing) {
            sourceScaleAttribute <- attr(sourceValue, "targetScale", exact = TRUE)
            if (is.list(sourceScaleAttribute)) {
                sourceScale <- sourceScaleAttribute$value
            }
        }
        if (sourceScale == targetScale) {
            return(sourceValue)
        }
        sourceValue <- as.numeric(sourceValue)
    }

    if (is(design, "TrialDesignPlan") || is(design, "SimulationResults")) {
        sourceDesignPlan <- design
        design <- design$.design
    }
    if (is.null(sourceDesignPlan)) {
        sourceDesignPlan <- attr(sourceValue, "designPlan", exact = TRUE)
    }

    if (sourceScale == "treatmentEffect" && targetScale == "treatmentEffect") {
        .assertIsNumericVector(as.vector(sourceValue), "sourceValue", naAllowed = naAllowed)
        args <- list(
            sourceValue = list(value = sourceValue, type = C_PARAM_USER_DEFINED),
            sourceScale = list(value = sourceScale, type = C_PARAM_USER_DEFINED),
            targetScale = list(value = targetScale, type = C_PARAM_USER_DEFINED),
            theta = list(value = NA_real_, type = C_PARAM_NOT_APPLICABLE),
            information = list(value = NA_real_, type = C_PARAM_NOT_APPLICABLE),
            design = list(
                value = if (is.null(sourceDesignPlan)) NULL else sourceDesignPlan$.design,
                type = if (is.null(sourceDesignPlan)) C_PARAM_NOT_APPLICABLE else C_PARAM_DERIVED
            )
        )
        return(.addFutilityBoundParameterTypes(sourceValue, args))
    }

    if (sourceScale == "treatmentEffect" || targetScale == "treatmentEffect") {
        .assertIsNumericVector(as.vector(sourceValue), "sourceValue", naAllowed = naAllowed)
        if (is.null(sourceDesignPlan)) {
            stopMissingArgument(
                .sQuote("design"), " must be a TrialDesignPlan object for conversion ",
                "from or to the treatment-effect scale",
                functionName = "getFutilityBounds",
                parameter = "design"
            )
        }
        .assertIsTrialDesignPlan(sourceDesignPlan)
        if (length(stage) == 1L && is.na(stage)) {
            stage <- sourceStages
        }
        if (is.null(stage) || length(stage) == 0L || anyNA(stage)) {
            stage <- seq_len(max(sourceDesignPlan$.design$kMax - 1L, 0L))
        }
        stage <- .assertIsIntegerVector(stage, "stage", validateType = FALSE)
        .assertIsInClosedInterval(
            stage,
            "stage",
            lower = 1L,
            upper = sourceDesignPlan$.design$kMax - 1L
        )

        if (targetScale == "treatmentEffect") {
            zValues <- if (sourceScale == "zValue") {
                sourceValue
            } else {
                .convertFutilityBoundsScaleWithDesignPlan(
                    designPlan = sourceDesignPlan,
                    values = sourceValue,
                    stages = stage,
                    sourceScale = sourceScale,
                    targetScale = "zValue",
                    theta = theta,
                    naAllowed = naAllowed
                )
            }
            return(.getFutilityBoundsOnTreatmentEffectScale(
                designPlan = sourceDesignPlan,
                stages = stage,
                zValues = zValues
            ))
        }

        zValues <- .getFutilityBoundsTreatmentEffectScaleToZValue(
            sourceDesignPlan,
            treatmentEffects = sourceValue,
            stages = stage
        )
        result <- if (targetScale == "zValue") {
            zValues
        } else {
            .convertFutilityBoundsScaleWithDesignPlan(
                designPlan = sourceDesignPlan,
                values = zValues,
                stages = stage,
                sourceScale = "zValue",
                targetScale = targetScale,
                theta = theta,
                naAllowed = naAllowed
            )
        }
        situations <- .getFisherInformationSituationLabels(sourceDesignPlan, ncol(result))
        dimnames(result) <- list(paste0("stage ", stage), situations)
        if (nrow(result) == 1L) result <- as.numeric(result[1L, ])
        if (!is.null(dim(result)) && ncol(result) == 1L) result <- as.numeric(result[, 1L])
        args <- list(
            sourceValue = list(value = sourceValue, type = C_PARAM_USER_DEFINED),
            sourceScale = list(value = "treatmentEffect", type = C_PARAM_USER_DEFINED),
            targetScale = list(value = targetScale, type = C_PARAM_USER_DEFINED),
            theta = list(value = NA_real_, type = C_PARAM_NOT_APPLICABLE),
            information = list(value = NA_real_, type = C_PARAM_NOT_APPLICABLE),
            design = list(value = sourceDesignPlan$.design, type = C_PARAM_DERIVED)
        )
        result <- .addFutilityBoundParameterTypes(result, args)
        attr(result, "stage") <- stage
        attr(result, "situations") <- situations
        attr(result, "designPlan") <- sourceDesignPlan
        informationType <- .getRequiredFutilityBoundsInformationType("zValue", targetScale)
        if (is.null(informationType)) {
            attr(result, "conversionDescription") <-
                .getFutilityBoundsTreatmentEffectConversionDescription(
                    sourceDesignPlan,
                    direction = "fromTreatmentEffect"
                )
        } else {
            attr(result, "informationType") <- informationType
            if (targetScale == "effectEstimate") {
                attr(result, "informationContext") <- "planningAssumptions"
            }
        }
        return(result)
    }

    sourceValue <- .assertIsNumericVector(sourceValue, "sourceValue", naAllowed = naAllowed)

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
                warnResultUnavailable(
                    "Source scale ", .vQuote(sourceScale), " not implemented for Fisher's combination test design",
                    call. = FALSE,
                    userInstructions = paste0(
                        "Choose a source scale supported for Fisher combination designs, or a different design ",
                        "if scientifically appropriate."
                    )
                )
                return(NA_real_)
            }
        },
        warning = function(w) {
            warnNumericalIssue("Failed to calculate ", sQuote(sourceScale), " source value from ",
                sourceValue, ": ", w$message,
                call. = FALSE,
                userInstructions = paste0(
                    "Check sourceScale and sourceValue and resolve the reported conversion problem before using ",
                    "the converted bounds."
                )
            )
        },
        error = function(e) {
            warnNumericalIssue("Failed to calculate ", sQuote(sourceScale), " source value from ",
                sourceValue, ": ", e$message,
                call. = FALSE,
                userInstructions = paste0(
                    "Check sourceScale and sourceValue and resolve the reported conversion problem before using ",
                    "the converted bounds."
                )
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
#' informationCumulative <- as.numeric(getFisherInformation(
#'     designPlan,
#'     stage = 1:2
#' ))
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
#' The \code{type} field in every result enables
#' \code{getFutilityBounds()} to detect incompatible use.
#' The complete result can also be piped into \code{getFutilityBounds()}. In
#' that case, its \code{stage} and \code{situations} fields are used to convert every
#' applicable futility bound separately. With the default cumulative type, an
#' omitted target scale means conversion to the effect-estimate scale:
#' \preformatted{
#' designPlan |>
#'     getFisherInformation() |>
#'     getFutilityBounds()
#' }
#' Alternatively, pipe \code{designPlan} directly into
#' \code{getFutilityBounds()}; the latter then calls this function internally
#' with the information type required by the requested target scale.
#' Cumulative information can also be piped into a conversion to the
#' design-specific treatment-effect scale:
#' \preformatted{
#' designPlan |>
#'     getFisherInformation(type = "cumulative") |>
#'     getFutilityBounds(targetScale = "treatmentEffect")
#' }
#' In this case, the information object's type, stages, and originating design
#' plan are validated before the endpoint-specific transformation is applied.
#' For two-group rates, the Farrington--Manning inversion uses the design plan's
#' group sizes and null-restricted rates directly. For Count Data, the design and
#' stages are reused without recalculating Fisher information, while the exact
#' negative-binomial inversion evaluates its variance at each candidate rate
#' ratio from the design-specific sample-size and exposure assumptions.
#'
#' @return
#' A \code{FisherInformation} R6 object. Its public fields are
#' \code{information} (the calculated numeric value, vector, or matrix),
#' \code{type}, \code{stage}, and \code{situations}. The latter contains
#' descriptive labels such as the corresponding alternatives, event
#' probabilities, hazard ratios, or count-data rates where available. The
#' originating design plan is stored privately for subsequent pipe-based
#' conversion with \code{getFutilityBounds()}.
#'
#' Use \code{as.numeric()} to extract a plain numeric vector, \code{as.matrix()}
#' to obtain a consistently arranged stage-by-situation matrix, and
#' \code{as.data.frame()} to obtain a long-format data frame. Unsupported
#' endpoint types are represented by \code{NA_real_} in the
#' \code{information} field.
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

    fisherInformationValues <- .combineFisherInformationStages(informationByStage)
    if (length(stage) > 1L && !is.null(dim(fisherInformationValues))) {
        rownames(fisherInformationValues) <- paste0("stage ", stage)
    }

    nSituations <- .getNumberOfFisherInformationSituations(fisherInformationValues, stage)
    situations <- .getFisherInformationSituationLabels(
        designPlan,
        nSituations
    )
    return(FisherInformation$new(
        information = fisherInformationValues,
        type = type,
        stage = stage,
        situations = situations,
        designPlan = designPlan
    ))
}

.getFutilityBoundsTreatmentEffectScaleRatesTwoGroups <- function(
        designPlan,
        boundary,
        nStages,
        futilityBounds = NULL,
        stages = seq_len(nStages)) {
    design <- designPlan$.design
    maxNumberOfSubjects <- designPlan$maxNumberOfSubjects
    allocationRatioPlanned <- designPlan$allocationRatioPlanned
    nParameters <- length(maxNumberOfSubjects)

    if (length(allocationRatioPlanned) == 1) {
        allocationRatioPlanned <- rep(allocationRatioPlanned, nParameters)
    }

    nStages <- length(stages)
    result <- matrix(NA_real_, nrow = nStages, ncol = nParameters)
    if (nStages == 0 || (is.null(futilityBounds) && !.hasApplicableFutilityBounds(design))) {
        return(result)
    }

    if (is.null(futilityBounds)) {
        futilityBounds <- .getFutilityBounds(design)
        if (length(futilityBounds) == 0) {
            return(result)
        }
        futilityBounds[.getInvalidFutilityBoundsIndices(design)] <- NA_real_
        futilityBounds <- futilityBounds[stages]
    }

    directionUpper <- .getDirectionUpper(designPlan, nParameters)
    method <- ifelse(designPlan$riskRatio, "ratio", "diff")

    for (index in seq_len(nParameters)) {
        n1 <- allocationRatioPlanned[index] *
            design$informationRates[stages] *
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

.getCountDataTreatmentEffectScaleParameters <- function(designPlan, stages) {
    design <- designPlan$.design
    nSituations <- ifelse(designPlan$.isPowerObject(), 1L, length(designPlan$lambda1))
    allocationRatio <- designPlan$allocationRatioPlanned
    followUpTime <- designPlan$followUpTime
    maxNumberOfSubjects <- designPlan$maxNumberOfSubjects
    if (length(allocationRatio) == 1L) {
        allocationRatio <- rep(allocationRatio, nSituations)
    }
    if (length(followUpTime) == 1L) {
        followUpTime <- rep(followUpTime, nSituations)
    }
    if (length(maxNumberOfSubjects) == 1L) {
        maxNumberOfSubjects <- rep(maxNumberOfSubjects, nSituations)
    }

    accrualTime <- designPlan$accrualTime
    if (length(accrualTime) > 1L) {
        accrualTime <- accrualTime[-1L]
    }
    if (all(is.na(followUpTime)) && is.na(designPlan$fixedExposureTime)) {
        followUpTime <- rep(designPlan$studyTime - max(accrualTime), nSituations)
    }

    parameters <- vector("list", nSituations)
    directionUpper <- .getDirectionUpper(designPlan, nSituations)
    for (situationIndex in seq_len(nSituations)) {
        if (!anyNA(designPlan$accrualIntensity)) {
            recruitmentTimes <- .generateRecruitmentTimes(
                allocationRatio[situationIndex],
                accrualTime,
                designPlan$accrualIntensity
            )
            recruit1 <- recruitmentTimes$recruit[recruitmentTimes$treatments == 1L]
            recruit2 <- recruitmentTimes$recruit[recruitmentTimes$treatments == 2L]
        } else if (!anyNA(accrualTime)) {
            recruit1 <- seq(
                0,
                accrualTime,
                length.out = maxNumberOfSubjects[situationIndex] *
                    allocationRatio[situationIndex] / (1 + allocationRatio[situationIndex])
            )
            recruit2 <- seq(
                0,
                accrualTime,
                length.out = maxNumberOfSubjects[situationIndex] / (1 + allocationRatio[situationIndex])
            )
        } else {
            recruit1 <- NA_real_
            recruit2 <- NA_real_
        }

        parameters[[situationIndex]] <- lapply(stages, function(stage) {
            numberOfSubjects <- if (all(is.na(designPlan$numberOfSubjects[, situationIndex]))) {
                maxNumberOfSubjects[situationIndex]
            } else {
                designPlan$numberOfSubjects[stage, situationIndex]
            }
            list(
                informationRate = design$informationRates[stage],
                lambda2 = designPlan$lambda2,
                thetaH0 = designPlan$thetaH0,
                directionUpper = directionUpper[situationIndex],
                allocationRatio = allocationRatio[situationIndex],
                overdispersion = designPlan$overdispersion,
                accrualTime = accrualTime,
                followUpTime = followUpTime[situationIndex],
                fixedExposureTime = designPlan$fixedExposureTime,
                numberOfSubjects = numberOfSubjects,
                recruit1 = recruit1[1:(allocationRatio[situationIndex] * numberOfSubjects /
                    (1 + allocationRatio[situationIndex]))],
                recruit2 = recruit2[1:(numberOfSubjects / (1 + allocationRatio[situationIndex]))]
            )
        })
    }
    return(parameters)
}

.getFutilityBoundsTreatmentEffectScaleCountData <- function(
        designPlan,
        boundary,
        futilityBounds = NULL,
        stages) {
    nStages <- length(stages)
    nSituations <- ifelse(designPlan$.isPowerObject(), 1L, length(designPlan$lambda1))
    result <- matrix(NA_real_, nrow = nStages, ncol = nSituations)
    if (nStages == 0L) {
        return(result)
    }

    # Reuse the values generated by the validated Count Data boundary calculation
    # for the design's own directed bounds.
    if (is.null(futilityBounds) && identical(boundary, "directed") &&
            designPlan$.design$sided == 1L && !is.null(designPlan$futilityBoundsEffectScale)) {
        storedBounds <- matrix(
            designPlan$futilityBoundsEffectScale,
            nrow = max(designPlan$.design$kMax - 1L, 0L)
        )
        return(storedBounds[stages, , drop = FALSE])
    }

    if (is.null(futilityBounds)) {
        futilityBounds <- .getFutilityBounds(designPlan$.design)
        if (length(futilityBounds) == 0L) {
            return(result)
        }
        futilityBounds[.getInvalidFutilityBoundsIndices(designPlan$.design)] <- NA_real_
        futilityBounds <- futilityBounds[stages]
    }
    futilityBounds <- .getFutilityBoundsValuesMatrix(
        futilityBounds,
        nStages = nStages,
        nSituations = nSituations,
        parameterName = "futilityBounds"
    )
    parameters <- .getCountDataTreatmentEffectScaleParameters(designPlan, stages)

    for (situationIndex in seq_len(nSituations)) {
        for (stageIndex in seq_len(nStages)) {
            zValue <- futilityBounds[stageIndex, situationIndex]
            if (is.na(zValue)) {
                next
            }
            args <- parameters[[situationIndex]][[stageIndex]]
            if (identical(boundary, "upper")) {
                args$directionUpper <- TRUE
            } else if (identical(boundary, "lower")) {
                zValue <- -zValue
                args$directionUpper <- TRUE
            }
            args$boundary <- zValue
            result[stageIndex, situationIndex] <- do.call(
                .getEffectScaleBoundaryCountDataTheta,
                args
            )
        }
    }
    result[!is.na(result) & result <= 0] <- NA_real_
    return(result)
}

.getFutilityBoundsTreatmentEffectScale <- function(
        designPlan,
        boundary = c("directed", "upper", "lower"),
        futilityBounds = NULL,
        stages = NULL,
        fisherInformation = NULL) {
    .assertIsTrialDesignPlan(designPlan)
    boundary <- match.arg(boundary)

    design <- designPlan$.design
    if (is.null(stages)) {
        stages <- seq_len(max(design$kMax - 1, 0))
    }
    nStages <- length(stages)
    suppliedInformation <- .getFisherInformationForTreatmentEffectScale(
        fisherInformation,
        designPlan = designPlan,
        stages = stages
    )
    if (.isTrialDesignPlanCountData(designPlan)) {
        expectedSituations <- ifelse(
            designPlan$.isPowerObject(),
            1L,
            length(designPlan$lambda1)
        )
        if (!is.null(suppliedInformation) &&
                ncol(suppliedInformation) != expectedSituations) {
            stopIllegalArgument(
                .pQuote("fisherInformation"), " must contain information for ",
                expectedSituations, " planning situation(s)",
                functionName = ".getFutilityBoundsTreatmentEffectScale",
                parameter = "fisherInformation",
                value = fisherInformation$information
            )
        }
        return(.getFutilityBoundsTreatmentEffectScaleCountData(
            designPlan,
            boundary,
            futilityBounds = futilityBounds,
            stages = stages
        ))
    }
    if (.isTrialDesignPlanRates(designPlan) && designPlan$groups == 2) {
        return(.getFutilityBoundsTreatmentEffectScaleRatesTwoGroups(
            designPlan,
            boundary,
            nStages,
            futilityBounds = futilityBounds,
            stages = stages
        ))
    }

    informationStage1 <- if (is.null(suppliedInformation)) {
        as.numeric(getFisherInformation(designPlan, stage = 1))
    } else {
        as.numeric(suppliedInformation[1L, ])
    }
    nParameters <- max(length(informationStage1), 1)

    if (nStages == 0) {
        return(matrix(numeric(0), nrow = 0, ncol = nParameters))
    }

    result <- matrix(NA_real_, nrow = nStages, ncol = nParameters)
    if ((is.null(futilityBounds) && !.hasApplicableFutilityBounds(design)) || all(is.na(informationStage1))) {
        return(result)
    }

    if (is.null(futilityBounds)) {
        futilityBounds <- .getFutilityBounds(design)
        if (length(futilityBounds) == 0) {
            return(result)
        }
        futilityBounds[.getInvalidFutilityBoundsIndices(design)] <- NA_real_
        futilityBounds <- futilityBounds[stages]
    }

    informationRates <- design$informationRates[stages]
    if (!is.null(suppliedInformation)) {
        # Reuse the validated cumulative values for the requested stages.
        stageInformation <- suppliedInformation
    } else if (grepl("CountData", .getClassName(designPlan))) {
        stageInformation <- NULL
        for (stage in stages) {
            stageInformation <- rbind(
                stageInformation,
                as.numeric(getFisherInformation(designPlan, stage = stage))
            )
        }
    } else {
        stageInformation <- (informationRates / design$informationRates[1]) %*% t(informationStage1)
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

.getFutilityBoundsTreatmentEffectConversionDescription <- function(
        designPlan,
        direction = c("toTreatmentEffect", "fromTreatmentEffect")) {
    direction <- match.arg(direction)
    if (.isTrialDesignPlanRates(designPlan) && designPlan$groups == 2) {
        return(ifelse(
            direction == "toTreatmentEffect",
            "design-specific Farrington-Manning inversion",
            "design-specific Farrington-Manning standardization"
        ))
    }
    if (.isTrialDesignPlanMeans(designPlan) && !designPlan$normalApproximation) {
        return(ifelse(
            direction == "toTreatmentEffect",
            "design-specific Student t transformation",
            "inverse design-specific Student t transformation"
        ))
    }
    if (.isTrialDesignPlanCountData(designPlan)) {
        return(ifelse(
            direction == "toTreatmentEffect",
            "design-specific negative-binomial inversion",
            "design-specific negative-binomial standardization"
        ))
    }
    return("design-specific endpoint transformation")
}

.getFutilityBoundsTreatmentEffectMatrix <- function(
        designPlan,
        stages,
        zValues = NULL,
        fisherInformation = NULL) {
    if (is.null(zValues)) {
        return(.getFutilityBoundsTreatmentEffectScale(
            designPlan,
            stages = stages,
            fisherInformation = fisherInformation
        ))
    }

    reference <- .getFutilityBoundsTreatmentEffectScale(designPlan)
    nSituations <- ncol(reference)
    nStages <- length(stages)
    if (is.null(dim(zValues))) {
        if (length(zValues) == 1L) {
            zValues <- matrix(zValues, nrow = nStages, ncol = nSituations)
        } else if (length(zValues) == nStages) {
            zValues <- matrix(zValues, nrow = nStages, ncol = nSituations)
        } else if (nStages == 1L && length(zValues) == nSituations) {
            zValues <- matrix(zValues, nrow = 1L)
        } else if (length(zValues) == nStages * nSituations) {
            zValues <- matrix(zValues, nrow = nStages, ncol = nSituations)
        } else {
            stopIllegalArgument(
                .pQuote("sourceValue"), " cannot be aligned with ", nStages,
                " stage(s) and ", nSituations, " planning situation(s)",
                functionName = ".getFutilityBoundsTreatmentEffectMatrix",
                parameter = "sourceValue",
                value = zValues
            )
        }
    }
    if (nrow(zValues) != nStages || ncol(zValues) != nSituations) {
        stopIllegalArgument(
            .pQuote("sourceValue"), " must have ", nStages, " row(s) and ",
            nSituations, " column(s)",
            functionName = ".getFutilityBoundsTreatmentEffectMatrix",
            parameter = "sourceValue",
            value = zValues
        )
    }

    result <- matrix(NA_real_, nrow = nStages, ncol = nSituations)
    for (situationIndex in seq_len(nSituations)) {
        converted <- .getFutilityBoundsTreatmentEffectScale(
            designPlan,
            futilityBounds = zValues[, situationIndex],
            stages = stages
        )
        result[, situationIndex] <- converted[, situationIndex]
    }
    return(result)
}

.getFutilityBoundsTreatmentEffectScaleToZValue <- function(
        designPlan,
        treatmentEffects,
        stages) {
    design <- designPlan$.design
    reference <- .getFutilityBoundsTreatmentEffectScale(designPlan)
    nSituations <- ncol(reference)
    nStages <- length(stages)
    treatmentEffects <- .getFutilityBoundsValuesMatrix(
        treatmentEffects,
        nStages = nStages,
        nSituations = nSituations
    )
    directionUpper <- .getDirectionUpper(designPlan, nSituations)
    directionSign <- ifelse(directionUpper, 1, -1)

    if (.isTrialDesignPlanRates(designPlan) && designPlan$groups == 2) {
        allocationRatio <- designPlan$allocationRatioPlanned
        if (length(allocationRatio) == 1L) {
            allocationRatio <- rep(allocationRatio, nSituations)
        }
        result <- matrix(NA_real_, nrow = nStages, ncol = nSituations)
        method <- ifelse(designPlan$riskRatio, "ratio", "diff")
        for (situationIndex in seq_len(nSituations)) {
            n1 <- allocationRatio[situationIndex] *
                design$informationRates[stages] *
                designPlan$maxNumberOfSubjects[situationIndex] /
                (1 + allocationRatio[situationIndex])
            n2 <- n1 / allocationRatio[situationIndex]
            pi2 <- designPlan$pi2[min(situationIndex, length(designPlan$pi2))]
            for (stageIndex in seq_len(nStages)) {
                pi1 <- if (designPlan$riskRatio) {
                    treatmentEffects[stageIndex, situationIndex] * pi2
                } else {
                    treatmentEffects[stageIndex, situationIndex] + pi2
                }
                fm <- .getFarringtonManningValues(
                    rate1 = pi1,
                    rate2 = pi2,
                    theta = designPlan$thetaH0,
                    allocation = allocationRatio[situationIndex],
                    method = method
                )
                numerator <- if (designPlan$riskRatio) {
                    pi1 - designPlan$thetaH0 * pi2
                } else {
                    pi1 - pi2 - designPlan$thetaH0
                }
                standardError <- sqrt(
                    fm$ml1 * (1 - fm$ml1) / n1[stageIndex] +
                        ifelse(designPlan$riskRatio, designPlan$thetaH0^2, 1) *
                            fm$ml2 * (1 - fm$ml2) / n2[stageIndex]
                )
                result[stageIndex, situationIndex] <-
                    directionSign[situationIndex] * numerator / standardError
            }
        }
        return(result)
    }

    if (.isTrialDesignPlanCountData(designPlan)) {
        parameters <- .getCountDataTreatmentEffectScaleParameters(designPlan, stages)
        result <- matrix(NA_real_, nrow = nStages, ncol = nSituations)
        for (situationIndex in seq_len(nSituations)) {
            for (stageIndex in seq_len(nStages)) {
                treatmentEffect <- treatmentEffects[stageIndex, situationIndex]
                if (is.na(treatmentEffect)) {
                    next
                }
                args <- parameters[[situationIndex]][[stageIndex]]
                vHat <- .getVarianceEstimate(
                    lambda1 = treatmentEffect * args$lambda2,
                    lambda2 = args$lambda2,
                    allocation = args$allocationRatio,
                    overdispersion = args$overdispersion,
                    accrualTime = args$accrualTime,
                    followUpTime = args$followUpTime,
                    fixedExposureTime = args$fixedExposureTime,
                    recruit1 = args$recruit1,
                    recruit2 = args$recruit2
                )
                result[stageIndex, situationIndex] <-
                    (args$directionUpper * 2 - 1) *
                    (log(treatmentEffect) - log(args$thetaH0)) /
                    sqrt(vHat) * sqrt(args$informationRate * args$numberOfSubjects)
            }
        }
        return(result)
    }

    informationStage1 <- as.numeric(getFisherInformation(designPlan, stage = 1L))
    informationRates <- design$informationRates[stages]
    if (grepl("CountData", .getClassName(designPlan))) {
        stageInformation <- do.call(rbind, lapply(stages, function(stage) {
            as.numeric(getFisherInformation(designPlan, stage = stage))
        }))
    } else {
        stageInformation <- (informationRates / design$informationRates[1]) %*% t(informationStage1)
    }

    centeredEffect <- treatmentEffects - designPlan$thetaH0
    if (.isTrialDesignPlanSurvival(designPlan)) {
        centeredEffect <- log(treatmentEffects / designPlan$thetaH0)
    }
    standardizedBounds <- sweep(centeredEffect, 2L, directionSign, FUN = "/") * sqrt(stageInformation)
    if (.isTrialDesignPlanMeans(designPlan) && !designPlan$normalApproximation) {
        degreesOfFreedom <- pmax(
            informationRates %*% t(designPlan$maxNumberOfSubjects) - designPlan$groups,
            1e-04
        )
        standardizedBounds <- stats::qnorm(stats::pt(standardizedBounds, degreesOfFreedom))
    }
    return(standardizedBounds)
}

.getFutilityBoundsValuesMatrix <- function(values, nStages, nSituations, parameterName = "sourceValue") {
    if (!is.null(dim(values))) {
        result <- matrix(as.numeric(values), nrow = nrow(values), ncol = ncol(values))
    } else if (length(values) == 1L) {
        result <- matrix(values, nrow = nStages, ncol = nSituations)
    } else if (length(values) == nStages) {
        result <- matrix(values, nrow = nStages, ncol = nSituations)
    } else if (nStages == 1L && length(values) == nSituations) {
        result <- matrix(values, nrow = 1L)
    } else if (length(values) == nStages * nSituations) {
        result <- matrix(values, nrow = nStages, ncol = nSituations)
    } else {
        stopIllegalArgument(
            .pQuote(parameterName), " cannot be aligned with ", nStages,
            " stage(s) and ", nSituations, " planning situation(s)",
            functionName = ".getFutilityBoundsValuesMatrix",
            parameter = parameterName,
            value = values
        )
    }
    if (nrow(result) != nStages || ncol(result) != nSituations) {
        stopIllegalArgument(
            .pQuote(parameterName), " must have ", nStages, " row(s) and ",
            nSituations, " column(s)",
            functionName = ".getFutilityBoundsValuesMatrix",
            parameter = parameterName,
            value = values
        )
    }
    return(result)
}

.convertFutilityBoundsScaleWithDesignPlan <- function(
        designPlan,
        values,
        stages,
        sourceScale,
        targetScale,
        theta,
        naAllowed) {
    nSituations <- ncol(.getFutilityBoundsTreatmentEffectScale(designPlan))
    values <- .getFutilityBoundsValuesMatrix(values, length(stages), nSituations)
    result <- matrix(NA_real_, nrow = length(stages), ncol = nSituations)
    informationType <- .getRequiredFutilityBoundsInformationType(sourceScale, targetScale)
    information <- NULL
    if (!is.null(informationType)) {
        informationStages <- if (informationType == "stageWise") {
            seq_len(designPlan$.design$kMax)
        } else {
            stages
        }
        information <- as.matrix(getFisherInformation(
            designPlan,
            stage = informationStages,
            type = informationType
        ))
    }
    directions <- .getDirectionUpper(designPlan, nSituations)
    calculationDesign <- if (
        sourceScale %in% c("conditionalPower", "condPowerAtObserved", "predictivePower", "reverseCondPower") ||
            targetScale %in% c("conditionalPower", "condPowerAtObserved", "predictivePower", "reverseCondPower")
    ) {
        designPlan$.design
    } else {
        NULL
    }

    for (situationIndex in seq_len(nSituations)) {
        for (stageIndex in seq_along(stages)) {
            informationValue <- NA_real_
            if (!is.null(information)) {
                if (informationType == "stageWise") {
                    informationValue <- information[, situationIndex]
                    if (sourceScale == "conditionalPower" || targetScale == "conditionalPower") {
                        informationValue[1L] <- NA_real_
                    }
                } else {
                    informationValue <- information[stageIndex, situationIndex]
                }
                attr(informationValue, "type") <- informationType
            }
            result[stageIndex, situationIndex] <- as.numeric(getFutilityBounds(
                sourceValue = values[stageIndex, situationIndex],
                sourceScale = sourceScale,
                targetScale = targetScale,
                design = calculationDesign,
                directionUpper = directions[situationIndex],
                theta = theta,
                information = informationValue,
                naAllowed = naAllowed
            ))
        }
    }
    return(result)
}
