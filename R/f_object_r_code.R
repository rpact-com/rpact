## |
## |  *Object R Code*
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
## |  File version: $Revision: 7126 $
## |  Last changed: $Date: 2023-06-23 14:26:39 +0200 (Fr, 23 Jun 2023) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_core_constants.R
#' @include f_logger.R
NULL

.getAgrumentSpecificFormattedValue <- function(value) {
    if (is.character(value)) {
        value <- paste0("\"", value, "\"")
        value[value == "\"NA\""] <- NA_character_
        value[is.na(value)] <- "\"NA\""
        return(value)
    } else if (is.integer(value)) {
        value[is.na(value)] <- "NA_integer_"
    } else if (is.numeric(value)) {
        value[!is.na(value)] <- format(value[!is.na(value)], digits = 8)
        value[is.na(value)] <- "NA_real_"
    } else if (is.complex(value)) {
        value[is.na(value)] <- "NA_complex_"
    }

    return(value)
}

.getArgumentValueRCode <- function(x, name) {
    if (is.null(x)) {
        return("NULL")
    }

    if (length(x) == 0) {
        if (is.list(x)) {
            return("list()")
        } else if (is.character(x)) {
            return("character(0)")
        } else if (is.integer(x)) {
            return("integer(0)")
        } else if (is.numeric(x)) {
            return("numeric(0)")
        } else if (is.complex(x)) {
            return("complex(0)")
        }
    }

    if (is.function(x) || isS4(x)) {
        return("NULL")
    }

    if (length(x) == 1 && is.na(x)) {
        if (is.character(x)) {
            return("NA_character_")
        } else if (is.integer(x)) {
            return("NA_integer_")
        } else if (is.numeric(x)) {
            return("NA_real_")
        } else if (is.complex(x)) {
            return("NA_complex_")
        }
        return("NA")
    }

    if (is.list(x)) {
        params <- c()
        for (paramName in names(x)) {
            paramValue <- x[[paramName]]
            if (name != "effectList" || paramName != "piControls" || (!is.null(paramValue) && length(paramValue) > 0)) {
                params <- c(params, paste0(paramName, " = ", .getArgumentValueRCode(x = paramValue, name = paramName)))
            }
        }
        return(paste0("list(", paste0(params, collapse = ", "), ")"))
    }

    leadingZeroAdded <- FALSE
    expectedResult <- ""
    if (name == "accrualTime" && length(x) > 0 && !is.na(x[1]) && x[1] != 0) {
        expectedResult <- "0"
        leadingZeroAdded <- TRUE
    } else if (name == "followUpTime" && length(x) == 1 && !is.na(x)) {
        x <- round(x, 3)
    } else if (name == "maxNumberOfSubjects" && length(x) == 1 && !is.na(x)) {
        x <- floor(x * 100) / 100
    } else if (is.numeric(x) && !is.matrix(x)) {
        seqTest <- .reconstructSequenceCommand(x)
        if (!is.null(seqTest) && length(seqTest) == 1 &&
                !is.na(seqTest) && grepl("^seq", seqTest)) {
            return(seqTest)
        }
    }

    if (is.matrix(x) && name == "effectMatrix") {
        x <- t(x)
    }

    for (i in 1:length(x)) {
        if (nchar(expectedResult) > 0) {
            expectedResult <- paste0(expectedResult, ", ")
        }
        expectedResult <- paste0(expectedResult, .getAgrumentSpecificFormattedValue(x[i]))
    }
    if (leadingZeroAdded || length(x) > 1) {
        expectedResult <- paste0("c(", expectedResult, ")")
    }
    if (is.matrix(x) && grepl("effectMatrix|effects|piTreatments|hazardRatios", name)) {
        expectedResult <- paste0("matrix(", expectedResult, ", ncol = ", ncol(x), ")")
    }

    return(expectedResult)
}

#' @rdname getObjectRCode
#' @export
rcmd <- function(obj, ...,
        leadingArguments = NULL,
        includeDefaultParameters = FALSE,
        stringWrapParagraphWidth = 90,
        prefix = "",
        postfix = "",
        stringWrapPrefix = "",
        newArgumentValues = list()) {
    getObjectRCode(
        obj = obj,
        leadingArguments = leadingArguments,
        includeDefaultParameters = includeDefaultParameters,
        stringWrapParagraphWidth = stringWrapParagraphWidth,
        prefix = prefix,
        postfix = postfix,
        stringWrapPrefix = stringWrapPrefix,
        newArgumentValues = newArgumentValues
    )
}

#'
#' @title
#' Get Object R Code
#'
#' @description
#' Returns the R source command of a result object.
#'
#' @param obj The result object.
#' @param leadingArguments A character vector with arguments that shall be inserted at the beginning of the function command,
#'        e.g., \code{design = x}. Be careful with this option because the created R command may no longer be valid if used.
#' @param includeDefaultParameters If \code{TRUE}, default parameters will be included in all \code{rpact} commands;
#'        default is \code{FALSE}.
#' @param stringWrapParagraphWidth An integer value defining the number of characters after which a line break shall be inserted;
#'        set to \code{NULL} to insert no line breaks.
#' @param prefix A character string that shall be added to the beginning of the R command.
#' @param postfix A character string that shall be added to the end of the R command.
#' @param stringWrapPrefix A prefix character string that shall be added to each new line, typically some spaces.
#' @param newArgumentValues A named list with arguments that shall be renewed in the R command, e.g.,
#'        \code{newArgumentValues = list(informationRates = c(0.5, 1))}.
#' @param tolerance The tolerance for defining a value as default.
#' @param pipeOperator The pipe operator to use in the R code, default is "none".
#' @param output The output format, default is a character "vector".
#' @param explicitPrint Show an explicit \code{print} command, default is \code{FALSE}.
#' @inheritParams param_three_dots
#'
#' @details
#' \code{\link[=getObjectRCode]{getObjectRCode()}} (short: \code{\link[=rcmd]{rcmd()}}) recreates
#' the R commands that result in the specified object \code{obj}.
#' \code{obj} must be an instance of class \code{ParameterSet}.
#'
#' @return A \code{\link[base]{character}} value or vector will be returned.
#'
#' @export
#'
getObjectRCode <- function(obj, ...,
        leadingArguments = NULL,
        includeDefaultParameters = FALSE,
        stringWrapParagraphWidth = 90,
        prefix = "",
        postfix = "",
        stringWrapPrefix = "",
        newArgumentValues = list(),
        tolerance = 1e-07,
        pipeOperator = c("auto", "none", "magrittr", "R"),
        output = c("vector", "cat", "test", "markdown", "internal"),
        explicitPrint = FALSE) {
    functionName <- deparse(substitute(obj))
    functionName <- sub("\\(.*\\)$", "", functionName)

    output <- match.arg(output)

    .assertIsSingleLogical(includeDefaultParameters, "includeDefaultParameters")
    .assertIsSingleLogical(explicitPrint, "explicitPrint")
    if (!is.null(stringWrapParagraphWidth)) {
        .assertIsSingleInteger(stringWrapParagraphWidth, "stringWrapParagraphWidth", validateType = FALSE)
        .assertIsInClosedInterval(stringWrapParagraphWidth, "stringWrapParagraphWidth", lower = 10, upper = 50000)
    }
    .assertIsSingleCharacter(prefix, "prefix")
    .assertIsCharacter(postfix, "postfix")
    .assertIsSingleCharacter(stringWrapPrefix, "stringWrapPrefix")
    .assertIsSingleNumber(tolerance, "tolerance")
    .assertIsInClosedInterval(tolerance, "tolerance", lower = 1e-15, upper = 1e-03)

    if (output == "test") {
        stringWrapParagraphWidth <- NULL
    } else if (output %in% c("cat", "markdown")) {
        if (stringWrapPrefix == "") {
            stringWrapPrefix <- "    "
        }
    }

    pipeOperator <- match.arg(pipeOperator)
    if (pipeOperator == "auto") {
        rVersion <- R.Version()
        if (rVersion$major >= 4 && rVersion$minor >= 1) {
            pipeOperator <- "R"
        } else if (.isPackageInstalled("magrittr")) {
            pipeOperator <- "magrittr"
        } else {
            pipeOperator <- "none"
        }
    }
    pipeOperatorPostfix <- ""
    if (pipeOperator == "magrittr") {
        pipeOperatorPostfix <- " %>% "
    } else if (pipeOperator == "R") {
        pipeOperatorPostfix <- " |> "
    }

    if (!is.null(obj) && is.function(obj)) {
        lines <- .getFunctionAsString(obj,
            stringWrapPrefix = stringWrapPrefix,
            stringWrapParagraphWidth = stringWrapParagraphWidth
        )
        if (length(lines) == 0) {
            return("")
        }

        lines[1] <- paste0(prefix, lines[1])
        if (any(postfix != "")) {
            if (grepl("(\\|>)|(%>%)", postfix[1])) {
                lines[length(lines)] <- paste0(lines[length(lines)], postfix[1])
                if (length(postfix) > 1) {
                    lines <- c(lines, paste0(postfix[2:length(postfix)], collapse = ""))
                }
            } else {
                lines <- c(lines, paste0(postfix, collapse = ""))
            }
        }
        return(lines)
    }

    .assertIsParameterSetClass(obj, "ParameterSet")

    if (!is.list(newArgumentValues)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'newArgumentValues' must be a named list ",
            "(is ", .getClassName(newArgumentValues), ")"
        )
    }

    precondition <- character(0)
    if (is.null(leadingArguments)) {
        leadingArguments <- character(0)
    }
    if (!inherits(obj, "ConditionalPowerResults") &&
            !is.null(obj[[".design"]]) &&
            (is.null(leadingArguments) || !any(grepl("design", leadingArguments)))) {
        preconditionDesign <- getObjectRCode(obj$.design,
            prefix = ifelse(pipeOperator == "none", "design <- ", ""),
            postfix = pipeOperatorPostfix,
            includeDefaultParameters = includeDefaultParameters,
            stringWrapParagraphWidth = stringWrapParagraphWidth,
            stringWrapPrefix = stringWrapPrefix,
            newArgumentValues = newArgumentValues,
            pipeOperator = pipeOperator,
            output = "internal"
        )
        if (!grepl("getDesign(GroupSequential|InverseNormal)\\(kMax = 1\\)", paste0(preconditionDesign, collapse = " "))) {
            precondition <- c(precondition, preconditionDesign)
            if (pipeOperator == "none") {
                leadingArguments <- c(leadingArguments, "design = design")
            }
        }
    }
    if (!is.null(obj[[".dataInput"]]) && (is.null(leadingArguments) || !any(grepl("data", leadingArguments)))) {
        precondition <- c(precondition, getObjectRCode(obj$.dataInput,
            prefix = ifelse(pipeOperator == "none", "data <- ", ""),
            postfix = pipeOperatorPostfix,
            includeDefaultParameters = includeDefaultParameters,
            stringWrapParagraphWidth = stringWrapParagraphWidth,
            stringWrapPrefix = stringWrapPrefix,
            newArgumentValues = newArgumentValues,
            pipeOperator = pipeOperator,
            output = "internal"
        ))
        if (pipeOperator == "none") {
            leadingArguments <- c(leadingArguments, "dataInput = data")
        }
    }
    if (!is.null(obj[["calcSubjectsFunction"]]) &&
            (is.null(leadingArguments) || !any(grepl("calcSubjectsFunction", leadingArguments))) &&
            obj$.getParameterType("calcSubjectsFunction") == C_PARAM_USER_DEFINED) {
        precond <- getObjectRCode(obj$calcSubjectsFunction,
            prefix = "calcSubjectsFunction <- ",
            includeDefaultParameters = includeDefaultParameters,
            stringWrapParagraphWidth = stringWrapParagraphWidth,
            stringWrapPrefix = stringWrapPrefix,
            newArgumentValues = newArgumentValues,
            pipeOperator = pipeOperator,
            output = "internal"
        )
        if (pipeOperator == "none") {
            precondition <- c(precondition, precond)
        } else {
            precondition <- c(precond, precondition)
        }
    }
    if (!is.null(obj[["calcEventsFunction"]]) &&
            (is.null(leadingArguments) || !any(grepl("calcEventsFunction", leadingArguments))) &&
            obj$.getParameterType("calcEventsFunction") == C_PARAM_USER_DEFINED) {
        precond <- getObjectRCode(obj$calcEventsFunction,
            prefix = "calcEventsFunction <- ",
            includeDefaultParameters = includeDefaultParameters,
            stringWrapParagraphWidth = stringWrapParagraphWidth,
            stringWrapPrefix = stringWrapPrefix,
            newArgumentValues = newArgumentValues,
            pipeOperator = pipeOperator,
            output = "internal"
        )
        if (pipeOperator == "none") {
            precondition <- c(precondition, precond)
        } else {
            precondition <- c(precond, precondition)
        }
    }
    if (!is.null(obj[["selectArmsFunction"]]) &&
            (is.null(leadingArguments) || !any(grepl("selectArmsFunction", leadingArguments))) &&
            !is.null(obj[["typeOfSelection"]]) && obj$typeOfSelection == "userDefined") {
        precond <- getObjectRCode(obj$selectArmsFunction,
            prefix = "selectArmsFunction <- ",
            includeDefaultParameters = includeDefaultParameters,
            stringWrapParagraphWidth = stringWrapParagraphWidth,
            stringWrapPrefix = stringWrapPrefix,
            newArgumentValues = newArgumentValues,
            pipeOperator = pipeOperator,
            output = "internal"
        )
        if (pipeOperator == "none") {
            precondition <- c(precondition, precond)
        } else {
            precondition <- c(precond, precondition)
        }
        leadingArguments <- c(leadingArguments, "selectArmsFunction = selectArmsFunction")
    }
    if (inherits(obj, "ConditionalPowerResults") &&
            !is.null(obj[[".stageResults"]]) &&
            (is.null(leadingArguments) || !any(grepl("stageResults", leadingArguments)))) {
        precond <- getObjectRCode(obj$.stageResults,
            prefix = ifelse(pipeOperator == "none", "stageResults <- ", ""),
            postfix = pipeOperatorPostfix,
            includeDefaultParameters = includeDefaultParameters,
            stringWrapParagraphWidth = stringWrapParagraphWidth,
            stringWrapPrefix = stringWrapPrefix,
            newArgumentValues = newArgumentValues,
            pipeOperator = pipeOperator,
            output = "internal"
        )
        if (pipeOperator == "none") {
            precondition <- c(precondition, precond)
        } else {
            precondition <- c(precond, precondition)
        }
        leadingArguments <- c(leadingArguments, "stageResults = stageResults")
    }

    if (grepl("SimulationResultsEnrichment(Means|Rates|Survival)", .getClassName(obj))) {
        precond <- paste0(
            "effectList <- ",
            .getArgumentValueRCode(obj$effectList, "effectList")
        )
        if (pipeOperator == "none") {
            precondition <- c(precondition, precond)
        } else {
            precondition <- c(precond, precondition)
        }
    }

    if ("TrialDesignPlanMeans" == .getClassName(obj)) {
        if (obj$.isSampleSizeObject()) {
            functionName <- "getSampleSizeMeans"
        } else {
            functionName <- "getPowerMeans"
        }
    } else if ("TrialDesignPlanRates" == .getClassName(obj)) {
        if (obj$.isSampleSizeObject()) {
            functionName <- "getSampleSizeRates"
        } else {
            functionName <- "getPowerRates"
        }
    } else if ("TrialDesignPlanSurvival" == .getClassName(obj)) {
        if (obj$.isSampleSizeObject()) {
            functionName <- "getSampleSizeSurvival"
        } else {
            functionName <- "getPowerSurvival"
        }
    } else if (inherits(obj, "TrialDesign")) {
        functionName <- paste0("get", sub("^Trial", "", .getClassName(obj)))
    } else if (inherits(obj, "Dataset")) {
        functionName <- "getDataset"
    } else if (inherits(obj, "AnalysisResults")) {
        functionName <- "getAnalysisResults"
    } else if ("TrialDesignSet" == .getClassName(obj)) {
        functionName <- "getDesignSet"
    } else if ("TrialDesignCharacteristics" == .getClassName(obj)) {
        functionName <- "getDesignCharacteristics"
    } else if (inherits(obj, "SimulationResultsMeans")) {
        functionName <- "getSimulationMeans"
    } else if (inherits(obj, "SimulationResultsRates")) {
        functionName <- "getSimulationRates"
    } else if (inherits(obj, "SimulationResultsSurvival")) {
        functionName <- "getSimulationSurvival"
    } else if (inherits(obj, "SimulationResultsMultiArmMeans")) {
        functionName <- "getSimulationMultiArmMeans"
    } else if (inherits(obj, "SimulationResultsMultiArmRates")) {
        functionName <- "getSimulationMultiArmRates"
    } else if (inherits(obj, "SimulationResultsMultiArmSurvival")) {
        functionName <- "getSimulationMultiArmSurvival"
    } else if (inherits(obj, "SimulationResultsEnrichmentMeans")) {
        functionName <- "getSimulationEnrichmentMeans"
    } else if (inherits(obj, "SimulationResultsEnrichmentRates")) {
        functionName <- "getSimulationEnrichmentRates"
    } else if (inherits(obj, "SimulationResultsEnrichmentSurvival")) {
        functionName <- "getSimulationEnrichmentSurvival"
    } else if (inherits(obj, "PiecewiseSurvivalTime")) {
        functionName <- "getPiecewiseSurvivalTime"
    } else if (inherits(obj, "AccrualTime")) {
        functionName <- "getAccrualTime"
    } else if (inherits(obj, "StageResults")) {
        functionName <- "getStageResults"
    } else if (inherits(obj, "ConditionalPowerResults")) {
        functionName <- "getConditionalPower"
    } else if (inherits(obj, "PowerAndAverageSampleNumberResult")) {
        functionName <- "getPowerAndAverageSampleNumber"
    } else if (inherits(obj, "EventProbabilities")) {
        functionName <- "getEventProbabilities"
    } else if (inherits(obj, "NumberOfSubjects")) {
        functionName <- "getNumberOfSubjects"
    } else if (inherits(obj, "SummaryFactory") || "SummaryFactory" == .getClassName(obj)) {
        return(getObjectRCode(obj$object,
            prefix = ifelse(pipeOperator == "none", "summary(", ""),
            postfix = {
                if (pipeOperator == "none") ")" else c(pipeOperatorPostfix, "summary()")
            },
            includeDefaultParameters = includeDefaultParameters,
            stringWrapParagraphWidth = stringWrapParagraphWidth,
            stringWrapPrefix = stringWrapPrefix,
            newArgumentValues = newArgumentValues,
            pipeOperator = pipeOperator,
            output = output,
            explicitPrint = explicitPrint
        ))
    } else {
        stop("Runtime issue: function 'getObjectRCode' is not implemented for class ", .getClassName(obj))
    }

    objNames <- names(obj)

    objNames <- objNames[objNames != "effectList"]

    if (inherits(obj, "ParameterSet")) {
        if (includeDefaultParameters) {
            objNames <- obj$.getInputParameters()
        } else {
            objNames <- obj$.getUserDefinedParameters()
        }
        objNames <- objNames[objNames != "stages"]
    }

    if (inherits(obj, "TrialDesign") && !inherits(obj, "TrialDesignConditionalDunnett") &&
            !("informationRates" %in% objNames) && !("kMax" %in% objNames) && obj$kMax != 3) {
        objNames <- c("kMax", objNames)
    }

    thetaH0 <- NA_real_
    if (inherits(obj, "SimulationResultsSurvival") &&
            obj$.getParameterType("thetaH1") == "g") {
        objNames <- c(objNames, "thetaH1")
        thetaH0 <- obj[["thetaH0"]]
    }

    if (inherits(obj, "SimulationResultsSurvival")) {
        objNames <- objNames[objNames != "allocationRatioPlanned"] # allocation1 and allocation2 are used instead
    }

    if (inherits(obj, "AnalysisResults") && grepl("Fisher", .getClassName(obj))) {
        if (!is.null(obj[["seed"]]) && length(obj$seed) == 1 && !is.na(obj$seed)) {
            if (!("iterations" %in% objNames)) {
                objNames <- c(objNames, "iterations")
            }
            if (!("seed" %in% objNames)) {
                objNames <- c(objNames, "seed")
            }
        } else if (!is.null(obj[[".conditionalPowerResults"]]) &&
                !is.null(obj$.conditionalPowerResults[["seed"]]) &&
                length(obj$.conditionalPowerResults$seed) == 1 &&
                !is.na(obj$.conditionalPowerResults$seed)) {
            if (!("iterations" %in% objNames)) {
                objNames <- c(
                    objNames,
                    ".conditionalPowerResults$iterations"
                )
            }
            if (!("seed" %in% objNames)) {
                objNames <- c(
                    objNames,
                    ".conditionalPowerResults$seed"
                )
            }
        }
    }

    if (!("accrualIntensity" %in% objNames) && !is.null(obj[[".accrualTime"]]) &&
            !obj$.accrualTime$absoluteAccrualIntensityEnabled) {
        objNames <- c(objNames, "accrualIntensity")
    }

    newArgumentValueNames <- character(0)
    if (length(newArgumentValues) > 0) {
        newArgumentValueNames <- names(newArgumentValues)
        illegalArgumentValueNames <- newArgumentValueNames[which(!(newArgumentValueNames %in% names(obj)))]
        if (length(illegalArgumentValueNames) > 0) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'",
                illegalArgumentValueNames, "' is not a valid ", functionName, "() argument"
            )
        }

        defaultParams <- newArgumentValueNames[!(newArgumentValueNames %in% objNames)]
        objNames <- c(objNames, defaultParams)
    }

    if (inherits(obj, "TrialDesign") && "informationRates" %in% objNames &&
            !("informationRates" %in% newArgumentValueNames)) {
        informationRates <- obj[["informationRates"]]
        if (!is.null(informationRates) && length(informationRates) > 0) {
            kMax <- obj[["kMax"]]
            if (isTRUE(all.equal(
                    target = .getInformationRatesDefault(kMax),
                    current = informationRates, tolerance = tolerance
                ))) {
                objNames <- objNames[objNames != "informationRates"]
                if (!("kMax" %in% objNames) && kMax != 3) {
                    objNames <- c("kMax", objNames)
                }
            }
        }
    }

    # TODO implement PerformanceScore

    if (inherits(obj, "Dataset")) {
        lines <- .getDatasetArgumentsRCodeLines(obj, complete = FALSE, digits = NA_integer_)
        argumentsRCode <- paste0(lines, collapse = ", ")
    } else {
        argumentsRCode <- ""
        arguments <- c()
        if (length(objNames) > 0) {
            for (name in objNames) {
                if (grepl("^\\.conditionalPowerResults\\$", name)) {
                    name <- sub("^\\.conditionalPowerResults\\$", "", name)
                    value <- obj$.conditionalPowerResults[[name]]
                } else {
                    value <- obj[[name]]
                }

                if (name == "accrualTime" && inherits(obj, "AccrualTime") &&
                        !isTRUE(obj$endOfAccrualIsUserDefined) &&
                        isTRUE(length(obj$accrualIntensity) < length(value))) {
                    value <- value[1:(length(value) - 1)]
                }

                if (name == "accrualIntensityRelative") {
                    name <- "accrualIntensity"
                }
                if (name == "accrualIntensity" && !is.null(obj[[".accrualTime"]]) &&
                        !obj$.accrualTime$absoluteAccrualIntensityEnabled) {
                    value <- obj$.accrualTime$accrualIntensityRelative
                }

                originalValue <- value
                newValue <- newArgumentValues[[name]]
                if (!is.null(newValue)) {
                    originalValue <- newValue
                }

                value <- .getArgumentValueRCode(originalValue, name)

                if (name == "allocationRatioPlanned") {
                    optimumAllocationRatio <- obj[["optimumAllocationRatio"]]
                    if (!is.null(optimumAllocationRatio) && isTRUE(optimumAllocationRatio)) {
                        value <- 0
                    } else if (inherits(obj, "ParameterSet")) {
                        if (obj$.getParameterType("allocationRatioPlanned") == "g") {
                            value <- 0
                        }
                    }
                } else if (name == "optimumAllocationRatio") {
                    name <- "allocationRatioPlanned"
                    value <- 0
                } else if (name == "maxNumberOfSubjects") {
                    value <- .getArgumentValueRCode(originalValue[1], name)
                } else if (name == "thetaH1" && length(thetaH0) == 1 && !is.na(thetaH0) && value != 1) {
                    value <- .getArgumentValueRCode(originalValue * thetaH0, name)
                } else if (name == "nPlanned") {
                    if (!all(is.na(originalValue))) {
                        value <- .getArgumentValueRCode(na.omit(originalValue), name)
                    }
                }

                if (name == "calcSubjectsFunction" &&
                        obj$.getParameterType("calcSubjectsFunction") == C_PARAM_USER_DEFINED &&
                        !is.null(obj[["calcSubjectsFunction"]])) {
                    value <- "calcSubjectsFunction"
                } else if (name == "calcEventsFunction" &&
                        obj$.getParameterType("calcEventsFunction") == C_PARAM_USER_DEFINED &&
                        !is.null(obj[["calcEventsFunction"]])) {
                    value <- "calcEventsFunction"
                }

                if ((name == "twoSidedPower" && isFALSE(originalValue)) || name == "accrualIntensityRelative") {
                    # do not add
                    # arguments <- c(arguments, paste0(name, "_DoNotAdd"))
                } else {
                    if (length(value) > 0 && nchar(as.character(value)) > 0) {
                        argument <- paste0(name, " = ", value)
                    } else {
                        argument <- name
                    }
                    if (!(argument %in% leadingArguments)) {
                        arguments <- c(arguments, argument)
                    }
                }
            }
        }

        if (inherits(obj, "TrialDesignPlanSurvival")) {
            if (!("accrualTime" %in% objNames) &&
                    obj$.getParameterType("accrualTime") == "g" && !all(is.na(obj$accrualTime))) {
                # case 2: follow-up time and absolute intensity given
                accrualType2 <- (length(obj$accrualIntensity) == 1 && obj$accrualIntensity >= 1 &&
                    obj$.getParameterType("accrualIntensity") == "u" &&
                    obj$.getParameterType("followUpTime") == "u" &&
                    obj$.getParameterType("maxNumberOfSubjects") == "g")

                if (!accrualType2) {
                    accrualTime <- .getArgumentValueRCode(obj$accrualTime, "accrualTime")
                    if (length(obj$accrualTime) > 1 && length(obj$accrualTime) == length(obj$accrualIntensity) &&
                            (obj$.getParameterType("maxNumberOfSubjects") == "u" ||
                                obj$.getParameterType("followUpTime") == "u")) {
                        accrualTime <- .getArgumentValueRCode(obj$accrualTime[1:(length(obj$accrualTime) - 1)], "accrualTime")
                    }
                    accrualTimeArg <- paste0("accrualTime = ", accrualTime)

                    index <- which(grepl("^accrualIntensity", arguments))
                    if (length(index) == 1 && index > 1) {
                        arguments <- c(arguments[1:(index - 1)], accrualTimeArg, arguments[index:length(arguments)])
                    } else {
                        arguments <- c(arguments, accrualTimeArg)
                    }
                } else if (obj$.getParameterType("followUpTime") == "u") {
                    arguments <- c(arguments, "accrualTime = 0")
                }
            }

            accrualIntensityRelative <- obj$.accrualTime$accrualIntensityRelative
            if (!("accrualIntensity" %in% objNames) && !all(is.na(accrualIntensityRelative))) {
                arguments <- c(arguments, paste0(
                    "accrualIntensity = ",
                    .getArgumentValueRCode(accrualIntensityRelative, "accrualIntensity")
                ))
            }

            if (!("maxNumberOfSubjects" %in% objNames) && obj$.accrualTime$.getParameterType("maxNumberOfSubjects") == "u" &&
                    !(obj$.getParameterType("followUpTime") %in% c("u", "d"))) {
                arguments <- c(arguments, paste0(
                    "maxNumberOfSubjects = ",
                    .getArgumentValueRCode(obj$maxNumberOfSubjects[1], "maxNumberOfSubjects")
                ))
            }
        } else if (inherits(obj, "AnalysisResults")) {
            arguments <- c(arguments, paste0("stage = ", obj$.stageResults$stage))
        } else if (inherits(obj, "StageResults")) {
            arguments <- c(arguments, paste0("stage = ", obj$stage))
        }

        if (length(arguments) > 0) {
            argumentsRCode <- paste0(argumentsRCode, arguments, collapse = ", ")
        }
    }

    if (!is.null(leadingArguments) && length(leadingArguments) > 0) {
        leadingArguments <- unique(leadingArguments)
        leadingArguments <- paste0(leadingArguments, collapse = ", ")
        if (nchar(argumentsRCode) > 0) {
            argumentsRCode <- paste0(leadingArguments, ", ", argumentsRCode)
        } else {
            argumentsRCode <- leadingArguments
        }
    }

    rCode <- paste0(prefix, functionName, "(", argumentsRCode, ")")
    if (any(postfix != "")) {
        if (length(postfix) > 1 && grepl("(\\|>)|(%>%)", postfix[1])) {
            if (!grepl("(\\|>)|(%>%) *$", rCode[length(rCode)])) {
                rCode <- paste0(rCode, postfix[1])
            }
            if (length(postfix) > 1) {
                rCode <- c(rCode, paste0(postfix[2:length(postfix)], collapse = ""))
            }
        } else {
            rCode <- paste0(rCode, paste0(postfix, collapse = ""))
        }
    }

    if (output != "internal" && explicitPrint) {
        if (pipeOperator == "none") {
            rCode <- paste0("print(", rCode, ")")
        } else {
            rCode[length(rCode)] <- paste0(rCode[length(rCode)], pipeOperatorPostfix)
            rCode <- c(rCode, "print()")
        }
    }

    rCode <- c(precondition, rCode)

    if (!is.null(stringWrapParagraphWidth) &&
            length(stringWrapParagraphWidth) == 1 &&
            !is.na(stringWrapParagraphWidth) &&
            is.numeric(stringWrapParagraphWidth) &&
            stringWrapParagraphWidth >= 10 &&
            !is.null(stringWrapPrefix) &&
            length(stringWrapPrefix) == 1 &&
            !is.na(stringWrapPrefix) &&
            is.character(stringWrapPrefix)) {
        rCodeNew <- character(0)
        for (rCodeLine in rCode) {
            rCodeLine <- gsub("   ", "___", rCodeLine)
            rCodeLine <- gsub("  ", "__", rCodeLine)
            rCodeLines <- strwrap(rCodeLine, width = stringWrapParagraphWidth)
            if (length(rCodeLines) > 1) {
                for (i in 2:length(rCodeLines)) {
                    if (grepl("^ *(\\|>|%>%) *", rCodeLines[i])) {
                        rCodeLines[i - 1] <- paste0(rCodeLines[i - 1], pipeOperatorPostfix)
                        rCodeLines[i] <- sub("^ *(\\|>|%>%) *", "", rCodeLines[i])
                    } else if (!grepl("^ *([a-zA-Z0-9]+ *<-)|(^ *get[a-zA-Z]+\\()|summary\\(", rCodeLines[i])) {
                        rCodeLines[i] <- paste0(stringWrapPrefix, rCodeLines[i])
                    }
                }
            }
            rCodeLines <- gsub("___", "   ", rCodeLines)
            rCodeLines <- gsub("__", "  ", rCodeLines)
            rCodeLines <- rCodeLines[nchar(trimws(rCodeLines)) > 0]
            rCodeNew <- c(rCodeNew, rCodeLines)
        }
        rCode <- rCodeNew
    }

    if (output %in% c("vector", "internal")) {
        return(rCode)
    }

    if (output == "cat") {
        collapse <- "\n"
        if (pipeOperator != "none") {
            collapse <- paste0("\n", stringWrapPrefix)
        }
        cat(paste0(rCode, collapse = collapse), "\n")
        return(invisible(rCode))
    }

    if (output == "markdown") {
        collapse <- "\n"
        if (pipeOperator != "none") {
            collapse <- paste0("\n", stringWrapPrefix)
            if (explicitPrint) {
                rCode <- gsub("print\\(\\)", "print(markdown = TRUE)", rCode)
            } else if (!any(grepl("kable\\(", rCode))) {
                rCode[length(rCode)] <- paste0(rCode[length(rCode)], pipeOperatorPostfix)
                rCode <- c(rCode, "kable()")
            }
        }

        return(paste0(rCode, collapse = collapse))
    }

    if (output == "test") {
        message("Evaluate and parse the following code:")
        cat(rCode, "\n")
        x <- eval(parse(text = rCode))
        return(invisible(x))
    }

    return(invisible(rCode))
}
