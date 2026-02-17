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
            return("character()")
        } else if (is.integer(x)) {
            return("integer(0)")
        } else if (is.numeric(x)) {
            return("numeric(0)")
        } else if (is.complex(x)) {
            return("complex(0)")
        }
    }

    if (is.function(x) || .isResultObjectBaseClass(x)) {
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
        if (!is.null(seqTest) && length(seqTest) == 1 && !is.na(seqTest) && grepl("^seq", seqTest)) {
            return(seqTest)
        }
    }

    if (is.matrix(x) && name == "effectMatrix") {
        x <- t(x)
    }

    for (i in seq_len(length(x))) {
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

.getGeneratorFunctionName <- function(obj) {
    if ("TrialDesignPlanMeans" == .getClassName(obj)) {
        if (obj$.isSampleSizeObject()) {
            return("getSampleSizeMeans")
        }

        return("getPowerMeans")
    }

    if ("TrialDesignPlanRates" == .getClassName(obj)) {
        if (obj$.isSampleSizeObject()) {
            return("getSampleSizeRates")
        }

        return("getPowerRates")
    }

    if ("TrialDesignPlanSurvival" == .getClassName(obj)) {
        if (obj$.isSampleSizeObject()) {
            return("getSampleSizeSurvival")
        }

        return("getPowerSurvival")
    }

    if ("TrialDesignPlanCountData" == .getClassName(obj)) {
        if (obj$.isSampleSizeObject()) {
            return("getSampleSizeCounts")
        }

        return("getPowerCounts")
    }

    if (inherits(obj, "TrialDesign")) {
        return(paste0("get", sub("^Trial", "", .getClassName(obj))))
    }

    if (inherits(obj, "Dataset")) {
        return("getDataset")
    }

    if (inherits(obj, "AnalysisResults")) {
        return("getAnalysisResults")
    }

    if ("TrialDesignSet" == .getClassName(obj)) {
        return("getDesignSet")
    }

    if ("TrialDesignCharacteristics" == .getClassName(obj)) {
        return("getDesignCharacteristics")
    }

    if (inherits(obj, "SimulationResultsMeans")) {
        return("getSimulationMeans")
    }

    if (inherits(obj, "SimulationResultsRates")) {
        return("getSimulationRates")
    }

    if (inherits(obj, "SimulationResultsSurvival")) {
        return("getSimulationSurvival")
    }

    if (inherits(obj, "SimulationResultsCountData")) {
        return("getSimulationCounts")
    }

    if (inherits(obj, "SimulationResultsMultiArmMeans")) {
        return("getSimulationMultiArmMeans")
    }

    if (inherits(obj, "SimulationResultsMultiArmRates")) {
        return("getSimulationMultiArmRates")
    }

    if (inherits(obj, "SimulationResultsMultiArmSurvival")) {
        return("getSimulationMultiArmSurvival")
    }

    if (inherits(obj, "SimulationResultsEnrichmentMeans")) {
        return("getSimulationEnrichmentMeans")
    }

    if (inherits(obj, "SimulationResultsEnrichmentRates")) {
        return("getSimulationEnrichmentRates")
    }

    if (inherits(obj, "SimulationResultsEnrichmentSurvival")) {
        return("getSimulationEnrichmentSurvival")
    }

    if (inherits(obj, "PiecewiseSurvivalTime")) {
        return("getPiecewiseSurvivalTime")
    }

    if (inherits(obj, "AccrualTime")) {
        return("getAccrualTime")
    }

    if (inherits(obj, "StageResults")) {
        return("getStageResults")
    }

    if (inherits(obj, "ConditionalPowerResults")) {
        return("getConditionalPower")
    }

    if (inherits(obj, "PowerAndAverageSampleNumberResult")) {
        return("getPowerAndAverageSampleNumber")
    }

    if (inherits(obj, "EventProbabilities")) {
        return("getEventProbabilities")
    }

    if (inherits(obj, "NumberOfSubjects")) {
        return("getNumberOfSubjects")
    }

    if (inherits(obj, "PerformanceScore")) {
        return("gePerformanceScore")
    }

    if (inherits(obj, "SummaryFactory") || "SummaryFactory" == .getClassName(obj)) {
        return(.getGeneratorFunctionName(obj$object))
    }

    stop(
        C_EXCEPTION_TYPE_RUNTIME_ISSUE,
        "function '.getGeneratorFunctionName' is not implemented for class ",
        .getClassName(obj)
    )
}

#' @rdname getObjectRCode
#' @export
rcmd <- function(obj,
        ...,
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
    getObjectRCode(
        obj = obj,
        leadingArguments = leadingArguments,
        includeDefaultParameters = includeDefaultParameters,
        stringWrapParagraphWidth = stringWrapParagraphWidth,
        prefix = prefix,
        postfix = postfix,
        stringWrapPrefix = stringWrapPrefix,
        newArgumentValues = newArgumentValues,
        tolerance = tolerance,
        pipeOperator = pipeOperator,
        output = output,
        explicitPrint = explicitPrint
    )
}

.getPreconditionDesignRCode <- function(design, pipeOperator, pipeOperatorPostfix, includeDefaultParameters,
        stringWrapParagraphWidth, stringWrapPrefix, newArgumentValues,
        leadingArguments) {
    preconditionDesign <- getObjectRCode(
        design,
        prefix = ifelse(pipeOperator == "none", "design <- ", ""),
        postfix = pipeOperatorPostfix,
        includeDefaultParameters = includeDefaultParameters,
        stringWrapParagraphWidth = stringWrapParagraphWidth,
        stringWrapPrefix = stringWrapPrefix,
        newArgumentValues = newArgumentValues,
        pipeOperator = pipeOperator,
        output = "internal"
    )
    precondition <- character()
    if (!grepl(
            "getDesign(GroupSequential|InverseNormal)\\(kMax = 1\\)",
            paste0(preconditionDesign, collapse = " ")
        )) {
        precondition <- c(precondition, preconditionDesign)
        if (pipeOperator == "none") {
            leadingArguments <- c(leadingArguments, "design = design")
        }
    }
    return(list(
        precondition = precondition,
        leadingArguments = leadingArguments
    ))
}

.getObjectRCodeFutilityBounds <- function(obj,
        ...,
        precondition,
        leadingArguments,
        includeDefaultParameters,
        stringWrapParagraphWidth,
        prefix,
        postfix,
        stringWrapPrefix,
        newArgumentValues,
        pipeOperator,
        pipeOperatorPostfix,
        output,
        explicitPrint = FALSE) {
    args <- character()
    for (paramName in c("sourceValue", "sourceScale", "targetScale", "theta", "information", "design")) {
        if (identical(attr(obj, paramName)$type, C_PARAM_USER_DEFINED)) {
            paramValue <- attr(obj, paramName)$value
            if (.isTrialDesign(paramValue)) {
                preconditionDesign <- .getPreconditionDesignRCode(
                    paramValue, pipeOperator, pipeOperatorPostfix, includeDefaultParameters,
                    stringWrapParagraphWidth, stringWrapPrefix, newArgumentValues,
                    leadingArguments
                )
                precondition <- c(precondition, preconditionDesign$precondition)
                leadingArguments <- c(leadingArguments, preconditionDesign$leadingArguments)
            } else {
                paramValue <- .getArgumentValueRCode(paramValue, paramName)
                args <- c(args, paste0(paramName, " = ", paramValue))
            }
        }
    }
    args <- unique(c(leadingArguments, args))
    rCode <- paste0(prefix, "getFutilityBounds(", paste0(args, collapse = ", "), ")")
    rCode <- .formatRCode(
        rCode = rCode,
        precondition = precondition,
        stringWrapParagraphWidth = stringWrapParagraphWidth,
        postfix = postfix,
        stringWrapPrefix = stringWrapPrefix,
        pipeOperator = pipeOperator,
        pipeOperatorPostfix = pipeOperatorPostfix,
        output = output,
        explicitPrint = explicitPrint
    )
    if (output %in% c("vector", "internal", "markdown")) {
        return(rCode)
    }

    return(invisible(rCode))
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
getObjectRCode <- function(obj,
        ...,
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
        lines <- .getFunctionAsString(obj, stringWrapPrefix = stringWrapPrefix)
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

    precondition <- character()
    if (is.null(leadingArguments)) {
        leadingArguments <- character()
    }

    if (!is.null(obj) && is(obj, "FutilityBounds")) {
        return(.getObjectRCodeFutilityBounds(
            obj = obj,
            precondition = precondition,
            leadingArguments = leadingArguments,
            includeDefaultParameters = includeDefaultParameters,
            stringWrapParagraphWidth = stringWrapParagraphWidth,
            prefix = prefix,
            postfix = postfix,
            stringWrapPrefix = stringWrapPrefix,
            newArgumentValues = newArgumentValues,
            pipeOperator = pipeOperator,
            pipeOperatorPostfix = pipeOperatorPostfix,
            output = output,
            explicitPrint = explicitPrint
        ))
    }

    .assertIsParameterSetClass(obj, "ParameterSet")

    if (!is.list(newArgumentValues)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'newArgumentValues' must be a named list ",
            "(is ",
            .getClassName(newArgumentValues),
            ")"
        )
    }

    if (!inherits(obj, "ConditionalPowerResults") &&
            !is.null(obj[[".design"]]) &&
            (is.null(leadingArguments) || !any(grepl("design", leadingArguments)))
        ) {
        preconditionDesign <- .getPreconditionDesignRCode(
            obj[[".design"]], pipeOperator, pipeOperatorPostfix, includeDefaultParameters,
            stringWrapParagraphWidth, stringWrapPrefix, newArgumentValues,
            leadingArguments
        )
        precondition <- c(precondition, preconditionDesign$precondition)
        leadingArguments <- c(leadingArguments, preconditionDesign$leadingArguments)
    }
    if (inherits(obj, "PerformanceScore")) {
        preconditionSimulationResults <- getObjectRCode(
            obj$.simulationResults,
            prefix = ifelse(pipeOperator == "none", "simulationResults <- ", ""),
            postfix = pipeOperatorPostfix,
            includeDefaultParameters = includeDefaultParameters,
            stringWrapParagraphWidth = stringWrapParagraphWidth,
            stringWrapPrefix = stringWrapPrefix,
            newArgumentValues = newArgumentValues,
            pipeOperator = pipeOperator,
            output = "internal"
        )
        precondition <- c(precondition, preconditionSimulationResults)
        if (pipeOperator == "none") {
            leadingArguments <- c(leadingArguments, "simulationResults = simulationResults")
        }
    }
    if (!is.null(obj[[".dataInput"]]) && (is.null(leadingArguments) || !any(grepl("data", leadingArguments)))) {
        precondition <- c(
            precondition,
            getObjectRCode(
                obj$.dataInput,
                prefix = ifelse(pipeOperator == "none", "data <- ", ""),
                postfix = pipeOperatorPostfix,
                includeDefaultParameters = includeDefaultParameters,
                stringWrapParagraphWidth = stringWrapParagraphWidth,
                stringWrapPrefix = stringWrapPrefix,
                newArgumentValues = newArgumentValues,
                pipeOperator = pipeOperator,
                output = "internal"
            )
        )
        if (pipeOperator == "none") {
            leadingArguments <- c(leadingArguments, "dataInput = data")
        }
    }
    if (
        !is.null(obj[["calcSubjectsFunction"]]) &&
            (is.null(leadingArguments) || !any(grepl("calcSubjectsFunction", leadingArguments))) &&
            obj$.getParameterType("calcSubjectsFunction") == C_PARAM_USER_DEFINED
        ) {
        precond <- getObjectRCode(
            obj$calcSubjectsFunction,
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
    if (
        !is.null(obj[["calcEventsFunction"]]) &&
            (is.null(leadingArguments) || !any(grepl("calcEventsFunction", leadingArguments))) &&
            obj$.getParameterType("calcEventsFunction") == C_PARAM_USER_DEFINED
        ) {
        precond <- getObjectRCode(
            obj$calcEventsFunction,
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
    if (
        !is.null(obj[["selectArmsFunction"]]) &&
            (is.null(leadingArguments) || !any(grepl("selectArmsFunction", leadingArguments))) &&
            !is.null(obj[["typeOfSelection"]]) &&
            obj$typeOfSelection == "userDefined"
        ) {
        precond <- getObjectRCode(
            obj$selectArmsFunction,
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
    if (
        inherits(obj, "ConditionalPowerResults") &&
            !is.null(obj[[".stageResults"]]) &&
            (is.null(leadingArguments) || !any(grepl("stageResults", leadingArguments)))
        ) {
        precond <- getObjectRCode(
            obj$.stageResults,
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

    precondition <- unique(precondition)

    if (inherits(obj, "SummaryFactory") || "SummaryFactory" == .getClassName(obj)) {
        return(getObjectRCode(
            obj$object,
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
        functionName <- .getGeneratorFunctionName(obj)
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

    if (
        inherits(obj, "TrialDesign") &&
            !inherits(obj, "TrialDesignConditionalDunnett") &&
            !("informationRates" %in% objNames) &&
            !("kMax" %in% objNames) &&
            obj$kMax != 3
        ) {
        objNames <- c("kMax", objNames)
    }

    thetaH0 <- NA_real_
    if (
        inherits(obj, "SimulationResultsSurvival") &&
            obj$.getParameterType("thetaH1") == C_PARAM_GENERATED
        ) {
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
        } else if (
            !is.null(obj[[".conditionalPowerResults"]]) &&
                !is.null(obj$.conditionalPowerResults[["seed"]]) &&
                length(obj$.conditionalPowerResults$seed) == 1 &&
                !is.na(obj$.conditionalPowerResults$seed)
            ) {
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

    if (
        !("accrualTime" %in% objNames) &&
            !is.null(obj[[".accrualTime"]]) &&
            obj$.getParameterType("accrualTime") == C_PARAM_GENERATED &&
            obj$.accrualTime$.getParameterType("accrualTimeOriginal") == C_PARAM_USER_DEFINED
        ) {
        objNames <- c(objNames, "accrualTime")
    }

    if (
        !("accrualIntensity" %in% objNames) &&
            !is.null(obj[[".accrualTime"]]) &&
            !obj$.accrualTime$absoluteAccrualIntensityEnabled &&
            obj$.getParameterType("accrualIntensity") == C_PARAM_USER_DEFINED &&
            !all(is.na(obj$accrualIntensity))
        ) {
        objNames <- c(objNames, "accrualIntensity")
        objNames <- objNames[objNames != "accrualIntensityRelative"]
    }

    newArgumentValueNames <- character()
    if (length(newArgumentValues) > 0) {
        newArgumentValueNames <- names(newArgumentValues)
        illegalArgumentValueNames <- newArgumentValueNames[which(!(newArgumentValueNames %in% names(obj)))]
        if (length(illegalArgumentValueNames) > 0) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'",
                illegalArgumentValueNames,
                "' is not a valid ",
                functionName,
                "() argument"
            )
        }

        defaultParams <- newArgumentValueNames[!(newArgumentValueNames %in% objNames)]
        objNames <- c(objNames, defaultParams)
    }

    if (
        inherits(obj, "TrialDesign") &&
            "informationRates" %in% objNames &&
            !("informationRates" %in% newArgumentValueNames)
        ) {
        informationRates <- obj[["informationRates"]]
        if (!is.null(informationRates) && length(informationRates) > 0) {
            kMax <- obj[["kMax"]]
            if (
                isTRUE(all.equal(
                    target = .getInformationRatesDefault(kMax),
                    current = informationRates,
                    tolerance = tolerance
                ))
                ) {
                objNames <- objNames[objNames != "informationRates"]
                if (!("kMax" %in% objNames) && kMax != 3) {
                    objNames <- c("kMax", objNames)
                }
            }
        }
    }

    if (inherits(obj, "Dataset")) {
        lines <- .getDatasetArgumentsRCodeLines(obj, complete = FALSE, digits = NA_integer_)
        argumentsRCode <- paste0(lines, collapse = ", ")
    } else {
        argumentsRCode <- ""
        arguments <- c()
        if (length(objNames) > 0) {
            for (objName in objNames) {
                if (grepl("^\\.conditionalPowerResults\\$", objName)) {
                    objName <- sub("^\\.conditionalPowerResults\\$", "", objName)
                    value <- obj$.conditionalPowerResults[[objName]]
                } else {
                    value <- obj[[objName]]
                }

                if (
                    objName == "accrualTime" &&
                        inherits(obj, "AccrualTime") &&
                        !isTRUE(obj$endOfAccrualIsUserDefined) &&
                        isTRUE(length(obj$accrualIntensity) < length(value))
                    ) {
                    value <- value[1:(length(value) - 1)]
                }
                if (
                    objName == "accrualTime" &&
                        !is.null(obj[[".accrualTime"]]) &&
                        obj$.getParameterType("accrualTime") == C_PARAM_GENERATED &&
                        obj$.accrualTime$.getParameterType("accrualTimeOriginal") == C_PARAM_USER_DEFINED
                    ) {
                    value <- obj$.accrualTime$accrualTimeOriginal
                }
                if (objName == "accrualTimeOriginal") {
                    objName <- "accrualTime"
                }

                if (objName == "accrualIntensityRelative") {
                    objName <- "accrualIntensity"
                }
                if (
                    objName == "accrualIntensity" &&
                        !is.null(obj[[".accrualTime"]]) &&
                        !obj$.accrualTime$absoluteAccrualIntensityEnabled
                    ) {
                    value <- obj$.accrualTime$accrualIntensityRelative
                }

                futilityBoundsScale <- NULL
                if (objName == "futilityBounds" && is(value, "FutilityBounds")) {
                    sourceScale <- attr(value, "sourceScale")$value
                    if (!is.null(sourceScale) && sourceScale %in% c("conditionalPower", "effectEstimate")) {
                        precondition <- c(
                            precondition,
                            getObjectRCode(
                                value,
                                prefix = ifelse(pipeOperator == "none", "futilityBounds <- ", ""),
                                postfix = pipeOperatorPostfix,
                                includeDefaultParameters = includeDefaultParameters,
                                stringWrapParagraphWidth = stringWrapParagraphWidth,
                                stringWrapPrefix = stringWrapPrefix,
                                newArgumentValues = newArgumentValues,
                                pipeOperator = pipeOperator,
                                output = "internal"
                            )
                        )
                        if (pipeOperator == "none") {
                            leadingArguments <- c(leadingArguments, "futilityBounds = futilityBounds")
                        }
                        next
                    } else {
                        futilityBoundsScale <- attr(value, "sourceScale")$value
                        if (!is.null(futilityBoundsScale) && !identical(futilityBoundsScale, "zValue")) {
                            value <- attr(value, "sourceValue")$value
                        } else {
                            futilityBoundsScale <- NULL
                        }
                    }
                }

                alpha0Scale <- NULL
                if (objName == "alpha0Vec" && is(value, "FutilityBounds")) {
                    sourceScale <- attr(value, "sourceScale")$value
                    if (!is.null(sourceScale) && sourceScale %in% c("conditionalPower", "effectEstimate")) {
                        precondition <- c(
                            precondition,
                            getObjectRCode(
                                value,
                                prefix = ifelse(pipeOperator == "none", "alpha0Vec <- ", ""),
                                postfix = pipeOperatorPostfix,
                                includeDefaultParameters = includeDefaultParameters,
                                stringWrapParagraphWidth = stringWrapParagraphWidth,
                                stringWrapPrefix = stringWrapPrefix,
                                newArgumentValues = newArgumentValues,
                                pipeOperator = pipeOperator,
                                output = "internal"
                            )
                        )
                        if (pipeOperator == "none") {
                            leadingArguments <- c(leadingArguments, "alpha0Vec = alpha0Vec")
                        }
                        next
                    } else {
                        alpha0Scale <- attr(value, "sourceScale")$value
                        if (!is.null(alpha0Scale) && !identical(alpha0Scale, "pValue")) {
                            value <- attr(value, "sourceValue")$value
                        } else {
                            alpha0Scale <- NULL
                        }
                    }
                }

                originalValue <- value
                newValue <- newArgumentValues[[objName]]
                if (!is.null(newValue)) {
                    originalValue <- newValue
                }

                value <- .getArgumentValueRCode(originalValue, objName)

                if (objName == "allocationRatioPlanned") {
                    optimumAllocationRatio <- obj[["optimumAllocationRatio"]]
                    if (!is.null(optimumAllocationRatio) && isTRUE(optimumAllocationRatio)) {
                        value <- 0
                    } else if (inherits(obj, "ParameterSet")) {
                        if (obj$.getParameterType("allocationRatioPlanned") == C_PARAM_GENERATED) {
                            value <- 0
                        }
                    }
                } else if (objName == "optimumAllocationRatio") {
                    objName <- "allocationRatioPlanned"
                    value <- 0
                } else if (objName == "maxNumberOfSubjects") {
                    value <- .getArgumentValueRCode(originalValue[1], objName)
                } else if (objName == "thetaH1" && length(thetaH0) == 1 && !is.na(thetaH0) && value != 1) {
                    value <- .getArgumentValueRCode(originalValue * thetaH0, objName)
                } else if (objName == "nPlanned") {
                    if (!all(is.na(originalValue))) {
                        value <- .getArgumentValueRCode(na.omit(originalValue), objName)
                    }
                }

                if (
                    objName == "calcSubjectsFunction" &&
                        obj$.getParameterType("calcSubjectsFunction") == C_PARAM_USER_DEFINED &&
                        !is.null(obj[["calcSubjectsFunction"]])
                    ) {
                    value <- "calcSubjectsFunction"
                } else if (
                    objName == "calcEventsFunction" &&
                        obj$.getParameterType("calcEventsFunction") == C_PARAM_USER_DEFINED &&
                        !is.null(obj[["calcEventsFunction"]])
                    ) {
                    value <- "calcEventsFunction"
                }

                if ((objName == "twoSidedPower" && isFALSE(originalValue)) || objName == "accrualIntensityRelative") {
                    # do not add
                    # arguments <- c(arguments, paste0(name, "_DoNotAdd"))
                } else {
                    argument <- NULL
                    if (length(value) > 0 && nchar(as.character(value)) > 0) {
                        if (!all(is.na(value)) && !all(grepl("^NA(_(real|integer|character)_)?$", as.character(value)))) {
                            argument <- paste0(objName, " = ", value)
                        }
                    } else {
                        argument <- objName
                    }
                    if (!is.null(argument) && !(argument %in% leadingArguments)) {
                        arguments <- c(arguments, argument)
                    }

                    if (!is.null(futilityBoundsScale)) {
                        arguments <- c(arguments, paste0('futilityBoundsScale = "', futilityBoundsScale, '"'))
                    }
                    if (!is.null(alpha0Scale)) {
                        arguments <- c(arguments, paste0('alpha0Scale = "', alpha0Scale, '"'))
                    }
                }
            }
        }

        if (inherits(obj, "TrialDesignPlanSurvival") || inherits(obj, "SimulationResultsSurvival")) {
            if (
                !("accrualTime" %in% objNames) &&
                    obj$.getParameterType("accrualTime") == C_PARAM_GENERATED &&
                    !all(is.na(obj$accrualTime))
                ) {
                # case 2: follow-up time and absolute intensity given
                accrualType2 <- (length(obj$accrualIntensity) == 1 &&
                    obj$accrualIntensity >= 1 &&
                    obj$.getParameterType("accrualIntensity") == C_PARAM_USER_DEFINED &&
                    obj$.getParameterType("followUpTime") == C_PARAM_USER_DEFINED &&
                    obj$.getParameterType("maxNumberOfSubjects") == C_PARAM_GENERATED)

                if (!accrualType2) {
                    accrualTime <- .getArgumentValueRCode(obj$accrualTime, "accrualTime")
                    if (
                        length(obj$accrualTime) > 1 &&
                            length(obj$accrualTime) == length(obj$accrualIntensity) &&
                            (obj$.getParameterType("maxNumberOfSubjects") == C_PARAM_USER_DEFINED ||
                                obj$.getParameterType("followUpTime") == C_PARAM_USER_DEFINED)
                        ) {
                        accrualTime <- .getArgumentValueRCode(
                            obj$accrualTime[1:(length(obj$accrualTime) - 1)],
                            "accrualTime"
                        )
                    }

                    if (
                        !identical(accrualTime, c(0, 12)) &&
                            !identical(accrualTime, c(0L, 12L)) &&
                            getParameterType(obj$.accrualTime, "accrualTime") != C_PARAM_GENERATED
                        ) {
                        accrualTimeArg <- paste0("accrualTime = ", accrualTime)

                        index <- which(grepl("^accrualIntensity", arguments))
                        if (length(index) == 1 && index > 1) {
                            arguments <- c(arguments[1:(index - 1)], accrualTimeArg, arguments[index:length(arguments)])
                        } else {
                            arguments <- c(arguments, accrualTimeArg)
                        }
                    }
                } else if (obj$.getParameterType("followUpTime") == C_PARAM_USER_DEFINED) {
                    arguments <- c(arguments, "accrualTime = 0")
                }
            }

            accrualIntensityRelative <- obj$.accrualTime$accrualIntensityRelative
            if (
                !("accrualIntensity" %in% objNames) &&
                    !all(is.na(accrualIntensityRelative)) &&
                    obj$.accrualTime$.getParameterType("accrualIntensityRelative") == C_PARAM_USER_DEFINED
                ) {
                arguments <- c(
                    arguments,
                    paste0(
                        "accrualIntensity = ",
                        .getArgumentValueRCode(accrualIntensityRelative, "accrualIntensity")
                    )
                )
            }

            if (
                !("maxNumberOfSubjects" %in% objNames) &&
                    obj$.accrualTime$.getParameterType("maxNumberOfSubjects") == C_PARAM_USER_DEFINED &&
                    !(obj$.getParameterType("followUpTime") %in% c(C_PARAM_USER_DEFINED, C_PARAM_DEFAULT_VALUE))
                ) {
                arguments <- c(
                    arguments,
                    paste0(
                        "maxNumberOfSubjects = ",
                        .getArgumentValueRCode(obj$maxNumberOfSubjects[1], "maxNumberOfSubjects")
                    )
                )
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
    .formatRCode(
        rCode = rCode,
        precondition = precondition,
        stringWrapParagraphWidth = stringWrapParagraphWidth,
        postfix = postfix,
        stringWrapPrefix = stringWrapPrefix,
        pipeOperator = pipeOperator,
        pipeOperatorPostfix = pipeOperatorPostfix,
        output = output,
        explicitPrint = explicitPrint
    )
}

.formatRCode <- function(rCode,
        precondition,
        stringWrapParagraphWidth,
        postfix,
        stringWrapPrefix,
        pipeOperator,
        pipeOperatorPostfix,
        output,
        explicitPrint) {
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

    if (
        !is.null(stringWrapParagraphWidth) &&
            length(stringWrapParagraphWidth) == 1 &&
            !is.na(stringWrapParagraphWidth) &&
            is.numeric(stringWrapParagraphWidth) &&
            stringWrapParagraphWidth >= 10 &&
            !is.null(stringWrapPrefix) &&
            length(stringWrapPrefix) == 1 &&
            !is.na(stringWrapPrefix) &&
            is.character(stringWrapPrefix)
        ) {
        rCodeNew <- character()
        for (rCodeLine in rCode) {
            for (i in 12:2) {
                rCodeLine <- gsub(
                    pattern = paste(rep(" ", i), collapse = ""),
                    replacement = paste(rep("_", i), collapse = ""),
                    x = rCodeLine
                )
            }
            if (!grepl("^__", rCodeLine)) {
                rCodeLine <- gsub(" = ", "_=_", rCodeLine)
                rCodeLines <- strwrap(rCodeLine, width = stringWrapParagraphWidth - 2)
                rCodeLines <- gsub("_=_", " = ", rCodeLines)
            } else {
                rCodeLines <- rCodeLine
            }
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
            for (i in 12:2) {
                rCodeLines <- gsub(
                    pattern = paste(rep("_", i), collapse = ""),
                    replacement = paste(rep(" ", i), collapse = ""),
                    x = rCodeLines
                )
            }
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
