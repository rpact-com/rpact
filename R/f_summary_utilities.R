## |
## |  *Summary classes and functions*
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
#' @include f_core_assertions.R
NULL

.formatSummaryValues <- function(
        values,
        ...,
        digits,
        smoothedZeroFormat = FALSE,
        formatRepeatedPValues = FALSE,
        showNA = FALSE) {
    if (is.na(digits)) {
        digits <- 3
    }

    if (digits < 1) {
        formattedValue <- as.character(values)
        formattedValue[is.na(formattedValue) | trimws(formattedValue) == "NA"] <-
            .getEnvironmentVariable("RPACT_SUMMARY_NA",
                "rpact.summary.na",
                default = "", type = "character"
            )
        return(formattedValue)
    }

    if (sum(is.na(values)) == length(values)) {
        formattedValue <- rep(
            .getEnvironmentVariable("RPACT_SUMMARY_NA",
                "rpact.summary.na",
                default = "", type = "character"
            ),
            length(values)
        )
        return(formattedValue)
    }

    threshold <- 10^-digits
    text <- "<0."
    if (digits > 1) {
        for (i in 1:(digits - 1)) {
            text <- paste0(text, "0")
        }
    }
    text <- paste0(text, "1")

    if (smoothedZeroFormat) {
        values[abs(values) < 1e-15] <- 0
    }
    indices <- (!is.na(values) & values > 1e-10 & abs(values) < threshold)
    values[!is.na(values) & !indices] <- round(values[!is.na(values) & !indices], digits)
    if (sum(indices) > 0) {
        values[indices] <- threshold
        formattedValue <- .getFormattedValue(values, digits = digits, nsmall = digits, scientific = FALSE)
        formattedValue[indices] <- text
    } else {
        formattedValue <- .getFormattedValue(values, digits = digits, nsmall = digits, scientific = FALSE)
        formattedValue <- format(formattedValue, scientific = FALSE)
    }

    if (formatRepeatedPValues) {
        formattedValue[!is.na(formattedValue) &
            nchar(gsub("\\D", "", (formattedValue))) > 0 & formattedValue > 0.4999] <- ">0.5"
    }

    if (as.logical(.getEnvironmentVariable(
            "RPACT_SUMMARY_TRIM_ZEROES",
            "rpact.summary.trim.zeroes",
            default = TRUE,
            type = "logical"
        ))) {
        zeroes <- grepl("^0\\.0*$", formattedValue)
        if (sum(zeroes) > 0) {
            formattedValue[zeroes] <- "0"
        }
    }

    if (any(is.nan(formattedValue), na.rm = TRUE) || any(trimws(formattedValue) == "NaN", na.rm = TRUE)) {
        formattedValue[is.nan(formattedValue) | trimws(formattedValue) == "NaN"] <- NA_real_
        showNA <- TRUE
    }

    formattedValue[is.na(formattedValue) | trimws(formattedValue) == "NA"] <-
        ifelse(showNA, "n/a", .getEnvironmentVariable(
            "RPACT_SUMMARY_NA",
            "rpact.summary.na",
            default = "",
            type = "character"
        ))

    return(formattedValue)
}

.getSummaryValuesFormatted <- function(
        fieldSet,
        parameterName,
        values,
        ...,
        roundDigits = NA_integer_,
        ceilingEnabled = FALSE,
        cumsumEnabled = FALSE,
        smoothedZeroFormat = FALSE,
        formatRepeatedPValues = FALSE,
        roundDigitsAsInformation = FALSE,
        showNA = FALSE) {
    if (!is.numeric(values)) {
        return(values)
    }

    if (cumsumEnabled) {
        values <- cumsum(values)
    }

    if (ceilingEnabled) {
        values <- ceiling(values)
    } else {
        tryCatch(
            {
                formatFunctionName <- NULL
                if (roundDigitsAsInformation) {
                    maxValue <- max(values, na.rm = TRUE)
                    if (length(maxValue) == 1) {
                        if (maxValue > 10) {
                            roundDigits <- 1
                        } else if (maxValue < 1) {
                            roundDigits <- 3
                        } else {
                            roundDigits <- 2
                        }
                    }
                } else if (!is.null(parameterName) && length(parameterName) == 1 && !is.na(parameterName)) {
                    if (parameterName == "futilityBounds") {
                        values <- .getFormattedFutilityBounds(design = fieldSet, futilityBounds = values)
                    } else if (parameterName %in% c(
                            "criticalValues",
                            "decisionCriticalValue", "overallAdjustedTestStatistics"
                        )) {
                        design <- fieldSet
                        if (!.isTrialDesign(design)) {
                            design <- fieldSet[[".design"]]
                        }
                        if (!is.null(design) && .isTrialDesignFisher(design)) {
                            roundDigits <- 0
                        }
                    }
                    if (!is.na(roundDigits) && roundDigits == 0) {
                        if (inherits(fieldSet, "Dataset") &&
                                grepl("samplesize|event", tolower(parameterName))) {} else {
                            formatFunctionName <- .getParameterFormatFunction(parameterName, fieldSet)
                        }
                    }
                }

                if (!is.null(formatFunctionName)) {
                    values <- .getParameterValueFormattedByFormatFunctionName(
                        formatFunctionName, values, fieldSet
                    )
                } else {
                    values <- .formatSummaryValues(values,
                        digits = roundDigits,
                        smoothedZeroFormat = smoothedZeroFormat,
                        formatRepeatedPValues = formatRepeatedPValues,
                        showNA = showNA
                    )
                }
            },
            error = function(e) {
                stopRuntimeIssue("failed to show parameter ", .pQuote(parameterName), ": ", e$message,
                    functionName = ".getSummaryValuesFormatted",
                    parameter = parameterName
                )
            }
        )
    }

    return(format(values))
}

.isRatioComparisonEnabled <- function(object) {
    if (!is.null(object[["meanRatio"]]) && isTRUE(object[["meanRatio"]])) {
        return(TRUE)
    }

    if (!is.null(object[["riskRatio"]]) && isTRUE(object[["riskRatio"]])) {
        return(TRUE)
    }

    if (.isTrialDesignPlanCountData(object)) {
        return(TRUE)
    }

    return(FALSE)
}

#'
#' @title
#' Summary Factory Plotting
#'
#' @param x The summary factory object.
#' @param y Not available for this kind of plot (is only defined
#'        to be compatible to the generic plot function).
#' @param showSummary Show the summary before creating the
#'        plot output, default is \code{FALSE}.
#' @inheritParams param_three_dots_plot
#'
#' @description
#' Plots a summary factory.
#'
#' @details
#' Generic function to plot all kinds of summary factories.
#'
#' @template return_object_ggplot
#'
#' @export
#'
plot.SummaryFactory <- function(x, y, ..., showSummary = FALSE) {
    fCall <- match.call(expand.dots = TRUE)
    cmd <- paste0(paste(trimws(capture.output(print(fCall$x))), collapse = " "), "$object")

    parentFunctionCallArgs <- .addAnalysisPlotArgumentsToFunctionCall(x$object, fCall, result = list())
    if (isTRUE(showSummary) || .isSummaryPipe(fCall)) {
        markdown <- .getOptionalArgument("markdown", ..., optionalArgumentDefaultValue = NA)
        if (is.na(markdown)) {
            markdown <- .isMarkdownEnabled("plot")
        }
        if (markdown) {
            sep <- .getMarkdownPlotPrintSeparator()
            type <- .getOptionalArgument("type", optionalArgumentDefaultValue = NA_integer_, ...)
            grid <- .getOptionalArgument("grid", optionalArgumentDefaultValue = 1, ...)
            .assertIsValidPlotType(type, naAllowed = TRUE)
            .assertIsSingleInteger(grid, "grid", naAllowed = FALSE, validateType = FALSE)

            if (!all(is.na(type)) && length(type) > 1 && grid == 1) {
                grid <- 0
            }
            if (grid > 0) {
                suppressWarnings(print(plot(
                    x = x$object, y = NULL, markdown = FALSE,
                    parentFunctionCallArgs = parentFunctionCallArgs,
                    cmd = cmd,
                    ...
                )))
            } else {
                suppressWarnings(plot(
                    x = x$object, y = NULL, markdown = TRUE,
                    parentFunctionCallArgs = parentFunctionCallArgs,
                    cmd = cmd,
                    ...
                ))
            }
            return(.knitPrintQueue(x, sep = sep, prefix = sep))
        } else {
            x$show()
        }
    }
    suppressWarnings(print(plot(
        x = x$object, y = NULL,
        parentFunctionCallArgs = parentFunctionCallArgs,
        cmd = cmd,
        ...
    )))
}

.getKnitPrintPart <- function(x) {
    part <- paste0(utils::capture.output(x$.catMarkdownText()), collapse = "\n")
    part <- na.omit(part)
    return(part)
}

.addKnitPrintPart <- function(x, result, ..., sep, prefix = "", suffix = "") {
    result <- na.omit(result)
    return(paste0(prefix, paste0(c(result, .getKnitPrintPart(x)), collapse = sep), suffix))
}

#' @title
#' Knit Print Queue
#'
#' @description
#' The `.knitPrintQueue` function handles the printing of objects in a queue,
#' specifically for `SummaryFactory` objects, in a format suitable for `knitr`.
#' It ensures that the queue is reset after execution.
#'
#' @param x The object to be printed, which can be a `SummaryFactory` object.
#' @param ... Additional arguments passed to the function.
#' @param sep The separator used between parts of the output. Defaults to `NA_character_`.
#' @param prefix A prefix to be added to the output. Defaults to an empty string.
#'
#' @return
#' Returns a `knitr::asis_output` object containing the formatted output, or `invisible()` if the result is empty.
#'
#' @details
#' The function starts by initializing an empty character vector `result`.
#' It then checks if the input object `x` inherits from `SummaryFactory`.
#' Depending on this, it retrieves the `queue` attribute from either `x$object` or `x` itself.
#' If the `sep` parameter is `NA`, it is set to the default markdown plot print separator.
#' The function processes each object in the `queue` and appends the formatted part to the `result` vector.
#' If `x` inherits from `SummaryFactory`, it adds the formatted part of `x` itself to the `result` vector.
#' Finally, if the `result` vector is empty or contains only whitespace, the function returns `invisible()`.
#' Otherwise, it returns the `result` as a `knitr::asis_output` object.
#'
#' @noRd
#'
.knitPrintQueue <- function(x, ..., sep = NA_character_, prefix = "", resetPipeOperatorQueue = TRUE) {
    # ensure the queue is reset after execution
    if (isTRUE(resetPipeOperatorQueue)) {
        on.exit(.resetPipeOperatorQueue(x))
    }

    # initialize an empty character vector for the result
    result <- character()

    # get queue from x
    queue <- .getPipeOperatorQueue(x)

    # set the separator to the default markdown plot print separator if it is NA
    if (is.na(sep)) {
        sep <- .getMarkdownPlotPrintSeparator()
    }

    # process each object in the queue and append the formatted part to the result
    if (!is.null(queue) && length(queue) > 0) {
        result <- ifelse(!inherits(x, "SummaryFactory"), "", result)
        for (obj in queue) {
            result <- .addKnitPrintPart(obj, result, sep = sep)
        }
    }

    # add the formatted part of x itself to the result if it inherits from SummaryFactory
    if (inherits(x, "SummaryFactory")) {
        result <- .addKnitPrintPart(x, result, sep = sep, prefix = prefix)
    }

    # return invisible() if the result is empty or contains only whitespace
    if (length(result) == 0 || all(nchar(trimws(result)) == 0)) {
        return(invisible())
    }

    # return the result as a knitr::asis_output object
    return(knitr::asis_output(result))
}

#'
#' @title
#' Print Summary Factory in Markdown Code Chunks
#'
#' @description
#' The function `knit_print.SummaryFactory` is the default
#' printing function for rpact summary objects in knitr.
#' The chunk option `render` uses this function by default.
#' To fall back to the normal printing behavior set the
#' chunk option `render = normal_print`.
#' For more information see \code{\link[knitr]{knit_print}}.
#'
#' @param x A \code{SummaryFactory}.
#' @param  ... Other arguments (see \code{\link[knitr]{knit_print}}).
#'
#' @details
#' Generic function to print a summary object in Markdown.
#'
#' @template details_knit_print
#'
#' @export
#'
knit_print.SummaryFactory <- function(x, ...) {
    .knitPrintQueue(x, sep = .getMarkdownPlotPrintSeparator(), ...)
}

#'
#' @title
#' Summary Factory Printing
#'
#' @param x The summary factory object.
#' @param markdown If \code{TRUE}, the object \code{x}
#'        will be printed using markdown syntax;
#'        normal representation will be used otherwise (default is \code{FALSE})
#' @param sep The separator line between the summary and the print output, default is \code{"\n\n-----\n\n"}.
#' @inheritParams param_three_dots_plot
#'
#' @description
#' Prints the result object stored inside a summary factory.
#'
#' @details
#' Generic function to print all kinds of summary factories.
#'
#' @export
#'
print.SummaryFactory <- function(
        x,
        ...,
        markdown = NA,
        sep = NA_character_) {
    .assertIsSingleCharacter(sep, "sep", naAllowed = TRUE)

    if (is.na(markdown)) {
        markdown <- .isMarkdownEnabled("summary")
    }
    if (is.na(sep)) {
        sep <- .getMarkdownPlotPrintSeparator()
    }

    if (markdown || isTRUE(x[["markdown"]])) {
        .addObjectToPipeOperatorQueue(x$object)
        return(.knitPrintQueue(x, sep = sep))
    }

    x$show()
}

.addSummaryLineBreak <- function(text, newLineLength) {
    maxLineLength <- .getEnvironmentVariable(
        "RPACT_SUMMARY_WIDTH",
        "rpact.summary.width",
        default = 83,
        type = "integer"
    )
    lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
    lastLine <- lines[length(lines)]
    if (nchar(lastLine) + newLineLength > maxLineLength) {
        text <- paste0(text, "\n")
    }
    return(text)
}

.concatenateSummaryText <- function(a, b, sep = ", ") {
    .assertIsSingleCharacter(a, "a")
    .assertIsSingleCharacter(b, "b")
    if (is.na(b) || nchar(trimws(b)) == 0) {
        return(a)
    }

    if (a == "") {
        return(b)
    }

    a <- paste0(a, sep)
    a <- .addSummaryLineBreak(a, nchar(b))
    return(paste0(a, b))
}

.getSummaryDigits <- function(digits = NA_integer_) {
    if (is.na(digits)) {
        digits <- .getEnvironmentVariable(
            "RPACT_SUMMARY_DIGITS",
            "rpact.summary.digits",
            default = 3L,
            type = "integer"
        )
    }
    .assertIsSingleInteger(digits, "digits", validateType = FALSE, naAllowed = TRUE)
    .assertIsInClosedInterval(digits, "digits", lower = -1, upper = 12, naAllowed = TRUE)

    digitsSampleSize <- 1
    digitsTime <- 2
    if (digits > 0) {
        digitsGeneral <- digits
        digitsProbabilities <- NA_integer_
        tryCatch(
            {
                digitsProbabilities <- .getEnvironmentVariable(
                    "RPACT_SUMMARY_DIGITS_PROBS",
                    "rpact.summary.digits.probs",
                    default = as.integer(digits + 1L),
                    type = "integer"
                )
            },
            warning = function(e) {}
        )
        if (is.na(digitsProbabilities)) {
            digitsProbabilities <- digits + 1
        }
        .assertIsSingleInteger(digitsProbabilities, "digitsProbabilities",
            validateType = FALSE, naAllowed = FALSE
        )
        .assertIsInClosedInterval(digitsProbabilities, "digitsProbabilities",
            lower = -1, upper = 12, naAllowed = FALSE
        )
    } else {
        digitsSampleSize <- digits
        digitsGeneral <- digits
        digitsProbabilities <- digits
        digitsTime <- digits
    }
    return(list(
        digits = digits,
        digitsSampleSize = digitsSampleSize,
        digitsGeneral = digitsGeneral,
        digitsProbabilities = digitsProbabilities,
        digitsTime = digitsTime
    ))
}

.getSummaryValuesInPercent <- function(values, percentFormatEnabled = TRUE, digits = 1) {
    if (!percentFormatEnabled) {
        return(as.character(round(values, digits + 2)))
    }
    return(paste0(round(100 * values, digits), "%"))
}

.getSummaryDesignCharacteristics <- function(design, kMaxMin = 2) {
    if (design$kMax < kMaxMin) {
        return(NULL)
    }

    if (!.isTrialDesignGroupSequentialOrFixed(design) && !.isTrialDesignInverseNormal(design)) {
        return(NULL)
    }

    tryCatch(
        {
            return(getDesignCharacteristics(design))
        },
        error = function(e) {
            .logError("Cannot add design characteristics to summary: ", e$message)
        }
    )
    return(NULL)
}

.addLegendEntry <- function(
        type = c("effectMatrix", "treatmentArm", "treatmentArms", "treatmentEffectScale"), 
        legendEntry = list()) {
        
    type <- match.arg(type)
    if (type == "effectMatrix") {
        entry <- C_SUMMARY_LEGEND_ENTRY_EFFECT_MATRIX
    } else if (type == "treatmentArm") {
        entry <- C_SUMMARY_LEGEND_ENTRY_TREATMENT_ARM
    } else if (type == "treatmentArms") {
        entry <- C_SUMMARY_LEGEND_ENTRY_TREATMENT_ARMS
    } else if (type == "treatmentEffectScale") {
        entry <- C_SUMMARY_LEGEND_ENTRY_TREATMENT_EFFECT_SCALE
    } else {
        stopRuntimeIssue("type ", .pQuote(type), " is not yet implemented for legend entry addition",
            functionName = ".addLegendEntry",
            parameter = "type", 
            value = type
        )
    }
    if (!is.null(names(entry)) && !is.null(names(legendEntry)) && any(names(entry) %in% names(legendEntry))) {
        return(legendEntry)
    }
    
    legendEntry <- c(legendEntry, entry)
    return(legendEntry)
}

.addParameterToSummaryFactory <- function(
        summaryFactory,
        parameterSet,
        ...,
        parameterName = NULL,
        values = NULL,
        parameterCaption,
        roundDigits = NA_integer_,
        ceilingEnabled = FALSE,
        cumsumEnabled = FALSE,
        twoSided = FALSE,
        transpose = FALSE,
        smoothedZeroFormat = FALSE,
        parameterCaptionSingle = parameterCaption,
        legendEntry = list(),
        enforceFirstCase = FALSE,
        formatRepeatedPValues = FALSE,
        validateParameterType = TRUE,
        lastStage = NA_integer_,
        roundDigitsAsInformation = FALSE,
        showNA = FALSE) {
    if (!is.null(parameterName) && length(parameterName) == 1 &&
            inherits(parameterSet, "ParameterSet") &&
            parameterSet$isNotApplicableParameter(parameterName)) {
        if (!is.null(values) && .getLogicalEnvironmentVariable("RPACT_DEVELOPMENT_MODE") &&
                validateParameterType && !.isMarkdownEnabled()) {
            warning(
                "Failed to add parameter ", .arrayToString(parameterName), " (",
                .arrayToString(values), ") stored in ",
                .getClassName(parameterSet), " because the ",
                "parameter has type C_PARAM_NOT_APPLICABLE"
            )
        }

        return(invisible())
    }

    parameterName1 <- parameterName[1]
    if (!is.null(parameterName1) && is.character(parameterName1) && is.null(values)) {
        values <- parameterSet[[parameterName1]]
        if (is.null(values)) {
            stopRuntimeIssue(.getClassName(parameterSet), " does not ",
                "contain a field ", .pQuote(parameterName1), "",
                functionName = "addParameter",
                parameter = "parameterSet",
                value = parameterSet,
                relatedParameter = "parameterName1",
                relatedValue = parameterName1
            )
        }
    }

    parameterName2 <- NA_character_
    values2 <- NA_real_
    if (!is.null(parameterName) && length(parameterName) > 1) {
        parameterName2 <- parameterName[2]
        values2 <- parameterSet[[parameterName2]]
        parameterName <- parameterName[1]
        if (is.null(values2)) {
            stopRuntimeIssue(.getClassName(parameterSet), " does not ",
                "contain a field ", .pQuote(parameterName2), "",
                functionName = "addParameter",
                parameter = "parameterSet",
                value = parameterSet,
                relatedParameter = "parameterName2",
                relatedValue = parameterName2
            )
        }
    }

    if (is.null(values) && is.null(parameterName1)) {
        stopRuntimeIssue("'parameterName' or 'values' must be defined",
            functionName = "addParameter",
            parameter = "parameterName",
            value = parameterName,
            relatedParameter = "values",
            relatedValue = values
        )
    }

    transposed <- NA
    if (is.null(values) || length(values) <= 1) {
        transpose <- FALSE
    }
    if (isTRUE(transpose)) {
        if (!is.matrix(values)) {
            values <- as.matrix(values)
            if (!is.na(lastStage) && lastStage > ncol(values)) {
                nCol <- lastStage - ncol(values)
                values <- cbind(matrix(rep(NA_real_, nCol * nrow(values)), nrow = nrow(values)), values)
                if (!is.null(parameterName) && length(parameterName) == 1 &&
                        parameterName %in% c(
                            "expectedNumberOfSubjects",
                            "expectedNumberOfSubjectsH1",
                            "expectedEventsH1",
                            "expectedNumberOfEvents",
                            "expectedInformationH0",
                            "expectedInformationH01",
                            "expectedInformationH1",
                            "studyDuration",
                            "earlyStop"
                        )) {
                    transposed <- TRUE
                }
            }
        } else {
            values <- t(values)
        }
    }

    if (is.list(parameterSet) && is.matrix(values)) {
        parameterSet <- parameterSet[["parameterSet"]]
        if (is.null(parameterSet)) {
            stopRuntimeIssue("'parameterSet' must be added to list",
                functionName = "addParameter",
                parameter = "parameterSet", value = parameterSet
            )
        }
    }

    parameterNames <- ""
    numberOfVariants <- 1
    numberOfStages <- ifelse(is.matrix(values), ncol(values), length(values))
    if (inherits(parameterSet, "ParameterSet")) {
        parameterNames <- parameterSet$.getVisibleFieldNamesOrdered()
        numberOfVariants <- .getMultidimensionalNumberOfVariants(parameterSet, parameterNames)
        numberOfStages <- parameterSet$.getMultidimensionalNumberOfStages(parameterNames)
    }

    stages <- parameterSet[["stages"]]
    if (is.null(stages) && !is.null(parameterSet[[".stageResults"]])) {
        stages <- parameterSet[[".stageResults"]][["stages"]]
    }
    if (is.null(stages) && inherits(parameterSet, "ClosedCombinationTestResults")) {
        stages <- parameterSet[[".design"]][["stages"]]
    }
    if (!is.null(stages) && length(stages) > 0) {
        numberOfStages <- max(na.omit(stages))
        if (is.matrix(values) && nrow(values) > 0) {
            numberOfVariants <- nrow(values)
        }
        if (is.matrix(values) && ncol(values) > 0) {
            numberOfStages <- ncol(values)
        }
    }

    if (!is.null(parameterSet[[".piecewiseSurvivalTime"]]) &&
            isTRUE(parameterSet[[".piecewiseSurvivalTime"]]$delayedResponseEnabled)) {
        numberOfVariants <- 1
    }

    if (twoSided) {
        values <- 2 * values
    }

    caseCondition <- list(
        and1 = enforceFirstCase,
        and2 = inherits(parameterSet, "Dataset"),
        and3 = list(
            or1 = list(
                and1 = !transpose,
                and2 = numberOfVariants == 1
            ),
            or2 = list(
                and1 = !is.matrix(values),
                and2 = (!transpose && ncol(values) == 1),
                and3 = (transpose && nrow(values) == 1)
            ),
            or3 = list(
                and1 = .isTrialDesign(parameterSet),
                and2 = (numberOfStages > 1 && numberOfStages == length(values)),
                and3 = length(values) != numberOfVariants,
                and4 = length(values) == 1,
                and5 = !is.null(parameterName) && length(parameterName) == 1 &&
                    parameterName %in% c(
                        "futilityBoundsEffectScale",
                        "futilityBoundsEffectScaleLower",
                        "futilityBoundsEffectScaleUpper",
                        "futilityPerStage",
                        "earlyStop"
                    )
            )
        )
    )

    if (.isConditionTrue(caseCondition, "or", showDebugMessages = FALSE)) {
        valuesToShow <- .getSummaryValuesFormatted(
            parameterSet,
            parameterName1,
            values,
            roundDigits = roundDigits,
            ceilingEnabled = ceilingEnabled,
            cumsumEnabled = cumsumEnabled,
            smoothedZeroFormat = smoothedZeroFormat,
            formatRepeatedPValues = formatRepeatedPValues,
            roundDigitsAsInformation = roundDigitsAsInformation,
            showNA = showNA
        )

        if (parameterName1 %in% c("piControl", "overallPiControl", "overallPooledStDevs")) {
            valuesToShow <- summaryFactory$.getInnerValues(valuesToShow, transpose = TRUE)
        } else {
            valuesToShow <- summaryFactory$.getInnerValues(valuesToShow, transpose = transpose)
        }

        valuesToShow2 <- NA_real_
        if (!all(is.na(values2))) {
            valuesToShow2 <- .getSummaryValuesFormatted(parameterSet,
                parameterName1, values2,
                roundDigits = roundDigits,
                ceilingEnabled = ceilingEnabled,
                cumsumEnabled = cumsumEnabled,
                smoothedZeroFormat = smoothedZeroFormat,
                formatRepeatedPValues = formatRepeatedPValues,
                roundDigitsAsInformation = roundDigitsAsInformation,
                showNA = showNA
            )
            valuesToShow2 <- summaryFactory$.getInnerValues(valuesToShow2, transpose = transpose)
        }

        valuesToShow <- summaryFactory$.getFormattedParameterValue(valuesToShow, valuesToShow2)
        summaryFactory$addItem(parameterCaptionSingle, valuesToShow, legendEntry)
    } else {
        if (!inherits(parameterSet, "ParameterSet")) {
            stopIllegalArgument("for varied values 'parameterSet' must be an instance of ",
                "class 'ParameterSet' (was ", .getClassName(parameterSet, quote = TRUE), ")",
                functionName = "addParameter",
                parameter = "parameterSet",
                value = parameterSet
            )
        }

        if (is.na(transposed)) {
            transposed <- !transpose &&
                grepl("MultiArm|Enrichment", .getClassName(parameterSet)) &&
                (!is.matrix(values) || ncol(values) > 1)
        }

        userDefinedEffectMatrix <- FALSE
        if (grepl("MultiArm|Enrichment", .getClassName(parameterSet)) ||
                inherits(parameterSet, "AnalysisResultsConditionalDunnett") ||
                inherits(parameterSet, "ClosedCombinationTestResults") ||
                inherits(parameterSet, "ConditionalPowerResults")) {
            if (grepl("SimulationResults(MultiArm|Enrichment)", .getClassName(parameterSet)) &&
                    parameterName %in% c(
                        "rejectAtLeastOne",
                        "earlyStop",
                        "futilityPerStage",
                        "successPerStage",
                        "expectedNumberOfSubjects",
                        "expectedNumberOfEvents",
                        "singleEventsPerArmAndStage",
                        "singleEventsPerSubsetAndStage",
                        "numberOfSelectedArms",
                        "numberOfPopulations",
                        "conditionalPowerAchieved",
                        "plannedCalendarTime",
                        "analysisTime",
                        "studyDuration"
                    )) {
                transposed <- TRUE
                userDefinedEffectMatrix <-
                    parameterSet$isUserDefinedOrDerivedParameter("effectMatrix")
                if (userDefinedEffectMatrix) {
                    legendEntry <- .addLegendEntry("effectMatrix", legendEntry)
                }
                if (grepl("Survival", .getClassName(parameterSet)) &&
                        !grepl("Enrichment", .getClassName(parameterSet))) {
                    legendEntry <- .addLegendEntry("treatmentArm", legendEntry)
                }

                if (grepl("SimulationResultsEnrichment", .getClassName(parameterSet))) {
                    variedParameterName <- .getSummaryVariedParameterNameEnrichment(parameterSet)
                    variedParameterValues <- parameterSet$effectList[[variedParameterName]]
                    if (variedParameterName == "piTreatments") {
                        variedParameterCaption <- "pi(treatment)"
                    } else {
                        variedParameterCaption <- .getParameterCaption(variedParameterName)
                        if (is.matrix(variedParameterValues) && ncol(variedParameterValues) == 1) {
                            variedParameterCaption <- sub("s$", "", variedParameterCaption)
                        }
                    }
                    if (is.matrix(variedParameterValues)) {
                        numberOfVariants <- nrow(variedParameterValues)
                    } else {
                        numberOfVariants <- length(variedParameterValues)
                    }
                } else if (grepl("SimulationResultsMultiArm", .getClassName(parameterSet))) {
                    variedParameterName <- .getVariedParameterSimulationMultiArm(parameterSet)
                    variedParameterValues <- parameterSet[[variedParameterName]]
                    variedParameterCaption <- .getParameterCaption(variedParameterName)
                    numberOfVariants <- length(variedParameterValues)
                } else {
                    stopRuntimeIssue("varied parameter identification ", 
                        "is not implemented for ", .getClassName(parameterSet),
                        functionName = "addParameter",
                        parameter = "parameterSet", value = parameterSet
                    )
                }
                variedParameterCaption <- tolower(variedParameterCaption)
            } else if (summaryFactory$.isEnrichmentObject(parameterSet)) {
                transposed <- TRUE
                variedParameterCaption <- "populations"
                if (parameterName1 %in% c(
                        "indices",
                        "conditionalErrorRate",
                        "secondStagePValues",
                        "adjustedStageWisePValues",
                        "overallAdjustedTestStatistics",
                        "rejectedIntersections"
                    )) {
                    if (.isEnrichmentAnalysisResults(parameterSet)) {
                        variedParameterValues <- parameterSet$.closedTestResults$.getHypothesisPopulationVariants()
                    } else {
                        variedParameterValues <- parameterSet$.getHypothesisPopulationVariants()
                    }
                } else {
                    variedParameterValues <- c(paste0("S", 1:(numberOfVariants - 1)), "F")
                }
                numberOfVariants <- length(variedParameterValues)
                legendEntry[["S[i]"]] <- "population i"
                legendEntry[["F"]] <- "full population"
            } else if (!inherits(parameterSet, "ClosedCombinationTestResults") ||
                    parameterName %in% c("rejected", "separatePValues")) {
                if (inherits(parameterSet, "AnalysisResultsConditionalDunnett") &&
                        (!is.matrix(values) || ncol(values) > 1)) {
                    transposed <- TRUE
                }

                if (inherits(parameterSet, "ClosedCombinationTestResults") &&
                        parameterSet$isNotGeneratedParameter("adjustedStageWisePValues") &&
                        parameterName == "separatePValues") {
                    transposed <- TRUE
                }

                if (inherits(parameterSet, "ClosedCombinationTestResults") &&
                        parameterName %in% c("rejected")) {
                    transposed <- TRUE
                }

                if (inherits(parameterSet, "ConditionalPowerResults") &&
                        parameterName %in% c("conditionalPower", "values")) {
                    transposed <- TRUE
                }

                variedParameterCaption <- "arm"
                variedParameterValues <- 1:numberOfVariants
                legendEntry <- .addLegendEntry("treatmentArm", legendEntry)
            } else {
                transposed <- TRUE
                variedParameterCaption <- "arms"
                variedParameterValues <- parameterSet$.getHypothesisTreatmentArmVariants()
                numberOfVariants <- length(variedParameterValues)
                legendEntry <- .addLegendEntry("treatmentArms", legendEntry)
            }
        } else {
            if (inherits(parameterSet, "Dataset")) {
                variedParameter <- "groups"
            } else if (inherits(parameterSet, "PerformanceScore")) {
                variedParameter <- ".alternative"
            } else {
                variedParameter <- parameterSet$.getVariedParameter(parameterNames, numberOfVariants)
            }
            if (is.null(variedParameter) || length(variedParameter) == 0 || variedParameter == "") {
                if (.getLogicalEnvironmentVariable("RPACT_DEVELOPMENT_MODE")) {
                    warning(
                        "Failed to get varied parameter from ", .getClassName(parameterSet),
                        " (", length(parameterNames), " parameter names; numberOfVariants: ", numberOfVariants, ";",
                        length(variedParameter), " varied parameter values)"
                    )
                }
                return(invisible())
            }

            variedParameterCaption <- parameterSet$.getDataFrameColumnCaption(
                variedParameter,
                niceColumnNamesEnabled = TRUE
            )
            variedParameterCaption <- tolower(variedParameterCaption)

            if (variedParameterCaption == "alternative" || variedParameterCaption == ".alternative") {
                legendEntry[["alt."]] <- "alternative"
                variedParameterCaption <- "alt."
            } else if (variedParameterCaption == "hazard ratio") {
                legendEntry[["HR"]] <- "hazard ratio"
                variedParameterCaption <- "HR"
            } else if (grepl("\\(1\\)$", variedParameterCaption)) {
                groups <- parameterSet[["groups"]]
                if (!is.null(groups) && length(groups) == 1 && groups == 1) {
                    variedParameterCaption <- sub(" \\(1\\)$", "", variedParameterCaption)
                }
            }

            variedParameterValues <- round(parameterSet[[variedParameter]], 3)
        }

        for (variantIndex in 1:numberOfVariants) {
            colValues <- summaryFactory$.getColumnValues(
                parameterName, values, variantIndex, transposed)
            colValues <- .getSummaryValuesFormatted(
                parameterSet,
                parameterName1,
                values = colValues,
                roundDigits = roundDigits,
                ceilingEnabled = ceilingEnabled, cumsumEnabled = cumsumEnabled,
                smoothedZeroFormat = smoothedZeroFormat,
                formatRepeatedPValues = formatRepeatedPValues,
                roundDigitsAsInformation = roundDigitsAsInformation,
                showNA = showNA
            )
            colValues2 <- NA_real_
            if (!all(is.na(values2))) {
                colValues2 <- summaryFactory$.getColumnValues(
                    parameterName, values2, variantIndex, transposed)
                colValues2 <- .getSummaryValuesFormatted(
                    parameterSet,
                    parameterName2,
                    values = colValues2,
                    roundDigits = roundDigits,
                    ceilingEnabled = ceilingEnabled,
                    cumsumEnabled = cumsumEnabled,
                    smoothedZeroFormat = smoothedZeroFormat,
                    formatRepeatedPValues = formatRepeatedPValues,
                    roundDigitsAsInformation = roundDigitsAsInformation,
                    showNA = showNA
                )
            }
            colValues <- summaryFactory$.getFormattedParameterValue(
                valuesToShow = colValues, valuesToShow2 = colValues2)

            if (numberOfVariants == 1) {
                summaryFactory$addItem(parameterCaption, colValues, legendEntry)
            } else if (summaryFactory$.isEnrichmentObject(parameterSet)) {
                summaryFactory$addItem(paste0(
                    parameterCaption, " ",
                    variedParameterValues[variantIndex]
                ), colValues, legendEntry)
            } else if (
                (grepl("MultiArm|Enrichment", .getClassName(parameterSet)) &&
                    !grepl("Simulation", .getClassName(parameterSet))) ||
                    inherits(parameterSet, "AnalysisResultsConditionalDunnett") ||
                    inherits(parameterSet, "ClosedCombinationTestResults") ||
                    inherits(parameterSet, "ConditionalPowerResults")) {
                spacePrefix <- ifelse(parameterCaption %in% c("pi", "lambda", "median"), "", " ")
                summaryFactory$addItem(paste0(
                    parameterCaption, spacePrefix,
                    "(", variedParameterValues[variantIndex], ")"
                ), colValues, legendEntry)
            } else if (userDefinedEffectMatrix) {
                summaryFactory$addItem(paste0(parameterCaption, " [", variantIndex, "]"), colValues, legendEntry)
            } else {
                if (is.matrix(variedParameterValues) && ncol(variedParameterValues) > 1) {
                    variedParameterValuesFormatted <-
                        .arrayToString(variedParameterValues[variantIndex, ], vectorLookAndFeelEnabled = TRUE)
                } else {
                    variedParameterValuesFormatted <- variedParameterValues[variantIndex]
                }
                summaryFactory$addItem(
                    paste0(
                        parameterCaption, ", ",
                        variedParameterCaption, " = ", variedParameterValuesFormatted
                    ),
                    colValues, legendEntry
                )
            }
        }
    }
}
