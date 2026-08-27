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

    if (!.isTrialDesignGroupSequentialOrFixed(design)) {
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

