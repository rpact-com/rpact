## |
## |  *Comparison parameter information*
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

.formatComparisonParameterValues <- function(values, digits = NA_integer_) {
    if (!length(values)) {
        return(character())
    }
    if (is.numeric(values) && !is.object(values) && !is.na(digits)) {
        values <- round(values, digits)
    }
    if (is.numeric(values) && !is.object(values) && is.na(digits)) {
        for (precision in 7:17) {
            labels <- format(values, digits = precision, trim = TRUE)
            if (length(unique(labels)) == length(unique(values))) {
                return(labels)
            }
        }
    }
    as.character(values)
}

#'
#' @title
#' Get Comparison Parameter Information
#'
#' @description
#' Describes the observed values and plotting roles of comparison-data columns.
#' This analysis does not change, round, or recalculate the supplied data.
#'
#' @param data A data.frame, typically returned by .getComparisonData(). Columns
#'   must be one-dimensional atomic vectors, including factors and dates.
#' @param ... Further arguments. Currently unused.
#' @param removeConstantParameters Whether to omit columns with at most one
#'   distinct observed value. Defaults to FALSE so all parameters are available.
#' @param binaryIntegerAsFactor Whether integer-valued binary variables are also
#'   eligible for use as factors. This does not change the source column.
#'
#' @details
#' Missing values and blank character/factor entries are excluded from observed
#' levels. They are recorded in hasMissingValues. Thus a column containing one
#' value and NA is constant among observed values; it is not identical to a
#' complete column. Empty columns are constant but are not single-valued.
#' Numeric character columns are recognized when every observed entry is a
#' signed decimal or scientific numeric literal, or a comma-separated sequence
#' of these literals as exported for vector-valued parameters. Every token must
#' be numeric; incomplete or mixed text sequences remain character. Original
#' cell values are retained without parsing them as R code. dataType records the original
#' first class; paramType records the inferred plotting type. Finite, exactly
#' integral numeric values are classified as integer without integer coercion.
#' Factors and date/time classes retain their types. isFactor describes eligibility
#' for categorical use, rather than whether the original column is an R factor.
#' isContinuous and isFactor may both be TRUE for integer-valued binary data.
#'
#' Variation is evaluated across rows, including stages and situations. This is
#' not a test of differences between complete stage profiles of separate objects.
#' No numerical tolerance is applied to distinctness. Unused factor levels do
#' not count as observed values.
#'
#' @return A data.frame with one row per column: paramName, dataType, paramType,
#'   isContinuous, isFactor, isBinary, isEmpty, isSingleValue, isConstant,
#'   isVarying, hasMissingValues, paramLevelLength, and binaryValues (display text).
#'   The list columns values and singleValue preserve the original value types
#'   and precision; singleValue contains NULL except for single-valued columns.
#'   Empty inputs produce a zero-row table with the same schema.
#'
#' @examples
#' data <- data.frame(alpha = c(0.025, 0.025), effect = c(0.2, 0.3))
#' getComparisonParameterInfo(data)
#'
#' @export
#'
getComparisonParameterInfo <- function(data, ..., removeConstantParameters = FALSE,
        binaryIntegerAsFactor = TRUE) {
    .warnInCaseOfUnknownArguments(functionName = "getComparisonParameterInfo", ...)
    .assertIsSingleLogical(removeConstantParameters, "removeConstantParameters")
    .assertIsSingleLogical(binaryIntegerAsFactor, "binaryIntegerAsFactor")
    if (!is.data.frame(data) || anyNA(names(data)) || any(!nzchar(names(data))) || anyDuplicated(names(data))) {
        stopIllegalArgument("'data' must be a data.frame with unique non-empty column names",
            functionName = "getComparisonParameterInfo", parameter = "data", value = data,
            constraint = "a data.frame with unique non-empty column names",
            diagnosticId = "comparison.parameter_data_invalid")
    }
    result <- data.frame(paramName = character(), dataType = character(), paramType = character(),
        isContinuous = logical(), isFactor = logical(), isBinary = logical(),
        isEmpty = logical(), isSingleValue = logical(), isConstant = logical(),
        isVarying = logical(), hasMissingValues = logical(), paramLevelLength = integer(),
        binaryValues = character(), stringsAsFactors = FALSE)
    result$values <- list()
    result$singleValue <- list()
    if (!nrow(data) || !ncol(data)) {
        return(result)
    }
    rows <- lapply(seq_along(data), function(i) {
        x <- data[[i]]
        if (!is.atomic(x) || !is.null(dim(x))) {
            stopIllegalDataInput("Comparison columns must be one-dimensional atomic vectors",
                functionName = "getComparisonParameterInfo", parameter = "data", value = x,
                constraint = "one-dimensional atomic columns",
                diagnosticId = "comparison.parameter_column_unsupported",
                context = list(column = names(data)[i], columnPosition = i))
        }
        missingValues <- is.na(x)
        if (is.character(x) || is.factor(x)) {
            missingValues <- missingValues | trimws(as.character(x)) == ""
        }
        values <- unique(x[!missingValues])
        n <- length(values)
        dataType <- class(x)[1L]
        paramType <- dataType
        interpreted <- values
        if (is.character(x) && n) {
            # Atomic numeric vectors can be exported as comma-separated cells.
            # Inspect every token strictly; never evaluate formatted text as R code.
            tokens <- trimws(unlist(strsplit(values, ",", fixed = TRUE), use.names = FALSE))
            numericPattern <- "^[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)([eE][+-]?[0-9]+)?$"
            if (length(tokens) && !any(grepl(",[[:space:]]*$", values)) &&
                    all(grepl(numericPattern, tokens))) {
                interpreted <- as.numeric(tokens)
                paramType <- "numeric"
            }
        }
        numericValues <- (is.integer(x) || is.double(x)) && !is.object(x)
        if ((numericValues || identical(paramType, "numeric")) && n &&
                all(is.finite(interpreted)) && all(interpreted == trunc(interpreted))) {
            paramType <- "integer"
        }
        continuous <- paramType %in% c("numeric", "integer", "double") &&
            (numericValues || is.character(x))
        binary <- n == 2L
        row <- data.frame(paramName = names(data)[i], dataType = dataType, paramType = paramType,
            isContinuous = continuous,
            isFactor = !continuous || (binaryIntegerAsFactor && binary && paramType == "integer"),
            isBinary = binary, isEmpty = n == 0L, isSingleValue = n == 1L,
            isConstant = n <= 1L, isVarying = n > 1L,
            hasMissingValues = any(missingValues), paramLevelLength = as.integer(n),
            binaryValues = if (binary) {
                paste(.formatComparisonParameterValues(values), collapse = ", ")
            } else {
                NA_character_
            },
            stringsAsFactors = FALSE)
        row$values <- list(values)
        row$singleValue <- list(if (n == 1L) {
            values
        } else {
            NULL
        })
        row
    })
    result <- do.call(rbind, rows)
    if (removeConstantParameters) {
        result <- result[result$isVarying, , drop = FALSE]
    }
    rownames(result) <- NULL
    result
}

#'
#' @title
#' Select Comparison Parameters
#'
#' @description
#' Selects varying parameters, axis candidates, or grouping variables using
#' getComparisonParameterInfo(). Column order is retained.
#'
#' @inheritParams getComparisonParameterInfo
#' @param varyingOnly Whether to exclude constant and empty columns.
#' @param type One of "all", "continuous", "factor", "binary", or "groupable".
#'   Groupable columns are categorical or have at most maxNumberOfLevels observed
#'   values. An empty column is never groupable. Continuous binary columns remain
#'   eligible as continuous axes.
#' @param exclude Character vector of names to exclude, e.g., identifiers or
#'   variables already selected for an axis or a category.
#' @param maxNumberOfLevels Positive upper limit for numerical grouping variables.
#'
#' @return A character vector of column names. No plot or GUI state is changed.
#'
#' @examples
#' data <- data.frame(alpha = 0.025, effect = c(0.2, 0.3), power = c(0.8, 0.9))
#' getComparisonParameterNames(data)
#' getComparisonParameterNames(data, exclude = "effect", type = "continuous")
#'
#' @export
#'
getComparisonParameterNames <- function(data, ..., varyingOnly = TRUE, type = "all",
        exclude = character(), maxNumberOfLevels = 5L, binaryIntegerAsFactor = TRUE) {
    .warnInCaseOfUnknownArguments(functionName = "getComparisonParameterNames", ...)
    .assertIsSingleLogical(varyingOnly, "varyingOnly")
    allowed <- c("all", "continuous", "factor", "binary", "groupable")
    if (!is.character(type) || length(type) != 1L || is.na(type) || !type %in% allowed) {
        stopIllegalArgument("Unknown comparison parameter selection type",
            functionName = "getComparisonParameterNames", parameter = "type", value = type,
            constraint = "one supported selection type", diagnosticId = "comparison.parameter_selection_type_invalid",
            context = list(allowedValues = allowed))
    }
    if (!is.character(exclude) || anyNA(exclude)) {
        stopIllegalArgument("'exclude' must be a character vector without missing values",
            functionName = "getComparisonParameterNames", parameter = "exclude", value = exclude,
            constraint = "a character vector without NA", diagnosticId = "comparison.parameter_exclusions_invalid")
    }
    if ((!is.numeric(maxNumberOfLevels) || is.complex(maxNumberOfLevels)) || length(maxNumberOfLevels) != 1L ||
            is.na(maxNumberOfLevels)) {
        stopIllegalArgument("'maxNumberOfLevels' must be a positive number",
            functionName = "getComparisonParameterNames", parameter = "maxNumberOfLevels", value = maxNumberOfLevels,
            constraint = "a single number >= 1, including Inf", diagnosticId = "comparison.parameter_level_limit_invalid")
    }
    if (maxNumberOfLevels < 1) {
        stopArgumentOutOfRange("'maxNumberOfLevels' must be at least 1",
            functionName = "getComparisonParameterNames", parameter = "maxNumberOfLevels", value = maxNumberOfLevels,
            constraint = "a number in [1, Inf]", lowerBound = 1, upperBound = Inf,
            diagnosticId = "comparison.parameter_level_limit_out_of_range")
    }
    info <- getComparisonParameterInfo(data, binaryIntegerAsFactor = binaryIntegerAsFactor)
    keep <- switch(type, all = rep(TRUE, nrow(info)), continuous = info$isContinuous,
        factor = info$isFactor, binary = info$isBinary,
        groupable = !info$isEmpty & (info$isFactor | info$paramLevelLength <= maxNumberOfLevels))
    if (varyingOnly) {
        keep <- keep & info$isVarying
    }
    info$paramName[keep & !info$paramName %in% exclude]
}

#'
#' @title
#' Describe Comparison Parameters
#'
#' @description
#' Formats comparison parameter metadata for console output, summaries, and UI
#' labels. Formatting does not alter the metadata or comparison data.
#'
#' @param parameterInfo A table returned by getComparisonParameterInfo(), or a
#'   row subset of such a table.
#' @param ... Further arguments. Currently unused.
#' @param digits Optional number of decimal places for displayed values.
#' @param showDetails Whether to include the inferred type and plotting role.
#'
#' @return A character vector named by parameter names. Empty input returns
#'   character(). Empty columns are described as empty, not as single-valued.
#'
#' @examples
#' info <- getComparisonParameterInfo(data.frame(effect = c(0.2, 0.3)))
#' getComparisonParameterDescriptions(info)
#'
#' @export
#'
getComparisonParameterDescriptions <- function(parameterInfo, ..., digits = NA_integer_, showDetails = TRUE) {
    .warnInCaseOfUnknownArguments(functionName = "getComparisonParameterDescriptions", ...)
    .assertIsSingleLogical(showDetails, "showDetails")
    if ((!is.numeric(digits) || is.complex(digits)) || length(digits) != 1L || (!is.na(digits) &&
            (digits != trunc(digits)))) {
        stopIllegalArgument("'digits' must be NA or an integer from 0 to 22",
            functionName = "getComparisonParameterDescriptions", parameter = "digits", value = digits,
            constraint = "NA or an integer in [0, 22]", diagnosticId = "comparison.parameter_digits_invalid")
    }
    if (!is.na(digits) && (!is.finite(digits) || digits < 0 || digits > 22)) {
        stopArgumentOutOfRange("'digits' must be between 0 and 22",
            functionName = "getComparisonParameterDescriptions", parameter = "digits", value = digits,
            constraint = "an integer in [0, 22]", lowerBound = 0, upperBound = 22,
            diagnosticId = "comparison.parameter_digits_out_of_range")
    }
    required <- c("paramName", "paramType", "isFactor", "isContinuous", "isBinary", "isEmpty", "isSingleValue", "values")
    validInfo <- is.data.frame(parameterInfo) && all(required %in% names(parameterInfo))
    if (validInfo) {
        flags <- c("isFactor", "isContinuous", "isBinary", "isEmpty", "isSingleValue")
        validInfo <- is.character(parameterInfo$paramName) && is.character(parameterInfo$paramType) &&
            is.list(parameterInfo$values) && !anyNA(parameterInfo$paramName) && !anyNA(parameterInfo$paramType) &&
            all(vapply(parameterInfo[flags], function(x) is.logical(x) && !anyNA(x), logical(1)))
    }
    if (!validInfo) {
        stopIllegalArgument("'parameterInfo' must be a comparison parameter information table",
            functionName = "getComparisonParameterDescriptions", parameter = "parameterInfo", value = parameterInfo,
            constraint = "a table from getComparisonParameterInfo",
            diagnosticId = "comparison.parameter_info_invalid")
    }
    descriptions <- vapply(seq_len(nrow(parameterInfo)), function(i) {
        info <- parameterInfo[i, , drop = FALSE]
        detail <- character()
        if (showDetails) {
            detail <- c(info$paramType, if (info$isFactor) {
                "factor"
            } else {
                "continuous"
            })
        }
        values <- paste(.formatComparisonParameterValues(info$values[[1L]], digits), collapse = ", ")
        if (info$isEmpty) {
            detail <- c(detail, "empty")
        } else if (info$isSingleValue) {
            detail <- c(detail, paste0("single value (", values, ")"))
        } else if (info$isBinary) {
            detail <- c(detail, paste0("binary (", values, ")"))
        }
        if (!showDetails) {
            return(paste0(info$paramName, if (info$isSingleValue || info$isBinary) {
                paste0(" (", values, ")")
            } else {
                ""
            }))
        }
        paste0(info$paramName, ": ", paste(detail, collapse = ", "))
    }, character(1))
    stats::setNames(descriptions, parameterInfo$paramName)
}
