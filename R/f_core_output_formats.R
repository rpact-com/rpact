## |
## |  *Output formats*
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
## |  File version: $Revision: 7742 $
## |  Last changed: $Date: 2024-03-22 13:46:29 +0100 (Fr, 22 Mrz 2024) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_core_constants.R
#' @include f_core_utilities.R
NULL

C_ROUND_FUNCTIONS <- c("ceiling", "floor", "trunc", "round", "signif")

C_OUTPUT_FORMAT_ARGUMENTS <- c(
    "digits", "nsmall", "trimSingleZeros",
    "futilityProbabilityEnabled", "roundFunction"
)

C_OUTPUT_FORMAT_DEFAULT_VALUES <- pairlist(
    "rpact.output.format.p.value" = "digits = 4, nsmall = 4",
    "rpact.output.format.repeated.p.value" = "digits = 4, nsmall = 4",
    "rpact.output.format.probability" = "digits = 3, nsmall = 3",
    "rpact.output.format.futility.probability" = "digits = 4, nsmall = 4, futilityProbabilityEnabled = TRUE",
    "rpact.output.format.sample.size" = "digits = 1, nsmall = 1",
    "rpact.output.format.event" = "digits = 1, nsmall = 1, trimSingleZeros = TRUE",
    "rpact.output.format.event.time" = "digits = 3, trimSingleZeros = TRUE",
    "rpact.output.format.conditional.power" = "digits = 4",
    "rpact.output.format.critical.value" = "digits = 3, nsmall = 3",
    "rpact.output.format.critical.value.fisher" = "digits = 4",
    "rpact.output.format.test.statistic.fisher" = "digits = 4",
    "rpact.output.format.test.statistic" = "digits = 3, nsmall = 3",
    "rpact.output.format.rate" = "digits = 3, nsmall = 3",
    "rpact.output.format.rate1" = "digits = 1, nsmall = 1",
    "rpact.output.format.accrual.intensity" = "digits = 2, nsmall = 1",
    "rpact.output.format.mean" = "digits = 4",
    "rpact.output.format.ratio" = "digits = 3",
    "rpact.output.format.st.dev" = "digits = 4",
    "rpact.output.format.duration" = "digits = 2, nsmall = 2",
    "rpact.output.format.time" = "digits = 2, nsmall = 2"
)

.getFormattedValue <- function(value, ..., digits, nsmall = NA_integer_,
        futilityProbabilityEnabled = FALSE, roundFunction = NA_character_, scientific = NA,
        trimEndingZerosAfterDecimalPoint = FALSE) {
    if (missing(value)) {
        return("NA")
    }

    if (is.null(value) || length(value) == 0) {
        return(value)
    }

    if (!is.numeric(value)) {
        stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'value' must be a numeric vector")
    }

    if (futilityProbabilityEnabled) {
        value[value >= 0 & value < 1e-09] <- 0 # only futility probilities
    }

    if (!is.na(roundFunction)) {
        if (roundFunction == "ceiling") {
            value <- ceiling(value * 10^digits) / 10^digits
        } else if (roundFunction == "floor") {
            value <- floor(value * 10^digits) / 10^digits
        } else if (roundFunction == "trunc") {
            value <- trunc(value)
        } else if (roundFunction == "round ") {
            value <- round(value, digits = digits)
        } else if (roundFunction == "signif ") {
            value <- signif(value, digits = digits)
        }
    }

    if (is.na(nsmall)) {
        nsmall <- 0L
    }

    formattedValue <- format(value,
        digits = digits, nsmall = nsmall,
        scientific = scientific, justify = "left", trim = TRUE
    )

    if ((is.na(scientific) || scientific) && any(grepl("e", formattedValue))) {
        formattedValueTemp <- c()
        for (valueTemp in value) {
            if (!is.na(scientific) && !scientific && digits > 0 && nsmall == 0) {
                maxValue <- 1 / 10^digits
                if (valueTemp < maxValue) {
                    valueTemp <- paste0("<", maxValue)
                }
            } else {
                valueTemp <- format(valueTemp,
                    digits = digits, nsmall = nsmall,
                    scientific = scientific, justify = "left", trim = TRUE
                )
            }
            formattedValueTemp <- c(formattedValueTemp, valueTemp)
        }
        formattedValue <- formattedValueTemp
    }

    if (futilityProbabilityEnabled) {
        formattedValue[value == 0] <- "0"
    }

    if (trimEndingZerosAfterDecimalPoint) {
        formattedValue <- gsub("\\.0+$", "", formattedValue)
    }

    return(formattedValue)
}

.getZeroCorrectedValue <- function(value) {
    if (is.numeric(value)) {
        value[abs(value) < 1e-08] <- 0
    }
    return(value)
}

.getPValueDecimalPlaces <- function(value) {
    value <- stats::na.omit(value)
    if (length(value) == 0) {
        return(4)
    }

    fv <- .getFormattedValue(value[value >= 1e-04], digits = 4, nsmall = 4)
    fv <- fv[!((1:length(fv)) %in% grep("e", fv))]
    numberOfCharacters <- ifelse(length(fv) > 0, nchar(fv[1]), 6)
    numberOfCharacters <- ifelse(numberOfCharacters < 6, 6, numberOfCharacters)
    decimalPlaces <- numberOfCharacters - 2
    return(decimalPlaces)
}

.assertIsValitOutputFormatOptionValue <- function(optionKey, optionValue) {
    if (is.null(optionValue) || length(optionValue) == 0 || nchar(trimws(optionValue)) == 0) {
        return(invisible())
    }

    parts <- base::strsplit(optionValue, " *, *", fixed = FALSE)[[1]]
    if (length(parts) == 0) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "the value (", optionValue, ") of output format option '", optionKey, "' is invalid"
        )
    }

    for (part in parts) {
        if (!grepl(" *= *", part)) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", optionKey, "' (", part,
                ") must contain a valid argument-value-pair: \"argument = value\""
            )
        }

        keyValuePair <- base::strsplit(part, " *= *", fixed = FALSE)[[1]]
        if (length(keyValuePair) != 2) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", optionKey,
                "' contains an invalid argument-value-pair: ", part
            )
        }

        key <- trimws(keyValuePair[1])
        if (nchar(key) == 0) {
            stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", optionKey, "' contains an invalid argument")
        }

        if (!(key %in% C_OUTPUT_FORMAT_ARGUMENTS)) {
            stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", optionKey, "' contains an invalid argument: ", key)
        }

        value <- trimws(keyValuePair[2])
        if (nchar(value) == 0) {
            stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", optionKey, "' contains an invalid value")
        }

        if (key %in% c("digits", "nsmall")) {
            if (grepl("\\D", value)) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "the value (", value, ") of '", optionKey, "' must be an integer value"
                )
            }
        } else if (key %in% c("roundFunction")) {
            if (!(value %in% C_ROUND_FUNCTIONS)) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "the value (", value, ") of '", optionKey, "' must be one of these character values: ",
                    .arrayToString(C_ROUND_FUNCTIONS, encapsulate = TRUE)
                )
            }
        } else if (key %in% c("trimSingleZeros", "futilityProbabilityEnabled")) {
            if (!grepl("TRUE|FALSE", toupper(value))) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "the value (", value, ") of '", optionKey, "' must be a logical value"
                )
            }
        }
    }
}

.assertIsValitOutputFormatOptionValue("rpact.output.format.sample.size", "roundFunction = ceiling")

.getOutputFormatOptions <- function(optionKey) {
    str <- getOption(optionKey)
    if (is.null(str) || length(str) == 0 || nchar(trimws(str)) == 0) {
        return(NULL)
    }

    parts <- base::strsplit(str, " *, *", fixed = FALSE)[[1]]
    if (length(parts) == 0) {
        return(NULL)
    }

    result <- list()
    for (part in parts) {
        .assertIsValitOutputFormatOptionValue(optionKey, optionValue = part)
        keyValuePair <- base::strsplit(part, " *= *", fixed = FALSE)[[1]]
        key <- trimws(keyValuePair[1])
        value <- trimws(keyValuePair[2])
        if (key %in% c("digits", "nsmall")) {
            value <- as.integer(value)
        } else if (key %in% c("trimSingleZeros", "futilityProbabilityEnabled")) {
            value <- as.logical(value)
        }
        result[[key]] <- value
    }
    return(result)
}

.getOptionBasedFormattedValue <- function(optionKey, value, digits, nsmall = NA_integer_,
        trimSingleZeros = FALSE, futilityProbabilityEnabled = FALSE, roundFunction = NA_character_) {
    outputFormatOptions <- .getOutputFormatOptions(optionKey)
    if (is.null(outputFormatOptions) || length(outputFormatOptions) == 0) {
        return(NULL)
    }

    if (!is.null(outputFormatOptions[["digits"]])) {
        digits <- outputFormatOptions[["digits"]]
    }
    if (!is.null(outputFormatOptions[["nsmall"]])) {
        nsmall <- outputFormatOptions[["nsmall"]]
    }
    if (!is.null(outputFormatOptions[["trimSingleZeros"]])) {
        trimSingleZeros <- outputFormatOptions[["trimSingleZeros"]]
    }
    if (!is.null(outputFormatOptions[["futilityProbabilityEnabled"]])) {
        futilityProbabilityEnabled <- outputFormatOptions[["futilityProbabilityEnabled"]]
    }
    if (!is.null(outputFormatOptions[["roundFunction"]])) {
        roundFunction <- outputFormatOptions[["roundFunction"]]
    }

    if (trimSingleZeros) {
        value <- .getZeroCorrectedValue(value)
    }

    return(.getFormattedValue(value,
        digits = digits, nsmall = nsmall,
        futilityProbabilityEnabled = futilityProbabilityEnabled, roundFunction = roundFunction
    ))
}


#
# @title
# Format P Values
#
# @description
# Formats the output of p-values.
#
# @details
# Digits = 4, nsmall = 4.
# Replaces p-values in scientific format (e.g., 1e-07) by a non-scientific format (e.g., <0.00001).
#
# @param value a vector of p-values.
#
.formatPValues <- function(value) {
    if (sum(is.na(value)) == length(value)) {
        return(value)
    }

    x <- .getOptionBasedFormattedValue("rpact.output.format.p.value",
        value = value, digits = 4, nsmall = 4
    )
    if (!is.null(x)) {
        return(x)
    }

    decimalPlaces <- .getPValueDecimalPlaces(value)
    if (is.na(decimalPlaces) || is.nan(decimalPlaces)) {
        decimalPlaces <- 4
    } else if (decimalPlaces > 4) {
        decimalPlaces <- decimalPlaces - 1
    }

    threshold <- 10^-decimalPlaces
    text <- "<0."
    for (i in 1:(decimalPlaces - 1)) {
        text <- paste0(text, "0")
    }
    text <- paste0(text, "1")

    indices <- (value < threshold)
    value[indices] <- threshold
    formattedValue <- .getFormattedValue(value, digits = 4, nsmall = 4)
    formattedValue[indices] <- text
    return(formattedValue)
}

#
# @title
# Format Repeated P Values
#
# @description
# Formats the output of repeated p-values.
#
# @details
# If p-value > 0.4999 then ">=0.5" will be returned.
#
# @param value a vector of p-values.
#
.formatRepeatedPValues <- function(value) {
    x <- .getOptionBasedFormattedValue("rpact.output.format.repeated.p.value",
        value = value, digits = 4, nsmall = 4
    )
    if (!is.null(x)) {
        return(x)
    }
    pValues <- .formatPValues(value)
    pValues[value > 0.4999] <- ">0.5"
    return(pValues)
}

#
# @title
# Format Probabilities
#
# @description
# Formats the output of probabilities.
#
# @details
# Digits = 4, nsmall = 4
#
.formatProbabilities <- function(value) {
    x <- .getOptionBasedFormattedValue("rpact.output.format.probability",
        value = value, digits = 4, nsmall = 4
    )
    if (!is.null(x)) {
        return(x)
    }
    value[abs(value) < 1e-08] <- 0
    return(.getFormattedValue(value, digits = 4, nsmall = 4))
}

#
# @title
# Format Sample Sizes
#
# @description
# Formats the output of sample sizes.
#
# @details
# Digits = 1, nsmall = 1
#
.formatSampleSizes <- function(value) {
    x <- .getOptionBasedFormattedValue("rpact.output.format.sample.size",
        value = value, digits = 1, nsmall = 1, trimSingleZeros = TRUE
    )
    if (!is.null(x)) {
        return(x)
    }

    return(.getFormattedValue(.getZeroCorrectedValue(value), digits = 1, nsmall = 1, trimEndingZerosAfterDecimalPoint = TRUE))
}

#
# @title
# Format Events
#
# @description
# Formats the output of events.
#
# @details
# Digits = 1, nsmall = 1, trimSingleZeros = TRUE
#
.formatEvents <- function(value) {
    x <- .getOptionBasedFormattedValue("rpact.output.format.event",
        value = value, digits = 1, nsmall = 1, trimSingleZeros = TRUE
    )
    if (!is.null(x)) {
        return(x)
    }
    return(.getFormattedValue(.getZeroCorrectedValue(value), digits = 1, nsmall = 1, trimEndingZerosAfterDecimalPoint = TRUE))
}

#
# @title
# Format Conditional Power
#
# @description
# Formats the output of contional power.
#
# @details
# Digits = 4
#
.formatConditionalPower <- function(value) {
    x <- .getOptionBasedFormattedValue("rpact.output.format.conditional.power",
        value = value, digits = 4
    )
    if (!is.null(x)) {
        return(x)
    }

    value <- round(value, digits = 4)
    conditionalPower <- .getFormattedValue(value, digits = 4)
    conditionalPower[value == 0] <- "0"
    return(conditionalPower)
}

#
# @title
# Format Futility Probabilities
#
# @description
# Formats the output of futility probabilities.
#
# @details
# Digits = 4, nsmall = 4
#
.formatFutilityProbabilities <- function(value) {
    x <- .getOptionBasedFormattedValue("rpact.output.format.futility.probability",
        value = value, digits = 4, nsmall = 4, futilityProbabilityEnabled = TRUE
    )
    if (!is.null(x)) {
        return(x)
    }
    return(.getFormattedValue(value, digits = 4, nsmall = 4, futilityProbabilityEnabled = TRUE))
}

#
# @title
# Format Group Sequential Critical Values
#
# @description
# Formats the output of group sequential critical values.
#
# @details
# Digits = 3, nsmall = 3
#
.formatCriticalValues <- function(value) {
    value[value == C_FUTILITY_BOUNDS_DEFAULT] <- -Inf
    x <- .getOptionBasedFormattedValue("rpact.output.format.critical.value",
        value = value, digits = 3, nsmall = 3
    )
    if (!is.null(x)) {
        return(x)
    }
    return(.getFormattedValue(value, digits = 3, nsmall = 3))
}

#
# @title
# Format Fisher Critical Values
#
# @description
# Formats the output of Fisher's combination critical values.
#
# @details
# Digits = 4
#
.formatCriticalValuesFisher <- function(value) {
    x <- .getOptionBasedFormattedValue("rpact.output.format.critical.value.fisher",
        value = value, digits = 4
    )
    if (!is.null(x)) {
        return(x)
    }
    return(.getFormattedValue(value, digits = 4))
}

#
# @title
# Format Fisher Test Statistics
#
# @description
# Formats the output of Fisher's combination test statistics.
#
# @details
# Digits = 4
#
.formatTestStatisticsFisher <- function(value) {
    x <- .getOptionBasedFormattedValue("rpact.output.format.test.statistic.fisher",
        value = value, digits = 4
    )
    if (!is.null(x)) {
        return(x)
    }
    return(.getFormattedValue(value, digits = 4)) # , scientific = FALSE
}

#
# @title
# Format Test Statistics
#
# @description
# Formats the output of test statistics (e.g., inverse normal).
#
# @details
# Digits = 3, nsmall = 3
#
.formatTestStatistics <- function(value) {
    x <- .getOptionBasedFormattedValue("rpact.output.format.test.statistic",
        value = value, digits = 3, nsmall = 3
    )
    if (!is.null(x)) {
        return(x)
    }
    return(.getFormattedValue(value, digits = 3, nsmall = 3)) # , scientific = FALSE
}

#
# @title
# Format Rates
#
# @description
# Formats the output of rates.
#
# @details
# Digits = 3, nsmall = 3
#
.formatRates <- function(value) {
    x <- .getOptionBasedFormattedValue("rpact.output.format.rate",
        value = value, digits = 3, nsmall = 3
    )
    if (!is.null(x)) {
        return(x)
    }
    return(.getFormattedValue(value, digits = 3, nsmall = 3))
}

#
# @title
# Format Rates Dynamic
#
# @description
# Formats the output of rates.
#
# @details
# Digits = 3, nsmall = 3 if value < 1; digits = 1, nsmall = 1 otherwise
#
.formatRatesDynamic <- function(value) {
    if (!any(is.na(value)) && all(value >= 1)) {
        x <- .getOptionBasedFormattedValue("rpact.output.format.rate1",
            value = value, digits = 1, nsmall = 1
        )
        if (!is.null(x)) {
            return(x)
        }
        return(.getFormattedValue(value, digits = 1, nsmall = 1))
    }
    x <- .getOptionBasedFormattedValue("rpact.output.format.rate",
        value = value, digits = 3, nsmall = 3
    )
    if (!is.null(x)) {
        return(x)
    }
    return(.getFormattedValue(value, digits = 3, nsmall = 3))
}

#
# @title
# Format Accrual Intensities
#
# @description
# Formats the output of accrual intensities.
#
# @details
# Digits = 1, nsmall = 1
#
.formatAccrualIntensities <- function(value) {
    x <- .getOptionBasedFormattedValue("rpact.output.format.accrual.intensity",
        value = value, digits = 2, nsmall = 1
    )
    if (!is.null(x)) {
        return(x)
    }
    return(.getFormattedValue(value, digits = 2, nsmall = 1))
}

#
# @title
# Format Means
#
# @description
# Formats the output of means.
#
# @details
# Digits = 4
#
.formatMeans <- function(value) {
    x <- .getOptionBasedFormattedValue("rpact.output.format.mean",
        value = value, digits = 4
    )
    if (!is.null(x)) {
        return(x)
    }
    return(.getFormattedValue(value, digits = 4))
}

#
# @title
# Format Ratios
#
# @description
# Formats the output of ratios.
#
# @details
# Digits = 3
#
.formatRatios <- function(value) {
    x <- .getOptionBasedFormattedValue("rpact.output.format.ratio",
        value = value, digits = 3
    )
    if (!is.null(x)) {
        return(x)
    }
    return(.getFormattedValue(value, digits = 3))
}

#
# @title
# Format StDevs
#
# @description
# Formats the output of standard deviations.
#
# @details
# Digits = 4
#
.formatStDevs <- function(value) {
    x <- .getOptionBasedFormattedValue("rpact.output.format.st.dev",
        value = value, digits = 4
    )
    if (!is.null(x)) {
        return(x)
    }
    return(.getFormattedValue(value, digits = 4))
}

#
# @title
# Format Durations
#
# @description
# Formats the output of study durations.
#
# @details
# Digits = 3
#
.formatDurations <- function(value) {
    x <- .getOptionBasedFormattedValue("rpact.output.format.duration",
        value = value, digits = 2, nsmall = 2
    )
    if (!is.null(x)) {
        return(x)
    }
    return(.getFormattedValue(value, digits = 2, nsmall = 2))
}

#
# @title
# Format Time
#
# @description
# Formats the output of time values, e.g. months.
#
# @details
# Digits = 3
#
.formatTime <- function(value) {
    x <- .getOptionBasedFormattedValue("rpact.output.format.time",
        value = value, digits = 2, nsmall = 2
    )
    if (!is.null(x)) {
        return(x)
    }
    return(.getFormattedValue(value, digits = 2, nsmall = 2))
}

#
# @title
# Format Time
#
# @description
# Formats the output of time values, e.g. months.
#
# @details
# Digits = 3
#
.formatEventTime <- function(value) {
    x <- .getOptionBasedFormattedValue("rpact.output.format.event.time",
        value = value, digits = 3, trimSingleZeros = TRUE
    )
    if (!is.null(x)) {
        return(x)
    }
    return(.getFormattedValue(.getZeroCorrectedValue(value), digits = 3))
}

.formatHowItIs <- function(value) {
    return(format(value, scientific = FALSE))
}

.getFormattedVariableName <- function(name, n, prefix = "", postfix = "") {
    if (!is.character(name)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'name' must be of type 'character' (is '", .getClassName(name), "')"
        )
    }

    if (!is.numeric(n)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'n' must be of type 'numeric' (is '", .getClassName(n), "')")
    }

    if (n < 1 || n > 300) {
        stop(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS, "'n' (", n, ") is out of bounds [1; 300]")
    }

    if (nchar(prefix) > 0) {
        name <- paste(prefix, name)
    }

    if (nchar(postfix) > 0) {
        name <- paste0(name, postfix)
    }

    while (nchar(name) < n) {
        name <- paste0(name, " ")
    }

    name <- paste0("  ", name, " :")

    return(name)
}

#' @title
#' Set Output Format
#'
#' @description
#' With this function the format of the standard outputs of all \code{rpact}
#' objects can be changed and set user defined respectively.
#'
#' @param parameterName The name of the parameter whose output format shall be edited.
#'        Leave the default \code{NA_character_} if
#'        the output format of all parameters shall be edited.
#' @param digits How many significant digits are to be used for a numeric value.
#'        The default, \code{NULL}, uses getOption("digits").
#'        Allowed values are \code{0 <= digits <= 20}.
#' @param nsmall The minimum number of digits to the right of the decimal point in
#'        formatting real numbers in non-scientific formats.
#'        Allowed values are \code{0 <= nsmall <= 20}.
#' @param trimSingleZeros If \code{TRUE} zero values will be trimmed in the output, e.g.,
#'        "0.00" will displayed as "0"
#' @param futilityProbabilityEnabled If \code{TRUE} very small value (< 1e-09) will
#'        be displayed as "0", default is \code{FALSE}.
#' @param file An optional file name of an existing text file that contains output format definitions
#'        (see Details for more information).
#' @param resetToDefault If \code{TRUE} all output formats will be reset to default value.
#'        Note that other settings will be executed afterwards if specified, default is \code{FALSE}.
#' @param roundFunction A character value that specifies the R base round function
#'        to use, default is \code{NA_character_}.
#'        Allowed values are "ceiling", "floor", "trunc", "round", "signif", and \code{NA_character_}.
#' @inheritParams param_three_dots
#'
#' @details
#' Output formats can be written to a text file (see \code{\link[=getOutputFormat]{getOutputFormat()}}).
#' To load your personal output formats read a formerly saved file at the beginning of your
#' work with \code{rpact}, e.g. execute \code{setOutputFormat(file = "my_rpact_output_formats.txt")}.
#'
#' Note that the \code{parameterName} must not match exactly, e.g., for p-values the
#' following parameter names will be recognized amongst others:
#' \enumerate{
#'   \item \code{p value}
#'   \item \code{p.values}
#'   \item \code{p-value}
#'   \item \code{pValue}
#'   \item \code{rpact.output.format.p.value}
#' }
#'
#' @seealso \code{\link[base]{format}} for details on the
#'          function used internally to format the values.
#'
#' @template examples_set_output_format
#'
#' @family output formats
#'
#' @export
#'
setOutputFormat <- function(parameterName = NA_character_, ...,
        digits = NA_integer_,
        nsmall = NA_integer_,
        trimSingleZeros = NA,
        futilityProbabilityEnabled = NA,
        file = NA_character_,
        resetToDefault = FALSE,
        roundFunction = NA_character_) {
    .assertIsCharacter(parameterName, "parameterName", naAllowed = TRUE)
    .assertIsSingleInteger(digits, "digits", naAllowed = TRUE, validateType = FALSE)
    .assertIsInClosedInterval(digits, "digits", lower = 0, upper = 20, naAllowed = TRUE)
    .assertIsSingleInteger(nsmall, "nsmall", naAllowed = TRUE, validateType = FALSE)
    .assertIsInClosedInterval(nsmall, "nsmall", lower = 0, upper = 20, naAllowed = TRUE)
    .assertIsSingleLogical(trimSingleZeros, "trimSingleZeros", naAllowed = TRUE)
    .assertIsSingleLogical(futilityProbabilityEnabled, "futilityProbabilityEnabled", naAllowed = TRUE)
    .assertIsSingleCharacter(file, "file", naAllowed = TRUE)
    .assertIsSingleLogical(resetToDefault, "resetToDefault")
    .assertIsSingleCharacter(roundFunction, "roundFunction", naAllowed = TRUE)

    .warnInCaseOfUnknownArguments(functionName = "setOutputFormat", ...)

    if (resetToDefault) {
        .resetAllOutputFormats()
    }

    if (!is.na(file)) {
        if (!file.exists(file)) {
            stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'file' (", file, ") does not exist")
        }

        args <- list()
        outputFormatLines <- .readLinesFromFile(file)
        counter <- 0
        for (line in outputFormatLines) {
            if (!grepl("^ *#", line)) {
                keyValuePair <- base::strsplit(line, " *: *", fixed = FALSE)[[1]]
                if (length(keyValuePair) == 2) {
                    key <- .getOutputFormatKey(keyValuePair[1], silent = TRUE)
                    if (!is.null(key)) {
                        value <- trimws(keyValuePair[2])
                        .assertIsValitOutputFormatOptionValue(optionKey = key, optionValue = value)
                        if (grepl("digits|nsmall|trimSingleZeros|futilityProbabilityEnabled", value)) {
                            args[[key]] <- value
                        } else {
                            warning('Line "', line, '" contains an invalid value: ', value)
                        }
                    } else {
                        warning('Line "', line, '" contains an invalid key: ', keyValuePair[1])
                    }
                } else if (nchar(trimws(line)) > 0) {
                    warning('Line "', line, '" does not contain a valid key-value-pair')
                }
                if (nchar(trimws(line)) > 0) {
                    counter <- counter + 1
                }
            }
        }
        if (length(args) > 0) {
            base::options(args)
            cat(length(args), " (of ", counter, " defined) output format", ifelse(length(args) == 1, "", "s"),
                " successfully set via file\n",
                sep = ""
            )
        }
    }

    if (!all(is.na(parameterName))) {
        for (param in parameterName) {
            key <- .getOutputFormatKeyByFieldName(param)
            if (is.null(key)) {
                key <- .getOutputFormatKey(param)
            }
            cmds <- c()
            if (!is.na(digits)) {
                cmds <- c(cmds, paste0("digits = ", digits))
            }
            if (!is.na(nsmall)) {
                cmds <- c(cmds, paste0("nsmall = ", nsmall))
            }
            if (!is.na(trimSingleZeros)) {
                cmds <- c(cmds, paste0("trimSingleZeros = ", trimSingleZeros))
            }
            if (!is.na(futilityProbabilityEnabled)) {
                cmds <- c(cmds, paste0("futilityProbabilityEnabled = ", futilityProbabilityEnabled))
            }
            if (!is.na(roundFunction)) {
                cmds <- c(cmds, paste0("roundFunction = ", roundFunction))
            }
            cmd <- NULL
            resetPrefix <- ""
            if (length(cmds) > 0) {
                cmd <- paste0(cmds, collapse = ", ")
            } else {
                cmd <- C_OUTPUT_FORMAT_DEFAULT_VALUES[[key]]
                resetPrefix <- "re"
            }
            args <- list()
            args[[key]] <- cmd
            base::options(args)
            cat("Output format successfully ", resetPrefix, 'set: "', key, '" = "', cmd, '"\n', sep = "")
            fields <- .getOutputFormatParameterNames(key)
            if (!is.null(fields) && length(fields) > 0) {
                if (length(fields) == 1) {
                    cat("This output format affects the following parameter:", fields, "\n")
                } else {
                    cat("This output format affects ", length(fields),
                        " parameters: ", .arrayToString(fields), "\n",
                        sep = ""
                    )
                }
            } else {
                warning("The output format ", key, " affects no parameters", call. = FALSE)
            }
        }
    }
}

.getOutputFormatKey <- function(parameterName, silent = FALSE) {
    .assertIsSingleCharacter(parameterName, "parameterName")

    if (grepl("^rpact\\.output\\.format\\.[a-z1\\.]*", parameterName)) {
        value <- C_OUTPUT_FORMAT_DEFAULT_VALUES[[parameterName]]
        if (is.null(value)) {
            if (silent) {
                return(NULL)
            }

            stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'parameterName' (", parameterName, ") does not exist")
        }

        return(parameterName)
    }

    x <- tolower(parameterName)
    keys <- names(C_OUTPUT_FORMAT_DEFAULT_VALUES)
    for (key in keys) {
        keyRegex <- sub("^rpact\\.output\\.format\\.", "", key)
        keyRegex <- gsub("\\.asn$", ".(asn|average.sample.number)", keyRegex)
        keyRegex <- gsub("^simulation\\.result$", "simulation.(results?)?", keyRegex)
        keyRegex <- gsub("^st\\.", "st(andard)?.", keyRegex)
        keyRegex <- gsub("\\.dev$", ".dev(iation)?", keyRegex)
        keyRegex <- gsub("\\.", " ?(\\.|-)? ?", keyRegex)
        keyRegex <- gsub("1", "s? ?(\\.|-)? ?1", keyRegex)
        keyRegex <- sub("y$", "(y|ies)", keyRegex)
        if (grepl("(e|t|c|n|o)$", keyRegex)) {
            keyRegex <- paste0(keyRegex, "s?")
        }
        keyRegex <- paste0("^", keyRegex, "$")
        if (grepl(keyRegex, x)) {
            return(key)
        }
    }

    if (silent) {
        return(NULL)
    }

    stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "output format key for 'parameterName' (", parameterName, ") could not be found")
}

.writeOutputFormatsToFile <- function(outputFormatList, file) {
    outputFormatLines <- c()
    outputFormatLines <- c(outputFormatLines, "##")
    outputFormatLines <- c(outputFormatLines, "## rpact output formats")
    outputFormatLines <- c(outputFormatLines, "## www.rpact.com")
    outputFormatLines <- c(outputFormatLines, paste0("## creation date: ", format(Sys.time(), "%d %b %Y, %X")))
    outputFormatLines <- c(outputFormatLines, "##")
    for (key in names(outputFormatList)) {
        outputFormatLines <- c(outputFormatLines, paste(key, ":", outputFormatList[[key]]))
    }
    .writeLinesToFile(outputFormatLines, file)
    cat(length(outputFormatList), " output format", ifelse(length(args) == 1, "", "s"),
        " successfully written to file\n",
        sep = ""
    )
}

#' @title
#' Get Output Format
#'
#' @description
#' With this function the format of the standard outputs of all \code{rpact}
#' objects can be shown and written to a file.
#'
#' @param parameterName The name of the parameter whose output format shall be returned.
#'        Leave the default \code{NA_character_} if
#'        the output format of all parameters shall be returned.
#' @param file An optional file name where to write the output formats
#'        (see Details for more information).
#' @param default If \code{TRUE} the default output format of the specified parameter(s)
#'        will be returned, default is \code{FALSE}.
#' @param fields If \code{TRUE} the names of all affected object fields will be displayed, default is \code{TRUE}.
#' @inheritParams param_three_dots
#'
#' @details
#' Output formats can be written to a text file by specifying a \code{file}.
#' See \code{\link[=setOutputFormat]{setOutputFormat()}}() to learn how to read a formerly saved file.
#'
#' Note that the \code{parameterName} must not match exactly, e.g., for p-values the
#' following parameter names will be recognized amongst others:
#' \enumerate{
#'   \item \code{p value}
#'   \item \code{p.values}
#'   \item \code{p-value}
#'   \item \code{pValue}
#'   \item \code{rpact.output.format.p.value}
#' }
#'
#' @return A named list of output formats.
#'
#' @template examples_set_output_format
#'
#' @family output formats
#'
#' @export
#'
getOutputFormat <- function(parameterName = NA_character_, ...,
        file = NA_character_, default = FALSE, fields = TRUE) {
    if (all(is.na(parameterName)) || length(parameterName) <= 1) {
        return(.getOutputFormat(
            parameterName = parameterName,
            file = file, default = default, fields = fields, ...
        ))
    }

    .assertIsSingleCharacter(file, "file", naAllowed = TRUE)
    .assertIsSingleLogical(fields, "fields")
    results <- c()
    currentOutputFormats <- c()
    for (p in parameterName) {
        results <- c(results, .getOutputFormat(
            parameterName = p,
            file = NA_character_, default = default, fields = fields, ...
        ))
        if (!is.na(file)) {
            currentOutputFormats <- c(
                currentOutputFormats,
                .getOutputFormat(
                    parameterName = p,
                    file = NA_character_, default = default, fields = FALSE, ...
                )
            )
        }
    }
    if (!is.na(file)) {
        .writeOutputFormatsToFile(currentOutputFormats, file)
    }
    return(results)
}

.getOutputFormat <- function(parameterName = NA_character_, ...,
        file = NA_character_, default = FALSE, fields = TRUE) {
    .assertIsSingleCharacter(parameterName, "parameterName", naAllowed = TRUE)
    .assertIsSingleCharacter(file, "file", naAllowed = TRUE)
    .assertIsSingleLogical(default, "default")
    .assertIsSingleLogical(fields, "fields")
    .warnInCaseOfUnknownArguments(functionName = "getOutputFormat", ...)

    currentOutputFormats <- pairlist()
    if (is.na(parameterName)) {
        if (default) {
            currentOutputFormats <- C_OUTPUT_FORMAT_DEFAULT_VALUES
        } else {
            for (key in names(C_OUTPUT_FORMAT_DEFAULT_VALUES)) {
                currentOutputFormats[[key]] <- getOption(key,
                    default = C_OUTPUT_FORMAT_DEFAULT_VALUES[[key]]
                )
            }
        }
        if (!is.na(file)) {
            .writeOutputFormatsToFile(currentOutputFormats, file)
            return(invisible(.addFieldsToOutputFormatList(currentOutputFormats, fields)))
        }
        return(.addFieldsToOutputFormatList(currentOutputFormats, fields))
    }

    key <- .getOutputFormatKey(parameterName)
    if (default) {
        value <- C_OUTPUT_FORMAT_DEFAULT_VALUES[[key]]
    } else {
        value <- getOption(key, default = C_OUTPUT_FORMAT_DEFAULT_VALUES[[key]])
    }
    currentOutputFormats[[key]] <- value
    if (!is.na(file)) {
        .writeOutputFormatsToFile(currentOutputFormats, file)
    }
    return(.addFieldsToOutputFormatList(currentOutputFormats, fields))
}

.addFieldsToOutputFormatList <- function(outputFormatList, fields = TRUE) {
    if (!fields) {
        return(outputFormatList)
    }

    results <- list()
    for (key in names(outputFormatList)) {
        results[[key]] <- list(
            format = outputFormatList[[key]],
            fields = .getOutputFormatParameterNames(key)
        )
    }
    return(results)
}

.getOutputFormatParameterNames <- function(key) {
    functionName <- .getOutputFormatFunctionName(key)
    if (is.null(functionName)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'key' (", key, ") does not exist")
    }

    parameterNames <- c()
    for (parameterName in names(C_PARAMETER_FORMAT_FUNCTIONS)) {
        if (functionName == C_PARAMETER_FORMAT_FUNCTIONS[[parameterName]]) {
            parameterNames <- c(parameterNames, parameterName)
        }
    }

    if (key == "rpact.output.format.rate") {
        return(c(parameterNames, .getOutputFormatParameterNames("rpact.output.format.rate1")))
    }

    return(parameterNames)
}

.getOutputFormatFunctionName <- function(key) {
    if (key == "rpact.output.format.p.value") {
        return(".formatPValues")
    }
    if (key == "rpact.output.format.repeated.p.value") {
        return(".formatRepeatedPValues")
    }
    if (key == "rpact.output.format.probability") {
        return(".formatProbabilities")
    }
    if (key == "rpact.output.format.futility.probability") {
        return(".formatFutilityProbabilities")
    }
    if (key == "rpact.output.format.sample.size") {
        return(".formatSampleSizes")
    }
    if (key == "rpact.output.format.event") {
        return(".formatEvents")
    }
    if (key == "rpact.output.format.event.time") {
        return(".formatEventTime")
    }
    if (key == "rpact.output.format.conditional.power") {
        return(".formatConditionalPower")
    }
    if (key == "rpact.output.format.critical.value") {
        return(".formatCriticalValues")
    }
    if (key == "rpact.output.format.critical.value.fisher") {
        return(".formatCriticalValuesFisher")
    }
    if (key == "rpact.output.format.test.statistic.fisher") {
        return(".formatTestStatisticsFisher")
    }
    if (key == "rpact.output.format.test.statistic") {
        return(".formatTestStatistics")
    }
    if (key == "rpact.output.format.rate") {
        return(".formatRates")
    }
    if (key == "rpact.output.format.rate1") {
        return(".formatRatesDynamic")
    }
    if (key == "rpact.output.format.accrual.intensity") {
        return(".formatAccrualIntensities")
    }
    if (key == "rpact.output.format.mean") {
        return(".formatMeans")
    }
    if (key == "rpact.output.format.ratio") {
        return(".formatRatios")
    }
    if (key == "rpact.output.format.st.dev") {
        return(".formatStDevs")
    }
    if (key == "rpact.output.format.duration") {
        return(".formatDurations")
    }
    if (key == "rpact.output.format.time") {
        return(".formatTime")
    }
    return(NULL)
}

.getOutputFormatKeyByFieldName <- function(fieldName) {
    if (is.null(fieldName) || length(fieldName) != 1 || is.na(fieldName)) {
        return(NULL)
    }

    if (!(fieldName %in% names(C_PARAMETER_FORMAT_FUNCTIONS))) {
        return(NULL)
    }

    functionName <- C_PARAMETER_FORMAT_FUNCTIONS[[fieldName]]
    if (is.null(functionName)) {
        return(NULL)
    }

    return(.getOutputFormatKeyByFunctionName(functionName))
}

.getOutputFormatKeyByFunctionName <- function(functionName) {
    for (key in names(C_OUTPUT_FORMAT_DEFAULT_VALUES)) {
        if (.getOutputFormatFunctionName(key) == functionName) {
            return(key)
        }
    }
    return(NULL)
}

.resetAllOutputFormats <- function() {
    base::options(C_OUTPUT_FORMAT_DEFAULT_VALUES)
    cat(length(C_OUTPUT_FORMAT_DEFAULT_VALUES), "output formats were successfully reset\n")
}
