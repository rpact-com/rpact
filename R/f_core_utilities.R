## |
## |  *Core utilities*
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
## |  File version: $Revision: 8270 $
## |  Last changed: $Date: 2024-09-25 16:37:39 +0200 (Mi, 25 Sep 2024) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_core_constants.R
#' @include f_logger.R
NULL

.getLogicalEnvironmentVariable <- function(variableName) {
    result <- as.logical(Sys.getenv(variableName))
    return(ifelse(is.na(result), FALSE, result))
}

.getPackageName <- function(functionName) {
    .assertIsSingleCharacter(functionName, "functionName")
    tryCatch(
        {
            return(environmentName(environment(base::get(functionName))))
        },
        error = function(e) {}
    )
    return(NA_character_)
}

.toCapitalized <- function(x, ignoreBlackList = FALSE) {
    if (is.null(x) || is.na(x) || !is.character(x)) {
        return(x)
    }

    if (!ignoreBlackList) {
        if (x %in% c("pi", "pi1", "pi2", "mu", "mu1", "mu2")) {
            return(x)
        }
    }

    s <- strsplit(x, " ")[[1]]
    s <- paste0(toupper(substring(s, 1, 1)), substring(s, 2))
    wordsToExclude <- c("And", "The", "Of", "Or", "By")
    s[s %in% wordsToExclude] <- tolower(s[s %in% wordsToExclude])
    s <- paste(s, collapse = " ")
    s <- sub("non\\-binding", "Non-Binding", s)
    s <- sub("binding", "Binding", s)
    return(s)
}

.formatCamelCaseSingleWord <- function(x, title = FALSE) {
    if (length(x) == 0 || nchar(trimws(x)) == 0) {
        return(x)
    }

    indices <- gregexpr("[A-Z]", x)[[1]]
    parts <- strsplit(x, "[A-Z]")[[1]]
    result <- ""
    for (i in seq_len(length(indices))) {
        index <- indices[i]
        y <- tolower(substring(x, index, index))
        if (title) {
            y <- .firstCharacterToUpperCase(y)
        }
        value <- ifelse(title, .firstCharacterToUpperCase(parts[i]), parts[i])
        result <- paste0(result, value, " ", y)
    }
    if (length(parts) > length(indices)) {
        result <- paste0(result, parts[length(parts)])
    }
    return(trimws(result))
}

.formatCamelCase <- function(x, title = FALSE, ..., ignoreBlackList = FALSE) {
    words <- strsplit(x, " ")[[1]]
    parts <- character()
    for (word in words) {
        parts <- c(parts, .formatCamelCaseSingleWord(word, title = title))
    }
    result <- paste0(parts, collapse = " ")
    if (grepl(" $", x)) {
        result <- paste0(result, " ")
    }
    if (title) {
        result <- .toCapitalized(result, ignoreBlackList = ignoreBlackList)
    }
    if (grepl(" $", x) && !grepl(" $", result)) {
        result <- paste0(result, " ")
    }
    return(result)
}

.firstCharacterToUpperCase <- function(x, ..., sep = "") {
    args <- list(...)
    if (length(args) > 0) {
        x <- paste(x, unlist(args, use.names = FALSE), collapse = sep, sep = sep)
    }
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    return(x)
}

.equalsRegexpIgnoreCase <- function(x, pattern) {
    x <- tolower(x)
    pattern <- tolower(pattern)
    result <- grep(pattern, x)
    return(sum(result) > 0)
}

#'
#' @title
#' Get Optional Argument
#'
#' @description
#' Returns the value of an optional argument if it exists.
#'
#' @param optionalArgumentName the name of the optional argument.
#'
#' @details
#' Internal function.
#'
#' @return the value of the optional argument if it exists; NULL otherwise.
#'
#' @examples
#' \dontrun{
#' f <- function(...) {
#'     print(.getOptionalArgument("x", ...))
#' }
#' f(x = 1)
#' f(y = 1)
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
.getOptionalArgument <- function(optionalArgumentName, ..., optionalArgumentDefaultValue = NULL) {
    args <- list(...)
    if (optionalArgumentName %in% names(args)) {
        return(args[[optionalArgumentName]])
    }

    return(optionalArgumentDefaultValue)
}

.isUndefinedArgument <- function(arg) {
    if (missing(arg) || is.null(arg)) {
        return(TRUE)
    }

    tryCatch(
        {
            if (length(arg) == 0) {
                return(TRUE)
            }

            if (length(arg) > 1) {
                return(FALSE)
            }

            return(is.na(arg))
        },
        warning = function(w) {
            paramName <- deparse(substitute(arg))
            .logWarn(
                "Failed to execute '.isUndefinedArgument(%s)' ('%s' is an instance of class '%s'): %s",
                paramName, paramName, .getClassName(arg), w$message
            )
            return(FALSE)
        }
    )

    return(FALSE)
}

.isDefinedArgument <- function(arg, argumentExistsValidationEnabled = TRUE) {
    paramName <- deparse(substitute(arg))
    if (argumentExistsValidationEnabled &&
            length(grep("\\$|\\[|\\]", paramName)) == 0 && !exists(paramName)) {
        tryCatch(
            {
                if (missing(arg) || is.null(arg)) {
                    return(FALSE)
                }
            },
            error = function(e) {
                stop(
                    C_EXCEPTION_TYPE_MISSING_ARGUMENT,
                    "the object '", paramName, "' has not been defined anywhere. ",
                    "Please define it first, e.g., run '", paramName, " <- 1'"
                )
            }
        )
    }

    if (missing(arg) || is.null(arg)) {
        return(FALSE)
    }

    tryCatch(
        {
            if (length(arg) == 0) {
                return(FALSE)
            }

            if (length(arg) > 1) {
                return(TRUE)
            }

            return(!is.na(arg))
        },
        warning = function(e) {
            paramName <- deparse(substitute(arg))
            .logWarn(
                "Failed to execute '.isDefinedArgument(%s)' ('%s' is an instance of class '%s'): %s",
                paramName, paramName, .getClassName(arg), e
            )
        }
    )

    return(FALSE)
}

.getConcatenatedValues <- function(x, separator = ", ", mode = c("csv", "vector", "and", "or")) {
    if (is.null(x) || length(x) <= 1) {
        return(x)
    }

    mode <- match.arg(mode)
    if (mode %in% c("csv", "vector")) {
        result <- paste(x, collapse = separator)
        if (mode == "vector") {
            result <- paste0("c(", result, ")")
        }
        return(result)
    }

    if (length(x) == 2) {
        return(paste(x, collapse = paste0(" ", mode, " ")))
    }

    space <- ifelse(grepl(" $", separator), "", " ")
    part1 <- x[seq_len(length(x)) - 1]
    part2 <- x[length(x)]
    return(paste0(paste(part1, collapse = separator), separator, space, mode, " ", part2))
}

#'
#' @examples
#' \dontrun{
#' .getConcatenatedValues(1)
#' .getConcatenatedValues(1:2)
#' .getConcatenatedValues(1:3)
#' .getConcatenatedValues(1, mode = "vector")
#' .getConcatenatedValues(1:2, mode = "vector")
#' .getConcatenatedValues(1:3, mode = "vector")
#' .getConcatenatedValues(1, mode = "and")
#' .getConcatenatedValues(1:2, mode = "and")
#' .getConcatenatedValues(1:3, mode = "and")
#' .getConcatenatedValues(1, mode = "or")
#' .getConcatenatedValues(1:2, mode = "or")
#' .getConcatenatedValues(1:3, mode = "or")
#' .getConcatenatedValues(1, mode = "or", separator = ";")
#' .getConcatenatedValues(1:2, mode = "or", separator = ";")
#' .getConcatenatedValues(1:3, mode = "or", separator = ";")
#' }
#'
#' @noRd
#'
.arrayToString <- function(x, ..., separator = ", ",
        vectorLookAndFeelEnabled = FALSE,
        encapsulate = FALSE,
        digits = 3,
        maxLength = 80L,
        maxCharacters = 160L,
        mode = c("csv", "vector", "and", "or")) {
    .assertIsSingleInteger(digits, "digits", naAllowed = TRUE, validateType = FALSE)
    .assertIsInClosedInterval(digits, "digits", lower = 0, upper = NULL)
    .assertIsSingleInteger(maxLength, "maxLength", naAllowed = FALSE, validateType = FALSE)
    .assertIsInClosedInterval(maxLength, "maxLength", lower = 1, upper = NULL)
    .assertIsSingleInteger(maxCharacters, "maxCharacters", naAllowed = FALSE, validateType = FALSE)
    .assertIsInClosedInterval(maxCharacters, "maxCharacters", lower = 3, upper = NULL)

    if (missing(x) || is.null(x) || length(x) == 0) {
        return("NULL")
    }

    if (length(x) == 1 && is.na(x)) {
        return("NA")
    }

    if (!is.numeric(x) && !is.character(x) && !is.logical(x) && !is.integer(x)) {
        return(.getClassName(x))
    }

    if (is.numeric(x) && !is.na(digits)) {
        if (digits > 0) {
            indices <- which(!is.na(x) & abs(x) >= 10^-digits)
        } else {
            indices <- which(!is.na(x))
        }
        x[indices] <- as.character(round(x[indices], digits))
    }

    mode <- match.arg(mode)
    if (mode == "csv" && vectorLookAndFeelEnabled) {
        mode <- "vector"
    }

    if (is.matrix(x) && nrow(x) > 1 && ncol(x) > 1) {
        result <- c()
        for (i in 1:nrow(x)) {
            row <- x[i, ]
            if (encapsulate) {
                row <- paste0("'", row, "'")
            }
            result <- c(result, paste0("(", paste(row, collapse = separator), ")"))
        }
        return(.getConcatenatedValues(result, separator = separator, mode = mode))
    }

    if (encapsulate) {
        x <- paste0("'", x, "'")
    }

    if (length(x) > maxLength) {
        x <- c(x[1:maxLength], "...")
    }

    s <- .getConcatenatedValues(x, separator = separator, mode = mode)
    if (nchar(s) > maxCharacters && length(x) > 1) {
        s <- x[1]
        index <- 2
        while (nchar(paste0(s, separator, x[index])) <= maxCharacters && index <= length(x)) {
            s <- paste0(s, separator, x[index])
            index <- index + 1
        }
        s <- paste0(s, separator, "...")
        if (vectorLookAndFeelEnabled && length(x) > 1) {
            s <- paste0("c(", s, ")")
        }
    }

    return(s)
}

.listToString <- function(a, separator = ", ", listLookAndFeelEnabled = FALSE, encapsulate = FALSE) {
    if (missing(a) || is.null(a) || length(a) == 0) {
        return("NULL")
    }

    if (length(a) == 1 && is.na(a)) {
        return("NA")
    }

    result <- ""
    for (name in names(a)) {
        value <- a[[name]]

        if (is.list(value)) {
            value <- .listToString(value,
                separator = separator,
                listLookAndFeelEnabled = listLookAndFeelEnabled,
                encapsulate = encapsulate
            )
            if (!listLookAndFeelEnabled) {
                value <- paste0("{", value, "}")
            }
        } else {
            if (length(value) > 1) {
                value <- .arrayToString(value,
                    separator = separator,
                    encapsulate = encapsulate
                )
                value <- paste0("(", value, ")")
            } else if (encapsulate) {
                value <- sQuote(value)
            }
        }

        entry <- paste(name, "=", value)

        if (nchar(result) > 0) {
            result <- paste(result, entry, sep = ", ")
        } else {
            result <- entry
        }
    }

    if (!listLookAndFeelEnabled) {
        return(result)
    }

    return(paste0("list(", result, ")"))
}

.getInputForZeroOutputInsideTolerance <- function(input, output, tolerance = .Machine$double.eps^0.25) {
    if (is.null(tolerance) || is.na(tolerance)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'tolerance' must be a valid double")
    }

    if (tolerance < 0) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'tolerance' (", tolerance, ") must be >= 0")
    }

    if (is.null(input)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'input' must be a valid double or NA")
    }

    if (is.null(output) || is.na(output)) {
        return(NA_real_)
    }

    if (abs(output) <= tolerance) {
        return(input)
    }

    return(NA_real_)
}

.getInputProducingZeroOutput <- function(input1, output1, input2, output2,
        tolerance = .Machine$double.eps^0.25) {
    if ((is.na(output1) || is.null(output1)) &&
            (is.na(output2) || is.null(output2))) {
        return(NA_real_)
    }

    if (is.na(output1) || is.null(output1)) {
        return(.getInputForZeroOutputInsideTolerance(input2, output2, tolerance))
    }

    if (is.na(output2) || is.null(output2)) {
        return(.getInputForZeroOutputInsideTolerance(input1, output1, tolerance))
    }

    if (abs(output1) <= abs(output2) && !is.na(input1)) {
        return(.getInputForZeroOutputInsideTolerance(input1, output1, tolerance))
    }

    return(.getInputForZeroOutputInsideTolerance(input2, output2, tolerance))
}

#'
#' @title
#' Get One Dimensional Root
#'
#' @description
#' Searches and returns the one dimensional root of a function using \code{uniroot}.
#'
#' @param acceptResultsOutOfTolerance if \code{TRUE}, results will be accepted in any case;
#'        if \code{FALSE}, \code{NA_real_} will be returned in case of tolerance discrepancy
#'
#' @details
#' Internal function.
#'
#' @return the root.
#'
#' @keywords internal
#'
#' @noRd
#'
.getOneDimensionalRoot <- function(fun,
        ...,
        lower,
        upper,
        tolerance = .Machine$double.eps^0.25,
        acceptResultsOutOfTolerance = FALSE,
        suppressWarnings = TRUE,
        callingFunctionInformation = NA_character_,
        cppEnabled = FALSE) {
    .assertIsSingleNumber(lower, "lower")
    .assertIsSingleNumber(upper, "upper")
    .assertIsSingleNumber(tolerance, "tolerance")

    resultLower <- fun(lower, ...)
    resultUpper <- fun(upper, ...)
    result <- .getInputProducingZeroOutput(lower, resultLower, upper, resultUpper, tolerance)
    if (!is.na(result)) {
        return(result)
    }

    unirootResult <- NULL
    tryCatch(
        {
            unirootResult <- stats::uniroot(
                f = fun, lower = lower, upper = upper,
                tol = tolerance, trace = 2, extendInt = "no", ...
            )
        },
        warning = function(w) {
            .logWarn(
                .getCallingFunctionInformation(callingFunctionInformation),
                "uniroot(f, lower = %s, upper = %s, tol = %s) produced a warning: %s",
                lower, upper, tolerance, w
            )
        },
        error = function(e) {
            msg <- "Failed to run uniroot(f, lower = %s, upper = %s, tol = %s): %s"
            if (getLogLevel() == C_LOG_LEVEL_DEBUG) {
                .logError(msg, lower, upper, tolerance, e)
            } else {
                .logWarn(msg, lower, upper, tolerance, e)
            }
        }
    )

    if (!is.null(unirootResult) && abs(unirootResult$f.root) <= max(tolerance * 100, 1e-07) * 1.2) {
        return(unirootResult$root)
    }

    if (cppEnabled && missing(...)) {
        tryCatch(
            {
                zeroinResult <- zeroin(fun, lower, upper, tolerance, 100)
            },
            warning = function(w) {
                .logWarn(
                    .getCallingFunctionInformation(callingFunctionInformation),
                    "zeroin(f, lower = %s, upper = %s, tol = %s) produced a warning: %s",
                    lower, upper, tolerance, w
                )
            },
            error = function(e) {
                msg <- "Failed to run zeroin(f, lower = %s, upper = %s, tol = %s): %s"
                if (getLogLevel() == C_LOG_LEVEL_DEBUG) {
                    .logError(msg, lower, upper, tolerance, e)
                } else {
                    .logWarn(msg, lower, upper, tolerance, e)
                }
            }
        )
        if (!is.null(zeroinResult) && !(abs(fun(zeroinResult)) > max(tolerance * 100, 1e-07))) {
            return(zeroinResult)
        }
    }

    if (is.null(unirootResult)) {
        direction <- ifelse(fun(lower) < fun(upper), 1, -1)
        if (is.na(direction)) {
            return(NA_real_)
        }

        return(.getOneDimensionalRootBisectionMethod(
            fun = fun,
            lower = lower, upper = upper, tolerance = tolerance,
            acceptResultsOutOfTolerance = acceptResultsOutOfTolerance, direction = direction,
            suppressWarnings = suppressWarnings, callingFunctionInformation = callingFunctionInformation
        ))
    }

    if (!acceptResultsOutOfTolerance) {
        if (!suppressWarnings) {
            warning(.getCallingFunctionInformation(callingFunctionInformation),
                "NA returned because root search by 'uniroot' produced a function result (",
                unirootResult$f.root, ") that differs from target 0 ",
                "(lower = ", lower, ", upper = ", upper, ", tolerance = ", tolerance,
                ", last function argument was ", unirootResult$root, ")",
                call. = FALSE
            )
        }
        return(NA_real_)
    } else if (!suppressWarnings) {
        warning(.getCallingFunctionInformation(callingFunctionInformation),
            "Root search by 'uniroot' produced a function result (", unirootResult$f.root, ") ",
            "that differs from target 0 ",
            "(lower = ", lower, ", upper = ", upper, ", tolerance = ", tolerance,
            ", last function argument was ", unirootResult$root, ")",
            call. = FALSE
        )
    }

    return(unirootResult$root)
}

.getCallingFunctionInformation <- function(x) {
    if (is.na(x)) {
        return("")
    }

    return(paste0(x, ": "))
}

#'
#' @title
#' Get One Dimensional Root Bisection Method
#'
#' @description
#' Searches and returns the one dimensional root of a function using the bisection method.
#'
#' @param acceptResultsOutOfTolerance if \code{TRUE}, results will be accepted in any case;
#'        if \code{FALSE}, \code{NA_real_} will be returned in case of tolerance discrepancy
#'
#' @details
#' Internal function.
#'
#' @keywords internal
#'
#' @noRd
#'
.getOneDimensionalRootBisectionMethod <- function(fun, ..., lower, upper,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT,
        acceptResultsOutOfTolerance = FALSE,
        maxSearchIterations = 50,
        direction = 0,
        suppressWarnings = TRUE,
        callingFunctionInformation = NA_character_) {
    lowerStart <- lower
    upperStart <- upper

    if (direction == 0) {
        direction <- ifelse(fun(lower) < fun(upper), 1, -1)
    }

    .logTrace(
        "Start special root search: lower = %s, upper = %s, tolerance = %s, direction = %s",
        lower, upper, tolerance, direction
    )

    precision <- 1
    while (!is.na(precision) && precision > tolerance) {
        argument <- (lower + upper) / 2
        result <- fun(argument)

        .logTrace(
            "Root search step: f(%s, lower = %s, upper = %s, direction = %s) = %s",
            argument, lower, upper, direction, result
        )

        ifelse(result * direction < 0, lower <- argument, upper <- argument)

        maxSearchIterations <- maxSearchIterations - 1
        if (maxSearchIterations < 0) {
            if (!suppressWarnings) {
                warning(.getCallingFunctionInformation(callingFunctionInformation),
                    "Root search via 'bisection' stopped: maximum number of search iterations reached. ",
                    "Check if lower and upper search bounds were calculated correctly",
                    call. = FALSE
                )
            }
            .plotMonotoneFunctionRootSearch(fun, lowerStart, upperStart)
            return(NA_real_)
        }

        precision <- upper - lower
    }

    if (is.infinite(result) || abs(result) > max(tolerance * 100, 1e-07)) { # 0.01) { # tolerance * 20
        .plotMonotoneFunctionRootSearch(fun, lowerStart, upperStart)

        if (!acceptResultsOutOfTolerance) {
            if (!suppressWarnings) {
                warning(.getCallingFunctionInformation(callingFunctionInformation),
                    "NA returned because root search via 'bisection' produced a function result (",
                    result, ") that differs from target 0 ",
                    "(tolerance is ", tolerance, ", last function argument was ", argument, ")",
                    call. = FALSE
                )
            }
            return(NA_real_)
        } else if (!suppressWarnings) {
            warning(.getCallingFunctionInformation(callingFunctionInformation),
                "Root search via 'bisection' produced a function result (", result, ") ",
                "that differs from target 0 ",
                "(tolerance is ", tolerance, ", last function argument was ", argument, ")",
                call. = FALSE
            )
        }
    }

    return(argument)
}

.plotMonotoneFunctionRootSearch <- function(f, lowerStart, upperStart) {
    if (getLogLevel() != C_LOG_LEVEL_TRACE) {
        return(invisible())
    }

    values <- c()
    params <- seq(from = lowerStart, to = upperStart, by = (upperStart - lowerStart) / 20)
    for (i in params) {
        values <- c(values, f(i))
    }
    graphics::plot(params, values)
}

.getTextLineWithLineBreak <- function(line, lineBreakIndex) {
    index <- .getSpaceIndex(line, lineBreakIndex)
    if (index == -1) {
        return(line)
    }

    a <- substr(line, 0, index - 1)
    b <- substr(line, index + 1, nchar(line))
    return(paste0(a, "\n", b))
}

.getSpaceIndex <- function(line, lineBreakIndex) {
    if (nchar(line) <= lineBreakIndex) {
        return(-1)
    }

    if (regexpr("\\n", line) > 0) {
        return(-1)
    }

    len <- nchar(line)
    lineSplit <- strsplit(line, "")[[1]]
    for (i in (len / 2):length(lineSplit)) {
        char <- lineSplit[i]
        if (char == " ") {
            return(i)
        }
    }
    return(-1)
}

.isFirstValueGreaterThanSecondValue <- function(firstValue, secondValue) {
    if (is.null(firstValue) || length(firstValue) != 1 || is.na(firstValue)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'firstValue' (", firstValue, ") must be a valid numeric value"
        )
    }
    if (is.null(secondValue) || length(secondValue) != 1 || is.na(secondValue)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'secondValue' (", secondValue, ") must be a valid numeric value"
        )
    }
    return(firstValue > secondValue)
}

.isFirstValueSmallerThanSecondValue <- function(firstValue, secondValue) {
    if (is.null(firstValue) || length(firstValue) != 1 || is.na(firstValue)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'firstValue' (", firstValue, ") must be a valid numeric value"
        )
    }
    if (is.null(secondValue) || length(secondValue) != 1 || is.na(secondValue)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'secondValue' (", secondValue, ") must be a valid numeric value"
        )
    }
    return(firstValue < secondValue)
}

.setParameterType <- function(parameterSet, parameterName, parameterType) {
    if (is.null(parameterSet)) {
        return(invisible())
    }

    parameterSet$.setParameterType(parameterName, parameterType)
}

.setValueAndParameterType <- function(parameterSet, parameterName, value, defaultValue,
        notApplicableIfNA = FALSE) {
    .assertIsParameterSetClass(parameterSet, "parameterSet")

    if (is.null(parameterSet)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'parameterSet' must be not null")
    }

    if (!(parameterName %in% names(parameterSet))) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'", .getClassName(parameterSet), "' does not contain a field with name '", parameterName, "'"
        )
    }

    parameterSet[[parameterName]] <- value

    if (notApplicableIfNA && all(is.na(value))) {
        parameterSet$.setParameterType(parameterName, C_PARAM_NOT_APPLICABLE)
    } else if (!is.null(value) && length(value) == length(defaultValue) && (
            (all(is.na(value)) && all(is.na(value) == is.na(defaultValue))) ||
                (!is.na(all(value == defaultValue)) && all(value == defaultValue))
        )) {
        parameterSet$.setParameterType(parameterName, C_PARAM_DEFAULT_VALUE)
    } else {
        parameterSet$.setParameterType(parameterName, C_PARAM_USER_DEFINED)
    }
}

.isDefaultVector <- function(x, default) {
    if (length(x) != length(default)) {
        return(FALSE)
    }

    return(sum(x == default) == length(x))
}

.getNumberOfZerosDirectlyAfterDecimalSeparator <- function(x) {
    zeroCounter <- 0
    startEnabled <- FALSE
    x <- round(x, 15)
    x <- sprintf("%.15f", x)
    for (i in 1:nchar(x)) {
        num <- substring(x, i, i)
        if (num == ".") {
            startEnabled <- TRUE
        } else if (startEnabled) {
            if (num == "0") {
                zeroCounter <- zeroCounter + 1
            } else {
                return(zeroCounter)
            }
        }
    }
    return(zeroCounter)
}

.getNextHigherValue <- function(x) {
    .assertIsNumericVector(x, "x")
    values <- c()
    for (value in x) {
        value <- round(value, 15)
        values <- c(values, 1 / 10^.getNumberOfZerosDirectlyAfterDecimalSeparator(value))
    }
    return(values)
}

.getVariedParameterVectorByValue <- function(variedParameter) {
    return((variedParameter[2] - variedParameter[1]) / C_VARIED_PARAMETER_SEQUENCE_LENGTH_DEFAULT)
}

.plotTheta <- function(theta) {
    if (is.null(theta) || length(theta) == 0 || all(is.na(theta))) {
        theta <- seq(-1, 1, 0.02)
    }
    theta <- .assertIsValidThetaRange(thetaRange = theta)
    return(theta)
}

.getVariedParameterVector <- function(variedParameter, variedParameterName) {
    if (is.null(variedParameter) || length(variedParameter) != 2 || any(is.na(variedParameter))) {
        return(variedParameter)
    }

    minValue <- variedParameter[1]
    maxValue <- variedParameter[2]
    if (minValue == maxValue) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'", variedParameterName, "' with length 2 must contain minimum != maximum (",
            minValue, " == ", maxValue, ")"
        )
    }
    by <- .getVariedParameterVectorByValue(variedParameter)
    variedParameter <- seq(minValue, maxValue, by)
    return(variedParameter)
}

.getVariedParameterVectorSeqCommand <- function(variedParameter) {
    return(paste0(
        "seq(", round(variedParameter[1], 4), ", ", round(variedParameter[2], 4), ", ",
        round(.getVariedParameterVectorByValue(variedParameter), 6), ")"
    ))
}

.getNumberOfSubjects1 <- function(numberOfSubjects, allocationRatioPlanned) {
    return((numberOfSubjects * allocationRatioPlanned) / (allocationRatioPlanned + 1))
}

.getNumberOfSubjects2 <- function(numberOfSubjects, allocationRatioPlanned) {
    return(numberOfSubjects / (allocationRatioPlanned + 1))
}

.fillWithNAs <- function(x, kMax) {
    if (length(x) >= kMax) {
        return(x)
    }

    x[(length(x) + 1):kMax] <- NA_real_
    return(x)
}

.matchArgument <- function(arg, defaultValue) {
    if (any(is.na(arg))) {
        return(defaultValue)
    }
    return(ifelse(length(arg) > 0, arg[1], defaultValue))
}

#' @title
#' Print Citation
#
#' @description
#' How to cite \code{rpact} and \code{R} in publications.
#'
#' @param inclusiveR If \code{TRUE} (default) the information on how to cite the base R system in publications will be added.
#' @param language Language code to use for the output, default is "en".
#' @param markdown If \code{TRUE}, the output will be created in Markdown.
#'
#' @details
#' This function shows how to cite \code{rpact} and \code{R} (\code{inclusiveR = TRUE}) in publications.
#'
#' @examples
#' printCitation()
#'
#' @keywords internal
#'
#' @export
#'
printCitation <- function(inclusiveR = TRUE, language = "en", markdown = NA) {
    if (is.na(markdown)) {
        markdown <- .isMarkdownEnabled("print")
    }

    if (isTRUE(markdown)) {
        result <- paste0(utils::capture.output(.printCitation(
            inclusiveR = inclusiveR, language = language
        )), collapse = "\n")
        return(knitr::asis_output(result))
    }

    .printCitation(inclusiveR = inclusiveR, language = language)
}

.printCitation <- function(inclusiveR = TRUE, language = "en") {
    currentLanguage <- Sys.getenv("LANGUAGE")
    tryCatch(
        {
            Sys.setenv(LANGUAGE = language)

            if (inclusiveR) {
                citR <- utils::capture.output(print(citation("base"), bibtex = FALSE))
                indices <- which(citR == "")
                indices <- indices[indices != 1 & indices != length(citR)]
                if (length(indices) > 1) {
                    index <- indices[length(indices)]
                    citR <- citR[1:min(index, length(citR))]
                }
                cat("\n", trimws(paste(citR, collapse = "\n")), "\n\n", sep = "")
            }

            print(citation("rpact"), bibtex = FALSE)
        },
        finally = {
            Sys.setenv(LANGUAGE = currentLanguage)
        }
    )
}

.writeLinesToFile <- function(lines, fileName) {
    if (is.null(lines) || length(lines) == 0 || !is.character(lines)) {
        warning("Empty lines. Stop to write '", fileName, "'")
        return(invisible(fileName))
    }

    fileConn <- base::file(fileName)
    tryCatch(
        {
            base::writeLines(lines, fileConn)
        },
        finally = {
            base::close(fileConn)
        }
    )
    invisible(fileName)
}

#'
#' Windows: CR (Carriage Return \r) and LF (LineFeed \n) pair
#'
#' OSX, Linux: LF (LineFeed \n)
#'
#' @noRd
#'
.readLinesFromFile <- function(inputFileName) {
    content <- .readContentFromFile(inputFileName)
    return(strsplit(content, split = "(\r?\n)|(\r\n?)")[[1]])
}

.readContentFromFile <- function(inputFileName) {
    return(readChar(inputFileName, file.info(inputFileName)$size))
}

.integerToWrittenNumber <- function(x) {
    if (is.null(x) || length(x) != 1 || !is.numeric(x) || is.na(x)) {
        return(x)
    }

    temp <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
    if (x >= 1 && x <= length(temp) && as.integer(x) == x) {
        return(temp[x])
    }

    return(as.character(x))
}

.getFunctionAsString <- function(fun, stringWrapPrefix = " ") {
    .assertIsFunction(fun)

    s <- utils::capture.output(print(fun))
    s <- s[!grepl("bytecode", s)]
    s <- s[!grepl("environment", s)]
    if (is.null(stringWrapPrefix) || is.na(stringWrapPrefix) || nchar(stringWrapPrefix) == 0) {
        stringWrapPrefix <- " "
    }
    s <- gsub("\u0009", stringWrapPrefix, s) # \t
    return(s)
}

.getFunctionArgumentNames <- function(fun, ignoreThreeDots = FALSE) {
    .assertIsFunction(fun)
    args <- methods::formalArgs(fun)
    if (ignoreThreeDots) {
        args <- args[args != "..."]
    }
    return(args)
}

.getDecimalPlaces <- function(values) {
    if (is.null(values) || length(values) == 0) {
        return(integer(0))
    }

    values[is.na(values)] <- 0
    decimalPlaces <- c()
    for (value in values) {
        decimalPlaces <- c(
            decimalPlaces,
            nchar(sub("^\\d+\\.", "", sub("0*$", "", format(round(value, 15), scientific = FALSE))))
        )
    }
    return(decimalPlaces)
}

.isResultObjectBaseClass <- function(x) {
    return(R6::is.R6(x))
}

#'
#' @title
#' Get Parameter Caption
#'
#' @description
#' Returns the parameter caption for a given object and parameter name.
#'
#' @param obj The rpact result object.
#' @param var The variable/parameter name.
#'
#' @details
#' This function identifies and returns the caption that will be used in print outputs of an rpact result object.
#'
#' @seealso
#' \code{\link[=getParameterName]{getParameterName()}} for getting the parameter name for a given caption.
#'
#' @return Returns a \code{\link[base]{character}} of specifying the corresponding caption of a given parameter name.
#' Returns \code{NULL} if the specified \code{parameterName} does not exist.
#'
#' @examples
#' \dontrun{
#' getParameterCaption(getDesignInverseNormal(), "kMax")
#' }
#'
#' @keywords internal
#'
#' @export
#'
getParameterCaption <- function(obj, var) {
    fCall <- match.call(expand.dots = FALSE)
    if (is.null(obj) || !.isResultObjectBaseClass(obj) || !inherits(obj, "FieldSet")) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'obj' ",
            "(", .getClassName(obj), ") must be an rpact result object"
        )
    }
    parameterName <- .getParameterSetVar(fCall, var)
    .assertIsSingleCharacter(parameterName, "parameterName", naAllowed = FALSE)
    return(.getParameterCaption(parameterName, obj))
}

#'
#' @title
#' Get Parameter Type
#'
#' @description
#' Returns the parameter type for a given object and parameter name.
#'
#' @param obj The rpact result object.
#' @param var The variable/parameter name.
#'
#' @details
#' This function identifies and returns the type that will be used in print outputs of an rpact result object.
#'
#' @seealso
#' \code{\link[=getParameterName]{getParameterName()}} for getting the parameter name for a given caption.
#' \code{\link[=getParameterCaption]{getParameterCaption()}} for getting the parameter caption for a given name.
#'
#' @return Returns a \code{\link[base]{character}} of specifying the corresponding type of a given parameter name.
#' Returns \code{NULL} if the specified \code{parameterName} does not exist.
#'
#' @examples
#' \dontrun{
#' getParameterType(getDesignInverseNormal(), "kMax")
#' }
#'
#' @keywords internal
#'
#' @export
#'
getParameterType <- function(obj, var) {
    fCall <- match.call(expand.dots = FALSE)
    parameterName <- .getParameterSetVar(fCall, var)
    if (is.null(obj) || !.isResultObjectBaseClass(obj) || !inherits(obj, "FieldSet")) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'obj' ",
            "(", .getClassName(obj), ") must be an rpact result object"
        )
    }

    .assertIsSingleCharacter(parameterName, "parameterName", naAllowed = FALSE)

    obj$.getParameterType(parameterName)
}

#'
#' @title
#' Get Parameter Name
#'
#' @description
#' Returns the parameter name for a given object and parameter caption.
#'
#' @param obj The rpact result object.
#' @param parameterCaption The parameter caption.
#'
#' @details
#' This function identifies and returns the parameter name for a given caption
#' that will be used in print outputs of an rpact result object.
#'
#' @seealso
#' \code{\link[=getParameterCaption]{getParameterCaption()}} for getting the parameter caption for a given name.
#'
#' @return Returns a \code{\link[base]{character}} of specifying the corresponding name of a given parameter caption.
#' Returns \code{NULL} if the specified \code{parameterCaption} does not exist.
#'
#' @examples
#' \dontrun{
#' getParameterName(getDesignInverseNormal(), "Maximum number of stages")
#' }
#'
#' @keywords internal
#'
#' @export
#'
getParameterName <- function(obj, parameterCaption) {
    if (is.null(obj) || !.isResultObjectBaseClass(obj) || !inherits(obj, "FieldSet")) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'obj' ",
            "(", .getClassName(obj), ") must be an rpact result object"
        )
    }
    .assertIsSingleCharacter(parameterCaption, "parameterCaption", naAllowed = FALSE)
    if (parameterCaption %in% names(obj)) {
        return(parameterCaption)
    }

    fieldNames <- obj$.getVisibleFieldNames()
    parameterName <- getDictionaryKeyByValue(C_PARAMETER_NAMES, parameterCaption)
    if (!is.null(parameterName) && length(parameterName) == 1 && parameterName %in% fieldNames) {
        return(parameterName)
    }

    parameterName <- getDictionaryKeyByValue(C_PARAMETER_NAMES_PLOT_SETTINGS, parameterCaption)
    if (!is.null(parameterName) && length(parameterName) == 1 && parameterName %in% fieldNames) {
        return(parameterName)
    }

    for (parameterName in fieldNames) {
        if (identical(.getParameterCaption(parameterName, obj), parameterCaption)) {
            return(parameterName)
        }

        if (identical(.getParameterCaption(parameterName, obj, tableOutputEnabled = TRUE), parameterCaption)) {
            return(parameterName)
        }
    }
    
    if (!is.null(obj[[".design"]])) {
        result <- getParameterName(obj$.design, parameterCaption)
        if (!is.na(result)) {
            return(paste0(".design$", result))
        }
    }

    return(NA_character_)
}

.removeLastEntryFromArray <- function(x) {
    if (!is.array(x)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'x' (", .getClassName(x), ") must be an array")
    }

    dataDim <- dim(x)
    if (length(dataDim) != 3) {
        stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "function .removeLastEntryFromArray() only works for 3-dimensional arrays")
    }
    if (dataDim[3] < 2) {
        return(NA_real_)
    }

    dataDim[3] <- dataDim[3] - 1
    subData <- x[, , 1:dataDim[3]]
    return(array(data = subData, dim = dataDim))
}

.moveColumn <- function(data, columnName, insertPositionColumnName) {
    if (!is.data.frame(data)) {
        stop("Illegal argument: 'data' (", .getClassName(data), ") must be a data.frame")
    }
    if (is.null(insertPositionColumnName) || length(insertPositionColumnName) != 1 ||
            is.na(insertPositionColumnName) || !is.character(insertPositionColumnName)) {
        stop(
            "Illegal argument: 'insertPositionColumnName' (", .getClassName(insertPositionColumnName),
            ") must be a valid character value"
        )
    }
    if (is.null(columnName) || length(columnName) != 1 || is.na(columnName) || !is.character(columnName)) {
        stop("Illegal argument: 'columnName' (", .getClassName(columnName), ") must be a valid character value")
    }

    colNames <- colnames(data)
    if (!(columnName %in% colNames)) {
        stop("Illegal argument: 'columnName' (", columnName, ") does not exist in the specified data.frame 'data'")
    }
    if (!(insertPositionColumnName %in% colNames)) {
        stop(
            "Illegal argument: 'insertPositionColumnName' (", insertPositionColumnName,
            ") does not exist in the specified data.frame 'data'"
        )
    }
    if (columnName == insertPositionColumnName) {
        return(data)
    }

    colNames <- colNames[colNames != columnName]
    insertPositioIndex <- which(colNames == insertPositionColumnName)
    if (insertPositioIndex != (which(colnames(data) == columnName) - 1)) {
        if (insertPositioIndex == length(colNames)) {
            data <- data[, c(colNames[1:insertPositioIndex], columnName)]
        } else {
            data <- data[, c(colNames[1:insertPositioIndex], columnName, colNames[(insertPositioIndex + 1):length(colNames)])]
        }
    }
    return(data)
}

#' @examples
#' \dontrun{
#' or1 <- list(
#'     and1 = FALSE,
#'     and2 = TRUE,
#'     and3 = list(
#'         or1 = list(
#'             and1 = TRUE,
#'             and2 = TRUE
#'         ),
#'         or2 = list(
#'             and1 = TRUE,
#'             and2 = TRUE,
#'             and3 = TRUE
#'         ),
#'         or3 = list(
#'             and1 = TRUE,
#'             and2 = TRUE,
#'             and3 = TRUE,
#'             and4 = TRUE,
#'             and5 = TRUE
#'         )
#'     )
#' )
#' }
#'
#' @noRd
#'
.isConditionTrue <- function(x, condType = c("and", "or"), xName = NA_character_,
        level = 0, showDebugMessages = FALSE) {
    if (is.logical(x)) {
        if (showDebugMessages) {
            message(rep("\t", level), x, "")
        }
        return(x)
    }

    condType <- match.arg(condType)

    if (is.list(x)) {
        listNames <- names(x)
        if (is.null(listNames) || any(is.na(listNames)) || any(trimws(listNames) == "")) {
            stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "list (", .arrayToString(unlist(x)), ") must be named")
        }

        results <- logical(0)
        for (listName in listNames) {
            type <- gsub("\\d*", "", listName)
            if (!(type %in% c("and", "or"))) {
                stop(
                    C_EXCEPTION_TYPE_RUNTIME_ISSUE, "all list names (", type, " / ", listName,
                    ") must have the format 'and[number]' or 'or[number]', where [number] is an integer"
                )
            }

            subList <- x[[listName]]

            result <- .isConditionTrue(subList,
                condType = type, xName = listName,
                level = level + 1, showDebugMessages = showDebugMessages
            )
            results <- c(results, result)
        }

        if (condType == "and") {
            result <- all(results == TRUE)
            if (showDebugMessages) {
                message(rep("\t", level), result, " (before: and)")
            }
            return(result)
        }

        result <- any(results == TRUE)
        if (showDebugMessages) {
            message(rep("\t", level), result, " (before: or)")
        }
        return(result)
    }

    stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "x must be of type logical or list (is ", .getClassName(x))
}

.getClassName <- function(x) {
    return(as.character(class(x))[1])
}

.isPackageInstalled <- function(packageName) {
    return(nzchar(try(system.file(package = packageName), silent = TRUE)))
}

.getQNorm <- function(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE, epsilon = C_QNORM_EPSILON) {
    if (any(p < -1e-07 | p > 1 + 1e-07, na.rm = TRUE)) {
        warning("Tried to get qnorm() from ", .arrayToString(p), " which is out of interval (0, 1)")
    }

    p[p <= 0] <- epsilon
    p[p > 1] <- 1

    result <- stats::qnorm(p, mean = mean, sd = sd, lower.tail = lower.tail, log.p = log.p)

    result[result < -C_QNORM_THRESHOLD] <- C_QNORM_MINIMUM
    result[result > C_QNORM_THRESHOLD] <- C_QNORM_MAXIMUM

    return(result)
}

.getOneMinusQNorm <- function(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE, ..., epsilon = C_QNORM_EPSILON) {
    if (all(is.na(p))) {
        return(p)
    }

    if (any(p < -1e-07 | p > 1 + 1e-07, na.rm = TRUE)) {
        warning("Tried to get 1 - qnorm() from ", .arrayToString(p), " which is out of interval (0, 1)")
    }

    p[p <= 0] <- epsilon
    p[p > 1] <- 1

    indices <- p < 0.5
    indices[is.na(indices)] <- FALSE

    result <- rep(NA_real_, length(p))
    if (is.matrix(p)) {
        result <- matrix(result, ncol = ncol(p))
    }

    if (any(indices)) {
        result[indices] <- -stats::qnorm(p[indices],
            mean = mean, sd = sd, lower.tail = lower.tail, log.p = log.p
        )
    }

    # prevent values that are close to 1 from becoming Inf, see qnorm(1)
    # example: 1 - 1e-17 = 1 in R, i.e., qnorm(1 - 1e-17) = Inf
    # on the other hand: qnorm(1e-323) = -38.44939
    if (any(!indices)) {
        result[!indices] <- stats::qnorm(1 - p[!indices],
            mean = mean, sd = sd, lower.tail = lower.tail, log.p = log.p
        )
    }

    result[result < -C_QNORM_THRESHOLD] <- C_QNORM_MINIMUM
    result[result > C_QNORM_THRESHOLD] <- C_QNORM_MAXIMUM

    return(result)
}

.moveValue <- function(values, value, insertPositionValue) {
    if (is.null(insertPositionValue) || length(insertPositionValue) != 1 || is.na(insertPositionValue)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'insertPositionValue' (", class(insertPositionValue), ") must be a valid single value"
        )
    }
    if (is.null(value) || length(value) != 1 || is.na(value)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'value' (", class(value), ") must be a valid single value"
        )
    }
    if (!(value %in% values)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'value' (", value, ") does not exist in the specified vector 'values'"
        )
    }
    if (!(insertPositionValue %in% values)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'insertPositionValue' (", insertPositionValue,
            ") does not exist in the specified vector 'values'"
        )
    }
    if (value == insertPositionValue) {
        return(values)
    }

    originalValues <- values
    values <- values[values != value]
    insertPositioIndex <- which(values == insertPositionValue)
    if (insertPositioIndex != (which(originalValues == value) - 1)) {
        if (insertPositioIndex == length(values)) {
            values <- c(values[1:insertPositioIndex], value)
        } else {
            values <- c(values[1:insertPositioIndex], value, values[(insertPositioIndex + 1):length(values)])
        }
    }
    return(values)
}

.reconstructSequenceCommand <- function(values) {
    if (length(values) == 0 || all(is.na(values))) {
        return(NA_character_)
    }

    if (length(values) <= 3 || any(is.na(values))) {
        return(.arrayToString(values, vectorLookAndFeelEnabled = (length(values) != 1)))
    }

    minValue <- min(values)
    maxValue <- max(values)
    by <- (maxValue - minValue) / (length(values) - 1)
    valuesTemp <- seq(minValue, maxValue, by)
    if (isTRUE(all.equal(values, valuesTemp, tolerance = 1e-10))) {
        return(paste0("seq(", minValue, ", ", maxValue, ", ", by, ")"))
    }

    return(.arrayToString(values, vectorLookAndFeelEnabled = TRUE, maxLength = 10))
}

.isSummaryPipe <- function(fCall) {
    tryCatch(
        {
            xCall <- deparse(fCall$x)
            return(identical(xCall[1], ".") || grepl("^summary\\(", xCall[1]))
        },
        error = function(e) {}
    )
    return(FALSE)
}

.isMarkdownEnabled <- function(type = c("all", "print", "summary", "plot")) {
    type <- match.arg(type)
    if (!as.logical(getOption(paste0("rpact.auto.markdown.", type), TRUE))) {
        return(FALSE)
    }

    return(!is.null(knitr::current_input()) ||
        knitr::is_html_output() ||
        knitr::is_latex_output())
}

.isRMarkdownEnabled <- function() {
    fileName <- knitr::current_input()
    return(!is.null(fileName) && grepl("\\.rmd$", tolower(fileName)))
}

.isQuartoEnabled <- function() {
    fileName <- knitr::current_input()
    return(!is.null(fileName) && grepl("\\.(qmd|rmarkdown)$", tolower(fileName)))
}

#'
#' options("rpact.multivar.dist.eps" = 1e-06)
#'
#' @noRd
.getMultivariateDistribution <- function(...,
        type = c("normal", "t", "quantile", "tQuantile"),
        upper,
        sigma,
        eps = NA_real_,
        df = NA_real_, alpha = NA_real_) {
    type <- match.arg(type)

    if (is.null(eps) || length(eps) != 1 || is.na(eps)) {
        epsilon <- getOption("rpact.multivar.dist.eps", 1e-05)
        if (is.numeric(epsilon) && epsilon < 0.1) {
            eps <- epsilon
        }
    }

    dimensionSigma <- length(base::diag(sigma))
    if (type == "normal") {
        if (dimensionSigma == 1) {
            return(stats::pnorm(upper))
        }

        return(as251Normal(lower = -Inf, upper = upper, sigma = sigma, eps = 1e-06)[1])
    }

    if (type == "t") {
        if (dimensionSigma == 1) {
            return(stats::pt(upper, df))
        }

        return(as251StudentT(
            lower = -Inf, upper = upper,
            sigma = sigma, eps = eps, df = df
        )[1])
    }

    if (type == "quantile") {
        if (dimensionSigma == 1) {
            return(.getOneMinusQNorm(alpha))
        }

        return(.getOneDimensionalRoot(
            function(x) {
                return(as251Normal(
                    lower = -Inf, upper = x,
                    sigma = sigma, eps = 1e-06
                )[1] - (1 - alpha))
            },
            lower = -8,
            upper = 8,
            tolerance = 1e-06,
            callingFunctionInformation = ".getMultivariateDistribution",
            suppressWarnings = FALSE,
            acceptResultsOutOfTolerance = TRUE
        ))
    }

    if (type == "tQuantile") {
        if (dimensionSigma == 1) {
            return(stats::qt(1 - alpha, df))
        }

        return(.getOneDimensionalRoot(
            function(x) {
                return(as251StudentT(
                    lower = -Inf, upper = x,
                    eps = eps, sigma = sigma, df = df
                )[1] - (1 - alpha))
            },
            lower = -8,
            upper = 8,
            tolerance = 1e-06,
            callingFunctionInformation = ".getMultivariateDistribution",
            suppressWarnings = FALSE,
            acceptResultsOutOfTolerance = TRUE
        ))
    }
}

.addDeprecatedFieldValues <- function(parameterSet, fieldName, fieldValues) {
    parameterSet[[fieldName]] <- fieldValues
    parameterSet$.setParameterType(fieldName, C_PARAM_NOT_APPLICABLE)
    parameterSet$.deprecatedFieldNames <- unique(c(parameterSet$.deprecatedFieldNames, fieldName))
}

.resetPipeOperatorQueue <- function(x) {
    attr(x, "queue") <- NULL
    return(x)
}

.getMarkdownPlotPrintSeparator <- function() {
    return(getOption("rpact.markdown.plot.print.separator", C_MARKDOWN_PLOT_PRINT_SEPARATOR))
}

.getCriticalValues <- function(design, indices = NULL) {
    if (!is.null(indices)) {
        return(design$criticalValues[indices])
    }
    
    return(design$criticalValues)
}

.applyDirectionOfAlternative <- function(
        value,
        directionUpper,
        ...,
        type = c(
            "oneMinusValue",
            "valueMinusOne",
            "negateIfUpper",
            "negateIfLower",
            "minMax",
            "maxMin"
        ),
        phase = c("unknown", "design", "planning", "analysis"), 
        syncLength = FALSE) {
        
    type <- match.arg(type)
    phase <- match.arg(phase)

    if (phase == "design") {
        return(value) # deactivate for current release
    }
    
    if (is.null(value) || length(value) == 0 || all(is.na(value))) {
        return(value)
    }
    
    if (syncLength && length(directionUpper) > 1 && length(directionUpper) > length(value)) {
        directionUpper <- directionUpper[1:length(value)]
    }

    if (length(directionUpper) > 1 && length(value) != length(directionUpper)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'value' (", .arrayToString(value), ") and ",
            "'directionUpper' (", .arrayToString(directionUpper), ") must have the same length"
        )
    }

    if (length(value) <= 1 || length(directionUpper) == 1) {
        return(.applyDirectionOfAlternativeSingle(
            value = value,
            directionUpper = directionUpper,
            type = type
        ))
    }

    values <- c()
    for (i in 1:length(value)) {
        values <- c(
            values,
            .applyDirectionOfAlternativeSingle(
                value = value[i],
                directionUpper = ifelse(length(directionUpper) == 1, directionUpper, directionUpper[i]),
                type = type
            )
        )
    }
    return(values)
}

.applyDirectionOfAlternativeSingle <- function(
        ...,
        value,
        directionUpper,
        type = c(
            "oneMinusValue",
            "valueMinusOne",
            "negateIfUpper",
            "negateIfLower",
            "minMax",
            "maxMin"
        )) {
    type <- match.arg(type)
    if (is.null(value) || length(value) == 0 || all(is.na(value))) {
        return(value)
    }

    if (is.na(directionUpper) || isTRUE(directionUpper)) {
        if (type == "oneMinusValue") {
            return(1 - value)
        }
        if (type == "valueMinusOne") {
            return(value - 1)
        }
        if (type == "negateIfLower") {
            return(value)
        }
        if (type == "negateIfUpper") {
            if (is.logical(value)) {
                return(!value)
            }

            return(-value)
        }
        if (type == "minMax") {
            return(min(value, na.rm = TRUE))
        }
        if (type == "maxMin") {
            return(max(value, na.rm = TRUE))
        }
    }

    if (type == "negateIfLower") {
        if (is.logical(value)) {
            return(!value)
        }

        return(-value)
    }
    if (type == "negateIfUpper") {
        return(value)
    }
    if (type == "minMax") {
        return(max(value, na.rm = TRUE))
    }
    if (type == "maxMin") {
        return(min(value, na.rm = TRUE))
    }

    return(value)
}
