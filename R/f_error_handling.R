## |
## |  *Core assertions*
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
NULL

.getErrorMessage <- function(...) {
    return(paste0(unlist(list(...)), collapse = ""))
}

.getErrorCall <- function(call. = getOption("rpact.error.call", FALSE), call = NULL) {
    if (!is.null(call)) {
        return(call)
    }

    if (isTRUE(call.) && sys.nframe() > 3L) {
        return(sys.call(-3L))
    }

    return(NULL)
}

.stripErrorExceptionType <- function(message, exceptionType) {
    if (startsWith(message, exceptionType)) {
        return(substring(message, nchar(exceptionType) + 1L))
    }

    return(message)
}

.createCondition <- function(
        message,
        ...,
        factoryKey,
        code = NULL,
        category = NULL,
        class = NULL,
        call = NULL) {
    factory <- getOption(factoryKey)
    if (!is.function(factory)) {
        factory <- getOption("rpact.error.factory.condition")
    }

    if (is.function(factory)) {
        condition <- tryCatch(
            factory(
                message = message,
                ...,
                code = code,
                category = category,
                call = call,
                class = class
            ),
            error = function(e) {
                message("Error in condition factory: ", e$message)
                NULL
            }
        )
        if (inherits(condition, "condition")) {
            return(condition)
        }
    }

    condition <- simpleError(message, call = call)
    class(condition) <- unique(c(class, class(condition)))
    return(condition)
}

.stopRpactError <- function(
        message,
        ...,
        exceptionType,
        factoryKey,
        code,
        category,
        class,
        call. = getOption("rpact.error.call", FALSE),
        call = NULL) {
    message <- .stripErrorExceptionType(message, exceptionType)
    call <- .getErrorCall(call. = call., call = call)
    condition <- .createCondition(
        paste0(exceptionType, message),
        ...,
        factoryKey = factoryKey,
        code = code,
        category = category,
        class = class,
        call = call
    )
    stop(condition)
}

#'
#' @examples
#' stopRuntimeIssue("An error occurred")
#'
#' @noRd
#'
stopRuntimeIssue <- function(
        ...,
        parameter = NULL,
        value = NULL,
        constraint = NULL,
        functionName = NULL,
        relatedParameter = NULL,
        relatedValue = NULL,
        call. = getOption("rpact.error.call", FALSE),
        call = NULL) {
    message <- .getErrorMessage(...)
    .stopRpactError(
        message,
        functionName = context$functionName,
        parameter = context$parameter,
        value = context$value,
        constraint = context$constraint,
        relatedParameter = context$relatedParameter,
        relatedValue = context$relatedValue,
        exceptionType = C_EXCEPTION_TYPE_RUNTIME_ISSUE,
        factoryKey = "rpact.error.factory.runtime.issue.condition",
        code = "RUNTIME_ISSUE",
        category = "runtime_issue",
        class = c("rpact_runtime_issue_error", "rpact_error"),
        call. = call.,
        call = call
    )
}

#'
#' @examples
#' stopIllegalArgument("An error occurred")
#'
#' @noRd
#'
stopIllegalArgument <- function(
        ...,
        parameter = NULL,
        value = NULL,
        constraint = NULL,
        functionName = NULL,
        relatedParameter = NULL,
        relatedValue = NULL,
        call. = getOption("rpact.error.call", FALSE),
        call = NULL) {
    message <- .getErrorMessage(...)
    .stopRpactError(
        message,
        functionName = context$functionName,
        parameter = context$parameter,
        value = context$value,
        constraint = context$constraint,
        relatedParameter = context$relatedParameter,
        relatedValue = context$relatedValue,
        exceptionType = C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
        factoryKey = "rpact.error.factory.illegal.argument.condition",
        code = "ILLEGAL_ARGUMENT",
        category = "invalid_user_input",
        class = c("rpact_illegal_argument_error", "rpact_input_error"),
        call. = call.,
        call = call
    )
}

stopIllegalDataInput <- function(
        ...,
        parameter = NULL,
        value = NULL,
        constraint = NULL,
        functionName = NULL,
        relatedParameter = NULL,
        relatedValue = NULL,
        call. = getOption("rpact.error.call", FALSE),
        call = NULL) {
    message <- .getErrorMessage(...)
    .stopRpactError(
        message,
        functionName = context$functionName,
        parameter = context$parameter,
        value = context$value,
        constraint = context$constraint,
        relatedParameter = context$relatedParameter,
        relatedValue = context$relatedValue,
        exceptionType = C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT,
        factoryKey = "rpact.error.factory.illegal.data.input.condition",
        code = "ILLEGAL_DATA_INPUT",
        category = "invalid_data_input",
        class = c("rpact_illegal_data_input_error", "rpact_input_error"),
        call. = call.,
        call = call
    )
}

stopConflictingArguments <- function(
        ...,
        parameter = NULL,
        value = NULL,
        constraint = NULL,
        functionName = NULL,
        relatedParameter = NULL,
        relatedValue = NULL,
        call. = getOption("rpact.error.call", FALSE),
        call = NULL) {
    message <- .getErrorMessage(...)
    .stopRpactError(
        message,
        functionName = context$functionName,
        parameter = context$parameter,
        value = context$value,
        constraint = context$constraint,
        relatedParameter = context$relatedParameter,
        relatedValue = context$relatedValue,
        exceptionType = C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
        factoryKey = "rpact.error.factory.conflicting.arguments.condition",
        code = "CONFLICTING_ARGUMENTS",
        category = "invalid_user_input",
        class = c("rpact_conflicting_arguments_error", "rpact_input_error"),
        call. = call.,
        call = call
    )
}

stopArgumentOutOfBounds <- function(
        ...,
        parameter = NULL,
        value = NULL,
        constraint = NULL,
        functionName = NULL,
        relatedParameter = NULL,
        relatedValue = NULL,
        lowerBound = NULL,
        upperBound = NULL,
        call. = getOption("rpact.error.call", FALSE),
        call = NULL) {
    message <- .getErrorMessage(...)
    .stopRpactError(
        message,
        functionName = context$functionName,
        parameter = context$parameter,
        value = context$value,
        constraint = context$constraint,
        relatedParameter = context$relatedParameter,
        relatedValue = context$relatedValue,
        lowerBound = context$lowerBound,
        upperBound = context$upperBound,
        exceptionType = C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS,
        factoryKey = "rpact.error.factory.argument.out.of.bounds.condition",
        code = "ARGUMENT_OUT_OF_BOUNDS",
        category = "invalid_user_input",
        class = c("rpact_argument_out_of_bounds_error", "rpact_input_error"),
        call. = call.,
        call = call
    )
}

stopArgumentLengthOutOfBounds <- function(
        ...,
        parameter = NULL,
        value = NULL,
        constraint = NULL,
        functionName = NULL,
        relatedParameter = NULL,
        relatedValue = NULL,
        lowerBound = NULL,
        upperBound = NULL,
        expectedLength = NULL,
        actualLength = NULL,
        call. = getOption("rpact.error.call", FALSE),
        call = NULL) {
    message <- .getErrorMessage(...)
    .stopRpactError(
        message,
        functionName = context$functionName,
        parameter = context$parameter,
        value = context$value,
        constraint = context$constraint,
        relatedParameter = context$relatedParameter,
        relatedValue = context$relatedValue,
        lowerBound = context$lowerBound,
        upperBound = context$upperBound,
        expectedLength = context$expectedLength,
        actualLength = context$actualLength,
        exceptionType = C_EXCEPTION_TYPE_ARGUMENT_LENGTH_OUT_OF_BOUNDS,
        factoryKey = "rpact.error.factory.argument.length.out.of.bounds.condition",
        code = "ARGUMENT_LENGTH_OUT_OF_BOUNDS",
        category = "invalid_user_input",
        class = c("rpact_argument_length_out_of_bounds_error", "rpact_input_error"),
        call. = call.,
        call = call
    )
}

stopIndexOutOfBounds <- function(
        ...,
        parameter = NULL,
        value = NULL,
        constraint = NULL,
        functionName = NULL,
        relatedParameter = NULL,
        relatedValue = NULL,
        lowerBound = NULL,
        upperBound = NULL,
        index = NULL,
        call. = getOption("rpact.error.call", FALSE),
        call = NULL) {
    message <- .getErrorMessage(...)
    .stopRpactError(
        message,
        functionName = context$functionName,
        parameter = context$parameter,
        value = context$value,
        constraint = context$constraint,
        relatedParameter = context$relatedParameter,
        relatedValue = context$relatedValue,
        lowerBound = context$lowerBound,
        upperBound = context$upperBound,
        index = context$index,
        exceptionType = C_EXCEPTION_TYPE_INDEX_OUT_OF_BOUNDS,
        factoryKey = "rpact.error.factory.index.out.of.bounds.condition",
        code = "INDEX_OUT_OF_BOUNDS",
        category = "invalid_user_input",
        class = c("rpact_index_out_of_bounds_error", "rpact_input_error"),
        call. = call.,
        call = call
    )
}

stopMissingArgument <- function(
        ...,
        parameter = NULL,
        value = NULL,
        constraint = NULL,
        functionName = NULL,
        relatedParameter = NULL,
        relatedValue = NULL,
        call. = getOption("rpact.error.call", FALSE),
        call = NULL) {
    message <- .getErrorMessage(...)
    .stopRpactError(
        message,
        functionName = context$functionName,
        parameter = context$parameter,
        value = context$value,
        constraint = context$constraint,
        relatedParameter = context$relatedParameter,
        relatedValue = context$relatedValue,
        exceptionType = C_EXCEPTION_TYPE_MISSING_ARGUMENT,
        factoryKey = "rpact.error.factory.missing.argument.condition",
        code = "MISSING_ARGUMENT",
        category = "invalid_user_input",
        class = c("rpact_missing_argument_error", "rpact_input_error"),
        call. = call.,
        call = call
    )
}

stopIncompleteArguments <- function(
        ...,
        parameter = NULL,
        value = NULL,
        constraint = NULL,
        functionName = NULL,
        relatedParameter = NULL,
        relatedValue = NULL,
        missingParameters = NULL,
        definedParameters = NULL,
        call. = getOption("rpact.error.call", FALSE),
        call = NULL) {
    message <- .getErrorMessage(...)
    .stopRpactError(
        message,
        functionName = context$functionName,
        parameter = context$parameter,
        value = context$value,
        constraint = context$constraint,
        relatedParameter = context$relatedParameter,
        relatedValue = context$relatedValue,
        missingParameters = context$missingParameters,
        definedParameters = context$definedParameters,
        exceptionType = C_EXCEPTION_TYPE_INCOMPLETE_ARGUMENTS,
        factoryKey = "rpact.error.factory.incomplete.arguments.condition",
        code = "INCOMPLETE_ARGUMENTS",
        category = "invalid_user_input",
        class = c("rpact_incomplete_arguments_error", "rpact_input_error"),
        call. = call.,
        call = call
    )
}
