## |
## |  *Structured warning conditions*
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

#' @include f_error_handling.R
NULL

# Warning factories deliberately do not fall back to error factories.
# Set rpact.warning.factory.condition (or a type-specific factory) to enrich
# warnings in a client package. Factories receive the same diagnostic fields as
# error factories, plus reason and userInstructions, and must return a warning
# condition, not an error. Alternatively, rpact.warning.context = TRUE attaches
# the built-in context without requiring a client package.
# See inst/WARNING_CONTEXT.md for the schema and a complete capture example.
# Capture with withCallingHandlers(..., warning = function(w) { ...;
# invokeRestart("muffleWarning") }) to retain the successful calculation result.
.warnWithContext <- function(
        ..., code, category, parameter = NULL, value = NULL,
        constraint = NULL, functionName = NULL, relatedParameter = NULL,
        relatedValue = NULL, reason = NULL, userInstructions = NULL,
        call = NULL, call. = TRUE) {
    message <- .getErrorMessage(...)
    warningClass <- c(paste0("rpact_", tolower(code), "_warning"), "rpact_warning")
    factory <- getOption(paste0("rpact.warning.factory.",
        gsub("_", ".", tolower(code)), ".condition"))
    if (!is.function(factory)) {
        factory <- getOption("rpact.warning.factory.condition")
    }
    condition <- simpleWarning(message, call = if (isTRUE(call.)) call else NULL)
    class(condition) <- unique(c(warningClass, class(condition)))
    if (is.function(factory) || isTRUE(getOption("rpact.warning.context", FALSE))) {
        if (is.null(functionName) && !is.null(call)) {
            functionName <- paste(deparse(call[[1L]]), collapse = "")
        }
        context <- list(message = message, code = code, category = category,
            functionName = functionName, parameter = parameter, value = value,
            constraint = constraint, relatedParameter = relatedParameter,
            relatedValue = relatedValue, reason = if (is.null(reason)) message else reason,
            userInstructions = userInstructions, call = call)
        condition$context <- context
        if (is.function(factory)) {
            # Quote language-valued context (especially call) so factories receive
            # diagnostic data rather than re-evaluating the originating call.
            customCondition <- tryCatch(
                do.call(factory, c(context, list(class = warningClass)), quote = TRUE),
                error = function(e) {
                    message("Error in warning condition factory: ", conditionMessage(e))
                    NULL
                }
            )
            if (inherits(customCondition, "warning") && inherits(customCondition, "condition") &&
                    !inherits(customCondition, "error")) {
                class(customCondition) <- unique(c(warningClass, class(customCondition)))
                condition <- customCondition
            }
        }
    }
    base::warning(condition)
    invisible(NULL)
}

#' @noRd
warnArgumentIgnored <- function(
        ..., parameter = NULL, value = NULL, constraint = NULL,
        functionName = NULL, relatedParameter = NULL, relatedValue = NULL,
        reason = NULL, userInstructions = NULL, call = sys.call(-1L),
        call. = FALSE) {
    .warnWithContext(..., code = "ARGUMENT_IGNORED", category = "ignored_input",
        parameter = parameter, value = value, constraint = constraint,
        functionName = functionName, relatedParameter = relatedParameter,
        relatedValue = relatedValue, reason = reason,
        userInstructions = userInstructions, call = call, call. = call.)
}

#' @noRd
warnArgumentIgnoredFixedDesign <- function(
        parameter, value = NULL, call = sys.call(-1L), call. = FALSE) {
    warnArgumentIgnored(
        paste0("'", parameter, "'", collapse = ", "),
        " will be ignored for fixed sample design",
        parameter = parameter, value = value,
        relatedParameter = "kMax", relatedValue = 1L,
        constraint = "kMax must be greater than 1",
        reason = "A fixed sample design has only one stage and no interim adaptation.",
        userInstructions = paste0(
            "Use a multi-stage design if interim adaptation is intended; otherwise remove ",
            if (length(parameter) == 1L) "this argument " else "these arguments ",
            "after confirming the fixed-sample design."
        ),
        call = call, call. = call.
    )
}

#' @noRd
warnArgumentAdjusted <- function(
        ..., parameter = NULL, value = NULL, constraint = NULL,
        functionName = NULL, relatedParameter = NULL, relatedValue = NULL,
        reason = NULL, userInstructions = NULL, call = sys.call(-1L),
        call. = FALSE) {
    .warnWithContext(..., code = "ARGUMENT_ADJUSTED", category = "adjusted_input",
        parameter = parameter, value = value, constraint = constraint,
        functionName = functionName, relatedParameter = relatedParameter,
        relatedValue = relatedValue, reason = reason,
        userInstructions = userInstructions, call = call, call. = call.)
}

#' @noRd
warnInvalidInput <- function(
        ..., parameter = NULL, value = NULL, constraint = NULL,
        functionName = NULL, relatedParameter = NULL, relatedValue = NULL,
        reason = NULL, userInstructions = NULL, call = sys.call(-1L),
        call. = FALSE) {
    .warnWithContext(..., code = "INVALID_INPUT", category = "invalid_input",
        parameter = parameter, value = value, constraint = constraint,
        functionName = functionName, relatedParameter = relatedParameter,
        relatedValue = relatedValue, reason = reason,
        userInstructions = userInstructions, call = call, call. = call.)
}

#' @noRd
warnNumericalIssue <- function(
        ..., parameter = NULL, value = NULL, constraint = NULL,
        functionName = NULL, relatedParameter = NULL, relatedValue = NULL,
        reason = NULL, userInstructions = NULL, call = sys.call(-1L),
        call. = FALSE) {
    .warnWithContext(..., code = "NUMERICAL_ISSUE", category = "numerical_issue",
        parameter = parameter, value = value, constraint = constraint,
        functionName = functionName, relatedParameter = relatedParameter,
        relatedValue = relatedValue, reason = reason,
        userInstructions = userInstructions, call = call, call. = call.)
}

#' @noRd
warnResultUnavailable <- function(
        ..., parameter = NULL, value = NULL, constraint = NULL,
        functionName = NULL, relatedParameter = NULL, relatedValue = NULL,
        reason = NULL, userInstructions = NULL, call = sys.call(-1L),
        call. = FALSE) {
    .warnWithContext(..., code = "RESULT_UNAVAILABLE", category = "unavailable_result",
        parameter = parameter, value = value, constraint = constraint,
        functionName = functionName, relatedParameter = relatedParameter,
        relatedValue = relatedValue, reason = reason,
        userInstructions = userInstructions, call = call, call. = call.)
}

#' @noRd
warnNotValidated <- function(
        ..., parameter = NULL, value = NULL, constraint = NULL,
        functionName = NULL, relatedParameter = NULL, relatedValue = NULL,
        reason = NULL, userInstructions = NULL, call = sys.call(-1L),
        call. = FALSE) {
    .warnWithContext(..., code = "NOT_VALIDATED", category = "validation_limitation",
        parameter = parameter, value = value, constraint = constraint,
        functionName = functionName, relatedParameter = relatedParameter,
        relatedValue = relatedValue, reason = reason,
        userInstructions = userInstructions, call = call, call. = call.)
}

#' @noRd
warnRuntimeIssue <- function(
        ..., parameter = NULL, value = NULL, constraint = NULL,
        functionName = NULL, relatedParameter = NULL, relatedValue = NULL,
        reason = NULL, userInstructions = NULL, call = sys.call(-1L),
        call. = TRUE) {
    .warnWithContext(..., code = "RUNTIME_ISSUE", category = "runtime_issue",
        parameter = parameter, value = value, constraint = constraint,
        functionName = functionName, relatedParameter = relatedParameter,
        relatedValue = relatedValue, reason = reason,
        userInstructions = userInstructions, call = call, call. = call.)
}

#' @noRd
warnDataIssue <- function(
        ..., parameter = NULL, value = NULL, constraint = NULL,
        functionName = NULL, relatedParameter = NULL, relatedValue = NULL,
        reason = NULL, userInstructions = NULL, call = sys.call(-1L),
        call. = FALSE) {
    .warnWithContext(..., code = "DATA_ISSUE", category = "data_issue",
        parameter = parameter, value = value, constraint = constraint,
        functionName = functionName, relatedParameter = relatedParameter,
        relatedValue = relatedValue, reason = reason,
        userInstructions = userInstructions, call = call, call. = call.)
}

