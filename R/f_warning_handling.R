## |
## |  rpact: Confirmatory Adaptive Clinical Trial Design and Analysis
## |  This file is part of the rpact R package.
## |
## |  RPACT — Statistical Tools for Drug Development
## |
## |  Authors: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
## |  SPDX-FileCopyrightText: RPACT GmbH, Germany <info@rpact.com>
## |  SPDX-License-Identifier: LGPL-3.0-only
## |
## |  Licensed under the GNU Lesser General Public License, version 3.
## |  License text: https://www.gnu.org/licenses/lgpl-3.0.html
## |
## |  Package documentation: https://www.rpact.org
## |  Resources and customer portal: https://connect.rpact.app
## |  Consulting and services: https://www.rpact.com
## |  Contact: info@rpact.com
## |

#' @include f_error_handling.R
NULL

.warnWithContext <- function(
        ..., code, category, parameter = NULL, value = NULL,
        constraint = NULL, functionName = NULL, relatedParameter = NULL,
        relatedValue = NULL, diagnosticId = NULL, context = NULL,
        call = NULL, call. = TRUE) {
    message <- .getErrorMessage(...)
    warningClass <- c(paste0("rpact_", tolower(code), "_warning"), "rpact_warning")
    optionId <- paste0("rpact.warning.factory.", gsub("_", ".", tolower(code)), ".condition")
    factory <- getOption(optionId)
    if (!is.function(factory)) {
        factory <- getOption("rpact.warning.factory.condition")
    }
    
    condition <- simpleWarning(message, call = if (isTRUE(call.)) call else NULL)
    class(condition) <- unique(c(warningClass, class(condition)))
    if (is.function(factory) || isTRUE(getOption("rpact.warning.context", FALSE))) {
        if (is.null(functionName) && !is.null(call)) {
            functionName <- paste(deparse(call[[1L]]), collapse = "")
        }
        diagnosticContext <- list(message = message, code = code, category = category,
            functionName = functionName, parameter = parameter, value = value,
            constraint = constraint, relatedParameter = relatedParameter,
            relatedValue = relatedValue, diagnosticId = diagnosticId,
            context = context, call = call)
        condition$context <- diagnosticContext
        if (is.function(factory)) {
            # Quote language-valued context (especially call) so factories receive
            # diagnostic data rather than re-evaluating the originating call.
            customCondition <- tryCatch(
                do.call(factory, c(diagnosticContext, list(class = warningClass)), quote = TRUE),
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
    
    if (isFALSE(as.logical(getOption(paste0(
            "rpact.warning.", gsub("_", ".", tolower(code)), ".enabled"))))) {
        return(invisible(NULL))
    }
    
    base::warning(condition)
    return(invisible(NULL))
}

#' @noRd
warnArgumentIgnored <- function(
        ..., parameter = NULL, value = NULL, constraint = NULL,
        functionName = NULL, relatedParameter = NULL, relatedValue = NULL,
        diagnosticId = NULL, context = NULL, call = sys.call(-1L),
        call. = FALSE) {
    .warnWithContext(..., code = "ARGUMENT_IGNORED", category = "ignored_input",
        parameter = parameter, value = value, constraint = constraint,
        functionName = functionName, relatedParameter = relatedParameter,
        relatedValue = relatedValue, diagnosticId = diagnosticId,
        context = context, call = call, call. = call.)
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
        call = call, call. = call.,
        diagnosticId = "design.argument_ignored_fixed_design"
    )
}

#' @noRd
warnArgumentAdjusted <- function(
        ..., parameter = NULL, value = NULL, constraint = NULL,
        functionName = NULL, relatedParameter = NULL, relatedValue = NULL,
        diagnosticId = NULL, context = NULL, call = sys.call(-1L),
        call. = FALSE) {
    .warnWithContext(..., code = "ARGUMENT_ADJUSTED", category = "adjusted_input",
        parameter = parameter, value = value, constraint = constraint,
        functionName = functionName, relatedParameter = relatedParameter,
        relatedValue = relatedValue, diagnosticId = diagnosticId,
        context = context, call = call, call. = call.)
}

#' @noRd
warnInvalidInput <- function(
        ..., parameter = NULL, value = NULL, constraint = NULL,
        functionName = NULL, relatedParameter = NULL, relatedValue = NULL,
        diagnosticId = NULL, context = NULL, call = sys.call(-1L),
        call. = FALSE) {
    .warnWithContext(..., code = "INVALID_INPUT", category = "invalid_input",
        parameter = parameter, value = value, constraint = constraint,
        functionName = functionName, relatedParameter = relatedParameter,
        relatedValue = relatedValue, diagnosticId = diagnosticId,
        context = context, call = call, call. = call.)
}

#' @noRd
warnNumericalIssue <- function(
        ..., parameter = NULL, value = NULL, constraint = NULL,
        functionName = NULL, relatedParameter = NULL, relatedValue = NULL,
        diagnosticId = NULL, context = NULL, call = sys.call(-1L),
        call. = FALSE) {
    .warnWithContext(..., code = "NUMERICAL_ISSUE", category = "numerical_issue",
        parameter = parameter, value = value, constraint = constraint,
        functionName = functionName, relatedParameter = relatedParameter,
        relatedValue = relatedValue, diagnosticId = diagnosticId,
        context = context, call = call, call. = call.)
}

#' @noRd
warnResultUnavailable <- function(
        ..., parameter = NULL, value = NULL, constraint = NULL,
        functionName = NULL, relatedParameter = NULL, relatedValue = NULL,
        diagnosticId = NULL, context = NULL, call = sys.call(-1L),
        call. = FALSE) {
    .warnWithContext(..., code = "RESULT_UNAVAILABLE", category = "unavailable_result",
        parameter = parameter, value = value, constraint = constraint,
        functionName = functionName, relatedParameter = relatedParameter,
        relatedValue = relatedValue, diagnosticId = diagnosticId,
        context = context, call = call, call. = call.)
}

#' @noRd
warnNotValidated <- function(
        ..., parameter = NULL, value = NULL, constraint = NULL,
        functionName = NULL, relatedParameter = NULL, relatedValue = NULL,
        diagnosticId = NULL, context = NULL, call = sys.call(-1L),
        call. = FALSE) {
    .warnWithContext(..., code = "NOT_VALIDATED", category = "validation_limitation",
        parameter = parameter, value = value, constraint = constraint,
        functionName = functionName, relatedParameter = relatedParameter,
        relatedValue = relatedValue, diagnosticId = diagnosticId,
        context = context, call = call, call. = call.)
}

#' @noRd
warnRuntimeIssue <- function(
        ..., parameter = NULL, value = NULL, constraint = NULL,
        functionName = NULL, relatedParameter = NULL, relatedValue = NULL,
        diagnosticId = NULL, context = NULL, call = sys.call(-1L),
        call. = TRUE) {
    .warnWithContext(..., code = "RUNTIME_ISSUE", category = "runtime_issue",
        parameter = parameter, value = value, constraint = constraint,
        functionName = functionName, relatedParameter = relatedParameter,
        relatedValue = relatedValue, diagnosticId = diagnosticId,
        context = context, call = call, call. = call.)
}

#' @noRd
warnDataIssue <- function(
        ..., parameter = NULL, value = NULL, constraint = NULL,
        functionName = NULL, relatedParameter = NULL, relatedValue = NULL,
        diagnosticId = NULL, context = NULL, call = sys.call(-1L),
        call. = FALSE) {
    .warnWithContext(..., code = "DATA_ISSUE", category = "data_issue",
        parameter = parameter, value = value, constraint = constraint,
        functionName = functionName, relatedParameter = relatedParameter,
        relatedValue = relatedValue, diagnosticId = diagnosticId,
        context = context, call = call, call. = call.)
}

