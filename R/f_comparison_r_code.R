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

.getComparisonObjectRCode <- function(object, includeDefaultParameters, tolerance, pipeOperator) {
    code <- getObjectRCode(object, includeDefaultParameters = includeDefaultParameters,
        stringWrapParagraphWidth = NULL, pipeOperator = "none", output = "internal", tolerance = tolerance)
    # Reuse standard reconstruction, then inline its prerequisite assignments.
    # Each object remains a single expression without shared temporary variables.
    expressions <- as.list(parse(text = code))
    bindings <- list()
    inlineExpression <- function(expression) {
        if (is.call(expression) && identical(expression[[1L]], as.name("function"))) {
            return(expression)
        }
        firstArgument <- NULL
        if (is.call(expression) && length(expression) > 1L && is.symbol(expression[[2L]])) {
            firstArgument <- as.character(expression[[2L]])
        }
        if (identical(firstArgument, "design") && !includeDefaultParameters &&
                identical(bindings$design, quote(getDesignFixed()))) {
            expression[[2L]] <- NULL
            firstArgument <- NULL
        }
        if (!is.null(firstArgument) && firstArgument %in% names(bindings) &&
                pipeOperator != "none" &&
                firstArgument %in% c("design", "data", "stageResults", "simulationResults", "futilityBounds", "alpha0Vec")) {
            expression[[2L]] <- NULL
            expression <- do.call(substitute, list(expression, bindings))
            operator <- if (pipeOperator == "R") {
                "%rpactReconstructionPipe%"
            } else {
                "%>%"
            }
            return(as.call(list(as.name(operator), bindings[[firstArgument]], expression)))
        }
        return(do.call(substitute, list(expression, bindings)))
    }
    for (expression in head(expressions, -1L)) {
        bindings[[as.character(expression[[2L]])]] <- inlineExpression(expression[[3L]])
    }
    code <- deparse(inlineExpression(expressions[[length(expressions)]]), width.cutoff = 100L)
    if (pipeOperator == "R") {
        # Deparsing native pipe calls uses function-call syntax. Render a temporary
        # infix token instead, preserving operators and strings in callback bodies.
        parseData <- utils::getParseData(parse(text = code, keep.source = TRUE))
        pipes <- parseData[parseData$terminal & parseData$token == "SPECIAL" & parseData$text == "%rpactReconstructionPipe%", ]
        pipes <- pipes[order(pipes$line1, pipes$col1, decreasing = TRUE), ]
        for (i in seq_len(nrow(pipes))) {
            line <- pipes$line1[i]
            code[line] <- paste0(substr(code[line], 1L, pipes$col1[i] - 1L), "|>",
                substring(code[line], pipes$col2[i] + 1L))
        }
    }
    return(code)
}

.getTrialComparisonRCode <- function(object, leadingArguments, includeDefaultParameters,
        prefix, postfix, stringWrapPrefix, newArgumentValues, tolerance,
        pipeOperator, pipeOperatorPostfix, output, explicitPrint) {
    if (length(leadingArguments) || !is.list(newArgumentValues) || length(newArgumentValues)) {
        stopIllegalArgument("TrialComparison reconstruction does not support argument overrides",
            functionName = "getObjectRCode", parameter = c("leadingArguments", "newArgumentValues"),
            value = list(leadingArguments = leadingArguments, newArgumentValues = newArgumentValues),
            constraint = "leadingArguments and newArgumentValues must be empty",
            diagnosticId = "comparison.r_code_overrides_unsupported")
    }
    objects <- object$getObjects()
    expressions <- vapply(objects, function(item) {
        return(paste(.getComparisonObjectRCode(item, includeDefaultParameters, tolerance, pipeOperator), collapse = "\n"))
    }, "")
    objectNames <- names(objects)
    # A list preserves names that would otherwise match getComparison's formals.
    useList <- any(objectNames %in% c("objects", ""))
    if (!is.null(objectNames) && !useList) {
        expressions <- paste0(vapply(objectNames, function(name) {
            return(paste(deparse(name), collapse = "\n"))
        }, ""), " = ", expressions)
    }
    objectsCode <- paste0("\n    ", gsub("\n", "\n    ", paste(expressions, collapse = ",\n")), "\n")
    if (!length(objects)) {
        objectsCode <- ""
    } else if (useList) {
        objectsCode <- paste0("structure(list(", objectsCode, "), names = ",
            paste(deparse(objectNames, width.cutoff = 500L), collapse = "\n"), ")")
    }
    # The generic word wrapper is not syntax-aware; preserve embedded newlines
    # and user-supplied names and callback code instead of reflowing them.
    result <- .formatRCode(paste0(prefix, "getComparison(", objectsCode, ")"),
        precondition = character(), stringWrapParagraphWidth = NULL, postfix = postfix,
        stringWrapPrefix = stringWrapPrefix, pipeOperator = pipeOperator,
        pipeOperatorPostfix = pipeOperatorPostfix, output = output, explicitPrint = explicitPrint)
    if (output %in% c("vector", "internal")) {
        return(unlist(strsplit(result, "\n", fixed = TRUE), use.names = FALSE))
    }
    if (output == "markdown") {
        return(result)
    }
    return(invisible(result))
}

