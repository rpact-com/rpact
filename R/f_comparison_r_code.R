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
        # Reconstructed designs, datasets and callbacks need independent scopes.
        code <- getObjectRCode(item, includeDefaultParameters = includeDefaultParameters,
            stringWrapParagraphWidth = NULL, pipeOperator = "none", output = "internal", tolerance = tolerance)
        paste0("local({\n", paste0("    ", code, collapse = "\n"), "\n})")
    }, "")
    objectsCode <- paste0("list(\n", paste(expressions, collapse = ",\n"), "\n)")
    if (!is.null(names(objects))) {
        objectsCode <- paste0("structure(", objectsCode, ", names = ",
            paste(deparse(names(objects), width.cutoff = 500L), collapse = "\n"), ")")
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
    result
}

