## |
## |  *Quantitative Trial Design (QTD)*
## |
## |  This file is part of the R package rpact:
## |  Confirmatory Adaptive Clinical Trial Design and Analysis
## |
## |  Author: Friedrich Pahlke, PhD and Daniel Saban<U+00E9>s Bov<U+00E9>, PhD
## |  Licensed under "GNU Lesser General Public License" version 3
## |  License text can be found here: https://www.r-project.org/Licenses/LGPL-3
## |
## |  RPACT company website: https://www.rpact.com
## |  rpact package website: https://www.rpact.org
## |
## |  Contact us for information about our services: info@rpact.com
## |

## Quantitative trial design summaries. Keep QTD-specific rendering here so that
## the shared summary infrastructure only needs small dispatch additions.

#' @include class_summary.R
NULL

#'
#' @name summaryQuantitative
#'
#' @title
#' Summaries of Quantitative Trial Designs and Operating Characteristics
#'
#' @description
#' The standard [summary()] method supports quantitative decision rules from
#' [getDesignQuantitative()] and results from [getOperatingCharcteristicsMeans()].
#'
#' @details
#' Use `summary(rule)` for the target value, lower reference value and decision
#' percentiles. Use `summary(oc)` for the decision rule, assumptions and category
#' probabilities. Each sample-size scenario has a separate table, with one row
#' per alternative and one column per decision category. Categories are retained
#' separately; probabilities are neither recomputed nor aggregated.
#'
#' The `digits` and `output` arguments follow [summary.ParameterSet()]. The
#' output parts are `"title"`, `"overview"`, `"body"` and `"all"`. For a decision
#' rule, the body contains its parameters. For operating characteristics, the
#' overview contains the rule and assumptions, and the body contains the tables.
#' Summary precision follows `rpact.summary.digits` and
#' `rpact.summary.digits.probs`. The summary keeps a copy of the supplied object,
#' so later changes to the original do not change an existing summary.
#'
#' Print a summary with `print(summary(oc), markdown = TRUE)` to obtain Markdown
#' with probability tables. Console tables use wrapped, readable column names.
#'
#' @return A `SummaryFactory` object supporting console and Markdown output.
#'
#' @seealso [summary.ParameterSet()]
#'
NULL

.isQuantitativeSummaryObject <- function(object) {
    return(inherits(object, "TrialDesignPlanQuantitative") || inherits(object, "OperatingCharcteristicsMeans"))
}

.createSummaryQuantitative <- function(object, digits, output, markdown = FALSE) {
    digitSettings <- .getSummaryDigits(digits)
    result <- SummaryFactory$new(
        object = object$clone(deep = TRUE), output = output, markdown = markdown
    )
    result$title <- object$.toString(startWithUpperCase = TRUE)
    attr(result, "qtdSummaryDigits") <- digitSettings
    return(result)
}

.showQuantitativeSummaryParameters <- function(summaryFactory, object, parameterNames,
        digitSettings, consoleOutputEnabled) {
    for (parameterName in parameterNames) {
        values <- object[[parameterName]]
        if (parameterName == "groups") {
            values <- .formatSampleSizes(values)
        } else if (is.numeric(values)) {
            digits <- digitSettings$digitsGeneral
            if (parameterName %in% c("upperPercentile", "lowerPercentile")) {
                digits <- digitSettings$digitsProbabilities
            }
            values <- trimws(.formatSummaryValues(values, digits = digits))
        }
        caption <- .getParameterCaption(parameterName, object)
        summaryFactory$.cat("  ", caption, ": ", paste(values, collapse = ", "), "\n",
            consoleOutputEnabled = consoleOutputEnabled
        )
    }
    summaryFactory$.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
}

.getQuantitativeSummaryTable <- function(object, scenario, digitSettings) {
    probabilities <- object$probabilities
    categories <- unique(probabilities$category)
    rowsPerScenario <- length(object$alternative) * length(categories)
    rows <- (scenario - 1L) * rowsPerScenario + seq_len(rowsPerScenario)
    probabilities <- probabilities[rows, , drop = FALSE]
    firstCategory <- probabilities$category == categories[1L]
    result <- data.frame(
        alternative = trimws(.formatSummaryValues(probabilities$alternative[firstCategory],
            digits = digitSettings$digitsGeneral
        )),
        stringsAsFactors = FALSE
    )
    for (category in categories) {
        result[[category]] <- trimws(.formatSummaryValues(
            probabilities$probability[probabilities$category == category],
            digits = digitSettings$digitsProbabilities, smoothedZeroFormat = TRUE
        ))
    }
    return(result)
}

.showSummaryQuantitative <- function(summaryFactory, digits = NA_integer_, consoleOutputEnabled = TRUE) {
    summaryFactory$.resetCat()
    object <- summaryFactory$object
    digitSettings <- attr(summaryFactory, "qtdSummaryDigits")
    if (!is.na(digits)) {
        digitSettings <- .getSummaryDigits(digits)
    }
    operatingCharacteristics <- inherits(object, "OperatingCharcteristicsMeans")
    ruleParameters <- c("targetValue", "lowerReferenceValue", "upperPercentile", "lowerPercentile")

    if (summaryFactory$output %in% c("all", "title")) {
        summaryFactory$.cat(summaryFactory$title, "\n\n",
            heading = 1, consoleOutputEnabled = consoleOutputEnabled
        )
    }
    if (summaryFactory$output %in% c("all", "overview")) {
        if (operatingCharacteristics) {
            summaryFactory$.cat("Decision rule:\n\n", heading = 2, consoleOutputEnabled = consoleOutputEnabled)
            .showQuantitativeSummaryParameters(summaryFactory, object$.design, ruleParameters,
                digitSettings, consoleOutputEnabled
            )
            summaryFactory$.cat("Assumptions:\n\n", heading = 2, consoleOutputEnabled = consoleOutputEnabled)
            .showQuantitativeSummaryParameters(summaryFactory, object,
                c("normalApproximation", "groups", "stDev"), digitSettings, consoleOutputEnabled
            )
        } else {
            summaryFactory$.cat("Fixed quantitative trial decision rule.\n\n",
                consoleOutputEnabled = consoleOutputEnabled
            )
        }
    }
    if (!(summaryFactory$output %in% c("all", "body"))) {
        return(invisible())
    }
    if (!operatingCharacteristics) {
        .showQuantitativeSummaryParameters(summaryFactory, object, ruleParameters,
            digitSettings, consoleOutputEnabled
        )
        return(invisible())
    }

    for (scenario in seq_len(nrow(object$numberOfSubjects))) {
        sampleSizes <- .formatSampleSizes(object$numberOfSubjects[scenario, ])
        summaryFactory$.cat("Number of subjects (control = ", sampleSizes[1L],
            ", treatment = ", sampleSizes[2L], ")\n\n",
            heading = 2, consoleOutputEnabled = consoleOutputEnabled
        )
        table <- .getQuantitativeSummaryTable(object, scenario, digitSettings)
        .showDataFrame(table, summaryFactory,
            niceColumnNamesEnabled = TRUE, consoleOutputEnabled = consoleOutputEnabled
        )
    }
}
