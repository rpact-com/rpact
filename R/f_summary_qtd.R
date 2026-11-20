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
#' [getDesignQuantitative()] and results from [getOperatingCharacteristicsMeans()]
#' and [getOperatingCharacteristicsRates()].
#'
#' @details
#' Use `print(rule)` for compact decision criteria and `summary(rule)` for
#' reference cutoffs, applicable percentile inputs and detailed decision tables.
#' Use `summary(oc)` for the decision rule, assumptions and category
#' probabilities. Each sample-size scenario has a separate table, with one row
#' per alternative and one column per decision category. Categories are retained
#' separately; probabilities are neither recomputed nor aggregated.
#' Names supplied with `pi1` or `alternative` label the rows in each table.
#' Rates tables show the treatment event probability and the beneficial risk
#' difference. Their assumptions include the control event probability,
#' beneficial direction and selected interval method.
#'
#' The `digits` and `output` arguments follow [summary.ParameterSet()]. The
#' output parts are `"title"`, `"overview"`, `"body"` and `"all"`. For a decision
#' rule, the body contains its tables. For operating characteristics, the
#' overview contains the rule and assumptions, and the body contains the tables.
#' An explicit `digits` sets the precision for both general values and
#' probabilities. Otherwise, precision follows `rpact.summary.digits` and
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
#' @examples
#' rule <- getDesignQuantitative(targetValue = 0, lowerReferenceValue = -0.05)
#' rates <- getOperatingCharacteristicsRates(rule,
#'     numberOfSubjects = matrix(c(75, 75, 75, 150), ncol = 2, byrow = TRUE),
#'     pi1 = c(0.1, 0.15, 0.175, 0.2, 0.25), pi2 = 0.15, directionUpper = FALSE)
#' summary(rates)
#' cat(print(summary(rates), markdown = TRUE)[1], "\n")
#'
NULL

#'
#' @title
#' Identify Quantitative Summary Objects
#'
#' @description
#' Identifies decision rules and operating characteristics handled by the QTD summary
#' renderer.
#'
#' @param object An object being considered for summary dispatch.
#'
#' @return A logical value indicating whether the object uses QTD summaries.
#'
#' @noRd
#'
.isQuantitativeSummaryObject <- function(object) {
    return(inherits(object, "TrialDesignPlanQuantitativeBase") || .isQtdOperatingCharacteristics(object))
}

#'
#' @title
#' Create a Quantitative Trial Design Summary
#'
#' @description
#' Clones the source object and captures the precision settings used to render its summary.
#'
#' @param object A QTD rule or operating characteristics object.
#' @param digits Display precision, or `NA_integer_` to use the summary settings.
#' @param output The requested summary part: `"all"`, `"title"`, `"overview"`, or `"body"`.
#' @param markdown Whether the summary should use Markdown output.
#'
#' @return A `SummaryFactory` with the cloned object and its display settings.
#'
#' @noRd
#'
.createSummaryQuantitative <- function(object, digits, output, markdown = FALSE) {
    digitSettings <- .getSummaryDigits(digits, useExplicitDigitsForProbabilities = TRUE)
    result <- SummaryFactory$new(
        object = object$clone(deep = TRUE), output = output, markdown = markdown
    )
    result$title <- object$.toString(startWithUpperCase = TRUE)
    attr(result, "qtdSummaryDigits") <- digitSettings
    return(result)
}

#'
#' @title
#' Render Quantitative Summary Parameters
#'
#' @description
#' Formats applicable parameters for the operating characteristics overview.
#'
#' @param summaryFactory The summary object receiving the formatted output.
#' @param object The QTD rule or operating characteristics object supplying the values.
#' @param parameterNames Character vector of parameter names in display order.
#' @param digitSettings The precision settings returned by `.getSummaryDigits()`.
#' @param consoleOutputEnabled Whether to write to the console. If false, collects Markdown.
#'
#' @noRd
#'
.showQuantitativeSummaryParameters <- function(summaryFactory, object, parameterNames,
        digitSettings, consoleOutputEnabled) {
    for (parameterName in parameterNames) {
        if (object$isNotApplicableParameter(parameterName)) {
            next
        }
        values <- object[[parameterName]]
        if (inherits(values, "QuantitativeDecisionThresholds")) {
            values <- paste0("P(effect ", ifelse(values$lowerTail, "<= ", "> "),
                values$quantiles, ") > ", values$probabilities)
        } else if (inherits(values, "mix") || inherits(values, "normMix") || inherits(values, "betaMix") ||
                (is.list(values) && length(values) == 2L)) {
            formatPrior <- function(prior) {
                return(paste0(class(prior)[1L], ": ", paste(vapply(seq_len(ncol(prior)), function(i) {
                    return(paste(signif(prior[, i], 5), collapse = ", "))
                }, character(1)), collapse = "; ")))
            }
            if (is.list(values)) {
                values <- paste(vapply(values, formatPrior, character(1)), collapse = " / ")
            } else {
                values <- formatPrior(values)
            }
        }
        if (parameterName == "groups") {
            values <- .formatSampleSizes(values)
        } else if (is.numeric(values)) {
            digits <- digitSettings$digitsGeneral
            if (parameterName %in% c("upperPercentile", "lowerPercentile", "pi1", "pi2")) {
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

#'
#' @title
#' Format Quantitative Decision Cutoffs
#'
#' @description
#' Formats numeric cutoffs for display and preserves small nonzero values that would round
#' to zero.
#'
#' @param values Numeric effect thresholds, percentile levels, or probability cutoffs.
#' @param digits Number of decimal places. A value below one uses standard character
#'   conversion.
#' @param trimZeros Whether to remove trailing decimal zeros.
#'
#' @return A character vector in the same order as `values`.
#'
#' @noRd
#'
.formatQtdRuleValues <- function(values, digits, trimZeros = FALSE) {
    if (digits < 1L) {
        return(as.character(values))
    }
    formatted <- sprintf("%.*f", digits, values)
    # Keep small nonzero cutoffs readable inside inequalities.
    roundedToZero <- !is.na(values) & values != 0 & as.numeric(formatted) == 0
    formatted[roundedToZero] <- as.character(values[roundedToZero])
    if (trimZeros) {
        formatted <- sub("(\\.[0-9]*[1-9])0+$", "\\1", formatted)
        formatted <- sub("\\.0+$", "", formatted)
    }
    return(formatted)
}

#'
#' @title
#' Format Posterior Decision Criteria
#'
#' @description
#' Formats the stored posterior comparisons as a conjunction while preserving the direction
#' of each tail.
#'
#' @param criteria A `QuantitativeDecisionThresholds` object for one decision category.
#' @param digitSettings The precision settings returned by `.getSummaryDigits()`.
#' @param trimZeros Whether to remove trailing decimal zeros from effects and probabilities.
#'
#' @return A character scalar containing the criteria joined by `and`.
#'
#' @noRd
#'
.formatQtdPosteriorCriteria <- function(criteria, digitSettings, trimZeros = FALSE) {
    effects <- .formatQtdRuleValues(criteria$quantiles, digitSettings$digitsGeneral, trimZeros)
    probabilities <- .formatQtdRuleValues(criteria$probabilities, digitSettings$digitsProbabilities, trimZeros)
    return(paste(
        paste0("P(effect ", ifelse(criteria$lowerTail, "<= ", "> "), effects, ") > ", probabilities),
        collapse = " and "
    ))
}

#'
#' @title
#' Extract Posterior Decision Criteria
#'
#' @description
#' Retrieves the criteria stored explicitly for Go, Stop, and Indeterminate 1.
#'
#' @param rule A Bayesian QTD decision rule.
#'
#' @return A named list of `QuantitativeDecisionThresholds` objects in display order.
#'
#' @noRd
#'
.getQtdPosteriorCriteria <- function(rule) {
    return(list(Go = rule$go, Stop = rule$stop, "Indeterminate 1" = rule$indeterminate1))
}

#'
#' @title
#' Render a Compact Quantitative Decision Rule
#'
#' @description
#' Displays the confidence-bound comparisons for a frequentist rule or the posterior
#' comparisons for a Bayesian rule.
#'
#' @param rule A frequentist or Bayesian QTD decision rule.
#' @param digits Display precision, or `NA_integer_` for standard numeric character
#'   conversion.
#' @param consoleOutputEnabled Whether to write to the console. If false, collects Markdown.
#'
#' @noRd
#'
.showQuantitativeRule <- function(rule, digits, consoleOutputEnabled) {
    if (is.na(digits)) {
        digits <- -1L
    }
    digitSettings <- .getSummaryDigits(digits)
    rule$.cat(rule$.toString(startWithUpperCase = TRUE), "\n\n",
        heading = 1, consoleOutputEnabled = consoleOutputEnabled
    )
    if (inherits(rule, "TrialDesignPlanQuantitativeBayes")) {
        description <- "Probabilities are posterior probabilities."
        criteria <- vapply(.getQtdPosteriorCriteria(rule), .formatQtdPosteriorCriteria,
            character(1), digitSettings = digitSettings, trimZeros = TRUE
        )
        criteria <- c(criteria, "Indeterminate 2" = "All remaining outcomes")
    } else {
        percentiles <- .formatQtdRuleValues(c(rule$lowerPercentile, rule$upperPercentile),
            digitSettings$digitsProbabilities, trimZeros = TRUE
        )
        description <- paste0("L = lower confidence bound (percentile level ", percentiles[1L], ")\n",
            "U = upper confidence bound (percentile level ", percentiles[2L], ")"
        )
        effects <- .formatQtdRuleValues(c(rule$lowerReferenceValue, rule$targetValue),
            digitSettings$digitsGeneral, trimZeros = TRUE
        )
        criteria <- c(
            Go = paste0("L >= ", effects[1L], " and U >= ", effects[2L]),
            Stop = paste0("L < ", effects[1L], " and U < ", effects[2L]),
            Indeterminate = "Exactly one Go criterion is met"
        )
    }
    if (!consoleOutputEnabled) {
        description <- sub("\n", "  \n", description, fixed = TRUE)
    }
    rule$.cat(description, "\n\n", consoleOutputEnabled = consoleOutputEnabled)
    labels <- names(criteria)
    if (consoleOutputEnabled) {
        lines <- paste(format(labels, justify = "left"), criteria, sep = "  ")
    } else {
        lines <- paste0("- **", labels, "** ", .escapeMarkdownTableCell(criteria))
    }
    rule$.cat(paste(lines, collapse = "\n"), "\n\n", consoleOutputEnabled = consoleOutputEnabled)
}

#'
#' @title
#' Render Quantitative Decision Rule Tables
#'
#' @description
#' Displays applicable reference inputs and the detailed decision regions. For a reference-
#' based Bayesian rule, expresses all posterior comparisons using the upper tail.
#'
#' @param summaryFactory The summary object receiving the tables.
#' @param rule A frequentist or Bayesian QTD decision rule.
#' @param digitSettings The precision settings returned by `.getSummaryDigits()`.
#' @param consoleOutputEnabled Whether to write to the console. If false, collects Markdown.
#'
#' @noRd
#'
.showQtdRuleTables <- function(summaryFactory, rule, digitSettings, consoleOutputEnabled) {
    bayesian <- inherits(rule, "TrialDesignPlanQuantitativeBayes")
    if (!bayesian || rule$isApplicableParameter("percentileMapping")) {
        effects <- .formatQtdRuleValues(c(rule$lowerReferenceValue, rule$targetValue), digitSettings$digitsGeneral)
        percentiles <- c(rule$lowerPercentile, rule$upperPercentile)
        references <- data.frame("Effect cutoff" = effects, check.names = FALSE,
            row.names = c("Lower reference value", "Target value")
        )
        if (bayesian) {
            if (identical(rule$percentileMapping, "posteriorProbability")) {
                percentiles <- rev(percentiles)
            }
            references[["Input percentile"]] <- .formatQtdRuleValues(percentiles, digitSettings$digitsProbabilities)
            references[["P(effect > cutoff)"]] <- .formatQtdRuleValues(rule$go$probabilities,
                digitSettings$digitsProbabilities
            )
        } else {
            percentiles <- .formatQtdRuleValues(100 * percentiles, digitSettings$digitsProbabilities, trimZeros = TRUE)
            references[["Confidence bound"]] <- paste0(c("L", "U"), " (", percentiles, "%)")
        }
        .showDataFrame(references, summaryFactory, niceColumnNamesEnabled = FALSE,
            consoleOutputEnabled = consoleOutputEnabled
        )
    }

    if (!bayesian) {
        lower <- effects[1L]
        target <- effects[2L]
        decisions <- data.frame(
            "Interval configuration" = c(
                paste0("L >= ", target),
                paste0(lower, " <= L < ", target, " and U >= ", target),
                paste0("L < ", lower, " and U >= ", target),
                paste0("L >= ", lower, " and U < ", target),
                paste0("L < ", lower, " and ", lower, " <= U < ", target),
                paste0("U < ", lower)
            ),
            row.names = .getQtdCategories(), check.names = FALSE
        )
    } else {
        criteria <- .getQtdPosteriorCriteria(rule)
        if (rule$isNotApplicableParameter("percentileMapping")) {
            decisions <- data.frame(
                "Posterior criteria" = c(vapply(criteria, .formatQtdPosteriorCriteria,
                    character(1), digitSettings = digitSettings
                ), "All remaining outcomes"),
                row.names = c(names(criteria), "Indeterminate 2"), check.names = FALSE
            )
        } else {
            conditions <- t(vapply(criteria, function(criterion) {
                probabilities <- ifelse(criterion$lowerTail, 1 - criterion$probabilities, criterion$probabilities)
                return(paste(ifelse(criterion$lowerTail, "<", ">"),
                    .formatQtdRuleValues(probabilities, digitSettings$digitsProbabilities)
                ))
            }, character(2)))
            decisions <- as.data.frame(conditions, optional = TRUE)
            names(decisions) <- paste0("P(effect > ", .formatQtdRuleValues(
                c(rule$lowerReferenceValue, rule$targetValue), digitSettings$digitsGeneral, trimZeros = TRUE
            ), ")")
            if (anyDuplicated(names(decisions))) {
                names(decisions) <- paste(c("LRV", "Target"), names(decisions))
            }
        }
    }
    .showDataFrame(decisions, summaryFactory, niceColumnNamesEnabled = FALSE,
        consoleOutputEnabled = consoleOutputEnabled
    )
    if (bayesian && rule$isApplicableParameter("percentileMapping")) {
        summaryFactory$.cat("Indeterminate 2 includes all remaining outcomes, including exact cutoff ties.\n\n",
            consoleOutputEnabled = consoleOutputEnabled
        )
    }
}

#'
#' @title
#' Build an Operating Characteristics Summary Table
#'
#' @description
#' Formats the stored decision probabilities for one sample-size scenario, preserving the
#' order of alternatives and categories.
#'
#' @param object A QTD operating characteristics object.
#' @param scenario The one-based row index in the sample-size matrix.
#' @param digitSettings The precision settings returned by `.getSummaryDigits()`.
#'
#' @return A data frame with one row per alternative and one probability column per decision
#'   category.
#'
#' @noRd
#'
.getQuantitativeSummaryTable <- function(object, scenario, digitSettings) {
    probabilities <- object$probabilities
    categories <- unique(probabilities$category)
    rowsPerScenario <- length(object$alternative) * length(categories)
    rows <- (scenario - 1L) * rowsPerScenario + seq_len(rowsPerScenario)
    probabilities <- probabilities[rows, , drop = FALSE]
    firstCategory <- probabilities$category == categories[1L]
    settings <- .getQtdEndpointSettings(object)
    rowNames <- names(object$alternative)
    if (inherits(object, "OperatingCharacteristicsRates")) {
        rowNames <- names(object$pi1)
    }
    if (is.null(rowNames)) {
        rowNames <- seq_len(sum(firstCategory))
    }
    result <- data.frame(row.names = rowNames)
    for (column in settings$summaryColumns) {
        digits <- digitSettings$digitsGeneral
        if (column == "pi1") {
            digits <- digitSettings$digitsProbabilities
        }
        result[[column]] <- trimws(.formatSummaryValues(probabilities[[column]][firstCategory], digits = digits))
    }
    for (category in categories) {
        result[[category]] <- trimws(.formatSummaryValues(
            probabilities$probability[probabilities$category == category],
            digits = digitSettings$digitsProbabilities, smoothedZeroFormat = TRUE
        ))
    }
    return(result)
}

#'
#' @title
#' Render a Quantitative Trial Design Summary
#'
#' @description
#' Renders the requested title, overview, or tables for a decision rule or operating
#' characteristics object.
#'
#' @param summaryFactory The QTD summary object supplying the source object and requested
#'   output part.
#' @param digits Display precision, or `NA_integer_` to use the captured settings.
#' @param consoleOutputEnabled Whether to write to the console. If false, collects Markdown.
#'
#' @noRd
#'
.showSummaryQuantitative <- function(summaryFactory, digits = NA_integer_, consoleOutputEnabled = TRUE) {
    summaryFactory$.resetCat()
    object <- summaryFactory$object
    digitSettings <- attr(summaryFactory, "qtdSummaryDigits")
    if (!is.na(digits)) {
        digitSettings <- .getSummaryDigits(digits, useExplicitDigitsForProbabilities = TRUE)
    }
    operatingCharacteristics <- .isQtdOperatingCharacteristics(object)
    ruleParameters <- c("targetValue", "lowerReferenceValue", "upperPercentile", "lowerPercentile")
    rule <- object
    if (operatingCharacteristics) {
        rule <- object$.design
    }
    if (inherits(rule, "TrialDesignPlanQuantitativeBayes")) {
        ruleParameters <- c("go", "stop", "indeterminate1", "percentileMapping")
    }

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
                .getQtdEndpointSettings(object)$assumptions, digitSettings, consoleOutputEnabled
            )
        } else {
            if (!inherits(rule, "TrialDesignPlanQuantitativeBayes")) {
                description <- "Compares two confidence bounds with prespecified effect thresholds."
            } else if (rule$isNotApplicableParameter("percentileMapping")) {
                description <- "Applies explicitly specified posterior probability criteria."
            } else if (identical(rule$percentileMapping, "quantile")) {
                description <- "The quantile mapping converts percentile inputs to posterior probability cutoffs."
            } else {
                description <- "The posteriorProbability mapping uses the inputs as posterior probability cutoffs."
            }
            summaryFactory$.cat(description, "\n\n", consoleOutputEnabled = consoleOutputEnabled)
        }
    }
    if (!(summaryFactory$output %in% c("all", "body"))) {
        return(invisible())
    }
    if (!operatingCharacteristics) {
        .showQtdRuleTables(summaryFactory, rule, digitSettings, consoleOutputEnabled)
        return(invisible())
    }

    for (scenario in seq_len(nrow(object$numberOfSubjects))) {
        sampleSizes <- .formatSampleSizes(unname(object$numberOfSubjects[scenario, ]))
        summaryFactory$.cat("control: ", sampleSizes[1L],
            ", treatment: ", sampleSizes[2L], "\n\n",
            heading = 2, consoleOutputEnabled = consoleOutputEnabled
        )
        table <- .getQuantitativeSummaryTable(object, scenario, digitSettings)
        .showDataFrame(table, summaryFactory,
            niceColumnNamesEnabled = TRUE, consoleOutputEnabled = consoleOutputEnabled
        )
    }
}
