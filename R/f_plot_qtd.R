## |
## |  *Public plot methods for quantitative trial design objects*
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

#'
#' @title
#' Plot a Quantitative Trial Decision Rule
#'
#' @description
#' Displays illustrative or observed intervals, decision regions, or a binary
#' event-count decision map for a quantitative decision rule.
#'
#' @param x A quantitative rule from [getDesignQuantitative()].
#' @param ... Additional arguments are checked for unknown names.
#' @param type Plot type: 1 for intervals, 2 for normal decision regions, or
#'   3 for a binary event-count decision map. A single matching caption from
#'   [getAvailablePlotTypes()] is also accepted.
#' @param estimates,standardErrors Numeric vectors supplied together for type 1.
#'   If omitted, illustrative intervals with varying standard errors are used.
#' @param labels Optional labels for the supplied estimates.
#' @param effectRange,standardErrorRange Increasing numeric pairs defining the
#'   displayed ranges for type 2. Defaults are derived from the reference values.
#' @param gridSize Number of grid values per axis for type 2 (between 2 and 1000).
#' @param numberOfSubjects For type 3, two positive integers: control and treatment.
#' @param intervalMethod For type 3, "qtd1" (default), "farringtonManning",
#'   "miettinenNurminen", or "newcombe". See Details for the method definitions.
#' @param directionUpper Whether a higher treatment event rate is beneficial.
#'   If FALSE, the effect is control rate minus treatment rate.
#' @param controlRate,treatmentRate Optional true rates, supplied together for
#'   type 3. Exact binomial cell probabilities are then included in the plot data.
#' @param observedControl Optional observed control event count, highlighted by
#'   a vertical reference line in type 3.
#' @param plotCat Number of decision categories, either 3 or 6.
#' @param main,xlab,ylab Optional plot title and axis labels.
#' @param palette Optional rpact color palette. NULL uses semantic decision colors.
#' @param legendPosition Legend position, including the rpact default "auto".
#' @param plotSettings Optional object from [getPlotSettings()].
#' @param showSource Whether to print the unrounded data used by the plot.
#'
#' @details
#' Intervals use `estimate + qnorm(percentile) * standardError`; the displayed
#' percentiles do not imply a symmetric two-sided confidence interval. Type 2
#' plots observed estimates against standard errors, not true effects.
#'
#' With `intervalMethod = "qtd1"`, type 3 reproduces the QTD1 construction with limits
#' rounded to four decimal places. Score limits supply the variance terms of
#' the lower and upper risk-difference limits. Boundary equality follows QTD1:
#' lower limits equal to LRV or TV satisfy the corresponding lower criterion;
#' upper limits equal to TV satisfy the target criterion. The event map contains
#' at most one million cells. With supplied rates, category probabilities can be
#' obtained by summing `cellProbability` by `category` in the returned plot data.
#' The QTD1 approximation requires `lowerPercentile <= 0.5 <= upperPercentile`.
#'
#' `"farringtonManning"` inverts the risk-difference score test using constrained
#' maximum-likelihood variance. `"miettinenNurminen"` uses the same score inversion
#' with variance multiplied by `N / (N - 1)`, where N is the combined sample size.
#' `"newcombe"` combines unrounded Wilson limits by the square-and-add method,
#' without continuity correction. Each requested percentile is evaluated as a
#' one-sided bound: below 0.5 the lower-tail construction is used, above 0.5 the
#' upper-tail construction is used, and at 0.5 the observed difference is returned.
#' All three additional methods support ordered percentiles anywhere in (0, 1).
#' They provide frequentist confidence bounds, not posterior quantiles. Counts
#' are assumed to arise from two independent binomial groups.
#'
#' @references
#' Farrington and Manning (1990), Statistics in Medicine 9, 1447-1454,
#' doi:10.1002/sim.4780091208.
#' Miettinen and Nurminen (1985), Statistics in Medicine 4, 213-226,
#' doi:10.1002/sim.4780040211.
#' Newcombe (1998), Statistics in Medicine 17, 873-890,
#' doi:10.1002/(SICI)1097-0258(19980430)17:8<873::AID-SIM779>3.0.CO;2-I.
#'
#' @return A ggplot object. Its `data` component contains the plotting data.
#'
#' @examples
#' rule <- getDesignQuantitative(targetValue = 1, lowerReferenceValue = 0)
#' plot(rule)
#' plot(rule, type = 2)
#' binaryRule <- getDesignQuantitative(targetValue = 0, lowerReferenceValue = -0.05)
#' plot(binaryRule, type = 3, numberOfSubjects = c(75, 75), directionUpper = FALSE)
#' plot(binaryRule, type = 3, numberOfSubjects = c(25, 25),
#'     intervalMethod = "miettinenNurminen", directionUpper = FALSE)
#'
#' @export
#'
plot.TrialDesignPlanQuantitative <- function(x, ..., type = 1L, estimates = NULL,
        standardErrors = NULL, labels = NULL, effectRange = NULL, standardErrorRange = NULL,
        gridSize = 200L, numberOfSubjects = NULL, intervalMethod = "qtd1", directionUpper = TRUE,
        controlRate = NULL, treatmentRate = NULL, observedControl = NULL, plotCat = 6L,
        main = NA_character_, xlab = NA_character_, ylab = NA_character_, palette = NULL,
        legendPosition = "auto", plotSettings = NULL, showSource = FALSE) {
    functionName <- "plot.TrialDesignPlanQuantitative"
    .warnInCaseOfUnknownArguments(functionName = functionName, ...)
    .assertGgplotIsInstalled()
    .assertQtdPlotRule(x, functionName)
    .assertIsSingleCharacter(main, "main", naAllowed = TRUE)
    if (is.character(type)) {
        type <- .getPlotTypeNumber(type, x)
    }
    .assertQtdPlotChoice(type, "type", 1:3, functionName)
    .assertQtdPlotChoice(plotCat, "plotCat", c(3L, 6L), functionName)
    if (type %in% c(1L, 2L) && xor(is.null(estimates), is.null(standardErrors))) {
        parameterNames <- c("estimates", "standardErrors")
        supplied <- c(!is.null(estimates), !is.null(standardErrors))
        stopIncompleteArguments("Supply estimates and standardErrors together",
            functionName = functionName, parameter = parameterNames,
            value = list(estimates = estimates, standardErrors = standardErrors),
            missingParameters = parameterNames[!supplied], definedParameters = parameterNames[supplied],
            constraint = "estimates and standardErrors supplied together",
            diagnosticId = "qtd.plot.incomplete_intervals"
        )
    }
    size <- 0.8
    pointSize <- 3
    if (inherits(plotSettings, "PlotSettings")) {
        size <- plotSettings$scaleSize(plotSettings$lineSize)
        pointSize <- plotSettings$scaleSize(plotSettings$pointSize)
    }
    subtitle <- .getQtdPlotSubtitle(x)
    if (type == 1L) {
        illustrative <- is.null(estimates) && is.null(standardErrors)
        if (illustrative) {
            data <- .getQtdIntervalExamples(x)
            subtitle <- paste0(subtitle, "\nIllustrative intervals with different standard errors")
        } else {
            .assertQtdPlotNumeric(estimates, "estimates", functionName)
            .assertQtdPlotNumeric(standardErrors, "standardErrors", functionName,
                lower = 0, openLower = TRUE, expectedLength = length(estimates)
            )
            data <- data.frame(estimate = estimates, standardError = standardErrors)
        }
        data$lower <- data$estimate + stats::qnorm(x$lowerPercentile) * data$standardError
        data$upper <- data$estimate + stats::qnorm(x$upperPercentile) * data$standardError
        data$category <- .classifyQtdIntervals(data$lower, data$upper, x, plotCat)
        if (is.null(labels)) {
            labels <- as.character(data$category)
        }
        if (length(labels) != nrow(data)) {
            stopArgumentLengthOutOfBounds("Supply one label per interval",
                functionName = functionName, parameter = "labels", value = labels,
                constraint = "one label per interval", relatedParameter = "estimates", relatedValue = data$estimate,
                expectedLength = nrow(data), actualLength = length(labels),
                diagnosticId = "qtd.plot.interval_label_length"
            )
        }
        if (!is.character(labels) || anyNA(labels)) {
            stopIllegalArgument("'labels' must contain one character label per interval",
                functionName = functionName, parameter = "labels", value = labels,
                constraint = "one nonmissing character label per interval",
                relatedParameter = "estimates", relatedValue = data$estimate,
                diagnosticId = "qtd.plot.invalid_interval_labels"
            )
        }
        data$row <- rev(seq_len(nrow(data)))
        p <- ggplot2::ggplot(data, ggplot2::aes(y = .data[["row"]], color = .data[["category"]])) +
            ggplot2::geom_segment(ggplot2::aes(x = .data[["lower"]], xend = .data[["upper"]],
                yend = .data[["row"]]), linewidth = size * 2) +
            ggplot2::geom_point(ggplot2::aes(x = .data[["estimate"]]), size = pointSize) +
            ggplot2::scale_y_continuous(breaks = data$row, labels = labels) +
            ggplot2::labs(x = "Estimated treatment effect", y = NULL, color = "Decision")
        p <- .addQtdReferenceLines(p, x)
        defaultTitle <- "Quantitative trial decision rule"
    } else if (type == 2L) {
        scale <- max(x$targetValue - x$lowerReferenceValue, 1)
        if (is.null(effectRange)) {
            effectRange <- c(x$lowerReferenceValue - scale, x$targetValue + scale)
        }
        if (is.null(standardErrorRange)) {
            standardErrorRange <- c(0, scale)
        }
        .assertQtdPlotRange(effectRange, "effectRange", functionName)
        .assertQtdPlotRange(standardErrorRange, "standardErrorRange", functionName, lower = 0)
        .assertQtdPlotNumeric(gridSize, "gridSize", functionName,
            lower = 2, upper = 1000, expectedLength = 1L, integer = TRUE
        )
        data <- expand.grid(estimate = seq(effectRange[1], effectRange[2], length.out = gridSize),
            standardError = seq(standardErrorRange[1], standardErrorRange[2], length.out = gridSize))
        data$lower <- data$estimate + stats::qnorm(x$lowerPercentile) * data$standardError
        data$upper <- data$estimate + stats::qnorm(x$upperPercentile) * data$standardError
        data$category <- .classifyQtdIntervals(data$lower, data$upper, x, plotCat)
        p <- ggplot2::ggplot(data, ggplot2::aes(.data[["estimate"]], .data[["standardError"]], fill = .data[["category"]])) +
            ggplot2::geom_raster() + ggplot2::coord_cartesian(xlim = effectRange, ylim = standardErrorRange) +
            ggplot2::labs(x = "Estimated treatment effect", y = "Standard error", fill = "Decision")
        if (!is.null(estimates) || !is.null(standardErrors)) {
            .assertQtdPlotNumeric(estimates, "estimates", functionName)
            .assertQtdPlotNumeric(standardErrors, "standardErrors", functionName,
                lower = 0, expectedLength = length(estimates)
            )
            p <- p + ggplot2::geom_point(data = data.frame(estimate = estimates, standardError = standardErrors),
                ggplot2::aes(.data[["estimate"]], .data[["standardError"]]),
                inherit.aes = FALSE, shape = 21, fill = "white", size = pointSize)
        }
        defaultTitle <- "Quantitative decision regions"
    } else {
        data <- .getQtdBinaryDecisionData(x, numberOfSubjects, directionUpper, controlRate,
            treatmentRate, plotCat, intervalMethod)
        p <- ggplot2::ggplot(data, ggplot2::aes(.data[["eventsControl"]], .data[["eventsTreatment"]], fill = .data[["category"]])) +
            ggplot2::geom_tile() + ggplot2::coord_equal() +
            ggplot2::labs(x = "Observed control events", y = "Observed treatment events", fill = "Decision")
        if (!is.null(observedControl)) {
            .assertQtdPlotNumeric(observedControl, "observedControl", functionName,
                lower = 0, upper = numberOfSubjects[1L], integer = TRUE, expectedLength = 1L
            )
            p <- p + ggplot2::geom_vline(xintercept = observedControl, linetype = 2)
        }
        subtitle <- paste0(subtitle, "\nN control / treatment = ", paste(numberOfSubjects, collapse = " / "),
            "; ", intervalMethod, "; beneficial direction: ", ifelse(directionUpper, "higher treatment rate", "lower treatment rate"))
        defaultTitle <- "Binary event-count decision map"
    }
    if (is.na(main)) {
        main <- defaultTitle
    }
    return(.finishQtdPlot(p, x, plotSettings, main, subtitle, xlab, ylab,
        palette, legendPosition, showSource, outcomeColors = TRUE))
}

#'
#' @title
#' Plot Quantitative Operating Characteristics
#'
#' @description
#' Plots decision probabilities, sample-size comparisons, sensitivity surfaces
#' and explicitly weighted decision-performance metrics for quantitative trials.
#'
#' @param x Operating characteristics from [getOperatingCharacteristicsMeans()]
#'   or [getOperatingCharacteristicsRates()].
#' @param plotCat Number of decision categories, either 3 (default) or 6.
#' @param main Optional title. The original positional `plotCat` and `main`
#'   arguments are retained for compatibility.
#' @param ... Additional arguments are checked for unknown names.
#' @param type Plot type: 1 OC curves, 2 stacked probabilities, 3 probabilities
#'   versus total sample size, 4 selected effect scenarios, 5 sensitivity heatmap,
#'   or 6 weighted desired-versus-observed decisions. A single matching caption
#'   from [getAvailablePlotTypes()] is also accepted.
#' @param alternatives Optional subset of the true effects already computed in x.
#'   For rates, these select the corresponding treatment rates at the object's
#'   original control rate, also when a different `pi2` grid is requested.
#' @param stDev Optional positive standard deviations for recalculating the OC
#'   grid with the existing normal calculation. NULL uses stored probabilities.
#' @param pi1 For rates, an optional ordered subset of the treatment rates already
#'   computed in x. Supply either `pi1` or `alternatives`, not both.
#' @param pi2 For rates, optional control event probabilities in `[0, 1]` for
#'   recalculation with the stored interval method. The treatment-rate grid stays
#'   fixed. NULL uses stored probabilities.
#' @param xAxis NULL selects the endpoint default: `"effect"` for means and
#'   `"treatmentRate"` for rates. Rates also support `"effect"` for the beneficial
#'   risk difference. This controls types 1--4 and sample-size sensitivity maps.
#' @param sensitivity For type 5: `"sampleSize"`, `"allocation"`, or the
#'   endpoint-specific assumption (`"stDev"` for means, `"pi2"` for rates).
#'   A `"pi2"` map always uses treatment rate and control rate as its two axes.
#' @param category For type 5, the displayed category; default "Go".
#' @param contourLevels Optional probability levels for sensitivity-map contours.
#' @param plotPointsEnabled Whether OC curves include points. NA enables points
#'   for grids with at most ten distinct effects. Types 3 and 4 always show points.
#' @param weights,desiredDecisions For type 6, one nonnegative weight and one
#'   desired decision per selected effect. Weights must sum to one. Desired
#'   decisions are "Go", "Indeterminate", or "Stop". All three observed decisions
#'   remain visible; no implicit treatment of Indeterminate as Stop is applied.
#'   For rates, provide one entry per distinct selected treatment rate, in `pi1`
#'   selection order (or original input order). The same distribution is applied
#'   separately to each control-rate panel.
#' @param xlab,ylab Optional axis labels.
#' @param palette Optional rpact color palette; NULL uses semantic colors when
#'   decisions are the color/fill categories, and the rpact hue palette for curves.
#' @param legendPosition Legend position, including "auto".
#' @param plotSettings Optional object from [getPlotSettings()].
#' @param showSource Whether to print unrounded plotting data.
#'
#' @details
#' Types 1 and 2 use true effects, unlike plots of observed decision regions.
#' Stacked areas interpolate linearly between supplied grid values. Type 3
#' separates allocation ratios so different allocations are not joined into one
#' sample-size curve. Type 4 shows actual computed grid points without interpolation.
#' Type 5 requires at least two distinct coordinates on each displayed axis.
#' Allocation maps require a single selected effect and a rectangular grid of
#' control/treatment sample sizes. Sensitivity maps use evaluated grid points;
#' they do not extrapolate to uncomputed combinations.
#'
#' Type 6 uses an explicitly supplied discrete effect distribution and definition
#' of the desired decision. Its cells are joint probabilities after summing
#' conditional decision probabilities weighted by the supplied effect weights.
#' Matching desired and observed decisions count as correct under that definition. These
#' quantities are neither default type-I/type-II error rates nor posterior
#' probabilities. The data retain the desired and observed decisions and a
#' logical `correct` column. Probabilities are never rounded before aggregation.
#' The plot attribute `qtdEffectDistribution` records the supplied effect,
#' weight and desired-decision tuples. Each panel reports its probability of a
#' correct decision under this definition.
#'
#' Rates plots facet by control rate and include the interval method and
#' beneficial direction in their subtitle. On treatment-rate axes, the reference
#' lines are transformed to `pi2 + reference` for increasing beneficial rates,
#' or `pi2 - reference` for decreasing beneficial rates. Unattainable reference
#' rates outside `[0, 1]` are omitted. All plotting probabilities come from the
#' stored results or the same endpoint calculation used by their constructor.
#' For rates, `qtdEffectDistribution` identifies treatment rates in a `pi1`
#' column; for means it retains the `effect` column.
#'
#' @return A ggplot object with its data in the `data` component.
#'
#' @examples
#' rule <- getDesignQuantitative(targetValue = 1, lowerReferenceValue = 0)
#' oc <- getOperatingCharacteristicsMeans(rule,
#'     numberOfSubjects = matrix(c(50, 50, 50, 100, 100, 100), ncol = 2, byrow = TRUE),
#'     alternative = seq(0, 1.5, by = 0.1))
#' plot(oc)
#' plot(oc, type = 2)
#' plot(oc, type = 3, alternatives = c(0, 1, 1.5))
#' plot(oc, type = 4, alternatives = c(0, 1, 1.5))
#' plot(oc, type = 5, sensitivity = "stDev", stDev = c(0.8, 1, 1.2))
#' plot(oc, type = 6, alternatives = c(0, 1, 1.5),
#'     weights = c(0.3, 0.3, 0.4), desiredDecisions = c("Stop", "Go", "Go"))
#' binaryRule <- getDesignQuantitative(targetValue = 0, lowerReferenceValue = -0.05)
#' rates <- getOperatingCharacteristicsRates(binaryRule,
#'     numberOfSubjects = matrix(c(25, 25, 50, 50, 75, 75), ncol = 2, byrow = TRUE),
#'     pi1 = seq(0.1, 0.25, by = 0.025), pi2 = 0.15, directionUpper = FALSE)
#' plot(rates)
#' plot(rates, type = 2)
#' plot(rates, xAxis = "effect")
#' plot(rates, type = 5, sensitivity = "pi2", pi2 = c(0.1, 0.15, 0.2))
#' plot(rates, type = 6, pi1 = c(0.1, 0.15, 0.2),
#'     weights = c(0.3, 0.4, 0.3), desiredDecisions = c("Go", "Go", "Stop"))
#'
#' @export
#'
plot.OperatingCharcteristics <- function(x, plotCat = c(3, 6), main = NA_character_, ...,
        type = 1L, alternatives = NULL, stDev = NULL, pi1 = NULL, pi2 = NULL, xAxis = NULL,
        sensitivity = "sampleSize", category = "Go",
        weights = NULL, desiredDecisions = NULL, contourLevels = NULL, plotPointsEnabled = NA,
        xlab = NA_character_, ylab = NA_character_,
        palette = NULL, legendPosition = "auto", plotSettings = NULL, showSource = FALSE) {
    functionName <- "plot.OperatingCharcteristics"
    .warnInCaseOfUnknownArguments(functionName = functionName, ...)
    .assertGgplotIsInstalled()
    .assertIsSingleCharacter(main, "main", naAllowed = TRUE)
    if (is.character(type)) {
        type <- .getPlotTypeNumber(type, x)
    }
    if (identical(as.character(plotCat), c("3", "6"))) {
        plotCat <- 3L
    }
    .assertQtdPlotChoice(plotCat, "plotCat", c(3L, 6L), functionName)
    .assertQtdPlotChoice(type, "type", 1:6, functionName)
    settings <- .getQtdEndpointSettings(x, xAxis, functionName)
    data <- .getQtdPlotData(x, plotCat, stDev, pi2)
    data <- .selectQtdOcPlotData(data, x, alternatives, pi1, settings, functionName)
    .assertIsSingleLogical(plotPointsEnabled, "plotPointsEnabled", naAllowed = TRUE)
    if (is.na(plotPointsEnabled)) {
        plotPointsEnabled <- length(unique(data[[settings$effectColumn]])) <= 10L
    }
    rule <- x$.design
    size <- 0.8
    if (inherits(plotSettings, "PlotSettings")) {
        size <- plotSettings$scaleSize(plotSettings$lineSize)
    }
    outcomeColors <- FALSE
    subtitle <- .getQtdOcPlotSubtitle(x, data, settings)
    references <- .getQtdOcPlotReferences(x, data, settings)
    categoryFacets <- stats::reformulate(c(settings$assumptionColumn, "category"))
    scenarioFacets <- stats::reformulate(c(settings$assumptionColumn, "scenarioLabel"))
    pointSize <- 3
    if (inherits(plotSettings, "PlotSettings")) {
        pointSize <- plotSettings$scaleSize(plotSettings$pointSize)
    }
    if (type == 1L) {
        p <- ggplot2::ggplot(data, ggplot2::aes(.data[[settings$xColumn]], .data[["probability"]],
            color = .data[["scenarioLabel"]], group = interaction(.data[["scenario"]], .data[[settings$assumptionColumn]]))) +
            ggplot2::geom_line(linewidth = size) +
            ggplot2::facet_wrap(categoryFacets, ncol = 3, labeller = .getQtdFacetLabeller()) +
            ggplot2::labs(x = settings$xCaption, y = "Decision probability", color = "Sample sizes")
        p <- .addQtdReferenceLines(p, rule, references = references)
        if (plotPointsEnabled) {
            p <- p + ggplot2::geom_point(size = pointSize)
        }
        defaultTitle <- paste("Operating characteristics for comparing", settings$endpoint)
    } else if (type == 2L) {
        data <- .uniqueQtdEffectRows(data, effectColumnName = settings$effectColumn,
            assumptionColumn = settings$assumptionColumn)
        p <- ggplot2::ggplot(data, ggplot2::aes(.data[[settings$xColumn]], .data[["probability"]], fill = .data[["category"]])) +
            ggplot2::geom_area(position = ggplot2::position_stack(reverse = TRUE)) +
            ggplot2::facet_wrap(scenarioFacets, ncol = 3, labeller = .getQtdFacetLabeller()) +
            ggplot2::labs(x = settings$xCaption, y = "Decision probability", fill = "Decision")
        p <- .addQtdReferenceLines(p, rule, references = references)
        defaultTitle <- "Composition of decision probabilities"
        outcomeColors <- TRUE
    } else if (type == 3L) {
        .assertQtdSampleSizeVariation(data, functionName)
        data <- .uniqueQtdEffectRows(data, includeSampleSize = TRUE,
            effectColumnName = settings$effectColumn, assumptionColumn = settings$assumptionColumn)
        data$effectLabel <- factor(data[[settings$xColumn]], levels = unique(data[[settings$xColumn]]))
        data$allocationLabel <- paste0("T/C = ", signif(data$allocationRatio, 5))
        p <- ggplot2::ggplot(data, ggplot2::aes(.data[["totalSubjects"]], .data[["probability"]], color = .data[["effectLabel"]],
            group = interaction(.data[[settings$effectColumn]], .data[["allocationRatio"]]))) +
            ggplot2::geom_line(linewidth = size) + ggplot2::geom_point(size = pointSize) +
            ggplot2::facet_grid(stats::as.formula(paste(settings$assumptionColumn, "+ allocationLabel ~ category")),
                labeller = .getQtdFacetLabeller()) +
            ggplot2::labs(x = "Total number of subjects", y = "Decision probability",
                color = ifelse(settings$xAxis == "treatmentRate", "Treatment rate", "True effect"))
        defaultTitle <- "Decision probabilities by sample size"
    } else if (type == 4L) {
        data$effectLabel <- factor(data[[settings$xColumn]], levels = unique(data[[settings$xColumn]]))
        p <- ggplot2::ggplot(data, ggplot2::aes(.data[["effectLabel"]], .data[["probability"]], color = .data[["scenarioLabel"]])) +
            ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.5), size = pointSize) +
            ggplot2::facet_wrap(categoryFacets, ncol = 3, labeller = .getQtdFacetLabeller()) +
            ggplot2::labs(x = ifelse(settings$xAxis == "treatmentRate", "Treatment event-rate scenario", "True effect scenario"),
                y = "Decision probability", color = "Sample sizes")
        defaultTitle <- "Selected effect scenarios"
    } else if (type == 5L) {
        p <- .plotQtdSensitivity(data, sensitivity, category, functionName, contourLevels, settings)
        defaultTitle <- "Sensitivity of decision probabilities"
    } else {
        data <- .getQtdDecisionPerformance(data, weights, desiredDecisions, functionName,
            effectColumnName = settings$effectColumn, assumptionColumn = settings$assumptionColumn)
        p <- ggplot2::ggplot(data, ggplot2::aes(.data[["observedDecision"]], .data[["desiredDecision"]], fill = .data[["probability"]])) +
            ggplot2::geom_tile(color = "white") +
            ggplot2::geom_text(ggplot2::aes(label = paste0(formatC(100 * .data[["probability"]], digits = 1, format = "f"), "%"))) +
            ggplot2::facet_wrap(stats::reformulate(c(settings$assumptionColumn, "performanceLabel")),
                ncol = 3, labeller = .getQtdFacetLabeller()) +
            ggplot2::labs(x = "Observed decision", y = "Desired decision", fill = "Joint probability")
        defaultTitle <- "Desired and observed decisions"
    }
    if (type %in% 1:4) {
        p <- p + ggplot2::scale_y_continuous(labels = function(value) {
            return(paste0(100 * value, "%"))
        }) + ggplot2::coord_cartesian(ylim = c(0, 1))
    }
    if (is.na(main)) {
        main <- defaultTitle
    }
    p <- .finishQtdPlot(p, rule, plotSettings, main, subtitle, xlab, ylab,
        palette, legendPosition, showSource, outcomeColors, continuousFill = type %in% c(5L, 6L))
    if (type == 6L) {
        attr(p, "qtdEffectDistribution") <- attr(data, "effectDistribution")
    }
    return(p)
}
