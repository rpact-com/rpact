## Shared preparation and validation for quantitative trial design plots.

.getQtdPlotCaptions <- function(object) {
    if (inherits(object, "TrialDesignPlanQuantitative")) {
        return(c("Decision rule intervals", "Normal decision regions", "Binary event-count decision map"))
    }
    if (inherits(object, "OperatingCharcteristicsMeans")) {
        return(c("Operating characteristics", "Stacked decision probabilities", "Decision probabilities by sample size",
            "Selected effect scenarios", "Sensitivity of decision probabilities", "Desired and observed decisions"))
    }
    return(character())
}

.getQtdCategories <- function(plotCat = 6L) {
    if (plotCat == 3L) {
        return(c("Go", "Indeterminate", "Stop"))
    }
    return(c("Go (Fixed)", "Go", "Indeterminate 1", "Indeterminate 2", "Stop", "Stop (Fixed)"))
}

.getQtdCoarseCategory <- function(category) {
    result <- as.character(category)
    result[result %in% c("Go (Fixed)", "Go")] <- "Go"
    result[result %in% c("Indeterminate 1", "Indeterminate 2")] <- "Indeterminate"
    result[result %in% c("Stop", "Stop (Fixed)")] <- "Stop"
    return(result)
}

.assertQtdPlotChoice <- function(value, parameter, allowedValues, functionName) {
    if (length(value) != 1L || is.na(value) || !(value %in% allowedValues)) {
        stopIllegalArgument("'", parameter, "' must be one of: ", paste(allowedValues, collapse = ", "),
            functionName = functionName, parameter = parameter, value = value,
            constraint = "one of the allowed choices", diagnosticId = "qtd.plot.invalid_choice",
            context = list(allowedValues = allowedValues)
        )
    }
}

.assertQtdPlotNumeric <- function(value, parameter, functionName, lower = -Inf, upper = Inf,
        expectedLength = NULL, integer = FALSE, openLower = FALSE, openUpper = FALSE) {
    if (is.null(value)) {
        stopMissingArgument("'", parameter, "' must be supplied for this plot",
            functionName = functionName, parameter = parameter,
            constraint = "required numeric input", diagnosticId = "qtd.plot.missing_numeric_input"
        )
    }
    if (!is.numeric(value) || length(value) == 0L || anyNA(value) || any(!is.finite(value)) ||
            (integer && any(value != floor(value)))) {
        stopIllegalArgument("'", parameter, "' must contain finite ",
            ifelse(integer, "integers", "numeric values"),
            functionName = functionName, parameter = parameter, value = value,
            constraint = "nonempty finite numeric vector of the required type",
            diagnosticId = "qtd.plot.invalid_numeric_input", context = list(integerRequired = integer)
        )
    }
    if (!is.null(expectedLength) && length(value) != expectedLength) {
        stopArgumentLengthOutOfBounds("'", parameter, "' has an invalid length",
            functionName = functionName, parameter = parameter, value = value,
            constraint = "exactly the expected number of entries", expectedLength = expectedLength,
            actualLength = length(value), diagnosticId = "qtd.plot.invalid_length"
        )
    }
    outside <- value < lower | value > upper
    if (openLower) {
        outside <- outside | value == lower
    }
    if (openUpper) {
        outside <- outside | value == upper
    }
    if (any(outside)) {
        stopArgumentOutOfRange("'", parameter, "' is outside its allowed range",
            functionName = functionName, parameter = parameter, value = value,
            lowerBound = lower, upperBound = upper,
            constraint = paste0(ifelse(openLower, "(", "["), lower, ", ", upper,
                ifelse(openUpper, ")", "]")),
            diagnosticId = "qtd.plot.numeric_outside_range"
        )
    }
}

.assertQtdPlotRule <- function(rule, functionName) {
    if (!inherits(rule, "TrialDesignPlanQuantitative")) {
        stopIllegalArgument("'x' must contain a quantitative decision rule",
            functionName = functionName, parameter = "x", value = rule,
            constraint = "TrialDesignPlanQuantitative", diagnosticId = "qtd.plot.invalid_rule"
        )
    }
    for (parameter in c("targetValue", "lowerReferenceValue")) {
        .assertQtdPlotNumeric(rule[[parameter]], parameter, functionName, expectedLength = 1L)
    }
    for (parameter in c("lowerPercentile", "upperPercentile")) {
        .assertQtdPlotNumeric(rule[[parameter]], parameter, functionName,
            lower = 0, upper = 1, expectedLength = 1L, openLower = TRUE, openUpper = TRUE
        )
    }
    if (rule$targetValue < rule$lowerReferenceValue || rule$lowerPercentile >= rule$upperPercentile) {
        stopConflictingArguments("Reference values and percentiles must be ordered",
            functionName = functionName,
            parameter = c("targetValue", "lowerPercentile"),
            value = c(rule$targetValue, rule$lowerPercentile),
            relatedParameter = c("lowerReferenceValue", "upperPercentile"),
            relatedValue = c(rule$lowerReferenceValue, rule$upperPercentile),
            constraint = "targetValue >= lowerReferenceValue and lowerPercentile < upperPercentile",
            diagnosticId = "qtd.plot.unordered_rule"
        )
    }
}

.classifyQtdIntervals <- function(lower, upper, rule, plotCat = 6L) {
    category <- rep("Stop (Fixed)", length(lower))
    category[upper >= rule$lowerReferenceValue] <- "Stop"
    category[lower >= rule$lowerReferenceValue & upper < rule$targetValue] <- "Indeterminate 2"
    category[lower < rule$lowerReferenceValue & upper >= rule$targetValue] <- "Indeterminate 1"
    category[lower >= rule$lowerReferenceValue & upper >= rule$targetValue] <- "Go"
    category[lower >= rule$targetValue] <- "Go (Fixed)"
    if (plotCat == 3L) {
        category <- .getQtdCoarseCategory(category)
    }
    return(factor(category, levels = .getQtdCategories(plotCat)))
}

.getQtdIntervalExamples <- function(rule) {
    scale <- max(rule$targetValue - rule$lowerReferenceValue, .Machine$double.eps^0.25)
    lower <- rule$lowerReferenceValue + scale * c(1.2, 0.3, -0.3, 0.2, -0.5, -0.8)
    upper <- rule$lowerReferenceValue + scale * c(1.8, 1.3, 1.4, 0.8, 0.3, -0.2)
    standardError <- (upper - lower) / (stats::qnorm(rule$upperPercentile) - stats::qnorm(rule$lowerPercentile))
    estimate <- lower - stats::qnorm(rule$lowerPercentile) * standardError
    return(data.frame(estimate = estimate, standardError = standardError))
}

.getQtdPlotData <- function(object, plotCat, stDev = NULL) {
    functionName <- "plot.OperatingCharcteristics"
    if (!inherits(object, "OperatingCharcteristicsMeans")) {
        stopIllegalArgument("This plot requires operating characteristics for means",
            functionName = functionName, parameter = "x", value = object,
            constraint = "OperatingCharcteristicsMeans", diagnosticId = "qtd.plot.unsupported_endpoint"
        )
    }
    .assertQtdPlotRule(object$.design, functionName)
    .assertQtdPlotNumeric(object$stDev, "stDev", functionName, lower = 0,
        openLower = TRUE, expectedLength = 1L)
    deviations <- object$stDev
    if (!is.null(stDev)) {
        .assertQtdPlotNumeric(stDev, "stDev", functionName, lower = 0, openLower = TRUE)
        deviations <- unique(stDev)
    }
    result <- list()
    for (i in seq_along(deviations)) {
        if (is.null(stDev)) {
            data <- object$probabilities
        } else {
            data <- .calcOperatingCharacteristicsMeans(object$.design, unname(object$numberOfSubjects),
                object$alternative, deviations[i]
            )
        }
        rowsPerScenario <- length(object$alternative) * 6L
        data$scenario <- rep(seq_len(nrow(object$numberOfSubjects)), each = rowsPerScenario)
        data$effectIndex <- rep(rep(seq_along(object$alternative), times = 6L), times = nrow(object$numberOfSubjects))
        data$category <- as.character(data$category)
        if (plotCat == 3L) {
            data$category <- .getQtdCoarseCategory(data$category)
            data <- stats::aggregate(probability ~ category + scenario + effectIndex +
                nControl + nTreatment + alternative + stDev, data = data, FUN = sum)
        }
        data$category <- factor(data$category, levels = .getQtdCategories(plotCat))
        data$sampleSize <- paste0("C: ", data$nControl, " / T: ", data$nTreatment)
        data$scenarioLabel <- paste0(data$scenario, ": ", data$sampleSize)
        data$totalSubjects <- data$nControl + data$nTreatment
        data$allocationRatio <- data$nTreatment / data$nControl
        result[[i]] <- data
    }
    return(do.call(rbind, result))
}

.getQtdScoreLimits <- function(events, n, percentile) {
    z <- abs(stats::qnorm(percentile))
    p <- events / n
    middle <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
    halfWidth <- z * sqrt((p * (1 - p) + z^2 / (4 * n)) / n) / (1 + z^2 / n)
    return(data.frame(lower = round(middle - halfWidth, 4), upper = round(middle + halfWidth, 4)))
}

.getQtdBinaryDecisionData <- function(rule, numberOfSubjects, directionUpper, controlRate = NULL,
        treatmentRate = NULL, plotCat = 6L, intervalMethod = "qtd1") {
    functionName <- "plot.TrialDesignPlanQuantitative"
    .assertQtdPlotChoice(intervalMethod, "intervalMethod",
        c("qtd1", "farringtonManning", "miettinenNurminen", "newcombe"), functionName)
    if (intervalMethod == "qtd1" && (rule$lowerPercentile > 0.5 || rule$upperPercentile < 0.5)) {
        stopIllegalArgument("The QTD1 approximation requires percentiles on either side of 0.5",
            functionName = functionName, parameter = c("lowerPercentile", "upperPercentile"),
            value = c(rule$lowerPercentile, rule$upperPercentile),
            relatedParameter = "intervalMethod", relatedValue = intervalMethod,
            constraint = "lowerPercentile <= 0.5 <= upperPercentile",
            diagnosticId = "qtd.plot.qtd1_percentiles_require_median",
            context = list(alternativeMethods = c("farringtonManning", "miettinenNurminen", "newcombe"))
        )
    }
    .assertQtdPlotNumeric(numberOfSubjects, "numberOfSubjects", functionName,
        lower = 1, expectedLength = 2L, integer = TRUE
    )
    .assertIsSingleLogical(directionUpper, "directionUpper")
    if (prod(numberOfSubjects + 1) > 1e6) {
        stopIllegalArgument("The binary decision map would contain more than one million cells",
            functionName = functionName, parameter = "numberOfSubjects", value = numberOfSubjects,
            constraint = "(nControl + 1) * (nTreatment + 1) <= 1000000",
            diagnosticId = "qtd.plot.binary_grid_too_large"
        )
    }
    nControl <- numberOfSubjects[1L]
    nTreatment <- numberOfSubjects[2L]
    data <- expand.grid(eventsControl = 0:nControl, eventsTreatment = 0:nTreatment)
    data$estimate <- data$eventsTreatment / nTreatment - data$eventsControl / nControl
    if (!directionUpper) {
        data$estimate <- -data$estimate
    }
    if (intervalMethod != "qtd1") {
        if (directionUpper) {
            bounds <- .getQtdRiskDifferenceBounds(data$eventsTreatment, nTreatment, data$eventsControl, nControl,
                rule$lowerPercentile, rule$upperPercentile, intervalMethod)
        } else {
            bounds <- .getQtdRiskDifferenceBounds(data$eventsControl, nControl, data$eventsTreatment, nTreatment,
                rule$lowerPercentile, rule$upperPercentile, intervalMethod)
        }
        data$lower <- bounds$lower
        data$upper <- bounds$upper
    } else {
        lowControl <- .getQtdScoreLimits(data$eventsControl, nControl, rule$lowerPercentile)
        lowTreatment <- .getQtdScoreLimits(data$eventsTreatment, nTreatment, rule$lowerPercentile)
        highControl <- .getQtdScoreLimits(data$eventsControl, nControl, rule$upperPercentile)
        highTreatment <- .getQtdScoreLimits(data$eventsTreatment, nTreatment, rule$upperPercentile)
        if (directionUpper) {
            sdLower <- sqrt(lowTreatment$lower * (1 - lowTreatment$lower) / nTreatment +
                lowControl$upper * (1 - lowControl$upper) / nControl)
            sdUpper <- sqrt(highTreatment$upper * (1 - highTreatment$upper) / nTreatment +
                highControl$lower * (1 - highControl$lower) / nControl)
        } else {
            sdLower <- sqrt(lowControl$lower * (1 - lowControl$lower) / nControl +
                lowTreatment$upper * (1 - lowTreatment$upper) / nTreatment)
            sdUpper <- sqrt(highControl$upper * (1 - highControl$upper) / nControl +
                highTreatment$lower * (1 - highTreatment$lower) / nTreatment)
        }
        data$lower <- data$estimate + stats::qnorm(rule$lowerPercentile) * sdLower
        data$upper <- data$estimate + stats::qnorm(rule$upperPercentile) * sdUpper
    }
    data$intervalMethod <- intervalMethod
    data$category <- .classifyQtdIntervals(data$lower, data$upper, rule, plotCat)
    if (xor(is.null(controlRate), is.null(treatmentRate))) {
        stopIncompleteArguments("Supply both controlRate and treatmentRate to compute cell probabilities",
            functionName = functionName, parameter = c("controlRate", "treatmentRate"),
            value = list(controlRate = controlRate, treatmentRate = treatmentRate),
            missingParameters = c("controlRate", "treatmentRate")[c(is.null(controlRate), is.null(treatmentRate))],
            definedParameters = c("controlRate", "treatmentRate")[c(!is.null(controlRate), !is.null(treatmentRate))],
            constraint = "both rates supplied together", diagnosticId = "qtd.plot.incomplete_binary_rates"
        )
    }
    if (!is.null(controlRate)) {
        .assertQtdPlotNumeric(controlRate, "controlRate", functionName, lower = 0, upper = 1, expectedLength = 1L)
        .assertQtdPlotNumeric(treatmentRate, "treatmentRate", functionName, lower = 0, upper = 1, expectedLength = 1L)
        data$cellProbability <- stats::dbinom(data$eventsControl, nControl, controlRate) *
            stats::dbinom(data$eventsTreatment, nTreatment, treatmentRate)
    }
    return(data)
}

.finishQtdPlot <- function(p, rule, plotSettings, main, subtitle, xlab, ylab,
        palette, legendPosition, showSource, outcomeColors = FALSE, continuousFill = FALSE) {
    if (is.null(plotSettings)) {
        plotSettings <- getPlotSettings()
    }
    if (!inherits(plotSettings, "PlotSettings")) {
        stopIllegalArgument("'plotSettings' must be a PlotSettings object",
            functionName = ".finishQtdPlot", parameter = "plotSettings", value = plotSettings,
            constraint = "PlotSettings or NULL", diagnosticId = "qtd.plot.invalid_settings"
        )
    }
    .assertIsValidLegendPosition(legendPosition)
    .assertIsSingleLogical(showSource, "showSource")
    .assertIsSingleCharacter(xlab, "xlab", naAllowed = TRUE)
    .assertIsSingleCharacter(ylab, "ylab", naAllowed = TRUE)
    if (!is.null(palette) &&
            (!(is.character(palette) || is.numeric(palette)) || length(palette) != 1L)) {
        stopIllegalArgument("'palette' must be a single rpact palette selection or NULL",
            functionName = ".finishQtdPlot", parameter = "palette", value = palette,
            constraint = "one palette name or numeric selection, or NULL",
            diagnosticId = "qtd.plot.invalid_palette"
        )
    }
    effectiveLegendPosition <- legendPosition
    filledPanel <- any(vapply(p$layers, function(layer) {
        return(any(vapply(c("GeomRaster", "GeomTile", "GeomArea"), function(className) {
            return(inherits(layer$geom, className))
        }, logical(1))))
    }, logical(1)))
    if (identical(legendPosition, "auto") && filledPanel) {
        effectiveLegendPosition <- "right"
    }
    p <- plotSettings$setTheme(p)
    p <- plotSettings$setAxesAppearance(p)
    p <- plotSettings$setMainTitle(p, main, subtitle = subtitle)
    p <- plotSettings$setLegendLabelSize(p)
    p <- plotSettings$setLegendPosition(p, effectiveLegendPosition, finalize = FALSE)
    if (!is.na(xlab)) {
        p <- p + ggplot2::labs(x = xlab)
    }
    if (!is.na(ylab)) {
        p <- p + ggplot2::labs(y = ylab)
    }
    if (continuousFill) {
        if (is.character(palette) && length(palette) == 1L && !is.na(palette)) {
            p <- p + ggplot2::scale_fill_distiller(palette = palette, direction = 1, limits = c(0, 1))
        } else {
            highColor <- "#216A8C"
            if (is.numeric(palette) && length(palette) == 1L && !is.na(palette) && palette == 0) {
                highColor <- "black"
            }
            p <- p + ggplot2::scale_fill_gradient(low = "white", high = highColor, limits = c(0, 1))
        }
    } else if (outcomeColors && is.null(palette)) {
        colors <- c("Go (Fixed)" = "#007D63", "Go" = "#52AA8C",
            "Indeterminate 1" = "#C49814", "Indeterminate 2" = "#EACB66",
            "Indeterminate" = "#D8B23C", "Stop" = "#D87567", "Stop (Fixed)" = "#A13939")
        mappings <- unique(c(names(p$mapping), unlist(lapply(p$layers, function(layer) {
            return(names(layer$mapping))
        }))))
        if ("colour" %in% mappings) {
            p <- p + ggplot2::scale_colour_manual(values = colors, drop = FALSE)
        }
        if ("fill" %in% mappings) {
            p <- p + ggplot2::scale_fill_manual(values = colors, drop = FALSE)
        }
    } else if (!any(vapply(p$scales$scales, function(scale) {
            return("fill" %in% scale$aesthetics)
        }, logical(1)))) {
        if (is.null(palette)) {
            palette <- NA_character_
        }
        p <- plotSettings$setColorPalette(p, palette, mode = "all")
    }
    if (showSource) {
        cat("Source data of the QTD plot:\n")
        print(p$data)
    }
    p <- .finalizePlotLegend(p, effectiveLegendPosition, plotSettings)
    p <- p + ggplot2::guides(linetype = "none")
    fillScale <- p$scales$get_scales("fill")
    if (!is.null(fillScale) && !fillScale$is_discrete()) {
        p <- p + ggplot2::guides(fill = ggplot2::guide_colorbar())
    }
    return(p)
}

.getQtdFacetLabeller <- function() {
    return(ggplot2::labeller(stDev = function(value) {
        return(paste0("SD = ", value))
    }, category = ggplot2::label_value, scenarioLabel = ggplot2::label_value,
        allocationLabel = ggplot2::label_value))
}

.getQtdPlotSubtitle <- function(rule, stDev = NULL) {
    result <- paste0("LRV = ", rule$lowerReferenceValue, "; TV = ", rule$targetValue,
        "; percentiles = ", rule$lowerPercentile, " / ", rule$upperPercentile)
    if (!is.null(stDev)) {
        result <- paste0(result, "; SD = ", paste(stDev, collapse = ", "))
    }
    return(result)
}

.addQtdReferenceLines <- function(p, rule, horizontal = FALSE) {
    references <- data.frame(value = c(rule$lowerReferenceValue, rule$targetValue),
        reference = c("LRV", "TV"))
    if (horizontal) {
        return(p + ggplot2::geom_hline(data = references,
            ggplot2::aes(yintercept = .data[["value"]], linetype = .data[["reference"]]), color = "grey40"))
    }
    return(p + ggplot2::geom_vline(data = references,
        ggplot2::aes(xintercept = .data[["value"]], linetype = .data[["reference"]]), color = "grey40") +
        ggplot2::geom_text(data = references, ggplot2::aes(x = .data[["value"]], y = Inf, label = .data[["reference"]]),
            inherit.aes = FALSE, hjust = -0.1, vjust = 1.2, color = "grey30", size = 3) +
        ggplot2::scale_linetype_manual(values = c(LRV = "dashed", TV = "dotted"), guide = "none"))
}

.assertQtdPlotRange <- function(value, parameter, functionName, lower = -Inf) {
    .assertQtdPlotNumeric(value, parameter, functionName, lower = lower, expectedLength = 2L)
    if (value[1L] >= value[2L]) {
        stopIllegalArgument("'", parameter, "' must be strictly increasing",
            functionName = functionName, parameter = parameter, value = value,
            constraint = "first endpoint < second endpoint", diagnosticId = "qtd.plot.invalid_range"
        )
    }
}

.selectQtdAlternatives <- function(data, alternatives, functionName) {
    if (is.null(alternatives)) {
        return(data)
    }
    .assertQtdPlotNumeric(alternatives, "alternatives", functionName)
    choices <- unique(data$alternative)
    indices <- vapply(alternatives, function(value) {
        matches <- which(abs(choices - value) <= 1e-12 * max(1, abs(value)))
        if (length(matches) == 0L) {
            return(NA_integer_)
        }
        return(matches[1L])
    }, integer(1))
    if (anyNA(indices) || anyDuplicated(indices)) {
        stopIllegalArgument("'alternatives' must select distinct computed effects",
            functionName = functionName, parameter = "alternatives", value = alternatives,
            constraint = "distinct effects already evaluated in the object",
            diagnosticId = "qtd.plot.uncomputed_alternatives", context = list(availableEffects = choices)
        )
    }
    selected <- choices[indices]
    result <- data[data$alternative %in% selected, , drop = FALSE]
    result <- result[order(match(result$alternative, selected)), , drop = FALSE]
    return(result)
}

.uniqueQtdEffectRows <- function(data, includeSampleSize = FALSE) {
    keys <- c("category", "scenario", "alternative", "stDev")
    if (includeSampleSize) {
        keys <- c("category", "nControl", "nTreatment", "alternative", "stDev")
    }
    return(data[!duplicated(data[keys]), , drop = FALSE])
}

.assertQtdSampleSizeVariation <- function(data, functionName) {
    if (length(unique(data$totalSubjects)) < 2L) {
        stopIllegalArgument("This plot requires at least two distinct total sample sizes",
            functionName = functionName, parameter = "numberOfSubjects",
            value = unique(data[c("nControl", "nTreatment")]),
            constraint = "at least two distinct total sample sizes",
            diagnosticId = "qtd.plot.sample_size_variation_required"
        )
    }
}

.plotQtdSensitivity <- function(data, sensitivity, category, functionName, contourLevels = NULL) {
    .assertQtdPlotChoice(sensitivity, "sensitivity", c("sampleSize", "stDev", "allocation"), functionName)
    .assertQtdPlotChoice(category, "category", levels(data$category), functionName)
    data <- data[as.character(data$category) == category, , drop = FALSE]
    data <- .uniqueQtdEffectRows(data, includeSampleSize = TRUE)
    if (sensitivity == "sampleSize") {
        .assertQtdSampleSizeVariation(data, functionName)
        data$x <- data$alternative
        data$y <- data$totalSubjects
        data$panel <- paste0("SD = ", data$stDev, "; T/C = ", signif(data$allocationRatio, 5))
        xlab <- "True treatment effect"
        ylab <- "Total number of subjects"
    } else if (sensitivity == "stDev") {
        data$x <- data$alternative
        data$y <- data$stDev
        data$panel <- data$scenarioLabel
        xlab <- "True treatment effect"
        ylab <- "Standard deviation"
    } else {
        if (length(unique(data$alternative)) != 1L) {
            stopIllegalArgument("An allocation map requires exactly one selected effect",
                functionName = functionName, parameter = "alternatives", value = unique(data$alternative),
                constraint = "one selected effect", diagnosticId = "qtd.plot.allocation_requires_single_effect"
            )
        }
        data$x <- data$nControl
        data$y <- data$nTreatment
        data$panel <- paste0("SD = ", data$stDev, "; effect = ", data$alternative)
        xlab <- "Number of control subjects"
        ylab <- "Number of treatment subjects"
    }
    for (panel in unique(data$panel)) {
        subset <- data[data$panel == panel, , drop = FALSE]
        distinctX <- length(unique(subset$x))
        distinctY <- length(unique(subset$y))
        if (min(distinctX, distinctY) < 2L || nrow(subset) != distinctX * distinctY) {
            stopIllegalArgument("Sensitivity maps require a rectangular grid with at least two values per axis",
                functionName = functionName, parameter = "x", value = subset[c("x", "y")],
                constraint = "complete rectangular grid with at least two values on each axis",
                diagnosticId = "qtd.plot.incomplete_sensitivity_grid", context = list(panel = panel)
            )
        }
    }
    p <- ggplot2::ggplot(data, ggplot2::aes(.data[["x"]], .data[["y"]], fill = .data[["probability"]])) +
        ggplot2::geom_tile() + ggplot2::facet_wrap(~panel) +
        ggplot2::coord_cartesian(xlim = range(data$x), ylim = range(data$y)) +
        ggplot2::labs(x = xlab, y = ylab, fill = paste0("Pr(", category, ")"))
    if (!is.null(contourLevels)) {
        .assertQtdPlotNumeric(contourLevels, "contourLevels", functionName, lower = 0, upper = 1)
        p <- p + ggplot2::geom_contour(ggplot2::aes(x = .data[["x"]], y = .data[["y"]], z = .data[["probability"]]),
            inherit.aes = FALSE, breaks = sort(unique(contourLevels)), color = "grey30")
    }
    return(p)
}

.getQtdDecisionPerformance <- function(data, weights, desiredDecisions, functionName) {
    effects <- unique(data$alternative)
    .assertQtdPlotNumeric(weights, "weights", functionName, lower = 0, expectedLength = length(effects))
    if (is.null(desiredDecisions)) {
        stopMissingArgument("'desiredDecisions' must be supplied for the decision-performance plot",
            functionName = functionName, parameter = "desiredDecisions",
            constraint = "one desired decision per selected effect",
            diagnosticId = "qtd.plot.missing_desired_decisions", context = list(effects = effects)
        )
    }
    if (abs(sum(weights) - 1) > 1e-10) {
        stopIllegalArgument("'weights' must sum to one",
            functionName = functionName, parameter = "weights", value = weights,
            constraint = "nonnegative weights summing to one", diagnosticId = "qtd.plot.weights_not_normalized",
            context = list(weightSum = sum(weights), effects = effects)
        )
    }
    decisions <- .getQtdCategories(3L)
    if (length(desiredDecisions) != length(effects)) {
        stopArgumentLengthOutOfBounds("Supply one desired decision per selected effect",
            functionName = functionName, parameter = "desiredDecisions", value = desiredDecisions,
            constraint = "one desired decision per selected effect",
            expectedLength = length(effects), actualLength = length(desiredDecisions),
            relatedParameter = "alternatives", relatedValue = effects,
            diagnosticId = "qtd.plot.desired_decision_length"
        )
    }
    if (!is.character(desiredDecisions) ||
            anyNA(desiredDecisions) || any(!(desiredDecisions %in% decisions))) {
        stopIllegalArgument("Supply one desired decision per selected effect",
            functionName = functionName, parameter = "desiredDecisions", value = desiredDecisions,
            constraint = "one Go, Indeterminate or Stop per selected effect",
            relatedParameter = "alternatives", relatedValue = effects,
            diagnosticId = "qtd.plot.invalid_desired_decisions", context = list(allowedValues = decisions)
        )
    }
    data <- .uniqueQtdEffectRows(data)
    indices <- match(data$alternative, effects)
    data$desiredDecision <- desiredDecisions[indices]
    data$observedDecision <- .getQtdCoarseCategory(data$category)
    data$probability <- data$probability * weights[indices]
    result <- stats::aggregate(probability ~ desiredDecision + observedDecision + scenario + scenarioLabel + stDev,
        data = data, FUN = sum)
    result$desiredDecision <- factor(result$desiredDecision, levels = decisions)
    result$observedDecision <- factor(result$observedDecision, levels = decisions)
    result$correct <- result$desiredDecision == result$observedDecision
    result$correctProbability <- stats::ave(ifelse(result$correct, result$probability, 0),
        result$scenario, result$stDev, FUN = sum)
    result$performanceLabel <- paste0(result$scenarioLabel, "\nPr(correct) = ",
        formatC(100 * result$correctProbability, digits = 1, format = "f"), "%")
    distribution <- data.frame(effect = effects, weight = weights, desiredDecision = desiredDecisions)
    distribution <- distribution[order(distribution$effect), , drop = FALSE]
    rownames(distribution) <- NULL
    attr(result, "effectDistribution") <- distribution
    return(result)
}
