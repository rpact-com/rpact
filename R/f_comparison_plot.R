## |
## |  *Plot comparison*
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

.getComparisonPlotInput <- function(data, objects = NULL) {
    if (inherits(data, "TrialComparisonSummary")) {
        data <- data$object
    }
    isComparison <- inherits(data, "TrialComparison")
    if (isComparison) {
        objects <- data$getObjects()
        data <- as.data.frame(data)
    }
    return(list(data = data, objects = objects, isComparison = isComparison))
}

.finishComparisonPlotParameters <- function(result, issues) {
    result$issues <- issues
    result["message"] <- list(if (length(issues)) {
        paste(vapply(issues, `[[`, character(1), "message"), collapse = "\n")
    } else {
        NULL
    })
    return(result)
}

#'
#' @title
#' Select Parameters for a Comparison Plot
#'
#' @description
#' Proposes axes and an object category without creating a plot. Selection
#' problems are returned as text so applications can display them before plotting.
#'
#' @param data A comparison data frame, a `TrialComparison`, or its summary.
#' @param xParamName,yParamName Optional explicit axis column names. Automatic
#'   axes must be varying numeric columns. The y-axis must always be numeric;
#'   an explicitly selected x-axis may also be categorical.
#' @param ... Further arguments. Currently unused.
#' @param categoryParamName Optional category column. NULL selects `objectName`,
#'   `objectNumber`, or `designNumber`, in that order, when it identifies multiple
#'   objects and is not selected as an axis or group. NA_character_ disables
#'   automatic categories. No category is needed for a single object.
#' @param groupParamName Optional explicit grouping column.
#' @param objects Optional original objects for role-aware selection when `data`
#'   is a data frame. A `TrialComparison` supplies its own objects automatically.
#'
#' @return A list containing `xParamName`, `yParamName`, `categoryParamName`,
#'   `groupParamName`, `message`, `candidates`, and `issues`. Unresolved selections
#'   are NULL. `message` is NULL on success, otherwise contains all selection
#'   problems. `candidates` lists eligible columns for each selection; `issues`
#'   contains diagnostic metadata used by [plotComparison()].
#'
#' @details
#' Explicit selections prevail. When original objects are available, inputs and
#' design settings are preferred for x and results for y, as in the comparison
#' plot method. Without original objects, only varying numeric columns determine
#' automatic axes. Identifiers are excluded, but stages remain eligible.
#' An omitted axis is selected only when exactly one eligible candidate remains.
#' Ambiguity, invalid column selections, and unsupported data structures are
#' returned without raising a selection error. A category may remain NULL without
#' an error when no object category is available or required. The data and the
#' original objects are not changed, and no ggplot2 installation is required.
#'
#' @examples
#' comparison <- getComparison(
#'     first = getDesignFixed(alpha = 0.025),
#'     second = getDesignFixed(alpha = 0.05)
#' )
#' selection <- getComparisonPlotParameters(comparison, yParamName = "criticalValues")
#' selection[c("xParamName", "yParamName", "categoryParamName", "message")]
#'
#' @export
#'
getComparisonPlotParameters <- function(data, xParamName = NULL, yParamName = NULL, ...,
        categoryParamName = NULL, groupParamName = NULL, objects = NULL) {
    .warnInCaseOfUnknownArguments(functionName = "getComparisonPlotParameters", ...)
    input <- .getComparisonPlotInput(data, objects)
    data <- input$data
    objects <- input$objects
    result <- list(xParamName = xParamName, yParamName = yParamName,
        categoryParamName = categoryParamName, groupParamName = groupParamName,
        message = NULL, candidates = list(xParamName = character(), yParamName = character(),
            categoryParamName = character(), groupParamName = character()), issues = list())
    issues <- list()
    addIssue <- function(message, parameter, value = NULL, constraint, diagnosticId,
            context = list(), type = "argument", relatedParameter = NULL, relatedValue = NULL) {
        issues[[length(issues) + 1L]] <<- list(message = message, parameter = parameter,
            value = value, constraint = constraint, diagnosticId = diagnosticId,
            context = context, type = type, relatedParameter = relatedParameter, relatedValue = relatedValue)
    }
    if (!is.data.frame(data) || !nrow(data) || anyNA(names(data)) ||
            any(!nzchar(names(data))) || anyDuplicated(names(data))) {
        addIssue("Cannot select plot parameters: 'data' must be a non-empty data.frame with unique column names",
            "data", data, "a data.frame with at least one row and unique non-empty column names",
            "comparison.plot_data_invalid", type = "data")
        return(.finishComparisonPlotParameters(result, issues))
    }
    supported <- vapply(data, function(column) {
        is.atomic(column) && is.null(dim(column))
    }, logical(1))
    if (!all(supported)) {
        addIssue("Comparison columns must be one-dimensional atomic vectors",
            "data", data[!supported], "one-dimensional atomic columns",
            "comparison.parameter_column_unsupported", context = list(columns = names(data)[!supported]), type = "data")
        return(.finishComparisonPlotParameters(result, issues))
    }
    if (!is.null(objects) && (!is.list(objects) || is.data.frame(objects) ||
            !all(vapply(objects, inherits, logical(1), c("TrialDesign", "TrialDesignPlan", "SimulationResults"))))) {
        addIssue("'objects' must be a list of original comparison objects",
            "objects", objects, "a list of designs, plans, or simulation results",
            "comparison.plot_objects_invalid")
        return(.finishComparisonPlotParameters(result, issues))
    }
    invalid <- stats::setNames(rep(FALSE, 4L), c("xParamName", "yParamName", "categoryParamName", "groupParamName"))
    categoryDisabled <- is.character(categoryParamName) && length(categoryParamName) == 1L && is.na(categoryParamName)
    if (categoryDisabled) {
        result["categoryParamName"] <- list(NULL)
    }
    for (parameter in names(invalid)) {
        value <- result[[parameter]]
        if (is.null(value)) {
            next
        }
        if (!is.character(value) || length(value) != 1L || is.na(value) || !value %in% names(data)) {
            addIssue(paste0("Invalid column selection for '", parameter, "'"), parameter, value,
                "one non-missing column name from data", "comparison.plot_column_invalid",
                context = list(availableColumns = names(data)))
            result[parameter] <- list(NULL)
            invalid[parameter] <- TRUE
        }
    }
    if (!is.null(result$yParamName) && !is.numeric(data[[result$yParamName]])) {
        addIssue("The y-axis column must be numeric", result$yParamName, data[[result$yParamName]],
            "the selected y-axis column must be numeric", "comparison.plot_y_column_not_numeric",
            type = "data", relatedParameter = "yParamName", relatedValue = result$yParamName)
        result["yParamName"] <- list(NULL)
        invalid["yParamName"] <- TRUE
    }
    available <- getComparisonParameterNames(data, type = "continuous",
        exclude = c("objectNumber", "designNumber", "situation"))
    available <- available[vapply(data[available], is.numeric, logical(1))]
    roles <- stats::setNames(rep("", length(available)), available)
    if (length(objects)) {
        roles <- vapply(available, function(parameter) {
            .comparisonRole(objects, parameter)
        }, character(1))
    }
    for (parameter in c("xParamName", "yParamName")) {
        other <- if (parameter == "xParamName") {
            result$yParamName
        } else {
            result$xParamName
        }
        candidates <- setdiff(available, other)
        preferredRole <- if (parameter == "xParamName") {
            "Inputs and design settings"
        } else {
            "Results and derived parameters"
        }
        preferred <- candidates[roles[candidates] == preferredRole]
        if (length(preferred)) {
            candidates <- preferred
        }
        result$candidates[[parameter]] <- candidates
        if (!is.null(result[[parameter]]) || invalid[parameter]) {
            next
        }
        if (length(candidates) == 1L) {
            result[parameter] <- list(candidates[[1L]])
            next
        }
        info <- getComparisonParameterInfo(data[candidates])
        descriptions <- getComparisonParameterDescriptions(info)
        addIssue(paste0("Cannot select '", parameter, "' unambiguously. Candidates: ",
            paste(head(.comparisonText(descriptions, 85L), 8L), collapse = "; "),
            if (length(candidates) > 8L) {
                "; ..."
            } else {
                ""
            }, "."), parameter, constraint = "supply an axis name when there is not exactly one candidate",
            type = "missingArgument", diagnosticId = "comparison.plot_axis_ambiguous",
            context = list(candidates = candidates, descriptions = descriptions))
    }
    exampleX <- if (!is.null(result$xParamName)) {
        result$xParamName
    } else {
        head(setdiff(available, result$yParamName), 1L)
    }
    exampleY <- if (!is.null(result$yParamName)) {
        result$yParamName
    } else {
        head(setdiff(available, exampleX), 1L)
    }
    example <- if (length(exampleX) == 1L && length(exampleY) == 1L) {
        paste0(if (input$isComparison) {
            "plot(x"
        } else {
            "plotComparison(data"
        }, ", xParamName = ", encodeString(exampleX, quote = '"'),
            ", yParamName = ", encodeString(exampleY, quote = '"'), ")")
    } else {
        "Choose numeric axis columns from the comparison data."
    }
    for (i in seq_along(issues)) {
        if (issues[[i]]$diagnosticId == "comparison.plot_axis_ambiguous") {
            issues[[i]]$context$example <- example
            issues[[i]]$message <- paste(issues[[i]]$message, example)
        }
    }
    identities <- setdiff(intersect(c("objectName", "objectNumber", "designNumber"), names(data)),
        c(result$xParamName, result$yParamName, result$groupParamName))
    categories <- identities[vapply(data[identities], function(column) {
        !anyNA(column) && all(nzchar(trimws(as.character(column)))) && length(unique(column)) > 1L
    }, logical(1))]
    # Names must identify the same objects as the numeric identifier; otherwise
    # distinct objects could be merged into a single curve by repeated labels.
    objectIds <- intersect(c("objectNumber", "designNumber"), names(data))
    if ("objectName" %in% categories && length(objectIds)) {
        id <- objectIds[[1L]]
        pairs <- unique(data[c("objectName", id)])
        if (anyNA(pairs[[id]]) || anyDuplicated(pairs$objectName) || anyDuplicated(pairs[[id]])) {
            categories <- setdiff(categories, "objectName")
        }
    }
    result$candidates$categoryParamName <- categories
    result$candidates$groupParamName <- setdiff(names(data), c(result$xParamName, result$yParamName))
    if (is.null(categoryParamName) && !categoryDisabled && !invalid["categoryParamName"] && length(categories)) {
        result["categoryParamName"] <- list(categories[[1L]])
    }
    return(.finishComparisonPlotParameters(result, issues))
}

.stopComparisonPlotSelection <- function(selection, functionName) {
    issue <- selection$issues[[1L]]
    context <- issue$context
    context$selectionIssues <- selection$issues
    errorHelper <- if (identical(issue$type, "data")) {
        stopIllegalDataInput
    } else if (identical(issue$type, "missingArgument")) {
        stopMissingArgument
    } else {
        stopIllegalArgument
    }
    errorHelper(selection$message, functionName = functionName, parameter = issue$parameter,
        value = issue$value, constraint = issue$constraint, diagnosticId = issue$diagnosticId,
        relatedParameter = issue$relatedParameter, relatedValue = issue$relatedValue, context = context)
}

#'
#' @title
#' Plot Comparison Data
#'
#' @description
#' Produces a ggplot object from unrounded comparison data.
#'
#' @param data A non-empty comparison data frame, a `TrialComparison`, or its summary.
#' @param xParamName,yParamName Optional axis columns, selected by
#'   [getComparisonPlotParameters()] when omitted. The y column must be numeric.
#' @param ... Reserved for future extensions.
#' @param groupParamName,categoryParamName Optional grouping and category columns.
#'   NULL categories are selected automatically; NA_character_ disables categories.
#' @param mainTitle,legendTitle Plot and legend titles.
#' @param plotLineEnabled,plotPointsEnabled Whether to show lines and points.
#' @param pointBorder Point border style, as used by rpact plots.
#' @param discreteXAxis Whether numeric x ticks should use the observed values only.
#' @param mirrorModeEnabled Whether to use fill categories without a legend.
#' @param legendPosition Legend position, either an rpact numeric position or a
#'   ggplot position name (none, left, right, bottom, top).
#' @param shinyOptimization Whether to use compact sizing for Shiny displays.
#' @param objects Optional original objects for role-aware selection with a data
#'   frame, as in [getComparisonPlotParameters()].
#'
#' @return A ggplot object, which can be customized with ggplot2 layers.
#'
#' @details
#' Selection is delegated to [getComparisonPlotParameters()]. If its `message`
#' is non-NULL, plotting raises an error using that text. Applications can call
#' the selector first to display choices and messages without creating a plot.
#' Categories and groups jointly identify lines. Without either,
#'   objectNumber (or designNumber) identifies separate objects. Stages are
#'   separated unless used as an axis. Select categories explicitly to compare
#'   results across objects along a parameter axis.
#'
#' @examples
#' \dontrun{
#' data <- getComparisonData(list(getDesignFixed(), getDesignGroupSequential()))
#' plotComparison(data, "stages", "criticalValues", plotLineEnabled = FALSE)
#' }
#'
#' @export
#'
plotComparison <- function(
        data,
        xParamName = NULL,
        yParamName = NULL,
        ..., 
        groupParamName = NULL,
        categoryParamName = NULL,
        mainTitle = "Object Comparison",
        legendTitle = "Legend",
        plotLineEnabled = TRUE,
        plotPointsEnabled = TRUE,
        pointBorder = 4,
        discreteXAxis = FALSE,
        mirrorModeEnabled = FALSE,
        legendPosition = "bottom",
        shinyOptimization = FALSE,
        objects = NULL) {
    
    .warnInCaseOfUnknownArguments(functionName = "plotComparison", ...)
    .assertGgplotIsInstalled()
    for (name in c("plotLineEnabled", "plotPointsEnabled", "discreteXAxis", "shinyOptimization", "mirrorModeEnabled")) {
        .assertIsSingleLogical(get(name), name)
    }
    selection <- getComparisonPlotParameters(data, xParamName, yParamName,
        categoryParamName = categoryParamName, groupParamName = groupParamName, objects = objects)
    if (!is.null(selection$message)) {
        functionName <- if (inherits(data, c("TrialComparison", "TrialComparisonSummary"))) {
            "plot.TrialComparison"
        } else {
            "plotComparison"
        }
        .stopComparisonPlotSelection(selection, functionName)
    }
    data <- .getComparisonPlotInput(data, objects)$data
    xParamName <- selection$xParamName
    yParamName <- selection$yParamName
    categoryParamName <- selection$categoryParamName
    groupParamName <- selection$groupParamName
    plotSettings <- PlotSettings$new()
    
    if (shinyOptimization) {
        plotSettings$.pointBorderEnabled <- FALSE
        plotSettings$pointSize <- 2.2
        plotSettings$scalingFactor <- 0.85
    }
    
    categoryEnabled <- !is.null(categoryParamName) && length(categoryParamName) == 1 && !is.na(categoryParamName)
    groupEnabled <- !is.null(groupParamName) && length(groupParamName) == 1 && !is.na(groupParamName)
    
    if (categoryEnabled && groupEnabled) {
        p <- ggplot2::ggplot(
            data = data, 
            mapping = ggplot2::aes(
                x = !!ggplot2::sym(xParamName), 
                y = !!ggplot2::sym(yParamName),
                colour = factor(!!ggplot2::sym(groupParamName)),
                fill = factor(!!ggplot2::sym(categoryParamName))
            )
        )
    } else if (groupEnabled && !categoryEnabled) {
        p <- ggplot2::ggplot(data, ggplot2::aes(
            x = !!ggplot2::sym(xParamName), y = !!ggplot2::sym(yParamName),
            colour = factor(!!ggplot2::sym(groupParamName))))
    } else if (categoryEnabled && mirrorModeEnabled) {
        p <- ggplot2::ggplot(
            data = data, 
            mapping = ggplot2::aes(
                x = !!ggplot2::sym(xParamName), 
                y = !!ggplot2::sym(yParamName),
                fill = factor(!!ggplot2::sym(categoryParamName))
            )
        )
    } else if (categoryEnabled) {
        p <- ggplot2::ggplot(
            data = data, 
            mapping = ggplot2::aes(
                x = !!ggplot2::sym(xParamName), 
                y = !!ggplot2::sym(yParamName),
                colour = factor(!!ggplot2::sym(categoryParamName))
            )
        )
    } else {
        p <- ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = !!ggplot2::sym(xParamName), y = !!ggplot2::sym(yParamName)))
    }
    
    if (grepl("situation|stage|objectNumber|kMax|sided", xParamName, ignore.case = TRUE)) {
        breaks <- sort(unique(round(na.omit(data[[xParamName]]))))
        p <- p + ggplot2::scale_x_continuous(breaks = breaks)
    }
    
    groups <- c(categoryParamName, groupParamName)
    if (!length(groups)) {
        groups <- intersect(c("objectNumber", "designNumber"), names(data))[1L]
    }
    groups <- groups[!is.na(groups)]
    stages <- setdiff(intersect(c("stages", "stage"), names(data)), c(xParamName, yParamName))
    groups <- unique(c(groups, stages))
    if (length(groups)) {
        groupColumn <- tail(make.unique(c(names(data), ".comparisonGroup")), 1L)
        data[[groupColumn]] <- do.call(interaction, c(data[groups], list(drop = TRUE, lex.order = TRUE)))
        p$data <- data
        p <- p + ggplot2::aes(group = !!ggplot2::sym(groupColumn))
    } else if (isTRUE(plotLineEnabled)) {
        p <- p + ggplot2::aes(group = 1L)
    }
    p <- plotSettings$setTheme(p)
    p <- plotSettings$hideGridLines(p)
    
    if (discreteXAxis && is.numeric(data[[xParamName]])) {
        p <- p + ggplot2::scale_x_continuous(breaks = sort(unique(data[[xParamName]])))
    }
    
    # set main title
    if (!is.null(mainTitle) && length(mainTitle) == 1 && !is.na(mainTitle)) {
        p <- plotSettings$setMainTitle(p, mainTitle)
    } else {
        p <- p + ggplot2::theme(plot.title = ggplot2::element_blank())
    }
    
    # set legend
    if ((!categoryEnabled && !groupEnabled) || mirrorModeEnabled || (!is.na(legendPosition) && legendPosition == -1)) {
        p <- p + ggplot2::theme(legend.position = "none")
    } else {
        if (is.numeric(legendPosition)) {
            p <- plotSettings$setLegendPosition(p, legendPosition)
        } else {
            if (!legendPosition %in% c("none", "left", "right", "bottom", "top")) {
                stopIllegalArgument("Invalid legend position",
                    functionName = "plotComparison", parameter = "legendPosition", value = legendPosition,
                    constraint = "one of none, left, right, bottom, top for a character legend position",
                    diagnosticId = "comparison.plot_legend_position_invalid",
                    context = list(allowedValues = c("none", "left", "right", "bottom", "top"))
                )
            }
            p <- p + ggplot2::theme(legend.position = legendPosition)
        }
        p <- plotSettings$setLegendBorder(p)
        p <- plotSettings$setLegendTitle(p, legendTitle)
        p <- plotSettings$setLegendLabelSize(p)
    }
    
    p <- plotSettings$plotValues(p,
        plotLineEnabled = isTRUE(plotLineEnabled),
        plotPointsEnabled = isTRUE(plotPointsEnabled), 
        pointBorder = pointBorder
    )
    
    p <- plotSettings$setAxesAppearance(p)
    p <- plotSettings$setColorPalette(p, "Set1")
    p <- plotSettings$enlargeAxisTicks(p)
    
    p
}
