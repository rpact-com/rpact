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

.comparisonYAxisSettings <- function(data, parameters, yAxisSide = "auto") {
    issue <- function(message, parameter, value, diagnosticId, constraint, type = "argument", context = list()) {
        return(list(message = message, parameter = parameter, value = value,
            diagnosticId = diagnosticId, constraint = constraint, type = type, context = context))
    }
    result <- list(side = NULL, transformation = NULL, ranges = NULL, issue = NULL)
    if (is.null(yAxisSide)) {
        yAxisSide <- "auto"
    }
    valid <- is.character(yAxisSide) && is.null(dim(yAxisSide)) && length(yAxisSide) > 0L && !anyNA(yAxisSide) &&
        all(yAxisSide %in% c("auto", "left", "right"))
    sideNames <- names(yAxisSide)
    if (valid && !is.null(sideNames)) {
        valid <- !anyNA(sideNames) && all(nzchar(sideNames)) && !anyDuplicated(sideNames) &&
            all(sideNames %in% parameters)
    } else if (valid) {
        valid <- length(yAxisSide) %in% c(1L, length(parameters))
    }
    if (!valid) {
        result$issue <- issue("'yAxisSide' must assign auto, left, or right to the selected y parameters",
            "yAxisSide", yAxisSide, "comparison.plot_y_axis_side_invalid",
            "one side for all parameters, one per parameter in order, or a named subset of selected y parameters",
            context = list(yParamName = parameters, allowedValues = c("auto", "left", "right")))
        return(result)
    }
    requested <- stats::setNames(rep("auto", length(parameters)), parameters)
    if (is.null(sideNames)) {
        requested[] <- rep(yAxisSide, length.out = length(parameters))
    } else {
        requested[sideNames] <- yAxisSide
    }
    magnitudes <- vapply(data[parameters], function(values) {
        finite <- values[is.finite(values)]
        if (!length(finite)) {
            return(0)
        }
        return(max(abs(finite)))
    }, numeric(1))
    suggested <- stats::setNames(rep("left", length(parameters)), parameters)
    positive <- sort(unique(magnitudes[magnitudes > 0]))
    if (length(positive) > 1L) {
        gaps <- diff(log10(positive))
        split <- which.max(gaps)
        # A one-order-of-magnitude gap separates scale groups. Keep the larger
        # scale on the left by default; close scales share a single axis.
        if (gaps[split] >= 1 - sqrt(.Machine$double.eps)) {
            suggested[magnitudes <= positive[split]] <- "right"
        }
    }
    specified <- requested != "auto"
    if (length(unique(suggested)) > 1L && any(specified) &&
            all(requested[specified] != suggested[specified])) {
        # An unambiguous explicit reversal also reverses the automatic group.
        suggested <- ifelse(suggested == "left", "right", "left")
    }
    suggested[specified] <- requested[specified]
    result$side <- suggested
    axisValues <- lapply(c("left", "right"), function(side) {
        values <- unlist(data[parameters[suggested == side]], use.names = FALSE)
        return(values[is.finite(values)])
    })
    names(axisValues) <- c("left", "right")
    ranges <- lapply(axisValues, function(values) {
        bounds <- range(c(0, values))
        if (!length(values) || diff(bounds) == 0) {
            return(c(0, 1))
        }
        return(bounds)
    })
    hasLeft <- any(suggested == "left")
    hasRight <- any(suggested == "right")
    for (side in names(axisValues)) {
        values <- axisValues[[side]]
        if (length(values) && all(values >= -sqrt(.Machine$double.eps) &
                values <= 1 + sqrt(.Machine$double.eps)) && max(values) >= 0.1) {
            ranges[[side]] <- c(0, 1)
        }
    }
    transformation <- list(scale = 1, offset = 0)
    if (hasLeft && hasRight) {
        transformation$scale <- diff(ranges$left) / diff(ranges$right)
        transformation$offset <- ranges$left[1L] - transformation$scale * ranges$right[1L]
        if (!is.finite(transformation$scale) || transformation$scale <= 0 || !is.finite(transformation$offset)) {
            result$issue <- issue("Cannot construct a finite invertible transformation between the y axes",
                "data", data[parameters], "comparison.plot_y_scaling_invalid",
                "finite axis ranges allowing a positive finite scale and finite offset", type = "data",
                context = list(yParamName = parameters, yAxisSide = suggested, ranges = ranges))
            return(result)
        }
    }
    result$transformation <- transformation
    result$ranges <- list(left = if (hasLeft) {
        ranges$left
    } else {
        NULL
    }, right = if (hasRight) {
        ranges$right
    } else {
        NULL
    })
    return(result)
}

.comparisonYCaptions <- function(parameters, objects) {
    return(vapply(parameters, function(parameter) {
        captions <- vapply(objects, function(object) {
            .comparisonCaption(parameter, object)
        }, character(1))
        .comparisonCaptionLabel(parameter, captions)
    }, character(1)))
}

.comparisonPlotYData <- function(data, parameters, captions, sides, transformation) {
    if (length(parameters) == 1L) {
        return(list(data = data, yColumn = parameters, parameterColumn = NULL, labels = NULL))
    }
    columns <- tail(make.unique(c(names(data), ".comparisonYParameter", ".comparisonYValue",
        ".comparisonYPlot", ".comparisonYAxis")), 4L)
    names(columns) <- c("parameter", "value", "plot", "side")
    twoAxes <- length(unique(sides)) > 1L
    labels <- if (twoAxes) {
        paste0(captions, " (", sides, ")")
    } else {
        captions
    }
    labels <- make.unique(labels)
    frames <- lapply(seq_along(parameters), function(i) {
        frame <- data
        frame[[columns[["parameter"]]]] <- factor(rep(parameters[i], nrow(data)),
            levels = parameters, labels = labels)
        frame[[columns[["value"]]]] <- data[[parameters[i]]]
        frame[[columns[["plot"]]]] <- if (twoAxes && sides[i] == "right") {
            data[[parameters[i]]] * transformation$scale + transformation$offset
        } else {
            data[[parameters[i]]]
        }
        frame[[columns[["side"]]]] <- rep(sides[i], nrow(data))
        return(frame)
    })
    combined <- do.call(rbind, frames)
    rownames(combined) <- NULL
    return(list(data = combined, yColumn = columns[["plot"]],
        parameterColumn = columns[["parameter"]], labels = labels))
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
#' @param xParamName,yParamName Optional explicit axis column names. `yParamName`
#'   may be a vector of distinct numeric columns. Automatic
#'   axes must be varying numeric columns. Every y column must be numeric;
#'   an explicitly selected x-axis may also be categorical.
#' @param ... Further arguments. Currently unused.
#' @param categoryParamName Optional character vector of category column names.
#'   All selected columns jointly define the categories. NULL selects `objectName`,
#'   `objectNumber`, or `designNumber`, in that order, when it identifies multiple
#'   objects and is not selected as an axis or group. NA_character_ disables
#'   automatic categories, as does character(0). No category is needed for a single object.
#' @param groupParamName Optional explicit grouping column.
#' @param objects Optional original objects for role-aware selection when `data`
#'   is a data frame. A `TrialComparison` supplies its own objects automatically.
#' @param yAxisSide Axis assignment: "auto" (default), "left", or "right".
#'   Supply one value for all y parameters, a vector in their order, or a named
#'   vector assigning individual parameters. Unspecified named entries remain
#'   automatic. The same argument is accepted by [plotComparison()].
#'
#' @return A list containing `xParamName`, `yParamName`, `categoryParamName`,
#'   `groupParamName`, `yAxisSide`, `yAxisTransformation`, `yAxisRanges`, `message`,
#'   `candidates`, and `issues`. `yAxisSide` is named by the selected parameters;
#'   the right-axis plotting transformation is `value * scale + offset` from
#'   `yAxisTransformation`. `yAxisRanges` contains the reference ranges used
#'   for scaling, not ggplot's expanded panel limits. Unresolved selections
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
#' Automatic y-axis assignment uses the largest gap between finite nonzero
#' parameter magnitudes (maximum absolute values). A gap of at least a factor
#' of ten separates the smaller scale onto the right axis. Otherwise all
#' parameters share the left axis. Consistent explicit reversals also reverse
#' automatic assignments; explicit assignments always prevail. There are at
#' most two shared scales, with no separate normalization within either side.
#' Dual-axis reference ranges include zero. Probability-like ranges within
#' numerical tolerance of `[0, 1]`, with maximum at least 0.1, use `[0, 1]`. Smaller
#' magnitudes retain their observed range. Constant zero or entirely missing
#' ranges use `[0, 1]` to avoid a degenerate transformation.
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
        categoryParamName = NULL, groupParamName = NULL, objects = NULL, yAxisSide = "auto") {
    .warnInCaseOfUnknownArguments(functionName = "getComparisonPlotParameters", ...)
    input <- .getComparisonPlotInput(data, objects)
    data <- input$data
    objects <- input$objects
    result <- list(xParamName = xParamName, yParamName = yParamName,
        categoryParamName = categoryParamName, groupParamName = groupParamName,
        yAxisSide = NULL, yAxisTransformation = NULL, yAxisRanges = NULL,
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
            parameter = "data", 
            value = data,
            constraint = "a data.frame with at least one row and unique non-empty column names",
            diagnosticId = "comparison.plot_data_invalid", 
            type = "data")
        return(.finishComparisonPlotParameters(result, issues))
    }
    supported <- vapply(data, function(column) {
        is.atomic(column) && is.null(dim(column))
    }, logical(1))
    if (!all(supported)) {
        addIssue("Comparison columns must be one-dimensional atomic vectors",
            parameter = "data", 
            value = data[!supported], 
            "one-dimensional atomic columns",
            diagnosticId = "comparison.parameter_column_unsupported", 
            context = list(columns = names(data)[!supported]), type = "data")
        return(.finishComparisonPlotParameters(result, issues))
    }
    if (!is.null(objects) && (!is.list(objects) || is.data.frame(objects) ||
            !all(vapply(objects, .isComparisonObject, logical(1))))) {
        addIssue("'objects' must be a list of original comparison objects",
            parameter = "objects", 
            objects, 
            "a list of designs, design characteristics, plans, or simulation results",
            diagnosticId = "comparison.plot_objects_invalid")
        return(.finishComparisonPlotParameters(result, issues))
    }
    invalid <- stats::setNames(rep(FALSE, 4L), c("xParamName", "yParamName", "categoryParamName", "groupParamName"))
    categoryDisabled <- is.character(categoryParamName) &&
        (length(categoryParamName) == 0L || (length(categoryParamName) == 1L && is.na(categoryParamName)))
    if (categoryDisabled) {
        result["categoryParamName"] <- list(NULL)
    }
    for (parameter in names(invalid)) {
        value <- result[[parameter]]
        if (is.null(value)) {
            next
        }
        
        multipleAllowed <- parameter %in% c("categoryParamName", "yParamName")
        valid <- is.character(value) && is.null(dim(value)) && length(value) > 0L && !anyNA(value) &&
            all(nzchar(value)) && all(value %in% names(data)) &&
            (multipleAllowed || length(value) == 1L)
        if (valid && multipleAllowed) {
            valid <- !anyDuplicated(value)
        }
        if (!valid) {
            constraint <- if (multipleAllowed) {
                "one or more distinct non-missing column names from data"
            } else {
                "one non-missing column name from data"
            }
            addIssue(paste0("Invalid column selection for '", parameter, "' (", .arrayToString(value, maxLength = 5), ")"),
                parameter = parameter, 
                value = value,
                constraint = constraint,
                diagnosticId = "comparison.plot_column_invalid",
                context = list(availableColumns = names(data), invalidColumns = if (is.character(value)) {
                    setdiff(value, names(data))
                } else {
                    character()
                }))
            result[parameter] <- list(NULL)
            invalid[parameter] <- TRUE
        }
    }
    if (!is.null(result$yParamName)) {
        nonNumeric <- result$yParamName[!vapply(data[result$yParamName], is.numeric, logical(1))]
        for (parameter in nonNumeric) {
            addIssue("The y-axis column must be numeric", 
                parameter = parameter, 
                value = data[[parameter]],
                constraint = "every selected y-axis column must be numeric", 
                diagnosticId = "comparison.plot_y_column_not_numeric",
                type = "data", 
                relatedParameter = "yParamName", 
                relatedValue = parameter)
        }
        if (length(nonNumeric)) {
            result["yParamName"] <- list(NULL)
            invalid["yParamName"] <- TRUE
        }
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
            }, "."), 
            parameter = parameter, 
            constraint = "supply an axis name when there is not exactly one candidate",
            type = "missingArgument", 
            diagnosticId = "comparison.plot_axis_ambiguous",
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
    example <- if (length(exampleX) == 1L && length(exampleY) >= 1L) {
        yCode <- if (length(exampleY) == 1L) {
            encodeString(exampleY, quote = '"')
        } else {
            paste0("c(", paste(encodeString(exampleY, quote = '"'), collapse = ", "), ")")
        }
        paste0(if (input$isComparison) {
            "plot(x"
        } else {
            "plotComparison(data"
        }, ", xParamName = ", encodeString(exampleX, quote = '"'),
            ", yParamName = ", yCode, ")")
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
    if (!is.null(result$yParamName)) {
        axes <- .comparisonYAxisSettings(data, result$yParamName, yAxisSide)
        result["yAxisSide"] <- list(axes$side)
        result["yAxisTransformation"] <- list(axes$transformation)
        result["yAxisRanges"] <- list(axes$ranges)
        if (!is.null(axes$issue)) {
            issue <- axes$issue
            addIssue(issue$message, 
                parameter = issue$parameter, 
                value = issue$value, 
                constraint = issue$constraint,
                diagnosticId = issue$diagnosticId, 
                context = issue$context, 
                type = issue$type)
        }
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

.comparisonInteraction <- function(data, columns, labelsEnabled = FALSE) {
    factors <- lapply(data[columns], function(column) {
        factor(column, exclude = NULL)
    })
    # Integer factor codes avoid collisions when category values contain the
    # separator. Missing values are explicit levels, so no rows lose their group.
    codes <- lapply(factors, as.integer)
    result <- do.call(interaction, c(unname(codes), list(drop = TRUE, lex.order = TRUE)))
    if (labelsEnabled) {
        positions <- match(levels(result), as.character(result))
        labels <- vapply(positions, function(position) {
            values <- vapply(factors, function(column) {
                as.character(column)[position]
            }, character(1))
            values[is.na(values)] <- "NA"
            paste(values, collapse = " / ")
        }, character(1))
        levels(result) <- make.unique(labels)
    }
    return(result)
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
#'   [getComparisonPlotParameters()] when omitted. `yParamName` may be a vector
#'   of distinct numeric columns.
#' @param ... Reserved for future extensions.
#' @param groupParamName Optional grouping column, mapped to colour when categories
#'   are also present.
#' @param categoryParamName Optional character vector of category columns. Multiple
#'   columns define their joint combinations. NULL selects categories automatically;
#'   NA_character_ or character(0) disables categories.
#' @param mainTitle Plot title. When omitted, generated from the original objects
#'   using the same title as print and summary. Supply NULL or NA to hide it.
#' @param legendTitle Category/group legend title. When omitted, object identifiers
#'   use Design, Calculation, Simulation, or a combination derived from the original
#'   objects or comparison metadata. Other categories use their parameter captions.
#'   With multiple y parameters and no categories, the title is Parameter.
#'   Explicit titles, including NULL to hide titles, take precedence.
#' @param plotLineEnabled,plotPointsEnabled Whether to show lines and points.
#' @param pointBorder Point border style, as used by rpact plots.
#' @param discreteXAxis Whether numeric x ticks should use the observed values only.
#' @param mirrorModeEnabled Whether to use fill categories without a legend.
#' @param legendPosition Shared legend position. "auto" (default) uses the
#'   adaptive placement described below. Alternatively use rpact positions 1 (top left),
#'   2 (centre left), 3 (bottom left), 4 (top right), 5 (centre right),
#'   6 (bottom right), -1 (hidden), 0 (default position with square aspect),
#'   or a ggplot position name (none, left, right, bottom, top).
#' @param categoryLegendPosition,parameterLegendPosition Optional separate positions
#'   for category/group and y-parameter legends, using the same values as
#'   `legendPosition`. NULL inherits the shared setting. With both guides present,
#'   shared automatic positioning uses the adaptive layout described below.
#'   Explicit positions override this layout; two explicit inside positions may
#'   place the guides independently. A shared hidden legend remains hidden.
#' @param shinyOptimization Whether to use compact sizing for Shiny displays.
#' @param objects Optional original objects for role-aware selection with a data
#'   frame, as in [getComparisonPlotParameters()].
#' @inheritParams getComparisonPlotParameters
#'
#' @return A ggplot object, which can be customized with ggplot2 layers.
#'
#' @details
#' Selection is delegated to [getComparisonPlotParameters()]. If its `message`
#' is non-NULL, plotting raises an error using that text. Applications can call
#' the selector first to display choices and messages without creating a plot.
#' Multiple category columns are combined into a discrete colour (or fill when
#' an explicit group is present or mirror mode is enabled). Legend labels show
#' category values in the supplied column order, separated by ' / '. Every
#' observed combination remains distinct, including missing values and values
#' containing separators. Category columns must be distinct valid column names.
#' More than nine colour groups use a hue palette instead of the limited Set1
#' palette, so every combination receives a colour.
#' Categories and groups jointly identify lines. Without either,
#'   objectNumber (or designNumber) identifies separate objects. Stages are
#'   separated unless used as an axis. Select categories explicitly to compare
#'   results across objects along a parameter axis.
#'
#' Multiple y parameters are drawn as separate series, identified by line type
#' and point shape. These styles cycle for larger selections and can be replaced
#' with ggplot2 scales. Categories and groups remain independent of the parameter
#' identity. All axis labels and parameter legends use the rpact parameter captions
#' with the original object context when available, falling back to technical
#' names for unknown parameters. This also applies to a single y parameter. With two
#' y axes, right-axis values are transformed onto the left plotting scale and
#' the right axis displays the exact inverse transformation, retaining original
#' units. Raw values and original columns remain in the long plot data; the
#' caller's data are not modified. Explicitly placing all parameters on the
#' right uses one primary axis on the right. Parameters assigned to the same
#' side always share that scale.
#'
#' For multiple y parameters with category/group legends, `legendPosition = "auto"`
#' prefers horizontal category entries below the x-axis and a separate vertical
#' parameter legend automatically placed inside. Category entries wrap into rows
#' according to measured guide and panel dimensions. If the parameter legend
#' takes too much inside space or covers more data, the alternative layout places
#' it vertically below the panel and positions the category legend inside. Layout changes and label
#' wrapping precede font reductions, which are limited to ten percent. If neither
#' inside layout fits, both guides are placed below the panel. Very large legends
#' still require a larger graphics device or an explicit layout.
#' Explicit shared positions, such as "bottom", continue to apply to both guides
#' unless a separate position is supplied. With only one guide, the common rpact
#' positioning rules apply. Automatic inside placement scores points and line
#' segments in panel coordinates, including secondary-axis scaling. This is a
#' heuristic using the current device (seven inches square if none is open);
#' resizing or exporting to another size may require recreating the plot.
#'
#' Interactive converters must preserve the secondary-axis transformation.
#' A plain `plotly::ggplotly()` conversion may omit the right axis and display
#' transformed hover values. [getComparisonPlotParameters()] exposes the side
#' assignments and transformation for application-specific adapters.
#'
#' @examples
#' \dontrun{
#' data <- getComparisonData(list(getDesignFixed(), getDesignGroupSequential()))
#' plotComparison(data, "stages", "criticalValues", plotLineEnabled = FALSE)
#' plotComparison(data, "stages", "criticalValues",
#'     categoryParamName = c("objectType", "sided"), plotLineEnabled = FALSE)
#' performance <- as.data.frame(getPowerAndAverageSampleNumber(
#'     getDesignGroupSequential(kMax = 3), nMax = 100))
#' plotComparison(performance, "theta",
#'     c("averageSampleNumber", "overallEarlyStop", "calculatedPower"),
#'     yAxisSide = c(averageSampleNumber = "left",
#'         overallEarlyStop = "right", calculatedPower = "right"),
#'     plotPointsEnabled = FALSE)
#' plotComparison(data, "stages", c("criticalValues", "stageLevels"),
#'     categoryLegendPosition = "auto", parameterLegendPosition = "bottom")
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
        mainTitle = NULL,
        legendTitle = NULL,
        plotLineEnabled = TRUE,
        plotPointsEnabled = TRUE,
        pointBorder = 4,
        discreteXAxis = FALSE,
        mirrorModeEnabled = FALSE,
        legendPosition = "auto",
        shinyOptimization = FALSE,
        objects = NULL,
        yAxisSide = "auto",
        categoryLegendPosition = NULL,
        parameterLegendPosition = NULL) {
    
    .warnInCaseOfUnknownArguments(functionName = "plotComparison", ...)
    .assertGgplotIsInstalled()
    .assertComparisonLegendPosition(categoryLegendPosition, "categoryLegendPosition")
    .assertComparisonLegendPosition(parameterLegendPosition, "parameterLegendPosition")
    categoryLegendPosition <- unname(categoryLegendPosition)
    parameterLegendPosition <- unname(parameterLegendPosition)
    for (name in c("plotLineEnabled", "plotPointsEnabled", "discreteXAxis", "shinyOptimization", "mirrorModeEnabled")) {
        .assertIsSingleLogical(get(name), name)
    }
    selection <- getComparisonPlotParameters(data, xParamName, yParamName,
        categoryParamName = categoryParamName, groupParamName = groupParamName, objects = objects,
        yAxisSide = yAxisSide)
    if (!is.null(selection$message)) {
        functionName <- if (inherits(data, c("TrialComparison", "TrialComparisonSummary"))) {
            "plot.TrialComparison"
        } else {
            "plotComparison"
        }
        .stopComparisonPlotSelection(selection, functionName)
    }
    input <- .getComparisonPlotInput(data, objects)
    if (missing(mainTitle)) {
        mainTitle <- .toCapitalizedSingleCharacter(.getTrialComparisonTitle(input$objects))
    }
    data <- input$data
    xParamName <- selection$xParamName
    yParamName <- selection$yParamName
    categoryParamName <- selection$categoryParamName
    groupParamName <- selection$groupParamName
    captions <- .comparisonYCaptions(yParamName, input$objects)
    sides <- selection$yAxisSide
    transformation <- selection$yAxisTransformation
    multipleY <- length(yParamName) > 1L
    twoAxes <- length(unique(sides)) > 1L
    plotSettings <- PlotSettings$new()
    
    if (shinyOptimization) {
        plotSettings$.pointBorderEnabled <- FALSE
        plotSettings$pointSize <- 2.2
        plotSettings$scalingFactor <- 0.85
    }
    
    categoryEnabled <- length(categoryParamName) > 0L
    groupEnabled <- !is.null(groupParamName) && length(groupParamName) == 1 && !is.na(groupParamName)
    colourParameters <- if (groupEnabled) {
        groupParamName
    } else {
        categoryParamName
    }
    if (missing(legendTitle)) {
        legendTitle <- .comparisonLegendTitle(data, input$objects, colourParameters)
    }
    fillLegendTitle <- .comparisonLegendTitle(data, input$objects, categoryParamName)
    categoryColumn <- categoryParamName
    if (length(categoryParamName) > 1L) {
        categoryColumn <- tail(make.unique(c(names(data), ".comparisonCategory")), 1L)
        data[[categoryColumn]] <- .comparisonInteraction(data, categoryParamName, labelsEnabled = TRUE)
    }
    
    prepared <- .comparisonPlotYData(data, yParamName, captions, sides, transformation)
    data <- prepared$data
    yColumn <- prepared$yColumn
    parameterColumn <- prepared$parameterColumn

    if (categoryEnabled && groupEnabled) {
        p <- ggplot2::ggplot(
            data = data, 
            mapping = ggplot2::aes(
                x = !!ggplot2::sym(xParamName), 
                y = !!ggplot2::sym(yColumn),
                colour = factor(!!ggplot2::sym(groupParamName)),
                fill = factor(!!ggplot2::sym(categoryColumn))
            )
        )
    } else if (groupEnabled && !categoryEnabled) {
        p <- ggplot2::ggplot(data, ggplot2::aes(
            x = !!ggplot2::sym(xParamName), y = !!ggplot2::sym(yColumn),
            colour = factor(!!ggplot2::sym(groupParamName))))
    } else if (categoryEnabled && mirrorModeEnabled) {
        p <- ggplot2::ggplot(
            data = data, 
            mapping = ggplot2::aes(
                x = !!ggplot2::sym(xParamName), 
                y = !!ggplot2::sym(yColumn),
                fill = factor(!!ggplot2::sym(categoryColumn))
            )
        )
    } else if (categoryEnabled) {
        p <- ggplot2::ggplot(
            data = data, 
            mapping = ggplot2::aes(
                x = !!ggplot2::sym(xParamName), 
                y = !!ggplot2::sym(yColumn),
                colour = factor(!!ggplot2::sym(categoryColumn))
            )
        )
    } else {
        p <- ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = !!ggplot2::sym(xParamName), y = !!ggplot2::sym(yColumn)))
    }
    
    if (multipleY) {
        if (plotLineEnabled) {
            p <- p + ggplot2::aes(linetype = !!ggplot2::sym(parameterColumn))
        }
        if (plotPointsEnabled) {
            p <- p + ggplot2::aes(shape = !!ggplot2::sym(parameterColumn))
        }
        if (!categoryEnabled && !groupEnabled) {
            p <- p + ggplot2::aes(colour = !!ggplot2::sym(parameterColumn))
        }
        parameterLegendTitle <- if (categoryEnabled || groupEnabled) {
            "Parameter"
        } else {
            legendTitle
        }
        if (plotLineEnabled) {
            p <- p + ggplot2::scale_linetype_manual(name = parameterLegendTitle,
                values = stats::setNames(rep(c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                    length.out = length(prepared$labels)), prepared$labels))
        }
        if (plotPointsEnabled) {
            p <- p + ggplot2::scale_shape_manual(name = parameterLegendTitle,
                values = stats::setNames(rep(c(21, 22, 24, 23, 25), length.out = length(prepared$labels)), prepared$labels))
        }
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
    groups <- unique(c(groups, stages, parameterColumn))
    if (length(groups)) {
        groupColumn <- tail(make.unique(c(names(data), ".comparisonGroup")), 1L)
        data[[groupColumn]] <- .comparisonInteraction(data, groups)
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
    automaticLegend <- identical(legendPosition, "auto")
    if ((!categoryEnabled && !groupEnabled && !multipleY) || mirrorModeEnabled || (!is.na(legendPosition) && legendPosition == -1)) {
        p <- p + ggplot2::theme(legend.position = "none")
    } else {
        if (automaticLegend || is.numeric(legendPosition)) {
            p <- plotSettings$setLegendPosition(p, legendPosition, finalize = FALSE)
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
        if (categoryEnabled && groupEnabled) {
            p <- p + ggplot2::labs(fill = fillLegendTitle)
        }
    }
    
    p <- plotSettings$plotValues(p,
        plotLineEnabled = isTRUE(plotLineEnabled),
        plotPointsEnabled = isTRUE(plotPointsEnabled) && !multipleY,
        pointBorder = pointBorder
    )
    if (multipleY && plotPointsEnabled) {
        if (pointBorder > 0 && plotSettings$.pointBorderEnabled) {
            p <- p + ggplot2::geom_point(colour = "white", fill = "white",
                size = plotSettings$scaleSize(plotSettings$pointSize, TRUE),
                stroke = pointBorder / 2.25, show.legend = FALSE)
        }
        if ((categoryEnabled && groupEnabled) || (categoryEnabled && mirrorModeEnabled)) {
            p <- p + ggplot2::geom_point(size = plotSettings$scaleSize(plotSettings$pointSize, TRUE), alpha = 1)
        } else {
            p <- p + ggplot2::geom_point(size = plotSettings$scaleSize(plotSettings$pointSize, TRUE),
                fill = "white", alpha = 1)
        }
    }
    
    p <- plotSettings$setAxesAppearance(p)
    colourValues <- if (groupEnabled) {
        data[[groupParamName]]
    } else if (categoryEnabled && !mirrorModeEnabled) {
        data[[categoryColumn]]
    } else if (multipleY && !mirrorModeEnabled) {
        data[[parameterColumn]]
    } else {
        NULL
    }
    # Set1 supports at most nine colours; joint categories can exceed that limit.
    palette <- if (length(unique(stats::na.omit(colourValues))) > 9L) {
        NULL
    } else {
        "Set1"
    }
    p <- plotSettings$setColorPalette(p, palette)
    p <- plotSettings$enlargeAxisTicks(p)
    p <- p + ggplot2::labs(
        x = unname(.comparisonYCaptions(xParamName, input$objects)),
        y = paste(captions, collapse = " / "))
    if (twoAxes) {
        scale <- transformation$scale
        offset <- transformation$offset
        axisRange <- selection$yAxisRanges$left
        p <- p + ggplot2::scale_y_continuous(
            name = paste(captions[sides == "left"], collapse = " / "),
            limits = function(limits) {
                range(c(limits, axisRange))
            },
            sec.axis = ggplot2::sec_axis(~ (. - offset) / scale,
                name = paste(captions[sides == "right"], collapse = " / ")))
    } else if (any(sides == "right")) {
        p <- p + ggplot2::scale_y_continuous(name = paste(captions, collapse = " / "), position = "right")
    }
    p <- .preparePlotLegend(p)
    categoryAesthetics <- if (groupEnabled && categoryEnabled) {
        c("colour", "fill")
    } else if (groupEnabled || categoryEnabled) {
        "colour"
    } else {
        character()
    }
    parameterAesthetics <- if (multipleY) {
        c(if (plotLineEnabled) {
            "linetype"
        }, if (plotPointsEnabled) {
            "shape"
        }, if (!categoryEnabled && !groupEnabled) {
            "colour"
        })
    } else {
        character()
    }
    return(.layoutComparisonLegends(p, plotSettings, legendPosition,
        categoryAesthetics, parameterAesthetics, categoryLegendPosition, parameterLegendPosition))
}

.comparisonLegendTitle <- function(data, objects, parameters) {
    if (!length(parameters)) {
        return("Parameter")
    }
    title <- vapply(parameters, function(parameter) {
        if (!parameter %in% c("objectNumber", "objectName", "designNumber")) {
            return(.comparisonYCaptions(parameter, objects))
        }
        types <- .comparisonLegendObjectTypes(data, objects)
        if (!length(objects) && !any(c("objectType", "calculationTarget") %in% names(data)) &&
                parameter == "designNumber") {
            types <- "Design"
        }
        
        types[is.na(types)] <- "Object"
        types <- intersect(c("Design", "Calculation", "Simulation", "Object"), unique(types))
        if (!length(types)) {
            return("Object")
        }
        
        if (!"Object" %in% types) {
            return("Design plan")
        }
        
        if (length(types) > 2L) {
            return("Object")
        }
        
        return(paste(types, collapse = " / "))
    }, character(1))
    return(paste(unique(title), collapse = " / "))
}

.comparisonLegendObjectTypes <- function(data, objects) {
    if (length(objects)) {
        return(vapply(objects, function(object) {
            if (inherits(object, c("TrialDesign", "TrialDesignCharacteristics"))) {
                return("Design")
            }
            if (inherits(object, "SimulationResults")) {
                return("Simulation")
            }
            if (inherits(object, "TrialDesignPlan")) {
                return("Calculation")
            }
            return("Object")
        }, character(1)))
    }
    types <- rep("Object", nrow(data))
    if ("objectType" %in% names(data)) {
        classes <- as.character(data$objectType)
        types[grepl("^TrialDesign", classes)] <- "Design"
        types[grepl("^TrialDesignPlan", classes)] <- "Calculation"
        types[grepl("^SimulationResults", classes)] <- "Simulation"
    }
    if ("calculationTarget" %in% names(data)) {
        targets <- as.character(data$calculationTarget)
        unknown <- types == "Object"
        types[unknown & targets %in% "Design"] <- "Design"
        types[unknown & targets %in% c("Sample size", "Power")] <- "Calculation"
        types[unknown & grepl("simulation", targets, ignore.case = TRUE)] <- "Simulation"
    }
    return(types)
}

.assertComparisonLegendPosition <- function(value, parameter) {
    if (is.null(value)) {
        return(invisible())
    }
    valid <- length(value) == 1L && is.null(dim(value)) && !anyNA(value)
    if (valid && is.numeric(value)) {
        valid <- !is.complex(value) && is.finite(value) && value == trunc(value) && value >= -1 && value <= 6
    } else if (valid) {
        valid <- is.character(value) && value %in% c("auto", "none", "left", "right", "bottom", "top")
    }
    if (!valid) {
        stopIllegalArgument("Invalid legend position for '", parameter, "'",
            functionName = "plotComparison", parameter = parameter, value = value,
            constraint = "NULL, auto, none, left, right, bottom, top, or an integer from -1 to 6",
            diagnosticId = "comparison.legend_position_invalid")
    }
}

.comparisonWrapLegendLabels <- function(labels) {
    if (is.expression(labels) || is.list(labels)) {
        return(labels)
    }
    result <- vapply(as.character(labels), function(label) {
        lines <- strwrap(label, width = 34L)
        # Also break long identifiers without whitespace; display wrapping must
        # not change the factor levels or the original category values.
        lines <- unlist(lapply(lines, function(line) {
            starts <- seq.int(1L, max(1L, nchar(line)), by = 34L)
            return(substring(line, starts, starts + 33L))
        }), use.names = FALSE)
        return(paste(lines, collapse = "\n"))
    }, character(1), USE.NAMES = FALSE)
    names(result) <- names(labels)
    return(result)
}

.comparisonLegendGuide <- function(p, aesthetics, position, plotSettings,
        horizontal = FALSE, columns = 1L, fontFactor = 1) {
    if (!length(aesthetics)) {
        return(p)
    }
    if (identical(position, "none") || identical(position, -1) || identical(position, -1L)) {
        guides <- stats::setNames(rep(list("none"), length(aesthetics)), aesthetics)
        return(p + do.call(ggplot2::guides, guides))
    }
    positionTheme <- ggplot2::theme()
    if (is.numeric(position) && position > 0) {
        positions <- list(c(0.05, 0.98), c(0.05, 0.5), c(0.05, 0.02),
            c(0.95, 0.98), c(0.95, 0.5), c(0.95, 0.02))
        justifications <- list(c(0, 1), c(0, 0.5), c(0, 0), c(1, 1), c(1, 0.5), c(1, 0))
        positionTheme <- ggplot2::theme(legend.position.inside = positions[[position]],
            legend.justification.inside = justifications[[position]])
        position <- "inside"
    } else if (identical(position, "auto")) {
        position <- "inside"
    } else if (is.numeric(position)) {
        p <- p + ggplot2::theme(aspect.ratio = 1)
        position <- "right"
    }
    guideTheme <- positionTheme + ggplot2::theme(
        legend.byrow = TRUE,
        legend.title.position = if (horizontal) {
            "left"
        } else {
            "top"
        },
        legend.text = ggplot2::element_text(size = plotSettings$scaleSize(plotSettings$legendFontSize) * fontFactor),
        legend.title = if (inherits(p$theme$legend.title, "element_blank")) {
            ggplot2::element_blank()
        } else {
            ggplot2::element_text(size = plotSettings$scaleSize(plotSettings$legendFontSize + 1) * fontFactor)
        })
    guides <- stats::setNames(lapply(aesthetics, function(aesthetic) {
        return(ggplot2::guide_legend(position = position, direction = if (horizontal) {
            "horizontal"
        } else {
            "vertical"
        }, ncol = if (horizontal) {
            columns
        } else {
            1L
        }, theme = guideTheme, order = if (aesthetic %in% c("linetype", "shape")) {
            2L
        } else {
            1L
        }))
    }), aesthetics)
    return(p + do.call(ggplot2::guides, guides))
}

.measureComparisonLegends <- function(p) {
    built <- ggplot2::ggplot_build(p)
    table <- suppressMessages(suppressWarnings(ggplot2::ggplot_gtable(built)))
    boxes <- which(grepl("^guide-box", table$layout$name))
    boxes <- boxes[!vapply(table$grobs[boxes], inherits, logical(1), "zeroGrob")]
    sizes <- lapply(boxes, function(index) {
        box <- table$grobs[[index]]
        return(c(width = grid::convertWidth(grid::grobWidth(box), "in", valueOnly = TRUE),
            height = grid::convertHeight(grid::grobHeight(box), "in", valueOnly = TRUE)))
    })
    names(sizes) <- sub("guide-box-", "", table$layout$name[boxes], fixed = TRUE)
    return(list(built = built, sizes = sizes,
        panelWidth = grDevices::dev.size("in")[1L] - grid::convertWidth(sum(table$widths), "in", valueOnly = TRUE),
        panelHeight = grDevices::dev.size("in")[2L] - grid::convertHeight(sum(table$heights), "in", valueOnly = TRUE),
        deviceHeight = grDevices::dev.size("in")[2L]))
}

.fitComparisonBottomLegend <- function(p, aesthetics, plotSettings, fontFactor) {
    initial <- .measureComparisonLegends(p)
    count <- max(1L, vapply(aesthetics, function(aesthetic) {
        scale <- initial$built$plot$scales$get_scales(aesthetic)
        if (is.null(scale)) {
            return(0L)
        }
        return(length(scale$get_breaks()))
    }, integer(1)))
    lower <- 1L
    upper <- min(count, 12L)
    best <- list(plot = p, measurement = initial)
    while (lower <= upper) {
        columns <- floor((lower + upper) / 2)
        candidate <- .comparisonLegendGuide(p, aesthetics, "bottom", plotSettings,
            horizontal = TRUE, columns = columns, fontFactor = fontFactor)
        measurement <- .measureComparisonLegends(candidate)
        if (is.null(measurement$sizes$bottom) ||
                measurement$sizes$bottom[["width"]] <= measurement$panelWidth * 0.98) {
            best <- list(plot = candidate, measurement = measurement)
            lower <- columns + 1L
        } else {
            upper <- columns - 1L
        }
    }
    return(best)
}

.comparisonSplitLegendFits <- function(measurement) {
    inside <- measurement$sizes$inside
    bottom <- measurement$sizes$bottom
    if (is.null(inside) || is.null(bottom)) {
        return(FALSE)
    }
    # Reserve useful space for the panel rather than accepting any guide that
    # technically fits. Layout choices remain a device-dependent heuristic.
    return(measurement$panelWidth > 0 && measurement$panelHeight >= measurement$deviceHeight * 0.35 &&
        inside[["width"]] <= measurement$panelWidth * 0.75 &&
        inside[["height"]] <= measurement$panelHeight * 0.55 &&
        bottom[["width"]] <= measurement$panelWidth * 0.98 &&
        bottom[["height"]] <= measurement$deviceHeight * 0.45)
}

.layoutComparisonLegends <- function(p, plotSettings, legendPosition, categoryAesthetics,
        parameterAesthetics, categoryPosition, parameterPosition) {
    if (identical(p$theme$legend.position, "none")) {
        return(p)
    }
    separate <- length(categoryAesthetics) && length(parameterAesthetics)
    adaptive <- separate && identical(legendPosition, "auto") &&
        (is.null(categoryPosition) || identical(categoryPosition, "auto")) &&
        (is.null(parameterPosition) || identical(parameterPosition, "auto"))
    if (!adaptive && is.null(categoryPosition) && is.null(parameterPosition)) {
        return(.finalizePlotLegend(p, legendPosition, plotSettings))
    }
    if (grDevices::dev.cur() == 1L) {
        grDevices::pdf(file = NULL, width = 7, height = 7)
        on.exit(grDevices::dev.off(), add = TRUE)
    }
    for (legendScale in p$scales$scales) {
        if (legendScale$is_discrete() && any(legendScale$aesthetics %in% c("colour", "fill", "linetype", "shape"))) {
            legendScale$labels <- .comparisonWrapLegendLabels
        }
    }
    p <- p + ggplot2::theme(legend.position = "inside", legend.justification.bottom = "center")
    if (adaptive) {
        for (fontFactor in c(1, 0.95, 0.9)) {
            first <- .comparisonLegendGuide(p, parameterAesthetics, "inside", plotSettings, fontFactor = fontFactor)
            first <- .comparisonLegendGuide(first, categoryAesthetics, "bottom", plotSettings,
                horizontal = TRUE, fontFactor = fontFactor)
            first <- .fitComparisonBottomLegend(first, categoryAesthetics, plotSettings, fontFactor)
            firstFits <- .comparisonSplitLegendFits(first$measurement)
            if (firstFits) {
                first$plot <- .positionPlotLegend(first$plot, plotSettings)
                firstScore <- .comparisonInsideLegendScore(first$plot, first$measurement)
                if (firstScore <= sqrt(.Machine$double.eps)) {
                    return(first$plot)
                }
            }
            second <- .comparisonLegendGuide(p, categoryAesthetics, "inside", plotSettings, fontFactor = fontFactor)
            second <- .comparisonLegendGuide(second, parameterAesthetics, "bottom", plotSettings, fontFactor = fontFactor)
            secondMeasurement <- .measureComparisonLegends(second)
            if (.comparisonSplitLegendFits(secondMeasurement)) {
                second <- .positionPlotLegend(second, plotSettings)
                if (!firstFits || .comparisonInsideLegendScore(second, secondMeasurement) + sqrt(.Machine$double.eps) < firstScore) {
                    return(second)
                }
            }
            if (firstFits) {
                return(first$plot)
            }
        }
        # Neither inside layout fits: retain readable text and move both guides
        # outside. Extremely large legends still require a larger graphics device.
        fallback <- .comparisonLegendGuide(first$plot, parameterAesthetics, "bottom", plotSettings, fontFactor = 0.9)
        return(.fitComparisonBottomLegend(fallback, categoryAesthetics, plotSettings, 0.9)$plot)
    }
    inheritedPosition <- if (is.na(legendPosition)) {
        "auto"
    } else {
        legendPosition
    }
    if (is.null(categoryPosition)) {
        categoryPosition <- inheritedPosition
    }
    if (is.null(parameterPosition)) {
        parameterPosition <- inheritedPosition
    }
    isInside <- function(position) {
        return(is.numeric(position) && !is.na(position) && position > 0)
    }
    automaticInside <- FALSE
    if (identical(categoryPosition, "auto")) {
        categoryPosition <- if (isInside(parameterPosition) || identical(parameterPosition, "auto")) {
            "bottom"
        } else {
            automaticInside <- TRUE
            "inside"
        }
    }
    if (identical(parameterPosition, "auto")) {
        parameterPosition <- if (isInside(categoryPosition)) {
            "bottom"
        } else {
            automaticInside <- TRUE
            "inside"
        }
    }
    p <- .comparisonLegendGuide(p, categoryAesthetics, categoryPosition, plotSettings,
        horizontal = identical(categoryPosition, "bottom"))
    p <- .comparisonLegendGuide(p, parameterAesthetics, parameterPosition, plotSettings)
    if (identical(categoryPosition, "bottom")) {
        p <- .fitComparisonBottomLegend(p, categoryAesthetics, plotSettings, 1)$plot
    }
    if (automaticInside) {
        return(.positionPlotLegend(p, plotSettings))
    }
    return(p)
}

.comparisonInsideLegendScore <- function(p, measurement) {
    width <- measurement$sizes$inside[["width"]] / measurement$panelWidth
    height <- measurement$sizes$inside[["height"]] / measurement$panelHeight
    lower <- p$theme$legend.position.inside - c(width, height) * p$theme$legend.justification
    rectangle <- c(lower[1L] - 0.01, lower[1L] + width + 0.01,
        lower[2L] - 0.01, lower[2L] + height + 0.01)
    built <- measurement$built
    score <- 0
    for (layer in seq_along(built$data)) {
        if (!nrow(built$data[[layer]])) {
            next
        }
        data <- built$layout$coord$transform(built$data[[layer]], built$layout$panel_params[[1L]])
        score <- score + .plotLegendLayerOverlap(data, p$layers[[layer]]$geom, rectangle)
    }
    return(score)
}
