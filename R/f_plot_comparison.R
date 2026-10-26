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

#' 
#' @title 
#' Plot Comparison Data
#'
#' @description 
#' Produces a ggplot object from unrounded comparison data.
#' 
#' @param data A non-empty data.frame, for example from getComparisonData().
#' @param xParamName,yParamName Names of the axis columns. The y column must be numeric.
#' @param ... Reserved for future extensions.
#' @param groupParamName,categoryParamName Optional column names for grouping and categories.
#' @param mainTitle,legendTitle Plot and legend titles.
#' @param plotLineEnabled,plotPointsEnabled Whether to show lines and points.
#' @param pointBorder Point border style, as used by rpact plots.
#' @param discreteXAxis Whether numeric x ticks should use the observed values only.
#' @param mirrorModeEnabled Whether to use fill categories without a legend.
#' @param legendPosition Legend position, either an rpact numeric position or a
#'   ggplot position name (none, left, right, bottom, top).
#' @param shinyOptimization Whether to use compact sizing for Shiny displays.
#' 
#' @return A ggplot object, which can be customized with ggplot2 layers.
#' 
#' @details Categories and groups jointly identify lines. Without either,
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
        xParamName,
        yParamName,
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
        shinyOptimization = FALSE) {
    
    .warnInCaseOfUnknownArguments(functionName = "plotComparison", ...)
    .assertGgplotIsInstalled()
    for (name in c("plotLineEnabled", "plotPointsEnabled", "discreteXAxis", "shinyOptimization", "mirrorModeEnabled")) {
        .assertIsSingleLogical(get(name), name)
    }
    if (!is.data.frame(data) || !nrow(data) || anyDuplicated(names(data))) stop("'data' must be a non-empty data.frame")
    selected <- list(xParamName = xParamName, yParamName = yParamName,
        categoryParamName = categoryParamName, groupParamName = groupParamName)
    for (name in names(selected)) {
        value <- selected[[name]]
        if (is.null(value) && name %in% c("categoryParamName", "groupParamName")) next
        if (!is.character(value) || length(value) != 1L || is.na(value) || !value %in% names(data)) {
            stop("Invalid column selection for '", name, "'")
        }
    }
    if (!is.numeric(data[[yParamName]])) stop("The y-axis column must be numeric")
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
    
    groups <- c(categoryParamName, groupParamName)
    if (!length(groups)) groups <- intersect(c("objectNumber", "designNumber"), names(data))[1L]
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
            if (!legendPosition %in% c("none", "left", "right", "bottom", "top")) stop("Invalid legend position")
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
