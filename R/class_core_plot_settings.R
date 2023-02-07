## |
## |  *Plot setting classes*
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
## |  File version: $Revision: 6801 $
## |  Last changed: $Date: 2023-02-06 15:29:57 +0100 (Mon, 06 Feb 2023) $
## |  Last changed by: $Author: pahlke $
## |

PlotSubTitleItem <- setRefClass("PlotSubTitleItem",
    fields = list(
        title = "character",
        subscript = "character",
        value = "numeric",
        digits = "integer"
    ),
    methods = list(
        initialize = function(..., title, value, subscript = NA_character_, digits = 3L) {
            callSuper(
                title = trimws(title), value = value,
                subscript = trimws(subscript), digits = digits, ...
            )
            value <<- round(value, digits)
        },
        show = function() {
            cat(toString(), "\n")
        },
        toQuote = function() {
            if (!is.null(subscript) && length(subscript) == 1 && !is.na(subscript)) {
                return(bquote(" " * .(title)[.(subscript)] == .(value)))
            }

            return(bquote(" " * .(title) == .(value)))
        },
        toString = function() {
            if (!is.null(subscript) && length(subscript) == 1 && !is.na(subscript)) {
                if (grepl("^(\\d+)|max|min$", subscript)) {
                    return(paste0(title, "_", subscript, " = ", value))
                }
                return(paste0(title, "(", trimws(subscript), ") = ", value))
            }

            return(paste(title, "=", value))
        }
    )
)

PlotSubTitleItems <- setRefClass("PlotSubTitleItems",
    fields = list(
        title = "character",
        subtitle = "character",
        items = "list"
    ),
    methods = list(
        initialize = function(...) {
            callSuper(...)
            items <<- list()
        },
        show = function() {
            cat(title, "\n")
            if (length(subtitle) == 1 && !is.na(subtitle)) {
                cat(subtitle, "\n")
            }
            s <- toString()
            if (length(s) == 1 && !is.na(s) && nchar(s) > 0) {
                cat(s, "\n")
            }
        },
        addItem = function(item) {
            items <<- c(items, item)
        },
        add = function(title, value, subscript = NA_character_, ..., digits = 3L) {
            titleTemp <- title
            if (length(items) == 0) {
                titleTemp <- .formatCamelCase(titleTemp, title = TRUE)
            }

            titleTemp <- paste0(" ", titleTemp)
            if (length(subscript) == 1 && !is.na(subscript)) {
                subscript <- paste0(as.character(subscript), " ")
            } else {
                titleTemp <- paste0(titleTemp, " ")
            }
            addItem(PlotSubTitleItem(title = titleTemp, value = value, subscript = subscript, digits = digits))
        },
        toString = function() {
            if (is.null(items) || length(items) == 0) {
                return(NA_character_)
            }

            s <- character(0)
            for (item in items) {
                s <- c(s, item$toString())
            }
            return(paste0(s, collapse = ", "))
        },
        toHtml = function() {
            htmlStr <- title
            if (length(subtitle) == 1 && !is.na(subtitle)) {
                htmlStr <- paste0(htmlStr, "<br><sup>", subtitle, "</sup>")
            }
            s <- toString()
            if (length(s) == 1 && !is.na(s) && nchar(s) > 0) {
                htmlStr <- paste0(htmlStr, "<br><sup>", s, "</sup>")
            }
            return(htmlStr)
        },
        toQuote = function() {
            quotedItems <- .getQuotedItems()
            if (is.null(quotedItems)) {
                if (length(subtitle) > 0) {
                    return(bquote(atop(
                        bold(.(title)),
                        atop(.(subtitle))
                    )))
                }

                return(title)
            }

            if (length(subtitle) > 0) {
                return(bquote(atop(
                    bold(.(title)),
                    atop(.(subtitle) * "," ~ .(quotedItems))
                )))
            }

            return(bquote(atop(
                bold(.(title)),
                atop(.(quotedItems))
            )))
        },
        .getQuotedItems = function() {
            item1 <- NULL
            item2 <- NULL
            item3 <- NULL
            item4 <- NULL
            if (length(items) > 0) {
                item1 <- items[[1]]
            }
            if (length(items) > 1) {
                item2 <- items[[2]]
            }
            if (length(items) > 2) {
                item3 <- items[[3]]
            }
            if (length(items) > 3) {
                item4 <- items[[4]]
            }

            if (!is.null(item1) && !is.null(item2) && !is.null(item3) && !is.null(item4)) {
                if (length(item1$subscript) == 1 && !is.na(item1$subscript) &&
                        length(item2$subscript) == 1 && !is.na(item2$subscript)) {
                    return(bquote(" " * .(item1$title)[.(item1$subscript)] == .(item1$value) * "," ~ .(item2$title)[.(item2$subscript)] == .(item2$value) * "," ~ .(item3$title) == .(item3$value) * "," ~ .(item4$title) == .(item4$value) * ""))
                }

                if (length(item1$subscript) == 1 && !is.na(item1$subscript)) {
                    return(bquote(" " * .(item1$title)[.(item1$subscript)] == .(item1$value) * "," ~ .(item2$title) == .(item2$value) * "," ~ .(item3$title) == .(item3$value) * "," ~ .(item4$title) == .(item4$value) * ""))
                }

                if (length(item2$subscript) == 1 && !is.na(item2$subscript)) {
                    return(bquote(" " * .(item1$title) == .(item1$value) * "," ~ .(item2$title)[.(item2$subscript)] == .(item2$value) * "," ~ .(item3$title) == .(item3$value) * "," ~ .(item4$title) == .(item4$value) * ""))
                }

                return(bquote(" " * .(item1$title) == .(item1$value) * "," ~ .(item2$title) == .(item2$value) * "," ~ .(item3$title) == .(item3$value) * "," ~ .(item4$title) == .(item4$value) * ""))
            }

            if (!is.null(item1) && !is.null(item2) && !is.null(item3)) {
                if (length(item1$subscript) == 1 && !is.na(item1$subscript) &&
                        length(item2$subscript) == 1 && !is.na(item2$subscript)) {
                    return(bquote(" " * .(item1$title)[.(item1$subscript)] == .(item1$value) * "," ~ .(item2$title)[.(item2$subscript)] == .(item2$value) * "," ~ .(item3$title) == .(item3$value) * ""))
                }

                if (length(item1$subscript) == 1 && !is.na(item1$subscript)) {
                    return(bquote(" " * .(item1$title)[.(item1$subscript)] == .(item1$value) * "," ~ .(item2$title) == .(item2$value) * "," ~ .(item3$title) == .(item3$value) * ""))
                }

                if (length(item2$subscript) == 1 && !is.na(item2$subscript)) {
                    return(bquote(" " * .(item1$title) == .(item1$value) * "," ~ .(item2$title)[.(item2$subscript)] == .(item2$value) * "," ~ .(item3$title) == .(item3$value) * ""))
                }

                return(bquote(" " * .(item1$title) == .(item1$value) * "," ~ .(item2$title) == .(item2$value) * "," ~ .(item3$title) == .(item3$value) * ""))
            }

            if (!is.null(item1) && !is.null(item2)) {
                if (length(item1$subscript) == 1 && !is.na(item1$subscript) &&
                        length(item2$subscript) == 1 && !is.na(item2$subscript)) {
                    return(bquote(" " * .(item1$title)[.(item1$subscript)] == .(item1$value) * "," ~ .(item2$title)[.(item2$subscript)] == .(item2$value) * ""))
                }

                if (length(item1$subscript) == 1 && !is.na(item1$subscript)) {
                    return(bquote(" " * .(item1$title)[.(item1$subscript)] == .(item1$value) * "," ~ .(item2$title) == .(item2$value) * ""))
                }

                if (length(item2$subscript) == 1 && !is.na(item2$subscript)) {
                    return(bquote(" " * .(item1$title) == .(item1$value) * "," ~ .(item2$title)[.(item2$subscript)] == .(item2$value) * ""))
                }

                return(bquote(" " * .(item1$title) == .(item1$value) * "," ~ .(item2$title) == .(item2$value) * ""))
            }

            if (!is.null(item1)) {
                if (length(item1$subscript) == 1 && !is.na(item1$subscript)) {
                    return(bquote(" " * .(item1$title)[.(item1$subscript)] == .(item1$value) * ""))
                }

                return(bquote(" " * .(item1$title) == .(item1$value) * ""))
            }

            return(NULL)
        }
    )
)

#'
#' @title
#' Get Plot Settings
#'
#' @description
#' Returns a plot settings object.
#'
#' @param lineSize The line size, default is \code{0.8}.
#' @param pointSize The point size, default is \code{3}.
#' @param pointColor The point color (character), default is \code{NA_character_}.
#' @param mainTitleFontSize The main title font size, default is \code{14}.
#' @param axesTextFontSize The axes text font size, default is \code{10}.
#' @param legendFontSize The legend font size, default is \code{11}.
#' @param scalingFactor The scaling factor, default is \code{1}.
#'
#' @details
#' Returns an object of class \code{PlotSettings} that collects typical plot settings.
#'
#' @export
#'
#' @keywords internal
#'
getPlotSettings <- function(lineSize = 0.8,
        pointSize = 3,
        pointColor = NA_character_,
        mainTitleFontSize = 14,
        axesTextFontSize = 10,
        legendFontSize = 11,
        scalingFactor = 1) {
    return(PlotSettings(
        lineSize = lineSize,
        pointSize = pointSize,
        pointColor = pointColor,
        mainTitleFontSize = mainTitleFontSize,
        axesTextFontSize = axesTextFontSize,
        legendFontSize = legendFontSize,
        scalingFactor = scalingFactor
    ))
}

#'
#' @name PlotSettings
#'
#' @title
#' Plot Settings
#'
#' @description
#' Class for plot settings.
#'
#' @field lineSize The line size.
#' @field pointSize The point size.
#' @field pointColor The point color, e.g., "red" or "blue".
#' @field mainTitleFontSize The main tile font size.
#' @field axesTextFontSize The text font size.
#' @field legendFontSize The legend font size.
#' @field scalingFactor The scaling factor.
#'
#' @details
#' Collects typical plot settings in an object.
#'
#' @keywords internal
#'
#' @include class_core_parameter_set.R
#'
#' @importFrom methods new
#'
PlotSettings <- setRefClass("PlotSettings",
    contains = "ParameterSet",
    fields = list(
        .legendLineBreakIndex = "numeric",
        .pointSize = "numeric",
        .legendFontSize = "numeric",
        .htmlTitle = "character",
        lineSize = "numeric",
        pointSize = "numeric",
        pointColor = "character",
        mainTitleFontSize = "numeric",
        axesTextFontSize = "numeric",
        legendFontSize = "numeric",
        scalingFactor = "numeric"
    ),
    methods = list(
        initialize = function(lineSize = 0.8,
                pointSize = 3,
                pointColor = NA_character_,
                mainTitleFontSize = 14,
                axesTextFontSize = 10,
                legendFontSize = 11,
                scalingFactor = 1,
                ...) {
            callSuper(
                lineSize = lineSize,
                pointSize = pointSize,
                pointColor = pointColor,
                mainTitleFontSize = mainTitleFontSize,
                axesTextFontSize = axesTextFontSize,
                legendFontSize = legendFontSize,
                scalingFactor = scalingFactor,
                ...
            )
            .legendLineBreakIndex <<- 15
            .pointSize <<- pointSize
            .legendFontSize <<- legendFontSize
            .htmlTitle <<- NA_character_

            .parameterNames <<- list(
                "lineSize" = "Line size",
                "pointSize" = "Point size",
                "pointColor" = "Point color",
                "mainTitleFontSize" = "Main title font size",
                "axesTextFontSize" = "Axes text font size",
                "legendFontSize" = "Legend font size",
                "scalingFactor" = "Scaling factor"
            )
        },
        clone = function() {
            return(PlotSettings(
                lineSize = .self$lineSize,
                pointSize = .self$pointSize,
                pointColor = .self$pointColor,
                mainTitleFontSize = .self$mainTitleFontSize,
                axesTextFontSize = .self$axesTextFontSize,
                legendFontSize = .self$legendFontSize,
                scalingFactor = .self$scalingFactor
            ))
        },
        show = function(showType = 1, digits = NA_integer_) {
            .show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
        },
        .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
            "Method for automatically printing plot setting objects"
            .resetCat()
            .showParametersOfOneGroup(
                parameters = .getVisibleFieldNames(),
                title = "Plot settings", orderByParameterName = FALSE,
                consoleOutputEnabled = consoleOutputEnabled
            )
        },
        setColorPalette = function(p, palette, mode = c("colour", "fill", "all")) {
            "Sets the color palette"

            mode <- match.arg(mode)

            # l = 45: make colors slightly darker
            if (is.null(palette) || is.na(palette)) {
                if (mode %in% c("colour", "all")) {
                    p <- p + ggplot2::scale_colour_hue(l = 45)
                }
                if (mode %in% c("fill", "all")) {
                    p <- p + ggplot2::scale_fill_hue(l = 45)
                }
            } else if (is.character(palette)) {
                if (mode %in% c("colour", "all")) {
                    p <- p + ggplot2::scale_colour_brewer(palette = palette)
                }
                if (mode %in% c("fill", "all")) {
                    p <- p + ggplot2::scale_fill_brewer(palette = palette)
                }
            } else if (palette == 0) {
                if (mode %in% c("colour", "all")) {
                    p <- p + ggplot2::scale_colour_grey()
                }
                if (mode %in% c("fill", "all")) {
                    p <- p + ggplot2::scale_fill_grey()
                }
            } else {
                if (mode %in% c("colour", "all")) {
                    p <- p + ggplot2::scale_colour_hue(l = 45)
                }
                if (mode %in% c("fill", "all")) {
                    p <- p + ggplot2::scale_fill_hue(l = 45)
                }
            }
            return(p)
        },
        enlargeAxisTicks = function(p) {
            "Enlarges the axis ticks"
            p <- p + ggplot2::theme(axis.ticks.length = ggplot2::unit(scaleSize(0.3), "cm"))
            return(p)
        },
        setAxesAppearance = function(p) {
            "Sets the font size and face of the axes titles and texts"
            p <- p + ggplot2::theme(axis.title.x = ggplot2::element_text(size = scaleSize(.self$axesTextFontSize + 1), face = "bold"))
            p <- p + ggplot2::theme(axis.title.y = ggplot2::element_text(size = scaleSize(.self$axesTextFontSize + 1), face = "bold"))
            p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(size = scaleSize(.self$axesTextFontSize)))
            p <- p + ggplot2::theme(axis.text.y = ggplot2::element_text(size = scaleSize(.self$axesTextFontSize)))
            return(p)
        },

        # Sets the axes labels
        setAxesLabels = function(p, xAxisLabel = NULL, yAxisLabel1 = NULL, yAxisLabel2 = NULL,
                xlab = NA_character_, ylab = NA_character_,
                scalingFactor1 = 1, scalingFactor2 = 1) {
            if (is.null(xAxisLabel) && !is.na(xlab)) {
                xAxisLabel <- xlab
            }

            plotLabsType <- getOption("rpact.plot.labs.type", "quote")
            if (plotLabsType == "quote" && !is.null(xAxisLabel)) {
                if (xAxisLabel == "Theta") {
                    xAxisLabel <- bquote(bold("Theta" ~ Theta))
                } else if (xAxisLabel == "pi1") {
                    xAxisLabel <- bquote(bold("pi"["1"]))
                } else if (xAxisLabel == "pi2") {
                    xAxisLabel <- bbquote(bold("pi"["2"]))
                } else if (xAxisLabel == "Theta") {
                    xAxisLabel <- bquote(bold("Theta" ~ Theta))
                }
            }

            p <- p + ggplot2::xlab(xAxisLabel)
            if (sum(is.na(ylab)) == 0) {
                yAxisLabel1 <- ylab[1]
                if (length(ylab) == 2) {
                    yAxisLabel2 <- ylab[2]
                }
            }
            p <- p + ggplot2::ylab(yAxisLabel1)

            p <- setSecondYAxisOnRightSide(p, yAxisLabel1, yAxisLabel2, scalingFactor1, scalingFactor2)

            return(p)
        },
        setSecondYAxisOnRightSide = function(p, yAxisLabel1, yAxisLabel2, scalingFactor1 = 1, scalingFactor2 = 1) {
            if (!is.null(yAxisLabel2) && scalingFactor1 != scalingFactor2) {
                p <- p + ggplot2::scale_y_continuous(yAxisLabel1,
                    sec.axis = ggplot2::sec_axis(~ . * scalingFactor1 / scalingFactor2, name = yAxisLabel2)
                )
            }
            return(p)
        },
        setLegendTitle = function(p, legendTitle, mode = c("colour", "fill")) {
            mode <- match.arg(mode)

            if (!is.null(legendTitle) && !is.na(legendTitle) && trimws(legendTitle) != "") {
                if (mode == "colour") {
                    p <- p + ggplot2::labs(colour = .getTextLineWithLineBreak(legendTitle,
                        lineBreakIndex = scaleSize(.legendLineBreakIndex)
                    ))
                } else {
                    p <- p + ggplot2::labs(fill = .getTextLineWithLineBreak(legendTitle,
                        lineBreakIndex = scaleSize(.legendLineBreakIndex)
                    ))
                }
                p <- p + ggplot2::theme(legend.title = ggplot2::element_text(
                    colour = "black", size = scaleSize(.self$legendFontSize + 1), face = "bold"
                ))
            } else {
                p <- p + ggplot2::theme(legend.title = ggplot2::element_blank())
                p <- p + ggplot2::labs(colour = NULL)
            }
            return(p)
        },
        setLegendLabelSize = function(p) {
            p <- p + ggplot2::theme(legend.text = ggplot2::element_text(size = scaleSize(.self$legendFontSize)))
            return(p)
        },
        setLegendPosition = function(p, legendPosition) {
            .assertIsValidLegendPosition(legendPosition)

            switch(as.character(legendPosition),
                "-1" = {
                    p <- p + ggplot2::theme(legend.position = "none")
                },
                "0" = {
                    p <- p + ggplot2::theme(aspect.ratio = 1)
                },
                "1" = {
                    p <- p + ggplot2::theme(legend.position = c(0.05, 1), legend.justification = c(0, 1))
                },
                "2" = {
                    p <- p + ggplot2::theme(legend.position = c(0.05, 0.5), legend.justification = c(0, 0.5))
                },
                "3" = {
                    p <- p + ggplot2::theme(legend.position = c(0.05, 0.05), legend.justification = c(0, 0))
                },
                "4" = {
                    p <- p + ggplot2::theme(legend.position = c(0.95, 1), legend.justification = c(1, 1))
                },
                "5" = {
                    p <- p + ggplot2::theme(legend.position = c(0.95, 0.5), legend.justification = c(1, 0.5))
                },
                "6" = {
                    p <- p + ggplot2::theme(legend.position = c(0.95, 0.05), legend.justification = c(1, 0))
                }
            )

            return(p)
        },
        setLegendBorder = function(p) {
            "Sets the legend border"
            p <- p + ggplot2::theme(
                legend.background =
                    ggplot2::element_rect(fill = "white", colour = "black", linewidth = scaleSize(0.4))
            )
            return(p)
        },
        adjustPointSize = function(adjustingValue) {
            .assertIsInClosedInterval(adjustingValue, "adjustingValue", lower = 0.1, upper = 2)
            pointSize <<- .self$.pointSize * adjustingValue
        },
        adjustLegendFontSize = function(adjustingValue) {
            "Adjusts the legend font size, e.g., run \\cr
			\\code{adjustLegendFontSize(-2)} # makes the font size 2 points smaller"
            .assertIsInClosedInterval(adjustingValue, "adjustingValue", lower = 0.1, upper = 2)
            legendFontSize <<- .self$.legendFontSize * adjustingValue
        },
        scaleSize = function(size, pointEnabled = FALSE) {
            if (pointEnabled) {
                return(size * .self$scalingFactor^2)
            }

            return(size * .self$scalingFactor)
        },
        setMainTitle = function(p, mainTitle, subtitle = NA_character_) {
            "Sets the main title"

            caption <- NA_character_
            if (!is.null(mainTitle) && inherits(mainTitle, "PlotSubTitleItems")) {
                plotLabsType <- getOption("rpact.plot.labs.type", "quote")
                if (plotLabsType == "quote") {
                    mainTitle <- mainTitle$toQuote()
                } else {
                    items <- mainTitle
                    mainTitle <- items$title
                    if (length(items$subtitle) == 1 && !is.na(items$subtitle)) {
                        if (length(subtitle) == 1 && !is.na(subtitle)) {
                            subtitle <- paste0(subtitle, ", ", items$subtitle)
                        } else {
                            subtitle <- items$subtitle
                        }
                    }
                    s <- items$toString()
                    if (length(s) == 1 && !is.na(s) && nchar(s) > 0) {
                        plotLabsCaptionEnabled <- as.logical(getOption("rpact.plot.labs.caption.enabled", "true"))
                        if (isTRUE(plotLabsCaptionEnabled)) {
                            caption <- s
                        } else {
                            if (length(subtitle) == 1 && !is.na(subtitle)) {
                                subtitle <- paste0(subtitle, ", ", s)
                            } else {
                                subtitle <- s
                            }
                        }
                    }

                    if (plotLabsType == "html") {
                        .htmlTitle <<- items$toHtml()
                    }
                }
            }

            subtitleFontSize <- NA_real_
            if (length(subtitle) == 1 && !is.na(subtitle)) {
                if (is.na(caption)) {
                    caption <- ggplot2::waiver()
                }
                p <- p + ggplot2::labs(title = mainTitle, subtitle = subtitle, caption = caption)
                targetWidth <- 130
                subtitleFontSize <- targetWidth / nchar(subtitle) * 8
                if (subtitleFontSize > scaleSize(.self$mainTitleFontSize) - 2) {
                    subtitleFontSize <- scaleSize(.self$mainTitleFontSize) - 2
                }
            } else if (length(caption) == 1 && !is.na(caption)) {
                p <- p + ggplot2::labs(title = mainTitle, caption = caption)
            } else {
                p <- p + ggplot2::ggtitle(mainTitle)
            }

            p <- p + ggplot2::theme(plot.title = ggplot2::element_text(
                hjust = 0.5, size = scaleSize(.self$mainTitleFontSize), face = "bold"
            ))

            if (!is.na(subtitleFontSize)) {
                p <- p + ggplot2::theme(
                    plot.subtitle = ggplot2::element_text(
                        hjust = 0.5,
                        size = scaleSize(subtitleFontSize)
                    )
                )
            }

            return(p)
        },
        setMarginAroundPlot = function(p, margin = 0.2) {
            "Sets the margin around the plot, e.g., run \\cr
			\\code{setMarginAroundPlot(p, .2)} or \\cr
			\\code{setMarginAroundPlot(p, c(.1, .2, .1, .2)}"
            if (length(margin == 1)) {
                margin <- base::rep(margin, 4)
            }
            if (!(length(margin) %in% c(1, 4))) {
                stop(
                    C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'margin' (", .arrayToString(margin),
                    ") must be a numeric vector with length 1 or 4"
                )
            }
            p <- p + ggplot2::theme(plot.margin = ggplot2::unit(margin, "cm"))
            return(p)
        },
        expandAxesRange = function(p, x = NA_real_, y = NA_real_) {
            "Expands the axes range"
            if (!is.na(x)) {
                p <- p + ggplot2::expand_limits(x = x)
            }
            if (!is.na(y)) {
                p <- p + ggplot2::expand_limits(y = y)
            }
            return(p)
        },
        hideGridLines = function(p) {
            "Hides the grid lines"
            p <- p + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
            p <- p + ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank())
            p <- p + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
            p <- p + ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank())
            return(p)
        },
        setTheme = function(p) {
            "Sets the theme"
            p <- p + ggplot2::theme_bw()
            p <- p + ggplot2::theme(
                panel.border = ggplot2::element_blank(),
                axis.line = ggplot2::element_line(colour = "black")
            )
            return(p)
        },
        plotPoints = function(p, pointBorder, ..., mapping = NULL) {

            # plot white border around the points
            if (pointBorder > 0) {
                p <- p + ggplot2::geom_point(
                    mapping = mapping,
                    color = "white", size = scaleSize(.self$pointSize, TRUE), alpha = 1,
                    shape = 21, stroke = pointBorder / 2.25, show.legend = FALSE
                )
            }

            if (!is.null(.self$pointColor) && length(.self$pointColor) == 1 && !is.na(.self$pointColor)) {
                p <- p + ggplot2::geom_point(
                    mapping = mapping,
                    color = .self$pointColor, size = scaleSize(.self$pointSize, TRUE), alpha = 1,
                    shape = 19, show.legend = FALSE
                )
            } else {
                p <- p + ggplot2::geom_point(
                    mapping = mapping,
                    size = scaleSize(.self$pointSize, TRUE), alpha = 1,
                    shape = 19, show.legend = FALSE
                )
            }
            return(p)
        },
        plotValues = function(p, ..., plotLineEnabled = TRUE,
                plotPointsEnabled = TRUE, pointBorder = 4) {
            if (plotLineEnabled) {
                p <- p + ggplot2::geom_line(size = scaleSize(.self$lineSize))
            }
            if (plotPointsEnabled) {
                p <- plotPoints(p, pointBorder)
            }
            return(p)
        },
        mirrorYValues = function(p, yValues, plotLineEnabled = TRUE,
                plotPointsEnabled = TRUE, pointBorder = 4) {
            if (plotLineEnabled) {
                p <- p + ggplot2::geom_line(ggplot2::aes(y = -yValues), size = scaleSize(.self$lineSize))
            }
            if (plotPointsEnabled) {
                p <- plotPoints(p, pointBorder, mapping = ggplot2::aes(y = -yValues))
            }
            return(p)
        },
        addCompanyAnnotation = function(p, enabled = TRUE) {
            if (!enabled) {
                return(p)
            }

            label <- "www.rpact.org"
            p <- p + ggplot2::annotate("label",
                x = -Inf, y = Inf, hjust = -0.1, vjust = 1,
                label = label, size = scaleSize(2.8), colour = "white", fill = "white"
            )

            p <- p + ggplot2::annotate("text",
                x = -Inf, y = Inf, label = label,
                hjust = -.12, vjust = 1, colour = "lightgray", size = scaleSize(2.7)
            )
            return(p)
        }
    )
)
