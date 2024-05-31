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
## |  File version: $Revision: 7958 $
## |  Last changed: $Date: 2024-05-30 09:56:27 +0200 (Do, 30 Mai 2024) $
## |  Last changed by: $Author: pahlke $
## |

PlotSubTitleItem <- R6::R6Class("PlotSubTitleItem",
    public = list(
        title = NULL,
        subscript = NULL,
        value = NULL,
        digits = NULL,
        initialize = function(..., title, value, subscript = NA_character_, digits = 3L) {
            self$title <- trimws(title)
            self$value <- value
            self$subscript <- trimws(subscript)
            self$digits <- digits

            self$value <- round(value, digits)
        },
        show = function() {
            cat(self$toString(), "\n")
        },
        toQuote = function() {
            if (!is.null(self$subscript) && length(self$subscript) == 1 && !is.na(self$subscript)) {
                return(bquote(" " * .(self$title)[.(self$subscript)] == .(self$value)))
            }

            return(bquote(" " * .(self$title) == .(self$value)))
        },
        toString = function() {
            if (!is.null(self$subscript) && length(self$subscript) == 1 && !is.na(self$subscript)) {
                if (grepl("^(\\d+)|max|min$", self$subscript)) {
                    return(paste0(self$title, "_", self$subscript, " = ", self$value))
                }
                return(paste0(self$title, "(", trimws(self$subscript), ") = ", self$value))
            }

            return(paste(self$title, "=", self$value))
        }
    )
)

PlotSubTitleItems <- R6::R6Class("PlotSubTitleItems",
    public = list(
        title = NULL,
        subtitle = NULL,
        items = NULL,
        initialize = function(..., title = NULL, subtitle = NULL) {
            self$title <- title
            self$subtitle <- subtitle

            self$items <- list()
        },
        show = function() {
            cat(self$title, "\n")
            if (length(self$subtitle) == 1 && !is.na(self$subtitle)) {
                cat(self$subtitle, "\n")
            }
            s <- self$toString()
            if (length(s) == 1 && !is.na(s) && nchar(s) > 0) {
                cat(s, "\n")
            }
        },
        addItem = function(item) {
            self$items <- c(self$items, item)
        },
        add = function(title, value, subscript = NA_character_, ..., digits = 3L, condition = TRUE) {
            if (isFALSE(condition)) {
                return(invisible())
            }

            titleTemp <- title
            if (length(self$items) == 0) {
                titleTemp <- .formatCamelCase(titleTemp, title = TRUE)
            }

            titleTemp <- paste0(" ", titleTemp)
            if (length(subscript) == 1 && !is.na(subscript)) {
                subscript <- paste0(as.character(subscript), " ")
            } else {
                titleTemp <- paste0(titleTemp, " ")
            }
            self$addItem(PlotSubTitleItem$new(title = titleTemp, value = value, subscript = subscript, digits = digits))
        },
        toString = function() {
            if (is.null(self$items) || length(self$items) == 0) {
                return(NA_character_)
            }

            s <- character()
            for (item in self$items) {
                s <- c(s, item$toString())
            }
            return(paste0(s, collapse = ", "))
        },
        toHtml = function() {
            htmlStr <- self$title
            if (length(self$subtitle) == 1 && !is.na(self$subtitle)) {
                htmlStr <- paste0(htmlStr, "<br><sup>", self$subtitle, "</sup>")
            }
            s <- self$toString()
            if (length(s) == 1 && !is.na(s) && nchar(s) > 0) {
                htmlStr <- paste0(htmlStr, "<br><sup>", s, "</sup>")
            }
            return(htmlStr)
        },
        toQuote = function() {
            quotedItems <- self$.getQuotedItems()
            if (is.null(quotedItems)) {
                if (length(self$subtitle) > 0) {
                    return(bquote(atop(
                        bold(.(self$title)),
                        atop(.(self$subtitle))
                    )))
                }

                return(self$title)
            }

            if (length(self$subtitle) > 0) {
                return(bquote(atop(
                    bold(.(self$title)),
                    atop(.(self$subtitle) * "," ~ .(quotedItems))
                )))
            }

            return(bquote(atop(
                bold(.(self$title)),
                atop(.(self$quotedItems))
            )))
        },
        .getQuotedItems = function() {
            item1 <- NULL
            item2 <- NULL
            item3 <- NULL
            item4 <- NULL
            if (length(self$items) > 0) {
                item1 <- self$items[[1]]
            }
            if (length(self$items) > 1) {
                item2 <- self$items[[2]]
            }
            if (length(self$items) > 2) {
                item3 <- self$items[[3]]
            }
            if (length(self$items) > 3) {
                item4 <- self$items[[4]]
            }

            if (!is.null(item1) && !is.null(item2) && !is.null(item3) && !is.null(item4)) {
                if (length(item1$subscript) == 1 && !is.na(item1$subscript) &&
                        length(item2$subscript) == 1 && !is.na(item2$subscript)) {
                    return(bquote(" " * .(item1$title)[.(item1$subscript)] == .(item1$value) *
                        "," ~ .(item2$title)[.(item2$subscript)] == .(item2$value) * "," ~
                        .(item3$title) == .(item3$value) * "," ~ .(item4$title) == .(item4$value) * ""))
                }

                if (length(item1$subscript) == 1 && !is.na(item1$subscript)) {
                    return(bquote(" " * .(item1$title)[.(item1$subscript)] == .(item1$value) *
                        "," ~ .(item2$title) == .(item2$value) * "," ~
                        .(item3$title) == .(item3$value) * "," ~ .(item4$title) == .(item4$value) * ""))
                }

                if (length(item2$subscript) == 1 && !is.na(item2$subscript)) {
                    return(bquote(" " * .(item1$title) == .(item1$value) * "," ~
                        .(item2$title)[.(item2$subscript)] == .(item2$value) *
                            "," ~ .(item3$title) == .(item3$value) * "," ~ .(item4$title) == .(item4$value) * ""))
                }

                return(bquote(" " * .(item1$title) == .(item1$value) * "," ~
                    .(item2$title) == .(item2$value) * "," ~ .(item3$title) == .(item3$value) *
                    "," ~ .(item4$title) == .(item4$value) * ""))
            }

            if (!is.null(item1) && !is.null(item2) && !is.null(item3)) {
                if (length(item1$subscript) == 1 && !is.na(item1$subscript) &&
                        length(item2$subscript) == 1 && !is.na(item2$subscript)) {
                    return(bquote(" " * .(item1$title)[.(item1$subscript)] == .(item1$value) *
                        "," ~ .(item2$title)[.(item2$subscript)] == .(item2$value) *
                        "," ~ .(item3$title) == .(item3$value) * ""))
                }

                if (length(item1$subscript) == 1 && !is.na(item1$subscript)) {
                    return(bquote(" " * .(item1$title)[.(item1$subscript)] == .(item1$value) *
                        "," ~ .(item2$title) == .(item2$value) * "," ~ .(item3$title) == .(item3$value) * ""))
                }

                if (length(item2$subscript) == 1 && !is.na(item2$subscript)) {
                    return(bquote(" " * .(item1$title) == .(item1$value) * "," ~
                        .(item2$title)[.(item2$subscript)] == .(item2$value) *
                            "," ~ .(item3$title) == .(item3$value) * ""))
                }

                return(bquote(" " * .(item1$title) == .(item1$value) * "," ~
                    .(item2$title) == .(item2$value) * "," ~ .(item3$title) == .(item3$value) * ""))
            }

            if (!is.null(item1) && !is.null(item2)) {
                if (length(item1$subscript) == 1 && !is.na(item1$subscript) &&
                        length(item2$subscript) == 1 && !is.na(item2$subscript)) {
                    return(bquote(" " * .(item1$title)[.(item1$subscript)] == .(item1$value) *
                        "," ~ .(item2$title)[.(item2$subscript)] == .(item2$value) * ""))
                }

                if (length(item1$subscript) == 1 && !is.na(item1$subscript)) {
                    return(bquote(" " * .(item1$title)[.(item1$subscript)] == .(item1$value) *
                        "," ~ .(item2$title) == .(item2$value) * ""))
                }

                if (length(item2$subscript) == 1 && !is.na(item2$subscript)) {
                    return(bquote(" " * .(item1$title) == .(item1$value) * "," ~
                        .(item2$title)[.(item2$subscript)] == .(item2$value) * ""))
                }

                return(bquote(" " * .(item1$title) == .(item1$value) * "," ~
                    .(item2$title) == .(item2$value) * ""))
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
    return(PlotSettings$new(
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
PlotSettings <- R6::R6Class("PlotSettings",
    inherit = ParameterSet,
    public = list(
        .legendLineBreakIndex = NULL,
        .pointSize = NULL,
        .legendFontSize = NULL,
        .htmlTitle = NULL,
        .scalingEnabled = NULL,
        .pointScalingCorrectionEnabled = NULL,
        .pointBorderEnabled = NULL,
        lineSize = NULL,
        pointSize = NULL,
        pointColor = NULL,
        mainTitleFontSize = NULL,
        axesTextFontSize = NULL,
        legendFontSize = NULL,
        scalingFactor = NULL,
        initialize = function(lineSize = 0.8,
                pointSize = 3,
                pointColor = NA_character_,
                mainTitleFontSize = 14,
                axesTextFontSize = 10,
                legendFontSize = 11,
                scalingFactor = 1,
                ...) {
            super$initialize(...)
            self$lineSize <- lineSize
            self$pointSize <- pointSize
            self$pointColor <- pointColor
            self$mainTitleFontSize <- mainTitleFontSize
            self$axesTextFontSize <- axesTextFontSize
            self$legendFontSize <- legendFontSize
            self$scalingFactor <- scalingFactor

            self$.legendLineBreakIndex <- 15
            self$.pointSize <- pointSize
            self$.legendFontSize <- legendFontSize
            self$.htmlTitle <- NA_character_
            self$.scalingEnabled <- TRUE
            self$.pointScalingCorrectionEnabled <- TRUE
            self$.pointBorderEnabled <- TRUE
        },
        show = function(showType = 1, digits = NA_integer_) {
            self$.show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
        },
        .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
            "Method for automatically printing plot setting objects"
            self$.resetCat()
            self$.showParametersOfOneGroup(
                parameters = self$.getVisibleFieldNames(),
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
            p <- p + ggplot2::theme(axis.ticks.length = ggplot2::unit(self$scaleSize(0.3), "cm"))
            return(p)
        },
        setAxesAppearance = function(p) {
            "Sets the font size and face of the axes titles and texts"
            p <- p + ggplot2::theme(axis.title.x = ggplot2::element_text(
                size = self$scaleSize(self$axesTextFontSize + 1), face = "bold"
            ))
            p <- p + ggplot2::theme(axis.title.y = ggplot2::element_text(
                size = self$scaleSize(self$axesTextFontSize + 1), face = "bold"
            ))
            p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(
                size = self$scaleSize(self$axesTextFontSize)
            ))
            p <- p + ggplot2::theme(axis.text.y = ggplot2::element_text(
                size = self$scaleSize(self$axesTextFontSize)
            ))
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

            p <- self$setSecondYAxisOnRightSide(p, yAxisLabel1, yAxisLabel2, scalingFactor1, scalingFactor2)

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
                        lineBreakIndex = self$scaleSize(self$.legendLineBreakIndex)
                    ))
                } else {
                    p <- p + ggplot2::labs(fill = .getTextLineWithLineBreak(legendTitle,
                        lineBreakIndex = self$scaleSize(self$.legendLineBreakIndex)
                    ))
                }
                p <- p + ggplot2::theme(legend.title = ggplot2::element_text(
                    colour = "black", size = self$scaleSize(self$legendFontSize + 1), face = "bold"
                ))
            } else {
                p <- p + ggplot2::theme(legend.title = ggplot2::element_blank())
                p <- p + ggplot2::labs(colour = NULL)
            }
            return(p)
        },
        setLegendLabelSize = function(p) {
            p <- p + ggplot2::theme(legend.text = ggplot2::element_text(size = self$scaleSize(self$legendFontSize)))
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
                    p <- p + ggplot2::theme(
                        legend.position = "inside",
                        legend.position.inside = c(0.05, 1), legend.justification = c(0, 1)
                    )
                },
                "2" = {
                    p <- p + ggplot2::theme(
                        legend.position = "inside",
                        legend.position.inside = c(0.05, 0.5), legend.justification = c(0, 0.5)
                    )
                },
                "3" = {
                    p <- p + ggplot2::theme(
                        legend.position = "inside",
                        legend.position.inside = c(0.05, 0.05), legend.justification = c(0, 0)
                    )
                },
                "4" = {
                    p <- p + ggplot2::theme(
                        legend.position = "inside",
                        legend.position.inside = c(0.95, 1), legend.justification = c(1, 1)
                    )
                },
                "5" = {
                    p <- p + ggplot2::theme(
                        legend.position = "inside",
                        legend.position.inside = c(0.95, 0.5), legend.justification = c(1, 0.5)
                    )
                },
                "6" = {
                    p <- p + ggplot2::theme(
                        legend.position = "inside",
                        legend.position.inside = c(0.95, 0.05), legend.justification = c(1, 0)
                    )
                }
            )

            return(p)
        },
        setLegendBorder = function(p) {
            "Sets the legend border"
            if (packageVersion("ggplot2") >= "3.4.0") {
                p <- p + ggplot2::theme(
                    legend.background =
                        ggplot2::element_rect(fill = "white", colour = "black", linewidth = self$scaleSize(0.4))
                )
            } else {
                p <- p + ggplot2::theme(
                    legend.background =
                        ggplot2::element_rect(fill = "white", colour = "black", size = self$scaleSize(0.4))
                )
            }
            return(p)
        },
        adjustPointSize = function(adjustingValue) {
            .assertIsInClosedInterval(adjustingValue, "adjustingValue", lower = 0.1, upper = 2)
            self$pointSize <- self$.pointSize * adjustingValue
        },
        adjustLegendFontSize = function(adjustingValue) {
            "Adjusts the legend font size, e.g., run \\cr
			\\code{adjustLegendFontSize(-2)} # makes the font size 2 points smaller"
            .assertIsInClosedInterval(adjustingValue, "adjustingValue", lower = 0.1, upper = 2)
            self$legendFontSize <- self$.legendFontSize * adjustingValue
        },
        scaleSize = function(size, pointEnabled = FALSE) {
            if (isFALSE(self$.scalingEnabled)) {
                return(size)
            }

            if (pointEnabled) {
                if (isFALSE(self$.pointScalingCorrectionEnabled)) {
                    return(size)
                }

                return(size * self$scalingFactor^2)
            }

            return(size * self$scalingFactor)
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
                        plotLabsCaptionEnabled <- as.logical(getOption(
                            "rpact.plot.labs.caption.enabled", "true"
                        ))
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
                        self$.htmlTitle <- items$toHtml()
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
                if (subtitleFontSize > self$scaleSize(self$mainTitleFontSize) - 2) {
                    subtitleFontSize <- self$scaleSize(self$mainTitleFontSize) - 2
                }
            } else if (length(caption) == 1 && !is.na(caption)) {
                p <- p + ggplot2::labs(title = mainTitle, caption = caption)
            } else {
                p <- p + ggplot2::ggtitle(mainTitle)
            }

            p <- p + ggplot2::theme(plot.title = ggplot2::element_text(
                hjust = 0.5, size = self$scaleSize(self$mainTitleFontSize), face = "bold"
            ))

            if (!is.na(subtitleFontSize)) {
                p <- p + ggplot2::theme(
                    plot.subtitle = ggplot2::element_text(
                        hjust = 0.5,
                        size = self$scaleSize(subtitleFontSize)
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
            if (pointBorder > 0 && self$.pointBorderEnabled) {
                p <- p + ggplot2::geom_point(
                    mapping = mapping,
                    color = "white",
                    size = self$scaleSize(self$pointSize, TRUE),
                    alpha = 1,
                    shape = 21,
                    stroke = pointBorder / 2.25,
                    show.legend = FALSE
                )
            }

            if (!is.null(self$pointColor) && length(self$pointColor) == 1 && !is.na(self$pointColor)) {
                p <- p + ggplot2::geom_point(
                    mapping = mapping,
                    color = self$pointColor,
                    size = self$scaleSize(self$pointSize, TRUE),
                    alpha = 1,
                    shape = 19,
                    show.legend = FALSE
                )
            } else {
                p <- p + ggplot2::geom_point(
                    mapping = mapping,
                    size = self$scaleSize(self$pointSize, TRUE), alpha = 1,
                    shape = 19, show.legend = FALSE
                )
            }
            return(p)
        },
        plotValues = function(p, ..., plotLineEnabled = TRUE,
                plotPointsEnabled = TRUE, pointBorder = 4) {
            if (plotLineEnabled) {
                if (packageVersion("ggplot2") >= "3.4.0") {
                    p <- p + ggplot2::geom_line(linewidth = self$scaleSize(self$lineSize))
                } else {
                    p <- p + ggplot2::geom_line(size = self$scaleSize(self$lineSize))
                }
            }
            if (plotPointsEnabled) {
                p <- self$plotPoints(p, pointBorder)
            }
            return(p)
        },
        mirrorYValues = function(p, yValues, plotLineEnabled = TRUE,
                plotPointsEnabled = TRUE, pointBorder = 4) {
            if (plotLineEnabled) {
                if (packageVersion("ggplot2") >= "3.4.0") {
                    p <- p + ggplot2::geom_line(ggplot2::aes(y = -yValues),
                        linewidth = self$scaleSize(self$lineSize)
                    )
                } else {
                    p <- p + ggplot2::geom_line(ggplot2::aes(y = -yValues),
                        size = self$scaleSize(self$lineSize)
                    )
                }
            }
            if (plotPointsEnabled) {
                p <- self$plotPoints(p, pointBorder, mapping = ggplot2::aes(y = -yValues))
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
                label = label, size = self$scaleSize(2.8), colour = "white", fill = "white"
            )

            p <- p + ggplot2::annotate("text",
                x = -Inf, y = Inf, label = label,
                hjust = -.12, vjust = 1, colour = "lightgray", size = self$scaleSize(2.7)
            )
            return(p)
        }
    )
)
