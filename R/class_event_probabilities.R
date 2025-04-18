## |
## |  *Event probabilities classes*
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
## |  File version: $Revision: 8624 $
## |  Last changed: $Date: 2025-03-21 13:24:59 +0100 (Fr, 21 Mrz 2025) $
## |  Last changed by: $Author: pahlke $
## |

#'
#' @name EventProbabilities
#'
#' @title
#' Event Probabilities
#'
#' @template field_time
#' @template field_accrualTime
#' @template field_accrualIntensity
#' @template field_kappa
#' @template field_piecewiseSurvivalTime
#' @template field_lambda1
#' @template field_lambda2
#' @template field_allocationRatioPlanned
#' @template field_hazardRatio
#' @template field_dropoutRate1
#' @template field_dropoutRate2
#' @template field_dropoutTime
#' @template field_maxNumberOfSubjects
#' @template field_overallEventProbabilities
#' @template field_cumulativeEventProbabilities
#' @template field_eventProbabilities1
#' @template field_eventProbabilities2
#'
#' @description
#' Class for the definition of event probabilities.
#'
#' @details
#' \code{EventProbabilities} is a class for the definition of event probabilities.
#'
#' @importFrom methods new
#'
#' @include f_core_constants.R
#' @include class_core_parameter_set.R
#' @include class_time.R
#'
#' @keywords internal
#'
EventProbabilities <- R6::R6Class("EventProbabilities",
    inherit = ParameterSet,
    public = list(
        .piecewiseSurvivalTime = NULL,
        .accrualTime = NULL,
        .plotSettings = NULL,
        time = NULL,
        accrualTime = NULL,
        accrualIntensity = NULL,
        kappa = NULL,
        piecewiseSurvivalTime = NULL,
        lambda1 = NULL,
        lambda2 = NULL,
        allocationRatioPlanned = NULL,
        hazardRatio = NULL,
        dropoutRate1 = NULL,
        dropoutRate2 = NULL,
        dropoutTime = NULL,
        maxNumberOfSubjects = NULL,
        overallEventProbabilities = NULL, # deprecated
        cumulativeEventProbabilities = NULL,
        eventProbabilities1 = NULL,
        eventProbabilities2 = NULL,
        initialize = function(..., .piecewiseSurvivalTime = NULL,
                .accrualTime = NULL,
                time = NULL,
                accrualTime = NULL,
                accrualIntensity = NULL,
                kappa = NULL,
                piecewiseSurvivalTime = NULL,
                lambda1 = NULL,
                lambda2 = NULL,
                allocationRatioPlanned = NULL,
                hazardRatio = NULL,
                dropoutRate1 = NULL,
                dropoutRate2 = NULL,
                dropoutTime = NULL,
                maxNumberOfSubjects = NULL) {
            self$.piecewiseSurvivalTime <- .piecewiseSurvivalTime
            self$.accrualTime <- .accrualTime
            self$time <- time
            self$accrualTime <- accrualTime
            self$accrualIntensity <- accrualIntensity
            self$kappa <- kappa
            self$piecewiseSurvivalTime <- piecewiseSurvivalTime
            self$lambda1 <- lambda1
            self$lambda2 <- lambda2
            self$allocationRatioPlanned <- allocationRatioPlanned
            self$hazardRatio <- hazardRatio
            self$dropoutRate1 <- dropoutRate1
            self$dropoutRate2 <- dropoutRate2
            self$dropoutTime <- dropoutTime
            self$maxNumberOfSubjects <- maxNumberOfSubjects

            super$initialize()

            self$.plotSettings <- PlotSettings$new()
            self$.setParameterType("overallEventProbabilities", C_PARAM_NOT_APPLICABLE) # deprecated
        },
        getPlotSettings = function() {
            return(self$.plotSettings)
        },
        show = function(showType = 1, digits = NA_integer_) {
            self$.show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
        },
        .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
            "Method for automatically printing event probabilities objects"
            self$.resetCat()
            if (showType == 2) {
                super$.show(showType = showType, digits = digits, consoleOutputEnabled = consoleOutputEnabled)
            } else {
                self$.cat("Event probabilities at given time:\n\n",
                    heading = 1,
                    consoleOutputEnabled = consoleOutputEnabled
                )

                self$.showParametersOfOneGroup(self$.getUserDefinedParameters(), "User defined parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showParametersOfOneGroup(self$.getDefaultParameters(), "Default parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showParametersOfOneGroup(self$.getGeneratedParameters(), "Time and output",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )

                self$.showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)

                self$.cat("Legend:\n", heading = 2, consoleOutputEnabled = consoleOutputEnabled)
                self$.cat("  (i): values of treatment arm i\n", consoleOutputEnabled = consoleOutputEnabled)

                self$.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
            }
        }
    )
)

#'
#' @name NumberOfSubjects
#'
#' @title
#' Number Of Subjects
#'
#' @description
#' Class for the definition of number of subjects results.
#'
#' @template field_time
#' @template field_accrualTime
#' @template field_accrualIntensity
#' @template field_maxNumberOfSubjects
#' @template field_numberOfSubjects
#'
#' @details
#' \code{NumberOfSubjects} is a class for the definition of number of subjects results.
#'
#' @importFrom methods new
#'
#' @include f_core_constants.R
#' @include class_core_parameter_set.R
#' @include class_time.R
#'
#' @keywords internal
#'
NumberOfSubjects <- R6::R6Class("NumberOfSubjects",
    inherit = ParameterSet,
    public = list(
        .accrualTime = NULL,
        .plotSettings = NULL,
        time = NULL,
        accrualTime = NULL,
        accrualIntensity = NULL,
        maxNumberOfSubjects = NULL,
        numberOfSubjects = NULL,
        initialize = function(
                ..., 
                accrualSetup = NULL,
                time = NULL,
                accrualTime = NULL,
                accrualIntensity = NULL,
                maxNumberOfSubjects = NULL,
                numberOfSubjects = NULL) {
            self$.accrualTime <- accrualSetup
            self$time <- time
            self$accrualTime <- accrualTime
            self$accrualIntensity <- accrualIntensity
            self$maxNumberOfSubjects <- maxNumberOfSubjects
            self$numberOfSubjects <- numberOfSubjects

            super$initialize()

            self$.plotSettings <- PlotSettings$new()
        },
        getPlotSettings = function() {
            return(self$.plotSettings)
        },
        show = function(showType = 1, digits = NA_integer_) {
            self$.show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
        },
        .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
            "Method for automatically printing number of subjects objects"
            self$.resetCat()
            if (showType == 2) {
                super$.show(showType = showType, digits = digits, consoleOutputEnabled = consoleOutputEnabled)
            } else {
                self$.cat("Number of recruited subjects at given time:\n\n",
                    heading = 1,
                    consoleOutputEnabled = consoleOutputEnabled
                )

                self$.showParametersOfOneGroup(self$.getUserDefinedParameters(), "User defined parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showParametersOfOneGroup(self$.getDefaultParameters(), "Default parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showParametersOfOneGroup(self$.getGeneratedParameters(), "Time and output",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )

                self$.showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)

                self$.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
            }
        }
    )
)

#'
#' @title
#' Event Probabilities Plotting
#'
#' @description
#' Plots an object that inherits from class \code{\link{EventProbabilities}}.
#'
#' @details
#' Generic function to plot an event probabilities object.
#'
#' @param x The object that inherits from \code{\link{EventProbabilities}}.
#' @param y An optional object that inherits from \code{\link{NumberOfSubjects}}.
#' @inheritParams param_allocationRatioPlanned
#' @param main The main title.
#' @param xlab The x-axis label.
#' @param ylab The y-axis label.
#' @param type The plot type (default = 1). Note that at the moment only one type is available.
#' @param legendTitle The legend title, default is \code{""}.
#' @inheritParams param_palette
#' @inheritParams param_plotPointsEnabled
#' @inheritParams param_showSource
#' @inheritParams param_plotSettings
#' @inheritParams param_legendPosition
#' @inheritParams param_three_dots_plot
#'
#' @details
#' Generic function to plot an event probabilities object.
#'
#' @template return_object_ggplot
#'
#' @export
#'
plot.EventProbabilities <- function(
        x, 
        y, 
        ...,
        allocationRatioPlanned = x$allocationRatioPlanned,
        main = NA_character_, 
        xlab = NA_character_, 
        ylab = NA_character_, 
        type = 1L,
        legendTitle = NA_character_, 
        palette = "Set1",
        plotPointsEnabled = NA, 
        legendPosition = NA_integer_, 
        showSource = FALSE,
        plotSettings = NULL) {
        
    markdown <- .getOptionalArgument("markdown", ..., optionalArgumentDefaultValue = NA)
    if (is.na(markdown)) {
        markdown <- .isMarkdownEnabled("plot")
    }
    
    args <- list(
        x = x, 
        y = NULL,
        allocationRatioPlanned = allocationRatioPlanned,
        main = main,
        xlab = xlab,
        ylab = ylab,
        type = type,
        legendTitle = legendTitle,
        palette = palette,
        plotPointsEnabled = plotPointsEnabled,
        legendPosition = legendPosition,
        showSource = showSource,
        plotSettings = plotSettings, 
        ...)
    
    if (markdown) {
        sep <- .getMarkdownPlotPrintSeparator()
        print(do.call(.plot.EventProbabilities, args))
        return(.knitPrintQueue(x, sep = sep, prefix = sep))
    }
    
    return(do.call(.plot.EventProbabilities, args))
}

.plot.EventProbabilities <- function(
        x, 
        y, 
        ...,
        allocationRatioPlanned = x$allocationRatioPlanned,
        main = NA_character_, 
        xlab = NA_character_, 
        ylab = NA_character_, 
        type = 1L,
        legendTitle = NA_character_, 
        palette = "Set1",
        plotPointsEnabled = NA, 
        legendPosition = NA_integer_, 
        showSource = FALSE,
        plotSettings = NULL) {
    fCall <- match.call(expand.dots = FALSE)
    xObjectName <- deparse(fCall$x)
    yObjectName <- NA_character_

    .assertGgplotIsInstalled()
    .assertIsValidLegendPosition(legendPosition)
    .assertIsValidAllocationRatioPlanned(allocationRatioPlanned, 2L)

    numberOfSubjectsObject <- NULL
    if (!missing(y) && inherits(y, "NumberOfSubjects")) {
        numberOfSubjectsObject <- y
        yObjectName <- deparse(fCall$y)
    }

    maxNumberOfSubjects <- 1
    maxNumberOfSubjects1 <- 1
    maxNumberOfSubjects2 <- 1

    maxNumberOfSubjectsToUse <- NA_integer_
    if (!is.null(numberOfSubjectsObject)) {
        maxNumberOfSubjectsToUse <- numberOfSubjectsObject$maxNumberOfSubjects
    }

    if (is.na(maxNumberOfSubjectsToUse)) {
        maxNumberOfSubjectsToUse <- x$maxNumberOfSubjects
    } else if (!is.na(x$maxNumberOfSubjects) &&
            x$maxNumberOfSubjects != maxNumberOfSubjectsToUse) {
        stop(C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS, 
            "'x' (EventProbabilities) and 'y' (NumberOfSubjects) ",
            "must have the same 'maxNumberOfSubjects' defined", 
            call. = FALSE
        )
    }

    if (!is.na(maxNumberOfSubjectsToUse)) {
        maxNumberOfSubjects <- maxNumberOfSubjectsToUse
        maxNumberOfSubjects1 <- .getNumberOfSubjects1(maxNumberOfSubjects, allocationRatioPlanned)
        maxNumberOfSubjects2 <- .getNumberOfSubjects2(maxNumberOfSubjects, allocationRatioPlanned)
    }

    if (is.na(maxNumberOfSubjectsToUse)) {
        mainDefault <- "Event Probabilities"
    } else {
        mainDefault <- ifelse(!is.null(numberOfSubjectsObject),
            "Number of subjects and expected number of events",
            "Expected number of events"
        )
    }
    main <- ifelse(is.na(main), mainDefault, main)
    if (!is.null(numberOfSubjectsObject)) {
        ylabDefault <- "Number of subjects/events"
    } else {
        ylabDefault <- ifelse(is.na(maxNumberOfSubjectsToUse),
            "Event probabilities", "Expected number of events"
        )
    }
    ylab <- ifelse(is.na(ylab), ylabDefault, ylab)
    data <- data.frame(
        xValues = c(x$time, x$time, x$time),
        yValues = c(
            x$cumulativeEventProbabilities * maxNumberOfSubjects, # cumulative
            x$eventProbabilities1 * maxNumberOfSubjects1, # treatment
            x$eventProbabilities2 * maxNumberOfSubjects2 # control
        ),
        categories = c(
            rep("Overall", length(x$time)),
            rep("Treatment", length(x$time)),
            rep("Control", length(x$time))
        )
    )
    data$categories <- factor(data$categories, levels = c("Overall", "Treatment", "Control"))

    if (!is.null(numberOfSubjectsObject)) {
        data <- rbind(
            data,
            data.frame(
                xValues = numberOfSubjectsObject$time,
                yValues = numberOfSubjectsObject$numberOfSubjects,
                categories = "Number of subjects"
            )
        )
    }

    if (is.na(legendPosition)) {
        legendPosition <- C_POSITION_LEFT_TOP
    }
    if (is.na(legendTitle)) {
        legendTitle <- ""
    }

    srcCmd <- .showPlotSourceInformation(
        objectName = xObjectName,
        xParameterName = "time",
        yParameterNames = c(
            "cumulativeEventProbabilities",
            "eventProbabilities1", "eventProbabilities2"
        ),
        type = type,
        showSource = showSource
    )
    if (!is.na(yObjectName)) {
        srcCmd2 <- .showPlotSourceInformation(
            objectName = yObjectName,
            xParameterName = "time",
            yParameterNames = "numberOfSubjects",
            type = type,
            showSource = showSource
        )
        if (is.list(srcCmd)) {
            if (!is.null(srcCmd2[["y"]])) {
                if (identical(x[["time"]], y[["time"]])) {
                    srcCmd$y <- c(srcCmd$y, srcCmd2$y)
                } else {
                    srcCmd$x2 <- srcCmd2[["x"]]
                    srcCmd$y2 <- srcCmd2$y
                }
            }
        } else {
            srcCmd <- c(srcCmd, srcCmd2)
        }
    }
    if (!is.null(srcCmd)) {
        if (.isSpecialPlotShowSourceArgument(showSource)) {
            return(invisible(srcCmd))
        }
        return(srcCmd)
    }

    if (is.null(plotSettings)) {
        plotSettings <- x$.plotSettings
    }

    return(.plotDataFrame(data,
        mainTitle = main,
        xlab = xlab, ylab = ylab, xAxisLabel = "Time",
        yAxisLabel1 = NA_character_, yAxisLabel2 = NA_character_,
        palette = palette, plotPointsEnabled = plotPointsEnabled,
        legendTitle = legendTitle,
        legendPosition = legendPosition, scalingFactor1 = 1, scalingFactor2 = 1,
        addPowerAndAverageSampleNumber = FALSE, mirrorModeEnabled = FALSE,
        ratioEnabled = FALSE, plotSettings = plotSettings, sided = 1, ...
    ))
}

#'
#' @title
#' Number Of Subjects Plotting
#'
#' @description
#' Plots an object that inherits from class \code{\link{NumberOfSubjects}}.
#'
#' @details
#' Generic function to plot an "number of subjects" object.
#'
#' @param x The object that inherits from \code{\link{NumberOfSubjects}}.
#' @param y An optional object that inherits from \code{\link{EventProbabilities}}.
#' @param allocationRatioPlanned The planned allocation ratio \code{n1 / n2} for a two treatment groups
#'   design, default is \code{1}. Will be ignored if \code{y} is undefined.
#' @param main The main title.
#' @param xlab The x-axis label.
#' @param ylab The y-axis label.
#' @param type The plot type (default = 1). Note that at the moment only one type is available.
#' @param legendTitle The legend title, default is \code{""}.
#' @inheritParams param_palette
#' @inheritParams param_plotPointsEnabled
#' @inheritParams param_showSource
#' @inheritParams param_plotSettings
#' @inheritParams param_legendPosition
#' @inheritParams param_three_dots_plot
#'
#' @details
#' Generic function to plot a "number of subjects" object.
#'
#' @template return_object_ggplot
#'
#' @export
#'
plot.NumberOfSubjects <- function(
        x, 
        y, 
        ...,
        allocationRatioPlanned = NA_real_,
        main = NA_character_, 
        xlab = NA_character_, 
        ylab = NA_character_, 
        type = 1L,
        legendTitle = NA_character_, 
        palette = "Set1",
        plotPointsEnabled = NA, 
        legendPosition = NA_integer_, 
        showSource = FALSE,
        plotSettings = NULL) {

    markdown <- .getOptionalArgument("markdown", ..., optionalArgumentDefaultValue = NA)
    if (is.na(markdown)) {
        markdown <- .isMarkdownEnabled("plot")
    }
    
    args <- list(
        x = x, 
        y = NULL,
        allocationRatioPlanned = allocationRatioPlanned,
        main = main,
        xlab = xlab,
        ylab = ylab,
        type = type,
        legendTitle = legendTitle,
        palette = palette,
        plotPointsEnabled = plotPointsEnabled,
        legendPosition = legendPosition,
        showSource = showSource,
        plotSettings = plotSettings, 
        ...)
    
    if (markdown) {
        sep <- .getMarkdownPlotPrintSeparator()
        print(do.call(.plot.NumberOfSubjects, args))
        return(.knitPrintQueue(x, sep = sep, prefix = sep))
    }
    
    return(do.call(.plot.NumberOfSubjects, args))    
}

.plot.NumberOfSubjects <- function(x, y, ...,
        allocationRatioPlanned = NA_real_,
        main = NA_character_, xlab = NA_character_, ylab = NA_character_, type = 1L,
        legendTitle = NA_character_, palette = "Set1",
        plotPointsEnabled = NA, legendPosition = NA_integer_, showSource = FALSE,
        plotSettings = NULL) {
    fCall <- match.call(expand.dots = FALSE)
    objectName <- deparse(fCall$x)

    if (!missing(y) && inherits(y, "EventProbabilities")) {
        return(plot.EventProbabilities(
            x = y, y = x,
            allocationRatioPlanned = ifelse(is.na(allocationRatioPlanned),
                y$allocationRatioPlanned, allocationRatioPlanned
            ),
            main = main, xlab = xlab, ylab = ylab, type = type,
            legendTitle = legendTitle, palette = palette,
            plotPointsEnabled = plotPointsEnabled, legendPosition = legendPosition,
            showSource = showSource, plotSettings = plotSettings, ...
        ))
    }

    if (!is.na(allocationRatioPlanned)) {
        warning("'allocationRatioPlanned' (", allocationRatioPlanned,
            ") will be ignored because 'y' is undefined ",
            "(for more information see ?plot.NumberOfSubjects)",
            call. = FALSE
        )
    }

    .assertGgplotIsInstalled()
    .assertIsValidLegendPosition(legendPosition)

    main <- ifelse(is.na(main), "Number of Subjects", main)
    ylab <- ifelse(is.na(ylab), "Number of subjects", ylab)
    data <- data.frame(
        xValues = x$time,
        yValues = x$numberOfSubjects,
        categories = "Number of subjects"
    )

    if (is.na(legendPosition)) {
        legendPosition <- -1
    }
    if (is.na(legendTitle)) {
        legendTitle <- ""
    }

    srcCmd <- .showPlotSourceInformation(
        objectName = objectName,
        xParameterName = "time",
        yParameterNames = "numberOfSubjects",
        type = type,
        showSource = showSource
    )
    if (!is.null(srcCmd)) {
        if (.isSpecialPlotShowSourceArgument(showSource)) {
            return(invisible(srcCmd))
        }
        return(srcCmd)
    }

    if (is.null(plotSettings)) {
        plotSettings <- x$.plotSettings
    }

    return(.plotDataFrame(data,
        mainTitle = main,
        xlab = xlab, ylab = ylab, xAxisLabel = "Time",
        yAxisLabel1 = NA_character_, yAxisLabel2 = NA_character_,
        palette = palette, plotPointsEnabled = plotPointsEnabled,
        legendTitle = legendTitle,
        legendPosition = legendPosition, scalingFactor1 = 1, scalingFactor2 = 1,
        addPowerAndAverageSampleNumber = FALSE, mirrorModeEnabled = FALSE,
        ratioEnabled = FALSE, plotSettings = plotSettings, sided = 1, ...
    ))
}
