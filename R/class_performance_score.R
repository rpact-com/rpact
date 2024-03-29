## |
## |  *Performance score classes*
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
## |  File version: $Revision: 7620 $
## |  Last changed: $Date: 2024-02-09 12:57:37 +0100 (Fr, 09 Feb 2024) $
## |  Last changed by: $Author: pahlke $
## |

#'
#' @name PerformanceScore
#'
#' @title
#' Performance Score
#'
#' @description
#' Contains the conditional performance score, its sub-scores and components according to
#' Herrmann et al. (2020) for a given simulation result.
#'
#' @details
#' Use \link{getPerformanceScore} to calculate the performance score.
#'
#' @include f_core_constants.R
#' @include f_core_assertions.R
#' @include f_core_plot.R
#' @include class_core_parameter_set.R
#' @include class_simulation_results.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
PerformanceScore <- setRefClass("PerformanceScore",
    contains = "ParameterSet",
    fields = list(
        .simulationResults = "ANY",
        .plotSettings = "PlotSettings",
        .alternative = "numeric",
        locationSampleSize = "numeric",
        variationSampleSize = "numeric",
        subscoreSampleSize = "numeric",
        locationConditionalPower = "numeric",
        variationConditionalPower = "numeric",
        subscoreConditionalPower = "numeric",
        performanceScore = "numeric"
    ),
    methods = list(
        initialize = function(simulationResults, ...) {
            callSuper(.simulationResults = simulationResults, ...)

            .plotSettings <<- PlotSettings()
        },
        .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
            "Method for automatically printing performance score objects"
            .resetCat()
            if (!is.null(.simulationResults)) {
                .simulationResults$.show(
                    showType = showType,
                    digits = digits,
                    showStatistics = FALSE,
                    consoleOutputEnabled = consoleOutputEnabled,
                    performanceScore = .self
                )
            }
        },
        show = function(showType = 1, digits = NA_integer_) {
            .show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
        }
    )
)
