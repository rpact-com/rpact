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
## |  File version: $Revision: 7742 $
## |  Last changed: $Date: 2024-03-22 13:46:29 +0100 (Fr, 22 Mrz 2024) $
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
PerformanceScore <- R6::R6Class("PerformanceScore",
    inherit = ParameterSet,
    public = list(
        .simulationResults = NULL,
        .plotSettings = NULL,
        .alternative = NULL,
        locationSampleSize = NULL,
        variationSampleSize = NULL,
        subscoreSampleSize = NULL,
        locationConditionalPower = NULL,
        variationConditionalPower = NULL,
        subscoreConditionalPower = NULL,
        performanceScore = NULL,
        initialize = function(simulationResults, ...) {
            super$initialize(...)
            self$.simulationResults <- simulationResults
            self$.plotSettings <- PlotSettings$new()
        },
        .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
            "Method for automatically printing performance score objects"
            self$.resetCat()
            if (!is.null(self$.simulationResults)) {
                self$.simulationResults$.show(
                    showType = showType,
                    digits = digits,
                    showStatistics = FALSE,
                    consoleOutputEnabled = consoleOutputEnabled,
                    performanceScore = self
                )
            }
        },
        show = function(showType = 1, digits = NA_integer_) {
            self$.show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
        }
    )
)
