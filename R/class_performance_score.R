## |
## |  rpact: Confirmatory Adaptive Clinical Trial Design and Analysis
## |  This file is part of the rpact R package.
## |
## |  RPACT — Statistical Tools for Drug Development
## |
## |  Authors: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
## |  SPDX-FileCopyrightText: RPACT GmbH, Germany <info@rpact.com>
## |  SPDX-License-Identifier: LGPL-3.0-only
## |
## |  Licensed under the GNU Lesser General Public License, version 3.
## |  License text: https://www.gnu.org/licenses/lgpl-3.0.html
## |
## |  Package documentation: https://www.rpact.org
## |  Resources and customer portal: https://connect.rpact.app
## |  Consulting and services: https://www.rpact.com
## |  Contact: info@rpact.com
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
