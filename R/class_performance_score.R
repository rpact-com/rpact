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
## |  File version: $Revision: 7019 $
## |  Last changed: $Date: 2023-05-31 07:23:47 +0200 (Mi, 31 Mai 2023) $
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
            .parameterNames <<- C_PARAMETER_NAMES
            .parameterFormatFunctions <<- C_PARAMETER_FORMAT_FUNCTIONS
        },
        show = function(showType = 1, digits = NA_integer_) {
            .show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
        },
        .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
            "Method for automatically printing performance score objects"
            .resetCat()
            .simulationResults$.show(showType = showType, digits = digits, 
                showStatistics = FALSE, consoleOutputEnabled = consoleOutputEnabled,
                performanceScore = .self)
        }
    )
)

