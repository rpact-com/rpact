## |
## |  *Time functions for piecewise exponential survival multi-group simulations*
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

#' @include class_time_piecewise_survival_multigroup.R
NULL

#'
#' @title
#' Piecewise Exponential Multi-Arm Survival Time
#'
#' @description
#' Creates a validated piecewise exponential survival specification for a
#' patient-wise multi-arm simulation.
#'
#' @param piecewiseSurvivalTime Numeric interval start times beginning with 0.
#' @param lambdaControl Positive control hazards, one per interval.
#'   Specify either `lambdaControl` or `medianControl`.
#' @param hazardRatios One active-arm-by-interval matrix, a named list of such
#'   matrices (one per situation), or an active-arm-by-interval-by-situation
#'   array. In the array, situation is the slowest-changing dimension.
#'   Specify either `hazardRatios` or `medianTreatments`.
#' @param medianControl Positive, finite exponential control medians, one per
#'   interval, as an alternative to `lambdaControl`. Values are not recycled.
#' @param medianTreatments Positive, finite exponential treatment medians, in the
#'   same matrix, list, or array format as `hazardRatios`, as an alternative to it.
#'
#' @return A \code{PiecewiseSurvivalTimeMultiArm} object.
#'
#' @details
#' At least two finite, strictly increasing interval starts are required. The last
#' interval is open-ended. Hazards must be finite and strictly positive. Group and
#' situation names must be unique and non-empty. Unnamed groups are positional;
#' named groups must agree in order. Interval columns are positional and receive
#' labels derived from the interval starts. Only piecewise exponential event times
#' are supported; simulation requires `kappa = 1`.
#' The returned time definition inherits from `ParameterSet` and supports `names()`,
#' `print()`, `summary()`, `as.data.frame()`, `as.matrix()`, and `getObjectRCode()`.
#' Hazard ratios specify the data-generating distribution, rather than separate
#' interval-specific analysis estimands.
#' Median inputs parameterize each interval's exponential hazard through
#' `getLambdaByMedian(median, kappa = 1)`. They are not the actual medians of the
#' complete piecewise survival distributions. The original interval medians are
#' retained as user-defined parameters; hazards and hazard ratios calculated from
#' them are marked as derived. No automatic recycling across intervals or groups
#' is performed.
#'
#' @examples
#' hazardRatios <- getPiecewiseHazardRatioArray(
#'     favorable = rbind(lowDose = c(0.9, 0.8), highDose = c(0.8, 0.6)),
#'     conservative = rbind(lowDose = c(1, 0.9), highDose = c(0.9, 0.75))
#' )
#' piecewiseTime <- getPiecewiseSurvivalTimeMultiArm(
#'     piecewiseSurvivalTime = c(0, 6),
#'     lambdaControl = c(0.08, 0.12),
#'     hazardRatios = hazardRatios
#' )
#' piecewiseTime
#'
#' getPiecewiseSurvivalTimeMultiArm(
#'     piecewiseSurvivalTime = c(0, 6),
#'     medianControl = c(12, 24),
#'     medianTreatments = rbind(lowDose = c(15, 30), highDose = c(18, 36))
#' )
#'
#' @export
#'
getPiecewiseSurvivalTimeMultiArm <- function(piecewiseSurvivalTime = NULL,
        lambdaControl = NULL, hazardRatios = NULL, medianControl = NULL, medianTreatments = NULL) {
    return(PiecewiseSurvivalTimeMultiArm$new(piecewiseSurvivalTime, lambdaControl, hazardRatios,
        medianControl = medianControl, medianTreatments = medianTreatments))
}

#'
#' @title
#' Piecewise Exponential Enrichment Survival Time
#'
#' @description
#' Creates a validated piecewise exponential survival specification for a
#' patient-wise population-enrichment simulation.
#'
#' @param piecewiseSurvivalTime Numeric interval start times beginning with 0.
#' @param lambdaControls Positive subset-by-interval control-hazard matrix.
#'   If both control hazards and hazard ratios have row names, their names and
#'   order must agree.
#'   Specify either `lambdaControls` or `medianControls`.
#' @param hazardRatios One subset-by-interval matrix, a named list of such
#'   matrices (one per situation), or a subset-by-interval-by-situation array.
#'   In the array, situation is the slowest-changing dimension.
#'   Specify either `hazardRatios` or `medianTreatments`.
#' @param medianControls Positive, finite exponential control medians in a
#'   subset-by-interval matrix, as an alternative to `lambdaControls`. Names and
#'   dimensions follow the same rules as for `lambdaControls`.
#' @param medianTreatments Positive, finite exponential treatment medians, in the
#'   same matrix, list, or array format as `hazardRatios`, as an alternative to it.
#'
#' @return A \code{PiecewiseSurvivalTimeEnrichment} object.
#'
#' @details
#' At least two finite, strictly increasing interval starts are required. The last
#' interval is open-ended. Hazards must be finite and strictly positive. Group and
#' situation names must be unique and non-empty. Unnamed groups are positional;
#' named groups must agree in order. Interval columns are positional and receive
#' labels derived from the interval starts. Only piecewise exponential event times
#' are supported; simulation requires `kappa = 1`.
#' The returned time definition inherits from `ParameterSet` and supports `names()`,
#' `print()`, `summary()`, `as.data.frame()`, `as.matrix()`, and `getObjectRCode()`.
#' Hazard ratios specify the data-generating distribution, rather than separate
#' interval-specific analysis estimands.
#' Median inputs parameterize each interval's exponential hazard through
#' `getLambdaByMedian(median, kappa = 1)`. They are not the actual medians of the
#' complete piecewise survival distributions. The original interval medians are
#' retained as user-defined parameters; hazards and hazard ratios calculated from
#' them are marked as derived. No automatic recycling across intervals or groups
#' is performed.
#'
#' @examples
#' piecewiseTime <- getPiecewiseSurvivalTimeEnrichment(
#'     piecewiseSurvivalTime = c(0, 6),
#'     lambdaControls = rbind(markerPositive = c(0.08, 0.12),
#'         markerNegative = c(0.05, 0.09)),
#'     hazardRatios = list(
#'         favorable = rbind(markerPositive = c(0.65, 0.55),
#'             markerNegative = c(0.9, 0.8)),
#'         conservative = rbind(markerPositive = c(0.8, 0.7),
#'             markerNegative = c(1, 0.9))
#'     )
#' )
#' as.data.frame(piecewiseTime)
#'
#' getPiecewiseSurvivalTimeEnrichment(
#'     piecewiseSurvivalTime = c(0, 6),
#'     medianControls = rbind(markerPositive = c(12, 24), markerNegative = c(18, 30)),
#'     medianTreatments = rbind(markerPositive = c(18, 36), markerNegative = c(20, 32))
#' )
#'
#' @export
#'
getPiecewiseSurvivalTimeEnrichment <- function(piecewiseSurvivalTime = NULL,
        lambdaControls = NULL, hazardRatios = NULL, medianControls = NULL, medianTreatments = NULL) {
    return(PiecewiseSurvivalTimeEnrichment$new(piecewiseSurvivalTime, lambdaControls, hazardRatios,
        medianControls = medianControls, medianTreatments = medianTreatments))
}
