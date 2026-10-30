## |
## |  *Time functions for survival two-group simulations*
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

#' @include f_core_constants.R
#' @include f_design_general_utilities.R
NULL

#' @title
#' Get Piecewise Survival Time
#'
#' @description
#' Returns a \code{PiecewiseSurvivalTime} object that contains the all relevant parameters
#' of an exponential survival time cumulative distribution function.
#' Use \code{\link[base]{names}} to obtain the field names.
#'
#' @param piecewiseSurvivalTime A vector that specifies the time intervals for the piecewise
#'        definition of the exponential survival time cumulative distribution function (see details).
#' @inheritParams param_lambda1
#' @inheritParams param_lambda2
#' @inheritParams param_median1
#' @inheritParams param_median2
#' @inheritParams param_pi1_survival
#' @inheritParams param_pi2_survival
#' @inheritParams param_hazardRatio
#' @inheritParams param_eventTime
#' @inheritParams param_kappa
#' @param delayedResponseAllowed If \code{TRUE}, delayed response is allowed;
#'        otherwise it will be validated that the response is not delayed, default is \code{FALSE}.
#' @inheritParams param_three_dots
#'
#' @template details_piecewise_survival
#'
#' @return Returns a \code{\link{PiecewiseSurvivalTime}} object.
#' The following generics (R generic functions) are available for this result object:
#' \itemize{
#'   \item \code{\link[=names.FieldSet]{names()}} to obtain the field names,
#'   \item \code{\link[=print.FieldSet]{print()}} to print the object,
#'   \item \code{\link[=summary.ParameterSet]{summary()}} to display a summary of the object,
#'   \item \code{\link[=plot.ParameterSet]{plot()}} to plot the object,
#'   \item \code{\link[=as.data.frame.ParameterSet]{as.data.frame()}} to coerce the object to a \code{\link[base]{data.frame}},
#'   \item \code{\link[=as.matrix.FieldSet]{as.matrix()}} to coerce the object to a \code{\link[base]{matrix}}.
#' }
#' @template how_to_get_help_for_generics
#'
#' @template examples_get_piecewise_survival_time
#'
#' @export
#'
getPiecewiseSurvivalTime <- function(
        piecewiseSurvivalTime = NA_real_,
        ...,
        lambda1 = NA_real_,
        lambda2 = NA_real_,
        hazardRatio = NA_real_,
        pi1 = NA_real_,
        pi2 = NA_real_,
        median1 = NA_real_,
        median2 = NA_real_,
        eventTime = 12, # C_EVENT_TIME_DEFAULT
        kappa = 1,
        delayedResponseAllowed = FALSE) {
    .warnInCaseOfUnknownArguments(
        functionName = "getPiecewiseSurvivalTime", ...,
        ignore = c(".pi1Default", ".lambdaBased", ".silent"), exceptionEnabled = TRUE
    )

    if (inherits(piecewiseSurvivalTime, "TrialDesignPlanSurvival")) {
        piecewiseSurvivalTime <- piecewiseSurvivalTime$.piecewiseSurvivalTime
    }

    if (inherits(piecewiseSurvivalTime, "PiecewiseSurvivalTime")) {
        lambdaBased <- .getOptionalArgument(".lambdaBased", ...)
        if (!is.null(lambdaBased) && isTRUE(lambdaBased) &&
                !piecewiseSurvivalTime$.isLambdaBased()) {
            stopIllegalArgument("'piecewiseSurvivalTime' must be lambda or median based; ",
                "pi based defintion is not allowed",
                functionName = "getPiecewiseSurvivalTime",
                parameter = "piecewiseSurvivalTime", value = piecewiseSurvivalTime,
                diagnosticId = "survival.piecewise_probability_input_unsupported"
            )
        }

        .warnInCaseOfUnusedArgument(lambda1, "lambda1", NA_real_, "getPiecewiseSurvivalTime")
        .warnInCaseOfUnusedArgument(lambda2, "lambda2", NA_real_, "getPiecewiseSurvivalTime")
        .warnInCaseOfUnusedArgument(hazardRatio, "hazardRatio", NA_real_, "getPiecewiseSurvivalTime")
        .warnInCaseOfUnusedArgument(pi1, "pi1", NA_real_, "getPiecewiseSurvivalTime")
        .warnInCaseOfUnusedArgument(pi2, "pi2", NA_real_, "getPiecewiseSurvivalTime")
        .warnInCaseOfUnusedArgument(eventTime, "eventTime", C_EVENT_TIME_DEFAULT, "getPiecewiseSurvivalTime")
        .warnInCaseOfUnusedArgument(kappa, "kappa", 1, "getPiecewiseSurvivalTime")

        return(piecewiseSurvivalTime)
    }

    .assertIsValidLambda(lambda1, 1)
    .assertIsValidLambda(lambda2, 2)
    hazardRatio <- .assertIsNumericVector(hazardRatio, "hazardRatio", naAllowed = TRUE)
    pi1 <- .assertIsNumericVector(pi1, "pi1", naAllowed = TRUE)
    pi2 <- .assertIsSingleNumber(pi2, "pi2", naAllowed = TRUE)
    median1 <- .assertIsNumericVector(median1, "median1", naAllowed = TRUE)
    median2 <- .assertIsSingleNumber(median2, "median2", naAllowed = TRUE)
    eventTime <- .assertIsSingleNumber(eventTime, "eventTime", naAllowed = TRUE)
    .assertIsValidKappa(kappa)
    delayedResponseAllowed <- .assertIsSingleLogical(delayedResponseAllowed, "delayedResponseAllowed")

    return(PiecewiseSurvivalTime$new(
        piecewiseSurvivalTime = piecewiseSurvivalTime,
        lambda1 = lambda1,
        lambda2 = lambda2,
        hazardRatio = hazardRatio,
        pi1 = pi1,
        pi2 = pi2,
        median1 = median1,
        median2 = median2,
        eventTime = eventTime,
        kappa = kappa,
        delayedResponseAllowed = delayedResponseAllowed,
        ...
    ))
}
