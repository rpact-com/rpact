## |
## |  *Time classes*
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
## |  File version: $Revision: 7659 $
## |  Last changed: $Date: 2024-02-23 10:42:33 +0100 (Fr, 23 Feb 2024) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_design_general_utilities.R
NULL

C_REGEXP_GREATER_OR_EQUAL <- ">= ?"
C_REGEXP_SMALLER <- "< ?"
C_REGEXP_SMALLER_OR_EQUAL <- "<= ?"
C_REGEXP_DECIMAL_NUMBER <- "\\d*(\\.{1}\\d*)?"

TimeDefinition <- R6::R6Class("TimeDefinition",
    inherit = ParameterSet,
    public = list(
        initialize = function(...) {
            super$initialize()
        },
        .getRegexpFromTo = function(..., from, to, fromPrefix = "", toPrefix = "") {
            return(paste0("(^ *", fromPrefix, from, " *- *", toPrefix, to, " *$)"))
        },
        .getRegexpSmallerThan = function() {
            return(paste0("(^ *", C_REGEXP_SMALLER, C_REGEXP_DECIMAL_NUMBER, " *$)"))
        },
        .getRegexpDecimalNumber = function() {
            return(paste0("(^ *", C_REGEXP_DECIMAL_NUMBER, " *$)"))
        },
        .getRegexpGreaterOrEqualThan = function() {
            return(paste0("(^ *", C_REGEXP_GREATER_OR_EQUAL, C_REGEXP_DECIMAL_NUMBER, " *$)"))
        },
        .getRegexpDecimalRangeStart = function() {
            return(self$.getRegexpFromTo(from = "0", to = C_REGEXP_DECIMAL_NUMBER, toPrefix = C_REGEXP_SMALLER))
        },
        .getRegexpDecimalRange = function() {
            return(self$.getRegexpFromTo(
                from = C_REGEXP_DECIMAL_NUMBER, to = C_REGEXP_DECIMAL_NUMBER,
                toPrefix = C_REGEXP_SMALLER
            ))
        },
        .getRegexpDecimalRangeEnd = function() {
            return(self$.getRegexpFromTo(
                from = C_REGEXP_DECIMAL_NUMBER, to = "(Inf|x|\\?)",
                toPrefix = paste0("(", C_REGEXP_SMALLER, " *)?")
            ))
        },
        .getRegexpDecimalRangeFiniteEnd = function() {
            return(self$.getRegexpFromTo(
                from = C_REGEXP_DECIMAL_NUMBER, to = C_REGEXP_DECIMAL_NUMBER,
                toPrefix = "<=? ?"
            ))
        },
        .getRegexpOr = function(...) {
            args <- list(...)
            if (length(args) == 0) {
                return("")
            }

            if (length(args) == 1) {
                return(args[[1]])
            }

            return(paste(unlist(args, recursive = FALSE, use.names = FALSE), collapse = "|"))
        },
        .validateTimePeriod = function(timePeriod, i, n, accrualTimeMode = FALSE) {
            endOfAccrualIsUndefined <- FALSE
            if (i == 1 && (n > 1 || !accrualTimeMode)) {
                if (!grepl(self$.getRegexpOr(self$.getRegexpSmallerThan(), self$.getRegexpDecimalRangeStart()),
                        timePeriod,
                        perl = TRUE
                    )) {
                    if (!accrualTimeMode && n == 1 && !grepl("(0 *- ?)?<?\\?|x|\\.", timePeriod)) {
                        stop(
                            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                            "the name of the first region must have the format ",
                            "\"<time\" or \"0 - <time\", e.g., \"<5\" or \"0 - <5\""
                        )
                    }
                }
                if (grepl(self$.getRegexpSmallerThan(), timePeriod, perl = TRUE)) {
                    timePeriod <- sub("^ *< *", "0 - <", timePeriod)
                }
                if (!accrualTimeMode && n == 1 && !grepl("(0 *- ?)?<?\\?|x|\\.", timePeriod)) {
                    warning("Defined time period \"", timePeriod, "\" will be ignored ",
                        "because 'piecewiseSurvivalTime' list has only 1 entry",
                        call. = FALSE
                    )
                }
            } else if (i == n) {
                if (accrualTimeMode) {
                    if (!grepl(
                            self$.getRegexpOr(
                                self$.getRegexpDecimalNumber(),
                                self$.getRegexpGreaterOrEqualThan(), self$.getRegexpDecimalRangeEnd(),
                                self$.getRegexpDecimalRangeFiniteEnd()
                            ),
                            timePeriod,
                            perl = TRUE
                        )) {
                        stop(
                            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                            "the name of the last region must have the format \"time\", ",
                            "\">=time\", \"time - Inf\" or \"time1 - <=time2\", ",
                            "e.g., \"20\", \">=20\" or \"20 - Inf\" or \"20 - <=30\""
                        )
                    }
                    if (grepl(self$.getRegexpOr(self$.getRegexpGreaterOrEqualThan(), self$.getRegexpDecimalRangeEnd()),
                            timePeriod,
                            perl = TRUE
                        )) {
                        endOfAccrualIsUndefined <- TRUE
                    }
                    timePeriod <- gsub("([Inf >=\\?x]*)|-", "", timePeriod)
                } else {
                    if (!grepl(self$.getRegexpOr(self$.getRegexpGreaterOrEqualThan(), self$.getRegexpDecimalRangeEnd()),
                            timePeriod,
                            perl = TRUE
                        )) {
                        stop(
                            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                            "the name of the last region must have the format ",
                            "\">=time\" or \"time - Inf\", e.g., \">=20\" or \"20 - Inf\""
                        )
                    }
                }
            } else {
                if (!grepl(self$.getRegexpDecimalRange(), timePeriod, perl = TRUE)) {
                    stop(
                        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                        "the name of the inner regions must have the format \"time_1 - <time_2\", e.g., \"5 - <20\""
                    )
                }
            }

            if (accrualTimeMode) {
                return(list(timePeriod = timePeriod, endOfAccrualIsUndefined = endOfAccrualIsUndefined))
            }

            return(timePeriod)
        }
    )
)

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
getPiecewiseSurvivalTime <- function(piecewiseSurvivalTime = NA_real_,
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
        if (!is.null(lambdaBased) && isTRUE(lambdaBased) && !piecewiseSurvivalTime$.isLambdaBased()) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'piecewiseSurvivalTime' must be lambda or median based; ",
                "pi based defintion is not allowed"
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
    .assertIsNumericVector(hazardRatio, "hazardRatio", naAllowed = TRUE)
    .assertIsNumericVector(pi1, "pi1", naAllowed = TRUE)
    .assertIsSingleNumber(pi2, "pi2", naAllowed = TRUE)
    .assertIsNumericVector(median1, "median1", naAllowed = TRUE)
    .assertIsSingleNumber(median2, "median2", naAllowed = TRUE)
    .assertIsSingleNumber(eventTime, "eventTime", naAllowed = TRUE)
    .assertIsValidKappa(kappa)
    .assertIsSingleLogical(delayedResponseAllowed, "delayedResponseAllowed")

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

#' @title
#' Get Accrual Time
#'
#' @description
#' Returns an \code{AccrualTime} object that contains the accrual time and the accrual intensity.
#'
#' @inheritParams param_accrualTime
#' @inheritParams param_accrualIntensity
#' @inheritParams param_accrualIntensityType
#' @param maxNumberOfSubjects The maximum number of subjects.
#' @inheritParams param_three_dots
#'
#' @template details_piecewise_accrual
#'
#' @seealso \code{\link[=getNumberOfSubjects]{getNumberOfSubjects()}} for calculating the number of subjects at given time points.
#'
#' @return Returns an \code{\link{AccrualTime}} object.
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
#' @template examples_get_accrual_time
#'
#' @export
#'
getAccrualTime <- function(accrualTime = NA_real_,
        ...,
        accrualIntensity = NA_real_,
        accrualIntensityType = c("auto", "absolute", "relative"),
        maxNumberOfSubjects = NA_real_) {
    .warnInCaseOfUnknownArguments(
        functionName = "getAccrualTime", ...,
        ignore = c("showWarnings")
    )

    if (inherits(accrualTime, "AccrualTime") ||
            inherits(accrualTime, "TrialDesignPlanSurvival")) {
        if (!identical(accrualIntensity, C_ACCRUAL_INTENSITY_DEFAULT)) {
            .warnInCaseOfUnusedArgument(accrualIntensity, "accrualIntensity", NA_real_, "getAccrualTime")
        }
        .warnInCaseOfUnusedArgument(maxNumberOfSubjects, "maxNumberOfSubjects", NA_real_, "getAccrualTime")
    }

    if (inherits(accrualTime, "AccrualTime")) {
        return(accrualTime)
    }

    if (inherits(accrualTime, "TrialDesignPlanSurvival")) {
        return(accrualTime$.accrualTime)
    }

    accrualIntensityType <- match.arg(accrualIntensityType)

    .assertIsNumericVector(accrualIntensity, "accrualIntensity", naAllowed = TRUE)
    .assertIsValidMaxNumberOfSubjects(maxNumberOfSubjects, naAllowed = TRUE)
    .assertIsSingleCharacter(accrualIntensityType, "accrualIntensityType")
    absoluteAccrualIntensityEnabled <- NA
    if (accrualIntensityType == "absolute") {
        absoluteAccrualIntensityEnabled <- TRUE
        if (!all(is.na(accrualIntensity)) && any(na.omit(accrualIntensity) < 1)) {
            stop(
                C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                "'accrualIntensityType' is 'absolute' and the 'accrualIntensity' (",
                .arrayToString(accrualIntensity), ") therefore must be >= 1"
            )
        }
    } else if (accrualIntensityType == "relative") {
        absoluteAccrualIntensityEnabled <- FALSE
    }

    args <- list(...)
    showWarnings <- args[["showWarnings"]]
    if (is.null(showWarnings) || !is.logical(showWarnings)) {
        showWarnings <- TRUE
    }

    return(AccrualTime$new(
        accrualTime = accrualTime,
        accrualIntensity = accrualIntensity,
        maxNumberOfSubjects = maxNumberOfSubjects,
        showWarnings = showWarnings,
        absoluteAccrualIntensityEnabled = absoluteAccrualIntensityEnabled
    ))
}

#'
#' @name PiecewiseSurvivalTime
#'
#' @title
#' Piecewise Exponential Survival Time
#'
#' @description
#' Class for the definition of piecewise survival times.
#'
#' @template field_piecewiseSurvivalTime
#' @template field_lambda1
#' @template field_lambda2
#' @template field_hazardRatio
#' @template field_pi1_survival
#' @template field_pi2_survival
#' @template field_median1
#' @template field_median2
#' @template field_eventTime
#' @template field_kappa
#' @template field_piecewiseSurvivalEnabled
#' @template field_delayedResponseAllowed
#' @template field_delayedResponseEnabled
#'
#' @details
#' \code{PiecewiseSurvivalTime} is a class for the definition of piecewise survival times.
#'
#' @include f_core_constants.R
#' @include class_core_parameter_set.R
#' @include f_core_utilities.R
#' @include f_logger.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
PiecewiseSurvivalTime <- R6::R6Class("PiecewiseSurvivalTime",
    inherit = TimeDefinition,
    public = list(
        .pi1Default = NULL,
        .lambdaBased = NULL,
        .silent = NULL,
        piecewiseSurvivalTime = NULL,
        lambda1 = NULL,
        lambda2 = NULL,
        hazardRatio = NULL,
        pi1 = NULL,
        pi2 = NULL,
        median1 = NULL,
        median2 = NULL,
        eventTime = NULL,
        kappa = NULL,
        piecewiseSurvivalEnabled = NULL,
        delayedResponseAllowed = NULL,
        delayedResponseEnabled = NULL,
        initialize = function(piecewiseSurvivalTime = NA_real_,
                ...,
                lambda1 = NA_real_,
                lambda2 = NA_real_,
                hazardRatio = NA_real_,
                pi1 = NA_real_,
                pi2 = NA_real_,
                median1 = NA_real_,
                median2 = NA_real_,
                eventTime = C_EVENT_TIME_DEFAULT,
                kappa = 1,
                delayedResponseAllowed = FALSE) {
            super$initialize()
            self$piecewiseSurvivalTime <- piecewiseSurvivalTime
            self$lambda1 <- lambda1
            self$lambda2 <- lambda2
            self$hazardRatio <- hazardRatio
            self$pi1 <- pi1
            self$pi2 <- pi2
            self$median1 <- median1
            self$median2 <- median2
            self$eventTime <- eventTime
            self$kappa <- kappa
            self$delayedResponseAllowed <- delayedResponseAllowed

            if (length(self$piecewiseSurvivalTime) == 0) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'piecewiseSurvivalTime' must be defined (set to NA_real_ if not applicable)"
                )
            }

            self$.stopInCaseOfConflictingArguments(self$lambda1, "lambda1", self$median1, "median1")
            self$.stopInCaseOfConflictingArguments(self$lambda2, "lambda2", self$median2, "median2")

            self$.stopInCaseOfConflictingArguments(self$pi1, "pi1", self$median1, "median1")
            self$.stopInCaseOfConflictingArguments(self$pi1, "pi1", self$median2, "median2")
            self$.stopInCaseOfConflictingArguments(self$pi1, "pi1", self$lambda1, "lambda1")
            self$.stopInCaseOfConflictingArguments(self$pi1, "pi1", self$lambda2, "lambda2")
            self$.stopInCaseOfConflictingArguments(self$pi2, "pi2", self$median1, "median1")
            self$.stopInCaseOfConflictingArguments(self$pi2, "pi2", self$median2, "median2")
            self$.stopInCaseOfConflictingArguments(self$pi2, "pi2", self$lambda1, "lambda1")
            self$.stopInCaseOfConflictingArguments(self$pi2, "pi2", self$lambda2, "lambda2")

            if (length(self$median1) > 0 && !all(is.na(self$median1))) {
                self$lambda1 <- getLambdaByMedian(self$median1, kappa = self$kappa)
                self$.setParameterType("median1", C_PARAM_USER_DEFINED)
                self$.setParameterType("lambda1", C_PARAM_GENERATED)
            } else {
                self$.setParameterType("median1", C_PARAM_NOT_APPLICABLE)
                self$.setParameterType("lambda1", ifelse(length(self$lambda1) == 1 && is.na(self$lambda1),
                    C_PARAM_NOT_APPLICABLE, C_PARAM_USER_DEFINED
                ))
            }
            if (length(self$median2) > 0 && !all(is.na(self$median2))) {
                self$lambda2 <- getLambdaByMedian(self$median2, kappa = self$kappa)
                self$.setParameterType("median2", C_PARAM_USER_DEFINED)
                self$.setParameterType("lambda2", C_PARAM_GENERATED)
            } else {
                self$.setParameterType("median2", C_PARAM_NOT_APPLICABLE)
                self$.setParameterType("lambda2", C_PARAM_NOT_APPLICABLE)
            }

            args <- list(...)
            if (!is.null(args[[".pi1Default"]])) {
                self$.pi1Default <- args[[".pi1Default"]]
            }
            if (!is.null(args[[".lambdaBased"]])) {
                self$.lambdaBased <- args[[".lambdaBased"]]
            }
            if (!is.null(args[[".silent"]])) {
                self$.silent <- args[[".silent"]]
            } else {
                self$.silent <- FALSE
            }

            self$piecewiseSurvivalEnabled <- FALSE
            self$delayedResponseEnabled <- FALSE

            self$.setParameterType("piecewiseSurvivalTime", C_PARAM_NOT_APPLICABLE)
            self$.setParameterType("piecewiseSurvivalEnabled", C_PARAM_GENERATED)
            self$.setParameterType("delayedResponseEnabled", ifelse(isTRUE(self$delayedResponseAllowed),
                C_PARAM_GENERATED, C_PARAM_NOT_APPLICABLE
            ))
            self$.setParameterType("delayedResponseAllowed", ifelse(isTRUE(self$delayedResponseAllowed),
                C_PARAM_USER_DEFINED, C_PARAM_DEFAULT_VALUE
            ))
            self$.setParameterType("pi1", C_PARAM_NOT_APPLICABLE)
            self$.setParameterType("pi2", C_PARAM_NOT_APPLICABLE)
            self$.setParameterType("eventTime", ifelse(length(self$eventTime) == 1 && is.na(self$eventTime),
                C_PARAM_NOT_APPLICABLE,
                ifelse(self$eventTime == C_EVENT_TIME_DEFAULT, C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED)
            ))
            self$.setParameterType("kappa", ifelse(length(self$kappa) == 1 && !is.na(self$kappa) && self$kappa == 1,
                C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
            ))

            self$.init(self$piecewiseSurvivalTime)

            if (self$.getParameterType("median1") == C_PARAM_USER_DEFINED &&
                    self$.getParameterType("lambda1") == C_PARAM_USER_DEFINED) {
                self$.setParameterType("lambda1", C_PARAM_GENERATED)
            }

            if (self$.getParameterType("median2") == C_PARAM_USER_DEFINED &&
                    self$.getParameterType("lambda2") == C_PARAM_USER_DEFINED) {
                self$.setParameterType("lambda2", C_PARAM_GENERATED)
            }

            if (!is.na(self$eventTime) &&
                    self$.getParameterType("pi1") != C_PARAM_USER_DEFINED &&
                    self$.getParameterType("pi1") != C_PARAM_DEFAULT_VALUE &&
                    self$.getParameterType("pi2") != C_PARAM_USER_DEFINED &&
                    self$.getParameterType("pi2") != C_PARAM_DEFAULT_VALUE) {
                if (self$.getParameterType("eventTime") == C_PARAM_USER_DEFINED) {
                    warning("'eventTime' (", round(self$eventTime, 3), ") will be ignored", call. = FALSE)
                }
                self$.setParameterType("eventTime", C_PARAM_NOT_APPLICABLE)
                self$eventTime <- NA_real_
            }

            self$.validateCalculatedArguments()
        },
        .validateCalculatedArguments = function() {
            if (self$.getParameterType("median1") == C_PARAM_USER_DEFINED) {
                if (!isTRUE(all.equal(getLambdaByMedian(self$median1, kappa = self$kappa), self$lambda1, tolerance = 1e-05))) {
                    stop(
                        C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'lambda1' must be ",
                        round(getLambdaByMedian(self$median1, kappa = self$kappa), 5), ", but is ", round(self$lambda1, 5)
                    )
                }
                if (!any(is.na(self$pi1)) &&
                        !isTRUE(all.equal(getPiByMedian(self$median1, eventTime = self$eventTime, kappa = self$kappa),
                            self$pi1,
                            tolerance = 1e-05
                        ))) {
                    stop(
                        C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'pi1' must be ",
                        round(getPiByMedian(self$median1, eventTime = self$eventTime, kappa = self$kappa), 5), ", but is ", round(self$pi1, 5)
                    )
                }
            }

            if (self$.getParameterType("median2") == C_PARAM_USER_DEFINED) {
                if (!isTRUE(all.equal(getLambdaByMedian(self$median2, kappa = self$kappa), self$lambda2, tolerance = 1e-05))) {
                    stop(
                        C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'lambda2' must be ",
                        round(getLambdaByMedian(self$median2, kappa = self$kappa), 5), ", but is ", round(self$lambda2, 5)
                    )
                }
                if (!is.na(self$pi2) &&
                        !isTRUE(all.equal(getPiByMedian(self$median2, eventTime = self$eventTime, kappa = self$kappa),
                            self$pi2,
                            tolerance = 1e-05
                        ))) {
                    stop(
                        C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'pi2' must be ",
                        round(getPiByMedian(self$median2, eventTime = self$eventTime, kappa = self$kappa), 5), ", but is ", round(self$pi2, 5)
                    )
                }
            }

            if (self$.getParameterType("lambda1") == C_PARAM_USER_DEFINED ||
                    self$.getParameterType("median1") == C_PARAM_USER_DEFINED ||
                    self$.getParameterType("lambda2") == C_PARAM_USER_DEFINED ||
                    self$.getParameterType("median2") == C_PARAM_USER_DEFINED) {
                if (!any(is.na(self$pi1))) {
                    stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'pi1' (", self$pi1, ") must be NA_real_")
                }
                if (self$.getParameterType("pi1") != C_PARAM_NOT_APPLICABLE) {
                    stop(
                        C_EXCEPTION_TYPE_RUNTIME_ISSUE, "parameter type of 'pi1' (",
                        self$.getParameterType("pi1"), ") must be C_PARAM_NOT_APPLICABLE"
                    )
                }
                if (!any(is.na(self$pi1))) {
                    stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'pi2' (", self$pi2, ") must be NA_real_")
                }
                if (self$.getParameterType("pi2") != C_PARAM_NOT_APPLICABLE) {
                    stop(
                        C_EXCEPTION_TYPE_RUNTIME_ISSUE, "parameter type of 'pi2' (",
                        self$.getParameterType("pi2"), ") must be C_PARAM_NOT_APPLICABLE"
                    )
                }
                if (!any(is.na(self$eventTime))) {
                    stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'eventTime' (", self$eventTime, ") must be NA_real_")
                }
                if (self$.getParameterType("eventTime") != C_PARAM_NOT_APPLICABLE) {
                    stop(
                        C_EXCEPTION_TYPE_RUNTIME_ISSUE, "parameter type of 'eventTime' (",
                        self$.getParameterType("eventTime"), ") must be C_PARAM_NOT_APPLICABLE"
                    )
                }
            }

            if (self$.getParameterType("hazardRatio") == C_PARAM_TYPE_UNKNOWN) {
                stop(
                    C_EXCEPTION_TYPE_RUNTIME_ISSUE, "parameter type of 'hazardRatio' (",
                    self$hazardRatio, ") must be != C_PARAM_TYPE_UNKNOWN"
                )
            }
        },
        .stopInCaseOfConflictingArguments = function(arg1, argName1, arg2, argName2) {
            if (length(arg1) > 0 && !all(is.na(arg1)) && length(arg2) > 0 && !all(is.na(arg2))) {
                stop(
                    C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                    "it is not allowed to specify '", argName1, "' (", .arrayToString(arg1), ")",
                    " and '", argName2, "' (", .arrayToString(arg2), ") concurrently"
                )
            }
        },
        .asDataFrame = function() {
            data <- data.frame(
                piecewiseSurvivalTime = self$piecewiseSurvivalTime,
                lambda1 = self$lambda1,
                lambda2 = self$lambda2
            )
            rownames(data) <- as.character(1:nrow(data))
            colnames(data) <- c(
                "Start time",
                .getParameterCaption("lambda1", tableOutputEnabled = TRUE),
                .getParameterCaption("lambda2", tableOutputEnabled = TRUE)
            )
            return(data)
        },
        .isPiBased = function() {
            return(!self$.isLambdaBased())
        },
        .isLambdaBased = function(minNumberOfLambdas = 2) {
            if (self$.getParameterType("lambda2") == C_PARAM_USER_DEFINED ||
                    self$.getParameterType("median2") == C_PARAM_USER_DEFINED) {
                if (length(self$lambda2) >= minNumberOfLambdas && !any(is.na(self$lambda2))) {
                    return(TRUE)
                }
            }

            return((length(self$pi1) == 0 || any(is.na(self$pi1))) && (length(self$pi2) == 0 || any(is.na(self$pi2))))
        },
        show = function(showType = 1, digits = NA_integer_) {
            self$.show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
        },
        .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
            "Method for automatically printing piecewise survival time objects"
            self$.resetCat()
            if (showType == 2) {
                super$.show(showType = showType, digits = digits, consoleOutputEnabled = consoleOutputEnabled)
            } else {
                self$.cat("Piecewise exponential survival times:\n",
                    sep = "", heading = 1,
                    consoleOutputEnabled = consoleOutputEnabled
                )
                if (!self$piecewiseSurvivalEnabled) {
                    self$.cat("  Piecewise exponential survival is disabled.\n\n", consoleOutputEnabled = consoleOutputEnabled)
                } else if (length(self$piecewiseSurvivalTime) == 1) {
                    self$.cat("  At all times:", self$lambda2[1], "\n\n", consoleOutputEnabled = consoleOutputEnabled)
                } else {
                    piecewiseSurvivalTimeStr <- format(self$piecewiseSurvivalTime)
                    lambda2Str <- format(self$lambda2)
                    for (i in 1:length(self$piecewiseSurvivalTime)) {
                        if (i < length(self$piecewiseSurvivalTime)) {
                            self$.cat("  ", piecewiseSurvivalTimeStr[i], " - <",
                                piecewiseSurvivalTimeStr[i + 1], ": ",
                                lambda2Str[i], "\n",
                                sep = "",
                                consoleOutputEnabled = consoleOutputEnabled
                            )
                        } else {
                            self$.cat("  ", rep(" ", 2 + max(nchar(piecewiseSurvivalTimeStr))),
                                ">=", piecewiseSurvivalTimeStr[i], ": ",
                                lambda2Str[i], "\n",
                                sep = "",
                                consoleOutputEnabled = consoleOutputEnabled
                            )
                        }
                    }
                    if (self$delayedResponseEnabled) {
                        self$.cat("Delayed response is enabled.\n", consoleOutputEnabled = consoleOutputEnabled)
                    }
                    self$.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
                }

                self$.cat("Details:\n\n", sep = "", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
                self$.showParametersOfOneGroup(self$.getUserDefinedParameters(), "User defined parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showParametersOfOneGroup(self$.getDefaultParameters(), "Default parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showParametersOfOneGroup(self$.getGeneratedParameters(), "Generated parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)
            }
        },
        .toString = function(startWithUpperCase = FALSE) {
            s <- "piecewise survival time"
            return(ifelse(startWithUpperCase, .firstCharacterToUpperCase(s), s))
        },
        isDelayedResponseEnabled = function() {
            return(self$delayedResponseEnabled)
        },
        isPiecewiseSurvivalEnabled = function() {
            if (length(self$piecewiseSurvivalTime) == 0) {
                return(FALSE)
            }

            if (length(self$piecewiseSurvivalTime) == 1 && is.na(self$piecewiseSurvivalTime)) {
                return(FALSE)
            }

            return(TRUE)
        },
        .initFromList = function(pwSurvTimeList) {
            if (!is.list(pwSurvTimeList)) {
                stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'piecewiseSurvivalTime' must be a list")
            }

            if (length(pwSurvTimeList) == 0) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'piecewiseSurvivalTime' must contain at least one entry"
                )
            }

            if (is.null(names(pwSurvTimeList))) {
                stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'piecewiseSurvivalTime' must be a named list")
            }

            if (!all(is.na(self$lambda2))) {
                warning("'lambda2' (", .arrayToString(self$lambda2),
                    ") will be ignored because 'piecewiseSurvivalTime' is a list",
                    call. = FALSE
                )
            }

            pwSurvStartTimes <- c(0)
            pwSurvLambda2 <- c()
            pwSurvTimeNames <- names(pwSurvTimeList)
            for (i in 1:length(pwSurvTimeNames)) {
                timePeriod <- pwSurvTimeNames[i]
                lambdaValue <- pwSurvTimeList[[timePeriod]]
                .assertIsSingleNumber(lambdaValue, paste0("pwSurvLambda[", i, "]"))

                timePeriod <- self$.validateTimePeriod(timePeriod, i = i, n = length(pwSurvTimeNames))

                if (i < length(pwSurvTimeNames)) {
                    parts <- strsplit(timePeriod, "- *(< *)?", perl = TRUE)[[1]]
                    if (length(parts) != 2) {
                        stop(
                            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                            "all regions (", timePeriod, ") must have the format ",
                            "\"time_1 - <time_2\", e.g., \"3 - <6\""
                        )
                    }
                    intervalBoundary <- as.numeric(trimws(parts[2]))
                    pwSurvStartTimes <- c(pwSurvStartTimes, intervalBoundary)
                }
                pwSurvLambda2 <- c(pwSurvLambda2, lambdaValue)
            }

            self$piecewiseSurvivalTime <- pwSurvStartTimes
            self$.setParameterType("piecewiseSurvivalTime", C_PARAM_USER_DEFINED)
            if (length(self$hazardRatio) == 1 && !is.na(self$hazardRatio)) {
                self$lambda1 <- pwSurvLambda2 * self$hazardRatio^(1 / self$kappa)
                self$.setParameterType("lambda1", C_PARAM_GENERATED)
            } else if (length(self$hazardRatio) > 1 && self$delayedResponseAllowed) {
                if (length(self$hazardRatio) != length(pwSurvLambda2)) {
                    warning("Only the first 'hazardRatio' (", round(self$hazardRatio[1], 4),
                        ") was used for piecewise survival time definition ",
                        "(use a loop over the function to simulate different hazard ratios)",
                        call. = FALSE
                    )
                    self$hazardRatio <- self$hazardRatio[1]
                } else {
                    self$delayedResponseEnabled <- TRUE
                }
                self$lambda1 <- pwSurvLambda2 * self$hazardRatio^(1 / self$kappa)
                self$.setParameterType("lambda1", C_PARAM_GENERATED)
            } else {
                self$lambda1 <- NA_real_
                self$.setParameterType("lambda1", C_PARAM_NOT_APPLICABLE)
            }

            self$lambda2 <- pwSurvLambda2
            self$.setParameterType("lambda2", C_PARAM_USER_DEFINED)

            self$piecewiseSurvivalEnabled <- !identical(self$piecewiseSurvivalTime, 0)
        },
        .init = function(pwSurvTime) {
            .logDebug("pwSurvTime %s, %s", ifelse(is.numeric(pwSurvTime),
                .arrayToString(pwSurvTime), pwSurvTime
            ), .getClassName(pwSurvTime[1]))
            .logDebug("lambda1 %s, %s", self$lambda1, self$.getParameterType("lambda1"))
            .logDebug("lambda2 %s, %s", self$lambda2, self$.getParameterType("lambda2"))

            # case 1: lambda1 and lambda2 = NA or generated
            if (length(pwSurvTime) == 1 && (is.na(pwSurvTime) || is.numeric(pwSurvTime)) &&
                    (all(is.na(self$lambda1)) || self$.getParameterType("lambda1") == C_PARAM_GENERATED) &&
                    length(self$lambda2) == 1 && (is.na(self$lambda2) || self$.getParameterType("lambda2") == C_PARAM_GENERATED)
                ) {
                .logDebug(".init, case 1: lambda1 and lambda2 = NA")

                if (!is.null(self$.lambdaBased) && isTRUE(self$.lambdaBased)) {
                    stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'lambda1' and 'lambda2' must be specified")
                }

                if (!any(is.na(self$hazardRatio))) {
                    self$.setParameterType("hazardRatio", C_PARAM_USER_DEFINED)
                }

                if (!is.na(pwSurvTime)) {
                    warning("'piecewiseSurvivalTime' (", pwSurvTime, ") will be ignored")
                }

                if (is.na(self$pi2)) {
                    if (!is.na(self$median2) || !any(is.na(self$median1))) {
                        .logDebug(".init: set pi2 to 'not applicable'")
                        self$.setParameterType("pi2", C_PARAM_NOT_APPLICABLE)
                    } else {
                        .logDebug(".init: set pi2 to default")
                        self$pi2 <- C_PI_2_DEFAULT
                        self$.setParameterType("pi2", C_PARAM_DEFAULT_VALUE)
                    }
                } else {
                    .assertIsSingleNumber(self$pi2, "pi2")
                    self$.setParameterType("pi2", ifelse(self$pi2 == C_PI_2_DEFAULT,
                        C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
                    ))
                    if (!any(is.na(self$median2))) {
                        warning("'median2' (", .arrayToString(self$median2), ") will be ignored")
                        self$median2 <- NA_real_
                    }
                }

                hazardRatioCalculationEnabled <- TRUE
                if (all(is.na(self$pi1))) {
                    if (length(self$hazardRatio) > 0 && !all(is.na(self$hazardRatio))) {
                        self$.setParameterType("hazardRatio", C_PARAM_USER_DEFINED)
                        hazardRatioCalculationEnabled <- FALSE
                    }

                    if (!any(is.na(self$median1))) {
                        .logDebug(".init: set pi1 to 'not applicable'")
                        self$.setParameterType("pi1", C_PARAM_NOT_APPLICABLE)

                        if (is.na(self$median2)) {
                            if (any(is.na(self$hazardRatio))) {
                                stop(
                                    C_EXCEPTION_TYPE_MISSING_ARGUMENT,
                                    "'hazardRatio', 'lambda2', or 'median2' must be specified"
                                )
                            }

                            if (length(self$hazardRatio) != length(self$median1)) {
                                stop(
                                    C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                                    "length of 'hazardRatio' (", .arrayToString(self$hazardRatio), ") must be ",
                                    "equal to length of 'median1' (", .arrayToString(self$median1), ")"
                                )
                            }

                            .logDebug(".init: calculate lambda2 and median2 by median1")

                            self$lambda2 <- getLambdaByMedian(self$median1, self$kappa) / self$hazardRatio^(1 / self$kappa)

                            if (!self$delayedResponseAllowed && length(unique(round(self$lambda2, 8))) > 1) {
                                stop(
                                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                                    "'lambda2' can only be calculated if 'unique(lambda1 / hazardRatio^(1 / kappa))' ",
                                    "result in a single value; current result = ",
                                    .arrayToString(round(self$lambda2, 4), vectorLookAndFeelEnabled = TRUE),
                                    " (e.g., delayed response is not allowed)"
                                )
                            }

                            self$median2 <- getMedianByLambda(self$lambda2, self$kappa)
                            self$.setParameterType("lambda2", C_PARAM_GENERATED)
                            self$.setParameterType("median2", C_PARAM_GENERATED)
                        }
                    } else if (length(self$hazardRatio) > 0 && !all(is.na(self$hazardRatio))) {
                        self$.setParameterType("pi1", C_PARAM_NOT_APPLICABLE)

                        if (!any(is.na(self$lambda1))) {
                            .logDebug(".init: calculate median1 by lambda1")
                            self$median1 <- getMedianByLambda(self$lambda1, self$kappa)
                            self$.setParameterType("median1", C_PARAM_GENERATED)
                        } else if (!is.na(self$median2)) {
                            .logDebug(".init: calculate lambda1 and median1 by median2")
                            self$lambda1 <- getLambdaByMedian(self$median2, self$kappa) * self$hazardRatio^(1 / self$kappa)
                            self$median1 <- getMedianByLambda(self$lambda1, self$kappa)
                            self$.setParameterType("lambda1", C_PARAM_GENERATED)
                            self$.setParameterType("median1", C_PARAM_GENERATED)
                        }
                    } else {
                        .logDebug(".init: set pi1 to default")
                        if (!is.null(self$.pi1Default) && is.numeric(self$.pi1Default) &&
                                length(self$.pi1Default) > 0) {
                            self$pi1 <- self$.pi1Default
                        } else {
                            self$pi1 <- C_PI_1_SAMPLE_SIZE_DEFAULT
                        }
                        self$.setParameterType("pi1", C_PARAM_DEFAULT_VALUE)
                    }
                } else {
                    .assertIsNumericVector(self$pi1, "pi1")
                    if (!any(is.na(self$median1))) {
                        .logDebug(".init: set median1 to NA")
                        warning("'median1' (", .arrayToString(self$median1), ") will be ignored")
                        self$median1 <- NA_real_
                    }
                }

                if (hazardRatioCalculationEnabled) {
                    if (length(self$hazardRatio) > 0 && !all(is.na(self$hazardRatio))) {
                        warning("'hazardRatio' (", .arrayToString(self$hazardRatio),
                            ") will be ignored because it will be calculated",
                            call. = FALSE
                        )
                    }

                    if (!any(is.na(self$lambda1)) && !is.na(self$lambda2)) {
                        .logDebug(".init: calculate hazardRatio by lambda1 and lambda2")
                        self$hazardRatio <- (self$lambda1 / self$lambda2)^self$kappa
                        self$.setParameterType("hazardRatio", C_PARAM_GENERATED)
                    } else if (!any(is.na(self$pi1)) && !is.na(self$pi2)) {
                        .logDebug(".init: calculate hazardRatio by pi1 and pi2")
                        self$hazardRatio <- getHazardRatioByPi(self$pi1, self$pi2, self$eventTime, kappa = self$kappa)
                        self$.setParameterType("hazardRatio", C_PARAM_GENERATED)
                    }
                }

                if (length(self$pi1) > 0 && !any(is.na(self$pi1))) {
                    pi1Default <- C_PI_1_SAMPLE_SIZE_DEFAULT
                    if (!is.null(self$.pi1Default) && is.numeric(self$.pi1Default) &&
                            length(self$.pi1Default) > 0) {
                        pi1Default <- self$.pi1Default
                    }
                    if (identical(self$pi1, pi1Default)) {
                        self$.setParameterType("pi1", C_PARAM_DEFAULT_VALUE)
                    } else if (hazardRatioCalculationEnabled && self$.getParameterType("pi1") != C_PARAM_GENERATED) {
                        self$.setParameterType("pi1", C_PARAM_USER_DEFINED)
                    }
                }

                if (length(self$pi2) == 1 && !is.na(self$pi2)) {
                    if (length(self$eventTime) == 1 && !is.na(self$eventTime)) {
                        self$lambda2 <- getLambdaByPi(self$pi2, self$eventTime, kappa = self$kappa)
                        self$.setParameterType("lambda2", C_PARAM_GENERATED)
                    }

                    if (length(self$pi1) == 1 && is.na(self$pi1) && !any(is.na(self$hazardRatio))) {
                        self$pi1 <- getPiByLambda(
                            getLambdaByPi(
                                self$pi2, self$eventTime,
                                kappa = self$kappa
                            ) * self$hazardRatio^(1 / self$kappa),
                            self$eventTime,
                            kappa = self$kappa
                        )
                        self$.setParameterType("pi1", C_PARAM_GENERATED)
                    }
                    if (length(self$pi1) > 0 && !any(is.na(self$pi1)) &&
                            length(self$eventTime) == 1 && !is.na(self$eventTime)) {
                        self$lambda1 <- getLambdaByPi(self$pi1, self$eventTime, kappa = self$kappa)
                        self$.setParameterType("lambda1", C_PARAM_GENERATED)
                    }
                }

                self$.initMedian()
                return(invisible())
            }

            if (length(pwSurvTime) == 1 && is.na(pwSurvTime)) {
                pwSurvTime <- NA_real_
            }

            if (is.list(pwSurvTime)) {
                .assertIsValidHazardRatioVector(self$hazardRatio)
                self$.initFromList(pwSurvTime)
                self$.initHazardRatio()
                if (!self$piecewiseSurvivalEnabled) {
                    self$.initPi()
                    self$.initMedian()
                }
            } else if (self$delayedResponseAllowed && length(self$lambda2) == 1 &&
                    !is.na(self$lambda2) && length(self$hazardRatio) > 0 &&
                    (all(is.na(pwSurvTime)) || identical(pwSurvTime, 0))) {
                .logDebug(".init, case 2: delayedResponseAllowed")

                self$piecewiseSurvivalEnabled <- FALSE

                if (!all(is.na(pwSurvTime)) && !identical(pwSurvTime, 0)) {
                    warning("'piecewiseSurvivalTime' (", .arrayToString(pwSurvTime), ") will be ignored")
                }
                self$piecewiseSurvivalTime <- 0

                self$.initPi()
                self$.initHazardRatio()
                self$.initMedian()
            } else if (!is.numeric(pwSurvTime)) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'piecewiseSurvivalTime' must be a list, a numeric value, or vector"
                )
            } else {
                self$piecewiseSurvivalTime <- pwSurvTime
                if ((all(is.na(self$piecewiseSurvivalTime)) || identical(self$piecewiseSurvivalTime, 0)) &&
                        length(self$lambda2) == 1 && !is.na(self$lambda2)) {
                    .logDebug(".init, case 3: piecewise survival is disabled")
                    if (!all(is.na(self$piecewiseSurvivalTime)) && !identical(self$piecewiseSurvivalTime, 0)) {
                        warning("'piecewiseSurvivalTime' (", .arrayToString(self$piecewiseSurvivalTime), ") will be ignored")
                    }
                    self$piecewiseSurvivalTime <- 0
                    self$.setParameterType("piecewiseSurvivalTime", C_PARAM_DEFAULT_VALUE)
                    self$piecewiseSurvivalEnabled <- FALSE
                    self$.initHazardRatio()
                    self$.initPi()
                    self$.initMedian()
                } else {
                    .logDebug(".init, case 3: piecewise survival is enabled")
                    if (all(is.na(self$piecewiseSurvivalTime))) {
                        if (self$.getParameterType("median1") == C_PARAM_USER_DEFINED) {
                            stop(
                                C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                                "'median1' (", .arrayToString(self$median1), ") with length > 1 can only ",
                                "defined together with a single 'median2', 'lambda2' or 'pi2'"
                            )
                        }

                        if (self$delayedResponseAllowed && length(self$lambda1 > 0) && !all(is.na(self$lambda1)) &&
                                length(self$lambda1) != length(self$lambda2) && self$delayedResponseAllowed) {
                            stop(
                                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                                "length of 'lambda1' (", length(self$lambda1), "), 'lambda2' (", length(self$lambda2), "), and ",
                                "'piecewiseSurvivalTime' (", length(self$piecewiseSurvivalTime), ") must be equal"
                            )
                        }

                        stop(
                            C_EXCEPTION_TYPE_MISSING_ARGUMENT,
                            "'piecewiseSurvivalTime' must be specified"
                        )
                    }
                    self$.setParameterType("piecewiseSurvivalTime", C_PARAM_USER_DEFINED)
                    self$piecewiseSurvivalEnabled <- TRUE
                    self$.initHazardRatio()
                    self$.initPi()
                }
            }

            if (self$piecewiseSurvivalEnabled) {
                for (param in c("pi", "median")) {
                    for (group in 1:2) {
                        paramName <- paste0(param, group)
                        if (self$.getParameterType(paramName) == C_PARAM_USER_DEFINED) {
                            warning(
                                "'", paramName, "' (", .arrayToString(self[[paramName]]), ") ",
                                "was converted to 'lambda", group, "' ",
                                "and is not available in output because piecewise ",
                                "exponential survival time is enabled"
                            )
                        }
                    }
                }
                self$pi1 <- NA_real_
                self$pi2 <- NA_real_
                self$median1 <- NA_real_
                self$median2 <- NA_real_
                self$.setParameterType("pi1", C_PARAM_NOT_APPLICABLE)
                self$.setParameterType("pi2", C_PARAM_NOT_APPLICABLE)
                self$.setParameterType("median1", C_PARAM_NOT_APPLICABLE)
                self$.setParameterType("median2", C_PARAM_NOT_APPLICABLE)
                self$.setParameterType("eventTime", C_PARAM_NOT_APPLICABLE)
                if (!is.na(self$eventTime) && self$eventTime != C_EVENT_TIME_DEFAULT) {
                    warning("Event time (", self$eventTime, ") will be ignored because it is not ",
                        "applicable for piecewise exponential survival time",
                        call. = FALSE
                    )
                    self$eventTime <- C_EVENT_TIME_DEFAULT
                }
            }

            self$.validateInitialization()
        },
        .initMedian = function() {
            if (length(self$eventTime) == 1 && !is.na(self$eventTime)) {
                if (length(self$pi1) > 0 && !all(is.na(self$pi1)) && self$.getParameterType("median1") != C_PARAM_USER_DEFINED) {
                    self$median1 <- getMedianByPi(self$pi1, self$eventTime, kappa = self$kappa)
                    self$.setParameterType("median1", C_PARAM_GENERATED)
                }
                if (length(self$pi2) == 1 && !is.na(self$pi2) && self$.getParameterType("median2") != C_PARAM_USER_DEFINED) {
                    self$median2 <- getMedianByPi(self$pi2, self$eventTime, kappa = self$kappa)
                    self$.setParameterType("median2", C_PARAM_GENERATED)
                }
            } else {
                if (length(self$lambda1) > 0 && !all(is.na(self$lambda1)) && self$.getParameterType("median1") != C_PARAM_USER_DEFINED) {
                    self$median1 <- getMedianByLambda(self$lambda1, kappa = self$kappa)
                    self$.setParameterType("median1", C_PARAM_GENERATED)
                }
                if (length(self$lambda2) == 1 && !is.na(self$lambda2) && self$.getParameterType("median2") != C_PARAM_USER_DEFINED) {
                    self$median2 <- getMedianByLambda(self$lambda2, kappa = self$kappa)
                    self$.setParameterType("median2", C_PARAM_GENERATED)
                }
            }
        },
        .initPi = function() {
            .logDebug(".initPi: set pi1, pi2, and eventTime to NA")

            if (!is.na(self$eventTime) && self$.getParameterType("eventTime") == C_PARAM_USER_DEFINED) {
                warning("'eventTime' (", round(self$eventTime, 3), ") will be ignored", call. = FALSE)
            }
            if (!is.na(self$pi1) && !identical(self$pi2, C_PI_1_DEFAULT) && !identical(self$pi2, C_PI_1_SAMPLE_SIZE_DEFAULT)) {
                warning("'pi1' (", .arrayToString(self$pi1), ") will be ignored", call. = FALSE)
            }
            if (!is.na(self$pi2) && self$pi2 != C_PI_2_DEFAULT) {
                warning("'pi2' (", self$pi2, ") will be ignored", call. = FALSE)
            }

            self$.setParameterType("eventTime", C_PARAM_NOT_APPLICABLE)
            self$.setParameterType("pi1", C_PARAM_NOT_APPLICABLE)
            self$.setParameterType("pi2", C_PARAM_NOT_APPLICABLE)
            self$eventTime <- NA_real_
            self$pi1 <- NA_real_
            self$pi2 <- NA_real_

            if (length(self$lambda2) == 0 || any(is.na(self$lambda2))) {
                stop(
                    C_EXCEPTION_TYPE_RUNTIME_ISSUE,
                    "'lambda2' must be defined before .initPi() can be called"
                )
            }

            self$.setParameterType("lambda2", C_PARAM_USER_DEFINED)

            if (self$piecewiseSurvivalEnabled && length(self$hazardRatio) > 1) {
                return(invisible())
            }

            if (length(self$lambda1) == 0 || any(is.na(self$lambda1))) {
                if (length(self$hazardRatio) > 0 && !any(is.na(self$hazardRatio))) {
                    .logDebug(".initPi: calculate lambda1 by hazardRatio")
                    self$lambda1 <- self$lambda2 * self$hazardRatio^(1 / self$kappa)
                    self$.setParameterType("lambda1", C_PARAM_GENERATED)
                } else if (length(self$lambda1) == 0) {
                    self$lambda1 <- NA_real_
                } else if (self$delayedResponseAllowed) {
                    self$.setParameterType("lambda1", C_PARAM_USER_DEFINED)
                }
            }
        },
        .initHazardRatio = function() {
            .logDebug(".initHazardRatio")

            if (!is.null(self$hazardRatio) && length(self$hazardRatio) > 0 && !all(is.na(self$hazardRatio))) {
                if ((length(self$lambda1) == 1 && is.na(self$lambda1)) ||
                        self$.getParameterType("lambda1") == C_PARAM_GENERATED) {
                    self$.setParameterType("hazardRatio", C_PARAM_USER_DEFINED)
                    return(invisible())
                }

                if (!self$.silent) {
                    warning("'hazardRatio' (", .arrayToString(self$hazardRatio),
                        ") will be ignored because it will be calculated",
                        call. = FALSE
                    )
                }
            }

            if (any(is.na(self$lambda2))) {
                stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'lambda2' must be specified")
            }

            if (any(is.na(self$lambda1))) {
                if (self$delayedResponseAllowed && any(is.na(self$hazardRatio) &&
                        !any(is.na(self$piecewiseSurvivalTime)) && length(self$lambda2) == length(self$piecewiseSurvivalTime))) {
                    stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'hazardRatio' must be specified")
                }
                if (any(is.na(self$hazardRatio))) {
                    stop(
                        C_EXCEPTION_TYPE_MISSING_ARGUMENT,
                        "'hazardRatio', 'lambda1' or 'median1' must be specified"
                    )
                }
                stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'lambda1' must be specified")
            }

            self$.setParameterType("lambda1", C_PARAM_USER_DEFINED)

            hr <- unique(round(self$lambda1 / self$lambda2, 8)^self$kappa)
            if (length(hr) != 1) {
                if (length(self$lambda2) == 1 && length(self$lambda1) > 1) {
                    self$hazardRatio <- (self$lambda1 / self$lambda2)^self$kappa
                    self$.setParameterType("hazardRatio", C_PARAM_GENERATED)
                    return(invisible())
                } else if (self$delayedResponseAllowed) {
                    self$hazardRatio <- (self$lambda1 / self$lambda2)^self$kappa
                    self$.setParameterType("hazardRatio", C_PARAM_GENERATED)
                    self$delayedResponseEnabled <- TRUE
                    return(invisible())
                } else {
                    stop(
                        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                        "'hazardRatio' can only be calculated if 'unique(lambda1 / lambda2)' ",
                        "result in a single value; current result = ",
                        .arrayToString(round(hr, 4), vectorLookAndFeelEnabled = TRUE),
                        " (e.g., delayed response is not allowed)"
                    )
                }
            }

            self$hazardRatio <- ((self$lambda1 / self$lambda2)^self$kappa)[1]
            self$.setParameterType("hazardRatio", C_PARAM_GENERATED)
        },
        .validateInitialization = function() {
            if (length(self$piecewiseSurvivalTime) == 0) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'piecewiseSurvivalTime' must contain at least one survival start time"
                )
            }

            if (any(is.na(self$piecewiseSurvivalTime))) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'piecewiseSurvivalTime' must contain valid survival start times"
                )
            }

            if (self$piecewiseSurvivalTime[1] != 0) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "the first value of 'piecewiseSurvivalTime' must be 0"
                )
            }

            if (length(self$piecewiseSurvivalTime) != length(self$lambda2)) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "length of 'piecewiseSurvivalTime' (", length(self$piecewiseSurvivalTime),
                    ") and length of 'lambda2' (", length(self$lambda2), ") must be equal"
                )
            }

            .assertValuesAreStrictlyIncreasing(self$piecewiseSurvivalTime, "piecewiseSurvivalTime")

            if ((length(self$lambda1) != 1 || is.na(self$lambda1)) &&
                    !(self$.getParameterType("lambda1") %in% c(C_PARAM_GENERATED, C_PARAM_USER_DEFINED))) {
                if (length(self$hazardRatio) == 1 && !is.na(self$hazardRatio)) {
                    self$lambda1 <- self$lambda2 * self$hazardRatio^(1 / self$kappa)
                    self$.setParameterType("lambda1", C_PARAM_GENERATED)
                } else if (length(self$hazardRatio) > 1 && self$delayedResponseAllowed &&
                        !is.na(self$hazardRatio[1])) {
                    if (!self$delayedResponseEnabled && self$.isLambdaBased()) {
                        if (self$delayedResponseAllowed) {
                            if (length(self$hazardRatio) != length(self$lambda2)) {
                                stop(
                                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                                    "length of 'hazardRatio' (", length(self$hazardRatio),
                                    ") and length of 'lambda2' (", length(self$lambda2), ") must be equal"
                                )
                            }
                            self$delayedResponseEnabled <- TRUE
                        } else {
                            warning("Only the first 'hazardRatio' (", round(self$hazardRatio[1], 4),
                                ") was used for piecewise survival time definition",
                                call. = FALSE
                            )
                            self$hazardRatio <- self$hazardRatio[1]
                        }
                        self$lambda1 <- self$lambda2 * self$hazardRatio^(1 / self$kappa)
                        self$.setParameterType("lambda1", C_PARAM_GENERATED)
                    }
                } else if (!self$delayedResponseEnabled && !(length(self$lambda2) == 1 && length(self$lambda1) > 1)) {
                    if (length(self$lambda1) > 1) {
                        warning("'lambda1' (", .arrayToString(self$lambda1),
                            ") will be ignored",
                            call. = FALSE
                        )
                    }
                    self$lambda1 <- NA_real_
                    self$.setParameterType("lambda1", C_PARAM_NOT_APPLICABLE)
                }
            } else if (length(self$hazardRatio) == 1 && !is.na(self$hazardRatio) &&
                    length(self$lambda1) > 0 && !any(is.na(self$lambda1)) &&
                    length(self$lambda2) > 0 && !any(is.na(self$lambda2))) {
                target <- self$lambda2 * self$hazardRatio^(1 / self$kappa)
                if (length(self$lambda1) > 0 && !all(is.na(self$lambda1)) &&
                        !isTRUE(all.equal(target, self$lambda1))) {
                    stop(
                        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                        "'lambda1' (", .arrayToString(self$lambda1), ") ",
                        "is not as expected (", .arrayToString(target), ") for given hazard ratio ", self$hazardRatio
                    )
                }
            }

            if (self$piecewiseSurvivalEnabled && !(length(self$lambda1) == 1 && is.na(self$lambda1)) &&
                    length(self$piecewiseSurvivalTime) != length(self$lambda1)) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "length of 'piecewiseSurvivalTime' (", length(self$piecewiseSurvivalTime),
                    ") and length of 'lambda1' (", length(self$lambda1), ") must be equal"
                )
            }
        }
    )
)

#'
#' @name AccrualTime
#'
#' @title
#' Accrual Time
#'
#' @description
#' Class for the definition of accrual time and accrual intensity.
#'
#' @template field_endOfAccrualIsUserDefined
#' @template field_followUpTimeMustBeUserDefined
#' @template field_maxNumberOfSubjectsIsUserDefined
#' @template field_maxNumberOfSubjectsCanBeCalculatedDirectly
#' @template field_absoluteAccrualIntensityEnabled
#' @template field_accrualTime
#' @template field_accrualIntensity
#' @template field_accrualIntensityRelative
#' @template field_maxNumberOfSubjects
#' @template field_remainingTime
#' @template field_piecewiseAccrualEnabled
#'
#' @details
#' \code{AccrualTime} is a class for the definition of accrual time and accrual intensity.
#'
#' @include f_core_constants.R
#' @include f_core_utilities.R
#' @include class_core_parameter_set.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
AccrualTime <- R6::R6Class("AccrualTime",
    inherit = TimeDefinition,
    public = list(
        .showWarnings = NULL,
        endOfAccrualIsUserDefined = NULL,
        followUpTimeMustBeUserDefined = NULL,
        maxNumberOfSubjectsIsUserDefined = NULL,
        maxNumberOfSubjectsCanBeCalculatedDirectly = NULL,
        absoluteAccrualIntensityEnabled = NULL,
        accrualTime = NULL,
        accrualIntensity = NULL,
        accrualIntensityRelative = NULL,
        maxNumberOfSubjects = NULL,
        remainingTime = NULL,
        piecewiseAccrualEnabled = NULL,
        initialize = function(accrualTime = NA_real_,
                ...,
                accrualIntensity = NA_real_,
                maxNumberOfSubjects = NA_real_,
                showWarnings = TRUE,
                absoluteAccrualIntensityEnabled = NA) {
            super$initialize()
            self$accrualTime <- accrualTime
            self$accrualIntensity <- accrualIntensity
            self$maxNumberOfSubjects <- maxNumberOfSubjects
            self$.showWarnings <- showWarnings
            self$absoluteAccrualIntensityEnabled <- absoluteAccrualIntensityEnabled

            self$endOfAccrualIsUserDefined <- NA
            self$followUpTimeMustBeUserDefined <- NA
            self$maxNumberOfSubjectsIsUserDefined <- NA
            self$maxNumberOfSubjectsCanBeCalculatedDirectly <- TRUE
            # absoluteAccrualIntensityEnabled <<- NA
            self$.setParameterType("endOfAccrualIsUserDefined", C_PARAM_GENERATED)
            self$.setParameterType("followUpTimeMustBeUserDefined", C_PARAM_GENERATED)
            self$.setParameterType("maxNumberOfSubjectsIsUserDefined", C_PARAM_GENERATED)
            self$.setParameterType("maxNumberOfSubjectsCanBeCalculatedDirectly", C_PARAM_GENERATED)
            self$.setParameterType(
                "absoluteAccrualIntensityEnabled",
                ifelse(is.na(self$absoluteAccrualIntensityEnabled), C_PARAM_GENERATED, C_PARAM_USER_DEFINED)
            )

            self$accrualIntensityRelative <- NA_real_
            self$.setParameterType("accrualIntensityRelative", C_PARAM_NOT_APPLICABLE)
            self$remainingTime <- NA_real_

            self$.init(self$accrualTime)

            # case 6 correction
            if (!self$endOfAccrualIsUserDefined && self$maxNumberOfSubjectsIsUserDefined &&
                    !self$absoluteAccrualIntensityEnabled) {
                self$remainingTime <- NA_real_
                self$.setParameterType("remainingTime", C_PARAM_NOT_APPLICABLE)
                self$accrualTime <- self$accrualTime[1:length(self$accrualIntensity)]
            }

            self$.initAccrualIntensityAbsolute()
            self$.validateFormula()
            self$.showWarningIfCaseIsNotAllowed() # TODO wrong naming upstream!
        },
        .asDataFrame = function() {
            accrualIntensityTemp <- self$accrualIntensity
            if (!all(is.na(self$accrualIntensityRelative))) {
                accrualIntensityTemp <- self$accrualIntensityRelative
            }
            if (length(accrualIntensityTemp) + 1 == length(self$accrualTime)) {
                accrualIntensityTemp <- c(accrualIntensityTemp, NA_real_)
            }
            data <- data.frame(
                accrualTime = self$accrualTime,
                accrualIntensity = accrualIntensityTemp
            )
            rownames(data) <- as.character(1:nrow(data))
            colnames(data) <- c(
                "Start time",
                .getParameterCaption("accrualIntensity", self)
            )
            return(data)
        },
        show = function(showType = 1, digits = NA_integer_) {
            self$.show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
        },
        .isAbsoluteAccrualIntensity = function(x) {
            return(!self$.isRelativeAccrualIntensity(x))
        },
        .isRelativeAccrualIntensity = function(x) {
            return(all(x < 1))
        },
        .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
            "Method for automatically printing accrual time objects"
            self$.resetCat()
            if (showType == 2) {
                super$.show(showType = showType, digits = digits, consoleOutputEnabled = consoleOutputEnabled)
            } else {
                self$.cat("Accrual time and intensity:\n",
                    sep = "", heading = 1,
                    consoleOutputEnabled = consoleOutputEnabled
                )
                if (!self$isAccrualTimeEnabled()) {
                    self$.cat("  Accrual time is disabled.\n", consoleOutputEnabled = consoleOutputEnabled)
                } else if (length(self$accrualTime) == 1) {
                    self$.cat("  At all times:", self$accrualIntensity[1], "\n", consoleOutputEnabled = consoleOutputEnabled)
                } else {
                    accrualTimeStr <- format(self$accrualTime)
                    accrualIntensityStr <- format(self$accrualIntensity)
                    for (i in 1:length(self$accrualTime)) {
                        prefix <- ifelse(i == length(self$accrualTime) - 1, "<=", " <")
                        suffix <- ""
                        if (!self$maxNumberOfSubjectsIsUserDefined) {
                            suffix <- " "
                        }
                        if (i < length(self$accrualTime)) {
                            self$.cat("  ", accrualTimeStr[i], " - ", prefix, accrualTimeStr[i + 1], suffix, ": ",
                                accrualIntensityStr[i], "\n",
                                consoleOutputEnabled = consoleOutputEnabled
                            )
                        } else if (!self$maxNumberOfSubjectsIsUserDefined && !is.na(accrualIntensityStr[i]) &&
                                accrualIntensityStr[i] != "NA") {
                            self$.cat("  ", accrualTimeStr[i], " - <=[?]: ",
                                accrualIntensityStr[i], "\n",
                                consoleOutputEnabled = consoleOutputEnabled
                            )
                        }
                    }
                    self$.cat("", consoleOutputEnabled = consoleOutputEnabled)
                }
                self$.cat("\n", consoleOutputEnabled = consoleOutputEnabled)

                if (self$isAccrualTimeEnabled()) {
                    self$.showFormula(consoleOutputEnabled = consoleOutputEnabled)
                    self$.cat("\n", consoleOutputEnabled = consoleOutputEnabled)

                    self$.showCase(consoleOutputEnabled = consoleOutputEnabled)
                    self$.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
                }

                self$.cat("Details:\n\n", sep = "", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
                self$.showParametersOfOneGroup(self$.getUserDefinedParameters(), "User defined parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showParametersOfOneGroup(self$.getDefaultParameters(), "Default parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showParametersOfOneGroup(self$.getGeneratedParameters(), "Generated parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)
            }
        },
        .getFormula = function() {
            s <- ""
            for (i in 1:length(self$accrualTime)) {
                if (i < length(self$accrualTime)) {
                    s <- paste0(
                        s, (round(self$accrualTime[i + 1], 4) - round(self$accrualTime[i], 4)),
                        " * ", round(self$accrualIntensity[i], 4)
                    )
                    if (!self$absoluteAccrualIntensityEnabled &&
                            (!self$maxNumberOfSubjectsIsUserDefined || !self$endOfAccrualIsUserDefined)) {
                        s <- paste0(s, " * c ")
                    }
                    if (i < length(self$accrualIntensity)) {
                        s <- paste0(s, " + ")
                    }
                }
            }
            return(s)
        },
        .validateFormula = function() {
            if (is.na(self$maxNumberOfSubjects) || length(self$accrualTime) != length(self$accrualIntensity) + 1) {
                return(invisible())
            }

            numberOfSubjects <- 0
            for (i in 1:length(self$accrualTime)) {
                if (i < length(self$accrualTime)) {
                    numberOfSubjects <- numberOfSubjects +
                        (self$accrualTime[i + 1] - self$accrualTime[i]) * self$accrualIntensity[i]
                }
            }
            if (!isTRUE(all.equal(numberOfSubjects, self$maxNumberOfSubjects, tolerance = 1e-03)) &&
                    self$absoluteAccrualIntensityEnabled) {
                stop(
                    C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                    "'maxNumberOfSubjects' (", self$maxNumberOfSubjects, ") disagrees with ",
                    "the defined accrual time and intensity: ",
                    self$.getFormula(), " = ", numberOfSubjects
                )
            }
        },
        .showWarningIfCaseIsNotAllowed = function() {
            caseIsAllowed <- TRUE
            if (!self$endOfAccrualIsUserDefined && self$maxNumberOfSubjectsIsUserDefined &&
                    !self$absoluteAccrualIntensityEnabled) {
                caseIsAllowed <- FALSE
            } else if (!self$endOfAccrualIsUserDefined && !self$maxNumberOfSubjectsIsUserDefined &&
                    self$followUpTimeMustBeUserDefined && !self$absoluteAccrualIntensityEnabled) {
                caseIsAllowed <- FALSE
            }
            if (!caseIsAllowed) {
                warning("The specified accrual time and intensity cannot be ",
                    "supplemented automatically with the missing information; ",
                    "therefore further calculations are not possible",
                    call. = FALSE
                )
            }
        },
        .showFormula = function(consoleOutputEnabled) {
            self$.cat("Formula:\n", sep = "", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
            self$.cat("  ", consoleOutputEnabled = consoleOutputEnabled)
            self$.cat("maxNumberOfSubjects = ", consoleOutputEnabled = consoleOutputEnabled)
            if (!is.na(self$maxNumberOfSubjects)) {
                self$.cat(self$maxNumberOfSubjects, " = ", consoleOutputEnabled = consoleOutputEnabled)
            }
            self$.cat(self$.getFormula(), consoleOutputEnabled = consoleOutputEnabled)
            if (length(self$accrualTime) == length(self$accrualIntensity)) {
                self$.cat("(x - ", self$accrualTime[length(self$accrualTime)], ") * ",
                    self$accrualIntensity[length(self$accrualIntensity)],
                    consoleOutputEnabled = consoleOutputEnabled
                )
                if (!self$absoluteAccrualIntensityEnabled &&
                        (!self$maxNumberOfSubjectsIsUserDefined || !self$endOfAccrualIsUserDefined)) {
                    self$.cat(" * c ", consoleOutputEnabled = consoleOutputEnabled)
                }
                self$.cat(", where 'x' is the unknown last accrual time",
                    consoleOutputEnabled = consoleOutputEnabled
                )
                if (!self$absoluteAccrualIntensityEnabled &&
                        (!self$maxNumberOfSubjectsIsUserDefined || !self$endOfAccrualIsUserDefined)) {
                    self$.cat(" and 'c' a constant factor", consoleOutputEnabled = consoleOutputEnabled)
                }
            } else if (!self$absoluteAccrualIntensityEnabled &&
                    (!self$maxNumberOfSubjectsIsUserDefined || !self$endOfAccrualIsUserDefined)) {
                self$.cat(", where 'c' is a constant factor", consoleOutputEnabled = consoleOutputEnabled)
            }
            self$.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
        },
        .showCase = function(consoleOutputEnabled = TRUE) {
            caseIsAllowed <- TRUE

            prefix <- "  "

            # Case 1
            # example: getAccrualTime(accrualTime = c(0, 6, 30), accrualIntensity = c(22, 33),
            #          maxNumberOfSubjects = 1000)
            if (self$endOfAccrualIsUserDefined && self$maxNumberOfSubjectsIsUserDefined &&
                    self$absoluteAccrualIntensityEnabled) {
                self$.cat("Case (#1):\n", sep = "", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
                self$.cat(prefix, "End of accrual, absolute accrual intensity and 'maxNumberOfSubjects' are given, ",
                    " 'followUpTime'** shall be calculated.\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
                self$.cat(prefix, "Example: getAccrualTime(accrualTime = c(0, 6, 30), ",
                    "accrualIntensity = c(22, 33), maxNumberOfSubjects = 924)\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
            }

            # Case 2
            # example: getAccrualTime(accrualTime = c(0, 6, 30), accrualIntensity = c(0.22, 0.33),
            #          maxNumberOfSubjects = 1000)
            else if (self$endOfAccrualIsUserDefined && self$maxNumberOfSubjectsIsUserDefined &&
                    !self$absoluteAccrualIntensityEnabled) {
                self$.cat("Case (#2):\n", sep = "", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
                self$.cat(prefix, "End of accrual, relative accrual intensity and 'maxNumberOfSubjects' are given, ",
                    "absolute accrual intensity* and 'followUpTime'** shall be calculated.\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
                self$.cat(prefix, "Example: getAccrualTime(accrualTime = c(0, 6, 30), ",
                    "accrualIntensity = c(0.22, 0.33), maxNumberOfSubjects = 1000)\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
            }

            # Case 3
            # example: getAccrualTime(accrualTime = c(0, 6, 30), accrualIntensity = c(22, 33))
            else if (self$endOfAccrualIsUserDefined && !self$maxNumberOfSubjectsIsUserDefined &&
                    self$absoluteAccrualIntensityEnabled) {
                self$.cat("Case (#3):\n", sep = "", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
                self$.cat(prefix, "End of accrual and absolute accrual intensity are given, ",
                    "'maxNumberOfSubjects'* and 'followUpTime'** shall be calculated.\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
                self$.cat(prefix, "Example: getAccrualTime(accrualTime = c(0, 6, 30), accrualIntensity = c(22, 33))\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
            }

            # Case 4
            # example: getAccrualTime(accrualTime = c(0, 6, 30), accrualIntensity = c(0.22, 0.33))
            else if (self$endOfAccrualIsUserDefined && !self$maxNumberOfSubjectsIsUserDefined &&
                    !self$absoluteAccrualIntensityEnabled) {
                self$.cat("Case (#4):\n", sep = "", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
                self$.cat(prefix, "End of accrual, relative accrual intensity and 'followUpTime' are given, ",
                    "absolute accrual intensity** and 'maxNumberOfSubjects'** shall be calculated.\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
                self$.cat(prefix, "Example: getAccrualTime(accrualTime = c(0, 6, 30), accrualIntensity = c(0.22, 0.33))\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
            }

            # Case 5
            # example: getAccrualTime(accrualTime = c(0, 6), accrualIntensity = c(22, 33),
            #          maxNumberOfSubjects = 1000)
            else if (!self$endOfAccrualIsUserDefined && self$maxNumberOfSubjectsIsUserDefined &&
                    self$absoluteAccrualIntensityEnabled) {
                self$.cat("Case (#5):\n", sep = "", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
                self$.cat(prefix, "'maxNumberOfSubjects' and absolute accrual intensity are given, ",
                    "end of accrual* and 'followUpTime'** shall be calculated\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
                self$.cat(prefix, "Example: getAccrualTime(accrualTime = c(0, 6), ",
                    "accrualIntensity = c(22, 33), maxNumberOfSubjects = 1000)\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
            }

            # Case 6
            # example: getAccrualTime(accrualTime = c(0, 6), accrualIntensity = c(0.22, 0.33),
            #          maxNumberOfSubjects = 1000)
            else if (!self$endOfAccrualIsUserDefined && self$maxNumberOfSubjectsIsUserDefined &&
                    !self$absoluteAccrualIntensityEnabled) {
                caseIsAllowed <- FALSE
                self$.cat("Case (#6):\n", sep = "", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
                self$.cat(prefix, "'maxNumberOfSubjects' and relative accrual intensity are given, ",
                    "absolute accrual intensity@, end of accrual* and 'followUpTime'** shall be calculated\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
                self$.cat(prefix, "Example: getAccrualTime(accrualTime = c(0, 6), ",
                    "accrualIntensity = c(0.22, 0.33), maxNumberOfSubjects = 1000)\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
            }

            # Case 7
            # example: getAccrualTime(accrualTime = c(0, 6), accrualIntensity = c(22, 33))
            else if (!self$endOfAccrualIsUserDefined && !self$maxNumberOfSubjectsIsUserDefined &&
                    self$followUpTimeMustBeUserDefined && self$absoluteAccrualIntensityEnabled) {
                self$.cat("Case (#7):\n", sep = "", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
                self$.cat(prefix, "'followUpTime' and absolute accrual intensity are given, ",
                    "end of accrual** and 'maxNumberOfSubjects'** shall be calculated\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
                self$.cat(prefix, "Example: getAccrualTime(accrualTime = c(0, 6), accrualIntensity = c(22, 33))\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
            }

            # Case 8
            # example: getAccrualTime(accrualTime = c(0, 6), accrualIntensity = c(0.22, 0.33))
            else if (!self$endOfAccrualIsUserDefined && !self$maxNumberOfSubjectsIsUserDefined &&
                    self$followUpTimeMustBeUserDefined && !self$absoluteAccrualIntensityEnabled) {
                caseIsAllowed <- FALSE
                self$.cat("Case (#8):\n", sep = "", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
                self$.cat(prefix, "'followUpTime' and relative accrual intensity are given, ",
                    "absolute accrual intensity@, end of accrual and 'maxNumberOfSubjects' shall be calculated\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
                self$.cat(prefix, "Example: getAccrualTime(accrualTime = c(0, 6), accrualIntensity = c(0.22, 0.33))\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
            }

            # .cat("\n", consoleOutputEnabled = consoleOutputEnabled)
            if (!caseIsAllowed) {
                self$.cat(prefix, "(@) Cannot be calculated.\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
            }
            self$.cat(prefix, "(*) Can be calculated directly.\n",
                consoleOutputEnabled = consoleOutputEnabled
            )
            self$.cat(prefix, "(**) Cannot be calculated directly but with ",
                "'getSampleSizeSurvival()' or 'getPowerSurvival()'.\n",
                consoleOutputEnabled = consoleOutputEnabled
            )
        },
        .followUpTimeShallBeCalculated = function() {
            # Case 1: 'followUpTime'** shall be calculated
            if (self$endOfAccrualIsUserDefined && self$maxNumberOfSubjectsIsUserDefined &&
                    self$absoluteAccrualIntensityEnabled) {
                return(TRUE)
            }

            # Case 2: 'followUpTime'** shall be calculated
            else if (self$endOfAccrualIsUserDefined && self$maxNumberOfSubjectsIsUserDefined &&
                    !self$absoluteAccrualIntensityEnabled) {
                return(TRUE)
            }

            # Case 3: 'followUpTime'** shall be calculated
            else if (self$endOfAccrualIsUserDefined && !self$maxNumberOfSubjectsIsUserDefined &&
                    self$absoluteAccrualIntensityEnabled) {
                return(TRUE)
            }


            # Case 5: 'followUpTime'** shall be calculated
            else if (!self$endOfAccrualIsUserDefined && self$maxNumberOfSubjectsIsUserDefined &&
                    self$absoluteAccrualIntensityEnabled) {
                return(TRUE)
            }

            # Case 6: 'followUpTime'** shall be calculated
            else if (!self$endOfAccrualIsUserDefined && self$maxNumberOfSubjectsIsUserDefined &&
                    !self$absoluteAccrualIntensityEnabled) {
                return(TRUE)
            }

            # (**) Cannot be calculated directly but with 'getSampleSizeSurvival()' or 'getPowerSurvival()'

            return(FALSE)
        },
        .validate = function() {
            # Case 6
            if (!self$endOfAccrualIsUserDefined && self$maxNumberOfSubjectsIsUserDefined &&
                    !self$absoluteAccrualIntensityEnabled) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "the calculation of 'followUpTime' for given 'maxNumberOfSubjects' ",
                    "and relative accrual intensities (< 1) ",
                    "can only be done if end of accrual is defined"
                )
            }

            # Case 8
            else if (!self$endOfAccrualIsUserDefined && !self$maxNumberOfSubjectsIsUserDefined &&
                    self$followUpTimeMustBeUserDefined && !self$absoluteAccrualIntensityEnabled) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "the calculation of 'maxNumberOfSubjects' for given 'followUpTime' ",
                    "and relative accrual intensities (< 1) ",
                    "can only be done if end of accrual is defined"
                )
            }
        },
        .toString = function(startWithUpperCase = FALSE) {
            s <- "accrual time"
            return(ifelse(startWithUpperCase, .firstCharacterToUpperCase(s), s))
        },
        .getAccrualTimeWithoutLeadingZero = function() {
            if (length(self$accrualTime) <= 1) {
                return(NA_real_)
            }

            return(self$accrualTime[2:length(self$accrualTime)])
        },
        isAccrualTimeEnabled = function() {
            if (length(self$accrualTime) == 0) {
                return(FALSE)
            }

            if (length(self$accrualTime) == 1 && is.na(self$accrualTime)) {
                return(FALSE)
            }

            return(TRUE)
        },
        .initFromList = function(accrualTimeList) {
            if (!is.list(accrualTimeList)) {
                stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'accrualTime' must be a list")
            }

            if (length(accrualTimeList) == 0) {
                stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'accrualTime' must contain at least one entry")
            }

            if (is.null(names(accrualTimeList))) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'accrualTime' must be a named list where the names specify ",
                    "the time regions and the values the accrual time"
                )
            }

            if (self$.showWarnings && !all(is.na(self$accrualIntensity)) && (length(self$accrualIntensity) != 1 ||
                    self$accrualIntensity != C_ACCRUAL_INTENSITY_DEFAULT)) {
                warning("'accrualIntensity' (", .arrayToString(self$accrualIntensity),
                    ") will be ignored because 'accrualTime' is a list",
                    call. = FALSE
                )
            }

            self$accrualTime <- numeric(0)
            self$accrualIntensity <- numeric(0)
            timeRegions <- names(accrualTimeList)
            endOfAccrualIsUndefined <- FALSE
            self$accrualTime <- c(self$accrualTime, 0)
            for (i in 1:length(timeRegions)) {
                timePeriod <- timeRegions[i]
                accrualTimeValue <- accrualTimeList[[timePeriod]]
                .assertIsSingleNumber(accrualTimeValue, paste0("accrualTime[", i, "]"))

                settings <- self$.validateTimePeriod(timePeriod, i = i, n = length(timeRegions), accrualTimeMode = TRUE)
                timePeriod <- settings$timePeriod
                endOfAccrualIsUndefined <- settings$endOfAccrualIsUndefined

                if (i < length(timeRegions)) {
                    parts <- strsplit(timePeriod, "- *(< *)?", perl = TRUE)[[1]]
                    if (length(parts) != 2) {
                        stop(
                            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                            "all regions (", timePeriod, ") must have the format ",
                            "\"time_1 - <time_2\", e.g., \"3 - <6\""
                        )
                    }
                    self$accrualTime <- c(self$accrualTime, as.numeric(trimws(parts[2])))
                } else {
                    parts <- strsplit(timePeriod, " *< *", perl = TRUE)[[1]]
                    if (length(parts) == 2) {
                        self$accrualTime <- c(self$accrualTime, as.numeric(trimws(parts[2])))
                    }
                }
                self$accrualIntensity <- c(self$accrualIntensity, accrualTimeValue)
            }

            self$.setParameterType("accrualTime", C_PARAM_USER_DEFINED)
            self$.setParameterType("accrualIntensity", C_PARAM_USER_DEFINED)

            return(endOfAccrualIsUndefined = endOfAccrualIsUndefined)
        },
        .initAccrualIntensityAbsolute = function() {
            if (is.null(self$maxNumberOfSubjects) || length(self$maxNumberOfSubjects) != 1 ||
                    is.na(self$maxNumberOfSubjects) || self$maxNumberOfSubjects == 0) {
                if (!self$absoluteAccrualIntensityEnabled) {
                    self$accrualIntensityRelative <- self$accrualIntensity
                }
                return(invisible())
            }

            if (!self$endOfAccrualIsUserDefined && self$maxNumberOfSubjectsIsUserDefined &&
                    !self$absoluteAccrualIntensityEnabled) {
                return(invisible()) # case 6
            }

            if (length(self$accrualTime) >= 2 && length(self$accrualTime) == length(self$accrualIntensity) + 1 &&
                    !any(is.na(self$accrualTime)) && !any(is.na(self$accrualIntensity))) {
                len <- length(self$accrualIntensity)
                accrualIntensityAbsolute <- self$maxNumberOfSubjects / sum((self$accrualTime[2:(len + 1)] -
                    self$accrualTime[1:len]) * self$accrualIntensity) * self$accrualIntensity

                if (!isTRUE(all.equal(accrualIntensityAbsolute, self$accrualIntensity, tolerance = 1e-06)) &&
                        !isTRUE(all.equal(accrualIntensityAbsolute, 0, tolerance = 1e-06))) {
                    self$.validateAccrualTimeAndIntensity()

                    if (self$absoluteAccrualIntensityEnabled &&
                            self$.getParameterType("maxNumberOfSubjects") == C_PARAM_USER_DEFINED) {
                        if (self$.getParameterType("accrualTime") == C_PARAM_DEFAULT_VALUE) {
                            self$accrualTime <- self$maxNumberOfSubjects / self$accrualIntensity
                            self$.setParameterType("accrualTime", C_PARAM_GENERATED)
                            self$remainingTime <- self$accrualTime
                            self$accrualTime <- c(0, self$accrualTime)
                        } else {
                            stop(
                                C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                                "'maxNumberOfSubjects' (", self$maxNumberOfSubjects, ") disagrees with ",
                                "the defined accrual time (", .arrayToString(self$accrualTime), ") and intensity: ",
                                self$.getFormula(), " = ", self$.getSampleSize()
                            )
                        }
                    } else {
                        if (!self$absoluteAccrualIntensityEnabled &&
                                self$.getParameterType("accrualIntensity") == C_PARAM_USER_DEFINED &&
                                self$.getParameterType("accrualTime") == C_PARAM_DEFAULT_VALUE &&
                                self$.getParameterType("maxNumberOfSubjects") == C_PARAM_USER_DEFINED) {
                            if (self$.showWarnings) {
                                warning("'accrualIntensity' (", self$accrualIntensity, ") will be ignored", call. = FALSE)
                            }
                            self$accrualIntensityRelative <- C_ACCRUAL_INTENSITY_DEFAULT
                            self$accrualIntensity <- accrualIntensityAbsolute
                            self$.setParameterType("accrualIntensity", C_PARAM_GENERATED)
                            self$.setParameterType("remainingTime", C_PARAM_NOT_APPLICABLE)
                        } else {
                            self$accrualIntensityRelative <- self$accrualIntensity
                            self$accrualIntensity <- accrualIntensityAbsolute
                            self$.setParameterType("accrualIntensity", C_PARAM_GENERATED)
                            self$.setParameterType("accrualIntensityRelative", C_PARAM_USER_DEFINED)
                        }
                    }
                }
            }
        },
        .isNoPiecewiseAccrualTime = function(accrualTimeArg) {
            if (length(accrualTimeArg) == 0 || any(is.na(accrualTimeArg)) ||
                    !all(is.numeric(accrualTimeArg))) {
                stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'accrualTimeArg' must a be valid numeric vector")
            }

            if (length(accrualTimeArg) == 1) {
                return(TRUE)
            }

            if (length(accrualTimeArg) == 2 && accrualTimeArg[1] == 0) {
                return(TRUE)
            }

            return(FALSE)
        },
        .init = function(accrualTimeArg) {
            if (length(accrualTimeArg) == 0) {
                stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'accrualTime' must be defined")
            }

            if (length(accrualTimeArg) == 1 && is.numeric(accrualTimeArg) && is.na(accrualTimeArg)) {
                accrualTimeArg <- C_ACCRUAL_TIME_DEFAULT
            }

            calculateLastAccrualTimeEnabled <- FALSE
            if (is.list(accrualTimeArg)) {
                endOfAccrualIsUndefined <- self$.initFromList(accrualTimeArg)
                calculateLastAccrualTimeEnabled <- endOfAccrualIsUndefined &&
                    !is.null(self$maxNumberOfSubjects) && length(self$maxNumberOfSubjects) == 1 &&
                    !is.na(self$maxNumberOfSubjects)
            } else if (is.numeric(accrualTimeArg)) {
                .assertIsNumericVector(accrualTimeArg, "accrualTime")
                if (length(self$accrualIntensity) > 1) {
                    .assertIsNumericVector(self$accrualIntensity, "accrualIntensity")
                }

                if (self$.isNoPiecewiseAccrualTime(accrualTimeArg) &&
                        (length(self$accrualIntensity) == 0 || is.null(self$accrualIntensity) ||
                            all(is.na(self$accrualIntensity)) ||
                            all(self$accrualIntensity == C_ACCRUAL_INTENSITY_DEFAULT))) {
                    accrualTimeArg <- accrualTimeArg[length(accrualTimeArg)]
                    self$accrualTime <- c(0L, accrualTimeArg)
                    self$.setParameterType("accrualTime", ifelse(
                        identical(as.integer(self$accrualTime), C_ACCRUAL_TIME_DEFAULT),
                        C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
                    ))

                    self$accrualIntensity <- C_ACCRUAL_INTENSITY_DEFAULT
                    self$.setParameterType("accrualIntensity", C_PARAM_DEFAULT_VALUE)

                    self$.setParameterType(
                        "maxNumberOfSubjects",
                        ifelse(length(self$maxNumberOfSubjects) == 1 && is.na(self$maxNumberOfSubjects),
                            C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
                        )
                    )

                    self$endOfAccrualIsUserDefined <- length(self$accrualTime) == length(self$accrualIntensity) + 1
                    self$maxNumberOfSubjectsIsUserDefined <-
                        self$.getParameterType("maxNumberOfSubjects") == C_PARAM_USER_DEFINED
                    self$followUpTimeMustBeUserDefined <- !self$endOfAccrualIsUserDefined &&
                        !self$maxNumberOfSubjectsIsUserDefined
                    self$absoluteAccrualIntensityEnabled <- FALSE

                    if (self$maxNumberOfSubjectsIsUserDefined) {
                        self$accrualIntensity <- self$maxNumberOfSubjects / self$accrualTime[length(self$accrualTime)]
                        self$.setParameterType("accrualIntensity", C_PARAM_GENERATED)
                    }

                    return(invisible())
                }

                self$accrualTime <- accrualTimeArg
                if (length(self$accrualTime) == 0) {
                    stop(
                        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                        "'accrualTime' must contain at least one time value"
                    )
                }

                if (self$accrualTime[1] != 0) {
                    stop(
                        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                        "the first value of 'accrualTime' (", .arrayToString(self$accrualTime), ") must be 0"
                    )
                }

                self$.setParameterType("accrualTime", ifelse(
                    identical(as.integer(self$accrualTime), C_ACCRUAL_TIME_DEFAULT),
                    C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
                ))
                self$.setParameterType("accrualIntensity", C_PARAM_USER_DEFINED)
            } else {
                stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'accrualTime' must be a list or a numeric vector")
            }

            if (is.na(self$absoluteAccrualIntensityEnabled)) {
                self$absoluteAccrualIntensityEnabled <- self$.isAbsoluteAccrualIntensity(self$accrualIntensity)
            }
            if (is.null(self$maxNumberOfSubjects) || length(self$maxNumberOfSubjects) == 0 ||
                    any(is.na(self$maxNumberOfSubjects))) {
                if (length(self$accrualTime) != length(self$accrualIntensity) + 1 ||
                        !self$absoluteAccrualIntensityEnabled) {
                    self$maxNumberOfSubjectsCanBeCalculatedDirectly <- FALSE
                }

                self$.setParameterType("maxNumberOfSubjects", C_PARAM_NOT_APPLICABLE)
            } else {
                if (!(length(self$accrualTime) %in% c(
                        length(self$accrualIntensity),
                        length(self$accrualIntensity) + 1
                    ))) {
                    stop(
                        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                        "length of 'accrualTime' (", length(self$accrualTime),
                        ") must be equal to length of 'accrualIntensity' if the last 'accrualTime' ",
                        "shall be calculated ",
                        "based on 'maxNumberOfSubjects' or length of 'accrualIntensity' (",
                        length(self$accrualIntensity), ") + 1 otherwise"
                    )
                }
                if (length(self$accrualTime) == length(self$accrualIntensity)) {
                    calculateLastAccrualTimeEnabled <- TRUE
                }

                self$.setParameterType("maxNumberOfSubjects", C_PARAM_USER_DEFINED)
            }

            self$endOfAccrualIsUserDefined <- length(self$accrualTime) == length(self$accrualIntensity) + 1

            if (calculateLastAccrualTimeEnabled) {
                self$.calculateRemainingTime()
            } else if (self$maxNumberOfSubjectsCanBeCalculatedDirectly) {
                if (length(self$accrualTime) == 1) {
                    if (length(self$maxNumberOfSubjects) > 0 && !is.na(self$maxNumberOfSubjects) &&
                            self$maxNumberOfSubjects > 0 && self$maxNumberOfSubjects < self$accrualIntensity[1]) {
                        stop(
                            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                            "'maxNumberOfSubjects' (", self$maxNumberOfSubjects, ") ",
                            "must be >= ", self$accrualIntensity[1], " ('accrualIntensity')"
                        )
                    }
                    self$remainingTime <- self$accrualTime
                    self$.setParameterType("remainingTime", C_PARAM_USER_DEFINED)
                } else if (length(self$accrualTime) > 1) {
                    sampleSize <- self$.getSampleSize()
                    if (!isTRUE(all.equal(sampleSize, self$maxNumberOfSubjects, tolerance = 1e-04))) {
                        if (length(self$maxNumberOfSubjects) == 1 && !is.na(self$maxNumberOfSubjects) &&
                                self$maxNumberOfSubjects > 0 && self$maxNumberOfSubjects < sampleSize) {
                            if (length(self$accrualIntensity) == 1 && length(self$accrualTime) == 1) {
                                self$.setParameterType("maxNumberOfSubjects", C_PARAM_USER_DEFINED)
                                self$accrualTime <- 0
                                self$.calculateRemainingTime()
                            } else {
                                if (length(self$accrualTime) == length(self$accrualIntensity) + 1 &&
                                        self$absoluteAccrualIntensityEnabled) {
                                    stop(
                                        C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                                        "'maxNumberOfSubjects' (", self$maxNumberOfSubjects, ") disagrees with ",
                                        "the defined accrual time and intensity: ",
                                        self$.getFormula(), " = ", sampleSize
                                    )
                                } else {
                                    stop(
                                        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'maxNumberOfSubjects' (",
                                        self$maxNumberOfSubjects, ") ", "must be >= ", sampleSize
                                    )
                                }
                            }
                        } else {
                            if ((length(self$maxNumberOfSubjects) != 1 || is.na(self$maxNumberOfSubjects)) &&
                                    self$absoluteAccrualIntensityEnabled) {
                                self$maxNumberOfSubjects <- sampleSize
                                self$.setParameterType("maxNumberOfSubjects", C_PARAM_GENERATED)
                            }
                            self$remainingTime <- self$accrualTime[length(self$accrualTime)] - self$accrualTime[length(self$accrualTime) - 1]
                            self$.setParameterType(
                                "remainingTime",
                                ifelse(!isTRUE(all.equal(0, self$remainingTime, tolerance = 1e-06)),
                                    C_PARAM_GENERATED, C_PARAM_NOT_APPLICABLE
                                )
                            )
                        }
                    }
                }
            }

            self$.validateInitialization()

            self$maxNumberOfSubjectsIsUserDefined <- self$.getParameterType("maxNumberOfSubjects") == C_PARAM_USER_DEFINED
            self$followUpTimeMustBeUserDefined <- !self$endOfAccrualIsUserDefined && !self$maxNumberOfSubjectsIsUserDefined
        },
        .getSampleSize = function() {
            if (length(self$accrualTime) < 2) {
                return(0)
            }

            sampleSize <- 0
            for (i in 2:length(self$accrualTime)) {
                time <- self$accrualTime[i] - self$accrualTime[i - 1]
                sampleSize <- sampleSize + time * self$accrualIntensity[i - 1]
            }
            return(sampleSize)
        },
        .getValuesAfterDecimalPoint = function(x) {
            values <- c()
            for (value in x) {
                baseLevel <- value - floor(value)
                if (baseLevel == 0) {
                    baseLevel <- 1
                }
                values <- c(values, baseLevel)
            }
            return(values)
        },
        .getBaseLevel = function(x) {
            return(min(self$.getValuesAfterDecimalPoint(x[x > 0])))
        },
        .calcSampleSize = function() {
            if (length(self$accrualTime) <= 1) {
                return(0)
            }

            accrualTimeTemp <- self$accrualTime
            accrualIntensityTemp <- self$accrualIntensity

            sampleSize <- 0
            for (i in 2:length(self$accrualTime)) {
                time <- self$accrualTime[i] - self$accrualTime[i - 1]
                sampleSize <- sampleSize + time * self$accrualIntensity[i - 1]
                if (sampleSize >= self$maxNumberOfSubjects &&
                        length(self$accrualTime) == length(self$accrualIntensity)) {
                    if (sampleSize > self$maxNumberOfSubjects) {
                        self$accrualTime <- self$accrualTime[1:(i - 1)]
                    }

                    i2 <- i
                    if (length(self$accrualTime) == length(self$accrualIntensity) + 1) {
                        i2 <- i - 1
                    }
                    self$accrualIntensity <- self$accrualIntensity[1:(i2 - 1)]

                    while (length(self$accrualTime) > length(self$accrualIntensity) + 1) {
                        self$accrualTime <- self$accrualTime[1:(length(self$accrualTime) - 1)]
                    }

                    sampleSize <- 0
                    if (length(self$accrualTime) > 1) {
                        sampleSize <- self$.getSampleSize()
                    }

                    if (self$.showWarnings) {
                        n1 <- length(accrualTimeTemp) - length(self$accrualTime)
                        n2 <- length(accrualIntensityTemp) - length(self$accrualIntensity)

                        if (n1 == 1) {
                            warning("Last accrual time value (",
                                accrualTimeTemp[length(accrualTimeTemp)], ") ignored",
                                call. = FALSE
                            )
                        } else if (n1 > 1) {
                            warning("Last ", n1, " accrual time values (",
                                .arrayToString(accrualTimeTemp[(length(accrualTimeTemp) - n1 + 1):length(accrualTimeTemp)]),
                                ") ignored",
                                call. = FALSE
                            )
                        }

                        if (n2 == 1) {
                            warning("Last accrual intensity value (",
                                accrualIntensityTemp[length(accrualIntensityTemp)], ") ignored",
                                call. = FALSE
                            )
                        } else if (n2 > 1) {
                            warning("Last ", n2, " accrual intensity values (",
                                .arrayToString(accrualIntensityTemp[i2:length(accrualIntensityTemp)]),
                                ") ignored",
                                call. = FALSE
                            )
                        }
                    }

                    return(sampleSize)
                }
            }
            return(sampleSize)
        },
        .calculateRemainingTime = function(stopInCaseOfError = TRUE) {
            .assertIsValidMaxNumberOfSubjects(self$maxNumberOfSubjects)

            sampleSize <- self$.calcSampleSize()
            remainingSubjects <- self$maxNumberOfSubjects - sampleSize
            if (remainingSubjects < 0) {
                if (!stopInCaseOfError) {
                    return(invisible())
                }
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'maxNumberOfSubjects' (", self$maxNumberOfSubjects, ") ",
                    "is too small for the defined accrual time (minimum = ", sampleSize, ")"
                )
            }

            lastAccrualIntensity <- self$accrualIntensity[length(self$accrualIntensity)]
            self$remainingTime <- remainingSubjects / lastAccrualIntensity
            self$.setParameterType(
                "remainingTime",
                ifelse(!isTRUE(all.equal(0, self$remainingTime, tolerance = 1e-06)),
                    C_PARAM_GENERATED, C_PARAM_NOT_APPLICABLE
                )
            )
            if (length(self$accrualTime) == length(self$accrualIntensity)) {
                self$accrualTime <- c(self$accrualTime, self$accrualTime[length(self$accrualTime)] + self$remainingTime)
            }
            # .setParameterType("accrualTime", C_PARAM_GENERATED)
            if (any(self$accrualTime < 0)) {
                if (!stopInCaseOfError) {
                    return(invisible())
                }
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'maxNumberOfSubjects' (", self$maxNumberOfSubjects, ") ",
                    "is too small for the defined accrual time"
                )
            }
        },
        .validateAccrualTimeAndIntensity = function() {
            if ((length(self$accrualTime) >= 2 && any(self$accrualTime[2:length(self$accrualTime)] < 0))) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'accrualTime' (", .arrayToString(self$accrualTime), ") must be > 0"
                )
            }

            .assertValuesAreStrictlyIncreasing(self$accrualTime, "accrualTime")

            if ((length(self$accrualTime) > 1) && any(self$accrualIntensity < 0)) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'accrualIntensity' (", .arrayToString(self$accrualIntensity), ") must be >= 0"
                )
            }

            if (length(self$accrualIntensity) == 1 && !is.na(self$accrualIntensity) &&
                    self$accrualIntensity == 0) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "at least one 'accrualIntensity' value must be > 0"
                )
            }

            if (length(self$accrualIntensity) > 0 && self$accrualIntensity[1] == 0) {
                warning(
                    "It makes no sense to start 'accrualIntensity' (",
                    .arrayToString(self$accrualIntensity), ") with 0"
                )
            }
        },
        .validateInitialization = function() {
            self$.validateAccrualTimeAndIntensity()

            self$piecewiseAccrualEnabled <- !self$.isNoPiecewiseAccrualTime(self$accrualTime)
        }
    )
)
