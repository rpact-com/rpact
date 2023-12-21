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
## |  File version: $Revision: 7476 $
## |  Last changed: $Date: 2023-12-07 11:57:03 +0100 (Do, 07 Dez 2023) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_design_general_utilities.R
NULL

C_REGEXP_GREATER_OR_EQUAL <- ">= ?"
C_REGEXP_SMALLER <- "< ?"
C_REGEXP_SMALLER_OR_EQUAL <- "<= ?"
C_REGEXP_DECIMAL_NUMBER <- "\\d*(\\.{1}\\d*)?"

TimeDefinition <- setRefClass("TimeDefinition",
    contains = "ParameterSet",
    methods = list(
        initialize = function(...) {
            callSuper(...)
            .parameterNames <<- C_PARAMETER_NAMES
            .parameterFormatFunctions <<- C_PARAMETER_FORMAT_FUNCTIONS
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
            return(.getRegexpFromTo(from = "0", to = C_REGEXP_DECIMAL_NUMBER, toPrefix = C_REGEXP_SMALLER))
        },
        .getRegexpDecimalRange = function() {
            return(.getRegexpFromTo(
                from = C_REGEXP_DECIMAL_NUMBER, to = C_REGEXP_DECIMAL_NUMBER,
                toPrefix = C_REGEXP_SMALLER
            ))
        },
        .getRegexpDecimalRangeEnd = function() {
            return(.getRegexpFromTo(
                from = C_REGEXP_DECIMAL_NUMBER, to = "(Inf|x|\\?)",
                toPrefix = paste0("(", C_REGEXP_SMALLER, " *)?")
            ))
        },
        .getRegexpDecimalRangeFiniteEnd = function() {
            return(.getRegexpFromTo(
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
                if (!grepl(.getRegexpOr(.getRegexpSmallerThan(), .getRegexpDecimalRangeStart()),
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
                if (grepl(.getRegexpSmallerThan(), timePeriod, perl = TRUE)) {
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
                            .getRegexpOr(
                                .getRegexpDecimalNumber(),
                                .getRegexpGreaterOrEqualThan(), .getRegexpDecimalRangeEnd(),
                                .getRegexpDecimalRangeFiniteEnd()
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
                    if (grepl(.getRegexpOr(.getRegexpGreaterOrEqualThan(), .getRegexpDecimalRangeEnd()),
                            timePeriod,
                            perl = TRUE
                        )) {
                        endOfAccrualIsUndefined <- TRUE
                    }
                    timePeriod <- gsub("([Inf >=\\?x]*)|-", "", timePeriod)
                } else {
                    if (!grepl(.getRegexpOr(.getRegexpGreaterOrEqualThan(), .getRegexpDecimalRangeEnd()),
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
                if (!grepl(.getRegexpDecimalRange(), timePeriod, perl = TRUE)) {
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

    return(PiecewiseSurvivalTime(
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

    return(AccrualTime(
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
PiecewiseSurvivalTime <- setRefClass("PiecewiseSurvivalTime",
    contains = "TimeDefinition",
    fields = list(
        .pi1Default = "numeric",
        .lambdaBased = "logical",
        .silent = "logical",
        piecewiseSurvivalTime = "numeric",
        lambda1 = "numeric",
        lambda2 = "numeric",
        hazardRatio = "numeric",
        pi1 = "numeric",
        pi2 = "numeric",
        median1 = "numeric",
        median2 = "numeric",
        eventTime = "numeric",
        kappa = "numeric",
        piecewiseSurvivalEnabled = "logical",
        delayedResponseAllowed = "logical",
        delayedResponseEnabled = "logical"
    ),
    methods = list(
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
            callSuper(
                piecewiseSurvivalTime = NA_real_,
                lambda1 = lambda1,
                lambda2 = lambda2,
                hazardRatio = hazardRatio,
                pi1 = pi1,
                pi2 = pi2,
                median1 = median1,
                median2 = median2,
                eventTime = eventTime,
                kappa = kappa,
                delayedResponseAllowed = delayedResponseAllowed, ...
            )

            if (length(piecewiseSurvivalTime) == 0) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'piecewiseSurvivalTime' must be defined (set to NA_real_ if not applicable)"
                )
            }

            .stopInCaseOfConflictingArguments(lambda1, "lambda1", median1, "median1")
            .stopInCaseOfConflictingArguments(lambda2, "lambda2", median2, "median2")

            .stopInCaseOfConflictingArguments(pi1, "pi1", median1, "median1")
            .stopInCaseOfConflictingArguments(pi1, "pi1", median2, "median2")
            .stopInCaseOfConflictingArguments(pi1, "pi1", lambda1, "lambda1")
            .stopInCaseOfConflictingArguments(pi1, "pi1", lambda2, "lambda2")
            .stopInCaseOfConflictingArguments(pi2, "pi2", median1, "median1")
            .stopInCaseOfConflictingArguments(pi2, "pi2", median2, "median2")
            .stopInCaseOfConflictingArguments(pi2, "pi2", lambda1, "lambda1")
            .stopInCaseOfConflictingArguments(pi2, "pi2", lambda2, "lambda2")

            if (length(median1) > 0 && !all(is.na(median1))) {
                .self$lambda1 <<- getLambdaByMedian(median1, kappa = kappa)
                .setParameterType("median1", C_PARAM_USER_DEFINED)
                .setParameterType("lambda1", C_PARAM_GENERATED)
            } else {
                .setParameterType("median1", C_PARAM_NOT_APPLICABLE)
                .setParameterType("lambda1", ifelse(length(lambda1) == 1 && is.na(lambda1),
                    C_PARAM_NOT_APPLICABLE, C_PARAM_USER_DEFINED
                ))
            }
            if (length(median2) > 0 && !all(is.na(median2))) {
                .self$lambda2 <<- getLambdaByMedian(median2, kappa = kappa)
                .setParameterType("median2", C_PARAM_USER_DEFINED)
                .setParameterType("lambda2", C_PARAM_GENERATED)
            } else {
                .setParameterType("median2", C_PARAM_NOT_APPLICABLE)
                .setParameterType("lambda2", C_PARAM_NOT_APPLICABLE)
            }

            args <- list(...)
            if (!is.null(args[[".pi1Default"]])) {
                .pi1Default <<- args[[".pi1Default"]]
            }
            if (!is.null(args[[".lambdaBased"]])) {
                .lambdaBased <<- args[[".lambdaBased"]]
            }
            if (!is.null(args[[".silent"]])) {
                .silent <<- args[[".silent"]]
            } else {
                .silent <<- FALSE
            }

            piecewiseSurvivalEnabled <<- FALSE
            delayedResponseEnabled <<- FALSE

            .setParameterType("piecewiseSurvivalTime", C_PARAM_NOT_APPLICABLE)
            .setParameterType("piecewiseSurvivalEnabled", C_PARAM_GENERATED)
            .setParameterType("delayedResponseEnabled", ifelse(isTRUE(delayedResponseAllowed),
                C_PARAM_GENERATED, C_PARAM_NOT_APPLICABLE
            ))
            .setParameterType("delayedResponseAllowed", ifelse(isTRUE(delayedResponseAllowed),
                C_PARAM_USER_DEFINED, C_PARAM_DEFAULT_VALUE
            ))
            .setParameterType("pi1", C_PARAM_NOT_APPLICABLE)
            .setParameterType("pi2", C_PARAM_NOT_APPLICABLE)
            .setParameterType("eventTime", ifelse(length(eventTime) == 1 && is.na(eventTime),
                C_PARAM_NOT_APPLICABLE,
                ifelse(eventTime == C_EVENT_TIME_DEFAULT, C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED)
            ))
            .setParameterType("kappa", ifelse(length(kappa) == 1 && !is.na(kappa) && kappa == 1,
                C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
            ))

            .init(piecewiseSurvivalTime)

            if (.getParameterType("median1") == C_PARAM_USER_DEFINED &&
                    .getParameterType("lambda1") == C_PARAM_USER_DEFINED) {
                .setParameterType("lambda1", C_PARAM_GENERATED)
            }

            if (.getParameterType("median2") == C_PARAM_USER_DEFINED &&
                    .getParameterType("lambda2") == C_PARAM_USER_DEFINED) {
                .setParameterType("lambda2", C_PARAM_GENERATED)
            }

            if (!is.na(eventTime) &&
                    .getParameterType("pi1") != C_PARAM_USER_DEFINED &&
                    .getParameterType("pi1") != C_PARAM_DEFAULT_VALUE &&
                    .getParameterType("pi2") != C_PARAM_USER_DEFINED &&
                    .getParameterType("pi2") != C_PARAM_DEFAULT_VALUE) {
                if (.getParameterType("eventTime") == C_PARAM_USER_DEFINED) {
                    warning("'eventTime' (", round(eventTime, 3), ") will be ignored", call. = FALSE)
                }
                .setParameterType("eventTime", C_PARAM_NOT_APPLICABLE)
                eventTime <<- NA_real_
            }

            .validateCalculatedArguments()
        },
        .validateCalculatedArguments = function() {
            if (.getParameterType("median1") == C_PARAM_USER_DEFINED) {
                if (!isTRUE(all.equal(getLambdaByMedian(median1, kappa = kappa), lambda1, tolerance = 1e-05))) {
                    stop(
                        C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'lambda1' must be ",
                        round(getLambdaByMedian(median1, kappa = kappa), 5), ", but is ", round(lambda1, 5)
                    )
                }
                if (!any(is.na(pi1)) &&
                        !isTRUE(all.equal(getPiByMedian(median1, eventTime = eventTime, kappa = kappa),
                            pi1,
                            tolerance = 1e-05
                        ))) {
                    stop(
                        C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'pi1' must be ",
                        round(getPiByMedian(median1, eventTime = eventTime, kappa = kappa), 5), ", but is ", round(pi1, 5)
                    )
                }
            }

            if (.getParameterType("median2") == C_PARAM_USER_DEFINED) {
                if (!isTRUE(all.equal(getLambdaByMedian(median2, kappa = kappa), lambda2, tolerance = 1e-05))) {
                    stop(
                        C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'lambda2' must be ",
                        round(getLambdaByMedian(median2, kappa = kappa), 5), ", but is ", round(lambda2, 5)
                    )
                }
                if (!is.na(pi2) &&
                        !isTRUE(all.equal(getPiByMedian(median2, eventTime = eventTime, kappa = kappa),
                            pi2,
                            tolerance = 1e-05
                        ))) {
                    stop(
                        C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'pi2' must be ",
                        round(getPiByMedian(median2, eventTime = eventTime, kappa = kappa), 5), ", but is ", round(pi2, 5)
                    )
                }
            }

            if (.getParameterType("lambda1") == C_PARAM_USER_DEFINED ||
                    .getParameterType("median1") == C_PARAM_USER_DEFINED ||
                    .getParameterType("lambda2") == C_PARAM_USER_DEFINED ||
                    .getParameterType("median2") == C_PARAM_USER_DEFINED) {
                if (!any(is.na(pi1))) {
                    stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'pi1' (", pi1, ") must be NA_real_")
                }
                if (.getParameterType("pi1") != C_PARAM_NOT_APPLICABLE) {
                    stop(
                        C_EXCEPTION_TYPE_RUNTIME_ISSUE, "parameter type of 'pi1' (",
                        .getParameterType("pi1"), ") must be C_PARAM_NOT_APPLICABLE"
                    )
                }
                if (!any(is.na(pi1))) {
                    stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'pi2' (", pi2, ") must be NA_real_")
                }
                if (.getParameterType("pi2") != C_PARAM_NOT_APPLICABLE) {
                    stop(
                        C_EXCEPTION_TYPE_RUNTIME_ISSUE, "parameter type of 'pi2' (",
                        .getParameterType("pi2"), ") must be C_PARAM_NOT_APPLICABLE"
                    )
                }
                if (!any(is.na(eventTime))) {
                    stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'eventTime' (", eventTime, ") must be NA_real_")
                }
                if (.getParameterType("eventTime") != C_PARAM_NOT_APPLICABLE) {
                    stop(
                        C_EXCEPTION_TYPE_RUNTIME_ISSUE, "parameter type of 'eventTime' (",
                        .getParameterType("eventTime"), ") must be C_PARAM_NOT_APPLICABLE"
                    )
                }
            }

            if (.getParameterType("hazardRatio") == C_PARAM_TYPE_UNKNOWN) {
                stop(
                    C_EXCEPTION_TYPE_RUNTIME_ISSUE, "parameter type of 'hazardRatio' (",
                    hazardRatio, ") must be != C_PARAM_TYPE_UNKNOWN"
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
                piecewiseSurvivalTime = piecewiseSurvivalTime,
                lambda1 = lambda1,
                lambda2 = lambda2
            )
            rownames(data) <- as.character(1:nrow(data))
            colnames(data) <- c(
                "Start time",
                C_PARAMETER_NAMES["lambda1"], # Hazard rate (1)
                C_PARAMETER_NAMES["lambda2"]
            ) # Hazard rate (2)
            return(data)
        },
        .isPiBased = function() {
            return(!.isLambdaBased())
        },
        .isLambdaBased = function(minNumberOfLambdas = 2) {
            if (.getParameterType("lambda2") == C_PARAM_USER_DEFINED ||
                    .getParameterType("median2") == C_PARAM_USER_DEFINED) {
                if (length(lambda2) >= minNumberOfLambdas && !any(is.na(lambda2))) {
                    return(TRUE)
                }
            }

            return((length(pi1) == 0 || any(is.na(pi1))) && (length(pi2) == 0 || any(is.na(pi2))))
        },
        show = function(showType = 1, digits = NA_integer_) {
            .show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
        },
        .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
            "Method for automatically printing piecewise survival time objects"
            .resetCat()
            if (showType == 2) {
                callSuper(showType = showType, digits = digits, consoleOutputEnabled = consoleOutputEnabled)
            } else {
                .cat("Piecewise exponential survival times:\n",
                    sep = "", heading = 1,
                    consoleOutputEnabled = consoleOutputEnabled
                )
                if (!piecewiseSurvivalEnabled) {
                    .cat("  Piecewise exponential survival is disabled.\n\n", consoleOutputEnabled = consoleOutputEnabled)
                } else if (length(piecewiseSurvivalTime) == 1) {
                    .cat("  At all times:", lambda2[1], "\n\n", consoleOutputEnabled = consoleOutputEnabled)
                } else {
                    piecewiseSurvivalTimeStr <- format(piecewiseSurvivalTime)
                    lambda2Str <- format(lambda2)
                    for (i in 1:length(piecewiseSurvivalTime)) {
                        if (i < length(piecewiseSurvivalTime)) {
                            .cat("  ", piecewiseSurvivalTimeStr[i], " - <",
                                piecewiseSurvivalTimeStr[i + 1], ": ",
                                lambda2Str[i], "\n",
                                sep = "",
                                consoleOutputEnabled = consoleOutputEnabled
                            )
                        } else {
                            .cat("  ", rep(" ", 2 + max(nchar(piecewiseSurvivalTimeStr))),
                                ">=", piecewiseSurvivalTimeStr[i], ": ",
                                lambda2Str[i], "\n",
                                sep = "",
                                consoleOutputEnabled = consoleOutputEnabled
                            )
                        }
                    }
                    if (delayedResponseEnabled) {
                        .cat("Delayed response is enabled.\n", consoleOutputEnabled = consoleOutputEnabled)
                    }
                    .cat("\n", consoleOutputEnabled = consoleOutputEnabled)
                }

                .cat("Details:\n\n", sep = "", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
                .showParametersOfOneGroup(.getUserDefinedParameters(), "User defined parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                .showParametersOfOneGroup(.getDefaultParameters(), "Default parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                .showParametersOfOneGroup(.getGeneratedParameters(), "Generated parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                .showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)
            }
        },
        .toString = function(startWithUpperCase = FALSE) {
            s <- "piecewise survival time"
            return(ifelse(startWithUpperCase, .firstCharacterToUpperCase(s), s))
        },
        isDelayedResponseEnabled = function() {
            return(delayedResponseEnabled)
        },
        isPiecewiseSurvivalEnabled = function() {
            if (length(piecewiseSurvivalTime) == 0) {
                return(FALSE)
            }

            if (length(piecewiseSurvivalTime) == 1 && is.na(piecewiseSurvivalTime)) {
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

            if (!all(is.na(lambda2))) {
                warning("'lambda2' (", .arrayToString(lambda2),
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

                timePeriod <- .validateTimePeriod(timePeriod, i = i, n = length(pwSurvTimeNames))

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

            piecewiseSurvivalTime <<- pwSurvStartTimes
            .setParameterType("piecewiseSurvivalTime", C_PARAM_USER_DEFINED)
            if (length(hazardRatio) == 1 && !is.na(hazardRatio)) {
                lambda1 <<- pwSurvLambda2 * hazardRatio^(1 / kappa)
                .setParameterType("lambda1", C_PARAM_GENERATED)
            } else if (length(hazardRatio) > 1 && delayedResponseAllowed) {
                if (length(hazardRatio) != length(pwSurvLambda2)) {
                    warning("Only the first 'hazardRatio' (", round(hazardRatio[1], 4),
                        ") was used for piecewise survival time definition ",
                        "(use a loop over the function to simulate different hazard ratios)",
                        call. = FALSE
                    )
                    hazardRatio <<- hazardRatio[1]
                } else {
                    delayedResponseEnabled <<- TRUE
                }
                lambda1 <<- pwSurvLambda2 * hazardRatio^(1 / kappa)
                .setParameterType("lambda1", C_PARAM_GENERATED)
            } else {
                lambda1 <<- NA_real_
                .setParameterType("lambda1", C_PARAM_NOT_APPLICABLE)
            }

            lambda2 <<- pwSurvLambda2
            .setParameterType("lambda2", C_PARAM_USER_DEFINED)

            piecewiseSurvivalEnabled <<- !identical(piecewiseSurvivalTime, 0)
        },
        .init = function(pwSurvTime) {
            .logDebug("pwSurvTime %s, %s", ifelse(is.numeric(pwSurvTime),
                .arrayToString(pwSurvTime), pwSurvTime
            ), .getClassName(pwSurvTime[1]))
            .logDebug("lambda1 %s, %s", lambda1, .getParameterType("lambda1"))
            .logDebug("lambda2 %s, %s", lambda2, .getParameterType("lambda2"))

            # case 1: lambda1 and lambda2 = NA or generated
            if (length(pwSurvTime) == 1 && (is.na(pwSurvTime) || is.numeric(pwSurvTime)) &&
                    (all(is.na(lambda1)) || .getParameterType("lambda1") == C_PARAM_GENERATED) &&
                    length(lambda2) == 1 && (is.na(lambda2) || .getParameterType("lambda2") == C_PARAM_GENERATED)
                ) {
                .logDebug(".init, case 1: lambda1 and lambda2 = NA")

                if (!is.null(.lambdaBased) && isTRUE(.lambdaBased)) {
                    stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'lambda1' and 'lambda2' must be specified")
                }

                if (!any(is.na(hazardRatio))) {
                    .setParameterType("hazardRatio", C_PARAM_USER_DEFINED)
                }

                if (!is.na(pwSurvTime)) {
                    warning("'piecewiseSurvivalTime' (", pwSurvTime, ") will be ignored")
                }

                if (is.na(pi2)) {
                    if (!is.na(median2) || !any(is.na(median1))) {
                        .logDebug(".init: set pi2 to 'not applicable'")
                        .setParameterType("pi2", C_PARAM_NOT_APPLICABLE)
                    } else {
                        .logDebug(".init: set pi2 to default")
                        pi2 <<- C_PI_2_DEFAULT
                        .setParameterType("pi2", C_PARAM_DEFAULT_VALUE)
                    }
                } else {
                    .assertIsSingleNumber(pi2, "pi2")
                    .setParameterType("pi2", ifelse(pi2 == C_PI_2_DEFAULT,
                        C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
                    ))
                    if (!any(is.na(median2))) {
                        warning("'median2' (", .arrayToString(median2), ") will be ignored")
                        median2 <<- NA_real_
                    }
                }

                hazardRatioCalculationEnabled <- TRUE
                if (all(is.na(pi1))) {
                    if (length(hazardRatio) > 0 && !all(is.na(hazardRatio))) {
                        .setParameterType("hazardRatio", C_PARAM_USER_DEFINED)
                        hazardRatioCalculationEnabled <- FALSE
                    }

                    if (!any(is.na(median1))) {
                        .logDebug(".init: set pi1 to 'not applicable'")
                        .setParameterType("pi1", C_PARAM_NOT_APPLICABLE)

                        if (is.na(median2)) {
                            if (any(is.na(hazardRatio))) {
                                stop(
                                    C_EXCEPTION_TYPE_MISSING_ARGUMENT,
                                    "'hazardRatio', 'lambda2', or 'median2' must be specified"
                                )
                            }

                            if (length(hazardRatio) != length(median1)) {
                                stop(
                                    C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                                    "length of 'hazardRatio' (", .arrayToString(hazardRatio), ") must be ",
                                    "equal to length of 'median1' (", .arrayToString(median1), ")"
                                )
                            }

                            .logDebug(".init: calculate lambda2 and median2 by median1")

                            lambda2 <<- getLambdaByMedian(median1, kappa) / hazardRatio^(1 / kappa)

                            if (!delayedResponseAllowed && length(unique(round(lambda2, 8))) > 1) {
                                stop(
                                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                                    "'lambda2' can only be calculated if 'unique(lambda1 / hazardRatio^(1 / kappa))' ",
                                    "result in a single value; current result = ",
                                    .arrayToString(round(lambda2, 4), vectorLookAndFeelEnabled = TRUE),
                                    " (e.g., delayed response is not allowed)"
                                )
                            }

                            median2 <<- getMedianByLambda(lambda2, kappa)
                            .setParameterType("lambda2", C_PARAM_GENERATED)
                            .setParameterType("median2", C_PARAM_GENERATED)
                        }
                    } else if (length(hazardRatio) > 0 && !all(is.na(hazardRatio))) {
                        .setParameterType("pi1", C_PARAM_NOT_APPLICABLE)

                        if (!any(is.na(lambda1))) {
                            .logDebug(".init: calculate median1 by lambda1")
                            median1 <<- getMedianByLambda(lambda1, kappa)
                            .setParameterType("median1", C_PARAM_GENERATED)
                        } else if (!is.na(median2)) {
                            .logDebug(".init: calculate lambda1 and median1 by median2")
                            lambda1 <<- getLambdaByMedian(median2, kappa) * hazardRatio^(1 / kappa)
                            median1 <<- getMedianByLambda(lambda1, kappa)
                            .setParameterType("lambda1", C_PARAM_GENERATED)
                            .setParameterType("median1", C_PARAM_GENERATED)
                        }
                    } else {
                        .logDebug(".init: set pi1 to default")
                        if (!is.null(.pi1Default) && is.numeric(.pi1Default) &&
                                length(.pi1Default) > 0) {
                            pi1 <<- .pi1Default
                        } else {
                            pi1 <<- C_PI_1_SAMPLE_SIZE_DEFAULT
                        }
                        .setParameterType("pi1", C_PARAM_DEFAULT_VALUE)
                    }
                } else {
                    .assertIsNumericVector(pi1, "pi1")
                    if (!any(is.na(median1))) {
                        .logDebug(".init: set median1 to NA")
                        warning("'median1' (", .arrayToString(median1), ") will be ignored")
                        median1 <<- NA_real_
                    }
                }

                if (hazardRatioCalculationEnabled) {
                    if (length(hazardRatio) > 0 && !all(is.na(hazardRatio))) {
                        warning("'hazardRatio' (", .arrayToString(hazardRatio),
                            ") will be ignored because it will be calculated",
                            call. = FALSE
                        )
                    }

                    if (!any(is.na(lambda1)) && !is.na(lambda2)) {
                        .logDebug(".init: calculate hazardRatio by lambda1 and lambda2")
                        hazardRatio <<- (lambda1 / lambda2)^kappa
                        .setParameterType("hazardRatio", C_PARAM_GENERATED)
                    } else if (!any(is.na(pi1)) && !is.na(pi2)) {
                        .logDebug(".init: calculate hazardRatio by pi1 and pi2")
                        hazardRatio <<- getHazardRatioByPi(pi1, pi2, eventTime, kappa = kappa)
                        .setParameterType("hazardRatio", C_PARAM_GENERATED)
                    }
                }

                if (length(pi1) > 0 && !any(is.na(pi1))) {
                    pi1Default <- C_PI_1_SAMPLE_SIZE_DEFAULT
                    if (!is.null(.pi1Default) && is.numeric(.pi1Default) &&
                            length(.pi1Default) > 0) {
                        pi1Default <- .pi1Default
                    }
                    if (identical(pi1, pi1Default)) {
                        .setParameterType("pi1", C_PARAM_DEFAULT_VALUE)
                    } else if (hazardRatioCalculationEnabled && .getParameterType("pi1") != C_PARAM_GENERATED) {
                        .setParameterType("pi1", C_PARAM_USER_DEFINED)
                    }
                }

                if (length(pi2) == 1 && !is.na(pi2)) {
                    if (length(eventTime) == 1 && !is.na(eventTime)) {
                        lambda2 <<- getLambdaByPi(pi2, eventTime, kappa = kappa)
                        .setParameterType("lambda2", C_PARAM_GENERATED)
                    }

                    if (length(pi1) == 1 && is.na(pi1) && !any(is.na(hazardRatio))) {
                        pi1 <<- getPiByLambda(
                            getLambdaByPi(
                                pi2, eventTime,
                                kappa = kappa
                            ) * hazardRatio^(1 / kappa),
                            eventTime,
                            kappa = kappa
                        )
                        .setParameterType("pi1", C_PARAM_GENERATED)
                    }
                    if (length(pi1) > 0 && !any(is.na(pi1)) &&
                            length(eventTime) == 1 && !is.na(eventTime)) {
                        lambda1 <<- getLambdaByPi(pi1, eventTime, kappa = kappa)
                        .setParameterType("lambda1", C_PARAM_GENERATED)
                    }
                }

                .initMedian()
                return(invisible())
            }

            if (length(pwSurvTime) == 1 && is.na(pwSurvTime)) {
                pwSurvTime <- NA_real_
            }

            if (is.list(pwSurvTime)) {
                .assertIsValidHazardRatioVector(hazardRatio)
                .initFromList(pwSurvTime)
                .initHazardRatio()
                if (!piecewiseSurvivalEnabled) {
                    .initPi()
                    .initMedian()
                }
            } else if (delayedResponseAllowed && length(lambda2) == 1 &&
                    !is.na(lambda2) && length(hazardRatio) > 0 &&
                    (all(is.na(pwSurvTime)) || identical(pwSurvTime, 0))) {
                .logDebug(".init, case 2: delayedResponseAllowed")

                piecewiseSurvivalEnabled <<- FALSE

                if (!all(is.na(pwSurvTime)) && !identical(pwSurvTime, 0)) {
                    warning("'piecewiseSurvivalTime' (", .arrayToString(pwSurvTime), ") will be ignored")
                }
                piecewiseSurvivalTime <<- 0

                .initPi()
                .initHazardRatio()
                .initMedian()
            } else if (!is.numeric(pwSurvTime)) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'piecewiseSurvivalTime' must be a list, a numeric value, or vector"
                )
            } else {
                piecewiseSurvivalTime <<- pwSurvTime
                if ((all(is.na(piecewiseSurvivalTime)) || identical(piecewiseSurvivalTime, 0)) &&
                        length(lambda2) == 1 && !is.na(lambda2)) {
                    .logDebug(".init, case 3: piecewise survival is disabled")
                    if (!all(is.na(piecewiseSurvivalTime)) && !identical(piecewiseSurvivalTime, 0)) {
                        warning("'piecewiseSurvivalTime' (", .arrayToString(piecewiseSurvivalTime), ") will be ignored")
                    }
                    piecewiseSurvivalTime <<- 0
                    .setParameterType("piecewiseSurvivalTime", C_PARAM_DEFAULT_VALUE)
                    piecewiseSurvivalEnabled <<- FALSE
                    .initHazardRatio()
                    .initPi()
                    .initMedian()
                } else {
                    .logDebug(".init, case 3: piecewise survival is enabled")
                    if (all(is.na(piecewiseSurvivalTime))) {
                        if (.getParameterType("median1") == C_PARAM_USER_DEFINED) {
                            stop(
                                C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                                "'median1' (", .arrayToString(median1), ") with length > 1 can only ",
                                "defined together with a single 'median2', 'lambda2' or 'pi2'"
                            )
                        }

                        if (delayedResponseAllowed && length(lambda1 > 0) && !all(is.na(lambda1)) &&
                                length(lambda1) != length(lambda2) && delayedResponseAllowed) {
                            stop(
                                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                                "length of 'lambda1' (", length(lambda1), "), 'lambda2' (", length(lambda2), "), and ",
                                "'piecewiseSurvivalTime' (", length(piecewiseSurvivalTime), ") must be equal"
                            )
                        }

                        stop(
                            C_EXCEPTION_TYPE_MISSING_ARGUMENT,
                            "'piecewiseSurvivalTime' must be specified"
                        )
                    }
                    .setParameterType("piecewiseSurvivalTime", C_PARAM_USER_DEFINED)
                    piecewiseSurvivalEnabled <<- TRUE
                    .initHazardRatio()
                    .initPi()
                }
            }

            if (piecewiseSurvivalEnabled) {
                for (param in c("pi", "median")) {
                    for (group in 1:2) {
                        paramName <- paste0(param, group)
                        if (.getParameterType(paramName) == C_PARAM_USER_DEFINED) {
                            warning(
                                "'", paramName, "' (", .arrayToString(.self[[paramName]]), ") ",
                                "was converted to 'lambda", group, "' ",
                                "and is not available in output because piecewise ",
                                "exponential survival time is enabled"
                            )
                        }
                    }
                }
                pi1 <<- NA_real_
                pi2 <<- NA_real_
                median1 <<- NA_real_
                median2 <<- NA_real_
                .setParameterType("pi1", C_PARAM_NOT_APPLICABLE)
                .setParameterType("pi2", C_PARAM_NOT_APPLICABLE)
                .setParameterType("median1", C_PARAM_NOT_APPLICABLE)
                .setParameterType("median2", C_PARAM_NOT_APPLICABLE)
                .setParameterType("eventTime", C_PARAM_NOT_APPLICABLE)
                if (!is.na(eventTime) && eventTime != C_EVENT_TIME_DEFAULT) {
                    warning("Event time (", eventTime, ") will be ignored because it is not ",
                        "applicable for piecewise exponential survival time",
                        call. = FALSE
                    )
                    eventTime <<- C_EVENT_TIME_DEFAULT
                }
            }

            .validateInitialization()
        },
        .initMedian = function() {
            if (length(eventTime) == 1 && !is.na(eventTime)) {
                if (length(pi1) > 0 && !all(is.na(pi1)) && .getParameterType("median1") != C_PARAM_USER_DEFINED) {
                    median1 <<- getMedianByPi(pi1, eventTime, kappa = kappa)
                    .setParameterType("median1", C_PARAM_GENERATED)
                }
                if (length(pi2) == 1 && !is.na(pi2) && .getParameterType("median2") != C_PARAM_USER_DEFINED) {
                    median2 <<- getMedianByPi(pi2, eventTime, kappa = kappa)
                    .setParameterType("median2", C_PARAM_GENERATED)
                }
            } else {
                if (length(lambda1) > 0 && !all(is.na(lambda1)) && .getParameterType("median1") != C_PARAM_USER_DEFINED) {
                    median1 <<- getMedianByLambda(lambda1, kappa = kappa)
                    .setParameterType("median1", C_PARAM_GENERATED)
                }
                if (length(lambda2) == 1 && !is.na(lambda2) && .getParameterType("median2") != C_PARAM_USER_DEFINED) {
                    median2 <<- getMedianByLambda(lambda2, kappa = kappa)
                    .setParameterType("median2", C_PARAM_GENERATED)
                }
            }
        },
        .initPi = function() {
            .logDebug(".initPi: set pi1, pi2, and eventTime to NA")

            if (!is.na(eventTime) && .getParameterType("eventTime") == C_PARAM_USER_DEFINED) {
                warning("'eventTime' (", round(eventTime, 3), ") will be ignored", call. = FALSE)
            }
            if (!is.na(pi1) && !identical(pi2, C_PI_1_DEFAULT) && !identical(pi2, C_PI_1_SAMPLE_SIZE_DEFAULT)) {
                warning("'pi1' (", .arrayToString(pi1), ") will be ignored", call. = FALSE)
            }
            if (!is.na(pi2) && pi2 != C_PI_2_DEFAULT) {
                warning("'pi2' (", pi2, ") will be ignored", call. = FALSE)
            }

            .setParameterType("eventTime", C_PARAM_NOT_APPLICABLE)
            .setParameterType("pi1", C_PARAM_NOT_APPLICABLE)
            .setParameterType("pi2", C_PARAM_NOT_APPLICABLE)
            eventTime <<- NA_real_
            pi1 <<- NA_real_
            pi2 <<- NA_real_

            if (length(lambda2) == 0 || any(is.na(lambda2))) {
                stop(
                    C_EXCEPTION_TYPE_RUNTIME_ISSUE,
                    "'lambda2' must be defined before .initPi() can be called"
                )
            }

            .setParameterType("lambda2", C_PARAM_USER_DEFINED)

            if (piecewiseSurvivalEnabled && length(hazardRatio) > 1) {
                return(invisible())
            }

            if (length(lambda1) == 0 || any(is.na(lambda1))) {
                if (length(hazardRatio) > 0 && !any(is.na(hazardRatio))) {
                    .logDebug(".initPi: calculate lambda1 by hazardRatio")
                    lambda1 <<- lambda2 * hazardRatio^(1 / kappa)
                    .setParameterType("lambda1", C_PARAM_GENERATED)
                } else if (length(lambda1) == 0) {
                    lambda1 <<- NA_real_
                } else if (delayedResponseAllowed) {
                    .setParameterType("lambda1", C_PARAM_USER_DEFINED)
                }
            }
        },
        .initHazardRatio = function() {
            .logDebug(".initHazardRatio")

            if (!is.null(hazardRatio) && length(hazardRatio) > 0 && !all(is.na(hazardRatio))) {
                if ((length(lambda1) == 1 && is.na(lambda1)) ||
                        .getParameterType("lambda1") == C_PARAM_GENERATED) {
                    .setParameterType("hazardRatio", C_PARAM_USER_DEFINED)
                    return(invisible())
                }

                if (!.silent) {
                    warning("'hazardRatio' (", .arrayToString(hazardRatio),
                        ") will be ignored because it will be calculated",
                        call. = FALSE
                    )
                }
            }

            if (any(is.na(lambda2))) {
                stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'lambda2' must be specified")
            }

            if (any(is.na(lambda1))) {
                if (delayedResponseAllowed && any(is.na(hazardRatio) &&
                        !any(is.na(piecewiseSurvivalTime)) && length(lambda2) == length(piecewiseSurvivalTime))) {
                    stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'hazardRatio' must be specified")
                }
                if (any(is.na(hazardRatio))) {
                    stop(
                        C_EXCEPTION_TYPE_MISSING_ARGUMENT,
                        "'hazardRatio', 'lambda1' or 'median1' must be specified"
                    )
                }
                stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'lambda1' must be specified")
            }

            .setParameterType("lambda1", C_PARAM_USER_DEFINED)

            hr <- unique(round(lambda1 / lambda2, 8)^kappa)
            if (length(hr) != 1) {
                if (length(lambda2) == 1 && length(lambda1) > 1) {
                    hazardRatio <<- (lambda1 / lambda2)^kappa
                    .setParameterType("hazardRatio", C_PARAM_GENERATED)
                    return(invisible())
                } else if (delayedResponseAllowed) {
                    hazardRatio <<- (lambda1 / lambda2)^kappa
                    .setParameterType("hazardRatio", C_PARAM_GENERATED)
                    delayedResponseEnabled <<- TRUE
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

            hazardRatio <<- ((lambda1 / lambda2)^kappa)[1]
            .setParameterType("hazardRatio", C_PARAM_GENERATED)
        },
        .validateInitialization = function() {
            if (length(piecewiseSurvivalTime) == 0) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'piecewiseSurvivalTime' must contain at least one survival start time"
                )
            }

            if (any(is.na(piecewiseSurvivalTime))) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'piecewiseSurvivalTime' must contain valid survival start times"
                )
            }

            if (piecewiseSurvivalTime[1] != 0) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "the first value of 'piecewiseSurvivalTime' must be 0"
                )
            }

            if (length(piecewiseSurvivalTime) != length(lambda2)) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "length of 'piecewiseSurvivalTime' (", length(piecewiseSurvivalTime),
                    ") and length of 'lambda2' (", length(lambda2), ") must be equal"
                )
            }

            .assertValuesAreStrictlyIncreasing(piecewiseSurvivalTime, "piecewiseSurvivalTime")

            if ((length(lambda1) != 1 || is.na(lambda1)) &&
                    !(.getParameterType("lambda1") %in% c(C_PARAM_GENERATED, C_PARAM_USER_DEFINED))) {
                if (length(hazardRatio) == 1 && !is.na(hazardRatio)) {
                    lambda1 <<- lambda2 * hazardRatio^(1 / kappa)
                    .setParameterType("lambda1", C_PARAM_GENERATED)
                } else if (length(hazardRatio) > 1 && delayedResponseAllowed &&
                        !is.na(hazardRatio[1])) {
                    if (!delayedResponseEnabled && .isLambdaBased()) {
                        if (delayedResponseAllowed) {
                            if (length(hazardRatio) != length(lambda2)) {
                                stop(
                                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                                    "length of 'hazardRatio' (", length(hazardRatio),
                                    ") and length of 'lambda2' (", length(lambda2), ") must be equal"
                                )
                            }
                            delayedResponseEnabled <<- TRUE
                        } else {
                            warning("Only the first 'hazardRatio' (", round(hazardRatio[1], 4),
                                ") was used for piecewise survival time definition",
                                call. = FALSE
                            )
                            hazardRatio <<- hazardRatio[1]
                        }
                        lambda1 <<- lambda2 * hazardRatio^(1 / kappa)
                        .setParameterType("lambda1", C_PARAM_GENERATED)
                    }
                } else if (!delayedResponseEnabled && !(length(lambda2) == 1 && length(lambda1) > 1)) {
                    if (length(lambda1) > 1) {
                        warning("'lambda1' (", .arrayToString(lambda1),
                            ") will be ignored",
                            call. = FALSE
                        )
                    }
                    lambda1 <<- NA_real_
                    .setParameterType("lambda1", C_PARAM_NOT_APPLICABLE)
                }
            } else if (length(hazardRatio) == 1 && !is.na(hazardRatio) &&
                    length(lambda1) > 0 && !any(is.na(lambda1)) &&
                    length(lambda2) > 0 && !any(is.na(lambda2))) {
                target <- lambda2 * hazardRatio^(1 / kappa)
                if (length(lambda1) > 0 && !all(is.na(lambda1)) &&
                        !isTRUE(all.equal(target, lambda1))) {
                    stop(
                        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                        "'lambda1' (", .arrayToString(lambda1), ") ",
                        "is not as expected (", .arrayToString(target), ") for given hazard ratio ", hazardRatio
                    )
                }
            }

            if (piecewiseSurvivalEnabled && !(length(lambda1) == 1 && is.na(lambda1)) &&
                    length(piecewiseSurvivalTime) != length(lambda1)) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "length of 'piecewiseSurvivalTime' (", length(piecewiseSurvivalTime),
                    ") and length of 'lambda1' (", length(lambda1), ") must be equal"
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
AccrualTime <- setRefClass("AccrualTime",
    contains = "TimeDefinition",
    fields = list(
        .showWarnings = "logical",
        endOfAccrualIsUserDefined = "logical",
        followUpTimeMustBeUserDefined = "logical",
        maxNumberOfSubjectsIsUserDefined = "logical",
        maxNumberOfSubjectsCanBeCalculatedDirectly = "logical",
        absoluteAccrualIntensityEnabled = "logical",
        accrualTime = "numeric",
        accrualIntensity = "numeric",
        accrualIntensityRelative = "numeric",
        maxNumberOfSubjects = "numeric",
        remainingTime = "numeric",
        piecewiseAccrualEnabled = "logical"
    ),
    methods = list(
        initialize = function(accrualTime = NA_real_,
                ...,
                accrualIntensity = NA_real_,
                maxNumberOfSubjects = NA_real_,
                showWarnings = TRUE,
                absoluteAccrualIntensityEnabled = NA) {
            callSuper(
                accrualTime = NA_real_,
                accrualIntensity = accrualIntensity,
                maxNumberOfSubjects = maxNumberOfSubjects,
                .showWarnings = showWarnings,
                absoluteAccrualIntensityEnabled = absoluteAccrualIntensityEnabled, ...
            )

            endOfAccrualIsUserDefined <<- NA
            followUpTimeMustBeUserDefined <<- NA
            maxNumberOfSubjectsIsUserDefined <<- NA
            maxNumberOfSubjectsCanBeCalculatedDirectly <<- TRUE
            # absoluteAccrualIntensityEnabled <<- NA
            .setParameterType("endOfAccrualIsUserDefined", C_PARAM_GENERATED)
            .setParameterType("followUpTimeMustBeUserDefined", C_PARAM_GENERATED)
            .setParameterType("maxNumberOfSubjectsIsUserDefined", C_PARAM_GENERATED)
            .setParameterType("maxNumberOfSubjectsCanBeCalculatedDirectly", C_PARAM_GENERATED)
            .setParameterType(
                "absoluteAccrualIntensityEnabled",
                ifelse(is.na(absoluteAccrualIntensityEnabled), C_PARAM_GENERATED, C_PARAM_USER_DEFINED)
            )

            accrualIntensityRelative <<- NA_real_
            .setParameterType("accrualIntensityRelative", C_PARAM_NOT_APPLICABLE)
            remainingTime <<- NA_real_

            .init(accrualTime)

            # case 6 correction
            if (!endOfAccrualIsUserDefined && maxNumberOfSubjectsIsUserDefined &&
                    !.self$absoluteAccrualIntensityEnabled) {
                remainingTime <<- NA_real_
                .setParameterType("remainingTime", C_PARAM_NOT_APPLICABLE)
                .self$accrualTime <<- .self$accrualTime[1:length(.self$accrualIntensity)]
            }

            .initAccrualIntensityAbsolute()
            .validateFormula()
            .showWarningIfCaseIsNotAllowd()
        },
        .asDataFrame = function() {
            accrualIntensityTemp <- accrualIntensity
            if (!all(is.na(accrualIntensityRelative))) {
                accrualIntensityTemp <- accrualIntensityRelative
            }
            if (length(accrualIntensityTemp) + 1 == length(accrualTime)) {
                accrualIntensityTemp <- c(accrualIntensityTemp, NA_real_)
            }
            data <- data.frame(
                accrualTime = accrualTime,
                accrualIntensity = accrualIntensityTemp
            )
            rownames(data) <- as.character(1:nrow(data))
            colnames(data) <- c(
                "Start time",
                C_PARAMETER_NAMES["accrualIntensity"]
            )
            return(data)
        },
        show = function(showType = 1, digits = NA_integer_) {
            .show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
        },
        .isAbsoluteAccrualIntensity = function(x) {
            return(!.isRelativeAccrualIntensity(x))
        },
        .isRelativeAccrualIntensity = function(x) {
            return(all(x < 1))
        },
        .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
            "Method for automatically printing accrual time objects"
            .resetCat()
            if (showType == 2) {
                callSuper(showType = showType, digits = digits, consoleOutputEnabled = consoleOutputEnabled)
            } else {
                .cat("Accrual time and intensity:\n",
                    sep = "", heading = 1,
                    consoleOutputEnabled = consoleOutputEnabled
                )
                if (!isAccrualTimeEnabled()) {
                    .cat("  Accrual time is disabled.\n", consoleOutputEnabled = consoleOutputEnabled)
                } else if (length(accrualTime) == 1) {
                    .cat("  At all times:", accrualIntensity[1], "\n", consoleOutputEnabled = consoleOutputEnabled)
                } else {
                    accrualTimeStr <- format(accrualTime)
                    accrualIntensityStr <- format(accrualIntensity)
                    for (i in 1:length(accrualTime)) {
                        prefix <- ifelse(i == length(accrualTime) - 1, "<=", " <")
                        suffix <- ""
                        if (!maxNumberOfSubjectsIsUserDefined) {
                            suffix <- " "
                        }
                        if (i < length(accrualTime)) {
                            .cat("  ", accrualTimeStr[i], " - ", prefix, accrualTimeStr[i + 1], suffix, ": ",
                                accrualIntensityStr[i], "\n",
                                consoleOutputEnabled = consoleOutputEnabled
                            )
                        } else if (!maxNumberOfSubjectsIsUserDefined && !is.na(accrualIntensityStr[i]) &&
                                accrualIntensityStr[i] != "NA") {
                            .cat("  ", accrualTimeStr[i], " - <=[?]: ",
                                accrualIntensityStr[i], "\n",
                                consoleOutputEnabled = consoleOutputEnabled
                            )
                        }
                    }
                    .cat("", consoleOutputEnabled = consoleOutputEnabled)
                }
                .cat("\n", consoleOutputEnabled = consoleOutputEnabled)

                if (isAccrualTimeEnabled()) {
                    .showFormula(consoleOutputEnabled = consoleOutputEnabled)
                    .cat("\n", consoleOutputEnabled = consoleOutputEnabled)

                    .showCase(consoleOutputEnabled = consoleOutputEnabled)
                    .cat("\n", consoleOutputEnabled = consoleOutputEnabled)
                }

                .cat("Details:\n\n", sep = "", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
                .showParametersOfOneGroup(.getUserDefinedParameters(), "User defined parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                .showParametersOfOneGroup(.getDefaultParameters(), "Default parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                .showParametersOfOneGroup(.getGeneratedParameters(), "Generated parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                .showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)
            }
        },
        .getFormula = function() {
            s <- ""
            for (i in 1:length(accrualTime)) {
                if (i < length(accrualTime)) {
                    s <- paste0(
                        s, (round(accrualTime[i + 1], 4) - round(accrualTime[i], 4)),
                        " * ", round(accrualIntensity[i], 4)
                    )
                    if (!absoluteAccrualIntensityEnabled &&
                            (!maxNumberOfSubjectsIsUserDefined || !endOfAccrualIsUserDefined)) {
                        s <- paste0(s, " * c ")
                    }
                    if (i < length(accrualIntensity)) {
                        s <- paste0(s, " + ")
                    }
                }
            }
            return(s)
        },
        .validateFormula = function() {
            if (is.na(maxNumberOfSubjects) || length(accrualTime) != length(accrualIntensity) + 1) {
                return(invisible())
            }

            numberOfSubjects <- 0
            for (i in 1:length(accrualTime)) {
                if (i < length(accrualTime)) {
                    numberOfSubjects <- numberOfSubjects +
                        (accrualTime[i + 1] - accrualTime[i]) * accrualIntensity[i]
                }
            }
            if (!isTRUE(all.equal(numberOfSubjects, maxNumberOfSubjects, tolerance = 1e-03)) &&
                    absoluteAccrualIntensityEnabled) {
                stop(
                    C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                    "'maxNumberOfSubjects' (", maxNumberOfSubjects, ") disagrees with ",
                    "the defined accrual time and intensity: ",
                    .getFormula(), " = ", numberOfSubjects
                )
            }
        },
        .showWarningIfCaseIsNotAllowd = function() {
            caseIsAllowed <- TRUE
            if (!endOfAccrualIsUserDefined && maxNumberOfSubjectsIsUserDefined &&
                    !absoluteAccrualIntensityEnabled) {
                caseIsAllowed <- FALSE
            } else if (!endOfAccrualIsUserDefined && !maxNumberOfSubjectsIsUserDefined &&
                    followUpTimeMustBeUserDefined && !absoluteAccrualIntensityEnabled) {
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
            .cat("Formula:\n", sep = "", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
            .cat("  ", consoleOutputEnabled = consoleOutputEnabled)
            .cat("maxNumberOfSubjects = ", consoleOutputEnabled = consoleOutputEnabled)
            if (!is.na(maxNumberOfSubjects)) {
                .cat(maxNumberOfSubjects, " = ", consoleOutputEnabled = consoleOutputEnabled)
            }
            .cat(.getFormula(), consoleOutputEnabled = consoleOutputEnabled)
            if (length(accrualTime) == length(accrualIntensity)) {
                .cat("(x - ", accrualTime[length(accrualTime)], ") * ",
                    accrualIntensity[length(accrualIntensity)],
                    consoleOutputEnabled = consoleOutputEnabled
                )
                if (!absoluteAccrualIntensityEnabled &&
                        (!maxNumberOfSubjectsIsUserDefined || !endOfAccrualIsUserDefined)) {
                    .cat(" * c ", consoleOutputEnabled = consoleOutputEnabled)
                }
                .cat(", where 'x' is the unknown last accrual time",
                    consoleOutputEnabled = consoleOutputEnabled
                )
                if (!absoluteAccrualIntensityEnabled &&
                        (!maxNumberOfSubjectsIsUserDefined || !endOfAccrualIsUserDefined)) {
                    .cat(" and 'c' a constant factor", consoleOutputEnabled = consoleOutputEnabled)
                }
            } else if (!absoluteAccrualIntensityEnabled &&
                    (!maxNumberOfSubjectsIsUserDefined || !endOfAccrualIsUserDefined)) {
                .cat(", where 'c' is a constant factor", consoleOutputEnabled = consoleOutputEnabled)
            }
            .cat("\n", consoleOutputEnabled = consoleOutputEnabled)
        },
        .showCase = function(consoleOutputEnabled = TRUE) {
            caseIsAllowed <- TRUE

            prefix <- "  "

            # Case 1
            # example: getAccrualTime(accrualTime = c(0, 6, 30), accrualIntensity = c(22, 33),
            #          maxNumberOfSubjects = 1000)
            if (endOfAccrualIsUserDefined && maxNumberOfSubjectsIsUserDefined &&
                    absoluteAccrualIntensityEnabled) {
                .cat("Case (#1):\n", sep = "", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
                .cat(prefix, "End of accrual, absolute accrual intensity and 'maxNumberOfSubjects' are given, ",
                    " 'followUpTime'** shall be calculated.\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
                .cat(prefix, "Example: getAccrualTime(accrualTime = c(0, 6, 30), ",
                    "accrualIntensity = c(22, 33), maxNumberOfSubjects = 924)\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
            }

            # Case 2
            # example: getAccrualTime(accrualTime = c(0, 6, 30), accrualIntensity = c(0.22, 0.33),
            #          maxNumberOfSubjects = 1000)
            else if (endOfAccrualIsUserDefined && maxNumberOfSubjectsIsUserDefined &&
                    !absoluteAccrualIntensityEnabled) {
                .cat("Case (#2):\n", sep = "", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
                .cat(prefix, "End of accrual, relative accrual intensity and 'maxNumberOfSubjects' are given, ",
                    "absolute accrual intensity* and 'followUpTime'** shall be calculated.\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
                .cat(prefix, "Example: getAccrualTime(accrualTime = c(0, 6, 30), ",
                    "accrualIntensity = c(0.22, 0.33), maxNumberOfSubjects = 1000)\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
            }

            # Case 3
            # example: getAccrualTime(accrualTime = c(0, 6, 30), accrualIntensity = c(22, 33))
            else if (endOfAccrualIsUserDefined && !maxNumberOfSubjectsIsUserDefined &&
                    absoluteAccrualIntensityEnabled) {
                .cat("Case (#3):\n", sep = "", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
                .cat(prefix, "End of accrual and absolute accrual intensity are given, ",
                    "'maxNumberOfSubjects'* and 'followUpTime'** shall be calculated.\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
                .cat(prefix, "Example: getAccrualTime(accrualTime = c(0, 6, 30), accrualIntensity = c(22, 33))\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
            }

            # Case 4
            # example: getAccrualTime(accrualTime = c(0, 6, 30), accrualIntensity = c(0.22, 0.33))
            else if (endOfAccrualIsUserDefined && !maxNumberOfSubjectsIsUserDefined &&
                    !absoluteAccrualIntensityEnabled) {
                .cat("Case (#4):\n", sep = "", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
                .cat(prefix, "End of accrual, relative accrual intensity and 'followUpTime' are given, ",
                    "absolute accrual intensity** and 'maxNumberOfSubjects'** shall be calculated.\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
                .cat(prefix, "Example: getAccrualTime(accrualTime = c(0, 6, 30), accrualIntensity = c(0.22, 0.33))\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
            }

            # Case 5
            # example: getAccrualTime(accrualTime = c(0, 6), accrualIntensity = c(22, 33),
            #          maxNumberOfSubjects = 1000)
            else if (!endOfAccrualIsUserDefined && maxNumberOfSubjectsIsUserDefined &&
                    absoluteAccrualIntensityEnabled) {
                .cat("Case (#5):\n", sep = "", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
                .cat(prefix, "'maxNumberOfSubjects' and absolute accrual intensity are given, ",
                    "end of accrual* and 'followUpTime'** shall be calculated\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
                .cat(prefix, "Example: getAccrualTime(accrualTime = c(0, 6), ",
                    "accrualIntensity = c(22, 33), maxNumberOfSubjects = 1000)\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
            }

            # Case 6
            # example: getAccrualTime(accrualTime = c(0, 6), accrualIntensity = c(0.22, 0.33),
            #          maxNumberOfSubjects = 1000)
            else if (!endOfAccrualIsUserDefined && maxNumberOfSubjectsIsUserDefined &&
                    !absoluteAccrualIntensityEnabled) {
                caseIsAllowed <- FALSE
                .cat("Case (#6):\n", sep = "", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
                .cat(prefix, "'maxNumberOfSubjects' and relative accrual intensity are given, ",
                    "absolute accrual intensity@, end of accrual* and 'followUpTime'** shall be calculated\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
                .cat(prefix, "Example: getAccrualTime(accrualTime = c(0, 6), ",
                    "accrualIntensity = c(0.22, 0.33), maxNumberOfSubjects = 1000)\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
            }

            # Case 7
            # example: getAccrualTime(accrualTime = c(0, 6), accrualIntensity = c(22, 33))
            else if (!endOfAccrualIsUserDefined && !maxNumberOfSubjectsIsUserDefined &&
                    followUpTimeMustBeUserDefined && absoluteAccrualIntensityEnabled) {
                .cat("Case (#7):\n", sep = "", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
                .cat(prefix, "'followUpTime' and absolute accrual intensity are given, ",
                    "end of accrual** and 'maxNumberOfSubjects'** shall be calculated\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
                .cat(prefix, "Example: getAccrualTime(accrualTime = c(0, 6), accrualIntensity = c(22, 33))\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
            }

            # Case 8
            # example: getAccrualTime(accrualTime = c(0, 6), accrualIntensity = c(0.22, 0.33))
            else if (!endOfAccrualIsUserDefined && !maxNumberOfSubjectsIsUserDefined &&
                    followUpTimeMustBeUserDefined && !absoluteAccrualIntensityEnabled) {
                caseIsAllowed <- FALSE
                .cat("Case (#8):\n", sep = "", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
                .cat(prefix, "'followUpTime' and relative accrual intensity are given, ",
                    "absolute accrual intensity@, end of accrual and 'maxNumberOfSubjects' shall be calculated\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
                .cat(prefix, "Example: getAccrualTime(accrualTime = c(0, 6), accrualIntensity = c(0.22, 0.33))\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
            }

            # .cat("\n", consoleOutputEnabled = consoleOutputEnabled)
            if (!caseIsAllowed) {
                .cat(prefix, "(@) Cannot be calculated.\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
            }
            .cat(prefix, "(*) Can be calculated directly.\n",
                consoleOutputEnabled = consoleOutputEnabled
            )
            .cat(prefix, "(**) Cannot be calculated directly but with ",
                "'getSampleSizeSurvival()' or 'getPowerSurvival()'.\n",
                consoleOutputEnabled = consoleOutputEnabled
            )
        },
        .followUpTimeShallBeCalculated = function() {
            # Case 1: 'followUpTime'** shall be calculated
            if (endOfAccrualIsUserDefined && maxNumberOfSubjectsIsUserDefined &&
                    absoluteAccrualIntensityEnabled) {
                return(TRUE)
            }

            # Case 2: 'followUpTime'** shall be calculated
            else if (endOfAccrualIsUserDefined && maxNumberOfSubjectsIsUserDefined &&
                    !absoluteAccrualIntensityEnabled) {
                return(TRUE)
            }

            # Case 3: 'followUpTime'** shall be calculated
            else if (endOfAccrualIsUserDefined && !maxNumberOfSubjectsIsUserDefined &&
                    absoluteAccrualIntensityEnabled) {
                return(TRUE)
            }


            # Case 5: 'followUpTime'** shall be calculated
            else if (!endOfAccrualIsUserDefined && maxNumberOfSubjectsIsUserDefined &&
                    absoluteAccrualIntensityEnabled) {
                return(TRUE)
            }

            # Case 6: 'followUpTime'** shall be calculated
            else if (!endOfAccrualIsUserDefined && maxNumberOfSubjectsIsUserDefined &&
                    !absoluteAccrualIntensityEnabled) {
                return(TRUE)
            }

            # (**) Cannot be calculated directly but with 'getSampleSizeSurvival()' or 'getPowerSurvival()'

            return(FALSE)
        },
        .validate = function() {
            # Case 6
            if (!endOfAccrualIsUserDefined && maxNumberOfSubjectsIsUserDefined &&
                    !absoluteAccrualIntensityEnabled) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "the calculation of 'followUpTime' for given 'maxNumberOfSubjects' ",
                    "and relative accrual intensities (< 1) ",
                    "can only be done if end of accrual is defined"
                )
            }

            # Case 8
            else if (!endOfAccrualIsUserDefined && !maxNumberOfSubjectsIsUserDefined &&
                    followUpTimeMustBeUserDefined && !absoluteAccrualIntensityEnabled) {
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
            if (length(accrualTime) <= 1) {
                return(NA_real_)
            }

            return(accrualTime[2:length(accrualTime)])
        },
        isAccrualTimeEnabled = function() {
            if (length(accrualTime) == 0) {
                return(FALSE)
            }

            if (length(accrualTime) == 1 && is.na(accrualTime)) {
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

            if (.showWarnings && !all(is.na(accrualIntensity)) && (length(accrualIntensity) != 1 ||
                    accrualIntensity != C_ACCRUAL_INTENSITY_DEFAULT)) {
                warning("'accrualIntensity' (", .arrayToString(accrualIntensity),
                    ") will be ignored because 'accrualTime' is a list",
                    call. = FALSE
                )
            }

            accrualTime <<- numeric(0)
            accrualIntensity <<- numeric(0)
            timeRegions <- names(accrualTimeList)
            endOfAccrualIsUndefined <- FALSE
            accrualTime <<- c(accrualTime, 0)
            for (i in 1:length(timeRegions)) {
                timePeriod <- timeRegions[i]
                accrualTimeValue <- accrualTimeList[[timePeriod]]
                .assertIsSingleNumber(accrualTimeValue, paste0("accrualTime[", i, "]"))

                settings <- .validateTimePeriod(timePeriod, i = i, n = length(timeRegions), accrualTimeMode = TRUE)
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
                    accrualTime <<- c(accrualTime, as.numeric(trimws(parts[2])))
                } else {
                    parts <- strsplit(timePeriod, " *< *", perl = TRUE)[[1]]
                    if (length(parts) == 2) {
                        accrualTime <<- c(accrualTime, as.numeric(trimws(parts[2])))
                    }
                }
                accrualIntensity <<- c(accrualIntensity, accrualTimeValue)
            }

            .setParameterType("accrualTime", C_PARAM_USER_DEFINED)
            .setParameterType("accrualIntensity", C_PARAM_USER_DEFINED)

            return(endOfAccrualIsUndefined = endOfAccrualIsUndefined)
        },
        .initAccrualIntensityAbsolute = function() {
            if (is.null(maxNumberOfSubjects) || length(maxNumberOfSubjects) != 1 ||
                    is.na(maxNumberOfSubjects) || maxNumberOfSubjects == 0) {
                return(invisible())
            }

            if (!endOfAccrualIsUserDefined && maxNumberOfSubjectsIsUserDefined &&
                    !absoluteAccrualIntensityEnabled) {
                return(invisible()) # case 6
            }

            if (length(accrualTime) >= 2 && length(accrualTime) == length(accrualIntensity) + 1 &&
                    !any(is.na(accrualTime)) && !any(is.na(accrualIntensity))) {
                len <- length(accrualIntensity)
                accrualIntensityAbsolute <- maxNumberOfSubjects / sum((accrualTime[2:(len + 1)] -
                    accrualTime[1:len]) * accrualIntensity) * accrualIntensity
                if (!isTRUE(all.equal(accrualIntensityAbsolute, accrualIntensity, tolerance = 1e-06)) &&
                        !isTRUE(all.equal(accrualIntensityAbsolute, 0, tolerance = 1e-06))) {
                    .validateAccrualTimeAndIntensity()

                    if (absoluteAccrualIntensityEnabled &&
                            .getParameterType("maxNumberOfSubjects") == C_PARAM_USER_DEFINED) {
                        if (.getParameterType("accrualTime") == C_PARAM_DEFAULT_VALUE) {
                            accrualTime <<- maxNumberOfSubjects / accrualIntensity
                            .setParameterType("accrualTime", C_PARAM_GENERATED)
                            remainingTime <<- accrualTime
                            accrualTime <<- c(0, accrualTime)
                        } else {
                            stop(
                                C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                                "'maxNumberOfSubjects' (", maxNumberOfSubjects, ") disagrees with ",
                                "the defined accrual time (", .arrayToString(accrualTime), ") and intensity: ",
                                .getFormula(), " = ", .getSampleSize()
                            )
                        }
                    } else {
                        if (!absoluteAccrualIntensityEnabled && # .isRelativeAccrualIntensity(accrualIntensity)
                                .getParameterType("accrualIntensity") == C_PARAM_USER_DEFINED &&
                                .getParameterType("accrualTime") == C_PARAM_DEFAULT_VALUE &&
                                .getParameterType("maxNumberOfSubjects") == C_PARAM_USER_DEFINED) {
                            if (.showWarnings) {
                                warning("'accrualIntensity' (", accrualIntensity, ") will be ignored", call. = FALSE)
                            }
                            accrualIntensityRelative <<- C_ACCRUAL_INTENSITY_DEFAULT
                            accrualIntensity <<- accrualIntensityAbsolute
                            .setParameterType("accrualIntensity", C_PARAM_GENERATED)
                            .setParameterType("remainingTime", C_PARAM_NOT_APPLICABLE)
                        } else {
                            accrualIntensityRelative <<- accrualIntensity
                            accrualIntensity <<- accrualIntensityAbsolute
                            .setParameterType("accrualIntensity", C_PARAM_GENERATED)
                            .setParameterType("accrualIntensityRelative", C_PARAM_USER_DEFINED)
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
                endOfAccrualIsUndefined <- .initFromList(accrualTimeArg)
                calculateLastAccrualTimeEnabled <- endOfAccrualIsUndefined &&
                    !is.null(maxNumberOfSubjects) && length(maxNumberOfSubjects) == 1 &&
                    !is.na(maxNumberOfSubjects)
            } else if (is.numeric(accrualTimeArg)) {
                .assertIsNumericVector(accrualTimeArg, "accrualTime")
                if (length(accrualIntensity) > 1) {
                    .assertIsNumericVector(accrualIntensity, "accrualIntensity")
                }

                if (.isNoPiecewiseAccrualTime(accrualTimeArg) &&
                        (length(accrualIntensity) == 0 || is.null(accrualIntensity) ||
                            all(is.na(accrualIntensity)) ||
                            all(accrualIntensity == C_ACCRUAL_INTENSITY_DEFAULT))) {
                    accrualTimeArg <- accrualTimeArg[length(accrualTimeArg)]
                    accrualTime <<- c(0L, accrualTimeArg)
                    .setParameterType("accrualTime", ifelse(
                        identical(as.integer(accrualTime), C_ACCRUAL_TIME_DEFAULT),
                        C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
                    ))

                    accrualIntensity <<- C_ACCRUAL_INTENSITY_DEFAULT
                    .setParameterType("accrualIntensity", C_PARAM_DEFAULT_VALUE)

                    .setParameterType(
                        "maxNumberOfSubjects",
                        ifelse(length(maxNumberOfSubjects) == 1 && is.na(maxNumberOfSubjects),
                            C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
                        )
                    )

                    endOfAccrualIsUserDefined <<- length(accrualTime) == length(accrualIntensity) + 1
                    maxNumberOfSubjectsIsUserDefined <<-
                        .getParameterType("maxNumberOfSubjects") == C_PARAM_USER_DEFINED
                    followUpTimeMustBeUserDefined <<- !endOfAccrualIsUserDefined &&
                        !maxNumberOfSubjectsIsUserDefined
                    absoluteAccrualIntensityEnabled <<- FALSE

                    if (maxNumberOfSubjectsIsUserDefined) {
                        accrualIntensity <<- maxNumberOfSubjects / accrualTime[length(accrualTime)]
                        .setParameterType("accrualIntensity", C_PARAM_GENERATED)
                    }

                    return(invisible())
                }

                accrualTime <<- accrualTimeArg
                if (length(accrualTime) == 0) {
                    stop(
                        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                        "'accrualTime' must contain at least one time value"
                    )
                }
                
                if (accrualTime[1] != 0) { 
                    stop(
                        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                        "the first value of 'accrualTime' (", .arrayToString(accrualTime), ") must be 0"
                    )
                }

                .setParameterType("accrualTime", ifelse(
                    identical(as.integer(accrualTime), C_ACCRUAL_TIME_DEFAULT),
                    C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
                ))
                .setParameterType("accrualIntensity", C_PARAM_USER_DEFINED)
            } else {
                stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'accrualTime' must be a list or a numeric vector")
            }

            if (is.na(absoluteAccrualIntensityEnabled)) {
                absoluteAccrualIntensityEnabled <<- .isAbsoluteAccrualIntensity(accrualIntensity)
            }
            if (is.null(maxNumberOfSubjects) || length(maxNumberOfSubjects) == 0 ||
                    any(is.na(maxNumberOfSubjects))) {
                if (length(accrualTime) != length(accrualIntensity) + 1 ||
                        !absoluteAccrualIntensityEnabled) {
                    maxNumberOfSubjectsCanBeCalculatedDirectly <<- FALSE
                }

                .setParameterType("maxNumberOfSubjects", C_PARAM_NOT_APPLICABLE)
            } else {
                if (!(length(accrualTime) %in% c(
                        length(accrualIntensity),
                        length(accrualIntensity) + 1
                    ))) {
                    stop(
                        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                        "length of 'accrualTime' (", length(accrualTime),
                        ") must be equal to length of 'accrualIntensity' if the last 'accrualTime' ",
                        "shall be calculated ",
                        "based on 'maxNumberOfSubjects' or length of 'accrualIntensity' (",
                        length(accrualIntensity), ") + 1 otherwise"
                    )
                }
                if (length(accrualTime) == length(accrualIntensity)) {
                    calculateLastAccrualTimeEnabled <- TRUE
                }

                .setParameterType("maxNumberOfSubjects", C_PARAM_USER_DEFINED)
            }

            endOfAccrualIsUserDefined <<- length(accrualTime) == length(accrualIntensity) + 1

            if (calculateLastAccrualTimeEnabled) {
                .calculateRemainingTime()
            } else if (maxNumberOfSubjectsCanBeCalculatedDirectly) {
                if (length(accrualTime) == 1) {
                    if (length(maxNumberOfSubjects) > 0 && !is.na(maxNumberOfSubjects) &&
                            maxNumberOfSubjects > 0 && maxNumberOfSubjects < accrualIntensity[1]) {
                        stop(
                            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                            "'maxNumberOfSubjects' (", maxNumberOfSubjects, ") ",
                            "must be >= ", accrualIntensity[1], " ('accrualIntensity')"
                        )
                    }
                    remainingTime <<- accrualTime
                    .setParameterType("remainingTime", C_PARAM_USER_DEFINED)
                } else if (length(accrualTime) > 1) {
                    sampleSize <- .getSampleSize()
                    if (!isTRUE(all.equal(sampleSize, maxNumberOfSubjects, tolerance = 1e-04))) {
                        if (length(maxNumberOfSubjects) == 1 && !is.na(maxNumberOfSubjects) &&
                                maxNumberOfSubjects > 0 && maxNumberOfSubjects < sampleSize) {
                            if (length(accrualIntensity) == 1 && length(accrualTime) == 1) {
                                .setParameterType("maxNumberOfSubjects", C_PARAM_USER_DEFINED)
                                accrualTime <<- 0
                                .calculateRemainingTime()
                            } else {
                                if (length(accrualTime) == length(accrualIntensity) + 1 &&
                                        absoluteAccrualIntensityEnabled) {
                                    stop(
                                        C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                                        "'maxNumberOfSubjects' (", maxNumberOfSubjects, ") disagrees with ",
                                        "the defined accrual time and intensity: ",
                                        .getFormula(), " = ", sampleSize
                                    )
                                } else {
                                    stop(
                                        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'maxNumberOfSubjects' (",
                                        maxNumberOfSubjects, ") ", "must be >= ", sampleSize
                                    )
                                }
                            }
                        } else {
                            if ((length(maxNumberOfSubjects) != 1 || is.na(maxNumberOfSubjects)) &&
                                    absoluteAccrualIntensityEnabled) {
                                maxNumberOfSubjects <<- sampleSize
                                .setParameterType("maxNumberOfSubjects", C_PARAM_GENERATED)
                            }
                            remainingTime <<- accrualTime[length(accrualTime)] - accrualTime[length(accrualTime) - 1]
                            .setParameterType(
                                "remainingTime",
                                ifelse(!isTRUE(all.equal(0, remainingTime, tolerance = 1e-06)),
                                    C_PARAM_GENERATED, C_PARAM_NOT_APPLICABLE
                                )
                            )
                        }
                    }
                }
            }

            .validateInitialization()

            maxNumberOfSubjectsIsUserDefined <<- .getParameterType("maxNumberOfSubjects") == C_PARAM_USER_DEFINED
            followUpTimeMustBeUserDefined <<- !endOfAccrualIsUserDefined && !maxNumberOfSubjectsIsUserDefined
        },
        .getSampleSize = function() {
            if (length(accrualTime) < 2) {
                return(0)
            }

            sampleSize <- 0
            for (i in 2:length(accrualTime)) {
                time <- accrualTime[i] - accrualTime[i - 1]
                sampleSize <- sampleSize + time * accrualIntensity[i - 1]
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
            return(min(.getValuesAfterDecimalPoint(x[x > 0])))
        },
        .calcSampleSize = function() {
            if (length(accrualTime) <= 1) {
                return(0)
            }

            accrualTimeTemp <- accrualTime
            accrualIntensityTemp <- accrualIntensity

            sampleSize <- 0
            for (i in 2:length(accrualTime)) {
                time <- accrualTime[i] - accrualTime[i - 1]
                sampleSize <- sampleSize + time * accrualIntensity[i - 1]
                if (sampleSize >= maxNumberOfSubjects &&
                        length(accrualTime) == length(accrualIntensity)) {
                    if (sampleSize > maxNumberOfSubjects) {
                        accrualTime <<- accrualTime[1:(i - 1)]
                    }

                    i2 <- i
                    if (length(accrualTime) == length(accrualIntensity) + 1) {
                        i2 <- i - 1
                    }
                    accrualIntensity <<- accrualIntensity[1:(i2 - 1)]

                    while (length(accrualTime) > length(accrualIntensity) + 1) {
                        accrualTime <<- accrualTime[1:(length(accrualTime) - 1)]
                    }

                    sampleSize <- 0
                    if (length(accrualTime) > 1) {
                        sampleSize <- .getSampleSize()
                    }

                    if (.showWarnings) {
                        n1 <- length(accrualTimeTemp) - length(accrualTime)
                        n2 <- length(accrualIntensityTemp) - length(accrualIntensity)

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
            .assertIsValidMaxNumberOfSubjects(maxNumberOfSubjects)

            sampleSize <- .calcSampleSize()
            remainingSubjects <- maxNumberOfSubjects - sampleSize
            if (remainingSubjects < 0) {
                if (!stopInCaseOfError) {
                    return(invisible())
                }
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'maxNumberOfSubjects' (", maxNumberOfSubjects, ") ",
                    "is too small for the defined accrual time (minimum = ", sampleSize, ")"
                )
            }

            lastAccrualIntensity <- accrualIntensity[length(accrualIntensity)]
            remainingTime <<- remainingSubjects / lastAccrualIntensity
            .setParameterType(
                "remainingTime",
                ifelse(!isTRUE(all.equal(0, remainingTime, tolerance = 1e-06)),
                    C_PARAM_GENERATED, C_PARAM_NOT_APPLICABLE
                )
            )
            if (length(accrualTime) == length(accrualIntensity)) {
                accrualTime <<- c(accrualTime, accrualTime[length(accrualTime)] + remainingTime)
            }
            # .setParameterType("accrualTime", C_PARAM_GENERATED)
            if (any(accrualTime < 0)) {
                if (!stopInCaseOfError) {
                    return(invisible())
                }
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'maxNumberOfSubjects' (", maxNumberOfSubjects, ") ",
                    "is too small for the defined accrual time"
                )
            }
        },
        .validateAccrualTimeAndIntensity = function() {
            if ((length(accrualTime) >= 2 && any(accrualTime[2:length(accrualTime)] < 0))) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'accrualTime' (", .arrayToString(accrualTime), ") must be > 0"
                )
            }

            .assertValuesAreStrictlyIncreasing(accrualTime, "accrualTime")

            if ((length(accrualTime) > 1) && any(accrualIntensity < 0)) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'accrualIntensity' (", .arrayToString(accrualIntensity), ") must be >= 0"
                )
            }

            if (length(accrualIntensity) == 1 && !is.na(accrualIntensity) &&
                    accrualIntensity == 0) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "at least one 'accrualIntensity' value must be > 0"
                )
            }

            if (length(accrualIntensity) > 0 && accrualIntensity[1] == 0) {
                warning(
                    "It makes no sense to start 'accrualIntensity' (",
                    .arrayToString(accrualIntensity), ") with 0"
                )
            }
        },
        .validateInitialization = function() {
            .validateAccrualTimeAndIntensity()

            piecewiseAccrualEnabled <<- !.isNoPiecewiseAccrualTime(accrualTime)
        }
    )
)
