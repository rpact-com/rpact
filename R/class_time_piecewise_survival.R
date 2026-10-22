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

#' @include f_core_constants.R
#' @include f_design_general_utilities.R
NULL

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
    private = list(
        hazardRatioIsUserDefined = FALSE
    ),
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
        initialize = function(
                piecewiseSurvivalTime = NA_real_,
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
                stopIllegalArgument(
                    "'piecewiseSurvivalTime' must be defined ", "(set to NA_real_ if not applicable)",
                    functionName = "initialize",
                    parameter = "piecewiseSurvivalTime",
                    value = piecewiseSurvivalTime
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
                self$.setParameterType("lambda1", C_PARAM_DERIVED)
            } else {
                self$.setParameterType("median1", C_PARAM_NOT_APPLICABLE)
                self$.setParameterType("lambda1", ifelse(length(self$lambda1) == 1 && is.na(self$lambda1),
                    C_PARAM_NOT_APPLICABLE, C_PARAM_USER_DEFINED
                ))
            }
            if (length(self$median2) > 0 && !all(is.na(self$median2))) {
                self$lambda2 <- getLambdaByMedian(self$median2, kappa = self$kappa)
                self$.setParameterType("median2", C_PARAM_USER_DEFINED)
                self$.setParameterType("lambda2", C_PARAM_DERIVED)
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
            self$.setParameterType("piecewiseSurvivalEnabled", C_PARAM_DERIVED)
            self$.setParameterType("delayedResponseEnabled", ifelse(isTRUE(self$delayedResponseAllowed),
                C_PARAM_DERIVED, C_PARAM_NOT_APPLICABLE
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

            if (self$isUserDefinedParameter("median1") &&
                    self$isUserDefinedParameter("lambda1")) {
                self$.setParameterType("lambda1", C_PARAM_DERIVED)
            }

            if (self$isUserDefinedParameter("median2") &&
                    self$isUserDefinedParameter("lambda2")) {
                self$.setParameterType("lambda2", C_PARAM_DERIVED)
            }

            if (!is.na(self$eventTime) &&
                    self$isNotUserDefinedParameter("pi1") &&
                    self$.getParameterType("pi1") != C_PARAM_DEFAULT_VALUE &&
                    self$isNotUserDefinedParameter("pi2") &&
                    self$.getParameterType("pi2") != C_PARAM_DEFAULT_VALUE) {
                if (self$isUserDefinedParameter("eventTime")) {
                    warnArgumentIgnored("'eventTime' (", round(self$eventTime, 3),
                        ") has no influence on simulated results",
                        parameter = "eventTime",
                        value = self$eventTime,
                        diagnosticId = "survival.event_time_without_probabilities"
                    )
                }
                self$.setParameterType("eventTime", C_PARAM_NOT_APPLICABLE)
                self$eventTime <- NA_real_
            }

            self$.validateCalculatedArguments()
        },
        .validateCalculatedArguments = function() {
            if (self$isUserDefinedParameter("median1")) {
                if (!equals(getLambdaByMedian(
                        self$median1,
                        kappa = self$kappa
                    ), self$lambda1, tolerance = 1e-05)) {
                    stopRuntimeIssue(
                        "'lambda1' must be ", round(
                            getLambdaByMedian(self$median1, kappa = self$kappa), 5
                        ),
                        ", ", "but is ", round(self$lambda1, 5),
                        functionName = ".validateCalculatedArguments",
                        parameter = "lambda1",
                        value = self$lambda1
                    )
                }
                if (!anyNA(self$pi1) &&
                        !equals(
                            getPiByMedian(self$median1,
                                eventTime = self$eventTime, kappa = self$kappa
                            ),
                            self$pi1,
                            tolerance = 1e-05
                        )) {
                    stopRuntimeIssue(
                        "'pi1' must be ", round(
                            getPiByMedian(self$median1, eventTime = self$eventTime, kappa = self$kappa),
                            5
                        ), ", but is ", round(self$pi1, 5),
                        functionName = ".validateCalculatedArguments",
                        parameter = "pi1",
                        value = self$pi1
                    )
                }
            }

            if (self$isUserDefinedParameter("median2")) {
                if (!equals(getLambdaByMedian(self$median2,
                        kappa = self$kappa
                    ), self$lambda2, tolerance = 1e-05)) {
                    stopRuntimeIssue(
                        "'lambda2' must be ",
                        round(getLambdaByMedian(self$median2, kappa = self$kappa), 5),
                        ", ", "but is ", round(self$lambda2, 5),
                        functionName = ".validateCalculatedArguments",
                        parameter = "lambda2",
                        value = self$lambda2
                    )
                }
                if (!is.na(self$pi2) &&
                        !equals(getPiByMedian(self$median2, eventTime = self$eventTime, kappa = self$kappa),
                            self$pi2,
                            tolerance = 1e-05
                        )) {
                    stopRuntimeIssue(
                        "'pi2' must be ", round(
                            getPiByMedian(self$median2, eventTime = self$eventTime, kappa = self$kappa),
                            5
                        ), ", but is ", round(self$pi2, 5),
                        functionName = ".validateCalculatedArguments",
                        parameter = "pi2",
                        value = self$pi2
                    )
                }
            }

            if (self$isUserDefinedParameter("lambda1") ||
                    self$isUserDefinedParameter("median1") ||
                    self$isUserDefinedParameter("lambda2") ||
                    self$isUserDefinedParameter("median2")) {
                if (!anyNA(self$pi1)) {
                    stopRuntimeIssue(
                        "'pi1' (", self$pi1, ") must be NA_real_",
                        functionName = ".validateCalculatedArguments",
                        parameter = "pi1", value = self$pi1
                    )
                }
                if (self$.getParameterType("pi1") != C_PARAM_NOT_APPLICABLE) {
                    stopRuntimeIssue(
                        "parameter type of 'pi1' (", self$.getParameterType("pi1"),
                        ") must be C_PARAM_NOT_APPLICABLE",
                        functionName = ".validateCalculatedArguments",
                        parameter = "pi1"
                    )
                }
                if (!anyNA(self$pi1)) {
                    stopRuntimeIssue(
                        "'pi2' (", self$pi2, ") must be NA_real_",
                        functionName = ".validateCalculatedArguments",
                        parameter = "pi2",
                        value = self$pi2
                    )
                }
                if (self$.getParameterType("pi2") != C_PARAM_NOT_APPLICABLE) {
                    stopRuntimeIssue(
                        "parameter type of 'pi2' (", self$.getParameterType("pi2"),
                        ") must be C_PARAM_NOT_APPLICABLE",
                        functionName = ".validateCalculatedArguments",
                        parameter = "pi2"
                    )
                }
                if (!anyNA(self$eventTime)) {
                    stopRuntimeIssue(
                        "'eventTime' (", self$eventTime, ") must be NA_real_",
                        functionName = ".validateCalculatedArguments",
                        parameter = "eventTime",
                        value = self$eventTime
                    )
                }
                if (self$.getParameterType("eventTime") != C_PARAM_NOT_APPLICABLE) {
                    stopRuntimeIssue(
                        "parameter type of 'eventTime' (",
                        self$.getParameterType("eventTime"),
                        ") must be C_PARAM_NOT_APPLICABLE",
                        functionName = ".validateCalculatedArguments",
                        parameter = "eventTime"
                    )
                }
            }

            if (self$isUndefinedParameter("hazardRatio")) {
                stopRuntimeIssue(
                    "parameter type of 'hazardRatio' (",
                    self$hazardRatio, ") must be != C_PARAM_TYPE_UNKNOWN",
                    functionName = ".validateCalculatedArguments",
                    parameter = "hazardRatio",
                    value = self$hazardRatio
                )
            }
        },
        .stopInCaseOfConflictingArguments = function(arg1, argName1, arg2, argName2) {
            if (length(arg1) > 0 && !all(is.na(arg1)) && length(arg2) > 0 && !all(is.na(arg2))) {
                stopConflictingArguments(
                    "it is not allowed to specify ", .pQuote(argName1),
                    " (", .arrayToString(arg1), ")",
                    " and ", .pQuote(argName2), " (", .arrayToString(arg2), ") concurrently",
                    functionName = ".stopInCaseOfConflictingArguments",
                    parameter = "argName1",
                    value = argName1,
                    relatedParameter = "argName2",
                    relatedValue = argName2,
                    diagnosticId = "survival.conflicting_parameterizations"
                )
            }
        },
        .asDataFrame = function() {
            data <- data.frame(
                piecewiseSurvivalTime = self$piecewiseSurvivalTime,
                lambda1 = self$lambda1,
                lambda2 = self$lambda2,
                hazardRatio = self$hazardRatio
            )
            colnames(data) <- c(
                "Start time",
                .getParameterCaption("lambda1", tableOutputEnabled = TRUE),
                .getParameterCaption("lambda2", tableOutputEnabled = TRUE),
                .getParameterCaption("hazardRatio", tableOutputEnabled = TRUE)
            )
            rownames(data) <- as.character(1:nrow(data))
            return(data)
        },
        .isPiBased = function() {
            return(!self$.isLambdaBased())
        },
        .isLambdaBased = function(minNumberOfLambdas = 2) {
            if (self$isUserDefinedParameter("lambda2") ||
                    self$isUserDefinedParameter("median2")) {
                if (length(self$lambda2) >= minNumberOfLambdas && !anyNA(self$lambda2)) {
                    return(TRUE)
                }
            }

            return((length(self$pi1) == 0 || anyNA(self$pi1)) && (length(self$pi2) == 0 || anyNA(self$pi2)))
        },
        show = function(showType = 1, digits = NA_integer_) {
            self$.show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
        },
        .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
            "Method for automatically printing piecewise survival time objects"
            self$.resetCat()
            if (showType == 2) {
                super$.show(
                    showType = showType, digits = digits,
                    consoleOutputEnabled = consoleOutputEnabled
                )
            } else {
                self$.cat("Piecewise exponential survival times:\n",
                    sep = "", heading = 1,
                    consoleOutputEnabled = consoleOutputEnabled
                )
                if (!self$piecewiseSurvivalEnabled) {
                    self$.cat("  Piecewise exponential survival is disabled.\n\n",
                        consoleOutputEnabled = consoleOutputEnabled
                    )
                } else if (length(self$piecewiseSurvivalTime) == 1) {
                    self$.cat("  At all times:", self$lambda2[1], "\n\n",
                        consoleOutputEnabled = consoleOutputEnabled
                    )
                } else {
                    piecewiseSurvivalTimeStr <- format(self$piecewiseSurvivalTime)
                    lambda2Str <- format(self$lambda2)
                    for (i in seq_len(length(self$piecewiseSurvivalTime))) {
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
                        self$.cat("Delayed response is enabled.\n",
                            consoleOutputEnabled = consoleOutputEnabled
                        )
                    }
                    self$.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
                }

                self$.cat("Details:\n\n",
                    sep = "", heading = 1,
                    consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showParametersOfOneGroup(self$.getUserDefinedParameters(),
                    "User defined parameters",
                    orderByParameterName = FALSE,
                    consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showParametersOfOneGroup(self$.getDefaultParameters(),
                    "Default parameters",
                    orderByParameterName = FALSE,
                    consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showParametersOfOneGroup(self$.getDerivedParameters(),
                    "Generated parameters",
                    orderByParameterName = FALSE,
                    consoleOutputEnabled = consoleOutputEnabled
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
                stopIllegalArgument("'piecewiseSurvivalTime' must be a list",
                    functionName = ".initFromList",
                    parameter = "piecewiseSurvivalTime"
                )
            }

            if (length(pwSurvTimeList) == 0) {
                stopIllegalArgument("'piecewiseSurvivalTime' must contain at least one entry",
                    functionName = ".initFromList",
                    parameter = "piecewiseSurvivalTime"
                )
            }

            if (is.null(names(pwSurvTimeList))) {
                stopIllegalArgument("'piecewiseSurvivalTime' must be a named list",
                    functionName = ".initFromList",
                    parameter = "piecewiseSurvivalTime"
                )
            }

            if (!all(is.na(self$lambda2))) {
                warnArgumentIgnored("'lambda2' (", .arrayToString(self$lambda2),
                    ") will be ignored because 'piecewiseSurvivalTime' is a list",
                    parameter = "lambda2",
                    value = self$lambda2,
                    diagnosticId = "survival.control_hazard_overridden"
                )
            }

            pwSurvStartTimes <- c(0)
            pwSurvLambda2 <- c()
            pwSurvTimeNames <- names(pwSurvTimeList)
            for (i in seq_len(length(pwSurvTimeNames))) {
                timePeriod <- pwSurvTimeNames[i]
                lambdaValue <- pwSurvTimeList[[timePeriod]]
                .assertIsSingleNumber(lambdaValue, paste0("pwSurvLambda[", i, "]"))

                timePeriod <- self$.validateTimePeriod(
                    timePeriod,
                    i = i, n = length(pwSurvTimeNames)
                )

                if (i < length(pwSurvTimeNames)) {
                    parts <- strsplit(timePeriod, "- *(< *)?", perl = TRUE)[[1]]
                    if (length(parts) != 2) {
                        stopIllegalArgument("all regions (", timePeriod, ") must have the format ",
                            "\"time_1 - <time_2\", e.g., \"3 - <6\"",
                            functionName = ".initFromList",
                            parameter = "timePeriod",
                            value = timePeriod
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
                self$lambda1 <- getLambda1ByLambda2AndHazardRatio(pwSurvLambda2, self$hazardRatio)
                self$.setParameterType("lambda1", C_PARAM_DERIVED)
            } else if (length(self$hazardRatio) > 1 && self$delayedResponseAllowed) {
                if (length(self$hazardRatio) != length(pwSurvLambda2)) {
                    warnArgumentAdjusted("Only the first 'hazardRatio' (", round(self$hazardRatio[1], 4),
                        ") was used for piecewise survival time definition ",
                        "(use a loop over the function to simulate different hazard ratios)",
                        diagnosticId = "survival.multiple_hazard_ratios_reduced"
                    )
                    self$hazardRatio <- self$hazardRatio[1]
                } else {
                    self$delayedResponseEnabled <- TRUE
                }
                self$lambda1 <- getLambda1ByLambda2AndHazardRatio(pwSurvLambda2, self$hazardRatio)
                self$.setParameterType("lambda1", C_PARAM_DERIVED)
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
            .logDebug(
                "lambda1 %s, %s", .arrayToString(self$lambda1, vectorLookAndFeelEnabled = TRUE),
                self$.getParameterType("lambda1")
            )

            if (length(self$hazardRatio) > 0 && !anyNA(self$hazardRatio) &&
                    (length(self$lambda2) == 0 || all(is.na(self$lambda2))) &&
                    length(self$lambda1) > 0 && !anyNA(self$lambda1) &&
                    !self$isGeneratedOrDerivedParameter("lambda1")) {
                self$lambda2 <- getLambda2ByLambda1AndHazardRatio(self$lambda1, self$hazardRatio)
                self$.setParameterType("lambda2", C_PARAM_DERIVED)
            }
            .logDebug(
                "lambda2 %s, %s", .arrayToString(self$lambda2, vectorLookAndFeelEnabled = TRUE),
                self$.getParameterType("lambda2")
            )

            # case 1: lambda1 and lambda2 = NA or generated
            if (length(pwSurvTime) == 1 && (is.na(pwSurvTime) || is.numeric(pwSurvTime)) &&
                    (all(is.na(self$lambda1)) || self$isGeneratedOrDerivedParameter("lambda1")) &&
                    length(self$lambda2) == 1 && (is.na(self$lambda2) ||
                    self$isGeneratedOrDerivedParameter("lambda2"))
                ) {
                .logDebug(".init, case 1: lambda1 and lambda2 = NA")

                if (!is.null(self$.lambdaBased) && isTRUE(self$.lambdaBased)) {
                    stopIllegalArgument("'lambda1' and 'lambda2' must be specified",
                        functionName = ".init",
                        parameter = "lambda1",
                        relatedParameter = "lambda2"
                    )
                }

                if (!anyNA(self$pi1)) {
                    self$.setParameterType("pi1", C_PARAM_USER_DEFINED)
                }

                if (!anyNA(self$hazardRatio)) {
                    self$.setParameterType("hazardRatio", C_PARAM_USER_DEFINED)
                    private$hazardRatioIsUserDefined <- TRUE
                }

                if (!is.na(pwSurvTime)) {
                    warnArgumentIgnored("'piecewiseSurvivalTime' (", pwSurvTime, ") will be ignored",
                        parameter = "piecewiseSurvivalTime",
                        diagnosticId = "survival.piecewise_input_disabled"
                    )
                }

                if (is.na(self$pi2)) {
                    if (!is.na(self$median2) || !anyNA(self$median1)) {
                        .logDebug(".init: set pi2 to 'not applicable'")
                        self$.setParameterType("pi2", C_PARAM_NOT_APPLICABLE)
                    } else {
                        if (length(self$pi1) > 0 && !anyNA(self$pi1) &&
                                length(self$hazardRatio) > 0 && !anyNA(self$hazardRatio)) {
                            .logDebug(".init: calculate pi2 via pi1 and hazard ratio")
                            self$lambda1 <- getLambdaByPi(pi = self$pi1, eventTime = self$eventTime, kappa = self$kappa)
                            self$.setParameterType("lambda1", C_PARAM_DERIVED)
                            self$lambda2 <- getLambda2ByLambda1AndHazardRatio(self$lambda1, self$hazardRatio)
                            self$.setParameterType("lambda2", C_PARAM_DERIVED)
                            self$pi2 <- getPiByLambda(self$lambda2, eventTime = self$eventTime, kappa = self$kappa)
                            self$.setParameterType("pi2", C_PARAM_DERIVED)
                            self$median2 <- getMedianByLambda(self$lambda2, self$kappa)
                            self$.setParameterType("median2", C_PARAM_DERIVED)
                        } else {
                            .logDebug(".init: set pi2 to default")
                            self$pi2 <- .getPi2Default(endpoint = "survival")
                            self$median2 <- getMedianByPi(pi = self$pi2, eventTime = self$eventTime, kappa = self$kappa)
                            self$.setParameterType("pi2", C_PARAM_DEFAULT_VALUE)
                        }
                    }
                } else {
                    .assertIsSingleNumber(self$pi2, "pi2")
                    self$.setParameterType(
                        "pi2",
                        ifelse(isTRUE(.isPi2Default(self$pi2, endpoint = "survival")),
                            C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
                        )
                    )
                    if (!anyNA(self$median2)) {
                        warnArgumentIgnored("'median2' (", .arrayToString(self$median2), ") will be ignored",
                            relatedParameter = "pi2",
                            relatedValue = self$pi2,
                            parameter = "median2",
                            value = self$median2,
                            diagnosticId = "survival.control_median_overridden"
                        )
                        self$median2 <- NA_real_
                    }
                }

                hazardRatioCalculationEnabled <- self$isNotUserDefinedParameter("hazardRatio")
                if (self$isUserDefinedParameter("pi1") && self$isUserDefinedParameter("pi2")) {
                    hazardRatioCalculationEnabled <- TRUE
                }
                if (all(is.na(self$pi1))) {
                    if ((all(is.na(self$median1)) || all(is.na(self$median2))) &&
                            length(self$hazardRatio) > 0 && !all(is.na(self$hazardRatio))) {
                        self$.setParameterType("hazardRatio", C_PARAM_USER_DEFINED)
                        hazardRatioCalculationEnabled <- FALSE
                    }

                    if (!anyNA(self$median1)) {
                        .logDebug(".init: set pi1 to 'not applicable'")
                        self$.setParameterType("pi1", C_PARAM_NOT_APPLICABLE)

                        if (is.na(self$median2)) {
                            if (anyNA(self$hazardRatio)) {
                                stopMissingArgument(
                                    "'hazardRatio', 'lambda2', or 'median2' must be specified",
                                    functionName = ".init",
                                    parameter = "hazardRatio",
                                    relatedParameter = "lambda2"
                                )
                            }

                            if (length(self$hazardRatio) != length(self$median1)) {
                                stopConflictingArguments(
                                    "length of 'hazardRatio' (", .arrayToString(self$hazardRatio), ") must be ",
                                    "equal to length of 'median1' (", .arrayToString(self$median1), ")",
                                    functionName = ".init",
                                    parameter = "hazardRatio",
                                    value = self$hazardRatio,
                                    relatedParameter = "median1",
                                    relatedValue = self$median1
                                )
                            }

                            .logDebug(".init: calculate lambda2 and median2 by median1")

                            self$lambda2 <- getLambda2ByLambda1AndHazardRatio(getLambdaByMedian(self$median1, self$kappa), self$hazardRatio)

                            if (!self$delayedResponseAllowed && length(unique(round(self$lambda2, 8))) > 1) {
                                stopIllegalArgument(
                                    "'lambda2' can only be calculated if ",
                                    "'unique(lambda1 / hazardRatio)' ",
                                    "result in a single value; current result = ",
                                    .arrayToString(round(self$lambda2, 4), vectorLookAndFeelEnabled = TRUE),
                                    " (e.g., delayed response is not allowed)",
                                    functionName = ".init",
                                    parameter = "lambda2",
                                    value = self$lambda2,
                                    relatedParameter = "unique(lambda1 / hazardRatio)",
                                    relatedValue = unique(round(self$lambda2, 8)),
                                    diagnosticId = "survival.nonconstant_derived_hazard"
                                )
                            }

                            self$median2 <- getMedianByLambda(self$lambda2, self$kappa)
                            self$.setParameterType("lambda2", C_PARAM_DERIVED)
                            self$.setParameterType("median2", C_PARAM_DERIVED)
                        }
                    } else if (length(self$hazardRatio) > 0 && !all(is.na(self$hazardRatio))) {
                        self$.setParameterType("pi1", C_PARAM_NOT_APPLICABLE)

                        if (!anyNA(self$lambda1)) {
                            .logDebug(".init: calculate median1 by lambda1")
                            self$median1 <- getMedianByLambda(self$lambda1, self$kappa)
                            self$.setParameterType("median1", C_PARAM_DERIVED)
                        } else if (!is.na(self$median2)) {
                            .logDebug(".init: calculate lambda1 and median1 by median2")
                            self$lambda1 <- getLambda1ByLambda2AndHazardRatio(
                                getLambdaByMedian(self$median2, self$kappa),
                                self$hazardRatio
                            )
                            self$median1 <- getMedianByLambda(self$lambda1, self$kappa)
                            self$.setParameterType("lambda1", C_PARAM_DERIVED)
                            self$.setParameterType("median1", C_PARAM_DERIVED)
                        }
                    } else {
                        .logDebug(".init: set pi1 to default")
                        if (!is.null(self$.pi1Default) && is.numeric(self$.pi1Default) &&
                                length(self$.pi1Default) > 0) {
                            self$pi1 <- self$.pi1Default
                        } else {
                            self$pi1 <- .getPi1Default(type = "power", endpoint = "survival")
                        }
                        self$.setParameterType("pi1", C_PARAM_DEFAULT_VALUE)
                    }
                } else {
                    .assertIsNumericVector(self$pi1, "pi1")
                    if (!anyNA(self$median1)) {
                        .logDebug(".init: set median1 to NA")
                        warnArgumentIgnored("'median1' (", .arrayToString(self$median1), ") will be ignored",
                            relatedParameter = "pi1",
                            relatedValue = self$pi1,
                            parameter = "median1",
                            value = self$median1,
                            diagnosticId = "survival.treatment_median_overridden"
                        )
                        self$median1 <- NA_real_
                    }
                }

                if (hazardRatioCalculationEnabled) {
                    if (length(self$hazardRatio) > 0 && !all(is.na(self$hazardRatio))) {
                        warnArgumentIgnored("'hazardRatio' (", .arrayToString(self$hazardRatio),
                            ") will be ignored because it will be calculated",
                            parameter = "hazardRatio",
                            value = self$hazardRatio,
                            diagnosticId = "survival.hazard_ratio_derived"
                        )
                    }

                    if (!anyNA(self$lambda1) && !anyNA(self$lambda2)) {
                        .logDebug(".init: calculate hazardRatio by lambda1 and lambda2")
                        self$hazardRatio <- getHazardRatioByLambda(self$lambda1, self$lambda2)
                        self$.setParameterType("hazardRatio", C_PARAM_DERIVED)
                    } else if (!anyNA(self$pi1) && !is.na(self$pi2)) {
                        .logDebug(".init: calculate hazardRatio by pi1 and pi2")
                        self$hazardRatio <- getHazardRatioByPi(
                            self$pi1, self$pi2, self$eventTime,
                            kappa = self$kappa
                        )
                        self$.setParameterType("hazardRatio", C_PARAM_DERIVED)
                    }
                }

                if (length(self$pi1) > 0 && !anyNA(self$pi1)) {
                    pi1Default <- .getPi1Default(type = "power", endpoint = "survival")
                    if (!is.null(self$.pi1Default) && is.numeric(self$.pi1Default) &&
                            length(self$.pi1Default) > 0) {
                        pi1Default <- self$.pi1Default
                    }
                    if (identical(self$pi1, pi1Default)) {
                        self$.setParameterType("pi1", C_PARAM_DEFAULT_VALUE)
                    } else if (hazardRatioCalculationEnabled &&
                            !self$isGeneratedOrDerivedParameter("pi1")) {
                        self$.setParameterType("pi1", C_PARAM_USER_DEFINED)
                    }
                }

                if (length(self$pi2) == 1 && !is.na(self$pi2)) {
                    if (length(self$eventTime) == 1 && !is.na(self$eventTime)) {
                        self$lambda2 <- getLambdaByPi(self$pi2, self$eventTime, kappa = self$kappa)
                        self$.setParameterType("lambda2", C_PARAM_DERIVED)
                    }

                    if (length(self$pi1) == 1 && is.na(self$pi1) && !anyNA(self$hazardRatio)) {
                        self$pi1 <- getPi1ByPi2AndHazardRatio(self$pi2, self$hazardRatio,
                            eventTime = self$eventTime, kappa = self$kappa
                        )
                        self$.setParameterType("pi1", C_PARAM_DERIVED)
                    }
                    if (length(self$pi1) > 0 && !anyNA(self$pi1) &&
                            length(self$eventTime) == 1 && !is.na(self$eventTime)) {
                        self$lambda1 <- getLambdaByPi(self$pi1, self$eventTime, kappa = self$kappa)
                        self$.setParameterType("lambda1", C_PARAM_DERIVED)
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
                self$.initHazardRatio(1L)
                if (!self$piecewiseSurvivalEnabled) {
                    self$.initPi()
                    self$.initMedian()
                }
            } else if (self$delayedResponseAllowed &&
                    (length(self$lambda2) == 1 && !is.na(self$lambda2) ||
                        (length(self$lambda1) > 0 && !anyNA(self$lambda1))) &&
                    length(self$hazardRatio) > 0 &&
                    (all(is.na(pwSurvTime)) || identical(pwSurvTime, 0))) {
                .logDebug(".init, case 2: delayedResponseAllowed")

                self$piecewiseSurvivalEnabled <- FALSE

                if (!all(is.na(pwSurvTime)) && !identical(pwSurvTime, 0)) {
                    warnArgumentIgnored("'piecewiseSurvivalTime' (", .arrayToString(pwSurvTime), ") will be ignored",
                        parameter = "piecewiseSurvivalTime",
                        diagnosticId = "survival.piecewise_input_disabled"
                    )
                }
                self$piecewiseSurvivalTime <- 0

                hazardRatioCalculationEnabled <- TRUE
                if ((length(self$lambda2) == 0 || all(is.na(self$lambda2))) &&
                        length(self$lambda1) > 0 && !anyNA(self$lambda1)) {
                    self$lambda2 <- getLambda2ByLambda1AndHazardRatio(self$lambda1, self$hazardRatio)
                    self$.setParameterType("lambda2", C_PARAM_DERIVED)
                    hazardRatioCalculationEnabled <- FALSE
                }

                self$.initPi()
                if (hazardRatioCalculationEnabled) {
                    self$.initHazardRatio(2L)
                }
                self$.initMedian()
            } else if (!is.numeric(pwSurvTime)) {
                stopIllegalArgument("'piecewiseSurvivalTime' must be a list, a numeric value, or vector",
                    functionName = ".init",
                    parameter = "piecewiseSurvivalTime"
                )
            } else {
                self$piecewiseSurvivalTime <- pwSurvTime
                if ((all(is.na(self$piecewiseSurvivalTime)) || identical(self$piecewiseSurvivalTime, 0)) &&
                        (length(self$lambda2) == 1 && !is.na(self$lambda2))) {
                    .logDebug(".init, case 3: piecewise survival is disabled")
                    if (!all(is.na(self$piecewiseSurvivalTime)) &&
                            !identical(self$piecewiseSurvivalTime, 0)) {
                        warnArgumentIgnored(
                            "'piecewiseSurvivalTime' (",
                            .arrayToString(self$piecewiseSurvivalTime), ") will be ignored",
                            parameter = "piecewiseSurvivalTime",
                            value = self$piecewiseSurvivalTime,
                            diagnosticId = "survival.piecewise_input_disabled"
                        )
                    }

                    self$piecewiseSurvivalTime <- 0
                    self$.setParameterType("piecewiseSurvivalTime", C_PARAM_DEFAULT_VALUE)
                    self$piecewiseSurvivalEnabled <- FALSE
                    self$.initHazardRatio(3L)
                    self$.initPi()
                    self$.initMedian()
                } else {
                    .logDebug(
                        ".init, case 3: piecewise survival is enabled (%s)",
                        .arrayToString(self$piecewiseSurvivalTime)
                    )
                    if (all(is.na(self$piecewiseSurvivalTime))) {
                        if (self$isUserDefinedParameter("median1")) {
                            stopConflictingArguments(
                                "'median1' (", .arrayToString(self$median1), ") with length > 1 can only ",
                                "defined together with a single 'median2', 'lambda2' or 'pi2'",
                                functionName = ".init",
                                parameter = "median1",
                                value = self$median1
                            )
                        }

                        if (self$delayedResponseAllowed && length(self$lambda1 > 0) &&
                                !all(is.na(self$lambda1)) &&
                                length(self$lambda1) != length(self$lambda2) &&
                                self$delayedResponseAllowed) {
                            stopIllegalArgument(
                                "length of 'lambda1' (", length(self$lambda1), "), ",
                                "'lambda2' (", length(self$lambda2),
                                "), and ", "'piecewiseSurvivalTime' (", length(self$piecewiseSurvivalTime),
                                ") must be equal",
                                functionName = ".init",
                                parameter = "lambda1",
                                value = length(self$lambda1),
                                relatedParameter = "lambda2",
                                relatedValue = length(self$lambda2)
                            )
                        }

                        stopMissingArgument("'piecewiseSurvivalTime' must be specified",
                            functionName = ".init",
                            parameter = "piecewiseSurvivalTime"
                        )
                    }
                    self$.setParameterType("piecewiseSurvivalTime", C_PARAM_USER_DEFINED)
                    self$piecewiseSurvivalEnabled <- TRUE
                    self$.initHazardRatio(4L)
                    self$.initPi()
                }
            }

            if (self$piecewiseSurvivalEnabled) {
                for (param in c("pi", "median")) {
                    for (group in 1:2) {
                        paramName <- paste0(param, group)
                        if (self$isUserDefinedParameter(paramName)) {
                            warnArgumentAdjusted(
                                .pQuote(paramName), " (", .arrayToString(self[[paramName]]), ") ",
                                "was converted to 'lambda", group, "' ",
                                "and is not available in output because piecewise ",
                                "exponential survival time is enabled",
                                diagnosticId = "survival.hazard_parameterization_converted"
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
                    warnArgumentIgnored("Event time (", self$eventTime, ") will be ignored because it is not ",
                        "applicable for piecewise exponential survival time",
                        diagnosticId = "survival.probabilities_ignored_for_piecewise_hazards"
                    )
                    self$eventTime <- C_EVENT_TIME_DEFAULT
                }
            }

            self$.validateInitialization()
        },
        .initMedian = function() {
            if (length(self$eventTime) == 1 && !is.na(self$eventTime)) {
                if (length(self$pi1) > 0 && !all(is.na(self$pi1)) &&
                        self$isNotUserDefinedParameter("median1")) {
                    self$median1 <- getMedianByPi(self$pi1, self$eventTime, kappa = self$kappa)
                    self$.setParameterType("median1", C_PARAM_DERIVED)
                }
                if (length(self$pi2) == 1 && !is.na(self$pi2) &&
                        self$isNotUserDefinedParameter("median2")) {
                    self$median2 <- getMedianByPi(self$pi2, self$eventTime, kappa = self$kappa)
                    self$.setParameterType("median2", C_PARAM_DERIVED)
                }
            } else {
                if (length(self$lambda1) > 0 && !all(is.na(self$lambda1)) &&
                        self$isNotUserDefinedParameter("median1")) {
                    self$median1 <- getMedianByLambda(self$lambda1, kappa = self$kappa)
                    self$.setParameterType("median1", C_PARAM_DERIVED)
                }
                if (length(self$lambda2) == 1 && !is.na(self$lambda2) &&
                        self$isNotUserDefinedParameter("median2")) {
                    self$median2 <- getMedianByLambda(self$lambda2, kappa = self$kappa)
                    self$.setParameterType("median2", C_PARAM_DERIVED)
                }
            }
        },
        .initPi = function() {
            .logDebug(".initPi: set pi1, pi2, and eventTime to NA")

            if (!is.na(self$eventTime) && self$isUserDefinedParameter("eventTime")) {
                warnArgumentIgnored("'eventTime' (", round(self$eventTime, 3), ") will be ignored",
                    parameter = "eventTime",
                    value = self$eventTime,
                    diagnosticId = "survival.probability_inputs_ignored"
                )
            }
            if (isFALSE(.isPi1Default(self$pi1, type = "sampleSize", endpoint = "survival")) &&
                    isFALSE(.isPi1Default(self$pi1, type = "power", endpoint = "survival"))) {
                warnArgumentIgnored("'pi1' (", .arrayToString(self$pi1), ") will be ignored",
                    parameter = "pi1",
                    value = self$pi1,
                    diagnosticId = "survival.probability_inputs_ignored"
                )
            }
            if (isFALSE(.isPi2Default(self$pi2, endpoint = "survival"))) {
                warnArgumentIgnored("'pi2' (", self$pi2, ") will be ignored",
                    parameter = "pi2",
                    value = self$pi2,
                    diagnosticId = "survival.probability_inputs_ignored"
                )
            }

            self$.setParameterType("eventTime", C_PARAM_NOT_APPLICABLE)
            self$.setParameterType("pi1", C_PARAM_NOT_APPLICABLE)
            self$.setParameterType("pi2", C_PARAM_NOT_APPLICABLE)
            self$eventTime <- NA_real_
            self$pi1 <- NA_real_
            self$pi2 <- NA_real_

            if (length(self$lambda2) == 0 || anyNA(self$lambda2)) {
                stopRuntimeIssue("'lambda2' must be defined before .initPi() can be called",
                    functionName = ".initPi",
                    parameter = "lambda2"
                )
            }

            if (self$isUndefinedParameter("lambda2") || self$isNotApplicableParameter("lambda2")) {
                self$.setParameterType("lambda2", C_PARAM_USER_DEFINED)
            }

            if (self$piecewiseSurvivalEnabled && length(self$hazardRatio) > 1) {
                return(invisible())
            }

            if (length(self$lambda1) == 0 || anyNA(self$lambda1)) {
                if (length(self$hazardRatio) > 0 && !anyNA(self$hazardRatio)) {
                    .logDebug(".initPi: calculate lambda1 by hazardRatio")
                    self$lambda1 <- getLambda1ByLambda2AndHazardRatio(self$lambda2, self$hazardRatio)
                    self$.setParameterType("lambda1", C_PARAM_DERIVED)
                } else if (length(self$lambda1) == 0) {
                    self$lambda1 <- NA_real_
                } else if (self$delayedResponseAllowed) {
                    self$.setParameterType("lambda1", C_PARAM_USER_DEFINED)
                }
            }
        },
        .initHazardRatio = function(number = -1L) {
            .logDebug(".initHazardRatio(#%s)", number)

            if (!is.null(self$hazardRatio) && length(self$hazardRatio) > 0 && !all(is.na(self$hazardRatio))) {
                if ((length(self$lambda1) == 1 && is.na(self$lambda1)) ||
                        (self$isGeneratedOrDerivedParameter("lambda1") &&
                            (
                                self$isNotUserDefinedParameter("median1") ||
                                    self$isNotUserDefinedParameter("median2")
                            )
                        ) ||
                        (self$isGeneratedOrDerivedParameter("lambda2") &&
                            (
                                self$isNotUserDefinedParameter("median1") ||
                                    self$isNotUserDefinedParameter("median2")
                            )
                        )
                    ) {
                    .logDebug(".init: set hazard ratio to user defined")
                    self$.setParameterType("hazardRatio", C_PARAM_USER_DEFINED)
                    private$hazardRatioIsUserDefined <- TRUE
                    return(invisible())
                }

                if (!self$.silent && (isTRUE(private$hazardRatioIsUserDefined) || number == 3L)) {
                    warnArgumentIgnored("'hazardRatio' (", .arrayToString(self$hazardRatio),
                        ") will be ignored because it will be calculated",
                        parameter = "hazardRatio",
                        value = self$hazardRatio,
                        diagnosticId = "survival.hazard_ratio_derived"
                    )
                }
            }

            if (anyNA(self$lambda2)) {
                stopMissingArgument("'lambda2' must be specified",
                    functionName = ".initHazardRatio",
                    parameter = "lambda2"
                )
            }

            if (anyNA(self$lambda1)) {
                if (self$delayedResponseAllowed && any(is.na(self$hazardRatio) &&
                        !anyNA(self$piecewiseSurvivalTime) &&
                        length(self$lambda2) == length(self$piecewiseSurvivalTime))) {
                    stopMissingArgument("'hazardRatio' must be specified",
                        functionName = ".initHazardRatio",
                        parameter = "hazardRatio"
                    )
                }
                if (anyNA(self$hazardRatio)) {
                    stopMissingArgument("'hazardRatio', 'lambda1' or 'median1' must be specified",
                        functionName = ".initHazardRatio",
                        parameter = "hazardRatio",
                        relatedParameter = "lambda1"
                    )
                }
                stopMissingArgument("'lambda1' must be specified",
                    functionName = ".initHazardRatio",
                    parameter = "lambda1"
                )
            }

            self$.setParameterType("lambda1", C_PARAM_USER_DEFINED)

            hr <- unique(round(getHazardRatioByLambda(self$lambda1, self$lambda2), 8))
            if (length(hr) == 1) {
                .logDebug(".init: calculate hazardRatio by lambda1 and lambda2 (one hazard ratio)")
                self$hazardRatio <- getHazardRatioByLambda(self$lambda1, self$lambda2)[1]
                self$.setParameterType("hazardRatio", C_PARAM_DERIVED)
                return(invisible())
            }

            if (length(self$lambda2) == 1 && length(self$lambda1) > 1) {
                .logDebug(".init: calculate hazardRatio by lambda1 and lambda2 (multiple lambda2 values)")
                self$hazardRatio <- getHazardRatioByLambda(self$lambda1, self$lambda2)
                self$.setParameterType("hazardRatio", C_PARAM_DERIVED)
                return(invisible())
            }

            if (self$delayedResponseAllowed) {
                .logDebug(".init: calculate hazardRatio by lambda1 and lambda2 (delayed response allowed)")
                self$hazardRatio <- getHazardRatioByLambda(self$lambda1, self$lambda2)
                self$.setParameterType("hazardRatio", C_PARAM_DERIVED)
                self$delayedResponseEnabled <- TRUE
                return(invisible())
            }

            stopIllegalArgument(
                "'hazardRatio' can only be calculated if 'unique(lambda1 / lambda2)' ",
                "result in a single value; current result = ",
                .arrayToString(round(hr, 4), vectorLookAndFeelEnabled = TRUE),
                " (e.g., delayed response is not allowed)",
                functionName = ".initHazardRatio",
                parameter = "hazardRatio",
                value = hr,
                relatedParameter = "unique(lambda1 / lambda2)",
                relatedValue = unique(round(hr, 4)),
                diagnosticId = "survival.nonconstant_derived_hazard"
            )
        },
        .validateInitialization = function() {
            if (length(self$piecewiseSurvivalTime) == 0) {
                stopIllegalArgument(
                    "'piecewiseSurvivalTime' must contain at least one survival start time",
                    functionName = ".validateInitialization",
                    parameter = "piecewiseSurvivalTime"
                )
            }

            if (anyNA(self$piecewiseSurvivalTime)) {
                stopIllegalArgument(
                    "'piecewiseSurvivalTime' must contain valid survival start times",
                    functionName = ".validateInitialization",
                    parameter = "piecewiseSurvivalTime"
                )
            }

            if (self$piecewiseSurvivalTime[1] != 0) {
                stopIllegalArgument(
                    "the first value of 'piecewiseSurvivalTime' must be 0",
                    functionName = ".validateInitialization",
                    parameter = "piecewiseSurvivalTime"
                )
            }

            if (length(self$piecewiseSurvivalTime) != length(self$lambda2)) {
                stopIllegalArgument(
                    "length of 'piecewiseSurvivalTime' (",
                    length(self$piecewiseSurvivalTime), ") and length of 'lambda2' (",
                    length(self$lambda2), ") must be equal",
                    functionName = ".validateInitialization",
                    parameter = "piecewiseSurvivalTime",
                    value = length(self$piecewiseSurvivalTime),
                    relatedParameter = "lambda2",
                    relatedValue = length(self$lambda2)
                )
            }

            .assertValuesAreStrictlyIncreasing(self$piecewiseSurvivalTime, "piecewiseSurvivalTime")

            if ((length(self$lambda1) != 1 || is.na(self$lambda1)) &&
                    !(self$.getParameterType("lambda1") %in% c(C_PARAM_GENERATED, C_PARAM_DERIVED, C_PARAM_USER_DEFINED))) {
                if (length(self$hazardRatio) == 1 && !is.na(self$hazardRatio)) {
                    self$lambda1 <- getLambda1ByLambda2AndHazardRatio(self$lambda2, self$hazardRatio)
                    self$.setParameterType("lambda1", C_PARAM_DERIVED)
                } else if (length(self$hazardRatio) > 1 && self$delayedResponseAllowed &&
                        !is.na(self$hazardRatio[1])) {
                    if (!self$delayedResponseEnabled && self$.isLambdaBased()) {
                        if (self$delayedResponseAllowed) {
                            if (length(self$hazardRatio) != length(self$lambda2)) {
                                stopIllegalArgument(
                                    "length of 'hazardRatio' (",
                                    length(self$hazardRatio), ") and length of 'lambda2' (",
                                    length(self$lambda2), ") must be equal",
                                    functionName = ".validateInitialization",
                                    parameter = "hazardRatio",
                                    value = length(self$hazardRatio),
                                    relatedParameter = "lambda2",
                                    relatedValue = length(self$lambda2)
                                )
                            }
                            self$delayedResponseEnabled <- TRUE
                        } else {
                            warnArgumentAdjusted("Only the first 'hazardRatio' (", round(self$hazardRatio[1], 4),
                                ") was used for piecewise survival time definition",
                                diagnosticId = "survival.multiple_hazard_ratios_reduced"
                            )
                            self$hazardRatio <- self$hazardRatio[1]
                        }
                        self$lambda1 <- getLambda1ByLambda2AndHazardRatio(self$lambda2, self$hazardRatio)
                        self$.setParameterType("lambda1", C_PARAM_DERIVED)
                    }
                } else if (!self$delayedResponseEnabled && !(length(self$lambda2) == 1 && length(self$lambda1) > 1)) {
                    if (length(self$lambda1) > 1) {
                        warnArgumentIgnored("'lambda1' (", .arrayToString(self$lambda1),
                            ") will be ignored",
                            parameter = "lambda1",
                            value = self$lambda1,
                            diagnosticId = "survival.multiple_treatment_hazards_ignored"
                        )
                    }
                    self$lambda1 <- NA_real_
                    self$.setParameterType("lambda1", C_PARAM_NOT_APPLICABLE)
                }
            } else if (length(self$hazardRatio) == 1 && !is.na(self$hazardRatio) &&
                    length(self$lambda1) > 0 && !anyNA(self$lambda1) &&
                    length(self$lambda2) > 0 && !anyNA(self$lambda2)) {
                target <- getLambda1ByLambda2AndHazardRatio(self$lambda2, self$hazardRatio)
                if (length(self$lambda1) > 0 && !all(is.na(self$lambda1)) &&
                        !equals(target, self$lambda1)) {
                    stopIllegalArgument(
                        "'lambda1' (", .arrayToString(self$lambda1), ") ",
                        "is not as expected (", .arrayToString(target), ") ",
                        "for given hazard ratio ", self$hazardRatio,
                        functionName = ".validateInitialization",
                        parameter = "lambda1",
                        value = self$lambda1
                    )
                }
            }

            if (self$piecewiseSurvivalEnabled && !(length(self$lambda1) == 1 && is.na(self$lambda1)) &&
                    length(self$piecewiseSurvivalTime) != length(self$lambda1)) {
                stopIllegalArgument(
                    "length of 'piecewiseSurvivalTime' (", length(self$piecewiseSurvivalTime), ") ",
                    "and length of 'lambda1' (", length(self$lambda1), ") must be equal",
                    functionName = ".validateInitialization",
                    parameter = "piecewiseSurvivalTime",
                    value = length(self$piecewiseSurvivalTime),
                    relatedParameter = "lambda1",
                    relatedValue = length(self$lambda1)
                )
            }
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
