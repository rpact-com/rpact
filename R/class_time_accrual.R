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
        accrualTimeOriginal = NULL,
        accrualIntensity = NULL,
        accrualIntensityRelative = NULL,
        maxNumberOfSubjects = NULL,
        remainingTime = NULL,
        piecewiseAccrualEnabled = NULL,
        initialize = function(
                accrualTime = NA_real_,
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
            self$.setParameterType("endOfAccrualIsUserDefined", C_PARAM_GENERATED)
            self$.setParameterType("followUpTimeMustBeUserDefined", C_PARAM_GENERATED)
            self$.setParameterType("maxNumberOfSubjectsIsUserDefined", C_PARAM_GENERATED)
            self$.setParameterType("maxNumberOfSubjectsCanBeCalculatedDirectly", C_PARAM_GENERATED)
            self$.setParameterType(
                "absoluteAccrualIntensityEnabled",
                ifelse(is.na(self$absoluteAccrualIntensityEnabled),
                    C_PARAM_GENERATED, C_PARAM_USER_DEFINED
                )
            )

            self$accrualTimeOriginal <- NA_real_
            self$.setParameterType("accrualTimeOriginal", C_PARAM_NOT_APPLICABLE)
            self$accrualIntensityRelative <- NA_real_
            self$.setParameterType("accrualIntensityRelative", C_PARAM_NOT_APPLICABLE)
            self$remainingTime <- NA_real_

            self$.init(self$accrualTime)

            # case 6 correction
            if (!self$endOfAccrualIsUserDefined && self$maxNumberOfSubjectsIsUserDefined &&
                    !self$absoluteAccrualIntensityEnabled) {
                self$remainingTime <- NA_real_
                self$.setParameterType("remainingTime", C_PARAM_NOT_APPLICABLE)
                self$accrualTime <- self$accrualTime[seq_len(length(self$accrualIntensity))]
            }

            self$.initAccrualIntensityAbsolute()
            self$.validateFormula()
            self$.showWarningIfCaseIsNotAllowed()
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
                super$.show(
                    showType = showType, digits = digits,
                    consoleOutputEnabled = consoleOutputEnabled
                )
            } else {
                self$.cat("Accrual time and intensity:\n",
                    sep = "", heading = 1,
                    consoleOutputEnabled = consoleOutputEnabled
                )
                if (!self$isAccrualTimeEnabled()) {
                    self$.cat("  Accrual time is disabled.\n",
                        consoleOutputEnabled = consoleOutputEnabled
                    )
                } else if (length(self$accrualTime) == 1) {
                    self$.cat("  At all times:", self$accrualIntensity[1], "\n",
                        consoleOutputEnabled = consoleOutputEnabled
                    )
                } else {
                    accrualTimeStr <- format(self$accrualTime)
                    accrualIntensityStr <- format(self$accrualIntensity)
                    for (i in seq_len(length(self$accrualTime))) {
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
                self$.showParametersOfOneGroup(
                    c(
                        self$.getDerivedParameters(),
                        self$.getGeneratedParameters()
                    ), "Generated parameters",
                    orderByParameterName = FALSE,
                    consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)
            }
        },
        .getFormula = function() {
            s <- ""
            for (i in seq_len(length(self$accrualTime))) {
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
            for (i in seq_len(length(self$accrualTime))) {
                if (i < length(self$accrualTime)) {
                    numberOfSubjects <- numberOfSubjects +
                        (self$accrualTime[i + 1] - self$accrualTime[i]) * self$accrualIntensity[i]
                }
            }
            if (!equals(numberOfSubjects, self$maxNumberOfSubjects, tolerance = 1e-03) &&
                    self$absoluteAccrualIntensityEnabled) {
                stopConflictingArguments("'maxNumberOfSubjects' (", self$maxNumberOfSubjects, ") disagrees with ",
                    "the defined accrual time and intensity: ",
                    self$.getFormula(), " = ", numberOfSubjects,
                    functionName = ".validateFormula",
                    parameter = "maxNumberOfSubjects",
                    value = self$maxNumberOfSubjects,
                    diagnosticId = "accrual.subject_total_mismatch"
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
                warnInvalidInput("The specified accrual time and intensity cannot be ",
                    "supplemented automatically with the missing information; ",
                    "therefore further calculations are not possible",
                    diagnosticId = "accrual.schedule_underdetermined"
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
                self$.cat("Case (#1):\n",
                    sep = "", heading = 1,
                    consoleOutputEnabled = consoleOutputEnabled
                )
                self$.cat(prefix, "End of accrual, absolute accrual intensity ",
                    "and 'maxNumberOfSubjects' are given, ",
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
                self$.cat("Case (#2):\n",
                    sep = "", heading = 1,
                    consoleOutputEnabled = consoleOutputEnabled
                )
                self$.cat(prefix, "End of accrual, relative accrual intensity ",
                    "and 'maxNumberOfSubjects' are given, ",
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
                self$.cat("Case (#3):\n",
                    sep = "", heading = 1,
                    consoleOutputEnabled = consoleOutputEnabled
                )
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
                self$.cat("Case (#4):\n",
                    sep = "", heading = 1,
                    consoleOutputEnabled = consoleOutputEnabled
                )
                self$.cat(prefix, "End of accrual, relative accrual ",
                    "intensity and 'followUpTime' are given, ",
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
                self$.cat("Case (#5):\n",
                    sep = "", heading = 1,
                    consoleOutputEnabled = consoleOutputEnabled
                )
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
                self$.cat("Case (#6):\n",
                    sep = "", heading = 1,
                    consoleOutputEnabled = consoleOutputEnabled
                )
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
                self$.cat("Case (#7):\n",
                    sep = "", heading = 1,
                    consoleOutputEnabled = consoleOutputEnabled
                )
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
                self$.cat("Case (#8):\n",
                    sep = "", heading = 1,
                    consoleOutputEnabled = consoleOutputEnabled
                )
                self$.cat(prefix, "'followUpTime' and relative accrual intensity are given, ",
                    "absolute accrual intensity@, end of accrual and 'maxNumberOfSubjects' shall be calculated\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
                self$.cat(prefix, "Example: getAccrualTime(accrualTime = c(0, 6), accrualIntensity = c(0.22, 0.33))\n",
                    consoleOutputEnabled = consoleOutputEnabled
                )
            }

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
                stopIllegalArgument(
                    "the calculation of 'followUpTime' for given 'maxNumberOfSubjects' ",
                    "and relative accrual intensities (< 1) ",
                    "can only be done if end of accrual is defined",
                    functionName = ".validate",
                    parameter = "followUpTime",
                    relatedParameter = "maxNumberOfSubjects",
                    relatedValue = self$maxNumberOfSubjects,
                    diagnosticId = "accrual.end_required_for_relative_intensities"
                )
            }

            # Case 8
            else if (!self$endOfAccrualIsUserDefined && !self$maxNumberOfSubjectsIsUserDefined &&
                    self$followUpTimeMustBeUserDefined && !self$absoluteAccrualIntensityEnabled) {
                stopIllegalArgument(
                    "the calculation of 'maxNumberOfSubjects' for given 'followUpTime' ",
                    "and relative accrual intensities (< 1) ",
                    "can only be done if end of accrual is defined",
                    functionName = ".validate",
                    parameter = "maxNumberOfSubjects",
                    relatedParameter = "followUpTime",
                    diagnosticId = "accrual.end_required_for_relative_intensities"
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
                stopIllegalArgument("'accrualTime' must be a list",
                    functionName = ".initFromList",
                    parameter = "accrualTime"
                )
            }

            if (length(accrualTimeList) == 0) {
                stopIllegalArgument("'accrualTime' must contain at least one entry",
                    functionName = ".initFromList",
                    parameter = "accrualTime"
                )
            }

            if (is.null(names(accrualTimeList))) {
                stopIllegalArgument("'accrualTime' must be a named list where the names specify ",
                    "the time regions and the values the accrual time",
                    functionName = ".initFromList",
                    parameter = "accrualTime"
                )
            }

            if (self$.showWarnings && !all(is.na(self$accrualIntensity)) &&
                    (length(self$accrualIntensity) != 1 ||
                        self$accrualIntensity != C_ACCRUAL_INTENSITY_DEFAULT)) {
                warnArgumentIgnored("'accrualIntensity' (", .arrayToString(self$accrualIntensity),
                    ") will be ignored because 'accrualTime' is a list",
                    parameter = "accrualIntensity",
                    value = self$accrualIntensity,
                    diagnosticId = "accrual.list_intensities_override_vector"
                )
            }

            self$accrualTime <- numeric(0)
            self$accrualIntensity <- numeric(0)
            timeRegions <- names(accrualTimeList)
            endOfAccrualIsUndefined <- FALSE
            self$accrualTime <- c(self$accrualTime, 0)
            for (i in seq_len(length(timeRegions))) {
                timePeriod <- timeRegions[i]
                accrualTimeValue <- accrualTimeList[[timePeriod]]
                .assertIsSingleNumber(accrualTimeValue, paste0("accrualTime[", i, "]"))

                settings <- self$.validateTimePeriod(timePeriod,
                    i = i,
                    n = length(timeRegions), accrualTimeMode = TRUE
                )
                timePeriod <- settings$timePeriod
                endOfAccrualIsUndefined <- settings$endOfAccrualIsUndefined

                if (i < length(timeRegions)) {
                    parts <- strsplit(timePeriod, "- *(< *)?", perl = TRUE)[[1]]
                    if (length(parts) != 2) {
                        stopIllegalArgument("all regions (", timePeriod, ") must have the format ",
                            "\"time_1 - <time_2\", e.g., \"3 - <6\"",
                            functionName = ".initFromList",
                            parameter = "timePeriod",
                            value = timePeriod
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
                    !anyNA(self$accrualTime) && !anyNA(self$accrualIntensity)) {
                len <- length(self$accrualIntensity)
                accrualIntensityAbsolute <- self$maxNumberOfSubjects / sum((self$accrualTime[2:(len + 1)] -
                    self$accrualTime[1:len]) * self$accrualIntensity) * self$accrualIntensity

                if (!equals(accrualIntensityAbsolute, self$accrualIntensity, tolerance = 1e-06) &&
                        !equals(accrualIntensityAbsolute, 0, tolerance = 1e-06)) {
                    self$.validateAccrualTimeAndIntensity()

                    if (self$absoluteAccrualIntensityEnabled &&
                            self$isUserDefinedParameter("maxNumberOfSubjects")) {
                        if (self$isDefaultParameter("accrualTime")) {
                            self$accrualTime <- self$maxNumberOfSubjects / self$accrualIntensity
                            self$.setParameterType("accrualTime", C_PARAM_DERIVED)
                            self$remainingTime <- self$accrualTime
                            self$accrualTime <- c(0, self$accrualTime)
                        } else {
                            stopConflictingArguments("'maxNumberOfSubjects' (", self$maxNumberOfSubjects,
                                ") disagrees with ", "the defined accrual time (",
                                .arrayToString(self$accrualTime), ") and intensity: ",
                                self$.getFormula(), " = ", self$.getSampleSize(),
                                functionName = ".initAccrualIntensityAbsolute",
                                parameter = "maxNumberOfSubjects",
                                value = self$maxNumberOfSubjects,
                                diagnosticId = "accrual.subject_total_mismatch"
                            )
                        }
                    } else {
                        if (!self$absoluteAccrualIntensityEnabled &&
                                self$isUserDefinedParameter("accrualIntensity") &&
                                self$isDefaultParameter("accrualTime") &&
                                self$isUserDefinedParameter("maxNumberOfSubjects")) {
                            if (self$.showWarnings) {
                                warnArgumentIgnored("'accrualIntensity' (", self$accrualIntensity, ") will be ignored",
                                    parameter = "accrualIntensity",
                                    value = self$accrualIntensity,
                                    diagnosticId = "accrual.relative_intensity_derived"
                                )
                            }
                            self$accrualIntensityRelative <- C_ACCRUAL_INTENSITY_DEFAULT
                            self$accrualIntensity <- accrualIntensityAbsolute
                            self$.setParameterType("accrualIntensity", C_PARAM_DERIVED)
                            self$.setParameterType("remainingTime", C_PARAM_NOT_APPLICABLE)
                        } else {
                            self$accrualIntensityRelative <- self$accrualIntensity
                            self$accrualIntensity <- accrualIntensityAbsolute
                            self$.setParameterType("accrualIntensity", C_PARAM_DERIVED)
                            self$.setParameterType("accrualIntensityRelative", C_PARAM_USER_DEFINED)
                        }
                    }
                }
            }
        },
        .isNoPiecewiseAccrualTime = function(accrualTimeArg) {
            if (length(accrualTimeArg) == 0 || anyNA(accrualTimeArg) ||
                    !all(is.numeric(accrualTimeArg))) {
                stopRuntimeIssue("'accrualTimeArg' must a be valid numeric vector",
                    functionName = ".isNoPiecewiseAccrualTime",
                    parameter = "accrualTimeArg",
                    value = accrualTimeArg
                )
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
                stopMissingArgument("'accrualTime' must be defined",
                    functionName = ".init",
                    parameter = "accrualTime"
                )
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
                    self$.setParameterType("accrualTime", ifelse(equals(self$accrualTime, C_ACCRUAL_TIME_DEFAULT),
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
                        self$isUserDefinedParameter("maxNumberOfSubjects")
                    self$followUpTimeMustBeUserDefined <- !self$endOfAccrualIsUserDefined &&
                        !self$maxNumberOfSubjectsIsUserDefined
                    self$absoluteAccrualIntensityEnabled <- FALSE

                    if (self$maxNumberOfSubjectsIsUserDefined) {
                        self$accrualIntensity <- self$maxNumberOfSubjects / self$accrualTime[length(self$accrualTime)]
                        self$.setParameterType("accrualIntensity", C_PARAM_DERIVED)
                    }

                    return(invisible())
                }

                self$accrualTime <- accrualTimeArg
                if (length(self$accrualTime) == 0) {
                    stopIllegalArgument("'accrualTime' must contain at least one time value",
                        functionName = ".init",
                        parameter = "accrualTime"
                    )
                }

                if (self$accrualTime[1] != 0) {
                    stopIllegalArgument(
                        "the first value of 'accrualTime' (", .arrayToString(self$accrualTime), ") must be 0",
                        functionName = ".init",
                        parameter = "accrualTime", value = self$accrualTime
                    )
                }

                self$.setParameterType("accrualTime", ifelse(
                    equals(self$accrualTime, C_ACCRUAL_TIME_DEFAULT),
                    C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
                ))
                self$.setParameterType("accrualIntensity", C_PARAM_USER_DEFINED)
            } else {
                stopIllegalArgument("'accrualTime' must be a list or a numeric vector",
                    functionName = ".init",
                    parameter = "accrualTime"
                )
            }

            if (is.na(self$absoluteAccrualIntensityEnabled)) {
                self$absoluteAccrualIntensityEnabled <- self$.isAbsoluteAccrualIntensity(self$accrualIntensity)
            }
            if (is.null(self$maxNumberOfSubjects) || length(self$maxNumberOfSubjects) == 0 ||
                    anyNA(self$maxNumberOfSubjects)) {
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
                    stopIllegalArgument(
                        "length of 'accrualTime' (", length(self$accrualTime),
                        ") must be equal to length of 'accrualIntensity' if the last 'accrualTime' ",
                        "shall be calculated ",
                        "based on 'maxNumberOfSubjects' or length of 'accrualIntensity' (", length(self$accrualIntensity),
                        ") + 1 otherwise",
                        functionName = ".init",
                        parameter = "accrualTime",
                        value = length(self$accrualTime),
                        relatedParameter = "accrualIntensity",
                        relatedValue = length(self$accrualIntensity)
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
                        stopIllegalArgument(
                            "'maxNumberOfSubjects' (", self$maxNumberOfSubjects, ") ",
                            "must be >= ", self$accrualIntensity[1],
                            " ('accrualIntensity')",
                            functionName = ".init",
                            parameter = "maxNumberOfSubjects",
                            value = self$maxNumberOfSubjects,
                            relatedParameter = "accrualIntensity",
                            relatedValue = self$accrualIntensity[1]
                        )
                    }
                    self$remainingTime <- self$accrualTime
                    self$.setParameterType("remainingTime", C_PARAM_USER_DEFINED)
                } else if (length(self$accrualTime) > 1) {
                    sampleSize <- self$.getSampleSize()
                    if (!equals(sampleSize, self$maxNumberOfSubjects, tolerance = 1e-04)) {
                        if (length(self$maxNumberOfSubjects) == 1 && !is.na(self$maxNumberOfSubjects) &&
                                self$maxNumberOfSubjects > 0 && self$maxNumberOfSubjects < sampleSize) {
                            if (length(self$accrualIntensity) == 1 && length(self$accrualTime) == 1) {
                                self$.setParameterType("maxNumberOfSubjects", C_PARAM_USER_DEFINED)
                                self$accrualTime <- 0
                                self$.calculateRemainingTime()
                            } else {
                                if (length(self$accrualTime) == length(self$accrualIntensity) + 1 &&
                                        self$absoluteAccrualIntensityEnabled) {
                                    stopConflictingArguments(
                                        "'maxNumberOfSubjects' (", self$maxNumberOfSubjects,
                                        ") disagrees with ", "the defined accrual time and intensity: ",
                                        self$.getFormula(), " = ", sampleSize,
                                        functionName = ".init",
                                        parameter = "maxNumberOfSubjects",
                                        value = self$maxNumberOfSubjects,
                                        diagnosticId = "accrual.subject_total_mismatch"
                                    )
                                } else {
                                    stopIllegalArgument(
                                        "'maxNumberOfSubjects' (", self$maxNumberOfSubjects,
                                        ") ", "must be >= ", sampleSize,
                                        functionName = ".init",
                                        parameter = "maxNumberOfSubjects",
                                        value = self$maxNumberOfSubjects
                                    )
                                }
                            }
                        } else {
                            if ((length(self$maxNumberOfSubjects) != 1 || is.na(self$maxNumberOfSubjects)) &&
                                    self$absoluteAccrualIntensityEnabled) {
                                self$maxNumberOfSubjects <- sampleSize
                                self$.setParameterType("maxNumberOfSubjects", C_PARAM_DERIVED)
                            }
                            self$remainingTime <- self$accrualTime[length(self$accrualTime)] -
                                self$accrualTime[length(self$accrualTime) - 1]
                            self$.setParameterType(
                                "remainingTime",
                                ifelse(!equals(0, self$remainingTime, tolerance = 1e-06),
                                    C_PARAM_GENERATED, C_PARAM_NOT_APPLICABLE
                                )
                            )
                        }
                    }
                }
            }

            self$.validateInitialization()

            self$maxNumberOfSubjectsIsUserDefined <- self$isUserDefinedParameter("maxNumberOfSubjects")
            self$followUpTimeMustBeUserDefined <- !self$endOfAccrualIsUserDefined &&
                !self$maxNumberOfSubjectsIsUserDefined
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
                            warnArgumentAdjusted("Last accrual time value (",
                                accrualTimeTemp[length(accrualTimeTemp)], ") ignored",
                                diagnosticId = "accrual.schedule_truncated"
                            )
                        } else if (n1 > 1) {
                            warnArgumentAdjusted("Last ", n1, " accrual time values (",
                                .arrayToString(accrualTimeTemp[(length(accrualTimeTemp) - n1 + 1):length(accrualTimeTemp)]),
                                ") ignored",
                                diagnosticId = "accrual.schedule_truncated"
                            )
                        }

                        if (n2 == 1) {
                            warnArgumentAdjusted("Last accrual intensity value (",
                                accrualIntensityTemp[length(accrualIntensityTemp)], ") ignored",
                                diagnosticId = "accrual.schedule_truncated"
                            )
                        } else if (n2 > 1) {
                            warnArgumentAdjusted("Last ", n2, " accrual intensity values (",
                                .arrayToString(accrualIntensityTemp[i2:length(accrualIntensityTemp)]),
                                ") ignored",
                                diagnosticId = "accrual.schedule_truncated"
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
                stopIllegalArgument(
                    "'maxNumberOfSubjects' (", self$maxNumberOfSubjects, ") ",
                    "is too small for the defined accrual time (minimum = ",
                    sampleSize, ")",
                    functionName = ".calculateRemainingTime",
                    parameter = "maxNumberOfSubjects",
                    value = self$maxNumberOfSubjects,
                    diagnosticId = "accrual.subject_cap_below_recruited_total"
                )
            }

            lastAccrualIntensity <- self$accrualIntensity[length(self$accrualIntensity)]
            self$remainingTime <- remainingSubjects / lastAccrualIntensity
            self$.setParameterType(
                "remainingTime",
                ifelse(!equals(0, self$remainingTime, tolerance = 1e-06),
                    C_PARAM_GENERATED, C_PARAM_NOT_APPLICABLE
                )
            )
            if (length(self$accrualTime) == length(self$accrualIntensity)) {
                if (self$isGeneratedParameter("remainingTime")) {
                    self$accrualTimeOriginal <- self$accrualTime
                    if (identical(self$accrualTime, c(0, 12)) || identical(self$accrualTime, C_ACCRUAL_TIME_DEFAULT)) {
                        self$.setParameterType("accrualTimeOriginal", C_PARAM_DEFAULT_VALUE)
                    } else {
                        self$.setParameterType("accrualTimeOriginal", C_PARAM_USER_DEFINED)
                    }
                    self$.setParameterType("accrualTime", C_PARAM_DERIVED)
                }
                self$accrualTime <- c(self$accrualTime, self$accrualTime[length(self$accrualTime)] + self$remainingTime)
            }
            if (any(self$accrualTime < 0)) {
                if (!stopInCaseOfError) {
                    return(invisible())
                }
                stopIllegalArgument(
                    "'maxNumberOfSubjects' (", self$maxNumberOfSubjects, ") ",
                    "is too small for the defined accrual time",
                    functionName = ".calculateRemainingTime",
                    parameter = "maxNumberOfSubjects",
                    value = self$maxNumberOfSubjects,
                    diagnosticId = "accrual.subject_cap_below_recruited_total"
                )
            }
        },
        .validateAccrualTimeAndIntensity = function() {
            if ((length(self$accrualTime) >= 2 && any(self$accrualTime[2:length(self$accrualTime)] < 0))) {
                stopIllegalArgument(
                    "'accrualTime' (", .arrayToString(self$accrualTime), ") must be > 0",
                    functionName = ".validateAccrualTimeAndIntensity",
                    parameter = "accrualTime",
                    value = self$accrualTime
                )
            }

            .assertValuesAreStrictlyIncreasing(self$accrualTime, "accrualTime")

            if ((length(self$accrualTime) > 1) && any(self$accrualIntensity < 0)) {
                stopIllegalArgument(
                    "'accrualIntensity' (", .arrayToString(self$accrualIntensity), ") must be >= 0",
                    functionName = ".validateAccrualTimeAndIntensity",
                    parameter = "accrualIntensity",
                    value = self$accrualIntensity
                )
            }

            if (length(self$accrualIntensity) == 1 && !is.na(self$accrualIntensity) &&
                    self$accrualIntensity == 0) {
                stopIllegalArgument(
                    "at least one 'accrualIntensity' value must be > 0",
                    functionName = ".validateAccrualTimeAndIntensity",
                    parameter = "accrualIntensity"
                )
            }

            if (length(self$accrualIntensity) > 0 && self$accrualIntensity[1] == 0) {
                warnInvalidInput(
                    "It makes no sense to start 'accrualIntensity' (",
                    .arrayToString(self$accrualIntensity), ") with 0",
                    diagnosticId = "accrual.initial_intensity_zero"
                )
            }
        },
        .validateInitialization = function() {
            self$.validateAccrualTimeAndIntensity()

            self$piecewiseAccrualEnabled <- !self$.isNoPiecewiseAccrualTime(self$accrualTime)
        }
    )
)

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
getAccrualTime <- function(
        accrualTime = NA_real_,
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
            .warnInCaseOfUnusedArgument(
                accrualIntensity,
                "accrualIntensity", NA_real_, "getAccrualTime"
            )
        }
        .warnInCaseOfUnusedArgument(
            maxNumberOfSubjects,
            "maxNumberOfSubjects", NA_real_, "getAccrualTime"
        )
    }

    if (inherits(accrualTime, "AccrualTime")) {
        return(accrualTime)
    }

    if (inherits(accrualTime, "TrialDesignPlanSurvival")) {
        return(accrualTime$.accrualTime)
    }

    accrualIntensityType <- match.arg(accrualIntensityType)

    accrualIntensity <- .assertIsNumericVector(accrualIntensity, "accrualIntensity", naAllowed = TRUE)
    .assertIsValidMaxNumberOfSubjects(maxNumberOfSubjects, naAllowed = TRUE)
    .assertIsSingleCharacter(accrualIntensityType, "accrualIntensityType")

    if (!is.null(accrualTime) && is.numeric(accrualTime) && !all(is.na(accrualTime)) &&
            !all(is.na(accrualIntensity))) {
        accrualTimeLength <- length(accrualTime)
        accrualIntensityLength <- length(accrualIntensity)
        if (!accrualIntensityLength %in% c(accrualTimeLength, accrualTimeLength - 1)) {
            stopIllegalArgument("length of 'accrualIntensity' (", accrualIntensityLength,
                ") must be ", "equal to the length of 'accrualTime' (",
                accrualTimeLength, ") or ", "one element shorter",
                functionName = "getAccrualTime",
                parameter = "accrualIntensity",
                value = accrualIntensity,
                relatedParameter = "accrualTime",
                relatedValue = accrualTime
            )
        }
    }

    absoluteAccrualIntensityEnabled <- NA
    if (accrualIntensityType == "absolute") {
        absoluteAccrualIntensityEnabled <- TRUE
        if (!all(is.na(accrualIntensity)) && any(na.omit(accrualIntensity) < 1)) {
            stopConflictingArguments(
                "'accrualIntensityType' is 'absolute' and the 'accrualIntensity' (",
                .arrayToString(accrualIntensity),
                ") therefore must be >= 1",
                functionName = "getAccrualTime",
                parameter = "accrualIntensityType",
                value = accrualIntensityType,
                relatedParameter = "accrualIntensity",
                relatedValue = accrualIntensity,
                diagnosticId = "accrual.absolute_intensity_invalid"
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
