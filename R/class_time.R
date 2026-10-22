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
            return(self$.getRegexpFromTo(
                from = "0",
                to = C_REGEXP_DECIMAL_NUMBER,
                toPrefix = C_REGEXP_SMALLER
            ))
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
                        stopIllegalArgument(
                            "the name of the first region must have the format ",
                            "\"<time\" or \"0 - <time\", e.g., \"<5\" or \"0 - <5\"",
                            functionName = ".validateTimePeriod"
                        )
                    }
                }
                if (grepl(self$.getRegexpSmallerThan(), timePeriod, perl = TRUE)) {
                    timePeriod <- sub("^ *< *", "0 - <", timePeriod)
                }
                if (!accrualTimeMode && n == 1 && !grepl("(0 *- ?)?<?\\?|x|\\.", timePeriod)) {
                    warnArgumentIgnored("Defined time period \"", timePeriod, "\" will be ignored ",
                        "because 'piecewiseSurvivalTime' list has only 1 entry",
                        diagnosticId = "survival.single_interval_label_ignored"
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
                        stopIllegalArgument(
                            "the name of the last region must have the format \"time\", ",
                            "\">=time\", \"time - Inf\" or \"time1 - <=time2\", ",
                            "e.g., \"20\", \">=20\" or \"20 - Inf\" or \"20 - <=30\"",
                            functionName = ".validateTimePeriod"
                        )
                    }
                    if (grepl(
                            self$.getRegexpOr(
                                self$.getRegexpGreaterOrEqualThan(),
                                self$.getRegexpDecimalRangeEnd()
                            ),
                            timePeriod,
                            perl = TRUE
                        )) {
                        endOfAccrualIsUndefined <- TRUE
                    }
                    timePeriod <- gsub("([Inf >=\\?x]*)|-", "", timePeriod)
                } else {
                    if (!grepl(
                            self$.getRegexpOr(
                                self$.getRegexpGreaterOrEqualThan(),
                                self$.getRegexpDecimalRangeEnd()
                            ),
                            timePeriod,
                            perl = TRUE
                        )) {
                        stopIllegalArgument("the name of the last region must have the format ",
                            "\">=time\" or \"time - Inf\", e.g., \">=20\" or \"20 - Inf\"",
                            functionName = ".validateTimePeriod"
                        )
                    }
                }
            } else {
                if (!grepl(self$.getRegexpDecimalRange(), timePeriod, perl = TRUE)) {
                    stopIllegalArgument("the name of the inner regions must have ",
                        "the format \"time_1 - <time_2\", e.g., \"5 - <20\"",
                        functionName = ".validateTimePeriod"
                    )
                }
            }

            if (accrualTimeMode) {
                return(list(
                    timePeriod = timePeriod,
                    endOfAccrualIsUndefined = endOfAccrualIsUndefined
                ))
            }

            return(timePeriod)
        }
    )
)

