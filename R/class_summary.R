## |
## |  *Summary classes and functions*
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

#' @include f_core_utilities.R
#' @include f_core_assertions.R
NULL

SummaryItem <- R6::R6Class("SummaryItem",
    public = list(
        title = NULL,
        values = NULL,
        legendEntry = NULL,
        initialize = function(
                title = NA_character_,
                values = NA_character_,
                legendEntry = NULL,
                ...) {
            self$title <- title
            self$values <- values
            self$legendEntry <- legendEntry

            if (!is.null(self$legendEntry) && length(self$legendEntry) > 0) {
                if (is.null(names(self$legendEntry))) {
                    stopIllegalArgument(sQuote("legendEntry"), " must be a named list",
                        functionName = "initialize",
                        parameter = "legendEntry",
                        value = legendEntry
                    )
                }
                for (l in self$legendEntry) {
                    if (length(l) == 0) {
                        stopIllegalArgument(sQuote("legendEntry"), " must be not empty",
                            functionName = "initialize",
                            parameter = "legendEntry",
                            value = legendEntry
                        )
                    }
                }
            }
        },
        show = function() {
            cat(self$title, "=", self$values, "\n")
        },
        toList = function() {
            result <- list()
            result[[self$title]] <- self$values
        }
    )
)

#' @name SummaryFactory
#'
#' @title
#' Summary Factory
#'
#' @description
#' Basic class for summaries
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
SummaryFactory <- R6::R6Class("SummaryFactory",
    inherit = ParameterSet,
    public = list(
        object = NULL,
        title = NULL,
        header = NULL,
        summaryItems = NULL,
        intervalFormat = NULL,
        justify = NULL,
        output = NULL,
        markdown = NULL,
        initialize = function(
                ...,
                object = NULL,
                intervalFormat = "[%s; %s]",
                output = "all",
                markdown = FALSE) {
            super$initialize(...)
            self$object <- object
            self$intervalFormat <- intervalFormat
            self$output <- output
            self$markdown <- markdown
            self$summaryItems <- list()
            self$justify <- .getEnvironmentVariable(
                "RPACT_SUMMARY_JUSTIFY",
                "rpact.summary.justify",
                default = "right",
                type = "character"
            )
        },
        show = function(showType = 1, digits = NA_integer_) {
            self$.show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
        },
        .show = function(showType = 1, digits = NA_integer_, ..., consoleOutputEnabled = TRUE) {
            if (self$output %in% c("all", "title")) {
                if (is.null(self$title) || length(self$title) == 0) {
                    self$title <- .createSummaryTitleObject(self$object)
                }
                if (!is.null(self$title) &&
                        length(self$title) == 1 &&
                        trimws(self$title) != "") {
                    self$.cat(self$title, "\n\n",
                        heading = 1,
                        consoleOutputEnabled = consoleOutputEnabled
                    )
                }
            }

            if (self$output %in% c("all", "overview")) {
                if (is.null(self$header) || length(self$header) == 0) {
                    self$header <- .createSummaryHeaderObject(self$object, self, digits)
                }
                if (!is.null(self$header) &&
                        length(self$header) == 1 &&
                        trimws(self$header) != "") {
                    self$.cat(self$header, "\n\n",
                        consoleOutputEnabled = consoleOutputEnabled
                    )
                }
            }

            if (!(self$output %in% c("all", "body"))) {
                return(invisible())
            }

            legendEntries <- c()
            legendEntriesUnique <- c()
            summaryItemNames <- c()
            for (summaryItem in self$summaryItems) {
                if (!is.null(summaryItem$title) &&
                        length(summaryItem$title) == 1 &&
                        !is.na(summaryItem$title)) {
                    summaryItemNames <- c(summaryItemNames, summaryItem$title)
                }
                if (length(summaryItem$legendEntry) > 0) {
                    a <- sort(names(summaryItem$legendEntry))
                    for (aa in a) {
                        if (!(aa %in% legendEntriesUnique)) {
                            legendEntriesUnique <- c(legendEntriesUnique, aa)
                            b <- summaryItem$legendEntry[[aa]]
                            legendEntries <- c(legendEntries, paste0("  ", aa, ": ", b))
                        }
                    }
                }
            }
            summaryItemNames <- paste0(format(summaryItemNames), " ")

            na <- ifelse(.isDataset(self$object), "NA", NA_character_)
            tableColumns <- 0
            maxValueWidth <- 1
            if (length(self$summaryItems) > 0) {
                for (i in seq_len(length(self$summaryItems))) {
                    validValues <- na.omit(self$summaryItems[[i]]$values)
                    if (length(validValues) > 0) {
                        w <- max(nchar(validValues))
                        maxValueWidth <- max(maxValueWidth, w)
                        tableColumns <- max(tableColumns, 1 + length(validValues))
                    }
                }
                spaceString <- paste0(rep(" ", maxValueWidth + 1), collapse = "")
                for (i in seq_len(length(self$summaryItems))) {
                    itemTitle <- self$summaryItems[[i]]$title
                    if (!is.null(itemTitle) && length(itemTitle) == 1 && !is.na(itemTitle)) {
                        summaryItemName <- summaryItemNames[i]
                        values <- self$summaryItems[[i]]$values
                        values <- trimws(values)
                        indices <- !grepl("(\\])$", values)
                        values[indices] <- paste0(values[indices], " ")
                        values <- format(c(spaceString, values),
                            justify = self$justify
                        )[2:(length(values) + 1)]
                        self$.cat(summaryItemName, values, "\n",
                            tableColumns = tableColumns,
                            consoleOutputEnabled = consoleOutputEnabled, na = na
                        )
                        if (!consoleOutputEnabled && trimws(summaryItemName) == "Stage") {
                            self$.cat(rep("----- ", tableColumns), "\n",
                                tableColumns = tableColumns,
                                consoleOutputEnabled = consoleOutputEnabled, na = na
                            )
                        }
                    }
                }
            }

            if (length(legendEntries) > 0) {
                self$.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
                self$.cat("Legend:\n", consoleOutputEnabled = consoleOutputEnabled)
                if (!consoleOutputEnabled) {
                    self$.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
                }
                for (legendEntry in legendEntries) {
                    self$.cat(legendEntry, "\n", consoleOutputEnabled = consoleOutputEnabled)
                }
                self$.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
            }
        },
        addItem = function(title, values, legendEntry = list()) {
            if (!is.character(values)) {
                values <- as.character(values)
            }
            tryCatch(
                {
                    self$addSummaryItem(SummaryItem$new(
                        title = title, values = values, legendEntry = legendEntry
                    ))
                },
                error = function(e) {
                    stopRuntimeIssue("failed to add summary item ",
                        .pQuote(title), " = ", .arrayToString(values),
                        " (class: ", .getClassName(values), "): ", e$message,
                        functionName = "addItem",
                        parameter = "title",
                        value = title,
                        relatedParameter = "values",
                        relatedValue = values
                    )
                }
            )
        },
        addSummaryItem = function(summaryItem) {
            if (!inherits(summaryItem, "SummaryItem")) {
                stopIllegalArgument("'summaryItem' must be an instance of class ",
                    "'SummaryItem' (was ", .getClassName(summaryItem, quote = TRUE), ")",
                    functionName = "addSummaryItem",
                    parameter = "summaryItem",
                    value = summaryItem,
                    relatedParameter = "SummaryItem"
                )
            }
            self$summaryItems <- c(self$summaryItems, summaryItem)
        },
        .getFormattedParameterValue = function(valuesToShow, valuesToShow2) {
            naText <- .getEnvironmentVariable("RPACT_SUMMARY_NA", "rpact.summary.na",
                default = "", type = "character"
            )
            if (length(valuesToShow) == length(valuesToShow2) && !all(is.na(valuesToShow2))) {
                for (variantIndex in seq_len(length(valuesToShow))) {
                    value1 <- trimws(as.character(valuesToShow[variantIndex]))
                    value2 <- trimws(as.character(valuesToShow2[variantIndex]))
                    if (grepl("^ *NA *$", value1)) {
                        value1 <- naText
                    }
                    if (grepl("^ *NA *$", value2)) {
                        value2 <- naText
                    }
                    if (value1 == "" && value2 == "") {
                        valuesToShow[variantIndex] <- naText
                    } else {
                        valuesToShow[variantIndex] <- sprintf(
                            self$intervalFormat, value1, value2
                        )
                    }
                }
            } else {
                valuesToShow[is.na(valuesToShow) | trimws(valuesToShow) == "NA"] <- naText
            }

            return(valuesToShow)
        },
        addParameter = function(
                parameterSet,
                ...,
                parameterName = NULL,
                values = NULL,
                parameterCaption,
                roundDigits = NA_integer_,
                ceilingEnabled = FALSE,
                cumsumEnabled = FALSE,
                twoSided = FALSE,
                transpose = FALSE,
                smoothedZeroFormat = FALSE,
                parameterCaptionSingle = parameterCaption,
                legendEntry = list(),
                enforceFirstCase = FALSE,
                formatRepeatedPValues = FALSE,
                validateParameterType = TRUE,
                lastStage = NA_integer_,
                roundDigitsAsInformation = FALSE,
                showNA = FALSE) {
            .addParameterToSummaryFactory(
                summaryFactory = self,
                parameterSet = parameterSet,
                parameterName = parameterName,
                values = values,
                parameterCaption = parameterCaption,
                roundDigits = roundDigits,
                ceilingEnabled = ceilingEnabled,
                cumsumEnabled = cumsumEnabled,
                twoSided = twoSided,
                transpose = transpose,
                smoothedZeroFormat = smoothedZeroFormat,
                parameterCaptionSingle = parameterCaptionSingle,
                legendEntry = legendEntry,
                enforceFirstCase = enforceFirstCase,
                formatRepeatedPValues = formatRepeatedPValues,
                validateParameterType = validateParameterType,
                lastStage = lastStage,
                roundDigitsAsInformation = roundDigitsAsInformation,
                showNA = showNA
            )
        },
        .isEnrichmentObject = function(parameterSet) {
            return(
                .isEnrichmentAnalysisResults(parameterSet) ||
                    .isEnrichmentStageResults(parameterSet) ||
                    .isEnrichmentConditionalPowerResults(parameterSet) ||
                    (inherits(parameterSet, "ClosedCombinationTestResults") &&
                        isTRUE(parameterSet$.enrichment))
            )
        },
        .getInnerValues = function(values, transpose = FALSE) {
            if (!is.matrix(values)) {
                return(values)
            }

            if (nrow(values) == 1 && ncol(values) == 1) {
                return(values[1, 1])
            }

            if (transpose) {
                return(values[1, ])
            }

            return(values[, 1])
        },
        .getColumnValues = function(
                parameterName,
                values,
                variantIndex,
                transposed = FALSE) {
            tryCatch(
                {
                    if (transposed) {
                        if (!is.matrix(values)) {
                            return(values)
                        }

                        if (nrow(values) == 0) {
                            return("")
                        }

                        if (nrow(values) == 1 && ncol(values) == 1) {
                            colValues <- values[1, 1]
                        } else if (nrow(values) == 1) {
                            colValues <- values[1, variantIndex]
                        } else if (ncol(values) == 1) {
                            colValues <- values[variantIndex, 1]
                        } else {
                            colValues <- values[variantIndex, ]
                        }
                        return(colValues)
                    }

                    if (length(values) <= 1 && !is.matrix(values)) {
                        colValues <- values
                    } else if (is.matrix(values)) {
                        if (length(values) == 0 || nrow(values) == 0 || ncol(values) == 0) {
                            return("")
                        }

                        if (nrow(values) == 1 && ncol(values) == 1) {
                            colValues <- values[1, 1]
                        } else if (ncol(values) == 1) {
                            colValues <- values[variantIndex, 1]
                        } else if (nrow(values) == 1) {
                            colValues <- values[1, variantIndex]
                        } else {
                            if (ncol(values) == 0) {
                                return("")
                            }

                            colValues <- values[, variantIndex]
                        }
                    } else {
                        colValues <- values[variantIndex]
                    }
                    return(colValues)
                },
                error = function(e) {
                    stopRuntimeIssue(".getColumnValues(", dQuote(parameterName), "): ",
                        e$message, "; .getClassName(values) = ",
                        .getClassName(values), "; dim(values) = ",
                        .arrayToString(dim(values), vectorLookAndFeelEnabled = TRUE),
                        "; variantIndex = ", variantIndex, "; transposed = ", transposed,
                        parameter = parameterName, value = values,
                        functionName = ".getColumnValues"
                    )
                }
            )
        }
    )
)
