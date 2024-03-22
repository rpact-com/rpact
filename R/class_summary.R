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
## |  File version: $Revision: 7668 $
## |  Last changed: $Date: 2024-02-26 10:47:27 +0100 (Mo, 26 Feb 2024) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_core_utilities.R
#' @include f_core_assertions.R
NULL

SummaryItem <- R6::R6Class("SummaryItem",
    public = list(
        title = NULL,
        values = NULL,
        legendEntry = NULL,
        initialize = function(title = NA_character_, values = NA_character_, legendEntry = NULL, ...) {
            self$title <- title
            self$values <- values
            self$legendEntry <- legendEntry

            if (!is.null(self$legendEntry) && length(self$legendEntry) > 0) {
                if (is.null(names(self$legendEntry))) {
                    stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, sQuote("legendEntry"), " must be a named list")
                }
                for (l in self$legendEntry) {
                    if (length(l) == 0) {
                        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, sQuote("legendEntry"), " must be not empty")
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

#'
#' @title
#' Summary Factory Plotting
#'
#' @param x The summary factory object.
#' @param y Not available for this kind of plot (is only defined to be compatible to the generic plot function).
#' @param showSummary Show the summary before creating the plot output, default is \code{FALSE}.
#' @inheritParams param_three_dots_plot
#'
#' @description
#' Plots a summary factory.
#'
#' @details
#' Generic function to plot all kinds of summary factories.
#'
#' @template return_object_ggplot
#'
#' @export
#'
plot.SummaryFactory <- function(x, y, ..., showSummary = FALSE) {
    fCall <- match.call(expand.dots = FALSE)
    if (isTRUE(showSummary) || .isSummaryPipe(fCall)) {
        markdown <- .getOptionalArgument("markdown", ..., optionalArgumentDefaultValue = NA)
        if (is.na(markdown)) {
            markdown <- .isMarkdownEnabled()
        }
        if (markdown) {
            x$.catMarkdownText()
        } else {
            x$show()
        }
    }
    plot(x = x$object, y = y, ...)
}

#'
#' @title
#' Print Summary Factory in Markdown Code Chunks
#'
#' @description
#' The function `knit_print.SummaryFactory` is the default printing function for rpact summary objects in knitr.
#' The chunk option `render` uses this function by default.
#' To fall back to the normal printing behavior set the chunk option `render = normal_print`.
#' For more information see \code{\link[knitr]{knit_print}}.
#'
#' @param x A \code{SummaryFactory}.
#' @param  ... Other arguments (see \code{\link[knitr]{knit_print}}).
#'
#' @details
#' Generic function to print a summary object in Markdown.
#' Use \code{options("rpact.print.heading.base.number" = "NUMBER")} (where \code{NUMBER} is an integer value >= -1) to
#' specify the heading level. The default is \code{options("rpact.print.heading.base.number" = "0")}, i.e., the
#' top headings start with \code{##} in Markdown. \code{options("rpact.print.heading.base.number" = "-1")} means
#' that all headings will be written bold but are not explicit defined as header.
#'
#' @export
#'
knit_print.SummaryFactory <- function(x, ...) {
    result <- paste0(utils::capture.output(x$.catMarkdownText()), collapse = "\n")

    if (isTRUE(base::attr(x$object, "printObject"))) {
        sep <- base::attr(x$object, "printObjectSeparator")
        if (is.null(sep) || !is.character(sep)) {
            sep <- "\n-----\n\n"
        }
        result <- paste0(
            result, sep,
            paste0(utils::capture.output(x$object$.catMarkdownText()), collapse = "\n")
        )
    }

    return(knitr::asis_output(result))
}

#'
#' @title
#' Summary Factory Printing
#'
#' @param x The summary factory object.
#' @param markdown If \code{TRUE}, the object \code{x} will be printed using markdown syntax;
#'        normal representation will be used otherwise (default is \code{FALSE})
#' @param sep The separator line between the summary and the print output.
#' @inheritParams param_three_dots_plot
#'
#' @description
#' Prints the result object stored inside a summary factory.
#'
#' @details
#' Generic function to print all kinds of summary factories.
#'
#' @export
#'
print.SummaryFactory <- function(x, ...,
        markdown = NA,
        sep = "\n-----\n\n") {
    if (is.na(markdown)) {
        markdown <- .isMarkdownEnabled()
    }

    if (markdown) {
        result <- paste0(utils::capture.output(x$.catMarkdownText()), collapse = "\n")
        cat(result, "\n")
        return(invisible())
    }

    x$show()
}

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
        initialize = function(..., object = NULL, intervalFormat = "[%s; %s]", output = "all") {
            super$initialize(...)
            self$object <- object
            self$intervalFormat <- intervalFormat
            self$output <- output
            self$summaryItems <- list()
            self$justify <- getOption("rpact.summary.justify", "right")
        },
        show = function(showType = 1, digits = NA_integer_) {
            self$.show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
        },
        .show = function(showType = 1, digits = NA_integer_, ..., consoleOutputEnabled = TRUE) {
            if (self$output %in% c("all", "title")) {
                if (is.null(self$title) || length(self$title) == 0) {
                    self$title <- .createSummaryTitleObject(self$object)
                }
                if (!is.null(self$title) && length(self$title) == 1 && trimws(self$title) != "") {
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
                if (!is.null(self$header) && length(self$header) == 1 && trimws(self$header) != "") {
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
                if (!is.null(summaryItem$title) && length(summaryItem$title) == 1 && !is.na(summaryItem$title)) {
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
                for (i in 1:length(self$summaryItems)) {
                    validValues <- na.omit(self$summaryItems[[i]]$values)
                    if (length(validValues) > 0) {
                        w <- max(nchar(validValues))
                        maxValueWidth <- max(maxValueWidth, w)
                        tableColumns <- max(tableColumns, 1 + length(validValues))
                    }
                }
                spaceString <- paste0(rep(" ", maxValueWidth + 1), collapse = "")
                for (i in 1:length(self$summaryItems)) {
                    itemTitle <- self$summaryItems[[i]]$title
                    if (!is.null(itemTitle) && length(itemTitle) == 1 && !is.na(itemTitle)) {
                        summaryItemName <- summaryItemNames[i]
                        values <- self$summaryItems[[i]]$values
                        values <- trimws(values)
                        indices <- !grepl("(\\])$", values)
                        values[indices] <- paste0(values[indices], " ")
                        values <- format(c(spaceString, values), justify = self$justify)[2:(length(values) + 1)]
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
                    self$addSummaryItem(SummaryItem$new(title = title, values = values, legendEntry = legendEntry))
                },
                error = function(e) {
                    stop(
                        C_EXCEPTION_TYPE_RUNTIME_ISSUE, "failed to add summary item '", title,
                        "' = ", .arrayToString(values), " (class: ", .getClassName(values), "): ", e$message
                    )
                }
            )
        },
        addSummaryItem = function(summaryItem) {
            if (!inherits(summaryItem, "SummaryItem")) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'summaryItem' must be an instance of class 'SummaryItem' (was '", .getClassName(summaryItem), "')"
                )
            }
            self$summaryItems <- c(self$summaryItems, summaryItem)
        },
        .getFormattedParameterValue = function(valuesToShow, valuesToShow2) {
            naText <- getOption("rpact.summary.na", "")
            if (length(valuesToShow) == length(valuesToShow2) && !all(is.na(valuesToShow2))) {
                for (variantIndex in 1:length(valuesToShow)) {
                    value1 <- as.character(valuesToShow[variantIndex])
                    value2 <- as.character(valuesToShow2[variantIndex])
                    if (grepl("^ *NA *$", value1)) {
                        value1 <- naText
                    }
                    if (grepl("^ *NA *$", value2)) {
                        value2 <- naText
                    }
                    if (trimws(value1) == "" && trimws(value2) == "") {
                        valuesToShow[variantIndex] <- naText
                    } else {
                        valuesToShow[variantIndex] <- sprintf(self$intervalFormat, value1, value2)
                    }
                }
            } else {
                valuesToShow[is.na(valuesToShow) | trimws(valuesToShow) == "NA"] <- naText
            }

            return(valuesToShow)
        },
        addParameter = function(parameterSet, ...,
                parameterName = NULL, values = NULL, parameterCaption,
                roundDigits = NA_integer_, ceilingEnabled = FALSE, cumsumEnabled = FALSE,
                twoSided = FALSE, transpose = FALSE, smoothedZeroFormat = FALSE,
                parameterCaptionSingle = parameterCaption, legendEntry = list(),
                enforceFirstCase = FALSE, formatRepeatedPValues = FALSE,
                validateParameterType = TRUE) {
            if (!is.null(parameterName) && length(parameterName) == 1 &&
                    inherits(parameterSet, "ParameterSet") &&
                    parameterSet$.getParameterType(parameterName) == C_PARAM_NOT_APPLICABLE) {
                if (.getLogicalEnvironmentVariable("RPACT_DEVELOPMENT_MODE") && validateParameterType) {
                    warning(
                        "Failed to add parameter ", .arrayToString(parameterName), " (",
                        .arrayToString(values), ") stored in ",
                        .getClassName(parameterSet), " because the parameter has type C_PARAM_NOT_APPLICABLE"
                    )
                }

                return(invisible())
            }

            parameterName1 <- parameterName[1]
            if (!is.null(parameterName1) && is.character(parameterName1) && is.null(values)) {
                values <- parameterSet[[parameterName1]]
                if (is.null(values)) {
                    stop(
                        C_EXCEPTION_TYPE_RUNTIME_ISSUE, .getClassName(parameterSet),
                        " does not contain a field '", parameterName1, "'"
                    )
                }
            }

            parameterName2 <- NA_character_
            values2 <- NA_real_
            if (!is.null(parameterName) && length(parameterName) > 1) {
                parameterName2 <- parameterName[2]
                values2 <- parameterSet[[parameterName2]]
                parameterName <- parameterName[1]
                if (is.null(values2)) {
                    stop(
                        C_EXCEPTION_TYPE_RUNTIME_ISSUE, .getClassName(parameterSet),
                        " does not contain a field '", parameterName2, "'"
                    )
                }
            }

            if (is.null(values) && is.null(parameterName1)) {
                stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'parameterName' or 'values' must be defined")
            }

            if (transpose) {
                if (!is.matrix(values)) {
                    values <- as.matrix(values)
                } else {
                    values <- t(values)
                }
            }

            if (is.list(parameterSet) && is.matrix(values)) {
                parameterSet <- parameterSet[["parameterSet"]]
                if (is.null(parameterSet)) {
                    stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'parameterSet' must be added to list")
                }
            }

            parameterNames <- ""
            numberOfVariants <- 1
            numberOfStages <- ifelse(is.matrix(values), ncol(values), length(values))
            if (inherits(parameterSet, "ParameterSet")) {
                parameterNames <- parameterSet$.getVisibleFieldNamesOrdered()
                numberOfVariants <- .getMultidimensionalNumberOfVariants(parameterSet, parameterNames)
                numberOfStages <- parameterSet$.getMultidimensionalNumberOfStages(parameterNames)
            }

            stages <- parameterSet[["stages"]]
            if (is.null(stages) && !is.null(parameterSet[[".stageResults"]])) {
                stages <- parameterSet[[".stageResults"]][["stages"]]
            }
            if (is.null(stages) && inherits(parameterSet, "ClosedCombinationTestResults")) {
                stages <- parameterSet[[".design"]][["stages"]]
            }
            if (!is.null(stages) && length(stages) > 0) {
                numberOfStages <- max(na.omit(stages))
                if (is.matrix(values) && nrow(values) > 0) {
                    numberOfVariants <- nrow(values)
                }
                if (is.matrix(values) && ncol(values) > 0) {
                    numberOfStages <- ncol(values)
                }
            }

            if (!is.null(parameterSet[[".piecewiseSurvivalTime"]]) &&
                    isTRUE(parameterSet[[".piecewiseSurvivalTime"]]$delayedResponseEnabled)) {
                numberOfVariants <- 1
            }

            if (twoSided) {
                values <- 2 * values
            }

            caseCondition <- list(
                and1 = enforceFirstCase,
                and2 = inherits(parameterSet, "Dataset"),
                and3 = list(
                    or1 = list(
                        and1 = !transpose,
                        and2 = numberOfVariants == 1
                    ),
                    or2 = list(
                        and1 = !is.matrix(values),
                        and2 = (!transpose && ncol(values) == 1),
                        and3 = (transpose && nrow(values) == 1)
                    ),
                    or3 = list(
                        and1 = .isTrialDesign(parameterSet),
                        and2 = (numberOfStages > 1 && numberOfStages == length(values)),
                        and3 = length(values) != numberOfVariants,
                        and4 = length(values) == 1,
                        and5 = parameterName %in% c(
                            "futilityBoundsEffectScale",
                            "futilityBoundsEffectScaleLower",
                            "futilityBoundsEffectScaleUpper",
                            "futilityPerStage",
                            "earlyStop"
                        )
                    )
                )
            )

            if (.isConditionTrue(caseCondition, "or", showDebugMessages = FALSE)) {
                valuesToShow <- .getSummaryValuesFormatted(
                    parameterSet, parameterName1, values,
                    roundDigits = roundDigits,
                    ceilingEnabled = ceilingEnabled, cumsumEnabled = cumsumEnabled,
                    smoothedZeroFormat = smoothedZeroFormat,
                    formatRepeatedPValues = formatRepeatedPValues
                )

                if (parameterName1 %in% c("piControl", "overallPiControl", "overallPooledStDevs")) {
                    valuesToShow <- self$.getInnerValues(valuesToShow, transpose = TRUE)
                } else {
                    valuesToShow <- self$.getInnerValues(valuesToShow, transpose = transpose)
                }

                valuesToShow2 <- NA_real_
                if (!all(is.na(values2))) {
                    valuesToShow2 <- .getSummaryValuesFormatted(parameterSet,
                        parameterName1, values2,
                        roundDigits = roundDigits,
                        ceilingEnabled = ceilingEnabled, cumsumEnabled = cumsumEnabled,
                        smoothedZeroFormat = smoothedZeroFormat,
                        formatRepeatedPValues = formatRepeatedPValues
                    )
                    valuesToShow2 <- self$.getInnerValues(valuesToShow2, transpose = transpose)
                }

                valuesToShow <- self$.getFormattedParameterValue(valuesToShow, valuesToShow2)
                self$addItem(parameterCaptionSingle, valuesToShow, legendEntry)
            } else {
                if (!inherits(parameterSet, "ParameterSet")) {
                    stop(
                        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                        "for varied values 'parameterSet' must be an instance of ",
                        "class 'ParameterSet' (was '", .getClassName(parameterSet), "')"
                    )
                }

                transposed <- !transpose && grepl("MultiArm|Enrichment", .getClassName(parameterSet)) &&
                    (!is.matrix(values) || ncol(values) > 1)

                userDefinedEffectMatrix <- FALSE
                if (grepl("MultiArm|Enrichment", .getClassName(parameterSet)) ||
                        inherits(parameterSet, "AnalysisResultsConditionalDunnett") ||
                        inherits(parameterSet, "ClosedCombinationTestResults") ||
                        inherits(parameterSet, "ConditionalPowerResults")) {
                    if (grepl("SimulationResults(MultiArm|Enrichment)", .getClassName(parameterSet)) &&
                            parameterName %in% c(
                                "rejectAtLeastOne",
                                "earlyStop",
                                "futilityPerStage",
                                "successPerStage",
                                "expectedNumberOfSubjects",
                                "expectedNumberOfEvents",
                                "singleEventsPerArmAndStage",
                                "singleEventsPerSubsetAndStage",
                                "numberOfActiveArms",
                                "numberOfPopulations",
                                "conditionalPowerAchieved"
                            )) {
                        transposed <- TRUE
                        userDefinedEffectMatrix <-
                            parameterSet$.getParameterType("effectMatrix") == C_PARAM_USER_DEFINED
                        if (userDefinedEffectMatrix) {
                            legendEntry[["[j]"]] <- "effect matrix row j (situation to consider)"
                        }
                        if (grepl("Survival", .getClassName(parameterSet)) && !grepl("Enrichment", .getClassName(parameterSet))) {
                            legendEntry[["(i)"]] <- "results of treatment arm i vs. control arm"
                        }

                        if (grepl("SimulationResultsEnrichment", .getClassName(parameterSet))) {
                            variedParameterName <- .getSummaryVariedParameterNameEnrichment(parameterSet)
                            variedParameterValues <- parameterSet$effectList[[variedParameterName]]
                            if (variedParameterName == "piTreatments") {
                                variedParameterCaption <- "pi(treatment)"
                            } else {
                                variedParameterCaption <- .getParameterCaption(variedParameterName)
                                if (is.matrix(variedParameterValues) && ncol(variedParameterValues) == 1) {
                                    variedParameterCaption <- sub("s$", "", variedParameterCaption)
                                }
                            }
                            if (is.matrix(variedParameterValues)) {
                                numberOfVariants <- nrow(variedParameterValues)
                            } else {
                                numberOfVariants <- length(variedParameterValues)
                            }
                        } else {
                            variedParameterName <- .getVariedParameterSimulationMultiArm(parameterSet)
                            variedParameterValues <- parameterSet[[variedParameterName]]
                            variedParameterCaption <- .getParameterCaption(variedParameterName)
                            numberOfVariants <- length(variedParameterValues)
                        }
                        variedParameterCaption <- tolower(variedParameterCaption)
                    } else if (self$.isEnrichmentObject(parameterSet)) {
                        transposed <- TRUE
                        variedParameterCaption <- "populations"
                        if (parameterName1 %in% c(
                                "indices",
                                "conditionalErrorRate",
                                "secondStagePValues",
                                "adjustedStageWisePValues",
                                "overallAdjustedTestStatistics",
                                "rejectedIntersections"
                            )) {
                            if (.isEnrichmentAnalysisResults(parameterSet)) {
                                variedParameterValues <- parameterSet$.closedTestResults$.getHypothesisPopulationVariants()
                            } else {
                                variedParameterValues <- parameterSet$.getHypothesisPopulationVariants()
                            }
                        } else {
                            variedParameterValues <- c(paste0("S", 1:(numberOfVariants - 1)), "F")
                        }
                        numberOfVariants <- length(variedParameterValues)
                        legendEntry[["S[i]"]] <- "population i"
                        legendEntry[["F"]] <- "full population"
                    } else if (!inherits(parameterSet, "ClosedCombinationTestResults") ||
                            parameterName %in% c("rejected", "separatePValues")) {
                        if (inherits(parameterSet, "AnalysisResultsConditionalDunnett") &&
                                (!is.matrix(values) || ncol(values) > 1)) {
                            transposed <- TRUE
                        }

                        if (inherits(parameterSet, "ClosedCombinationTestResults") &&
                                parameterSet$.getParameterType("adjustedStageWisePValues") != "g" &&
                                parameterName == "separatePValues") {
                            transposed <- TRUE
                        }

                        if (inherits(parameterSet, "ClosedCombinationTestResults") &&
                                parameterName %in% c("rejected")) {
                            transposed <- TRUE
                        }

                        if (inherits(parameterSet, "ConditionalPowerResults") &&
                                parameterName %in% c("conditionalPower", "values")) {
                            transposed <- TRUE
                        }

                        variedParameterCaption <- "arm"
                        variedParameterValues <- 1:numberOfVariants
                        legendEntry[["(i)"]] <- "results of treatment arm i vs. control arm"
                    } else {
                        transposed <- TRUE
                        variedParameterCaption <- "arms"
                        variedParameterValues <- parameterSet$.getHypothesisTreatmentArmVariants()
                        numberOfVariants <- length(variedParameterValues)
                        legendEntry[["(i, j, ...)"]] <- "comparison of treatment arms 'i, j, ...' vs. control arm"
                    }
                } else {
                    if (inherits(parameterSet, "Dataset")) {
                        variedParameter <- "groups"
                    } else if (inherits(parameterSet, "PerformanceScore")) {
                        variedParameter <- ".alternative"
                    } else {
                        variedParameter <- parameterSet$.getVariedParameter(parameterNames, numberOfVariants)
                    }
                    if (is.null(variedParameter) || length(variedParameter) == 0 || variedParameter == "") {
                        if (.getLogicalEnvironmentVariable("RPACT_DEVELOPMENT_MODE")) {
                            warning(
                                "Failed to get varied parameter from ", .getClassName(parameterSet),
                                " (", length(parameterNames), " parameter names; numberOfVariants: ", numberOfVariants, ")"
                            )
                        }
                        return(invisible())
                    }

                    variedParameterCaption <- parameterSet$.getDataFrameColumnCaption(
                        variedParameter,
                        niceColumnNamesEnabled = TRUE
                    )
                    variedParameterCaption <- tolower(variedParameterCaption)

                    if (variedParameterCaption == "alternative" || variedParameterCaption == ".alternative") {
                        legendEntry[["alt."]] <- "alternative"
                        variedParameterCaption <- "alt."
                    } else if (variedParameterCaption == "hazard ratio") {
                        legendEntry[["HR"]] <- "hazard ratio"
                        variedParameterCaption <- "HR"
                    } else if (grepl("\\(1\\)$", variedParameterCaption)) {
                        groups <- parameterSet[["groups"]]
                        if (!is.null(groups) && length(groups) == 1 && groups == 1) {
                            variedParameterCaption <- sub(" \\(1\\)$", "", variedParameterCaption)
                        }
                    }

                    variedParameterValues <- round(parameterSet[[variedParameter]], 3)
                }

                for (variantIndex in 1:numberOfVariants) {
                    colValues <- self$.getColumnValues(parameterName, values, variantIndex, transposed)
                    colValues <- .getSummaryValuesFormatted(parameterSet, parameterName1,
                        colValues,
                        roundDigits = roundDigits,
                        ceilingEnabled = ceilingEnabled, cumsumEnabled = cumsumEnabled,
                        smoothedZeroFormat = smoothedZeroFormat,
                        formatRepeatedPValues = formatRepeatedPValues
                    )
                    colValues2 <- NA_real_
                    if (!all(is.na(values2))) {
                        colValues2 <- self$.getColumnValues(parameterName, values2, variantIndex, transposed)
                        colValues2 <- .getSummaryValuesFormatted(parameterSet, parameterName2, colValues2,
                            roundDigits = roundDigits, ceilingEnabled = ceilingEnabled,
                            cumsumEnabled = cumsumEnabled,
                            smoothedZeroFormat = smoothedZeroFormat,
                            formatRepeatedPValues = formatRepeatedPValues
                        )
                    }
                    colValues <- self$.getFormattedParameterValue(valuesToShow = colValues, valuesToShow2 = colValues2)

                    if (numberOfVariants == 1) {
                        self$addItem(parameterCaption, colValues, legendEntry)
                    } else if (self$.isEnrichmentObject(parameterSet)) {
                        self$addItem(paste0(
                            parameterCaption, " ",
                            variedParameterValues[variantIndex]
                        ), colValues, legendEntry)
                    } else if (
                        (grepl("MultiArm|Enrichment", .getClassName(parameterSet)) &&
                            !grepl("Simulation", .getClassName(parameterSet))) ||
                            inherits(parameterSet, "AnalysisResultsConditionalDunnett") ||
                            inherits(parameterSet, "ClosedCombinationTestResults") ||
                            inherits(parameterSet, "ConditionalPowerResults")) {
                        spacePrefix <- ifelse(parameterCaption %in% c("pi", "lambda", "median"), "", " ")
                        self$addItem(paste0(
                            parameterCaption, spacePrefix,
                            "(", variedParameterValues[variantIndex], ")"
                        ), colValues, legendEntry)
                    } else if (userDefinedEffectMatrix) {
                        self$addItem(paste0(parameterCaption, " [", variantIndex, "]"), colValues, legendEntry)
                    } else {
                        if (is.matrix(variedParameterValues) && ncol(variedParameterValues) > 1) {
                            variedParameterValuesFormatted <-
                                .arrayToString(variedParameterValues[variantIndex, ], vectorLookAndFeelEnabled = TRUE)
                        } else {
                            variedParameterValuesFormatted <- variedParameterValues[variantIndex]
                        }
                        self$addItem(
                            paste0(
                                parameterCaption, ", ",
                                variedParameterCaption, " = ", variedParameterValuesFormatted
                            ),
                            colValues, legendEntry
                        )
                    }
                }
            }
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
        .getColumnValues = function(parameterName, values, variantIndex, transposed = FALSE) {
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
                    stop(
                        ".getColumnValues(", dQuote(parameterName), "): ", e$message,
                        "; .getClassName(values) = ", .getClassName(values),
                        "; dim(values) = ", .arrayToString(dim(values), vectorLookAndFeelEnabled = TRUE),
                        "; variantIndex = ", variantIndex,
                        "; transposed = ", transposed
                    )
                }
            )
        }
    )
)

.formatSummaryValues <- function(values, digits, smoothedZeroFormat = FALSE, formatRepeatedPValues = FALSE) {
    if (is.na(digits)) {
        digits <- 3
    }

    if (digits < 1) {
        formattedValue <- as.character(values)
        formattedValue[is.na(formattedValue) | trimws(formattedValue) == "NA"] <- getOption("rpact.summary.na", "")
        return(formattedValue)
    }

    if (sum(is.na(values)) == length(values)) {
        formattedValue <- rep(getOption("rpact.summary.na", ""), length(values))
        return(formattedValue)
    }

    threshold <- 10^-digits
    text <- "<0."
    if (digits > 1) {
        for (i in 1:(digits - 1)) {
            text <- paste0(text, "0")
        }
    }
    text <- paste0(text, "1")

    if (smoothedZeroFormat) {
        values[abs(values) < 1e-15] <- 0
    }
    indices <- (!is.na(values) & values > 1e-10 & abs(values) < threshold)
    values[!is.na(values) & !indices] <- round(values[!is.na(values) & !indices], digits)
    if (sum(indices) > 0) {
        values[indices] <- threshold
        formattedValue <- .getFormattedValue(values, digits = digits, nsmall = digits, scientific = FALSE)
        formattedValue[indices] <- text
    } else {
        formattedValue <- .getFormattedValue(values, digits = digits, nsmall = digits, scientific = FALSE)
        formattedValue <- format(formattedValue, scientific = FALSE)
    }

    if (formatRepeatedPValues) {
        formattedValue[!is.na(formattedValue) &
            nchar(gsub("\\D", "", (formattedValue))) > 0 & formattedValue > 0.4999] <- ">0.5"
    }

    if (as.logical(getOption("rpact.summary.trim.zeroes", TRUE))) {
        zeroes <- grepl("^0\\.0*$", formattedValue)
        if (sum(zeroes) > 0) {
            formattedValue[zeroes] <- "0"
        }
    }

    formattedValue[is.na(formattedValue) | trimws(formattedValue) == "NA"] <- getOption("rpact.summary.na", "")

    return(formattedValue)
}

.getSummaryValuesFormatted <- function(fieldSet, parameterName, values,
        roundDigits = NA_integer_, ceilingEnabled = FALSE, cumsumEnabled = FALSE,
        smoothedZeroFormat = FALSE, formatRepeatedPValues = FALSE) {
    if (!is.numeric(values)) {
        return(values)
    }

    if (cumsumEnabled) {
        values <- cumsum(values)
    }

    if (ceilingEnabled) {
        values <- ceiling(values)
    } else {
        tryCatch(
            {
                formatFunctionName <- NULL

                if (!is.null(parameterName) && length(parameterName) == 1 && !is.na(parameterName)) {
                    if (parameterName == "futilityBounds") {
                        values[!is.na(values) & values <= -6] <- -Inf
                    } else if (parameterName %in% c("criticalValues", "decisionCriticalValue", "overallAdjustedTestStatistics")) {
                        design <- fieldSet
                        if (!.isTrialDesign(design)) {
                            design <- fieldSet[[".design"]]
                        }
                        if (!is.null(design) && .isTrialDesignFisher(design)) {
                            roundDigits <- 0
                        }
                    }
                    if (!is.na(roundDigits) && roundDigits == 0) {
                        if (inherits(fieldSet, "Dataset") &&
                                grepl("samplesize|event", tolower(parameterName))) {
                        } else {
                            formatFunctionName <- .getParameterFormatFunction(parameterName, fieldSet)
                        }
                    }
                }

                if (!is.null(formatFunctionName)) {
                    values <- eval(call(formatFunctionName, values))
                } else {
                    values <- .formatSummaryValues(values,
                        digits = roundDigits,
                        smoothedZeroFormat = smoothedZeroFormat,
                        formatRepeatedPValues = formatRepeatedPValues
                    )
                }
            },
            error = function(e) {
                stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "failed to show parameter '", parameterName, "': ", e$message)
            }
        )
    }

    return(format(values))
}

.createSummaryTitleObject <- function(object) {
    design <- NULL
    designPlan <- NULL
    if (inherits(object, "TrialDesignCharacteristics")) {
        design <- object$.design
    } else if (.isTrialDesignPlan(object) || inherits(object, "SimulationResults")) {
        design <- object$.design
        designPlan <- object
    } else if (inherits(object, "AnalysisResults")) {
        return(.createSummaryTitleAnalysisResults(object$.design, object))
    } else if (.isTrialDesign(object)) {
        design <- object
    }
    if (!is.null(design)) {
        return(.createSummaryTitleDesign(design, designPlan))
    }
    return("")
}

.createSummaryTitleAnalysisResults <- function(design, analysisResults) {
    kMax <- design$kMax

    title <- ""
    if (kMax == 1) {
        title <- paste0(title, "Fixed sample analysis results")
    } else {
        title <- paste0(title, "Sequential analysis results with a maximum of ", kMax, " looks")
    }

    if (!is.null(analysisResults)) {
        if (.isMultiArmAnalysisResults(analysisResults)) {
            title <- "Multi-arm analysis results for a "
        } else if (.isEnrichmentAnalysisResults(analysisResults)) {
            title <- "Enrichment analysis results for a "
        } else {
            title <- "Analysis results for a "
        }

        if (grepl("Means", .getClassName(analysisResults$.dataInput))) {
            title <- paste0(title, "continuous endpoint")
        } else if (grepl("Rates", .getClassName(analysisResults$.dataInput))) {
            title <- paste0(title, "binary endpoint")
        } else if (grepl("Survival", .getClassName(analysisResults$.dataInput))) {
            title <- paste0(title, "survival endpoint")
        }

        if (.isMultiHypothesesAnalysisResults(analysisResults)) {
            gMax <- analysisResults$.stageResults$getGMax()
            if (.isMultiArmAnalysisResults(analysisResults)) {
                title <- paste0(title, " (", gMax, " active arms vs. control)")
            } else if (.isEnrichmentAnalysisResults(analysisResults)) {
                title <- paste0(title, " (", gMax, " populations)")
            }
        }
    } else if (kMax > 1) {
        prefix <- ifelse(design$.isDelayedResponseDesign(), "delayed response ", "")
        title <- .concatenateSummaryText(title,
            paste0("(", prefix, design$.toString(startWithUpperCase = FALSE), ")"),
            sep = " "
        )
    }

    return(title)
}

.createSummaryTitleDesign <- function(design, designPlan) {
    kMax <- design$kMax

    title <- ""
    if (kMax == 1) {
        title <- paste0(title, "Fixed sample analysis")
    } else {
        title <- paste0(title, "Sequential analysis with a maximum of ", kMax, " looks")
    }
    if (!is.null(designPlan)) {
        if (inherits(designPlan, "SimulationResults")) {
            title <- "Simulation of a "
        } else if (designPlan$.isSampleSizeObject()) {
            title <- "Sample size calculation for a "
        } else if (designPlan$.isPowerObject()) {
            title <- "Power calculation for a "
        }

        if (grepl("Means", .getClassName(designPlan))) {
            title <- paste0(title, "continuous endpoint")
        } else if (grepl("Rates", .getClassName(designPlan))) {
            title <- paste0(title, "binary endpoint")
        } else if (grepl("Survival", .getClassName(designPlan))) {
            title <- paste0(title, "survival endpoint")
        } else if (grepl("CountData", .getClassName(designPlan))) {
            title <- paste0(title, "count data endpoint")
        }

        if (grepl("MultiArm", .getClassName(designPlan)) &&
                !is.null(designPlan[["activeArms"]]) && designPlan$activeArms > 1) {
            title <- .concatenateSummaryText(title, "(multi-arm design)", sep = " ")
        } else if (grepl("Enrichment", .getClassName(designPlan))) {
            title <- .concatenateSummaryText(title, "(enrichment design)", sep = " ")
        }
    } else if (kMax > 1) {
        prefix <- ifelse(design$.isDelayedResponseDesign(), "delayed response ", "")
        title <- .concatenateSummaryText(title,
            paste0("(", prefix, design$.toString(startWithUpperCase = FALSE), ")"),
            sep = " "
        )
    }

    return(title)
}

.isRatioComparisonEnabled <- function(object) {
    if (!is.null(object[["meanRatio"]]) && isTRUE(object[["meanRatio"]])) {
        return(TRUE)
    }

    if (!is.null(object[["riskRatio"]]) && isTRUE(object[["riskRatio"]])) {
        return(TRUE)
    }

    if (.isTrialDesignPlanCountData(object)) {
        return(TRUE)
    }

    return(FALSE)
}

.getSummaryObjectSettings <- function(object) {
    multiArmEnabled <- grepl("MultiArm", .getClassName(object))
    enrichmentEnabled <- grepl("Enrichment", .getClassName(object))
    simulationEnabled <- grepl("Simulation", .getClassName(object))
    countDataEnabled <- FALSE
    ratioEnabled <- FALSE
    populations <- NA_integer_
    if (inherits(object, "AnalysisResults") || inherits(object, "StageResults")) {
        groups <- object$.dataInput$getNumberOfGroups()
        meansEnabled <- grepl("Means", .getClassName(object$.dataInput))
        ratesEnabled <- grepl("Rates", .getClassName(object$.dataInput))
        survivalEnabled <- grepl("Survival", .getClassName(object$.dataInput))
    } else {
        meansEnabled <- grepl("Means", .getClassName(object))
        ratesEnabled <- grepl("Rates", .getClassName(object))
        survivalEnabled <- grepl("Survival", .getClassName(object))
        countDataEnabled <- grepl("CountData", .getClassName(object))
        if (simulationEnabled && multiArmEnabled) {
            groups <- object$activeArms
        } else if (simulationEnabled && enrichmentEnabled) {
            groups <- 2
            populations <- object$populations
        } else {
            # for analysis multi-arm / enrichment always 2 groups are applicable
            groups <- ifelse(multiArmEnabled || enrichmentEnabled || survivalEnabled, 2, object[["groups"]])
        }
        ratioEnabled <- .isRatioComparisonEnabled(object)
    }

    return(list(
        meansEnabled = meansEnabled,
        ratesEnabled = ratesEnabled,
        survivalEnabled = survivalEnabled,
        countDataEnabled = countDataEnabled,
        groups = groups,
        populations = populations,
        multiArmEnabled = multiArmEnabled,
        enrichmentEnabled = enrichmentEnabled,
        simulationEnabled = simulationEnabled,
        ratioEnabled = ratioEnabled
    ))
}

.createSummaryHypothesisText <- function(object, summaryFactory) {
    if (!inherits(object, "AnalysisResults") && !inherits(object, "TrialDesignPlan") &&
            !inherits(object, "SimulationResults")) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'object' must be an instance of class 'AnalysisResults', 'TrialDesignPlan' ",
            "or 'SimulationResults' (is '", .getClassName(object), "')"
        )
    }

    design <- object[[".design"]]
    if (is.null(design)) {
        stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'.design' must be defined in specified ", .getClassName(object))
    }

    settings <- .getSummaryObjectSettings(object)
    sided <- ifelse(settings$multiArmEnabled || settings$enrichmentEnabled, 1, design$sided)
    directionUpper <- object[["directionUpper"]]
    if (is.null(directionUpper) || length(directionUpper) != 1 || is.na(directionUpper)) {
        directionUpper <- TRUE
    }

    comparisonH0 <- " = "
    comparisonH1 <- NA_character_
    if (inherits(object, "AnalysisResults") && !is.null(directionUpper)) {
        comparisonH1 <- ifelse(sided == 2, " != ", ifelse(directionUpper, " > ", " < "))
    }

    if (!is.null(object[["thetaH0"]])) {
        thetaH0 <- object$thetaH0
    } else {
        thetaH0 <- ifelse(settings$survivalEnabled, 1, 0)
    }

    treatmentArmIndex <- ifelse(settings$groups > 1, "(i)", "(treatment)")
    controlArmIndex <- ifelse(settings$groups > 1, "(i)", "(control)")

    if (settings$multiArmEnabled || settings$enrichmentEnabled) {
        if ((settings$survivalEnabled) && (settings$multiArmEnabled)) {
            treatmentArmIndex <- "(i)"
            controlArmIndex <- ""
        } else if ((settings$survivalEnabled) && (settings$enrichmentEnabled)) {
            treatmentArmIndex <- ""
            controlArmIndex <- ""
        } else if (settings$groups == 1) {
            treatmentArmIndex <- "(treatment)"
            controlArmIndex <- "(control)"
        } else {
            if (settings$enrichmentEnabled) {
                treatmentArmIndex <- "(treatment)"
            } else {
                treatmentArmIndex <- "(i)"
            }
            controlArmIndex <- "(control)"
        }
    } else {
        if (settings$groups == 1 || settings$survivalEnabled) {
            treatmentArmIndex <- ""
            controlArmIndex <- ""
        } else {
            treatmentArmIndex <- "(1)"
            controlArmIndex <- "(2)"
        }
    }

    value <- "?"
    if (settings$meansEnabled) {
        value <- "mu"
    } else if (settings$ratesEnabled) {
        value <- "pi"
    } else if (settings$survivalEnabled) {
        value <- "hazard ratio"
    } else if (settings$countDataEnabled) {
        value <- "lambda"
    }

    calcSep <- ifelse(settings$ratioEnabled, " / ", " - ")
    hypothesis <- ""
    if (!settings$survivalEnabled && (settings$multiArmEnabled || settings$enrichmentEnabled || settings$groups == 2)) {
        hypothesis <- paste0(
            hypothesis, "H0: ", value, treatmentArmIndex,
            calcSep, value, controlArmIndex, comparisonH0, thetaH0
        )
        if (!is.na(comparisonH1)) {
            hypothesis <- paste0(hypothesis, " against ")
            hypothesis <- paste0(
                hypothesis, "H1: ", value, treatmentArmIndex,
                calcSep, value, controlArmIndex, comparisonH1, thetaH0
            )
        }
    } else {
        hypothesis <- paste0(hypothesis, "H0: ", value, treatmentArmIndex, comparisonH0, thetaH0)
        if (!is.na(comparisonH1)) {
            hypothesis <- paste0(hypothesis, " against ")
            hypothesis <- paste0(hypothesis, "H1: ", value, treatmentArmIndex, comparisonH1, thetaH0)
        }
    }
    hypothesis <- .concatenateSummaryText(
        hypothesis,
        .createSummaryHypothesisPowerDirectionText(object, sided, directionUpper)
    )
    return(hypothesis)
}

.createSummaryHypothesisPowerDirectionText <- function(object, sided, directionUpper) {
    if (sided == 2 || is.null(directionUpper)) {
        return("")
    }

    directionUpper <- unique(directionUpper)
    if (length(directionUpper) != 1) {
        return("")
    }

    if (inherits(object, "AnalysisResults")) {
        return("")
    }

    if (.isTrialDesignPlan(object) && object$.objectType != "power") {
        return("")
    }

    if (directionUpper) {
        return("power directed towards larger values")
    } else {
        return("power directed towards smaller values")
    }
}

.addSummaryLineBreak <- function(text, newLineLength) {
    maxLineLength <- as.integer(getOption("rpact.summary.width", 83))
    lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
    lastLine <- lines[length(lines)]
    if (nchar(lastLine) + newLineLength > maxLineLength) {
        text <- paste0(text, "\n")
    }
    return(text)
}

.concatenateSummaryText <- function(a, b, sep = ", ") {
    .assertIsSingleCharacter(a, "a")
    .assertIsSingleCharacter(b, "b")
    if (is.na(b) || nchar(trimws(b)) == 0) {
        return(a)
    }

    if (a == "") {
        return(b)
    }

    a <- paste0(a, sep)
    a <- .addSummaryLineBreak(a, nchar(b))
    return(paste0(a, b))
}

.createSummaryHeaderObject <- function(object, summaryFactory, digits = NA_integer_) {
    if (inherits(object, "TrialDesignCharacteristics")) {
        return(.createSummaryHeaderDesign(object$.design, NULL, summaryFactory))
    }

    if (.isTrialDesignPlan(object) || inherits(object, "SimulationResults")) {
        return(.createSummaryHeaderDesign(object$.design, object, summaryFactory))
    }

    if (inherits(object, "AnalysisResults")) {
        return(.createSummaryHeaderAnalysisResults(object$.design, object, summaryFactory, digits))
    }

    if (.isTrialDesign(object)) {
        return(.createSummaryHeaderDesign(object, NULL, summaryFactory))
    }

    return("")
}

.addAllocationRatioToHeader <- function(parameterSet, header, sep = ", ") {
    if (!.isTrialDesignPlanSurvival(parameterSet) && !grepl("Simulation", .getClassName(parameterSet))) {
        numberOfGroups <- 1
        if (inherits(parameterSet, "TrialDesignPlan")) {
            numberOfGroups <- parameterSet$groups
        } else if (inherits(parameterSet, "AnalysisResults")) {
            numberOfGroups <- parameterSet$.dataInput$getNumberOfGroups()
        }
        if (numberOfGroups == 1) {
            return(header)
        }
    }

    prefix <- ""
    if (!is.null(parameterSet[["optimumAllocationRatio"]]) &&
            length(parameterSet$optimumAllocationRatio) == 1 &&
            parameterSet$optimumAllocationRatio) {
        if (length(unique(parameterSet$allocationRatioPlanned)) > 1) {
            return(.concatenateSummaryText(header, "optimum planned allocation ratio", sep = sep))
        }
        prefix <- "optimum "
    }

    allocationRatioPlanned <- round(unique(parameterSet$allocationRatioPlanned), 3)
    if (identical(allocationRatioPlanned, 1) && prefix == "") {
        return(header)
    }

    if (!all(is.na(allocationRatioPlanned))) {
        return(.concatenateSummaryText(header,
            paste0(
                prefix, "planned allocation ratio = ",
                .arrayToString(allocationRatioPlanned, vectorLookAndFeelEnabled = length(allocationRatioPlanned) > 1)
            ),
            sep = sep
        ))
    } else {
        return(header)
    }
}

.createSummaryHeaderAnalysisResults <- function(design, analysisResults, summaryFactory, digits) {
    digitSettings <- .getSummaryDigits(digits)
    digitsGeneral <- digitSettings$digitsGeneral

    stageResults <- analysisResults$.stageResults
    dataInput <- analysisResults$.dataInput

    multiArmEnabled <- .isMultiArmAnalysisResults(analysisResults)
    enrichmentEnabled <- .isEnrichmentAnalysisResults(analysisResults)
    multiHypothesesEnabled <- .isMultiHypothesesAnalysisResults(analysisResults)

    header <- ""
    if (design$kMax == 1) {
        header <- paste0(header, "Fixed sample analysis.")
    } else {
        header <- paste0(header, "Sequential analysis with ", design$kMax, " looks")
        header <- .concatenateSummaryText(header,
            paste0("(", design$.toString(startWithUpperCase = FALSE), ")."),
            sep = " "
        )
    }
    header <- paste0(header, "\n")

    header <- paste0(header, "The results were calculated using a ")
    if (stageResults$isDatasetMeans()) {
        if (dataInput$getNumberOfGroups() == 1) {
            header <- paste0(header, "one-sample t-test")
        } else if (dataInput$getNumberOfGroups() == 2) {
            header <- paste0(header, "two-sample t-test")
        } else {
            header <- paste0(header, "multi-arm t-test")
        }
    } else if (stageResults$isDatasetRates()) {
        if (dataInput$getNumberOfGroups() == 1) {
            header <- paste0(header, "one-sample test for rates")
        } else if (dataInput$getNumberOfGroups() == 2) {
            header <- paste0(header, "two-sample test for rates")
        } else {
            header <- paste0(header, "multi-arm test for rates")
        }
    } else if (stageResults$isDatasetSurvival()) {
        if (dataInput$getNumberOfGroups() == 2) {
            header <- paste0(header, "two-sample logrank test")
        } else {
            header <- paste0(header, "multi-arm logrank test")
        }
    }

    header <- .concatenateSummaryText(header,
        paste0("(", ifelse(design$sided == 1, "one", "two"), "-sided, alpha = ", round(design$alpha, 4), ")"),
        sep = " "
    )

    if (!.isTrialDesignConditionalDunnett(design) && multiHypothesesEnabled) {
        if (stageResults$intersectionTest == "Dunnett") {
            header <- .concatenateSummaryText(header, "Dunnett intersection test")
        } else if (stageResults$intersectionTest == "Bonferroni") {
            header <- .concatenateSummaryText(header, "Bonferroni intersection test")
        } else if (stageResults$intersectionTest == "Simes") {
            header <- .concatenateSummaryText(header, "Simes intersection test")
        } else if (stageResults$intersectionTest == "Sidak") {
            header <- .concatenateSummaryText(header, "Sidak intersection test")
        } else if (stageResults$intersectionTest == "Hierarchical") {
            header <- .concatenateSummaryText(header, "Hierarchical intersection test")
        } else if (stageResults$intersectionTest == "SpiessensDebois") {
            header <- .concatenateSummaryText(header, "Spiessens and Debois intersection test")
        }
    }

    if (!is.null(stageResults[["normalApproximation"]]) && stageResults$normalApproximation) {
        header <- .concatenateSummaryText(header, "normal approximation test")
    } else if (stageResults$isDatasetRates()) {
        if (dataInput$getNumberOfGroups() == 1) {
            header <- .concatenateSummaryText(header, "exact test")
        } else {
            header <- .concatenateSummaryText(header, "exact test of Fisher")
        }
    } else {
        # header <- .concatenateSummaryText(header, "exact t test")
    }

    if (stageResults$isDatasetMeans() && multiHypothesesEnabled) {
        if (stageResults$varianceOption == "overallPooled") {
            header <- .concatenateSummaryText(header, "overall pooled variances option")
        } else if (stageResults$varianceOption == "pairwisePooled") {
            header <- .concatenateSummaryText(header, "pairwise pooled variances option")
        } else if (stageResults$varianceOption == "pooledFromFull") {
            header <- .concatenateSummaryText(header, "pooled from full population variances option")
        } else if (stageResults$varianceOption == "pooled") {
            header <- .concatenateSummaryText(header, "pooled variances option")
        } else if (stageResults$varianceOption == "notPooled") {
            header <- .concatenateSummaryText(header, "not pooled variances option")
        }
    }

    if (inherits(stageResults, "StageResultsMeans") && (dataInput$getNumberOfGroups() == 2)) {
        if (stageResults$equalVariances) {
            header <- .concatenateSummaryText(header, "equal variances option")
        } else {
            header <- .concatenateSummaryText(header, "unequal variances option")
        }
    }

    if (.isTrialDesignConditionalDunnett(design)) {
        if (design$secondStageConditioning) {
            header <- .concatenateSummaryText(header, "conditional second stage p-values")
        } else {
            header <- .concatenateSummaryText(header, "unconditional second stage p-values")
        }
    }

    if (enrichmentEnabled) {
        header <- .concatenateSummaryText(header, paste0(
            ifelse(analysisResults$stratifiedAnalysis, "", "non-"), "stratified analysis"
        ))
    }

    header <- paste0(header, ".\n", .createSummaryHypothesisText(analysisResults, summaryFactory))

    if (stageResults$isDatasetMeans()) {
        header <- .getSummaryHeaderEntryAnalysisResults(header, analysisResults,
            paramName1 = "thetaH1",
            paramName2 = ifelse(multiHypothesesEnabled, "assumedStDevs", "assumedStDev"),
            paramCaption1 = "assumed effect",
            paramCaption2 = "assumed standard deviation",
            shortcut1 = "thetaH1",
            shortcut2 = "sd",
            digits1 = digitsGeneral,
            digits2 = digitsGeneral
        )
    } else if (stageResults$isDatasetRates()) {
        header <- .getSummaryHeaderEntryAnalysisResults(header, analysisResults,
            paramName1 = ifelse(enrichmentEnabled, "piTreatments", ifelse(multiArmEnabled, "piTreatments", "pi1")),
            paramName2 = ifelse(enrichmentEnabled, "piControls", ifelse(multiArmEnabled, "piControl", "pi2")),
            paramCaption1 = "assumed treatment rate",
            paramCaption2 = "assumed control rate",
            shortcut1 = "pi",
            shortcut2 = "pi"
        )
    } else if (stageResults$isDatasetSurvival()) {
        header <- .getSummaryHeaderEntryAnalysisResults(header, analysisResults,
            paramName1 = "thetaH1",
            paramCaption1 = "assumed effect",
            shortcut1 = "thetaH1",
            digits1 = digitsGeneral
        )
    }

    header <- paste0(header, ".")
    return(header)
}

.getSummaryHeaderEntryValueAnalysisResults <- function(shortcut, value, analysisResults) {
    if (is.matrix(value)) {
        stage <- analysisResults$.stageResults$stage
        if (stage <= ncol(value)) {
            value <- value[, stage]
        }
    }

    value[!is.na(value)] <- round(value[!is.na(value)], 2)

    if ((is.matrix(value) && nrow(value) > 1) || length(value) > 1) {
        treatmentNames <- 1:length(value)
        if (.isEnrichmentAnalysisResults(analysisResults)) {
            populations <- paste0("S", treatmentNames)
            gMax <- analysisResults$.stageResults$getGMax()
            populations[treatmentNames == gMax] <- "F"
            treatmentNames <- populations
        }
        value <- paste0(paste(paste0(shortcut, "(", treatmentNames, ") = ", value)), collapse = ", ")
    }
    return(value)
}

.getSummaryHeaderEntryAnalysisResults <- function(header, analysisResults, ...,
        paramName1, paramName2 = NA_character_,
        paramCaption1, paramCaption2 = NA_character_,
        shortcut1, shortcut2 = NA_character_,
        digits1 = 2, digits2 = 2) {
    if (analysisResults$.design$kMax == 1) {
        return(header)
    }

    if (length(analysisResults$nPlanned) == 0 || all(is.na(analysisResults$nPlanned))) {
        return(header)
    }

    paramValue1 <- analysisResults[[paramName1]]
    case1 <- analysisResults$.getParameterType(paramName1) != C_PARAM_NOT_APPLICABLE &&
        !all(is.na(paramValue1))
    if (!is.na(paramCaption1) && analysisResults$.getParameterType(paramName1) == C_PARAM_GENERATED) {
        paramCaption1 <- sub("assumed ", "overall ", paramCaption1)
    }

    case2 <- FALSE
    if (!is.na(paramName2)) {
        paramValue2 <- analysisResults[[paramName2]]
        case2 <- analysisResults$.getParameterType(paramName2) != C_PARAM_NOT_APPLICABLE &&
            !all(is.na(paramValue2))
        if (!is.na(paramCaption2) && analysisResults$.getParameterType(paramName2) == C_PARAM_GENERATED) {
            paramCaption2 <- sub("assumed ", "overall ", paramCaption2)
        }
    }

    if (!case1 && !case2) {
        return(header)
    }

    if (.isTrialDesignFisher(analysisResults$.design) &&
            length(analysisResults$nPlanned[!is.na(analysisResults$nPlanned)]) > 1) {
        header <- .concatenateSummaryText(header, paste0(
            "The conditional power simulation with planned sample size and ",
            analysisResults$iterations, " iterations is based on"
        ), sep = ". ")
    } else {
        header <- .concatenateSummaryText(header,
            "The conditional power calculation with planned sample size is based on",
            sep = ". "
        )
    }

    header <- .addAllocationRatioToHeader(analysisResults, header, sep = " ")

    sepPrefix <- ifelse(length(analysisResults$allocationRatioPlanned) == 0 ||
        identical(unique(analysisResults$allocationRatioPlanned), 1), "", ",")

    if (case1) {
        if (!any(is.na(paramValue1)) && length(unique(paramValue1)) == 1) {
            paramValue1 <- paramValue1[1]
        }
        if (length(paramValue1) == 1) {
            header <- .concatenateSummaryText(header,
                paste0(paramCaption1, " = ", ifelse(is.na(paramValue1), paramValue1, round(paramValue1, digits1))),
                sep = paste0(sepPrefix, " ")
            )
        } else {
            header <- .concatenateSummaryText(header,
                paste0(paramCaption1, ": ", .getSummaryHeaderEntryValueAnalysisResults(
                    shortcut1, paramValue1, analysisResults
                )),
                sep = paste0(sepPrefix, " ")
            )
        }
    }

    if (case2) {
        if (length(paramValue2) == 1) {
            header <- .concatenateSummaryText(header,
                paste0(paramCaption2, " = ", ifelse(is.na(paramValue2), paramValue2, round(paramValue2, digits2))),
                sep = ifelse(case1, paste0(sepPrefix, " and "), " ")
            )
        } else {
            header <- .concatenateSummaryText(header,
                paste0(paramCaption2, ": ", .getSummaryHeaderEntryValueAnalysisResults(
                    shortcut2, paramValue2, analysisResults
                )),
                sep = ifelse(case1, paste0(sepPrefix, " and "), " ")
            )
        }
    }
    return(header)
}

.addEnrichmentEffectListToHeader <- function(header, designPlan) {
    if (!grepl("SimulationResultsEnrichment", .getClassName(designPlan)) ||
            is.null(designPlan[["effectList"]])) {
        return(header)
    }


    subGroups <- designPlan$effectList$subGroups
    header <- .concatenateSummaryText(header, paste0(
        "subgroup",
        ifelse(length(subGroups) != 1, "s", ""),
        " = ",
        .arrayToString(subGroups, vectorLookAndFeelEnabled = TRUE)
    ))

    prevalences <- designPlan$effectList$prevalences
    header <- .concatenateSummaryText(header, paste0(
        "prevalence",
        ifelse(length(prevalences) != 1, "s", ""),
        " = ",
        .arrayToString(round(prevalences, 3), vectorLookAndFeelEnabled = TRUE)
    ))

    if (!is.null(designPlan$effectList[["piControls"]])) {
        piControls <- designPlan$effectList$piControls
        if (length(piControls) > 0) {
            if (length(unique(piControls)) == 1) {
                piControls <- piControls[1]
            }
            controlRateText <- paste0(
                "control rate", ifelse(length(piControls) == 1, "", "s"), " pi(control) = ",
                .arrayToString(round(piControls, 3), vectorLookAndFeelEnabled = (length(unique(piControls)) > 1))
            )
            header <- .concatenateSummaryText(header, controlRateText)
        }
    }

    return(header)
}

.createSummaryHeaderDesign <- function(design, designPlan, summaryFactory) {
    if (is.null(designPlan)) {
        if (.isTrialDesignFisher(design)) {
            designType <- "Fisher's combination test"
        } else if (.isTrialDesignConditionalDunnett(design)) {
            designType <- "Conditional Dunnett test"
        } else {
            designType <- C_TYPE_OF_DESIGN_LIST[[design$typeOfDesign]]
        }
        header <- .firstCharacterToUpperCase(designType)
        header <- paste0(header, " design")
        if (design$.isDelayedResponseDesign()) {
            header <- paste0(header, " with delayed response")
        }
        if (design$kMax > 1 && .isTrialDesignInverseNormalOrGroupSequential(design)) {
            if (design$typeOfDesign == C_TYPE_OF_DESIGN_WT) {
                header <- .concatenateSummaryText(header,
                    paste0("(deltaWT = ", round(design$deltaWT, 3), ")"),
                    sep = " "
                )
            } else if (design$typeOfDesign == C_TYPE_OF_DESIGN_WT_OPTIMUM) {
                header <- .concatenateSummaryText(header,
                    paste0("(", design$optimizationCriterion, ", deltaWT = ", round(design$deltaWT, 3), ")"),
                    sep = " "
                )
            } else if (design$typeOfDesign == C_TYPE_OF_DESIGN_PT) {
                header <- .concatenateSummaryText(header,
                    paste0("(deltaPT1 = ", round(design$deltaPT1, 3), ""),
                    sep = " "
                )
                header <- .concatenateSummaryText(header,
                    paste0("deltaPT0 = ", round(design$deltaPT0, 3), ")"),
                    sep = ", "
                )
            } else if (design$typeOfDesign == C_TYPE_OF_DESIGN_HP) {
                header <- .concatenateSummaryText(header,
                    paste0("(constant bounds = ", round(design$constantBoundsHP, 3), ")"),
                    sep = " "
                )
            } else if (design$typeOfDesign %in% c(C_TYPE_OF_DESIGN_AS_KD, C_TYPE_OF_DESIGN_AS_HSD)) {
                header <- .concatenateSummaryText(header,
                    paste0("(gammaA = ", round(design$gammaA, 3), ")"),
                    sep = " "
                )
            } else if (design$typeOfDesign == C_TYPE_OF_DESIGN_AS_USER) {
                header <- .concatenateSummaryText(header,
                    paste0("(", .arrayToString(round(design$userAlphaSpending, 3)), ")"),
                    sep = " "
                )
            }

            if (grepl("^as", design$typeOfDesign) && design$typeBetaSpending != C_TYPE_OF_DESIGN_BS_NONE) {
                typeBetaSpending <- C_TYPE_OF_DESIGN_BS_LIST[[design$typeBetaSpending]]
                header <- .concatenateSummaryText(header, typeBetaSpending, sep = " and ")
                if (design$typeBetaSpending %in% c(C_TYPE_OF_DESIGN_BS_KD, C_TYPE_OF_DESIGN_BS_HSD)) {
                    header <- .concatenateSummaryText(header,
                        paste0("(gammaB = ", round(design$gammaB, 3), ")"),
                        sep = " "
                    )
                } else if (design$typeBetaSpending == C_TYPE_OF_DESIGN_BS_USER) {
                    header <- .concatenateSummaryText(header,
                        paste0("(", .arrayToString(round(design$userBetaSpending, 3)), ")"),
                        sep = " "
                    )
                }
            }
        }
        if (!.isDelayedInformationEnabled(design = design) &&
                ((.isTrialDesignInverseNormalOrGroupSequential(design) && any(design$futilityBounds > -6, na.rm = TRUE)) ||
                    (.isTrialDesignFisher(design) && any(design$alpha0Vec < 1)))) {
            header <- .concatenateSummaryText(
                header,
                paste0(ifelse(design$bindingFutility, "binding", "non-binding"), " futility")
            )
        }
        header <- .concatenateSummaryText(header, paste0(
            ifelse(design$sided == 1, "one-sided", "two-sided"),
            ifelse(design$kMax == 1, "", " overall")
        ))
        header <- .concatenateSummaryText(header,
            paste0("significance level ", round(100 * design$alpha, 2), "%"),
            sep = " "
        )
        if (.isTrialDesignInverseNormalOrGroupSequential(design)) {
            header <- .concatenateSummaryText(header, paste0("power ", round(100 * (1 - design$beta), 1), "%"))
        }
        header <- .concatenateSummaryText(header, "undefined endpoint")

        if (design$kMax > 1 && .isTrialDesignInverseNormalOrGroupSequential(design)) {
            outputSize <- getOption("rpact.summary.output.size", C_SUMMARY_OUTPUT_SIZE_DEFAULT)
            designCharacteristics <- NULL
            tryCatch(
                {
                    designCharacteristics <- getDesignCharacteristics(design)
                },
                error = function(e) {
                    .logError("Cannot add design characteristics to summary: ", e$message)
                }
            )
            if (!is.null(designCharacteristics)) {
                header <- .concatenateSummaryText(
                    header,
                    paste0("inflation factor ", round(designCharacteristics$inflationFactor, 4))
                )
                if (outputSize == "large") {
                    header <- .concatenateSummaryText(
                        header,
                        paste0("ASN H1 ", round(designCharacteristics$averageSampleNumber1, 4))
                    )
                    header <- .concatenateSummaryText(
                        header,
                        paste0("ASN H01 ", round(designCharacteristics$averageSampleNumber01, 4))
                    )
                    header <- .concatenateSummaryText(
                        header,
                        paste0("ASN H0 ", round(designCharacteristics$averageSampleNumber0, 4))
                    )
                }
            }
        }

        header <- paste0(header, ".")
        return(header)
    }

    settings <- .getSummaryObjectSettings(designPlan)

    header <- ""
    if (design$kMax == 1) {
        header <- paste0(header, "Fixed sample analysis,")
    } else {
        header <- paste0(header, "Sequential analysis with a maximum of ", design$kMax, " looks")
        prefix <- ifelse(design$.isDelayedResponseDesign(), "delayed response ", "")
        header <- .concatenateSummaryText(header,
            paste0("(", prefix, design$.toString(startWithUpperCase = FALSE), ")"),
            sep = " "
        )
    }
    header <- .concatenateSummaryText(header, ifelse(design$kMax == 1, "", "overall"))
    header <- .concatenateSummaryText(header,
        paste0("significance level ", round(100 * design$alpha, 2), "%"),
        sep = " "
    )
    header <- .concatenateSummaryText(header, ifelse(design$sided == 1, "(one-sided).", "(two-sided)."), sep = " ")

    header <- paste0(header, "\n")

    header <- paste0(header, "The results were ")
    header <- paste0(header, ifelse(inherits(designPlan, "SimulationResults"), "simulated", "calculated"))
    header <- paste0(header, " for a ")
    if (settings$meansEnabled) {
        if (settings$multiArmEnabled && settings$groups > 1) {
            header <- .concatenateSummaryText(header, "multi-arm comparisons for means", sep = "")
        } else if (settings$enrichmentEnabled && settings$populations > 1) {
            header <- .concatenateSummaryText(header, "population enrichment comparisons for means", sep = "")
        } else if (settings$groups == 1 && !settings$multiArmEnabled) {
            header <- .concatenateSummaryText(header, "one-sample t-test", sep = "")
        } else if (settings$groups == 2 || settings$multiArmEnabled) {
            header <- .concatenateSummaryText(header, "two-sample t-test", sep = "")
        }
    } else if (settings$ratesEnabled) {
        if (settings$multiArmEnabled && settings$groups > 1) {
            header <- .concatenateSummaryText(header, "multi-arm comparisons for rates", sep = "")
        } else if (settings$enrichmentEnabled && settings$populations > 1) {
            header <- .concatenateSummaryText(header, "population enrichment comparisons for rates", sep = "")
        } else if (settings$groups == 1 && !settings$multiArmEnabled) {
            header <- .concatenateSummaryText(header, "one-sample test for rates", sep = "")
        } else if (settings$groups == 2 || settings$multiArmEnabled) {
            header <- .concatenateSummaryText(header, "two-sample test for rates", sep = "")
        }
    } else if (settings$survivalEnabled) {
        if (settings$multiArmEnabled && settings$groups > 1) {
            header <- .concatenateSummaryText(header, "multi-arm logrank test", sep = "")
        } else if (settings$enrichmentEnabled && settings$populations > 1) {
            header <- .concatenateSummaryText(header, "population enrichment logrank test", sep = "")
        } else if (settings$groups == 2 || settings$multiArmEnabled) {
            header <- .concatenateSummaryText(header, "two-sample logrank test", sep = "")
        }
    } else if (settings$countDataEnabled) {
        header <- .concatenateSummaryText(header, "two-sample test for count data", sep = "")
    }

    part <- ""
    if (settings$multiArmEnabled && settings$groups > 1) {
        part <- .concatenateSummaryText(part, paste0(settings$groups, " treatments vs. control"))
    } else if (settings$enrichmentEnabled) {
        if (settings$groups == 2) {
            part <- .concatenateSummaryText(part, "treatment vs. control")
        } else if (settings$groups > 2) {
            part <- .concatenateSummaryText(part, paste0(settings$groups, " treatments vs. control"))
        }
        part <- .concatenateSummaryText(part, paste0(
            settings$populations, " population",
            ifelse(settings$populations == 1, "", "s")
        ))
    }
    if (!is.null(designPlan) && (.isTrialDesignPlan(designPlan) || inherits(designPlan, "SimulationResults")) &&
            !settings$multiArmEnabled && !settings$enrichmentEnabled && !settings$survivalEnabled) {
        if (settings$ratesEnabled) {
            if (settings$groups == 1) {
                part <- .concatenateSummaryText(part, ifelse(designPlan$normalApproximation,
                    "normal approximation", "exact test"
                ))
            } else {
                part <- .concatenateSummaryText(part, ifelse(designPlan$normalApproximation,
                    "normal approximation", "exact test of Fisher"
                ))
            }
        } else if (!is.null(designPlan[["normalApproximation"]]) && designPlan$normalApproximation) {
            part <- .concatenateSummaryText(part, "normal approximation")
        }
    }
    if (part != "") {
        header <- .concatenateSummaryText(header, paste0("(", part, ")"), sep = " ")
    }
    if (settings$countDataEnabled && (.isTrialDesignInverseNormalOrGroupSequential(design) ||
            inherits(designPlan, "SimulationResults"))) {
        header <- .concatenateSummaryText(header, .createSummaryHypothesisText(designPlan, summaryFactory))
        if (!is.null(designPlan[["theta"]]) && length(designPlan$theta) == 1) {
            effectText <- paste0("H1: effect = ", round(designPlan$theta, 3))
        } else {
            effectText <- "H1: effect as specified"
        }
        header <- .concatenateSummaryText(header, effectText)
        header <- .addAdditionalArgumentsToHeader(header, designPlan, settings)
    } else if (settings$meansEnabled && (.isTrialDesignInverseNormalOrGroupSequential(design) ||
            inherits(designPlan, "SimulationResults"))) {
        header <- .concatenateSummaryText(header, .createSummaryHypothesisText(designPlan, summaryFactory))
        if (!is.null(designPlan[["alternative"]]) && length(designPlan$alternative) == 1) {
            alternativeText <- paste0("H1: effect = ", round(designPlan$alternative, 3))
        } else if (!is.null(designPlan[["muMaxVector"]]) && length(designPlan$muMaxVector) == 1) {
            alternativeText <- paste0("H1: mu_max = ", round(designPlan$muMaxVector, 3))
        } else if (!is.null(designPlan[["effectList"]]) && !is.null(designPlan$effectList[["effects"]]) &&
                isTRUE(nrow(designPlan$effectList$effects) == 1)) {
            alternativeText <- paste0(
                "H1: effects = ",
                .arrayToString(designPlan$effectList$effects, mode = "vector")
            )
        } else {
            alternativeText <- "H1: effect as specified"
        }
        header <- .concatenateSummaryText(header, alternativeText)

        header <- .addEnrichmentEffectListToHeader(header, designPlan)

        if (grepl("SimulationResultsEnrichment", .getClassName(designPlan))) {
            stDevs <- designPlan$effectList$stDevs
            if (length(unique(stDevs)) == 1) {
                stDevs <- unique(stDevs)
            }
            s <- ifelse(length(stDevs) != 1, "s", "")
            stDevCaption <- ifelse(.isRatioComparisonEnabled(designPlan),
                paste0("coefficient", s, " of variation"),
                paste0("standard deviation", s)
            )
            header <- .concatenateSummaryText(header, paste0(
                stDevCaption, " = ",
                .arrayToString(round(stDevs, 3), vectorLookAndFeelEnabled = TRUE)
            ))
        } else {
            stDevCaption <- ifelse(.isRatioComparisonEnabled(designPlan), "coefficient of variation", "standard deviation")
            header <- .concatenateSummaryText(header, paste0(stDevCaption, " = ", round(designPlan$stDev, 3)))
        }
        header <- .addAdditionalArgumentsToHeader(header, designPlan, settings)
    } else if (settings$ratesEnabled && (.isTrialDesignInverseNormalOrGroupSequential(design) ||
            inherits(designPlan, "SimulationResults"))) {
        if (settings$groups == 1) {
            if (!is.null(designPlan[["pi1"]]) && length(designPlan$pi1) == 1) {
                treatmentRateText <- paste0("H1: treatment rate pi = ", round(designPlan$pi1, 3))
            } else {
                treatmentRateText <- "H1: treatment rate pi as specified"
            }

            header <- paste0(header, ",\n", .createSummaryHypothesisText(designPlan, summaryFactory))
            header <- .concatenateSummaryText(header, treatmentRateText)
            header <- .addAdditionalArgumentsToHeader(header, designPlan, settings)
        } else {
            if (!is.null(designPlan[["pi1"]]) && length(designPlan$pi1) == 1) {
                treatmentRateText <- paste0("H1: treatment rate pi(1) = ", round(designPlan$pi1, 3))
            } else if (!is.null(designPlan[["piMaxVector"]]) && length(designPlan$piMaxVector) == 1) {
                treatmentRateText <- paste0(
                    "H1: treatment rate pi_max = ",
                    .arrayToString(round(designPlan$piMaxVector, 3), vectorLookAndFeelEnabled = TRUE)
                )
            } else if (settings$enrichmentEnabled && !is.null(designPlan[["effectList"]]) &&
                    !is.null(designPlan$effectList[["piTreatments"]])) {
                piTreatments <- designPlan$effectList[["piTreatments"]]
                if (is.matrix(piTreatments) && nrow(piTreatments) == 1) {
                    treatmentRateText <- paste0(
                        "H1: assumed treatment rate pi(treatment) = ",
                        .arrayToString(round(designPlan$effectList$piTreatments, 3), vectorLookAndFeelEnabled = TRUE)
                    )
                } else {
                    treatmentRateText <- paste0("H1: assumed treatment rate pi(treatment) as specified")
                }
            } else {
                treatmentRateText <- paste0(
                    "H1: treatment rate pi",
                    ifelse(settings$multiArmEnabled, "_max", "(1)"), " as specified"
                )
            }

            controlRateText <- NA_character_
            if (settings$multiArmEnabled && !is.null(designPlan[["piControl"]])) {
                controlRateText <- paste0("control rate pi(control) = ", round(designPlan$piControl, 3))
            } else if (settings$enrichmentEnabled && !is.null(designPlan[["piControls"]])) {
                controlRateText <- paste0(
                    "control rates pi(control) = ",
                    .arrayToString(round(designPlan$piControls, 3), vectorLookAndFeelEnabled = TRUE)
                )
            } else if (settings$enrichmentEnabled && !is.null(designPlan[["effectList"]]) &&
                    !is.null(designPlan$effectList[["piControls"]])) {
                # controlRateText will be created in .addEnrichmentEffectListToHeader()
            } else if (!is.null(designPlan[["pi2"]])) {
                controlRateText <- paste0("control rate pi(2) = ", round(designPlan$pi2, 3))
            } else {
                stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "failed to identify case to build ", sQuote("controlRateText"))
            }
            header <- paste0(header, ",\n", .createSummaryHypothesisText(designPlan, summaryFactory))
            header <- .concatenateSummaryText(header, treatmentRateText)
            if (!is.na(controlRateText)) {
                header <- .concatenateSummaryText(header, controlRateText)
            }
            header <- .addEnrichmentEffectListToHeader(header, designPlan)
            header <- .addAdditionalArgumentsToHeader(header, designPlan, settings)
        }
    } else if (settings$survivalEnabled && (.isTrialDesignInverseNormalOrGroupSequential(design) ||
            inherits(designPlan, "SimulationResults"))) {
        parameterNames <- designPlan$.getVisibleFieldNamesOrdered()
        numberOfVariants <- .getMultidimensionalNumberOfVariants(designPlan, parameterNames)

        if (grepl("SimulationResultsEnrichment", .getClassName(designPlan))) {
            userDefinedParam <- "hazardRatios"
            paramName <- "hazard ratios"
            paramValue <- designPlan$effectList$hazardRatios
        } else {
            userDefinedParam <- "pi1"
            for (param in c("pi1", "lambda1", "median1", "hazardRatio")) {
                if (designPlan$.getParameterType(param) == C_PARAM_USER_DEFINED &&
                        length(designPlan[[param]]) == numberOfVariants) {
                    userDefinedParam <- param
                }
            }
            paramValue <- designPlan[[userDefinedParam]]

            if (is.null(paramValue) || length(paramValue) == 0 || all(is.na(paramValue))) {
                userDefinedParam <- "hazardRatio"
            }
            paramName <- "treatment pi(1)"
            if (userDefinedParam == "lambda1") {
                paramName <- "treatment lambda(1)"
            } else if (userDefinedParam == "median1") {
                paramName <- "treatment median(1)"
            } else if (userDefinedParam == "hazardRatio") {
                paramName <- ifelse(grepl("SimulationResultsMultiArm", .getClassName(designPlan)), "omega_max", "hazard ratio")
            }
        }

        if (length(designPlan[[userDefinedParam]]) == 1) {
            treatmentRateText <- paste0("H1: ", paramName, " = ", round(designPlan[[userDefinedParam]], 3))
        } else if (!is.null(designPlan[["omegaMaxVector"]]) && length(designPlan$omegaMaxVector) == 1) {
            treatmentRateText <- paste0("H1: omega_max = ", round(designPlan$omegaMaxVector, 3))
        } else if (!is.null(designPlan[["hazardRatio"]]) && (length(designPlan$hazardRatio) == 1) ||
                (inherits(designPlan, "SimulationResults") && !is.null(designPlan[[".piecewiseSurvivalTime"]]) &&
                    designPlan$.piecewiseSurvivalTime$piecewiseSurvivalEnabled)) {
            treatmentRateText <- paste0(
                "H1: hazard ratio = ",
                .arrayToString(round(designPlan$hazardRatio, 3), vectorLookAndFeelEnabled = TRUE)
            )
        } else if (settings$enrichmentEnabled && !is.null(designPlan[["effectList"]]) &&
                !is.null(designPlan$effectList[["hazardRatios"]]) &&
                is.matrix(designPlan$effectList$hazardRatios) &&
                nrow(designPlan$effectList$hazardRatios) == 1) {
            treatmentRateText <- paste0(
                "H1: hazard ratios = ",
                .arrayToString(round(designPlan$effectList$hazardRatios, 3), vectorLookAndFeelEnabled = TRUE)
            )
        } else {
            treatmentRateText <- paste0("H1: ", paramName, " as specified")
        }
        if (userDefinedParam %in% c("hazardRatio", "pi1") &&
                (designPlan$.getParameterType("pi2") == C_PARAM_USER_DEFINED ||
                    designPlan$.getParameterType("pi2") == C_PARAM_DEFAULT_VALUE) &&
                length(designPlan$pi2) == 1) {
            treatmentRateText <- paste0(treatmentRateText, ", control pi(2) = ", round(designPlan$pi2, 3))
        } else if (userDefinedParam %in% c("hazardRatio", "lambda1") &&
                (designPlan$.getParameterType("lambda2") == C_PARAM_USER_DEFINED ||
                    designPlan$.getParameterType("lambda2") == C_PARAM_DEFAULT_VALUE) &&
                length(designPlan$lambda2) == 1) {
            treatmentRateText <- paste0(treatmentRateText, ", control lambda(2) = ", round(designPlan$lambda2, 3))
        } else if (userDefinedParam %in% c("hazardRatio", "median1") &&
                (designPlan$.getParameterType("median2") == C_PARAM_USER_DEFINED ||
                    designPlan$.getParameterType("median2") == C_PARAM_GENERATED) &&
                length(designPlan$median2) == 1) {
            treatmentRateText <- paste0(treatmentRateText, ", control median(2) = ", round(designPlan$median2, 3))
        } else if (!is.null(designPlan[[".piecewiseSurvivalTime"]]) &&
                designPlan$.piecewiseSurvivalTime$piecewiseSurvivalEnabled) {
            treatmentRateText <- paste0(treatmentRateText, ", piecewise survival distribution")
            treatmentRateText <- paste0(
                treatmentRateText, ", \n",
                "piecewise survival time = ", .arrayToString(round(designPlan$piecewiseSurvivalTime, 4), vectorLookAndFeelEnabled = TRUE), ", \n",
                "control lambda(2) = ", .arrayToString(round(designPlan$lambda2, 4), vectorLookAndFeelEnabled = TRUE)
            )
        }
        header <- paste0(header, ", \n", .createSummaryHypothesisText(designPlan, summaryFactory))
        header <- .concatenateSummaryText(header, treatmentRateText)
        header <- .addEnrichmentEffectListToHeader(header, designPlan)
        header <- .addAdditionalArgumentsToHeader(header, designPlan, settings)
    }
    if (!inherits(designPlan, "SimulationResults") && designPlan$.isSampleSizeObject()) {
        header <- .concatenateSummaryText(header, paste0("power ", round(100 * (1 - design$beta), 1), "%"))
    }


    if (inherits(designPlan, "SimulationResults")) {
        header <- .concatenateSummaryText(header, paste0("simulation runs = ", designPlan$maxNumberOfIterations))
        header <- .concatenateSummaryText(header, paste0("seed = ", designPlan$seed))
    }
    header <- paste0(header, ".")
    return(header)
}

.addAdditionalArgumentsToHeader <- function(header, designPlan, settings) {
    if (settings$countDataEnabled && !is.null(designPlan[["lambda1"]]) &&
            length(designPlan$lambda1) == 1 &&
            designPlan$.getParameterType("lambda1") == C_PARAM_USER_DEFINED) {
        header <- .concatenateSummaryText(header, paste0(
            "lambda(1) = ", designPlan$lambda1
        ))
    }

    if (settings$countDataEnabled && !is.null(designPlan[["lambda2"]]) &&
            designPlan$.getParameterType("lambda2") == C_PARAM_USER_DEFINED) {
        header <- .concatenateSummaryText(header, paste0(
            "lambda(2) = ", designPlan$lambda2[1]
        ))
    }

    if (settings$countDataEnabled && !is.null(designPlan[["lambda"]]) &&
            designPlan$.getParameterType("lambda") == C_PARAM_USER_DEFINED) {
        header <- .concatenateSummaryText(header, paste0(
            "lambda = ", designPlan$lambda
        ))
    }

    if (designPlan$.design$kMax > 1) {
        if (settings$survivalEnabled) {
            if (!is.null(designPlan[["plannedEvents"]])) {
                header <- .concatenateSummaryText(header, paste0(
                    "planned cumulative events = ",
                    .arrayToString(designPlan$plannedEvents,
                        vectorLookAndFeelEnabled = (length(designPlan$plannedEvents) > 1)
                    )
                ))
            }
        } else {
            if (!is.null(designPlan[["plannedSubjects"]])) {
                header <- .concatenateSummaryText(header, paste0(
                    "planned cumulative sample size = ",
                    .arrayToString(designPlan$plannedSubjects,
                        vectorLookAndFeelEnabled = (length(designPlan$plannedSubjects) > 1)
                    )
                ))
            }
        }

        if (!is.null(designPlan[["maxNumberOfSubjects"]]) &&
                designPlan$.getParameterType("maxNumberOfSubjects") == C_PARAM_USER_DEFINED) {
            header <- .concatenateSummaryText(header, paste0(
                "maximum number of subjects = ",
                ceiling(designPlan$maxNumberOfSubjects[1])
            ))
        }

        if (settings$survivalEnabled) {
            if (!is.null(designPlan[["maxNumberOfEvents"]]) &&
                    designPlan$.getParameterType("maxNumberOfEvents") == C_PARAM_USER_DEFINED) {
                header <- .concatenateSummaryText(header, paste0(
                    "maximum number of events = ",
                    ceiling(designPlan$maxNumberOfEvents[1])
                ))
            }
        }
    } else {
        if (settings$survivalEnabled) {
            if (!is.null(designPlan[["plannedEvents"]])) {
                header <- .concatenateSummaryText(header, paste0(
                    "planned events = ",
                    .arrayToString(designPlan$plannedEvents,
                        vectorLookAndFeelEnabled = (length(designPlan$plannedEvents) > 1)
                    )
                ))
            }
        } else {
            if (!is.null(designPlan[["plannedSubjects"]])) {
                header <- .concatenateSummaryText(header, paste0(
                    "planned sample size = ",
                    .arrayToString(designPlan$plannedSubjects,
                        vectorLookAndFeelEnabled = (length(designPlan$plannedSubjects) > 1)
                    )
                ))
            }
        }

        if (!is.null(designPlan[["maxNumberOfSubjects"]]) &&
                designPlan$.getParameterType("maxNumberOfSubjects") == C_PARAM_USER_DEFINED) {
            header <- .concatenateSummaryText(header, paste0(
                "number of subjects = ",
                ceiling(designPlan$maxNumberOfSubjects[1])
            ))
        }

        if (settings$survivalEnabled) {
            if (!is.null(designPlan[["maxNumberOfEvents"]]) &&
                    designPlan$.getParameterType("maxNumberOfEvents") == C_PARAM_USER_DEFINED) {
                header <- .concatenateSummaryText(header, paste0(
                    "number of events = ",
                    designPlan$maxNumberOfEvents[1]
                ))
            }
        }
    }

    header <- .addAllocationRatioToHeader(designPlan, header)

    if (settings$survivalEnabled || settings$countDataEnabled) {
        if (settings$survivalEnabled && !is.null(designPlan[["eventTime"]]) &&
                !all(is.na(designPlan[["eventTime"]]))) {
            header <- .concatenateSummaryText(header, paste0(
                "event time = ",
                .arrayToString(designPlan$eventTime,
                    vectorLookAndFeelEnabled = (length(designPlan$eventTime) > 1)
                )
            ))
        }
        if (settings$countDataEnabled && !is.null(designPlan[["overdispersion"]]) &&
                !is.na(designPlan[["overdispersion"]])) {
            header <- .concatenateSummaryText(header, paste0(
                "overdispersion = ", designPlan$overdispersion[1]
            ))
        }
        if (settings$countDataEnabled && !is.null(designPlan[["fixedExposureTime"]]) &&
                !is.na(designPlan[["fixedExposureTime"]])) {
            header <- .concatenateSummaryText(header, paste0(
                "fixed exposure time = ", designPlan$fixedExposureTime[1]
            ))
        }
        if (!is.null(designPlan[["accrualTime"]]) && !all(is.na(designPlan$accrualTime))) {
            header <- .concatenateSummaryText(header, paste0(
                "accrual time = ",
                .arrayToString(designPlan$accrualTime,
                    vectorLookAndFeelEnabled = (length(designPlan$accrualTime) > 1)
                )
            ))
        }
        if (settings$countDataEnabled && !is.null(designPlan[["accrualTime"]]) &&
                !is.null(designPlan[["accrualIntensity"]]) && !all(is.na(designPlan$accrualIntensity))) {
            header <- .concatenateSummaryText(header, paste0(
                "accrual intensity = ",
                .arrayToString(designPlan$accrualIntensity,
                    digits = 1,
                    vectorLookAndFeelEnabled = (length(designPlan$accrualIntensity) > 1)
                )
            ))
        }
        if (settings$survivalEnabled && !is.null(designPlan[["accrualTime"]]) &&
                !is.null(designPlan[["accrualIntensity"]]) && !all(is.na(designPlan$accrualIntensity)) &&
                length(designPlan$accrualIntensity) == length(designPlan$accrualTime)) {
            header <- .concatenateSummaryText(header, paste0(
                "accrual intensity = ",
                .arrayToString(designPlan$accrualIntensity,
                    digits = 1,
                    vectorLookAndFeelEnabled = (length(designPlan$accrualIntensity) > 1)
                )
            ))
        }
        if (!is.null(designPlan[["followUpTime"]]) &&
                designPlan$.getParameterType("followUpTime") == C_PARAM_USER_DEFINED &&
                length(designPlan$followUpTime) == 1 &&
                !is.na(designPlan$followUpTime)) {
            header <- .concatenateSummaryText(header, paste0(
                "follow-up time = ", designPlan$followUpTime[1]
            ))
        }
        if (settings$survivalEnabled && !is.null(designPlan[["dropoutTime"]])) {
            if (designPlan$dropoutRate1 > 0 || designPlan$dropoutRate2 > 0) {
                header <- .concatenateSummaryText(header, paste0(
                    "dropout rate(1) = ",
                    .arrayToString(designPlan$dropoutRate1,
                        vectorLookAndFeelEnabled = (length(designPlan$dropoutRate1) > 1)
                    )
                ))
                header <- .concatenateSummaryText(header, paste0(
                    "dropout rate(2) = ",
                    .arrayToString(designPlan$dropoutRate2,
                        vectorLookAndFeelEnabled = (length(designPlan$dropoutRate2) > 1)
                    )
                ))
                header <- .concatenateSummaryText(header, paste0(
                    "dropout time = ",
                    .arrayToString(designPlan$dropoutTime,
                        vectorLookAndFeelEnabled = (length(designPlan$dropoutTime) > 1)
                    )
                ))
            }
        }
    }

    if (settings$multiArmEnabled && designPlan$activeArms > 1) {
        header <- .addShapeToHeader(header, designPlan)
        header <- .addSelectionToHeader(header, designPlan)
    }

    if (settings$enrichmentEnabled && settings$populations > 1) {
        header <- .addSelectionToHeader(header, designPlan)
    }

    functionName <- ifelse(settings$survivalEnabled, "calcEventsFunction", "calcSubjectsFunction")
    userDefinedFunction <- !is.null(designPlan[[functionName]]) &&
        designPlan$.getParameterType(functionName) == C_PARAM_USER_DEFINED

    if (userDefinedFunction || (!is.null(designPlan[["conditionalPower"]]) && !is.na(designPlan$conditionalPower))) {
        if (userDefinedFunction) {
            header <- .concatenateSummaryText(
                header,
                paste0("sample size reassessment: user defined '", functionName, "'")
            )
            if ((!is.null(designPlan[["conditionalPower"]]) && !is.na(designPlan$conditionalPower))) {
                header <- .concatenateSummaryText(
                    header,
                    paste0("conditional power = ", designPlan$conditionalPower)
                )
            }
        } else {
            if ((!is.null(designPlan[["conditionalPower"]]) && !is.na(designPlan$conditionalPower))) {
                header <- .concatenateSummaryText(
                    header,
                    paste0("sample size reassessment: conditional power = ", designPlan$conditionalPower)
                )
            }
        }

        paramName1 <- ifelse(settings$survivalEnabled, "minNumberOfEventsPerStage", "minNumberOfSubjectsPerStage")
        paramName2 <- ifelse(settings$survivalEnabled, "maxNumberOfEventsPerStage", "maxNumberOfSubjectsPerStage")
        paramCaption <- ifelse(settings$survivalEnabled, "events", "subjects")
        if (!is.null(designPlan[[paramName1]])) {
            header <- .concatenateSummaryText(header, paste0(
                "minimum ", paramCaption, " per stage = ",
                .arrayToString(designPlan[[paramName1]],
                    vectorLookAndFeelEnabled = (length(designPlan[[paramName1]]) > 1)
                )
            ))
        }
        if (!is.null(designPlan[[paramName2]])) {
            header <- .concatenateSummaryText(header, paste0(
                "maximum ", paramCaption, " per stage = ",
                .arrayToString(designPlan[[paramName2]],
                    vectorLookAndFeelEnabled = (length(designPlan[[paramName2]]) > 1)
                )
            ))
        }

        if (settings$meansEnabled) {
            if (!is.na(designPlan$thetaH1)) {
                header <- .concatenateSummaryText(
                    header,
                    paste0("theta H1 = ", round(designPlan$thetaH1, 3))
                )
            }
            if (!is.na(designPlan$stDevH1)) {
                header <- .concatenateSummaryText(
                    header,
                    paste0("standard deviation H1 = ", round(designPlan$stDevH1, 3))
                )
            }
        } else if (settings$ratesEnabled) {
            if (settings$multiArmEnabled || settings$enrichmentEnabled) {
                if (settings$multiArmEnabled && !is.na(designPlan$piTreatmentsH1)) {
                    header <- .concatenateSummaryText(
                        header,
                        paste0("pi(treatment)H1 = ", round(designPlan$piTreatmentsH1, 3))
                    )
                } else if (settings$enrichmentEnabled) {
                    piTreatmentH1 <- designPlan[["piTreatmentH1"]]
                    if (is.null(piTreatmentH1)) {
                        piTreatmentH1 <- designPlan[["piTreatmentsH1"]]
                    }
                    if (!is.null(piTreatmentH1) && !is.na(piTreatmentH1)) {
                        header <- .concatenateSummaryText(
                            header,
                            paste0("pi(treatment)H1 = ", round(piTreatmentH1, 3))
                        )
                    }
                }
                if (!is.na(designPlan$piControlH1)) {
                    header <- .concatenateSummaryText(
                        header,
                        paste0("pi(control)H1 = ", round(designPlan$piControlH1, 3))
                    )
                }
            } else {
                if (!is.na(designPlan$pi1H1)) {
                    header <- .concatenateSummaryText(
                        header,
                        paste0("pi(treatment)H1 = ", round(designPlan$pi1H1, 3))
                    )
                }
                if (!is.na(designPlan$pi2H1)) {
                    header <- .concatenateSummaryText(
                        header,
                        paste0("pi(control)H1 = ", round(designPlan$pi2H1, 3))
                    )
                }
            }
        }

        if (settings$survivalEnabled && !is.null(designPlan[["thetaH1"]]) && !is.na(designPlan$thetaH1)) {
            header <- .concatenateSummaryText(header, paste0("thetaH1 = ", round(designPlan$thetaH1, 3)))
        }
    }

    return(header)
}

.addShapeToHeader <- function(header, designPlan) {
    header <- .concatenateSummaryText(header, paste0("effect shape = ", .formatCamelCase(designPlan$typeOfShape)))
    if (designPlan$typeOfShape == "sigmoidEmax") {
        header <- .concatenateSummaryText(header, paste0("slope = ", designPlan$slope))
        header <- .concatenateSummaryText(header, paste0("ED50 = ", designPlan$gED50))
    }

    return(header)
}

.addSelectionToHeader <- function(header, designPlan) {
    header <- .concatenateSummaryText(header, paste0("intersection test = ", designPlan$intersectionTest))

    if (designPlan$.design$kMax > 1) {
        typeOfSelectionText <- paste0("selection = ", .formatCamelCase(designPlan$typeOfSelection))
        if (designPlan$typeOfSelection == "rBest") {
            typeOfSelectionText <- paste0(typeOfSelectionText, ", r = ", designPlan$rValue)
        } else if (designPlan$typeOfSelection == "epsilon") {
            typeOfSelectionText <- paste0(typeOfSelectionText, " rule, eps = ", designPlan$epsilonValue)
        }
        if (!is.null(designPlan$threshold) && length(designPlan$threshold) == 1 && designPlan$threshold > -Inf) {
            typeOfSelectionText <- paste0(typeOfSelectionText, ", threshold = ", designPlan$threshold)
        }
        header <- .concatenateSummaryText(header, typeOfSelectionText)

        header <- .concatenateSummaryText(
            header,
            paste0("effect measure based on ", .formatCamelCase(designPlan$effectMeasure))
        )
    }

    header <- .concatenateSummaryText(
        header,
        paste0("success criterion: ", .formatCamelCase(designPlan$successCriterion))
    )

    return(header)
}

.createSummary <- function(object, digits = NA_integer_, output = c("all", "title", "overview", "body")) {
    output <- match.arg(output)
    if (inherits(object, "TrialDesignCharacteristics")) {
        return(.createSummaryDesignPlan(object, digits = digits, output = output, showStageLevels = TRUE))
    }

    if (.isTrialDesign(object) || .isTrialDesignPlan(object) || inherits(object, "SimulationResults")) {
        return(.createSummaryDesignPlan(object, digits = digits, output = output, showStageLevels = !.isTrialDesignPlan(object)))
    }

    if (inherits(object, "AnalysisResults")) {
        return(.createSummaryAnalysisResults(object, digits = digits, output = output))
    }

    if (inherits(object, "PerformanceScore")) {
        return(.createSummaryPerformanceScore(object, digits = digits, output = output))
    }

    stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "function 'summary' not implemented yet for class ", .getClassName(object))
}

.createSummaryPerformanceScore <- function(object, digits = NA_integer_, output = c("all", "title", "overview", "body")) {
    .createSummaryDesignPlan(object$.simulationResults,
        digits = digits, output = output,
        showStageLevels = TRUE, performanceScore = object
    )
}

.getSummaryParameterCaptionCriticalValues <- function(design) {
    parameterCaption <- ifelse(.isTrialDesignFisher(design),
        "Efficacy boundary (p product scale)", "Efficacy boundary (z-value scale)"
    )
    parameterCaption <- ifelse(.isDelayedInformationEnabled(design = design),
        "Upper bounds of continuation", parameterCaption
    )
    return(parameterCaption)
}

.getSummaryParameterCaptionFutilityBounds <- function(design) {
    bindingInfo <- ifelse(design$bindingFutility, "binding", "non-binding")
    parameterCaption <- ifelse(.isDelayedInformationEnabled(design = design),
        paste0("Lower bounds of continuation (", bindingInfo, ")"),
        paste0("Futility boundary (z-value scale)")
    )
    return(parameterCaption)
}

#'
#' Main function for creating a summary of an analysis result
#'
#' @noRd
#'
.createSummaryAnalysisResults <- function(object, digits = NA_integer_, output = c("all", "title", "overview", "body")) {
    output <- match.arg(output)
    if (!inherits(object, "AnalysisResults")) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'object' must be a valid analysis result object (is class ", .getClassName(object), ")"
        )
    }

    digitSettings <- .getSummaryDigits(digits)
    digits <- digitSettings$digits
    digitsSampleSize <- digitSettings$digitsSampleSize
    digitsGeneral <- digitSettings$digitsGeneral
    digitsProbabilities <- digitSettings$digitsProbabilities

    outputSize <- getOption("rpact.summary.output.size", C_SUMMARY_OUTPUT_SIZE_DEFAULT)

    intervalFormat <- getOption("rpact.summary.intervalFormat", "[%s; %s]")
    .assertIsValidSummaryIntervalFormat(intervalFormat)

    multiArmEnabled <- .isMultiArmAnalysisResults(object)
    enrichmentEnabled <- .isEnrichmentAnalysisResults(object)
    multiHypothesesEnabled <- .isMultiHypothesesAnalysisResults(object)

    analysisResults <- object
    design <- analysisResults$.design
    stageResults <- analysisResults$.stageResults
    dataInput <- analysisResults$.dataInput
    closedTestResults <- NULL
    conditionalPowerResults <- NULL
    if (multiHypothesesEnabled) {
        closedTestResults <- analysisResults$.closedTestResults
        if (length(analysisResults$nPlanned) > 0 && !all(is.na(analysisResults$nPlanned))) {
            conditionalPowerResults <- analysisResults$.conditionalPowerResults
        }
    }

    summaryFactory <- SummaryFactory$new(object = object, intervalFormat = intervalFormat, output = output)

    .addDesignInformationToSummary(design, object, summaryFactory, output = output)

    if (!.isTrialDesignConditionalDunnett(design)) {
        summaryFactory$addParameter(design,
            parameterName = "criticalValues",
            parameterCaption = .getSummaryParameterCaptionCriticalValues(design),
            roundDigits = digitsProbabilities - ifelse(.isTrialDesignFisher(design) || digitsProbabilities <= 1, 0, 1),
            smoothedZeroFormat = !.isTrialDesignFisher(design)
        )
    }

    if (.isTrialDesignFisher(design)) {
        if (any(design$alpha0Vec < 1)) {
            summaryFactory$addParameter(design,
                parameterName = "alpha0Vec",
                parameterCaption = "Futility boundary (separate p-value scale)",
                roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE
            )
        }
    } else if (!.isTrialDesignConditionalDunnett(design)) {
        if (any(design$futilityBounds > -6)) {
            summaryFactory$addParameter(design,
                parameterName = "futilityBounds",
                parameterCaption = .getSummaryParameterCaptionFutilityBounds(design),
                roundDigits = ifelse(digitsProbabilities > 1, digitsProbabilities - 1, digitsProbabilities),
                smoothedZeroFormat = TRUE
            )
        }
    }

    if (design$kMax > 1 && !.isTrialDesignConditionalDunnett(design)) {
        summaryFactory$addParameter(design,
            parameterName = "alphaSpent",
            parameterCaption = "Cumulative alpha spent",
            roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE
        )
    }

    if (!.isTrialDesignConditionalDunnett(design)) {
        summaryFactory$addParameter(design,
            parameterName = "stageLevels",
            parameterCaption = "Stage level", roundDigits = digitsProbabilities,
            smoothedZeroFormat = TRUE
        )
    }

    summaryFactory$addParameter(stageResults,
        parameterName = "effectSizes",
        parameterCaption = ifelse(stageResults$isDatasetRates() && dataInput$getNumberOfGroups() == 1,
            "Cumulative treatment rate", "Cumulative effect size"
        ), roundDigits = digitsGeneral
    )

    if (stageResults$isDatasetMeans()) {
        parameterCaption <- ifelse(stageResults$isOneSampleDataset(),
            "Cumulative standard deviation", "Cumulative (pooled) standard deviation"
        )
        parameterName <- ifelse(inherits(stageResults, "StageResultsMultiArmMeans") &&
            !inherits(stageResults, "StageResultsEnrichmentMeans"),
        "overallPooledStDevs", "overallStDevs"
        )
        summaryFactory$addParameter(stageResults,
            parameterName = parameterName,
            parameterCaption = parameterCaption, roundDigits = digitsGeneral,
            enforceFirstCase = (parameterName == "overallPooledStDevs")
        )
    } else if (stageResults$isDatasetRates()) {
        if (outputSize != "small" && dataInput$getNumberOfGroups() > 1) {
            treatmentRateParamName <- "overallPi1"
            controlRateParamName <- "overallPi2"
            if (.isEnrichmentStageResults(stageResults)) {
                treatmentRateParamName <- "overallPisTreatment"
                controlRateParamName <- "overallPisControl"
            } else if (.isMultiArmStageResults(stageResults)) {
                treatmentRateParamName <- "overallPiTreatments"
                controlRateParamName <- "overallPiControl"
            }
            summaryFactory$addParameter(stageResults,
                parameterName = treatmentRateParamName,
                parameterCaption = "Cumulative treatment rate", roundDigits = digitsGeneral
            )
            summaryFactory$addParameter(stageResults,
                parameterName = controlRateParamName,
                parameterCaption = "Cumulative control rate", roundDigits = digitsGeneral, enforceFirstCase = TRUE
            )
        }
    }

    if (.isTrialDesignGroupSequential(design)) {
        summaryFactory$addParameter(stageResults,
            parameterName = "overallTestStatistics",
            parameterCaption = "Overall test statistic",
            roundDigits = ifelse(digitsProbabilities > 1, digitsProbabilities - 1, digitsProbabilities),
            smoothedZeroFormat = TRUE
        )
        summaryFactory$addParameter(stageResults,
            parameterName = ifelse(multiHypothesesEnabled, "separatePValues", "overallPValues"),
            parameterCaption = "Overall p-value", roundDigits = digitsProbabilities
        )
    } else {
        summaryFactory$addParameter(stageResults,
            parameterName = "testStatistics",
            parameterCaption = "Stage-wise test statistic",
            roundDigits = ifelse(digitsProbabilities > 1, digitsProbabilities - 1, digitsProbabilities),
            smoothedZeroFormat = TRUE
        )
        summaryFactory$addParameter(stageResults,
            parameterName = ifelse(multiHypothesesEnabled, "separatePValues", "pValues"),
            parameterCaption = "Stage-wise p-value", roundDigits = digitsProbabilities
        )
    }

    if (!is.null(closedTestResults)) {
        if (outputSize == "large") {
            if (.isTrialDesignConditionalDunnett(design)) {
                summaryFactory$addParameter(closedTestResults,
                    parameterName = "conditionalErrorRate",
                    parameterCaption = "Conditional error rate", roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE
                )
                summaryFactory$addParameter(closedTestResults,
                    parameterName = "secondStagePValues",
                    parameterCaption = "Second stage p-value", roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE
                )
            } else {
                summaryFactory$addParameter(closedTestResults,
                    parameterName = "adjustedStageWisePValues",
                    parameterCaption = "Adjusted stage-wise p-value",
                    roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE
                )
                summaryFactory$addParameter(closedTestResults,
                    parameterName = "overallAdjustedTestStatistics",
                    parameterCaption = "Overall adjusted test statistic",
                    roundDigits = digitsProbabilities - ifelse(.isTrialDesignFisher(design) || digitsProbabilities <= 1, 0, 1),
                    smoothedZeroFormat = !.isTrialDesignFisher(design)
                )
            }
        } else if (outputSize == "medium") {
            legendEntry <- list("(i, j, ...)" = "comparison of treatment arms 'i, j, ...' vs. control arm")
            gMax <- stageResults$getGMax()
            if (.isTrialDesignConditionalDunnett(design)) {
                summaryFactory$addParameter(closedTestResults,
                    parameterName = "adjustedStageWisePValues",
                    values = closedTestResults$conditionalErrorRate[1, ],
                    parameterCaption = paste0(
                        "Conditional error rate (",
                        paste0(1:gMax, collapse = ", "), ")"
                    ), roundDigits = digitsProbabilities,
                    smoothedZeroFormat = TRUE,
                    legendEntry = legendEntry
                )
                summaryFactory$addParameter(closedTestResults,
                    parameterName = "overallAdjustedTestStatistics",
                    values = closedTestResults$secondStagePValues[1, ],
                    parameterCaption = paste0(
                        "Second stage p-value (",
                        paste0(1:gMax, collapse = ", "), ")"
                    ),
                    roundDigits = digitsProbabilities + ifelse(.isTrialDesignFisher(design), 1, 0),
                    smoothedZeroFormat = !.isTrialDesignFisher(design),
                    legendEntry = legendEntry
                )
            } else {
                summaryFactory$addParameter(closedTestResults,
                    parameterName = "adjustedStageWisePValues",
                    values = closedTestResults$adjustedStageWisePValues[1, ],
                    parameterCaption = paste0(
                        "Adjusted stage-wise p-value (",
                        paste0(1:gMax, collapse = ", "), ")"
                    ), roundDigits = digitsProbabilities,
                    smoothedZeroFormat = TRUE, legendEntry = legendEntry
                )
                summaryFactory$addParameter(closedTestResults,
                    parameterName = "overallAdjustedTestStatistics",
                    values = closedTestResults$overallAdjustedTestStatistics[1, ],
                    parameterCaption = paste0(
                        "Overall adjusted test statistic (",
                        paste0(1:gMax, collapse = ", "), ")"
                    ),
                    roundDigits = digitsProbabilities - ifelse(.isTrialDesignFisher(design) || digitsProbabilities <= 1, 0, 1),
                    smoothedZeroFormat = !.isTrialDesignFisher(design),
                    legendEntry = legendEntry
                )
            }
        }
    }

    if (multiHypothesesEnabled) {
        summaryFactory$addParameter(closedTestResults,
            parameterName = "rejected",
            parameterCaption = "Test action: reject", roundDigits = digitsGeneral
        )
    } else {
        if (.isTrialDesignFisher(design)) {
            summaryFactory$addParameter(stageResults,
                parameterName = "combFisher",
                parameterCaption = "Fisher combination", roundDigits = 0
            )
        } else if (.isTrialDesignInverseNormal(design)) {
            summaryFactory$addParameter(stageResults,
                parameterName = "combInverseNormal",
                parameterCaption = "Inverse normal combination",
                roundDigits = ifelse(digitsProbabilities > 1, digitsProbabilities - 1, digitsProbabilities),
                smoothedZeroFormat = TRUE
            )
        }
        summaryFactory$addParameter(analysisResults,
            parameterName = "testActions",
            parameterCaption = "Test action", roundDigits = digitsGeneral
        )
    }

    if (design$kMax > 1 && !.isTrialDesignConditionalDunnett(design)) {
        summaryFactory$addParameter(analysisResults,
            parameterName = "conditionalRejectionProbabilities",
            parameterCaption = "Conditional rejection probability",
            roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE
        )
    }

    if (design$kMax > 1) {
        if (!is.null(conditionalPowerResults)) {
            summaryFactory$addParameter(conditionalPowerResults,
                parameterName = "nPlanned",
                parameterCaption = "Planned sample size", roundDigits = -1
            )
        } else if (analysisResults$.getParameterType("nPlanned") != C_PARAM_NOT_APPLICABLE) {
            summaryFactory$addParameter(analysisResults,
                parameterName = "nPlanned",
                parameterCaption = "Planned sample size", roundDigits = -1
            )
        }
    }

    if (design$kMax > 1) {
        if (!is.null(conditionalPowerResults)) {
            summaryFactory$addParameter(conditionalPowerResults,
                parameterName = "conditionalPower",
                parameterCaption = "Conditional power",
                roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE
            )
        } else if (!multiHypothesesEnabled && analysisResults$.getParameterType("nPlanned") != C_PARAM_NOT_APPLICABLE) {
            parameterName <- "conditionalPower"
            if (!is.null(analysisResults[["conditionalPowerSimulated"]]) &&
                    length(analysisResults[["conditionalPowerSimulated"]]) > 0) {
                parameterName <- "conditionalPowerSimulated"
            }
            summaryFactory$addParameter(analysisResults,
                parameterName = parameterName,
                parameterCaption = "Conditional power",
                roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE
            )
        }
    }

    ciLevel <- round((1 - design$alpha * (3 - design$sided)) * 100, 2)
    if (.isTrialDesignConditionalDunnett(design)) {
        parameterCaptionRepeatedPValues <- "Overall p-value"
        parameterCaptionRepeatedCI <- paste0(ciLevel, "% overall confidence interval")
    } else {
        parameterCaptionRepeatedPValues <- ifelse(design$kMax == 1,
            ifelse(design$sided == 1, "One-sided p-value", "Two-sided p-value"),
            "Repeated p-value"
        )
        parameterCaptionRepeatedCI <- paste0(
            ciLevel, "% ",
            ifelse(design$kMax == 1, "confidence interval", "repeated confidence interval")
        )
    }

    summaryFactory$addParameter(analysisResults,
        parameterName = c("repeatedConfidenceIntervalLowerBounds", "repeatedConfidenceIntervalUpperBounds"),
        parameterCaption = parameterCaptionRepeatedCI,
        roundDigits = digitsGeneral
    )

    summaryFactory$addParameter(analysisResults,
        parameterName = "repeatedPValues",
        parameterCaption = parameterCaptionRepeatedPValues,
        roundDigits = digitsProbabilities, formatRepeatedPValues = TRUE
    )

    if (!multiHypothesesEnabled && !is.null(analysisResults[["finalStage"]]) && !all(is.na(analysisResults$finalStage))) {
        summaryFactory$addParameter(analysisResults,
            parameterName = "finalPValues",
            parameterCaption = "Final p-value", roundDigits = digitsProbabilities
        )
        summaryFactory$addParameter(analysisResults,
            parameterName = c("finalConfidenceIntervalLowerBounds", "finalConfidenceIntervalUpperBounds"),
            parameterCaption = "Final confidence interval", roundDigits = digitsGeneral
        )
        summaryFactory$addParameter(analysisResults,
            parameterName = "medianUnbiasedEstimates",
            parameterCaption = "Median unbiased estimate", roundDigits = digitsGeneral
        )
    }

    return(summaryFactory)
}

.getSummaryDigits <- function(digits = NA_integer_) {
    if (is.na(digits)) {
        digits <- as.integer(getOption("rpact.summary.digits", 3))
    }
    .assertIsSingleInteger(digits, "digits", validateType = FALSE, naAllowed = TRUE)
    .assertIsInClosedInterval(digits, "digits", lower = -1, upper = 12, naAllowed = TRUE)

    digitsSampleSize <- 1
    if (digits > 0) {
        digitsGeneral <- digits
        digitsProbabilities <- NA_integer_
        tryCatch(
            {
                digitsProbabilities <- as.integer(getOption("rpact.summary.digits.probs", digits + 1))
            },
            warning = function(e) {
            }
        )
        if (is.na(digitsProbabilities)) {
            digitsProbabilities <- digits + 1
        }
        .assertIsSingleInteger(digitsProbabilities, "digitsProbabilities", validateType = FALSE, naAllowed = FALSE)
        .assertIsInClosedInterval(digitsProbabilities, "digitsProbabilities", lower = -1, upper = 12, naAllowed = FALSE)
    } else {
        digitsSampleSize <- digits
        digitsGeneral <- digits
        digitsProbabilities <- digits
    }
    return(list(
        digits = digits,
        digitsSampleSize = digitsSampleSize,
        digitsGeneral = digitsGeneral,
        digitsProbabilities = digitsProbabilities
    ))
}

.getSummaryValuesInPercent <- function(values, percentFormatEnabled = TRUE, digits = 1) {
    if (!percentFormatEnabled) {
        return(as.character(round(values, digits + 2)))
    }
    return(paste0(round(100 * values, digits), "%"))
}

.addDesignInformationToSummary <- function(design, designPlan, summaryFactory,
        output = c("all", "title", "overview", "body")) {
    if (!(output %in% c("all", "overview"))) {
        return(invisible(summaryFactory))
    }

    if (design$kMax == 1) {
        summaryFactory$addItem("Stage", "Fixed")
        return(invisible(summaryFactory))
    }

    summaryFactory$addItem("Stage", c(1:design$kMax))

    if (.isTrialDesignConditionalDunnett(design)) {
        summaryFactory$addItem(
            "Fixed information at interim",
            .getSummaryValuesInPercent(design$informationAtInterim, FALSE)
        )
        return(invisible(summaryFactory))
    }

    informationRatesCaption <- ifelse(inherits(designPlan, "SimulationResults") ||
        inherits(designPlan, "AnalysisResults"), "Fixed weight", "Information")

    if (inherits(designPlan, "SimulationResults") || inherits(designPlan, "AnalysisResults")) {
        if (.isTrialDesignFisher(design)) {
            weights <- .getWeightsFisher(design)
        } else if (.isTrialDesignInverseNormal(design)) {
            weights <- .getWeightsInverseNormal(design)
        } else {
            weights <- design$informationRates
        }
        summaryFactory$addItem(informationRatesCaption, .getSummaryValuesInPercent(weights, FALSE))
    } else {
        summaryFactory$addItem(
            paste0(
                informationRatesCaption,
                ifelse(inherits(designPlan, "SimulationResults"), "", " rate")
            ),
            .getSummaryValuesInPercent(design$informationRates)
        )
    }

    if (design$.isDelayedResponseDesign()) {
        summaryFactory$addItem("Delayed information", .getSummaryValuesInPercent(design$delayedInformation, TRUE))
    }

    return(invisible(summaryFactory))
}

.addDesignParameterToSummary <- function(design, designPlan,
        designCharacteristics, summaryFactory, digitsGeneral, digitsProbabilities) {
    if (design$kMax > 1 && !inherits(designPlan, "SimulationResults") &&
            !.isTrialDesignConditionalDunnett(design)) {
        summaryFactory$addParameter(design,
            parameterName = "alphaSpent",
            parameterCaption = "Cumulative alpha spent",
            roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE
        )
        if (design$.getParameterType("betaSpent") == C_PARAM_GENERATED) {
            summaryFactory$addParameter(design,
                parameterName = "betaSpent",
                parameterCaption = "Cumulative beta spent",
                roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE
            )
        }
    }

    if (!is.null(designPlan)) {
        if (!grepl("SimulationResults(MultiArm|Enrichment)", .getClassName(designPlan))) {
            outputSize <- getOption("rpact.summary.output.size", C_SUMMARY_OUTPUT_SIZE_DEFAULT)
            if (outputSize == "large" && inherits(designPlan, "SimulationResults")) {
                summaryFactory$addParameter(designPlan,
                    parameterName = "conditionalPowerAchieved",
                    parameterCaption = "Conditional power (achieved)",
                    roundDigits = digitsProbabilities
                )
            }
        }
    } else {
        powerObject <- NULL
        if (!is.null(designCharacteristics)) {
            powerObject <- designCharacteristics
        } else if (design$.getParameterType("power") == C_PARAM_GENERATED) {
            powerObject <- design
        }
        if (!is.null(powerObject)) {
            summaryFactory$addParameter(powerObject,
                parameterName = "power",
                parameterCaption = ifelse(design$kMax == 1, "Power", "Overall power"),
                roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE
            )
        }
        if (design$kMax > 1 && .isTrialDesignInverseNormalOrGroupSequential(design)) {
            tryCatch(
                {
                    designCharacteristics <- getDesignCharacteristics(design)
                },
                error = function(e) {
                    designCharacteristics <- NULL
                }
            )
            if (!is.null(designCharacteristics) &&
                    !any(is.na(designCharacteristics$futilityProbabilities)) &&
                    any(designCharacteristics$futilityProbabilities > 0)) {
                summaryFactory$addParameter(designCharacteristics,
                    parameterName = "futilityProbabilities",
                    parameterCaption = "Futility probabilities under H1",
                    roundDigits = digitsGeneral, smoothedZeroFormat = TRUE
                )
            }
        }
    }

    if (design$.isDelayedResponseDesign()) {
        summaryFactory$addParameter(design,
            parameterName = "decisionCriticalValues",
            parameterCaption = "Decision critical values",
            roundDigits = digitsGeneral,
            smoothedZeroFormat = TRUE
        )

        outputSize <- getOption("rpact.summary.output.size", C_SUMMARY_OUTPUT_SIZE_DEFAULT)
        if (outputSize == "large") {
            summaryFactory$addParameter(design,
                parameterName = "reversalProbabilities",
                parameterCaption = "Reversal probabilities",
                roundDigits = digitsProbabilities,
                smoothedZeroFormat = TRUE
            )
        }
    }

    if (.isTrialDesignConditionalDunnett(design)) {
        summaryFactory$addParameter(design,
            parameterName = "alpha",
            parameterCaption = "Significance level", roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE
        )
    } else if (!is.null(designPlan) && !inherits(designPlan, "SimulationResults")) {
        summaryFactory$addParameter(design,
            parameterName = "stageLevels",
            twoSided = design$sided == 2,
            parameterCaption = paste0(ifelse(design$sided == 2, "Two", "One"), "-sided local significance level"),
            roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE
        )
    }
    return(summaryFactory)
}

#'
#' Main function for creating a summary of a design or design plan
#'
#' @noRd
#'
.createSummaryDesignPlan <- function(object, digits = NA_integer_,
        output = c("all", "title", "overview", "body"), showStageLevels = FALSE,
        performanceScore = NULL) {
    output <- match.arg(output)
    designPlan <- NULL
    if (.isTrialDesignPlan(object) || inherits(object, "SimulationResults")) {
        design <- object$.design
        designPlan <- object
    } else if (inherits(object, "TrialDesignCharacteristics")) {
        design <- object$.design
        # designPlan <- object
    } else if (.isTrialDesign(object)) {
        design <- object
    } else {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'object' must be a valid design, design plan, ",
            "or simulation result object (is class ", .getClassName(object), ")"
        )
    }

    digitSettings <- .getSummaryDigits(digits)
    digits <- digitSettings$digits
    digitsSampleSize <- digitSettings$digitsSampleSize
    digitsGeneral <- digitSettings$digitsGeneral
    digitsProbabilities <- digitSettings$digitsProbabilities

    outputSize <- getOption("rpact.summary.output.size", C_SUMMARY_OUTPUT_SIZE_DEFAULT)

    intervalFormat <- getOption("rpact.summary.intervalFormat", "[%s; %s]")
    .assertIsValidSummaryIntervalFormat(intervalFormat)

    summaryFactory <- SummaryFactory$new(object = object, intervalFormat = intervalFormat, output = output)

    if (output %in% c("all", "title", "overview")) {
        .addDesignInformationToSummary(design, designPlan, summaryFactory, output = output)
    }

    if (!(output %in% c("all", "body"))) {
        return(summaryFactory)
    }

    if (!.isTrialDesignConditionalDunnett(design)) {
        summaryFactory$addParameter(design,
            parameterName = "criticalValues",
            parameterCaption = .getSummaryParameterCaptionCriticalValues(design),
            roundDigits = digitsGeneral
        )

        if (showStageLevels) {
            summaryFactory$addParameter(design,
                parameterName = "stageLevels",
                parameterCaption = "Stage levels (one-sided)",
                roundDigits = digitsProbabilities,
                smoothedZeroFormat = TRUE
            )
        }
    }

    if (.isTrialDesignFisher(design)) {
        if (any(design$alpha0Vec < 1)) {
            summaryFactory$addParameter(design,
                parameterName = "alpha0Vec",
                parameterCaption = "Futility boundary (separate p-value scale)",
                roundDigits = digitsGeneral
            )
        }
    } else if (!.isTrialDesignConditionalDunnett(design)) {
        if (any(design$futilityBounds > C_FUTILITY_BOUNDS_DEFAULT, na.rm = TRUE)) {
            summaryFactory$addParameter(design,
                parameterName = "futilityBounds",
                parameterCaption = .getSummaryParameterCaptionFutilityBounds(design),
                roundDigits = digitsGeneral
            )
        }
    }

    designCharacteristics <- NULL
    if (design$kMax > 1 && .isTrialDesignInverseNormalOrGroupSequential(design)) {
        tryCatch(
            {
                designCharacteristics <- getDesignCharacteristics(design)
            },
            error = function(e) {
                designCharacteristics <- NULL
            }
        )
    }

    if (is.null(designPlan)) {
        return(.addDesignParameterToSummary(
            design,
            designPlan,
            designCharacteristics,
            summaryFactory,
            digitsGeneral,
            digitsProbabilities
        ))
    }

    multiArmEnabled <- grepl("MultiArm", .getClassName(designPlan))
    enrichmentEnabled <- grepl("Enrichment", .getClassName(designPlan))
    baseEnabled <- grepl(
        "(TrialDesignPlan|SimulationResults)(Means|Rates|Survival|CountData)",
        .getClassName(designPlan)
    )
    planningEnabled <- .isTrialDesignPlan(designPlan)
    simulationEnabled <- .isSimulationResults(designPlan)
    survivalEnabled <- grepl("Survival", .getClassName(designPlan))
    countDataEnabled <- .isTrialDesignPlanCountData(designPlan)

    probsH0 <- NULL
    probsH1 <- NULL
    if (design$kMax > 1) {
        if (!is.null(designCharacteristics) &&
                .isTrialDesignInverseNormalOrGroupSequential(design) &&
                length(designCharacteristics$shift) == 1 &&
                !is.na(designCharacteristics$shift) &&
                designCharacteristics$shift >= 1) {
            probsH0 <- getPowerAndAverageSampleNumber(design, theta = 0, nMax = designCharacteristics$shift)
            probsH1 <- getPowerAndAverageSampleNumber(design, theta = 1, nMax = designCharacteristics$shift)
        }
        if (!is.null(designPlan[["rejectPerStage"]])) {
            probsH1 <- list(
                earlyStop = designPlan$rejectPerStage[1:(design$kMax - 1), ] + as.vector(designPlan$futilityPerStage),
                rejectPerStage = designPlan$rejectPerStage,
                futilityPerStage = designPlan$futilityPerStage
            )
            numberOfVariants <- 1
            if (inherits(designPlan, "ParameterSet")) {
                parameterNames <- designPlan$.getVisibleFieldNamesOrdered()
                numberOfVariants <- .getMultidimensionalNumberOfVariants(designPlan, parameterNames)
            }
            if (numberOfVariants > 1 && is.matrix(probsH1$earlyStop) && ncol(probsH1$earlyStop) == 1) {
                probsH1$earlyStop <- matrix(rep(probsH1$earlyStop, numberOfVariants), ncol = numberOfVariants)
                probsH1$rejectPerStage <- matrix(rep(probsH1$rejectPerStage, numberOfVariants), ncol = numberOfVariants)
                probsH1$futilityPerStage <- matrix(rep(probsH1$futilityPerStage, numberOfVariants), ncol = numberOfVariants)
            }
        }
    }

    if (simulationEnabled && (multiArmEnabled || enrichmentEnabled)) {
        # simulation multi-arm #1:rejectAtLeastOne per mu_max
        summaryFactory$addParameter(designPlan,
            parameterName = "rejectAtLeastOne",
            parameterCaption = "Reject at least one", roundDigits = digitsProbabilities,
            smoothedZeroFormat = TRUE, transpose = TRUE,
            legendEntry = {
                if (multiArmEnabled) list("(i)" = "treatment arm i") else list()
            }
        )

        # simulation multi-arm #2: rejectedArmsPerStage
        if (outputSize == "large" && multiArmEnabled) {
            .addSimulationMultiArmArrayParameter(designPlan,
                parameterName = "rejectedArmsPerStage",
                parameterCaption = ifelse(design$kMax == 1, "Rejected arms", "Rejected arms per stage"),
                summaryFactory, roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE
            )
        }
        # simulation enrichment #2: rejectedPopulationsPerStage
        if (outputSize == "large" && enrichmentEnabled) {
            .addSimulationArrayToSummary(designPlan,
                parameterName = "rejectedPopulationsPerStage",
                parameterCaption = ifelse(design$kMax == 1, "Rejected populations", "Rejected populations per stage"),
                summaryFactory, digitsSampleSize = digitsProbabilities, smoothedZeroFormat = TRUE
            )
        }

        # simulation multi-arm #3: successPerStage
        summaryFactory$addParameter(designPlan,
            parameterName = "successPerStage",
            parameterCaption = "Success per stage",
            roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE, transpose = TRUE
        )

        # simulation multi-arm #4: futilityPerStage
        if (!planningEnabled && !baseEnabled && any(designPlan$futilityPerStage != 0)) {
            summaryFactory$addParameter(designPlan,
                parameterName = "futilityPerStage",
                parameterCaption = "Exit probability for futility", # (under H1)
                roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE, transpose = TRUE
            )
        }

        if (survivalEnabled) {
            summaryFactory$addParameter(designPlan,
                parameterName = "expectedNumberOfEvents",
                parameterCaption = "Expected number of events under H1",
                roundDigits = digitsSampleSize, transpose = TRUE
            )
        } else {
            summaryFactory$addParameter(designPlan,
                parameterName = "expectedNumberOfSubjects",
                parameterCaption = "Expected number of subjects under H1",
                roundDigits = digitsSampleSize, transpose = TRUE
            )
        }

        # simulation multi-arm #5: earlyStop per mu_max
        if (outputSize %in% c("medium", "large")) {
            summaryFactory$addParameter(designPlan,
                parameterName = "earlyStop",
                parameterCaption = "Overall exit probability", # (under H1)
                roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE, transpose = TRUE
            )
        }

        # simulation multi-arm / enrichment #6: sampleSizes
        if (outputSize %in% c("medium", "large")) {
            if (survivalEnabled) {
                if (enrichmentEnabled) {
                    parameterName <- "singleEventsPerSubsetAndStage"
                    parameterCaption <- "Single number of events"
                } else {
                    parameterName <- "cumulativeEventsPerStage"
                    parameterCaption <- "Cumulative number of events"
                }
            } else {
                parameterName <- "sampleSizes"
                parameterCaption <- "Stagewise number of subjects"
            }
            .addSimulationArrayToSummary(
                designPlan,
                parameterName,
                parameterCaption,
                summaryFactory,
                digitsSampleSize,
                smoothedZeroFormat = TRUE
            )
        }

        # simulation multi-arm #7: selectedArms
        if (multiArmEnabled && outputSize %in% c("medium", "large")) {
            .addSimulationMultiArmArrayParameter(
                designPlan = designPlan,
                parameterName = "selectedArms",
                parameterCaption = "Selected arms",
                summaryFactory = summaryFactory,
                roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE
            )
        }

        # simulation enrichment #7: selectedPopulations
        if (enrichmentEnabled && outputSize %in% c("medium", "large")) {
            .addSimulationArrayToSummary(
                designPlan = designPlan,
                parameterName = "selectedPopulations",
                parameterCaption = "Selected populations",
                summaryFactory = summaryFactory,
                digitsSampleSize = digitsProbabilities, smoothedZeroFormat = TRUE
            )
        }

        # simulation multi-arm #8: numberOfActiveArms
        if (multiArmEnabled && outputSize %in% c("medium", "large")) {
            summaryFactory$addParameter(designPlan,
                parameterName = "numberOfActiveArms",
                parameterCaption = "Number of active arms",
                roundDigits = digitsGeneral, transpose = TRUE
            )
        }

        # simulation enrichment #8: numberOfPopulations
        if (enrichmentEnabled && outputSize %in% c("medium", "large")) {
            summaryFactory$addParameter(designPlan,
                parameterName = "numberOfPopulations",
                parameterCaption = "Number of populations",
                roundDigits = digitsGeneral, transpose = TRUE
            )
        }

        if (outputSize == "large") {
            summaryFactory$addParameter(designPlan,
                parameterName = "conditionalPowerAchieved",
                parameterCaption = "Conditional power (achieved)",
                roundDigits = digitsProbabilities, transpose = TRUE
            )
        }
    }

    if (baseEnabled) {
        parameterName <- "rejectPerStage"
        if (design$kMax == 1) {
            parameterName <- "overallReject"
        }
        if (any(!is.na(designPlan[[parameterName]]))) {
            summaryFactory$addParameter(designPlan,
                parameterName = parameterName,
                parameterCaption = ifelse(design$kMax == 1, "Power", "Overall power"),
                roundDigits = digitsProbabilities, cumsumEnabled = TRUE, smoothedZeroFormat = TRUE
            )
        }

        if (inherits(designPlan, "SimulationResults")) {
            parameterNameSubjects <- ifelse(survivalEnabled, "numberOfSubjects", "sampleSizes")
            parameterNameEvents <- "cumulativeEventsPerStage"
        } else {
            if (design$kMax == 1 && (
                    designPlan$.isSampleSizeObject() ||
                        .isTrialDesignPlanMeans(designPlan) ||
                        .isTrialDesignPlanRates(designPlan) ||
                        countDataEnabled
                )) {
                parameterNameSubjects <- "nFixed"
                parameterNameEvents <- "eventsFixed"
            } else if (countDataEnabled) {
                parameterNameSubjects <- "numberOfSubjects"
            } else if (design$kMax == 1 && designPlan$.isPowerObject()) {
                parameterNameSubjects <- "expectedNumberOfSubjects"
                parameterNameEvents <- "expectedNumberOfEvents"
            } else {
                parameterNameSubjects <- "numberOfSubjects"
                parameterNameEvents <- "cumulativeEventsPerStage"
            }
        }

        if (countDataEnabled && !is.null(designPlan[["calendarTime"]]) &&
                designPlan$.getParameterType("calendarTime") == C_PARAM_GENERATED) {
            summaryFactory$addParameter(designPlan,
                parameterName = "calendarTime",
                parameterCaption = "Calendar time",
                roundDigits = digitsGeneral
            )
        }

        if (countDataEnabled && !is.null(designPlan[["expectedStudyDurationH1"]]) &&
                designPlan$.getParameterType("expectedStudyDurationH1") == C_PARAM_GENERATED) {
            summaryFactory$addParameter(designPlan,
                parameterName = "expectedStudyDurationH1",
                parameterCaption = "Expected study duration under H1",
                roundDigits = digitsGeneral,
                transpose = TRUE
            )
        }

        if (countDataEnabled && !is.null(designPlan[["studyTime"]]) &&
                designPlan$.getParameterType("studyTime") == C_PARAM_GENERATED) {
            summaryFactory$addParameter(designPlan,
                parameterName = "studyTime",
                parameterCaption = "Study time",
                roundDigits = digitsGeneral
            )
        }


        if (outputSize %in% c("medium", "large")) {
            subjectsCaption <- ifelse(design$kMax > 1 && inherits(designPlan, "SimulationResults") &&
                !survivalEnabled, "Stagewise number of subjects", "Number of subjects")
            summaryFactory$addParameter(designPlan,
                parameterName = parameterNameSubjects,
                parameterCaption = subjectsCaption, roundDigits = digitsSampleSize,
                validateParameterType = !countDataEnabled
            )
        }

        if (design$kMax > 1) {
            summaryFactory$addParameter(designPlan,
                parameterName = ifelse(inherits(designPlan, "TrialDesignPlan") &&
                    (designPlan$.isSampleSizeObject() || countDataEnabled),
                "expectedNumberOfSubjectsH1", "expectedNumberOfSubjects"
                ),
                parameterCaption = "Expected number of subjects under H1",
                roundDigits = digitsSampleSize, transpose = TRUE,
                validateParameterType = !countDataEnabled
            )
        }

        if (countDataEnabled && design$kMax > 1 &&
                !is.null(designPlan[["maxNumberOfSubjects"]]) &&
                designPlan$.getParameterType("maxNumberOfSubjects") == C_PARAM_GENERATED) {
            summaryFactory$addParameter(designPlan,
                parameterName = "maxNumberOfSubjects",
                parameterCaption = "Maximum number of subjects",
                roundDigits = digitsSampleSize
            )
        }

        if (countDataEnabled && !is.null(designPlan[["lambda1"]]) &&
                designPlan$.getParameterType("lambda1") == C_PARAM_GENERATED) {
            summaryFactory$addParameter(designPlan,
                parameterName = "lambda1",
                parameterCaption = "Lambda(1)",
                roundDigits = digitsGeneral
            )
        }

        if (countDataEnabled && !is.null(designPlan[["lambda2"]]) &&
                designPlan$.getParameterType("lambda2") == C_PARAM_GENERATED) {
            summaryFactory$addParameter(designPlan,
                parameterName = "lambda2",
                parameterCaption = "Lambda(2)",
                roundDigits = digitsGeneral
            )
        }

        if (countDataEnabled && design$kMax > 1 && outputSize %in% c("medium", "large") &&
                designPlan$.isSampleSizeObject()) {
            if (outputSize == "large") {
                summaryFactory$addParameter(designPlan,
                    parameterName = "informationOverStages",
                    parameterCaption = "Information over stages",
                    roundDigits = digitsSampleSize
                )
            }

            summaryFactory$addParameter(designPlan,
                parameterName = "expectedInformationH0",
                parameterCaption = "Expected information under H0",
                roundDigits = digitsSampleSize,
                transpose = TRUE
            )
            summaryFactory$addParameter(designPlan,
                parameterName = "expectedInformationH01",
                parameterCaption = "Expected information under H0/H1",
                roundDigits = digitsSampleSize,
                transpose = TRUE
            )
            summaryFactory$addParameter(designPlan,
                parameterName = "expectedInformationH1",
                parameterCaption = "Expected information under H1",
                roundDigits = digitsSampleSize,
                transpose = TRUE
            )
        }

        if (countDataEnabled && designPlan$.isSampleSizeObject()) {
            summaryFactory$addParameter(designPlan,
                parameterName = "maxInformation",
                parameterCaption = "Maximum information",
                roundDigits = digitsSampleSize,
                transpose = TRUE
            )
        }

        if (survivalEnabled) {
            if (design$kMax > 1 && !(inherits(designPlan, "TrialDesignPlanSurvival") &&
                    designPlan$.isSampleSizeObject())) {
                summaryFactory$addParameter(designPlan,
                    parameterName = "expectedNumberOfEvents",
                    parameterCaption = "Expected number of events",
                    roundDigits = digitsSampleSize, transpose = TRUE
                )
            }

            if (outputSize %in% c("medium", "large")) {
                summaryFactory$addParameter(designPlan,
                    parameterName = parameterNameEvents,
                    parameterCaption = ifelse(design$kMax == 1,
                        "Number of events", "Cumulative number of events"
                    ),
                    roundDigits = digitsSampleSize, cumsumEnabled = FALSE
                )
            }

            if (outputSize == "large") {
                summaryFactory$addParameter(designPlan,
                    parameterName = "analysisTime",
                    parameterCaption = "Analysis time", roundDigits = digitsGeneral
                )
            }

            summaryFactory$addParameter(designPlan,
                parameterName = "studyDuration",
                parameterCaption = "Expected study duration",
                roundDigits = digitsSampleSize, smoothedZeroFormat = TRUE, transpose = TRUE
            )
        }
    }

    if (!is.null(designPlan[["allocationRatioPlanned"]]) &&
            length(unique(designPlan$allocationRatioPlanned)) > 1) {
        summaryFactory$addParameter(designPlan,
            parameterName = "allocationRatioPlanned",
            parameterCaption = "Optimum allocation ratio", roundDigits = digitsGeneral
        )
    }

    .addDesignParameterToSummary(
        design, designPlan, designCharacteristics,
        summaryFactory, digitsGeneral, digitsProbabilities
    )

    if (baseEnabled && !planningEnabled && !is.null(designPlan[["futilityPerStage"]]) &&
            !any(is.na(designPlan[["futilityPerStage"]])) &&
            any(designPlan$futilityPerStage != 0) && any(designPlan$futilityPerStage > 1e-08)) {
        summaryFactory$addParameter(designPlan,
            parameterName = "futilityPerStage",
            parameterCaption = "Exit probability for futility", # (under H1)
            roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE
        )
    }

    if (baseEnabled && simulationEnabled && design$kMax > 1) {
        values <- NULL
        if (!is.null(probsH1)) {
            values <- probsH1$rejectPerStage
        }
        summaryFactory$addParameter(designPlan,
            parameterName = "rejectPerStage",
            values = values,
            parameterCaption = "Exit probability for efficacy",
            roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE
        )
    }

    # sample size and power only
    if (planningEnabled) {
        if (!countDataEnabled) {
            legendEntry <- list("(t)" = "treatment effect scale")

            if (ncol(designPlan$criticalValuesEffectScale) > 0) {
                summaryFactory$addParameter(designPlan,
                    parameterName = "criticalValuesEffectScale",
                    parameterCaption = ifelse(.isDelayedInformationEnabled(design = design),
                        "Upper bounds of continuation (t)", "Efficacy boundary (t)"
                    ),
                    roundDigits = digitsGeneral, legendEntry = legendEntry
                )
            } else if (ncol(designPlan$criticalValuesEffectScaleUpper) > 0) {
                summaryFactory$addParameter(designPlan,
                    parameterName = "criticalValuesEffectScaleLower",
                    parameterCaption = "Lower efficacy boundary (t)",
                    roundDigits = digitsGeneral, legendEntry = legendEntry
                )
                summaryFactory$addParameter(designPlan,
                    parameterName = "criticalValuesEffectScaleUpper",
                    parameterCaption = "Upper efficacy boundary (t)",
                    roundDigits = digitsGeneral, legendEntry = legendEntry
                )
            }

            if (ncol(designPlan$futilityBoundsEffectScale) > 0 &&
                    !all(is.na(designPlan$futilityBoundsEffectScale))) {
                summaryFactory$addParameter(designPlan,
                    parameterName = "futilityBoundsEffectScale",
                    parameterCaption = ifelse(.isDelayedInformationEnabled(design = design),
                        "Lower bounds of continuation (t)", "Futility boundary (t)"
                    ),
                    roundDigits = digitsGeneral, legendEntry = legendEntry
                )
            } else if (ncol(designPlan$futilityBoundsEffectScaleUpper) > 0 &&
                    (any(!is.na(designPlan$futilityBoundsEffectScaleLower)) ||
                        any(!is.na(designPlan$futilityBoundsEffectScaleUpper)))) {
                summaryFactory$addParameter(designPlan,
                    parameterName = "futilityBoundsEffectScaleLower",
                    parameterCaption = "Lower futility boundary (t)",
                    roundDigits = digitsGeneral, legendEntry = legendEntry
                )
                summaryFactory$addParameter(designPlan,
                    parameterName = "futilityBoundsEffectScaleUpper",
                    parameterCaption = "Upper futility boundary (t)",
                    roundDigits = digitsGeneral, legendEntry = legendEntry
                )
            }
        }

        if (!is.null(probsH1) && !is.null(probsH0) && design$kMax > 1) {
            probsH0$earlyStop <- matrix(probsH0$earlyStop[1:(design$kMax - 1), 1], ncol = 1)
            probsH0$rejectPerStage <- matrix(probsH0$rejectPerStage[1:(design$kMax - 1), 1], ncol = 1)

            if (is.matrix(probsH1$rejectPerStage)) {
                if (design$kMax > 1 && designPlan$.isSampleSizeObject()) {
                    probsH1$rejectPerStage <- probsH1$rejectPerStage[1:(design$kMax - 1), 1]
                } else {
                    probsH1$rejectPerStage <- matrix(probsH1$rejectPerStage[1:(design$kMax - 1), ],
                        ncol = ncol(probsH1$rejectPerStage)
                    )
                }
            } else {
                probsH1$rejectPerStage <- probsH1$rejectPerStage[1:(design$kMax - 1)]
            }

            if (any(design$futilityBounds > -6)) {
                if (is.matrix(probsH1$earlyStop)) {
                    probsH1$earlyStop <- matrix(probsH1$earlyStop[1:(design$kMax - 1), ],
                        ncol = ncol(probsH1$earlyStop)
                    )
                } else {
                    probsH1$earlyStop <- probsH1$earlyStop[1:(design$kMax - 1)]
                }
                summaryFactory$addParameter(probsH0,
                    parameterName = "earlyStop",
                    parameterCaption = "Overall exit probability (under H0)",
                    roundDigits = digitsProbabilities,
                    smoothedZeroFormat = TRUE
                )
                x <- designPlan
                if (is.null(x)) {
                    x <- design
                }
                summaryFactory$addParameter(x,
                    parameterName = "earlyStop",
                    values = probsH1$earlyStop,
                    parameterCaption = "Overall exit probability (under H1)",
                    roundDigits = digitsProbabilities,
                    smoothedZeroFormat = TRUE
                )
            }
            summaryFactory$addParameter(probsH0,
                parameterName = "rejectPerStage",
                parameterCaption = "Exit probability for efficacy (under H0)",
                roundDigits = digitsProbabilities,
                smoothedZeroFormat = TRUE
            )
            if (designPlan$.isPowerObject()) {
                summaryFactory$addParameter(designPlan,
                    parameterName = "rejectPerStage",
                    values = probsH1$rejectPerStage,
                    parameterCaption = "Exit probability for efficacy (under H1)",
                    roundDigits = digitsProbabilities,
                    smoothedZeroFormat = TRUE
                )
            } else {
                summaryFactory$addParameter(probsH1,
                    parameterName = "rejectPerStage",
                    parameterCaption = "Exit probability for efficacy (under H1)",
                    roundDigits = digitsProbabilities,
                    smoothedZeroFormat = TRUE
                )
            }

            if (any(design$futilityBounds > -6)) {
                summaryFactory$addParameter(probsH0,
                    parameterName = "futilityPerStage",
                    parameterCaption = "Exit probability for futility (under H0)",
                    roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE
                )
                x <- designPlan
                if (is.null(x)) {
                    x <- design
                }
                futilityPerStage <- probsH1$futilityPerStage
                if (.isTrialDesignPlan(x) && x$.isSampleSizeObject() && ncol(futilityPerStage) > 1) {
                    futilityPerStage <- futilityPerStage[, 1]
                }
                summaryFactory$addParameter(x,
                    parameterName = "futilityPerStage",
                    values = futilityPerStage,
                    parameterCaption = "Exit probability for futility (under H1)",
                    roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE
                )
            }
        }
    }

    if (!is.null(performanceScore)) {
        print(performanceScore)
        summaryFactory$addParameter(performanceScore,
            parameterName = "performanceScore",
            parameterCaption = "Performance score",
            roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE
        )
    }

    return(summaryFactory)
}

.getSummaryVariedParameterNameEnrichment <- function(designPlan) {
    if (grepl("Rates", .getClassName(designPlan))) {
        return("piTreatments")
    }
    if (grepl("Survival", .getClassName(designPlan))) {
        return("hazardRatios")
    }
    return("effects")
}

.getSummaryGroup <- function(parameterCaption,
        numberOfVariedParams,
        variedParamNumber,
        designPlan) {
    if (numberOfVariedParams <= 1) {
        return(list(
            groupCaption = parameterCaption,
            legendEntry = list()
        ))
    }

    enrichmentEnabled <- grepl("SimulationResultsEnrichment", .getClassName(designPlan))
    if (enrichmentEnabled) {
        variedParameterName <- .getSummaryVariedParameterNameEnrichment(designPlan)
        variedParameterValues <- designPlan$effectList[[variedParameterName]]
        if (variedParameterName == "piTreatments") {
            variedParameterCaption <- "pi(treatment)"
        } else {
            variedParameterCaption <- .getParameterCaption(variedParameterName)
        }
        if (is.matrix(variedParameterValues) && ncol(variedParameterValues) == 1) {
            variedParameterCaption <- sub("s$", "", variedParameterCaption)
        }
    } else {
        variedParameterName <- .getVariedParameterSimulationMultiArm(designPlan)
        variedParameterValues <- designPlan[[variedParameterName]]
        variedParameterCaption <- .getParameterCaption(variedParameterName)
    }

    userDefinedEffectMatrix <- !enrichmentEnabled &&
        designPlan$.getParameterType("effectMatrix") == C_PARAM_USER_DEFINED

    if (userDefinedEffectMatrix) {
        return(list(
            groupCaption = paste0(parameterCaption, " [", variedParamNumber, "]"),
            legendEntry = list("[j]" = "effect matrix row j (situation to consider)")
        ))
    }
    if (is.matrix(variedParameterValues)) {
        values <- variedParameterValues[variedParamNumber, ]
        if (length(values) > 1) {
            values <- .arrayToString(values, vectorLookAndFeelEnabled = TRUE)
        }
    } else {
        values <- variedParameterValues[variedParamNumber]
    }
    if (is.numeric(values)) {
        values <- round(values, 2)
    }
    return(list(
        groupCaption = paste0(
            parameterCaption, ", ",
            tolower(variedParameterCaption), " = ", values
        ),
        legendEntry = list()
    ))
}

.getSummaryGroupCaption <- function(designPlan, parameterName, numberOfGroups, groupNumber) {
    listItemPrefix <- getOption("rpact.summary.list.item.prefix", C_SUMMARY_LIST_ITEM_PREFIX_DEFAULT)

    if (grepl("Enrichment", .getClassName(designPlan))) {
        categoryCaption <- .getCategoryCaptionEnrichment(designPlan, parameterName, groupNumber)
        categoryCaption <- sub("^F$", "Full population F", categoryCaption)
        categoryCaption <- sub("^R$", "Remaining population R", categoryCaption)
        categoryCaption <- sub("^S", "Subset S", categoryCaption)

        return(paste0(listItemPrefix, categoryCaption))
    }

    treatmentCaption <- ifelse(numberOfGroups > 2, paste0("Treatment arm ", groupNumber), "Treatment arm")

    if (!grepl("Survival", .getClassName(designPlan)) ||
            (inherits(designPlan, "SimulationResultsMultiArmSurvival") &&
                parameterName == "singleEventsPerArmAndStage")) {
        return(ifelse(groupNumber == numberOfGroups,
            paste0(listItemPrefix, "Control arm"),
            paste0(listItemPrefix, treatmentCaption)
        ))
    }

    return(paste0(listItemPrefix, treatmentCaption, " vs. control"))
}

.addSimulationArrayToSummary <- function(designPlan,
        parameterName, parameterCaption, summaryFactory,
        digitsSampleSize, smoothedZeroFormat = FALSE) {
    arrayData <- designPlan[[parameterName]]
    if (is.null(arrayData)) {
        stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, class(designPlan)[1], " does not contain the field ", sQuote(parameterName))
    }

    numberOfVariedParams <- dim(arrayData)[2]
    numberOfGroups <- dim(arrayData)[3]

    for (variedParamNumber in 1:numberOfVariedParams) {
        summaryGroup <- .getSummaryGroup(
            parameterCaption,
            numberOfVariedParams,
            variedParamNumber,
            designPlan
        )
        groupCaption <- summaryGroup$groupCaption
        legendEntry <- summaryGroup$legendEntry
        if (numberOfGroups > 1) {
            summaryFactory$addItem(groupCaption, "", legendEntry = legendEntry)
        }

        for (groupNumber in 1:numberOfGroups) {
            dataPerGroupAndStage <- arrayData[, variedParamNumber, groupNumber]
            if (numberOfGroups > 1) {
                groupCaption <- .getSummaryGroupCaption(
                    designPlan,
                    parameterName, numberOfGroups, groupNumber
                )
            }
            summaryFactory$addParameter(designPlan,
                parameterName = parameterName,
                values = dataPerGroupAndStage, parameterCaption = groupCaption,
                roundDigits = digitsSampleSize,
                smoothedZeroFormat = smoothedZeroFormat,
                enforceFirstCase = TRUE
            )
        }
    }
}

.addSimulationMultiArmArrayParameter <- function(designPlan, parameterName, parameterCaption,
        summaryFactory, roundDigits, smoothedZeroFormat = FALSE) {
    arrayData <- designPlan[[parameterName]]
    if (is.array(arrayData) && length(dim(arrayData)) == 3) {
        totalNumberOfGroups <- dim(designPlan[[ifelse(grepl("Survival", .getClassName(designPlan)),
            "cumulativeEventsPerStage", "sampleSizes"
        )]])[3]

        numberOfGroups <- dim(arrayData)[3]
        if (parameterName == "selectedArms" && !grepl("Survival", .getClassName(designPlan))) {
            numberOfGroups <- numberOfGroups - 1 # remove control group
        }
        numberOfVariedParams <- dim(arrayData)[2]

        for (variedParamNumber in 1:numberOfVariedParams) {
            summaryGroup <- .getSummaryGroup(
                parameterCaption,
                numberOfVariedParams,
                variedParamNumber,
                designPlan
            )
            groupCaption <- summaryGroup$groupCaption
            legendEntry <- summaryGroup$legendEntry
            if (numberOfGroups > 1) {
                summaryFactory$addItem(groupCaption, "", legendEntry = legendEntry)
            }

            for (groupNumber in 1:numberOfGroups) {
                dataPerGroupAndStage <- arrayData[, variedParamNumber, groupNumber]
                if (numberOfGroups > 1) {
                    groupCaption <- .getSummaryGroupCaption(
                        designPlan,
                        parameterName, totalNumberOfGroups, groupNumber
                    )
                }
                summaryFactory$addParameter(designPlan,
                    parameterName = parameterName,
                    values = dataPerGroupAndStage, parameterCaption = groupCaption,
                    roundDigits = roundDigits, smoothedZeroFormat = smoothedZeroFormat,
                    enforceFirstCase = TRUE
                )
            }
        }
    } else {
        data <- designPlan[[parameterName]]
        numberOfGroups <- ncol(data)
        for (groupNumber in 1:numberOfGroups) {
            dataPerGroupAndStage <- data[, groupNumber]
            summaryFactory$addParameter(designPlan,
                parameterName = parameterName,
                values = dataPerGroupAndStage,
                parameterCaption = ifelse(groupNumber == numberOfGroups,
                    paste0(parameterCaption, ", control"),
                    paste0(parameterCaption, ", treatment ", groupNumber)
                ),
                roundDigits = roundDigits, smoothedZeroFormat = smoothedZeroFormat
            )
        }
    }
}
