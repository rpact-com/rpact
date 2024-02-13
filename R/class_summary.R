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
## |  File version: $Revision: 7408 $
## |  Last changed: $Date: 2023-11-09 10:36:19 +0100 (Do, 09 Nov 2023) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_core_utilities.R
#' @include f_core_assertions.R
NULL


SummaryItem <- setRefClass("SummaryItem",
    fields = list(
        title = "character",
        values = "character",
        legendEntry = "list"
    ),
    methods = list(
        initialize = function(title = NA_character_, values = NA_character_, ...) {
            callSuper(title = title, values = values, ...)
            if (!is.null(legendEntry) && length(legendEntry) > 0) {
                if (is.null(names(legendEntry))) {
                    stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, sQuote("legendEntry"), " must be a named list")
                }
                for (l in legendEntry) {
                    if (length(l) == 0) {
                        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, sQuote("legendEntry"), " must be not empty")
                    }
                }
            }
        },
        show = function() {
            cat(title, "=", values, "\n")
        },
        toList = function() {
            result <- list()
            result[[title]] <- values
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
            if (.isQuartoEnabled()) {
                #cat("#| results: 'asis'\n\n")
            }
      
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
        result <- paste0(result, sep, 
            paste0(utils::capture.output(x$object$.catMarkdownText()), collapse = "\n"))
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
SummaryFactory <- setRefClass("SummaryFactory",
    contains = "ParameterSet",
    fields = list(
        object = "ParameterSet",
        title = "character",
        header = "character",
        summaryItems = "list",
        intervalFormat = "character",
        justify = "character",
        output = "character"
    ),
    methods = list(
        initialize = function(..., intervalFormat = "[%s; %s]", output = "all") {
            callSuper(..., intervalFormat = intervalFormat, output = output)
            summaryItems <<- list()
            justify <<- getOption("rpact.summary.justify", "right")
        },
        show = function(showType = 1, digits = NA_integer_) {
            .show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
        },
        .show = function(showType = 1, digits = NA_integer_, ..., consoleOutputEnabled = TRUE) {
            if (output %in% c("all", "title")) {
                if (is.null(title) || length(title) == 0) {
                    title <<- .createSummaryTitleObject(object)
                }
                if (!is.null(title) && length(title) == 1 && trimws(title) != "") {
                    .cat(title, "\n\n",
                        heading = 1,
                        consoleOutputEnabled = consoleOutputEnabled
                    )
                }
            }

            if (output %in% c("all", "overview")) {
                if (is.null(header) || length(header) == 0) {
                    header <<- .createSummaryHeaderObject(object, .self, digits)
                }
                if (!is.null(header) && length(header) == 1 && trimws(header) != "") {
                    .cat(header, "\n\n",
                        consoleOutputEnabled = consoleOutputEnabled
                    )
                }
            }

            if (!(output %in% c("all", "body"))) {
                return(invisible())
            }

            legendEntries <- c()
            legendEntriesUnique <- c()
            summaryItemNames <- c()
            for (summaryItem in summaryItems) {
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

            na <- ifelse(.isDataset(object), "NA", NA_character_)
            tableColumns <- 0
            maxValueWidth <- 1
            if (length(summaryItems) > 0) {
                for (i in 1:length(summaryItems)) {
                    validValues <- na.omit(summaryItems[[i]]$values)
                    if (length(validValues) > 0) {
                        w <- max(nchar(validValues))
                        maxValueWidth <- max(maxValueWidth, w)
                        tableColumns <- max(tableColumns, 1 + length(validValues))
                    }
                }
                spaceString <- paste0(rep(" ", maxValueWidth + 1), collapse = "")
                for (i in 1:length(summaryItems)) {
                    itemTitle <- summaryItems[[i]]$title
                    if (!is.null(itemTitle) && length(itemTitle) == 1 && !is.na(itemTitle)) {
                        summaryItemName <- summaryItemNames[i]
                        values <- summaryItems[[i]]$values
                        values <- trimws(values)
                        indices <- !grepl("(\\])$", values)
                        values[indices] <- paste0(values[indices], " ")
                        values <- format(c(spaceString, values), justify = justify)[2:(length(values) + 1)]
                        .cat(summaryItemName, values, "\n",
                            tableColumns = tableColumns,
                            consoleOutputEnabled = consoleOutputEnabled, na = na
                        )
                        if (!consoleOutputEnabled && trimws(summaryItemName) == "Stage") {
                            .cat(rep("----- ", tableColumns), "\n",
                                tableColumns = tableColumns,
                                consoleOutputEnabled = consoleOutputEnabled, na = na
                            )
                        }
                    }
                }
            }

            if (length(legendEntries) > 0) {
                .cat("\n", consoleOutputEnabled = consoleOutputEnabled)
                .cat("Legend:\n", consoleOutputEnabled = consoleOutputEnabled)
                if (!consoleOutputEnabled) {
                    .cat("\n", consoleOutputEnabled = consoleOutputEnabled)
                }
                for (legendEntry in legendEntries) {
                    .cat(legendEntry, "\n", consoleOutputEnabled = consoleOutputEnabled)
                }
                .cat("\n", consoleOutputEnabled = consoleOutputEnabled)
            }
        },
        addItem = function(title, values, legendEntry = list()) {
            if (!is.character(values)) {
                values <- as.character(values)
            }
            tryCatch(
                {
                    addSummaryItem(SummaryItem(title = title, values = values, legendEntry = legendEntry))
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
            summaryItems <<- c(summaryItems, summaryItem)
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
                        valuesToShow[variantIndex] <- sprintf(intervalFormat, value1, value2)
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
                enforceFirstCase = FALSE, formatRepeatedPValues = FALSE) {
            if (!is.null(parameterName) && length(parameterName) == 1 &&
                    inherits(parameterSet, "ParameterSet") &&
                    parameterSet$.getParameterType(parameterName) == C_PARAM_NOT_APPLICABLE) {
                if (.getLogicalEnvironmentVariable("RPACT_DEVELOPMENT_MODE")) {
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
                            "futilityPerStage"
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
                    valuesToShow <- .getInnerValues(valuesToShow, transpose = TRUE)
                } else {
                    valuesToShow <- .getInnerValues(valuesToShow, transpose = transpose)
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
                    valuesToShow2 <- .getInnerValues(valuesToShow2, transpose = transpose)
                }

                valuesToShow <- .getFormattedParameterValue(valuesToShow, valuesToShow2)
                addItem(parameterCaptionSingle, valuesToShow, legendEntry)
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
                                "singleNumberOfEventsPerStage",
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
                                variedParameterCaption <- C_PARAMETER_NAMES[[variedParameterName]]
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
                            variedParameterCaption <- C_PARAMETER_NAMES[[variedParameterName]]
                            numberOfVariants <- length(variedParameterValues)
                        }
                        variedParameterCaption <- tolower(variedParameterCaption)
                    } else if (.isEnrichmentObject(parameterSet)) {
                        transposed <- TRUE
                        variedParameterCaption <- "populations"
                        if (parameterName1 %in% c(
                                "indices", "conditionalErrorRate", "secondStagePValues",
                                "adjustedStageWisePValues", "overallAdjustedTestStatistics", "rejectedIntersections"
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
                    } else if (inherits(parameterSet, "PerformanceScore") || inherits(parameterSet, "PerformanceScoreR6")) {
                        variedParameter <- ".alternative"
                    } else {
                        variedParameter <- parameterSet$.getVariedParameter(parameterNames, numberOfVariants)
                    }
                    if (length(variedParameter) == 0 || variedParameter == "") {
                        warning(
                            "Failed to get varied parameter from ", .getClassName(parameterSet),
                            " (", length(parameterNames), " parameter names; numberOfVariants: ", numberOfVariants, ")"
                        )
                        return(invisible())
                    }

                    variedParameterCaption <- parameterSet$.getDataFrameColumnCaption(variedParameter,
                        tableColumnNames = C_TABLE_COLUMN_NAMES, niceColumnNamesEnabled = TRUE
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
                    colValues <- .getColumnValues(parameterName, values, variantIndex, transposed)
                    colValues <- .getSummaryValuesFormatted(parameterSet, parameterName1,
                        colValues,
                        roundDigits = roundDigits,
                        ceilingEnabled = ceilingEnabled, cumsumEnabled = cumsumEnabled,
                        smoothedZeroFormat = smoothedZeroFormat,
                        formatRepeatedPValues = formatRepeatedPValues
                    )
                    colValues2 <- NA_real_
                    if (!all(is.na(values2))) {
                        colValues2 <- .getColumnValues(parameterName, values2, variantIndex, transposed)
                        colValues2 <- .getSummaryValuesFormatted(parameterSet, parameterName2, colValues2,
                            roundDigits = roundDigits, ceilingEnabled = ceilingEnabled,
                            cumsumEnabled = cumsumEnabled,
                            smoothedZeroFormat = smoothedZeroFormat,
                            formatRepeatedPValues = formatRepeatedPValues
                        )
                    }
                    colValues <- .getFormattedParameterValue(valuesToShow = colValues, valuesToShow2 = colValues2)

                    if (numberOfVariants == 1) {
                        addItem(parameterCaption, colValues, legendEntry)
                    } else if (.isEnrichmentObject(parameterSet)) {
                        addItem(paste0(
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
                        addItem(paste0(
                            parameterCaption, spacePrefix,
                            "(", variedParameterValues[variantIndex], ")"
                        ), colValues, legendEntry)
                    } else if (userDefinedEffectMatrix) {
                        addItem(paste0(parameterCaption, " [", variantIndex, "]"), colValues, legendEntry)
                    } else {
                        if (is.matrix(variedParameterValues) && ncol(variedParameterValues) > 1) {
                            variedParameterValuesFormatted <-
                                .arrayToString(variedParameterValues[variantIndex, ], vectorLookAndFeelEnabled = TRUE)
                        } else {
                            variedParameterValuesFormatted <- variedParameterValues[variantIndex]
                        }
                        addItem(
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
