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
        initialize = function(title = NA_character_, values = NA_character_, legendEntry = NULL, ...) {
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
                    stopRuntimeIssue("failed to add summary item ", .pQuote(title), " = ", .arrayToString(values),
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
            naText <- .getEnvironmentVariable("RPACT_SUMMARY_NA", "rpact.summary.na", default = "", type = "character")
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
                        valuesToShow[variantIndex] <- sprintf(self$intervalFormat, value1, value2)
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
            if (!is.null(parameterName) && length(parameterName) == 1 &&
                    inherits(parameterSet, "ParameterSet") &&
                    parameterSet$isNotApplicableParameter(parameterName)) {
                if (!is.null(values) && .getLogicalEnvironmentVariable("RPACT_DEVELOPMENT_MODE") &&
                        validateParameterType && !.isMarkdownEnabled()) {
                    warning(
                        "Failed to add parameter ", .arrayToString(parameterName), " (",
                        .arrayToString(values), ") stored in ",
                        .getClassName(parameterSet), " because the ",
                        "parameter has type C_PARAM_NOT_APPLICABLE"
                    )
                }

                return(invisible())
            }

            parameterName1 <- parameterName[1]
            if (!is.null(parameterName1) && is.character(parameterName1) && is.null(values)) {
                values <- parameterSet[[parameterName1]]
                if (is.null(values)) {
                    stopRuntimeIssue(.getClassName(parameterSet), " does not ",
                        "contain a field ", .pQuote(parameterName1), "",
                        functionName = "addParameter",
                        parameter = "parameterSet",
                        value = parameterSet,
                        relatedParameter = "parameterName1",
                        relatedValue = parameterName1
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
                    stopRuntimeIssue(.getClassName(parameterSet), " does not ",
                        "contain a field ", .pQuote(parameterName2), "",
                        functionName = "addParameter",
                        parameter = "parameterSet",
                        value = parameterSet,
                        relatedParameter = "parameterName2",
                        relatedValue = parameterName2
                    )
                }
            }

            if (is.null(values) && is.null(parameterName1)) {
                stopRuntimeIssue("'parameterName' or 'values' must be defined",
                    functionName = "addParameter",
                    parameter = "parameterName",
                    value = parameterName,
                    relatedParameter = "values",
                    relatedValue = values
                )
            }

            transposed <- NA
            if (transpose) {
                if (!is.matrix(values)) {
                    values <- as.matrix(values)
                    if (!is.na(lastStage) && lastStage > ncol(values)) {
                        nCol <- lastStage - ncol(values)
                        values <- cbind(matrix(rep(NA_real_, nCol * nrow(values)), nrow = nrow(values)), values)
                        if (!is.null(parameterName) && length(parameterName) == 1 &&
                                parameterName %in% c(
                                    "expectedNumberOfSubjects",
                                    "expectedNumberOfSubjectsH1",
                                    "expectedEventsH1",
                                    "expectedNumberOfEvents",
                                    "expectedStudyDurationH1",
                                    "expectedInformationH0",
                                    "expectedInformationH01",
                                    "expectedInformationH1",
                                    "studyDuration",
                                    "earlyStop"
                                )) {
                            transposed <- TRUE
                        }
                    }
                } else {
                    values <- t(values)
                }
            }

            if (is.list(parameterSet) && is.matrix(values)) {
                parameterSet <- parameterSet[["parameterSet"]]
                if (is.null(parameterSet)) {
                    stopRuntimeIssue("'parameterSet' must be added to list",
                        functionName = "addParameter",
                        parameter = "parameterSet", value = parameterSet
                    )
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
                        and5 = !is.null(parameterName) && length(parameterName) == 1 &&
                            parameterName %in% c(
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
                    parameterSet,
                    parameterName1,
                    values,
                    roundDigits = roundDigits,
                    ceilingEnabled = ceilingEnabled,
                    cumsumEnabled = cumsumEnabled,
                    smoothedZeroFormat = smoothedZeroFormat,
                    formatRepeatedPValues = formatRepeatedPValues,
                    roundDigitsAsInformation = roundDigitsAsInformation,
                    showNA = showNA
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
                        ceilingEnabled = ceilingEnabled,
                        cumsumEnabled = cumsumEnabled,
                        smoothedZeroFormat = smoothedZeroFormat,
                        formatRepeatedPValues = formatRepeatedPValues,
                        roundDigitsAsInformation = roundDigitsAsInformation,
                        showNA = showNA
                    )
                    valuesToShow2 <- self$.getInnerValues(valuesToShow2, transpose = transpose)
                }

                valuesToShow <- self$.getFormattedParameterValue(valuesToShow, valuesToShow2)
                self$addItem(parameterCaptionSingle, valuesToShow, legendEntry)
            } else {
                if (!inherits(parameterSet, "ParameterSet")) {
                    stopIllegalArgument("for varied values 'parameterSet' must be an instance of ",
                        "class 'ParameterSet' (was ", .getClassName(parameterSet, quote = TRUE), ")",
                        functionName = "addParameter",
                        parameter = "parameterSet",
                        value = parameterSet
                    )
                }

                if (is.na(transposed)) {
                    transposed <- !transpose &&
                        grepl("MultiArm|Enrichment", .getClassName(parameterSet)) &&
                        (!is.matrix(values) || ncol(values) > 1)
                }

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
                                "numberOfSelectedArms",
                                "numberOfPopulations",
                                "conditionalPowerAchieved",
                                "plannedCalendarTime"
                            )) {
                        transposed <- TRUE
                        userDefinedEffectMatrix <-
                            parameterSet$isUserDefinedOrDerivedParameter("effectMatrix")
                        if (userDefinedEffectMatrix) {
                            legendEntry[["[j]"]] <- "effect matrix row j (situation to consider)"
                        }
                        if (grepl("Survival", .getClassName(parameterSet)) &&
                                !grepl("Enrichment", .getClassName(parameterSet))) {
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
                        } else if (grepl("SimulationResultsMultiArm", .getClassName(parameterSet))) {
                            variedParameterName <- .getVariedParameterSimulationMultiArm(parameterSet)
                            variedParameterValues <- parameterSet[[variedParameterName]]
                            variedParameterCaption <- .getParameterCaption(variedParameterName)
                            numberOfVariants <- length(variedParameterValues)
                        } else {
                            stopRuntimeIssue("varied parameter identification ", "is not implemented for ", .getClassName(parameterSet),
                                functionName = "addParameter",
                                parameter = "parameterSet", value = parameterSet
                            )
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
                                parameterSet$isNotGeneratedParameter("adjustedStageWisePValues") &&
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
                                " (", length(parameterNames), " parameter names; numberOfVariants: ", numberOfVariants, ";",
                                length(variedParameter), " varied parameter values)"
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
                    colValues <- .getSummaryValuesFormatted(
                        parameterSet,
                        parameterName1,
                        values = colValues,
                        roundDigits = roundDigits,
                        ceilingEnabled = ceilingEnabled, cumsumEnabled = cumsumEnabled,
                        smoothedZeroFormat = smoothedZeroFormat,
                        formatRepeatedPValues = formatRepeatedPValues,
                        roundDigitsAsInformation = roundDigitsAsInformation,
                        showNA = showNA
                    )
                    colValues2 <- NA_real_
                    if (!all(is.na(values2))) {
                        colValues2 <- self$.getColumnValues(parameterName, values2, variantIndex, transposed)
                        colValues2 <- .getSummaryValuesFormatted(
                            parameterSet,
                            parameterName2,
                            values = colValues2,
                            roundDigits = roundDigits,
                            ceilingEnabled = ceilingEnabled,
                            cumsumEnabled = cumsumEnabled,
                            smoothedZeroFormat = smoothedZeroFormat,
                            formatRepeatedPValues = formatRepeatedPValues,
                            roundDigitsAsInformation = roundDigitsAsInformation,
                            showNA = showNA
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
                    stopRuntimeIssue(".getColumnValues(", dQuote(parameterName), "): ", e$message, "; .getClassName(values) = ",
                        .getClassName(values), "; dim(values) = ", .arrayToString(dim(values), vectorLookAndFeelEnabled = TRUE),
                        "; variantIndex = ", variantIndex, "; transposed = ", transposed,
                        parameter = parameterName, value = values,
                        functionName = ".getColumnValues"
                    )
                }
            )
        }
    )
)

