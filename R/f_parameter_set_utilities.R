## |
## |  *Parameter set utilities*
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
NULL

.isMatrix <- function(param) {
    if (missing(param) || is.null(param) || is.list(param)) {
        return(FALSE)
    }

    return(is.matrix(param))
}

.isArray <- function(param) {
    if (missing(param) || is.null(param) || is.list(param)) {
        return(FALSE)
    }

    return(is.array(param))
}

.isVector <- function(param) {
    if (missing(param) || is.null(param) || is.list(param)) {
        return(FALSE)
    }

    return(length(param) > 1)
}

.getMatrixFormatted <- function(paramValueFormatted, enforceListOuput = FALSE) {
    if (length(paramValueFormatted) == 0) {
        return(list(
            paramValueFormatted = matrix(nrow = 0, ncol = 0),
            type = "matrix"
        ))
    }

    if (!is.matrix(paramValueFormatted) && enforceListOuput) {
        paramValueFormatted <- matrix(paramValueFormatted, nrow = 1)
    }

    if (!is.matrix(paramValueFormatted)) {
        return(list(
            paramValueFormatted = matrix(as.character(paramValueFormatted), ncol = 1),
            type = "matrix"
        ))
    }

    matrixFormatted <- paramValueFormatted
    paramValueFormatted <- .arrayToString(matrixFormatted[1, ])
    type <- "vector"
    if (nrow(matrixFormatted) > 1 && ncol(matrixFormatted) > 0) {
        type <- "matrix"
        paramValueFormatted <- list(paramValueFormatted)
        for (i in 2:nrow(matrixFormatted)) {
            paramValueFormatted <- c(
                paramValueFormatted,
                .arrayToString(matrixFormatted[i, ])
            )
        }
    }

    return(list(
        paramValueFormatted = paramValueFormatted,
        type = type
    ))
}

.getParameterValueFormattedByFormatFunctionName <- function(formatFunctionName, paramValue, parameterSet) {
    if (identical(formatFunctionName, ".formatCriticalValues") && .isTrialDesign(parameterSet)) {
        return(.formatCriticalValues(paramValue, design = parameterSet))
    }

    return(eval(call(formatFunctionName, paramValue)))
}

.getDataFrameConsoleLines <- function(dataFrame, digits, columnNameWidth = 14L) {
    if (nrow(dataFrame) == 0L || ncol(dataFrame) == 0L) {
        return(utils::capture.output(print.data.frame(dataFrame, digits = digits)))
    }

    maxRows <- max(1L, floor(getOption("max.print") / ncol(dataFrame)))
    omittedRows <- max(0L, nrow(dataFrame) - maxRows)
    dataFrame <- utils::head(dataFrame, maxRows)
    values <- as.matrix(format(dataFrame, digits = digits, trim = TRUE))
    headers <- lapply(colnames(values), function(caption) {
        return(strwrap(caption, width = columnNameWidth))
    })
    columnWidths <- vapply(seq_len(ncol(values)), function(i) {
        return(max(nchar(c(values[, i], headers[[i]]), type = "width"), na.rm = TRUE))
    }, integer(1))
    rowNames <- rownames(values)
    rowNameWidth <- max(nchar(rowNames, type = "width"))
    lines <- character()
    firstColumn <- 1L
    while (firstColumn <= ncol(values)) {
        lastColumn <- firstColumn
        usedWidth <- rowNameWidth + 1L + columnWidths[firstColumn]
        while (lastColumn < ncol(values) &&
                usedWidth + 1L + columnWidths[lastColumn + 1L] <= getOption("width")) {
            lastColumn <- lastColumn + 1L
            usedWidth <- usedWidth + 1L + columnWidths[lastColumn]
        }
        columns <- seq.int(firstColumn, lastColumn)
        headerHeight <- max(lengths(headers[columns]))
        for (lineIndex in seq_len(headerHeight)) {
            cells <- vapply(columns, function(i) {
                caption <- ""
                if (lineIndex <= length(headers[[i]])) {
                    caption <- headers[[i]][lineIndex]
                }
                return(format(caption, width = columnWidths[i], justify = "right"))
            }, character(1))
            lines <- c(lines, paste(c(strrep(" ", rowNameWidth), cells), collapse = " "))
        }
        for (rowIndex in seq_len(nrow(values))) {
            cells <- vapply(columns, function(i) {
                return(format(values[rowIndex, i], width = columnWidths[i], justify = "right"))
            }, character(1))
            rowName <- format(rowNames[rowIndex], width = rowNameWidth, justify = "right")
            lines <- c(lines, paste(c(rowName, cells), collapse = " "))
        }
        firstColumn <- lastColumn + 1L
        if (firstColumn <= ncol(values)) {
            lines <- c(lines, "")
        }
    }
    if (omittedRows > 0L) {
        lines <- c(lines, paste0("[ reached getOption(\"max.print\") -- omitted ", omittedRows, " rows ]"))
    }
    return(lines)
}

.showDataFrame <- function(
        dataFrame,
        parameterSet,
        niceColumnNamesEnabled = TRUE,
        digits = NA_integer_,
        consoleOutputEnabled = TRUE) {
    if (niceColumnNamesEnabled) {
        columnNames <- names(dataFrame)
        for (i in seq_along(columnNames)) {
            caption <- .getParameterCaption(columnNames[i], parameterSet)
            if (!is.null(caption)) {
                columnNames[i] <- caption
            }
        }
        names(dataFrame) <- columnNames
    }

    if (is.na(digits)) {
        digits <- NULL
    }
    if (!consoleOutputEnabled) {
        values <- as.matrix(format(dataFrame, digits = digits, trim = TRUE))
        .showMatrix(parameterSet, values,
            digits = getOption("digits"), consoleOutputEnabled = FALSE,
            rowLabel = "", escapeMarkdown = TRUE
        )
        return(invisible())
    }
    output <- paste(.getDataFrameConsoleLines(dataFrame, digits = digits), collapse = "\n")
    parameterSet$.cat(output, "\n\n", consoleOutputEnabled = consoleOutputEnabled)
}

.getParameterValueFormatted <- function(obj, parameterName) {
    tryCatch(
        {
            result <- obj$.extractParameterNameAndValue(parameterName)
            parameterName <- result$parameterName
            paramValue <- result$paramValue

            if (.isResultObjectBaseClass(paramValue)) {
                return(NULL)
            }

            if (is.function(paramValue)) {
                valueStr <- ifelse(obj$isUserDefinedParameter(parameterName), "user defined", "default")
                return(list(
                    paramName = parameterName,
                    paramValue = valueStr,
                    paramValueFormatted = valueStr,
                    type = "function"
                ))
            }

            if (is.list(paramValue)) {
                resultList <- list()
                for (listParamName in names(paramValue)) {
                    listParamValue <- paramValue[[listParamName]]
                    type <- "vector"
                    paramValueFormatted <- listParamValue

                    if (.isMatrix(listParamValue)) {
                        m <- .getMatrixFormatted(paramValueFormatted)
                        paramValueFormatted <- m$paramValueFormatted
                        type <- m$type
                    } else if (.isVector(listParamValue)) {
                        paramValueFormatted <- .arrayToString(listParamValue)
                    }

                    entry <- list(
                        paramName = paste0(parameterName, "$", listParamName),
                        paramValue = listParamValue,
                        paramValueFormatted = paramValueFormatted,
                        type = type
                    )
                    resultList[[length(resultList) + 1]] <- entry
                }
                return(resultList)
            }

            paramValueFormatted <- paramValue

            if (obj$.getParameterType(parameterName) %in% c(C_PARAM_USER_DEFINED, C_PARAM_DERIVED, C_PARAM_DEFAULT_VALUE) &&
                    !is.numeric(paramValue)) {
                if (inherits(obj, C_CLASS_NAME_TRIAL_DESIGN_INVERSE_NORMAL) && parameterName == "typeOfDesign") {
                    paramValueFormatted <- C_TYPE_OF_DESIGN_LIST[[paramValue]]
                }
                if (inherits(obj, C_CLASS_NAME_TRIAL_DESIGN_INVERSE_NORMAL) && parameterName == "typeBetaSpending") {
                    paramValueFormatted <- C_TYPE_OF_DESIGN_BS_LIST[[paramValue]]
                }
            } else {
                formatFunctionName <- .getParameterFormatFunction(parameterName, obj)
                if (!is.null(formatFunctionName) && !is.null(paramValueFormatted)) {
                    tryCatch(
                        {
                            paramValueFormatted <- .getParameterValueFormattedByFormatFunctionName(formatFunctionName, paramValueFormatted, obj)
                        },
                        error = function(e) {
                            warnRuntimeIssue("Failed to format value ", sQuote(parameterName), " with function ",
                                formatFunctionName, "(): ", e$message,
                                diagnosticId = "output.parameter_formatting_failed"
                            )
                        }
                    )
                    if (.isArray(paramValue) && length(dim(paramValue)) == 2) {
                        paramValueFormatted <- matrix(paramValueFormatted, ncol = ncol(paramValue))
                    }
                } else if (inherits(obj, C_CLASS_NAME_TRIAL_DESIGN_INVERSE_NORMAL) && parameterName == "typeOfDesign") {
                    paramValueFormatted <- C_TYPE_OF_DESIGN_LIST[[paramValue]]
                } else if (inherits(obj, C_CLASS_NAME_TRIAL_DESIGN_INVERSE_NORMAL) && parameterName == "typeBetaSpending") {
                    paramValueFormatted <- C_TYPE_OF_DESIGN_BS_LIST[[paramValue]]
                }
            }

            type <- "vector"
            if (.isArray(paramValue) && length(dim(paramValue)) == 3) {
                arrayFormatted <- paramValueFormatted
                numberOfEntries <- dim(arrayFormatted)[3]
                numberOfCols <- dim(arrayFormatted)[2]
                numberOfRows <- dim(arrayFormatted)[1]
                enforceListOuput <- numberOfCols > 1
                m <- .getMatrixFormatted(arrayFormatted[, , 1], enforceListOuput = enforceListOuput)
                paramValueFormatted <- m$paramValueFormatted
                type <- m$type
                if (numberOfEntries > 1 && numberOfRows > 0) {
                    type <- "array"
                    for (i in 2:numberOfEntries) {
                        m <- .getMatrixFormatted(arrayFormatted[, , i], enforceListOuput = enforceListOuput)
                        paramValueFormatted <- c(paramValueFormatted, m$paramValueFormatted)
                    }
                }
            } else if (.isMatrix(paramValue) || .isArray(paramValue)) {
                m <- .getMatrixFormatted(paramValueFormatted)
                paramValueFormatted <- m$paramValueFormatted
                type <- m$type
            } else if (.isVector(paramValue)) {
                paramValueFormatted <- .arrayToString(paramValueFormatted)
            } else if (parameterName == "sided") {
                paramValueFormatted <- ifelse(paramValue == 1, "one-sided", "two-sided")
            }

            return(list(
                paramName = parameterName,
                paramValue = paramValue,
                paramValueFormatted = paramValueFormatted,
                type = type
            ))
        },
        error = function(e) {
            .logError(paste0(
                "Error in '.getParameterValueFormatted'. ",
                "Failed to show parameter '%s' (class '%s'): %s"
            ), parameterName, .getClassName(obj), e)
        }
    )

    return(NULL)
}

.getParameterSetParameterCaptionPerCategory <- function(parameterSet, paramName, paramCaption, category, matrixRow, numberOfCategories) {
    if (.isMultiArmSimulationResults(parameterSet) &&
            paramName %in% c("singleEventsPerArmAndStage")) {
        if (!inherits(parameterSet, "SimulationResultsEnrichmentSurvival") &&
                !is.na(numberOfCategories) && 
                numberOfCategories > 2 &&
                numberOfCategories == category &&
                paramName == "singleEventsPerArmAndStage") { # Single number of events
            category <- "control"
        }
        paramCaption <- paste0(paramCaption, " (", category, ")")
    } else if (paramName == "effectList") {
        paramCaption <- paste0(paramCaption, " [", category, "]")
    } else if (.isEnrichmentSimulationResults(parameterSet)) {
        categoryCaption <- .getCategoryCaptionEnrichment(parameterSet, paramName, category)
        paramCaption <- paste0(paramCaption, " (", categoryCaption, ")")
    } else if (paramName %in% c("rejectedArmsPerStage", "selectedArms")) {
        paramCaption <- paste0(paramCaption, " {", category, "}")
    } else {
        paramCaption <- paste0(paramCaption, " (", category, ")")
    }

    if (!is.na(matrixRow)) {
        if (paramName == "effectList") {
            paramCaption <- paste0(paramCaption, " (", matrixRow, ")")
        } else {
            paramCaption <- paste0(paramCaption, " [", matrixRow, "]")
        }
    }
    return(paramCaption)
}

.getParameterSetParameterCaptionPerRow <- function(parameterSet, paramName, paramCaption, matrixRow) {
    if (.isMultiArmAnalysisResults(parameterSet)) {
        return(paste0(paramCaption, " (", matrixRow, ")"))
    }

    if (grepl("StageResultsMultiArm", .getClassName(parameterSet))) {
        return(paste0(paramCaption, " (", matrixRow, ")"))
    }

    if (inherits(parameterSet, "SimulationResults") && paramName == "effectMatrix") {
        return(paste0(paramCaption, " (", matrixRow, ")"))
    }

    if (inherits(parameterSet, "ClosedCombinationTestResults") && paramName %in% c("rejected", "separatePValues")) {
        return(paste0(paramCaption, " (", matrixRow, ")"))
    }

    if (.isMultiArmSimulationResults(parameterSet) && paramName %in% c("lambdaTreatment", "medianTreatment")) {
        return(paste0(paramCaption, " (", matrixRow, ")"))
    }

    return(paste0(paramCaption, " [", matrixRow, "]"))
}

.getParameterSetParameterCaption <- function(
        parameterSet,
        ...,
        paramName,
        category = NULL,
        matrixRow = NA_integer_,
        paramNameRaw = NA_character_,
        numberOfCategories = NA_integer_) {
    paramCaption <- NULL
    if (!is.na(paramNameRaw)) {
        paramCaption <- .getParameterCaption(paramNameRaw, parameterSet)
    }
    if (is.null(paramCaption)) {
        paramCaption <- .getParameterCaption(paramName, parameterSet)
    }
    if (is.null(paramCaption)) {
        paramCaption <- paste0("%", paramName, "%")
    }

    if ((is.null(category) || is.na(category)) && is.na(matrixRow)) {
        return(paramCaption)
    }

    if (!is.null(category) && !is.na(category)) {
        return(.getParameterSetParameterCaptionPerCategory(
            parameterSet, paramName, paramCaption, category, matrixRow, numberOfCategories))
    }

    if (.isMultiArmAnalysisResults(parameterSet) && paramName %in%
            c(
                "conditionalErrorRate", "secondStagePValues",
                "adjustedStageWisePValues", "overallAdjustedTestStatistics"
            )) {
        treatments <- parameterSet$.closedTestResults$.getHypothesisTreatmentArmVariants()[matrixRow]
        paramCaption <- paste0(
            "Treatment", ifelse(grepl(",", treatments), "s", ""), " ",
            treatments, " vs. control"
        )

        return(paramCaption)
    }

    if (.isEnrichmentAnalysisResults(parameterSet) || .isEnrichmentStageResults(parameterSet) ||
            (inherits(parameterSet, "ClosedCombinationTestResults") && isTRUE(parameterSet$.enrichment))) {
        if (paramName %in% c(
                "indices", "conditionalErrorRate", "secondStagePValues",
                "adjustedStageWisePValues", "overallAdjustedTestStatistics", "rejectedIntersections"
            )) {
            if (.isEnrichmentAnalysisResults(parameterSet)) {
                populations <- parameterSet$.closedTestResults$.getHypothesisPopulationVariants()[matrixRow]
            } else if (inherits(parameterSet, "ClosedCombinationTestResults")) {
                populations <- parameterSet$.getHypothesisPopulationVariants()[matrixRow]
            } else {
                stopRuntimeIssue("only ClosedCombinationTestResults ",
                    "supports function .getHypothesisPopulationVariants() (object is ",
                    .getClassName(parameterSet), ")",
                    functionName = ".showParameterFormatted",
                    parameter = "parameterSet", value = parameterSet
                )
            }
            paramCaption <- paste0(paramCaption, " ", populations)
        } else {
            if (!is.na(numberOfCategories) && numberOfCategories == matrixRow) {
                paramCaption <- paste0(paramCaption, " F")
            } else {
                paramCaption <- paste0(paramCaption, " S", matrixRow)
            }
        }

        return(paramCaption)
    }

    return(.getParameterSetParameterCaptionPerRow(parameterSet, paramName, paramCaption, matrixRow))
}
