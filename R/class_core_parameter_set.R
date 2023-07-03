## |
## |  *Parameter set classes*
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
## |  File version: $Revision: 7148 $
## |  Last changed: $Date: 2023-07-03 15:50:22 +0200 (Mo, 03 Jul 2023) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_core_constants.R
#' @include f_core_assertions.R
NULL

#'
#' @name FieldSet
#'
#' @title
#' Field Set
#'
#' @description
#' Basic class for field sets.
#'
#' @details
#' The field set implements basic functions for a set of fields.
#'
#' @include class_core_plot_settings.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
FieldSet <- setRefClass("FieldSet",
    fields = list(
        .parameterTypes = "list",
        .parameterNames = "list",
        .parameterFormatFunctions = "list",
        .showParameterTypeEnabled = "logical",
        .catLines = "character"
    ),
    methods = list(
        .getFieldNames = function() {
            return(names(.self$getRefClass()$fields()))
        },
        .getVisibleFieldNames = function() {
            fieldNames <- names(.self$getRefClass()$fields())
            fieldNames <- fieldNames[!startsWith(fieldNames, ".")]
            return(fieldNames)
        },
        .resetCat = function() {
            .catLines <<- character(0)
        },
        .cat = function(..., file = "", sep = "", fill = FALSE, labels = NULL,
                append = FALSE, heading = 0, tableColumns = 0, consoleOutputEnabled = TRUE,
                na = NA_character_) {
            if (consoleOutputEnabled) {
                cat(..., file = file, sep = sep, fill = fill, labels = labels, append = append)
                return(invisible())
            }

            args <- list(...)
            line <- ""
            if (length(args) > 0) {
                if (tableColumns > 0) {
                    values <- unlist(args, use.names = FALSE)
                    values <- values[values != "\n"]
                    for (i in 1:length(values)) {
                        values[i] <- gsub("\n", "", values[i])
                    }
                    if (!is.null(na) && length(na) == 1 && !is.na(na)) {
                        len <- min(nchar(values))
                        naStr <- paste0(trimws(na), " ")
                        while (nchar(naStr) < len) {
                            naStr <- paste0(" ", naStr)
                        }
                        values[is.na(values) | nchar(trimws(values)) == 0] <- naStr
                    }
                    line <- paste0(values, collapse = "| ")
                    if (trimws(line) != "" && !grepl("\\| *$", line)) {
                        line <- paste0(line, "|")
                    }
                    line <- paste0("| ", line)
                    extraCells <- tableColumns - length(values)
                    if (extraCells > 0 && trimws(line) != "") {
                        line <- paste0(line, paste0(rep(" |", extraCells), collapse = ""))
                    }
                    line <- paste0(line, "\n")
                } else {
                    line <- paste0(args, collapse = sep)
                    listItemEnabled <- grepl("^  ", line)

                    headingBaseNumber <- as.integer(getOption("rpact.print.heading.base.number", 0L))
                    if (is.na(headingBaseNumber)) {
                        headingBaseNumber <- 0L
                    }
                    if (headingBaseNumber < -1) {
                        warning(
                            "Illegal option ", sQuote("rpact.print.heading.base.number"),
                            " (", headingBaseNumber, ") was set to 0"
                        )
                        headingBaseNumber <- 0L
                    }
                    if (headingBaseNumber > 4) {
                        warning(
                            "Illgeal option ", sQuote("rpact.print.heading.base.number"),
                            " (", headingBaseNumber, ") was set to 4 becasue it was too large"
                        )
                        headingBaseNumber <- 4L
                    }

                    if (heading > 0) {
                        if (headingBaseNumber == -1) {
                            lineBreak <- ""
                            if (grepl("\n *$", line)) {
                                lineBreak <- "\n\n"
                            }
                            line <- paste0("**", sub(": *", "", trimws(line)), "**", lineBreak)
                        } else {
                            headingCmd <- paste0(rep("#", heading + headingBaseNumber + 1), collapse = "")
                            lineBreak <- ""
                            if (grepl("\n *$", line)) {
                                lineBreak <- "\n\n"
                            }
                            line <- paste0(headingCmd, " ", sub(": *", "", trimws(line)), lineBreak)
                        }
                    } else {
                        parts <- strsplit(line, " *: ")[[1]]
                        if (length(parts) == 2) {
                            line <- paste0("*", trimws(parts[1]), "*: ", parts[2])
                        }
                    }
                    if (listItemEnabled) {
                        if (grepl("^  ", line)) {
                            line <- sub("^  ", "* ", line)
                        } else {
                            line <- paste0("* ", line)
                        }
                    }
                }
            }
            if (length(.catLines) == 0) {
                .catLines <<- line
            } else {
                .catLines <<- c(.catLines, line)
            }
            return(invisible())
        },
        .getFields = function(values) {
            flds <- names(.self$getRefClass()$fields())
            if (!missing(values)) {
                flds <- flds[flds %in% values]
            }
            result <- setNames(vector("list", length(flds)), flds)
            for (fld in flds) {
                result[[fld]] <- .self[[fld]]
            }
            return(result)
        }
    )
)

#'
#' @name ParameterSet
#'
#' @title
#' Parameter Set
#'
#' @description
#' Basic class for parameter sets.
#'
#' @details
#' The parameter set implements basic functions for a set of parameters.
#'
#' @include f_core_constants.R
#' @include f_core_utilities.R
#' @include f_parameter_set_utilities.R
#' @include f_analysis_utilities.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
ParameterSet <- setRefClass("ParameterSet",
    contains = "FieldSet",
    fields = list(
        .parameterTypes = "list",
        .parameterNames = "list",
        .parameterFormatFunctions = "list",
        .showParameterTypeEnabled = "logical",
        .catLines = "character"
    ),
    methods = list(
        initialize = function(...,
                .showParameterTypeEnabled = TRUE) {
            callSuper(...,
                .showParameterTypeEnabled = .showParameterTypeEnabled
            )
            .parameterTypes <<- list()
            .parameterNames <<- list()
            .parameterFormatFunctions <<- list()
            .catLines <<- character(0)
        },
        .toString = function(startWithUpperCase = FALSE) {
            s <- .formatCamelCase(.getClassName(.self))
            return(ifelse(startWithUpperCase, .firstCharacterToUpperCase(s), s))
        },
        .initParameterTypes = function() {
            for (parameterName in names(.parameterNames)) {
                .parameterTypes[[parameterName]] <<- C_PARAM_TYPE_UNKNOWN
            }
        },
        .getParameterType = function(parameterName) {
            if (is.null(parameterName) || length(parameterName) == 0 || is.na(parameterName)) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'parameterName' must be a valid character with length > 0"
                )
            }

            parameterType <- .parameterTypes[[parameterName]]
            if (is.null(parameterType)) {
                return(C_PARAM_TYPE_UNKNOWN)
            }

            return(parameterType[1])
        },
        .getParametersToShow = function() {
            return(.getVisibleFieldNames())
        },
        .setParameterType = function(parameterName, parameterType) {
            if (is.null(parameterName) || length(parameterName) == 0 || is.na(parameterName)) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'parameterName' must be a valid character with length > 0"
                )
            }

            parameterType <- parameterType[1]

            if (!all(parameterType %in% c(
                    C_PARAM_USER_DEFINED, C_PARAM_DEFAULT_VALUE,
                    C_PARAM_GENERATED, C_PARAM_DERIVED, C_PARAM_NOT_APPLICABLE
                ))) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'parameterType' ('", parameterType, "') is invalid"
                )
            }

            .parameterTypes[[parameterName]] <<- parameterType

            invisible(parameterType)
        },
        isUserDefinedParameter = function(parameterName) {
            return(.getParameterType(parameterName) == C_PARAM_USER_DEFINED)
        },
        isDefaultParameter = function(parameterName) {
            return(.getParameterType(parameterName) == C_PARAM_DEFAULT_VALUE)
        },
        isGeneratedParameter = function(parameterName) {
            return(.getParameterType(parameterName) == C_PARAM_GENERATED)
        },
        isDerivedParameter = function(parameterName) {
            return(.getParameterType(parameterName) == C_PARAM_DERIVED)
        },
        isUndefinedParameter = function(parameterName) {
            return(.getParameterType(parameterName) == C_PARAM_TYPE_UNKNOWN)
        },
        .getInputParameters = function() {
            params <- .getParametersOfOneGroup(c(C_PARAM_USER_DEFINED, C_PARAM_DEFAULT_VALUE))
            return(params)
        },
        .getUserDefinedParameters = function() {
            return(.getParametersOfOneGroup(C_PARAM_USER_DEFINED))
        },
        .getDefaultParameters = function() {
            return(.getParametersOfOneGroup(C_PARAM_DEFAULT_VALUE))
        },
        .getGeneratedParameters = function() {
            return(.getParametersOfOneGroup(C_PARAM_GENERATED))
        },
        .getDerivedParameters = function() {
            return(.getParametersOfOneGroup(C_PARAM_DERIVED))
        },
        .getUndefinedParameters = function() {
            return(.getParametersOfOneGroup(C_PARAM_TYPE_UNKNOWN))
        },
        .getParameterValueIfUserDefinedOrDefault = function(parameterName) {
            if (isUserDefinedParameter(parameterName) || isDefaultParameter(parameterName)) {
                return(.self[[parameterName]])
            }

            parameterType <- .self$getRefClass()$fields()[[parameterName]]
            if (parameterType == "numeric") {
                return(NA_real_)
            }

            if (parameterType == "integer") {
                return(NA_integer_)
            }

            if (parameterType == "character") {
                return(NA_character_)
            }

            return(NA)
        },
        .getParametersOfOneGroup = function(parameterType) {
            if (length(parameterType) == 1) {
                parameterNames <- names(.parameterTypes[.parameterTypes == parameterType])
            } else {
                parameterNames <- names(.parameterTypes[which(.parameterTypes %in% parameterType)])
            }
            parametersToShow <- .getParametersToShow()
            if (is.null(parametersToShow) || length(parametersToShow) == 0) {
                return(parameterNames)
            }

            return(parametersToShow[parametersToShow %in% parameterNames])
        },
        .showParameterType = function(parameterName) {
            if (!.showParameterTypeEnabled) {
                return("  ")
            }

            return(paste0("[", .getParameterType(parameterName), "]"))
        },
        .showAllParameters = function(consoleOutputEnabled = TRUE) {
            parametersToShow <- .getVisibleFieldNamesOrdered()
            for (parameter in parametersToShow) {
                .showParameter(parameter,
                    showParameterType = TRUE,
                    consoleOutputEnabled = consoleOutputEnabled
                )
            }
        },
        .getVisibleFieldNamesOrdered = function() {
            visibleFieldNames <- .getVisibleFieldNames()

            parametersToShowSorted <- .getParametersToShow()
            if (is.null(parametersToShowSorted) || length(parametersToShowSorted) == 0) {
                return(visibleFieldNames)
            }

            visibleFieldNames <- visibleFieldNames[!(visibleFieldNames %in% parametersToShowSorted)]
            visibleFieldNames <- c(parametersToShowSorted, visibleFieldNames)
            return(visibleFieldNames)
        },
        .show = function(..., consoleOutputEnabled = FALSE) {
            showType <- .getOptionalArgument("showType", ...)
            if (!is.null(showType) && showType == 2) {
                .cat("Technical developer summary of the ", .self$.toString(), " object (",
                    methods::classLabel(class(.self)), "):\n\n",
                    sep = "", heading = 1,
                    consoleOutputEnabled = consoleOutputEnabled
                )
                .showAllParameters(consoleOutputEnabled = consoleOutputEnabled)
                .showParameterTypeDescription(consoleOutputEnabled = consoleOutputEnabled)
            } else {
                stop(
                    C_EXCEPTION_TYPE_RUNTIME_ISSUE,
                    "method '.show()' is not implemented in class '", .getClassName(.self), "'"
                )
            }
        },
        .catMarkdownText = function(...) {
            .show(consoleOutputEnabled = FALSE, ...)
            if (length(.catLines) == 0) {
                return(invisible())
            }

            for (line in .catLines) {
                cat(line)
            }
        },
        .showParametersOfOneGroup = function(parameters, title,
                orderByParameterName = TRUE, consoleOutputEnabled = TRUE) {
            output <- ""
            if (is.null(parameters) || length(parameters) == 0 || all(is.na(parameters))) {
                if (!missing(title) && !is.null(title) && !is.na(title) && consoleOutputEnabled) {
                    output <- paste0(title, ": not available\n\n")
                    .cat(output, heading = 2, consoleOutputEnabled = consoleOutputEnabled)
                }
                invisible(output)
            } else {
                if (orderByParameterName) {
                    parameters <- sort(parameters)
                }

                if (!missing(title) && !is.null(title) && !is.na(title)) {
                    output <- paste0(title, ":\n")
                    .cat(output, heading = 2, consoleOutputEnabled = consoleOutputEnabled)
                }
                for (parameterName in parameters) {
                    output <- paste0(output, .showParameter(parameterName,
                        consoleOutputEnabled = consoleOutputEnabled
                    ))
                }
                .cat("\n", consoleOutputEnabled = consoleOutputEnabled)
                output <- paste0(output, "\n")
                invisible(output)
            }
        },
        .showParameter = function(parameterName, showParameterType = FALSE, consoleOutputEnabled = TRUE) {
            tryCatch(
                {
                    params <- .getParameterValueFormatted(obj = .self, parameterName = parameterName)
                    if (is.null(params) || !is.list(params)) {
                        return(invisible(""))
                    }

                    if (!is.null(names(params)) && "paramValue" %in% names(params)) {
                        return(.showParameterSingle(
                            param = params,
                            parameterName = parameterName,
                            showParameterType = showParameterType,
                            consoleOutputEnabled = consoleOutputEnabled
                        ))
                    }

                    output <- ""
                    for (i in 1:length(params)) {
                        param <- params[[i]]
                        category <- NULL
                        parts <- strsplit(param$paramName, "$", fixed = TRUE)[[1]]
                        if (length(parts) == 2) {
                            parameterName <- parts[1]
                            param$paramName <- parameterName

                            category <- parts[2]
                            categoryCaption <- .parameterNames[[category]]
                            if (is.null(categoryCaption)) {
                                categoryCaption <- paste0("%", category, "%")
                            }
                            category <- categoryCaption
                        }
                        outputPart <- .showParameterSingle(
                            param = param,
                            parameterName = parameterName,
                            category = category,
                            showParameterType = showParameterType,
                            consoleOutputEnabled = consoleOutputEnabled
                        )
                        if (nchar(output) > 0) {
                            output <- paste0(output, "\n", outputPart)
                        } else {
                            output <- outputPart
                        }
                    }
                    return(invisible(output))
                },
                error = function(e) {
                    if (consoleOutputEnabled) {
                        warning("Failed to show parameter '", parameterName, "': ", e$message)
                    }
                }
            )
        },
        .showParameterSingle = function(param,
                parameterName, ...,
                category = NULL,
                showParameterType = FALSE,
                consoleOutputEnabled = TRUE) {
            if (is.null(param)) {
                return(invisible(""))
            }

            output <- ""
            tryCatch(
                {
                    if (param$type == "array" && length(dim(param$paramValue)) == 3) {
                        numberOfEntries <- dim(param$paramValue)[3]
                        numberOfRows <- dim(param$paramValue)[1]
                        if (numberOfEntries > 0 && numberOfRows > 0) {
                            index <- 1
                            for (i in 1:numberOfEntries) {
                                for (j in 1:numberOfRows) {
                                    output <- paste0(output, .showParameterFormatted(
                                        paramName = param$paramName,
                                        paramValue = param$paramValue[j, , i],
                                        paramValueFormatted = param$paramValueFormatted[[index]],
                                        showParameterType = showParameterType,
                                        category = i,
                                        matrixRow = ifelse(numberOfRows == 1, NA_integer_, j),
                                        consoleOutputEnabled = consoleOutputEnabled,
                                        paramNameRaw = parameterName,
                                        numberOfCategories = numberOfEntries
                                    ))
                                    index <- index + 1
                                }
                            }
                        }
                    } else if (param$type %in% c("matrix", "array")) {
                        n <- length(param$paramValueFormatted)
                        if (n > 0) {
                            for (i in 1:n) {
                                paramValue <- param$paramValue
                                if (is.array(paramValue) &&
                                        length(dim(paramValue)) == 3 &&
                                        dim(paramValue)[3] == 1) {
                                    paramValue <- paramValue[i, , 1]
                                } else if (dim(paramValue)[1] > 1 || dim(paramValue)[2] > 1) {
                                    paramValue <- paramValue[i, ]
                                }

                                output <- paste0(output, .showParameterFormatted(
                                    paramName = param$paramName,
                                    paramValue = paramValue,
                                    paramValueFormatted = param$paramValueFormatted[[i]],
                                    showParameterType = showParameterType,
                                    category = category,
                                    matrixRow = ifelse(n == 1, NA_integer_, i),
                                    consoleOutputEnabled = consoleOutputEnabled,
                                    paramNameRaw = parameterName,
                                    numberOfCategories = n
                                ))
                            }
                        }
                    } else {
                        output <- .showParameterFormatted(
                            paramName = param$paramName,
                            paramValue = param$paramValue,
                            paramValueFormatted = param$paramValueFormatted,
                            showParameterType = showParameterType,
                            category = category,
                            consoleOutputEnabled = consoleOutputEnabled,
                            paramNameRaw = parameterName
                        )
                    }
                },
                error = function(e) {
                    if (consoleOutputEnabled) {
                        warning("Failed to show single parameter '", parameterName, "' (", param$type, "): ", e$message)
                    }
                }
            )
            return(invisible(output))
        },
        .extractParameterNameAndValue = function(parameterName) {
            d <- regexpr(paste0("\\..+\\$"), parameterName)
            if (d[1] != 1) {
                return(list(parameterName = parameterName, paramValue = get(parameterName)))
            }

            index <- attr(d, "match.length")
            objectName <- substr(parameterName, 1, index - 1)
            parameterName <- substr(parameterName, index + 1, nchar(parameterName))
            paramValue <- get(objectName)[[parameterName]]

            # .closedTestResults$rejected
            if (objectName == ".closedTestResults" && parameterName == "rejected") {
                paramValueLogical <- as.logical(paramValue)
                if (is.matrix(paramValue)) {
                    paramValueLogical <- matrix(paramValueLogical, ncol = ncol(paramValue))
                }
                paramValue <- paramValueLogical
            }

            return(list(parameterName = parameterName, paramValue = paramValue))
        },
        .showUnknownParameters = function(consoleOutputEnabled = TRUE) {
            params <- .getUndefinedParameters()
            if (length(params) > 0) {
                .showParametersOfOneGroup(params, "ISSUES (parameters with undefined type)",
                    consoleOutputEnabled = consoleOutputEnabled
                )
            }
        },
        .showParameterFormatted = function(paramName, paramValue, ..., paramValueFormatted = NA_character_,
                showParameterType = FALSE, category = NULL, matrixRow = NA_integer_, consoleOutputEnabled = TRUE,
                paramNameRaw = NA_character_, numberOfCategories = NA_integer_) {
            if (!is.na(paramNameRaw)) {
                paramCaption <- .parameterNames[[paramNameRaw]]
            }
            if (is.null(paramCaption)) {
                paramCaption <- .parameterNames[[paramName]]
            }
            if (is.null(paramCaption)) {
                paramCaption <- paste0("%", paramName, "%")
            }
            if (!is.null(category) && !is.na(category)) {
                if (.isMultiArmSimulationResults(.self) && paramName == "singleNumberOfEventsPerStage") {
                    if (!inherits(.self, "SimulationResultsEnrichmentSurvival") &&
                            !is.na(numberOfCategories) && numberOfCategories == category) {
                        category <- "control"
                    }
                    paramCaption <- paste0(paramCaption, " {", category, "}")
                } else if (paramName == "effectList") {
                    paramCaption <- paste0(paramCaption, " [", category, "]")
                } else if (.isEnrichmentSimulationResults(.self)) {
                    categoryCaption <- .getCategoryCaptionEnrichment(.self, paramName, category)
                    paramCaption <- paste0(paramCaption, " (", categoryCaption, ")")
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
            } else if (!is.na(matrixRow)) {
                if (.isMultiArmAnalysisResults(.self) && paramName %in%
                        c(
                            "conditionalErrorRate", "secondStagePValues",
                            "adjustedStageWisePValues", "overallAdjustedTestStatistics"
                        )) {
                    treatments <- .closedTestResults$.getHypothesisTreatmentArmVariants()[matrixRow]
                    paramCaption <- paste0(
                        "Treatment", ifelse(grepl(",", treatments), "s", ""), " ",
                        treatments, " vs. control"
                    )
                } else if (.isEnrichmentAnalysisResults(.self) || .isEnrichmentStageResults(.self) ||
                        (inherits(.self, "ClosedCombinationTestResults") && isTRUE(.self$.enrichment))) {
                    if (paramName %in% c(
                            "indices", "conditionalErrorRate", "secondStagePValues",
                            "adjustedStageWisePValues", "overallAdjustedTestStatistics", "rejectedIntersections"
                        )) {
                        if (.isEnrichmentAnalysisResults(.self)) {
                            populations <- .closedTestResults$.getHypothesisPopulationVariants()[matrixRow]
                        } else if (inherits(.self, "ClosedCombinationTestResults")) {
                            populations <- .self$.getHypothesisPopulationVariants()[matrixRow]
                        } else {
                            stop(
                                C_EXCEPTION_TYPE_RUNTIME_ISSUE, "only ClosedCombinationTestResults ",
                                "supports function .getHypothesisPopulationVariants() (object is ", .getClassName(.self), ")"
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
                } else if (.isMultiArmAnalysisResults(.self) || grepl("StageResultsMultiArm", .getClassName(.self)) ||
                        (inherits(.self, "SimulationResults") && paramName == "effectMatrix") ||
                        (inherits(.self, "ClosedCombinationTestResults") &&
                            paramName %in% c("rejected", "separatePValues"))) {
                    paramCaption <- paste0(paramCaption, " (", matrixRow, ")")
                } else {
                    paramCaption <- paste0(paramCaption, " [", matrixRow, "]")
                }
            }
            if (is.null(paramValueFormatted) || length(paramValueFormatted) == 0 ||
                    is.na(paramValueFormatted)) {
                paramValueFormatted <- paramValue
            }
            if (is.list(paramValueFormatted)) {
                paramValueFormatted <- .listToString(paramValueFormatted)
            }
            if (is.function(paramValue) || grepl("Function$", paramName)) {
                paramValueFormatted <- ifelse(
                    .getParameterType(paramName) == C_PARAM_USER_DEFINED,
                    ifelse(.isCppCode(paramValueFormatted), "user defined (C++)", "user defined"),
                    "default"
                )
            }
            prefix <- ifelse(showParameterType, .showParameterType(paramName), "")
            variableNameFormatted <- .getFormattedVariableName(
                name = paramCaption,
                n = .getNChar(), prefix = prefix
            )

            output <- paste(variableNameFormatted, paramValueFormatted, "\n")
            .cat(output, consoleOutputEnabled = consoleOutputEnabled)
            invisible(output)
        },
        .getNChar = function() {
            if (length(.parameterNames) == 0) {
                return(40)
            }

            return(min(40, max(nchar(.parameterNames))) + 4)
        },
        .showParameterTypeDescription = function(consoleOutputEnabled = consoleOutputEnabled) {
            .cat("\n", consoleOutputEnabled = consoleOutputEnabled)
            .cat("Legend:\n", heading = 2, consoleOutputEnabled = consoleOutputEnabled)
            .cat("  ", C_PARAM_USER_DEFINED, ": user defined\n", consoleOutputEnabled = consoleOutputEnabled)
            .cat("  ", C_PARAM_DERIVED, ": derived value\n", consoleOutputEnabled = consoleOutputEnabled)
            .cat("  ", C_PARAM_DEFAULT_VALUE, ": default value\n", consoleOutputEnabled = consoleOutputEnabled)
            .cat("  ", C_PARAM_GENERATED, ": generated/calculated value\n", consoleOutputEnabled = consoleOutputEnabled)
            .cat("  ", C_PARAM_NOT_APPLICABLE, ": not applicable or hidden\n", consoleOutputEnabled = consoleOutputEnabled)
        },
        .printAsDataFrame = function(parameterNames, niceColumnNamesEnabled = FALSE,
                includeAllParameters = FALSE, handleParameterNamesAsToBeExcluded = FALSE,
                lineBreakEnabled = FALSE) {
            if (.isTrialDesign(.self)) {
                tableColumnNames <- .getTableColumnNames(design = .self)
            } else {
                tableColumnNames <- C_TABLE_COLUMN_NAMES
            }

            if (.isTrialDesignPlan(.self)) {
                parameterNames <- NULL
            }

            dataFrame <- .getAsDataFrame(
                parameterSet = .self,
                parameterNames = parameterNames,
                niceColumnNamesEnabled = niceColumnNamesEnabled,
                includeAllParameters = includeAllParameters,
                handleParameterNamesAsToBeExcluded = handleParameterNamesAsToBeExcluded,
                returnParametersAsCharacter = TRUE,
                tableColumnNames = tableColumnNames
            )
            result <- as.matrix(dataFrame)
            if (.isTrialDesignPlan(.self)) {
                dimnames(result)[[1]] <- paste("  ", c(1:nrow(dataFrame)))
            } else if (!is.null(dataFrame[["stages"]])) {
                dimnames(result)[[1]] <- paste("  Stage", dataFrame$stages)
            }

            print(result, quote = FALSE, right = FALSE)
        },
        .getNumberOfRows = function(parameterNames) {
            numberOfRows <- 1
            for (parameterName in parameterNames) {
                parameterValues <- .self[[parameterName]]
                if (is.vector(parameterValues) && length(parameterValues) > numberOfRows) {
                    numberOfRows <- length(parameterValues)
                } else if (is.matrix(parameterValues) && (nrow(parameterValues) == 1 || ncol(parameterValues) == 1) &&
                        length(parameterValues) > numberOfRows) {
                    numberOfRows <- length(parameterValues)
                }
            }
            return(numberOfRows)
        },
        .containsMultidimensionalParameters = function(parameterNames) {
            for (parameterName in parameterNames) {
                parameterValues <- .self[[parameterName]]
                if (!is.null(parameterValues) && is.matrix(parameterValues) &&
                        nrow(parameterValues) > 0 && ncol(parameterValues) > 0) {
                    return(TRUE)
                }
            }
            return(FALSE)
        },
        .getMultidimensionalNumberOfStages = function(parameterNames) {
            if (!is.null(.self[[".design"]])) {
                return(.self$.design$kMax)
            }

            n <- 1
            for (parameterName in parameterNames) {
                parameterValues <- .self[[parameterName]]
                if (!is.null(parameterValues) && is.matrix(parameterValues) &&
                        ncol(parameterValues) > 0 && nrow(parameterValues) > n) {
                    n <- nrow(parameterValues)
                }
            }
            return(n)
        },
        .getVariedParameter = function(parameterNames, numberOfVariants) {
            # search for user defined parameters
            for (parameterName in parameterNames) {
                parameterValues <- .self[[parameterName]]
                if (!is.null(parameterValues) && !is.matrix(parameterValues) &&
                        length(parameterValues) == numberOfVariants &&
                        parameterName %in% C_VARIABLE_DESIGN_PLAN_PARAMETERS &&
                        .getParameterType(parameterName) == C_PARAM_USER_DEFINED) {
                    return(parameterName)
                }
            }

            # search for default values
            for (parameterName in parameterNames) {
                parameterValues <- .self[[parameterName]]
                if (!is.null(parameterValues) && !is.matrix(parameterValues) &&
                        length(parameterValues) == numberOfVariants &&
                        parameterName %in% C_VARIABLE_DESIGN_PLAN_PARAMETERS &&
                        .getParameterType(parameterName) == C_PARAM_DEFAULT_VALUE) {
                    return(parameterName)
                }
            }

            return(NULL)
        },
        .getDataFrameColumnCaption = function(parameterName, tableColumnNames, niceColumnNamesEnabled) {
            if (length(parameterName) == 0 || parameterName == "") {
                stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'parameterName' must be a valid parameter name")
            }

            tableColumnName <- tableColumnNames[[parameterName]]
            return(ifelse(niceColumnNamesEnabled && !is.null(tableColumnName),
                tableColumnName, parameterName
            ))
        },
        .getUnidimensionalNumberOfStages = function(parameterNames) {
            kMax <- .self[["kMax"]]
            if (is.null(kMax) && !is.null(.self[[".design"]])) {
                kMax <- .self[[".design"]][["kMax"]]
            }
            if (!is.null(kMax) && length(kMax) == 1 && is.integer(kMax)) {
                return(kMax)
            }

            n <- 1
            for (parameterName in parameterNames) {
                parameterValues <- .self[[parameterName]]
                if (!is.null(parameterValues) && !is.matrix(parameterValues) &&
                        length(parameterValues) > n) {
                    n <- length(parameterValues)
                }
            }
            return(n)
        },
        .formatDataFrameParametersAsCharacter = function(dataFrame,
                parameterName, parameterValues, parameterCaption) {
            tryCatch(
                {
                    formatFunctionName <- .parameterFormatFunctions[[parameterName]]
                    if (!is.null(formatFunctionName)) {
                        parameterValuesFormatted <- eval(call(formatFunctionName, parameterValues))
                    } else {
                        parameterValuesFormatted <- as.character(parameterValues)
                    }

                    if (parameterName == "sided") {
                        parameterValuesFormatted <- ifelse(parameterValues == 1,
                            "one-sided", "two-sided"
                        )
                    }

                    if (!is.null(dataFrame[[parameterCaption]])) {
                        parameterValuesFormatted[is.na(dataFrame[[parameterCaption]])] <- ""
                    }
                    parameterValuesFormatted[is.na(parameterValuesFormatted)] <- ""
                    parameterValuesFormatted[parameterValuesFormatted == "NA"] <- ""
                    if (is.null(dataFrame)) {
                        dataFrame <- data.frame(x = parameterValuesFormatted)
                        names(dataFrame) <- parameterCaption
                    } else {
                        dataFrame[[parameterCaption]] <- parameterValuesFormatted
                    }
                },
                error = function(e) {
                    .logError(paste0(
                        "Error in '.getAsDataFrame'. Failed to show parameter '%s' ",
                        "(class '%s'): %s"
                    ), parameterName, .getClassName(.self), e)
                }
            )
        },

        #
        # Returns a sub-list.
        #
        # @param x A list from which you would like to get a sub-list.
        # @param listEntryNames A vector of names which specify the entries of the sub-list to return.
        #
        .getSubListByNames = function(x, listEntryNames) {
            "Returns a sub-list."
            if (!is.list(x)) {
                stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'x' must be a list")
            }

            if (!is.character(listEntryNames)) {
                stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'listEntryNames' must be a character vector")
            }

            return(x[which(names(x) %in% listEntryNames)])
        },
        .isMultiHypothesesObject = function() {
            return(.isEnrichmentAnalysisResults(.self) || .isEnrichmentStageResults(.self) ||
                .isMultiArmAnalysisResults(.self) || .isMultiArmStageResults(.self))
        },
        .isEnrichmentObject = function() {
            return(.isEnrichmentAnalysisResults(.self) || .isEnrichmentStageResults(.self))
        }
    )
)

.getMultidimensionalNumberOfVariants <- function(parameterSet, parameterNames) {
    if (!is.null(parameterSet[["effectList"]])) {
        effectMatrixName <- .getSimulationEnrichmentEffectMatrixName(parameterSet)
        return(nrow(parameterSet$effectList[[effectMatrixName]]))
    }

    parameterNames <- parameterNames[!(parameterNames %in% c(
        "accrualTime", "accrualIntensity",
        "plannedSubjects", "plannedEvents",
        "minNumberOfSubjectsPerStage", "maxNumberOfSubjectsPerStage",
        "minNumberOfEventsPerStage", "maxNumberOfEventsPerStage",
        "piecewiseSurvivalTime", "lambda2", "adaptations",
        "adjustedStageWisePValues", "overallAdjustedTestStatistics"
    ))]
    if (!is.null(parameterSet[[".piecewiseSurvivalTime"]]) &&
            parameterSet$.piecewiseSurvivalTime$piecewiseSurvivalEnabled) {
        parameterNames <- parameterNames[!(parameterNames %in% c("lambda1"))]
    }

    n <- 1
    for (parameterName in parameterNames) {
        parameterValues <- parameterSet[[parameterName]]
        if (!is.null(parameterValues) && (is.matrix(parameterValues) || !is.array(parameterValues))) {
            if (is.matrix(parameterValues)) {
                if (parameterSet$.isMultiHypothesesObject()) {
                    if (nrow(parameterValues) > n && ncol(parameterValues) > 0) {
                        n <- nrow(parameterValues)
                    }
                } else if (nrow(parameterValues) > 0 && ncol(parameterValues) > n) {
                    n <- ncol(parameterValues)
                }
            } else if (length(parameterValues) > n &&
                    !parameterSet$.isMultiHypothesesObject()) {
                n <- length(parameterValues)
            }
        }
    }
    return(n)
}

.getDataFrameColumnValues <- function(parameterSet,
        parameterName,
        numberOfVariants,
        numberOfStages,
        includeAllParameters,
        mandatoryParameterNames) {
    if (parameterSet$.getParameterType(parameterName) == C_PARAM_TYPE_UNKNOWN &&
            parameterName != "futilityStop") {
        return(NULL)
    }

    if (!includeAllParameters &&
            parameterSet$.getParameterType(parameterName) == C_PARAM_NOT_APPLICABLE &&
            !(parameterName %in% mandatoryParameterNames)) {
        return(NULL)
    }

    parameterValues <- parameterSet[[parameterName]]
    if (is.null(parameterValues) || length(parameterValues) == 0) {
        return(NULL)
    }

    if (is.function(parameterValues)) {
        return(NULL)
    }

    if (is.array(parameterValues) && !is.matrix(parameterValues)) {
        return(NULL)
    }

    if (parameterName %in% c("adjustedStageWisePValues", "overallAdjustedTestStatistics")) {
        return(NULL)
    }

    if (!is.matrix(parameterValues)) {
        if (length(parameterValues) == 1) {
            return(rep(parameterValues, numberOfVariants * numberOfStages))
        }

        if (parameterSet$.isMultiHypothesesObject()) {
            if (length(parameterValues) == numberOfStages) {
                return(as.vector(sapply(FUN = rep, X = parameterValues, times = numberOfVariants)))
            }
        }

        if (length(parameterValues) == numberOfVariants) {
            return(rep(parameterValues, numberOfStages))
        }

        if (length(parameterValues) == numberOfStages &&
                parameterName %in% c(
                    "plannedEvents", "plannedSubjects",
                    "minNumberOfEventsPerStage", "maxNumberOfEventsPerStage",
                    "minNumberOfSubjectsPerStage", "maxNumberOfSubjectsPerStage",
                    "allocationRatioPlanned"
                )) {
            values <- c()
            for (stage in 1:numberOfStages) {
                values <- c(values, rep(parameterValues[stage], numberOfVariants))
            }
            return(values)
        }

        if (parameterName %in% c(
                "accrualTime", "accrualIntensity",
                "plannedEvents", "plannedSubjects",
                "minNumberOfEventsPerStage", "maxNumberOfEventsPerStage",
                "minNumberOfSubjectsPerStage", "maxNumberOfSubjectsPerStage",
                "piecewiseSurvivalTime", "lambda2"
            )) {
            return(NULL)
        }

        stop(
            C_EXCEPTION_TYPE_RUNTIME_ISSUE,
            "parameter '", parameterName, "' has an invalid ",
            "dimension (length is ", length(parameterValues), ")"
        )
    } else if (parameterName == "effectMatrix") {
        # return effect matrix row if 'effectMatrix' is user defined
        if (parameterSet$.getParameterType("effectMatrix") == C_PARAM_USER_DEFINED) {
            return(1:ncol(parameterValues))
        }

        return(parameterValues[nrow(parameterValues), ])
    }

    if (grepl("futility|alpha0Vec|earlyStop", parameterName) &&
            nrow(parameterValues) == numberOfStages - 1) {
        parameterValues <- rbind(parameterValues, rep(NA_real_, ncol(parameterValues)))
    }

    if (nrow(parameterValues) == numberOfStages && ncol(parameterValues) == 1) {
        columnValues <- c()
        for (parameterValue in parameterValues) {
            columnValues <- c(columnValues, rep(parameterValue, numberOfVariants))
        }
        return(columnValues)
    }

    if (nrow(parameterValues) == numberOfStages && ncol(parameterValues) == numberOfVariants) {
        columnValues <- c()
        for (i in 1:nrow(parameterValues)) {
            for (j in 1:ncol(parameterValues)) {
                columnValues <- c(columnValues, parameterValues[i, j])
            }
        }
        return(columnValues)
    }

    # applicable for analysis enrichment
    if (parameterSet$.isMultiHypothesesObject()) {
        if (nrow(parameterValues) %in% c(1, numberOfVariants) &&
                ncol(parameterValues) %in% c(1, numberOfStages)) {
            columnValues <- c()
            for (j in 1:ncol(parameterValues)) {
                for (i in 1:nrow(parameterValues)) {
                    columnValues <- c(columnValues, parameterValues[i, j])
                }
            }
            if (nrow(parameterValues) == 1) {
                columnValues <- as.vector(sapply(FUN = rep, X = columnValues, times = numberOfVariants))
            }
            if (ncol(parameterValues) == 1) {
                columnValues <- rep(columnValues, numberOfStages)
            }
            return(columnValues)
        }
    }

    if (nrow(parameterValues) == 1 && ncol(parameterValues) == 1) {
        return(rep(parameterValues[1, 1], numberOfStages * numberOfVariants))
    }

    if (nrow(parameterValues) == 1 && ncol(parameterValues) == numberOfVariants) {
        return(rep(parameterValues[1, ], numberOfStages))
    }

    if (nrow(parameterValues) == numberOfStages && ncol(parameterValues) == 1) {
        return(rep(parameterValues[, 1], numberOfVariants))
    }

    stop(
        C_EXCEPTION_TYPE_RUNTIME_ISSUE,
        "parameter '", parameterName, "' has an invalid ",
        "dimension (", nrow(parameterValues), " x ", ncol(parameterValues), "); ",
        "expected was (", numberOfStages, " x ", numberOfVariants, ")"
    )
}

.getAsDataFrameMultidimensional <- function(parameterSet,
        parameterNames,
        niceColumnNamesEnabled,
        includeAllParameters,
        returnParametersAsCharacter,
        tableColumnNames,
        mandatoryParameterNames) {
    numberOfVariants <- .getMultidimensionalNumberOfVariants(parameterSet, parameterNames)
    numberOfStages <- parameterSet$.getMultidimensionalNumberOfStages(parameterNames)

    stagesCaption <- parameterSet$.getDataFrameColumnCaption(
        "stages",
        tableColumnNames, niceColumnNamesEnabled
    )

    dataFrame <- data.frame(
        stages = sort(rep(1:numberOfStages, numberOfVariants))
    )
    names(dataFrame) <- stagesCaption

    if (parameterSet$.isEnrichmentObject()) {
        populations <- character(0)
        for (i in 1:numberOfVariants) {
            populations <- c(populations, ifelse(i == numberOfVariants, "F", paste0("S", i)))
        }
        dataFrame$populations <- rep(populations, numberOfStages)
        populationsCaption <- parameterSet$.getDataFrameColumnCaption(
            "populations",
            tableColumnNames, niceColumnNamesEnabled
        )
        names(dataFrame) <- c(stagesCaption, populationsCaption)
    }

    variedParameter <- parameterSet$.getVariedParameter(parameterNames, numberOfVariants)
    tryCatch(
        {
            if (!is.null(variedParameter) && variedParameter != "stages") {
                variedParameterCaption <- parameterSet$.getDataFrameColumnCaption(
                    variedParameter,
                    tableColumnNames, niceColumnNamesEnabled
                )
                dataFrame[[variedParameterCaption]] <- rep(parameterSet[[variedParameter]], numberOfStages)
            }
        },
        error = function(e) {
            warning(
                ".getAsDataFrameMultidimensional: ",
                "failed to add 'variedParameterCaption' to data.frame; ", e$message
            )
        }
    )

    usedParameterNames <- character(0)
    for (parameterName in parameterNames) {
        tryCatch(
            {
                if (!(parameterName %in% c("stages", "adaptations", "effectList")) &&
                        !grepl("Function$", parameterName) &&
                        (is.null(variedParameter) || parameterName != variedParameter)) {
                    columnValues <- .getDataFrameColumnValues(
                        parameterSet, parameterName,
                        numberOfVariants, numberOfStages,
                        includeAllParameters, mandatoryParameterNames
                    )
                    if (!is.null(columnValues)) {
                        columnCaption <- parameterSet$.getDataFrameColumnCaption(
                            parameterName,
                            tableColumnNames, niceColumnNamesEnabled
                        )
                        dataFrame[[columnCaption]] <- columnValues
                        if (returnParametersAsCharacter) {
                            parameterSet$.formatDataFrameParametersAsCharacter(
                                dataFrame,
                                parameterName, columnValues, columnCaption
                            )
                        }
                        usedParameterNames <- c(usedParameterNames, parameterName)
                    }
                }

                if (parameterName == "effectList") {
                    effectMatrixName <- .getSimulationEnrichmentEffectMatrixName(parameterSet)
                    effectMatrixNameSingular <- sub("s$", "", effectMatrixName)
                    effectMatrix <- parameterSet$effectList[[effectMatrixName]]
                    if (ncol(effectMatrix) == 1) {
                        dataFrame[[effectMatrixNameSingular]] <- rep(effectMatrix, numberOfStages)
                    } else {
                        for (j in 1:ncol(effectMatrix)) {
                            dataFrame[[paste0(effectMatrixNameSingular, j)]] <- rep(effectMatrix[, j], numberOfStages)
                        }
                    }
                    dataFrame$situation <- rep(1:nrow(effectMatrix), numberOfStages)
                    usedParameterNames <- c(usedParameterNames, parameterName)
                }
            },
            error = function(e) {
                warning(
                    ".getAsDataFrameMultidimensional: failed to add parameter ",
                    sQuote(parameterName), " to data.frame; ", e$message
                )
            }
        )
    }

    if (includeAllParameters) {
        extraParameterNames <- names(parameterSet)
        extraParameterNames <- extraParameterNames[!grepl("^\\.", extraParameterNames)]
        extraParameterNames <- extraParameterNames[!(extraParameterNames %in% parameterNames)]
        extraParameterNames <- unique(c(parameterNames[!(parameterNames %in% usedParameterNames)], extraParameterNames))
        for (extraParameter in extraParameterNames) {
            tryCatch(
                {
                    if (parameterSet$.getParameterType(extraParameter) != C_PARAM_TYPE_UNKNOWN) {
                        value <- parameterSet[[extraParameter]]
                        if (!is.null(value) && length(value) > 0 &&
                                !is.matrix(value) && !is.array(value) && !is.data.frame(value) &&
                                (is.numeric(value) || is.character(value) || is.logical(value))) {
                            columnCaption <- parameterSet$.getDataFrameColumnCaption(
                                extraParameter,
                                tableColumnNames, niceColumnNamesEnabled
                            )

                            if (length(value) == 1) {
                                dataFrame[[columnCaption]] <- rep(value, nrow(dataFrame))
                            } else {
                                dataFrame[[columnCaption]] <- rep(.arrayToString(value, maxLength = 10), nrow(dataFrame))
                            }
                        }
                    }
                },
                error = function(e) {
                    warning(
                        ".getAsDataFrameMultidimensional: failed to add extra parameter ",
                        sQuote(parameterName), " to data.frame; ", e$message
                    )
                }
            )
        }
    }

    return(dataFrame)
}

.getAsDataFrameUnidimensional <- function(parameterSet, parameterNames, niceColumnNamesEnabled,
        includeAllParameters, returnParametersAsCharacter, tableColumnNames) {
    numberOfStages <- parameterSet$.getUnidimensionalNumberOfStages(parameterNames)
    dataFrame <- NULL
    for (parameterName in parameterNames) {
        tryCatch(
            {
                parameterCaption <- ifelse(niceColumnNamesEnabled &&
                    !is.null(tableColumnNames[[parameterName]]),
                tableColumnNames[[parameterName]], parameterName
                )
                parameterValues <- parameterSet[[parameterName]]
                if (parameterName == "futilityBounds") {
                    parameterValues[parameterValues == C_FUTILITY_BOUNDS_DEFAULT] <- -Inf
                }
                if (length(parameterValues) == 1) {
                    parameterValues <- rep(parameterValues, numberOfStages)
                } else {
                    while (length(parameterValues) < numberOfStages) {
                        parameterValues <- c(parameterValues, NA)
                    }
                }
                if (includeAllParameters || (
                        parameterSet$.getParameterType(parameterName) != C_PARAM_NOT_APPLICABLE &&
                            sum(is.na(parameterValues)) < length(parameterValues))) {
                    if (is.null(dataFrame)) {
                        dataFrame <- data.frame(x = parameterValues)
                        names(dataFrame) <- parameterCaption
                    } else {
                        dataFrame[[parameterCaption]] <- parameterValues
                    }
                }
                if (returnParametersAsCharacter) {
                    parameterSet$.formatDataFrameParametersAsCharacter(
                        dataFrame,
                        parameterName, parameterValues, parameterCaption
                    )
                }
            },
            error = function(e) {
                .logError("Failed to add parameter '%s' to data.frame: %s", parameterName, e)
            }
        )
    }

    return(dataFrame)
}

.getAsDataFrame <- function(...,
        parameterSet,
        parameterNames,
        niceColumnNamesEnabled = FALSE,
        includeAllParameters = FALSE,
        handleParameterNamesAsToBeExcluded = FALSE,
        returnParametersAsCharacter = FALSE,
        tableColumnNames = C_TABLE_COLUMN_NAMES,
        mandatoryParameterNames = character(0)) {
    parameterNamesToBeExcluded <- c()
    if (handleParameterNamesAsToBeExcluded) {
        parameterNamesToBeExcluded <- parameterNames
        parameterNames <- parameterSet$.getVisibleFieldNamesOrdered()
        if (!is.null(parameterNamesToBeExcluded) && length(parameterNamesToBeExcluded) > 0) {
            parameterNames <- parameterNames[!(parameterNames %in% parameterNamesToBeExcluded)]
        }
    } else if (is.null(parameterNames)) {
        parameterNames <- parameterSet$.getVisibleFieldNamesOrdered()
    }
    parameterNames <- parameterNames[!grepl("^\\.", parameterNames)]

    parametersToIgnore <- character(0)
    if (!is.null(parameterSet[[".piecewiseSurvivalTime"]]) &&
            parameterSet$.piecewiseSurvivalTime$piecewiseSurvivalEnabled) {
        parametersToIgnore <- c(
            parametersToIgnore,
            "lambda1", "lambda2", "median1", "median2",
            "pi1", "pi2", "piecewiseSurvivalTime"
        )
        parametersToIgnore <- intersect(parametersToIgnore, parameterNames)
    }

    if (parameterSet$.getParameterType("hazardRatio") == C_PARAM_GENERATED &&
            !is.null(parameterSet[[".piecewiseSurvivalTime"]]) &&
            isTRUE(parameterSet$.piecewiseSurvivalTime$piecewiseSurvivalEnabled)) {
        parametersToIgnore <- c(parametersToIgnore, "hazardRatio")
    }

    if (!inherits(parameterSet, "AccrualTime")) {
        accrualTime <- parameterSet[["accrualTime"]]
        if (!is.null(accrualTime) && length(accrualTime) > 1) {
            parametersToIgnore <- c(parametersToIgnore, c("accrualTime", "accrualIntensity"))
        }
    }

    if (length(parametersToIgnore) > 0) {
        parameterNames <- parameterNames[!(parameterNames %in% parametersToIgnore)]
    }

    if (parameterSet$.containsMultidimensionalParameters(parameterNames)) {
        return(.addDelayedInformationRates(.getAsDataFrameMultidimensional(
            parameterSet, parameterNames, niceColumnNamesEnabled,
            includeAllParameters, returnParametersAsCharacter, tableColumnNames,
            mandatoryParameterNames
        )))
    }

    # remove matrices
    for (parameterName in parameterNames) {
        parameterValues <- parameterSet[[parameterName]]
        if (is.matrix(parameterValues) && nrow(parameterValues) != 1 && ncol(parameterValues) != 1) {
            parameterNames <- parameterNames[parameterNames != parameterName]
        }
    }

    if (length(parameterNames) == 0) {
        return(data.frame())
    }

    return(.addDelayedInformationRates(.getAsDataFrameUnidimensional(
        parameterSet, parameterNames, niceColumnNamesEnabled,
        includeAllParameters, returnParametersAsCharacter, tableColumnNames
    )))
}

.getCategoryCaptionEnrichment <- function(parameterSet, parameterName, categoryNumber) {
    categoryCaption <- categoryNumber
    if (parameterName %in% c("sampleSizes", "singleNumberOfEventsPerStage")) {
        categoryCaption <- parameterSet$effectList$subGroups[categoryNumber]
        maxNumberOfDigits <- max(nchar(sub("\\D*", "", parameterSet$effectList$subGroups)))
        if (parameterSet$populations > 2 && grepl(paste0("^S\\d{1,", maxNumberOfDigits - 1, "}$"), categoryCaption)) {
            categoryCaption <- paste0(categoryCaption, " only")
        }
    } else {
        if (parameterSet$populations <= 2) {
            categoryCaption <- ifelse(categoryNumber == parameterSet$populations, "F", "S")
        } else {
            categoryCaption <- ifelse(categoryNumber == parameterSet$populations, "F", paste0("S", categoryNumber))
        }
    }
    return(categoryCaption)
}

#'
#' @title
#' Names of a Field Set Object
#'
#' @description
#' Function to get the names of a \code{\link{FieldSet}} object.
#'
#' @param x A \code{\link{FieldSet}} object.
#'
#' @details
#' Returns the names of a field set that can be accessed by the user.
#'
#' @template return_names
#'
#' @export
#'
#' @keywords internal
#'
names.FieldSet <- function(x) {
    return(x$.getVisibleFieldNames())
}

#'
#' @title
#' Print Field Set Values
#'
#' @description
#' \code{print} prints its \code{\link{FieldSet}} argument and returns it invisibly (via \code{invisible(x)}).
#'
#' @param x A \code{\link{FieldSet}} object.
#' @inheritParams param_three_dots
#'
#' @details
#' Prints the field set.
#'
#' @export
#'
#' @keywords internal
#'
print.FieldSet <- function(x, ...) {
    x$show()
    invisible(x)
}

#'
#' @title
#' Coerce Parameter Set to a Data Frame
#'
#' @description
#' Returns the \code{ParameterSet} as data frame.
#'
#' @param x A \code{\link{FieldSet}} object.
#' @inheritParams param_niceColumnNamesEnabled
#' @inheritParams param_includeAllParameters
#' @inheritParams param_three_dots
#'
#' @details
#' Coerces the parameter set to a data frame.
#'
#' @template return_dataframe
#'
#' @export
#'
#' @keywords internal
#'
as.data.frame.ParameterSet <- function(x, row.names = NULL,
        optional = FALSE, niceColumnNamesEnabled = FALSE, includeAllParameters = FALSE, ...) {
    .warnInCaseOfUnknownArguments(functionName = "as.data.frame", ...)

    return(.getAsDataFrame(
        parameterSet = x,
        parameterNames = NULL,
        niceColumnNamesEnabled = niceColumnNamesEnabled,
        includeAllParameters = includeAllParameters
    ))
}

#'
#' @title
#' Field Set Transpose
#'
#' @description
#' Given a \code{FieldSet} \code{x}, t returns the transpose of \code{x}.
#'
#' @param x A \code{FieldSet}.
#'
#' @details
#' Implementation of the base R generic function \code{\link[base]{t}}
#'
#' @keywords internal
#'
#' @export
#'
setMethod(
    "t", "FieldSet",
    function(x) {
        x <- as.matrix(x, niceColumnNamesEnabled = TRUE)
        return(t(x))
    }
)

#'
#' @title
#' Coerce Field Set to a Matrix
#'
#' @description
#' Returns the \code{FrameSet} as matrix.
#'
#' @param x A \code{\link{FieldSet}} object.
#' @param enforceRowNames If \code{TRUE}, row names will be created
#'        depending on the object type, default is \code{TRUE}.
#' @inheritParams param_niceColumnNamesEnabled
#' @inheritParams param_three_dots
#'
#' @details
#' Coerces the frame set to a matrix.
#'
#' @template return_matrix
#'
#' @export
#'
#' @keywords internal
#'
as.matrix.FieldSet <- function(x, ..., enforceRowNames = TRUE, niceColumnNamesEnabled = TRUE) {
    dataFrame <- as.data.frame(x, niceColumnNamesEnabled = niceColumnNamesEnabled)
    dataFrame <- .setStagesAsFirstColumn(dataFrame)
    result <- as.matrix(dataFrame)

    if (nrow(result) == 0) {
        return(result)
    }

    if (inherits(x, "PowerAndAverageSampleNumberResult")) {
        dimnames(result)[[1]] <- rep("", nrow(result))
        return(result)
    }

    if (inherits(x, "AnalysisResults")) {
        dfDesign <- as.data.frame(x$.design, niceColumnNamesEnabled = niceColumnNamesEnabled)
        dfStageResults <- as.data.frame(x$.stageResults, niceColumnNamesEnabled = niceColumnNamesEnabled)
        dfStageResults <- dfStageResults[!is.na(dfStageResults[, grep("(test statistic)|(testStatistics)", colnames(dfStageResults))]), ]
        if (length(intersect(names(dfDesign), names(dfStageResults))) == 1) {
            dfTemp <- merge(dfDesign, dfStageResults)
            if (length(intersect(names(dfTemp), names(dataFrame))) >= 1) {
                dataFrame <- merge(dfTemp, dataFrame, all.x = FALSE, all.y = TRUE)
                dataFrame <- .setStagesAsFirstColumn(dataFrame)
                result <- as.matrix(dataFrame)
            }
        } else if (length(intersect(names(dfStageResults), names(dataFrame))) >= 1) {
            dataFrame <- merge(dfStageResults, dataFrame, all.x = FALSE, all.y = TRUE)
            dataFrame <- .setStagesAsFirstColumn(dataFrame)
            result <- as.matrix(dataFrame)
        }
    }

    if (any(grepl("^(S|s)tages?$", colnames(result)))) {
        dimnames(result)[[1]] <- rep("", nrow(result))
    }

    return(result)
}

.setStagesAsFirstColumn <- function(data) {
    columnNames <- colnames(data)
    index <- grep("^(S|s)tages?$", columnNames)
    if (length(index) == 0 || index == 1) {
        return(data)
    }

    stageName <- columnNames[index[1]]
    stageNumbers <- data[, stageName]
    if (is.null(stageNumbers) || length(stageNumbers) == 0) {
        return(data)
    }

    data <- data[, c(stageName, columnNames[columnNames != stageName])]

    return(data)
}

#'
#' @title
#' Parameter Set Summary
#'
#' @description
#' Displays a summary of \code{\link{ParameterSet}} object.
#'
#' @param object A \code{\link{ParameterSet}} object.
#' @inheritParams param_digits
#' @inheritParams param_three_dots
#'
#' @details
#' Summarizes the parameters and results of a parameter set.
#'
#' @template details_summary
#'
#' @template return_object_summary
#' @template how_to_get_help_for_generics
#'
#' @export
#'
#' @keywords internal
#'
summary.ParameterSet <- function(object, ..., type = 1, digits = NA_integer_, output = c("all", "title", "overview", "body")) {
    .warnInCaseOfUnknownArguments(functionName = "summary", ...)

    if (type == 1 && inherits(object, "SummaryFactory")) {
        return(object)
    }

    if (type == 1 && (inherits(object, "TrialDesign") || inherits(object, "TrialDesignPlan") ||
            inherits(object, "SimulationResults") || inherits(object, "AnalysisResults") ||
            inherits(object, "TrialDesignCharacteristics") ||
            inherits(object, "PerformanceScore"))) {
        output <- match.arg(output)
        return(.createSummary(object, digits = digits, output = output))
    }

    # create technical summary
    object$show(showType = 2)
    object$.cat("\n")

    if (!is.null(object[[".piecewiseSurvivalTim"]])) {
        object$.piecewiseSurvivalTime$show()
        object$.cat("\n")
    }

    if (!is.null(object[[".accrualTime"]])) {
        object$.accrualTime$show()
        object$.cat("\n")
    }

    object$.cat(object$.toString(startWithUpperCase = TRUE), " table:\n", heading = 1)
    parametersToShow <- object$.getParametersToShow()
    for (parameter in parametersToShow) {
        if (length(object[[parameter]]) == 1) {
            parametersToShow <- parametersToShow[parametersToShow != parameter]
        }
    }
    object$.printAsDataFrame(parameterNames = parametersToShow, niceColumnNamesEnabled = TRUE)
    invisible(object)
}

#'
#' @title
#' Print Parameter Set Values
#'
#' @description
#' \code{print} prints its \code{ParameterSet} argument and returns it invisibly (via \code{invisible(x)}).
#'
#' @param x The \code{\link{ParameterSet}} object to print.
#' @param markdown If \code{TRUE}, the object \code{x} will be printed using markdown syntax;
#'        normal representation will be used otherwise (default is \code{FALSE})
#' @inheritParams param_three_dots
#'
#' @details
#' Prints the parameters and results of a parameter set.
#'
#' @export
#'
#' @keywords internal
#'
print.ParameterSet <- function(x, ..., markdown = FALSE) {
    if (markdown) {
        x$.catMarkdownText()
        return(invisible(x))
    }

    x$show()
    invisible(x)
}

#'
#' @title
#' Parameter Set Plotting
#'
#' @description
#' Plots an object that inherits from class \code{\link{ParameterSet}}.
#'
#' @param x The object that inherits from \code{\link{ParameterSet}}.
#' @param y Not available for this kind of plot (is only defined to be compatible to the generic plot function).
#' @param main The main title.
#' @param xlab The x-axis label.
#' @param ylab The y-axis label.
#' @param type The plot type (default = 1).
#' @inheritParams param_palette
#' @inheritParams param_showSource
#' @inheritParams param_plotSettings
#' @inheritParams param_legendPosition
#' @inheritParams param_three_dots_plot
#'
#' @details
#' Generic function to plot a parameter set.
#'
#' @template return_object_ggplot
#'
#' @export
#'
plot.ParameterSet <- function(x, y, ..., main = NA_character_,
        xlab = NA_character_, ylab = NA_character_, type = 1L, palette = "Set1",
        legendPosition = NA_integer_, showSource = FALSE, plotSettings = NULL) {
    .assertGgplotIsInstalled()

    stop(
        C_EXCEPTION_TYPE_RUNTIME_ISSUE,
        "sorry, function 'plot' is not implemented yet for class '", .getClassName(x), "'"
    )
}

.getKnitPrintVersion <- function(x, ...) {
    fCall <- match.call(expand.dots = FALSE)

    .assertPackageIsInstalled("knitr")

    args <- list(x = x, markdown = TRUE)
    if (.isSimulationResults(x)) {
        showStatistics <- .getOptionalArgument("showStatistics", optionalArgumentDefaultValue = FALSE, ...)
        if (isTRUE(showStatistics)) {
            args$showStatistics <- TRUE
        }
    }
    if (inherits(x, "SummaryFactory") || .isSummaryPipe(fCall)) {
        args$showSummary <- TRUE
    }

    return(do.call(what = print, args = args))
}

#'
#' @title
#' Print Parameter Set in Markdown Code Chunks
#'
#' @description
#' The function `knit_print.ParameterSet` is the default printing function for rpact result objects in knitr.
#' The chunk option `render` uses this function by default.
#' To fall back to the normal printing behavior set the chunk option `render = normal_print`.
#' For more information see \code{\link[knitr]{knit_print}}.
#'
#' @param x A \code{ParameterSet}.
#' @param  ... Other arguments (see \code{\link[knitr]{knit_print}}).
#'
#' @details
#' Generic function to print a parameter set in Markdown.
#' Use \code{options("rpact.print.heading.base.number" = "NUMBER")} (where \code{NUMBER} is an integer value >= -1) to
#' specify the heading level. The default is \code{options("rpact.print.heading.base.number" = "0")}, i.e., the
#' top headings start with \code{##} in Markdown. \code{options("rpact.print.heading.base.number" = "-1")} means
#' that all headings will be written bold but are not explicit defined as header.
#'
#' @export
#'
knit_print.ParameterSet <- function(x, ...) {
    result <- paste0(utils::capture.output(.getKnitPrintVersion(x = x, ...)), collapse = "\n")
    return(knitr::asis_output(result))
}

#'
#' @title
#' Create output in Markdown
#'
#' @description
#' The \code{kable()} function returns the output of the specified object formatted in Markdown.
#'
#' @param x A \code{ParameterSet}. If x does not inherit from class \code{\link{ParameterSet}},
#'        \code{knitr::kable(x)} will be returned.
#' @param  ... Other arguments (see \code{\link[knitr]{kable}}).
#'
#' @details
#' Generic function to represent a parameter set in Markdown.
#' Use \code{options("rpact.print.heading.base.number" = "NUMBER")} (where \code{NUMBER} is an integer value >= -1) to
#' specify the heading level. The default is \code{options("rpact.print.heading.base.number" = "0")}, i.e., the
#' top headings start with \code{##} in Markdown. \code{options("rpact.print.heading.base.number" = "-1")} means
#' that all headings will be written bold but are not explicit defined as header.
#'
#' @export
#'
kable.ParameterSet <- function(x, ...) {
    fCall <- match.call(expand.dots = FALSE)
    if (inherits(x, "ParameterSet")) {
        objName <- deparse(fCall$x)
        if (length(objName) > 0) {
            if (length(objName) > 1) {
                objName <- paste0(objName[1], "...")
            }
            if (grepl("^ *print\\(", objName)) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "kable(", objName, ") does not work correctly. ",
                    "Use ", sub("print", "kable", objName), " without 'print' instead or ", sub("\\)", ", markdown = TRUE)", objName)
                )
            }
        }

        return(knit_print.ParameterSet(x = x, ...))
    }

    .assertPackageIsInstalled("knitr")
    knitr::kable(x, ...)
}

#'
#' @title
#' Create tables in Markdown
#'
#' @description
#' The \code{kable()} function returns a single table for a single object that inherits from class \code{\link{ParameterSet}}.
#'
#' @details
#' Generic to represent a parameter set in Markdown.
#'
#' @param x The object that inherits from \code{\link{ParameterSet}}.
#' @param  ... Other arguments (see \code{\link[knitr]{kable}}).
#'
#' @export
#'
setGeneric("kable", kable.ParameterSet)
