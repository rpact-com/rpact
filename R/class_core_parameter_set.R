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
## |  File version: $Revision: 8508 $
## |  Last changed: $Date: 2025-01-24 09:01:34 +0100 (Fr, 24 Jan 2025) $
## |  Last changed by: $Author: pahlke $
## |

#' @include class_dictionary.R
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
FieldSet <- R6::R6Class("FieldSet",
    public = list(
        .parameterTypes = NULL,
        .showParameterTypeEnabled = NULL,
        .catLines = NULL,
        .deprecatedFieldNames = NULL,
        .getFieldNames = function() {
            classNames <- class(self)
            classNames <- classNames[classNames != "R6"]
            classNames <- base::rev(classNames)
            fieldNames <- unlist(lapply(classNames, function(x) {
                names(base::get(x)$public_fields)
            }))
            startFieldNameIndices <- grepl("^\\.", fieldNames)
            fieldNames <- c(fieldNames[startFieldNameIndices], fieldNames[!startFieldNameIndices])
            return(fieldNames)
        },
        .getVisibleFieldNames = function() {
            fieldNames <- self$.getFieldNames()
            fieldNames <- fieldNames[!startsWith(fieldNames, ".")]
            fieldNames <- fieldNames[!(fieldNames %in% self$.deprecatedFieldNames)]
            return(fieldNames)
        },
        .resetCat = function() {
            self$.catLines <- character()
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
                    for (i in seq_len(length(values))) {
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

                    headingBaseNumber <- as.integer(getOption("rpact.print.heading.base.number", 
                            C_HEADING_BASE_NUMBER_DEFAULT))
                    if (is.na(headingBaseNumber)) {
                        headingBaseNumber <- C_HEADING_BASE_NUMBER_DEFAULT
                    }
                    if (headingBaseNumber < -2) {
                        warning(
                            "Illegal option ", sQuote("rpact.print.heading.base.number"),
                            " (", headingBaseNumber, ") was set to ", C_HEADING_BASE_NUMBER_DEFAULT
                        )
                        headingBaseNumber <- C_HEADING_BASE_NUMBER_DEFAULT
                    }
                    if (headingBaseNumber > 4) {
                        warning(
                            "Illgeal option ", sQuote("rpact.print.heading.base.number"),
                            " (", headingBaseNumber, ") was set to 4 because it was too large"
                        )
                        headingBaseNumber <- 4L
                    }

                    if (heading > 0) {
                        if (headingBaseNumber %in% c(-1, -2)) {
                            lineBreak <- ""
                            if (grepl("\n *$", line)) {
                                lineBreak <- "\n\n"
                            }
                            fontStyle <- ifelse(headingBaseNumber == -1, "**", "*")
                            line <- paste0(fontStyle, sub(": *", "", trimws(line)), fontStyle, lineBreak)
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
            if (length(self$.catLines) == 0) {
                self$.catLines <- line
            } else {
                self$.catLines <- c(self$.catLines, line)
            }
            return(invisible())
        },
        .catMarkdownText = function(...) {
            self$.show(consoleOutputEnabled = FALSE, ...)
            if (length(self$.catLines) == 0) {
                return(invisible())
            }
            
            for (line in self$.catLines) {
                cat(line)
            }
        },
        .getFields = function(values) {
            flds <- self$.getFieldNames()
            if (!missing(values)) {
                flds <- flds[flds %in% values]
            }
            result <- setNames(vector("list", length(flds)), flds)
            for (fld in flds) {
                result[[fld]] <- self[[fld]]
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
ParameterSet <- R6::R6Class("ParameterSet",
    inherit = FieldSet,
    public = list(
        initialize = function(..., .showParameterTypeEnabled = TRUE) {
            self$.showParameterTypeEnabled <- .showParameterTypeEnabled
            self$.parameterTypes <- list()
            self$.catLines <- character()
            self$.deprecatedFieldNames <- character()
        },
        .toString = function(startWithUpperCase = FALSE) {
            s <- .formatCamelCase(.getClassName(self))
            return(ifelse(startWithUpperCase, .firstCharacterToUpperCase(s), s))
        },
        .initParameterTypes = function() {
            self$.parameterTypes <- list()
            for (parameterName in self$.getVisibleFieldNames()) {
                self$.parameterTypes[[parameterName]] <- C_PARAM_TYPE_UNKNOWN
            }
        },
        .getParameterType = function(parameterName) {
            if (is.null(parameterName) || length(parameterName) == 0 || is.na(parameterName)) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'parameterName' must be a valid character with length > 0"
                )
            }

            parameterType <- self$.parameterTypes[[parameterName]]
            if (is.null(parameterType)) {
                return(C_PARAM_TYPE_UNKNOWN)
            }

            return(parameterType[1])
        },
        .getParametersToShow = function() {
            return(self$.getVisibleFieldNames())
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

            self$.parameterTypes[[parameterName]] <- parameterType

            invisible(parameterType)
        },
        isUserDefinedParameter = function(parameterName) {
            return(self$.getParameterType(parameterName) == C_PARAM_USER_DEFINED)
        },
        isDefaultParameter = function(parameterName) {
            return(self$.getParameterType(parameterName) == C_PARAM_DEFAULT_VALUE)
        },
        isGeneratedParameter = function(parameterName) {
            return(self$.getParameterType(parameterName) == C_PARAM_GENERATED)
        },
        isDerivedParameter = function(parameterName) {
            return(self$.getParameterType(parameterName) == C_PARAM_DERIVED)
        },
        isUndefinedParameter = function(parameterName) {
            return(self$.getParameterType(parameterName) == C_PARAM_TYPE_UNKNOWN)
        },
        isNotApplicableParameter = function(parameterName) {
            return(self$.getParameterType(parameterName) == C_PARAM_NOT_APPLICABLE)
        },
        .getInputParameters = function() {
            params <- self$.getParametersOfOneGroup(c(C_PARAM_USER_DEFINED, C_PARAM_DEFAULT_VALUE))
            return(params)
        },
        .getUserDefinedParameters = function() {
            return(self$.getParametersOfOneGroup(C_PARAM_USER_DEFINED))
        },
        .getDefaultParameters = function() {
            return(self$.getParametersOfOneGroup(C_PARAM_DEFAULT_VALUE))
        },
        .getGeneratedParameters = function() {
            return(self$.getParametersOfOneGroup(C_PARAM_GENERATED))
        },
        .getDerivedParameters = function() {
            return(self$.getParametersOfOneGroup(C_PARAM_DERIVED))
        },
        .getUndefinedParameters = function() {
            return(self$.getParametersOfOneGroup(C_PARAM_TYPE_UNKNOWN))
        },
        .getParameterValueIfUserDefinedOrDefault = function(parameterName) {
            if (self$isUserDefinedParameter(parameterName) || self$isDefaultParameter(parameterName)) {
                return(self[[parameterName]])
            }
            
            tryCatch({
                parameterType <- .getClassName(self[[parameterName]])
            }, error = function(e) {
                parameterType <- "unknown"
            })
        
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
                parameterNames <- names(self$.parameterTypes[self$.parameterTypes == parameterType])
            } else {
                parameterNames <- names(self$.parameterTypes[which(self$.parameterTypes %in% parameterType)])
            }
            parametersToShow <- self$.getParametersToShow()
            if (is.null(parametersToShow) || length(parametersToShow) == 0) {
                return(parameterNames)
            }

            return(parametersToShow[parametersToShow %in% parameterNames])
        },
        .showParameterType = function(parameterName) {
            if (!self$.showParameterTypeEnabled) {
                return("  ")
            }

            return(paste0("[", self$.getParameterType(parameterName), "]"))
        },
        .showAllParameters = function(consoleOutputEnabled = TRUE) {
            parametersToShow <- self$.getVisibleFieldNamesOrdered()
            for (parameter in parametersToShow) {
                self$.showParameter(parameter,
                    showParameterType = TRUE,
                    consoleOutputEnabled = consoleOutputEnabled
                )
            }
        },
        .getVisibleFieldNamesOrdered = function() {
            visibleFieldNames <- self$.getVisibleFieldNames()

            parametersToShowSorted <- self$.getParametersToShow()
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
                self$.cat("Technical developer summary of the ", self$.toString(), " object (",
                    methods::classLabel(class(self)), "):\n\n",
                    sep = "", heading = 1,
                    consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showAllParameters(consoleOutputEnabled = consoleOutputEnabled)
                self$.showParameterTypeDescription(consoleOutputEnabled = consoleOutputEnabled)
            } else {
                stop(
                    C_EXCEPTION_TYPE_RUNTIME_ISSUE,
                    "method '.show()' is not implemented in class '", .getClassName(self), "'"
                )
            }
        },
        .showParametersOfOneGroup = function(parameters, title,
                orderByParameterName = TRUE, consoleOutputEnabled = TRUE) {
            output <- ""
            if (is.null(parameters) || length(parameters) == 0 || all(is.na(parameters))) {
                if (!missing(title) && !is.null(title) && !is.na(title) && consoleOutputEnabled) {
                    output <- paste0(title, ": not available\n\n")
                    self$.cat(output, heading = 2, consoleOutputEnabled = consoleOutputEnabled)
                }
                invisible(output)
            } else {
                if (orderByParameterName) {
                    parameters <- sort(parameters)
                }

                if (!missing(title) && !is.null(title) && !is.na(title)) {
                    output <- paste0(title, ":\n")
                    self$.cat(output, heading = 2, consoleOutputEnabled = consoleOutputEnabled)
                }
                for (parameterName in parameters) {
                    output <- paste0(output, self$.showParameter(parameterName,
                        consoleOutputEnabled = consoleOutputEnabled
                    ))
                }
                self$.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
                output <- paste0(output, "\n")
                invisible(output)
            }
        },
        .showParameter = function(parameterName, showParameterType = FALSE, consoleOutputEnabled = TRUE) {
            tryCatch(
                {
                    params <- .getParameterValueFormatted(obj = self, parameterName = parameterName)
                    if (is.null(params) || !is.list(params)) {
                        return(invisible(""))
                    }

                    if (!is.null(names(params)) && "paramValue" %in% names(params)) {
                        return(self$.showParameterSingle(
                            param = params,
                            parameterName = parameterName,
                            showParameterType = showParameterType,
                            consoleOutputEnabled = consoleOutputEnabled
                        ))
                    }
                    
                    output <- ""
                    for (i in seq_len(length(params))) {
                        param <- params[[i]]
                        category <- NULL
                        parts <- strsplit(param$paramName, "$", fixed = TRUE)[[1]]
                        if (length(parts) == 2) {
                            parameterName <- parts[1]
                            param$paramName <- parameterName

                            category <- parts[2]
                            
                            categoryCaption <- .getParameterCaption(category, self[[parameterName]])
                            if (is.null(categoryCaption)) {
                                categoryCaption <- paste0("%", category, "%")
                            }
                            category <- categoryCaption
                        }
                        outputPart <- self$.showParameterSingle(
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
            if (is.null(param) || is.null(param$paramValue) || length(param$paramValue) == 0) {
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
                                    output <- paste0(output, self$.showParameterFormatted(
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

                                output <- paste0(output, self$.showParameterFormatted(
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
                        output <- self$.showParameterFormatted(
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
            tryCatch({
                d <- regexpr("\\..+\\$", parameterName)
                if (d[1] != 1) {
                    return(list(
                        parameterName = parameterName,
                        paramValue = base::get(parameterName, envir = self)
                    ))
                }
                
                index <- attr(d, "match.length")
                objectName <- substr(parameterName, 1, index - 1)
                parameterName <- substr(parameterName, index + 1, nchar(parameterName))
                obj <- base::get(objectName, envir = self)
                paramValue <- base::get(parameterName, envir = obj)
                #paramValue <- self[[objectName]][[parameterName]]
            
                if (objectName == ".closedTestResults" && parameterName == "rejected") {
                    paramValueLogical <- as.logical(paramValue)
                    if (is.matrix(paramValue)) {
                        paramValueLogical <- matrix(paramValueLogical, ncol = ncol(paramValue))
                    }
                    paramValue <- paramValueLogical
                }
                return(list(parameterName = parameterName, paramValue = paramValue))
            }, error = function(e) {
                if (consoleOutputEnabled) {
                    warning("Failed to extract parameter name and value from ", sQuote(parameterName), ": ", e$message)
                }
                return(list(parameterName = parameterName, paramValue = ""))
            })
        },
        .showUnknownParameters = function(consoleOutputEnabled = TRUE) {
            params <- self$.getUndefinedParameters()
            if (length(params) > 0) {
                self$.showParametersOfOneGroup(params, "ISSUES (parameters with undefined type)",
                    consoleOutputEnabled = consoleOutputEnabled
                )
            }
        },
        .showParameterFormatted = function(
                paramName, 
                paramValue, 
                ..., 
                paramValueFormatted = NA_character_,
                showParameterType = FALSE, 
                category = NULL, 
                matrixRow = NA_integer_, 
                consoleOutputEnabled = TRUE,
                paramNameRaw = NA_character_, 
                numberOfCategories = NA_integer_) {
                
            paramCaption <- NULL
            if (!is.na(paramNameRaw)) {
                paramCaption <- .getParameterCaption(paramNameRaw, self) 
            }
            if (is.null(paramCaption)) {
                paramCaption <- .getParameterCaption(paramName, self)
            }
            if (is.null(paramCaption)) {
                paramCaption <- paste0("%", paramName, "%")
            }
            if (!is.null(category) && !is.na(category)) {
                if (.isMultiArmSimulationResults(self) && paramName == "singleEventsPerArmAndStage") {
                    if (!inherits(self, "SimulationResultsEnrichmentSurvival") &&
                            !is.na(numberOfCategories) && numberOfCategories == category) {
                        category <- "control"
                    }
                    paramCaption <- paste0(paramCaption, " {", category, "}")
                } else if (paramName == "effectList") {
                    paramCaption <- paste0(paramCaption, " [", category, "]")
                } else if (.isEnrichmentSimulationResults(self)) {
                    categoryCaption <- .getCategoryCaptionEnrichment(self, paramName, category)
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
                if (.isMultiArmAnalysisResults(self) && paramName %in%
                        c(
                            "conditionalErrorRate", "secondStagePValues",
                            "adjustedStageWisePValues", "overallAdjustedTestStatistics"
                        )) {
                    treatments <- self$.closedTestResults$.getHypothesisTreatmentArmVariants()[matrixRow]
                    paramCaption <- paste0(
                        "Treatment", ifelse(grepl(",", treatments), "s", ""), " ",
                        treatments, " vs. control"
                    )
                } else if (.isEnrichmentAnalysisResults(self) || .isEnrichmentStageResults(self) ||
                        (inherits(self, "ClosedCombinationTestResults") && isTRUE(self$.enrichment))) {
                    if (paramName %in% c(
                            "indices", "conditionalErrorRate", "secondStagePValues",
                            "adjustedStageWisePValues", "overallAdjustedTestStatistics", "rejectedIntersections"
                        )) {
                        if (.isEnrichmentAnalysisResults(self)) {
                            populations <- self$.closedTestResults$.getHypothesisPopulationVariants()[matrixRow]
                        } else if (inherits(self, "ClosedCombinationTestResults")) {
                            populations <- self$.getHypothesisPopulationVariants()[matrixRow]
                        } else {
                            stop(
                                C_EXCEPTION_TYPE_RUNTIME_ISSUE, "only ClosedCombinationTestResults ",
                                "supports function .getHypothesisPopulationVariants() (object is ", .getClassName(self), ")"
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
                } else if (.isMultiArmAnalysisResults(self) || grepl("StageResultsMultiArm", .getClassName(self)) ||
                        (inherits(self, "SimulationResults") && paramName == "effectMatrix") ||
                        (inherits(self, "ClosedCombinationTestResults") &&
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
                    self$.getParameterType(paramName) == C_PARAM_USER_DEFINED,
                    ifelse(.isCppCode(paramValueFormatted), "user defined (C++)", "user defined"),
                    "default"
                )
            }
            prefix <- ifelse(showParameterType, self$.showParameterType(paramName), "")
            variableNameFormatted <- .getFormattedVariableName(
                name = paramCaption,
                n = self$.getNChar(), prefix = prefix
            )

            output <- paste(variableNameFormatted, paramValueFormatted, "\n")
            self$.cat(output, consoleOutputEnabled = consoleOutputEnabled)
            invisible(output)
        },
        .getNChar = function() {
            fieldNames <- self$.getVisibleFieldNames()
            if (length(fieldNames) == 0) {
                return(40)
            }

            fieldCaptions <- character()
            for (parameterName in fieldNames) {
                fieldCaptions <- c(fieldCaptions, .getParameterCaption(parameterName, self))
            }
            return(min(40, max(nchar(fieldCaptions))) + 4)
        },
        .showParameterTypeDescription = function(consoleOutputEnabled = consoleOutputEnabled) {
            self$.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
            self$.cat("Legend:\n", heading = 2, consoleOutputEnabled = consoleOutputEnabled)
            self$.cat("  ", C_PARAM_USER_DEFINED, ": user defined\n", consoleOutputEnabled = consoleOutputEnabled)
            self$.cat("  ", C_PARAM_DERIVED, ": derived value\n", consoleOutputEnabled = consoleOutputEnabled)
            self$.cat("  ", C_PARAM_DEFAULT_VALUE, ": default value\n", consoleOutputEnabled = consoleOutputEnabled)
            self$.cat("  ", C_PARAM_GENERATED, ": generated/calculated value\n", consoleOutputEnabled = consoleOutputEnabled)
            self$.cat("  ", C_PARAM_NOT_APPLICABLE, ": not applicable or hidden\n", consoleOutputEnabled = consoleOutputEnabled)
        },
        .printAsDataFrame = function(parameterNames, niceColumnNamesEnabled = FALSE,
                includeAllParameters = FALSE, handleParameterNamesAsToBeExcluded = FALSE,
                lineBreakEnabled = FALSE) {
            dataFrame <- .getAsDataFrame(
                parameterSet = self,
                parameterNames = parameterNames,
                niceColumnNamesEnabled = niceColumnNamesEnabled,
                includeAllParameters = includeAllParameters,
                handleParameterNamesAsToBeExcluded = handleParameterNamesAsToBeExcluded,
                returnParametersAsCharacter = TRUE
            )
            result <- as.matrix(dataFrame)
            print(result, quote = FALSE, right = FALSE)
        },
        .getNumberOfRows = function(parameterNames) {
            numberOfRows <- 1
            for (parameterName in parameterNames) {
                parameterValues <- self[[parameterName]]
                if (is.vector(parameterValues) && length(parameterValues) > numberOfRows) {
                    numberOfRows <- length(parameterValues)
                } else if (is.matrix(parameterValues) && 
                        (nrow(parameterValues) == 1 || ncol(parameterValues) == 1) &&
                        length(parameterValues) > numberOfRows) {
                    numberOfRows <- length(parameterValues)
                }
            }
            return(numberOfRows)
        },
        .containsMultidimensionalParameters = function(parameterNames) {
            for (parameterName in parameterNames) {
                parameterValues <- self[[parameterName]]
                if (!is.null(parameterValues) && is.matrix(parameterValues) &&
                        nrow(parameterValues) > 0 && ncol(parameterValues) > 0) {
                    return(TRUE)
                }
            }
            return(FALSE)
        },
        .getMultidimensionalNumberOfStages = function(parameterNames) {
            if (!is.null(self[[".design"]])) {
                return(self$.design$kMax)
            }

            n <- 1
            for (parameterName in parameterNames) {
                parameterValues <- self[[parameterName]]
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
                parameterValues <- self[[parameterName]]
                if (!is.null(parameterValues) && !is.matrix(parameterValues) &&
                        length(parameterValues) == numberOfVariants &&
                        parameterName %in% C_VARIABLE_DESIGN_PLAN_PARAMETERS &&
                        self$.getParameterType(parameterName) == C_PARAM_USER_DEFINED) {
                    return(parameterName)
                }
            }

            # search for default values
            for (parameterName in parameterNames) {
                parameterValues <- self[[parameterName]]
                if (!is.null(parameterValues) && !is.matrix(parameterValues) &&
                        length(parameterValues) == numberOfVariants &&
                        parameterName %in% C_VARIABLE_DESIGN_PLAN_PARAMETERS &&
                        self$.getParameterType(parameterName) == C_PARAM_DEFAULT_VALUE) {
                    return(parameterName)
                }
            }

            return(NULL)
        },
        .getDataFrameColumnCaption = function(parameterName, niceColumnNamesEnabled) {
            if (length(parameterName) == 0 || parameterName == "") {
                stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'parameterName' must be a valid parameter name")
            }

            if (!niceColumnNamesEnabled) {
                return(parameterName)
            }

            tableColumnName <- .getParameterCaption(parameterName, self, tableOutputEnabled = TRUE)
            return(ifelse(!is.null(tableColumnName), tableColumnName, parameterName))
        },
        .getUnidimensionalNumberOfStages = function(parameterNames) {
            kMax <- self[["kMax"]]
            if (is.null(kMax) && !is.null(self[[".design"]])) {
                kMax <- self[[".design"]][["kMax"]]
            }
            if (!is.null(kMax) && length(kMax) == 1 && is.integer(kMax)) {
                return(kMax)
            }

            n <- 1
            for (parameterName in parameterNames) {
                parameterValues <- self[[parameterName]]
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
                    formatFunctionName <- .getParameterFormatFunction(parameterName, self)
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
                    ), parameterName, .getClassName(self), e)
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
            if (!is.list(x) && !inherits(x, "Dictionary")) {
                stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'x' must be a list or Dictionary (is ", .getClassName(x), ")")
            }

            if (!is.character(listEntryNames)) {
                stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'listEntryNames' must be a character vector")
            }

            if (inherits(x, "Dictionary")) {
                return(getDictionarySubset(x, listEntryNames))
            }

            return(x[which(names(x) %in% listEntryNames)])
        }
    )
)

.getMultidimensionalNumberOfVariants <- function(parameterSet, parameterNames) {
    if (!is.null(parameterSet[["effectList"]])) {
        effectMatrixName <- .getSimulationEnrichmentEffectMatrixName(parameterSet)
        return(nrow(parameterSet$effectList[[effectMatrixName]]))
    }

    parameterNames <- parameterNames[!(parameterNames %in% c(
        "accrualTime", 
        "accrualIntensity",
        "plannedSubjects", 
        "plannedEvents",
        "minNumberOfSubjectsPerStage", 
        "maxNumberOfSubjectsPerStage",
        "minNumberOfEventsPerStage", 
        "maxNumberOfEventsPerStage",
        "piecewiseSurvivalTime", 
        "lambda2", 
        "adaptations",
        "adjustedStageWisePValues", 
        "overallAdjustedTestStatistics",
        "plannedCalendarTime",
        "doseLevels",
        "stDev"
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
                if (.isMultiHypothesesObject(parameterSet)) {
                    if (nrow(parameterValues) > n && ncol(parameterValues) > 0) {
                        n <- nrow(parameterValues)
                    }
                } else if (nrow(parameterValues) > 0 && ncol(parameterValues) > n) {
                    n <- ncol(parameterValues)
                }
            } else if (length(parameterValues) > n &&
                    !.isMultiHypothesesObject(parameterSet)) {
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

        if (.isMultiHypothesesObject(parameterSet)) {
            if (length(parameterValues) == numberOfStages) {
                return(as.vector(sapply(FUN = rep, X = parameterValues, times = numberOfVariants)))
            }
        }

        if (length(parameterValues) == numberOfVariants) {
            return(rep(parameterValues, numberOfStages))
        }

        if (length(parameterValues) == numberOfStages &&
                parameterName %in% c(
                    "plannedEvents", 
                    "plannedSubjects",
                    "minNumberOfEventsPerStage", 
                    "maxNumberOfEventsPerStage",
                    "minNumberOfSubjectsPerStage", 
                    "maxNumberOfSubjectsPerStage",
                    "allocationRatioPlanned"
                )) {
            values <- c()
            for (stage in 1:numberOfStages) {
                values <- c(values, rep(parameterValues[stage], numberOfVariants))
            }
            return(values)
        }

        if (parameterName %in% c(
                "accrualTime", 
                "accrualIntensity",
                "plannedEvents", 
                "plannedSubjects",
                "minNumberOfEventsPerStage", 
                "maxNumberOfEventsPerStage",
                "minNumberOfSubjectsPerStage", 
                "maxNumberOfSubjectsPerStage",
                "piecewiseSurvivalTime", 
                "lambda2"
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
    if (.isMultiHypothesesObject(parameterSet)) {
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
        mandatoryParameterNames) {
    numberOfVariants <- .getMultidimensionalNumberOfVariants(parameterSet, parameterNames)
    numberOfStages <- parameterSet$.getMultidimensionalNumberOfStages(parameterNames)

    stagesCaption <- parameterSet$.getDataFrameColumnCaption("stages", niceColumnNamesEnabled)

    dataFrame <- data.frame(
        stages = sort(rep(1:numberOfStages, numberOfVariants))
    )
    names(dataFrame) <- stagesCaption

    if (.isEnrichmentObject(parameterSet)) {
        populations <- character()
        for (i in 1:numberOfVariants) {
            populations <- c(populations, ifelse(i == numberOfVariants, "F", paste0("S", i)))
        }
        dataFrame$populations <- rep(populations, numberOfStages)
        populationsCaption <- parameterSet$.getDataFrameColumnCaption(
            "populations", niceColumnNamesEnabled
        )
        names(dataFrame) <- c(stagesCaption, populationsCaption)
    }

    variedParameter <- parameterSet$.getVariedParameter(parameterNames, numberOfVariants)
    tryCatch(
        {
            if (!is.null(variedParameter) && variedParameter != "stages") {
                variedParameterCaption <- parameterSet$.getDataFrameColumnCaption(
                    variedParameter, niceColumnNamesEnabled
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

    usedParameterNames <- character()
    for (parameterName in parameterNames) {
        tryCatch(
            {
                if (!(parameterName %in% c("stages", "adaptations", "effectList", "doseLevels", "plannedCalendarTime", "stDev")) &&
                        !grepl("Function$", parameterName) &&
                        (is.null(variedParameter) || parameterName != variedParameter)) {
                    columnValues <- .getDataFrameColumnValues(
                        parameterSet, parameterName,
                        numberOfVariants, numberOfStages,
                        includeAllParameters, mandatoryParameterNames
                    )
                    if (!is.null(columnValues)) {
                        columnCaption <- parameterSet$.getDataFrameColumnCaption(
                            parameterName, niceColumnNamesEnabled
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
                                extraParameter, niceColumnNamesEnabled
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
        includeAllParameters, returnParametersAsCharacter) {
    numberOfStages <- parameterSet$.getUnidimensionalNumberOfStages(parameterNames)
    dataFrame <- NULL
    for (parameterName in parameterNames) {
        tryCatch(
            {
                parameterCaption <- parameterName
                if (niceColumnNamesEnabled) {
                    parameterCaption <- .getParameterCaption(parameterName, parameterSet, tableOutputEnabled = TRUE)
                }
                if (is.null(parameterCaption)) {
                    parameterCaption <- parameterName
                }
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
                            !R6::is.R6(parameterValues) &&
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
        mandatoryParameterNames = character()) {
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

    parametersToIgnore <- character()
    if (!is.null(parameterSet[[".piecewiseSurvivalTime"]]) &&
            parameterSet$.piecewiseSurvivalTime$piecewiseSurvivalEnabled) {
        parametersToIgnore <- c(
            parametersToIgnore,
            "lambda1", "lambda2", "median1", "median2",
            "pi1", "pi2", "piecewiseSurvivalTime"
        )
        parametersToIgnore <- intersect(parametersToIgnore, parameterNames)
    }

    if (parameterSet$isGeneratedParameter("hazardRatio") &&
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

    if (parameterSet$.containsMultidimensionalParameters(parameterNames) ||
            (.isTrialDesignPlanCountData(parameterSet) && length(parameterSet$theta) > 1)) {
        return(.addDelayedInformationRates(.getAsDataFrameMultidimensional(
            parameterSet, parameterNames, niceColumnNamesEnabled,
            includeAllParameters, returnParametersAsCharacter,
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
        includeAllParameters, returnParametersAsCharacter
    )))
}

.getCategoryCaptionEnrichment <- function(parameterSet, parameterName, categoryNumber) {
    categoryCaption <- categoryNumber
    if (parameterName %in% c("sampleSizes", "singleEventsPerSubsetAndStage")) {
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
#' @param x The \code{\link{FieldSet}} object to print.
#' @param markdown If \code{TRUE}, the object \code{x} will be printed using markdown syntax;
#'        normal representation will be used otherwise (default is \code{FALSE})
#' @inheritParams param_three_dots
#'
#' @details
#' Prints the parameters and results of a field set.
#'
#' @export
#'
#' @keywords internal
#'
print.FieldSet <- function(x, ..., markdown = NA) {
    sysCalls <- sys.calls()
    
    if (is.na(markdown)) {
        markdown <- .isMarkdownEnabled("print")
    }
    
    if (isTRUE(markdown)) {
        if (.isPrintCall(sysCalls)) {
            result <- paste0(utils::capture.output(x$.catMarkdownText()), collapse = "\n")
            return(knitr::asis_output(result))
        }
        
        attr(x, "markdown") <- TRUE
        queue <- attr(x, "queue")
        if (is.null(queue)) {
            queue <- list()
        }
        queue[[length(queue) + 1]] <- x
        attr(x, "queue") <- queue
        return(invisible(x))
    }
    
    x$show()
    return(invisible(x))
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
        dfStageResults <- dfStageResults[!is.na(dfStageResults[, 
            grep("(test statistic)|(testStatistics)", colnames(dfStageResults))]), ]
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
#' @param output The output parts, default is \code{"all"}.
#' @param printObject Show also the print output after the summary, default is \code{FALSE}.
#' @param sep The separator line between the summary and the optional print output, default is \code{"\n\n-----\n\n"}.
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
summary.ParameterSet <- function(object, ...,
        type = 1,
        digits = NA_integer_,
        output = c("all", "title", "overview", "body"),
        printObject = FALSE,
        sep = NA_character_) {
        
    .warnInCaseOfUnknownArguments(functionName = "summary", ignore = c("printObject"), ...)
    .assertIsSingleCharacter(sep, "sep", naAllowed = TRUE)
    if (is.na(sep)) {
        sep <- .getMarkdownPlotPrintSeparator()
    }
    
    base::attr(object, "printObject") <- printObject
    base::attr(object, "printObjectSeparator") <- sep

    if (type == 1 && inherits(object, "SummaryFactory")) {
        return(object)
    }

    if (type == 1 && (
            inherits(object, "TrialDesign") ||
                inherits(object, "TrialDesignPlan") ||
                inherits(object, "SimulationResults") ||
                inherits(object, "AnalysisResults") ||
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
    if (.isTrialDesignPlan(object)) {
        parametersToShow <- NULL
    } else {
        parametersToShow <- object$.getParametersToShow()
        for (parameter in parametersToShow) {
            if (length(object[[parameter]]) == 1) {
                parametersToShow <- parametersToShow[parametersToShow != parameter]
            }
        }
    }
    
    object$.printAsDataFrame(parameterNames = parametersToShow, niceColumnNamesEnabled = TRUE)
    invisible(object)
}

.isPrintCall <- function(sysCalls) {
    if (is.null(sysCalls) || length(sysCalls) == 0) {
        return(TRUE)
    }
    
    for (i in length(sysCalls):1) {
        callObj <- sysCalls[[i]]
        if (!is.null(callObj) && is.call(callObj)) {
            callText <- capture.output(print(callObj))            
            if (any(grepl("(plot|summary)\\(", callText))) {
                return(FALSE)
            }
        }
    }
    return(TRUE)
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
print.ParameterSet <- function(x, ..., markdown = NA) {
    sysCalls <- sys.calls()
    
    if (is.na(markdown)) {
        markdown <- .isMarkdownEnabled("print")
    }
    
    showStatistics <- NULL
    if (inherits(x, "SimulationResults")) {
        showStatistics <- .getOptionalArgument("showStatistics", ...)
        if (!is.null(showStatistics)) {
            .assertIsSingleLogical(showStatistics, "showStatistics")
        }
    }

    if (isTRUE(markdown)) {
        if (.isPrintCall(sysCalls)) {
            if (!is.null(showStatistics)) {
                result <- paste0(utils::capture.output(x$.catMarkdownText(showStatistics = showStatistics)), collapse = "\n")
            } else {
                result <- paste0(utils::capture.output(x$.catMarkdownText()), collapse = "\n")
            }
            return(knitr::asis_output(result))
        }
        
        attr(x, "markdown") <- TRUE
        queue <- attr(x, "queue")
        if (is.null(queue)) {
            queue <- list()
        }
        queue[[length(queue) + 1]] <- x
        attr(x, "queue") <- queue
        return(invisible(x))
    }
    
    if (!is.null(showStatistics)) {
        x$show(showStatistics = showStatistics)
    } else {
        x$show()
    }
    return(invisible(x))
}

#' 
#' @rdname fetch.ParameterSet
#' 
#' @export 
#' 
obtain <- function(x, ..., output) UseMethod("obtain")

#'
#' @rdname fetch.ParameterSet
#' 
#' @export 
#' 
obtain.ParameterSet <- function(x, ..., output = c("named", "labeled", "value", "list")) {
    fCall <- match.call(expand.dots = TRUE)
    output <- match.arg(output)
    return(.fetchParameterSetValues(x, fCall, output))   
}

#' 
#' @rdname fetch.ParameterSet
#' 
#' @export 
#' 
fetch <- function(x, ..., output) UseMethod("fetch")

#'
#' @title
#' Extract a single parameter
#' 
#' @description
#' Fetch a parameter from a parameter set.
#' 
#' @param x The \code{\link{ParameterSet}} object to fetch from.
#' @param ... One or more variables specified as: 
#'  - a literal variable name 
#'  - a positive integer, giving the position counting from the left 
#'  - a negative integer, giving the position counting from the right. 
#' The default returns the last parameter.  
#' This argument is taken by expression and supports quasiquotation (you can unquote column names and column locations).
#' @param output A character defining the output type as follows:
#'  - "named" (default) returns the named value if the value is a single value, the value inside a named list otherwise
#'  - "value" returns only the value itself
#'  - "list" returns the value inside a named list
#' 
#' @template examples_fetch_parameter_from_result
#' 
#' @export 
#' 
fetch.ParameterSet <- function(x, ..., output = c("named", "labeled", "value", "list")) {
    fCall <- match.call(expand.dots = TRUE)
    output <- match.arg(output)
    return(.fetchParameterSetValues(x, fCall, output))   
}

.getParameterSetVarIndices <- function(var, x) {
    if (!is.character(var) && !is.call(var)) {
        return(var)
    }
    
    if (is.character(var) && var %in% names(x)) {
        return(var)
    }
    
    if (is.call(var) ) {
        var <- as.character(var)
    }
    
    result <- try(eval(parse(text = var)), silent = TRUE)
    if (methods::is(result, "try-error")) {
        return(var)
    }
    
    return(result)
}

.fetchParameterSetValues <- function(x, fCall, output) {
    .assertIsParameterSetClass(x, "x")
    vars <- c()
    for (i in 2:length(fCall)) {
        varValue <- fCall[[i]]
        varName <- names(fCall)[i]
        if (identical(varName, "")) {
            varName <- deparse(varValue)
        }
        if (!(varName %in% c("x", "output"))) {
            if (is.pairlist(varValue)) {
                for (j in 1:length(varValue)) {
                    var <- .getParameterSetVarNameOrIndex(
                        varName = deparse(varValue[[j]]), 
                        var = .getParameterSetVarIndices(varValue[[j]], x))
                    vars <- c(vars, var)
                }
            } else {
                var <- .getParameterSetVarNameOrIndex(varName, varValue)
                var <- .getParameterSetVarIndices(var, x)
                vars <- c(vars, var)
            }
        }
    }
    
    if (length(vars) == 0) {
        vars <- -1 
    }
    
    if (length(vars) == 1) {
        return(.getParameterSetValue(x = x, var = vars[[1]], output = output))
    }
    
    results <- list()
    for (var in vars) {
        result <- .getParameterSetValue(x = x, var = var, output = output)
        if (is.list(result) || (!is.null(names(result)) && !all(identical(names(result), "")))) {
            results <- c(results, result)
        } else {
            results[[length(results) + 1]] <- result
        }
    }
    return(results)  
}

.getParameterSetVar <- function(fCall, var) {
    varName <- deparse(fCall$var)
    return(.getParameterSetVarNameOrIndex(varName, var))
}

#' 
#' @examples 
#' .getParameterSetVarNameOrIndex("a", "a")
#' .getParameterSetVarNameOrIndex("a", a)
#' b <- 1
#' .getParameterSetVarNameOrIndex("b", b)
#' 
#' @noRd 
#' 
.getParameterSetVarNameOrIndex <- function(varName, var) {
    if (identical(varName, "NULL")) {
        return(var)
    }
    
    varNameExists <- !is.null(varName) && exists(varName)
    if (varNameExists) {
        return(var)
    }
    
    if (grepl("\"|'", varName)) {
        varName <- gsub('"', "", varName)
        varName <- gsub("'", "", varName)
        return(varName)
    }
    
    var <- suppressWarnings(as.integer(varName))
    if (!is.na(var)) {
        return(var)
    }
    
    varName <- gsub('"', "", varName)
    varName <- gsub("'", "", varName)
    return(varName)
}

.getParameterSetValue <- function(..., x, var, output) {
    if (is.character(var) && 
            identical(as.character(suppressWarnings(as.integer(var))), var)) {
        var <- as.integer(var)
    }
    if (is.call(var)) {
        var <- as.character(var)
    }
    if (is.list(var) && length(var) == 1) {
        var <- var[[1]]
    }
    if (is.name(var)) {
        var <- as.character(var)
    }
    if (is.character(var)) {
        varRoot <- var
        varOriginal <- NA_character_
        if (!(var %in% names(x))) {
            var <- getParameterName(x, var)
            if (is.na(var) && !is.null(x[[".design"]])) {
                result <- getParameterName(x$.design, varRoot)
                if (!is.na(result)) {
                    var <- paste0(".design$", result)
                }
            }
            if (length(var) > 1) {
                var <- var[!grepl("^\\.", var)]
            }
            if (length(var) > 1) {
                var <- var[1]
            }
            if (length(var) == 1 && !(var %in% names(x))) {
                if (grepl("\\$", var)) {
                    parts <- strsplit(var, "\\$")[[1]]
                    if (length(parts) == 2) {
                        x <- x[[parts[1]]]
                        varOriginal <- var
                        var <- parts[2]
                    }
                } else if (!is.null(x[[".design"]])) {
                    x <- x$.design
                }
            }
        }
        if (length(var) == 0 || !(var %in% names(x))) {
            stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "variable ", sQuote(varRoot), " does not exist")
        }
        
        value <- x[[var]]
        
        if (output == "value") {
            return(value)
        }
        
        label <- var
        if (output == "labeled") {
            label <- getParameterCaption(x, var)
        } else if (!is.na(varOriginal)) {
            label <- varOriginal
        }
        
        if (output %in% c("named", "labeled") && is.vector(value) && length(value) <= 1) {
            names(value) <- label
            return(value)
        }
        
        result <- list(value = value)
        names(result) <- label
        return(result)
    }
    
    .assertIsSingleInteger(var, "var", validateType = FALSE)
    varNames <- names(x)
    .assertIsInClosedInterval(var, "var", lower = -length(varNames), upper = length(varNames))
    if (var == 0) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'var' (", var, ") must != 0")
    }
    if (var < 0) {
        var <- length(varNames) + var + 1
    }
    
    varName <- varNames[var]
    value <- x[[varName]]
    
    if (output == "value") {
        return(value)
    }
    
    label <- names(x)[var]
    if (output == "labeled") {
        label <- getParameterCaption(x, names(x)[var])
    }
    
    if (output %in% c("named", "labeled") && is.vector(value) && length(value) <= 1) {
        names(value) <- label
        return(value)
    }
    
    result <- list(value = value)
    names(result) <- label
    return(result)
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

#'
#' @title
#' Print Field Set in Markdown Code Chunks
#'
#' @description
#' The function `knit_print.FieldSet` is the default printing function for rpact result objects in knitr.
#' The chunk option `render` uses this function by default.
#' To fall back to the normal printing behavior set the chunk option `render = normal_print`.
#' For more information see \code{\link[knitr]{knit_print}}.
#'
#' @param x A \code{FieldSet}.
#' @param  ... Other arguments (see \code{\link[knitr]{knit_print}}).
#'
#' @details
#' Generic function to print a field set in Markdown.
#' 
#' @template details_knit_print
#'
#' @keywords internal
#' 
#' @export
#'
knit_print.FieldSet <- function(x, ...) { 
    result <- paste0(utils::capture.output(x$.catMarkdownText()), collapse = "\n")
    return(knitr::asis_output(result))
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
#' 
#' @template details_knit_print
#'
#' @keywords internal
#' 
#' @export
#'
knit_print.ParameterSet <- function(x, ...) { 
    result <- paste0(utils::capture.output(x$.catMarkdownText()), collapse = "\n")
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
#' This function is deprecated and should no longer be used.
#' Manual use of kable() for rpact result objects is no longer needed, 
#' as the formatting and display will be handled automatically by the rpact package. 
#' Please remove any manual kable() calls from your code to avoid redundancy and potential issues. 
#' The results will be displayed in a consistent format automatically.
#' 
#' @name kableParameterSet
#' 
#' @keywords internal
#' 
#' @export
#'
kable.ParameterSet <- function(x, ...) {
    fCall <- match.call(expand.dots = FALSE)
    
    lastWarningTime <- getOption("rpact.deprecated.message.time.function.kable")
    if (is.null(lastWarningTime) || difftime(Sys.time(), lastWarningTime, units = "hours") > 8) {
        base::options("rpact.deprecated.message.time.function.kable" = Sys.time())
        .Deprecated(new = "",  
            msg = paste0("Manual use of kable() for rpact result objects is no longer needed, ",
                "as the formatting and display will be handled automatically by the rpact package"),
            old = "kable")
    }
    
    if (inherits(x, "ParameterSet")) {
        objName <- deparse(fCall$x)
        
        if (length(objName) > 0) {
            if (length(objName) > 1) {
                objName <- paste0(objName[1], "...")
            }
            if (grepl("^ *print\\(", objName)) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "kable(", objName, ") ",
                    "does not work correctly. ",
                    "Use ", sub("print", "kable", objName), " without 'print' ",
                    "instead or ", sub("\\)", ", markdown = TRUE)", objName)
                )
            }
        }

        return(knit_print.ParameterSet(x = x, ...))
    }

    .assertPackageIsInstalled("knitr")
    knitr::kable(x, ...)
}

#' 
#' @rdname kableParameterSet
#'
#' @keywords internal
#' 
#' @export 
#' 
kable.FieldSet <- function(x, ..., 
        enforceRowNames = TRUE, niceColumnNamesEnabled = TRUE) {
    .assertPackageIsInstalled("knitr")
    knitr::kable(as.matrix(x, 
        enforceRowNames = enforceRowNames, 
        niceColumnNamesEnabled = niceColumnNamesEnabled), ...)
}

#' 
#' @rdname kableParameterSet
#'
#' @keywords internal
#' 
#' @export 
#' 
kable.data.frame <- function(x, ...) {
    .assertPackageIsInstalled("knitr")
    knitr::kable(x, ...)
}

#' 
#' @rdname kableParameterSet
#'
#' @keywords internal
#' 
#' @export 
#' 
kable.table <- function(x, ...) {
    .assertPackageIsInstalled("knitr")
    knitr::kable(x, ...)
}

#' 
#' @rdname kableParameterSet
#'
#' @keywords internal
#' 
#' @export 
#' 
kable.matrix <- function(x, ...) {
    .assertPackageIsInstalled("knitr")
    knitr::kable(x, ...)
}

#' 
#' @rdname kableParameterSet
#'
#' @keywords internal
#' 
#' @export 
#' 
kable.array <- function(x, ...) {
    .assertPackageIsInstalled("knitr")
    knitr::kable(x, ...)
}

#' 
#' @rdname kableParameterSet
#'
#' @keywords internal
#' 
#' @export 
#' 
kable.numeric <- function(x, ...) {
    .assertPackageIsInstalled("knitr")
    knitr::kable(x, ...)
}

#' 
#' @rdname kableParameterSet
#'
#' @keywords internal
#' 
#' @export 
#' 
kable.character <- function(x, ...) {
    .assertPackageIsInstalled("knitr")
    knitr::kable(x, ...)
}

#' 
#' @rdname kableParameterSet
#'
#' @keywords internal
#' 
#' @export 
#' 
kable.logical <- function(x, ...) {
    .assertPackageIsInstalled("knitr")
    knitr::kable(x, ...)
}

#' 
#' @rdname kableParameterSet
#'
#' @keywords internal
#' 
#' @export 
#' 
kable <- function(x, ...) UseMethod("kable")
