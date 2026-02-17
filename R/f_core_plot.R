## |
## |  *Plot functions*
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

.addNumberToPlotCaption <- function(caption, type, numberInCaptionEnabled = FALSE) {
    if (!numberInCaptionEnabled) {
        return(caption)
    }

    return(paste0(caption, " [", type, "]"))
}

.getPlotCaption <- function(obj, type, numberInCaptionEnabled = FALSE, ..., stopIfNotFound = FALSE) {
    if (is.null(obj) || length(type) == 0) {
        return(NA_character_)
    }

    .assertIsSingleInteger(type, "type", validateType = FALSE)

    if (inherits(obj, "TrialDesignPlan")) {
        if (type == 1) {
            if (.isTrialDesignPlanSurvival(obj)) {
                return(.addNumberToPlotCaption("Boundaries Z Scale", type, numberInCaptionEnabled))
            } else {
                return(.addNumberToPlotCaption("Boundaries", type, numberInCaptionEnabled))
            }
        } else if (type == 2) {
            return(.addNumberToPlotCaption("Boundaries Effect Scale", type, numberInCaptionEnabled))
        } else if (type == 3) {
            return(.addNumberToPlotCaption("Boundaries p Values Scale", type, numberInCaptionEnabled))
        } else if (type == 4) {
            return(.addNumberToPlotCaption("Error Spending", type, numberInCaptionEnabled))
        }
    }

    if (.isMultiArmSimulationResults(obj)) {
        if (type == 1) {
            return(.addNumberToPlotCaption("Overall Success", type, numberInCaptionEnabled))
        } else if (type == 2) {
            return(.addNumberToPlotCaption("Success per Stage", type, numberInCaptionEnabled))
        } else if (type == 3) {
            return(.addNumberToPlotCaption("Selected Arms per Stage", type, numberInCaptionEnabled))
        } else if (type == 4) {
            return(.addNumberToPlotCaption(ifelse(obj$.design$kMax > 1,
                "Rejected Arms per Stage", "Rejected Arms"
            ), type, numberInCaptionEnabled))
        }
    } else if (.isEnrichmentSimulationResults(obj)) {
        if (type == 1) {
            return(.addNumberToPlotCaption("Overall Success", type, numberInCaptionEnabled))
        } else if (type == 2) {
            return(.addNumberToPlotCaption("Success per Stage", type, numberInCaptionEnabled))
        } else if (type == 3) {
            return(.addNumberToPlotCaption("Selected Populations per Stage", type, numberInCaptionEnabled))
        } else if (type == 4) {
            return(.addNumberToPlotCaption(ifelse(obj$.design$kMax > 1,
                "Rejected Populations per Stage", "Rejected Populations"
            ), type, numberInCaptionEnabled))
        }
    } else if (inherits(obj, "SimulationResults") && type == 4) {
        return(.addNumberToPlotCaption("Reject per Stage", type, numberInCaptionEnabled))
    }

    if (inherits(obj, "TrialDesignPlan") || inherits(obj, "SimulationResults")) {
        if (type == 5) {
            if (obj$.isSampleSizeObject()) {
                return(.addNumberToPlotCaption(
                    ifelse(
                        .isTrialDesignPlanSurvival(obj) || inherits(obj, "SimulationResultsSurvival"),
                        "Number of Events",
                        "Sample Size"
                    ), type, numberInCaptionEnabled
                ))
            } else {
                return(.addNumberToPlotCaption(
                    "Overall Power and Early Stopping",
                    type, numberInCaptionEnabled
                ))
            }
        } else if (type == 6) {
            return(.addNumberToPlotCaption(
                ifelse(
                    .isTrialDesignPlanSurvival(obj) || inherits(obj, "SimulationResultsSurvival"),
                    "Expected Number of Events and Power / Early Stop",
                    "Expected Sample Size and Power / Early Stop"
                ), type, numberInCaptionEnabled
            ))
        } else if (type == 7) {
            return(.addNumberToPlotCaption("Overall Power", type, numberInCaptionEnabled))
        } else if (type == 8) {
            return(.addNumberToPlotCaption("Overall Early Stopping", type, numberInCaptionEnabled))
        } else if (type == 9) {
            if (.isTrialDesignPlanSurvival(obj) ||
                    inherits(obj, "SimulationResultsSurvival")) {
                return(.addNumberToPlotCaption("Expected Number of Events", type, numberInCaptionEnabled))
            } else {
                return(.addNumberToPlotCaption("Expected Sample Size", type, numberInCaptionEnabled))
            }
        } else if (type == 10) {
            return(.addNumberToPlotCaption("Study Duration", type, numberInCaptionEnabled))
        } else if (type == 11) {
            return(.addNumberToPlotCaption("Expected Number of Subjects", type, numberInCaptionEnabled))
        } else if (type == 12) {
            return(.addNumberToPlotCaption("Analysis Time", type, numberInCaptionEnabled))
        } else if (type == 13) {
            return(.addNumberToPlotCaption("Cumulative Distribution Function", type, numberInCaptionEnabled))
        } else if (type == 14) {
            return(.addNumberToPlotCaption("Survival Function", type, numberInCaptionEnabled))
        }
    } else if (inherits(obj, "TrialDesign") || inherits(obj, "TrialDesignSet")) {
        if (type == 1) {
            return(.addNumberToPlotCaption("Boundaries", type, numberInCaptionEnabled))
        } else if (type == 3) {
            return(.addNumberToPlotCaption("Stage Levels", type, numberInCaptionEnabled))
        } else if (type == 4) {
            return(.addNumberToPlotCaption("Error Spending", type, numberInCaptionEnabled))
        } else if (type == 5) {
            return(.addNumberToPlotCaption("Power and Early Stopping", type, numberInCaptionEnabled))
        } else if (type == 6) {
            return(.addNumberToPlotCaption(
                "Average Sample Size and Power / Early Stop",
                type, numberInCaptionEnabled
            ))
        } else if (type == 7) {
            return(.addNumberToPlotCaption("Power", type, numberInCaptionEnabled))
        } else if (type == 8) {
            return(.addNumberToPlotCaption("Early Stopping", type, numberInCaptionEnabled))
        } else if (type == 9) {
            return(.addNumberToPlotCaption("Average Sample Size", type, numberInCaptionEnabled))
        }
    } else if (inherits(obj, "AnalysisResults")) {
        if (type == 1) {
            return(.addNumberToPlotCaption(C_PLOT_MAIN_CONDITIONAL_POWER_WITH_LIKELIHOOD, type, numberInCaptionEnabled))
        } else if (type == 2) {
            return(.addNumberToPlotCaption("Repeated Confidence Intervals", type, numberInCaptionEnabled))
        }
    } else if (inherits(obj, "StageResults")) {
        return(.addNumberToPlotCaption(C_PLOT_MAIN_CONDITIONAL_POWER_WITH_LIKELIHOOD, type, numberInCaptionEnabled))
    }

    if (stopIfNotFound) {
        stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "could not find plot caption for ", .getClassName(obj), " and type ", type)
    }

    return(NA_character_)
}

.getPlotTypeNumber <- function(type, x) {
    .assertIsValidPlotType(type, naAllowed = FALSE)

    if (is.character(type)) {
        if (length(type) == 1 && type == "all") {
            availablePlotTypes <- getAvailablePlotTypes(x)
            if (is.null(availablePlotTypes)) {
                stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "function 'getAvailablePlotTypes' not implemented for ", .getClassName(x))
            }
            return(availablePlotTypes)
        }

        types <- getAvailablePlotTypes(x, output = "numeric")
        captions <- tolower(getAvailablePlotTypes(x, output = "caption"))
        typeNumbers <- c()
        for (typeStr in type) {
            if (grepl("^\\d+$", typeStr)) {
                typeNumbers <- c(typeNumbers, as.integer(typeStr))
            } else {
                index <- pmatch(tolower(typeStr), captions)
                if (!is.na(index)) {
                    typeNumbers <- c(typeNumbers, types[index])
                } else {
                    index <- grep(tolower(typeStr), captions)
                    if (length(index) > 0) {
                        for (i in index) {
                            typeNumbers <- c(typeNumbers, types[i])
                        }
                    }
                }
            }
        }

        if (length(typeNumbers) > 0) {
            return(unique(typeNumbers))
        }

        message("Available plot types: ", .arrayToString(tolower(
            getAvailablePlotTypes(x, output = "caption")
        ), encapsulate = TRUE))
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'type' (", .arrayToString(type), ") could not be identified")
    }

    return(type)
}

.createPlotResultObject <- function(plotList, grid = 1) {
    .assertIsSingleInteger(grid, "grid", naAllowed = FALSE, validateType = FALSE)
    .assertIsInClosedInterval(grid, "grid", lower = 0, upper = 100)

    if (length(plotList) == 0) {
        if (grid == 0) {
            return(invisible(plotList))
        }

        return(plotList)
    }

    if (!inherits(plotList[[1]], "ggplot") || grid == 1) {
        return(plotList)
    }

    if (grid == 0) {
        for (p in plotList) {
            suppressMessages(print(p))
        }

        return(invisible(plotList))
    }

    if (length(plotList) > grid) {
        return(plotList)
    }

    plotCmd <- NA_character_
    if (grid > 1) {
        if ("ggpubr" %in% rownames(installed.packages())) {
            if (length(plotList) < 8 && length(plotList) %% 2 == 1) {
                plotCmd <- paste0(
                    "ggpubr::ggarrange(plotList[[1]], ",
                    "ggpubr::ggarrange(plotlist = plotList[2:", length(plotList), "]), ncol = 1)"
                )
            } else if (length(plotList) == 2) {
                plotCmd <- paste0("ggpubr::ggarrange(plotlist = plotList, ncol = 1)")
            } else {
                plotCmd <- paste0("ggpubr::ggarrange(plotlist = plotList)")
            }
        } else if ("gridExtra" %in% rownames(installed.packages())) {
            ncol <- ifelse(length(plotList) == 2, 1, 2)
            plotCmd <- paste0("gridExtra::grid.arrange(grobs = plotList, ncol = ", ncol, ")")
        } else if ("cowplot" %in% rownames(installed.packages())) {
            plotCmd <- "cowplot::plot_grid(plotlist = plotList)"
        } else {
            message(
                "Unable to create grid plot because neither 'ggpubr', 'gridExtra', nor 'cowplot' are installed. ",
                "Install one of these packages to enable grid plots"
            )
        }
    }
    if (!is.na(plotCmd)) {
        tryCatch(
            {
                return(eval(parse(text = plotCmd)))
            },
            error = function(e) {
                warning("Failed to create grid plot using command '", plotCmd, "': ", e$message)
            }
        )
    }

    return(plotList)
}

.printPlotShowSourceSeparator <- function(showSource, typeNumber, typeNumbers) {
    if (is.logical(showSource) && !showSource) {
        return(invisible())
    }

    if (length(typeNumbers) == 1) {
        return(invisible())
    }

    if (typeNumber == typeNumbers[length(typeNumbers)]) {
        return(invisible())
    }

    cat("--\n")
}

#' @rdname getAvailablePlotTypes
#' @export
plotTypes <- function(obj, output = c("numeric", "caption", "numcap", "capnum"),
        numberInCaptionEnabled = FALSE) {
    return(getAvailablePlotTypes(
        obj = obj, output = output,
        numberInCaptionEnabled = numberInCaptionEnabled
    ))
}

.isValidVariedParameterVectorForPlotting <- function(resultObject, plotType) {
    if (plotType > 12) {
        return(TRUE)
    }

    for (param in c("alternative", "pi1", "hazardRatio", "muMaxVector", "piMaxVector", "omegaMaxVector")) {
        if (!is.null(resultObject[[param]]) &&
                resultObject$.getParameterType(param) != C_PARAM_NOT_APPLICABLE &&
                (anyNA(resultObject[[param]]) ||
                    length(resultObject[[param]]) <= 1)) {
            return(FALSE)
        }
    }

    if (!is.null(resultObject[["hazardRatio"]]) && !is.null(resultObject[["overallReject"]]) &&
            resultObject$.getParameterType("hazardRatio") != C_PARAM_NOT_APPLICABLE &&
            resultObject$.getParameterType("overallReject") != C_PARAM_NOT_APPLICABLE &&
            length(resultObject$hazardRatio) > 0 &&
            length(resultObject$hazardRatio) != length(resultObject$overallReject)) {
        return(FALSE)
    }

    return(TRUE)
}

.removeInvalidPlotTypes <- function(resultObject, plotTypes, plotTypesToCheck) {
    if (is.null(plotTypes) || length(plotTypes) == 0) {
        return(integer(0))
    }

    validPlotTypes <- integer(0)
    for (plotType in plotTypes) {
        if (!(plotType %in% plotTypesToCheck)) {
            validPlotTypes <- c(validPlotTypes, plotType)
        } else if (.isValidVariedParameterVectorForPlotting(resultObject, plotType)) {
            validPlotTypes <- c(validPlotTypes, plotType)
        }
    }
    return(validPlotTypes)
}

#'
#' @title
#' Get Available Plot Types
#'
#' @description
#' Function to identify the available plot types of an object.
#'
#' @param obj The object for which the plot types shall be identified, e.g. produced by
#'        \code{\link[=getDesignGroupSequential]{getDesignGroupSequential()}} or \code{\link[=getSampleSizeMeans]{getSampleSizeMeans()}}.
#' @param output The output type. Can be one of \code{c("numeric", "caption", "numcap", "capnum")}.
#' @param numberInCaptionEnabled If \code{TRUE}, the number will be added to the
#'        caption, default is \code{FALSE}.
#'
#' @details
#' \code{plotTypes} and \code{getAvailablePlotTypes()} are equivalent, i.e.,
#' \code{plotTypes} is a short form of \code{getAvailablePlotTypes()}.
#'
#' \code{output}:
#' \enumerate{
#'   \item \code{numeric}: numeric output
#'   \item \code{caption}: caption as character output
#'   \item \code{numcap}:  list with number and caption
#'   \item \code{capnum}:  list with caption and number
#' }
#'
#' @return Returns a list if \code{option} is either \code{capnum} or \code{numcap}
#' or returns a vector that is of  character type for \code{option=caption} or
#' of numeric type for \code{option=numeric}.
#'
#' @examples
#' \dontrun{
#' design <- getDesignInverseNormal(kMax = 2)
#' getAvailablePlotTypes(design, "numeric")
#' plotTypes(design, "caption")
#' getAvailablePlotTypes(design, "numcap")
#' plotTypes(design, "capnum")
#' }
#'
#' @export
#'
getAvailablePlotTypes <- function(obj, output = c("numeric", "caption", "numcap", "capnum"),
        numberInCaptionEnabled = FALSE) {
    output <- match.arg(output)
    if (is.null(obj)) {
        if (output == "numeric") {
            return(NA_real_)
        }
        if (output == "caption") {
            return(NA_character_)
        }
        return(list())
    }

    types <- integer(0)
    if (inherits(obj, "TrialDesignPlan")) {
        if (obj$.design$kMax > 1) {
            types <- c(types, 1)
            if (!.isTrialDesignPlanCountData(obj)) {
                types <- c(types, 2)
            }
            if (!inherits(obj, "TrialDesignPlanCountData")) {
                types <- c(types, 3)
            }
            types <- c(types, 4)
        }
        if (obj$.isSampleSizeObject()) {
            if (!.isTrialDesignPlanCountData(obj) || length(obj[["theta"]]) > 1) {
                types <- c(types, 5)
            }
            if (.isTrialDesignPlanSurvival(obj)) {
                types <- c(types, 13, 14)
            }
        } else {
            if (obj$.design$kMax > 1) {
                types <- c(types, 5, 6)
            }
            types <- c(types, 7)
            if (obj$.design$kMax > 1) {
                types <- c(types, 8)
                if (!.isTrialDesignPlanCountData(obj) ||
                        obj$isGeneratedParameter("expectedNumberOfSubjectsH1")) {
                    types <- c(types, 9)
                }
            }
            if (.isTrialDesignPlanSurvival(obj)) {
                types <- c(types, 10:14)
            }
        }
        types <- .removeInvalidPlotTypes(obj, types, c(5:14))
    } else if (inherits(obj, "SimulationResults")) {
        if (grepl("Enrichment", .getClassName(obj)) && !.getSimulationEnrichmentEffectData(
                obj,
                validatePlotCapability = FALSE
            )$valid) {
            if (output == "numeric") {
                return(NA_real_)
            }
            if (output == "caption") {
                return(NA_character_)
            }
            return(list())
        }

        if (grepl("MultiArm|Enrichment", .getClassName(obj))) {
            types <- c(types, 1)
            if (obj$.design$kMax > 1) {
                types <- c(types, 2:3)
            }
        }
        types <- c(types, 4)
        if (!grepl("MultiArm", .getClassName(obj)) || obj$.design$kMax > 1) {
            types <- c(types, 5)
            if (!grepl("CountData", .getClassName(obj))) {
                types <- c(types, 6)
            }
        }
        types <- c(types, 7)
        if (obj$.design$kMax > 1) {
            types <- c(types, 8)
        }
        if (!grepl("(MultiArm|CountData)", .getClassName(obj)) || obj$.design$kMax > 1) {
            types <- c(types, 9)
        }
        if (inherits(obj, "SimulationResultsSurvival")) {
            types <- c(types, 10:14)
        }
        plotTypesToCheck <- c(4:14)
        if (grepl("MultiArm", .getClassName(obj))) {
            plotTypesToCheck <- c(1:14)
        }
        types <- .removeInvalidPlotTypes(obj, types, plotTypesToCheck)
    } else if (inherits(obj, "TrialDesign") || inherits(obj, "TrialDesignSet")) {
        design <- obj
        if (inherits(obj, "TrialDesignSet")) {
            design <- obj$getDesignMaster()
        }
        if (design$kMax > 1) {
            types <- c(types, 1, 3)
        }
        if (inherits(design, "TrialDesignFisher")) {
            types <- c(types, 4)
        } else {
            types <- c(types, 4:9)
        }
    } else if (inherits(obj, "AnalysisResults")) {
        types <- integer(0)
        if (.isConditionalPowerEnabled(obj$nPlanned)) {
            types <- c(1)
        }
        types <- c(types, 2)
    } else if (inherits(obj, "StageResults")) {
        types <- c(1)
    }

    if (output == "numeric") {
        return(types)
    }

    if (output == "caption") {
        captions <- character()
        for (type in types) {
            captions <- c(captions, .getPlotCaption(obj,
                type = type,
                numberInCaptionEnabled = numberInCaptionEnabled
            ))
        }
        return(captions)
    }

    if (output == "numcap") {
        numcap <- list()
        for (type in types) {
            numcap[[as.character(type)]] <- .getPlotCaption(obj,
                type = type,
                numberInCaptionEnabled = numberInCaptionEnabled
            )
        }
        return(numcap)
    }

    capnum <- list()
    for (type in types) {
        capnum[[.getPlotCaption(obj,
            type = type,
            numberInCaptionEnabled = numberInCaptionEnabled
        )]] <- type
    }
    return(capnum)
}

.getVariedParameterHint <- function(variedParameter, variedParameterName) {
    if (length(variedParameter) != 2) {
        return("")
    }

    return(paste0(
        "Note: interim values between ", round(variedParameter[1], 4), " and ",
        round(variedParameter[2], 4), " were calculated to get smoother lines; use, e.g., '",
        variedParameterName, " = ",
        .getVariedParameterVectorSeqCommand(variedParameter), "' to get all interim values"
    ))
}

.getRexepSaveCharacter <- function(x) {
    for (s in c("$", ".", "{", "}", "(", ")", "[", "]", "+", "-", "*", "/", "^", "?", "|")) {
        x <- gsub(paste0("\\", s), paste0("\\\\", s), x)
    }
    return(x)
}

.createValidParameterName <- function(objectName, parameterName) {
    if (grepl(paste0(.getRexepSaveCharacter(objectName), "\\$"), parameterName) &&
            !grepl("^\\.design", parameterName)) {
        return(parameterName)
    }

    if (is.null(objectName) || length(objectName) == 0 || is.na(objectName)) {
        return(parameterName)
    }

    if (grepl("^-?\\.?get[A-Z]{1}", parameterName)) {
        return(parameterName)
    }

    if (grepl("^rpact::", parameterName)) {
        return(parameterName)
    }

    if (grepl("^function", objectName) || nchar(objectName) > 40) {
        objectName <- "x"
    }

    return(paste0(objectName, "$", parameterName))
}

.showPlotSourceInformation <- function(objectName,
        ...,
        xParameterName,
        yParameterNames,
        hint = NA_character_,
        nMax = NA_integer_,
        type = NA_integer_,
        showSource = FALSE,
        xValues = NA_real_,
        lineType = TRUE) {
    if (is.character(showSource)) {
        if (length(showSource) != 1 || trimws(showSource) == "") {
            return(invisible(NULL))
        }

        if (!(showSource %in% C_PLOT_SHOW_SOURCE_ARGUMENTS)) {
            warning("'showSource' (", showSource, ") is not allowed and will be ignored", call. = FALSE)
            return(invisible())
        }
    } else if (!isTRUE(showSource)) {
        return(invisible(NULL))
    }

    .assertIsSingleCharacter(xParameterName, "xParameterName")
    if (length(yParameterNames) == 0 || !all(is.character(yParameterNames)) || all(is.na(yParameterNames))) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'yParameterNames' (", .arrayToString(yParameterNames),
            ") must be a valid character vector"
        )
    }
    .assertIsSingleCharacter(hint, "hint", naAllowed = TRUE)
    .assertIsSingleNumber(nMax, "nMax", naAllowed = TRUE)
    .assertIsNumericVector(xValues, "xValues", naAllowed = TRUE)

    cat("Source data of the plot", ifelse(!is.na(type), paste0(
        " (type ",
        type, ")"
    ), ""), ":\n", sep = "")

    if (grepl("^function", xParameterName) || nchar(xParameterName) > 40 ||
            any(grepl("^function", yParameterNames)) || any(nchar(yParameterNames) > 40)) {
        return(invisible(NULL))
    }

    xAxisCmd <- .reconstructSequenceCommand(xValues)
    if (is.na(xAxisCmd)) {
        if (!grepl("(\\$)|(^c\\()", xParameterName) || grepl("^\\.design", xParameterName)) {
            if (length(objectName) == 0 || is.na(objectName)) {
                objectName <- "x"
            }

            xAxisCmd <- paste0(objectName, "$", xParameterName)
        } else {
            xAxisCmd <- xParameterName
        }
    }
    if (!is.na(nMax) && length(yParameterNames) < 3 &&
            xParameterName == "informationRates") {
        xAxisCmd <- paste0(xAxisCmd, " * ", round(nMax, 1))
    }
    cat("  x-axis: ", xAxisCmd, "\n", sep = "")

    if (all(c("futilityBounds", "criticalValues") %in% yParameterNames)) {
        yParameterNames[1] <- paste0(
            "c(", objectName, "$futilityBounds, ",
            objectName, "$criticalValues[length(", objectName, "$criticalValues)])"
        )
    } else if (identical(yParameterNames, c("futilityBoundsEffectScale", "criticalValuesEffectScale"))) {
        yParameterNames[1] <- paste0(
            "c(", objectName, "$futilityBoundsEffectScale, ",
            objectName, "$criticalValuesEffectScale[length(", objectName, "$criticalValuesEffectScale)])"
        )
    }

    yAxisCmds <- character()
    if (length(yParameterNames) == 1) {
        yAxisCmds <- .createValidParameterName(objectName, yParameterNames)
    } else {
        for (yParameterName in yParameterNames) {
            yAxisCmds <- c(yAxisCmds, .createValidParameterName(objectName, yParameterName))
        }
    }
    if (length(yAxisCmds) == 1) {
        cat("  y-axis: ", yAxisCmds, "\n", sep = "")
    } else {
        cat("  y-axes:\n")
        for (i in seq_len(length(yAxisCmds))) {
            cat("    y", i, ": ", yAxisCmds[i], "\n", sep = "")
        }
    }

    if (!is.na(hint) && is.character(hint) && nchar(hint) > 0) {
        cat(hint, "\n", sep = "")
    }

    # add simple plot command examples
    cat("Simple plot command example", ifelse(length(yAxisCmds) == 1, "", "s"), ":\n", sep = "")
    plotCmds <- c()
    for (yAxisCmd in yAxisCmds) {
        plotCmd <- paste0("plot(", xAxisCmd, ", ", yAxisCmd)
        if (lineType) {
            plotCmd <- paste0(plotCmd, ", type = \"l\"")
        }
        plotCmd <- paste0(plotCmd, ")")
        plotCmds <- c(plotCmds, plotCmd)
        cat("  ", plotCmd, "\n", sep = "")
    }

    if (showSource == "commands") {
        return(invisible(plotCmds))
    } else if (showSource == "axes") {
        return(invisible(list(x = xAxisCmd, y = yAxisCmds)))
    } else if (showSource == "test") {
        success <- TRUE
        for (plotCmd in plotCmds) {
            if (!.testPlotCommand(plotCmd)) {
                success <- FALSE
            }
        }
        if (success) {
            cat("All plot commands are valid\n")
        } else {
            cat("One ore more plot commands are invalid\n")
        }
        return(invisible(plotCmds))
    } else if (showSource == "validate") {
        for (plotCmd in plotCmds) {
            .testPlotCommand(plotCmd, silent = FALSE)
        }
        return(invisible(plotCmds))
    }

    return(invisible(NULL))
}

.testPlotCommand <- function(plotCmd, silent = TRUE) {
    tryCatch(
        {
            eval(parse(text = plotCmd))
            return(invisible(TRUE))
        },
        error = function(e) {
            msg <- paste0(
                "failed to evaluate plot command \"", plotCmd, "\" ",
                "('", as.character(e$call), "'): ", e$message
            )
            if (!silent) {
                stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, msg[1])
            }
            cat(.firstCharacterToUpperCase(msg), "\n")
        }
    )
    return(invisible(FALSE))
}

.getParameterSetAsDataFrame <- function(...,
        parameterSet,
        designMaster,
        addPowerAndAverageSampleNumber = FALSE,
        theta = seq(-1, 1, 0.02),
        nMax = NA_integer_,
        mandatoryParameterNames = character(),
        yParameterNames = character()) {
    if (.isTrialDesignSet(parameterSet) && parameterSet$getSize() > 1 &&
            (is.null(parameterSet$variedParameters) || length(parameterSet$variedParameters) == 0)) {
        stop(
            C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'variedParameters' must be not empty; ",
            "use 'DesignSet$addVariedParameters(character)' to add one or more varied parameters"
        )
    }

    if (inherits(parameterSet, "TrialDesignSet")) {
        suppressWarnings(data <- as.data.frame(
            parameterSet,
            niceColumnNamesEnabled = FALSE,
            includeAllParameters = TRUE,
            addPowerAndAverageSampleNumber = addPowerAndAverageSampleNumber,
            theta = theta,
            nMax = nMax
        ))
    } else {
        parameterNames <- parameterSet$.getVisibleFieldNamesOrdered()
        suppressWarnings(data <- .getAsDataFrame(
            parameterSet = parameterSet,
            parameterNames = parameterNames,
            niceColumnNamesEnabled = FALSE,
            includeAllParameters = FALSE,
            mandatoryParameterNames = mandatoryParameterNames
        ))
    }

    if (!.isTrialDesignSet(parameterSet)) {
        variedParameters <- logical(0)
        if ("stages" %in% colnames(data)) {
            if ((!.isTrialDesignPlan(parameterSet) && !("overallReject" %in% yParameterNames)) ||
                    any(grepl("rejectPerStage|numberOfSubjects", yParameterNames))) {
                variedParameters <- "stages"
                names(variedParameters) <- "Stage"
            }
        }
        return(list(data = data, variedParameters = variedParameters))
    }

    if (parameterSet$getSize() <= 1) {
        return(list(data = data, variedParameters = parameterSet$variedParameters))
    }

    variedParameters <- parameterSet$variedParameters
    if (nrow(data) > 1) {
        for (variedParameter in variedParameters) {
            column <- data[[variedParameter]]
            if (length(column) <= 1) {
                stop(
                    C_EXCEPTION_TYPE_RUNTIME_ISSUE,
                    "varied parameter '", variedParameter, "' has length ", length(column)
                )
            }

            valueBefore <- column[1]
            for (i in 2:length(column)) {
                if (is.na(column[i])) {
                    column[i] <- valueBefore
                } else {
                    valueBefore <- column[i]
                }
            }
            data[[variedParameter]] <- column
        }
    }
    variedParameterNames <- c()
    for (variedParameter in variedParameters) {
        variedParameterNames <- c(
            variedParameterNames,
            .getParameterCaption(variedParameter, designMaster, tableOutputEnabled = TRUE)
        )
    }
    names(variedParameters) <- variedParameterNames
    return(list(data = data, variedParameters = variedParameters))
}

.getCategories <- function(data, yParameterName, parameterSet) {
    axisLabel <- .getAxisLabel(yParameterName, parameterSet)
    if (is.null(data$categories) || sum(is.na(data$categories)) > 0) {
        return(rep(axisLabel, nrow(data)))
    }

    return(paste(data$categories, axisLabel, sep = ", "))
}

.getAxisLabel <- function(parameterName, parameterSet) {
    axisLabel <- .getParameterCaption(parameterName, parameterSet, tableOutputEnabled = TRUE)
    if (is.null(axisLabel)) {
        return(paste0("%", parameterName, "%"))
    }
    axisLabel <- gsub(" \\(one-sided P-value Scale\\)$", "", axisLabel, ignore.case = TRUE)
    return(axisLabel)
}

.allGroupValuesEqual <- function(data, parameterName, groupName) {
    groupedValues <- base::by(data[[parameterName]], data[[groupName]], paste, collapse = ",")
    groupedValues <- groupedValues[!grepl("^NA(,NA)*$", groupedValues)]
    if (length(groupedValues) <= 1) {
        return(TRUE)
    }

    for (i in 1:(length(groupedValues) - 1)) {
        for (j in (i + 1):length(groupedValues)) {
            if (!is.na(groupedValues[i]) && !is.na(groupedValues[j]) &&
                    groupedValues[i] != groupedValues[j]) {
                return(FALSE)
            }
        }
    }
    return(TRUE)
}

.plotParameterSet <- function(...,
        parameterSet,
        designMaster,
        xParameterName,
        yParameterNames,
        mainTitle = NA_character_,
        xlab = NA_character_,
        ylab = NA_character_,
        palette = "Set1",
        theta = seq(-1, 1, 0.02),
        nMax = NA_integer_,
        plotPointsEnabled = NA,
        legendPosition = NA_integer_,
        variedParameters = logical(0),
        qnormAlphaLineEnabled = TRUE,
        yAxisScalingEnabled = TRUE,
        ratioEnabled = NA,
        plotSettings = NULL) {
    simulationEnrichmentEnmabled <- grepl("SimulationResultsEnrichment", .getClassName(parameterSet))
    if (.isParameterSet(parameterSet) || .isTrialDesignSet(parameterSet)) {
        parameterNames <- c(xParameterName, yParameterNames)
        parameterNames <- parameterNames[!(parameterNames %in% c(
            "theta", "averageSampleNumber",
            "overallEarlyStop", "calculatedPower"
        ))]
        fieldNames <- c(
            names(parameterSet),
            names(designMaster)
        )
        if (simulationEnrichmentEnmabled) {
            fieldNames <- c(fieldNames, gsub("s$", "", names(parameterSet$effectList)), "situation")
        }
        for (parameterName in parameterNames) {
            if (!is.na(parameterName) && !(parameterName %in% fieldNames)) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'", .getClassName(parameterSet), "' and '", .getClassName(designMaster), "' ",
                    "do not contain a field with name '", parameterName, "'"
                )
            }
        }
        if (is.null(plotSettings) || !inherits(plotSettings, "PlotSettings")) {
            plotSettings <- parameterSet$getPlotSettings()
        }
    } else {
        if (is.null(plotSettings) || !inherits(plotSettings, "PlotSettings")) {
            plotSettings <- PlotSettings$new()
        }
    }

    if (.isTrialDesignSet(parameterSet)) {
        parameterSet$assertHaveEqualSidedValues()
    }

    addPowerAndAverageSampleNumber <- xParameterName == "theta" &&
        yParameterNames[1] %in% c(
            "averageSampleNumber", "calculatedPower", "overallEarlyStop",
            "overallReject", "overallFutility"
        )

    if (!addPowerAndAverageSampleNumber) {
        addPowerAndAverageSampleNumber <- xParameterName %in% c("effect", "effectMatrix") &&
            yParameterNames[1] %in% c(
                "overallReject", "futilityStop",
                "earlyStop", "expectedNumberOfSubjects", "expectedNumberOfEvents"
            )
    }

    if (addPowerAndAverageSampleNumber && .isMultiArmSimulationResults(parameterSet)) {
        addPowerAndAverageSampleNumber <- FALSE
    }

    if (.isParameterSet(parameterSet) || .isTrialDesignSet(parameterSet)) {
        df <- .getParameterSetAsDataFrame(
            parameterSet = parameterSet,
            designMaster = designMaster,
            addPowerAndAverageSampleNumber = addPowerAndAverageSampleNumber,
            theta = theta,
            nMax = nMax,
            mandatoryParameterNames = c(xParameterName, yParameterNames),
            yParameterNames = yParameterNames
        )
        data <- df$data

        variedParameters <- df$variedParameters
        variedParameters <- na.omit(variedParameters)
        variedParameters <- variedParameters[variedParameters != "NA"]

        if (length(variedParameters) == 1 && length(yParameterNames) == 1) {
            if (.allGroupValuesEqual(data, parameterName = yParameterNames, groupName = variedParameters)) {
                variedParameters <- logical(0)
            }
        }
    } else if (is.data.frame(parameterSet)) {
        data <- parameterSet
    } else {
        stop(
            C_EXCEPTION_TYPE_RUNTIME_ISSUE,
            "'parameterSet' (", .getClassName(parameterSet), ") must be a data.frame, a 'TrialDesignSet' ",
            "or an object that inherits from 'ParameterSet'"
        )
    }

    if (length(variedParameters) > 0) {
        legendTitle <- .firstCharacterToUpperCase(paste(names(variedParameters), collapse = "\n"))
        categoryParameterName <- variedParameters[1]
    } else {
        legendTitle <- NA_character_
        categoryParameterName <- NA_character_
    }

    tryCatch(
        {
            if (all(c("criticalValuesPValueScale", "futilityBoundsPValueScale") %in% yParameterNames)) {
                index <- nrow(data)
                if ("stages" %in% colnames(data)) {
                    index <- which(data$stages == max(data$stages))
                }
                data$futilityBoundsPValueScale[index] <- data$criticalValuesPValueScale[index]
                data <- unique(data[, c(xParameterName, yParameterNames)])
            }
        },
        error = function(e) {
            .logError("Failed to set last 'futilityBoundsPValueScale' value: ", e$message)
        }
    )

    yParameterName1 <- yParameterNames[1]
    yParameterName2 <- NULL
    yParameterName3 <- NULL
    if (length(yParameterNames) >= 2) {
        yParameterName2 <- yParameterNames[2]
    }
    if (length(yParameterNames) >= 3) {
        yParameterName3 <- yParameterNames[3]
    }

    mirrorModeEnabled <- any(grepl("Mirrored$", yParameterNames))
    xAxisLabel <- .getAxisLabel(xParameterName, parameterSet)
    yAxisLabel1 <- .getAxisLabel(yParameterName1, parameterSet)
    yAxisLabel2 <- NULL
    if (!is.null(yParameterName2) && !is.null(yParameterName3)) {
        if (!is.na(yParameterName2)) {
            pn2 <- .getAxisLabel(yParameterName2, parameterSet)
            if (yParameterName2 == "overallEarlyStop") {
                pn2 <- "Stopping Probability"
            }
            yAxisLabel2 <- paste(pn2, .getAxisLabel(yParameterName3, parameterSet), sep = " and ")
        } else {
            yAxisLabel2 <- .getAxisLabel(yParameterName3, parameterSet)
        }
    } else if (xParameterName == "effectMatrix" && !is.null(yParameterName2) && !is.na(yParameterName2) &&
            yParameterName1 %in% c("expectedNumberOfEvents", "expectedNumberOfSubjects") &&
            yParameterName2 == "rejectAtLeastOne") {
        # special case: simulation results, plot type 6 (expected number of subjects and power)
        yAxisLabel2 <- .getAxisLabel(yParameterName2, parameterSet)
        yParameterName3 <- yParameterName2
        yParameterName2 <- NA_character_
    } else if (!is.null(yParameterName2) && !mirrorModeEnabled) {
        yAxisLabel1 <- paste(yAxisLabel1, .getAxisLabel(yParameterName2, parameterSet), sep = " and ")
    }
    if (yParameterName1 %in% c("alphaSpent", "betaSpent")) {
        yAxisLabel1 <- "Cumulative Error"
        if (is.null(yParameterName2)) {
            yAxisLabel1 <- paste0(yAxisLabel1, " (", .getAxisLabel(yParameterName1, parameterSet), ")")
        }
    }

    yAxisLabel1 <- sub(paste0(.getParameterCaption("futilityBoundsDelayedInformation"), " and"),
        "Lower and", yAxisLabel1,
        fixed = TRUE
    )
    yAxisLabel1 <- sub(paste0(.getParameterCaption("futilityBoundsDelayedInformationNonBinding"), " and"),
        "Lower and", yAxisLabel1,
        fixed = TRUE
    )

    if (!("xValues" %in% colnames(data)) || !("yValues" %in% colnames(data))) {
        if (!(xParameterName %in% colnames(data))) {
            print(colnames(data))
            stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, sQuote(xParameterName), " is not available in dataset (x-axis)")
        }
        if (!(yParameterName1 %in% colnames(data))) {
            stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, sQuote(yParameterName1), " is not available in dataset (y-axis)")
        }

        data$xValues <- data[[xParameterName]]
        data$yValues <- data[[yParameterName1]]
        if (yParameterName1 == "futilityBounds") {
            data$yValues[!is.na(data$yValues) &
                (is.infinite(data$yValues) | data$yValues == C_FUTILITY_BOUNDS_DEFAULT)] <- NA_real_
        } else if (yParameterName1 == "alpha0Vec") {
            data$yValues[!is.na(data$yValues) & data$yValues == C_ALPHA_0_VEC_DEFAULT] <- NA_real_
        }

        if (is.null(yParameterName2) || is.na(yParameterName2)) {
            data$yValues2 <- rep(NA_real_, nrow(data))
        } else {
            if (!(yParameterName2 %in% colnames(data))) {
                stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, sQuote(yParameterName2), " is not available in dataset (y-axis 2)")
            }
            data$yValues2 <- data[[yParameterName2]]
        }
        if (is.null(yParameterName3)) {
            data$yValues3 <- rep(NA_real_, nrow(data))
        } else {
            if (!(yParameterName3 %in% colnames(data))) {
                stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, sQuote(yParameterName3), " is not available in dataset (y-axis 3)")
            }
            data$yValues3 <- data[[yParameterName3]]
        }

        if (!is.na(categoryParameterName)) {
            data$categories <- data[[categoryParameterName]]
            if (length(variedParameters) > 1) {
                data$categories <- paste0(
                    variedParameters[1], " = ", data$categories, ", ",
                    variedParameters[2], " = ", data[[variedParameters[2]]]
                )
            }
        } else {
            data$categories <- rep(NA_character_, nrow(data))
        }
    }

    if (!is.na(nMax) && is.null(yParameterName3) && xParameterName == "informationRates") {
        xAxisLabel <- "Sample Size"
        data$xValues <- data$xValues * nMax
        tryCatch(
            {
                data$xValues <- as.numeric(.formatSampleSizes(data$xValues))
            },
            error = function(e) {
                warning("Failed to format sample sizes on x-axis: ", e$message)
            }
        )
    }

    # add zero point to data
    if (yParameterName1 %in% c("alphaSpent", "betaSpent")) {
        data <- data[, c("xValues", "yValues", "yValues2", "categories")]
        uc <- unique(data$categories)
        data <- rbind(data.frame(
            xValues = rep(-0.00001, length(uc)),
            yValues = rep(0, length(uc)),
            yValues2 = rep(0, length(uc)),
            categories = uc
        ), data)
    }

    scalingFactor1 <- 1
    scalingFactor2 <- 1
    if (!is.null(yParameterName2) && "yValues2" %in% colnames(data)) {
        if (yAxisScalingEnabled && !is.null(yParameterName3) && "yValues3" %in% colnames(data)) {
            if (is.na(yParameterName2)) {
                scalingFactors <- .getScalingFactors(data$yValues, data$yValues3)
            } else {
                scalingFactors <- .getScalingFactors(data$yValues, c(data$yValues2, data$yValues3))
            }
            scalingFactor1 <- scalingFactors$scalingFactor1
            scalingFactor2 <- scalingFactors$scalingFactor2
        }
        df1 <- data.frame(
            xValues = data$xValues,
            yValues = data$yValues * scalingFactor1,
            categories = .getCategories(data, yParameterName1, parameterSet)
        )
        if (!is.na(yParameterName2)) {
            df2 <- data.frame(
                xValues = data$xValues,
                yValues = data$yValues2 * scalingFactor2,
                categories = .getCategories(data, yParameterName2, parameterSet)
            )
        }
        if (!is.null(yParameterName3) && "yValues3" %in% colnames(data)) {
            df3 <- data.frame(
                xValues = data$xValues,
                yValues = data$yValues3 * scalingFactor2,
                categories = .getCategories(data, yParameterName3, parameterSet)
            )
            if (is.na(yParameterName2)) {
                data <- rbind(df1, df3)
            } else {
                data <- rbind(df1, df2, df3)
            }
        } else {
            data <- rbind(df1, df2)
        }

        # sort categories for pairwise printing of the legend
        unqiueValues <- unique(as.character(data$categories))
        decreasing <- addPowerAndAverageSampleNumber && xParameterName %in% c("effect", "effectMatrix")

        if (all(c("criticalValuesPValueScale", "futilityBoundsPValueScale") %in% yParameterNames)) {
            decreasing <- TRUE
        }

        catLevels <- unqiueValues[order(unqiueValues, decreasing = decreasing)]
        data$categories <- factor(data$categories, levels = catLevels)
        if (!is.na(legendTitle) && yParameterName1 == "alphaSpent" && yParameterName2 == "betaSpent") {
            sep <- ifelse(length(legendTitle) > 0 && nchar(legendTitle) > 0, "\n", "")
            legendTitle <- paste(legendTitle, "Type of error", sep = sep)
        }
    }

    if (is.na(legendPosition)) {
        legendPosition <- .getLegendPosition(
            plotSettings, designMaster, data, yParameterName1,
            yParameterName2, addPowerAndAverageSampleNumber
        )
    }

    if (is.na(ratioEnabled)) {
        ratioEnabled <- .isTrialDesignPlanSurvival(parameterSet) ||
            (.isTrialDesignPlanMeans(parameterSet) && parameterSet$meanRatio) ||
            (.isTrialDesignPlanRates(parameterSet) && parameterSet$riskRatio)
    }

    plotDashedHorizontalLine <- "criticalValuesEffectScale" %in% yParameterNames && designMaster$sided == 2

    p <- .plotDataFrame(
        data,
        mainTitle = mainTitle,
        xlab = xlab,
        ylab = ylab,
        xAxisLabel = xAxisLabel,
        yAxisLabel1 = yAxisLabel1,
        yAxisLabel2 = yAxisLabel2,
        palette = palette,
        plotPointsEnabled = plotPointsEnabled,
        legendTitle = legendTitle,
        legendPosition = legendPosition,
        scalingFactor1 = scalingFactor1,
        scalingFactor2 = scalingFactor2,
        addPowerAndAverageSampleNumber = addPowerAndAverageSampleNumber,
        mirrorModeEnabled = mirrorModeEnabled,
        plotDashedHorizontalLine = plotDashedHorizontalLine,
        ratioEnabled = ratioEnabled,
        plotSettings = plotSettings,
        sided = designMaster$sided,
        directionUpper = designMaster$directionUpper,
        ...
    )

    if (xParameterName == "informationRates") {
        p <- p + ggplot2::scale_x_continuous(breaks = c(0, round(data$xValues, 3)))
    } else if (xParameterName == "situation") { # simulation enrichment
        p <- p + ggplot2::scale_x_continuous(breaks = round(data$xValues))
    }

    # add mirrored lines
    if (!is.data.frame(parameterSet) && designMaster$sided == 2 &&
            ((yParameterName1 == "criticalValues" || yParameterName1 == "criticalValuesEffectScale") ||
                (!is.null(yParameterName2) && !is.na(yParameterName2) &&
                    (yParameterName2 == "criticalValues" || yParameterName2 == "criticalValuesEffectScale")))) {
        p <- plotSettings$mirrorYValues(p,
            yValues = data$yValues,
            plotPointsEnabled = !addPowerAndAverageSampleNumber,
            pointBorder = .getPointBorder(data, plotSettings)
        )

        # add zero line for Pampallona Tsiatis design
        p <- p + ggplot2::geom_hline(yintercept = 0, linetype = "solid") # longdash
    }

    if (!.isTrialDesignFisher(designMaster) && qnormAlphaLineEnabled &&
            (
                (
                    !is.data.frame(parameterSet) &&
                        (
                            yParameterName1 == "criticalValues" ||
                                (
                                    yParameterName1 == "futilityBounds" && !is.null(yParameterName2) &&
                                        yParameterName2 == "criticalValues"
                                )
                        )
                ) ||
                    (
                        !is.null(yParameterName2) &&
                            grepl("futilityBounds|criticalValues", yParameterName1) &&
                            grepl("criticalValues", yParameterName2)
                    )
            )
        ) {
        p <- .addQnormAlphaLine(p, designMaster, plotSettings, data)
    }

    if (!.isTrialDesignFisher(designMaster) &&
            (xParameterName == "informationRates" || xParameterName == "cumulativeEventsPerStage") &&
            yParameterName1 == "stageLevels") {
        yValue <- designMaster$alpha
        if (designMaster$sided == 2) {
            yValue <- yValue / 2
        }
        p <- p + ggplot2::geom_hline(yintercept = yValue, linetype = "dashed")
        yValueLabel <- paste0("alpha == ", round(yValue, 4))
        hjust <- plotSettings$scaleSize(-0.2)
        p <- p + ggplot2::annotate("label",
            x = -Inf, hjust = hjust, y = yValue,
            label = yValueLabel, size = plotSettings$scaleSize(2.5), parse = TRUE, colour = "white", fill = "white"
        )
        p <- p + ggplot2::annotate("text",
            x = -Inf, hjust = hjust - plotSettings$scaleSize(0.15), y = yValue,
            label = yValueLabel, size = plotSettings$scaleSize(2.5), parse = TRUE
        )
    }

    return(p)
}

.naAndNaNOmit <- function(x) {
    if (is.null(x) || length(x) == 0) {
        return(x)
    }

    x <- na.omit(x)
    return(x[!is.nan(x)])
}

.getScalingFactors <- function(leftAxisValues, rightAxisValues) {
    m1 <- ifelse(length(.naAndNaNOmit(leftAxisValues)) == 0, 1, max(.naAndNaNOmit(leftAxisValues)))
    m2 <- ifelse(length(.naAndNaNOmit(rightAxisValues)) == 0, 1, max(.naAndNaNOmit(rightAxisValues)))
    if (is.na(m1)) {
        stop(
            C_EXCEPTION_TYPE_RUNTIME_ISSUE, "y-values, left (",
            .arrayToString(leftAxisValues), ") are not specified correctly"
        )
    }
    if (is.na(m2)) {
        stop(
            C_EXCEPTION_TYPE_RUNTIME_ISSUE, "y-values, right (",
            .arrayToString(rightAxisValues), ") are not specified correctly"
        )
    }

    if (m1 > m2) {
        scalingFactor1 <- 1
        scalingFactor2 <- ifelse(m2 == 0, m1, m1 / m2)
    } else if (m1 < m2) {
        scalingFactor1 <- ifelse(m1 == 0, m2, m2 / m1)
        scalingFactor2 <- 1
    } else {
        scalingFactor1 <- 1
        scalingFactor2 <- 1
    }

    if (is.infinite(scalingFactor2)) {
        stop(
            "Failed to calculate 'scalingFactor2' (", scalingFactor2, ") for ",
            .arrayToString(leftAxisValues, maxLength = 15), " and ", .arrayToString(rightAxisValues, maxLength = 15)
        )
    }

    return(list(scalingFactor1 = scalingFactor1, scalingFactor2 = scalingFactor2))
}

.plotDataFrame <- function(data,
        ...,
        mainTitle = NA_character_,
        xlab = NA_character_,
        ylab = NA_character_,
        xAxisLabel = NA_character_,
        yAxisLabel1 = NA_character_,
        yAxisLabel2 = NA_character_,
        palette = "Set1",
        plotPointsEnabled = NA,
        legendTitle = NA_character_,
        legendPosition = NA_integer_,
        scalingFactor1 = 1,
        scalingFactor2 = 1,
        addPowerAndAverageSampleNumber = FALSE,
        mirrorModeEnabled = FALSE,
        plotDashedHorizontalLine = FALSE,
        ratioEnabled = FALSE,
        plotSettings = NULL,
        sided = 1,
        discreteXAxis = FALSE,
        directionUpper = NA) {
    if (!is.data.frame(data)) {
        stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'data' must be a data.frame (is ", .getClassName(data), ")")
    }

    if (is.null(plotSettings)) {
        plotSettings <- PlotSettings$new()
    }

    nRow <- nrow(data)
    data <- data[!(data$xValues == 0 & data$xValues == data$yValues), ]
    removedRows1 <- nRow - nrow(data)

    nRow <- nrow(data)
    data <- data[!is.na(data$yValues), ]
    removedRows2 <- nRow - nrow(data)

    if (getLogLevel() == C_LOG_LEVEL_WARN && (removedRows1 > 0 || removedRows2 > 0)) {
        warning(sprintf(
            "Removed %s rows containing (0, 0)-points and %s rows containing missing values",
            removedRows1, removedRows2
        ), call. = FALSE)
    }

    categoryEnabled <- !is.null(data[["categories"]]) && !all(is.na(data$categories))
    groupEnabled <- !is.null(data[["groups"]]) && !all(is.na(data$groups))
    if (categoryEnabled && groupEnabled) {
        data <- data[, c("xValues", "yValues", "categories", "groups")]
    } else if (categoryEnabled) {
        data <- data[, c("xValues", "yValues", "categories")]
    } else if (groupEnabled) {
        data <- data[, c("xValues", "yValues", "groups")]
    } else {
        data <- data[, c("xValues", "yValues")]
    }

    data$yValues[!is.na(data$yValues) & is.infinite(data$yValues)] <- NA_real_
    data <- data[!is.na(data$yValues), ]

    if (categoryEnabled && groupEnabled) {
        p <- ggplot2::ggplot(data, ggplot2::aes(
            x = .data[["xValues"]], y = .data[["yValues"]],
            colour = factor(.data[["groups"]]),
            fill = factor(.data[["categories"]])
        ))
    } else if (mirrorModeEnabled) {
        p <- ggplot2::ggplot(data, ggplot2::aes(
            x = .data[["xValues"]], y = .data[["yValues"]],
            fill = factor(.data[["categories"]])
        ))
    } else if (categoryEnabled) {
        p <- ggplot2::ggplot(data, ggplot2::aes(
            x = .data[["xValues"]], y = .data[["yValues"]],
            colour = factor(.data[["categories"]])
        ))
    } else {
        p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[["xValues"]], y = .data[["yValues"]]))
    }

    p <- plotSettings$setTheme(p)
    p <- plotSettings$hideGridLines(p)

    if (discreteXAxis) {
        p <- p + ggplot2::scale_x_continuous(breaks = round(data$xValues))
    }

    # set main title
    p <- plotSettings$setMainTitle(p, mainTitle)

    # set legend
    if (!categoryEnabled || mirrorModeEnabled || (!is.na(legendPosition) && legendPosition == -1)) {
        p <- p + ggplot2::theme(legend.position = "none")
    } else {
        p <- plotSettings$setLegendPosition(p, legendPosition = legendPosition)
        p <- plotSettings$setLegendBorder(p)
        p <- plotSettings$setLegendTitle(p, legendTitle)
        p <- plotSettings$setLegendLabelSize(p)
    }

    # set optional scale limits
    xLim <- .getOptionalArgument("xlim", ...)
    yLim <- .getOptionalArgument("ylim", ...)
    if (is.null(yLim) && !missing(yAxisLabel1) &&
            !is.na(yAxisLabel1) && yAxisLabel1 == "Critical value") {
        yMax <- .applyDirectionOfAlternative(na.omit(data$yValues),
            directionUpper,
            type = "maxMin", phase = "design"
        )
        if (length(yMax) == 1) {
            if (yMax > 0 && yMax < 0.1) {
                yLim <- c(0, 2 * yMax)
            } else if (yMax < 0 && yMax > -0.1) {
                yLim <- c(0, 2 * yMax)
            }
        }
    }

    if ((!is.null(xLim) && is.numeric(xLim) && length(xLim) == 2) ||
            (!is.null(yLim) && is.numeric(yLim) && length(yLim) == 2)) {
        p <- p + ggplot2::coord_cartesian(
            xlim = sort(xLim),
            ylim = sort(yLim),
            expand = TRUE,
            default = FALSE,
            clip = "on"
        )
    }

    # add dashed line to y = 0 or y = 1
    if (mirrorModeEnabled || plotDashedHorizontalLine) {
        p <- p + ggplot2::geom_hline(yintercept = ifelse(ratioEnabled, 1, 0), linetype = "dashed")
    }

    xAxisLabel <- .toCapitalized(xAxisLabel)
    yAxisLabel1 <- .toCapitalized(yAxisLabel1)
    yAxisLabel2 <- .toCapitalized(yAxisLabel2)

    p <- plotSettings$setAxesLabels(p,
        xAxisLabel = xAxisLabel,
        yAxisLabel1 = yAxisLabel1,
        yAxisLabel2 = yAxisLabel2,
        xlab = xlab,
        ylab = ylab,
        scalingFactor1 = scalingFactor1,
        scalingFactor2 = scalingFactor2
    )

    # plot lines and points
    plotPointsEnabled <- ifelse(is.na(plotPointsEnabled),
        !addPowerAndAverageSampleNumber, plotPointsEnabled
    )
    if (length(unique(data$xValues)) > 20) {
        plotPointsEnabled <- FALSE
    }
    p <- plotSettings$plotValues(p,
        plotPointsEnabled = plotPointsEnabled,
        pointBorder = .getPointBorder(data, plotSettings)
    )

    p <- plotSettings$setAxesAppearance(p)
    p <- plotSettings$setColorPalette(p, palette)
    p <- plotSettings$enlargeAxisTicks(p)

    companyAnnotationEnabled <- .getOptionalArgument("companyAnnotationEnabled", ...)
    if (is.null(companyAnnotationEnabled) || !is.logical(companyAnnotationEnabled)) {
        companyAnnotationEnabled <- FALSE
    }
    p <- plotSettings$addCompanyAnnotation(p, enabled = companyAnnotationEnabled)

    # start plot generation
    return(p)
}

.getPointBorder <- function(data, plotSettings) {
    numberOfCategories <- 1
    if (sum(is.na(data$categories)) < length(data$categories)) {
        numberOfCategories <- length(unique(as.character(data$categories)))
    }

    pointBorder <- 4
    if (length(unique(data$xValues)) / numberOfCategories > 10) {
        pointBorder <- 1
        plotSettings$adjustPointSize(0.333)
    } else if (numberOfCategories > 8) {
        pointBorder <- 1
    } else if (numberOfCategories > 6) {
        pointBorder <- 2
    } else if (numberOfCategories > 4) {
        pointBorder <- 3
    }
    return(pointBorder)
}

.getLegendPosition <- function(plotSettings,
        designMaster,
        data,
        yParameterName1,
        yParameterName2,
        addPowerAndAverageSampleNumber) {
    if (length(unique(data$categories)) > 6) {
        plotSettings$adjustPointSize(0.8)
        plotSettings$adjustLegendFontSize(0.8)
        return(C_POSITION_OUTSIDE_PLOT)
    }

    if (.isTrialDesignWithValidFutilityBounds(designMaster) &&
            yParameterName1 == "futilityBounds" && yParameterName2 == "criticalValues") {
        return(C_POSITION_RIGHT_BOTTOM)
    }

    if (.isTrialDesignWithValidAlpha0Vec(designMaster) &&
            yParameterName1 == "alpha0Vec" && yParameterName2 == "criticalValues") {
        return(C_POSITION_RIGHT_TOP)
    }

    if (yParameterName1 == "criticalValues") {
        return(C_POSITION_RIGHT_TOP)
    }

    if (yParameterName1 %in% c("stageLevels", "alphaSpent", "betaSpent")) {
        return(C_POSITION_LEFT_TOP)
    }

    if (addPowerAndAverageSampleNumber) {
        return(C_POSITION_LEFT_CENTER)
    }

    return(C_POSITION_OUTSIDE_PLOT)
}

.addQnormAlphaLine <- function(p, designMaster, plotSettings, data, annotationEnabled = TRUE) {
    alpha <- designMaster$alpha
    if (designMaster$sided == 2) {
        alpha <- alpha / 2
    }
    yValue <- .applyDirectionOfAlternative(.getOneMinusQNorm(alpha),
        designMaster$directionUpper,
        type = "negateIfLower", phase = "design"
    )
    prefix <- ifelse(isFALSE(designMaster$directionUpper), "-", "")
    yValueLabel <- paste0(prefix, "qnorm(1 - ", alpha, " ) == ", round(yValue, 4))
    if (designMaster$sided == 1) {
        p <- p + ggplot2::geom_hline(yintercept = yValue, linetype = "dashed")
    } else {
        p <- p + ggplot2::geom_hline(yintercept = yValue, linetype = "dashed")
        p <- p + ggplot2::geom_hline(yintercept = -yValue, linetype = "dashed")
    }
    if (annotationEnabled) {
        p <- p + ggplot2::annotate("label",
            x = -Inf, hjust = plotSettings$scaleSize(-0.1), y = yValue,
            label = yValueLabel, size = plotSettings$scaleSize(2.5), parse = TRUE, colour = "white", fill = "white"
        )
        p <- p + ggplot2::annotate("text",
            x = -Inf, hjust = plotSettings$scaleSize(-0.15), y = yValue,
            label = yValueLabel, size = plotSettings$scaleSize(2.5), parse = TRUE
        )
    }

    # expand y-axis range
    if (designMaster$sided == 1) {
        yMax <- .applyDirectionOfAlternative(na.omit(data$yValues),
            designMaster$directionUpper,
            type = "maxMin", phase = "design"
        )
        if (!is.null(data$yValues2) && length(data$yValues2) > 0) {
            yMax <- .applyDirectionOfAlternative(
                c(
                    yMax,
                    stats::na.omit(data$yValues2)
                ),
                designMaster$directionUpper,
                type = "maxMin", phase = "design"
            )
        }
        eps <- (yMax - yValue) * 0.15

        p <- plotSettings$expandAxesRange(p, y = yValue - eps)
    }

    return(p)
}

.getLambdaStepFunctionByTime <- function(time, piecewiseSurvivalTime, lambda2) {
    if (length(piecewiseSurvivalTime) == 0 || anyNA(piecewiseSurvivalTime)) {
        return(lambda2[1])
    }

    for (i in seq_len(length(piecewiseSurvivalTime))) {
        if (time <= piecewiseSurvivalTime[i]) {
            return(lambda2[i])
        }
    }
    return(lambda2[length(lambda2)])
}

.getLambdaStepFunction <- function(timeValues, piecewiseSurvivalTime, piecewiseLambda) {
    if (length(piecewiseSurvivalTime) != length(piecewiseLambda)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "length of 'piecewiseSurvivalTime' (", length(piecewiseSurvivalTime),
            ") must be equal to length of 'piecewiseLambda' (", length(piecewiseLambda), ") - 1"
        )
    }

    piecewiseSurvivalTime <- .getPiecewiseExpStartTimesWithoutLeadingZero(piecewiseSurvivalTime)
    if (length(piecewiseSurvivalTime) == 0) {
        return(piecewiseLambda[1])
    }

    lambdaValues <- c()
    for (time in timeValues) {
        lambdaValues <- c(lambdaValues, .getLambdaStepFunctionByTime(time, piecewiseSurvivalTime, piecewiseLambda))
    }
    return(lambdaValues)
}

#'
#' @title
#' Get Lambda Step Function
#'
#' @description
#' Calculates the lambda step values for a given time vector.
#'
#' @param timeValues A numeric vector that specifies the time values for which the lambda step values shall be calculated.
#' @param piecewiseSurvivalTime A numeric vector that specifies the time intervals for the piecewise
#'        definition of the exponential survival time cumulative distribution function (see details).
#' @param piecewiseLambda A numeric vector that specifies the assumed hazard rate in the treatment group.
#' @inheritParams param_three_dots
#'
#' @details
#' The first element of the vector \code{piecewiseSurvivalTime} must be equal to \code{0}.
#' This function is used for plotting of sample size survival results
#' (cf., \code{\link[=plot.TrialDesignPlan]{plot}}, \code{type = 13} and \code{type = 14}).
#'
#' @return A numeric vector containing the lambda step values that corresponds to the specified time values.
#'
#' @export
#'
#' @keywords internal
#'
getLambdaStepFunction <- function(timeValues, ..., piecewiseSurvivalTime, piecewiseLambda) {
    .assertIsNumericVector(timeValues, "timeValues")
    .assertIsNumericVector(piecewiseSurvivalTime, "piecewiseSurvivalTime")
    .assertIsNumericVector(piecewiseLambda, "piecewiseLambda")
    .warnInCaseOfUnknownArguments(functionName = "getLambdaStepFunction", ...)

    .getLambdaStepFunction(
        timeValues = timeValues,
        piecewiseSurvivalTime = piecewiseSurvivalTime,
        piecewiseLambda = piecewiseLambda
    )
}

.getRelativeFigureOutputPath <- function(subDir = NULL) {
    if (is.null(subDir)) {
        subDir <- format(Sys.Date(), format = "%Y-%m-%d")
    }
    figPath <- file.path(getwd(), "_examples", "output", "figures", subDir)
    if (!dir.exists(figPath)) {
        dir.create(figPath, showWarnings = FALSE, recursive = TRUE)
    }
    return(figPath)
}

# @title
# Save Last Plot
#
# @description
# Saves the last plot to a PNG file located in
# '[getwd()]/_examples/output/figures/[current date]/[filename].png'.
#
# @param filename The filename (without extension!).
#
# @details
# This is a wrapper function that creates a output path and uses \code{ggsave} to save the last plot.
#
# @examples
#
# # saveLastPlot('my_plot')
#
# @keywords internal
#
saveLastPlot <- function(filename, outputPath = .getRelativeFigureOutputPath()) {
    .assertGgplotIsInstalled()

    if (grepl("\\\\|/", filename)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'filename' seems to be a path. ",
            "Please specify 'outputPath' separately"
        )
    }

    if (!grepl("\\.png$", filename)) {
        filename <- paste0(filename, ".png")
    }

    path <- file.path(outputPath, filename)
    ggplot2::ggsave(
        filename = path,
        plot = ggplot2::last_plot(), device = NULL, path = NULL,
        scale = 1.2, width = 16, height = 15, units = "cm", dpi = 600, limitsize = TRUE
    )

    cat("Last plot was saved to '", path, "'\n")
}

.getGridPlotSettings <- function(x, typeNumbers, grid) {
    if (length(typeNumbers) <= 3 || grid <= 1) {
        return(NULL)
    }

    if (is.null(x[[".plotSettings"]])) {
        stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'x' (", .getClassName(x), ") does not contain field .plotSettings")
    }

    plotSettings <- x$.plotSettings
    if (is.null(plotSettings)) {
        plotSettings <- PlotSettings$new()
    } else {
        plotSettings <- plotSettings$clone()
    }
    if (plotSettings$scalingFactor == 1) {
        plotSettings$scalingFactor <- 0.6
    }
    return(plotSettings)
}

.getGridLegendPosition <- function(legendPosition, typeNumbers, grid) {
    if (length(typeNumbers) <= 3 || grid <= 1) {
        return(legendPosition)
    }

    if (is.na(legendPosition)) {
        return(-1L) # hide legend for more than 3 plots
    }

    return(legendPosition)
}

.formatSubTitleValue <- function(value, paramName) {
    if (paramName == "allocationRatioPlanned") {
        return(round(value, 2))
    }

    if (paramName %in% c("assumedStDev", "assumedStDevs")) {
        if (length(value) > 1) {
            return(paste0("(", .arrayToString(round(value, 1), encapsulate = FALSE), ")"))
        }

        return(round(value, 2))
    }

    if (paramName %in% c("piControls", "pi2")) {
        if (length(value) > 1) {
            return(paste0("(", .arrayToString(round(value, 3), encapsulate = FALSE), ")"))
        }

        return(round(value, 3))
    }

    return(.arrayToString(round(value, 2)))
}

.showWarningIfPlotArgumentWillBeIgnored <- function(type, ..., obj = NULL) {
    if ((is.null(type) || all(is.na(type)) || any(type == "all", na.rm = TRUE)) &&
            (is.null(obj) || inherits(obj, "TrialDesignPlan"))) {
        return(invisible())
    }

    showFutilityBounds <- .getOptionalArgument("showFutilityBounds", ...)
    if ((all(type != 3, na.rm = TRUE) || (!is.null(obj) && !inherits(obj, "TrialDesignPlan"))) && !is.null(showFutilityBounds)) {
        objTypeInfo <- ifelse(!is.null(obj) && !inherits(obj, "TrialDesignPlan"), " design plan", "")
        warning("Argument 'showFutilityBounds' (", showFutilityBounds, ") is only available for", objTypeInfo, " plot type 3; ",
            "it will be ignored",
            call. = FALSE
        )
    }

    showAlphaSpent <- .getOptionalArgument("showAlphaSpent", ...)
    showBetaSpent <- .getOptionalArgument("showBetaSpent", ...)
    if (all(type != 4, na.rm = TRUE) && all(type != "all", na.rm = TRUE) &&
            (!is.null(showAlphaSpent) || !is.null(showBetaSpent))) {
        if (!is.null(showAlphaSpent) && !is.null(showBetaSpent)) {
            warning("Arguments 'showAlphaSpent' (", showAlphaSpent, ") and 'showBetaSpent' (", showBetaSpent, ") ",
                "are only available for plot type 4; they will be ignored",
                call. = FALSE
            )
        } else if (!is.null(showAlphaSpent)) {
            warning("Argument 'showAlphaSpent' (", showAlphaSpent, ") is only available for plot type 4; ",
                "it will be ignored",
                call. = FALSE
            )
        } else {
            warning("Argument 'showBetaSpent' (", showBetaSpent, ") is only available for plot type 4; ",
                "it will be ignored",
                call. = FALSE
            )
        }
    }
}
