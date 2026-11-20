## Endpoint-specific settings and shared output for quantitative trial results.

.isQtdOperatingCharacteristics <- function(object) {
    return(inherits(object, "OperatingCharacteristicsMeans") || inherits(object, "OperatingCharacteristicsRates"))
}

.getQtdEndpointSettings <- function(object, xAxis = NULL, functionName = ".getQtdEndpointSettings") {
    if (inherits(object, "OperatingCharacteristicsMeans")) {
        settings <- list(endpoint = "means", assumptionColumn = "stDev", assumptionCaption = "SD",
            assumptions = c("normalApproximation", "groups", "stDev"), summaryColumns = "alternative",
            effectColumn = "alternative", effectCaption = "True effect", defaultAxis = "effect",
            sensitivity = c("sampleSize", "stDev", "allocation"))
    } else if (inherits(object, "OperatingCharacteristicsRates")) {
        settings <- list(endpoint = "rates", assumptionColumn = "pi2", assumptionCaption = "Control rate",
            assumptions = c("groups", "pi2", "directionUpper", "intervalMethod"),
            summaryColumns = c("pi1", "alternative"), effectColumn = "pi1",
            effectCaption = "Treatment rate", defaultAxis = "treatmentRate",
            sensitivity = c("sampleSize", "pi2", "allocation"))
    } else {
        stopIllegalArgument("This operation requires QTD operating characteristics for means or rates",
            functionName = functionName, parameter = "x", value = object,
            constraint = "OperatingCharacteristicsMeans or OperatingCharacteristicsRates",
            diagnosticId = "qtd.plot.unsupported_endpoint")
    }
    if (inherits(object$.design, "TrialDesignPlanQuantitativeBayes")) {
        if (settings$endpoint == "means") {
            settings$assumptions <- c("groups", "stDev", "priorDist", "controlMean")
        } else {
            settings$assumptions <- c("groups", "pi2", "directionUpper", "priorDist1", "priorDist2")
        }
    }
    if (is.null(xAxis)) {
        xAxis <- settings$defaultAxis
    }
    allowedAxes <- "effect"
    if (settings$endpoint == "rates") {
        allowedAxes <- c("treatmentRate", "effect")
    }
    .assertQtdPlotChoice(xAxis, "xAxis", allowedAxes, functionName)
    settings$xAxis <- xAxis
    settings$xColumn <- "alternative"
    settings$xCaption <- "True treatment effect"
    if (settings$endpoint == "rates") {
        settings$xCaption <- "True beneficial risk difference"
    }
    if (xAxis == "treatmentRate") {
        settings$xColumn <- "pi1"
        settings$xCaption <- "True treatment event rate"
    }
    return(settings)
}

#'
#' @title
#' Render Quantitative Operating Characteristics
#'
#' @description
#' Renders the parameters and a preview of the probability table.
#'
#' @param object An OperatingCharacteristics object.
#' @param showType Output mode.
#' @param digits Display precision.
#' @param consoleOutputEnabled Whether to print or collect the output.
#' @param maxRows Maximum number of probability rows to show.
#'
#' @return Invisibly, whether the output mode was handled.
#'
.showQtdOperatingCharacteristics <- function(object, showType, digits, consoleOutputEnabled, maxRows) {
    object$.resetCat()
    if (showType == 3) {
        .createSummary(object, digits = digits)$.show(showType = 1, digits = digits,
            consoleOutputEnabled = consoleOutputEnabled)
        return(invisible(TRUE))
    }
    if (showType == 2) {
        return(invisible(FALSE))
    }
    if (!is.numeric(maxRows) || is.complex(maxRows) || length(maxRows) != 1L ||
            is.na(maxRows) || maxRows < 1 || maxRows != trunc(maxRows)) {
        stopIllegalArgument("'maxRows' must be a positive integer or Inf",
            functionName = "OperatingCharacteristics$.show", parameter = "maxRows", value = maxRows,
            constraint = "one positive integer or positive infinity", diagnosticId = "qtd.display.max_rows_invalid"
        )
    }
    object$.cat("Parameters and output of ", object$.toString(), ":\n\n",
        heading = 1, consoleOutputEnabled = consoleOutputEnabled)
    object$.showParametersOfOneGroup(object$.getUserDefinedParameters(), "User defined parameters",
        orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
    object$.showParametersOfOneGroup(object$.getDerivedParameters(), "Derived from user defined parameters",
        orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
    object$.showParametersOfOneGroup(object$.getDefaultParameters(), "Default parameters",
        orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
    object$.cat("Output:\n", heading = 2, consoleOutputEnabled = consoleOutputEnabled)
    probabilities <- object$probabilities
    totalRows <- nrow(probabilities)
    if (totalRows > maxRows) {
        probabilities <- utils::head(probabilities, maxRows)
    }
    .showDataFrame(probabilities, object, niceColumnNamesEnabled = TRUE, digits = digits,
        consoleOutputEnabled = consoleOutputEnabled)
    if (totalRows > maxRows) {
        object$.cat("Preview limited to ", maxRows, " of ", totalRows, " rows.\n\n",
            consoleOutputEnabled = consoleOutputEnabled)
    }
    object$.showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)
    return(invisible(TRUE))
}

#'
#' @title
#' Extract Quantitative Decision Probability Tables
#'
#' @description
#' Returns the stored probability table of frequentist or Bayesian quantitative OCs.
#'
#' @param x An OperatingCharacteristicsMeans or OperatingCharacteristicsRates object.
#' @param row.names Optional row names for the resulting data frame.
#' @param optional Reserved for compatibility with as.data.frame.
#' @param ... Additional arguments are checked for unknown names.
#' @param niceColumnNamesEnabled Replace column identifiers with available captions.
#'
#' @return A data frame of sampling probabilities on the scale zero to one.
#'   Assumptions, including priors, remain on the originating result object.
#'
#' @export
#'
as.data.frame.OperatingCharacteristics <- function(x, row.names = NULL, optional = FALSE,
        ..., niceColumnNamesEnabled = FALSE) {
    .warnInCaseOfUnknownArguments(functionName = "as.data.frame.OperatingCharacteristics", ...)
    .assertIsSingleLogical(niceColumnNamesEnabled, "niceColumnNamesEnabled")
    result <- x$probabilities
    if (!is.null(row.names)) {
        rownames(result) <- row.names
    }
    if (niceColumnNamesEnabled) {
        captions <- vapply(names(result), function(name) {
            caption <- .getParameterCaption(name, x)
            if (length(caption) == 0L || is.na(caption) || !nzchar(caption)) {
                return(name)
            }
            return(caption)
        }, character(1))
        names(result) <- captions
    }
    return(result)
}
