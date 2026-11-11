## Endpoint-specific settings and shared output for quantitative trial results.

.isQtdOperatingCharacteristics <- function(object) {
    return(inherits(object, "OperatingCharcteristicsMeans") || inherits(object, "OperatingCharcteristicsRates"))
}

.getQtdEndpointSettings <- function(object, xAxis = NULL, functionName = ".getQtdEndpointSettings") {
    if (inherits(object, "OperatingCharcteristicsMeans")) {
        settings <- list(endpoint = "means", assumptionColumn = "stDev", assumptionCaption = "SD",
            assumptions = c("normalApproximation", "groups", "stDev"), summaryColumns = "alternative",
            effectColumn = "alternative", effectCaption = "True effect", defaultAxis = "effect",
            sensitivity = c("sampleSize", "stDev", "allocation"))
    } else if (inherits(object, "OperatingCharcteristicsRates")) {
        settings <- list(endpoint = "rates", assumptionColumn = "pi2", assumptionCaption = "Control rate",
            assumptions = c("groups", "pi2", "directionUpper", "intervalMethod"),
            summaryColumns = c("pi1", "alternative"), effectColumn = "pi1",
            effectCaption = "Treatment rate", defaultAxis = "treatmentRate",
            sensitivity = c("sampleSize", "pi2", "allocation"))
    } else {
        stopIllegalArgument("This operation requires QTD operating characteristics for means or rates",
            functionName = functionName, parameter = "x", value = object,
            constraint = "OperatingCharcteristicsMeans or OperatingCharcteristicsRates",
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

.showQtdOperatingCharacteristics <- function(object, showType, digits, consoleOutputEnabled) {
    object$.resetCat()
    if (showType == 3) {
        .createSummary(object, digits = digits)$.show(showType = 1, digits = digits,
            consoleOutputEnabled = consoleOutputEnabled)
        return(invisible(TRUE))
    }
    if (showType == 2) {
        return(invisible(FALSE))
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
    .showDataFrame(object$probabilities, object, niceColumnNamesEnabled = TRUE, digits = digits,
        consoleOutputEnabled = consoleOutputEnabled)
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
#' @param x An OperatingCharcteristicsMeans or OperatingCharcteristicsRates object.
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
as.data.frame.OperatingCharcteristics <- function(x, row.names = NULL, optional = FALSE,
        ..., niceColumnNamesEnabled = FALSE) {
    .warnInCaseOfUnknownArguments(functionName = "as.data.frame.OperatingCharcteristics", ...)
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
