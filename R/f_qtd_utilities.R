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
