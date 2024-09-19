## |
## |  *Simulation result plot functions*
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
## |  File version: $Revision: 8200 $
## |  Last changed: $Date: 2024-09-12 15:05:38 +0200 (Do, 12 Sep 2024) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_core_utilities.R
NULL

.assertIsValidVariedParameterVectorForSimulationResultsPlotting <- function(simulationResults, plotType) {
    if (inherits(simulationResults, "SimulationResultsMeans")) {
        if (is.null(simulationResults$alternative) ||
                any(is.na(simulationResults$alternative)) ||
                length(simulationResults$alternative) <= 1) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "plot type ", plotType,
                " is only available if 'alternative' with length > 1 is defined"
            )
        }
    } else if (inherits(simulationResults, "SimulationResultsRates")) {
        if (is.null(simulationResults$pi1) ||
                any(is.na(simulationResults$pi1)) ||
                length(simulationResults$pi1) <= 1) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "plot type ", plotType,
                " is only available if 'pi1' with length > 1 is defined"
            )
        }
    } else if (inherits(simulationResults, "SimulationResultsSurvival")) {
        if (is.null(simulationResults$hazardRatio) ||
                any(is.na(simulationResults$hazardRatio)) ||
                length(simulationResults$hazardRatio) <= 1) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "plot type ", plotType,
                " is only available if 'hazardRatio' with length > 1 is defined or derived"
            )
        }
        if (length(simulationResults$hazardRatio) != length(simulationResults$overallReject)) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "plot type ", plotType,
                " is not available for piecewise survival (only type 13 and 14)"
            )
        }
    }
}

.getSimulationPlotXAxisParameterName <- function(simulationResults,
        showSource = FALSE, simulationResultsName = NA_character_) {
    if (grepl("SimulationResultsEnrichment", .getClassName(simulationResults))) {
        effectDataList <- .getSimulationEnrichmentEffectData(simulationResults)
        if (ncol(effectDataList$effectData) == 1) {
            if (!isFALSE(showSource)) {
                return(paste0(simulationResultsName, "$effectList$", effectDataList$effectMatrixName, "[, 1]"))
            }

            return(sub("s$", "", effectDataList$effectMatrixName))
        }

        if (!isFALSE(showSource)) {
            numberOfSituations <- nrow(simulationResults$effectList[[effectDataList$effectMatrixName]])
            return(paste0("c(1:", numberOfSituations, ")"))
        }

        return("situation")
    }

    survivalEnabled <- grepl("Survival", .getClassName(simulationResults))
    meansEnabled <- grepl("Means", .getClassName(simulationResults))
    if (grepl("MultiArm", .getClassName(simulationResults))) {
        if (!isFALSE(showSource)) {
            gMax <- nrow(simulationResults$effectMatrix)
            return(paste0(simulationResultsName, "$effectMatrix[", gMax, ", ]"))
        }

        return("effectMatrix")
    }

    if (grepl("Survival", .getClassName(simulationResults))) {
        return("hazardRatio")
    }

    if (grepl("CountData", .getClassName(simulationResults))) {
        return("lambda1")
    }

    return("effect")
}

.getSimulationPlotXAxisLabel <- function(simulationResults, xlab = NULL) {
    if (grepl("SimulationResultsEnrichment", .getClassName(simulationResults))) {
        effectDataList <- .getSimulationEnrichmentEffectData(simulationResults)
        if (ncol(effectDataList$effectData) == 1) {
            xLabel <- .getParameterCaption(effectDataList$effectMatrixName, simulationResults)
            return(sub("s$", "", xLabel))
        }

        return("Situation")
    }

    multiArmEnabled <- grepl("MultiArm", .getClassName(simulationResults))
    userDefinedEffectMatrix <- multiArmEnabled && simulationResults$.getParameterType("effectMatrix") == C_PARAM_USER_DEFINED
    if (!is.null(xlab) && !is.na(xlab)) {
        return(xlab)
    }

    if (!multiArmEnabled) {
        return("Effect")
    }

    return(ifelse(userDefinedEffectMatrix, "Effect Matrix Row", "Maximum Effect"))
}

.getPowerAndStoppingProbabilities <- function(simulationResults, xValues, parameters) {
    yParameterNames <- c()

    if ("expectedNumberOfEvents" %in% parameters) {
        yParameterNames <- c(yParameterNames, "expectedNumberOfEvents")
    }
    if ("expectedNumberOfSubjects" %in% parameters) {
        yParameterNames <- c(yParameterNames, "expectedNumberOfSubjects")
    }
    if ("rejectAtLeastOne" %in% parameters) {
        yParameterNames <- c(yParameterNames, "rejectAtLeastOne")
    }
    if ("futilityStop" %in% parameters) {
        yParameterNames <- c(yParameterNames, "futilityStop")
    }

    yParameterNamesSrc <- yParameterNames

    data <- NULL
    for (yParameterName in yParameterNames) {
        category <- .getParameterCaption(yParameterName, simulationResults)
        part <- data.frame(
            categories = rep(category, length(xValues)),
            xValues = xValues,
            yValues = simulationResults[[yParameterName]]
        )
        if (is.null(data)) {
            data <- part
        } else {
            data <- rbind(data, part)
        }
    }

    if ("earlyStop" %in% parameters) {
        yParameterNames <- c(yParameterNames, "earlyStop")

        maxEarlyStoppingStages <- nrow(simulationResults$earlyStop)
        for (k in 1:maxEarlyStoppingStages) {
            category <- "Early stop"
            if (maxEarlyStoppingStages > 1) {
                category <- paste0(category, ", stage ", k)
            }
            part <- data.frame(
                categories = rep(category, ncol(simulationResults$earlyStop)),
                xValues = xValues,
                yValues = simulationResults$earlyStop[k, ]
            )
            data <- rbind(data, part)
            yParameterNamesSrc <- c(yParameterNamesSrc, paste0("earlyStop[", k, ", ]"))
        }
    }

    return(list(
        data = data,
        yParameterNames = yParameterNames,
        yParameterNamesSrc = yParameterNamesSrc
    ))
}

.plotSimulationResults <- function(simulationResults, designMaster, type = 5L, main = NA_character_,
        xlab = NA_character_, ylab = NA_character_, palette = "Set1",
        theta = seq(-1, 1, 0.02), plotPointsEnabled = NA,
        legendPosition = NA_integer_, showSource = FALSE,
        simulationResultsName = NA_character_, plotSettings = NULL, ...) {
    .assertGgplotIsInstalled()
    .assertIsSimulationResults(simulationResults)
    .assertIsValidLegendPosition(legendPosition)
    .assertIsSingleInteger(type, "type", naAllowed = FALSE, validateType = FALSE)
    theta <- .assertIsValidThetaRange(thetaRange = theta)

    if (is.null(plotSettings)) {
        plotSettings <- simulationResults$.plotSettings
    }

    survivalEnabled <- grepl("Survival", .getClassName(simulationResults))
    meansEnabled <- grepl("Means", .getClassName(simulationResults))
    multiArmEnabled <- grepl("MultiArm", .getClassName(simulationResults))
    enrichmentEnabled <- grepl("Enrichment", .getClassName(simulationResults))
    countDataEnabled <- grepl("CountData", .getClassName(simulationResults))
    userDefinedEffectMatrix <- multiArmEnabled && 
        simulationResults$.getParameterType("effectMatrix") == C_PARAM_USER_DEFINED

    gMax <- NA_integer_
    if (multiArmEnabled || enrichmentEnabled) {
        gMax <- ifelse(multiArmEnabled,
            simulationResults$activeArms,
            simulationResults$populations
        )
    }

    # use first value for plotting
    if (survivalEnabled) {
        nMax <- simulationResults$expectedNumberOfEvents[1] 
    } else if (countDataEnabled) {
        nMax <- simulationResults$numberOfSubjects[1] 
    } else {
        nMax <- simulationResults$expectedNumberOfSubjects[1] 
    }

    if (type %in% c(1:3) && !multiArmEnabled && !enrichmentEnabled) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'type' (", type,
            ") is not available for non-multi-arm/non-enrichment simulation results (type must be > 3)"
        )
    }

    if ((!survivalEnabled || multiArmEnabled || enrichmentEnabled) && type %in% c(10:14)) {
        if (multiArmEnabled || enrichmentEnabled) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'type' (", type,
                ") is only available for non-multi-arm/non-enrichment survival simulation results"
            )
        } else {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'type' (", type,
                ") is only available for survival simulation results"
            )
        }
    }

    variedParameters <- logical(0)

    if (is.na(plotPointsEnabled)) {
        plotPointsEnabled <- userDefinedEffectMatrix
    }

    showSourceHint <- ""

    discreteXAxis <- FALSE
    effectData <- NULL
    xValues <- NA_integer_
    if (multiArmEnabled) {
        effectData <- simulationResults$effectMatrix
        effectDataParamName <- paste0("effectMatrix", "[", gMax, ", ]")
        xParameterNameSrc <- paste0(simulationResultsName, "$", effectDataParamName)
        xValues <- effectData[gMax, ]
    } else {
        if (enrichmentEnabled) {
            effectDataList <- .getSimulationEnrichmentEffectData(simulationResults)
            xValues <- effectDataList$xValues
            discreteXAxis <- effectDataList$discreteXAxis
            if (length(xValues) <= 1) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "2 ore more situations must be specifed in ",
                    sQuote(paste0("effectList$", effectDataList$effectMatrixName))
                )
            }
        }

        xParameterNameSrc <- .getSimulationPlotXAxisParameterName(simulationResults,
            showSource = showSource, simulationResultsName = simulationResultsName
        )
    }

    armCaption <- ifelse(enrichmentEnabled, "Population", "Arm")
    armsCaption <- paste0(armCaption, "s")

    srcCmd <- NULL
    if (type == 1) { # Multi-arm, Overall Success
        .assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, type)
        if (is.na(main)) {
            main <- PlotSubTitleItems$new(title = "Overall Success")
            .addPlotSubTitleItems(simulationResults, designMaster, main, type)
        }

        data <- data.frame(
            xValues = xValues,
            yValues = colSums(simulationResults$successPerStage)
        )
        if (userDefinedEffectMatrix) {
            data$xValues <- 1:nrow(data)
        }

        legendPosition <- ifelse(is.na(legendPosition), C_POSITION_LEFT_CENTER, legendPosition)

        srcCmd <- .showPlotSourceInformation(
            objectName = simulationResultsName,
            xParameterName = xParameterNameSrc,
            yParameterNames = paste0("colSums(", simulationResultsName, "$successPerStage)"),
            hint = showSourceHint, nMax = nMax,
            type = type, showSource = showSource
        )
        if (!is.null(srcCmd)) {
            if (.isSpecialPlotShowSourceArgument(showSource)) {
                return(invisible(srcCmd))
            }
            return(srcCmd)
        }

        return(.plotDataFrame(data,
            mainTitle = main,
            xlab = NA_character_, ylab = NA_character_,
            xAxisLabel = .getSimulationPlotXAxisLabel(simulationResults),
            yAxisLabel1 = "Overall Success",
            yAxisLabel2 = NA_character_,
            plotPointsEnabled = plotPointsEnabled, legendTitle = NA_character_,
            legendPosition = legendPosition, sided = designMaster$sided,
            palette = palette, plotSettings = plotSettings,
            discreteXAxis = discreteXAxis
        ))
    } else if (type == 2) { # Multi-arm, Success per Stage
        .assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, type)
        if (is.na(main)) {
            main <- PlotSubTitleItems$new(title = "Success per Stage")
            .addPlotSubTitleItems(simulationResults, designMaster, main, type)
        }

        yParameterNamesSrc <- c()
        data <- NULL
        if (designMaster$kMax > 1) {
            for (k in 1:designMaster$kMax) {
                part <- data.frame(
                    categories = rep(k, length(xValues)),
                    xValues = xValues,
                    yValues = simulationResults$successPerStage[k, ]
                )
                if (userDefinedEffectMatrix) {
                    part$xValues <- 1:nrow(part)
                }
                if (is.null(data)) {
                    data <- part
                } else {
                    data <- rbind(data, part)
                }
                yParameterNamesSrc <- c(yParameterNamesSrc, paste0("successPerStage[", k, ", ]"))
            }
        } else {
            data <- data.frame(
                xValues = xValues,
                yValues = simulationResults$successPerStage[1, ]
            )
            if (userDefinedEffectMatrix) {
                data$xValues <- 1:nrow(data)
            }
            yParameterNamesSrc <- c(yParameterNamesSrc, "successPerStage[1, ]")
        }

        legendPosition <- ifelse(is.na(legendPosition), C_POSITION_LEFT_TOP, legendPosition)

        srcCmd <- .showPlotSourceInformation(
            objectName = simulationResultsName,
            xParameterName = xParameterNameSrc,
            yParameterNames = yParameterNamesSrc,
            hint = showSourceHint, nMax = nMax,
            type = type, showSource = showSource
        )
        if (!is.null(srcCmd)) {
            if (.isSpecialPlotShowSourceArgument(showSource)) {
                return(invisible(srcCmd))
            }
            return(srcCmd)
        }

        return(.plotDataFrame(data,
            mainTitle = main,
            xlab = NA_character_, ylab = NA_character_,
            xAxisLabel = .getSimulationPlotXAxisLabel(simulationResults),
            yAxisLabel1 = "Success",
            yAxisLabel2 = NA_character_,
            plotPointsEnabled = plotPointsEnabled, legendTitle = "Stage",
            legendPosition = legendPosition, sided = designMaster$sided,
            palette = palette, plotSettings = plotSettings,
            discreteXAxis = discreteXAxis
        ))
    } else if (type == 3) { # Multi-arm, Selected Arms/Populations per Stage

        .assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, type)
        if (is.na(main)) {
            main <- PlotSubTitleItems$new(title = paste0("Selected ", armsCaption, " per Stage"))
            .addPlotSubTitleItems(simulationResults, designMaster, main, type)
        }

        selectedDataParamName <- ifelse(multiArmEnabled, "selectedArms", "selectedPopulations")
        selectedData <- simulationResults[[selectedDataParamName]]

        yParameterNamesSrc <- c()
        data <- NULL
        if (designMaster$kMax > 1) {
            for (g in 1:gMax) {
                for (k in 2:designMaster$kMax) {
                    stages <- rep(k, length(xValues))

                    populationCaption <- g
                    if (enrichmentEnabled) {
                        populationCaption <- ifelse(g == gMax, "F", paste0("S", g))
                    }

                    part <- data.frame(
                        categories = ifelse(designMaster$kMax > 2,
                            paste0(populationCaption, ", ", stages), populationCaption
                        ),
                        xValues = xValues,
                        yValues = selectedData[k, , g]
                    )
                    if (userDefinedEffectMatrix) {
                        part$xValues <- 1:nrow(part)
                    }
                    if (is.null(data)) {
                        data <- part
                    } else {
                        data <- rbind(data, part)
                    }
                    yParameterNamesSrc <- c(yParameterNamesSrc, paste0(selectedDataParamName, "[", k, ", , ", g, "]"))
                }
            }
        } else {
            for (g in 1:gMax) {
                part <- data.frame(
                    categories = g,
                    xValues = xValues,
                    yValues = selectedData[1, , g]
                )
                if (userDefinedEffectMatrix) {
                    data$xValues <- 1:nrow(data)
                }
                if (is.null(data)) {
                    data <- part
                } else {
                    data <- rbind(data, part)
                }
                yParameterNamesSrc <- c(yParameterNamesSrc, paste0(selectedDataParamName, "[1, , ", g, "]"))
            }
        }

        legendPosition <- ifelse(is.na(legendPosition), C_POSITION_LEFT_TOP, legendPosition)

        srcCmd <- .showPlotSourceInformation(
            objectName = simulationResultsName,
            xParameterName = xParameterNameSrc,
            yParameterNames = yParameterNamesSrc,
            hint = showSourceHint, nMax = nMax,
            type = type, showSource = showSource
        )
        if (!is.null(srcCmd)) {
            if (.isSpecialPlotShowSourceArgument(showSource)) {
                return(invisible(srcCmd))
            }
            return(srcCmd)
        }

        legendTitle <- ifelse(gMax > 1,
            ifelse(designMaster$kMax > 2, paste0(armCaption, ", Stage"), armCaption),
            ifelse(designMaster$kMax > 2, "Stage", armCaption)
        )
        return(.plotDataFrame(data,
            mainTitle = main,
            xlab = NA_character_, ylab = NA_character_,
            xAxisLabel = .getSimulationPlotXAxisLabel(simulationResults),
            yAxisLabel1 = paste0("Selected ", armsCaption),
            yAxisLabel2 = NA_character_,
            plotPointsEnabled = plotPointsEnabled,
            legendTitle = legendTitle,
            legendPosition = legendPosition, sided = designMaster$sided,
            palette = palette, plotSettings = plotSettings,
            discreteXAxis = discreteXAxis
        ))
    } else if (type == 4) { # Multi-arm, Rejected Arms/Populations per Stage
        .assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, type)
        if (is.na(main)) {
            main <- PlotSubTitleItems$new(title = ifelse(!multiArmEnabled,
                "Reject per Stage",
                ifelse(designMaster$kMax > 1,
                    paste0("Rejected ", armsCaption, " per Stage"), paste0("Rejected ", armsCaption)
                )
            ))
            .addPlotSubTitleItems(simulationResults, designMaster, main, type)
        }

        yParameterNamesSrc <- c()
        data <- NULL
        if (multiArmEnabled || enrichmentEnabled) {
            rejectedDataParamName <- ifelse(multiArmEnabled, "rejectedArmsPerStage", "rejectedPopulationsPerStage")
            rejectedData <- simulationResults[[rejectedDataParamName]]
            if (designMaster$kMax > 1) {
                for (g in 1:gMax) {
                    for (k in 1:designMaster$kMax) {
                        stages <- rep(k, length(xValues))
                        populationCaption <- g
                        if (enrichmentEnabled) {
                            populationCaption <- ifelse(g == gMax, "F", paste0("S", g))
                        }
                        part <- data.frame(
                            categories = ifelse(gMax > 1, paste0(populationCaption, ", ", stages), stages),
                            xValues = xValues,
                            yValues = rejectedData[k, , g]
                        )
                        if (userDefinedEffectMatrix) {
                            part$xValues <- 1:nrow(part)
                        }
                        if (is.null(data)) {
                            data <- part
                        } else {
                            data <- rbind(data, part)
                        }
                        yParameterNamesSrc <- c(yParameterNamesSrc, paste0(rejectedDataParamName, "[", k, ", , ", g, "]"))
                    }
                }
            } else {
                for (g in 1:gMax) {
                    part <- data.frame(
                        categories = g,
                        xValues = xValues,
                        yValues = rejectedData[1, , g]
                    )
                    if (userDefinedEffectMatrix) {
                        part$xValues <- 1:nrow(part)
                    }
                    if (is.null(data)) {
                        data <- part
                    } else {
                        data <- rbind(data, part)
                    }
                    yParameterNamesSrc <- c(yParameterNamesSrc, paste0(rejectedDataParamName, "[1, , ", g, "]"))
                }
            }
        } else {
            xParameterName <- .getSimulationPlotXAxisParameterName(simulationResults)
            if (designMaster$kMax > 1) {
                for (k in 1:designMaster$kMax) {
                    part <- data.frame(
                        categories = k,
                        xValues = simulationResults[[xParameterName]],
                        yValues = simulationResults$rejectPerStage[k, ]
                    )
                    if (userDefinedEffectMatrix) {
                        part$xValues <- 1:nrow(part)
                    }
                    if (is.null(data)) {
                        data <- part
                    } else {
                        data <- rbind(data, part)
                    }
                    yParameterNamesSrc <- c(yParameterNamesSrc, paste0("rejectPerStage[", k, ", ]"))
                }
            } else {
                data <- data.frame(
                    xValues = simulationResults[[xParameterName]],
                    yValues = simulationResults$rejectPerStage[1, ]
                )
                if (userDefinedEffectMatrix) {
                    data$xValues <- 1:nrow(data)
                }
                yParameterNamesSrc <- c(yParameterNamesSrc, "rejectPerStage[1, ]")
            }
        }

        legendPosition <- ifelse(is.na(legendPosition), C_POSITION_LEFT_TOP, legendPosition)

        srcCmd <- .showPlotSourceInformation(
            objectName = simulationResultsName,
            xParameterName = xParameterNameSrc,
            yParameterNames = yParameterNamesSrc,
            hint = showSourceHint, nMax = nMax,
            type = type, showSource = showSource
        )
        if (!is.null(srcCmd)) {
            if (.isSpecialPlotShowSourceArgument(showSource)) {
                return(invisible(srcCmd))
            }
            return(srcCmd)
        }

        palette <- NULL

        legendTitle <- "Stage"
        if (multiArmEnabled) {
            legendTitle <- ifelse(designMaster$kMax > 1, paste0(armCaption, ", Stage"), armCaption)
        } else if (enrichmentEnabled) {
            legendTitle <- ifelse(gMax > 1, paste0(armCaption, ", Stage"), "Stage")
        }
        yAxisLabel1 <- ifelse(.isMultiArmSimulationResults(simulationResults),
            paste0("Rejected ", armsCaption), "Rejection Probability"
        )
        return(.plotDataFrame(data,
            mainTitle = main,
            xlab = NA_character_, ylab = NA_character_,
            xAxisLabel = .getSimulationPlotXAxisLabel(simulationResults),
            yAxisLabel1 = yAxisLabel1,
            yAxisLabel2 = NA_character_,
            plotPointsEnabled = plotPointsEnabled,
            legendTitle = legendTitle,
            legendPosition = legendPosition, sided = designMaster$sided,
            palette = palette, plotSettings = plotSettings,
            discreteXAxis = discreteXAxis
        ))
    } else if (type == 5) { # Power and Stopping Probabilities

        .assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, type)

        if (is.na(main)) {
            main <- PlotSubTitleItems$new(title = ifelse(designMaster$kMax == 1,
                "Overall Power", "Overall Power and Early Stopping"
            ))
            .addPlotSubTitleItems(simulationResults, designMaster, main, type)
        }

        xParameterName <- .getSimulationPlotXAxisParameterName(simulationResults)

        if ((multiArmEnabled || enrichmentEnabled) && designMaster$kMax > 1) {
            powerAndStoppingProbabilities <- .getPowerAndStoppingProbabilities(simulationResults,
                xValues = xValues,
                parameters = c("rejectAtLeastOne", "futilityStop", "earlyStop")
            )
            data <- powerAndStoppingProbabilities$data
            yParameterNames <- powerAndStoppingProbabilities$yParameterNames
            yParameterNamesSrc <- powerAndStoppingProbabilities$yParameterNamesSrc
        } else {
            yParameterNames <- ifelse(multiArmEnabled || enrichmentEnabled, "rejectAtLeastOne", "overallReject")
            if (designMaster$kMax > 1) {
                if (!multiArmEnabled && !enrichmentEnabled) {
                    yParameterNames <- c(yParameterNames, "earlyStop")
                }
                yParameterNames <- c(yParameterNames, "futilityStop")
            }
            yParameterNamesSrc <- yParameterNames
        }

        xlab <- .getSimulationPlotXAxisLabel(simulationResults, xlab)
        ylab <- ifelse(is.na(ylab), "", ylab)
        legendPosition <- ifelse(is.na(legendPosition), C_POSITION_LEFT_TOP, legendPosition)

        srcCmd <- .showPlotSourceInformation(
            objectName = simulationResultsName,
            xParameterName = xParameterNameSrc,
            yParameterNames = yParameterNamesSrc,
            hint = showSourceHint, nMax = nMax,
            type = type, showSource = showSource
        )
        if (!is.null(srcCmd)) {
            if (.isSpecialPlotShowSourceArgument(showSource)) {
                return(invisible(srcCmd))
            }
            return(srcCmd)
        }

        if ((multiArmEnabled || enrichmentEnabled) && designMaster$kMax > 1) {
            return(.plotDataFrame(data,
                mainTitle = main,
                xlab = xlab, ylab = ylab,
                xAxisLabel = .getSimulationPlotXAxisLabel(simulationResults),
                yAxisLabel1 = NA_character_,
                yAxisLabel2 = NA_character_,
                plotPointsEnabled = plotPointsEnabled,
                legendTitle = NA_character_,
                legendPosition = legendPosition, sided = designMaster$sided,
                palette = palette, plotSettings = plotSettings,
                discreteXAxis = discreteXAxis
            ))
        } else {
            if (is.null(list(...)[["ylim"]])) {
                ylim <- c(0, 1)
                return(.plotParameterSet(
                    parameterSet = simulationResults, designMaster = designMaster,
                    xParameterName = xParameterName,
                    yParameterNames = yParameterNames, mainTitle = main, xlab = xlab, ylab = ylab,
                    palette = palette, theta = theta, nMax = nMax, plotPointsEnabled = plotPointsEnabled,
                    legendPosition = legendPosition, variedParameters = variedParameters,
                    qnormAlphaLineEnabled = FALSE, yAxisScalingEnabled = FALSE,
                    plotSettings = plotSettings, ylim = ylim # , ...
                )) # ratioEnabled = TRUE
            } else {
                return(.plotParameterSet(
                    parameterSet = simulationResults, designMaster = designMaster,
                    xParameterName = xParameterName,
                    yParameterNames = yParameterNames, mainTitle = main, xlab = xlab, ylab = ylab,
                    palette = palette, theta = theta, nMax = nMax, plotPointsEnabled = plotPointsEnabled,
                    legendPosition = legendPosition, variedParameters = variedParameters,
                    qnormAlphaLineEnabled = FALSE, yAxisScalingEnabled = FALSE,
                    plotSettings = plotSettings # , ...
                ))
            }
        }
    } else if (type == 6) { # Average Sample Size / Average Event Number
        .assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, type)

        if (is.na(main)) {
            titlePart <- paste0("Expected ", ifelse(survivalEnabled, "Number of Events", "Number of Subjects"))
            main <- PlotSubTitleItems$new(title = paste0(
                titlePart,
                ifelse(designMaster$kMax == 1, "", paste0(
                    " and Power",
                    ifelse(multiArmEnabled, "", " / Early Stop")
                ))
            ))
            .addPlotSubTitleItems(simulationResults, designMaster, main, type)
        }

        xParameterName <- .getSimulationPlotXAxisParameterName(simulationResults)
        yParameterNames <- ifelse(survivalEnabled, "expectedNumberOfEvents", "expectedNumberOfSubjects")
        if (designMaster$kMax > 1) {
            if (multiArmEnabled || enrichmentEnabled) {
                yParameterNames <- c(yParameterNames, "rejectAtLeastOne")
            } else {
                yParameterNames <- c(yParameterNames, "overallReject")
            }
            yParameterNames <- c(yParameterNames, "earlyStop")
        }

        xlab <- .getSimulationPlotXAxisLabel(simulationResults, xlab)
        legendPosition <- ifelse(is.na(legendPosition), C_POSITION_LEFT_CENTER, legendPosition)
        srcCmd <- .showPlotSourceInformation(
            objectName = simulationResultsName,
            xParameterName = xParameterNameSrc,
            yParameterNames = yParameterNames,
            hint = showSourceHint, nMax = nMax,
            type = type, showSource = showSource
        )
    } else if (type == 7) {
        .assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, type)
        if (is.na(main)) {
            main <- PlotSubTitleItems$new(title = "Overall Power")
            .addPlotSubTitleItems(simulationResults, designMaster, main, type)
        }

        xParameterName <- .getSimulationPlotXAxisParameterName(simulationResults)
        yParameterNames <- ifelse(multiArmEnabled || enrichmentEnabled, "rejectAtLeastOne", "overallReject")
        xlab <- .getSimulationPlotXAxisLabel(simulationResults, xlab)
        legendPosition <- ifelse(is.na(legendPosition), C_POSITION_RIGHT_CENTER, legendPosition)
        srcCmd <- .showPlotSourceInformation(
            objectName = simulationResultsName,
            xParameterName = xParameterNameSrc,
            yParameterNames = yParameterNames,
            hint = showSourceHint, nMax = nMax,
            type = type, showSource = showSource
        )
    } else if (type == 8) {
        if (designMaster$kMax == 1) {
            stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "plot type 8 (Early Stopping) is not available for 'kMax' = 1")
        }

        .assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, type)

        futilityStopEnabled <- !is.null(simulationResults[["futilityStop"]]) &&
            !all(na.omit(simulationResults$futilityStop) == 0)

        if (is.na(main)) {
            main <- PlotSubTitleItems$new(title = paste0(
                "Overall Early Stopping",
                ifelse(futilityStopEnabled, " and Futility Stopping", "")
            ))
            .addPlotSubTitleItems(simulationResults, designMaster, main, type)
        }

        xParameterName <- .getSimulationPlotXAxisParameterName(simulationResults)
        yParameterNames <- c("earlyStop")
        if (futilityStopEnabled) {
            yParameterNames <- c(yParameterNames, "futilityStop")
        }
        xlab <- .getSimulationPlotXAxisLabel(simulationResults, xlab)
        legendPosition <- ifelse(is.na(legendPosition), C_POSITION_LEFT_CENTER, legendPosition)
        srcCmd <- .showPlotSourceInformation(
            objectName = simulationResultsName,
            xParameterName = xParameterNameSrc,
            yParameterNames = yParameterNames,
            hint = showSourceHint, nMax = nMax,
            type = type, showSource = showSource
        )
    } else if (type == 9) {
        .assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, type)

        if (is.na(main)) {
            main <- PlotSubTitleItems$new(title = ifelse(survivalEnabled,
                "Expected Number of Events", "Expected Number of Subjects"
            ))
            .addPlotSubTitleItems(simulationResults, designMaster, main, type)
        }

        xParameterName <- .getSimulationPlotXAxisParameterName(simulationResults)
        yParameterNames <- ifelse(survivalEnabled, "expectedNumberOfEvents", "expectedNumberOfSubjects")
        xlab <- .getSimulationPlotXAxisLabel(simulationResults, xlab)
        srcCmd <- .showPlotSourceInformation(
            objectName = simulationResultsName,
            xParameterName = xParameterNameSrc,
            yParameterNames = yParameterNames,
            hint = showSourceHint, nMax = nMax,
            type = type, showSource = showSource
        )
    } else if (type == 10) { # Study Duration
        .assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, type)
        if (is.na(main)) {
            main <- PlotSubTitleItems$new(title = "Study Duration")
            .addPlotSubTitleItems(simulationResults, designMaster, main, type)
        }
        xParameterName <- "hazardRatio"
        yParameterNames <- "studyDuration"
        srcCmd <- .showPlotSourceInformation(
            objectName = simulationResultsName,
            xParameterName = xParameterName,
            yParameterNames = yParameterNames,
            hint = showSourceHint, nMax = nMax,
            type = type, showSource = showSource
        )
    } else if (type == 11) {
        .assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, type)
        if (is.na(main)) {
            main <- PlotSubTitleItems$new(title = "Expected Number of Subjects")
            .addPlotSubTitleItems(simulationResults, designMaster, main, type)
        }
        xParameterName <- "hazardRatio"
        yParameterNames <- "expectedNumberOfSubjects"
        srcCmd <- .showPlotSourceInformation(
            objectName = simulationResultsName,
            xParameterName = xParameterName,
            yParameterNames = yParameterNames,
            hint = showSourceHint, nMax = nMax,
            type = type, showSource = showSource
        )
    } else if (type == 12) { # Analysis Time
        .assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, type)
        if (is.na(main)) {
            main <- PlotSubTitleItems$new(title = "Analysis Time")
            .addPlotSubTitleItems(simulationResults, designMaster, main, type)
        }

        xParameterName <- "hazardRatio"
        yParameterNames <- "analysisTime"
        yParameterNamesSrc <- c()
        for (i in 1:nrow(simulationResults[["analysisTime"]])) {
            yParameterNamesSrc <- c(yParameterNamesSrc, paste0("analysisTime[", i, ", ]"))
        }

        data <- NULL
        for (k in 1:designMaster$kMax) {
            part <- data.frame(
                categories = rep(k, length(simulationResults$hazardRatio)),
                xValues = simulationResults$hazardRatio,
                yValues = simulationResults$analysisTime[k, ]
            )
            if (is.null(data)) {
                data <- part
            } else {
                data <- rbind(data, part)
            }
        }

        if (is.na(legendPosition)) {
            legendPosition <- C_POSITION_LEFT_CENTER
        }

        srcCmd <- .showPlotSourceInformation(
            objectName = simulationResultsName,
            xParameterName = xParameterName,
            yParameterNames = yParameterNamesSrc,
            hint = showSourceHint, nMax = nMax,
            type = type, showSource = showSource
        )
        if (!is.null(srcCmd)) {
            if (.isSpecialPlotShowSourceArgument(showSource)) {
                return(invisible(srcCmd))
            }
            return(srcCmd)
        }

        return(.plotDataFrame(data,
            mainTitle = main,
            xlab = NA_character_, ylab = NA_character_, xAxisLabel = "Hazard Ratio",
            yAxisLabel1 = "Analysis Time", yAxisLabel2 = NA_character_,
            plotPointsEnabled = TRUE, legendTitle = "Stage",
            legendPosition = legendPosition, sided = designMaster$sided, plotSettings = plotSettings,
            discreteXAxis = discreteXAxis
        ))
    } else if (type == 13 || type == 14) { # Cumulative Distribution Function / Survival function
        return(.plotSurvivalFunction(simulationResults,
            designMaster = designMaster, type = type, main = main,
            xlab = xlab, ylab = ylab, palette = palette,
            legendPosition = legendPosition, designPlanName = simulationResultsName,
            showSource = showSource, plotSettings = plotSettings
        ))
    } else {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'type' (", type, ") is not allowed; must be 5, 6, ..., 14")
    }

    if (!is.null(srcCmd)) {
        if (.isSpecialPlotShowSourceArgument(showSource)) {
            return(invisible(srcCmd))
        }
        return(srcCmd)
    }

    return(.plotParameterSet(
        parameterSet = simulationResults, designMaster = designMaster,
        xParameterName = xParameterName,
        yParameterNames = yParameterNames, mainTitle = main, xlab = xlab, ylab = ylab,
        palette = palette, theta = theta, nMax = nMax, plotPointsEnabled = plotPointsEnabled,
        legendPosition = legendPosition, variedParameters = variedParameters,
        qnormAlphaLineEnabled = (type != 2), ratioEnabled = TRUE, plotSettings = plotSettings # , ...
    ))
}

#'
#' @title
#' Simulation Results Plotting
#'
#' @param x The simulation results, obtained from \cr
#'        \code{\link[=getSimulationSurvival]{getSimulationSurvival()}}.
#' @param y Not available for this kind of plot (is only defined to be compatible to the generic plot function).
#' @param main The main title.
#' @param xlab The x-axis label.
#' @param ylab The y-axis label.
#' @inheritParams param_palette
#' @inheritParams param_theta
#' @inheritParams param_plotPointsEnabled
#' @inheritParams param_showSource
#' @inheritParams param_plotSettings
#' @inheritParams param_legendPosition
#' @inheritParams param_grid
#' @param type The plot type (default = \code{1}). The following plot types are available:
#' \itemize{
#'   \item \code{1}: creates a 'Overall Success' plot (multi-arm and enrichment only)
#'   \item \code{2}: creates a 'Success per Stage' plot (multi-arm and enrichment  only)
#'   \item \code{3}: creates a 'Selected Arms per Stage' plot (multi-arm and enrichment  only)
#'   \item \code{4}: creates a 'Reject per Stage' or 'Rejected Arms per Stage' plot
#'   \item \code{5}: creates a 'Overall Power and Early Stopping' plot
#'   \item \code{6}: creates a 'Expected Number of Subjects and Power / Early Stop' or
#'         'Expected Number of Events and Power / Early Stop' plot
#'   \item \code{7}: creates an 'Overall Power' plot
#'   \item \code{8}: creates an 'Overall Early Stopping' plot
#'   \item \code{9}: creates an 'Expected Sample Size' or 'Expected Number of Events' plot
#'   \item \code{10}: creates a 'Study Duration' plot (non-multi-arm and non-enrichment survival only)
#'   \item \code{11}: creates an 'Expected Number of Subjects' plot (non-multi-arm and non-enrichment survival only)
#'   \item \code{12}: creates an 'Analysis Times' plot (non-multi-arm and non-enrichment survival only)
#'   \item \code{13}: creates a 'Cumulative Distribution Function' plot (non-multi-arm and non-enrichment survival only)
#'   \item \code{14}: creates a 'Survival Function' plot (non-multi-arm and non-enrichment survival only)
#'   \item \code{"all"}: creates all available plots and returns it as a grid plot or list
#' }
#' @inheritParams param_three_dots_plot
#'
#' @description
#' Plots simulation results.
#'
#' @details
#' Generic function to plot all kinds of simulation results.
#'
#' @template return_object_ggplot
#'
#' @examples
#' \dontrun{
#' results <- getSimulationMeans(
#'     alternative = 0:4, stDev = 5,
#'     plannedSubjects = 40, maxNumberOfIterations = 1000
#' )
#' plot(results, type = 5)
#' }
#'
#' @export
#'
plot.SimulationResults <- function(
        x, 
        y, 
        ..., 
        main = NA_character_,
        xlab = NA_character_, 
        ylab = NA_character_, 
        type = NA_integer_, 
        palette = "Set1",
        theta = seq(-1, 1, 0.01), 
        plotPointsEnabled = NA,
        legendPosition = NA_integer_, 
        showSource = FALSE,
        grid = 1, 
        plotSettings = NULL) {
    
    .assertIsValidPlotType(type, naAllowed = TRUE)
    if (is.na(type)) {
        type <- na.omit(getAvailablePlotTypes(x))
        if (length(type) == 0) {
            stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "not plot type available")
        }
        
        type <- type[1]
    }
    .assertIsSingleInteger(grid, "grid", validateType = FALSE)
    markdown <- .getOptionalArgument("markdown", ..., optionalArgumentDefaultValue = NA)
    if (is.na(markdown)) {
        markdown <- .isMarkdownEnabled("plot")
    }
    
    args <- list(
        x = x, 
        y = NULL,
        main = main,
        xlab = xlab,
        ylab = ylab,
        type = type,
        palette = palette,
        theta = theta, 
        plotPointsEnabled = plotPointsEnabled,
        legendPosition = legendPosition,
        showSource = showSource,
        grid = grid,
        plotSettings = plotSettings, 
        ...)
    
    if (markdown) {
        sep <- .getMarkdownPlotPrintSeparator()
        if (!all(is.na(type)) && length(type) > 1 && grid == 1) {
            grid <- 0
            args$grid <- 0
        }
        if (grid > 0) {
            print(do.call(.plot.SimulationResults, args))            
        } else {
            do.call(.plot.SimulationResults, args)
        }
        return(.knitPrintQueue(x, sep = sep, prefix = sep))
    }
    
    return(do.call(.plot.SimulationResults, args))
}

.plot.SimulationResults <- function(
        x, 
        y, 
        ..., 
        main = NA_character_,
        xlab = NA_character_, 
        ylab = NA_character_, 
        type = 1L, 
        palette = "Set1",
        theta = seq(-1, 1, 0.01), 
        plotPointsEnabled = NA,
        legendPosition = NA_integer_, 
        showSource = FALSE,
        grid = 1, 
        plotSettings = NULL) {
        
    fCall <- match.call(expand.dots = FALSE)
    simulationResultsName <- deparse(fCall$x)
    .assertGgplotIsInstalled()
    .assertIsSingleInteger(grid, "grid", validateType = FALSE)
    typeNumbers <- .getPlotTypeNumber(type, x)
    if (is.null(plotSettings)) {
        plotSettings <- .getGridPlotSettings(x, typeNumbers, grid)
    }
    p <- NULL
    plotList <- list()
    for (typeNumber in typeNumbers) {
        p <- .plotSimulationResults(
            simulationResults = x, designMaster = x$.design,
            main = main, xlab = xlab, ylab = ylab, type = typeNumber,
            palette = palette, theta = theta, plotPointsEnabled = plotPointsEnabled,
            legendPosition = .getGridLegendPosition(legendPosition, typeNumbers, grid),
            showSource = showSource, simulationResultsName = simulationResultsName,
            plotSettings = plotSettings, ...
        )
        .printPlotShowSourceSeparator(showSource, typeNumber, typeNumbers)
        if (length(typeNumbers) > 1) {
            caption <- .getPlotCaption(x, typeNumber, stopIfNotFound = TRUE)
            plotList[[caption]] <- p
        }
    }
    if (length(typeNumbers) == 1) {
        if (.isSpecialPlotShowSourceArgument(showSource)) {
            return(invisible(p))
        }

        return(p)
    }

    if (.isSpecialPlotShowSourceArgument(showSource)) {
        return(invisible(plotList))
    }

    return(.createPlotResultObject(plotList, grid))
}
