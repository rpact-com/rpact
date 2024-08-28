## |
## |  *Trial design plan plot*
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
## |  File version: $Revision: 8141 $
## |  Last changed: $Date: 2024-08-28 15:03:46 +0200 (Mi, 28 Aug 2024) $
## |  Last changed by: $Author: pahlke $
## |


.addPlotSubTitleItems <- function(designPlan, designMaster, items, type) {
    if (type %in% c(1, 3, 4)) {
        return(invisible())
    }

    if (.isTrialDesignPlanMeans(designPlan)) {
        nMax <- designPlan$maxNumberOfSubjects[1] # use first value for plotting

        if (!(type %in% c(5))) {
            items$add("N", round(nMax, 1), "max")
        }

        if ((type %in% c(5)) && !(items$title == "Sample Size")) {
            items$add("N", round(nMax, 1), "max")
        }

        if (designPlan$meanRatio) {
            items$add("coefficient of variation", designPlan$stDev)
        } else {
            items$add("standard deviation", designPlan$stDev)
        }

        if (designPlan$groups == 1) {
            if (type %in% c(2, (5:9))) {
                items$add("H0: mu", designPlan$thetaH0)
                items$add("allocation ratio", round(designPlan$allocationRatioPlanned, 2),
                    condition = (designPlan$allocationRatioPlanned != 1)
                )
            }
        } else {
            if (type %in% c(2, (5:9))) {
                if (designPlan$meanRatio) {
                    items$add("H0: mean ratio", designPlan$thetaH0)
                } else {
                    items$add("H0: mean difference", designPlan$thetaH0)
                }
                items$add("allocation ratio", round(designPlan$allocationRatioPlanned, 2),
                    condition = (designPlan$allocationRatioPlanned != 1)
                )
            }
        }
    } else if (.isTrialDesignPlanRates(designPlan)) {
        nMax <- designPlan$maxNumberOfSubjects[1] # use first value for plotting

        if (!(type %in% c(5))) {
            items$add("N", round(nMax, 1), "max")
        }

        if ((type %in% c(5)) && !(items$title == "Sample Size")) {
            items$add("N", round(nMax, 1), "max")
        }

        if (designPlan$groups == 2 && !(type %in% c(3, 4)) &&
                length(designPlan$pi2) == 1 && !is.na(designPlan$pi2)) {
            items$add("pi", designPlan$pi2, 2)
        }

        if (designPlan$groups == 1) {
            if (type %in% c(2, (5:9))) {
                items$add("H0: pi", designPlan$thetaH0)
            }
        } else {
            if (type %in% c(2, (5:9))) {
                if (designPlan$riskRatio) {
                    items$add("H0: risk ratio", designPlan$thetaH0)
                } else {
                    items$add("H0: risk difference", designPlan$thetaH0)
                }
                items$add("allocation ratio", round(designPlan$allocationRatioPlanned, 2),
                    condition = (designPlan$allocationRatioPlanned != 1)
                )
            }
        }
    } else if (.isTrialDesignPlanSurvival(designPlan)) {
        if (designPlan$.isPowerObject() && !(type %in% (13:14))) {
            items$add("maximum number of events", designPlan$maxNumberOfEvents[1])
        }
        if (type %in% (10:12)) {
            items$add("maximum number of subjects", designPlan$maxNumberOfSubjects[1])
        }
        if (type %in% c(2, (5:12))) {
            items$add("H0: hazard ratio", designPlan$thetaH0)
            items$add("allocation ratio", round(designPlan$allocationRatioPlanned, 2),
                condition = (designPlan$allocationRatioPlanned != 1)
            )
        }
    } else if (.isTrialDesignPlanCountData(designPlan)) {
        if (type %in% c(2, (5:9))) {
            items$add("H0: lambda(1) / lambda(2)", designPlan$thetaH0)
            if (length(designPlan$theta) == 1) {
                items$add("H1: effect", designPlan$theta)
            }
            items$add("allocation ratio", round(designPlan$allocationRatioPlanned, 2),
                condition = (designPlan$allocationRatioPlanned != 1)
            )
        }
    }
}

.assertIsValidVariedParameterVectorForPlotting <- function(designPlan, plotType) {
    if (.isTrialDesignPlanMeans(designPlan)) {
        if (is.null(designPlan$alternative) || any(is.na(designPlan$alternative)) ||
                length(designPlan$alternative) <= 1) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "plot type ", plotType,
                " is only available if 'alternative' with length > 1 is defined"
            )
        }
    } else if (.isTrialDesignPlanRates(designPlan)) {
        if (is.null(designPlan$pi1) || any(is.na(designPlan$pi1)) ||
                length(designPlan$pi1) <= 1) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "plot type ", plotType,
                " is only available if 'pi1' with length > 1 is defined"
            )
        }
    } else if (.isTrialDesignPlanSurvival(designPlan)) {
        if (is.null(designPlan$hazardRatio) || any(is.na(designPlan$hazardRatio)) ||
                length(designPlan$hazardRatio) <= 1) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "plot type ", plotType,
                " is only available if 'hazardRatio' with length > 1 is defined"
            )
        }
    }
}

.getTrialDesignPlanTheta <- function(designPlan, theta) {
    thetaName <- NA_character_
    if (.isTrialDesignPlanMeans(designPlan) &&
            designPlan$.getParameterType("alternative") %in% c(C_PARAM_USER_DEFINED, C_PARAM_DEFAULT_VALUE)) {
        thetaName <- "alternative"
    } else if ((.isTrialDesignPlanRates(designPlan) || .isTrialDesignPlanSurvival(designPlan)) &&
            designPlan$.getParameterType("pi1") %in% c(C_PARAM_USER_DEFINED, C_PARAM_DEFAULT_VALUE)) {
        thetaName <- "pi1"
    } else if (.isTrialDesignPlanCountData(designPlan) &&
            designPlan$.getParameterType("theta") %in% c(C_PARAM_USER_DEFINED, C_PARAM_DEFAULT_VALUE)) {
        thetaName <- "theta"
    } else if (.isTrialDesignPlanCountData(designPlan) &&
            designPlan$.getParameterType("lambda1") %in% c(C_PARAM_USER_DEFINED, C_PARAM_DEFAULT_VALUE)) {
        thetaName <- "lambda1"
    } else if (.isTrialDesignPlanSurvival(designPlan) &&
            designPlan$.getParameterType("hazardRatio") %in% c(C_PARAM_USER_DEFINED, C_PARAM_DEFAULT_VALUE)) {
        thetaName <- "hazardRatio"
    }
    if (is.na(thetaName)) {
        return(list(theta = NA_real_, thetaName = thetaName))
    }

    if (!is.null(theta) && length(theta) > 1 && !all(is.na(theta))) {
        return(list(theta = theta, thetaName = thetaName))
    }

    return(list(theta = designPlan[[thetaName]], thetaName = thetaName))
}

.plotTrialDesignPlan <- function(designPlan,
        type = 1L,
        main = NA_character_,
        xlab = NA_character_,
        ylab = NA_character_,
        palette = "Set1",
        theta = NA_real_,
        plotPointsEnabled = NA,
        legendPosition = NA_integer_,
        showSource = FALSE,
        designPlanName = NA_character_,
        plotSettings = NULL, ...) {
    .assertGgplotIsInstalled()
    .assertIsTrialDesignPlan(designPlan)
    .assertIsValidLegendPosition(legendPosition)
    .assertIsSingleInteger(type, "type", naAllowed = FALSE, validateType = FALSE)

    availablePlotTypes <- getAvailablePlotTypes(designPlan, output = "numeric", numberInCaptionEnabled = FALSE)
    if (!(type %in% availablePlotTypes)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'type' (", type,
            ") is not available; 'type' can ", ifelse(length(availablePlotTypes) == 1, "only ", ""),
            "be ", .arrayToString(availablePlotTypes, mode = "or")
        )
    }

    survivalDesignPlanEnabled <- .isTrialDesignPlanSurvival(designPlan)

    nMax <- NA_integer_
    if (!.isTrialDesignPlanCountData(designPlan)) {
        nMax <- ifelse(survivalDesignPlanEnabled,
            designPlan$maxNumberOfEvents[1],
            designPlan$maxNumberOfSubjects[1]
        ) # use first value for plotting
    }

    if (is.null(plotSettings)) {
        plotSettings <- designPlan$.plotSettings
    }

    designMaster <- designPlan$.design

    if (is.na(plotPointsEnabled)) {
        plotPointsEnabled <- type < 4
    }

    ratioEnabled <- (survivalDesignPlanEnabled ||
        (.isTrialDesignPlanMeans(designPlan) && designPlan$meanRatio) ||
        (.isTrialDesignPlanRates(designPlan) && designPlan$riskRatio))

    variedParameters <- logical(0)

    showSourceHint <- ""
    if (type %in% c(5:12)) {
        result <- .getTrialDesignPlanTheta(designPlan, theta)
        if (!all(is.na(result$theta)) && !is.na(result$thetaName) &&
                (length(result$theta) == 2 || !identical(result$theta, designPlan[[result$thetaName]]))) {
            if (!is.logical(showSource) || isTRUE(showSource)) {
                showSourceHint <- .getVariedParameterHint(result$theta, result$thetaName)
            }
            parameterList <- list()
            parameterList[[result$thetaName]] <- .getVariedParameterVector(result$theta, result$thetaName)
            designPlan <- do.call(designPlan$recreate, parameterList)
        }
    }

    srcCmd <- NULL

    reducedParam <- NULL
    if (type %in% c(1:4)) {
        reducedParam <- .warnInCaseOfUnusedValuesForPlotting(designPlan)
    }

    if (type == 1) { # Boundary plot
        if (survivalDesignPlanEnabled) {
            if (is.na(main)) {
                main <- PlotSubTitleItems$new(title = "Boundaries Z Scale")
                .addPlotSubTitleItems(designPlan, designMaster, main, type)
                if (!is.null(reducedParam)) {
                    main$add(reducedParam$title, reducedParam$value, reducedParam$subscript)
                }
            }

            if (designMaster$sided == 1) {
                designPlan <- data.frame(
                    cumulativeEventsPerStage = designPlan$cumulativeEventsPerStage[, 1],
                    criticalValues = designMaster$criticalValues,
                    futilityBounds = c(designMaster$futilityBounds, designMaster$criticalValues[designMaster$kMax])
                )
            } else {
                designPlan <- data.frame(
                    cumulativeEventsPerStage = designPlan$cumulativeEventsPerStage[, 1],
                    criticalValues = designMaster$criticalValues,
                    criticalValuesMirrored = -designMaster$criticalValues
                )
            }

            xParameterName <- "cumulativeEventsPerStage"
            if (designMaster$sided == 1) {
                if (any(designMaster$futilityBounds > C_FUTILITY_BOUNDS_DEFAULT)) {
                    yParameterNames <- c("futilityBounds", "criticalValues")
                } else {
                    yParameterNames <- "criticalValues"
                }
                yParameterNamesSrc <- yParameterNames
            } else {
                yParameterNames <- c("criticalValues", "criticalValuesMirrored")
                yParameterNamesSrc <- c("criticalValues", paste0("-", designPlanName, "$.design$criticalValues"))
            }

            if (is.na(legendPosition)) {
                legendPosition <- C_POSITION_RIGHT_TOP
            }

            srcCmd <- .showPlotSourceInformation(
                objectName = paste0(designPlanName, "$.design"),
                xParameterName = paste0(designPlanName, "$", xParameterName, "[, 1]"),
                yParameterNames = yParameterNamesSrc,
                hint = showSourceHint, nMax = nMax,
                type = type, showSource = showSource
            )
        } else {
            if (is.na(main)) {
                main <- PlotSubTitleItems$new(title = "Boundaries")
                .addPlotSubTitleItems(designPlan, designMaster, main, type)
                if (!is.null(reducedParam)) {
                    main$add(reducedParam$title, reducedParam$value, reducedParam$subscript)
                }
            }

            designSet <- TrialDesignSet$new(design = designMaster, singleDesign = TRUE)
            designSet$.plotSettings <- designPlan$.plotSettings
            designPlanName <- paste0(designPlanName, "$.design")
            return(.plotTrialDesignSet(
                x = designSet, y = NULL, main = main,
                xlab = xlab, ylab = ylab, type = type,
                palette = palette, theta = .plotTheta(theta), nMax = nMax,
                plotPointsEnabled = plotPointsEnabled, legendPosition = legendPosition,
                designSetName = designPlanName, showSource = showSource,
                plotSettings = plotSettings # , ...
            ))
        }
    } else if (type == 2) { # Effect Scale Boundary plot
        if (is.na(main)) {
            main <- PlotSubTitleItems$new(title = "Boundaries Effect Scale")
            .addPlotSubTitleItems(designPlan, designMaster, main, type)
            if (!is.null(reducedParam)) {
                main$add(reducedParam$title, reducedParam$value, reducedParam$subscript)
            }
        }

        if (is.na(ylab)) {
            if (.isTrialDesignPlanMeans(designPlan)) {
                if (designPlan$groups == 1) {
                    ylab <- "Mean"
                } else if (!designPlan$meanRatio) {
                    ylab <- "Mean Difference"
                } else {
                    ylab <- "Mean Ratio"
                }
            } else if (.isTrialDesignPlanRates(designPlan)) {
                if (designPlan$groups == 1) {
                    ylab <- "Rate"
                } else if (!designPlan$riskRatio) {
                    ylab <- "Rate Difference"
                } else {
                    ylab <- "Risk Ratio"
                }
            } else if (survivalDesignPlanEnabled) {
                ylab <- "Hazard Ratio"
            }
        }

        groupedPlotEnabled <- FALSE
        yParameterNamesSrc <- c()
        if (designMaster$sided == 1) {
            if (any(designMaster$futilityBounds > C_FUTILITY_BOUNDS_DEFAULT)) {
                data <- data.frame(
                    criticalValuesEffectScale = designPlan$criticalValuesEffectScale[, 1],
                    futilityBoundsEffectScale = c(
                        designPlan$futilityBoundsEffectScale[, 1],
                        designPlan$criticalValuesEffectScale[designMaster$kMax, 1]
                    )
                )
                yParameterNamesSrc <- c(yParameterNamesSrc, "criticalValuesEffectScale[, 1]")
                yParameterNamesSrc <- c(yParameterNamesSrc, paste0(
                    "c(", designPlanName, "$futilityBoundsEffectScale[, 1], ",
                    designPlanName, "$criticalValuesEffectScale[nrow(", designPlanName, "$criticalValuesEffectScale), 1])"
                ))
            } else {
                data <- data.frame(
                    criticalValuesEffectScale = designPlan$criticalValuesEffectScale[, 1]
                )
                yParameterNamesSrc <- c(yParameterNamesSrc, "criticalValuesEffectScale[, 1]")
            }
        } else if (designMaster$typeOfDesign == C_TYPE_OF_DESIGN_PT) {
            data <- data.frame(
                criticalValues = designPlan$criticalValuesEffectScaleUpper[, 1],
                criticalValuesMirrored = designPlan$criticalValuesEffectScaleLower[, 1],
                futilityBounds = c(
                    designPlan$futilityBoundsEffectScaleUpper[, 1],
                    designPlan$criticalValuesEffectScaleUpper[designMaster$kMax, 1]
                ),
                futilityBoundsMirrored = c(
                    designPlan$futilityBoundsEffectScaleLower[, 1],
                    designPlan$criticalValuesEffectScaleLower[designMaster$kMax, 1]
                )
            )
            yParameterNamesSrc <- c(yParameterNamesSrc, "criticalValuesEffectScaleUpper[, 1]")
            yParameterNamesSrc <- c(yParameterNamesSrc, "criticalValuesEffectScaleLower[, 1]")
            yParameterNamesSrc <- c(yParameterNamesSrc, paste0(
                "c(", designPlanName, "$futilityBoundsEffectScaleUpper[, 1], ",
                designPlanName, "$criticalValuesEffectScaleUpper[nrow(", designPlanName, "$criticalValuesEffectScaleUpper), 1])"
            ))
            yParameterNamesSrc <- c(yParameterNamesSrc, paste0(
                "c(", designPlanName, "$futilityBoundsEffectScaleLower[, 1], ",
                designPlanName, "$criticalValuesEffectScaleLower[nrow(", designPlanName, "$criticalValuesEffectScaleLower), 1])"
            ))
            groupedPlotEnabled <- TRUE
        } else {
            data <- data.frame(
                criticalValuesEffectScale = designPlan$criticalValuesEffectScaleUpper[, 1],
                criticalValuesEffectScaleMirrored = designPlan$criticalValuesEffectScaleLower[, 1]
            )
            yParameterNamesSrc <- c(yParameterNamesSrc, "criticalValuesEffectScaleUpper[, 1]")
            yParameterNamesSrc <- c(yParameterNamesSrc, "criticalValuesEffectScaleLower[, 1]")
        }

        if (survivalDesignPlanEnabled) {
            xParameterName <- "cumulativeEventsPerStage"
            xParameterNameSrc <- paste0(designPlanName, "$", xParameterName, "[, 1]")
            data <- cbind(data.frame(cumulativeEventsPerStage = designPlan$cumulativeEventsPerStage[, 1]), data)
        } else {
            xParameterName <- "informationRates"
            xParameterNameSrc <- paste0(designPlanName, "$.design$", xParameterName)
            data <- cbind(data.frame(informationRates = designMaster$informationRates), data)
        }
        if (designMaster$sided == 1 || designMaster$typeOfDesign == C_TYPE_OF_DESIGN_PT) {
            if (any(designMaster$futilityBounds > C_FUTILITY_BOUNDS_DEFAULT)) {
                yParameterNames <- c("futilityBoundsEffectScale", "criticalValuesEffectScale")
            } else {
                yParameterNames <- "criticalValuesEffectScale"
            }
        } else {
            yParameterNames <- c("criticalValuesEffectScale", "criticalValuesEffectScaleMirrored")
        }

        if (is.na(legendPosition)) {
            legendPosition <- C_POSITION_RIGHT_TOP
        }

        if (is.na(legendPosition)) {
            legendPosition <- C_POSITION_RIGHT_TOP
        }

        srcCmd <- .showPlotSourceInformation(
            objectName = designPlanName,
            xParameterName = xParameterNameSrc,
            yParameterNames = yParameterNamesSrc,
            hint = showSourceHint, nMax = nMax,
            type = type, showSource = showSource
        )

        if (groupedPlotEnabled) {
            criticalValuesName <- designPlan$.getDataFrameColumnCaption("criticalValuesEffectScale", TRUE)
            futilityBoundsName <- designPlan$.getDataFrameColumnCaption("futilityBoundsEffectScale", TRUE)

            designPlan <- data.frame(
                xValues = rep(data[[xParameterName]], 4),
                yValues = c(
                    data$criticalValues, data$criticalValuesMirrored,
                    data$futilityBounds, data$futilityBoundsMirrored
                ),
                categories = c(
                    rep(criticalValuesName, nrow(data)), rep("criticalValuesMirrored", nrow(data)),
                    rep(futilityBoundsName, nrow(data)), rep("futilityBoundsMirrored", nrow(data))
                ),
                groups = c(rep(criticalValuesName, 2 * nrow(data)), rep(futilityBoundsName, 2 * nrow(data)))
            )
        } else {
            designPlan <- data
        }
    } else if (type == 3) { # Stage Levels
        if (is.na(main)) {
            main <- PlotSubTitleItems$new(title = "Boundaries p Values Scale")
            .addPlotSubTitleItems(designPlan, designMaster, main, type)
            if (!is.null(reducedParam)) {
                main$add(reducedParam$title, reducedParam$value, reducedParam$subscript)
            }
        }

        if (survivalDesignPlanEnabled) {
            xParameterName <- "cumulativeEventsPerStage"
            yParameterNames <- "stageLevels"
            designPlan <- data.frame(
                cumulativeEventsPerStage = designPlan$cumulativeEventsPerStage[, 1],
                stageLevels = designMaster$stageLevels
            )
            xParameterNameSrc <- "cumulativeEventsPerStage[, 1]"
            yParameterNamesSrc <- ".design$stageLevels"
        } else {
            xParameterName <- "informationRates"
            yParameterNames <- "stageLevels"
            designPlan <- TrialDesignSet$new(design = designMaster, singleDesign = TRUE)
            xParameterNameSrc <- ".design$informationRates"
            yParameterNamesSrc <- ".design$stageLevels"
        }

        srcCmd <- .showPlotSourceInformation(
            objectName = designPlanName,
            xParameterName = xParameterNameSrc,
            yParameterNames = yParameterNamesSrc,
            hint = showSourceHint, nMax = nMax,
            type = type, showSource = showSource
        )
    } else if (type == 4) { # Alpha Spending
        if (is.na(main)) {
            main <- PlotSubTitleItems$new(title = "Error Spending")
            .addPlotSubTitleItems(designPlan, designMaster, main, type)
            if (!is.null(reducedParam)) {
                main$add(reducedParam$title, reducedParam$value, reducedParam$subscript)
            }
        }
        if (survivalDesignPlanEnabled) {
            xParameterName <- "cumulativeEventsPerStage"
            yParameterNames <- "alphaSpent"
            designPlan <- data.frame(
                cumulativeEventsPerStage = designPlan$cumulativeEventsPerStage[, 1],
                alphaSpent = designMaster$alphaSpent
            )
            xParameterNameSrc <- "cumulativeEventsPerStage[, 1]"
            yParameterNamesSrc <- ".design$alphaSpent"
        } else {
            xParameterName <- "informationRates"
            yParameterNames <- "alphaSpent"
            designPlan <- TrialDesignSet$new(design = designMaster, singleDesign = TRUE)
            xParameterNameSrc <- ".design$informationRates"
            yParameterNamesSrc <- ".design$alphaSpent"
        }
        plotPointsEnabled <- ifelse(is.na(plotPointsEnabled), FALSE, plotPointsEnabled)

        srcCmd <- .showPlotSourceInformation(
            objectName = designPlanName,
            xParameterName = xParameterNameSrc,
            yParameterNames = yParameterNamesSrc,
            hint = showSourceHint, nMax = nMax,
            type = type, showSource = showSource
        )
    } else if (type == 5) { # Power and Stopping Probabilities

        .assertIsValidVariedParameterVectorForPlotting(designPlan, type)

        if (designPlan$.isSampleSizeObject()) {
            if (is.na(main)) {
                main <- PlotSubTitleItems$new(title = "Sample Size")
                .addPlotSubTitleItems(designPlan, designMaster, main, type)
            }

            yAxisScalingEnabled <- TRUE

            if (.isTrialDesignPlanMeans(designPlan)) {
                xParameterName <- "alternative"
                yParameterNames <- c("nFixed")
                if (designMaster$kMax > 1) {
                    yParameterNames <- c(yParameterNames, "maxNumberOfSubjects", "expectedNumberOfSubjectsH1")
                }
                if (is.na(ylab)) {
                    ylab <- "Sample Size"
                }
                yAxisScalingEnabled <- FALSE
                if (is.na(legendPosition)) {
                    legendPosition <- C_POSITION_RIGHT_TOP
                }
                yParameterNamesSrc <- yParameterNames
            } else if (.isTrialDesignPlanRates(designPlan)) {
                xParameterName <- "pi1"
                yParameterNames <- c("nFixed")
                if (designMaster$kMax > 1) {
                    yParameterNames <- c(yParameterNames, "maxNumberOfSubjects", "expectedNumberOfSubjectsH1")
                }
                if (is.na(ylab)) {
                    ylab <- "Sample Size"
                }
                yAxisScalingEnabled <- FALSE
                if (is.na(legendPosition)) {
                    legendPosition <- C_POSITION_RIGHT_TOP
                }
                yParameterNamesSrc <- yParameterNames
            } else if (survivalDesignPlanEnabled) {
                designPlan <- data.frame(
                    hazardRatio = designPlan$hazardRatio,
                    eventsFixed = designPlan$eventsFixed,
                    maxNumberOfEvents = designPlan$cumulativeEventsPerStage[designMaster$kMax, ],
                    expectedEventsH1 = designPlan$expectedEventsH1
                )
                xParameterName <- "hazardRatio"
                yParameterNames <- c("eventsFixed")
                if (designMaster$kMax > 1) {
                    yParameterNames <- c(yParameterNames, "maxNumberOfEvents", "expectedEventsH1")
                }
                if (is.na(ylab)) {
                    ylab <- "# Events"
                }

                if (is.na(legendPosition)) {
                    legendPosition <- C_POSITION_RIGHT_TOP
                }
                yParameterNamesSrc <- c(
                    "eventsFixed",
                    paste0("cumulativeEventsPerStage[", designMaster$kMax, ", ]"), "expectedEventsH1"
                )
            } else if (.isTrialDesignPlanCountData(designPlan)) {
                xParameterName <- "theta" # "lambda1"
                yParameterNames <- c("nFixed")
                if (designMaster$kMax > 1) {
                    yParameterNames <- c("maxNumberOfSubjects", "maxNumberOfSubjects", "expectedNumberOfSubjectsH1")
                }
                if (is.na(ylab)) {
                    ylab <- "Sample Size"
                }
                yAxisScalingEnabled <- FALSE
                if (is.na(legendPosition)) {
                    legendPosition <- C_POSITION_RIGHT_TOP
                }
                yParameterNamesSrc <- yParameterNames
            } else {
                stop("Plot type 5 is not implemented for class ", sQuote(.getClassName(designPlan)))
            }

            srcCmd <- .showPlotSourceInformation(
                objectName = designPlanName,
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

            return(.plotParameterSet(
                parameterSet = designPlan, designMaster = designMaster,
                xParameterName = xParameterName,
                yParameterNames = yParameterNames, mainTitle = main, xlab = xlab, ylab = ylab,
                palette = palette, theta = .plotTheta(theta),
                nMax = nMax, plotPointsEnabled = plotPointsEnabled,
                legendPosition = legendPosition, variedParameters = variedParameters,
                qnormAlphaLineEnabled = FALSE, yAxisScalingEnabled = FALSE,
                plotSettings = plotSettings # , ...
            ))
        } else {
            if (is.na(main)) {
                main <- PlotSubTitleItems$new(title = "Overall Power and Early Stopping")
                .addPlotSubTitleItems(designPlan, designMaster, main, type)
            }
            if (survivalDesignPlanEnabled) {
                xParameterName <- "hazardRatio"
            } else if (.isTrialDesignPlanCountData(designPlan)) {
                xParameterName <- "theta"
            } else {
                xParameterName <- "effect"
            }
            yParameterNames <- c("overallReject", "futilityStop", "earlyStop")

            if (is.na(ylab)) {
                ylab <- ""
            }
            if (is.na(legendPosition)) {
                legendPosition <- C_POSITION_LEFT_TOP
            }

            srcCmd <- .showPlotSourceInformation(
                objectName = designPlanName,
                xParameterName = xParameterName,
                yParameterNames = yParameterNames,
                hint = showSourceHint, nMax = nMax,
                type = type, showSource = showSource
            )
            if (!is.null(srcCmd)) {
                if (.isSpecialPlotShowSourceArgument(showSource)) {
                    return(invisible(srcCmd))
                }
                return(srcCmd)
            }

            if (is.null(list(...)[["ylim"]])) {
                ylim <- c(0, 1)
                return(.plotParameterSet(
                    parameterSet = designPlan, designMaster = designMaster,
                    xParameterName = xParameterName,
                    yParameterNames = yParameterNames, mainTitle = main, xlab = xlab, ylab = ylab,
                    palette = palette, theta = .plotTheta(theta),
                    nMax = nMax, plotPointsEnabled = plotPointsEnabled,
                    legendPosition = legendPosition, variedParameters = variedParameters,
                    qnormAlphaLineEnabled = FALSE, yAxisScalingEnabled = FALSE,
                    plotSettings = plotSettings, ylim = ylim # , ...
                ))
            } else {
                return(.plotParameterSet(
                    parameterSet = designPlan, designMaster = designMaster,
                    xParameterName = xParameterName,
                    yParameterNames = yParameterNames, mainTitle = main, xlab = xlab, ylab = ylab,
                    palette = palette, theta = .plotTheta(theta),
                    nMax = nMax, plotPointsEnabled = plotPointsEnabled,
                    legendPosition = legendPosition, variedParameters = variedParameters,
                    qnormAlphaLineEnabled = FALSE, yAxisScalingEnabled = FALSE,
                    plotSettings = plotSettings # , ...
                ))
            }
        }
    } else if (type == 6) { # Average Sample Size / Average Event Number
        .assertIsValidVariedParameterVectorForPlotting(designPlan, type)

        if (is.na(main)) {
            if (.isTrialDesignPlanCountData(designPlan) &&
                    (length(designPlan$expectedNumberOfSubjectsH1) == 0 ||
                        all(is.na(designPlan$expectedNumberOfSubjectsH1)))) {
                main <- PlotSubTitleItems$new(title = "Power / Early Stop")
            } else {
                titlePart <- ifelse(survivalDesignPlanEnabled, "Number of Events", "Sample Size")
                main <- PlotSubTitleItems$new(title = paste0("Expected ", titlePart, " and Power / Early Stop"))
            }
            .addPlotSubTitleItems(designPlan, designMaster, main, type)
        }

        if (survivalDesignPlanEnabled) {
            xParameterName <- "hazardRatio"
            yParameterNames <- "expectedNumberOfEvents"
            expectedNumberOfEvents <- designPlan[["expectedNumberOfEvents"]]
            if (is.null(expectedNumberOfEvents) || length(expectedNumberOfEvents) == 0) {
                yParameterNames <- "expectedEventsH1"
            }
            yParameterNames <- c(yParameterNames, "overallReject", "earlyStop") # overallReject = power
            if (is.na(legendPosition)) {
                legendPosition <- C_POSITION_RIGHT_CENTER
            }
        } else {
            xParameterName <- ifelse(.isTrialDesignPlanCountData(designPlan), "theta", "effect")
            yParameterNames <- character()
            if (!.isTrialDesignPlanCountData(designPlan)) {
                yParameterNames <- c(yParameterNames, "expectedNumberOfSubjects")
            } else if (length(designPlan$expectedNumberOfSubjectsH1) > 0 &&
                    all(is.na(designPlan$expectedNumberOfSubjectsH1))) {
                yParameterNames <- c(yParameterNames, "expectedNumberOfSubjectsH1")
            }
            yParameterNames <- c(
                yParameterNames,
                "overallReject", # overallReject = power
                "earlyStop"
            )
        }
        srcCmd <- .showPlotSourceInformation(
            objectName = designPlanName,
            xParameterName = xParameterName,
            yParameterNames = yParameterNames,
            hint = showSourceHint, nMax = nMax,
            type = type, showSource = showSource
        )
    } else if (type == 7) {
        .assertIsValidVariedParameterVectorForPlotting(designPlan, type)

        if (is.na(main)) {
            main <- PlotSubTitleItems$new(title = "Overall Power")
            .addPlotSubTitleItems(designPlan, designMaster, main, type)
        }

        if (survivalDesignPlanEnabled) {
            xParameterName <- "hazardRatio"
        } else if (.isTrialDesignPlanCountData(designPlan)) {
            xParameterName <- "theta"
        } else {
            xParameterName <- "effect"
        }
        yParameterNames <- "overallReject"
        if (is.na(legendPosition)) {
            legendPosition <- C_POSITION_RIGHT_CENTER
        }
        srcCmd <- .showPlotSourceInformation(
            objectName = designPlanName,
            xParameterName = xParameterName,
            yParameterNames = yParameterNames,
            hint = showSourceHint, nMax = nMax,
            type = type, showSource = showSource
        )
    } else if (type == 8) {
        .assertIsValidVariedParameterVectorForPlotting(designPlan, type)

        if (is.na(main)) {
            main <- PlotSubTitleItems$new(title = "Overall Early Stopping")
            .addPlotSubTitleItems(designPlan, designMaster, main, type)
        }

        if (survivalDesignPlanEnabled) {
            xParameterName <- "hazardRatio"
        } else if (.isTrialDesignPlanCountData(designPlan)) {
            xParameterName <- "theta"
        } else {
            xParameterName <- "effect"
        }
        yParameterNames <- c("earlyStop", "futilityStop")
        if (is.na(legendPosition)) {
            legendPosition <- C_POSITION_RIGHT_CENTER
        }
        srcCmd <- .showPlotSourceInformation(
            objectName = designPlanName,
            xParameterName = xParameterName,
            yParameterNames = yParameterNames,
            hint = showSourceHint, nMax = nMax,
            type = type, showSource = showSource
        )
    } else if (type == 9) {
        .assertIsValidVariedParameterVectorForPlotting(designPlan, type)

        if (is.na(main)) {
            if (survivalDesignPlanEnabled) {
                main <- PlotSubTitleItems$new(title = "Expected Number of Events")
            } else {
                main <- PlotSubTitleItems$new(title = "Expected Sample Size")
            }
            .addPlotSubTitleItems(designPlan, designMaster, main, type)
        }

        if (survivalDesignPlanEnabled) {
            xParameterName <- "hazardRatio"
            yParameterNames <- "expectedNumberOfEvents"
            expectedNumberOfEvents <- designPlan[["expectedNumberOfEvents"]]
            if (is.null(expectedNumberOfEvents) || length(expectedNumberOfEvents) == 0) {
                yParameterNames <- c("expectedEventsH0", "expectedEventsH1")
                if (is.na(legendPosition)) {
                    legendPosition <- C_POSITION_RIGHT_CENTER
                }
            }
        } else if (.isTrialDesignPlanCountData(designPlan)) {
            if (designPlan$.getParameterType("expectedNumberOfSubjectsH1") != C_PARAM_GENERATED) {
                stop("Plot type 9 is only available for count data endpoint if 'expectedNumberOfSubjectsH1' was not calculated")
            }

            xParameterName <- "theta"
            yParameterNames <- "expectedNumberOfSubjectsH1"
        } else {
            xParameterName <- "effect"
            yParameterNames <- "expectedNumberOfSubjects"
        }
        srcCmd <- .showPlotSourceInformation(
            objectName = designPlanName,
            xParameterName = xParameterName,
            yParameterNames = yParameterNames,
            hint = showSourceHint, nMax = nMax,
            type = type, showSource = showSource
        )
    } else if (survivalDesignPlanEnabled) {
        if (type == 10) { # Study Duration
            .assertIsValidVariedParameterVectorForPlotting(designPlan, type)
            if (is.na(main)) {
                main <- PlotSubTitleItems$new(title = "Study Duration")
                .addPlotSubTitleItems(designPlan, designMaster, main, type)
            }
            xParameterName <- "hazardRatio"
            yParameterNames <- "studyDuration"
            srcCmd <- .showPlotSourceInformation(
                objectName = designPlanName,
                xParameterName = xParameterName,
                yParameterNames = yParameterNames,
                hint = showSourceHint, nMax = nMax,
                type = type, showSource = showSource
            )
        } else if (type == 11) {
            .assertIsValidVariedParameterVectorForPlotting(designPlan, type)
            if (is.na(main)) {
                main <- PlotSubTitleItems$new(title = "Expected Number of Subjects")
                .addPlotSubTitleItems(designPlan, designMaster, main, type)
            }
            xParameterName <- "hazardRatio"
            yParameterNames <- "expectedNumberOfSubjects"
            srcCmd <- .showPlotSourceInformation(
                objectName = designPlanName,
                xParameterName = xParameterName,
                yParameterNames = yParameterNames,
                hint = showSourceHint, nMax = nMax,
                type = type, showSource = showSource
            )
        } else if (type == 12) { # Analysis Time
            .assertIsValidVariedParameterVectorForPlotting(designPlan, type)
            if (is.na(main)) {
                main <- PlotSubTitleItems$new(title = "Analysis Time")
                .addPlotSubTitleItems(designPlan, designMaster, main, type)
            }

            xParameterName <- "hazardRatio"
            yParameterNames <- "analysisTime"
            yParameterNamesSrc <- c()
            for (i in 1:nrow(designPlan[["analysisTime"]])) {
                yParameterNamesSrc <- c(yParameterNamesSrc, paste0("analysisTime[", i, ", ]"))
            }

            data <- NULL
            for (k in 1:designMaster$kMax) {
                part <- data.frame(
                    categories = rep(k, length(designPlan$hazardRatio)),
                    xValues = designPlan$hazardRatio,
                    yValues = designPlan$analysisTime[k, ]
                )
                if (is.null(data)) {
                    data <- part
                } else {
                    data <- rbind(data, part)
                }
            }

            srcCmd <- .showPlotSourceInformation(
                objectName = designPlanName,
                xParameterName = xParameterName,
                yParameterNames = yParameterNamesSrc,
                hint = showSourceHint,
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
                legendPosition = legendPosition, sided = designMaster$sided,
                plotSettings = plotSettings, ...
            ))
        } else if (type == 13 || type == 14) { # Cumulative Distribution Function / Survival function
            return(.plotSurvivalFunction(designPlan,
                designMaster = designMaster, type = type, main = main,
                xlab = xlab, ylab = ylab, palette = palette,
                legendPosition = legendPosition, showSource = showSource,
                designPlanName = designPlanName,
                plotSettings = plotSettings, ...
            ))
        } else {
            stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'type' (", type, ") is not allowed; must be 1, 2, ..., 14")
        }
    } else {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'type' (", type, ") is not allowed; must be 1, 2, ..., 9")
    }

    if (!is.null(srcCmd)) {
        if (.isSpecialPlotShowSourceArgument(showSource)) {
            return(invisible(srcCmd))
        }
        return(srcCmd)
    }

    p <- .plotParameterSet(
        parameterSet = designPlan, designMaster = designMaster,
        xParameterName = xParameterName,
        yParameterNames = yParameterNames, mainTitle = main, xlab = xlab, ylab = ylab,
        palette = palette, theta = .plotTheta(theta),
        nMax = nMax, plotPointsEnabled = plotPointsEnabled,
        legendPosition = legendPosition, variedParameters = variedParameters,
        qnormAlphaLineEnabled = (type != 2), ratioEnabled = ratioEnabled,
        plotSettings = plotSettings # , ...
    )

    if (type == 1 && survivalDesignPlanEnabled) {
        p <- .addDecistionCriticalValuesToPlot(p = p, designMaster = designMaster, type = type, nMax = nMax)
    }
    return(p)
}

.getSurvivalFunctionPlotCommand <- function(functionType = c("pwExpDist", "lambdaStep"), timeValues, lambda,
        designPlan, type, piecewiseSurvivalEnabled, multiplyByHazardRatio = FALSE) {
    functionType <- match.arg(functionType)
    signPrefix <- ifelse(type == 13, "", "-")
    if (functionType == "pwExpDist") {
        functionName <- "getPiecewiseExponentialDistribution"
    } else {
        functionName <- "getLambdaStepFunction"
    }
    cmd <- paste0(
        signPrefix, functionName,
        "(", .reconstructSequenceCommand(timeValues),
        ", piecewiseLambda = ", .arrayToString(lambda, vectorLookAndFeelEnabled = TRUE)
    )
    if (piecewiseSurvivalEnabled) {
        cmd <- paste0(
            cmd, ", piecewiseSurvivalTime = ",
            .arrayToString(designPlan$piecewiseSurvivalTime, vectorLookAndFeelEnabled = TRUE)
        )
    }
    if (functionType == "pwExpDist") {
        cmd <- paste0(cmd, ", kappa = ", designPlan$kappa)
    }
    cmd <- paste0(cmd, ")")
    if (multiplyByHazardRatio) {
        cmd <- paste0(cmd, " * ", designPlan$hazardRatio[1])
    }
    return(cmd)
}

# Cumulative Distribution Function / Survival function
.plotSurvivalFunction <- function(designPlan, ..., designMaster, type = c(13, 14), main = NA_character_,
        xlab = NA_character_, ylab = NA_character_, palette = "Set1",
        legendPosition = NA_integer_, showSource = FALSE,
        designPlanName = NA_character_, plotSettings = NULL) {
    startTime <- Sys.time()
    if (is.null(designPlan$piecewiseSurvivalTime) ||
            length(designPlan$piecewiseSurvivalTime) == 0) {
        stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'piecewiseSurvivalTime' must be specified")
    }

    type <- type[1]
    if (!(type %in% c(13, 14))) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'type' must be 13 or 14")
    }

    lambda1 <- designPlan[["lambda1"]]
    lambda2 <- designPlan[["lambda2"]]
    if (is.null(lambda2) || length(lambda2) == 0) {
        stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'lambda2' must be specified")
    }

    if (is.null(designPlan$kappa) || length(designPlan$kappa) == 0) {
        stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'kappa' must be specified")
    }

    if (is.null(designPlan$hazardRatio) || length(designPlan$hazardRatio) == 0) {
        stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'hazardRatio' must be specified")
    }

    piecewiseSurvivalEnabled <- designPlan$.piecewiseSurvivalTime$piecewiseSurvivalEnabled

    if (is.na(main)) {
        if (type == 13) {
            main <- PlotSubTitleItems$new(title = "Cumulative Distribution Function")
        } else {
            main <- PlotSubTitleItems$new(title = "Survival Function")
        }
        .addPlotSubTitleItems(designPlan, designMaster, main, type)
        if (!piecewiseSurvivalEnabled) {
            if (designPlan$.piecewiseSurvivalTime$.isLambdaBased(minNumberOfLambdas = 1)) {
                main$add("lambda", round(designPlan$lambda1[1], 4), 1)
                main$add("lambda", round(designPlan$lambda2, 4), 2)
            } else {
                main$add("pi", round(designPlan$pi1[1], 3), 1)
                main$add("pi", round(designPlan$pi2, 3), 2)
            }
        } else if (length(designPlan$hazardRatio) == 1) {
            main$add("Hazard Ratio", round(designPlan$hazardRatio[1], 3))
        }
    }

    if (!piecewiseSurvivalEnabled || (length(designPlan$piecewiseSurvivalTime) == 1 &&
            designPlan$piecewiseSurvivalTime[1] == 0)) {
        timeTo <- max(designPlan$analysisTime[designMaster$kMax, ])
    } else {
        timeTo <- max(designPlan$piecewiseSurvivalTime)
    }
    if (is.na(timeTo) || !is.numeric(timeTo) || is.infinite(timeTo)) {
        # unable to determine upper bound of time values
        timeTo <- 0
    }

    timeTo <- timeTo + 10
    by <- timeTo / 1000
    timeValues <- seq(from = 0, to = timeTo, by = by)

    data <- data.frame(
        time = timeValues,
        lambdaGroup1 = rep(-1, length(timeValues)),
        lambdaGroup2 = rep(-1, length(timeValues)),
        survival1 = rep(-1, length(timeValues)),
        survival2 = rep(-1, length(timeValues)),
        survivalGroup1 = rep(-1, length(timeValues)),
        survivalGroup2 = rep(-1, length(timeValues))
    )

    signPrefix <- ifelse(type == 13, "", "-")
    if (piecewiseSurvivalEnabled) {
        data$survival2 <- .getPiecewiseExponentialDistribution(
            timeValues,
            lambda2, designPlan$piecewiseSurvivalTime, designPlan$kappa
        )

        yParameterNames <- .getSurvivalFunctionPlotCommand(
            "pwExpDist",
            timeValues, lambda2, designPlan, type, piecewiseSurvivalEnabled
        )

        if (!is.null(lambda1) && !all(is.na(lambda1)) &&
                length(lambda1) == length(lambda2)) {
            data$survival1 <- .getPiecewiseExponentialDistribution(
                timeValues,
                lambda1, designPlan$piecewiseSurvivalTime, designPlan$kappa
            )
            yParameterNames <- c(
                yParameterNames,
                .getSurvivalFunctionPlotCommand(
                    "pwExpDist",
                    timeValues, lambda1, designPlan, type, piecewiseSurvivalEnabled
                )
            )
        } else {
            .warnInCaseOfUnusedValuesForPlottingSurvival(designPlan$hazardRatio)
            data$survival1 <- data$survival2 * designPlan$hazardRatio[1]
            yParameterNames <- c(
                yParameterNames,
                .getSurvivalFunctionPlotCommand("pwExpDist", timeValues, lambda2,
                    designPlan, type, piecewiseSurvivalEnabled,
                    multiplyByHazardRatio = TRUE
                )
            )
        }

        yParameterNames <- c(
            yParameterNames,
            .getSurvivalFunctionPlotCommand(
                "lambdaStep",
                timeValues, lambda2, designPlan, type, piecewiseSurvivalEnabled
            )
        )
        if (!is.null(lambda1) && !all(is.na(lambda1)) &&
                length(lambda1) == length(lambda2)) {
            yParameterNames <- c(
                yParameterNames,
                .getSurvivalFunctionPlotCommand(
                    "lambdaStep",
                    timeValues, lambda1, designPlan, type, piecewiseSurvivalEnabled
                )
            )
        } else {
            yParameterNames <- c(
                yParameterNames,
                .getSurvivalFunctionPlotCommand("lambdaStep", timeValues, lambda2,
                    designPlan, type, piecewiseSurvivalEnabled,
                    multiplyByHazardRatio = TRUE
                )
            )
        }
    } else {
        if (designPlan$.piecewiseSurvivalTime$.isLambdaBased(minNumberOfLambdas = 1)) {
            if (length(designPlan$lambda1) > 1) {
                lambda1 <- designPlan$lambda1[1]
                warning("Only the first 'lambda1' (", round(lambda1, 4),
                    ") was used for plotting",
                    call. = FALSE
                )
            }
        } else {
            .warnInCaseOfUnusedValuesForPlottingRates(designPlan$pi1)
        }

        if (!is.na(designPlan$pi1[1]) && !is.na(designPlan$pi2) && !is.na(designPlan$eventTime)) {
            lambda2 <- (-log(1 - designPlan$pi2))^(1 / designPlan$kappa) / designPlan$eventTime
            lambda1 <- (-log(1 - designPlan$pi1[1]))^(1 / designPlan$kappa) / designPlan$eventTime
        }

        data$survival2 <- .getPiecewiseExponentialDistribution(
            timeValues,
            lambda2, 0, designPlan$kappa
        )
        data$survival1 <- .getPiecewiseExponentialDistribution(
            timeValues,
            lambda1, 0, designPlan$kappa
        )

        yParameterNames <- .getSurvivalFunctionPlotCommand(
            "pwExpDist",
            timeValues, lambda2, designPlan, type, piecewiseSurvivalEnabled
        )
        yParameterNames <- c(
            yParameterNames,
            .getSurvivalFunctionPlotCommand(
                "pwExpDist",
                timeValues, lambda1, designPlan, type, piecewiseSurvivalEnabled
            )
        )
        yParameterNames <- c(
            yParameterNames,
            .getSurvivalFunctionPlotCommand(
                "lambdaStep",
                timeValues, lambda2, designPlan, type, piecewiseSurvivalEnabled
            )
        )
        yParameterNames <- c(
            yParameterNames,
            .getSurvivalFunctionPlotCommand(
                "lambdaStep", timeValues, lambda1,
                designPlan, type, piecewiseSurvivalEnabled
            )
        )
    }

    # two groups: 1 = treatment, 2 = control
    if (type == 14) {
        data$survival1 <- 1 - data$survival1
        data$survival2 <- 1 - data$survival2
    }

    if (piecewiseSurvivalEnabled) {
        data$lambdaGroup2 <- .getLambdaStepFunction(
            timeValues,
            designPlan$piecewiseSurvivalTime, lambda2
        )
        if (length(lambda1) == 1) {
            if (!is.na(lambda1)) {
                data$lambdaGroup1 <- rep(lambda1, length(data$lambdaGroup2))
            } else {
                data$lambdaGroup1 <- data$lambdaGroup2 * designPlan$hazardRatio[1]
            }
        } else {
            data$lambdaGroup1 <- .getLambdaStepFunction(
                timeValues,
                designPlan$piecewiseSurvivalTime, lambda1
            )
        }
    } else {
        data$lambdaGroup2 <- .getLambdaStepFunction(timeValues, 0, lambda2)
        data$lambdaGroup1 <- .getLambdaStepFunction(timeValues, 0, lambda1)
    }

    scalingBaseValues1 <- na.omit(c(data$survival1, data$survival2))
    scalingBaseValues2 <- na.omit(c(data$lambdaGroup1, data$lambdaGroup2))
    scalingFactor <- 1
    if (length(scalingBaseValues1) > 0 && length(scalingBaseValues2) > 0) {
        scalingFactor <- max(scalingBaseValues1) / max(.getNextHigherValue(scalingBaseValues2))
    }
    data2 <- data.frame(
        categories = c(
            rep("Treatm. piecew. exp.", nrow(data)),
            rep("Control piecew. exp.", nrow(data)),
            rep("Treatm. piecew. lambda", nrow(data)),
            rep("Control piecew. lambda", nrow(data))
        ),
        xValues = rep(data$time, 4),
        yValues = c(
            data$survival1,
            data$survival2,
            data$lambdaGroup1 * scalingFactor,
            data$lambdaGroup2 * scalingFactor
        )
    )

    if (is.na(legendPosition)) {
        if (type == 13) {
            legendPosition <- C_POSITION_LEFT_TOP
        } else {
            legendPosition <- C_POSITION_RIGHT_TOP
        }
    }

    if (is.na(palette) || palette == "Set1") {
        palette <- "Paired"
    }

    if (type == 13) {
        yAxisLabel1 <- "Cumulative Distribution Function"
    } else {
        yAxisLabel1 <- "Survival Function"
    }

    srcCmd <- .showPlotSourceInformation(
        objectName = designPlanName,
        xParameterName = "time",
        yParameterNames = yParameterNames,
        showSource = showSource,
        xValues = timeValues
    )
    if (!is.null(srcCmd)) {
        if (.isSpecialPlotShowSourceArgument(showSource)) {
            return(invisible(srcCmd))
        }
        return(srcCmd)
    }

    if (is.null(plotSettings)) {
        plotSettings <- designPlan$.plotSettings
    }

    return(.plotDataFrame(data2,
        mainTitle = main,
        xlab = xlab, ylab = ylab, xAxisLabel = "Time",
        yAxisLabel1 = yAxisLabel1, yAxisLabel2 = "Lambda",
        plotPointsEnabled = FALSE, legendTitle = NA_character_,
        legendPosition = legendPosition, scalingFactor1 = 1,
        scalingFactor2 = scalingFactor, palette = palette, sided = designMaster$sided,
        plotSettings = plotSettings
    ))
}

.warnInCaseOfUnusedValuesForPlottingMeans <- function(alternative) {
    if (length(alternative) > 1) {
        warning("Only the first 'alternative' (", round(alternative[1], 3),
            ") was used for plotting",
            call. = FALSE
        )
        return(list(title = "alternative", value = alternative[1], subscript = NA_character_))
    }
    return(NULL)
}

.warnInCaseOfUnusedValuesForPlottingRates <- function(pi1) {
    if (length(pi1) > 1) {
        warning("Only the first 'pi1' (", round(pi1[1], 3),
            ") was used for plotting",
            call. = FALSE
        )
        return(list(title = "pi", value = pi1[1], subscript = "1"))
    }
    return(NULL)
}

.warnInCaseOfUnusedValuesForPlottingSurvival <- function(hazardRatio) {
    if (length(hazardRatio) > 1) {
        warning("Only the first 'hazardRatio' (", round(hazardRatio[1], 3),
            ") was used for plotting",
            call. = FALSE
        )
        return(list(title = "hazardRatio", value = hazardRatio[1], subscript = NA_character_))
    }
    return(NULL)
}

.warnInCaseOfUnusedValuesForPlotting <- function(designPlan) {
    if (.isTrialDesignPlanMeans(designPlan) && designPlan$.isSampleSizeObject()) {
        return(.warnInCaseOfUnusedValuesForPlottingMeans(designPlan$alternative))
    }
    if (.isTrialDesignPlanRates(designPlan) && designPlan$.isSampleSizeObject()) {
        return(.warnInCaseOfUnusedValuesForPlottingRates(designPlan$pi1))
    }
    if (.isTrialDesignPlanSurvival(designPlan) && designPlan$.isSampleSizeObject()) {
        return(.warnInCaseOfUnusedValuesForPlottingSurvival(designPlan$hazardRatio))
    }
    return(NULL)
}

#'
#' @title
#' Trial Design Plan Plotting
#'
#' @param x The trial design plan, obtained from \cr
#'        \code{\link[=getSampleSizeMeans]{getSampleSizeMeans()}}, \cr
#'        \code{\link[=getSampleSizeRates]{getSampleSizeRates()}}, \cr
#'        \code{\link[=getSampleSizeSurvival]{getSampleSizeSurvival()}}, \cr
#'        \code{\link[=getSampleSizeCounts]{getSampleSizeCounts()}}, \cr
#'        \code{\link[=getPowerMeans]{getPowerMeans()}}, \cr
#'        \code{\link[=getPowerRates]{getPowerRates()}} or \cr
#'        \code{\link[=getPowerSurvival]{getPowerSurvival()}} or \cr
#'        \code{\link[=getPowerCounts]{getPowerCounts()}}.
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
#'   \item \code{1}: creates a 'Boundaries' plot
#'   \item \code{2}: creates a 'Boundaries Effect Scale' plot
#'   \item \code{3}: creates a 'Boundaries p Values Scale' plot
#'   \item \code{4}: creates a 'Error Spending' plot
#'   \item \code{5}: creates a 'Sample Size' or 'Overall Power and Early Stopping' plot
#'   \item \code{6}: creates a 'Number of Events' or 'Sample Size' plot
#'   \item \code{7}: creates an 'Overall Power' plot
#'   \item \code{8}: creates an 'Overall Early Stopping' plot
#'   \item \code{9}: creates an 'Expected Number of Events' or 'Expected Sample Size' plot
#'   \item \code{10}: creates a 'Study Duration' plot
#'   \item \code{11}: creates an 'Expected Number of Subjects' plot
#'   \item \code{12}: creates an 'Analysis Times' plot
#'   \item \code{13}: creates a 'Cumulative Distribution Function' plot
#'   \item \code{14}: creates a 'Survival Function' plot
#'   \item \code{"all"}: creates all available plots and returns it as a grid plot or list
#' }
#' @inheritParams param_three_dots_plot
#'
#' @description
#' Plots a trial design plan.
#'
#' @details
#' Generic function to plot all kinds of trial design plans.
#'
#' @examples
#' \dontrun{
#' if (require(ggplot2)) plot(getSampleSizeMeans())
#' }
#'
#' @template return_object_ggplot
#'
#' @export
#'
plot.TrialDesignPlan <- function(
        x, 
        y, ...,
        main = NA_character_,
        xlab = NA_character_,
        ylab = NA_character_,
        type = NA_integer_,
        palette = "Set1",
        theta = NA_real_, 
        plotPointsEnabled = NA,
        legendPosition = NA_integer_,
        showSource = FALSE,
        grid = 1,
        plotSettings = NULL) {
        
    .assertIsIntegerVector(type, "type", naAllowed = TRUE, validateType = FALSE)
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
            print(do.call(.plot.TrialDesignPlan, args))            
        } else {
            do.call(.plot.TrialDesignPlan, args)
        }
        return(.knitPrintQueue(x, sep = sep, prefix = sep))
    }
    
    return(do.call(.plot.TrialDesignPlan, args))
}
  
.plot.TrialDesignPlan <- function(
        x, 
        y, 
        ...,
        main = NA_character_,
        xlab = NA_character_,
        ylab = NA_character_,
        type = NA_integer_,
        palette = "Set1",
        theta = NA_real_, 
        plotPointsEnabled = NA,
        legendPosition = NA_integer_,
        showSource = FALSE,
        grid = 1,
        plotSettings = NULL) {
    fCall <- match.call(expand.dots = FALSE)
    
    designPlanName <- deparse(fCall$x)
    .assertGgplotIsInstalled()
    .assertIsSingleInteger(grid, "grid", validateType = FALSE)

    nMax <- list(...)[["nMax"]]
    if (!is.null(nMax)) {
        warning(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'nMax' (", nMax,
            ") will be ignored because it will be taken from design plan"
        )
    }

    if (all(is.na(type))) {
        availablePlotTypes <- getAvailablePlotTypes(x)
        if (length(availablePlotTypes) == 0) {
            stop("No plot available for this ", 
                .formatCamelCaseSingleWord(x$.objectType), " ", x$.toString(), 
                " result object")
        }
        
        type <- 1L
        if (length(availablePlotTypes) > 0 && !(type %in% availablePlotTypes)) {
            type <- availablePlotTypes[1]
        }
    }

    typeNumbers <- .getPlotTypeNumber(type, x)
    if (is.null(plotSettings)) {
        plotSettings <- .getGridPlotSettings(x, typeNumbers, grid)
    }
    p <- NULL
    plotList <- list()
    for (typeNumber in typeNumbers) {
        p <- .plotTrialDesignPlan(
            designPlan = x,
            main = main, xlab = xlab, ylab = ylab, type = typeNumber,
            palette = palette, theta = theta, plotPointsEnabled = plotPointsEnabled,
            legendPosition = .getGridLegendPosition(legendPosition, typeNumbers, grid),
            showSource = showSource, designPlanName = designPlanName,
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

    if (length(plotList) == 0) {
        message("No plots available for the specified design plan for ", x$.toString())
    }

    if (.isSpecialPlotShowSourceArgument(showSource)) {
        return(invisible(plotList))
    }

    return(.createPlotResultObject(plotList, grid))
}
