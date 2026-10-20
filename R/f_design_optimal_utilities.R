## |
## |  *Optimal conditional error output and plots*
## |
## |  This file is part of the R package rpact:
## |  Confirmatory Adaptive Clinical Trial Design and Analysis
## |
## |  Original contribution: Morten Dreher
## |  Licensed under "GNU Lesser General Public License" version 3
## |  License text: https://www.r-project.org/Licenses/LGPL-3
## |

#' Print optimal conditional error trial design
#'
#' @description
#' Print an overview of the specified design parameters.
#'
#' @param x Design object of class \code{TrialDesignOptimalConditionalError}
#' @param ... Additional arguments for the generic method.
#'
#'
#' @param markdown Logical; use the rpact Markdown output setting by default.
#' @return The design, invisibly.
#' @export
print.TrialDesignOptimalConditionalError <- function(x, ..., markdown = NA) {
    return(print.ParameterSet(x, ..., markdown = markdown))
}

.showOptimalDesign <- function(x, consoleOutputEnabled = TRUE) {
    cat <- function(..., sep = " ") {
        x$.cat(..., sep = sep, consoleOutputEnabled = consoleOutputEnabled)
    }
    cat("Optimal Conditional Error Design: \n \n")
    cat("General design parameters: \n")
    cat("  Overall significance level:", x$alpha, "\n")
    cat("  First-stage efficacy boundary (p-value scale):", x$efficacyBounds, "\n")
    cat("  Binding first-stage futility boundary (p-value scale):", x$futilityBounds, "\n")
    if (x$minConditionalError > 0 || x$maxConditionalError < 1) {
        cat(
            "  Constraints on optimal conditional error:",
            paste("[", x$minConditionalError, ", ", x$maxConditionalError, "]", sep = ""),
            "\n"
        )
    }
    if (x$minInformationPerStage > 0 || x$maxInformationPerStage < Inf) {
        cat(
            "  Constraints on second-stage information:",
            paste("[", x$minInformationPerStage, ", ", x$maxInformationPerStage, "]", sep = ""),
            "\n"
        )
    }

    cat("\n")

    cat("Conditional power specification: \n")
    if (is.na(x$conditionalPower)) {
        cat("  Data-dependent user-specified function \n")
    } else if (!is.na(x$conditionalPower)) {
        cat("  Target conditional power:", x$conditionalPower, "\n")
    }
    if (x$useInterimEstimate) {
        cat(
            "  Alternative: interim estimate restricted to",
            paste("[", x$minThetaH1, ", ", x$maxThetaH1, "]", sep = ""),
            "\n"
        )
        cat(
            "  First-stage non-centrality parameter restricted to",
            paste("[", x$minNonCentralityParameterH1, ", ", x$maxNonCentralityParameterH1, "]", sep = ""),
            "\n"
        )
    } else {
        cat("  Alternative:", x$thetaH1, "\n")
        cat("  First-stage non-centrality parameter:", x$nonCentralityParameterH1, "\n")
    }
    cat("  First-stage information:", x$firstStageInformation, "\n")
    cat("\n")

    cat("Likelihood ratio specification: \n")
    switch(
        x$likelihoodRatioDistribution,
        fixed = {
            cat(
                "  Fixed parameter(s) in likelihood ratio: ",
                paste(format(x$thetaLR, trim = TRUE), collapse = ", "),
                "\n"
            )
            cat("  Parameter weights: ", paste(format(x$weightsLR, trim = TRUE), collapse = ", "), "\n")
        },
        normal = {
            cat(
                "  Normally distributed prior in likelihood ratio with mean ",
                x$thetaLR,
                " and standard deviation ",
                x$stDevLR,
                "\n"
            )
        },
        unif = {
            cat("  Uniformly distributed prior in likelihood ratio with maximum ", x$maxThetaLR, "\n")
        },
        exp = {
            cat("  Exponentially distributed prior in likelihood ratio with scaled rate ", x$kappaLR, "\n")
        },
        maxlr = {
            cat("  Maximum likelihood ratio \n")
        }
    )

    cat("\n")
    cat("Level constant: \n")
    cat("  Constant:", x$levelConstant, "\n")
    cat(
        "  Searched on interval:",
        paste("[", x$minLevelConstant, ", ", x$maxLevelConstant, "]", sep = ""),
        "\n"
    )

    if (!is.null(unlist(x$monotonisationConstants))) {
        cat("\n")
        cat("Monotonisation constants: \n")
        cat(
            "  Intervals (p-value scale):",
            paste(
                "[",
                apply(
                    X = cbind(x$monotonisationConstants$dls, x$monotonisationConstants$dus),
                    FUN = paste,
                    MARGIN = 1,
                    collapse = ", "
                ),
                "]",
                sep = ""
            ),
            "\n"
        )
        cat("  Constant(s) (Q scale):", x$monotonisationConstants$qs, "\n")
    }
    if (!x$enforceMonotonicity) {
        cat("\n")
        cat("Monotonicity was not enforced \n")
    }
}

#' Plot the optimal conditional error function
#'
#' @description
#' The returned plot is a \code{ggplot2} object and can be supplemented with additional layers using \code{ggplot2} commands.
#'
#' @param x Design object of class \code{TrialDesignOptimalConditionalError}.
#' @param y Not used; included for compatibility with `plot()`.
#' @importFrom rlang .data
#' @return A `ggplot` object.
#' @param range Numeric vector with two entries specifying the range of the x-axis of the plot.
#' @param type Type of plot to be created. Options are: \itemize{
#' \item \code{type = "conditionalError"} (or \code{1}): Plot the values of the optimal conditional error function against the first-stage p-value.
#' \item \code{type = "stageInformation"} (or \code{2}): Plot the second-stage information resulting from the optimal conditional error function against the first-stage p-value.
#' \item \code{type = "likelihoodRatio"} (or \code{3}): Plot the likelihood ratio of the given specification of the optimal conditional error function against the first-stage p-value.
#' \item \code{type = "qFunction"} (or \code{4}): Plot the function Q of the given specification of the optimal conditional error function against the first-stage p-value.
#' }
#' @param plotNonMonotoneFunction Logical. Should the non-monotone version of the plot be drawn? Not applicable for `type = "likelihoodRatio"`. Default: \code{FALSE}.
#' @param ... Additional arguments for the generic method.
#'
#' @export
plot.TrialDesignOptimalConditionalError <- function(x, y, ..., range = c(0, 1), type = "conditionalError", plotNonMonotoneFunction = FALSE) {
    .assertIsOptimalDesign(x)
    .assertIsNumericVector(range, "range", len = 2)
    .assertIsInClosedInterval(range, "range", lower = 0, upper = 1)
    if (range[1] >= range[2]) {
        stopIllegalArgument(
            "'range' must be increasing.",
            parameter = "range",
            value = range,
            constraint = "range[1] < range[2]",
            functionName = "plot.TrialDesignOptimalConditionalError"
        )
    }
    if (is.character(type)) {
        .assertIsSingleCharacter(type, "type")
        plotTypes <- c("conditionalError", "stageInformation", "likelihoodRatio", "qFunction")
        if (!type %in% plotTypes) {
            stopIllegalArgument(
                "Unknown optimal design plot type.", parameter = "type", value = type,
                constraint = paste(plotTypes, collapse = ", "),
                functionName = "plot.TrialDesignOptimalConditionalError"
            )
        }
        type <- match(type, plotTypes)
    }
    .assertIsSingleInteger(type, "type", validateType = FALSE)
    .assertIsInClosedInterval(type, "type", lower = 1, upper = 4)
    .assertIsSingleLogical(plotNonMonotoneFunction, "plotNonMonotoneFunction")
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stopRuntimeIssue(
            "Package 'ggplot2' is required for plotting.",
            parameter = "package",
            value = "ggplot2",
            constraint = "ggplot2 must be installed for plotting",
            functionName = "plot.TrialDesignOptimalConditionalError"
        )
    }
    if (type == 4) {
        continuationRange <- c(max(range[1], x$efficacyBounds), min(range[2], x$futilityBounds))
        if (continuationRange[1] >= continuationRange[2]) {
            stopConflictingArguments(
                "'range' must overlap the continuation region for plot type 4.",
                parameter = "range",
                value = range,
                constraint = "must overlap the continuation region for type 4",
                relatedParameter = c("type", "efficacyBounds", "futilityBounds"),
                relatedValue = list(type = type, efficacyBounds = x$efficacyBounds, futilityBounds = x$futilityBounds),
                functionName = "plot.TrialDesignOptimalConditionalError"
            )
        }
        range <- continuationRange
    }
    # Open endpoints avoid infinite likelihood ratios in diagnostic plots.
    pValues <- seq(range[1], range[2], length.out = 1000)
    if (type %in% c(3, 4)) pValues <- pValues[pValues > 0 & pValues < 1]
    getValues <- function(design) {
        switch(
            type,
            getConditionalError(pValue = pValues, design = design),
            getStageInformation(pValue = pValues, design = design),
            .getOptimalDesignLikelihoodRatio(pValues, design),
            .getOptimalDesignMonotoneValues(pValues, fun = .getOptimalDesignQ, design = design)
        )
    }
    values <- getValues(x)
    data <- data.frame(pValue = pValues, value = values)
    result <- ggplot2::ggplot(data, ggplot2::aes(x = .data$pValue, y = .data$value)) +
        ggplot2::geom_line(linewidth = 1.1) +
        ggplot2::labs(x = "First-stage p-value", y = c(
            "Optimal conditional error", "Second-stage information", "Likelihood ratio", "Q"
        )[type]) +
        ggplot2::theme_bw() +
        ggplot2::geom_vline(xintercept = x$futilityBounds, linetype = "dotted", colour = "red") +
        ggplot2::geom_vline(xintercept = x$efficacyBounds, linetype = "dotted", colour = "blue") +
        ggplot2::coord_cartesian(xlim = range)

    if (plotNonMonotoneFunction && type != 3) {
        if (!x$enforceMonotonicity || length(x$monotonisationConstants) == 0) {
            warnResultUnavailable("No distinct non-monotone function is available for this design.",
                diagnosticId = "optimal.nonmonotone_power_unavailable"
            )
        } else {
            secondDesign <- x$clone(deep = TRUE)
            secondDesign$enforceMonotonicity <- FALSE
            if (type %in% c(1, 2)) secondDesign$levelConstant <- .getOptimalDesignLevelConstant(secondDesign)$root
            comparisonData <- data.frame(pValue = pValues, value = getValues(secondDesign))
            result <- result + ggplot2::geom_line(
                data = comparisonData,
                colour = "gray", linetype = "dashed", linewidth = 1.1
            )
        }
    }
    return(result)
}

#' Summary of the optimal conditional error trial design
#'
#' @description
#' Provide an overview of the operating characteristics of the optimal conditional error trial design.
#'
#' @param object Design object of class \code{TrialDesignOptimalConditionalError}
#' @param ... Additional arguments for the generic method.
#'
#'
#' @return The design, invisibly, after printing its operating characteristics.
#' @export
summary.TrialDesignOptimalConditionalError <- function(object, ...) {
    .assertIsOptimalDesign(object)
    cat("Summary of the Optimal Conditional Error Design: \n \n")
    cat("General design parameters: \n")
    cat("  Overall significance level:", object$alpha, "\n")
    cat("  First-stage efficacy boundary (p-value scale):", object$efficacyBounds, "\n")
    cat("  Binding first-stage futility boundary (p-value scale):", object$futilityBounds, "\n")
    cat("\n")

    cat("Second-stage information: \n")
    cat(
        "  Expected second-stage information (theta=0):",
        getStageInformation(type = "expected", design = object, 
            likelihoodRatioDistribution = "fixed", thetaLR = 0),
        "\n"
    )
    if (!object$useInterimEstimate && length(object$weightsLR) <= 1) {
        cat(
            "  Expected second-stage information (theta=thetaH1=",
            object$thetaH1,
            "): ",
            getStageInformation(type = "expected",
                design = object,
                likelihoodRatioDistribution = "fixed",
                thetaLR = object$thetaH1
            ),
            "\n",
            sep = ""
        )
    }
    if (object$useInterimEstimate) {
        cat(
            "  Expected Second Stage Information (theta=minThetaH1=",
            object$minThetaH1,
            "): ",
            getStageInformation(type = "expected",
                design = object,
                likelihoodRatioDistribution = "fixed",
                thetaLR = object$minThetaH1
            ),
            "\n",
            sep = ""
        )
    }
    if (object$likelihoodRatioDistribution != "maxlr") {
        cat(
            "  Expected second-stage information (Given likelihood ratio distr.):",
            getStageInformation(type = "expected", design = object),
            "\n"
        )
        if (object$likelihoodRatioDistribution == "fixed") {
            if (length(object$weightsLR) <= 1) {
                theta <- object$thetaLR
            } else {
                theta <- object$thetaLR %*% object$weightsLR
            }
        }
        if (object$likelihoodRatioDistribution == "normal") {
            theta <- object$thetaLR
        }
        if (object$likelihoodRatioDistribution == "exp") {
            theta <- 1 / (object$kappaLR * object$firstStageInformation)
        }
        if (object$likelihoodRatioDistribution == "unif") {
            theta <- object$maxThetaLR / 2
        }
        cat(
            "  Expected second-stage information (theta=Mean of given likelihood ratio distr.=",
            theta,
            "): ",
            getStageInformation(type = "expected", design = object, 
                likelihoodRatioDistribution = "fixed", thetaLR = theta),
            "\n",
            sep = ""
        )
    }
    if (!is.na(object$conditionalPower)) {
        cat(
            "  Second-stage information at the futility boundary:",
            getStageInformation(design = object, pValue = object$futilityBounds)
        )
    }
    cat("\n \n")

    cat("Power and stopping probabilities: \n")
    if (is.na(object$conditionalPower)) {
        cat("  Conditional power: Data-dependent user-specified function \n")
    } else if (!is.na(object$conditionalPower)) {
        cat("  Conditional power (fixed):", object$conditionalPower, "\n")
    }
    if (!object$useInterimEstimate && length(object$weightsLR) <= 1) {
        powerResults <- getDesignCharacteristics(design = object, theta = object$thetaH1)
        cat("  Overall power (thetaH1=", object$thetaH1, "): ", powerResults$overallReject, "\n", sep = "")
        cat(
            "  Efficacy stopping probability (thetaH1=",
            object$thetaH1,
            "): ",
            powerResults$rejectPerStage[1, ],
            "\n",
            sep = ""
        )
        cat(
            "  Futility stopping probability (thetaH1=",
            object$thetaH1,
            "): ",
            powerResults$futilityPerStage[1, ],
            "\n",
            sep = ""
        )
    }
    if (object$useInterimEstimate) {
        powerResults <- getDesignCharacteristics(design = object, theta = object$minThetaH1)
        cat("  Overall power (thetaH1=minThetaH1= ", object$minThetaH1, "): ", powerResults$overallReject, "\n", sep = "")
        cat(
            "  Efficacy stopping probability (thetaH1=minThetaH1= ",
            object$minThetaH1,
            "): ",
            powerResults$rejectPerStage[1, ],
            "\n",
            sep = ""
        )
        cat(
            "  Futility stopping probability (thetaH1=minThetaH1= ",
            object$minThetaH1,
            "): ",
            powerResults$futilityPerStage[1, ],
            "\n",
            sep = ""
        )
    }
    if (object$likelihoodRatioDistribution != "maxlr") {
        if (object$likelihoodRatioDistribution == "fixed") {
            if (length(object$weightsLR) <= 1) {
                thetaH1 <- object$thetaLR
            } else {
                thetaH1 <- as.numeric(object$thetaLR %*% object$weightsLR)
            }
        }
        if (object$likelihoodRatioDistribution == "normal") {
            thetaH1 <- object$thetaLR
        }
        if (object$likelihoodRatioDistribution == "exp") {
            thetaH1 <- 1 / (object$kappaLR * object$firstStageInformation)
        }
        if (object$likelihoodRatioDistribution == "unif") {
            thetaH1 <- object$maxThetaLR / 2
        }
        powerResults <- getDesignCharacteristics(design = object, theta = thetaH1)
        cat(
            "  Overall power (thetaH1=Mean of given likelihood ratio distr.= ",
            thetaH1,
            "): ",
            powerResults$overallReject,
            "\n",
            sep = ""
        )
        cat(
            "  Efficacy stopping probability (thetaH1=Mean of given likelihood ratio distr.= ",
            thetaH1,
            "): ",
            powerResults$rejectPerStage[1, ],
            "\n",
            sep = ""
        )
        cat(
            "  Futility stopping probability (thetaH1=Mean of given likelihood ratio distr.= ",
            thetaH1,
            "): ",
            powerResults$futilityPerStage[1, ],
            "\n",
            sep = ""
        )
    }
    cat("\n")
    return(invisible(object))
}
