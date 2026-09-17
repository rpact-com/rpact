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

.showOptimalConditionalErrorDesign <- function(x, consoleOutputEnabled = TRUE) {
    cat <- function(..., sep = " ") {
        x$.cat(..., sep = sep, consoleOutputEnabled = consoleOutputEnabled)
    }
    cat("Optimal Conditional Error Function Design: \n \n")
    cat("General design parameters: \n")
    cat("  Overall significance level:", x$alpha, "\n")
    cat("  First-stage efficacy boundary (p-value scale):", x$alpha1, "\n")
    cat("  Binding first-stage futility boundary (p-value scale):", x$alpha0, "\n")
    if (x$minimumConditionalError > 0 || x$maximumConditionalError < 1) {
        cat(
            "  Constraints on optimal conditional error:",
            paste("[", x$minimumConditionalError, ", ", x$maximumConditionalError, "]", sep = ""),
            "\n"
        )
    }
    if (x$minimumSecondStageInformation > 0 || x$maximumSecondStageInformation < Inf) {
        cat(
            "  Constraints on second-stage information:",
            paste("[", x$minimumSecondStageInformation, ", ", x$maximumSecondStageInformation, "]", sep = ""),
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
            paste("[", x$delta1Min, ", ", x$delta1Max, "]", sep = ""),
            "\n"
        )
        cat(
            "  First-stage non-centrality parameter restricted to",
            paste("[", x$ncp1Min, ", ", x$ncp1Max, "]", sep = ""),
            "\n"
        )
    } else {
        cat("  Alternative:", x$delta1, "\n")
        cat("  First-stage non-centrality parameter:", x$ncp1, "\n")
    }
    cat("  First-stage information:", x$firstStageInformation, "\n")
    cat("\n")

    cat("Likelihood ratio specification: \n")
    switch(
        x$likelihoodRatioDistribution,
        fixed = {
            cat(
                "  Fixed parameter(s) in likelihood ratio: ",
                paste(format(x$deltaLR, trim = TRUE), collapse = ", "),
                "\n"
            )
            cat("  Parameter weights: ", paste(format(x$weightsDeltaLR, trim = TRUE), collapse = ", "), "\n")
        },
        normal = {
            cat(
                "  Normally distributed prior in likelihood ratio with mean ",
                x$deltaLR,
                " and standard deviation ",
                x$tauLR,
                "\n"
            )
        },
        unif = {
            cat("  Uniformly distributed prior in likelihood ratio with maximum ", x$deltaMaxLR, "\n")
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
        paste("[", x$levelConstantMinimum, ", ", x$levelConstantMaximum, "]", sep = ""),
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
#' \item \code{type = 1}: Plot the values of the optimal conditional error function against the first-stage p-value.
#' \item \code{type = 2}: Plot the second-stage information resulting from the optimal conditional error function against the first-stage p-value.
#' \item \code{type = 3}: Plot the likelihood ratio of the given specification of the optimal conditional error function against the first-stage p-value.
#' \item \code{type = 4}: Plot the function Q of the given specification of the optimal conditional error function against the first-stage p-value.
#' }
#' @param plotNonMonotoneFunction Logical. Should the non-monotone version of the plot be drawn? Not applicable for plot type 3. Default: \code{FALSE}.
#' @param ... Additional arguments for the generic method.
#'
#' @export
plot.TrialDesignOptimalConditionalError <- function(x, y, ..., range = c(0, 1), type = 1, plotNonMonotoneFunction = FALSE) {
    .assertIsOptimalConditionalErrorDesign(x)
    .assertIsNumericVector(range, "range", len = 2)
    .assertIsInClosedInterval(range, "range", lower = 0, upper = 1)
    if (range[1] >= range[2]) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'range' must be increasing.", call. = FALSE)
    }
    .assertIsSingleInteger(type, "type", validateType = FALSE)
    .assertIsInClosedInterval(type, "type", lower = 1, upper = 4)
    .assertIsSingleLogical(plotNonMonotoneFunction, "plotNonMonotoneFunction")
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package 'ggplot2' is required for plotting.", call. = FALSE)
    }
    if (type == 4) {
        range <- c(max(range[1], x$alpha1), min(range[2], x$alpha0))
        if (range[1] >= range[2]) {
            stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'range' must overlap the continuation region for plot type 4.",
                call. = FALSE
            )
        }
    }
    # Open endpoints avoid infinite likelihood ratios in diagnostic plots.
    firstStagePValues <- seq(range[1], range[2], length.out = 1000)
    if (type %in% c(3, 4)) firstStagePValues <- firstStagePValues[firstStagePValues > 0 & firstStagePValues < 1]
    getValues <- function(design) {
        switch(
            type,
            getOptimalConditionalError(firstStagePValues, design),
            getSecondStageInformation(firstStagePValues, design),
            .getLikelihoodRatio(firstStagePValues, design),
            .getMonotoneFunction(firstStagePValues, fun = .getQ, design = design)
        )
    }
    values <- getValues(x)
    data <- data.frame(firstStagePValue = firstStagePValues, value = values)
    result <- ggplot2::ggplot(data, ggplot2::aes(x = .data$firstStagePValue, y = .data$value)) +
        ggplot2::geom_line(linewidth = 1.1) +
        ggplot2::labs(x = "First-stage p-value", y = c(
            "Optimal conditional error", "Second-stage information", "Likelihood ratio", "Q"
        )[type]) +
        ggplot2::theme_bw() +
        ggplot2::geom_vline(xintercept = x$alpha0, linetype = "dotted", colour = "red") +
        ggplot2::geom_vline(xintercept = x$alpha1, linetype = "dotted", colour = "blue") +
        ggplot2::coord_cartesian(xlim = range)

    if (plotNonMonotoneFunction && type != 3) {
        if (!x$enforceMonotonicity || length(x$monotonisationConstants) == 0) {
            warning("No distinct non-monotone function is available for this design.", call. = FALSE)
        } else {
            secondDesign <- x$clone(deep = TRUE)
            secondDesign$enforceMonotonicity <- FALSE
            if (type %in% c(1, 2)) secondDesign$levelConstant <- .getLevelConstant(secondDesign)$root
            comparisonData <- data.frame(firstStagePValue = firstStagePValues, value = getValues(secondDesign))
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
    .assertIsOptimalConditionalErrorDesign(object)
    cat("Summary of the Optimal Conditional Error Function Design: \n \n")
    cat("General design parameters: \n")
    cat("  Overall significance level:", object$alpha, "\n")
    cat("  First-stage efficacy boundary (p-value scale):", object$alpha1, "\n")
    cat("  Binding first-stage futility boundary (p-value scale):", object$alpha0, "\n")
    cat("\n")

    cat("Second-stage information: \n")
    cat(
        "  Expected second-stage information (delta=0):",
        getExpectedSecondStageInformation(design = object, likelihoodRatioDistribution = "fixed", deltaLR = 0),
        "\n"
    )
    if (!object$useInterimEstimate && length(object$weightsDeltaLR) <= 1) {
        cat(
            "  Expected second-stage information (delta=delta1=",
            object$delta1,
            "): ",
            getExpectedSecondStageInformation(
                design = object,
                likelihoodRatioDistribution = "fixed",
                deltaLR = object$delta1
            ),
            "\n",
            sep = ""
        )
    }
    if (object$useInterimEstimate) {
        cat(
            "  Expected Second Stage Information (delta=delta1Min=",
            object$delta1Min,
            "): ",
            getExpectedSecondStageInformation(
                design = object,
                likelihoodRatioDistribution = "fixed",
                deltaLR = object$delta1Min
            ),
            "\n",
            sep = ""
        )
    }
    cat(
        "  Expected second-stage information (Given likelihood ratio distr.):",
        getExpectedSecondStageInformation(design = object),
        "\n"
    )
    if (object$likelihoodRatioDistribution != "maxlr") {
        if (object$likelihoodRatioDistribution == "fixed") {
            if (length(object$weightsDeltaLR) <= 1) {
                delta <- object$deltaLR
            } else {
                delta <- object$deltaLR %*% object$weightsDeltaLR
            }
        }
        if (object$likelihoodRatioDistribution == "normal") {
            delta <- object$deltaLR
        }
        if (object$likelihoodRatioDistribution == "exp") {
            delta <- 1 / (object$kappaLR * object$firstStageInformation)
        }
        if (object$likelihoodRatioDistribution == "unif") {
            delta <- object$deltaMaxLR / 2
        }
        cat(
            "  Expected second-stage information (delta=Mean of given likelihood ratio distr.=",
            delta,
            "): ",
            getExpectedSecondStageInformation(design = object, likelihoodRatioDistribution = "fixed", deltaLR = delta),
            "\n",
            sep = ""
        )
    }
    if (!is.na(object$conditionalPower)) {
        cat(
            "  Second-stage information at the futility boundary:",
            getSecondStageInformation(design = object, firstStagePValue = object$alpha0)
        )
    }
    cat("\n \n")

    cat("Power and stopping probabilities: \n")
    if (is.na(object$conditionalPower)) {
        cat("  Conditional power: Data-dependent user-specified function \n")
    } else if (!is.na(object$conditionalPower)) {
        cat("  Conditional power (fixed):", object$conditionalPower, "\n")
    }
    if (!object$useInterimEstimate && length(object$weightsDeltaLR) <= 1) {
        powerResults <- getOverallPower(design = object, alternative = object$delta1)
        cat("  Overall power (delta1=", object$delta1, "): ", powerResults$overallPower, "\n", sep = "")
        cat(
            "  Efficacy stopping probability (delta1=",
            object$delta1,
            "): ",
            powerResults$firstStageEfficacy,
            "\n",
            sep = ""
        )
        cat(
            "  Futility stopping probability (delta1=",
            object$delta1,
            "): ",
            powerResults$firstStageFutility,
            "\n",
            sep = ""
        )
    }
    if (object$useInterimEstimate) {
        powerResults <- getOverallPower(design = object, alternative = object$delta1Min)
        cat("  Overall power (delta1=delta1Min= ", object$delta1Min, "): ", powerResults$overallPower, "\n", sep = "")
        cat(
            "  Efficacy stopping probability (delta1=delta1Min= ",
            object$delta1Min,
            "): ",
            powerResults$firstStageEfficacy,
            "\n",
            sep = ""
        )
        cat(
            "  Futility stopping probability (delta1=delta1Min= ",
            object$delta1Min,
            "): ",
            powerResults$firstStageFutility,
            "\n",
            sep = ""
        )
    }
    if (object$likelihoodRatioDistribution != "maxlr") {
        if (object$likelihoodRatioDistribution == "fixed") {
            if (length(object$weightsDeltaLR) <= 1) {
                delta1 <- object$deltaLR
            } else {
                delta1 <- as.numeric(object$deltaLR %*% object$weightsDeltaLR)
            }
        }
        if (object$likelihoodRatioDistribution == "normal") {
            delta1 <- object$deltaLR
        }
        if (object$likelihoodRatioDistribution == "exp") {
            delta1 <- 1 / (object$kappaLR * object$firstStageInformation)
        }
        if (object$likelihoodRatioDistribution == "unif") {
            delta1 <- object$deltaMaxLR / 2
        }
        powerResults <- getOverallPower(design = object, alternative = delta1)
        cat(
            "  Overall power (delta1=Mean of given likelihood ratio distr.= ",
            delta1,
            "): ",
            powerResults$overallPower,
            "\n",
            sep = ""
        )
        cat(
            "  Efficacy stopping probability (delta1=Mean of given likelihood ratio distr.= ",
            delta1,
            "): ",
            powerResults$firstStageEfficacy,
            "\n",
            sep = ""
        )
        cat(
            "  Futility stopping probability (delta1=Mean of given likelihood ratio distr.= ",
            delta1,
            "): ",
            powerResults$firstStageFutility,
            "\n",
            sep = ""
        )
    }
    cat("\n")
    return(invisible(object))
}
