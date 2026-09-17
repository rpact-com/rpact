#' Print optimal conditional error trial design
#'
#' @description
#' Print an overview of the specified design parameters.
#'
#' @param x Design object of class \code{TrialDesignOptimalConditionalError}
#' @param ... Additional arguments required for generic compatibility
#'
#'
#' @export
print.TrialDesignOptimalConditionalError <- function(x, ...) {
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
            cat("  Exponentially distributed prior in likelihood ratio with mean ", x$kappaLR, "\n")
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
#' @param range Numeric vector with two entries specifying the range of the x-axis of the plot.
#' @param type Type of plot to be created. Options are: \itemize{
#' \item \code{type = 1}: Plot the values of the optimal conditional error function against the first-stage p-value.
#' \item \code{type = 2}: Plot the second-stage information resulting from the optimal conditional error function against the first-stage p-value.
#' \item \code{type = 3}: Plot the likelihood ratio of the given specification of the optimal conditional error function against the first-stage p-value.
#' \item \code{type = 4}: Plot the function Q of the given specification of the optimal conditional error function against the first-stage p-value.
#' }
#' @param plotNonMonotoneFunction Logical. Should the non-monotone version of the plot be drawn? Not applicable for plot type 3. Default: \code{FALSE}.
#' @param ... Additional arguments required for generic compatibility
#'
#' @export
plot.TrialDesignOptimalConditionalError <- function(
    x,
    range = c(0, 1),
    type = 1,
    plotNonMonotoneFunction = FALSE,
    ...
) {
    # Set a range of first-stage p-values
    firstStagePValues <- seq(from = range[1], to = range[2], length.out = 1e3)

    # Plot type 1: draw the optimal conditional error function
    if (type == 1) {
        # Calculate optimal conditional error for provided design
        optimalConditionalErrors <- getOptimalConditionalError(
            firstStagePValue = firstStagePValues,
            design = x
        )

        # Create base plot and save it for possible addition of the non-monotone function
        designPlot <- ggplot2::ggplot(data = NULL) +
            ggplot2::geom_line(
                mapping = ggplot2::aes(x = firstStagePValues, y = optimalConditionalErrors),
                colour = "black",
                linetype = "solid",
                linewidth = 1.1
            ) +
            ggplot2::labs(x = "First-stage p-value", y = "Optimal Conditional Error") +
            ggplot2::theme_bw() +
            ggplot2::geom_vline(xintercept = x$alpha0, linetype = "dotted", col = "red", linewidth = 0.8) +
            ggplot2::geom_vline(xintercept = x$alpha1, linetype = "dotted", col = "blue", linewidth = 0.8) +
            ggplot2::xlim(c(range[1], range[2]))

        # In addition to the monotone function, the non-monotone one should be drawn
        if (plotNonMonotoneFunction) {
            # There are no monotonisation constants -> monotonisation is not required
            if (is.null(unlist(x$monotonisationConstants))) {
                warning("No monotonisation required. Displaying monotone function only.")
                designPlot # Display base plot
            } else {
                # Plotting the non-monotone function only makes sense if the provided design is enforced to be monotone
                if (x$enforceMonotonicity) {
                    # Here, a new design must be created, modifying the old one will not work as intended (Operation is not "copy-on-modify")
                    # All fields apart from enforceMonotonicity are copied from the original design object.
                    # (There may be a better way to implement this)
                    # suppressWarnings() is used because this code is expected to always produce a warning
                    secondDesign <- suppressWarnings(new(
                        "TrialDesignOptimalConditionalError",
                        alpha = x$alpha,
                        alpha1 = x$alpha1,
                        alpha0 = x$alpha0,
                        conditionalPower = x$conditionalPower,
                        conditionalPowerFunction = x$conditionalPowerFunction,
                        delta1 = x$delta1,
                        firstStageInformation = x$firstStageInformation,
                        useInterimEstimate = x$useInterimEstimate,
                        likelihoodRatioDistribution = x$likelihoodRatioDistribution,
                        deltaLR = x$deltaLR,
                        weightsDeltaLR = x$weightsDeltaLR,
                        tauLR = x$tauLR,
                        kappaLR = x$kappaLR,
                        deltaMaxLR = x$deltaMaxLR,
                        minimumConditionalError = x$minimumConditionalError,
                        maximumConditionalError = x$maximumConditionalError,
                        levelConstantMinimum = x$levelConstantMinimum,
                        levelConstantMaximum = x$levelConstantMaximum,
                        ncp1Min = x$ncp1Min,
                        ncp1Max = x$ncp1Max,
                        enforceMonotonicity = FALSE
                    ))

                    # Calculate optimal conditional error for the new design
                    nonMonoOptimalConditionalErrors <- getOptimalConditionalError(
                        firstStagePValue = firstStagePValues,
                        design = secondDesign
                    )

                    # Add non-monotone optimal conditional error to base plot and display it
                    designPlot +
                        ggplot2::geom_line(
                            mapping = ggplot2::aes(x = firstStagePValues, y = nonMonoOptimalConditionalErrors),
                            colour = "gray",
                            linetype = "dashed",
                            linewidth = 1.1
                        )
                } else {
                    warning(
                        "When using plotNonMonotoneFunction=TRUE, x should provide a monotone function. Consider setting enforceMonotonicity=TRUE in design object."
                    )
                    designPlot # Display base plot
                }
            }
        } else {
            designPlot # Display base plot
        }
    } else if (type == 2) {
        # Plot type 2: draw second-stage information
        # Calculate second-stage information
        secondStageInformation <- getSecondStageInformation(
            firstStagePValue = firstStagePValues,
            design = x
        )

        # Create base plot and save it for possible addition of the non-monotone function
        informationPlot <- ggplot2::ggplot() +
            ggplot2::geom_line(
                mapping = ggplot2::aes(x = firstStagePValues, y = secondStageInformation),
                colour = "black",
                linetype = "solid",
                linewidth = 1.1
            ) +
            ggplot2::labs(x = "First-stage p-value", y = "Second-stage information") +
            ggplot2::theme_bw() +
            ggplot2::geom_vline(xintercept = x$alpha0, linetype = "dotted", col = "red", linewidth = 0.8) +
            ggplot2::geom_vline(xintercept = x$alpha1, linetype = "dotted", col = "blue", linewidth = 0.8) +
            ggplot2::xlim(c(range[1], range[2]))

        if (plotNonMonotoneFunction) {
            # There are no monotonisation constants -> monotonisation is not required
            if (is.null(unlist(x$monotonisationConstants))) {
                warning("No monotonisation required. Displaying monotone function only.")
                informationPlot # Display base plot
            } else {
                # Plotting the non-monotone function only makes sense if the provided design is enforced to be monotone
                if (x$enforceMonotonicity) {
                    # Here, a new design must be created, modifying the old one will not work as intended (Operation is not "copy-on-modify")
                    # All fields apart from enforceMonotonicity are copied from the original design object.
                    # (There may be a better way to implement this)
                    # suppressWarnings() is used because this code is expected to always produce a warning
                    secondDesign <- suppressWarnings(new(
                        "TrialDesignOptimalConditionalError",
                        alpha = x$alpha,
                        alpha1 = x$alpha1,
                        alpha0 = x$alpha0,
                        conditionalPower = x$conditionalPower,
                        conditionalPowerFunction = x$conditionalPowerFunction,
                        delta1 = x$delta1,
                        firstStageInformation = x$firstStageInformation,
                        useInterimEstimate = x$useInterimEstimate,
                        likelihoodRatioDistribution = x$likelihoodRatioDistribution,
                        deltaLR = x$deltaLR,
                        weightsDeltaLR = x$weightsDeltaLR,
                        tauLR = x$tauLR,
                        kappaLR = x$kappaLR,
                        deltaMaxLR = x$deltaMaxLR,
                        minimumConditionalError = x$minimumConditionalError,
                        maximumConditionalError = x$maximumConditionalError,
                        levelConstantMinimum = x$levelConstantMinimum,
                        levelConstantMaximum = x$levelConstantMaximum,
                        ncp1Min = x$ncp1Min,
                        ncp1Max = x$ncp1Max,
                        enforceMonotonicity = FALSE
                    ))

                    # Calculate optimal conditional error for the new design
                    nonMonoSecondStageInformation <- getSecondStageInformation(
                        firstStagePValue = firstStagePValues,
                        design = secondDesign
                    )

                    # Add non-monotone optimal conditional error to base plot and display it
                    informationPlot +
                        ggplot2::geom_line(
                            mapping = ggplot2::aes(x = firstStagePValues, y = nonMonoSecondStageInformation),
                            colour = "gray",
                            linetype = "dashed",
                            linewidth = 1.1
                        )
                } else {
                    warning(
                        "When using plotNonMonotoneFunction=TRUE, x should provide a monotone function. Consider setting enforceMonotonicity=TRUE in design object."
                    )
                    informationPlot # Display base plot
                }
            }
        } else {
            informationPlot # Display base plot
        }
    } else if (type == 3) {
        # Plot type 3: likelihood ratio
        # Calculate likelihood ratio values
        likelihoodRatios <- .getLikelihoodRatio(
            firstStagePValue = firstStagePValues,
            design = x
        )

        likelihoodRatioPlot <- ggplot2::ggplot() +
            ggplot2::geom_line(
                mapping = ggplot2::aes(x = firstStagePValues, y = likelihoodRatios),
                colour = "black",
                linetype = "solid",
                linewidth = 1.1
            ) +
            ggplot2::labs(x = "First-stage p-value", y = "Likelihood ratio") +
            ggplot2::theme_bw() +
            ggplot2::geom_vline(xintercept = x$alpha0, linetype = "dotted", col = "red", linewidth = 0.8) +
            ggplot2::geom_vline(xintercept = x$alpha1, linetype = "dotted", col = "blue", linewidth = 0.8) +
            ggplot2::xlim(c(range[1], range[2]))

        likelihoodRatioPlot
    } else if (type == 4) {
        # Plot type 4: Q
        firstStagePValues <- seq(from = max(range[1], x$alpha1), to = min(range[2], x$alpha0), length.out = 1e3)
        # Calculate Q values
        Q <- .getMonotoneFunction(
            x = firstStagePValues,
            fun = .getQ,
            design = x
        )
        QPlot <- ggplot2::ggplot() +
            ggplot2::geom_line(
                mapping = ggplot2::aes(x = firstStagePValues, y = Q),
                colour = "black",
                linetype = "solid",
                linewidth = 1.1
            ) +
            ggplot2::labs(x = "First-stage p-value", y = "Q") +
            ggplot2::theme_bw() +
            ggplot2::geom_vline(xintercept = x$alpha0, linetype = "dotted", col = "red", linewidth = 0.8) +
            ggplot2::geom_vline(xintercept = x$alpha1, linetype = "dotted", col = "blue", linewidth = 0.8) +
            ggplot2::xlim(c(max(range[1], x$alpha1), min(x$alpha0, range[2])))

        if (plotNonMonotoneFunction && x$enforceMonotonicity && !is.null(unlist(x$monotonisationConstants))) {
            nonMonoQ <- .getQ(
                firstStagePValue = firstStagePValues,
                design = x
            )

            QPlot <- QPlot +
                ggplot2::geom_line(
                    mapping = ggplot2::aes(
                        x = firstStagePValues,
                        y = nonMonoQ
                    ),
                    colour = "gray",
                    linetype = "dashed",
                    linewidth = 1.1
                )
        }
        QPlot
    }
}

#' Summary of the optimal conditional error trial design
#'
#' @description
#' Provide an overview of the operating characteristics of the optimal conditional error trial design.
#'
#' @param object Design object of class \code{TrialDesignOptimalConditionalError}
#' @param ... Additional arguments required for generic compatibility
#'
#'
#' @export
summary.TrialDesignOptimalConditionalError <- function(object, ...) {
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
    if (object$useInterimEstimate == FALSE & length(object$weightsDeltaLR) <= 1) {
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
    if (object$useInterimEstimate == TRUE) {
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
            delta <- object$kappaLR
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
            "  Maximum second-stage information:",
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
    if (object$useInterimEstimate == FALSE & length(object$weightsDeltaLR) <= 1) {
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
    if (object$useInterimEstimate == TRUE) {
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
            delta1 <- object$kappaLR
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
}
