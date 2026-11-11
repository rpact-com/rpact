#'
#' @title
#' Plot Bayesian Quantitative Decision Regions
#'
#' @description
#' Displays decisions based on posterior criteria rather than frequentist intervals.
#'
#' @param x A Bayesian rule from [getDesignQuantitative()].
#' @param ... Additional arguments are checked for unknown names.
#' @param type 1 for observed effect estimates and posterior intervals, 2 for
#'   an effect-estimate/standard-error decision map, 3 for a binary count map.
#' @param estimates Numeric vector of observed treatment-minus-control estimates
#'   for type 1.
#' @param standardErrors Positive sampling standard errors, one per estimate or
#'   a common scalar. These describe effect estimates, not arm-level SDs.
#' @param priorDist RBesT normMix effect prior for types 1 and 2. Defaults to
#'   mixnorm(c(1, 0, 100)). Group-mean priors are supported by the OC function;
#'   these effect-estimate plots require an effect prior.
#' @param estimateRange,standardErrorRange Increasing two-element ranges for type 2.
#' @param gridPoints Integer grid resolution per axis for type 2, at most 200.
#' @param numberOfSubjects Positive integer vector (control, treatment) for type 3.
#' @param priorDist1,priorDist2 RBesT betaMix treatment and control priors for type 3.
#' @param directionUpper Whether increasing treatment rates are beneficial.
#' @param plotCat Three or four decision categories.
#' @param main Plot title.
#' @param plotSettings Optional PlotSettings object.
#'
#' @details
#' Type 1 shows equal-tail 95 percent posterior intervals for orientation;
#' categories always use the actual posterior criteria, not these display bounds.
#' Exact posterior probability-threshold ties belong to Indeterminate 2.
#' Type 3 uses the same full posterior decision grid as binary OCs. The grid
#' contains at most one million cells; large grids can be expensive to evaluate.
#'
#' @return A ggplot object containing the evaluated decisions in its data.
#'
#' @seealso [getOperatingCharacteristicsMeans()], [getOperatingCharacteristicsRates()]
#'
#' @export
#'
plot.TrialDesignPlanQuantitativeBayes <- function(x, ..., type = 1L,
        estimates = NULL, standardErrors = 1, priorDist = NULL,
        estimateRange = c(-1, 2), standardErrorRange = c(0.1, 1), gridPoints = 50L,
        numberOfSubjects = NULL, priorDist1 = NULL, priorDist2 = NULL,
        directionUpper = TRUE, plotCat = 4L, main = "Bayesian quantitative decisions", plotSettings = NULL) {
    functionName <- "plot.TrialDesignPlanQuantitativeBayes"
    .warnInCaseOfUnknownArguments(functionName = functionName, ...)
    .assertQtdRBest(functionName)
    .assertGgplotIsInstalled()
    .validateQtdBayesRule(x, functionName)
    .assertQtdPlotChoice(type, "type", 1:3, functionName)
    .assertQtdPlotChoice(plotCat, "plotCat", c(3L, 4L), functionName)
    if (type == 3L) {
        .assertQtdPlotNumeric(numberOfSubjects, "numberOfSubjects", functionName,
            lower = 1, integer = TRUE, expectedLength = 2L)
        .assertIsSingleLogical(directionUpper, "directionUpper")
        if (is.null(priorDist1)) {
            priorDist1 <- RBesT::mixbeta(c(1, 1 / 3, 1 / 3))
        }
        if (is.null(priorDist2)) {
            priorDist2 <- RBesT::mixbeta(c(1, 1 / 3, 1 / 3))
        }
        .assertQtdPrior(priorDist1, "priorDist1", "betaMix", functionName)
        .assertQtdPrior(priorDist2, "priorDist2", "betaMix", functionName)
        data <- .getQtdBayesBinaryGrid(x, numberOfSubjects, directionUpper, priorDist1, priorDist2,
            functionName = functionName)
        if (plotCat == 3L) {
            data$category <- .getQtdCoarseCategory(data$category)
        }
        data$category <- factor(data$category, levels = .getQtdCategories(plotCat))
        p <- ggplot2::ggplot(data, ggplot2::aes(.data[["control"]], .data[["treatment"]], fill = .data[["category"]])) +
            ggplot2::geom_tile() + ggplot2::labs(x = "Control events", y = "Treatment events", fill = "Decision")
    } else {
        if (is.null(priorDist)) {
            priorDist <- RBesT::mixnorm(c(1, 0, 100))
        }
        .assertQtdPrior(priorDist, "priorDist", "normMix", functionName)
        if (type == 1L) {
            .assertQtdPlotNumeric(estimates, "estimates", functionName)
            .assertQtdPlotNumeric(standardErrors, "standardErrors", functionName, lower = 0, openLower = TRUE)
            if (!(length(standardErrors) %in% c(1L, length(estimates)))) {
                stopArgumentLengthOutOfBounds("Supply one standard error or one per estimate",
                    functionName = functionName, parameter = "standardErrors", value = standardErrors,
                    constraint = "one or length(estimates)", expectedLength = c(1L, length(estimates)),
                    actualLength = length(standardErrors), diagnosticId = "qtd.bayes.plot_sd_length")
            }
            data <- data.frame(estimate = estimates, standardError = rep(standardErrors, length.out = length(estimates)))
        } else {
            .assertQtdPlotRange(estimateRange, "estimateRange", functionName)
            .assertQtdPlotRange(standardErrorRange, "standardErrorRange", functionName, lower = 0)
            .assertQtdPlotNumeric(standardErrorRange, "standardErrorRange", functionName, lower = 0, openLower = TRUE)
            .assertQtdPlotNumeric(gridPoints, "gridPoints", functionName,
                lower = 2, upper = 200, integer = TRUE, expectedLength = 1L)
            data <- expand.grid(estimate = seq(estimateRange[1L], estimateRange[2L], length.out = gridPoints),
                standardError = seq(standardErrorRange[1L], standardErrorRange[2L], length.out = gridPoints))
        }
        posteriorData <- lapply(seq_len(nrow(data)), function(i) {
            posterior <- RBesT::postmix(priorDist, m = data$estimate[i], se = data$standardError[i])
            passes <- vapply(c("go", "stop", "indeterminate1"), function(name) {
                criteria <- x[[name]]
                probabilities <- vapply(seq_along(criteria$quantiles), function(j) {
                    return(RBesT::pmix(posterior, criteria$quantiles[j], lower.tail = criteria$lowerTail[j]))
                }, numeric(1))
                return(all(probabilities > criteria$probabilities))
            }, logical(1))
            category <- c("Go", "Stop", "Indeterminate 1", "Indeterminate 2")[match(TRUE, c(passes, TRUE))]
            bounds <- RBesT::qmix(posterior, c(0.025, 0.975))
            return(data.frame(category = category, lower = bounds[1L], upper = bounds[2L]))
        })
        data <- cbind(data, do.call(rbind, posteriorData))
        if (plotCat == 3L) {
            data$category <- .getQtdCoarseCategory(data$category)
        }
        data$category <- factor(data$category, levels = .getQtdCategories(plotCat))
        if (type == 1L) {
            data$row <- seq_len(nrow(data))
            p <- ggplot2::ggplot(data, ggplot2::aes(.data[["estimate"]], .data[["row"]], color = .data[["category"]])) +
                ggplot2::geom_segment(ggplot2::aes(x = .data[["lower"]], xend = .data[["upper"]], yend = .data[["row"]])) +
                ggplot2::geom_point() + ggplot2::labs(x = "Effect estimate and posterior interval", y = "Observation", color = "Decision")
            p <- .addQtdReferenceLines(p, x)
        } else {
            p <- ggplot2::ggplot(data, ggplot2::aes(.data[["estimate"]], .data[["standardError"]], fill = .data[["category"]])) +
                ggplot2::geom_tile() + ggplot2::labs(x = "Effect estimate", y = "Standard error", fill = "Decision")
        }
    }
    return(.finishQtdPlot(p, x, plotSettings, main, "Posterior probability criteria",
        NA_character_, NA_character_, NULL, "auto", FALSE, TRUE))
}
