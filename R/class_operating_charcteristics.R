## |
## |  *Quantitative Trial Design (QTD)*
## |
## |  This file is part of the R package rpact:
## |  Confirmatory Adaptive Clinical Trial Design and Analysis
## |
## |  Author: Friedrich Pahlke, PhD and Daniel Saban<U+00E9>s Bov<U+00E9>, PhD
## |  Licensed under "GNU Lesser General Public License" version 3
## |  License text can be found here: https://www.r-project.org/Licenses/LGPL-3
## |
## |  RPACT company website: https://www.rpact.com
## |  rpact package website: https://www.rpact.org
## |
## |  Contact us for information about our services: info@rpact.com
## |

#' 
#' OperatingCharcteristics R6 class
#'
#' A lightweight container for operating-characteristics results produced by the
#' QTD helpers. Instances store the originating design, tested sample-size
#' scenarios and the resulting category probabilities used for plotting and
#' reporting.
#'
#' @field design An object (usually of class \code{TrialDesignPlanQuantitative})
#'   describing the decision rule used to generate the results.
#' @field numberOfSubjects Numeric matrix with per-arm sample sizes (one row per
#'   scenario).
#' @field probabilities A data.frame with the computed category probabilities.
#'
#' @return An R6 object of class \code{OperatingCharcteristics}.
#'
#' @examples
#' rule <- getDesignQuantitative(targetValue = 1, lowerReferenceValue = 0)
#' nMatrix <- matrix(c(50, 100), ncol = 2, byrow = TRUE)
#' # oc <- getOperatingCharcteristicsMeans(design = rule, numberOfSubjects = nMatrix)
#'
#' @export
#' 
#' @keywords internal
#' 
OperatingCharcteristics <- R6::R6Class(
    "OperatingCharcteristics",
    inherit = ParameterSet,
    public = list(
        .design = NULL,
        numberOfSubjects = NULL,
        probabilities = NULL,
        initialize = function(
                design,
                numberOfSubjects,
                probabilities = NA_real_
                ) {
            self$.design <- design
            self$numberOfSubjects <- numberOfSubjects
            self$probabilities <- probabilities
            self$.setParameterType("numberOfSubjects", C_PARAM_USER_DEFINED)
            self$.setParameterType("probabilities", C_PARAM_GENERATED)
        }
    )
)

#' 
#' OperatingCharcteristicsMeans R6 class
#'
#' Specialised operating-characteristics container for comparing means
#' (two-group designs). Inherits from \code{OperatingCharcteristics} and adds
#' fields relevant for mean comparisons and plotting.
#'
#' @field normalApproximation Logical. Indicates the use of a normal
#'   approximation for the sampling distribution.
#' @field alternative Numeric vector. Grid of true mean differences used when
#'   computing probabilities.
#' @field stDev Numeric scalar. Assumed common standard deviation used for the
#'   normal approximation.
#'
#' @return An R6 object of class \code{OperatingCharcteristicsMeans}.
#'
#' @examples
#' # See getOperatingCharcteristicsMeans for an end-to-end example.
#'
#' @export
#' 
#' @keywords internal
#' 
OperatingCharcteristicsMeans <- R6::R6Class(
    "OperatingCharcteristicsMeans",
    inherit = OperatingCharcteristics,
    public = list(
        normalApproximation = NULL,
        groups = NULL,
        alternative = NULL,
        stDev = NULL,
        initialize = function(
                ...,
                design,
                numberOfSubjects,
                normalApproximation = TRUE,
                groups = 2L,
                alternative = seq(0, 1, 0.2),
                stDev = 1,
                probabilities = NA_real_
                ) {      
            super$initialize(
                design = design, 
                numberOfSubjects = numberOfSubjects, 
                probabilities = probabilities, ...)
            
            .setValueAndParameterType(self, "normalApproximation", normalApproximation, TRUE)
            .setValueAndParameterType(self, "groups", groups, 2L)
            .setValueAndParameterType(self, "alternative", alternative, seq(0, 1, 0.2))
            .setValueAndParameterType(self, "stDev", stDev, 1)
        },
        show = function(showType = 1, digits = NA_integer_) {
            self$.show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
        },
        .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
            "Method for automatically printing trial operating-characteristics"
            self$.resetCat()
            if (showType == 3) {
                .createSummary(self, digits = digits)$.show(
                    showType = 1,
                    digits = digits, consoleOutputEnabled = consoleOutputEnabled
                )
            } else if (showType == 2) {
                super$.show(showType = showType, digits = digits, consoleOutputEnabled = consoleOutputEnabled)
            } else {
                self$.cat("Parameters and output of ", self$.toString(), ":\n\n",
                    heading = 1,
                    consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showParametersOfOneGroup(self$.getUserDefinedParameters(), "User defined parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showParametersOfOneGroup(self$.getDerivedParameters(), "Derived from user defined parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showParametersOfOneGroup(self$.getDefaultParameters(), "Default parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showParametersOfOneGroup(self$.getGeneratedParameters(), "Output",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)
            }
        },
        .toString = function(startWithUpperCase = FALSE) {
            s <- "operating-characteristics for comparing means"
            return(ifelse(startWithUpperCase, .firstCharacterToUpperCase(s), s))
        },
        .catMarkdownText = function() {
            self$.show()
        }
    )
)

#' 
#' Plot operating characteristics
#'
#' S3 plot method for \code{OperatingCharcteristics} objects. Produces a
#' ggplot2 object visualizing category probabilities over a grid of
#' alternatives and sample-size scenarios. By default the plot shows six
#' categories; use \code{plotCat = 3} to merge them into the three coarse
#' categories (Go/Indeterminate/Stop).
#'
#' @param x An \code{OperatingCharcteristics} or \code{OperatingCharcteristicsMeans}
#'   object as returned by \code{getOperatingCharcteristicsMeans}.
#' @param plotCat Character or numeric; either 6 or 3. When 3, categories are
#'   merged into "Go", "Indeterminate", and "Stop". Default: 3.
#' @param main Character. Plot title. Default: "Operating Characteristics".
#'
#' @return A ggplot2 plot object (invisibly) showing the requested facet grid.
#'
#' @examples
#' # oc <- getOperatingCharcteristicsMeans(...)
#' # plot(oc, plotCat = 3)
#'
#' @export
#' 
plot.OperatingCharcteristics <- function(
        x,
        plotCat = c(3, 6),
        main = "Operating Characteristics"
        ) {
    plotCat <- match.arg(as.character(plotCat), c("3", "6"))

    # Prepare data for plotting.
    df <- x$probabilities
    df$category <- factor(df$category, levels = unique(df$category))

    # Merge categories if needed.
    if (plotCat == "3") {
        df$category <- factor(
            ifelse(df$category %in% c("Go (Fixed)", "Go"), "Go",
                ifelse(df$category %in% c("Indeterminate 1", "Indeterminate 2"), "Indeterminate",
                    ifelse(df$category %in% c("Stop", "Stop (Fixed)"), "Stop", df$category)
                )
            ),
            levels = c("Go", "Indeterminate", "Stop")
        )
        # Aggregate probabilities for the 3 categories.
        df <- aggregate(probability ~ category + nTreatment + nControl + alternative, data = df, FUN = sum)
    }
    df$percentage <- df$probability * 100

    # Merge together the treatment and control sample sizes for plotting.
    df$sampleSize <- paste0("T: ", df$nTreatment, " / C: ", df$nControl)

    # x-axis label should contain more details.
    x_label <- paste0(
        "Expected Value",
        "(sd=", x$stDev, ")\n",
        "LRV=", x$design$lowerReferenceValue,
        ", TV=", x$design$targetValue, "\n",
        "LB=", round(x$design$lowerPercentile * 100), "%, ",
        "UB=", round(x$design$upperPercentile * 100), "%"
    )

    ggplot2::ggplot(df, ggplot2::aes(x = alternative, y = percentage, color = sampleSize, group = sampleSize)) +
        ggplot2::geom_line() +
        ggplot2::facet_wrap(~category, ncol = 2) +
        ggplot2::ylim(c(0, 100)) +
        ggplot2::labs(
            title = main,
            x = x_label,
            y = "Probability of Decision (%)"
        ) +
        ggplot2::geom_vline(
            xintercept = x$design$targetValue,
            linetype = "dotted",
            size = 1,
            show.legend = TRUE
        ) +
        ggplot2::geom_vline(
            xintercept = x$design$lowerReferenceValue,
            linetype = "dashed",
            size = 1,
            show.legend = TRUE
        )
}
