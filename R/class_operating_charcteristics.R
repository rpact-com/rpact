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
                self$.cat("Output:\n", heading = 2, consoleOutputEnabled = consoleOutputEnabled)
                .showDataFrame(self$probabilities, self,
                    niceColumnNamesEnabled = TRUE, digits = digits,
                    consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)
            }
        },
        .toString = function(startWithUpperCase = FALSE) {
            s <- "operating-characteristics for comparing means"
            return(ifelse(startWithUpperCase, .firstCharacterToUpperCase(s), s))
        }
    )
)
