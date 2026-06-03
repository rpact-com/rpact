## |
## |  *Quantitative Trial Design (QTD)*
## |
## |  This file is part of the R package rpact:
## |  Confirmatory Adaptive Clinical Trial Design and Analysis
## |
## |  Author: Friedrich Pahlke, PhD and Daniel Sabanés Bové, PhD
## |  Licensed under "GNU Lesser General Public License" version 3
## |  License text can be found here: https://www.r-project.org/Licenses/LGPL-3
## |
## |  RPACT company website: https://www.rpact.com
## |  rpact package website: https://www.rpact.org
## |
## |  Contact us for information about our services: info@rpact.com
## |

#' 
#' TrialDesignPlanQuantitative R6 class
#'
#' An R6 class that represents a decision-rule (design) for a fixed
#' quantitative trial. The object stores the target value (TV), a lower
#' reference value (LRV), and two percentiles that define the decision
#' boundaries used by the QTD operating-characteristics and decision helpers.
#'
#' @field targetValue Numeric scalar. The target value (TV) used by the rule.
#' @field lowerReferenceValue Numeric scalar. The lower reference value (LRV).
#' @field upperPercentile Numeric scalar in (0,1). Upper percentile for the
#'   decision boundary.
#' @field lowerPercentile Numeric scalar in (0, upperPercentile). Lower
#'   percentile for the decision boundary.
#'
#' @return An R6 object of class \code{TrialDesignPlanQuantitative}.
#'
#' @examples
#' # Create a simple decision rule
#' rule <- getDesignQuantitative(targetValue = 2, lowerReferenceValue = 0)
#' rule$show()
#'
#' @export
#' 
TrialDesignPlanQuantitative <- R6::R6Class(
    "TrialDesignPlanQuantitative",
    inherit = rpact:::ParameterSet,
    public = list(
        targetValue = NULL,
        lowerReferenceValue = NULL,
        upperPercentile = NULL,
        lowerPercentile = NULL,
        initialize = function(targetValue, lowerReferenceValue, upperPercentile, lowerPercentile) {
            self$targetValue <- targetValue
            self$lowerReferenceValue <- lowerReferenceValue
            self$upperPercentile <- upperPercentile
            self$lowerPercentile <- lowerPercentile
        },
        show = function() {
            self$.show()
        },
        .show = function(consoleOutputEnabled = FALSE) {
            cat("Decision Rule:\n")
            cat("Target Value:", self$targetValue, "\n")
            cat("Lower Reference Value:", self$lowerReferenceValue, "\n")
            cat("Upper Percentile:", self$upperPercentile, "\n")
            cat("Lower Percentile:", self$lowerPercentile, "\n")
        }
    )
)