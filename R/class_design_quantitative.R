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
#' @keywords internal
#' 
TrialDesignPlanQuantitative <- R6::R6Class(
    "TrialDesignPlanQuantitative",
    inherit = ParameterSet,
    public = list(
        targetValue = NULL,
        lowerReferenceValue = NULL,
        upperPercentile = NULL,
        lowerPercentile = NULL,
        initialize = function(
                ...,
                targetValue = NA_real_, 
                lowerReferenceValue = NA_real_, 
                upperPercentile = 0.9, 
                lowerPercentile = 0.2) {
                
            .setValueAndParameterType(self, "targetValue", targetValue, NA_real_)
            .setValueAndParameterType(self, "lowerReferenceValue", lowerReferenceValue, NA_real_)
            .setValueAndParameterType(self, "upperPercentile", upperPercentile, 0.9)
            .setValueAndParameterType(self, "lowerPercentile", lowerPercentile, 0.2)
        },
        show = function(showType = 1, digits = NA_integer_) {
            self$.show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
        },
        .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
            "Method for automatically printing trial design objects"
            self$.resetCat()
            if (showType == 3) {
                .createSummary(self, digits = digits)$.show(
                    showType = 1,
                    digits = digits, consoleOutputEnabled = consoleOutputEnabled
                )
            } else if (showType == 2) {
                super$.show(showType = showType, digits = digits, consoleOutputEnabled = consoleOutputEnabled)
            } else {
                self$.cat("Design parameters and output of ", self$.toString(), ":\n\n",
                    heading = 1,
                    consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showParametersOfOneGroup(self$.getUserDefinedParameters(), "User defined parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showParametersOfOneGroup(self$.getDefaultParameters(), "Default parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)
            }
        },
        .toString = function(startWithUpperCase = FALSE) {
            s <- "quantitative trial design (decision rule)"
            return(ifelse(startWithUpperCase, .firstCharacterToUpperCase(s), s))
        }
    )
)