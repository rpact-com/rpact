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
#' Create a quantitative fixed-design decision rule
#'
#' getDesignQuantitative constructs a decision-rule object for a fixed (non-adaptive)
#' trial with a quantitative endpoint. The returned object contains the target value
#' (TV), a lower reference value (LRV) and two percentiles that define the
#' decision boundaries used by the graphicalQTD methods.
#'
#' @param targetValue Numeric scalar. The target value (TV) for the endpoint.
#' @param lowerReferenceValue Numeric scalar. The lower reference value (LRV).
#' @param upperPercentile Numeric scalar in (0, 1). The upper percentile used to
#'   define the upper decision boundary. Default is 0.9.
#' @param lowerPercentile Numeric scalar in (0, upperPercentile). The lower
#'   percentile used to define the lower decision boundary. Default is 0.2.
#'
#' @details
#' The function performs basic input validation and returns an object of class
#' \code{TrialDesignPlanQuantitative}. This object is used by helper functions
#' that compute operating characteristics and make decisions for fixed designs
#' with normally approximated endpoints (see \code{getOperatingCharcteristicsMeans}).
#'
#' @return An object of class \code{TrialDesignPlanQuantitative} containing the
#'   supplied design parameters.
#'
#' @examples
#' # Basic usage with default percentiles
#' decisionRule <- getDesignQuantitative(
#'     targetValue = 100,
#'     lowerReferenceValue = 0
#' )
#' decisionRule
#'
#' # Custom percentiles
#' decisionRule2 <- getDesignQuantitative(
#'     targetValue = 5.5,
#'     lowerReferenceValue = 0,
#'     upperPercentile = 0.95,
#'     lowerPercentile = 0.1
#' )
#' decisionRule2
#'
#' @export
#' 
getDesignQuantitative <- function(targetValue, lowerReferenceValue, upperPercentile = 0.9, lowerPercentile = 0.2) {
    .assertIsSingleNumber(targetValue, "targetValue")
    .assertIsSingleNumber(lowerReferenceValue, "lowerReferenceValue")
    .assertIsSingleNumber(upperPercentile, "upperPercentile")
    .assertIsSingleNumber(lowerPercentile, "lowerPercentile")
    .assertIsInOpenInterval(upperPercentile, "upperPercentile", 0, 1)
    .assertIsInOpenInterval(lowerPercentile, "lowerPercentile", 0, upperPercentile)

    TrialDesignPlanQuantitative$new(
        targetValue = targetValue,
        lowerReferenceValue = lowerReferenceValue,
        upperPercentile = upperPercentile,
        lowerPercentile = lowerPercentile
    )
}