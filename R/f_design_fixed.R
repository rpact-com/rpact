## |
## |  *Fixed design*
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

#' 
#' @title
#' Get Fixed Design
#'
#' @description
#' Create a fixed (single-stage) trial design. This convenience wrapper
#' constructs an object of class \code{TrialDesignFixed} with \code{kMax = 1}.
#'
#' @inheritParams param_alpha
#' @inheritParams param_beta
#' @inheritParams param_sided
#' @inheritParams param_directionUpper
#' @inheritParams param_twoSidedPower
#'
#' @details
#' The fixed design represents a single-stage hypothesis test. The returned
#' object can be used with the standard analysis and plotting helpers in the
#' package. Typical use is to specify the type I error \code{alpha} and the
#' desired type II error \code{beta} (or equivalently power = 1 - beta).
#'
#' @template return_object_trial_design
#' 
#' @family design functions
#' 
#' @examples
#' # Basic fixed design with default alpha and beta
#' design <- getDesignFixed()
#' design
#'
#' # Custom significance and power
#' design2 <- getDesignFixed(alpha = 0.05, beta = 0.1)
#' design2
#'
#' @export
#' 
getDesignFixed <- function(
        alpha = NA_real_,
        beta = NA_real_,
        sided = 1L, # C_SIDED_DEFAULT
        directionUpper = NA,
        twoSidedPower = NA
        ) {
    
    .assertIsValidAlphaAndBeta(alpha = alpha, beta = beta, naAllowed = TRUE)
    .assertIsValidSidedParameter(sided)
    .assertIsSingleLogical(directionUpper, "directionUpper", naAllowed = TRUE)
    .assertIsSingleLogical(twoSidedPower, "twoSidedPower", naAllowed = TRUE)
    
    if (is.na(alpha)) {
        alpha <- 0.025
    }
    if (is.na(beta)) {
        beta <- 0.2
    }
        
    design <- TrialDesignFixed$new(
        kMax = 1L,
        alpha = alpha,
        beta = beta,
        sided = sided,
        directionUpper = directionUpper,
        twoSidedPower = twoSidedPower
    )
    design$.setParameterType("kMax", C_PARAM_NOT_APPLICABLE)
    design$.setParameterType("informationRates", C_PARAM_NOT_APPLICABLE)
    design$.setParameterType("futilityBounds", C_PARAM_NOT_APPLICABLE)
    design$.setParameterType("stages", C_PARAM_NOT_APPLICABLE)
    
    .setValueAndParameterType(design, "alpha", alpha, 0.025)
    .setValueAndParameterType(design, "beta", beta, 0.2)
    .setValueAndParameterType(design, "sided", sided, 1L)
    
    if (sided == 2L) {
        if (is.na(twoSidedPower)) {
            twoSidedPower <- FALSE
        }
        .setValueAndParameterType(design, "twoSidedPower", twoSidedPower, FALSE)
    } else {
        .setValueAndParameterType(design, "twoSidedPower", twoSidedPower, NA)
        design$.setParameterType("twoSidedPower", C_PARAM_NOT_APPLICABLE)
        
        if (is.na(directionUpper)) {
            design$.setParameterType("directionUpper", C_PARAM_NOT_APPLICABLE)
        } else {
            .setValueAndParameterType(design, "directionUpper", directionUpper, C_DIRECTION_UPPER_DEFAULT)
        }
    }
    
    design$criticalValues <- .getOneMinusQNorm(alpha / sided)
    design$.setParameterType("criticalValues", C_PARAM_GENERATED)
    design$stageLevels <- 1 - stats::pnorm(.getCriticalValues(design))
    design$.setParameterType("stageLevels", C_PARAM_GENERATED)
    
    return(design)
}

