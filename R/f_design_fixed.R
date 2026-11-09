## |
## |  rpact: Confirmatory Adaptive Clinical Trial Design and Analysis
## |  This file is part of the rpact R package.
## |
## |  RPACT — Statistical Tools for Drug Development
## |
## |  Authors: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
## |  SPDX-FileCopyrightText: RPACT GmbH, Germany <info@rpact.com>
## |  SPDX-License-Identifier: LGPL-3.0-only
## |
## |  Licensed under the GNU Lesser General Public License, version 3.
## |  License text: https://www.gnu.org/licenses/lgpl-3.0.html
## |
## |  Package documentation: https://www.rpact.org
## |  Resources and customer portal: https://connect.rpact.app
## |  Consulting and services: https://www.rpact.com
## |  Contact: info@rpact.com
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
        twoSidedPower = NA) {
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
        directionUpper <- .warnInCaseOfIgnoredDirectionUpper(directionUpper, sided)
        design$.setParameterType("directionUpper", C_PARAM_NOT_APPLICABLE)
    } else {
        .setValueAndParameterType(design, "twoSidedPower", twoSidedPower, NA)
        design$.setParameterType("twoSidedPower", C_PARAM_NOT_APPLICABLE)

        if (is.na(directionUpper)) {
            design$.setParameterType("directionUpper", C_PARAM_NOT_APPLICABLE)
        } else {
            .setValueAndParameterType(design, "directionUpper", directionUpper, NA)
        }
    }

    criticalValues <- .getOneMinusQNorm(alpha / sided)
    criticalValues <- .applyDirectionOfAlternative(criticalValues, directionUpper,
        type = "negateIfLower", phase = "design"
    )
    design$criticalValues <- criticalValues
    design$.setParameterType("criticalValues", C_PARAM_GENERATED)
    design$stageLevels <- 1 - stats::pnorm(.getCriticalValues(design))
    design$.setParameterType("stageLevels", C_PARAM_GENERATED)

    return(design)
}
