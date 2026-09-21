## |
## |  *Optimal conditional error numerical methods*
## |
## |  This file is part of the R package rpact:
## |  Confirmatory Adaptive Clinical Trial Design and Analysis
## |
## |  Original contribution: Morten Dreher
## |  Licensed under "GNU Lesser General Public License" version 3
## |  License text: https://www.r-project.org/Licenses/LGPL-3
## |

#' @include parameter_descriptions.R
NULL

#' Internal variation of \code{getPsi()} used by \code{findLevelConstant()}
#'
#' @description Calculate psi for the given scenario, incl. constant
#' @details Internal function called by \code{getIntegral()} that calculates a point value of \code{getPsi()}.
#'
#' @inheritParams param_firstStagePValueIntegralOCEF
#' @inheritParams param_constantOCEF
#' @inheritParams param_designOCEF
#'
#' @return Point value of \code{getPsi()}.
#' @noRd

.getInnerPsi <- function(firstStagePValue, constant, design) {
    # If monotonisation constants provided, perform non-increasing transformation
    if (design$enforceMonotonicity) {
        likelihoodRatioOverEffect <- .getMonotoneFunction(
            x = firstStagePValue,
            fun = .getQ,
            argument = "firstStagePValue",
            design = design
        )
    } else {
        likelihoodRatioOverEffect <- .getQ(firstStagePValue = firstStagePValue, design = design)
    }

    # Calculate the value to be supplied to getPsi
    inner <- -exp(constant) / likelihoodRatioOverEffect

    constraintList <- .getOptimalConditionalErrorConstraints(
        design = design,
        firstStagePValue = firstStagePValue
    )

    conditionalErrorConstraintUpper <- constraintList$conditionalErrorConstraintUpper
    conditionalErrorConstraintLower <- constraintList$conditionalErrorConstraintLower
    conditionalPower <- constraintList$conditionalPower

    conditionalErrorWithConstraints <- pmin(
        pmax(.getPsi(
            nuPrime = inner, conditionalPower = conditionalPower,
            lower = conditionalErrorConstraintLower, upper = conditionalErrorConstraintUpper
        ), conditionalErrorConstraintLower),
        conditionalErrorConstraintUpper
    )

    return(conditionalErrorWithConstraints)
}

#' Internal calculation of the integral over psi
#'
#' @description Helper function that integrates over the optimal conditional error function. Used to find appropriate level constant.
#'
#' @details Internal function called by \code{findLevelConstant()} that should be solved (i.e., root should be found).
#'
#' @inheritParams param_constantOCEF
#' @inheritParams param_designOCEF
#'
#' @return Distance of integral over psi to (alpha-alpha1).
#' @noRd

.getIntegral <- function(constant, design) {
    # Introduce an option which allows consistent use of standard integration
    enforceBasicIntegration <- isTRUE(as.logical(getOption("rpact.design.optimal.enforce.basic.integration", FALSE)))

    designHasConstraints <- (design$maximumConditionalError < 1) ||
        (design$minimumConditionalError > 0) ||
        (design$minimumSecondStageInformation > 0) ||
        (design$maximumSecondStageInformation < Inf)

    # In the following cases, use basic integration:
    # - enforced through option (see above)
    # - design has constraints
    # - monotonicity of design is not enforced
    # - design has no monotonisation constants
    # - design uses a conditional power function
    if (
        enforceBasicIntegration ||
            designHasConstraints ||
            !design$enforceMonotonicity ||
            is.null(unlist(design$monotonisationConstants)) ||
            is.function(design$conditionalPowerFunction)
    ) {
        integral <- stats::integrate(
            f = .getInnerPsi,
            lower = design$alpha1,
            upper = design$alpha0,
            constant = constant,
            design = design,
            rel.tol = 1e-8,
            abs.tol = 1e-10,
            subdivisions = 1000L
        )$value
    } else {
        # If monotonisation constants exist, use alternative integration routine better adapted to constant functions
        integral <- .getIntegralWithConstants(
            constant = constant,
            design = design
        )
    }
    return(integral - (design$alpha - design$alpha1))
}

#' Calculate the integral over a partially constant function
#'
#' @description This function aims to provide a more accurate integration routine than the \code{integrate()} function
#' for a partially constant function.
#'
#' @inheritParams param_constantOCEF
#' @inheritParams param_designOCEF
#'
#' @return Integral over the partially constant function.
#' @noRd
#'

.getIntegralWithConstants <- function(constant, design) {
    # Routine is only used for constant conditional power
    conditionalPower <- design$conditionalPower

    # Number of non-decreasing intervals of the optimal conditional error function
    numberOfIntervals <- length(design$monotonisationConstants$qs)

    # Integrate over the first (non-constant) part of the function
    firstPart <- stats::integrate(
        f = .getInnerPsi,
        lower = design$alpha1,
        upper = design$monotonisationConstants$dls[1],
        constant = constant,
        design = design,
        rel.tol = 1e-8,
        abs.tol = 1e-10,
        subdivisions = 1000L
    )$value

    # Integrate over the final (non-constant) part of the function
    lastPart <- stats::integrate(
        f = .getInnerPsi,
        lower = design$monotonisationConstants$dus[numberOfIntervals],
        upper = design$alpha0,
        constant = constant,
        design = design,
        rel.tol = 1e-8,
        abs.tol = 1e-10,
        subdivisions = 1000L
    )$value

    # Calculate integral for all constant parts
    constantParts <- 0
    for (x in seq_len(numberOfIntervals)) {
        newPart <- min(
            max(
                .getPsi(
                    nuPrime = -exp(constant) / design$monotonisationConstants$qs[x],
                    conditionalPower = conditionalPower
                ),
                design$minimumConditionalError
            ),
            design$maximumConditionalError
        ) *
            (design$monotonisationConstants$dus[x] - design$monotonisationConstants$dls[x])
        constantParts <- constantParts + newPart
    }

    # Calculate integral for all remaining non-constant parts
    nonConstantParts <- 0
    if (numberOfIntervals > 1) {
        for (x in 1:(numberOfIntervals - 1)) {
            newPart <- stats::integrate(
                f = .getInnerPsi,
                lower = design$monotonisationConstants$dus[x],
                upper = design$monotonisationConstants$dls[x + 1],
                constant = constant,
                design = design,
                rel.tol = 1e-8,
                abs.tol = 1e-10,
                subdivisions = 1000L
            )$value
            nonConstantParts <- nonConstantParts + newPart
        }
    }

    # Return the sum of all parts
    return(sum(c(firstPart, lastPart, constantParts, nonConstantParts)))
}


.getOptimalConditionalErrorConstraints <- function(design, firstStagePValue) {
    # Constraints on conditional error scale
    conditionalErrorConstraintMaximumConditionalError <- design$maximumConditionalError
    conditionalErrorConstraintMinimumConditionalError <- design$minimumConditionalError

    # Constraints on information scale
    conditionalErrorConstraintMinimumInformation <- 1
    conditionalErrorConstraintMaximumInformation <- 0

    conditionalPower <- NULL

    # Check if conditional power function should be used
    if (is.function(design$conditionalPowerFunction)) {
        conditionalPower <- .getOptimalConditionalPower(firstStagePValue, design)

        # Check if interim estimate is used
        if (design$useInterimEstimate) {
            delta1 <- pmin(
                pmax(stats::qnorm(firstStagePValue, lower.tail = FALSE) / sqrt(design$firstStageInformation), design$delta1Min),
                design$delta1Max
            )
        } else {
            # Otherwise use fixed effect
            delta1 <- design$delta1
        }
        # Calculate constraint based on minimumSecondStageInformation
        if (design$minimumSecondStageInformation > 0) {
            conditionalErrorConstraintMinimumInformation <- 1 -
                stats::pnorm(delta1 * sqrt(design$minimumSecondStageInformation) - stats::qnorm(conditionalPower))
        }

        # Calculate constraint based on maximumSecondStageInformation
        if (design$maximumSecondStageInformation < Inf) {
            conditionalErrorConstraintMaximumInformation <- 1 -
                stats::pnorm(delta1 * sqrt(design$maximumSecondStageInformation) - stats::qnorm(conditionalPower))
        }
    } else {
        conditionalPower <- design$conditionalPower

        # Check if interim estimate is used
        if (design$useInterimEstimate) {
            delta1MaximumAchieved <- min(
                qnorm(1 - design$alpha1) / sqrt(design$firstStageInformation),
                design$delta1Max
            )
            delta1MinimumAchieved <- max(
                qnorm(1 - design$alpha0) / sqrt(design$firstStageInformation),
                design$delta1Min
            )
        } else {
            # Otherwise use fixed effect
            delta1MaximumAchieved <- design$delta1
            delta1MinimumAchieved <- design$delta1
        }

        # Calculate constraint based on minimumSecondStageInformation
        if (design$minimumSecondStageInformation > 0) {
            conditionalErrorConstraintMinimumInformation <- 1 -
                pnorm(
                    delta1MaximumAchieved * sqrt(design$minimumSecondStageInformation) - stats::qnorm(conditionalPower)
                )
        }

        # Calculate constraint based on maximumSecondStageInformation
        if (design$maximumSecondStageInformation < Inf) {
            conditionalErrorConstraintMaximumInformation <- 1 -
                pnorm(
                    delta1MinimumAchieved * sqrt(design$maximumSecondStageInformation) - stats::qnorm(conditionalPower)
                )
        }
    }

    # Use the constraint that is the stronger restriction
    conditionalErrorConstraintUpper <- pmin(
        conditionalErrorConstraintMinimumInformation,
        conditionalErrorConstraintMaximumConditionalError
    )
    conditionalErrorConstraintLower <- pmax(
        conditionalErrorConstraintMaximumInformation,
        conditionalErrorConstraintMinimumConditionalError
    )

    if (any(conditionalErrorConstraintLower > pmin(conditionalErrorConstraintUpper, conditionalPower))) {
        stopConflictingArguments(
            "Conditional error and second-stage information constraints are incompatible.",
            parameter = c("minimumConditionalError", "maximumSecondStageInformation"),
            value = list(minimumConditionalError = design$minimumConditionalError, maximumSecondStageInformation = design$maximumSecondStageInformation),
            constraint = "effective lower conditional error bound must not exceed the upper bound or conditional power",
            relatedParameter = c("maximumConditionalError", "minimumSecondStageInformation", "conditionalPower", "firstStagePValue"),
            relatedValue = list(maximumConditionalError = design$maximumConditionalError, minimumSecondStageInformation = design$minimumSecondStageInformation, conditionalPower = conditionalPower, firstStagePValue = firstStagePValue),
            functionName = ".getOptimalConditionalErrorConstraints"
        )
    }

    constraintList <- list(
        "conditionalErrorConstraintUpper" = conditionalErrorConstraintUpper,
        "conditionalErrorConstraintLower" = conditionalErrorConstraintLower,
        "conditionalPower" = conditionalPower
    )

    return(constraintList)
}

#' Get Level Constant for Optimal Conditional Error Function
#'
#' @description Find the constant required such that the conditional error function meets the overall level condition.
#'
#' @details The level condition is defined as:
#' \deqn{\alpha = \alpha_1 + \int_{\alpha_1}^{\alpha_0} \alpha_2(p_1)dp_1.}
#' The constant \eqn{c_0} of the optimal conditional error function is calibrated such that it meets the level condition.
#' For a valid design, the additional following condition must be met to be able to exhaust the level \eqn{\alpha}:
#' \deqn{\alpha_1 + CP(\alpha_0-\alpha_1)>\alpha.}
#' This condition is checked by \code{getLevelConstant()} and the execution is terminated if it is not met. \cr
#' In case a conditional power function is used, the condition is instead:
#' \deqn{\alpha_1 + \int_{\alpha_1}^{\alpha_0} CP(p_1)dp_1>\alpha.}
#'
#' @inheritParams param_designOCEF
#'
#' @return A list that contains the constant (element \code{$root}) and other components provided by \code{uniroot()}.
#' The level constant is calculated corresponding to the mean difference scale.
#'
#' @noRd
#'
#'
#' @template reference_optimal
#' @template reference_monotone

.getLevelConstant <- function(design) {
    # Check basic condition for decision rules
    # Fixed conditional power
    if (!is.na(design$conditionalPower)) {
        if (design$alpha1 + design$conditionalPower * (design$alpha0 - design$alpha1) <= design$alpha) {
            stopConflictingArguments(
                "(alpha1 + conditionalPower*(alpha0-alpha1)) must exceed alpha, otherwise no level constant fully exhausting alpha can be found.",
                parameter = "conditionalPower",
                value = design$conditionalPower,
                constraint = "alpha1 + conditionalPower * (alpha0 - alpha1) > alpha",
                relatedParameter = c("alpha", "alpha1", "alpha0"),
                relatedValue = c(alpha = design$alpha, alpha1 = design$alpha1, alpha0 = design$alpha0),
                functionName = ".getLevelConstant"
            )
        }
    } else if (is.function(design$conditionalPowerFunction)) {
        # Conditional power function
        if (
            stats::integrate(f = .getOptimalConditionalPower, lower = design$alpha1, upper = design$alpha0, design = design)$value <=
                design$alpha - design$alpha1
        ) {
            stopConflictingArguments(
                "Integral over conditional power function from alpha1 to alpha0 must exceed (alpha-alpha1), otherwise no level constant fully exhausting alpha can be found.",
                parameter = "conditionalPowerFunction",
                value = design$conditionalPowerFunction,
                constraint = "integral of conditionalPowerFunction over (alpha1, alpha0) > alpha - alpha1",
                relatedParameter = c("alpha", "alpha1", "alpha0"),
                relatedValue = c(alpha = design$alpha, alpha1 = design$alpha1, alpha0 = design$alpha0),
                functionName = ".getLevelConstant"
            )
        }
    } else {
        # Unexpected issue
        stopRuntimeIssue(
            "Unexpected error: both conditionalPower and conditionalPowerFunction are specified inappropriately.",
            parameter = "conditionalPower",
            value = design$conditionalPower,
            constraint = "design must specify conditional power or a valid callback",
            relatedParameter = "conditionalPowerFunction",
            relatedValue = design$conditionalPowerFunction,
            functionName = ".getLevelConstant"
        )
    }

    # Feasibility depends on the integrated bounds, not just their pointwise order.
    if (design$minimumConditionalError > 0 || design$maximumConditionalError < 1 ||
            design$minimumSecondStageInformation > 0 || design$maximumSecondStageInformation < Inf) {
        integrateBound <- function(upper) {
            stats::integrate(function(pValue) {
                constraints <- .getOptimalConditionalErrorConstraints(design, pValue)
                if (upper) {
                    return(rep_len(
                        pmin(constraints$conditionalErrorConstraintUpper, constraints$conditionalPower),
                        length(pValue)
                    ))
                }
                rep_len(constraints$conditionalErrorConstraintLower, length(pValue))
            }, lower = design$alpha1, upper = design$alpha0,
                rel.tol = 1e-8, abs.tol = 1e-10, subdivisions = 1000L
            )$value + design$alpha1
        }
        if (integrateBound(TRUE) < design$alpha - 1e-10) {
            stopConflictingArguments(
                "The upper conditional error constraint (maximumConditionalError or minimumSecondStageInformation) is too strict to attain alpha.",
                parameter = c("maximumConditionalError", "minimumSecondStageInformation"),
                value = list(maximumConditionalError = design$maximumConditionalError, minimumSecondStageInformation = design$minimumSecondStageInformation),
                constraint = "integrated upper bound + alpha1 >= alpha",
                relatedParameter = c("alpha", "alpha1", "alpha0"),
                relatedValue = c(alpha = design$alpha, alpha1 = design$alpha1, alpha0 = design$alpha0),
                functionName = ".getLevelConstant"
            )
        }
        if (integrateBound(FALSE) > design$alpha + 1e-10) {
            stopConflictingArguments(
                "The lower conditional error constraint (minimumConditionalError or maximumSecondStageInformation) is too strict to attain alpha.",
                parameter = c("minimumConditionalError", "maximumSecondStageInformation"),
                value = list(minimumConditionalError = design$minimumConditionalError, maximumSecondStageInformation = design$maximumSecondStageInformation),
                constraint = "integrated lower bound + alpha1 <= alpha",
                relatedParameter = c("alpha", "alpha1", "alpha0"),
                relatedValue = c(alpha = design$alpha, alpha1 = design$alpha1, alpha0 = design$alpha0),
                functionName = ".getLevelConstant"
            )
        }
    }

    # Find the level constant.
    # Expects an error if specified non-centrality parameter is very large or very small
    withCallingHandlers(
        expr = {
            stats::uniroot(
                f = .getIntegral,
                lower = design$levelConstantMinimum,
                upper = design$levelConstantMaximum,
                design = design,
                tol = 1e-16
            )
        },
        error = function(e) {
            # This specific error may occur if the given non-centrality parameter is too small or too large or if the
            # provided constraints are not suitable and is handled separately
            if (e$message == "f() values at end points not of opposite sign") {
                stopRuntimeIssue(
                    "Root finding for level constant failed. Try changing the search interval via arguments levelConstantMinimum and levelConstantMaximum. \n Alternatively, the constraints on the optimal conditional error function or second-stage information may not be appropriate.",
                    parameter = c("levelConstantMinimum", "levelConstantMaximum"),
                    value = c(design$levelConstantMinimum, design$levelConstantMaximum),
                    constraint = "search interval must bracket a root of the level equation",
                    relatedParameter = "alpha",
                    relatedValue = design$alpha,
                    functionName = ".getLevelConstant"
                )
            }
        }
    )
}

#' Calculate Likelihood Ratio
#'
#'
#' @description Calculate the likelihood ratio of a p-value for a given distribution.
#'
#' @details The calculation of the likelihood ratio for a first-stage p-value \eqn{p_1} is done based on a distributional assumption, specified in the \code{design} object.
#' The different options require different parameters, elaborated in the following.
#' \itemize{
#'    \item \code{likelihoodRatioDistribution="fixed"}: calculates the likelihood ratio for a fixed \eqn{\Delta}. The non-centrality parameter of the likelihood ratio \eqn{\vartheta} is then computed as \code{deltaLR}*\code{sqrt(firstStageInformation)} and the likelihood ratio is calculated as:
#'          \deqn{l(p_1) = e^{\Phi^{-1}(1-p_1)\vartheta - \vartheta^2/2}.} \code{deltaLR} may also contain multiple elements, in which case a weighted likelihood ratio is calculated for the given values. Unless positive weights that sum to 1 are provided by the argument \code{weightsDeltaLR}, equal weights are assumed.
#'    \item \code{likelihoodRatioDistribution="normal"}: calculates the likelihood ratio for a normally distributed prior of \eqn{\vartheta} with mean \code{deltaLR}*\code{sqrt(firstStageInformation)} (\eqn{\mu}) and standard deviation \code{tauLR}*\code{sqrt(firstStageInformation)} (\eqn{\sigma}). The parameters \code{deltaLR} and \code{tauLR} must be specified on the mean difference scale.
#'          \deqn{l(p_1) = (1+\sigma^2)^{-\frac{1}{2}}\cdot e^{-(\mu/\sigma)^2/2 + (\sigma\Phi^{-1}(1-p_1) + \mu/\sigma)^2 / (2\cdot (1+\sigma^2))}}
#'    \item \code{likelihoodRatioDistribution="exp"}: calculates the likelihood ratio for an exponentially distributed prior of \eqn{\vartheta} with rate \code{kappaLR}*\code{sqrt(firstStageInformation)} (\eqn{\eta}). The likelihood ratio is then calculated as:
#'          \deqn{l(p_1) = \eta \cdot \sqrt{2\pi} \cdot e^{(\Phi^{-1}(1-p_1)-\eta)^2/2} \cdot \Phi(\Phi^{-1}(1-p_1)-\eta)}
#'    \item \code{likelihoodRatioDistribution="unif"}: calculates the likelihood ratio for a uniformly distributed prior of \eqn{\vartheta} on the support \eqn{[0, \Delta\cdot\sqrt{I_1}]}, where \eqn{\Delta} is specified as \code{deltaMaxLR} and \eqn{I_1} is the \code{firstStageInformation}.
#'          \deqn{l(p_1) = \frac{\sqrt{2\pi}}{\Delta\cdot\sqrt{I_1}} \cdot e^{\Phi^{-1}(1-p_1)^2/2} \cdot (\Phi(\Delta\cdot\sqrt{I_1} - \Phi^{-1}(1-p_1))-p_1)}
#'    \item \code{likelihoodRatioDistribution="maxlr"}: the non-centrality parameter \eqn{\vartheta} is estimated from the data and no additional parameters must be specified. The likelihood ratio is estimated from the data as:
#'          \deqn{l(p_1) = e^{max(0, \Phi^{-1}(1-p_1))^2/2}}
#'          The maximum likelihood ratio is always restricted to effect sizes \eqn{\vartheta \geq 0} (corresponding to \eqn{p_1 \leq 0.5}).
#' }
#'
#' @inheritParams param_firstStagePValueOCEF
#' @inheritParams param_designOCEF
#'
#' @return The value of the likelihood ratio for the given specification.
#' @noRd
#'
#' @template reference_optimal
#' @references Hung, H. M. J., O<U+2019>Neill, R. T., Bauer, P. & Kohne, K. (1997). The behavior of the p-value when the alternative hypothesis is true. Biometrics. http://www.jstor.org/stable/2533093

.getLikelihoodRatio <- function(firstStagePValue, design) {
    # Initialise likelihood ratio
    likelihoodRatio <- NA
    # Fixed effect case
    if (design$likelihoodRatioDistribution == "fixed") {
        # Get non-centrality parameter and weights

        # Ensure that ncp argument is provided
        # This is a fallback protection, as the design function should already ensure this
        if (is.null(design$deltaLR)) {
            stopMissingArgument(
                "Argument deltaLR required for fixed likelihood case, but not found in design object.",
                parameter = "deltaLR",
                value = design$deltaLR,
                constraint = "required for the selected likelihood ratio distribution",
                relatedParameter = "likelihoodRatioDistribution",
                relatedValue = design$likelihoodRatioDistribution,
                functionName = ".getLikelihoodRatio"
            )
        }

        nonCentralityParameter <- design$deltaLR * sqrt(design$firstStageInformation)
        weights <- design$weightsDeltaLR

        # If weights argument was not specified, automatically use equal weights
        if (is.null(weights)) {
            weights <- rep(1 / length(nonCentralityParameter), length(nonCentralityParameter))
        }

        # Ensure that weights are positive and sum up to 1
        # This is a fallback protection, as the design function should already ensure this
        if (length(weights) != length(nonCentralityParameter) || any(!is.finite(weights)) ||
            abs(sum(weights) - 1) > sqrt(.Machine$double.eps) || any(weights < 0)) {
            stopIllegalArgument(
                "weightsDeltaLR must be finite, nonnegative, have one entry per deltaLR and sum up to 1",
                parameter = "weightsDeltaLR",
                value = weights,
                constraint = "finite nonnegative weights, one per deltaLR entry, summing to one",
                relatedParameter = "deltaLR",
                relatedValue = design$deltaLR,
                functionName = ".getLikelihoodRatio"
            )
        }

        if (firstStagePValue %in% c(0, 1)) {
            direction <- if (firstStagePValue == 0) 1 else -1
            if (any(direction * nonCentralityParameter[weights > 0] > 0)) {
                return(Inf)
            }
            return(sum(weights[nonCentralityParameter == 0]))
        }
        # Calculate likelihood ratio
        likelihoodRatio <- exp(
            stats::qnorm(firstStagePValue, lower.tail = FALSE) * nonCentralityParameter - nonCentralityParameter^2 / 2
        ) %*%
            weights
    } else if (design$likelihoodRatioDistribution == "normal") {
        # Normal prior
        # Get ncp and tau

        # Ensure that arguments were specified
        # This is a fallback protection, as the design function should already ensure this
        if (is.null(design$deltaLR) || is.null(design$tauLR)) {
            stopMissingArgument(
                "Arguments deltaLR and tauLR required for normal likelihood case, but not found in design object.",
                parameter = c("deltaLR", "tauLR"),
                value = list(deltaLR = design$deltaLR, tauLR = design$tauLR),
                constraint = "required for the selected likelihood ratio distribution",
                relatedParameter = "likelihoodRatioDistribution",
                relatedValue = design$likelihoodRatioDistribution,
                functionName = ".getLikelihoodRatio"
            )
        }

        nonCentralityParameter <- design$deltaLR * sqrt(design$firstStageInformation)
        tau <- design$tauLR * sqrt(design$firstStageInformation)

        if (firstStagePValue %in% c(0, 1)) {
            return(Inf)
        }
        # Calculate likelihood ratio
        likelihoodRatio <- (1 / sqrt(1 + tau^2)) *
            exp(
                -(nonCentralityParameter / tau)^2 /
                    2 +
                    (tau * stats::qnorm(firstStagePValue, lower.tail = FALSE) + (nonCentralityParameter / tau))^2 / (2 * (1 + tau^2))
            )
    } else if (design$likelihoodRatioDistribution == "exp") {
        # Exponential prior
        # Get kap0

        # Ensure that argument was specified
        # This is a fallback protection, as the design function should already ensure this
        if (is.null(design$kappaLR)) {
            stopMissingArgument(
                "Argument kappaLR required for exponential likelihood case, but not found in design object.",
                parameter = "kappaLR",
                value = design$kappaLR,
                constraint = "required for the selected likelihood ratio distribution",
                relatedParameter = "likelihoodRatioDistribution",
                relatedValue = design$likelihoodRatioDistribution,
                functionName = ".getLikelihoodRatio"
            )
        }

        nonCentralityParameter <- design$kappaLR * sqrt(design$firstStageInformation)

        if (firstStagePValue == 0) {
            return(Inf)
        }
        if (firstStagePValue == 1) {
            return(0)
        }
        # Evaluate the normal tail on the log scale to avoid Inf * 0.
        z <- stats::qnorm(firstStagePValue, lower.tail = FALSE) - nonCentralityParameter
        likelihoodRatio <- exp(log(nonCentralityParameter) + log(2 * pi) / 2 +
            z^2 / 2 + stats::pnorm(z, log.p = TRUE))
    } else if (design$likelihoodRatioDistribution == "unif") {
        # Uniform prior
        # Get delMax

        # Ensure that argument was specified
        # This is a fallback protection, as the design function should already ensure this
        if (is.null(design$deltaMaxLR)) {
            stopMissingArgument(
                "Argument deltaMaxLR required for uniform likelihood case, but not found in design object.",
                parameter = "deltaMaxLR",
                value = design$deltaMaxLR,
                constraint = "required for the selected likelihood ratio distribution",
                relatedParameter = "likelihoodRatioDistribution",
                relatedValue = design$likelihoodRatioDistribution,
                functionName = ".getLikelihoodRatio"
            )
        }

        nonCentralityParameter <- design$deltaMaxLR * sqrt(design$firstStageInformation)

        if (firstStagePValue == 0) {
            return(Inf)
        }
        if (firstStagePValue == 1) {
            return(0)
        }
        z <- stats::qnorm(firstStagePValue, lower.tail = FALSE)
        # Choose the normal tail that avoids subtracting values close to one.
        if (z <= nonCentralityParameter / 2) {
            logUpper <- stats::pnorm(z, log.p = TRUE)
            logLower <- stats::pnorm(z - nonCentralityParameter, log.p = TRUE)
        } else {
            logUpper <- stats::pnorm(nonCentralityParameter - z, log.p = TRUE)
            logLower <- stats::pnorm(-z, log.p = TRUE)
        }
        logDifference <- logUpper + log(-expm1(logLower - logUpper))
        likelihoodRatio <- exp(log(2 * pi) / 2 + z^2 / 2 + logDifference - log(nonCentralityParameter))
    } else if (design$likelihoodRatioDistribution == "maxlr") {
        # Maximum likelihood ratio case
        # Calculate likelihood ratio
        likelihoodRatio <- exp(max(0, stats::qnorm(firstStagePValue, lower.tail = FALSE))^2 / 2)
    } else {
        stopIllegalArgument(
            "Distribution not matched.",
            parameter = "likelihoodRatioDistribution",
            value = design$likelihoodRatioDistribution,
            constraint = "one of fixed, normal, exp, unif or maxlr",
            functionName = ".getLikelihoodRatio"
        )
    }

    # Return likelihood ratio
    return(unname(likelihoodRatio))
}

.getLikelihoodRatio <- Vectorize(.getLikelihoodRatio, "firstStagePValue")

#' Return Monotone Function Values
#'
#' @description Applies the provided monotonisation constants to a specified, possibly non-monotone function. The returned function values are non-increasing.
#'
#' @details The exact monotonisation process is outlined in Brannath et al. (2024), but specified in terms of the first-stage test statistic \eqn{z_1} rather than the first-stage p-value \eqn{p_1}. \cr
#' The algorithm can easily be translated to the use of p-values by switching the maximum and minimum functions, i.e., replacing \eqn{\min\{q, Q(z_1)\}} by \eqn{\max\{q, Q(p_1)\}} and \eqn{\min\{q, Q(z_1)\}} by \eqn{\max\{q, Q(p_1\}}.
#'
#' @param x Argument values.
#' @inheritParams param_funOCEF
#' @inheritParams param_lowerOCEF
#' @inheritParams param_upperOCEF
#' @inheritParams param_argumentOCEF
#' @inheritParams param_nStepsOCEF
#' @inheritParams param_epsilonOCEF
#' @inheritParams param_numberOfIterationsQOCEF
#' @inheritParams param_designOCEF
#'
#' @return Monotone function values.
#' @noRd
#'
#'
#' @template reference_monotone

.getMonotoneFunction <- function(
    x,
    fun,
    lower = NULL,
    upper = NULL,
    argument = NULL,
    nSteps = 10^4,
    epsilon = 10^(-5),
    numberOfIterationsQ = 10^4,
    design
) {
    # If monotonisation is enforced, extract constants
    if (design$enforceMonotonicity) {
        out <- design$monotonisationConstants
    } else {
        out <- list()
    }

    # If the length of object out is 0, the function is already non-increasing
    if (length(out) == 0) {
        modFunctionValues <- fun(x, design = design)
    } else {
        # If the length of out is larger than 0, a transformation is necessary
        positionLower <- apply(outer(x, out$dls, ">="), 1, sum)
        positionUpper <- apply(outer(x, out$dus, ">"), 1, sum) + 1
        modFunctionValues <- ifelse(
            positionLower == positionUpper,
            yes = out$qs[pmax(1, positionLower)],
            no = fun(x, design = design)
        )
    }
    return(modFunctionValues)
}

#' Calculate the Constants for Monotonisation
#'
#' @description Computes the constants required to make a function non-increasing on the specified interval. The output of this function is necessary to calculate the monotone optimal conditional error function.
#' The output object is a list that contains the intervals on which constant values are required, specified by the minimum \code{dls} and maximum \code{dus} of the interval and the respective constants, \code{qs}.
#'
#' @inheritParams param_funOCEF
#' @inheritParams param_lowerOCEF
#' @inheritParams param_upperOCEF
#' @inheritParams param_argumentOCEF
#' @inheritParams param_nStepsOCEF
#' @inheritParams param_epsilonOCEF
#' @inheritParams param_numberOfIterationsQOCEF
#' @inheritParams param_designOCEF
#'
#' @return A list containing the monotonisation constants (element \code{qs}) and the intervals on which they must be applied, specified via minimum (element \code{dls}) and maximum (element \code{dus}).
#' @noRd
#'
#' @template reference_monotone

.getMonotonisationConstants <- function(
    fun,
    lower = 0,
    upper = 1,
    argument,
    nSteps = 10^4,
    epsilon = 10^(-5),
    numberOfIterationsQ = 10^4,
    design
) {
    # Sequence of argument values
    argumentValues <- lower + (upper - lower) * seq_len(nSteps) / (nSteps + 1)

    # Create a list of arguments that fun requires
    argumentList <- list(argumentValues, design)
    names(argumentList) <- c(argument, "design")

    # Call fun with the specified arguments
    functionValues <- do.call(what = fun, args = argumentList)

    # Get min and max function value
    minFunctionValue <- min(functionValues)
    maxFunctionValue <- max(functionValues)

    # Append max and min values
    functionValues <- c(maxFunctionValue, functionValues, minFunctionValue)

    # Helper variable that saves the number of function values
    m <- length(functionValues)

    # Calculate initial "integral"
    initialIntegral <- (sum(functionValues) - 0.5 * (minFunctionValue + maxFunctionValue)) / m

    # Changes in function values
    derivativeFunctionValues <- functionValues[-1] - functionValues[-m]

    output <- NULL

    # Check if function is increasing anywhere
    if (max(derivativeFunctionValues) > 0) {
        # Vector for the constants
        qs <- NULL

        for (i in seq_len(m)) {
            # Recalculate min and max value
            minFunctionValue <- min(functionValues)
            maxFunctionValue <- max(functionValues)

            # Determine derivatives of function values
            derivativeFunctionValues <- c(0, functionValues[-1] - functionValues[-m])

            # Additional check to potentially save runtime: function already non-increasing?
            if (max(derivativeFunctionValues) <= 0) {
                break
            }

            # Index of lower boundary of first interval
            dl1Position <- min(c(sum(cummin(derivativeFunctionValues <= 0)) + 1, m))

            # Index of upper boundary of first interval
            if (dl1Position < length(functionValues)) {
                duPosition <- dl1Position + sum(cummin(derivativeFunctionValues[dl1Position:(m - 2)] > 0))
            } else {
                duPosition <- dl1Position
            }

            # Index of lower boundary of second interval
            if (duPosition < length(functionValues)) {
                dl2Position <- duPosition + sum(cummin(derivativeFunctionValues[duPosition:(m - 1)] <= 0))
            } else {
                dl2Position <- duPosition
            }

            # Repeat until integrals are similar enough or maximum number of iterations reached
            for (j in seq_len(numberOfIterationsQ)) {
                # Initial guess for q
                q <- (minFunctionValue + maxFunctionValue) / 2

                # Temporal modification of functionValues with the current guess for q
                modFunctionValues <- pmax(q, functionValues[1:dl1Position])
                modFunctionValues <- c(modFunctionValues, rep(q, duPosition + 1 - dl1Position))

                if (duPosition < (m - 1)) {
                    modFunctionValues <- c(modFunctionValues, pmin(q, functionValues[(duPosition + 2):dl2Position]))
                }
                if (dl2Position < m) {
                    modFunctionValues <- c(modFunctionValues, functionValues[(dl2Position + 1):m])
                }

                newIntegral <- (sum(modFunctionValues) - 0.5 * (max(modFunctionValues) + min(modFunctionValues))) / m

                # If difference between integrals is small enough, stop current iteration
                if (abs(newIntegral - initialIntegral) < epsilon) {
                    break
                }

                # If difference between integrals is not small enough yet, update the maximal or minimal values
                if (newIntegral > initialIntegral) {
                    maxFunctionValue <- q
                } else {
                    minFunctionValue <- q
                }
            }

            # No constants q so far: add
            if (is.null(qs)) {
                qs <- c(qs, q)
            } else {
                # If some constants already exist: if the new constant is larger than the last constant, it must replace the last constant.
                # This ensures that the constants are also non-increasing
                if (q >= qs[length(qs)]) {
                    qs[length(qs)] <- q
                } else {
                    qs <- c(qs, q)
                }
            }
            # No more increasing interval -> Finished
            if (dl2Position == m - 1) {
                break
            } else {
                functionValues <- modFunctionValues
            }
        }
        dls <- NULL
        dus <- NULL
        argumentValues <- c(lower, argumentValues, upper)

        # Find min and max of the intervals for each q
        for (q in qs) {
            dls <- c(dls, argumentValues[sum(cummin(modFunctionValues[2:(m - 1)] > q)) + 1])
            dus <- c(dus, argumentValues[sum(cummin(modFunctionValues[2:(m - 1)] >= q)) + 1])
        }

        output <- list(qs = qs, dls = dls, dus = dus, integral = newIntegral)
    }

    # Special case: function is increasing at the last element -> set last du to upper
    if (length(output$dus) > 0) {
        if (output$dus[length(output$dus)] >= argumentValues[m - 1]) {
            output$dus[length(output$dus)] <- upper
        }
    }

    # If there are no entries, no monotonisation is required
    if (is.null(unlist(output))) {
        output <- list()
    } else {
        if (!design$enforceMonotonicity) {
            warnNumericalIssue(
                "Monotonisation is required. Set enforceMonotonicity to TRUE in design object for strict type I error control.",
                userInstructions = paste0(
                    "Set enforceMonotonicity = TRUE in the design object for strict Type I error control."
                )
            )
        }
    }

    return(output)
}

#' Calculate Nu
#'
#' @description Calculate the factor which relates \eqn{\alpha_2} to the second-stage information for given conditional power.
#'
#' @details Note that this function uses factor 1 instead of factor 2 (Brannath & Bauer 2004). This has no impact on the optimal conditional error function, as constant factors are absorbed by the level constant \eqn{c_0}. \cr
#' The calculation is:
#' \deqn{\nu(\alpha_2(p_1)) = (\Phi^{-1}(1-\alpha_2(p_1)) + \Phi^{-1}(CP))^2.}
#' @inheritParams param_alphaGenericOCEF
#' @inheritParams param_conditionalPowerOCEF
#'
#' @return Factor linking information and \eqn{\alpha_2}.
#' @noRd
#'
#' @examples
#' .getNu(alpha = 0.05, conditionalPower = 0.9)
#'
#' # Returns 0 if alpha exceeds conditionalPower
#' .getNu(alpha = 0.8, conditionalPower = 0.7)
#'
#' @template reference_optimal

.getNu <- function(alpha, conditionalPower) {
    nu <- 0
    if (any(alpha > conditionalPower)) {
        warnInvalidInput("alpha/conditional error should not exceed conditionalPower. Information is otherwise 0",
            userInstructions = paste0(
                "Choose conditionalPower above alpha or the applicable conditional error if positive ",
                "information is required."
            )
        )
    } else {
        nu <- (stats::qnorm(alpha, lower.tail = FALSE) + stats::qnorm(conditionalPower))^2
    }
    return(nu)
}

.getNu <- Vectorize(FUN = .getNu, vectorize.args = c("alpha", "conditionalPower"))

#' Calculate the Derivative of Nu
#'
#' @description Calculates the derivative of nu for a given conditional error and conditional power.
#'
#' @details The function \eqn{\nu'} is defined as
#' \deqn{\nu'(p_1) = -2 \cdot (\Phi^{-1}(1-\alpha_2(p_1)) + \Phi^{-1}(CP))/\phi(\Phi^{-1}(1-\alpha_2(p_1))).}
#' Note that in this implementation, the factor -2 is used instead of -4, which is used in by Brannath & Bauer (2004), who explicitly investigate the setting of a balanced two-group trial.
#' The argument \code{conditionalPower} is either the fixed target conditional power or the value of the conditional power function at the corresponding first-stage p-value.
#'
#' @inheritParams param_alphaGenericOCEF
#' @inheritParams param_conditionalPowerOCEF
#'
#' @return Value for nu prime.
#' @noRd
#'
#' @examples
#' .getNuPrime(alpha = 0.05, conditionalPower = 0.9)
#'
#' @template reference_optimal
#'
.getNuPrime <- function(alpha, conditionalPower) {
    nuPrime <- -2 * (stats::qnorm(alpha, lower.tail = FALSE) + stats::qnorm(conditionalPower)) / stats::dnorm(stats::qnorm(alpha, lower.tail = FALSE))
    return(nuPrime)
}

.getNuPrime <- Vectorize(FUN = .getNuPrime, vectorize.args = "alpha")

#' Calculate Psi, the Inverse of Nu Prime
#'
#' @description Get point-wise values of psi (inverse of nu prime)
#'
#' @param nuPrime The function value to be inverted.
#' @param lower,upper Effective conditional error bounds. Competing minima are
#' compared after applying these bounds; the upper bound is capped at conditional power.
#' @inheritParams param_conditionalPowerOCEF
#'
#' @return The value of alpha which corresponds to nuPrime and lies between 0 and \code{conditionalPower}.
#' @noRd
#'
#' @details
#' The function \eqn{\psi} is the inverse of:
#' \deqn{\nu'(\alpha) = -2 \cdot(\Phi^{-1}(1-\alpha) + \Phi^{-1}(CP)) / \phi(\Phi^{-1}(1-\alpha))}.
#' If the conditional power \eqn{CP} lies outside of the range \eqn{1-\Phi(2) \leq CP \leq \Phi(2)}, the calculation is slightly more complicated.
#' The argument \code{conditionalPower} is either the fixed target conditional power or the value of the conditional power function at the corresponding first-stage p-value.
#'
#' @examples
#' # Returns 0.05
#' .getPsi(.getNuPrime(alpha = 0.05, conditionalPower = 0.9), conditionalPower = 0.9)
.getPsi <- function(nuPrime, conditionalPower, lower = 0, upper = conditionalPower) {
    upper <- min(upper, conditionalPower)
    constrain <- function(alpha) max(lower, min(upper, alpha))
    if (lower == upper) {
        return(lower)
    }
    if (identical(as.numeric(nuPrime), -Inf)) {
        return(lower)
    }
    if (nuPrime == 0) {
        return(upper)
    }
    # If the conditional power is between 1-pnorm(2) and pnorm(2) nu prime is monotone and we can build the inverse directly
    if ((stats::pnorm(-2) <= conditionalPower && conditionalPower <= stats::pnorm(2))) {
        rootlist <- uniroot(
            f = function(alpha) {
                .getNuPrime(alpha = alpha, conditionalPower = conditionalPower) - nuPrime
            },
            lower = 0,
            upper = conditionalPower,
            tol = 1e-16
        )
        return(constrain(rootlist$root))

        # If the conditional power is not between 1-pnorm(2) and pnorm(2) nu prime is not monotone and we need to build the inverse differently
    } else {
        # Calculate the minimum and the maximum of NuPrime(u)
        maximumValue <- 1 -
            stats::pnorm(-stats::qnorm(conditionalPower) / 2 + sqrt(stats::qnorm(conditionalPower)^2 / 4 - 1))
        minimumValue <- 1 -
            stats::pnorm(-stats::qnorm(conditionalPower) / 2 - sqrt(stats::qnorm(conditionalPower)^2 / 4 - 1))
        nuPrimeAtMax <- .getNuPrime(alpha = maximumValue, conditionalPower = conditionalPower)
        nuPrimeAtMin <- .getNuPrime(alpha = minimumValue, conditionalPower = conditionalPower)

        if (nuPrime > nuPrimeAtMax) {
            rootlist <- uniroot(
                f = function(alpha) {
                    .getNuPrime(alpha = alpha, conditionalPower = conditionalPower) - nuPrime
                },
                lower = minimumValue,
                upper = conditionalPower,
                tol = 1e-16
            )
            return(constrain(rootlist$root))
        } else if (nuPrime < nuPrimeAtMin) {
            rootlist <- uniroot(
                f = function(alpha) {
                    .getNuPrime(alpha = alpha, conditionalPower = conditionalPower) - nuPrime
                },
                lower = 0,
                upper = maximumValue,
                tol = 1e-16
            )
            return(constrain(rootlist$root))
        } else {
            # Calculate psiLower and psiUpper
            rootlistLower <- uniroot(
                f = function(alpha) {
                    .getNuPrime(alpha = alpha, conditionalPower = conditionalPower) - nuPrime
                },
                lower = 0,
                upper = maximumValue,
                tol = 1e-16
            )
            psiLower <- constrain(rootlistLower$root)
            rootlistUpper <- uniroot(
                f = function(alpha) {
                    .getNuPrime(alpha = alpha, conditionalPower = conditionalPower) - nuPrime
                },
                lower = minimumValue,
                upper = conditionalPower,
                tol = 1e-16
            )
            psiUpper <- constrain(rootlistUpper$root)
            # Compare feasible candidates before selecting a branch. Clipping only
            # the unconstrained optimum can select the wrong local minimum.
            objectiveLower <- .getNu(psiLower, conditionalPower) - nuPrime * psiLower
            objectiveUpper <- .getNu(psiUpper, conditionalPower) - nuPrime * psiUpper
            if (objectiveUpper <= objectiveLower) {
                return(psiUpper)
            } else {
                return(psiLower)
            }
        }
    }
}

.getPsi <- Vectorize(FUN = .getPsi, vectorize.args = c("nuPrime", "conditionalPower", "lower", "upper"))

#' Calculate Q
#'
#' @description Calculate the ratio of likelihood ratio and squared effect size.
#'
#' @inheritParams param_firstStagePValueOCEF
#' @inheritParams param_designOCEF
#'
#' @details For more information on how to specify the likelihood ratio, see \code{?getLikelihoodRatio()}.
#' In case the optimal conditional error function is ever increasing in the first-stage p-value \eqn{p_1}, a monotone transformation of \code{getQ()}
#' is needed for logical consistency and type I error rate control. \cr
#' The formula for \eqn{Q(p_1)} is:
#' \deqn{Q(p_1) = l(p_1) / \Delta_1^2,}
#' where \eqn{l(p_1)} is the likelihood ratio and \eqn{\Delta_1} is the effect size at which the conditional power should be achieved.
#' The effect size may also depend on the interim data (i.e., on \eqn{p_1}) in case \code{useInterimEstimate = TRUE} was specified for the design object.
#'
#' @return Ratio of likelihood ratio and squared effect size.
#' @noRd
#'
#' @template reference_monotone

.getQ <- function(firstStagePValue, design) {
    # Initialise effect and likelihood ratio
    effect <- NA
    likelihoodRatio <- NA

    # When using interim estimate, apply the restrictions given in the design
    if (design$useInterimEstimate) {
        effect <- min(max(design$ncp1Min, stats::qnorm(firstStagePValue, lower.tail = FALSE)), design$ncp1Max) /
            sqrt(design$firstStageInformation)
    } else {
        # Fixed effect case
        effect <- design$delta1
    }

    likelihoodRatio <- .getLikelihoodRatio(firstStagePValue = firstStagePValue, design = design)

    # At p = 0, positive-tail likelihood ratios grow faster than the squared interim estimate.
    if (firstStagePValue == 0 && is.infinite(effect)) {
        return(if (is.infinite(likelihoodRatio)) Inf else 0)
    }
    Q <- likelihoodRatio / (effect^2)

    return(Q)
}

.getQ <- Vectorize(.getQ, vectorize.args = "firstStagePValue")

#' Integrate over information
#'
#' @description Internal function used by \code{getExpectedSecondStageInformation()} to calculate the integral over the information.
#'
#' @inheritParams param_firstStagePValueOCEF
#' @inheritParams param_likelihoodRatioDistributionOCEF
#' @inheritParams param_designOCEF
#' @param ... Additional arguments needed for \code{getOptimalConditionalError()} and \code{getLikelihoodRatio()}.
#'
#' @return Integral over the information of the second stage
#'
#' @noRd

.integrateExpectedInformation <- function(firstStagePValue, design, likelihoodRatioDistribution, ...) {
    # Calculate optimal conditional error function
    conditionalError <- getOptimalConditionalError(
        firstStagePValue = firstStagePValue,
        design = design
    )

    # Identify how the likelihood ratio should be calculated
    likelihoodRatio <- NA
    args <- list(...)

    # Fixed effect
    # If NULL, use specification in design object
    if (is.null(likelihoodRatioDistribution)) {
        ghostDesign <- design
        likelihoodRatio <- .getLikelihoodRatio(
            firstStagePValue = firstStagePValue,
            design = ghostDesign
        )
    } else if (likelihoodRatioDistribution == "fixed") {
        deltaLR <- unlist(args["deltaLR"])
        weights <- unlist(args["weightsDeltaLR"])

        # Ensure argument specified
        if (is.null(deltaLR)) {
            stopMissingArgument(
                "Argument deltaLR must be provided for fixed likelihood ratio case.",
                parameter = "deltaLR",
                value = deltaLR,
                constraint = "required for the selected likelihood ratio distribution",
                relatedParameter = "likelihoodRatioDistribution",
                relatedValue = likelihoodRatioDistribution,
                functionName = ".integrateExpectedInformation"
            )
        }
        .assertIsNumericVector(x = deltaLR, argumentName = "deltaLR")
        if (is.null(weights)) {
            weights <- rep(1 / length(deltaLR), length(deltaLR))
        }

        # Create a list that acts as a design object to calculate true likelihood ratio
        ghostDesign <- list(
            "likelihoodRatioDistribution" = likelihoodRatioDistribution,
            "deltaLR" = deltaLR,
            "weightsDeltaLR" = weights,
            "firstStageInformation" = design$firstStageInformation
        )

        # Calculate likelihood ratio
        likelihoodRatio <- .getLikelihoodRatio(
            firstStagePValue = firstStagePValue,
            design = ghostDesign
        )
    } else if (likelihoodRatioDistribution == "normal") {
        # Normal prior for effect
        deltaLR <- unlist(args["deltaLR"])
        tauLR <- unlist(args["tauLR"])

        # Ensure arguments specified
        if (is.null(deltaLR) || is.null(tauLR)) {
            stopMissingArgument(
                "Arguments deltaLR and tauLR must be provided for normally distributed likelihood ratio case.",
                parameter = c("deltaLR", "tauLR"),
                value = list(deltaLR = deltaLR, tauLR = tauLR),
                constraint = "required for the selected likelihood ratio distribution",
                relatedParameter = "likelihoodRatioDistribution",
                relatedValue = likelihoodRatioDistribution,
                functionName = ".integrateExpectedInformation"
            )
        }
        .assertIsSingleNumber(x = deltaLR, argumentName = "deltaLR")
        .assertIsSingleNumber(x = tauLR, argumentName = "tauLR")

        .assertIsInOpenInterval(x = tauLR, xName = "tauLR", lower = 0, upper = Inf)

        # Create a list that acts as a design object to calculate true likelihood ratio
        ghostDesign <- list(
            "likelihoodRatioDistribution" = likelihoodRatioDistribution,
            "deltaLR" = deltaLR,
            "tauLR" = tauLR,
            "firstStageInformation" = design$firstStageInformation
        )

        # Calculate likelihood ratio
        likelihoodRatio <- .getLikelihoodRatio(
            firstStagePValue = firstStagePValue,
            design = ghostDesign
        )
    } else if (likelihoodRatioDistribution == "exp") {
        # Exponential prior for effect
        kappaLR <- unlist(args["kappaLR"])

        # Ensure argument specified
        if (is.null(kappaLR)) {
            stopMissingArgument(
                "Argument kappaLR must be specified for exponential likelihood case.",
                parameter = "kappaLR",
                value = kappaLR,
                constraint = "required for the selected likelihood ratio distribution",
                relatedParameter = "likelihoodRatioDistribution",
                relatedValue = likelihoodRatioDistribution,
                functionName = ".integrateExpectedInformation"
            )
        }
        .assertIsSingleNumber(x = kappaLR, argumentName = "kappaLR")
        .assertIsInOpenInterval(x = kappaLR, xName = "kappaLR", lower = 0, upper = Inf)

        # Create a list that acts as a design object to calculate true likelihood ratio
        ghostDesign <- list(
            "likelihoodRatioDistribution" = likelihoodRatioDistribution,
            "kappaLR" = kappaLR,
            "firstStageInformation" = design$firstStageInformation
        )

        # Calculate likelihood ratio
        likelihoodRatio <- .getLikelihoodRatio(
            firstStagePValue = firstStagePValue,
            design = ghostDesign
        )
    } else if (likelihoodRatioDistribution == "unif") {
        # Uniform prior for effect
        deltaMaxLR <- unlist(args["deltaMaxLR"])

        # Ensure argument specified
        if (is.null(deltaMaxLR)) {
            stopMissingArgument(
                "Argument deltaMaxLR must be specified for uniform likelihood case.",
                parameter = "deltaMaxLR",
                value = deltaMaxLR,
                constraint = "required for the selected likelihood ratio distribution",
                relatedParameter = "likelihoodRatioDistribution",
                relatedValue = likelihoodRatioDistribution,
                functionName = ".integrateExpectedInformation"
            )
        }

        .assertIsSingleNumber(x = deltaMaxLR, argumentName = "deltaMaxLR")
        .assertIsInOpenInterval(x = deltaMaxLR, xName = "deltaMaxLR", lower = 0, upper = Inf)

        # Create a list that acts as a design object to calculate true likelihood ratio
        ghostDesign <- list(
            "likelihoodRatioDistribution" = likelihoodRatioDistribution,
            "deltaMaxLR" = deltaMaxLR,
            "firstStageInformation" = design$firstStageInformation
        )

        # Calculate likelihood ratio
        likelihoodRatio <- .getLikelihoodRatio(
            firstStagePValue = firstStagePValue,
            design = ghostDesign
        )
    } else if (likelihoodRatioDistribution == "maxlr") {
        # Maximum likelihood ratio
        # Create a list that acts as a design object to calculate true likelihood ratio
        ghostDesign <- list("likelihoodRatioDistribution" = likelihoodRatioDistribution)

        # Calculate likelihood ratio
        likelihoodRatio <- .getLikelihoodRatio(
            firstStagePValue = firstStagePValue,
            design = ghostDesign
        )
    } else {
        # Unknown distribution specified
        stopIllegalArgument(
            "Distribution not matched.",
            parameter = "likelihoodRatioDistribution",
            value = likelihoodRatioDistribution,
            constraint = "one of fixed, normal, exp, unif or maxlr",
            functionName = ".integrateExpectedInformation"
        )
    }

    # Identify effect size to calculate second-stage information
    # The effect is the same as the one specified in the design object.

    # Fixed effect case
    if (!design$useInterimEstimate) {
        delta1 <- design$delta1
    } else {
        # Interim estimate
        # Apply restrictions that are given in the design object
        delta1 <- pmin(
            pmax(design$delta1Min, stats::qnorm(firstStagePValue, lower.tail = FALSE) / sqrt(design$firstStageInformation)),
            design$delta1Max
        )
    }

    # Check if conditional power function should be used
    if (is.function(design$conditionalPowerFunction)) {
        conditionalPower <- .getOptimalConditionalPower(firstStagePValue, design)
    } else {
        conditionalPower <- design$conditionalPower
    }

    secondStageInformation <- (.getNu(alpha = conditionalError, conditionalPower = conditionalPower) *
        likelihoodRatio) /
        (delta1^2)
    return(secondStageInformation)
}


.assertIsOptimalConditionalErrorDesign <- function(design) {
    if (!inherits(design, "TrialDesignOptimalConditionalError")) {
        stopIllegalArgument(
            "'design' must be a TrialDesignOptimalConditionalError object.",
            parameter = "design",
            value = design,
            constraint = "must inherit from TrialDesignOptimalConditionalError",
            functionName = ".assertIsOptimalConditionalErrorDesign"
        )
    }
    invisible(design)
}

.getOptimalConditionalPower <- function(firstStagePValue, design) {
    if (!is.function(design$conditionalPowerFunction)) {
        return(rep(design$conditionalPower, length(firstStagePValue)))
    }
    vapply(firstStagePValue, function(pValue) {
        value <- design$conditionalPowerFunction(pValue)
        .assertIsSingleNumber(value, "conditionalPowerFunction result")
        .assertIsInOpenInterval(value, "conditionalPowerFunction result", lower = 0, upper = 1)
        value
    }, numeric(1))
}


.getOptimalConditionalError <- function(firstStagePValue, design) {
    conditionalErrorWithConstraints <- NULL

    # Check if firstStagePValue lies outside early decision boundaries
    if (firstStagePValue <= design$alpha1 && design$alpha1 != 0) {
        conditionalErrorWithConstraints <- 1
    } else if (firstStagePValue > design$alpha0) {
        conditionalErrorWithConstraints <- 0
    } else {
        # If monotonisation constants specified and monotonisation enforced, perform non-increasing transformation
        if (design$enforceMonotonicity && !is.null(unlist(design$monotonisationConstants))) {
            likelihoodRatioOverEffect <- .getMonotoneFunction(
                x = firstStagePValue,
                fun = .getQ,
                design = design
            )
        } else {
            likelihoodRatioOverEffect <- .getQ(firstStagePValue = firstStagePValue, design = design)
        }

        # Take constraints into account (minimumConditionalError, maximumConditionalError,
        # minimumSecondStageInformation, maximumSecondStageInformation)
        constraintList <- .getOptimalConditionalErrorConstraints(
            design = design,
            firstStagePValue = firstStagePValue
        )

        conditionalErrorConstraintUpper <- constraintList$conditionalErrorConstraintUpper
        conditionalErrorConstraintLower <- constraintList$conditionalErrorConstraintLower
        conditionalPower <- constraintList$conditionalPower

        conditionalErrorWithConstraints <- max(
            conditionalErrorConstraintLower,
            min(
                conditionalErrorConstraintUpper,
                .getPsi(
                    nuPrime = (-exp(design$levelConstant) / likelihoodRatioOverEffect),
                    conditionalPower = conditionalPower,
                    lower = conditionalErrorConstraintLower, upper = conditionalErrorConstraintUpper
                )
            )
        )
    }

    return(conditionalErrorWithConstraints)
}

.getSecondStageInformation <- function(firstStagePValue, design) {
    .assertIsNumericVector(x = firstStagePValue, argumentName = "firstStagePValue")
    .assertIsInClosedInterval(x = firstStagePValue, xName = "firstStagePValue", lower = 0, upper = 1)

    secondStageInformation <- NULL
    # For p-values outside of the continuation region, return information 0
    if ((firstStagePValue <= design$alpha1 && design$alpha1 > 0) || firstStagePValue > design$alpha0) {
        secondStageInformation <- 0
    } else {
        # For design with interim estimate, apply effect restrictions
        if (design$useInterimEstimate) {
            effect <- min(
                max(stats::qnorm(firstStagePValue, lower.tail = FALSE) / sqrt(design$firstStageInformation), design$delta1Min),
                design$delta1Max
            )
        } else {
            # For design without interim estimate, use fixed effect
            effect <- design$delta1
        }

        # Calculate conditional error
        conditionalError <- getOptimalConditionalError(
            firstStagePValue = firstStagePValue,
            design = design
        )

        # Check if conditional power function should be used
        if (is.function(design$conditionalPowerFunction)) {
            conditionalPower <- .getOptimalConditionalPower(firstStagePValue, design)
        } else {
            conditionalPower <- design$conditionalPower
        }

        secondStageInformation <- (.getNu(alpha = conditionalError, conditionalPower = conditionalPower)) / (effect^2)
    }
    return(secondStageInformation)
}
