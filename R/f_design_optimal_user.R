## |
## |  *Optimal conditional error design functions*
## |
## |  This file is part of the R package rpact:
## |  Confirmatory Adaptive Clinical Trial Design and Analysis
## |
## |  Original contribution: Morten Dreher
## |  Licensed under "GNU Lesser General Public License" version 3
## |  License text: https://www.r-project.org/Licenses/LGPL-3
## |

#' Create an Optimal Conditional Error Design
#'
#' @description This function returns a design object which contains all important parameters for the specification of the optimal conditional error function.
#' The returned object is of class \code{TrialDesignOptimalConditionalError} and can be passed to other package functions.
#'
#'
#' @details
#' The design object contains the information required to determine the specific setting of the optimal conditional error function and can be passed to other package functions.
#' From the given user specifications, the constant to achieve level condition for control of the overall type I error rate as well as the constants to ensure a non-increasing optimal CEF (if required) are automatically calculated.
#'
#' @section Likelihood ratio distribution:
#' To calculate the optimal conditional error function, an assumption about the true parameter under which the second-stage information is to be minimised is required.
#' Various options are available and can be specified via the argument \code{likelihoodRatioDistribution}:
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
#' @section Effect for conditional power:
#' For the treatment effect at which the target conditional power should be achieved, either a fixed effect or an interim estimate can be used.
#' The usage of a fixed effect is indicated by setting \code{useInterimEstimate=FALSE}, in which case the fixed effect is provided by \code{delta1} on the mean difference scale.
#' For an interim estimate, specified by \code{useInterimEstimate=TRUE}, a lower cut-off for the interim estimate must be provided, by \code{delta1Min} on the mean difference scale.
#' In addition, an upper limit of the estimate may be analogously provided by \code{delta1Max}. These effects may alternatively be specified on the non-centrality parameter scale as
#' \code{ncp1}, \code{ncp1Min} and \code{ncp1Max}.
#'
#' @section Sample size and information:
#' The first-stage information of the trial design must be specified to allow for calculations between the mean difference and non-centrality parameter scale.
#' It is provided to the design object via \code{firstStageInformation}. \cr
#' Listed below are some examples for the calculation between information (\eqn{I_1}) and sample size:
#' \itemize{
#'  \item One-sample z-test with \eqn{n} total patients: \eqn{I_1 = \frac{n}{\sigma^2}}, where \eqn{\sigma^2} is the variance of an individual observation
#'
#' \item Balanced two-sample z-test with \eqn{n_1} patients per group: \eqn{I_1 = \frac{1}{2}\cdot\frac{n_1}{\sigma^2}}, where \eqn{\sigma^2} is the common variance
#'
#' \item General two-sample z-test with \eqn{n_1}, \eqn{n_2} patients per group: \eqn{I_1 = 1/(\frac{\sigma_1^2}{n_1}+\frac{\sigma_2^2}{n_2})}, where \eqn{\sigma_1^2}, \eqn{\sigma_2^2} are the group-wise variances
#' }
#'
#' @section Monotonicity:
#' By default, the function Q (likelihood ratio divided by the squared effect)
#' is transformed to be non-increasing in the first-stage p-value. For constant
#' conditional power this yields a non-increasing conditional error function.
#' A conditional power callback combined with interim effect estimates and
#' information constraints can still produce a non-monotone conditional error
#' function; `enforceMonotonicity` does not guarantee monotonicity in that setting.
#' The necessary intervals and constants for the transformation are calculated by an internal monotonisation routine.
#' Although not recommended for the operating characteristics of the design, the transformation may be omitted by setting \code{enforceMonotonicity=FALSE}.
#'
#' @section Constraints:
#' In some applications, it may be feasible to restrict the optimal conditional error function by a lower and/or upper limit.
#' These constraints can be directly implemented on the function by using the arguments \code{minimumConditionalError} and \code{maximumConditionalError}.
#' By default, \code{minimumConditionalError=0} and \code{maximumConditionalError=1}, i.e., no constraints are applied.
#' The constraints may also be specified on the second-stage information via \code{minimumSecondStageInformation} and \code{maximumSecondStageInformation}.
#' If both \code{minimumConditionalError} and \code{maximumSecondStageInformation} respectively \code{maximumConditionalError} and \code{minimumSecondStageInformation}
#' are provided, both constraints will be applied.
#'
#' @section Level constant:
#' The level constant is determined by an internal root-finding routine. It is identified using the \code{uniroot()} function and by default, the interval between 0 and 10 is searched for the level constant.
#' In specific settings, the level constant may lie outside of this interval. In such cases, the search interval can be changed by altering the parameters \code{levelConstantMinimum} and \code{levelConstantMaximum}. \cr
#' If inappropriate constraints to the optimal conditional error function are provided via \code{minimumConditionalError} and \code{maximumConditionalError}
#' or \code{minimumSecondStageInformation} and \code{maximumSecondStageInformation}, it may be impossible to find a level constant which exhausts the full alpha level.
#'
#' Numerical integration uses an adapted routine for piecewise constant functions
#' where applicable. Set `options(rpact.design.optimal.enforce.basic.integration = TRUE)`
#' to use standard adaptive integration for comparison.
#'
#' @section Generic functions:
#' The \code{print()} and \code{plot()} functions are available for objects of class \code{TrialDesignOptimalConditionalError}.
#' For details, see \code{?print.TrialDesignOptimalConditionalError} and \code{?plot.TrialDesignOptimalConditionalError}.
#'
#' @inheritParams param_alphaOCEF
#' @inheritParams param_alpha1OCEF
#' @inheritParams param_alpha0OCEF
#' @inheritParams param_conditionalPowerOCEF
#' @inheritParams param_conditionalPowerFunctionOCEF
#' @inheritParams param_delta1OCEF
#' @inheritParams param_delta1MinOCEF
#' @inheritParams param_delta1MaxOCEF
#' @inheritParams param_likelihoodRatioDistributionOCEF
#' @inheritParams param_firstStageInformationOCEF
#' @inheritParams param_useInterimEstimateOCEF
#' @inheritParams param_minimumSecondStageInformationOCEF
#' @inheritParams param_maximumSecondStageInformationOCEF
#' @inheritParams param_minimumConditionalErrorOCEF
#' @inheritParams param_maximumConditionalErrorOCEF
#' @inheritParams param_levelConstantMinimumOCEF
#' @inheritParams param_levelConstantMaximumOCEF
#' @inheritParams param_enforceMonotonicityOCEF
#' @param ... Distribution parameters `deltaLR`, `weightsDeltaLR`, `tauLR`,
#'   `kappaLR`, and `deltaMaxLR`, as described below. The effect for conditional
#'   power may alternatively be supplied as `ncp1`, or `ncp1Min` and `ncp1Max`,
#'   on the non-centrality scale (mean difference times the square root of
#'   first-stage information). The default upper bound `ncp1Max` is `Inf`.
#'   Mean-difference arguments take precedence when both scales are supplied.
#'
#' @return An object of class \code{TrialDesignOptimalConditionalError}, which can be passed to [getOptimalConditionalError()], [getSecondStageInformation()],
#' [getExpectedSecondStageInformation()], and [getOverallPower()].
#' This adaptive design is not interchangeable with a conventional `TrialDesign`.
#'
#' @examples
#' # Create a single-arm design with fixed parameter for the likelihood ratio
#' # and a fixed effect for conditional power. 80 patients are observed in the
#' # first-stage (firstStageInformation = 80 in the one-sample test, variance 1).
#' # The second-stage information is restricted to be between 40 and 160.
#' getDesignOptimalConditionalErrorFunction(
#'     alpha = 0.025, alpha1 = 0.001, alpha0 = 0.5, conditionalPower = 0.9,
#'     delta1 = 0.25, likelihoodRatioDistribution = "fixed", deltaLR = 0.25,
#'     firstStageInformation = 80, useInterimEstimate = FALSE,
#'     minimumSecondStageInformation = 40, maximumSecondStageInformation = 160
#' )
#'
#' # Create a design comparing two groups using the maximum likelihood ratio
#' # and an interim estimate for the effect for conditional power.
#' # 160 patients per arm are observed in the first stage
#' # (firstStageInformation = 80 in the balanced two-sample test, variance 1).
#' getDesignOptimalConditionalErrorFunction(
#'     alpha = 0.025, alpha1 = 0.001, alpha0 = 0.5, conditionalPower = 0.9,
#'     delta1Min = 0.25, likelihoodRatioDistribution = "maxlr",
#'     firstStageInformation = 80, useInterimEstimate = TRUE
#' )
#'
#' @export
#'
#' @template reference_optimal
#' @template reference_monotone
#'
getDesignOptimalConditionalErrorFunction <- function(
    alpha,
    alpha1,
    alpha0,
    conditionalPower = NA_real_,
    delta1 = NA_real_,
    delta1Min = NA_real_,
    delta1Max = Inf,
    useInterimEstimate = TRUE,
    firstStageInformation,
    likelihoodRatioDistribution,
    minimumSecondStageInformation = 0,
    maximumSecondStageInformation = Inf,
    minimumConditionalError = 0,
    maximumConditionalError = 1,
    conditionalPowerFunction = NULL,
    levelConstantMinimum = 0,
    levelConstantMaximum = 10,
    enforceMonotonicity = TRUE,
    ...
) {
    design <- TrialDesignOptimalConditionalError$new(
        alpha = alpha,
        alpha1 = alpha1,
        alpha0 = alpha0,
        conditionalPower = conditionalPower,
        delta1 = delta1,
        delta1Min = delta1Min,
        delta1Max = delta1Max,
        firstStageInformation = firstStageInformation,
        useInterimEstimate = useInterimEstimate,
        likelihoodRatioDistribution = likelihoodRatioDistribution,
        minimumSecondStageInformation = minimumSecondStageInformation,
        maximumSecondStageInformation = maximumSecondStageInformation,
        minimumConditionalError = minimumConditionalError,
        maximumConditionalError = maximumConditionalError,
        conditionalPowerFunction = conditionalPowerFunction,
        levelConstantMinimum = levelConstantMinimum,
        levelConstantMaximum = levelConstantMaximum,
        enforceMonotonicity = enforceMonotonicity,
        ...
    )

    suppliedArguments <- names(as.list(match.call(expand.dots = TRUE)))[-1]
    for (parameterName in setdiff(names(formals(getDesignOptimalConditionalErrorFunction)), suppliedArguments)) {
        if (parameterName %in% design$.getVisibleFieldNames() &&
            design$.getParameterType(parameterName) == C_PARAM_USER_DEFINED) {
            design$.setParameterType(parameterName, C_PARAM_DEFAULT_VALUE)
        }
    }
    return(design)
}


#' Calculate Expected Second-stage Information
#'
#' @description Calculate the expected second-stage information using the optimal conditional error function with specific assumptions.
#'
#' @details {The expected second-stage information is calculated as:
#'  \deqn{\mathbb{E}(I_{2})=\int_{\alpha_1}^{\alpha_0}\frac{\nu(\alpha_2(p_1)) \cdot l(p_1)}{\Delta_1^2} dp_1,}
#'    where
#'    \itemize{
#'        \item \eqn{\alpha_1, \alpha_0} are the first-stage efficacy and futility boundaries
#'        \item \eqn{\alpha_2(p_1)} is the optimal conditional error calculated for \eqn{p_1}
#'        \item \eqn{l(p_1)} is the "true" likelihood ratio under which to calculate the expected sample size. This can be different from the likelihood ratio used to calibrate the optimal conditional error function.
#'        \item \eqn{\Delta_1} is the assumed treatment effect to power for, expressed as a mean difference. It may depend on the interim data (i.e., \eqn{p_1}) in case \code{useInterimEstimate = TRUE} was specified for the design object.
#'        \item \eqn{\nu(\alpha_2(p_1)) = (\Phi^{-1}(1-\alpha_2(p_1))+\Phi^{-1}(CP))^2} is a factor calculated for the specific assumptions about the optimal conditional error function and the target conditional power \eqn{CP}.
#' }}
#'
#' @inheritParams param_designOCEF
#' @inheritParams param_likelihoodRatioDistributionExpectedOCEF
#' @param ... {Additional parameters required for the specification of \code{likelihoodRatioDistribution}}.
#'
#' @return A numeric scalar: unconditional expected second-stage information,
#'   including zero information for trials stopped at stage one. The maximum
#'   likelihood ratio is not a probability density; designs using `maxlr` require
#'   an explicit distribution for this calculation.
#'
#' @examples
#' # Get a design
#' design <- getDesignOptimalConditionalErrorFunction(
#'     alpha = 0.025, alpha1 = 0.001, alpha0 = 0.5, conditionalPower = 0.9,
#'     delta1 = 0.25, likelihoodRatioDistribution = "fixed", deltaLR = 0.25,
#'     firstStageInformation = 80, useInterimEstimate = FALSE
#' )
#' # Calculate expected information under correct specification
#' getExpectedSecondStageInformation(design)
#'
#' # Calculate expected information under the null hypothesis
#' getExpectedSecondStageInformation(
#'     design = design, likelihoodRatioDistribution = "fixed", deltaLR = 0
#' )
#'
#' @export
#' @seealso [getDesignOptimalConditionalErrorFunction()], [getSecondStageInformation()]
#' @template reference_optimal

getExpectedSecondStageInformation <- function(design, likelihoodRatioDistribution = NULL, ...) {
    .assertIsOptimalConditionalErrorDesign(design)
    distribution <- if (is.null(likelihoodRatioDistribution)) design$likelihoodRatioDistribution else likelihoodRatioDistribution
    if (identical(distribution, "maxlr")) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "Expected information requires a probability distribution; specify 'fixed', 'normal', 'exp', or 'unif'.",
            call. = FALSE
        )
    }
    # Integrate over a helper function from alpha1 to alpha0
    return(
        stats::integrate(
            f = .integrateExpectedInformation,
            lower = design$alpha1,
            upper = design$alpha0,
            design = design,
            likelihoodRatioDistribution = likelihoodRatioDistribution,
            ...
        )$value
    )
}

#' Calculate the Optimal Conditional Error
#'
#' @details The optimal conditional error \eqn{\alpha_2} given a first-stage p-value \eqn{p_1} is calculated as:
#' \deqn{\alpha_2(p_1)=\psi(-e^{c_0} \cdot \frac{\Delta_1^2}{l(p_1)}).}
#'
#' The level constant \eqn{c_0} as well as the specification of the effect size \eqn{\Delta_1} and the likelihood ratio \eqn{l(p_1)}
#' must be contained in the \code{design} object (see \code{?getDesignOptimalConditionalErrorFunction}).
#' Early stopping rules are supported, i.e., for \eqn{p_1 \leq \alpha_1} with \eqn{\alpha_1 > 0}, the returned conditional error is 1 and for \eqn{p_1 > \alpha_0}, the returned conditional error is 0.
#'
#'
#' @inheritParams param_firstStagePValueOCEF
#' @inheritParams param_designOCEF
#'
#' @return A numeric vector of conditional errors, one per first-stage p-value.
#' @export
#'
#' @template reference_optimal
#'
#' @seealso [getDesignOptimalConditionalErrorFunction()]
#'
#' @examples
#' # Create a design
#' design <- getDesignOptimalConditionalErrorFunction(
#'     alpha = 0.025, alpha1 = 0.001, alpha0 = 0.5, conditionalPower = 0.9,
#'     delta1 = 0.5, firstStageInformation = 40, useInterimEstimate = FALSE,
#'     likelihoodRatioDistribution = "fixed", deltaLR = 0.5
#' )
#'
#' # Calculate optimal conditional error
#' getOptimalConditionalError(
#'     firstStagePValue = c(0.1, 0.2, 0.3), design = design
#' )
#'

#' @rdname getOptimalConditionalError
#' @export
getOptimalConditionalError <- function(firstStagePValue, design) {
    .assertIsOptimalConditionalErrorDesign(design)
    .assertIsNumericVector(firstStagePValue, "firstStagePValue")
    .assertIsInClosedInterval(firstStagePValue, "firstStagePValue", lower = 0, upper = 1)
    return(vapply(firstStagePValue, .getOptimalConditionalError, numeric(1), design = design))
}

#' Calculate the overall power
#'
#' @description Calculate the overall power and other operating characteristics of a design.
#'
#' @details
#' This function is used to evaluate the overall performance of a design.
#' The probabilities for first-stage futility, first-stage efficacy and overall efficacy (power) are returned in a named list.
#'
#' @inheritParams param_designOCEF
#' @inheritParams param_alternativeOCEF
#'
#' @seealso [getDesignOptimalConditionalErrorFunction()]
#'
#' @examples
#' design <- getDesignOptimalConditionalErrorFunction(
#'     alpha = 0.025, alpha1 = 0.001, alpha0 = 0.5, conditionalPower = 0.9,
#'     delta1 = 0.5, useInterimEstimate = FALSE, firstStageInformation = 4,
#'     likelihoodRatioDistribution = "maxlr"
#' )
#' getOverallPower(design, alternative = c(0, 0.25, 0.5))
#'
#' @return A list with numeric vectors `alternative`, `firstStageFutility`,
#'   `firstStageEfficacy`, and `overallPower`, one entry per effect size.
#' @export

getOverallPower <- function(design, alternative) {
    .assertIsOptimalConditionalErrorDesign(design)
    .assertIsNumericVector(x = alternative, argumentName = "alternative")

    if (any(!is.finite(alternative))) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'alternative' must contain finite values.", call. = FALSE)
    }
    alternativeNonCentralityParameterScale <- alternative * base::sqrt(design$firstStageInformation)

    # One value per entry in alternative
    firstStageFutility <- numeric(length(alternative))
    firstStageEfficacy <- numeric(length(alternative))
    overallPower <- numeric(length(alternative))

    for (i in seq_along(alternative)) {
        # Early decision probabilities
        firstStageFutility[i] <- stats::pnorm(
            stats::qnorm(1 - design$alpha0) - alternativeNonCentralityParameterScale[i]
        )
        firstStageEfficacy[i] <- 1 -
            stats::pnorm(stats::qnorm(1 - design$alpha1) - alternativeNonCentralityParameterScale[i])

        # Calculate probability to reject at the second stage for given delta
        .secondStageRejection <- function(firstStagePValue) {
            (1 -
                stats::pnorm(
                    stats::qnorm(1 - getOptimalConditionalError(firstStagePValue, design = design)) -
                        sqrt(getSecondStageInformation(firstStagePValue, design = design)) * alternative[i]
                )) *
                exp(
                    qnorm(1 - firstStagePValue) *
                        alternativeNonCentralityParameterScale[i] -
                        alternativeNonCentralityParameterScale[i]^2 / 2
                )
        }

        integral <- stats::integrate(f = .secondStageRejection, lower = design$alpha1, upper = design$alpha0)$value

        overallPower[i] <- firstStageEfficacy[i] + integral
    }

    powerResults <- list(
        alternative = alternative,
        firstStageFutility = firstStageFutility,
        firstStageEfficacy = firstStageEfficacy,
        overallPower = overallPower
    )

    return(powerResults)
}

#' Calculate the Second-stage Information
#'
#' @description Calculate second-stage information for given first-stage p-value and design.
#'
#' @details The second-stage information \eqn{I_{2}} is calculated given a first-stage p-value \eqn{p_1} as:
#' \deqn{I_{2}(p_1) = \frac{(\Phi^{-1}(1-\alpha_2(p_1)) + \Phi^{-1}(CP))^2}{\Delta_1^2} = \frac{\nu(\alpha_2(p_1))}{\Delta_1^2},}
#' where
#' \itemize{
#'    \item \eqn{\alpha_2(p_1)} is the conditional error function
#'    \item \eqn{CP} is the target conditional power
#'    \item \eqn{\Delta_1} is the assumed treatment effect (expressed as a mean difference).
#' }
#' The conditional error is calculated according to the specification provided in the \code{design} argument.
#' For p-values smaller or equal to the first-stage efficacy boundary as well as p-values greater than the first-stage futility boundary,
#' the returned information is 0 (since the trial is ended early in both cases).
#' When `alpha1 = 0`, early efficacy stopping is disabled, including at a p-value of zero.
#'
#' @inheritParams param_firstStagePValueOCEF
#' @inheritParams param_designOCEF
#'
#' @return A numeric vector of second-stage information values, one per first-stage p-value.
#' @export
#'
#' @seealso [getDesignOptimalConditionalErrorFunction()], [getExpectedSecondStageInformation()], [getOptimalConditionalError()]
#'
#' @examples
#' design <- getDesignOptimalConditionalErrorFunction(
#'     alpha = 0.025, alpha1 = 0.001, alpha0 = 0.5,
#'     conditionalPower = 0.9, delta1 = 0.25, useInterimEstimate = FALSE,
#'     firstStageInformation = 40, likelihoodRatioDistribution = "maxlr"
#' )
#'
#' getSecondStageInformation(
#'     firstStagePValue = c(0.05, 0.1, 0.2), design = design
#' )
#'
#' @template reference_optimal
#' @rdname getSecondStageInformation
#' @export
getSecondStageInformation <- function(firstStagePValue, design) {
    .assertIsOptimalConditionalErrorDesign(design)
    .assertIsNumericVector(firstStagePValue, "firstStagePValue")
    .assertIsInClosedInterval(firstStagePValue, "firstStagePValue", lower = 0, upper = 1)
    return(vapply(firstStagePValue, .getSecondStageInformation, numeric(1), design = design))
}
