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
#' @description This function returns a design object which contains all 
#' important parameters for the specification of the optimal conditional 
#' error function.
#' The returned object is of class \code{TrialDesignOptimalConditionalError} 
#' and can be passed to other package functions.
#'
#'
#' @details
#' The design object contains the information required to determine the 
#' specific setting of the optimal conditional error function and can be 
#' passed to other package functions.
#' From the given user specifications, the constant to achieve level 
#' condition for control of the overall type I error rate as well as the 
#' constants to ensure a non-increasing optimal CEF (if required) are 
#' automatically calculated.
#'
#' @section Hypotheses and trial decisions:
#' The one-sided hypotheses are \eqn{H_0: \Delta \leq 0} versus
#' \eqn{H_1: \Delta > 0}. With a first-stage p-value \eqn{p_1}, stop for
#' efficacy if \eqn{p_1 \leq \alpha_1} and \eqn{\alpha_1 > 0}; stop for
#' futility if \eqn{p_1 > \alpha_0}. Futility is binding: continuing beyond
#' this boundary is not part of the calibrated design. In the continuation
#' region, reject at stage two if its p-value is no larger than
#' [getConditionalError()]. The second-stage p-value must be based on
#' the new, independent second-stage data, not the cumulative data.
#' Setting `efficacyBounds = 0` disables early efficacy and 
#' `futilityBounds = 1` disables early futility.
#' Here, \eqn{\alpha_1} denotes `efficacyBounds` and \eqn{\alpha_0} denotes
#' `futilityBounds`; both are single interim cutoffs on the p-value scale.
#' The calibration satisfies
#' \deqn{\alpha = \alpha_1 + \int_{\alpha_1}^{\alpha_0} \alpha_2(p_1) \, dp_1.}
#' Target conditional power refers to a specified planning effect after
#' continuation; it is not the overall power returned by [getDesignCharacteristics()].
#'
#' @section Likelihood ratio distribution:
#' To calculate the optimal conditional error function, an assumption about 
#' the true parameter under which the second-stage information is to be 
#' minimised is required.
#' Various options are available and can be specified via the argument 
#' \code{likelihoodRatioDistribution}:
#' \itemize{
#'    \item \code{likelihoodRatioDistribution="fixed"}: calculates the likelihood 
#'          ratio for a fixed \eqn{\Delta}. The non-centrality parameter of the 
#'          likelihood ratio \eqn{\vartheta} is then computed as 
#'          \code{thetaLR}*\code{sqrt(firstStageInformation)} and the likelihood 
#'          ratio is calculated as:
#'          \deqn{l(p_1) = e^{\Phi^{-1}(1-p_1)\vartheta - \vartheta^2/2}.} \code{thetaLR} 
#'          may also contain multiple elements, in which case a weighted likelihood ratio 
#'          is calculated for the given values. Unless positive weights that sum to 1 are 
#'          provided by the argument \code{weightsLR}, equal weights are assumed.
#'    \item \code{likelihoodRatioDistribution="normal"}: calculates the likelihood ratio 
#'          for a normally distributed prior of \eqn{\vartheta} with mean 
#'          \code{thetaLR}*\code{sqrt(firstStageInformation)} (\eqn{\mu}) and standard 
#'          deviation \code{stDevLR}*\code{sqrt(firstStageInformation)} (\eqn{\sigma}). 
#'          The parameters \code{thetaLR} and \code{stDevLR} must be specified on the 
#'          mean difference scale.
#'          \deqn{l(p_1) = (1+\sigma^2)^{-\frac{1}{2}}\cdot e^{-(\mu/\sigma)^2/2 + (\sigma\Phi^{-1}(1-p_1) + \mu/\sigma)^2 / (2\cdot (1+\sigma^2))}}
#'    \item \code{likelihoodRatioDistribution="exp"}: calculates the likelihood 
#'          ratio for an exponentially distributed prior of \eqn{\vartheta} with 
#'          rate \code{kappaLR}*\code{sqrt(firstStageInformation)} (\eqn{\eta}). 
#'          The likelihood ratio is then calculated as:
#'          \deqn{l(p_1) = \eta \cdot \sqrt{2\pi} \cdot e^{(\Phi^{-1}(1-p_1)-\eta)^2/2} \cdot \Phi(\Phi^{-1}(1-p_1)-\eta)}
#'    \item \code{likelihoodRatioDistribution="unif"}: 
#'          calculates the likelihood ratio for a uniformly distributed prior of 
#'          \eqn{\vartheta} on the support \eqn{[0, \Delta\cdot\sqrt{I_1}]}, where 
#'          \eqn{\Delta} is specified as \code{maxThetaLR} and \eqn{I_1} 
#'          is the \code{firstStageInformation}.
#'          \deqn{l(p_1) = \frac{\sqrt{2\pi}}{\Delta\cdot\sqrt{I_1}} \cdot e^{\Phi^{-1}(1-p_1)^2/2} \cdot (\Phi(\Delta\cdot\sqrt{I_1} - \Phi^{-1}(1-p_1))-p_1)}
#'    \item \code{likelihoodRatioDistribution="maxlr"}: 
#'          the non-centrality parameter \eqn{\vartheta} is estimated from the data 
#'          and no additional parameters must be specified. 
#'          The likelihood ratio is estimated from the data as:
#'          \deqn{l(p_1) = e^{max(0, \Phi^{-1}(1-p_1))^2/2}}
#'          The maximum likelihood ratio is always restricted to effect sizes 
#'          \eqn{\vartheta \geq 0} (corresponding to \eqn{p_1 \leq 0.5}).
#' }
#'
#' @section Effect for conditional power:
#' For the treatment effect at which the target conditional power should be 
#' achieved, either a fixed effect or an interim estimate can be used.
#' The planning effect `thetaH1` need not equal `thetaLR`: the former sets the
#' conditional power target, whereas the latter specifies the effect or mixture
#' under which expected second-stage information is minimised.
#' The usage of a fixed effect is indicated by setting \code{useInterimEstimate=FALSE}, 
#' in which case the fixed effect is provided by \code{thetaH1} on the mean difference scale.
#' For an interim estimate, specified by \code{useInterimEstimate=TRUE}, 
#' a lower cut-off for the interim estimate must be provided, by \code{minThetaH1} 
#' on the mean difference scale.
#' In addition, an upper limit of the estimate may be analogously provided by 
#' \code{maxThetaH1}. These effects may alternatively be specified on the 
#' non-centrality parameter scale as
#' \code{nonCentralityParameterH1}, \code{minNonCentralityParameterH1}, and 
#' \code{maxNonCentralityParameterH1}.
#'
#' @section Sample size and information:
#' The first-stage information of the trial design must be specified to allow for 
#' calculations between the mean difference and non-centrality parameter scale.
#' It is provided to the design object via \code{firstStageInformation}. \cr
#' Listed below are some examples for the calculation between information (\eqn{I_1}) and sample size:
#' \itemize{
#'  \item One-sample z-test with \eqn{n} total patients: 
#'       \eqn{I_1 = \frac{n}{\sigma^2}}, where \eqn{\sigma^2} is the variance of an individual observation
#'
#' \item Balanced two-sample z-test with \eqn{n_1} patients per group: 
#'       \eqn{I_1 = \frac{1}{2}\cdot\frac{n_1}{\sigma^2}}, where \eqn{\sigma^2} is the common variance
#'
#' \item General two-sample z-test with \eqn{n_1}, \eqn{n_2} patients per group: 
#'       \eqn{I_1 = 1/(\frac{\sigma_1^2}{n_1}+\frac{\sigma_2^2}{n_2})}, where \eqn{\sigma_1^2}, \eqn{\sigma_2^2} are the group-wise variances
#' }
#'
#' @section Monotonicity:
#' By default, the function Q (likelihood ratio divided by the squared effect)
#' is transformed to be non-increasing in the first-stage p-value. For constant
#' conditional power this yields a non-increasing conditional error function.
#' A conditional power callback combined with interim effect estimates and
#' information constraints can still produce a non-monotone conditional error
#' function; `enforceMonotonicity` does not guarantee monotonicity in that setting.
#' The necessary intervals and constants for the transformation are calculated by 
#' an internal monotonisation routine.
#' Although not recommended for the operating characteristics of the design, 
#' the transformation may be omitted by setting \code{enforceMonotonicity=FALSE}.
#'
#' @section Constraints:
#' In some applications, it may be feasible to restrict the optimal conditional 
#' error function by a lower and/or upper limit.
#' These constraints can be directly implemented on the function by using the 
#' arguments \code{minConditionalError} and \code{maxConditionalError}.
#' By default, \code{minConditionalError=0} and \code{maxConditionalError=1}, 
#' i.e., no constraints are applied.
#' The constraints may also be specified on the second-stage information via 
#' \code{minInformationPerStage} and \code{maxInformationPerStage}.
#' If both \code{minConditionalError} and \code{maxInformationPerStage} 
#' respectively \code{maxConditionalError} and \code{minInformationPerStage}
#' are provided, both constraints will be applied.
#' Set these constraints when creating the design, so that calibration accounts
#' for them. For fixed \eqn{\Delta_1} and conditional power \eqn{CP},
#' an upper information bound corresponds to the lower conditional error bound
#' \eqn{\Phi(\Phi^{-1}(CP) - \Delta_1 \sqrt{I_{2,\max}})}; a lower information
#' bound analogously corresponds to an upper conditional error bound.
#' Information bounds apply only when the trial continues: information is zero
#' after early stopping. They refer to additional stage-two information, not
#' cumulative information.
#' `minInformationPerStage` and `maxInformationPerStage` are scalar limits for
#' stage two only; they do not constrain `firstStageInformation`.
#' In the continuation region, conditional error cannot exceed the conditional power;
#' an upper bound above it has no additional effect. Before calibration, the integrated
#' lower and upper bounds are checked for compatibility with the overall alpha level.
#'
#' @section Level constant:
#' The level constant is determined by an internal root-finding routine. 
#' It is identified using the \code{uniroot()} function and by default, the 
#' interval between 0 and 10 is searched for the level constant.
#' In specific settings, the level constant may lie outside of this interval. 
#' In such cases, the search interval can be changed by altering the 
#' parameters \code{minLevelConstant} and \code{maxLevelConstant}. \cr
#' If inappropriate constraints to the optimal conditional error function are 
#' provided via \code{minConditionalError} and \code{maxConditionalError}
#' or \code{minInformationPerStage} and \code{maxInformationPerStage}, it may 
#' be impossible to find a level constant which exhausts the full alpha level.
#'
#' Numerical integration uses an adapted routine for piecewise constant functions
#' where applicable. Set `options(rpact.design.optimal.enforce.basic.integration = TRUE)`
#' to use standard adaptive integration for comparison.
#'
#' @section Generic functions:
#' The \code{print()} and \code{plot()} functions are available for objects 
#' of class \code{TrialDesignOptimalConditionalError}.
#' For details, see \code{?print.TrialDesignOptimalConditionalError} and 
#' \code{?plot.TrialDesignOptimalConditionalError}.
#'
#' @inheritParams param_alphaOCEF
#' @inheritParams param_efficacyBoundsOCEF
#' @inheritParams param_futilityBoundsOCEF
#' @inheritParams param_conditionalPowerOCEF
#' @inheritParams param_conditionalPowerFunctionOCEF
#' @inheritParams param_thetaH1OCEF
#' @inheritParams param_minThetaH1OCEF
#' @inheritParams param_maxThetaH1OCEF
#' @inheritParams param_likelihoodRatioDistributionOCEF
#' @inheritParams param_firstStageInformationOCEF
#' @inheritParams param_useInterimEstimateOCEF
#' @inheritParams param_minInformationPerStageOCEF
#' @inheritParams param_maxInformationPerStageOCEF
#' @inheritParams param_minConditionalErrorOCEF
#' @inheritParams param_maxConditionalErrorOCEF
#' @inheritParams param_minLevelConstantOCEF
#' @inheritParams param_maxLevelConstantOCEF
#' @inheritParams param_enforceMonotonicityOCEF
#' @param efficacyBoundsScale,futilityBoundsScale Scale of the interim boundaries.
#'   Currently only `"pValue"` is supported. These are p-value cutoffs, not the
#'   test-statistic boundaries used by conventional group sequential designs.
#' @param nonCentralityParameterH1 Fixed conditional-power effect on the
#'   non-centrality scale; alternative to `thetaH1` when `useInterimEstimate = FALSE`.
#' @param minNonCentralityParameterH1,maxNonCentralityParameterH1 Lower and upper
#'   limits for the interim effect on the non-centrality scale; alternatives to
#'   `minThetaH1` and `maxThetaH1` when `useInterimEstimate = TRUE`.
#'   Non-centrality equals the mean difference times `sqrt(firstStageInformation)`.
#'   Mean-difference arguments take precedence when both scales are supplied.
#'   The default upper limit is `Inf`.
#' @param ... Distribution parameters `thetaLR`, `weightsLR`, `stDevLR`,
#'   `kappaLR`, and `maxThetaLR`, as described under Likelihood ratio distribution.
#'
#' @return An object of class \code{TrialDesignOptimalConditionalError}, 
#' which can be passed to [getConditionalError()], [getStageInformation()],
#' and [getDesignCharacteristics()].
#' This adaptive design is not interchangeable with a conventional `TrialDesign`.
#'
#' @examples
#' # Create a single-arm design with fixed parameter for the likelihood ratio
#' # and a fixed effect for conditional power. 80 patients are observed in the
#' # first-stage (firstStageInformation = 80 in the one-sample test, variance 1).
#' # The second-stage information is restricted to be between 40 and 160.
#' getDesignOptimalConditionalError(
#'     alpha = 0.025, efficacyBounds = 0.001, futilityBounds = 0.5, conditionalPower = 0.9,
#'     thetaH1 = 0.25, likelihoodRatioDistribution = "fixed", thetaLR = 0.25,
#'     firstStageInformation = 80, useInterimEstimate = FALSE,
#'     minInformationPerStage = 40, maxInformationPerStage = 160
#' )
#'
#' # Create a design comparing two groups using the maximum likelihood ratio
#' # and an interim estimate for the effect for conditional power.
#' # 160 patients per arm are observed in the first stage
#' # (firstStageInformation = 80 in the balanced two-sample test, variance 1).
#' getDesignOptimalConditionalError(
#'     alpha = 0.025, efficacyBounds = 0.001, futilityBounds = 0.5, conditionalPower = 0.9,
#'     minThetaH1 = 0.25, likelihoodRatioDistribution = "maxlr",
#'     firstStageInformation = 80, useInterimEstimate = TRUE
#' )
#'
#' # Weight several effects for the optimisation; retain a separate power target.
#' weightedDesign <- getDesignOptimalConditionalError(
#'     alpha = 0.025, efficacyBounds = 0.001, futilityBounds = 0.5, conditionalPower = 0.9,
#'     thetaH1 = 0.25, useInterimEstimate = FALSE, firstStageInformation = 80,
#'     likelihoodRatioDistribution = "fixed", thetaLR = c(0, 0.25, 0.5),
#'     weightsLR = c(0.25, 0.5, 0.25)
#' )
#' getStageInformation(pValue = c(0.05, 0.1, 0.3), design = weightedDesign)
#'
#' # Allow a lower conditional power target for less promising interim results.
#' # Leave conditionalPower unspecified when supplying a callback.
#' flexibleDesign <- getDesignOptimalConditionalError(
#'     alpha = 0.025, efficacyBounds = 0.001, futilityBounds = 0.5,
#'     conditionalPowerFunction = function(p) pnorm(1 - p),
#'     thetaH1 = 0.25, useInterimEstimate = FALSE, firstStageInformation = 80,
#'     likelihoodRatioDistribution = "maxlr"
#' )
#' getConditionalError(pValue = c(0.05, 0.1, 0.3), design = flexibleDesign)
#'
#' @export
#'
#' @template reference_optimal
#' @template reference_monotone
#' @template reference_optconerrf
#'
getDesignOptimalConditionalError <- function(
    alpha,
    efficacyBounds,
    futilityBounds,
    conditionalPower = NA_real_,
    thetaH1 = NA_real_,
    minThetaH1 = NA_real_,
    maxThetaH1 = Inf,
    useInterimEstimate = TRUE,
    firstStageInformation,
    likelihoodRatioDistribution,
    minInformationPerStage = 0,
    maxInformationPerStage = Inf,
    minConditionalError = 0,
    maxConditionalError = 1,
    conditionalPowerFunction = NULL,
    minLevelConstant = 0,
    maxLevelConstant = 10,
    enforceMonotonicity = TRUE,
    efficacyBoundsScale = "pValue",
    futilityBoundsScale = "pValue",
    nonCentralityParameterH1 = NULL,
    minNonCentralityParameterH1 = NULL,
    maxNonCentralityParameterH1 = Inf,
    ...
) {
    design <- TrialDesignOptimalConditionalError$new(
        alpha = alpha,
        efficacyBounds = efficacyBounds,
        futilityBounds = futilityBounds,
        conditionalPower = conditionalPower,
        thetaH1 = thetaH1,
        minThetaH1 = minThetaH1,
        maxThetaH1 = maxThetaH1,
        firstStageInformation = firstStageInformation,
        useInterimEstimate = useInterimEstimate,
        likelihoodRatioDistribution = likelihoodRatioDistribution,
        minInformationPerStage = minInformationPerStage,
        maxInformationPerStage = maxInformationPerStage,
        minConditionalError = minConditionalError,
        maxConditionalError = maxConditionalError,
        conditionalPowerFunction = conditionalPowerFunction,
        minLevelConstant = minLevelConstant,
        maxLevelConstant = maxLevelConstant,
        enforceMonotonicity = enforceMonotonicity,
        efficacyBoundsScale = efficacyBoundsScale,
        futilityBoundsScale = futilityBoundsScale,
        nonCentralityParameterH1 = nonCentralityParameterH1,
        minNonCentralityParameterH1 = minNonCentralityParameterH1,
        maxNonCentralityParameterH1 = maxNonCentralityParameterH1,
        ...
    )

    suppliedArguments <- names(as.list(match.call(expand.dots = TRUE)))[-1]
    for (parameterName in setdiff(names(formals(getDesignOptimalConditionalError)), suppliedArguments)) {
        if (parameterName %in% design$.getVisibleFieldNames() &&
            design$.getParameterType(parameterName) == C_PARAM_USER_DEFINED) {
            design$.setParameterType(parameterName, C_PARAM_DEFAULT_VALUE)
        }
    }
    return(design)
}


.getOptimalDesignExpectedInformation <- function(design, likelihoodRatioDistribution = NULL, ...) {
    .assertIsOptimalDesign(design)
    distribution <- if (is.null(likelihoodRatioDistribution)) design$likelihoodRatioDistribution else likelihoodRatioDistribution
    if (identical(distribution, "maxlr")) {
        stopIllegalArgument(
            "Expected information requires a probability distribution; specify 'fixed', 'normal', 'exp', or 'unif'.",
            parameter = "likelihoodRatioDistribution",
            value = distribution,
            constraint = "a probability distribution: fixed, normal, exp or unif",
            functionName = ".getOptimalDesignExpectedInformation",
            reason = "Expected information requires a probability distribution; maxlr does not supply one.",
            userInstructions = paste0(
                "Choose fixed, normal, exp or unif with its required parameters if that distribution matches ",
                "the intended effect assumptions."
            )
        )
    }
    # Integrate over a helper function from efficacyBounds to futilityBounds
    return(
        stats::integrate(
            f = .getOptimalDesignExpectedInformationIntegrand,
            lower = design$efficacyBounds,
            upper = design$futilityBounds,
            design = design,
            likelihoodRatioDistribution = likelihoodRatioDistribution,
            ...
        )$value
    )
}

#' Calculate the Optimal Conditional Error
#'
#' @details The optimal conditional error \eqn{\alpha_2} given a 
#' first-stage p-value \eqn{p_1} is calculated as:
#' \deqn{\alpha_2(p_1)=\psi(-e^{c_0} \cdot \frac{\Delta_1^2}{l(p_1)}).}
#'
#' The level constant \eqn{c_0} as well as the specification of the effect 
#' size \eqn{\Delta_1} and the likelihood ratio \eqn{l(p_1)}
#' must be contained in the \code{design} object (see \code{?getDesignOptimalConditionalError}).
#' Early stopping rules are supported, i.e., for \eqn{p_1 \leq \alpha_1} 
#' with \eqn{\alpha_1 > 0}, the returned conditional error is 1 and for 
#' \eqn{p_1 > \alpha_0}, the returned conditional error is 0.
#'
#'
#' @inheritParams param_pValueOCEF
#' @inheritParams param_designOCEF
#'
#' @return A numeric vector of conditional errors, one per first-stage p-value.
#' @export
#'
#' @template reference_optimal
#' @template reference_monotone
#' @template reference_optconerrf
#'
#' @seealso [getDesignOptimalConditionalError()]
#'
#' @examples
#' # Create a design
#' design <- getDesignOptimalConditionalError(
#'     alpha = 0.025, efficacyBounds = 0.001, futilityBounds = 0.5, conditionalPower = 0.9,
#'     thetaH1 = 0.5, firstStageInformation = 40, useInterimEstimate = FALSE,
#'     likelihoodRatioDistribution = "fixed", thetaLR = 0.5
#' )
#'
#' # Early efficacy gives 1, continuation gives the stage-two threshold,
#' # and binding futility gives 0.
#' getConditionalError(
#'     pValue = c(0.0005, 0.1, 0.3, 0.8), design = design
#' )
#'

#' @param stage Completed interim stage. Currently only `1` is supported.
#' @rdname getConditionalError
#' @export
getConditionalError <- function(design, pValue, stage = 1) {
    .assertIsOptimalDesignStage(stage, supportedStage = 1, functionName = "getConditionalError")
    .assertIsOptimalDesign(design)
    .assertIsNumericVector(pValue, "pValue")
    .assertIsInClosedInterval(pValue, "pValue", lower = 0, upper = 1)
    return(vapply(pValue, .getOptimalDesignConditionalErrorValue, numeric(1), design = design))
}

.getOptimalDesignCharacteristics <- function(design, theta, ...) {
    .warnInCaseOfUnknownArguments(functionName = "getDesignCharacteristics", ...)
    if (missing(theta)) {
        stopMissingArgument(
            "Specify 'theta' to evaluate an optimal conditional error design.",
            parameter = "theta", constraint = "finite treatment effects on the mean difference scale",
            functionName = "getDesignCharacteristics"
        )
    }
    .assertIsOptimalDesign(design)
    .assertIsNumericVector(x = theta, argumentName = "theta")

    if (any(!is.finite(theta))) {
        stopIllegalArgument(
            "'theta' must contain finite values.",
            parameter = "theta",
            value = theta,
            constraint = "must contain finite values",
            functionName = "getDesignCharacteristics"
        )
    }
    nonCentralityParameters <- theta * base::sqrt(design$firstStageInformation)

    # One value per entry in theta
    firstStageFutility <- numeric(length(theta))
    firstStageEfficacy <- numeric(length(theta))
    overallReject <- numeric(length(theta))

    for (i in seq_along(theta)) {
        # Early decision probabilities
        firstStageFutility[i] <- stats::pnorm(
            stats::qnorm(1 - design$futilityBounds) - nonCentralityParameters[i]
        )
        firstStageEfficacy[i] <- 1 -
            stats::pnorm(stats::qnorm(1 - design$efficacyBounds) - nonCentralityParameters[i])

        # Calculate probability to reject at the second stage for the given effect
        .getOptimalDesignStageRejectionIntegrand <- function(pValue) {
            (1 -
                stats::pnorm(
                    stats::qnorm(1 - getConditionalError(pValue = pValue, design = design)) -
                        sqrt(getStageInformation(pValue = pValue, design = design)) * theta[i]
                )) *
                exp(
                    qnorm(1 - pValue) *
                        nonCentralityParameters[i] -
                        nonCentralityParameters[i]^2 / 2
                )
        }

        integral <- stats::integrate(f = .getOptimalDesignStageRejectionIntegrand, 
            lower = design$efficacyBounds, upper = design$futilityBounds)$value

        overallReject[i] <- firstStageEfficacy[i] + integral
    }

    return(TrialDesignOptimalConditionalErrorCharacteristics$new(
        design = design, theta = theta, overallReject = overallReject,
        rejectPerStage = rbind(firstStageEfficacy, overallReject - firstStageEfficacy),
        futilityPerStage = matrix(firstStageFutility, nrow = 1)
    ))
}

#' Calculate Stage Information for an Adaptive Design
#'
#' @description Calculate the additional information required after an interim
#'   analysis, or its unconditional expectation, for an optimal conditional error design.
#'
#' @inheritParams param_designOCEF
#' @inheritParams param_pValueOCEF
#' @inheritParams param_likelihoodRatioDistributionExpectedOCEF
#' @param stage Target stage for which additional information is required.
#'   Currently only `2` is supported; the interim p-value comes from stage one.
#' @param type `"conditional"` (default) for information given `pValue`, or
#'   `"expected"` for the unconditional expected additional information.
#' @param ... Distribution parameters for `type = "expected"`: `thetaLR`,
#'   `weightsLR`, `stDevLR`, `kappaLR`, and `maxThetaLR`, as applicable.
#' @return For `type = "conditional"`, a numeric vector with one information
#'   value per p-value, including zero after early stopping. For `type = "expected"`,
#'   a numeric scalar including zero information for trials stopped at stage one.
#'   Neither result includes first-stage information. Designs using `"maxlr"`
#'   require an explicit probability distribution for the expectation.
#'
#' @section Conditional information: The second-stage information \eqn{I_{2}} is calculated given a first-stage p-value \eqn{p_1} as:
#' \deqn{I_{2}(p_1) = \frac{(\Phi^{-1}(1-\alpha_2(p_1)) + \Phi^{-1}(CP))^2}{\Delta_1^2} = \frac{\nu(\alpha_2(p_1))}{\Delta_1^2},}
#' where
#' \itemize{
#'    \item \eqn{\alpha_2(p_1)} is the conditional error function
#'    \item \eqn{CP} is the target conditional power
#'    \item \eqn{\Delta_1} is the assumed treatment effect (expressed as a mean difference).
#' }
#' The conditional error is calculated according to the specification provided in the \code{design} argument.
#' For p-values smaller or equal to the first-stage efficacy boundary as well 
#' as p-values greater than the first-stage futility boundary,
#' the returned information is 0 (since the trial is ended early in both cases).
#' When `efficacyBounds = 0`, early efficacy stopping is disabled, including at a p-value of zero.
#'
#' @section Expected information:
#' The expected second-stage information is calculated as:
#'  \deqn{\mathbb{E}(I_{2})=\int_{\alpha_1}^{\alpha_0}\frac{\nu(\alpha_2(p_1)) \cdot l(p_1)}{\Delta_1^2} dp_1,}
#'    where
#'    \itemize{
#'        \item \eqn{\alpha_1, \alpha_0} are the first-stage efficacy and futility boundaries
#'        \item \eqn{\alpha_2(p_1)} is the optimal conditional error calculated for \eqn{p_1}
#'        \item \eqn{l(p_1)} is the "true" likelihood ratio under which to calculate the 
#'              expected sample size. This can be different from the likelihood ratio used 
#'              to calibrate the optimal conditional error function.
#'        \item \eqn{\Delta_1} is the assumed treatment effect to power for, expressed as a 
#'              mean difference. It may depend on the interim data (i.e., \eqn{p_1}) in case 
#'              \code{useInterimEstimate = TRUE} was specified for the design object.
#'        \item \eqn{\nu(\alpha_2(p_1)) = (\Phi^{-1}(1-\alpha_2(p_1))+\Phi^{-1}(CP))^2} is a 
#'              factor calculated for the specific assumptions about the optimal conditional 
#'              error function and the target conditional power \eqn{CP}.
#' }
#'
#' Add `design$firstStageInformation` to obtain expected total information.
#' This expectation is unconditional, not conditional on reaching stage two.
#' Changing the evaluation distribution here does not recalibrate the design.
#'
#' @examples
#' # Get a design
#' design <- getDesignOptimalConditionalError(
#'     alpha = 0.025, efficacyBounds = 0.001, futilityBounds = 0.5, 
#'     conditionalPower = 0.9, thetaH1 = 0.25, 
#'     likelihoodRatioDistribution = "fixed", thetaLR = 0.25,
#'     firstStageInformation = 80, useInterimEstimate = FALSE
#' )
#' # Calculate expected information under correct specification
#' getStageInformation(type = "expected", design = design)
#'
#' # Compare operating characteristics under a different true effect.
#' getStageInformation(type = "expected", design = design, 
#'     likelihoodRatioDistribution = "fixed", thetaLR = 0.15)
#'
#' # Calculate expected information under the null hypothesis
#' getStageInformation(type = "expected",
#'     design = design, likelihoodRatioDistribution = "fixed", thetaLR = 0
#' )
#'
#' @examples
#' # Required additional information given the interim results
#' getStageInformation(design, pValue = c(0.05, 0.1, 0.3))
#'
#' @seealso [getDesignOptimalConditionalError()], [getConditionalError()],
#'   [getFisherInformation()] for information at planned analyses of conventional designs.
#' @template reference_optimal
#' @template reference_monotone
#' @template reference_optconerrf
#' @export
getStageInformation <- function(design, stage = 2, type = c("conditional", "expected"),
        pValue = NULL, likelihoodRatioDistribution = NULL, ...) {
    .assertIsOptimalDesign(design)
    .assertIsOptimalDesignStage(stage, supportedStage = 2, functionName = "getStageInformation")
    if (missing(type)) type <- "conditional"
    .assertIsSingleCharacter(type, "type")
    if (!type %in% c("conditional", "expected")) {
        stopIllegalArgument(
            "'type' must be conditional or expected.", parameter = "type", value = type,
            constraint = "conditional or expected", functionName = "getStageInformation"
        )
    }
    if (type == "expected") {
        if (!is.null(pValue)) {
            stopConflictingArguments(
                "Expected stage information is unconditional; do not supply 'pValue'.",
                parameter = "pValue", value = pValue, relatedParameter = "type", relatedValue = type,
                constraint = "pValue must be NULL for type = expected", functionName = "getStageInformation"
            )
        }
        return(.getOptimalDesignExpectedInformation(design, likelihoodRatioDistribution, ...))
    }
    if (!is.null(likelihoodRatioDistribution) || length(list(...)) > 0) {
        stopConflictingArguments(
            "Evaluation distribution arguments are only used for expected information.",
            parameter = "likelihoodRatioDistribution", value = likelihoodRatioDistribution,
            relatedParameter = "type", relatedValue = type,
            constraint = "distribution arguments require type = expected", functionName = "getStageInformation"
        )
    }
    if (is.null(pValue)) {
        stopMissingArgument(
            "Specify 'pValue' for conditional stage information.", parameter = "pValue",
            constraint = "numeric p-values between 0 and 1", functionName = "getStageInformation"
        )
    }
    .assertIsNumericVector(pValue, "pValue")
    .assertIsInClosedInterval(pValue, "pValue", lower = 0, upper = 1)
    return(vapply(pValue, .getOptimalDesignStageInformation, numeric(1), design = design))
}
