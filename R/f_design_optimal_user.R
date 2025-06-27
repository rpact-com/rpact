#' Create a design object for the optimal conditional error function.
#' @name getDesignOptimalConditionalErrorFunction
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
#'    \item \code{likelihoodRatioDistribution="exp"}: calculates the likelihood ratio for an exponentially distributed prior of \eqn{\vartheta} with mean \code{kappaLR}*\code{sqrt(firstStageInformation)} (\eqn{\eta}). The likelihood ratio is then calculated as:
#'          \deqn{l(p_1) = \kappa \cdot \sqrt{2\pi} \cdot e^{(\Phi^{-1}(1-p_1)-\eta)^2/2} \cdot \Phi(\Phi^{-1}(1-p_1)-\eta)}
#'    \item \code{likelihoodRatioDistribution="unif"}: calculates the likelihood ratio for a uniformly distributed prior of \eqn{\vartheta} on the support \eqn{[0, \Delta\cdot\sqrt{I_1}]}, where \eqn{\Delta} is specified as \code{deltaMaxLR} and \eqn{I_1} is the \code{firstStageInformation}.
#'          \deqn{l(p_1) = \frac{\sqrt{2\pi}}{\Delta\cdot\sqrt{I_1}} \cdot e^{\Phi^{-1}(1-p_1)^2/2} \cdot (\Phi(\Delta\cdot\sqrt{I_1} - \Phi^{-1}(1-p_1))-p_1)}
#'    \item \code{likelihoodRatioDistribution="maxlr"}: the non-centrality parameter \eqn{\vartheta} is estimated from the data and no additional parameters must be specified. The likelihood ratio is estimated from the data as:
#'          \deqn{l(p_1) = e^{max(0, \Phi^{-1}(1-p_1))^2/2}}
#'          The maximum likelihood ratio is always restricted to effect sizes \eqn{\vartheta \geq 0} (corresponding to \eqn{p_1 \leq 0.5}).
#' }
#'
#' @section Effect for conditional power:
#' For the treatment effect at which the target conditional power should be achieved, either a fixed effect or an interim estimate can be used.
#' The usage of a fixed effect is indicated by setting \code{useInterimEstimate=FALSE}, in which case the fixed effect is provided by \code{delta1} on the mean difference scale or by \code{ncp1} on the non-centrality parameter scale (i.e., \code{delta1*sqrt(firstStageInformation)}).
#' For an interim estimate, specified by \code{useInterimEstimate=TRUE}, a lower cut-off for the interim estimate must be provided, either by \code{delta1Min} on the mean difference scale, or \code{ncp1Min} on the non-centrality parameter scale.
#' In addition, an upper limit of the estimate may be analogously provided by \code{delta1Max} or \code{ncp1Max}.
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
#' By default, the optimal conditional error function returned by \code{getDesignOptimalConditionalErrorFunction()} is transformed to be non-increasing in the first-stage p-value \eqn{p_1} if found to be increasing on any interval.
#' The necessary intervals and constants for the transformation are calculated by \code{getMonotonisationConstants()}.
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
#' The level constant is determined by the helper function \code{getLevelConstant()}. It is identified using the \code{uniroot()} function and by default, the interval between 0 and 10 is searched for the level constant.
#' In specific settings, the level constant may lie outside of this interval. In such cases, the search interval can be changed by altering the parameters \code{levelConstantMinimum} and \code{levelConstantMaximum}. \cr
#' If inappropriate constraints to the optimal conditional error function are provided via \code{minimumConditionalError} and \code{maximumConditionalError}
#' or \code{minimumSecondStageInformation} and \code{maximumSecondStageInformation}, it may be impossible to find a level constant which exhausts the full alpha level.
#'
#' @section Generic functions:
#' The \code{print()} and \code{plot()} functions are available for objects of class \code{TrialDesignOptimalConditionalError}.
#' For details, see \code{?print.TrialDesignOptimalConditionalError} and \code{?plot.TrialDesignOptimalConditionalError}.
#'
#' @template param_alpha
#' @template param_alpha1
#' @template param_alpha0
#' @template param_conditionalPower
#' @template param_conditionalPowerFunction
#' @template param_ncp1
#' @template param_ncp1Min
#' @template param_ncp1Max
#' @template param_delta1
#' @template param_delta1Min
#' @template param_delta1Max
#' @template param_likelihoodRatioDistribution
#' @template param_firstStageInformation
#' @template param_useInterimEstimate
#' @template param_minimumSecondStageInformation
#' @template param_maximumSecondStageInformation
#' @template param_minimumConditionalError
#' @template param_maximumConditionalError
#' @template param_levelConstantMinimum
#' @template param_levelConstantMaximum
#' @template param_enforceMonotonicity
#' @param ... Additional arguments required for the specification of the likelihood ratio.
#'
#' @importFrom methods new
#'
#' @return An object of class \code{TrialDesignOptimalConditionalError}, which can be passed to other package functions.
#'
#' @examples
#' \dontrun{
#' # Create a single-arm design with fixed parameter for the likelihood ratio
#' # and a fixed effect for conditional power. 80 patients are observed in the
#' # first-stage (firstStageInformation = 80 in the one-sample test, variance 1).
#' # The second-stage information is restricted to be between 40 and 160.
#' getDesignOptimalConditionalErrorFunction(
#'   alpha = 0.025, alpha1 = 0.001, alpha0 = 0.5, conditionalPower = 0.9,
#'   delta1 = 0.25, likelihoodRatioDistribution = "fixed", deltaLR = 0.25,
#'   firstStageInformation = 80, useInterimEstimate = FALSE,
#'   minimumSecondStageInformation = 40, maximumSecondStageInformation = 160
#' )
#'
#' # Create a design comparing two groups using the maximum likelihood ratio
#' # and an interim estimate for the effect for conditional power.
#' # 160 patients per arm are observed in the first stage
#' # (firstStageInformation = 80 in the balanced two-sample test, variance 1).
#' getDesignOptimalConditionalErrorFunction(
#'   alpha = 0.025, alpha1 = 0.001, alpha0 = 0.5, conditionalPower = 0.9,
#'   delta1Min = 0.25, likelihoodRatioDistribution = "maxlr",
#'   firstStageInformation = 80, useInterimEstimate = TRUE
#' )
#' }
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
    ncp1 = NA_real_,
    ncp1Min = NA_real_,
    ncp1Max = Inf,
    useInterimEstimate = TRUE,
    firstStageInformation,
    likelihoodRatioDistribution,
    minimumSecondStageInformation = 0,
    maximumSecondStageInformation = Inf,
    minimumConditionalError = 0,
    maximumConditionalError = 1,
    conditionalPowerFunction = NA,
    levelConstantMinimum = 0,
    levelConstantMaximum = 10,
    enforceMonotonicity = TRUE,
    ...
) {
    design <- new(
        "TrialDesignOptimalConditionalError",
        alpha = alpha,
        alpha1 = alpha1,
        alpha0 = alpha0,
        conditionalPower = conditionalPower,
        delta1 = delta1,
        delta1Min = delta1Min,
        delta1Max = delta1Max,
        ncp1 = ncp1,
        ncp1Min = ncp1Min,
        ncp1Max = ncp1Max,
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
        ... = ...
    )

    return(design)
}


#' Calculate Expected Second-stage Information
#' @name getExpectedSecondStageInformation
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
#' @template param_design
#' @template param_likelihoodRatioDistribution_expected
#' @param ... {Additional parameters required for the specification of \code{likelihoodRatioDistribution}}.
#'
#' @return Expected second-stage information.
#'
#' @examples
#' \dontrun{
#' # Get a design
#' design <- getDesignOptimalConditionalErrorFunction(
#' alpha = 0.025, alpha1 = 0.001, alpha0 = 0.5, conditionalPower = 0.9,
#' delta1 = 0.25, likelihoodRatioDistribution = "fixed", deltaLR = 0.25,
#' firstStageInformation = 80, useInterimEstimate = FALSE,
#' )
#' # Calculate expected information under correct specification
#' getExpectedSecondStageInformation(design)
#'
#' # Calculate expected information under the null hypothesis
#' getExpectedSecondStageInformation(
#'  design = design, likelihoodRatioDistribution = "fixed", deltaLR = 0
#' )
#' }
#'
#' @export
#' @seealso [getDesignOptimalConditionalErrorFunction()], [getSecondStageInformation()]
#' @template reference_optimal

getExpectedSecondStageInformation <- function(design, likelihoodRatioDistribution = NULL, ...) {
    # Integrate over a helper function from alpha1 to alpha0
    return(
        stats::integrate(
            f = .integrateExpectedInformation,
            lower = design$alpha1,
            upper = design$alpha0,
            design = design,
            likelihoodRatioDistribution = likelihoodRatioDistribution,
            ... = ...
        )$value
    )
}

#' Calculate the Optimal Conditional Error
#' @name getOptimalConditionalError
#'
#' @details The optimal conditional error \eqn{\alpha_2} given a first-stage p-value \eqn{p_1} is calculated as:
#' \deqn{\alpha_2(p_1)=\psi(-e^{c_0} \cdot \frac{\Delta_1^2}{l(p_1)}).}
#'
#' The level constant \eqn{c_0} as well as the specification of the effect size \eqn{\Delta_1} and the likelihood ratio \eqn{l(p_1)}
#' must be contained in the \code{design} object (see \code{?getDesignOptimalConditionalErrorFunction}).
#' Early stopping rules are supported, i.e., for \eqn{p_1 \leq \alpha_1}, the returned conditional error is 1 and for \eqn{p_1 > \alpha_0}, the returned conditional error is 0.
#'
#'
#' @template param_firstStagePValue
#' @template param_design
#'
#' @return Value of the optimal conditional error function.
#' @export
#'
#' @template reference_optimal
#'
#' @seealso [getDesignOptimalConditionalErrorFunction()]
#'
#' @examples
#' \dontrun{
#' # Create a design
#' design <- getDesignOptimalConditionalErrorFunction(
#' alpha = 0.025, alpha1 = 0.001, alpha0 = 0.5, conditionalPower = 0.9,
#' delta1 = 0.5, firstStageInformation = 40, useInterimEstimate = FALSE,
#' likelihoodRatioDistribution = "fixed", deltaLR = 0.5)
#'
#' # Calculate optimal conditional error
#' getOptimalConditionalError(
#' firstStagePValue = c(0.1, 0.2, 0.3), design = design
#' )
#' }

getOptimalConditionalError <- function(firstStagePValue, design) {
    conditionalErrorWithConstraints <- NULL

    # Check if firstStagePValue lies outside early decision boundaries
    if (firstStagePValue <= design$alpha1 && design$alpha1 != 0) {
        conditionalErrorWithConstraints <- 1
    } else if (firstStagePValue > design$alpha0) {
        conditionalErrorWithConstraints <- 0
    } else {
        # If monotonisation constants specified and monotonisation enforced, perform non-increasing transformation
        if (design$enforceMonotonicity && !is.null(unlist(design$monotonisationConstants))) {
            likelihoodRatioOverEffect <- getMonotoneFunction(
                x = firstStagePValue,
                fun = getQ,
                design = design
            )
        } else {
            likelihoodRatioOverEffect <- getQ(firstStagePValue = firstStagePValue, design = design)
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

        #Handling of the special case firstStagePValue=0 and no early stopping
        if (firstStagePValue == 0 && design$alpha1 == 0) {
            # Calculate the specified conditional power for a firstStagePValue of 0
            if (!is.null(suppressWarnings(body(design$conditionalPowerFunction)))) {
                conditionalPower0 <- design$conditionalPowerFunction(0)
            } else {
                conditionalPower0 <- design$conditionalPower
            }
            return(min(conditionalErrorConstraintUpper, conditionalPower0))
        }

        conditionalErrorWithConstraints <- max(
            conditionalErrorConstraintLower,
            min(
                conditionalErrorConstraintUpper,
                getPsi(
                    nuPrime = (-exp(design$levelConstant) / likelihoodRatioOverEffect),
                    conditionalPower = conditionalPower
                )
            )
        )
    }

    return(conditionalErrorWithConstraints)
}

getOptimalConditionalError <- Vectorize(FUN = getOptimalConditionalError, vectorize.args = c("firstStagePValue"))

#' Calculate the overall power
#' @name getOverallPower
#'
#' @description Calculate the overall power and other operating characteristics of a design.
#'
#' @details
#' This function is used to evaluate the overall performance of a design.
#' The probabilities for first-stage futility, first-stage efficacy and overall efficacy (i.e., overall power) are saved in an object of class \code{PowerResultsOptimalConditionalError}.
#'
#' @template param_design
#' @template param_alternative
#'
#' @seealso [getDesignOptimalConditionalErrorFunction()], [getSimulationResults()]
#'
#' @return The overall power of the design at the provided effect size.
#' @export

getOverallPower <- function(design, alternative) {
    .assertIsNumericVector(x = alternative, argumentName = "alternative")

    alternativeNonCentralityParameterScale <- alternative * base::sqrt(design$firstStageInformation)

    # One value per entry in alternative
    firstStageFutility <- numeric(length(alternative))
    firstStageEfficacy <- numeric(length(alternative))
    overallPower <- numeric(length(alternative))

    for (i in 1:length(alternative)) {
        # Early decision probabilities
        firstStageFutility[i] <- stats::pnorm(
            stats::qnorm(1 - design$alpha0) - alternativeNonCentralityParameterScale[i]
        )
        firstStageEfficacy[i] <- 1 -
            stats::pnorm(stats::qnorm(1 - design$alpha1) - alternativeNonCentralityParameterScale[i])

        # Calculate probability to reject at the second stage for given delta
        secondStageRejection <- function(firstStagePValue) {
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

        integral <- stats::integrate(f = secondStageRejection, lower = design$alpha1, upper = design$alpha0)$value

        overallPower[i] <- firstStageEfficacy[i] + integral
    }

    powerResults <- new(
        "PowerResultsOptimalConditionalError",
        alternative = alternative,
        firstStageFutility = firstStageFutility,
        firstStageEfficacy = firstStageEfficacy,
        overallPower = overallPower
    )

    return(powerResults)
}

#' Calculate the Second-stage Information
#' @name getSecondStageInformation
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
#'
#' @template param_firstStagePValue
#' @template param_design
#'
#' @return The second-stage information.
#' @export
#'
#' @seealso [getDesignOptimalConditionalErrorFunction()], [getExpectedSecondStageInformation()], [getOptimalConditionalError()]
#'
#' @examples
#' \dontrun{
#' design <- getDesignOptimalConditionalErrorFunction(
#'   alpha = 0.025, alpha1 = 0.001, alpha0 = 0.5,
#'   conditionalPower = 0.9, delta1 = 0.25, useInterimEstimate = FALSE,
#'   firstStageInformation = 40, likelihoodRatioDistribution = "maxlr"
#' )
#'
#' getSecondStageInformation(
#'   firstStagePValue = c(0.05, 0.1, 0.2), design = design
#' )
#' }
#'
#' @template reference_optimal
getSecondStageInformation <- function(firstStagePValue, design) {
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
                max(qnorm(1 - firstStagePValue) / design$firstStageInformation, design$delta1Min),
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
        if (!is.null(suppressWarnings(body(design$conditionalPowerFunction)))) {
            conditionalPower <- design$conditionalPowerFunction(firstStagePValue)
        } else {
            conditionalPower <- design$conditionalPower
        }

        secondStageInformation <- (getNu(alpha = conditionalError, conditionalPower = conditionalPower)) / (effect^2)
    }
    return(secondStageInformation)
}

getSecondStageInformation <- Vectorize(
    FUN = getSecondStageInformation,
    vectorize.args = c("firstStagePValue")
)
