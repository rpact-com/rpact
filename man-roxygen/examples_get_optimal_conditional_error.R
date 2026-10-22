#' @examples
#' \dontrun{
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
#' getOptimalConditionalError(pValue = c(0.05, 0.1, 0.3), design = flexibleDesign)
#' }
#' 
