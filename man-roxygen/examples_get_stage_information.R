#' @examples
#' \dontrun{
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
#' # Required additional information given the interim results
#' getStageInformation(design, pValue = c(0.05, 0.1, 0.3))
#' }
#' 
