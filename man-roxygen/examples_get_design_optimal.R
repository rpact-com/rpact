#' @examples
#' \dontrun{
#' # Create a design
#' design <- getDesignOptimalConditionalError(
#'     alpha = 0.025, efficacyBounds = 0.001, futilityBounds = 0.5, conditionalPower = 0.9,
#'     thetaH1 = 0.5, firstStageInformation = 40, useInterimEstimate = FALSE,
#'     likelihoodRatioDistribution = "fixed", thetaLR = 0.5
#' )
#'
#' # Early efficacy gives 1, continuation gives the stage-two threshold,
#' # and binding futility gives 0.
#' getOptimalConditionalError(
#'     pValue = c(0.0005, 0.1, 0.3, 0.8), design = design
#' )
#' }
#' 
