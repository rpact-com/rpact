#' @examples
#' \dontrun{
#' # Calculate two-sided critical values for a four-stage 
#' # Wang & Tsiatis design with Delta = 0.25 at level alpha = 0.05
#' getDesignInverseNormal(kMax = 4, alpha = 0.05, sided = 2, 
#'     typeOfDesign = "WT", deltaWT = 0.25) 
#' 
#' # Defines a two-stage design at one-sided alpha = 0.025 with provision of early stopping  
#' # if the one-sided p-value exceeds 0.5 at interim and no early stopping for efficacy. 
#' # The futility bound is non-binding.
#' getDesignInverseNormal(kMax = 2, typeOfDesign = "noEarlyEfficacy", futilityBounds = 0)  
#' 
#' # Calculate one-sided critical values and binding futility bounds for a three-stage 
#' # design with alpha- and beta-spending functions according to Kim & DeMets with gamma = 2.5
#' # (planned informationRates as specified, default alpha = 0.025 and beta = 0.2)
#' getDesignInverseNormal(kMax = 3, informationRates = c(0.3, 0.75, 1), 
#'     typeOfDesign = "asKD", gammaA = 2.5, typeBetaSpending = "bsKD", 
#'     gammaB = 2.5, bindingFutility = TRUE)
#' } 
#' 
