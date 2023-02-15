#' @examples
#' # Calculate two-sided critical values for a four-stage 
#' # Wang & Tsiatis design with Delta = 0.25 at level alpha = 0.05
#' getDesignGroupSequential(kMax = 4, alpha = 0.05, sided = 2, 
#'     typeOfDesign = "WT", deltaWT = 0.25) 
#' 
#' \dontrun{
#' # Calculate one-sided critical values and binding futility bounds for a three-stage 
#' # design with alpha- and beta-spending functions according to Kim & DeMets with gamma = 2.5
#' # (planned informationRates as specified, default alpha = 0.025 and beta = 0.2)
#' getDesignGroupSequential(kMax = 3, informationRates = c(0.3, 0.75, 1), 
#'     typeOfDesign = "asKD", gammaA = 2.5, typeBetaSpending = "bsKD", 
#'     gammaB = 2.5, bindingFutility = TRUE)
#' } 
#' 
#' # Calculate the Pocock type alpha spending critical values if the first 
#' # interim analysis was performed after 40% of the maximum information was observed
#' # and the second after 70% of the maximum information was observed (default alpha = 0.025)
#' getDesignGroupSequential(informationRates = c(0.4, 0.7), typeOfDesign = "asP") 
#' 
