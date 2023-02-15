#' @examples
#' # Calculate the power, stopping probabilities, and expected sample size 
#' # for testing H0: mu1 - mu2 = 0 in a two-armed design against a range of 
#' # alternatives H1: mu1 - m2 = delta, delta = (0, 1, 2, 3, 4, 5), 
#' # standard deviation sigma = 8, maximum sample size N = 80 (both treatment 
#' # arms), and an allocation ratio n1/n2 = 2. The design is a three stage 
#' # O'Brien & Fleming design with non-binding futility bounds (-0.5, 0.5) 
#' # for the two interims. The computation takes into account that the t test 
#' # is used (normalApproximation = FALSE). 
#' getPowerMeans(getDesignGroupSequential(alpha = 0.025, 
#'     sided = 1, futilityBounds = c(-0.5, 0.5)), 
#'     groups = 2, alternative = c(0:5), stDev = 8,
#'     normalApproximation = FALSE, maxNumberOfSubjects = 80, 
#'     allocationRatioPlanned = 2)
#' 
