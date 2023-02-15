#' @examples
#' # Calculate the power, stopping probabilities, and expected sample size in a
#' # two-armed design at given maximum sample size N = 200 in a three-stage 
#' # O'Brien & Fleming design with information rate vector (0.2,0.5,1), 
#' # non-binding futility boundaries (0,0), i.e., the study stops for futility 
#' # if the p-value exceeds 0.5 at interm, and allocation ratio = 2 for a range 
#' # of pi1 values when testing H0: pi1 - pi2 = -0.1:
#' getPowerRates(getDesignGroupSequential(informationRates = c(0.2, 0.5, 1), 
#'     futilityBounds = c(0, 0)), groups = 2, thetaH0 = -0.1, 
#'     pi1 = seq(0.3, 0.6, 0.1), directionUpper = FALSE, 
#'     pi2 = 0.7, allocationRatioPlanned = 2, maxNumberOfSubjects = 200)
#' \dontrun{
#' # Calculate the power, stopping probabilities, and expected sample size in a single 
#' # arm design at given maximum sample size N = 60 in a three-stage two-sided 
#' # O'Brien & Fleming design with information rate vector (0.2, 0.5,1) 
#' # for a range of pi1 values when testing H0: pi = 0.3:
#' getPowerRates(getDesignGroupSequential(informationRates = c(0.2, 0.5,1), 
#'     sided = 2), groups = 1, thetaH0 = 0.3, pi1 = seq(0.3, 0.5, 0.05),  
#'     maxNumberOfSubjects = 60)
#' }
#' 
