#' @examples
#' \dontrun{
#' # Calculate sample sizes in a fixed sample size parallel group design 
#' # with allocation ratio \code{n1 / n2 = 2} for a range of 
#' # alternative values 1, ..., 5 with assumed standard deviation = 3.5; 
#' # two-sided alpha = 0.05, power 1 - beta = 90%:
#' getSampleSizeMeans(alpha = 0.05, beta = 0.1, sided = 2, groups = 2, 
#'     alternative = seq(1, 5, 1), stDev = 3.5, allocationRatioPlanned = 2)
#' 
#' # Calculate sample sizes in a three-stage Pocock paired comparison design testing 
#' # H0: mu = 2 for a range of alternative values 3,4,5 with assumed standard 
#' # deviation = 3.5; one-sided alpha = 0.05, power 1 - beta = 90%:
#' getSampleSizeMeans(getDesignGroupSequential(typeOfDesign = "P", alpha = 0.05, 
#'     sided = 1, beta = 0.1), groups = 1, thetaH0 = 2, 
#'     alternative = seq(3, 5, 1), stDev = 3.5)
#'     
#' # Calculate sample sizes in a three-stage Pocock two-armed design testing 
#' # H0: mu = 2 for a range of alternative values 3,4,5 with assumed standard 
#' # deviations = 3 and 4, respectively, in the two groups of observations; 
#' # one-sided alpha = 0.05, power 1 - beta = 90%:
#' getSampleSizeMeans(getDesignGroupSequential(typeOfDesign = "P", alpha = 0.05, 
#'     sided = 1, beta = 0.1), groups = 2,
#'     alternative = seq(3, 5, 1), stDev = c(3, 4))
#' }
#' 
