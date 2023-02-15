#' @examples
#' # Calculate the stage-wise sample sizes, maximum sample sizes, and the optimum 
#' # allocation ratios for a range of pi1 values when testing 
#' # H0: pi1 - pi2 = -0.1 within a two-stage O'Brien & Fleming design;
#' # alpha = 0.05 one-sided, power 1 - beta = 90%:
#' getSampleSizeRates(getDesignGroupSequential(kMax = 2, alpha = 0.05,  
#'     beta = 0.1), groups = 2, thetaH0 = -0.1, pi1 = seq(0.4, 0.55, 0.025), 
#'     pi2 = 0.4, allocationRatioPlanned = 0)
#' \dontrun{
#' # Calculate the stage-wise sample sizes, maximum sample sizes, and the optimum 
#' # allocation ratios for a range of pi1 values when testing 
#' # H0: pi1 / pi2 = 0.80 within a three-stage O'Brien & Fleming design;
#' # alpha = 0.025 one-sided, power 1 - beta = 90%:
#' getSampleSizeRates(getDesignGroupSequential(kMax = 3, alpha = 0.025, 
#'     beta = 0.1), groups = 2, riskRatio = TRUE, thetaH0 = 0.80, 
#'     pi1 = seq(0.3, 0.5, 0.025), pi2 = 0.3, allocationRatioPlanned = 0)
#' }
#' 
