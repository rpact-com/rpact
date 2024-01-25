#' @examples
#' # Fixed sample size trial where a therapy is assumed to decrease the 
#' # exacerbation rate from 1.4 to 1.05 (25% decrease) within an 
#' # observation period of 1 year, i.e., each subject has a equal 
#' # follow-up of 1 year.
#' # Calculate power at significance level 0.025 at given sample size = 180 
#' # for a range of lambda1 values if the overdispersion is assumed to be 
#' # equal to 0.5, is obtained by
#' getPowerCounts(alpha = 0.025, lambda1 = seq(1, 1.4, 0.05), lambda2 = 1.4, 
#'     maxNumberOfSubjects = 180, overdispersion = 0.5, fixedExposureTime = 1)
#' \dontrun{
#' # Group sequential alpha and beta spending function design with O'Brien and 
#' # Fleming type boundaries: Power and test characteristics for N = 286, 
#' # under the assumption of a fixed exposure time, and for a range of 
#' # lambda1 values: 
#' getPowerCounts(design = getDesignGroupSequential(
#'         kMax = 3, alpha = 0.025, beta = 0.2, 
#'         typeOfDesign = "asOF", typeBetaSpending = "bsOF"), 
#'     lambda1 = seq(0.17, 0.23, 0.01), lambda2 = 0.3, 
#'     directionUpper = FALSE, overdispersion = 1, maxNumberOfSubjects = 286, 
#'     fixedExposureTime = 12, accrualTime = 6)
#' 
#' # Group sequential design alpha spending function design with O'Brien and 
#' # Fleming type boundaries: Power and test characteristics for N = 1976, 
#' # under variable exposure time with uniform recruitment over 1.25 months,
#' # study time (accrual + followup) = 4 (lambda1, lambda2, and overdispersion 
#' # as specified, no futility stopping):
#' getPowerCounts(design = getDesignGroupSequential(
#'         kMax = 3, alpha = 0.025, beta = 0.2, typeOfDesign = "asOF"),
#'     lambda1 = seq(0.08, 0.09, 0.0025), lambda2 = 0.125, 
#'     overdispersion = 5, directionUpper = FALSE, maxNumberOfSubjects = 1976, 
#'     followUpTime = 2.75, accrualTime = 1.25)
#' }
#' 
