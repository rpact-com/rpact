#' @examples
#' # Fixed sample size trial where a therapy is assumed to decrease the 
#' # exacerbation rate from 1.4 to 1.05 (25% decrease) within an observation 
#' # period of 1 year, i.e., each subject has an equal follow-up of 1 year.
#' # The sample size that yields 90% power at significance level 0.025 for 
#' # detecting such a difference, if the overdispersion is assumed to be 
#' # equal to 0.5, is obtained by
#' getSampleSizeCounts(alpha = 0.025, beta = 0.1, lambda2 = 1.4, 
#'     theta = 0.75, overDispersion = 0.5, fixedExposureTime = 1)
#' \dontrun{
#' # Noninferiority test with blinded sample size reassessment to reproduce 
#' # Table 2 from Friede and Schmidli (2010):
#' getSampleSizeCounts(alpha = 0.025, beta = 0.2, lambda = 1, theta = 1,
#'     thetaH0 = 1.15, overDispersion = 0.4, fixedExposureTime = 1)
#' 
#' # Group sequential alpha and beta spending function design with O'Brien and 
#' # Fleming type boundaries: Estimate observation time under uniform 
#' # recruitment of patients over 6 months and a fixed exposure time of 12 
#' # months (lambda1, lambda2, and overdispersion as specified):
#' getSampleSizeCounts(design = getDesignGroupSequential(
#'         kMax = 3, alpha = 0.025, beta = 0.2, 
#'         typeOfDesign = "asOF", typeBetaSpending = "bsOF"), 
#'     lambda1 = 0.2, lambda2 = 0.3, overDispersion = 1, 
#'     fixedExposureTime = 12, accrualTime = 6)
#' 
#' # Group sequential alpha spending function design with O'Brien and Fleming 
#' # type boundaries: Sample size for variable exposure time with uniform 
#' # recruitment over 1.25 months and study time (accrual + followup) = 4 
#' # (lambda1, lambda2, and overdispersion as specified, no futility stopping):
#' getSampleSizeCounts(design = getDesignGroupSequential(
#'         kMax = 3, alpha = 0.025, beta = 0.2, typeOfDesign = "asOF"),
#'     lambda1 = 0.0875, lambda2 = 0.125, overDispersion = 5, 
#'     followUpTime = 2.75, accrualTime = 1.25)
#' }
#' 
