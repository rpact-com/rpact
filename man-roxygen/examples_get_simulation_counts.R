#' @examples
#' \dontrun{
#' # Fixed sample size design with two groups
#' getSimulationCounts(
#'     plannedMaxSubjects = 200,
#'     plannedCalendarTime = 8,
#'     theta = 1.8,
#'     lambda2 = 0.2,
#'     maxNumberOfIterations = 1000,
#'     fixedExposureTime = 6,
#'     accrualTime = 3,
#'     overdispersion = 2)
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
