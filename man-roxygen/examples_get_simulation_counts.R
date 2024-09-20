#' @examples
#' \dontrun{
#' # Fixed sample size design with two groups, fixed exposure time
#' getSimulationCounts(
#'     theta = 1.8,
#'     lambda2 = 0.2,
#'     maxNumberOfSubjects = 200,
#'     plannedCalendarTime = 8,
#'     maxNumberOfIterations = 1000,
#'     fixedExposureTime = 6,
#'     accrualTime = 3,
#'     overdispersion = 2)
#' 
#' # Group sequential design alpha spending function design with O'Brien and 
#' # Fleming type boundaries: Power and test characteristics for N = 264, 
#' # under variable exposure time with uniform recruitment over 1.25 months,
#' # study time (accrual + followup) = 4, interim analysis take place after
#' # equidistant time points (lambda1, lambda2, and overdispersion as specified,
#' # no futility stopping):
#' dOF <- getDesignGroupSequential(
#'     kMax = 3,
#'     alpha = 0.025,
#'     beta = 0.2,
#'     typeOfDesign = "asOF")
#' 
#' getSimulationCounts(design = dOF,
#'     lambda1 = seq(0.04, 0.12, 0.02),
#'     lambda2 = 0.12,
#'     directionUpper = FALSE,
#'     overdispersion = 5,
#'     plannedCalendarTime = (1:3)/3*4,
#'     maxNumberOfSubjects = 264,
#'     followUpTime = 2.75,
#'     accrualTime = 1.25,
#'     maxNumberOfIterations = 1000)
#' }
#' 
