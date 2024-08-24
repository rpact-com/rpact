#' @examples 
#' \dontrun{
#' getPiecewiseSurvivalTime(lambda2 = 0.5, hazardRatio = 0.8)
#' 
#' getPiecewiseSurvivalTime(lambda2 = 0.5, lambda1 = 0.4)
#' 
#' getPiecewiseSurvivalTime(pi2 = 0.5, hazardRatio = 0.8)
#' 
#' getPiecewiseSurvivalTime(pi2 = 0.5, pi1 = 0.4)
#' 
#' getPiecewiseSurvivalTime(pi1 = 0.3)
#' 
#' getPiecewiseSurvivalTime(hazardRatio = c(0.6, 0.8), lambda2 = 0.4)
#' 
#' getPiecewiseSurvivalTime(piecewiseSurvivalTime = c(0, 6, 9), 
#'     lambda2 = c(0.025, 0.04, 0.015), hazardRatio = 0.8)
#' 
#' getPiecewiseSurvivalTime(piecewiseSurvivalTime = c(0, 6, 9), 
#'     lambda2 = c(0.025, 0.04, 0.015), 
#'     lambda1 = c(0.025, 0.04, 0.015) * 0.8)
#' 
#' pwst <- getPiecewiseSurvivalTime(list(
#'     "0 - <6"   = 0.025, 
#'     "6 - <9"   = 0.04, 
#'     "9 - <15"  = 0.015, 
#'     "15 - <21" = 0.01, 
#'     ">=21"     = 0.007), hazardRatio = 0.75)
#' pwst
#' 
#' # The object created by getPiecewiseSurvivalTime() can be used directly in 
#' # getSampleSizeSurvival():
#' getSampleSizeSurvival(piecewiseSurvivalTime = pwst)
#' 
#' # The object created by getPiecewiseSurvivalTime() can be used directly in 
#' # getPowerSurvival():
#' getPowerSurvival(piecewiseSurvivalTime = pwst, 
#'     maxNumberOfEvents = 40, maxNumberOfSubjects = 100)
#' }
#'  
