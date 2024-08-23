#' @examples 
#' \dontrun{
#' # Calculate event probabilities for staggered subjects' entry, piecewisely defined
#' # survival time and hazards, and plot it.
#' timeVector <- seq(0, 100, 1)
#' y <- getEventProbabilities(timeVector, accrualTime = c(0, 20, 60), 
#'     accrualIntensity = c(5, 20), 
#'     piecewiseSurvivalTime = c(0, 20, 80),
#'     lambda2 = c(0.02, 0.06, 0.1), 
#'     hazardRatio = 2
#' )
#' plot(timeVector, y$cumulativeEventProbabilities, type = 'l')
#' }
