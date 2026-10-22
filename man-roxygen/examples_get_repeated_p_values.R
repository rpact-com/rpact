#' @examples
#' \dontrun{
#' design <- getDesignInverseNormal(kMax = 2)
#' data <- getDataset(
#'     n      = c( 20,  30),
#'     means  = c( 50,  51),
#'     stDevs = c(130, 140)
#' )
#' getRepeatedPValues(getStageResults(design, dataInput = data))
#'
#' # User-defined cumulative spending: keep spending proportions fixed
#' # when varying alpha to calculate repeated p-values.
#' userDesign <- getDesignGroupSequential(
#'     kMax = 2, typeOfDesign = "asUser",
#'     userAlphaSpending = c(0.005, 0.025)
#' )
#' generalData <- getDataset(est = c(2, 1), se = c(1, 1))
#' getRepeatedPValues(getStageResults(userDesign, generalData))
#'
#' # Optimize the Wang-Tsiatis shape once, then keep deltaWT fixed
#' # when varying alpha to calculate repeated p-values.
#' optimumDesign <- getDesignGroupSequential(
#'     kMax = 2, typeOfDesign = "WToptimum", optimizationCriterion = "ASNsum"
#' )
#' getRepeatedPValues(getStageResults(optimumDesign, generalData))
#' }
#'
