#' @examples 
#' \dontrun{
#' data <- getDataset(
#'    n1     = c(22, 13, 22, 13),
#'    n2     = c(22, 11, 22, 11),  
#'    means1 = c(1, 1.1, 1, 1),
#'    means2 = c(1.4, 1.5, 1, 2.5), 
#'    stds1  = c(1, 2, 2, 1.3),
#'    stds2  = c(1, 2, 2, 1.3))
#' stageResults <- getStageResults(
#'    getDesignGroupSequential(kMax = 4), 
#'    dataInput = data, stage = 2, directionUpper = FALSE) 
#' getConditionalPower(stageResults, thetaH1 = -0.4, 
#'    nPlanned = c(64, 64), assumedStDev = 1.5, allocationRatioPlanned = 3)
#' }
#' 
