#' @examples 
#' \dontrun{
#' design <- getDesignGroupSequential(kMax = 2)
#' 
#' dataExample <- getDataset(
#'     n = c(20, 30),
#'     means = c(50, 51),
#'     stDevs = c(130, 140)
#' )
#' 
#' result <- getAnalysisResults(design = design, 
#'     dataInput = dataExample, thetaH0 = 20, 
#'     nPlanned = c(30), thetaH1 = 1.5, stage = 1) 
#' 
#' if (require(ggplot2)) plot(result, thetaRange = c(0, 100))
#' }
#'
