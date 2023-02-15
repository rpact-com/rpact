#' @examples
#' design <- getDesignInverseNormal(kMax = 2)
#' data <- getDataset(
#'     n      = c( 20,  30),
#'     means  = c( 50,  51),
#'     stDevs = c(130, 140)
#' )
#' getTestActions(getStageResults(design, dataInput = data))
#' 
