#' @examples
#' \dontrun{
#' # Calculate CRP for a Fisher's combination test design with 
#' # two remaining stages and check the results by simulation.
#' design <- getDesignFisher(kMax = 4, 
#'     informationRates = c(0.1, 0.3, 0.8, 1), alpha = 0.01)
#' data <- getDataset(n = c(40, 40), events = c(20, 22))
#' sr <- getStageResults(design, data, thetaH0 = 0.4)
#' getConditionalRejectionProbabilities(sr)
#' getConditionalRejectionProbabilities(sr, simulateCRP = TRUE, 
#'     seed = 12345, iterations = 10000) 
#' }
#' 
