#' @examples
#' \dontrun{
#' # In a four-stage combination test design with O'Brien & Fleming boundaries 
#' # at the first stage the second treatment arm was dropped. With the Bonferroni 
#' # intersection test, the results of a closed adaptive test procedure are 
#' # obtained as follows with the given data (treatment arm 4 refers to the 
#' # reference group):
#' data <- getDataset(
#'     n1 = c(22, 23),
#'     n2 = c(21, NA),
#'     n3 = c(20, 25),
#'     n4 = c(25, 27),
#'     means1 = c(1.63, 1.51),
#'     means2 = c(1.4, NA),
#'     means3 = c(0.91, 0.95),
#'     means4 = c(0.83, 0.75),
#'     stds1 = c(1.2, 1.4),
#'     stds2 = c(1.3, NA),
#'     stds3 = c(1.1, 1.14),
#'     stds4 = c(1.02, 1.18))
#' 
#' design <- getDesignInverseNormal(kMax = 4)
#' stageResults <- getStageResults(design, dataInput = data, 
#'     intersectionTest = "Bonferroni")  
#' getClosedCombinationTestResults(stageResults)
#' }
#' 
