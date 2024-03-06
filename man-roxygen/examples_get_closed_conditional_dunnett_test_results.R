#' @examples
#' \dontrun{
#' # In a two-stage design a conditional Dunnett test should be performed
#' # where the  unconditional second stage p-values should be used for the
#' # test decision.
#' # At the first stage the second treatment arm was dropped. The results of
#' # a closed conditionsal Dunnett test are obtained as follows with the given
#' # data (treatment arm 4 refers to the reference group):
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
#'     stds4 = c(1.02, 1.18)
#' )
#'
#' # For getting the results of the closed test procedure, use the following commands:
#' design <- getDesignConditionalDunnett(secondStageConditioning = FALSE)
#' stageResults <- getStageResults(design, dataInput = data)
#' getClosedConditionalDunnettTestResults(stageResults)
#' }
#'
