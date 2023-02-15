#' @examples 
#' # show output format of p values
#' getOutputFormat("p.value")
#' \dontrun{
#' # set new p value output format
#' setOutputFormat("p.value", digits = 5, nsmall = 5)
#' 
#' # show sample sizes as smallest integers not less than the not rounded values
#' setOutputFormat("sample size", digits = 0, nsmall = 0, roundFunction = "ceiling")
#' getSampleSizeMeans()
#' 
#' # show sample sizes as smallest integers not greater than the not rounded values
#' setOutputFormat("sample size", digits = 0, nsmall = 0, roundFunction = "floor")
#' getSampleSizeMeans()
#' 
#' # set new sample size output format without round function
#' setOutputFormat("sample size", digits = 2, nsmall = 2)
#' getSampleSizeMeans()
#' 
#' # reset sample size output format to default
#' setOutputFormat("sample size")
#' getSampleSizeMeans()
#' getOutputFormat("sample size")
#' }
