#' @section Summary options:
#' The following options can be set globally:
#' \enumerate{
#'   \item \code{rpact.summary.output.size}: one of \code{c("small", "medium", "large")}; 
#'     defines how many details will be included into the summary; 
#'     default is \code{"large"}, i.e., all available details are displayed.
#'   \item \code{rpact.summary.justify}: one of \code{c("right", "left", "centre")}; 
#'     shall the values be right-justified (the default), left-justified or centered.
#'   \item \code{rpact.summary.width}: defines the maximum number of characters to be used per line (default is \code{83}).
#'   \item \code{rpact.summary.intervalFormat}: defines how intervals will be displayed in the summary, 
#'     default is \code{"[\%s; \%s]"}.
#'   \item \code{rpact.summary.digits}: defines how many digits are to be used for numeric values (default is \code{3}).
#'   \item \code{rpact.summary.digits.probs}: defines how many digits are to be used for numeric values 
#'         (default is one more than value of \code{rpact.summary.digits}, i.e., \code{4}). 
#'   \item \code{rpact.summary.trim.zeroes}: if \code{TRUE} (default) zeroes will always displayed as "0",
#'     e.g. "0.000" will become "0". 
#' }
#' Example: \code{options("rpact.summary.intervalFormat" = "\%s - \%s")}
#' 
