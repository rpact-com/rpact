#' @section Markdown options:
#' Use \code{options("rpact.print.heading.base.number" = NUMBER)}
#' (where \code{NUMBER} is an integer value >= -2) to
#' specify the heading level. 
#' 
#' NUMBER = 1 results in the heading prefix \code{#}, NUMBER = 2 results in \code{##}, ...
#' 
#' The default is
#' \code{options("rpact.print.heading.base.number" = -2)}, i.e., the
#' top headings will be written italic but are not
#' explicit defined as header.
#' \code{options("rpact.print.heading.base.number" = -1)} means
#' that all headings will be written bold but are not
#' explicit defined as header.
#' 
#' Furthermore the following options can be set globally:
#' \itemize{
#'   \item \code{rpact.auto.markdown.all}: if \code{TRUE}, all output types will be rendered in Markdown format automatically.
#'   \item \code{rpact.auto.markdown.print}: if \code{TRUE}, all print outputs will be rendered in Markdown format automatically.
#'   \item \code{rpact.auto.markdown.summary}: if \code{TRUE}, all summary outputs will be rendered in Markdown format automatically.
#'   \item \code{rpact.auto.markdown.plot}: if \code{TRUE}, all plot outputs will be rendered in Markdown format automatically.
#' }
#' Example: \code{options("rpact.auto.markdown.plot" = FALSE)} disables the automatic knitting of plots inside Markdown documents.
#' 
