#' Field description: information epsilon
#' @field informationEpsilon The absolute information epsilon, which defines 
#'        the maximum distance from the observed information to the maximum 
#'        information that causes the final analysis. 
#'        Updates at the final analysis if the observed information at the 
#'        final analysis is smaller ("under-running") than the planned 
#'        maximum information. Is either a positive integer value specifying 
#'        the absolute information epsilon or a floating point number >0 and 
#'        <1 to define a relative information epsilon.
#' @keywords internal
