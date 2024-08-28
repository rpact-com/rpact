#' @details 
#' Depending on \code{typeOfDesign} some parameters are specified, others not. 
#' For example, only if \code{typeOfDesign} \code{"asHSD"} is selected, \code{gammaA} needs to be specified.
#' 
#' If an alpha spending approach was specified (\code{"asOF"}, \code{"asP"}, \code{"asKD"}, \code{"asHSD"}, or \code{"asUser"}) 
#' additionally a beta spending function can be specified to produce futility bounds.
#' 
#' For optimum designs, \code{"ASNH1"} minimizes the expected sample size under H1, 
#' \code{"ASNIFH1"} minimizes the sum of the maximum sample and the expected sample size under H1, 
#' and \code{"ASNsum"}  minimizes the sum of the maximum sample size, the expected sample size under a value midway H0 and H1, 
#' and the expected sample size under H1. 
#'
#' @seealso \code{\link[=getDesignSet]{getDesignSet()}} for creating a set of designs to compare different designs.
#' 
