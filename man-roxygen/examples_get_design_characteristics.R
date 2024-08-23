#' @examples
#' \dontrun{
#' # Calculate design characteristics for a three-stage O'Brien & Fleming 
#' # design at power 90% and compare it with Pocock's design.  
#' getDesignCharacteristics(getDesignGroupSequential(beta = 0.1))
#' getDesignCharacteristics(getDesignGroupSequential(beta = 0.1, typeOfDesign = "P")) 
#' }
#' 
