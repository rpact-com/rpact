#' @examples
#' \dontrun{
#' # Calculate Type I error rates in the two-sided group sequential setting when
#' # performing kMax stages with constant critical boundaries at level alpha:
#' alpha <- 0.05
#' kMax <- 10
#' decisionMatrix <- matrix(c(
#'     rep(-qnorm(1 - alpha / 2), kMax),
#'     rep(qnorm(1 - alpha / 2), kMax)
#' ), nrow = 2, byrow = TRUE)
#' informationRates <- (1:kMax) / kMax
#' probs <- getGroupSequentialProbabilities(decisionMatrix, informationRates)
#' cumsum(probs[3, ] - probs[2, ] + probs[1, ])
#' 
#' # Do the same for a one-sided design without futility boundaries:
#' decisionMatrix <- matrix(c(
#'     rep(-Inf, kMax),
#'     rep(qnorm(1 - alpha), kMax)
#' ), nrow = 2, byrow = TRUE)
#' informationRates <- (1:kMax) / kMax
#' probs <- getGroupSequentialProbabilities(decisionMatrix, informationRates)
#' cumsum(probs[3, ] - probs[2, ])
#' 
#' # Check that two-sided Pampallona and Tsiatis boundaries with binding 
#' # futility bounds obtain Type I error probabilities equal to alpha:
#' x <- getDesignGroupSequential(
#'     alpha = 0.05, beta = 0.1, kMax = 3, typeOfDesign = "PT",
#'     deltaPT0 = 0, deltaPT1 = 0.4, sided = 2, bindingFutility = TRUE
#' )
#' dm <- matrix(c(
#'     -x$criticalValues, -x$futilityBounds, 0,
#'     x$futilityBounds, 0, x$criticalValues
#' ), nrow = 4, byrow = TRUE)
#' dm[is.na(dm)] <- 0
#' probs <- getGroupSequentialProbabilities(
#'     decisionMatrix = dm, informationRates = (1:3) / 3
#' )
#' sum(probs[5, ] - probs[4, ] + probs[1, ])
#' 
#' # Check the Type I error rate decrease when using non-binding futility bounds:
#' x <- getDesignGroupSequential(
#'     alpha = 0.05, beta = 0.1, kMax = 3, typeOfDesign = "PT",
#'     deltaPT0 = 0, deltaPT1 = 0.4, sided = 2, bindingFutility = FALSE
#' )
#' dm <- matrix(c(
#'     -x$criticalValues, -x$futilityBounds, 0,
#'     x$futilityBounds, 0, x$criticalValues
#' ), nrow = 4, byrow = TRUE)
#' dm[is.na(dm)] <- 0
#' probs <- getGroupSequentialProbabilities(
#'     decisionMatrix = dm, informationRates = (1:3) / 3
#' )
#' sum(probs[5, ] - probs[4, ] + probs[1, ])
#' }
#' 
