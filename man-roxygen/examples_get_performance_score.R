#' @examples
#' \dontrun{
#' # Example from Table 3 in "A new conditional performance score for 
#' # the evaluation of adaptive group sequential designs with samplesize 
#' # recalculation from Herrmann et al 2023", p. 2097 for 
#' # Observed Conditional Power approach and Delta = 0.5
#' 
#' # Create two-stage Pocock design with binding futility boundary at 0
#' design <- getDesignGroupSequential(
#'     kMax = 2, typeOfDesign = "P", 
#'     futilityBounds = 0, bindingFutility = TRUE)
#' 
#' # Initialize sample sizes and effect; 
#' # Sample sizes are referring to overall stage-wise sample sizes
#' n1 <- 100
#' n2 <- 100
#' nMax <- n1 + n2
#' alternative <- 0.5
#' 
#' # Perform Simulation; nMax * 1.5 defines the maximum 
#' # sample size for the additional stage
#' simulationResult <- getSimulationMeans(
#'     design = design,
#'     normalApproximation = TRUE,
#'     thetaH0 = 0,
#'     alternative = alternative,
#'     plannedSubjects = c(n1, nMax),
#'     minNumberOfSubjectsPerStage = c(NA_real_, 1),
#'     maxNumberOfSubjectsPerStage = c(NA_real_, nMax * 1.5),
#'     conditionalPower = 0.8,
#'     directionUpper = TRUE,
#'     maxNumberOfIterations = 1e05,
#'     seed = 140
#' )
#' 
#' # Calculate performance score
#' getPerformanceScore(simulationResult)
#' }
#' 
