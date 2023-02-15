#' @examples
#' \dontrun{
#' # Assess different selection rules for a two-stage survival design with 
#' # O'Brien & Fleming alpha spending boundaries and (non-binding) stopping 
#' # for futility if the test statistic is negative. 
#' # Number of events at the second stage is adjusted based on conditional 
#' # power 80% and specified minimum and maximum number of Events.
#' design <- getDesignInverseNormal(typeOfDesign = "asOF", futilityBounds = 0)
#' 
#' y1 <- getSimulationMultiArmSurvival(design = design, activeArms = 4, 
#'     intersectionTest = "Simes", typeOfShape = "sigmoidEmax", 
#'     omegaMaxVector = seq(1, 2, 0.5), gED50 = 2, slope = 4, 
#'     typeOfSelection = "best", conditionalPower = 0.8, 
#'     minNumberOfEventsPerStage = c(NA_real_, 30), 
#'     maxNumberOfEventsPerStage = c(NA_real_, 90),
#'     maxNumberOfIterations = 50, 
#'     plannedEvents = c(75, 120))
#' 
#' y2 <- getSimulationMultiArmSurvival(design = design, activeArms = 4, 
#'     intersectionTest = "Simes", typeOfShape = "sigmoidEmax", 
#'     omegaMaxVector = seq(1,2,0.5), gED50 = 2, slope = 4,
#'     typeOfSelection = "epsilon", epsilonValue = 0.2, 
#'     effectMeasure = "effectEstimate",
#'     conditionalPower = 0.8, minNumberOfEventsPerStage = c(NA_real_, 30), 
#'     maxNumberOfEventsPerStage = c(NA_real_, 90),
#'     maxNumberOfIterations = 50, 
#' 	   plannedEvents = c(75, 120))
#' 
#' y1$effectMatrix
#' 
#' y1$rejectAtLeastOne
#' y2$rejectAtLeastOne
#' 
#' y1$selectedArms
#' y2$selectedArms
#' }
#' 
