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
#'
#' # Non-inferiority with one active arm (H0: hazard ratio = 1.2)
#' simulationResultsNonInferiority <- getSimulationMultiArmSurvival(
#'     design = getDesignFixed(sided = 1), thetaH0 = 1.2,
#'     directionUpper = FALSE, activeArms = 1,
#'     typeOfShape = "userDefined", effectMatrix = matrix(1),
#'     piControl = 0.3, plannedEvents = 80,
#'     allocationRatioPlanned = 1, maxNumberOfSubjects = 400,
#'     accrualTime = c(0, 20), accrualIntensity = 20,
#'     maxNumberOfIterations = 50, simulationType = "patientWise")
#'
#' # Piecewise exponential event times for two active arms and two situations
#' piecewiseTime <- getPiecewiseMultiArmSurvivalTime(
#'     piecewiseSurvivalTime = c(0, 6),
#'     lambdaControl = c(0.08, 0.12),
#'     hazardRatios = list(
#'         favorable = rbind(lowDose = c(0.9, 0.8), highDose = c(0.8, 0.6)),
#'         conservative = rbind(lowDose = c(1, 0.9), highDose = c(0.9, 0.75))
#'     )
#' )
#' simulationResultsPiecewise <- getSimulationMultiArmSurvival(
#'     design = getDesignFixed(), simulationType = "patientWise",
#'     piecewiseSurvivalTime = piecewiseTime, directionUpper = FALSE,
#'     plannedEvents = 80, maxNumberOfSubjects = 300,
#'     accrualTime = c(0, 12), accrualIntensity = 25,
#'     maxNumberOfIterations = 50)
#' }
#' 
