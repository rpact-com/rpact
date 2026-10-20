#' @examples
#' \dontrun{
#' # Assess a population selection strategy with one subset population and
#' # a survival endpoint. The considered situations are defined through the 
#' # event rates yielding a range of hazard ratios in the subsets. Design 
#' # with O'Brien and Fleming alpha spending and a reassessment of event 
#' # number in the first interim based on conditional power and assumed 
#' # hazard ratio using weighted inverse normal combination test.  
#'     
#' subGroups <- c("S", "R")
#' prevalences <- c(0.40, 0.60)
#'  
#' p2 <- c(0.3, 0.4)
#' range1 <- p2[1] + seq(0, 0.3, 0.05)
#' 
#' p1 <- c()
#' for (x1 in range1) {
#'     p1 <- c(p1, x1, p2[2] + 0.1)
#' }		
#' hazardRatios <- log(matrix(1 - p1, byrow = TRUE, ncol = 2)) /
#'     matrix(log(1 - p2), byrow = TRUE, ncol = 2,
#'     nrow = length(range1))
#' 
#' effectList <- list(subGroups=subGroups, prevalences=prevalences,
#'     hazardRatios = hazardRatios)
#' 
#' design <- getDesignInverseNormal(informationRates = c(0.3, 0.7, 1),
#'     typeOfDesign = "asOF")
#' 
#' simResultsPE <- getSimulationEnrichmentSurvival(design, 
#'     plannedEvents = c(40, 90, 120),
#'     effectList = effectList,
#'     typeOfSelection = "rbest", rValue = 2,
#'     conditionalPower = 0.8, minNumberOfEventsPerStage = c(NA, 50, 30),
#'     maxNumberOfEventsPerStage = c(NA, 150, 30), thetaH1 = 4 / 3,
#'     maxNumberOfIterations = 100)
#' print(simResultsPE)
#'
#' # Non-inferiority in the full population (H0: hazard ratio = 1.2)
#' effectListNonInferiority <- list(
#'     subGroups = "F", prevalences = 1,
#'     hazardRatios = matrix(1), piControls = 0.3)
#' simulationResultsNonInferiority <- getSimulationEnrichmentSurvival(
#'     design = getDesignFixed(sided = 1), thetaH0 = 1.2,
#'     directionUpper = FALSE, effectList = effectListNonInferiority,
#'     plannedEvents = 80, allocationRatioPlanned = 1,
#'     maxNumberOfSubjects = 400, accrualTime = c(0, 20),
#'     accrualIntensity = 20, maxNumberOfIterations = 50,
#'     simulationType = "patientWise")
#'
#' # Piecewise exponential event times in two disjoint subgroups
#' piecewiseTime <- getPiecewiseEnrichmentSurvivalTime(
#'     piecewiseSurvivalTime = c(0, 6),
#'     lambdaControls = rbind(S = c(0.08, 0.12), R = c(0.05, 0.09)),
#'     hazardRatios = list(
#'         favorable = rbind(S = c(0.65, 0.55), R = c(0.9, 0.8)),
#'         conservative = rbind(S = c(0.8, 0.7), R = c(1, 0.9))
#'     )
#' )
#' effectListPiecewise <- list(
#'     subGroups = c("S", "R"), prevalences = c(0.4, 0.6),
#'     piecewiseSurvivalTime = piecewiseTime)
#' simulationResultsPiecewise <- getSimulationEnrichmentSurvival(
#'     design = getDesignFixed(), simulationType = "patientWise",
#'     effectList = effectListPiecewise, directionUpper = FALSE,
#'     plannedEvents = 80, maxNumberOfSubjects = 300,
#'     accrualTime = c(0, 12), accrualIntensity = 25,
#'     maxNumberOfIterations = 50)
#' }
#' 
