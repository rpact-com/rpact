#' @examples
#' # Fixed sample size design (two groups) with total sample 
#' # size 120, pi1 = (0.3,0.4,0.5,0.6) and pi2 = 0.3
#' getSimulationRates(pi1 = seq(0.3, 0.6, 0.1), pi2 = 0.3, 
#'     plannedSubjects = 120, maxNumberOfIterations = 10)
#' \dontrun{
#' # Increase number of simulation iterations and compare results with power calculator
#' getSimulationRates(pi1 = seq(0.3, 0.6, 0.1), pi2 = 0.3, 
#'     plannedSubjects = 120, maxNumberOfIterations = 50)
#' getPowerRates(pi1 = seq(0.3, 0.6, 0.1), pi2 = 0.3, maxNumberOfSubjects = 120)
#' 
#' # Do the same for a two-stage Pocock inverse normal group sequential 
#' # design with non-binding futility stops
#' designIN <- getDesignInverseNormal(typeOfDesign = "P", futilityBounds = c(0))
#' getSimulationRates(designIN, pi1 = seq(0.3, 0.6, 0.1), pi2 = 0.3, 
#'     plannedSubjects = c(40, 80), maxNumberOfIterations = 50)
#' getPowerRates(designIN, pi1 = seq(0.3, 0.6, 0.1), pi2 = 0.3, maxNumberOfSubjects = 80)
#' 
#' # Assess power and average sample size if a sample size reassessment is 
#' # foreseen at conditional power 80% for the subsequent stage (decrease and increase) 
#' # based on observed overall (cumulative) rates and specified minNumberOfSubjectsPerStage 
#' # and maxNumberOfSubjectsPerStage
#' 
#' # Do the same under the assumption that a sample size increase only takes place 
#' # if the rate difference exceeds the value 0.1 at interim. For this, the sample 
#' # size recalculation method needs to be redefined:  
#' mySampleSizeCalculationFunction <- function(..., stage,
#'         plannedSubjects,
#'         minNumberOfSubjectsPerStage,
#'         maxNumberOfSubjectsPerStage,
#'         conditionalPower,
#'         conditionalCriticalValue,
#'         overallRate) {
#'     if (overallRate[1] - overallRate[2] < 0.1) {
#'         return(plannedSubjects[stage] - plannedSubjects[stage - 1]) 
#'     } else {
#'         rateUnderH0 <- (overallRate[1] + overallRate[2]) / 2 
#'         stageSubjects <- 2 * (max(0, conditionalCriticalValue * 
#'             sqrt(2 * rateUnderH0 * (1 - rateUnderH0)) + 
#'             stats::qnorm(conditionalPower) * sqrt(overallRate[1] * 
#'             (1 - overallRate[1]) + overallRate[2] * (1 - overallRate[2]))))^2 /
#'             (max(1e-12, (overallRate[1] - overallRate[2])))^2
#'         stageSubjects <- ceiling(min(max(
#'             minNumberOfSubjectsPerStage[stage], 
#'             stageSubjects), maxNumberOfSubjectsPerStage[stage]))
#'         return(stageSubjects)
#'     }
#' }
#' getSimulationRates(designIN, pi1 = seq(0.3, 0.6, 0.1), pi2 = 0.3, 
#'     plannedSubjects = c(40, 80), minNumberOfSubjectsPerStage = c(40, 20), 
#'     maxNumberOfSubjectsPerStage = c(40, 160), conditionalPower = 0.8, 
#'     calcSubjectsFunction = mySampleSizeCalculationFunction, maxNumberOfIterations = 50)
#' }
#' 
