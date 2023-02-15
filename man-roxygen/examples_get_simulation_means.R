#' @examples
#' # Fixed sample size design with two groups, total sample size 40, 
#' # alternative = c(0, 0.2, 0.4, 0.8, 1), and standard deviation = 1 (the default) 
#' getSimulationMeans(plannedSubjects = 40, maxNumberOfIterations = 10)
#' \dontrun{
#' # Increase number of simulation iterations and compare results 
#' # with power calculator using normal approximation 
#' getSimulationMeans(alternative = 0:4, stDev = 5, 
#'     plannedSubjects = 40, maxNumberOfIterations = 1000)
#' getPowerMeans(alternative = 0:4, stDev = 5, 
#'     maxNumberOfSubjects = 40, normalApproximation = TRUE)
#' 
#' # Do the same for a three-stage O'Brien&Fleming inverse 
#' # normal group sequential design with non-binding futility stops
#' designIN <- getDesignInverseNormal(typeOfDesign = "OF", futilityBounds = c(0, 0))
#' x <- getSimulationMeans(designIN, alternative = c(0:4), stDev = 5, 
#'     plannedSubjects = c(20, 40, 60), maxNumberOfIterations = 1000)
#' getPowerMeans(designIN, alternative = 0:4, stDev = 5, 
#'     maxNumberOfSubjects = 60, normalApproximation = TRUE)
#' 
#' # Assess power and average sample size if a sample size increase is foreseen 
#' # at conditional power 80% for each subsequent stage based on observed overall 
#' # effect and specified minNumberOfSubjectsPerStage and
#' # maxNumberOfSubjectsPerStage
#' getSimulationMeans(designIN, alternative = 0:4, stDev = 5, 
#'     plannedSubjects = c(20, 40, 60), 
#'     minNumberOfSubjectsPerStage = c(NA, 20, 20), 
#'     maxNumberOfSubjectsPerStage = c(NA, 80, 80),
#'     conditionalPower = 0.8,
#'     maxNumberOfIterations = 50)
#' 
#' # Do the same under the assumption that a sample size increase only takes 
#' # place at the first interim. The sample size for the third stage is set equal 
#' # to the second stage sample size.
#' mySampleSizeCalculationFunction <- function(..., stage, 
#' 		minNumberOfSubjectsPerStage,
#' 		maxNumberOfSubjectsPerStage,
#' 		sampleSizesPerStage,
#' 		conditionalPower,
#' 		conditionalCriticalValue,
#' 		allocationRatioPlanned,
#' 		thetaH1,
#' 		stDevH1) {
#' 	if (stage <= 2) {
#' 		stageSubjects <- (1 + allocationRatioPlanned)^2/allocationRatioPlanned * 
#' 			(max(0, conditionalCriticalValue + stats::qnorm(conditionalPower)))^2 / 
#'          (max(1e-12, thetaH1/stDevH1))^2
#' 		stageSubjects <- min(max(minNumberOfSubjectsPerStage[stage], 
#'          stageSubjects), maxNumberOfSubjectsPerStage[stage])
#' 	} else {
#' 		stageSubjects <- sampleSizesPerStage[stage - 1]
#' 	}
#' 	return(stageSubjects)
#' }
#' getSimulationMeans(designIN, alternative = 0:4, stDev = 5, 
#'     plannedSubjects = c(20, 40, 60), 
#'     minNumberOfSubjectsPerStage = c(NA, 20, 20), 
#'     maxNumberOfSubjectsPerStage = c(NA, 80, 80),
#'     conditionalPower = 0.8, 
#'     calcSubjectsFunction = mySampleSizeCalculationFunction, 
#'     maxNumberOfIterations = 50)
#' }
#' 
