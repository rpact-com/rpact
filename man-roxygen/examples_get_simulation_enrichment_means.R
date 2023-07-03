#' @examples
#' \dontrun{
#' # Assess a population selection strategy with one subset population.
#' # If the subset is better than the full population, then the subset
#' # is selected for the second stage, otherwise the full. Print and plot
#' # design characteristics.
#'
#' # Define design
#' designIN <- getDesignInverseNormal(kMax = 2)
#'
#' # Define subgroups and their prevalences
#' subGroups <- c("S", "R") # fixed names!
#' prevalences <- c(0.2, 0.8)
#'
#' # Define effect matrix and variability
#' effectR <- 0.2
#' m <- c()
#' for (effectS in seq(0, 0.5, 0.25)) {
#'     m <- c(m, effectS, effectR)
#' }
#' effects <- matrix(m, byrow = TRUE, ncol = 2)
#' stDev <- c(0.4, 0.8)
#'
#' # Define effect list
#' effectList <- list(subGroups=subGroups, prevalences=prevalences, stDevs = stDev, effects = effects)
#'
#' # Perform simulation
#' simResultsPE <- getSimulationEnrichmentMeans(design = designIN,
#'     effectList = effectList, plannedSubjects = c(50, 100),
#'     maxNumberOfIterations = 100)
#' print(simResultsPE)
#'
#' # Assess the design characteristics of a user defined selection
#' # strategy in a three-stage design with no interim efficacy stop
#' # using the inverse normal method for combining the stages.
#' # Only the second interim is used for a selecting of a study
#' # population. There is a small probability for stopping the trial
#' # at the first interim.
#'
#' # Define design
#' designIN2 <- getDesignInverseNormal(typeOfDesign = "noEarlyEfficacy", kMax = 3)
#'
#' # Define selection function
#' mySelection <- function(effectVector, stage) {
#'     selectedPopulations <- rep(TRUE, 3)
#'     if (stage == 2) {
#'         selectedPopulations <- (effectVector >= c(1, 2, 3))
#'     }
#'     return(selectedPopulations)
#' }
#'
#' # Define subgroups and their prevalences
#' subGroups <- c("S1", "S12", "S2", "R")   # fixed names!
#' prevalences <- c(0.2, 0.3, 0.4, 0.1)
#'
#' effectR <- 1.5
#' effectS12 = 5
#' m <- c()
#' for (effectS1 in seq(0, 5, 1)) {
#'     for (effectS2 in seq(0, 5, 1)) {
#'         m <- c(m, effectS1, effectS12, effectS2, effectR)
#'     }
#' }
#' effects <- matrix(m, byrow = TRUE, ncol = 4)
#' stDev <- 10
#'
#' # Define effect list
#' effectList <- list(subGroups=subGroups, prevalences=prevalences, stDevs = stDev, effects = effects)
#'
#' # Perform simulation
#' simResultsPE <- getSimulationEnrichmentMeans(
#'     design = designIN2,
#'     effectList = effectList,
#'     typeOfSelection = "userDefined",
#'     selectPopulationsFunction = mySelection,
#'     intersectionTest = "Simes",
#'     plannedSubjects = c(50, 100, 150),
#'     maxNumberOfIterations = 100)
#' print(simResultsPE)
#' if (require(ggplot2)) plot(simResultsPE, type = 3)
#' }
#'
