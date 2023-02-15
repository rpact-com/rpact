#' @examples
#' \dontrun{
#' # Assess a population selection strategy with two subset populations and
#' # a binary endpoint using a stratified analysis. No early efficacy stop,
#' # weighted inverse normal method with weight sqrt(0.4).
#' pi2 <- c(0.3, 0.4, 0.3, 0.55)
#' pi1Seq <- seq(0.0, 0.2, 0.2)
#' pi1 <- matrix(rep(pi1Seq, length(pi2)), ncol = length(pi1Seq), byrow = TRUE) + pi2
#' effectList <- list(
#'     subGroups = c("S1", "S2", "S12", "R"), 
#'     prevalences = c(0.1, 0.4, 0.2, 0.3),
#'     piControl = pi2, 
#'     piTreatments = expand.grid(pi1[1, ], pi1[2, ], pi1[3, ], pi1[4, ])
#' )
#' design <- getDesignInverseNormal(informationRates = c(0.4, 1),
#'     typeOfDesign = "noEarlyEfficacy")
#' simResultsPE <- getSimulationEnrichmentRates(design, 
#'     plannedSubjects = c(150, 300),
#'     allocationRatioPlanned = 1.5, directionUpper = TRUE,
#'     effectList = effectList, stratifiedAnalysis = TRUE,
#'     intersectionTest = "Sidak",
#'     typeOfSelection = "epsilon", epsilonValue = 0.025,
#'     maxNumberOfIterations = 100)
#' print(simResultsPE)	
#' }
#' 
