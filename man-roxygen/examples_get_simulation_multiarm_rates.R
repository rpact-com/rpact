#' @examples
#' \dontrun{
#' # Simulate the power of the combination test with two interim stages and 
#' # O'Brien & Fleming boundaries using Dunnett's intersection tests if the 
#' # best treatment arm is selected at first interim. Selection only take 
#' # place if a non-negative treatment effect is observed (threshold = 0); 
#' # 20 subjects per stage and treatment arm, simulation is performed for 
#' # four parameter configurations.
#' design <- getDesignInverseNormal(typeOfDesign = "OF")
#' effectMatrix <- matrix(c(0.2,0.2,0.2,
#'     0.4,0.4,0.4,
#'     0.4,0.5,0.5,
#'     0.4,0.5,0.6),
#'     byrow = TRUE, nrow = 4, ncol = 3)
#' x <- getSimulationMultiArmRates(design = design, typeOfShape = "userDefined", 
#'     effectMatrix = effectMatrix , piControl = 0.2, 
#'     typeOfSelection = "best", threshold = 0, intersectionTest = "Dunnett", 
#'     plannedSubjects = c(20, 40, 60), 
#'     maxNumberOfIterations = 50)
#' summary(x)
#' }
#' 
