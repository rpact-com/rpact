#' @examples
#' \dontrun{
#' # Assess a treatment-arm selection strategy with three active arms, 
#' # if the better of the arms is selected for the second stage, and 
#' # compare it with the no-selection case. 
#' # Assume a linear dose-response relationship 
#' maxNumberOfIterations <- 100
#' designIN <- getDesignInverseNormal(typeOfDesign = "OF", kMax = 2)
#' sim <- getSimulationMultiArmMeans(design = designIN,
#'     activeArms = 3, typeOfShape = "linear",
#'     muMaxVector = seq(0,0.8,0.2),
#'     intersectionTest = "Simes",
#'     typeOfSelection = "best",
#'     plannedSubjects = c(30,60),
#'     maxNumberOfIterations = maxNumberOfIterations)
#' 
#' sim0 <- getSimulationMultiArmMeans(design = designIN,
#'     activeArms = 3, typeOfShape = "linear",
#'     muMaxVector = seq(0,0.8,0.2),
#'     intersectionTest = "Simes",
#'     typeOfSelection = "all",
#'     plannedSubjects = c(30,60),
#'     maxNumberOfIterations = maxNumberOfIterations)
#' 
#' sim$rejectAtLeastOne
#' sim$expectedNumberOfSubjects
#' 
#' sim0$rejectAtLeastOne
#' sim0$expectedNumberOfSubjects
#' 
#' # Compare the power of the conditional Dunnett test with the power of the 
#' # combination test using Dunnett's intersection tests if no treatment arm 
#' # selection takes place. Asseume a linear dose-response relationship.
#' maxNumberOfIterations <- 100
#' designIN <- getDesignInverseNormal(typeOfDesign = "asUser", 
#'     userAlphaSpending = c(0, 0.025))
#' designCD <- getDesignConditionalDunnett(secondStageConditioning = TRUE)
#' 
#' index <- 1
#' for (design in c(designIN, designCD)) {
#'     results <- getSimulationMultiArmMeans(design, activeArms = 3, 
#'         muMaxVector = seq(0, 1, 0.2), typeOfShape = "linear", 
#'         plannedSubjects = cumsum(rep(20, 2)), 
#'         intersectionTest = "Dunnett", 
#'         typeOfSelection = "all", maxNumberOfIterations = maxNumberOfIterations)
#'     if (index == 1) {
#'         drift <- results$effectMatrix[nrow(results$effectMatrix), ]
#'         plot(drift, results$rejectAtLeastOne, type = "l", lty = 1, 
#'             lwd = 3, col = "black", ylab = "Power")
#'     } else {
#'         lines(drift,results$rejectAtLeastOne, type = "l", 
#'             lty = index, lwd = 3, col = "red")
#'     }
#'     index <- index + 1
#' }
#' legend("topleft", legend=c("Combination Dunnett", "Conditional Dunnett"),
#'     col=c("black", "red"), lty = (1:2), cex = 0.8)
#' 
#' # Assess the design characteristics of a user defined selection
#' # strategy in a two-stage design using the inverse normal method
#' # with constant bounds. Stopping for futility due to
#' # de-selection of all treatment arms.
#' designIN <- getDesignInverseNormal(typeOfDesign = "P", kMax = 2)
#' 
#' mySelection <- function(effectVector) {
#'     selectedArms <- (effectVector >= c(0, 0.1, 0.3))
#'     return(selectedArms)
#' }
#' 
#' results <- getSimulationMultiArmMeans(designIN, activeArms = 3, 
#'     muMaxVector = seq(0, 1, 0.2), 
#'     typeOfShape = "linear", 
#'     plannedSubjects = c(30,60), 
#'     intersectionTest = "Dunnett", 
#'     typeOfSelection = "userDefined",
#'     selectArmsFunction = mySelection,
#'     maxNumberOfIterations = 100)
#' 
#' options(rpact.summary.output.size = "medium")         
#' summary(results)
#' if (require(ggplot2)) plot(results, type = c(5,3,9), grid = 4)
#' }
#' 
