## |
## |  *Unit tests*
## |
## |  This file is part of the R package rpact:
## |  Confirmatory Adaptive Clinical Trial Design and Analysis
## |
## |  Author: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
## |  Licensed under "GNU Lesser General Public License" version 3
## |  License text can be found here: https://www.r-project.org/Licenses/LGPL-3
## |
## |  RPACT company website: https://www.rpact.com
## |  RPACT package website: https://www.rpact.org
## |
## |  Contact us for information about our services: info@rpact.com
## |
## |  File name: test-f_simulation_performance_score.R
## |  Creation date: 06 February 2023, 12:14:51
## |  File version: $Revision: 7644 $
## |  Last changed: $Date: 2024-02-16 10:36:28 +0100 (Fr, 16 Feb 2024) $
## |  Last changed by: $Author: pahlke $
## |

test_plan_section("Testing Performance Score Functions")

test_that("Simulation performance score functions throw errors when arguments are missing or wrong", {
    expect_error(getPerformanceScore())
})

# Mock a correct SimulationResult object
createCorrectSimulationResultObject <- function(className) {
    simulationResult <- list(
        .design = list(
            bindingFutility = TRUE,
            kMax = 2,
            alpha = 0.05,
            beta = 0.2
        ),
        show = function(...) {},
        .show = function(...) {},
        alternative = c(0.5, 1, 1.5),
        plannedSubjects = c(30, 60),
        maxNumberOfSubjectsPerStage = c(30, 60),
        maxNumberOfIterations = 1000,
        normalApproximation = TRUE,
        groups = 2,
        stDev = 1,
        conditionalPower = NA,
        .data = data.frame(
            alternative = rep(c(0.5, 1, 1.5), times = 1000),
            stageNumber = sample(c(1, 2), size = 3000, replace = TRUE),
            numberOfCumulatedSubjects = sample(c(30, 60), size = 3000, replace = TRUE),
            conditionalPowerAchieved = runif(3000, 0, 1)
        )
    )
    
    class(simulationResult) <- c(className, "SimulationResults", class(simulationResult))
    return(simulationResult)
}

.assertIsSimulationResults <- function(simulationResult) {
    if (!inherits(simulationResult, "SimulationResults")) {
        stop("Expected 'simulationResult' to be an object of class 'SimulationResults'")
    }
}

test_that("getPerformanceScore handles SimulationResultsMeans", {
    simulationResult <- createCorrectSimulationResultObject("SimulationResultsNonMeans")
    expect_error(
        getPerformanceScore(simulationResult),
        "Illegal argument: performance score so far implemented only for single comparisons with continuous endpoints"
    )
})

# 1. Test for a simulationResult that does not have `bindingFutility = TRUE`.
test_that("getPerformanceScore handles non-binding futility", {
    simulationResult <- createCorrectSimulationResultObject("SimulationResultsMeans")
    simulationResult$.design$bindingFutility <- FALSE
    expect_warning(getPerformanceScore(simulationResult))
})

# 2. Test for a simulationResult that does not have `kMax = 2`.
test_that("getPerformanceScore handles non-two-stage designs", {
    simulationResult <- createCorrectSimulationResultObject("SimulationResultsMeans")
    simulationResult$.design$kMax <- 3
    expect_error(
        getPerformanceScore(simulationResult),
        "Illegal argument: performance score so far implemented only for two-stage designs"
    )
})

# 3. Test for a simulationResult that has a non-null `conditionalPower`.
test_that("getPerformanceScore handles non-null conditionalPower", {
    simulationResult <- createCorrectSimulationResultObject("SimulationResultsMeans")
    simulationResult$conditionalPower <- 0.8
    suppressWarnings(expect_type(getPerformanceScore(simulationResult), "S4"))
})

# 4. Test to verify the correctness of the performance score calculation.
test_that("getPerformanceScore calculates performance score correctly", {
    simulationResult <- createCorrectSimulationResultObject("SimulationResultsMeans")
    suppressWarnings(scores <- getPerformanceScore(simulationResult))
    expect_type(scores, "S4")
})

# 5. Test to verify that the warning about the function being experimental is issued.
test_that("getPerformanceScore issues warning", {
    simulationResult <- createCorrectSimulationResultObject("SimulationResultsMeans")
    expect_warning(
        getPerformanceScore(simulationResult),
        "The performance score function is experimental and hence not fully validated"
    )
})

# 6. Test to check if the correct values are returned in the resultList.
test_that("getPerformanceScore returns correct resultList", {
    simulationResult <- createCorrectSimulationResultObject("SimulationResultsMeans")
    suppressWarnings(result <- getPerformanceScore(simulationResult))
    expect_type(result, "S4")
})
