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
## |  File version: $Revision: 7742 $
## |  Last changed: $Date: 2024-03-22 13:46:29 +0100 (Fr, 22 Mrz 2024) $
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
        "Illegal argument: performance score so far implemented only for single comparisons with continuous and binary endpoints"
    )
})

# Test for a simulationResult that does not have `bindingFutility = TRUE`
test_that("getPerformanceScore handles non-binding futility", {
    simulationResult <- createCorrectSimulationResultObject("SimulationResultsMeans")
    simulationResult$.design$bindingFutility <- FALSE
    expect_warning(getPerformanceScore(simulationResult))
})

# Test for a simulationResult that does not have `kMax = 2`
test_that("getPerformanceScore handles non-two-stage designs", {
    simulationResult <- createCorrectSimulationResultObject("SimulationResultsMeans")
    simulationResult$.design$kMax <- 3
    expect_error(
        getPerformanceScore(simulationResult),
        "Illegal argument: performance score so far implemented only for two-stage designs"
    )
})

# Test for a simulationResult that has a non-null `conditionalPower`.
test_that("getPerformanceScore handles non-null conditionalPower", {
    simulationResult <- createCorrectSimulationResultObject("SimulationResultsMeans")
    simulationResult$conditionalPower <- 0.8
    suppressWarnings(expect_type(getPerformanceScore(simulationResult), "environment"))
})

# Test to verify the correctness of the performance score calculation
test_that("getPerformanceScore calculates performance score correctly", {
    simulationResult <- createCorrectSimulationResultObject("SimulationResultsMeans")
    suppressWarnings(scores <- getPerformanceScore(simulationResult))
    expect_type(scores, "environment")
})

# Test to verify that the warning about the function being experimental is issued
test_that("getPerformanceScore issues warning", {
    simulationResult <- createCorrectSimulationResultObject("SimulationResultsMeans")
    expect_warning(
        getPerformanceScore(simulationResult),
        "The performance score function is experimental and hence not fully validated"
    )
})

# Test to check if the correct values are returned 
test_that("getPerformanceScore returns correct result object", {
    simulationResult <- createCorrectSimulationResultObject("SimulationResultsMeans")
    suppressWarnings(result <- getPerformanceScore(simulationResult))
    expect_type(result, "environment")
})

# Test to check if the correct values are returned
test_that("Print getPerformanceScore of simualtion means results", {
    .skipTestIfDisabled()
        
    design <- getDesignGroupSequential(
        kMax = 2,
        alpha = 0.025,
        beta = 0.2,
        sided = 1,
        typeOfDesign = "P",
        futilityBounds = 0,
        bindingFutility = TRUE
    )
    simulationResult <- getSimulationMeans(
        design = design,
        normalApproximation = TRUE,
        alternative = c(0.1, 0.2, 0.3, 0.35, 0.4, 0.5, 0.6),
        plannedSubjects = c(100, 300),
        minNumberOfSubjectsPerStage = c(NA, 1),
        maxNumberOfSubjectsPerStage = c(NA, 300),
        conditionalPower = 0.8,
        maxNumberOfIterations = 5,
        showStatistics = FALSE,
        seed = 4378258
    )
    suppressWarnings(result <- getPerformanceScore(simulationResult))
    
    ## Comparison of the results of PerformanceScore object 'result' with expected results
    expect_equal(result$locationSampleSize, c(0.33333333, 0.46480206, 0.98145492, 0.68310847, 0.32073998, 0.58520454, 0.17612196), tolerance = 1e-07, label = paste0(result$locationSampleSize))
    expect_equal(result$variationSampleSize, c(1, 0.4741251, 0.41415267, 0.41990327, 1, 0.93197635, 0.44658613), tolerance = 1e-07, label = paste0(result$variationSampleSize))
    expect_equal(result$subscoreSampleSize, c(0.66666667, 0.46946358, 0.69780379, 0.55150587, 0.66036999, 0.75859044, 0.31135404), tolerance = 1e-07, label = paste0(result$subscoreSampleSize))
    expect_equal(result$locationConditionalPower, c(0.30735073, 0.49743062, 0.60705436, 0.76291858, 0.5606313, 1, 0.97603637), tolerance = 1e-07, label = paste0(result$locationConditionalPower))
    expect_equal(result$variationConditionalPower, c(0.82758598, 0.40491784, 0.33046135, 0.35800273, 0.6214103, 1, 0.94089187), tolerance = 1e-07, label = paste0(result$variationConditionalPower))
    expect_equal(result$subscoreConditionalPower, c(0.56746835, 0.45117423, 0.46875786, 0.56046066, 0.5910208, 1, 0.95846412), tolerance = 1e-07, label = paste0(result$subscoreConditionalPower))
    expect_equal(result$performanceScore, c(0.61706751, 0.4603189, 0.58328083, 0.55598326, 0.6256954, 0.87929522, 0.63490908), tolerance = 1e-07, label = paste0(result$performanceScore))
    
    expect_true(any(grepl("Performance score", capture.output(result))))
})

# Test to check if the correct values are returned (rates)
test_that("Print getPerformanceScore of simualtion rates results", {
    .skipTestIfDisabled()
        
    design <- getDesignGroupSequential(
        kMax = 2,
        alpha = 0.025,
        beta = 0.2,
        sided = 1,
        typeOfDesign = "P",
        futilityBounds = 0,
        bindingFutility = TRUE
    )
    simulationResult <- getSimulationRates(
        design = design,
        normalApproximation = TRUE,
        plannedSubjects = c(100, 300),
        minNumberOfSubjectsPerStage = c(NA, 1),
        maxNumberOfSubjectsPerStage = c(NA, 300),
        conditionalPower = 0.8,
        maxNumberOfIterations = 5,
        showStatistics = FALSE,
        seed = 4378258
    )
    suppressWarnings(result <- getPerformanceScore(simulationResult))
    
    ## Comparison of the results of PerformanceScore object 'result' with expected results
    expect_equal(result$locationSampleSize, c(0.33333333, 0.76333333, 0.6331616, NaN), tolerance = 1e-07, label = paste0(result$locationSampleSize))
    expect_equal(result$variationSampleSize, c(NA_real_, 0.32538077, 0.33802988, NA_real_), tolerance = 1e-07, label = paste0(result$variationSampleSize))
    expect_equal(result$subscoreSampleSize, c(0.33333333, 0.54435705, 0.48559574, NaN), tolerance = 1e-07, label = paste0(result$subscoreSampleSize))
    expect_equal(result$locationConditionalPower, c(0.32576, 0.9990159, 0.99927128, NaN), tolerance = 1e-07, label = paste0(result$locationConditionalPower))
    expect_equal(result$variationConditionalPower, c(NA_real_, 0.99864022, 0.99927015, NA_real_), tolerance = 1e-07, label = paste0(result$variationConditionalPower))
    expect_equal(result$subscoreConditionalPower, c(0.32576, 0.99882806, 0.99927071, NaN), tolerance = 1e-07, label = paste0(result$subscoreConditionalPower))
    expect_equal(result$performanceScore, c(0.32954667, 0.77159255, 0.74243323, NaN), tolerance = 1e-07, label = paste0(result$performanceScore))
    
    expect_true(any(grepl("Performance score", capture.output(result))))
})
