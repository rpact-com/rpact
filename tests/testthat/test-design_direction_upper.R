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
## |  File name: test-class_summary.R
## |  Creation date: 08 November 2023, 08:49:48
## |  File version: $Revision: 8151 $
## |  Last changed: $Date: 2024-08-30 10:39:49 +0200 (Fr, 30 Aug 2024) $
## |  Last changed by: $Author: pahlke $
## |  

test_plan_section("Testing designs with user defined 'directionUpper'")


test_that("Utility function .applyDirectionOfAlternative() works as expected", {
    expect_equal(.applyDirectionOfAlternative(1, FALSE, type = "negateIfLower"), -1)
    expect_equal(.applyDirectionOfAlternative(1, TRUE, type = "negateIfLower"), 1)
    expect_equal(.applyDirectionOfAlternative(1, NA, type = "negateIfLower"), 1)
    
    expect_equal(.applyDirectionOfAlternative(1, FALSE, type = "negateIfUpper"), 1)
    expect_equal(.applyDirectionOfAlternative(1, TRUE, type = "negateIfUpper"), -1)
    expect_equal(.applyDirectionOfAlternative(1, NA, type = "negateIfUpper"), -1)
    
    expect_false(.applyDirectionOfAlternative(TRUE, FALSE, type = "negateIfLower"))
    expect_true(.applyDirectionOfAlternative(TRUE, TRUE, type = "negateIfLower"))
    expect_true(.applyDirectionOfAlternative(TRUE, NA, type = "negateIfLower"))
    
    expect_true(.applyDirectionOfAlternative(FALSE, FALSE, type = "negateIfLower"))
    expect_false(.applyDirectionOfAlternative(FALSE, TRUE, type = "negateIfLower"))
    expect_false(.applyDirectionOfAlternative(FALSE, NA, type = "negateIfLower"))
    
    expect_true(.applyDirectionOfAlternative(TRUE, FALSE, type = "negateIfUpper"))
    expect_false(.applyDirectionOfAlternative(TRUE, TRUE, type = "negateIfUpper"))
    expect_false(.applyDirectionOfAlternative(TRUE, NA, type = "negateIfUpper"))
    
    expect_false(.applyDirectionOfAlternative(FALSE, FALSE, type = "negateIfUpper"))
    expect_true(.applyDirectionOfAlternative(FALSE, TRUE, type = "negateIfUpper"))
    expect_true(.applyDirectionOfAlternative(FALSE, NA, type = "negateIfUpper"))
    
    expect_equal(.applyDirectionOfAlternative(10, TRUE, type = "oneMinusValue"), -9)
    expect_equal(.applyDirectionOfAlternative(10, FALSE, type = "oneMinusValue"), 10)
    expect_equal(.applyDirectionOfAlternative(10, NA, type = "oneMinusValue"), -9)
    
    expect_equal(.applyDirectionOfAlternative(10, TRUE, type = "valueMinusOne"), 9)
    expect_equal(.applyDirectionOfAlternative(10, FALSE, type = "valueMinusOne"), 10)
    expect_equal(.applyDirectionOfAlternative(10, NA, type = "valueMinusOne"), 9)
    
    expect_equal(.applyDirectionOfAlternative(1:5, TRUE, type = "minMax"), 1)
    expect_equal(.applyDirectionOfAlternative(1:5, FALSE, type = "minMax"), 5)
    expect_equal(.applyDirectionOfAlternative(1:5, NA, type = "minMax"), 1)
})

test_that("Design with user defined 'directionUpper'", {
    .skipTestIfDisabled()
    .skipTestIfPipeOperatorNotAvailable()
    
    getDesignInverseNormal() |> 
        fetch(directionUpper) |> 
        is.na() |> 
        expect_true()
    
    getDesignInverseNormal(directionUpper = TRUE) |> 
        fetch(directionUpper) |> 
        expect_true()
    
    getDesignInverseNormal(directionUpper = FALSE) |> 
        fetch(directionUpper) |> 
        expect_false()
    
    getDesignFisher() |> 
        fetch(directionUpper) |> 
        is.na() |> 
        expect_true()
    
    getDesignFisher(directionUpper = TRUE) |> 
        fetch(directionUpper) |> 
        expect_true()
    
    getDesignFisher(directionUpper = FALSE) |> 
        fetch(directionUpper) |> 
        expect_false()
    
    getDesignFisher(directionUpper = FALSE) |> 
        getParameterType(directionUpper) |>
        expect_equal("u")
    
    getDesignInverseNormal() |> 
        fetch(directionUpper) |> 
        is.na() |> 
        expect_true()
    
    getDesignInverseNormal(directionUpper = TRUE) |> 
        fetch(directionUpper) |> 
        expect_true()
    
    getDesignInverseNormal(directionUpper = FALSE) |> 
        fetch(directionUpper) |> 
        expect_false()
    
    getDesignGroupSequential() |> 
        getPowerMeans(maxNumberOfSubjects = 100) |> 
        fetch(directionUpper) |> 
        expect_true()
    
    getPowerMeans(getDesignGroupSequential(), maxNumberOfSubjects = 100) |>
        getParameterType(directionUpper) |>
        expect_equal("d")
    
    getDesignGroupSequential(directionUpper = TRUE) |> 
        getPowerMeans(maxNumberOfSubjects = 100) |> 
        fetch(directionUpper) |> 
        expect_true()
    
    getDesignGroupSequential(directionUpper = FALSE) |> 
        getPowerMeans(maxNumberOfSubjects = 100) |> 
        fetch(directionUpper) |> 
        expect_false()
    
    getDesignGroupSequential(directionUpper = FALSE) |> 
        getPowerMeans(maxNumberOfSubjects = 100, directionUpper = FALSE) |> 
        fetch(directionUpper) |> 
        expect_false()
    
    getDesignGroupSequential(directionUpper = TRUE) |> 
        getPowerMeans(maxNumberOfSubjects = 100, directionUpper = TRUE) |> 
        fetch(directionUpper) |> 
        expect_true()
    
    getDesignGroupSequential(directionUpper = FALSE) |> 
        getPowerMeans(maxNumberOfSubjects = 100, directionUpper = TRUE) |>
        expect_error()
    
    getDesignGroupSequential(directionUpper = TRUE) |> 
        getPowerMeans(maxNumberOfSubjects = 100, directionUpper = FALSE) |>
        expect_error()

})

test_that("Analysis means with user defined 'directionUpper' in the design", {
    .skipTestIfDisabled()
    .skipTestIfPipeOperatorNotAvailable()
            
    getDesignInverseNormal(kMax = 2) |>
        getDataset(n = 120, means = 0.45, stDevs = 1.3) |>
        getAnalysisResults(nPlanned = 130, thetaH1 = 0.22, 
            assumedStDev = 1, thetaH0 = 0.25) |> 
        fetch(directionUpper) |> 
        expect_true()
    
    getDesignInverseNormal(kMax = 2, directionUpper = TRUE) |>
        getDataset(n = 120, means = 0.45, stDevs = 1.3) |>
        getAnalysisResults(nPlanned = 130, thetaH1 = 0.22, 
            assumedStDev = 1, thetaH0 = 0.25) |> 
        fetch(directionUpper) |> 
        expect_true()
    
    getDesignInverseNormal(kMax = 2) |>
        getDataset(n = 120, means = 0.45, stDevs = 1.3) |>
        getAnalysisResults(nPlanned = 130, thetaH1 = 0.22, 
            assumedStDev = 1, thetaH0 = 0.25, directionUpper = TRUE) |> 
        fetch(directionUpper) |> 
        expect_true()
    
    getDesignInverseNormal(kMax = 2, directionUpper = FALSE) |>
        getDataset(n = 120, means = 0.45, stDevs = 1.3) |>
        getAnalysisResults(nPlanned = 130, thetaH1 = 0.22, 
            assumedStDev = 1, thetaH0 = 0.25) |> 
        fetch(directionUpper) |> 
        expect_false()
    
    getDesignInverseNormal(kMax = 2) |>
        getDataset(n = 120, means = 0.45, stDevs = 1.3) |>
        getAnalysisResults(nPlanned = 130, thetaH1 = 0.22, 
            assumedStDev = 1, thetaH0 = 0.25, directionUpper = FALSE) |> 
        fetch(directionUpper) |> 
        expect_false()
    
    getDesignInverseNormal(kMax = 2, directionUpper = TRUE) |>
        getDataset(n = 120, means = 0.45, stDevs = 1.3) |>
        getAnalysisResults(nPlanned = 130, thetaH1 = 0.22, 
            assumedStDev = 1, thetaH0 = 0.25, directionUpper = FALSE) |>
        expect_error()
    
    getDesignInverseNormal(kMax = 2, directionUpper = FALSE) |>
        getDataset(n = 120, means = 0.45, stDevs = 1.3) |>
        getAnalysisResults(nPlanned = 130, thetaH1 = 0.22, 
            assumedStDev = 1, thetaH0 = 0.25, directionUpper = TRUE) |>
        expect_error()

})

test_that("Analysis multi-arm survival with user defined 'directionUpper' in the design", {
    .skipTestIfDisabled()
    .skipTestIfPipeOperatorNotAvailable()
            
    getDesignInverseNormal(kMax = 2) |>
        getDataset(
            events1   = c(25, 32),
            events2   = c(18, NA),
            logRanks1 = c(2.2, 1.8),
            logRanks2 = c(1.99, NA)
        ) |>
        getAnalysisResults() |> 
        fetch(directionUpper) |> 
        expect_true()
    
    getDesignInverseNormal(kMax = 2, directionUpper = TRUE) |>
        getDataset(
            events1   = c(25, 32),
            events2   = c(18, NA),
            logRanks1 = c(2.2, 1.8),
            logRanks2 = c(1.99, NA)
        ) |>
        getAnalysisResults() |> 
        fetch(directionUpper) |> 
        expect_true()
    
    getDesignInverseNormal(kMax = 2) |>
        getDataset(
            events1   = c(25, 32),
            events2   = c(18, NA),
            logRanks1 = c(2.2, 1.8),
            logRanks2 = c(1.99, NA)
        ) |>
        getAnalysisResults(directionUpper = TRUE) |> 
        fetch(directionUpper) |> 
        expect_true()
    
    getDesignInverseNormal(kMax = 2, directionUpper = FALSE) |>
        getDataset(
            events1   = c(25, 32),
            events2   = c(18, NA),
            logRanks1 = c(2.2, 1.8),
            logRanks2 = c(1.99, NA)
        ) |>
        getAnalysisResults() |> 
        fetch(directionUpper) |> 
        expect_false()
    
    getDesignInverseNormal(kMax = 2) |>
        getDataset(
            events1   = c(25, 32),
            events2   = c(18, NA),
            logRanks1 = c(2.2, 1.8),
            logRanks2 = c(1.99, NA)
        ) |>
        getAnalysisResults(directionUpper = FALSE) |> 
        fetch(directionUpper) |> 
        expect_false()
    
    getDesignInverseNormal(kMax = 2, directionUpper = TRUE) |>
        getDataset(
            events1   = c(25, 32),
            events2   = c(18, NA),
            logRanks1 = c(2.2, 1.8),
            logRanks2 = c(1.99, NA)
        ) |>
        getAnalysisResults(directionUpper = FALSE) |> 
        expect_error()
    
    getDesignInverseNormal(kMax = 2, directionUpper = FALSE) |>
        getDataset(
            events1   = c(25, 32),
            events2   = c(18, NA),
            logRanks1 = c(2.2, 1.8),
            logRanks2 = c(1.99, NA)
        ) |>
        getAnalysisResults(directionUpper = TRUE) |> 
        expect_error()
})
