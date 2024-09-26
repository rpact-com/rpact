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
## |  File name: test-class_analysis_dataset.R
## |  Creation date: 06 February 2023, 12:04:06
## |  File version: $Revision: 7742 $
## |  Last changed: $Date: 2024-03-22 13:46:29 +0100 (Fr, 22 Mrz 2024) $
## |  Last changed by: $Author: pahlke $
## |

test_plan_section("Testing the Class 'ParameterSet'")

test_that("Test fetch method", {
    .skipTestIfDisabled()
    .skipTestIfPipeOperatorNotAvailable()
    
    design <- getDesignGroupSequential() 
    
    kMax <- 7
    design |> fetch(kMax) |> expect_equal(c(kMax = 3))
    design |> fetch(informationRates = 4) |> expect_equal(
        list(informationRates = c(0.3333333, 0.6666667, 1)), tolerance = 1e-6)
    
    i <- 7
    expect_error(design |> fetch(i))
    
    design |> fetch(1) |> expect_equal(c(kMax = 3))
    design |> fetch(1:3) |> expect_equal(list(kMax = 3, alpha = 0.025, stages = c(1, 2, 3)))
    design |> fetch("kMax", 3:5) |> expect_equal(
        list(kMax = 3, stages = c(1, 2, 3), informationRates = c(0.3333333, 0.6666667, 1), 
            userAlphaSpending = NA_real_), tolerance = 1e-6)
    design |> fetch(-3) |> expect_equal(c(delayedInformation = NA_real_))
    design |> fetch(-3, 1) |> expect_equal(list(delayedInformation = NA_real_, kMax = 3))
    design |> fetch(-3, "kMax") |> expect_equal(list(delayedInformation = NA_real_, kMax = 3))
    design |> fetch("kMax", informationRates) |> expect_equal(
        list(kMax = 3, informationRates = c(0.3333333, 0.6666667, 1)), tolerance = 1e-6)
    design |> fetch("kMax", informationRates, output = "labeled") |> expect_equal(
        list("Maximum number of stages" = 3, 
            "Information rates" = c(0.3333333, 0.6666667, 1)), tolerance = 1e-6)
    design |> fetch("kMax") |> expect_equal(c(kMax = 3))
    design |> fetch(kMax) |> expect_equal(c(kMax = 3))
    design |> fetch(kMax, output = "named") |> expect_equal(c(kMax = 3))
    design |> fetch(kMax, output = "labeled") |> expect_equal(c("Maximum number of stages" = 3))
    design |> fetch(kMax, output = "value") |> expect_equal(3)
    design |> fetch(kMax, output = "list") |> expect_equal(list(kMax = 3))
})

test_that("Test obtain method", {
    .skipTestIfDisabled()
    .skipTestIfPipeOperatorNotAvailable()
    
    design <- getDesignGroupSequential() 
    
    kMax <- 7
    design |> obtain(kMax) |> expect_equal(c(kMax = 3))
    design |> obtain(informationRates = 4) |> expect_equal(
        list(informationRates = c(0.3333333, 0.6666667, 1)), tolerance = 1e-6)
    
    i <- 7
    expect_error(design |> obtain(i))
    
    design |> obtain(1) |> expect_equal(c(kMax = 3))
    design |> obtain(1:3) |> expect_equal(list(kMax = 3, alpha = 0.025, stages = c(1, 2, 3)))
    design |> obtain("kMax", 3:5) |> expect_equal(
        list(kMax = 3, stages = c(1, 2, 3), informationRates = c(0.3333333, 0.6666667, 1), 
            userAlphaSpending = NA_real_), tolerance = 1e-6)
    design |> obtain(-3) |> expect_equal(c(delayedInformation = NA_real_))
    design |> obtain(-3, 1) |> expect_equal(list(delayedInformation = NA_real_, kMax = 3))
    design |> obtain(-3, "kMax") |> expect_equal(list(delayedInformation = NA_real_, kMax = 3))
    design |> obtain("kMax", informationRates) |> expect_equal(
        list(kMax = 3, informationRates = c(0.3333333, 0.6666667, 1)), tolerance = 1e-6)
    design |> obtain("kMax", informationRates, output = "labeled") |> expect_equal(
        list("Maximum number of stages" = 3, 
            "Information rates" = c(0.3333333, 0.6666667, 1)), tolerance = 1e-6)
    design |> obtain("kMax") |> expect_equal(c(kMax = 3))
    design |> obtain(kMax) |> expect_equal(c(kMax = 3))
    design |> obtain(kMax, output = "named") |> expect_equal(c(kMax = 3))
    design |> obtain(kMax, output = "labeled") |> expect_equal(c("Maximum number of stages" = 3))
    design |> obtain(kMax, output = "value") |> expect_equal(3)
    design |> obtain(kMax, output = "list") |> expect_equal(list(kMax = 3))
})

test_that("Test fetch/obtain method with analysis result", {
    .skipTestIfDisabled()
    .skipTestIfPipeOperatorNotAvailable()

    S <- getDataSet(
        events1 = c(11, 12),
        events2 = c(6, 7),
        n1 = c(36, 39),
        n2 = c(38, 40)
    )
    R <- getDataSet(
        events1 = c(12, 10),
        events2 = c(8, 8),
        n1 = c(32, 33),
        n2 = c(31, 29)
    )
    design <- getDesignInverseNormal(
        kMax = 2,
        typeOfDesign = "noEarlyEfficacy"
    )
    dataset <- getDataSet(S1 = S, R = R)
    result <- getAnalysisResults(
        design,
        dataset,
        stage = 1,
        nPlanned = 150,
        intersectionTest = "Simes"
    )
    
    expect_error(S |> obtain(xxx))
    expect_error(design |> obtain(xxx))
    expect_error(dataset |> obtain(xxx))
    expect_error(result |> obtain("xxx"))
    
    expect_equal(R |> obtain(sampleSizes), list(sampleSizes = c(32, 31, 33, 29)))
    expect_equal(R |> obtain(sampleSizes, output = "value"), c(32, 31, 33, 29))
    expect_equal(R |> obtain("sampleSizes"), list(sampleSizes = c(32, 31, 33, 29)))
    
    expect_equal(design |> obtain(1), c("kMax" = 2L))
    expect_equal(design |> obtain("kMax"), c("kMax" = 2L))
    expect_equal(design |> obtain(kMax, output = "value"), 2)
    expect_equal(design |> obtain(kMax, output = "list"), list("kMax" = 2L))
    
    expect_equal(dataset |> obtain(overallSampleSizes), list(overallSampleSizes = c(36, 32, 38, 31, 75, 65, 78, 60)))
    expect_equal(dataset |> obtain(overallSampleSizes, output = "value"), c(36, 32, 38, 31, 75, 65, 78, 60))
    expect_equal(dataset |> obtain(overallSampleSizes, output = "list"), list(overallSampleSizes = c(36, 32, 38, 31, 75, 65, 78, 60)))
    
    expect_equal(result |> obtain(conditionalPower), list(conditionalPower = matrix(c(NA_real_, NA_real_, 0.81442253, 0.72909925), ncol = 2)), tolerance = 1e-07)
    expect_equal(result |> obtain(conditionalPower, output = "value"), matrix(c(NA_real_, NA_real_, 0.81442253, 0.72909925), ncol = 2), tolerance = 1e-07)
    expect_equal(result |> obtain(conditionalPower, output = "list"), list(conditionalPower = matrix(c(NA_real_, NA_real_, 0.81442253, 0.72909925), ncol = 2)), tolerance = 1e-07)
    
    expect_equal(result |> fetch(-5), list(repeatedConfidenceIntervalLowerBounds = matrix(rep(NA_real_, 4), ncol = 2)), tolerance = 1e-07)
    
    expect_equal(result |> fetch(), c(stratifiedAnalysis = TRUE))
})
