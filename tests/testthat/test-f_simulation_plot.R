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
## |  File version: $Revision: 7682 $
## |  Last changed: $Date: 2024-03-05 07:53:40 +0100 (Di, 05 Mrz 2024) $
## |  Last changed by: $Author: pahlke $
## |

test_plan_section("Testing Simulation Plots")

test_that(".assertIsValidVariedParameterVectorForSimulationResultsPlotting handles inputs correctly", {
    .skipTestIfDisabled()

    simulationResults <- list(
        "SimulationResultsMeans" = list(alternative = c(1, 2, 3)),
        "SimulationResultsRates" = list(pi1 = c(1, 2, 3)),
        "SimulationResultsSurvival" = list(hazardRatio = c(1, 2, 3), overallReject = c(1, 2, 3))
    )

    expect_null(.assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, "SimulationResultsMeans"))
    expect_error(.assertIsValidVariedParameterVectorForSimulationResultsPlotting())

    simulationResults$SimulationResultsMeans$alternative <- c(1)
    expect_null(.assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, "SimulationResultsMeans"))
})

test_that(".assertIsValidVariedParameterVectorForSimulationResultsPlotting handles valid inputs", {
    .skipTestIfDisabled()

    simulationResults <- list("SimulationResultsMeans" = list(alternative = c(1, 2, 3)))
    expect_null(.assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, "SimulationResultsMeans"))
})

test_that(".assertIsValidVariedParameterVectorForSimulationResultsPlotting handles invalid inputs", {
    .skipTestIfDisabled()

    simulationResults <- list("SimulationResultsMeans" = list(alternative = c(1)))
    expect_null(.assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, "SimulationResultsMeans"))
})

test_that(".getSimulationPlotXAxisParameterName handles SimulationResultsEnrichment", {
    .skipTestIfDisabled()

    simulationResults <- list("SimulationResultsEnrichment" = list(effectData = matrix(1:2, nrow = 1)))
    expect_equal(.getSimulationPlotXAxisParameterName(simulationResults, TRUE), "effect")
    expect_error(.getSimulationPlotXAxisParameterName())
})

test_that(".getSimulationPlotXAxisParameterName handles SimulationResultsSurvival", {
    .skipTestIfDisabled()

    simulationResults <- list("SimulationResultsSurvival" = list())
    expect_equal(.getSimulationPlotXAxisParameterName(simulationResults, TRUE), "effect")
})

test_that(".getSimulationPlotXAxisLabel handles SimulationResultsEnrichment", {
    .skipTestIfDisabled()

    simulationResults <- list("SimulationResultsEnrichment" = list(effectData = matrix(1:2, nrow = 1)))
    expect_equal(.getSimulationPlotXAxisLabel(simulationResults), "Effect")
    expect_error(.getSimulationPlotXAxisLabel())
})

test_that(".getSimulationPlotXAxisLabel handles SimulationResultsSurvival", {
    .skipTestIfDisabled()

    simulationResults <- list("SimulationResultsSurvival" = list())
    expect_equal(.getSimulationPlotXAxisLabel(simulationResults), "Effect")
})

test_that(".getPowerAndStoppingProbabilities handles expectedNumberOfEvents", {
    .skipTestIfDisabled()

    simulationResults <- list(
        "expectedNumberOfEvents" = c(1, 2, 3),
        ".parameterNames" = list("expectedNumberOfEvents" = "Expected Number of Events")
    )
    result <- .getPowerAndStoppingProbabilities(simulationResults, xValues = c(1, 2, 3), parameters = c("expectedNumberOfEvents"))

    expect_equal(nrow(result$data), length(c(1, 2, 3)))
    expect_equal(result$data$yValues, c(1, 2, 3))
})

test_that(".getPowerAndStoppingProbabilities handles expectedNumberOfSubjects", {
    .skipTestIfDisabled()

    simulationResults <- list(
        "expectedNumberOfSubjects" = c(1, 2, 3),
        ".parameterNames" = list("expectedNumberOfSubjects" = "Expected Number of Subjects")
    )
    result <- .getPowerAndStoppingProbabilities(simulationResults, xValues = c(1, 2, 3), parameters = c("expectedNumberOfSubjects"))

    expect_equal(nrow(result$data), length(c(1, 2, 3)))
    expect_equal(result$data$yValues, c(1, 2, 3))
})

test_that("Plot simulation means results", {
    .skipTestIfDisabled()

    maxNumberOfSubjects <- 90
    informationRates <- c(0.2, 0.5, 1)
    plannedSubjects <- round(informationRates * maxNumberOfSubjects)
    design <- getDesignInverseNormal(
        futilityBounds = c(-0.5, 0.5),
        informationRates = informationRates
    )
    simulationMeansResult <- getSimulationMeans(
        design = design, groups = 2, meanRatio = TRUE,
        thetaH0 = 0.4, plannedSubjects = plannedSubjects,
        maxNumberOfIterations = 500, allocationRatioPlanned = 3,
        stDev = 1.5, seed = 1234567890
    )
    expect_silent(plot(simulationMeansResult, type = "all", grid = 0))
})

test_that("Plot simulation rates results", {
    .skipTestIfDisabled()

    maxNumberOfSubjects <- 90
    informationRates <- (1:3) / 3
    plannedSubjects <- round(informationRates * maxNumberOfSubjects)
    design <- getDesignInverseNormal(
        futilityBounds = c(-0.5, 0.5),
        informationRates = informationRates
    )
    simulationRatesResult <- getSimulationRates(
        design = getDesignFisher(),
        groups = 2, riskRatio = TRUE,
        thetaH0 = 0.8, plannedSubjects = plannedSubjects,
        maxNumberOfIterations = 500, allocationRatioPlanned = 3,
        seed = 1234567890
    )
    expect_silent(plot(simulationRatesResult, type = "all", grid = 0))
})

test_that("Plot simulation survival results", {
    .skipTestIfDisabled()

    design <- getDesignFisher(kMax = 3, alpha0Vec = c(0.5, 0.5))
    simulationSurvivalResult <- getSimulationSurvival(
        design = design, pi2 = 0.6,
        pi1 = seq(0.3, 0.45, 0.05), directionUpper = FALSE,
        maxNumberOfSubjects = 500, plannedEvents = (1:design$kMax) * 20,
        allocation1 = 1, allocation2 = 1, accrualTime = c(0, 3, 6, 12),
        accrualIntensity = c(0.1, 0.2, 0.2), dropoutRate1 = 0,
        dropoutRate2 = 0, dropoutTime = 12, conditionalPower = 0.8,
        minNumberOfEventsPerStage = c(NA_real_, 10, 10),
        maxNumberOfEventsPerStage = c(NA_real_, 100, 200),
        maxNumberOfIterations = 500, seed = 1234567890
    )
    expect_silent(plot(simulationSurvivalResult, type = 4, grid = 0))
})

test_that("Plot simulation piecewise survival time results", {
    .skipTestIfDisabled()

    design <- getDesignGroupSequential(kMax = 3, typeOfDesign = "WT", deltaWT = 0.25)
    piecewiseSurvivalTime <- list(
        "<6" = 0.025,
        "6 - <9" = 0.04,
        "9 - <15" = 0.015,
        "15 - <21" = 0.01,
        ">=21" = 0.007
    )
    simulationSurvivalResult <- getSimulationSurvival(
        design = design,
        directionUpper = TRUE, maxNumberOfSubjects = 500,
        plannedEvents = (1:design$kMax) * 20, allocation1 = 1,
        allocation2 = 1, accrualTime = c(0, 3, 6, 12),
        piecewiseSurvivalTime = piecewiseSurvivalTime, hazardRatio = 1.7,
        accrualIntensity = c(0.1, 0.2, 0.2), dropoutRate1 = 0,
        dropoutRate2 = 0, dropoutTime = 12, conditionalPower = 0.8,
        minNumberOfEventsPerStage = c(NA_real_, 10, 10),
        maxNumberOfEventsPerStage = c(NA_real_, 100, 200),
        maxNumberOfIterations = 500, seed = 1234567890
    )
    expect_silent(plot(simulationSurvivalResult, type = "all", grid = 0))
})

test_that("Plot simulation multi-arm means results", {
    .skipTestIfDisabled()

    design <- getDesignInverseNormal(
        informationRates = c(0.2, 0.6, 1),
        futilityBounds = c(-0.5, 0.5)
    )
    x <- getSimulationMultiArmMeans(
        design = design, typeOfShape = "linear",
        activeArms = 4, plannedSubjects = c(10, 30, 50), stDev = 1.2,
        muMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
        conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4),
        maxNumberOfSubjectsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 500, seed = 1234567890
    )
    expect_silent(plot(x, type = "all", grid = 0))
})

test_that("Plot simulation multi-arm rates results", {
    .skipTestIfDisabled()

    design <- getDesignInverseNormal(
        informationRates = c(0.2, 0.6, 1),
        futilityBounds = c(-0.5, 0.5)
    )
    x <- getSimulationMultiArmRates(
        design = design, typeOfShape = "linear",
        activeArms = 4, plannedSubjects = c(10, 30, 50),
        piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1),
        adaptations = rep(TRUE, 2), conditionalPower = 0.8,
        minNumberOfSubjectsPerStage = c(10, 4, 4),
        maxNumberOfSubjectsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 500, seed = 1234567890
    )
    expect_silent(plot(x, type = "all", grid = 0))
})

test_that("Plot simulation multi-arm survival results", {
    .skipTestIfDisabled()

    design <- getDesignInverseNormal(
        informationRates = c(0.2, 0.6, 1),
        futilityBounds = c(-0.5, 0.5)
    )
    x <- getSimulationMultiArmSurvival(
        design = design, activeArms = 4,
        typeOfSelection = "rBest", rValue = 2, plannedEvents = c(10, 30, 50),
        omegaMaxVector = seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2),
        conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4),
        maxNumberOfEventsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 500, seed = 1234567890
    )
    expect_silent(plot(x, type = "all", grid = 0))
})

test_that("Plot simulation enrichment means results", {
    .skipTestIfDisabled()

    design <- getDesignInverseNormal(
        informationRates = c(0.2, 0.6, 1),
        futilityBounds = c(-0.5, 0.5)
    )
    # Define subgroups and their prevalences
    subGroups <- c("S1", "S12", "S2", "R") # fixed names!
    prevalences <- c(0.2, 0.3, 0.4, 0.1)

    effectR <- 1.5
    effectS12 <- 5
    m <- c()
    for (effectS1 in seq(0, 5, 5)) {
        for (effectS2 in seq(0, 5, 5)) {
            m <- c(m, effectS1, effectS12, effectS2, effectR)
        }
    }
    effects <- matrix(m, byrow = TRUE, ncol = 4)
    stDev <- 10
    # Define effect list
    el <- list(
        subGroups = subGroups, prevalences = prevalences,
        stDevs = stDev, effects = effects
    )

    suppressWarnings(x <- getSimulationEnrichmentMeans(
        design = design,
        plannedSubjects = c(10, 30, 50),
        effectList = el,
        adaptations = rep(TRUE, 2),
        conditionalPower = 0.8,
        minNumberOfSubjectsPerStage = c(10, 4, 4),
        maxNumberOfSubjectsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 500,
        seed = 1234567890
    ))

    suppressWarnings(expect_silent(plot(x, type = "all", grid = 0)))
})

test_that("Plot simulation enrichment rates results", {
    .skipTestIfDisabled()

    design <- getDesignInverseNormal(
        informationRates = c(0.2, 0.6, 1),
        futilityBounds = c(-0.5, 0.5)
    )
    # Define effect list
    subGroups <- c("S", "R")
    prevalences <- c(0.4, 0.6)
    piControl <- c(0.1, 0.4)
    range1 <- piControl[1] + seq(0.0, 0.2, 0.1)
    range2 <- piControl[2] + seq(0.0, 0.2, 0.1)
    piTreatments <- c()
    for (x1 in range1) {
        for (x2 in range2) {
            piTreatments <- c(piTreatments, x1, x2)
        }
    }
    el <- list(
        subGroups = subGroups,
        prevalences = prevalences,
        piControl = piControl,
        piTreatments = matrix(piTreatments,
            byrow = TRUE, ncol = 2
        )
    )

    suppressWarnings(x <- getSimulationEnrichmentRates(
        design = design,
        plannedSubjects = c(10, 30, 50),
        effectList = el,
        maxNumberOfIterations = 500,
        seed = 1234567890
    ))

    suppressWarnings(expect_silent(plot(x, type = "all", grid = 0)))
})


test_that("Plot simulation enrichment survival results", {
    .skipTestIfDisabled()

    # Define subgroups and their prevalences
    subGroups <- c("S1", "S2", "S12", "R") # fixed names!
    prevalences <- c(0.2, 0.3, 0.4, 0.1)

    piControls <- c(0.2, 0.4, 0.15, 0.3)
    effect <- c(-0.05, -0.02, -0.10, -0.10)
    piTreatments <- piControls + effect

    hr <- log(1 - piTreatments) / log(1 - piControls)

    # Define effect list
    el <- list(
        subGroups = subGroups, prevalences = prevalences,
        piControls = piControls, hazardRatios = matrix(rep(hr, 3), nrow = 3)
    )

    # Perform simulation
    suppressWarnings(x <- getSimulationEnrichmentSurvival(
        design = getDesignInverseNormal(typeOfDesign = "noEarlyEfficacy"),
        effectList = el,
        typeOfSelection = "rbest",
        rValue = 2,
        intersectionTest = "Simes",
        plannedEvents = c(30, 80, 120),
        maxNumberOfIterations = 500,
        directionUpper = FALSE
    ))

    suppressWarnings(expect_silent(plot(x, type = "all", grid = 0)))
})

test_that("Plot simulation results with wrong plot types", {
    .skipTestIfDisabled()

    simulationResults <- getSimulationSurvival(
        maxNumberOfSubjects = 200, plannedEvents = 50,
        accrualTime = c(0, 3, 6, 12), accrualIntensity = c(0.1, 0.2, 0.2),
        maxNumberOfIterations = 100, seed = 1234567890
    )
    designMaster <- list("kMax" = 1)
    expect_silent(.plotSimulationResults(simulationResults, designMaster, type = 4))
    expect_error(.plotSimulationResults(simulationResults, designMaster, type = 1))
    expect_error(.plotSimulationResults(simulationResults, designMaster, type = 2))
    expect_error(.plotSimulationResults(simulationResults, designMaster, type = 3))
    expect_error(.plotSimulationResults())
})
