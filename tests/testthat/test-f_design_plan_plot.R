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
## |  File name: test-f_design_plan_means.R
## |  Creation date: 21 December 2023, 08:52:45
## |  File version: $Revision: 7961 $
## |  Last changed: $Date: 2024-05-30 14:58:05 +0200 (Do, 30 Mai 2024) $
## |  Last changed by: $Author: pahlke $
## |

test_plan_section("Testing Design Plots")

test_that(".addPlotSubTitleItems function works as expected", {
    .skipTestIfDisabled()

    designPlan <- getDesignInverseNormal(
        typeOfDesign = "OF", kMax = 2, alpha =
            0.025, beta = 0.2, sided = 1, tolerance = 1e-08
    )
    designMaster <- designPlan
    items <- list()
    type <- 2
    result <- .addPlotSubTitleItems(designPlan, designMaster, items, type)
    expect_equal(result, invisible())
    expect_error(.addPlotSubTitleItems())
})

# Test case for .assertIsValidVariedParameterVectorForPlotting function
test_that(".assertIsValidVariedParameterVectorForPlotting function works as expected", {
    .skipTestIfDisabled()
    
    designPlan <- getDesignInverseNormal(
        typeOfDesign = "OF", kMax = 2, alpha =
            0.025, beta = 0.2, sided = 1, tolerance = 1e-08
    )
    plotType <- 1
    result <- .assertIsValidVariedParameterVectorForPlotting(designPlan, plotType)
    expect_null(result)
    expect_error(.assertIsValidVariedParameterVectorForPlotting())
})

# Test case for .getTrialDesignPlanTheta function
test_that(".getTrialDesignPlanTheta function works as expected", {
    .skipTestIfDisabled()
    
    survivalDesignPlanEnabled <- .isTrialDesignPlanSurvival(getDesignGroupSequential(
        informationRates = c(0.2, 0.5, 1), sided = 1,
        beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3
    ))
    designPlan <- list()
    theta <- NA
    result <- .getTrialDesignPlanTheta(designPlan, theta)
    expect_equal(result, list(theta = NA_real_, thetaName = NA_character_))
    expect_error(.getTrialDesignPlanTheta())
})

# Test case for .plotTrialDesignPlan function
test_that(".plotTrialDesignPlan function works as expected", {
    .skipTestIfDisabled(ignoreInTestPlan = TRUE)

    design <- getDesignInverseNormal(
        typeOfDesign = "OF", kMax = 2, alpha =
            0.025, beta = 0.2, sided = 1, tolerance = 1e-08
    )
    sampleSizeMeansResult <- getSampleSizeMeans(design,
        meanRatio = FALSE, thetaH0 = 0,
        normalApproximation = FALSE, alternative = 0.2, stDev = 1, groups =
            2, allocationRatioPlanned = 1
    )
    powerMeansResult <- getPowerMeans(design,
        meanRatio = FALSE, thetaH0 = 0, normalApproximation =
            FALSE, alternative = 0.2, stDev = 1, groups = 2,
        allocationRatioPlanned = 1, directionUpper = TRUE,
        maxNumberOfSubjects = 200
    )
    sampleSizeSurvivalResult <- getSampleSizeSurvival(design,
        thetaH0 = 1, typeOfComputation =
            "Schoenfeld", pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 1,
        eventTime = 12, accrualTime = c(0, 12), kappa = 1, followUpTime =
            6, dropoutRate1 = 0, dropoutRate2 = 0, dropoutTime = 12,
        accrualIntensity = NA_real_
    )
    powerSurvivalResult <- getPowerSurvival(design,
        thetaH0 = 1, typeOfComputation = "Schoenfeld",
        directionUpper = TRUE, pi1 = 0.4, pi2 = 0.2, maxNumberOfSubjects =
            200, maxNumberOfEvents = 100, allocationRatioPlanned = 1,
        eventTime = 12, accrualTime = c(0, 12), kappa = 1, dropoutRate1 = 0,
        dropoutRate2 = 0, dropoutTime = 12, accrualIntensity = NA_real_
    )
    sampleSizeRatesResult <- getSampleSizeRates(design,
        riskRatio = FALSE, thetaH0 = 0,
        pi1 = 0.4, pi2 = 0.2, groups = 2,
        allocationRatioPlanned = 1
    )
    powerRatesResult <- getPowerRates(design,
        riskRatio = FALSE, thetaH0 = 0,
        pi1 = 0.4, pi2 = 0.2, groups = 2, allocationRatioPlanned = 1,
        directionUpper = TRUE, maxNumberOfSubjects = 200
    )
    sampleSizeMeansResult$.design$sided <- as.integer(1)
    main <- NA_character_
    xlab <- NA_character_
    ylab <- NA_character_
    palette <- "Set1"
    theta <- NA_real_
    plotPointsEnabled <- NA
    legendPosition <- NA_integer_
    showSource <- FALSE
    designPlanName <- NA_character_
    plotSettings <- NULL
    for (type in c(1:4)) {
        result1 <- .plotTrialDesignPlan(
            sampleSizeMeansResult, type, main, xlab, ylab, palette, theta,
            plotPointsEnabled, legendPosition, showSource, designPlanName, plotSettings
        )
        result2 <- .plotTrialDesignPlan(
            powerMeansResult, type, main, xlab, ylab, palette, theta,
            plotPointsEnabled, legendPosition, showSource, designPlanName, plotSettings
        )
        result3 <- .plotTrialDesignPlan(
            sampleSizeSurvivalResult, type, main, xlab, ylab, palette, theta,
            plotPointsEnabled, legendPosition, showSource, designPlanName, plotSettings
        )
        result4 <- .plotTrialDesignPlan(
            powerSurvivalResult, type, main, xlab, ylab, palette, theta,
            plotPointsEnabled, legendPosition, showSource, designPlanName, plotSettings
        )
        result5 <- .plotTrialDesignPlan(
            sampleSizeRatesResult, type, main, xlab, ylab, palette, theta,
            plotPointsEnabled, legendPosition, showSource, designPlanName, plotSettings
        )
        result6 <- .plotTrialDesignPlan(
            powerRatesResult, type, main, xlab, ylab, palette, theta,
            plotPointsEnabled, legendPosition, showSource, designPlanName, plotSettings
        )
        expect_type(result1, "list")
        expect_type(result2, "list")
        expect_type(result3, "list")
        expect_type(result4, "list")
        expect_type(result5, "list")
        expect_type(result6, "list")
    }

    for (illegalType in c(5, 8, 10, 11, 12, 13, 14)) {
        expect_error(.plotTrialDesignPlan(
            designPlan_power, illegalType, main, xlab, ylab, palette, theta,
            plotPointsEnabled, legendPosition, showSource, designPlanName, plotSettings
        ))
    }

    expect_error(.plotTrialDesignPlan())

    designGS <- getDesignGroupSequential(
        informationRates = c(0.2, 0.5, 1),
        sided = 1, beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3
    )
    expect_equal(.isTrialDesignPlanSurvival(designGS), FALSE)

    expect_error(.plotTrialDesignPlan(designGS))

    expect_error(.warnInCaseOfUnusedValuesForPlottingMeans())
    expect_error(.warnInCaseOfUnusedValuesForPlottingRates())
    expect_error(.warnInCaseOfUnusedValuesForPlottingSurvival())
    expect_error(.warnInCaseOfUnusedValuesForPlotting())
})

test_that(".getSurvivalFunctionPlotCommand works as intended", {
    .skipTestIfDisabled()
    
    expect_error(.getSurvivalFunctionPlotCommand())
})

test_that(".plotSurvivalFunction works as intended", {
    .skipTestIfDisabled()
    
    expect_error(.plotSurvivalFunction())
})

test_that("The plot warning functions work as intended", {
    .skipTestIfDisabled()
    
    expect_error(.warnInCaseOfUnusedValuesForPlottingMeans())
    expect_error(.warnInCaseOfUnusedValuesForPlottingRates())
    expect_error(.warnInCaseOfUnusedValuesForPlottingSurvival())
    expect_error(.warnInCaseOfUnusedValuesForPlotting())
})

test_that("plot.TrialDesignPlan works as intended", {
    .skipTestIfDisabled()
    
    expect_error(plot.TrialDesignPlan())
})

test_that("The plot of a getPowerSurvival() result works as intended", {
    .skipTestIfDisabled()
    
    design <- getDesignGroupSequential(
        kMax = 3, typeOfDesign = "OF",
        sided = 2, twoSidedPower = TRUE
    )
    piecewiseSurvivalTime <- list(
        "<5" = 0.04,
        "5 - <10" = 0.02,
        ">= 10" = 0.008
    )
    powerSurvival <- getPowerSurvival(
        design = design,
        typeOfComputation = "Schoenfeld", thetaH0 = 1,
        allocationRatioPlanned = 1, kappa = 1,
        piecewiseSurvivalTime = piecewiseSurvivalTime,
        maxNumberOfSubjects = 2480, maxNumberOfEvents = 70,
        hazardRatio = c(0.5, 2)
    )
    expect_silent(plot(powerSurvival, type = 1))
    expect_silent(plot(powerSurvival, type = 2))
    expect_silent(plot(powerSurvival, type = 12))
    
    powerSurvival2 <- getPowerSurvival(
        design = design,
        typeOfComputation = "Schoenfeld", thetaH0 = 1,
        allocationRatioPlanned = 1, kappa = 1,
        piecewiseSurvivalTime = piecewiseSurvivalTime,
        maxNumberOfSubjects = 2480, maxNumberOfEvents = 70,
        hazardRatio = 0.5
    )
    
    expect_silent(plot(powerSurvival2, type = 13, legendPosition = 1))
    expect_silent(plot(powerSurvival2, type = 14, legendPosition = 5))
})
