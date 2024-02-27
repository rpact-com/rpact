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
## |  File version: $Revision: 7560 $
## |  Last changed: $Date: 2024-01-15 14:20:32 +0100 (Mo, 15 Jan 2024) $
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
    .skipTestIfDisabled()
    designGS1 <- getDesignGroupSequential(informationRates = c(0.2, 0.5, 1), 
        sided = 1, beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3)
    survivalDesignPlanEnabled <- .isTrialDesignPlanSurvival(getDesignGroupSequential(
        informationRates = c(0.2, 0.5, 1), sided = 1,
        beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3
    ))

    designPlan <- getDesignInverseNormal(
        typeOfDesign = "OF", kMax = 2, alpha =
            0.025, beta = 0.2, sided = 1, tolerance = 1e-08
    )
    designPlan <- getSampleSizeMeans(designPlan,
        meanRatio = FALSE, thetaH0 = 0,
        normalApproximation = FALSE, alternative = 0.2, stDev = 1, groups =
            2, allocationRatioPlanned = 1
    )

    designPlan_power <- getDesignInverseNormal(
        typeOfDesign = "OF", kMax = 2, alpha =
            0.025, beta = 0.2, sided = 1, tolerance = 1e-08
    )
    designPlan_power <- getPowerMeans(designPlan_power,
        meanRatio = FALSE, thetaH0 = 0, normalApproximation =
            FALSE, alternative = 0.2, stDev = 1, groups = 2,
        allocationRatioPlanned = 1, directionUpper = TRUE,
        maxNumberOfSubjects = 200
    )

    designPlan_surv <- getDesignInverseNormal(
        typeOfDesign = "OF", kMax = 2, alpha =
            0.025, beta = 0.2, sided = 1, tolerance = 1e-08
    )
    designPlan_surv <- getSampleSizeSurvival(designPlan_surv,
        thetaH0 = 1, typeOfComputation =
            "Schoenfeld", pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 1,
        eventTime = 12, accrualTime = c(0, 12), kappa = 1, followUpTime =
            6, dropoutRate1 = 0, dropoutRate2 = 0, dropoutTime = 12,
        accrualIntensity = NA_real_
    )

    designPlan_surv_pwr <- getDesignInverseNormal(
        typeOfDesign = "OF", kMax = 2, alpha =
            0.025, beta = 0.2, sided = 1, tolerance = 1e-08
    )
    designPlan_surv_pwr <- getPowerSurvival(designPlan_surv_pwr,
        thetaH0 = 1, typeOfComputation = "Schoenfeld",
        directionUpper = TRUE, pi1 = 0.4, pi2 = 0.2, maxNumberOfSubjects =
            200, maxNumberOfEvents = 100, allocationRatioPlanned = 1,
        eventTime = 12, accrualTime = c(0, 12), kappa = 1, dropoutRate1 = 0,
        dropoutRate2 = 0, dropoutTime = 12, accrualIntensity = NA_real_
    )
    designPlan_rates <- getDesignInverseNormal(
        typeOfDesign = "OF", kMax = 2, alpha =
            0.025, beta = 0.2, sided = 1, tolerance = 1e-08
    )
    designPlan_rates <- getSampleSizeRates(designPlan_rates,
        riskRatio = FALSE, thetaH0 = 0,
        pi1 = 0.4, pi2 = 0.2, groups = 2,
        allocationRatioPlanned = 1
    )
    designPlan_rates_pwr <- getDesignInverseNormal(
        typeOfDesign = "OF", kMax = 2, alpha =
            0.025, beta = 0.2, sided = 1, tolerance = 1e-08
    )
    designPlan_rates_pwr <- getPowerRates(designPlan_rates_pwr,
        riskRatio = FALSE, thetaH0 = 0,
        pi1 = 0.4, pi2 = 0.2, groups = 2, allocationRatioPlanned = 1,
        directionUpper = TRUE, maxNumberOfSubjects = 200
    )

    designPlan_2 <- getDesignInverseNormal(
        typeOfDesign = "OF", kMax = 2, alpha =
            0.025, beta = 0.2, sided = 1, tolerance = 1e-08
    )
    designPlan_2 <- getSampleSizeMeans(designPlan_2,
        meanRatio = FALSE, thetaH0 = 0,
        normalApproximation = FALSE, alternative = 0.2, stDev = 1, groups =
            2, allocationRatioPlanned = 1
    )
    designPlan_2$`.design`$sided <- as.integer(1)
    type <- c(1:4)
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
    for (i in type) {
        result <- .plotTrialDesignPlan(designPlan, type[i], main, xlab, ylab, palette, theta, 
            plotPointsEnabled, legendPosition, showSource, designPlanName, plotSettings)
        result_2 <- .plotTrialDesignPlan(designPlan_2, type[i], main, xlab, ylab, palette, theta, 
            plotPointsEnabled, legendPosition, showSource, designPlanName, plotSettings)
        result_pwr <- .plotTrialDesignPlan(designPlan_power, type[i], main, xlab, ylab, palette, theta, 
            plotPointsEnabled, legendPosition, showSource, designPlanName, plotSettings)
        result_surv <- .plotTrialDesignPlan(designPlan_surv, type[i], main, xlab, ylab, palette, theta, 
            plotPointsEnabled, legendPosition, showSource, designPlanName, plotSettings)
        result_surv_pwr <- .plotTrialDesignPlan(designPlan_surv_pwr, type[i], main, xlab, ylab, palette, theta, 
            plotPointsEnabled, legendPosition, showSource, designPlanName, plotSettings)
        result_rates <- .plotTrialDesignPlan(designPlan_rates, type[i], main, xlab, ylab, palette, theta, 
            plotPointsEnabled, legendPosition, showSource, designPlanName, plotSettings)
        result_rates_pwr <- .plotTrialDesignPlan(designPlan_rates_pwr, type[i], main, xlab, ylab, palette, theta, 
            plotPointsEnabled, legendPosition, showSource, designPlanName, plotSettings)
        expect_type(result, "list")
        expect_type(result_2, "list")
        expect_type(result_pwr, "list")
        expect_type(result_surv, "list")
        expect_type(result_surv_pwr, "list")
        expect_type(result_rates, "list")
        expect_type(result_rates_pwr, "list")
    }
    type_bad <- c(5, 8, 10, 11, 12, 13, 14)
    for (i in type_bad) {
        expect_error(.plotTrialDesignPlan(designPlan_power, type_bad[i], main, xlab, ylab, palette, theta, 
                plotPointsEnabled, legendPosition, showSource, designPlanName, plotSettings))
    }
    expect_error(.plotTrialDesignPlan())
    expect_error(.plotTrialDesignPlan(designGS1))

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

test_that("warnings works as intended", {
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
