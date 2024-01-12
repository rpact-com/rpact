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
## |  File version: $Revision: 7403 $
## |  Last changed: $Date: 2023-11-08 16:12:00 +0100 (Mi, 08 Nov 2023) $
## |  Last changed by: $Author: pahlke $
## |

test_plan_section("Testing Class 'SummaryFactory'")


test_that("Testing 'summary.ParameterSet': no errors occur", {
    .skipTestIfDisabled()

    # @refFS[Function]{fs:outputOfGenericFunctions}
    invisible(capture.output(expect_error(summary(getDesignGroupSequential(
        beta = 0.05, typeOfDesign = "asKD", gammaA = 1, typeBetaSpending = "bsOF"
    )), NA)))
    invisible(capture.output(expect_error(summary(getDesignGroupSequential(kMax = 1)), NA)))
    invisible(capture.output(expect_error(summary(getDesignGroupSequential(kMax = 4, sided = 2)), NA)))
    invisible(capture.output(expect_error(summary(getDesignGroupSequential(kMax = 4, sided = 2), digits = 0), NA)))
    invisible(capture.output(expect_error(summary(getDesignGroupSequential(kMax = 1, sided = 2)), NA)))
    invisible(capture.output(expect_error(summary(getDesignGroupSequential(futilityBounds = c(-6, 0))), NA)))
    invisible(capture.output(expect_error(summary(getDesignGroupSequential(futilityBounds = c(-6, 0)), digits = 5), NA)))

    invisible(capture.output(expect_error(summary(getDataset(
        n      = c(13, 25),
        means  = c(242, 222),
        stDevs = c(244, 221)
    )), NA)))

    invisible(capture.output(expect_error(summary(getDataset(
        n      = c(13),
        means  = c(242),
        stDevs = c(244)
    )), NA)))

    invisible(capture.output(expect_error(summary(getDataset(
        n1      = c(13, 25),
        n2      = c(15, NA),
        n3      = c(14, 27),
        n4      = c(12, 29),
        means1  = c(242, 222),
        means2  = c(188, NA),
        means3  = c(267, 277),
        means4  = c(92, 122),
        stDevs1 = c(244, 221),
        stDevs2 = c(212, NA),
        stDevs3 = c(256, 232),
        stDevs4 = c(215, 227)
    )), NA)))

    invisible(capture.output(expect_error(summary(getDataset(
        n1 = c(11, 13, 12, 13),
        n2 = c(8, 10, 9, 11),
        n3 = c(7, 10, 8, 9),
        events1 = c(10, 10, 12, 12),
        events2 = c(3, 5, 5, 6),
        events3 = c(2, 4, 3, 5)
    )), NA)))

    invisible(capture.output(expect_error(summary(getDataset(
        events1   = c(25, 32),
        events2   = c(18, NA),
        events3   = c(22, 36),
        logRanks1 = c(2.2, 1.8),
        logRanks2 = c(1.99, NA),
        logRanks3 = c(2.32, 2.11)
    )), NA)))

    invisible(capture.output(expect_error(summary(getDesignInverseNormal(kMax = 1)), NA)))
    invisible(capture.output(expect_error(summary(getDesignInverseNormal(futilityBounds = c(0, 1))), NA)))
    invisible(capture.output(expect_error(summary(getDesignInverseNormal(kMax = 1)), NA)))
    invisible(capture.output(expect_error(summary(getDesignInverseNormal(kMax = 4, sided = 2)), NA)))
    invisible(capture.output(expect_error(summary(getDesignInverseNormal(kMax = 4, sided = 2), digits = 0), NA)))
    invisible(capture.output(expect_error(summary(getDesignInverseNormal(kMax = 1, sided = 2)), NA)))

    invisible(capture.output(expect_error(summary(getDesignFisher()), NA)))
    invisible(capture.output(expect_error(summary(getDesignFisher(alpha0Vec = c(0.1, 0.2))), NA)))
    invisible(capture.output(expect_error(summary(getDesignFisher(kMax = 1)), NA)))
    invisible(capture.output(expect_error(summary(getDesignFisher(kMax = 4), digits = 5), NA)))
    invisible(capture.output(expect_error(summary(getDesignFisher(kMax = 4), digits = 0), NA)))
    invisible(capture.output(expect_error(summary(getDesignFisher(kMax = 1)), NA)))

    ## test design plans - means

    invisible(capture.output(expect_error(summary(getSampleSizeMeans(sided = 2, alternative = -0.5)), NA)))
    invisible(capture.output(expect_warning(summary(getSampleSizeMeans(sided = 2), alternative = -0.5)))) # warning expected
    invisible(capture.output(expect_error(summary(getPowerMeans(
        sided = 1, alternative = c(-0.5, -0.3),
        maxNumberOfSubjects = 100, directionUpper = FALSE
    )), NA)))
    invisible(capture.output(expect_error(summary(getSampleSizeMeans(
        thetaH0 = 0,
        alternative = 0.5, sided = 1, stDev = 2.5
    )), NA)))
    invisible(capture.output(expect_error(summary(getSampleSizeMeans(
        thetaH0 = 0,
        alternative = 0.5, sided = 1, stDev = 1, groups = 1
    )), NA)))
    invisible(capture.output(expect_error(summary(getSampleSizeMeans(
        thetaH0 = 0,
        sided = 2, stDev = 1, groups = 1
    )), NA)))

    invisible(capture.output(expect_error(summary(getSampleSizeMeans(
        thetaH0 = 0,
        alternative = 1.2, sided = 2, stDev = 5
    )), NA)))
    invisible(capture.output(expect_error(summary(getSampleSizeMeans(
        thetaH0 = 0,
        alternative = 1.2, sided = 2, stDev = 5, allocationRatioPlanned = 0
    )), NA)))
    invisible(capture.output(expect_error(summary(getSampleSizeMeans(
        thetaH0 = 0,
        alternative = 1.2, sided = 2, stDev = 5, groups = 1
    )), NA)))
    invisible(capture.output(expect_error(summary(getSampleSizeMeans(
        getDesignGroupSequential(futilityBounds = c(1, 2))
    )), NA)))
    invisible(capture.output(expect_error(summary(getSampleSizeMeans(
        getDesignGroupSequential(futilityBounds = c(1, 2))
    ), digits = 0), NA)))
    invisible(capture.output(expect_error(summary(getPowerMeans(
        getDesignGroupSequential(futilityBounds = c(1, 2)),
        maxNumberOfSubjects = 100, alternative = 1
    )), NA)))
    invisible(capture.output(expect_error(summary(getSampleSizeMeans(
        getDesignGroupSequential(kMax = 4, sided = 2)
    )), NA)))
    invisible(capture.output(expect_error(summary(getPowerMeans(
        getDesignGroupSequential(kMax = 4, sided = 2),
        maxNumberOfSubjects = 100
    )), NA)))
    invisible(capture.output(expect_error(summary(getPowerMeans(
        getDesignGroupSequential(kMax = 1, sided = 2),
        maxNumberOfSubjects = 100
    )), NA)))

    invisible(capture.output(expect_error(summary(getSampleSizeMeans(
        getDesignGroupSequential(futilityBounds = c(1, 2))
    )), NA)))
    invisible(capture.output(expect_error(summary(getSampleSizeMeans(
        getDesignGroupSequential(futilityBounds = c(1, 2))
    ), digits = 4), NA)))
    invisible(capture.output(expect_error(summary(getSampleSizeMeans(
        getDesignGroupSequential(futilityBounds = c(1, 2))
    ), digits = 3), NA)))
    invisible(capture.output(expect_error(summary(getSampleSizeMeans(
        getDesignGroupSequential(futilityBounds = c(1, 2))
    ), digits = 2), NA)))
    invisible(capture.output(expect_error(summary(getSampleSizeMeans(
        getDesignGroupSequential(futilityBounds = c(1, 2))
    ), digits = -1), NA)))

    ## test design plans - rates

    invisible(capture.output(expect_error(summary(getSampleSizeRates(pi2 = 0.3)), NA)))
    invisible(capture.output(expect_error(summary(getSampleSizeRates(groups = 1, thetaH0 = 0.3)), NA)))
    invisible(capture.output(expect_error(summary(getSampleSizeRates(groups = 1, thetaH0 = 0.45)), NA)))
    invisible(capture.output(expect_error(summary(getSampleSizeRates(
        groups = 2, thetaH0 = 0.45, allocationRatioPlanned = 0
    )), NA)))
    invisible(capture.output(expect_error(summary(getSampleSizeRates(
        getDesignGroupSequential(futilityBounds = c(1, 2))
    )), NA)))
    invisible(capture.output(expect_error(summary(getPowerRates(
        getDesignGroupSequential(futilityBounds = c(1, 2)),
        maxNumberOfSubjects = 100
    )), NA)))
    invisible(capture.output(expect_error(summary(getSampleSizeRates(
        getDesignGroupSequential(kMax = 4, sided = 2)
    )), NA)))
    invisible(capture.output(expect_error(summary(getSampleSizeRates(
        getDesignGroupSequential(kMax = 4, sided = 2),
        groups = 1, thetaH0 = 0.3
    )), NA)))
    invisible(capture.output(expect_error(summary(getSampleSizeRates(
        getDesignGroupSequential(kMax = 1, sided = 2),
        groups = 1, thetaH0 = 0.2, pi1 = c(0.4, 0.5)
    )), NA)))
    invisible(capture.output(expect_error(summary(getSampleSizeRates(
        getDesignGroupSequential(kMax = 1, sided = 2),
        groups = 1, thetaH0 = 0.2, pi1 = 0.4
    )), NA)))
    invisible(capture.output(expect_error(summary(getSampleSizeRates(
        getDesignGroupSequential(kMax = 1, sided = 2),
        groups = 2, thetaH0 = 0, pi1 = 0.25
    )), NA)))
    invisible(capture.output(expect_error(summary(getPowerRates(
        getDesignGroupSequential(kMax = 4, sided = 2),
        maxNumberOfSubjects = 100
    )), NA)))

    ## test design plans - survival

    invisible(capture.output(expect_error(summary(getSampleSizeSurvival()), NA)))
    invisible(capture.output(expect_error(summary(getSampleSizeSurvival(lambda2 = 0.3, hazardRatio = 1.2)), NA)))
    invisible(capture.output(expect_error(summary(getSampleSizeSurvival(lambda2 = 0.3, hazardRatio = c(1.2, 2))), NA)))
    invisible(capture.output(expect_error(summary(getSampleSizeSurvival(pi2 = 0.3, hazardRatio = 1.2)), NA)))
    invisible(capture.output(expect_error(summary(getSampleSizeSurvival(pi1 = 0.1, pi2 = 0.3)), NA)))

    invisible(capture.output(expect_error(summary(getSampleSizeSurvival(lambda2 = 0.03, lambda1 = c(0.040))), NA)))
    invisible(capture.output(expect_error(summary(getSampleSizeSurvival(piecewiseSurvivalTime = list(
        "0 - <6"   = 0.025,
        "6 - <9"   = 0.04,
        "9 - <15"  = 0.015,
        "15 - <21" = 0.01,
        ">=21"     = 0.007
    ), hazardRatio = 1.2)), NA)))
    invisible(capture.output(expect_error(summary(getSampleSizeSurvival(
        getDesignGroupSequential(futilityBounds = c(1, 2))
    )), NA)))
    invisible(capture.output(expect_error(summary(getPowerSurvival(
        getDesignGroupSequential(futilityBounds = c(1, 2)),
        maxNumberOfSubjects = 100, maxNumberOfEvents = 60
    )), NA)))
    invisible(capture.output(expect_error(summary(getSampleSizeSurvival(
        getDesignGroupSequential(kMax = 4, sided = 2)
    )), NA)))
    invisible(capture.output(expect_error(summary(getPowerSurvival(
        getDesignGroupSequential(kMax = 4, sided = 2),
        maxNumberOfSubjects = 100, maxNumberOfEvents = 60
    )), NA)))

    invisible(capture.output(expect_error(summary(getSampleSizeSurvival(sided = 2, lambda2 = log(2) / 6, lambda1 = log(2) / 8)), NA)))

    invisible(capture.output(expect_error(summary(getPowerSurvival(sided = 2, maxNumberOfSubjects = 200, maxNumberOfEvents = 40, lambda2 = log(2) / 6, lambda1 = log(2) / 8)), NA)))

    invisible(capture.output(expect_error(summary(getSampleSizeSurvival(
        getDesignGroupSequential(sided = 2),
        lambda2 = log(2) / 6, hazardRatio = c(0.55),
        accrualTime = c(0, 10), accrualIntensity = 20
    )), NA)))

    invisible(capture.output(expect_error(summary(getPowerSurvival(
        getDesignGroupSequential(kMax = 2),
        maxNumberOfEvents = 200, maxNumberOfSubjects = 400,
        lambda2 = log(2) / 60, lambda1 = log(2) / 50,
        dropoutRate1 = 0.025, dropoutRate2 = 0.025, dropoutTime = 12,
        accrualTime = 0, accrualIntensity = 30
    )), NA)))

    invisible(capture.output(expect_error(summary(getPowerSurvival(
        getDesignGroupSequential(kMax = 3),
        maxNumberOfEvents = 200, maxNumberOfSubjects = 400,
        lambda2 = log(2) / 60, lambda1 = c(log(2) / 50, log(2) / 60),
        dropoutRate1 = 0.025, dropoutRate2 = 0.025, dropoutTime = 12,
        accrualTime = 0, accrualIntensity = 30
    )), NA)))

    invisible(capture.output(expect_error(summary(getPowerSurvival(
        getDesignGroupSequential(kMax = 3),
        maxNumberOfEvents = 200, maxNumberOfSubjects = 400,
        lambda2 = log(2) / 60, hazardRatio = c(0.7, 0.8), directionUpper = FALSE,
        dropoutRate1 = 0.025, dropoutRate2 = 0.025, dropoutTime = 12,
        accrualTime = 0, accrualIntensity = 30
    )), NA)))

    design1 <- getDesignGroupSequential(
        sided = 2, alpha = 0.05, beta = 0.2,
        informationRates = c(0.6, 1),
        typeOfDesign = "asOF", twoSidedPower = FALSE
    )

    invisible(capture.output(expect_error(summary(getSampleSizeSurvival(
        design1,
        lambda2 = log(2) / 60, hazardRatio = 0.74,
        dropoutRate1 = 0.025, dropoutRate2 = 0.025, dropoutTime = 12,
        accrualTime = 0, accrualIntensity = 30,
        followUpTime = 12
    )), NA)))

    invisible(capture.output(expect_error(summary(getSampleSizeSurvival(
        design1,
        lambda2 = log(2) / 60, lambda1 = log(2) / 50,
        dropoutRate1 = 0.025, dropoutRate2 = 0.025, dropoutTime = 12,
        accrualTime = 0, accrualIntensity = 30,
        followUpTime = 12
    )), NA)))

    invisible(capture.output(expect_error(summary(getSampleSizeSurvival(
        getDesignGroupSequential(kMax = 4, sided = 2)
    )), NA)))

    ## simulations

    design2 <- getDesignInverseNormal(
        alpha = 0.05, kMax = 4, futilityBounds = c(0, 0, 0),
        sided = 1, typeOfDesign = "WT", deltaWT = 0.1
    )

    invisible(capture.output(expect_error(summary(getSimulationSurvival(design2,
        lambda2 = log(2) / 60, lambda1 = c(log(2) / 80),
        maxNumberOfSubjects = 1000, plannedEvents = c(50, 100, 150, 200), seed = 12345, directionUpper = FALSE
    )), NA)))

    invisible(capture.output(expect_error(summary(getSimulationSurvival(design2,
        lambda2 = log(2) / 60, hazardRatio = c(1.2, 1.4),
        maxNumberOfSubjects = 1000, plannedEvents = c(50, 100, 150, 200), seed = 12345
    )), NA)))

    design3 <- getDesignGroupSequential(typeOfDesign = "P", futilityBounds = c(1, 1))

    invisible(capture.output(expect_error(summary(getSampleSizeMeans(design3)), NA)))

    invisible(capture.output(expect_error(summary(getSimulationMeans(design3, stDev = 4, plannedSubjects = (1:3) * 200, alternative = c(1, 2))), NA)))

    invisible(capture.output(expect_error(summary(getSimulationRates(design3,
        plannedSubjects = (1:3) * 200, pi1 = c(0.3, 0.4), maxNumberOfIterations = 1000,
        minNumberOfSubjectsPerStage = c(NA, 40, 40), maxNumberOfSubjectsPerStage = c(NA, 40, 400), conditionalPower = 0.8
    )), NA)))

    invisible(capture.output(expect_error(summary(getSimulationMeans(
        getDesignGroupSequential(kMax = 1),
        stDev = 4, plannedSubjects = 200, alternative = c(1)
    )), NA)))
})

test_that("Testing 'summary.ParameterSet': output will be produced", {
    .skipTestIfDisabled()

    # @refFS[Function]{fs:outputOfGenericFunctions}
    expect_output(summary(getDesignGroupSequential(beta = 0.05, typeOfDesign = "asKD", gammaA = 1, typeBetaSpending = "bsOF"))$show())
    expect_output(summary(getDesignGroupSequential(kMax = 1))$show())
    expect_output(summary(getDesignGroupSequential(kMax = 4, sided = 2))$show())
    expect_output(summary(getDesignGroupSequential(kMax = 4, sided = 2), digits = 0)$show())
    expect_output(summary(getDesignGroupSequential(kMax = 1, sided = 2))$show())
    expect_output(summary(getDesignGroupSequential(futilityBounds = c(-6, 0)))$show())
    expect_output(summary(getDesignGroupSequential(futilityBounds = c(-6, 0)), digits = 5)$show())

    expect_output(summary(getDataset(
        n      = c(13, 25),
        means  = c(242, 222),
        stDevs = c(244, 221)
    ))$show())

    expect_output(summary(getDataset(
        n      = c(13),
        means  = c(242),
        stDevs = c(244)
    ))$show())

    expect_output(summary(getDataset(
        n1      = c(13, 25),
        n2      = c(15, NA),
        n3      = c(14, 27),
        n4      = c(12, 29),
        means1  = c(242, 222),
        means2  = c(188, NA),
        means3  = c(267, 277),
        means4  = c(92, 122),
        stDevs1 = c(244, 221),
        stDevs2 = c(212, NA),
        stDevs3 = c(256, 232),
        stDevs4 = c(215, 227)
    ))$show())

    expect_output(summary(getDataset(
        n1 = c(11, 13, 12, 13),
        n2 = c(8, 10, 9, 11),
        n3 = c(7, 10, 8, 9),
        events1 = c(10, 10, 12, 12),
        events2 = c(3, 5, 5, 6),
        events3 = c(2, 4, 3, 5)
    ))$show())

    expect_output(summary(getDataset(
        events1   = c(25, 32),
        events2   = c(18, NA),
        events3   = c(22, 36),
        logRanks1 = c(2.2, 1.8),
        logRanks2 = c(1.99, NA),
        logRanks3 = c(2.32, 2.11)
    ))$show())

    expect_output(summary(getDesignInverseNormal(kMax = 1))$show())
    expect_output(summary(getDesignInverseNormal(futilityBounds = c(0, 1)))$show())
    expect_output(summary(getDesignInverseNormal(kMax = 1))$show())
    expect_output(summary(getDesignInverseNormal(kMax = 4, sided = 2))$show())
    expect_output(summary(getDesignInverseNormal(kMax = 4, sided = 2), digits = 0)$show())
    expect_output(summary(getDesignInverseNormal(kMax = 1, sided = 2))$show())

    expect_output(summary(getDesignFisher())$show())
    expect_output(summary(getDesignFisher(alpha0Vec = c(0.1, 0.2)))$show())
    expect_output(summary(getDesignFisher(kMax = 1))$show())
    expect_output(summary(getDesignFisher(kMax = 4), digits = 5)$show())
    expect_output(summary(getDesignFisher(kMax = 4), digits = 0)$show())
    expect_output(summary(getDesignFisher(kMax = 1))$show())

    ## test design plans - means

    expect_output(summary(getSampleSizeMeans(sided = 2, alternative = -0.5))$show())
    expect_output(summary(getPowerMeans(sided = 1, alternative = c(-0.5, -0.3), maxNumberOfSubjects = 100, directionUpper = FALSE))$show())
    expect_output(summary(getSampleSizeMeans(thetaH0 = 0, alternative = 0.5, sided = 1, stDev = 2.5))$show())
    expect_output(summary(getSampleSizeMeans(thetaH0 = 0, alternative = 0.5, sided = 1, stDev = 1, groups = 1))$show())
    expect_output(summary(getSampleSizeMeans(thetaH0 = 0, sided = 2, stDev = 1, groups = 1))$show())

    expect_output(summary(getSampleSizeMeans(thetaH0 = 0, alternative = 1.2, sided = 2, stDev = 5))$show())
    expect_output(summary(getSampleSizeMeans(thetaH0 = 0, alternative = 1.2, sided = 2, stDev = 5, allocationRatioPlanned = 0))$show())
    expect_output(summary(getSampleSizeMeans(thetaH0 = 0, alternative = 1.2, sided = 2, stDev = 5, groups = 1))$show())
    expect_output(summary(getSampleSizeMeans(
        getDesignGroupSequential(futilityBounds = c(1, 2))
    ))$show())
    expect_output(summary(getSampleSizeMeans(
        getDesignGroupSequential(futilityBounds = c(1, 2))
    ), digits = 0)$show())
    expect_output(summary(getPowerMeans(
        getDesignGroupSequential(futilityBounds = c(1, 2)),
        maxNumberOfSubjects = 100, alternative = 1
    ))$show())
    expect_output(summary(getSampleSizeMeans(
        getDesignGroupSequential(kMax = 4, sided = 2)
    ))$show())
    expect_output(summary(getPowerMeans(
        getDesignGroupSequential(kMax = 4, sided = 2),
        maxNumberOfSubjects = 100
    ))$show())
    expect_output(summary(getPowerMeans(
        getDesignGroupSequential(kMax = 1, sided = 2),
        maxNumberOfSubjects = 100
    ))$show())

    expect_output(summary(getSampleSizeMeans(getDesignGroupSequential(futilityBounds = c(1, 2))))$show())
    expect_output(summary(getSampleSizeMeans(getDesignGroupSequential(futilityBounds = c(1, 2))), digits = 4)$show())
    expect_output(summary(getSampleSizeMeans(getDesignGroupSequential(futilityBounds = c(1, 2))), digits = 3)$show())
    expect_output(summary(getSampleSizeMeans(getDesignGroupSequential(futilityBounds = c(1, 2))), digits = 2)$show())
    expect_output(summary(getSampleSizeMeans(getDesignGroupSequential(futilityBounds = c(1, 2))), digits = -1)$show())

    ## test design plans - rates

    expect_output(summary(getSampleSizeRates(pi2 = 0.3))$show())
    expect_output(summary(getSampleSizeRates(groups = 1, thetaH0 = 0.3))$show())
    expect_output(summary(getSampleSizeRates(groups = 1, thetaH0 = 0.45))$show())
    expect_output(summary(getSampleSizeRates(groups = 2, thetaH0 = 0.45, allocationRatioPlanned = 0))$show())
    expect_output(summary(getSampleSizeRates(getDesignGroupSequential(futilityBounds = c(1, 2))))$show())
    expect_output(summary(getPowerRates(getDesignGroupSequential(futilityBounds = c(1, 2)), maxNumberOfSubjects = 100))$show())
    expect_output(summary(getSampleSizeRates(getDesignGroupSequential(kMax = 4, sided = 2)))$show())
    expect_output(summary(getSampleSizeRates(getDesignGroupSequential(kMax = 4, sided = 2), groups = 1, thetaH0 = 0.3))$show())
    expect_output(summary(getSampleSizeRates(getDesignGroupSequential(kMax = 1, sided = 2),
        groups = 1, thetaH0 = 0.2, pi1 = c(0.4, 0.5)
    ))$show())
    expect_output(summary(getSampleSizeRates(getDesignGroupSequential(
        kMax = 1, sided = 2
    ), groups = 1, thetaH0 = 0.2, pi1 = 0.4))$show())
    expect_output(summary(getSampleSizeRates(getDesignGroupSequential(
        kMax = 1, sided = 2
    ), groups = 2, thetaH0 = 0, pi1 = 0.25))$show())
    expect_output(summary(getPowerRates(getDesignGroupSequential(kMax = 4, sided = 2), maxNumberOfSubjects = 100))$show())

    ## test design plans - survival

    expect_output(summary(getSampleSizeSurvival())$show())
    expect_output(summary(getSampleSizeSurvival(lambda2 = 0.3, hazardRatio = 1.2))$show())
    expect_output(summary(getSampleSizeSurvival(lambda2 = 0.3, hazardRatio = c(1.2, 2)))$show())
    expect_output(summary(getSampleSizeSurvival(pi2 = 0.3, hazardRatio = 1.2))$show())
    expect_output(summary(getSampleSizeSurvival(pi1 = 0.1, pi2 = 0.3))$show())

    expect_output(summary(getSampleSizeSurvival(lambda2 = 0.03, lambda1 = c(0.040)))$show())
    expect_output(summary(getSampleSizeSurvival(piecewiseSurvivalTime = list(
        "0 - <6"   = 0.025,
        "6 - <9"   = 0.04,
        "9 - <15"  = 0.015,
        "15 - <21" = 0.01,
        ">=21"     = 0.007
    ), hazardRatio = 1.2))$show())
    expect_output(summary(getSampleSizeSurvival(getDesignGroupSequential(futilityBounds = c(1, 2))))$show())
    expect_output(summary(getPowerSurvival(getDesignGroupSequential(futilityBounds = c(1, 2)),
        maxNumberOfSubjects = 100, maxNumberOfEvents = 60
    ))$show())
    expect_output(summary(getSampleSizeSurvival(getDesignGroupSequential(kMax = 4, sided = 2)))$show())
    expect_output(summary(getPowerSurvival(getDesignGroupSequential(kMax = 4, sided = 2),
        maxNumberOfSubjects = 100, maxNumberOfEvents = 60
    ))$show())

    expect_output(summary(getSampleSizeSurvival(sided = 2, lambda2 = log(2) / 6, lambda1 = log(2) / 8))$show())

    expect_output(summary(getPowerSurvival(
        sided = 2, maxNumberOfSubjects = 200,
        maxNumberOfEvents = 40, lambda2 = log(2) / 6, lambda1 = log(2) / 8
    ))$show())

    expect_warning(
        expect_output(summary(getSampleSizeSurvival(getDesignGroupSequential(sided = 2),
            lambda2 = log(2) / 6, hazardRatio = c(0.55),
            accrualTime = c(0, 10), accrualIntensity = 60
        ))$show()),
        "Accrual duration longer than maximal study duration (time to maximal number of events); followUpTime = -2.959",
        fixed = TRUE
    )

    expect_output(summary(getPowerSurvival(getDesignGroupSequential(kMax = 2),
        maxNumberOfEvents = 150, maxNumberOfSubjects = 400,
        lambda2 = log(2) / 60, lambda1 = log(2) / 50,
        dropoutRate1 = 0.025, dropoutRate2 = 0.025, dropoutTime = 12,
        accrualTime = 0, accrualIntensity = 30
    ))$show())

    expect_output(summary(getPowerSurvival(getDesignGroupSequential(kMax = 3),
        maxNumberOfEvents = 200, maxNumberOfSubjects = 400,
        lambda2 = log(2) / 60, lambda1 = c(log(2) / 50, log(2) / 60),
        dropoutRate1 = 0.025, dropoutRate2 = 0.025, dropoutTime = 12,
        accrualTime = 0, accrualIntensity = 30
    ))$show())

    expect_output(summary(getPowerSurvival(getDesignGroupSequential(kMax = 3),
        maxNumberOfEvents = 200, maxNumberOfSubjects = 400,
        lambda2 = log(2) / 60, hazardRatio = c(0.7, 0.8), directionUpper = FALSE,
        dropoutRate1 = 0.025, dropoutRate2 = 0.025, dropoutTime = 12,
        accrualTime = 0, accrualIntensity = 30
    ))$show())

    design1 <- getDesignGroupSequential(
        sided = 2, alpha = 0.05, beta = 0.2,
        informationRates = c(0.6, 1),
        typeOfDesign = "asOF", twoSidedPower = FALSE
    )

    expect_output(summary(getSampleSizeSurvival(
        design1,
        lambda2 = log(2) / 60, hazardRatio = 0.74,
        dropoutRate1 = 0.025, dropoutRate2 = 0.025, dropoutTime = 12,
        accrualTime = 0, accrualIntensity = 30,
        followUpTime = 12
    ))$show())

    expect_output(summary(getSampleSizeSurvival(
        design1,
        lambda2 = log(2) / 60, lambda1 = log(2) / 50,
        dropoutRate1 = 0.025, dropoutRate2 = 0.025, dropoutTime = 12,
        accrualTime = 0, accrualIntensity = 30,
        followUpTime = 12
    ))$show())

    expect_output(summary(getSampleSizeSurvival(getDesignGroupSequential(kMax = 4, sided = 2)))$show())

    ## simulations

    design2 <- getDesignInverseNormal(
        alpha = 0.05, kMax = 4, futilityBounds = c(0, 0, 0),
        sided = 1, typeOfDesign = "WT", deltaWT = 0.1
    )

    expect_output(summary(getSimulationSurvival(design2,
        lambda2 = log(2) / 60, lambda1 = c(log(2) / 80),
        maxNumberOfSubjects = 1000, plannedEvents = c(50, 100, 150, 200), seed = 12345, directionUpper = FALSE
    ))$show())

    expect_output(summary(getSimulationSurvival(design2,
        lambda2 = log(2) / 60, hazardRatio = c(1.2, 1.4),
        maxNumberOfSubjects = 1000, plannedEvents = c(50, 100, 150, 200), seed = 12345
    ))$show())

    design3 <- getDesignGroupSequential(typeOfDesign = "P", futilityBounds = c(1, 1))

    expect_output(summary(getSampleSizeMeans(design3))$show())

    expect_output(summary(getSimulationMeans(design3, stDev = 4, plannedSubjects = (1:3) * 200, alternative = c(1, 2)))$show())

    expect_output(summary(getSimulationRates(design3,
        plannedSubjects = (1:3) * 200, pi1 = c(0.3, 0.4), maxNumberOfIterations = 1000,
        minNumberOfSubjectsPerStage = c(NA, 40, 40), maxNumberOfSubjectsPerStage = c(NA, 40, 400), conditionalPower = 0.8
    ))$show())

    expect_output(summary(getSimulationMeans(getDesignGroupSequential(kMax = 1),
        stDev = 4, plannedSubjects = 200, alternative = 1
    ))$show())
})
