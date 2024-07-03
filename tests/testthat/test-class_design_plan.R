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
## |  File name: test-class_design_plan.R
## |  Creation date: 27 May 2024, 11:11:19
## |  File version: $Revision: 7968 $
## |  Last changed: $Date: 2024-06-07 15:03:21 +0200 (Fr, 07 Jun 2024) $
## |  Last changed by: $Author: pahlke $
## |

test_plan_section("Testing Class 'TrialDesignPlan'")


test_that("Test design plan classes and utility functions", {
    expect_error(.addPlotSubTitleItems())
})

test_that("Sample size means result object clone function", {
    x1 <- getSampleSizeMeans(groups = 1, thetaH0 = 0.1, stDev = 2)
    x2 <- x1$clone()


    ## Pairwise comparison of the results of x1 with the results of x2
    expect_equal(x1$meanRatio, x2$meanRatio)
    expect_equal(x1$thetaH0, x2$thetaH0, tolerance = 1e-07)
    expect_equal(x1$normalApproximation, x2$normalApproximation)
    expect_equal(x1$alternative, x2$alternative, tolerance = 1e-07)
    expect_equal(x1$stDev, x2$stDev)
    expect_equal(x1$groups, x2$groups)
    expect_equal(x1$allocationRatioPlanned, x2$allocationRatioPlanned)
    expect_equal(x1$optimumAllocationRatio, x2$optimumAllocationRatio)
    expect_equal(x1$maxNumberOfSubjects, x2$maxNumberOfSubjects, tolerance = 1e-07)
    expect_equal(x2$numberOfSubjects[1, ], x1$numberOfSubjects[1, ], tolerance = 1e-07)
    expect_equal(x1$nFixed, x2$nFixed, tolerance = 1e-07)
    expect_equal(x2$criticalValuesEffectScale[1, ], x1$criticalValuesEffectScale[1, ], tolerance = 1e-07)
    expect_equal(x2$criticalValuesPValueScale[1, ], x1$criticalValuesPValueScale[1, ], tolerance = 1e-07)
})

test_that("Sample size means result object utility functions", {
    sampleSizeResult <- getSampleSizeMeans(groups = 1, thetaH0 = 0.1, stDev = 2)
    expect_true(R6::is.R6(sampleSizeResult$getPlotSettings()))
    expect_true(any(grepl("Technical developer summary", utils::capture.output(sampleSizeResult$show(showType = 2)))))
    expect_true(any(grepl("Legend", utils::capture.output(sampleSizeResult$show(showType = 3)))))
})

test_that("Power means result object clone function", {
    x1 <- getPowerMeans(groups = 1, thetaH0 = -0.5, stDev = 2, alternative = -1.2, maxNumberOfSubjects = 50)
    x2 <- x1$clone()


    ## Pairwise comparison of the results of x1 with the results of x2
    expect_equal(x1$meanRatio, x2$meanRatio)
    expect_equal(x1$thetaH0, x2$thetaH0, tolerance = 1e-07)
    expect_equal(x1$normalApproximation, x2$normalApproximation)
    expect_equal(x1$alternative, x2$alternative, tolerance = 1e-07)
    expect_equal(x1$stDev, x2$stDev)
    expect_equal(x1$groups, x2$groups)
    expect_equal(x1$allocationRatioPlanned, x2$allocationRatioPlanned)
    expect_equal(x1$optimumAllocationRatio, x2$optimumAllocationRatio)
    expect_equal(x1$directionUpper, x2$directionUpper)
    expect_equal(x1$effect, x2$effect, tolerance = 1e-07)
    expect_equal(x1$maxNumberOfSubjects, x2$maxNumberOfSubjects)
    expect_equal(x2$numberOfSubjects[1, ], x1$numberOfSubjects[1, ])
    expect_equal(x2$numberOfSubjects1[1, ], x1$numberOfSubjects1[1, ])
    expect_equal(x2$numberOfSubjects2[1, ], x1$numberOfSubjects2[1, ])
    expect_equal(x1$overallReject, x2$overallReject, tolerance = 1e-07)
    expect_equal(x2$rejectPerStage[1, ], x1$rejectPerStage[1, ], tolerance = 1e-07)
    expect_equal(x1$futilityStop, x2$futilityStop)
    expect_equal(x1$earlyStop, x2$earlyStop)
    expect_equal(x1$expectedNumberOfSubjects, x2$expectedNumberOfSubjects)
    expect_equal(x1$nFixed, x2$nFixed)
    expect_equal(x2$criticalValuesEffectScale[1, ], x1$criticalValuesEffectScale[1, ], tolerance = 1e-07)
    expect_equal(x2$criticalValuesPValueScale[1, ], x1$criticalValuesPValueScale[1, ], tolerance = 1e-07)
})

test_that("Power means result object utility functions", {
    powerResult <- getPowerMeans(groups = 1, thetaH0 = -0.5, stDev = 2, alternative = -1.2, maxNumberOfSubjects = 50)
    expect_true(R6::is.R6(powerResult$getPlotSettings()))
    expect_true(any(grepl("Technical developer summary", utils::capture.output(powerResult$show(showType = 2)))))
    expect_true(any(grepl("Legend", utils::capture.output(powerResult$show(showType = 3)))))
})

test_that("Sample size rates result object clone function", {
    x1 <- getSampleSizeRates(groups = 1, thetaH0 = 0.5, pi1 = 0.8, normalApproximation = FALSE)
    x2 <- x1$clone()


    ## Pairwise comparison of the results of x1 with the results of x2
    expect_equal(x1$riskRatio, x2$riskRatio)
    expect_equal(x1$thetaH0, x2$thetaH0, tolerance = 1e-07)
    expect_equal(x1$normalApproximation, x2$normalApproximation)
    expect_equal(x1$pi1, x2$pi1, tolerance = 1e-07)
    expect_equal(x1$pi2, x2$pi2, tolerance = 1e-07)
    expect_equal(x1$groups, x2$groups)
    expect_equal(x1$allocationRatioPlanned, x2$allocationRatioPlanned)
    expect_equal(x1$optimumAllocationRatio, x2$optimumAllocationRatio)
    expect_equal(x1$directionUpper, x2$directionUpper)
    expect_equal(x1$maxNumberOfSubjects, x2$maxNumberOfSubjects)
    expect_equal(x2$numberOfSubjects[1, ], x1$numberOfSubjects[1, ])
    expect_equal(x1$nFixed, x2$nFixed)
    expect_equal(x2$criticalValuesEffectScale[1, ], x1$criticalValuesEffectScale[1, ], tolerance = 1e-07)
    expect_equal(x2$criticalValuesPValueScale[1, ], x1$criticalValuesPValueScale[1, ], tolerance = 1e-07)
})

test_that("Sample size rates result object utility functions", {
    sampleSizeResult <- getSampleSizeRates(groups = 1, thetaH0 = 0.5, pi1 = 0.8, normalApproximation = FALSE)
    expect_true(R6::is.R6(sampleSizeResult$getPlotSettings()))
    expect_true(any(grepl("Technical developer summary", utils::capture.output(sampleSizeResult$show(showType = 2)))))
    expect_true(any(grepl("Legend", utils::capture.output(sampleSizeResult$show(showType = 3)))))
})

test_that("Power rates result object clone function", {
    x1 <- getPowerRates(groups = 1, thetaH0 = 0.4, pi1 = c(0.2, 0.3, 0.4), directionUpper = FALSE, maxNumberOfSubjects = 40)
    x2 <- x1$clone()


    ## Pairwise comparison of the results of x1 with the results of x2
    expect_equal(x1$riskRatio, x2$riskRatio)
    expect_equal(x1$thetaH0, x2$thetaH0, tolerance = 1e-07)
    expect_equal(x1$normalApproximation, x2$normalApproximation)
    expect_equal(x1$pi1, x2$pi1, tolerance = 1e-07)
    expect_equal(x1$pi2, x2$pi2, tolerance = 1e-07)
    expect_equal(x1$groups, x2$groups)
    expect_equal(x1$allocationRatioPlanned, x2$allocationRatioPlanned)
    expect_equal(x1$optimumAllocationRatio, x2$optimumAllocationRatio)
    expect_equal(x1$directionUpper, x2$directionUpper)
    expect_equal(x1$effect, x2$effect, tolerance = 1e-07)
    expect_equal(x1$maxNumberOfSubjects, x2$maxNumberOfSubjects)
    expect_equal(x2$numberOfSubjects[1, ], x1$numberOfSubjects[1, ])
    expect_equal(x2$numberOfSubjects1[1, ], x1$numberOfSubjects1[1, ])
    expect_equal(x2$numberOfSubjects2[1, ], x1$numberOfSubjects2[1, ])
    expect_equal(x1$overallReject, x2$overallReject, tolerance = 1e-07)
    expect_equal(x2$rejectPerStage[1, ], x1$rejectPerStage[1, ], tolerance = 1e-07)
    expect_equal(x1$futilityStop, x2$futilityStop)
    expect_equal(x1$earlyStop, x2$earlyStop)
    expect_equal(x1$expectedNumberOfSubjects, x2$expectedNumberOfSubjects)
    expect_equal(x1$nFixed, x2$nFixed)
    expect_equal(x2$criticalValuesEffectScale[1, ], x1$criticalValuesEffectScale[1, ], tolerance = 1e-07)
    expect_equal(x2$criticalValuesPValueScale[1, ], x1$criticalValuesPValueScale[1, ], tolerance = 1e-07)
})

test_that("Power rates result object utility functions", {
    powerResult <- getPowerRates(groups = 1, thetaH0 = 0.4, pi1 = c(0.2, 0.3, 0.4), directionUpper = FALSE, maxNumberOfSubjects = 40)
    expect_true(R6::is.R6(powerResult$getPlotSettings()))
    expect_true(any(grepl("Technical developer summary", utils::capture.output(powerResult$show(showType = 2)))))
    expect_true(any(grepl("Legend", utils::capture.output(powerResult$show(showType = 3)))))
})

test_that("Sample size survival result object clone function", {
    x1 <- getSampleSizeSurvival(alpha = 0.01)
    x2 <- x1$clone()


    ## Pairwise comparison of the results of x1 with the results of x2
    expect_equal(x1$thetaH0, x2$thetaH0)
    expect_equal(x1$typeOfComputation, x2$typeOfComputation)
    expect_equal(x1$directionUpper, x2$directionUpper)
    expect_equal(x1$pi1, x2$pi1, tolerance = 1e-07)
    expect_equal(x1$pi2, x2$pi2, tolerance = 1e-07)
    expect_equal(x1$median1, x2$median1, tolerance = 1e-07)
    expect_equal(x1$median2, x2$median2, tolerance = 1e-07)
    expect_equal(x1$lambda1, x2$lambda1, tolerance = 1e-07)
    expect_equal(x1$lambda2, x2$lambda2, tolerance = 1e-07)
    expect_equal(x1$hazardRatio, x2$hazardRatio, tolerance = 1e-07)
    expect_equal(x1$maxNumberOfSubjects, x2$maxNumberOfSubjects, tolerance = 1e-07)
    expect_equal(x2$numberOfSubjects[1, ], x1$numberOfSubjects[1, ], tolerance = 1e-07)
    expect_equal(x2$numberOfSubjects1[1, ], x1$numberOfSubjects1[1, ], tolerance = 1e-07)
    expect_equal(x2$numberOfSubjects2[1, ], x1$numberOfSubjects2[1, ], tolerance = 1e-07)
    expect_equal(x1$maxNumberOfEvents, x2$maxNumberOfEvents, tolerance = 1e-07)
    expect_equal(x1$allocationRatioPlanned, x2$allocationRatioPlanned)
    expect_equal(x1$optimumAllocationRatio, x2$optimumAllocationRatio)
    expect_equal(x1$accountForObservationTimes, x2$accountForObservationTimes)
    expect_equal(x1$eventTime, x2$eventTime)
    expect_equal(x1$accrualTime, x2$accrualTime)
    expect_equal(x1$totalAccrualTime, x2$totalAccrualTime)
    expect_equal(x1$accrualIntensity, x2$accrualIntensity, tolerance = 1e-07)
    expect_equal(x1$accrualIntensityRelative, x2$accrualIntensityRelative, tolerance = 1e-07)
    expect_equal(x1$kappa, x2$kappa)
    expect_equal(x1$piecewiseSurvivalTime, x2$piecewiseSurvivalTime)
    expect_equal(x1$followUpTime, x2$followUpTime)
    expect_equal(x1$dropoutRate1, x2$dropoutRate1)
    expect_equal(x1$dropoutRate2, x2$dropoutRate2)
    expect_equal(x1$dropoutTime, x2$dropoutTime)
    expect_equal(x1$chi, x2$chi, tolerance = 1e-07)
    expect_equal(x1$eventsFixed, x2$eventsFixed, tolerance = 1e-07)
    expect_equal(x1$nFixed, x2$nFixed, tolerance = 1e-07)
    expect_equal(x1$nFixed1, x2$nFixed1, tolerance = 1e-07)
    expect_equal(x1$nFixed2, x2$nFixed2, tolerance = 1e-07)
    expect_equal(x2$analysisTime[1, ], x1$analysisTime[1, ])
    expect_equal(x1$studyDuration, x2$studyDuration)
    expect_equal(x1$maxStudyDuration, x2$maxStudyDuration)
    expect_equal(x2$singleEventsPerStage[1, ], x1$singleEventsPerStage[1, ], tolerance = 1e-07)
    expect_equal(x2$cumulativeEventsPerStage[1, ], x1$cumulativeEventsPerStage[1, ], tolerance = 1e-07)
    expect_equal(x1$expectedEventsH1, x2$expectedEventsH1, tolerance = 1e-07)
    expect_equal(x2$criticalValuesEffectScale[1, ], x1$criticalValuesEffectScale[1, ], tolerance = 1e-07)
    expect_equal(x2$criticalValuesPValueScale[1, ], x1$criticalValuesPValueScale[1, ], tolerance = 1e-07)
})

test_that("Sample size survival result object utility functions", {
    .skipTestIfDisabled()

    sampleSizeResult <- getSampleSizeSurvival(alpha = 0.01)
    expect_true(R6::is.R6(sampleSizeResult$getPlotSettings()))
    expect_true(any(grepl("Technical developer summary", utils::capture.output(sampleSizeResult$show(showType = 2)))))
    expect_true(any(grepl("Legend", utils::capture.output(sampleSizeResult$show(showType = 3)))))
})

test_that("Power survival result object clone function", {
    x1 <- getPowerSurvival(maxNumberOfEvents = 40, maxNumberOfSubjects = 200)
    x2 <- x1$clone()


    ## Pairwise comparison of the results of x1 with the results of x2
    expect_equal(x1$thetaH0, x2$thetaH0)
    expect_equal(x1$typeOfComputation, x2$typeOfComputation)
    expect_equal(x1$directionUpper, x2$directionUpper)
    expect_equal(x1$pi1, x2$pi1, tolerance = 1e-07)
    expect_equal(x1$pi2, x2$pi2, tolerance = 1e-07)
    expect_equal(x1$median1, x2$median1, tolerance = 1e-07)
    expect_equal(x1$median2, x2$median2, tolerance = 1e-07)
    expect_equal(x1$lambda1, x2$lambda1, tolerance = 1e-07)
    expect_equal(x1$lambda2, x2$lambda2, tolerance = 1e-07)
    expect_equal(x1$hazardRatio, x2$hazardRatio, tolerance = 1e-07)
    expect_equal(x1$maxNumberOfSubjects, x2$maxNumberOfSubjects)
    expect_equal(x2$numberOfSubjects[1, ], x1$numberOfSubjects[1, ])
    expect_equal(x1$maxNumberOfEvents, x2$maxNumberOfEvents)
    expect_equal(x1$allocationRatioPlanned, x2$allocationRatioPlanned)
    expect_equal(x1$optimumAllocationRatio, x2$optimumAllocationRatio)
    expect_equal(x1$accountForObservationTimes, x2$accountForObservationTimes)
    expect_equal(x1$eventTime, x2$eventTime)
    expect_equal(x1$accrualTime, x2$accrualTime)
    expect_equal(x1$totalAccrualTime, x2$totalAccrualTime)
    expect_equal(x1$accrualIntensity, x2$accrualIntensity, tolerance = 1e-07)
    expect_equal(x1$kappa, x2$kappa)
    expect_equal(x1$piecewiseSurvivalTime, x2$piecewiseSurvivalTime)
    expect_equal(x1$followUpTime, x2$followUpTime, tolerance = 1e-07)
    expect_equal(x1$dropoutRate1, x2$dropoutRate1)
    expect_equal(x1$dropoutRate2, x2$dropoutRate2)
    expect_equal(x1$dropoutTime, x2$dropoutTime)
    expect_equal(x1$expectedNumberOfEvents, x2$expectedNumberOfEvents)
    expect_equal(x1$overallReject, x2$overallReject, tolerance = 1e-07)
    expect_equal(x2$rejectPerStage[1, ], x1$rejectPerStage[1, ], tolerance = 1e-07)
    expect_equal(x1$futilityStop, x2$futilityStop)
    expect_equal(x1$earlyStop, x2$earlyStop)
    expect_equal(x2$analysisTime[1, ], x1$analysisTime[1, ], tolerance = 1e-07)
    expect_equal(x1$studyDuration, x2$studyDuration, tolerance = 1e-07)
    expect_equal(x1$maxStudyDuration, x2$maxStudyDuration, tolerance = 1e-07)
    expect_equal(x2$singleEventsPerStage[1, ], x1$singleEventsPerStage[1, ])
    expect_equal(x2$cumulativeEventsPerStage[1, ], x1$cumulativeEventsPerStage[1, ])
    expect_equal(x1$expectedNumberOfSubjects, x2$expectedNumberOfSubjects)
    expect_equal(x2$criticalValuesEffectScale[1, ], x1$criticalValuesEffectScale[1, ], tolerance = 1e-07)
    expect_equal(x2$criticalValuesPValueScale[1, ], x1$criticalValuesPValueScale[1, ], tolerance = 1e-07)
})

test_that("Power survival result object utility functions", {
    .skipTestIfDisabled()

    powerResult <- getPowerSurvival(maxNumberOfEvents = 40, maxNumberOfSubjects = 200)
    expect_true(R6::is.R6(powerResult$getPlotSettings()))
    expect_true(any(grepl("Technical developer summary", utils::capture.output(powerResult$show(showType = 2)))))
    expect_true(any(grepl("Legend", utils::capture.output(powerResult$show(showType = 3)))))
})

test_that("Sample size counts result object clone function", {
    x1 <- getSampleSizeCounts(
        alpha = 0.01, beta = 0.05, lambda = 0.234, theta = 0.7,
        overdispersion = 0.71, accrualTime = 7, fixedExposureTime = 1
    )
    x2 <- x1$clone()


    ## Pairwise comparison of the results of x1 with the results of x2
    expect_equal(x1$thetaH0, x2$thetaH0)
    expect_equal(x1$groups, x2$groups)
    expect_equal(x1$allocationRatioPlanned, x2$allocationRatioPlanned)
    expect_equal(x1$optimumAllocationRatio, x2$optimumAllocationRatio)
    expect_equal(x1$directionUpper, x2$directionUpper)
    expect_equal(x1$lambda1, x2$lambda1, tolerance = 1e-07)
    expect_equal(x1$lambda2, x2$lambda2, tolerance = 1e-07)
    expect_equal(x1$lambda, x2$lambda, tolerance = 1e-07)
    expect_equal(x1$theta, x2$theta, tolerance = 1e-07)
    expect_equal(x1$nFixed, x2$nFixed)
    expect_equal(x1$nFixed1, x2$nFixed1)
    expect_equal(x1$nFixed2, x2$nFixed2)
    expect_equal(x1$maxNumberOfSubjects, x2$maxNumberOfSubjects)
    expect_equal(x1$maxNumberOfSubjects1, x2$maxNumberOfSubjects1)
    expect_equal(x1$maxNumberOfSubjects2, x2$maxNumberOfSubjects2)
    expect_equal(x1$overdispersion, x2$overdispersion, tolerance = 1e-07)
    expect_equal(x1$fixedExposureTime, x2$fixedExposureTime)
    expect_equal(x1$accrualTime, x2$accrualTime)
    expect_equal(x1$accrualIntensity, x2$accrualIntensity)
    expect_equal(x1$followUpTime, x2$followUpTime)
    expect_equal(x2$calendarTime[1, ], x1$calendarTime[1, ])
    expect_equal(x1$expectedStudyDurationH1, x2$expectedStudyDurationH1)
    expect_equal(x1$studyTime, x2$studyTime)
    expect_equal(x2$numberOfSubjects[1, ], x1$numberOfSubjects[1, ])
    expect_equal(x1$expectedNumberOfSubjectsH1, x2$expectedNumberOfSubjectsH1)
    expect_equal(x1$maxInformation, x2$maxInformation, tolerance = 1e-07)
})

test_that("Sample size counts result object utility functions", {
    .skipTestIfDisabled()

    sampleSizeResult <- getSampleSizeCounts(
        alpha = 0.01, beta = 0.05, lambda = 0.234, theta = 0.7,
        overdispersion = 0.71, accrualTime = 7, fixedExposureTime = 1
    )
    expect_true(R6::is.R6(sampleSizeResult$getPlotSettings()))
    expect_true(any(grepl("Technical developer summary", utils::capture.output(sampleSizeResult$show(showType = 2)))))
    expect_true(any(grepl("Sample size calculation for a count data endpoint", utils::capture.output(sampleSizeResult$show(showType = 3)))))
})

test_that("Power counts result object clone function", {
    .skipTestIfDisabled()

    x1 <- getPowerCounts(
        maxNumberOfSubjects = 400, directionUpper = FALSE,
        overdispersion = 1, fixedExposureTime = 1, lambda1 = seq(1.05, 1.55, 0.1), lambda2 = 1.4
    )
    x2 <- x1$clone()


    ## Pairwise comparison of the results of x1 with the results of x2
    expect_equal(x1$thetaH0, x2$thetaH0)
    expect_equal(x1$groups, x2$groups)
    expect_equal(x1$allocationRatioPlanned, x2$allocationRatioPlanned)
    expect_equal(x1$optimumAllocationRatio, x2$optimumAllocationRatio)
    expect_equal(x1$directionUpper, x2$directionUpper)
    expect_equal(x1$lambda1, x2$lambda1, tolerance = 1e-07)
    expect_equal(x1$lambda2, x2$lambda2, tolerance = 1e-07)
    expect_equal(x1$lambda, x2$lambda)
    expect_equal(x1$theta, x2$theta, tolerance = 1e-07)
    expect_equal(x1$nFixed, x2$nFixed)
    expect_equal(x1$nFixed1, x2$nFixed1)
    expect_equal(x1$nFixed2, x2$nFixed2)
    expect_equal(x1$maxNumberOfSubjects, x2$maxNumberOfSubjects)
    expect_equal(x1$maxNumberOfSubjects1, x2$maxNumberOfSubjects1)
    expect_equal(x1$maxNumberOfSubjects2, x2$maxNumberOfSubjects2)
    expect_equal(x1$overallReject, x2$overallReject, tolerance = 1e-07)
    expect_equal(x1$overdispersion, x2$overdispersion)
    expect_equal(x1$fixedExposureTime, x2$fixedExposureTime)
    expect_equal(x1$accrualTime, x2$accrualTime)
    expect_equal(x1$accrualIntensity, x2$accrualIntensity)
    expect_equal(x1$followUpTime, x2$followUpTime)
})

test_that("Power counts result object utility functions", {
    powerResult <- getPowerCounts(
        maxNumberOfSubjects = 400, directionUpper = FALSE,
        overdispersion = 1, fixedExposureTime = 1, lambda1 = seq(1.05, 1.55, 0.1), lambda2 = 1.4
    )
    expect_true(R6::is.R6(powerResult$getPlotSettings()))
    expect_true(any(grepl("Technical developer summary", utils::capture.output(powerResult$show(showType = 2)))))
    expect_true(any(grepl("Power calculation for a count data endpoint", utils::capture.output(powerResult$show(showType = 3)))))
})

test_that("Design plan utility functions", {
    .skipTestIfDisabled()

    trialDesignPlan <- TrialDesignPlan$new(design = NULL)

    expect_error(trialDesignPlan$.isSampleSizeObject())
    expect_error(trialDesignPlan$.isPowerObject())
    expect_equal(trialDesignPlan$getBeta(), NA_real_)
    expect_equal(trialDesignPlan$getTwoSidedPower(), NA)
    expect_equal(trialDesignPlan$.toString(), "unknown data class 'TrialDesignPlan'")
    expect_error(trialDesignPlan$.setObjectType("xxx"))
})

test_that("Sample size means recreate function", {
    trialDesignPlan <- getSampleSizeMeans()
    trialDesignPlanCopy <- trialDesignPlan$recreate()

    ## Pairwise comparison of the results of trialDesignPlan with the results of trialDesignPlanCopy
    expect_equal(trialDesignPlan$nFixed, trialDesignPlanCopy$nFixed, tolerance = 1e-07)
    expect_equal(trialDesignPlan$nFixed1, trialDesignPlanCopy$nFixed1, tolerance = 1e-07)
    expect_equal(trialDesignPlan$nFixed2, trialDesignPlanCopy$nFixed2, tolerance = 1e-07)
    expect_equal(trialDesignPlanCopy$criticalValuesEffectScale[1, ], trialDesignPlan$criticalValuesEffectScale[1, ], tolerance = 1e-07)
})

test_that("Sample size rates recreate function", {
    trialDesignPlan <- getSampleSizeRates()
    trialDesignPlanCopy <- trialDesignPlan$recreate()

    ## Pairwise comparison of the results of trialDesignPlan with the results of trialDesignPlanCopy
    expect_equal(trialDesignPlan$directionUpper, trialDesignPlanCopy$directionUpper)
    expect_equal(trialDesignPlan$nFixed, trialDesignPlanCopy$nFixed, tolerance = 1e-07)
    expect_equal(trialDesignPlan$nFixed1, trialDesignPlanCopy$nFixed1, tolerance = 1e-07)
    expect_equal(trialDesignPlan$nFixed2, trialDesignPlanCopy$nFixed2, tolerance = 1e-07)
    expect_equal(trialDesignPlanCopy$criticalValuesEffectScale[1, ], trialDesignPlan$criticalValuesEffectScale[1, ], tolerance = 1e-07)
})

test_that("Sample size survival recreate function", {
    trialDesignPlan <- getSampleSizeSurvival()
    trialDesignPlanCopy <- trialDesignPlan$recreate()

    expect_equal(trialDesignPlan$directionUpper, trialDesignPlanCopy$directionUpper)
    expect_equal(trialDesignPlan$median1, trialDesignPlanCopy$median1, tolerance = 1e-07)
    expect_equal(trialDesignPlan$median2, trialDesignPlanCopy$median2, tolerance = 1e-07)
    expect_equal(trialDesignPlan$lambda1, trialDesignPlanCopy$lambda1, tolerance = 1e-07)
    expect_equal(trialDesignPlan$lambda2, trialDesignPlanCopy$lambda2, tolerance = 1e-07)
    expect_equal(trialDesignPlan$hazardRatio, trialDesignPlanCopy$hazardRatio, tolerance = 1e-07)
    expect_equal(trialDesignPlan$maxNumberOfEvents, trialDesignPlanCopy$maxNumberOfEvents, tolerance = 1e-07)
    expect_equal(trialDesignPlan$eventsFixed, trialDesignPlanCopy$eventsFixed, tolerance = 1e-07)
    expect_equal(trialDesignPlanCopy$criticalValuesEffectScale[1, ], trialDesignPlan$criticalValuesEffectScale[1, ], tolerance = 1e-07)
})

test_that("Count data recreate function", {
    trialDesignPlan <- getSampleSizeCounts(
        alpha = 0.01, beta = 0.05, lambda = 0.234, theta = 0.7,
        overdispersion = 0.71, accrualTime = 7, fixedExposureTime = 1
    )
    trialDesignPlanCopy <- trialDesignPlan$recreate()

    ## Pairwise comparison of the results of trialDesignPlan with the results of trialDesignPlanCopy
    expect_equal(trialDesignPlan$directionUpper, trialDesignPlanCopy$directionUpper)
    expect_equal(trialDesignPlan$lambda1, trialDesignPlanCopy$lambda1, tolerance = 1e-07)
    expect_equal(trialDesignPlan$lambda2, trialDesignPlanCopy$lambda2, tolerance = 1e-07)
    expect_equal(trialDesignPlan$nFixed, trialDesignPlanCopy$nFixed)
    expect_equal(trialDesignPlan$nFixed1, trialDesignPlanCopy$nFixed1)
    expect_equal(trialDesignPlan$nFixed2, trialDesignPlanCopy$nFixed2)
    expect_equal(trialDesignPlan$maxNumberOfSubjects, trialDesignPlanCopy$maxNumberOfSubjects)
    expect_equal(trialDesignPlanCopy$calendarTime[1, ], trialDesignPlan$calendarTime[1, ])
    expect_equal(trialDesignPlan$expectedStudyDurationH1, trialDesignPlanCopy$expectedStudyDurationH1)
    expect_equal(trialDesignPlan$expectedNumberOfSubjectsH1, trialDesignPlanCopy$expectedNumberOfSubjectsH1)
    expect_equal(trialDesignPlan$maxInformation, trialDesignPlanCopy$maxInformation, tolerance = 1e-07)
})
