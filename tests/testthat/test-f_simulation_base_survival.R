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
## |  File name: test-f_simulation_base_survival.R
## |  Creation date: 16 September 2024, 08:44:51
## |  File version: $Revision: 8260 $
## |  Last changed: $Date: 2024-09-25 10:42:43 +0200 (Mi, 25 Sep 2024) $
## |  Last changed by: $Author: pahlke $
## |

test_plan_section("Testing Simulation Survival Function")


test_that("'getSimulationSurvival': configuration 1", {
    .skipTestIfDisabled()
    
    # @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
    # @refFS[Sec.]{fs:sec:simulatingTestingOneHypothesis}
    # @refFS[Tab.]{fs:tab:output:getSimulationSurvival}
    # @refFS[Formula]{fs:simulationSurvivalTimeGenerate}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    # @refFS[Formula]{fs:simulationSurvivalLogRank}
    # @refFS[Formula]{fs:simulationSurvivalIncrements}
    # @refFS[Formula]{fs:simulationSurvivalHazardEstimate}
    simulationResults <- getSimulationSurvival(
        maxNumberOfSubjects = 200, plannedEvents = 50,
        accrualTime = c(0, 3, 6, 12), accrualIntensity = c(0.1, 0.2, 0.2),
        maxNumberOfIterations = 100, seed = 1234567890
    )

    ## Comparison of the results of SimulationResultsSurvival object 'simulationResults' with expected results
    expect_equal(simulationResults$median1, c(37.275405, 23.320299, 16.282985, 12), tolerance = 1e-07, label = paste0(simulationResults$median1))
    expect_equal(simulationResults$median2, 37.275405, tolerance = 1e-07, label = paste0(simulationResults$median2))
    expect_equal(simulationResults$accrualIntensity, c(9.5238095, 19.047619, 19.047619), tolerance = 1e-07, label = paste0(simulationResults$accrualIntensity))
    expect_equal(simulationResults$lambda1, c(0.018595296, 0.029722912, 0.042568802, 0.057762265), tolerance = 1e-07, label = paste0(simulationResults$lambda1))
    expect_equal(simulationResults$lambda2, 0.018595296, tolerance = 1e-07, label = paste0(simulationResults$lambda2))
    expect_equal(simulationResults$hazardRatio, c(1, 1.5984103, 2.2892242, 3.1062837), tolerance = 1e-07, label = paste0(simulationResults$hazardRatio))
    expect_equal(simulationResults$analysisTime[1, ], c(22.223223, 18.818775, 16.321595, 14.790808), tolerance = 1e-07, label = paste0(simulationResults$analysisTime[1, ]))
    expect_equal(simulationResults$studyDuration, c(22.223223, 18.818775, 16.321595, 14.790808), tolerance = 1e-07, label = paste0(simulationResults$studyDuration))
    expect_equal(simulationResults$eventsNotAchieved[1, ], c(0, 0, 0, 0), label = paste0(simulationResults$eventsNotAchieved[1, ]))
    expect_equal(simulationResults$cumulativeEventsPerStage[1, ], c(50, 50, 50, 50), label = paste0(simulationResults$cumulativeEventsPerStage[1, ]))
    expect_equal(simulationResults$iterations[1, ], c(100, 100, 100, 100), label = paste0(simulationResults$iterations[1, ]))
    expect_equal(simulationResults$overallReject, c(0.01, 0.41, 0.81, 1), tolerance = 1e-07, label = paste0(simulationResults$overallReject))
    expect_gte(object = simulationResults$overallReject[1], expected = 0)
    expect_gte(object = simulationResults$overallReject[2], expected = 0)
    expect_gte(object = simulationResults$overallReject[3], expected = 0)
    expect_gte(object = simulationResults$overallReject[4], expected = 0)
    expect_lte(object = simulationResults$overallReject[1], expected = 1)
    expect_lte(object = simulationResults$overallReject[2], expected = 1)
    expect_lte(object = simulationResults$overallReject[3], expected = 1)
    expect_lte(object = simulationResults$overallReject[4], expected = 1)
    expect_equal(simulationResults$rejectPerStage[1, ], c(0.01, 0.41, 0.81, 1), tolerance = 1e-07, label = paste0(simulationResults$rejectPerStage[1, ]))
    expect_equal(simulationResults$earlyStop, c(0, 0, 0, 0), label = paste0(simulationResults$earlyStop))
    expect_equal(simulationResults$expectedNumberOfSubjects, c(200, 200, 200, 200), label = paste0(simulationResults$expectedNumberOfSubjects))
    expect_equal(simulationResults$expectedNumberOfEvents, c(50, 50, 50, 50), label = paste0(simulationResults$expectedNumberOfEvents))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(simulationResults), NA)))
        expect_output(print(simulationResults)$show())
        invisible(capture.output(expect_error(summary(simulationResults), NA)))
        expect_output(summary(simulationResults)$show())
        simulationResultsCodeBased <- eval(parse(text = getObjectRCode(simulationResults, stringWrapParagraphWidth = NULL)))
        expect_equal(simulationResultsCodeBased$median1, simulationResults$median1, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$median2, simulationResults$median2, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$accrualIntensity, simulationResults$accrualIntensity, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$lambda1, simulationResults$lambda1, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$lambda2, simulationResults$lambda2, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$hazardRatio, simulationResults$hazardRatio, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$analysisTime, simulationResults$analysisTime, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$studyDuration, simulationResults$studyDuration, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$eventsNotAchieved, simulationResults$eventsNotAchieved, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$cumulativeEventsPerStage, simulationResults$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$iterations, simulationResults$iterations, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$overallReject, simulationResults$overallReject, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$rejectPerStage, simulationResults$rejectPerStage, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$futilityPerStage, simulationResults$futilityPerStage, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$earlyStop, simulationResults$earlyStop, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$expectedNumberOfSubjects, simulationResults$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$expectedNumberOfEvents, simulationResults$expectedNumberOfEvents, tolerance = 1e-07)
        expect_type(names(simulationResults), "character")
        df <- as.data.frame(simulationResults)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(simulationResults)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSimulationSurvival': configuration 2", {
    .skipTestIfDisabled()

    # @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
    # @refFS[Sec.]{fs:sec:simulatingTestingOneHypothesis}
    # @refFS[Tab.]{fs:tab:output:getSimulationSurvival}
    # @refFS[Formula]{fs:simulationSurvivalTimeGenerate}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    # @refFS[Formula]{fs:simulationSurvivalLogRank}
    # @refFS[Formula]{fs:simulationSurvivalIncrements}
    # @refFS[Formula]{fs:simulationSurvivalHazardEstimate}
    # @refFS[Formula]{fs:testStatisticFisherCombinationTest}
    design <- getDesignFisher(kMax = 3, alpha0Vec = c(0.5, 0.5))
    design2 <- getDesignFisher(kMax = 3, alpha0Vec = c(0.5, 0.5), directionUpper = FALSE)

    simulationResults <- getSimulationSurvival(
        design = design, pi2 = 0.6, pi1 = seq(0.3, 0.45, 0.05),
        directionUpper = FALSE, maxNumberOfSubjects = 500, plannedEvents = (1:design$kMax) * 20,
        allocation1 = 1, allocation2 = 1, accrualTime = c(0, 3, 6, 12),
        accrualIntensity = c(0.1, 0.2, 0.2), dropoutRate1 = 0, dropoutRate2 = 0,
        dropoutTime = 12, conditionalPower = 0.8, minNumberOfEventsPerStage = c(NA_real_, 10, 10),
        maxNumberOfEventsPerStage = c(NA_real_, 100, 200), maxNumberOfIterations = 100,
        seed = 1234567890
    )


    ## Comparison of the results of SimulationResultsSurvival object 'simulationResults' with expected results
    expect_equal(simulationResults$median1, c(23.320299, 19.308487, 16.282985, 13.9131), tolerance = 1e-07, label = paste0(simulationResults$median1))
    expect_equal(simulationResults$median2, 9.0776496, tolerance = 1e-07, label = paste0(simulationResults$median2))
    expect_equal(simulationResults$accrualIntensity, c(23.809524, 47.619048, 47.619048), tolerance = 1e-07, label = paste0(simulationResults$accrualIntensity))
    expect_equal(simulationResults$lambda1, c(0.029722912, 0.035898576, 0.042568802, 0.04981975), tolerance = 1e-07, label = paste0(simulationResults$lambda1))
    expect_equal(simulationResults$lambda2, 0.076357561, tolerance = 1e-07, label = paste0(simulationResults$lambda2))
    expect_equal(simulationResults$hazardRatio, c(0.38925958, 0.47013781, 0.55749295, 0.6524534), tolerance = 1e-07, label = paste0(simulationResults$hazardRatio))
    expect_equal(simulationResults$analysisTime[1, ], c(5.4183926, 5.2945044, 5.1495619, 5.0392001), tolerance = 1e-07, label = paste0(simulationResults$analysisTime[1, ]))
    expect_equal(simulationResults$analysisTime[2, ], c(10.130549, 10.39649, 10.458778, 9.7641943), tolerance = 1e-07, label = paste0(simulationResults$analysisTime[2, ]))
    expect_equal(simulationResults$analysisTime[3, ], c(13.506679, 14.455396, 18.382917, 18.866629), tolerance = 1e-07, label = paste0(simulationResults$analysisTime[3, ]))
    expect_equal(simulationResults$studyDuration, c(8.500396, 9.4448778, 11.628285, 12.227203), tolerance = 1e-07, label = paste0(simulationResults$studyDuration))
    expect_equal(simulationResults$eventsNotAchieved[1, ], c(0, 0, 0, 0), label = paste0(simulationResults$eventsNotAchieved[1, ]))
    expect_equal(simulationResults$eventsNotAchieved[2, ], c(0, 0, 0, 0), label = paste0(simulationResults$eventsNotAchieved[2, ]))
    expect_equal(simulationResults$eventsNotAchieved[3, ], c(0, 0, 0, 0), label = paste0(simulationResults$eventsNotAchieved[3, ]))
    expect_equal(simulationResults$numberOfSubjects[1, ], c(186.51, 180.63, 173.73, 168.48), tolerance = 1e-07, label = paste0(simulationResults$numberOfSubjects[1, ]))
    expect_equal(simulationResults$numberOfSubjects[2, ], c(406.05, 420.67123, 424.60256, 393.44615), tolerance = 1e-07, label = paste0(simulationResults$numberOfSubjects[2, ]))
    expect_equal(simulationResults$numberOfSubjects[3, ], c(428.4, 466.33333, 480.96429, 488.78261), tolerance = 1e-07, label = paste0(simulationResults$numberOfSubjects[3, ]))
    expect_equal(simulationResults$cumulativeEventsPerStage[1, ], c(20, 20, 20, 20), label = paste0(simulationResults$cumulativeEventsPerStage[1, ]))
    expect_equal(simulationResults$cumulativeEventsPerStage[2, ], c(84.483333, 93.054795, 98.884615, 92.015385), tolerance = 1e-07, label = paste0(simulationResults$cumulativeEventsPerStage[2, ]))
    expect_equal(simulationResults$cumulativeEventsPerStage[3, ], c(155.08333, 173.61035, 233.02747, 248.03712), tolerance = 1e-07, label = paste0(simulationResults$cumulativeEventsPerStage[3, ]))
    expect_equal(simulationResults$iterations[1, ], c(100, 100, 100, 100), label = paste0(simulationResults$iterations[1, ]))
    expect_equal(simulationResults$iterations[2, ], c(60, 73, 78, 65), label = paste0(simulationResults$iterations[2, ]))
    expect_equal(simulationResults$iterations[3, ], c(5, 9, 28, 46), label = paste0(simulationResults$iterations[3, ]))
    expect_equal(simulationResults$overallReject, c(1, 0.93, 0.96, 0.69), tolerance = 1e-07, label = paste0(simulationResults$overallReject))
    expect_gte(object = simulationResults$overallReject[1], expected = 0)
    expect_gte(object = simulationResults$overallReject[2], expected = 0)
    expect_gte(object = simulationResults$overallReject[3], expected = 0)
    expect_gte(object = simulationResults$overallReject[4], expected = 0)
    expect_lte(object = simulationResults$overallReject[1], expected = 1)
    expect_lte(object = simulationResults$overallReject[2], expected = 1)
    expect_lte(object = simulationResults$overallReject[3], expected = 1)
    expect_lte(object = simulationResults$overallReject[4], expected = 1)
    expect_equal(simulationResults$rejectPerStage[1, ], c(0.4, 0.21, 0.2, 0.13), tolerance = 1e-07, label = paste0(simulationResults$rejectPerStage[1, ]))
    expect_equal(simulationResults$rejectPerStage[2, ], c(0.55, 0.63, 0.5, 0.15), tolerance = 1e-07, label = paste0(simulationResults$rejectPerStage[2, ]))
    expect_equal(simulationResults$rejectPerStage[3, ], c(0.05, 0.09, 0.26, 0.41), tolerance = 1e-07, label = paste0(simulationResults$rejectPerStage[3, ]))
    expect_equal(simulationResults$futilityStop, c(0, 0.07, 0.02, 0.26), tolerance = 1e-07, label = paste0(simulationResults$futilityStop))
    expect_equal(simulationResults$futilityPerStage[1, ], c(0, 0.06, 0.02, 0.22), tolerance = 1e-07, label = paste0(simulationResults$futilityPerStage[1, ]))
    expect_equal(simulationResults$futilityPerStage[2, ], c(0, 0.01, 0, 0.04), tolerance = 1e-07, label = paste0(simulationResults$futilityPerStage[2, ]))
    expect_equal(simulationResults$earlyStop, c(0.95, 0.91, 0.72, 0.54), tolerance = 1e-07, label = paste0(simulationResults$earlyStop))
    expect_equal(simulationResults$expectedNumberOfSubjects, c(319.3515, 359.96969, 385.19188, 358.56277), tolerance = 1e-07, label = paste0(simulationResults$expectedNumberOfSubjects))
    expect_equal(simulationResults$expectedNumberOfEvents, c(62.22, 80.58, 119.09, 138.58), tolerance = 1e-07, label = paste0(simulationResults$expectedNumberOfEvents))
    expect_equal(simulationResults$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(simulationResults$conditionalPowerAchieved[1, ]))
    expect_equal(simulationResults$conditionalPowerAchieved[2, ], c(0.61612368, 0.57564124, 0.49458667, 0.52832804), tolerance = 1e-07, label = paste0(simulationResults$conditionalPowerAchieved[2, ]))
    expect_equal(simulationResults$conditionalPowerAchieved[3, ], c(0.78816558, 0.77803263, 0.64572713, 0.66129837), tolerance = 1e-07, label = paste0(simulationResults$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(simulationResults), NA)))
        expect_output(print(simulationResults)$show())
        invisible(capture.output(expect_error(summary(simulationResults), NA)))
        expect_output(summary(simulationResults)$show())
        simulationResultsCodeBased <- eval(parse(text = getObjectRCode(simulationResults, stringWrapParagraphWidth = NULL)))
        expect_equal(simulationResultsCodeBased$median1, simulationResults$median1, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$median2, simulationResults$median2, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$accrualIntensity, simulationResults$accrualIntensity, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$lambda1, simulationResults$lambda1, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$lambda2, simulationResults$lambda2, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$hazardRatio, simulationResults$hazardRatio, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$analysisTime, simulationResults$analysisTime, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$studyDuration, simulationResults$studyDuration, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$eventsNotAchieved, simulationResults$eventsNotAchieved, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$numberOfSubjects, simulationResults$numberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$cumulativeEventsPerStage, simulationResults$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$iterations, simulationResults$iterations, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$overallReject, simulationResults$overallReject, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$rejectPerStage, simulationResults$rejectPerStage, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$futilityStop, simulationResults$futilityStop, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$futilityPerStage, simulationResults$futilityPerStage, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$earlyStop, simulationResults$earlyStop, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$expectedNumberOfSubjects, simulationResults$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$expectedNumberOfEvents, simulationResults$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$conditionalPowerAchieved, simulationResults$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(simulationResults), "character")
        df <- as.data.frame(simulationResults)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(simulationResults)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
    simulationResults2 <- getSimulationSurvival(
        design = design2, pi2 = 0.6, pi1 = seq(0.3, 0.45, 0.05),
        maxNumberOfSubjects = 500, plannedEvents = (1:design$kMax) * 20,
        allocation1 = 1, allocation2 = 1, accrualTime = c(0, 3, 6, 12),
        accrualIntensity = c(0.1, 0.2, 0.2), dropoutRate1 = 0, dropoutRate2 = 0,
        dropoutTime = 12, conditionalPower = 0.8, minNumberOfEventsPerStage = c(NA_real_, 10, 10),
        maxNumberOfEventsPerStage = c(NA_real_, 100, 200), maxNumberOfIterations = 100,
        seed = 1234567890
    )


    ## Pairwise comparison of the results of simulationResults with the results of simulationResults2
    expect_equal(simulationResults$median1, simulationResults2$median1, tolerance = 1e-07)
    expect_equal(simulationResults$median2, simulationResults2$median2, tolerance = 1e-07)
    expect_equal(simulationResults$accrualIntensity, simulationResults2$accrualIntensity, tolerance = 1e-07)
    expect_equal(simulationResults$lambda1, simulationResults2$lambda1, tolerance = 1e-07)
    expect_equal(simulationResults$lambda2, simulationResults2$lambda2, tolerance = 1e-07)
    expect_equal(simulationResults$hazardRatio, simulationResults2$hazardRatio, tolerance = 1e-07)
    expect_equal(simulationResults2$analysisTime[1, ], simulationResults$analysisTime[1, ], tolerance = 1e-07)
    expect_equal(simulationResults2$analysisTime[2, ], simulationResults$analysisTime[2, ], tolerance = 1e-07)
    expect_equal(simulationResults2$analysisTime[3, ], simulationResults$analysisTime[3, ], tolerance = 1e-07)
    expect_equal(simulationResults$studyDuration, simulationResults2$studyDuration, tolerance = 1e-07)
    expect_equal(simulationResults2$eventsNotAchieved[1, ], simulationResults$eventsNotAchieved[1, ])
    expect_equal(simulationResults2$eventsNotAchieved[2, ], simulationResults$eventsNotAchieved[2, ])
    expect_equal(simulationResults2$eventsNotAchieved[3, ], simulationResults$eventsNotAchieved[3, ])
    expect_equal(simulationResults2$numberOfSubjects[1, ], simulationResults$numberOfSubjects[1, ], tolerance = 1e-07)
    expect_equal(simulationResults2$numberOfSubjects[2, ], simulationResults$numberOfSubjects[2, ], tolerance = 1e-07)
    expect_equal(simulationResults2$numberOfSubjects[3, ], simulationResults$numberOfSubjects[3, ], tolerance = 1e-07)
    expect_equal(simulationResults2$cumulativeEventsPerStage[1, ], simulationResults$cumulativeEventsPerStage[1, ])
    expect_equal(simulationResults2$cumulativeEventsPerStage[2, ], simulationResults$cumulativeEventsPerStage[2, ], tolerance = 1e-07)
    expect_equal(simulationResults2$cumulativeEventsPerStage[3, ], simulationResults$cumulativeEventsPerStage[3, ], tolerance = 1e-07)
    expect_equal(simulationResults2$iterations[1, ], simulationResults$iterations[1, ])
    expect_equal(simulationResults2$iterations[2, ], simulationResults$iterations[2, ])
    expect_equal(simulationResults2$iterations[3, ], simulationResults$iterations[3, ])
    expect_equal(simulationResults$overallReject, simulationResults2$overallReject, tolerance = 1e-07)
    expect_equal(simulationResults2$rejectPerStage[1, ], simulationResults$rejectPerStage[1, ], tolerance = 1e-07)
    expect_equal(simulationResults2$rejectPerStage[2, ], simulationResults$rejectPerStage[2, ], tolerance = 1e-07)
    expect_equal(simulationResults2$rejectPerStage[3, ], simulationResults$rejectPerStage[3, ], tolerance = 1e-07)
    expect_equal(simulationResults$futilityStop, simulationResults2$futilityStop, tolerance = 1e-07)
    expect_equal(simulationResults2$futilityPerStage[1, ], simulationResults$futilityPerStage[1, ], tolerance = 1e-07)
    expect_equal(simulationResults2$futilityPerStage[2, ], simulationResults$futilityPerStage[2, ], tolerance = 1e-07)
    expect_equal(simulationResults$earlyStop, simulationResults2$earlyStop, tolerance = 1e-07)
    expect_equal(simulationResults$expectedNumberOfSubjects, simulationResults2$expectedNumberOfSubjects, tolerance = 1e-07)
    expect_equal(simulationResults$expectedNumberOfEvents, simulationResults2$expectedNumberOfEvents, tolerance = 1e-07)
    expect_equal(simulationResults2$conditionalPowerAchieved[1, ], simulationResults$conditionalPowerAchieved[1, ])
    expect_equal(simulationResults2$conditionalPowerAchieved[2, ], simulationResults$conditionalPowerAchieved[2, ], tolerance = 1e-07)
    expect_equal(simulationResults2$conditionalPowerAchieved[3, ], simulationResults$conditionalPowerAchieved[3, ], tolerance = 1e-07)
})

test_that("'getSimulationSurvival': configuration 3", {
    .skipTestIfDisabled()

    # @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
    # @refFS[Sec.]{fs:sec:simulatingTestingOneHypothesis}
    # @refFS[Tab.]{fs:tab:output:getSimulationSurvival}
    # @refFS[Formula]{fs:simulationSurvivalTimeGenerate}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    # @refFS[Formula]{fs:simulationSurvivalLogRank}
    # @refFS[Formula]{fs:simulationSurvivalIncrements}
    # @refFS[Formula]{fs:simulationSurvivalHazardEstimate}
    # @refFS[Formula]{fs:testStatisticFisherCombinationTest}
    design <- getDesignFisher(kMax = 3, alpha0Vec = c(0.5, 0.5))

    simulationResults <- getSimulationSurvival(
        design = design, pi2 = 0.2, pi1 = seq(0.3, 0.45, 0.05),
        directionUpper = TRUE, maxNumberOfSubjects = 500, plannedEvents = (1:design$kMax) * 20,
        allocation1 = 1, allocation2 = 1, accrualTime = c(0, 3, 6, 12),
        accrualIntensity = c(0.1, 0.2, 0.2), dropoutRate1 = 0, dropoutRate2 = 0,
        dropoutTime = 12, conditionalPower = 0.8, minNumberOfEventsPerStage = c(NA_real_, 10, 10),
        maxNumberOfEventsPerStage = c(NA_real_, 100, 200), maxNumberOfIterations = 100,
        seed = 1234567890
    )

    ## Comparison of the results of SimulationResultsSurvival object 'simulationResults' with expected results
    expect_equal(simulationResults$median1, c(23.320299, 19.308487, 16.282985, 13.9131), tolerance = 1e-07, label = paste0(simulationResults$median1))
    expect_equal(simulationResults$median2, 37.275405, tolerance = 1e-07, label = paste0(simulationResults$median2))
    expect_equal(simulationResults$accrualIntensity, c(23.809524, 47.619048, 47.619048), tolerance = 1e-07, label = paste0(simulationResults$accrualIntensity))
    expect_equal(simulationResults$lambda1, c(0.029722912, 0.035898576, 0.042568802, 0.04981975), tolerance = 1e-07, label = paste0(simulationResults$lambda1))
    expect_equal(simulationResults$lambda2, 0.018595296, tolerance = 1e-07, label = paste0(simulationResults$lambda2))
    expect_equal(simulationResults$hazardRatio, c(1.5984103, 1.9305192, 2.2892242, 2.6791588), tolerance = 1e-07, label = paste0(simulationResults$hazardRatio))
    expect_equal(simulationResults$analysisTime[1, ], c(7.2763799, 7.0838561, 6.7193502, 6.3616317), tolerance = 1e-07, label = paste0(simulationResults$analysisTime[1, ]))
    expect_equal(simulationResults$analysisTime[2, ], c(16.764021, 14.756285, 13.821816, 12.988284), tolerance = 1e-07, label = paste0(simulationResults$analysisTime[2, ]))
    expect_equal(simulationResults$analysisTime[3, ], c(38.977945, 24.200748, 26.934721, 11.875967), tolerance = 1e-07, label = paste0(simulationResults$analysisTime[3, ]))
    expect_equal(simulationResults$studyDuration, c(22.098154, 13.978342, 11.899449, 9.7796143), tolerance = 1e-07, label = paste0(simulationResults$studyDuration))
    expect_equal(simulationResults$eventsNotAchieved[1, ], c(0, 0, 0, 0), label = paste0(simulationResults$eventsNotAchieved[1, ]))
    expect_equal(simulationResults$eventsNotAchieved[2, ], c(0, 0, 0, 0), label = paste0(simulationResults$eventsNotAchieved[2, ]))
    expect_equal(simulationResults$eventsNotAchieved[3, ], c(0, 0, 0, 0), label = paste0(simulationResults$eventsNotAchieved[3, ]))
    expect_equal(simulationResults$numberOfSubjects[1, ], c(275.04, 265.86, 248.46, 231.45), tolerance = 1e-07, label = paste0(simulationResults$numberOfSubjects[1, ]))
    expect_equal(simulationResults$numberOfSubjects[2, ], c(496.07246, 481.84722, 476, 463.84), tolerance = 1e-07, label = paste0(simulationResults$numberOfSubjects[2, ]))
    expect_equal(simulationResults$numberOfSubjects[3, ], c(500, 500, 500, 494), label = paste0(simulationResults$numberOfSubjects[3, ]))
    expect_equal(simulationResults$cumulativeEventsPerStage[1, ], c(20, 20, 20, 20), label = paste0(simulationResults$cumulativeEventsPerStage[1, ]))
    expect_equal(simulationResults$cumulativeEventsPerStage[2, ], c(106.50725, 94.541667, 94.677966, 94.48), tolerance = 1e-07, label = paste0(simulationResults$cumulativeEventsPerStage[2, ]))
    expect_equal(simulationResults$cumulativeEventsPerStage[3, ], c(261.75049, 192.20833, 218.96368, 131.48), tolerance = 1e-07, label = paste0(simulationResults$cumulativeEventsPerStage[3, ]))
    expect_equal(simulationResults$iterations[1, ], c(100, 100, 100, 100), label = paste0(simulationResults$iterations[1, ]))
    expect_equal(simulationResults$iterations[2, ], c(69, 72, 59, 50), label = paste0(simulationResults$iterations[2, ]))
    expect_equal(simulationResults$iterations[3, ], c(37, 12, 7, 2), label = paste0(simulationResults$iterations[3, ]))
    expect_equal(simulationResults$overallReject, c(0.84, 0.92, 0.98, 0.99), tolerance = 1e-07, label = paste0(simulationResults$overallReject))
    expect_gte(object = simulationResults$overallReject[1], expected = 0)
    expect_gte(object = simulationResults$overallReject[2], expected = 0)
    expect_gte(object = simulationResults$overallReject[3], expected = 0)
    expect_gte(object = simulationResults$overallReject[4], expected = 0)
    expect_lte(object = simulationResults$overallReject[1], expected = 1)
    expect_lte(object = simulationResults$overallReject[2], expected = 1)
    expect_lte(object = simulationResults$overallReject[3], expected = 1)
    expect_lte(object = simulationResults$overallReject[4], expected = 1)
    expect_equal(simulationResults$rejectPerStage[1, ], c(0.18, 0.22, 0.39, 0.49), tolerance = 1e-07, label = paste0(simulationResults$rejectPerStage[1, ]))
    expect_equal(simulationResults$rejectPerStage[2, ], c(0.31, 0.59, 0.52, 0.48), tolerance = 1e-07, label = paste0(simulationResults$rejectPerStage[2, ]))
    expect_equal(simulationResults$rejectPerStage[3, ], c(0.35, 0.11, 0.07, 0.02), tolerance = 1e-07, label = paste0(simulationResults$rejectPerStage[3, ]))
    expect_equal(simulationResults$futilityStop, c(0.14, 0.07, 0.02, 0.01), tolerance = 1e-07, label = paste0(simulationResults$futilityStop))
    expect_equal(simulationResults$futilityPerStage[1, ], c(0.13, 0.06, 0.02, 0.01), tolerance = 1e-07, label = paste0(simulationResults$futilityPerStage[1, ]))
    expect_equal(simulationResults$futilityPerStage[2, ], c(0.01, 0.01, 0, 0), tolerance = 1e-07, label = paste0(simulationResults$futilityPerStage[2, ]))
    expect_equal(simulationResults$earlyStop, c(0.63, 0.88, 0.93, 0.98), tolerance = 1e-07, label = paste0(simulationResults$earlyStop))
    expect_equal(simulationResults$expectedNumberOfSubjects, c(429.00559, 423.54913, 384.3886, 348.2482), tolerance = 1e-07, label = paste0(simulationResults$expectedNumberOfSubjects))
    expect_equal(simulationResults$expectedNumberOfEvents, c(137.13, 85.39, 72.76, 57.98), tolerance = 1e-07, label = paste0(simulationResults$expectedNumberOfEvents))
    expect_equal(simulationResults$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(simulationResults$conditionalPowerAchieved[1, ]))
    expect_equal(simulationResults$conditionalPowerAchieved[2, ], c(0.46273079, 0.58305775, 0.61313502, 0.59484117), tolerance = 1e-07, label = paste0(simulationResults$conditionalPowerAchieved[2, ]))
    expect_equal(simulationResults$conditionalPowerAchieved[3, ], c(0.66165116, 0.75066235, 0.71981679, 0.8), tolerance = 1e-07, label = paste0(simulationResults$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(simulationResults), NA)))
        expect_output(print(simulationResults)$show())
        invisible(capture.output(expect_error(summary(simulationResults), NA)))
        expect_output(summary(simulationResults)$show())
        simulationResultsCodeBased <- eval(parse(text = getObjectRCode(simulationResults, stringWrapParagraphWidth = NULL)))
        expect_equal(simulationResultsCodeBased$median1, simulationResults$median1, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$median2, simulationResults$median2, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$accrualIntensity, simulationResults$accrualIntensity, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$lambda1, simulationResults$lambda1, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$lambda2, simulationResults$lambda2, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$hazardRatio, simulationResults$hazardRatio, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$analysisTime, simulationResults$analysisTime, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$studyDuration, simulationResults$studyDuration, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$eventsNotAchieved, simulationResults$eventsNotAchieved, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$numberOfSubjects, simulationResults$numberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$cumulativeEventsPerStage, simulationResults$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$iterations, simulationResults$iterations, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$overallReject, simulationResults$overallReject, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$rejectPerStage, simulationResults$rejectPerStage, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$futilityStop, simulationResults$futilityStop, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$futilityPerStage, simulationResults$futilityPerStage, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$earlyStop, simulationResults$earlyStop, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$expectedNumberOfSubjects, simulationResults$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$expectedNumberOfEvents, simulationResults$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$conditionalPowerAchieved, simulationResults$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(simulationResults), "character")
        df <- as.data.frame(simulationResults)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(simulationResults)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSimulationSurvival': configuration 4", {
    .skipTestIfDisabled()

    # @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
    # @refFS[Sec.]{fs:sec:simulatingTestingOneHypothesis}
    # @refFS[Tab.]{fs:tab:output:getSimulationSurvival}
    # @refFS[Formula]{fs:simulationSurvivalTimeGenerate}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    # @refFS[Formula]{fs:simulationSurvivalLogRank}
    # @refFS[Formula]{fs:simulationSurvivalIncrements}
    # @refFS[Formula]{fs:simulationSurvivalHazardEstimate}
    # @refFS[Formula]{fs:testStatisticGroupSequential}
    design <- getDesignGroupSequential(kMax = 3, typeOfDesign = "WT", deltaWT = 0.25)

    piecewiseSurvivalTime <- list(
        "<6" = 0.025,
        "6 - <9" = 0.04,
        "9 - <15" = 0.015,
        "15 - <21" = 0.01,
        ">=21" = 0.007
    )

    simulationResults <- getSimulationSurvival(
        design = design,
        directionUpper = TRUE, maxNumberOfSubjects = 500, plannedEvents = (1:design$kMax) * 20,
        allocation1 = 1, allocation2 = 1, accrualTime = c(0, 3, 6, 12),
        piecewiseSurvivalTime = piecewiseSurvivalTime, hazardRatio = 1.7,
        accrualIntensity = c(0.1, 0.2, 0.2), dropoutRate1 = 0, dropoutRate2 = 0,
        dropoutTime = 12, conditionalPower = 0.8, minNumberOfEventsPerStage = c(NA_real_, 10, 10),
        maxNumberOfEventsPerStage = c(NA_real_, 100, 200), maxNumberOfIterations = 100,
        seed = 1234567890
    )

    ## Comparison of the results of SimulationResultsSurvival object 'simulationResults' with expected results
    expect_equal(simulationResults$accrualIntensity, c(23.809524, 47.619048, 47.619048), tolerance = 1e-07, label = paste0(simulationResults$accrualIntensity))
    expect_equal(simulationResults$lambda1, c(0.0425, 0.068, 0.0255, 0.017, 0.0119), tolerance = 1e-07, label = paste0(simulationResults$lambda1))
    expect_equal(simulationResults$analysisTime[1, ], 6.3619038, tolerance = 1e-07, label = paste0(simulationResults$analysisTime[1, ]))
    expect_equal(simulationResults$analysisTime[2, ], 12.345684, tolerance = 1e-07, label = paste0(simulationResults$analysisTime[2, ]))
    expect_equal(simulationResults$analysisTime[3, ], 36.687962, tolerance = 1e-07, label = paste0(simulationResults$analysisTime[3, ]))
    expect_equal(simulationResults$studyDuration, 19.26207, tolerance = 1e-07, label = paste0(simulationResults$studyDuration))
    expect_equal(simulationResults$eventsNotAchieved[1, ], 0, label = paste0(simulationResults$eventsNotAchieved[1, ]))
    expect_equal(simulationResults$eventsNotAchieved[2, ], 0, label = paste0(simulationResults$eventsNotAchieved[2, ]))
    expect_equal(simulationResults$eventsNotAchieved[3, ], 0, label = paste0(simulationResults$eventsNotAchieved[3, ]))
    expect_equal(simulationResults$numberOfSubjects[1, ], 231.41, tolerance = 1e-07, label = paste0(simulationResults$numberOfSubjects[1, ]))
    expect_equal(simulationResults$numberOfSubjects[2, ], 448.23158, tolerance = 1e-07, label = paste0(simulationResults$numberOfSubjects[2, ]))
    expect_equal(simulationResults$numberOfSubjects[3, ], 491.66667, tolerance = 1e-07, label = paste0(simulationResults$numberOfSubjects[3, ]))
    expect_equal(simulationResults$cumulativeEventsPerStage[1, ], 20, label = paste0(simulationResults$cumulativeEventsPerStage[1, ]))
    expect_equal(simulationResults$cumulativeEventsPerStage[2, ], 91.694737, tolerance = 1e-07, label = paste0(simulationResults$cumulativeEventsPerStage[2, ]))
    expect_equal(simulationResults$cumulativeEventsPerStage[3, ], 203.4614, tolerance = 1e-07, label = paste0(simulationResults$cumulativeEventsPerStage[3, ]))
    expect_equal(simulationResults$iterations[1, ], 100, label = paste0(simulationResults$iterations[1, ]))
    expect_equal(simulationResults$iterations[2, ], 95, label = paste0(simulationResults$iterations[2, ]))
    expect_equal(simulationResults$iterations[3, ], 30, label = paste0(simulationResults$iterations[3, ]))
    expect_equal(simulationResults$overallReject, 0.99, tolerance = 1e-07, label = paste0(simulationResults$overallReject))
    expect_gte(object = simulationResults$overallReject[1], expected = 0)
    expect_lte(object = simulationResults$overallReject[1], expected = 1)
    expect_equal(simulationResults$rejectPerStage[1, ], 0.05, tolerance = 1e-07, label = paste0(simulationResults$rejectPerStage[1, ]))
    expect_equal(simulationResults$rejectPerStage[2, ], 0.65, tolerance = 1e-07, label = paste0(simulationResults$rejectPerStage[2, ]))
    expect_equal(simulationResults$rejectPerStage[3, ], 0.29, tolerance = 1e-07, label = paste0(simulationResults$rejectPerStage[3, ]))
    expect_equal(simulationResults$futilityStop, 0, label = paste0(simulationResults$futilityStop))
    expect_equal(simulationResults$futilityPerStage[1, ], 0, label = paste0(simulationResults$futilityPerStage[1, ]))
    expect_equal(simulationResults$futilityPerStage[2, ], 0, label = paste0(simulationResults$futilityPerStage[2, ]))
    expect_equal(simulationResults$earlyStop, 0.7, tolerance = 1e-07, label = paste0(simulationResults$earlyStop))
    expect_equal(simulationResults$expectedNumberOfSubjects, 450.42103, tolerance = 1e-07, label = paste0(simulationResults$expectedNumberOfSubjects))
    expect_equal(simulationResults$expectedNumberOfEvents, 121.64, tolerance = 1e-07, label = paste0(simulationResults$expectedNumberOfEvents))
    expect_equal(simulationResults$conditionalPowerAchieved[1, ], NA_real_, label = paste0(simulationResults$conditionalPowerAchieved[1, ]))
    expect_equal(simulationResults$conditionalPowerAchieved[2, ], 0.49425129, tolerance = 1e-07, label = paste0(simulationResults$conditionalPowerAchieved[2, ]))
    expect_equal(simulationResults$conditionalPowerAchieved[3, ], 0.73157546, tolerance = 1e-07, label = paste0(simulationResults$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(simulationResults), NA)))
        expect_output(print(simulationResults)$show())
        invisible(capture.output(expect_error(summary(simulationResults), NA)))
        expect_output(summary(simulationResults)$show())
        simulationResultsCodeBased <- eval(parse(text = getObjectRCode(simulationResults, stringWrapParagraphWidth = NULL)))
        expect_equal(simulationResultsCodeBased$accrualIntensity, simulationResults$accrualIntensity, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$lambda1, simulationResults$lambda1, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$analysisTime, simulationResults$analysisTime, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$studyDuration, simulationResults$studyDuration, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$eventsNotAchieved, simulationResults$eventsNotAchieved, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$numberOfSubjects, simulationResults$numberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$cumulativeEventsPerStage, simulationResults$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$iterations, simulationResults$iterations, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$overallReject, simulationResults$overallReject, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$rejectPerStage, simulationResults$rejectPerStage, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$futilityStop, simulationResults$futilityStop, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$futilityPerStage, simulationResults$futilityPerStage, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$earlyStop, simulationResults$earlyStop, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$expectedNumberOfSubjects, simulationResults$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$expectedNumberOfEvents, simulationResults$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$conditionalPowerAchieved, simulationResults$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(simulationResults), "character")
        df <- as.data.frame(simulationResults)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(simulationResults)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSimulationSurvival': configuration 5", {
    .skipTestIfDisabled()

    # @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
    # @refFS[Sec.]{fs:sec:simulatingTestingOneHypothesis}
    # @refFS[Tab.]{fs:tab:output:getSimulationSurvival}
    # @refFS[Formula]{fs:simulationSurvivalTimeGenerate}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    # @refFS[Formula]{fs:simulationSurvivalLogRank}
    # @refFS[Formula]{fs:simulationSurvivalIncrements}
    # @refFS[Formula]{fs:simulationSurvivalHazardEstimate}
    # @refFS[Formula]{fs:testStatisticGroupSequential}
    design <- getDesignGroupSequential(kMax = 3, typeOfDesign = "WT", deltaWT = 0.25)
    design2 <- getDesignGroupSequential(kMax = 3, typeOfDesign = "WT", deltaWT = 0.25, directionUpper = FALSE)

    simulationResults <- getSimulationSurvival(
        design = design, pi2 = 0.6, pi1 = seq(0.3, 0.45, 0.05),
        directionUpper = FALSE, maxNumberOfSubjects = 200, plannedEvents = (1:design$kMax) * 20,
        allocation1 = 1, allocation2 = 1, accrualTime = c(0, 3, 6, 12),
        accrualIntensity = c(0.1, 0.2, 0.2), dropoutRate1 = 0, dropoutRate2 = 0,
        dropoutTime = 12, conditionalPower = 0.8, minNumberOfEventsPerStage = c(NA_real_, 10, 10),
        maxNumberOfEventsPerStage = c(NA_real_, 40, 40), maxNumberOfIterations = 100,
        seed = 1234567890
    )

    ## Comparison of the results of SimulationResultsSurvival object 'simulationResults' with expected results
    expect_equal(simulationResults$median1, c(23.320299, 19.308487, 16.282985, 13.9131), tolerance = 1e-07, label = paste0(simulationResults$median1))
    expect_equal(simulationResults$median2, 9.0776496, tolerance = 1e-07, label = paste0(simulationResults$median2))
    expect_equal(simulationResults$accrualIntensity, c(9.5238095, 19.047619, 19.047619), tolerance = 1e-07, label = paste0(simulationResults$accrualIntensity))
    expect_equal(simulationResults$lambda1, c(0.029722912, 0.035898576, 0.042568802, 0.04981975), tolerance = 1e-07, label = paste0(simulationResults$lambda1))
    expect_equal(simulationResults$lambda2, 0.076357561, tolerance = 1e-07, label = paste0(simulationResults$lambda2))
    expect_equal(simulationResults$hazardRatio, c(0.38925958, 0.47013781, 0.55749295, 0.6524534), tolerance = 1e-07, label = paste0(simulationResults$hazardRatio))
    expect_equal(simulationResults$analysisTime[1, ], c(8.1674426, 7.9228743, 7.6045868, 7.4881493), tolerance = 1e-07, label = paste0(simulationResults$analysisTime[1, ]))
    expect_equal(simulationResults$analysisTime[2, ], c(12.354338, 12.56529, 12.380125, 12.254955), tolerance = 1e-07, label = paste0(simulationResults$analysisTime[2, ]))
    expect_equal(simulationResults$analysisTime[3, ], c(16.473595, 17.9949, 17.847597, 17.390492), tolerance = 1e-07, label = paste0(simulationResults$analysisTime[3, ]))
    expect_equal(simulationResults$studyDuration, c(12.562909, 13.818364, 15.044701, 16.144285), tolerance = 1e-07, label = paste0(simulationResults$studyDuration))
    expect_equal(simulationResults$eventsNotAchieved[1, ], c(0, 0, 0, 0), label = paste0(simulationResults$eventsNotAchieved[1, ]))
    expect_equal(simulationResults$eventsNotAchieved[2, ], c(0, 0, 0, 0), label = paste0(simulationResults$eventsNotAchieved[2, ]))
    expect_equal(simulationResults$eventsNotAchieved[3, ], c(0, 0, 0, 0), label = paste0(simulationResults$eventsNotAchieved[3, ]))
    expect_equal(simulationResults$numberOfSubjects[1, ], c(126.03, 121.42, 115.37, 113.16), tolerance = 1e-07, label = paste0(simulationResults$numberOfSubjects[1, ]))
    expect_equal(simulationResults$numberOfSubjects[2, ], c(187.50575, 190.98876, 193.16304, 192.33), tolerance = 1e-07, label = paste0(simulationResults$numberOfSubjects[2, ]))
    expect_equal(simulationResults$numberOfSubjects[3, ], c(199.11111, 200, 199.39655, 199.28571), tolerance = 1e-07, label = paste0(simulationResults$numberOfSubjects[3, ]))
    expect_equal(simulationResults$cumulativeEventsPerStage[1, ], c(20, 20, 20, 20), label = paste0(simulationResults$cumulativeEventsPerStage[1, ]))
    expect_equal(simulationResults$cumulativeEventsPerStage[2, ], c(48.54023, 51.561798, 55.130435, 55.79), tolerance = 1e-07, label = paste0(simulationResults$cumulativeEventsPerStage[2, ]))
    expect_equal(simulationResults$cumulativeEventsPerStage[3, ], c(74.929119, 85.120621, 92.285607, 93.582208), tolerance = 1e-07, label = paste0(simulationResults$cumulativeEventsPerStage[3, ]))
    expect_equal(simulationResults$iterations[1, ], c(100, 100, 100, 100), label = paste0(simulationResults$iterations[1, ]))
    expect_equal(simulationResults$iterations[2, ], c(87, 89, 92, 100), label = paste0(simulationResults$iterations[2, ]))
    expect_equal(simulationResults$iterations[3, ], c(18, 34, 58, 77), label = paste0(simulationResults$iterations[3, ]))
    expect_equal(simulationResults$overallReject, c(0.99, 0.97, 0.68, 0.48), tolerance = 1e-07, label = paste0(simulationResults$overallReject))
    expect_gte(object = simulationResults$overallReject[1], expected = 0)
    expect_gte(object = simulationResults$overallReject[2], expected = 0)
    expect_gte(object = simulationResults$overallReject[3], expected = 0)
    expect_gte(object = simulationResults$overallReject[4], expected = 0)
    expect_lte(object = simulationResults$overallReject[1], expected = 1)
    expect_lte(object = simulationResults$overallReject[2], expected = 1)
    expect_lte(object = simulationResults$overallReject[3], expected = 1)
    expect_lte(object = simulationResults$overallReject[4], expected = 1)
    expect_equal(simulationResults$rejectPerStage[1, ], c(0.13, 0.11, 0.08, 0), tolerance = 1e-07, label = paste0(simulationResults$rejectPerStage[1, ]))
    expect_equal(simulationResults$rejectPerStage[2, ], c(0.69, 0.55, 0.34, 0.23), tolerance = 1e-07, label = paste0(simulationResults$rejectPerStage[2, ]))
    expect_equal(simulationResults$rejectPerStage[3, ], c(0.17, 0.31, 0.26, 0.25), tolerance = 1e-07, label = paste0(simulationResults$rejectPerStage[3, ]))
    expect_equal(simulationResults$futilityStop, c(0, 0, 0, 0), label = paste0(simulationResults$futilityStop))
    expect_equal(simulationResults$futilityPerStage[1, ], c(0, 0, 0, 0), label = paste0(simulationResults$futilityPerStage[1, ]))
    expect_equal(simulationResults$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(simulationResults$futilityPerStage[2, ]))
    expect_equal(simulationResults$earlyStop, c(0.82, 0.66, 0.42, 0.23), tolerance = 1e-07, label = paste0(simulationResults$earlyStop))
    expect_equal(simulationResults$expectedNumberOfSubjects, c(181.60287, 186.40002, 190.55503, 197.6859), tolerance = 1e-07, label = paste0(simulationResults$expectedNumberOfSubjects))
    expect_equal(simulationResults$expectedNumberOfEvents, c(49.58, 59.5, 73.87, 84.89), tolerance = 1e-07, label = paste0(simulationResults$expectedNumberOfEvents))
    expect_equal(simulationResults$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(simulationResults$conditionalPowerAchieved[1, ]))
    expect_equal(simulationResults$conditionalPowerAchieved[2, ], c(0.56161185, 0.47418383, 0.31608317, 0.29578133), tolerance = 1e-07, label = paste0(simulationResults$conditionalPowerAchieved[2, ]))
    expect_equal(simulationResults$conditionalPowerAchieved[3, ], c(0.71394365, 0.57778506, 0.37448609, 0.32265113), tolerance = 1e-07, label = paste0(simulationResults$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(simulationResults), NA)))
        expect_output(print(simulationResults)$show())
        invisible(capture.output(expect_error(summary(simulationResults), NA)))
        expect_output(summary(simulationResults)$show())
        simulationResultsCodeBased <- eval(parse(text = getObjectRCode(simulationResults, stringWrapParagraphWidth = NULL)))
        expect_equal(simulationResultsCodeBased$median1, simulationResults$median1, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$median2, simulationResults$median2, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$accrualIntensity, simulationResults$accrualIntensity, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$lambda1, simulationResults$lambda1, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$lambda2, simulationResults$lambda2, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$hazardRatio, simulationResults$hazardRatio, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$analysisTime, simulationResults$analysisTime, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$studyDuration, simulationResults$studyDuration, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$eventsNotAchieved, simulationResults$eventsNotAchieved, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$numberOfSubjects, simulationResults$numberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$cumulativeEventsPerStage, simulationResults$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$iterations, simulationResults$iterations, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$overallReject, simulationResults$overallReject, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$rejectPerStage, simulationResults$rejectPerStage, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$futilityStop, simulationResults$futilityStop, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$futilityPerStage, simulationResults$futilityPerStage, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$earlyStop, simulationResults$earlyStop, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$expectedNumberOfSubjects, simulationResults$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$expectedNumberOfEvents, simulationResults$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$conditionalPowerAchieved, simulationResults$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(simulationResults), "character")
        df <- as.data.frame(simulationResults)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(simulationResults)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    simulationResults2 <- getSimulationSurvival(
        design = design2, pi2 = 0.6, pi1 = seq(0.3, 0.45, 0.05),
        maxNumberOfSubjects = 200, plannedEvents = (1:design$kMax) * 20,
        allocation1 = 1, allocation2 = 1, accrualTime = c(0, 3, 6, 12),
        accrualIntensity = c(0.1, 0.2, 0.2), dropoutRate1 = 0, dropoutRate2 = 0,
        dropoutTime = 12, conditionalPower = 0.8, minNumberOfEventsPerStage = c(NA_real_, 10, 10),
        maxNumberOfEventsPerStage = c(NA_real_, 40, 40), maxNumberOfIterations = 100,
        seed = 1234567890
    )


    ## Pairwise comparison of the results of simulationResults with the results of simulationResults2
    expect_equal(simulationResults$median1, simulationResults2$median1, tolerance = 1e-07)
    expect_equal(simulationResults$median2, simulationResults2$median2, tolerance = 1e-07)
    expect_equal(simulationResults$accrualIntensity, simulationResults2$accrualIntensity, tolerance = 1e-07)
    expect_equal(simulationResults$lambda1, simulationResults2$lambda1, tolerance = 1e-07)
    expect_equal(simulationResults$lambda2, simulationResults2$lambda2, tolerance = 1e-07)
    expect_equal(simulationResults$hazardRatio, simulationResults2$hazardRatio, tolerance = 1e-07)
    expect_equal(simulationResults2$analysisTime[1, ], simulationResults$analysisTime[1, ], tolerance = 1e-07)
    expect_equal(simulationResults2$analysisTime[2, ], simulationResults$analysisTime[2, ], tolerance = 1e-07)
    expect_equal(simulationResults2$analysisTime[3, ], simulationResults$analysisTime[3, ], tolerance = 1e-07)
    expect_equal(simulationResults$studyDuration, simulationResults2$studyDuration, tolerance = 1e-07)
    expect_equal(simulationResults2$eventsNotAchieved[1, ], simulationResults$eventsNotAchieved[1, ])
    expect_equal(simulationResults2$eventsNotAchieved[2, ], simulationResults$eventsNotAchieved[2, ])
    expect_equal(simulationResults2$eventsNotAchieved[3, ], simulationResults$eventsNotAchieved[3, ])
    expect_equal(simulationResults2$numberOfSubjects[1, ], simulationResults$numberOfSubjects[1, ], tolerance = 1e-07)
    expect_equal(simulationResults2$numberOfSubjects[2, ], simulationResults$numberOfSubjects[2, ], tolerance = 1e-07)
    expect_equal(simulationResults2$numberOfSubjects[3, ], simulationResults$numberOfSubjects[3, ], tolerance = 1e-07)
    expect_equal(simulationResults2$cumulativeEventsPerStage[1, ], simulationResults$cumulativeEventsPerStage[1, ])
    expect_equal(simulationResults2$cumulativeEventsPerStage[2, ], simulationResults$cumulativeEventsPerStage[2, ], tolerance = 1e-07)
    expect_equal(simulationResults2$cumulativeEventsPerStage[3, ], simulationResults$cumulativeEventsPerStage[3, ], tolerance = 1e-07)
    expect_equal(simulationResults2$iterations[1, ], simulationResults$iterations[1, ])
    expect_equal(simulationResults2$iterations[2, ], simulationResults$iterations[2, ])
    expect_equal(simulationResults2$iterations[3, ], simulationResults$iterations[3, ])
    expect_equal(simulationResults$overallReject, simulationResults2$overallReject, tolerance = 1e-07)
    expect_equal(simulationResults2$rejectPerStage[1, ], simulationResults$rejectPerStage[1, ], tolerance = 1e-07)
    expect_equal(simulationResults2$rejectPerStage[2, ], simulationResults$rejectPerStage[2, ], tolerance = 1e-07)
    expect_equal(simulationResults2$rejectPerStage[3, ], simulationResults$rejectPerStage[3, ], tolerance = 1e-07)
    expect_equal(simulationResults$futilityStop, simulationResults2$futilityStop)
    expect_equal(simulationResults2$futilityPerStage[1, ], simulationResults$futilityPerStage[1, ])
    expect_equal(simulationResults2$futilityPerStage[2, ], simulationResults$futilityPerStage[2, ])
    expect_equal(simulationResults$earlyStop, simulationResults2$earlyStop, tolerance = 1e-07)
    expect_equal(simulationResults$expectedNumberOfSubjects, simulationResults2$expectedNumberOfSubjects, tolerance = 1e-07)
    expect_equal(simulationResults$expectedNumberOfEvents, simulationResults2$expectedNumberOfEvents, tolerance = 1e-07)
    expect_equal(simulationResults2$conditionalPowerAchieved[1, ], simulationResults$conditionalPowerAchieved[1, ])
    expect_equal(simulationResults2$conditionalPowerAchieved[2, ], simulationResults$conditionalPowerAchieved[2, ], tolerance = 1e-07)
    expect_equal(simulationResults2$conditionalPowerAchieved[3, ], simulationResults$conditionalPowerAchieved[3, ], tolerance = 1e-07)
})

test_that("'getSimulationSurvival': configuration 6", {
    .skipTestIfDisabled()

    # @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
    # @refFS[Sec.]{fs:sec:simulatingTestingOneHypothesis}
    # @refFS[Tab.]{fs:tab:output:getSimulationSurvival}
    # @refFS[Formula]{fs:simulationSurvivalTimeGenerate}
    # @refFS[Formula]{fs:pieceWiseExponentialRandomVariable}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    # @refFS[Formula]{fs:simulationSurvivalLogRank}
    # @refFS[Formula]{fs:simulationSurvivalIncrements}
    # @refFS[Formula]{fs:simulationSurvivalHazardEstimate}
    # @refFS[Formula]{fs:testStatisticGroupSequential}
    design <- getDesignGroupSequential(kMax = 3, typeOfDesign = "WT", deltaWT = 0.25)

    piecewiseSurvivalTime <- list(
        "0 - <6" = 0.025,
        "6 - <9" = 0.04,
        "9 - <15" = 0.015,
        "15 - <21" = 0.01,
        ">=21" = 0.007
    )

    suppressWarnings(simulationResults <- getSimulationSurvival(
        design = design,
        directionUpper = FALSE, maxNumberOfSubjects = 260, plannedEvents = (1:design$kMax) * 20,
        allocation1 = 1, allocation2 = 1, accrualTime = c(0, 3, 6, 12),
        piecewiseSurvivalTime = piecewiseSurvivalTime, hazardRatio = 0.8,
        accrualIntensity = c(0.1, 0.2, 0.2), dropoutRate1 = 0, dropoutRate2 = 0,
        dropoutTime = 12, conditionalPower = 0.8, minNumberOfEventsPerStage = c(NA_real_, 10, 10),
        maxNumberOfEventsPerStage = c(NA_real_, 400, 200), maxNumberOfIterations = 100,
        seed = 1234567890
    ))


    ## Comparison of the results of SimulationResultsSurvival object 'simulationResults' with expected results
    expect_equal(simulationResults$accrualIntensity, c(12.380952, 24.761905, 24.761905), tolerance = 1e-07, label = paste0(simulationResults$accrualIntensity))
    expect_equal(simulationResults$lambda1, c(0.02, 0.032, 0.012, 0.008, 0.0056), tolerance = 1e-07, label = paste0(simulationResults$lambda1))
    expect_equal(simulationResults$analysisTime[1, ], 9.8001583, tolerance = 1e-07, label = paste0(simulationResults$analysisTime[1, ]))
    expect_equal(simulationResults$analysisTime[2, ], 134.1032, tolerance = 1e-07, label = paste0(simulationResults$analysisTime[2, ]))
    expect_equal(simulationResults$analysisTime[3, ], 189.74226, tolerance = 1e-07, label = paste0(simulationResults$analysisTime[3, ]))
    expect_equal(simulationResults$studyDuration, 39.514056, tolerance = 1e-07, label = paste0(simulationResults$studyDuration))
    expect_equal(simulationResults$eventsNotAchieved[1, ], 0, label = paste0(simulationResults$eventsNotAchieved[1, ]))
    expect_equal(simulationResults$eventsNotAchieved[2, ], 0.62, tolerance = 1e-07, label = paste0(simulationResults$eventsNotAchieved[2, ]))
    expect_equal(simulationResults$eventsNotAchieved[3, ], 0.18, tolerance = 1e-07, label = paste0(simulationResults$eventsNotAchieved[3, ]))
    expect_equal(simulationResults$numberOfSubjects[1, ], 205.17, tolerance = 1e-07, label = paste0(simulationResults$numberOfSubjects[1, ]))
    expect_equal(simulationResults$numberOfSubjects[2, ], 258.63158, tolerance = 1e-07, label = paste0(simulationResults$numberOfSubjects[2, ]))
    expect_equal(simulationResults$numberOfSubjects[3, ], 258.76923, tolerance = 1e-07, label = paste0(simulationResults$numberOfSubjects[3, ]))
    expect_equal(simulationResults$cumulativeEventsPerStage[1, ], 20, label = paste0(simulationResults$cumulativeEventsPerStage[1, ]))
    expect_equal(simulationResults$cumulativeEventsPerStage[2, ], 125.21053, tolerance = 1e-07, label = paste0(simulationResults$cumulativeEventsPerStage[2, ]))
    expect_equal(simulationResults$cumulativeEventsPerStage[3, ], 222.51822, tolerance = 1e-07, label = paste0(simulationResults$cumulativeEventsPerStage[3, ]))
    expect_equal(simulationResults$iterations[1, ], 100, label = paste0(simulationResults$iterations[1, ]))
    expect_equal(simulationResults$iterations[2, ], 38, label = paste0(simulationResults$iterations[2, ]))
    expect_equal(simulationResults$iterations[3, ], 13, label = paste0(simulationResults$iterations[3, ]))
    expect_equal(simulationResults$overallReject, 0.1, tolerance = 1e-07, label = paste0(simulationResults$overallReject))
    expect_gte(object = simulationResults$overallReject[1], expected = 0)
    expect_lte(object = simulationResults$overallReject[1], expected = 1)
    expect_equal(simulationResults$rejectPerStage[1, ], 0, label = paste0(simulationResults$rejectPerStage[1, ]))
    expect_equal(simulationResults$rejectPerStage[2, ], 0.07, tolerance = 1e-07, label = paste0(simulationResults$rejectPerStage[2, ]))
    expect_equal(simulationResults$rejectPerStage[3, ], 0.03, tolerance = 1e-07, label = paste0(simulationResults$rejectPerStage[3, ]))
    expect_equal(simulationResults$futilityStop, 0, label = paste0(simulationResults$futilityStop))
    expect_equal(simulationResults$futilityPerStage[1, ], 0, label = paste0(simulationResults$futilityPerStage[1, ]))
    expect_equal(simulationResults$futilityPerStage[2, ], 0, label = paste0(simulationResults$futilityPerStage[2, ]))
    expect_equal(simulationResults$earlyStop, 0.07, tolerance = 1e-07, label = paste0(simulationResults$earlyStop))
    expect_equal(simulationResults$expectedNumberOfSubjects, 258.7596, tolerance = 1e-07, label = paste0(simulationResults$expectedNumberOfSubjects))
    expect_equal(simulationResults$expectedNumberOfEvents, 215.70668, tolerance = 1e-07, label = paste0(simulationResults$expectedNumberOfEvents))
    expect_equal(simulationResults$conditionalPowerAchieved[1, ], NA_real_, label = paste0(simulationResults$conditionalPowerAchieved[1, ]))
    expect_equal(simulationResults$conditionalPowerAchieved[2, ], 0.80033324, tolerance = 1e-07, label = paste0(simulationResults$conditionalPowerAchieved[2, ]))
    expect_equal(simulationResults$conditionalPowerAchieved[3, ], 0.64354689, tolerance = 1e-07, label = paste0(simulationResults$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(simulationResults), NA)))
        expect_output(print(simulationResults)$show())
        invisible(capture.output(expect_error(summary(simulationResults), NA)))
        expect_output(summary(simulationResults)$show())
        suppressWarnings(simulationResultsCodeBased <- eval(parse(text = getObjectRCode(simulationResults, stringWrapParagraphWidth = NULL))))
        expect_equal(simulationResultsCodeBased$accrualIntensity, simulationResults$accrualIntensity, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$lambda1, simulationResults$lambda1, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$analysisTime, simulationResults$analysisTime, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$studyDuration, simulationResults$studyDuration, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$eventsNotAchieved, simulationResults$eventsNotAchieved, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$numberOfSubjects, simulationResults$numberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$cumulativeEventsPerStage, simulationResults$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$iterations, simulationResults$iterations, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$overallReject, simulationResults$overallReject, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$rejectPerStage, simulationResults$rejectPerStage, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$futilityStop, simulationResults$futilityStop, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$futilityPerStage, simulationResults$futilityPerStage, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$earlyStop, simulationResults$earlyStop, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$expectedNumberOfSubjects, simulationResults$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$expectedNumberOfEvents, simulationResults$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$conditionalPowerAchieved, simulationResults$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(simulationResults), "character")
        df <- as.data.frame(simulationResults)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(simulationResults)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
    expect_warning(
        getSimulationSurvival(
            design = design,
            directionUpper = FALSE, maxNumberOfSubjects = 200, plannedEvents = (1:design$kMax) * 20,
            allocation1 = 1, allocation2 = 1, accrualTime = c(0, 3, 6, 12),
            piecewiseSurvivalTime = piecewiseSurvivalTime, hazardRatio = 0.8,
            accrualIntensity = c(0.1, 0.2, 0.2), dropoutRate1 = 0, dropoutRate2 = 0,
            dropoutTime = 12, conditionalPower = 0.8, minNumberOfEventsPerStage = c(NA_real_, 10, 10),
            maxNumberOfEventsPerStage = c(NA_real_, 400, 200), maxNumberOfIterations = 100,
            seed = 1234567890
        ),
        paste0(
            "Presumably due to drop-outs, required number of events were not achieved for at least one situation. ",
            "Increase the maximum number of subjects (200) to avoid this situation"
        ),
        fixed = TRUE
    )

    expect_warning(
        getSimulationSurvival(
            design = design,
            directionUpper = FALSE, maxNumberOfSubjects = 200, plannedEvents = (1:design$kMax) * 20,
            allocation1 = 1, allocation2 = 1, accrualTime = c(0, 3, 6, 12),
            piecewiseSurvivalTime = piecewiseSurvivalTime, hazardRatio = 0.8,
            accrualIntensity = c(0.1, 0.2, 0.2), dropoutRate1 = 0, dropoutRate2 = 0,
            dropoutTime = 12, conditionalPower = 0.8, minNumberOfEventsPerStage = c(NA_real_, 10, 10),
            maxNumberOfEventsPerStage = c(NA_real_, 400, 200), maxNumberOfIterations = 100,
            seed = 1234567890
        ),
        paste0(
            "Presumably due to drop-outs, required number of events were not achieved for at least one situation. ",
            "Increase the maximum number of subjects (200) to avoid this situation"
        ),
        fixed = TRUE
    )

    expect_warning(
        getSimulationSurvival(
            piecewiseSurvivalTime = list("<6" = 1.7, "6 - Inf" = 1.2),
            hazardRatio = c(0.65, 0.7, 0.8),
            plannedEvents = 98,
            maxNumberOfSubjects = 120,
            directionUpper = FALSE,
            maxNumberOfIterations = 1000,
            sided = 1, alpha = 0.1
        ),
        paste0(
            "Only the first 'hazardRatio' (0.65) was used for piecewise survival time definition ",
            "(use a loop over the function to simulate different hazard ratios)"
        ),
        fixed = TRUE
    )
})

test_that("'getSimulationSurvival': configuration 7", {
    .skipTestIfDisabled()

    # @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
    # @refFS[Sec.]{fs:sec:simulatingTestingOneHypothesis}
    # @refFS[Tab.]{fs:tab:output:getSimulationSurvival}
    # @refFS[Formula]{fs:simulationSurvivalTimeGenerate}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    # @refFS[Formula]{fs:pieceWiseExponentialRandomVariable}
    # @refFS[Formula]{fs:simulationSurvivalLogRank}
    # @refFS[Formula]{fs:simulationSurvivalIncrements}
    # @refFS[Formula]{fs:simulationSurvivalHazardEstimate}
    # @refFS[Formula]{fs:testStatisticGroupSequential}
    design <- getDesignGroupSequential(kMax = 3, typeOfDesign = "WT", deltaWT = 0.25)
    simulationResults <- getSimulationSurvival(
        design = design,
        directionUpper = FALSE, maxNumberOfSubjects = 260, plannedEvents = (1:design$kMax) * 20,
        allocation1 = 1, allocation2 = 1, accrualTime = c(0, 3, 6, 12),
        piecewiseSurvivalTime = list("0 - ?" = 0.025), hazardRatio = 0.8,
        accrualIntensity = c(0.1, 0.2, 0.2), dropoutRate1 = 0, dropoutRate2 = 0,
        dropoutTime = 12, conditionalPower = 0.8, minNumberOfEventsPerStage = c(NA_real_, 10, 10),
        maxNumberOfEventsPerStage = c(NA_real_, 100, 100), maxNumberOfIterations = 100,
        seed = 1234567890
    )

    ## Comparison of the results of SimulationResultsSurvival object 'simulationResults' with expected results
    expect_equal(simulationResults$median1, 34.657359, tolerance = 1e-07, label = paste0(simulationResults$median1))
    expect_equal(simulationResults$median2, 27.725887, tolerance = 1e-07, label = paste0(simulationResults$median2))
    expect_equal(simulationResults$accrualIntensity, c(12.380952, 24.761905, 24.761905), tolerance = 1e-07, label = paste0(simulationResults$accrualIntensity))
    expect_equal(simulationResults$lambda1, 0.02, tolerance = 1e-07, label = paste0(simulationResults$lambda1))
    expect_equal(simulationResults$analysisTime[1, ], 10.071413, tolerance = 1e-07, label = paste0(simulationResults$analysisTime[1, ]))
    expect_equal(simulationResults$analysisTime[2, ], 31.014645, tolerance = 1e-07, label = paste0(simulationResults$analysisTime[2, ]))
    expect_equal(simulationResults$analysisTime[3, ], 78.484045, tolerance = 1e-07, label = paste0(simulationResults$analysisTime[3, ]))
    expect_equal(simulationResults$studyDuration, 75.086051, tolerance = 1e-07, label = paste0(simulationResults$studyDuration))
    expect_equal(simulationResults$eventsNotAchieved[1, ], 0, label = paste0(simulationResults$eventsNotAchieved[1, ]))
    expect_equal(simulationResults$eventsNotAchieved[2, ], 0, label = paste0(simulationResults$eventsNotAchieved[2, ]))
    expect_equal(simulationResults$eventsNotAchieved[3, ], 0, label = paste0(simulationResults$eventsNotAchieved[3, ]))
    expect_equal(simulationResults$numberOfSubjects[1, ], 211.81, tolerance = 1e-07, label = paste0(simulationResults$numberOfSubjects[1, ]))
    expect_equal(simulationResults$numberOfSubjects[2, ], 259.98, tolerance = 1e-07, label = paste0(simulationResults$numberOfSubjects[2, ]))
    expect_equal(simulationResults$numberOfSubjects[3, ], 260, label = paste0(simulationResults$numberOfSubjects[3, ]))
    expect_equal(simulationResults$cumulativeEventsPerStage[1, ], 20, label = paste0(simulationResults$cumulativeEventsPerStage[1, ]))
    expect_equal(simulationResults$cumulativeEventsPerStage[2, ], 107.53, tolerance = 1e-07, label = paste0(simulationResults$cumulativeEventsPerStage[2, ]))
    expect_equal(simulationResults$cumulativeEventsPerStage[3, ], 200.90634, tolerance = 1e-07, label = paste0(simulationResults$cumulativeEventsPerStage[3, ]))
    expect_equal(simulationResults$iterations[1, ], 100, label = paste0(simulationResults$iterations[1, ]))
    expect_equal(simulationResults$iterations[2, ], 100, label = paste0(simulationResults$iterations[2, ]))
    expect_equal(simulationResults$iterations[3, ], 93, label = paste0(simulationResults$iterations[3, ]))
    expect_equal(simulationResults$overallReject, 0.26, tolerance = 1e-07, label = paste0(simulationResults$overallReject))
    expect_gte(object = simulationResults$overallReject[1], expected = 0)
    expect_lte(object = simulationResults$overallReject[1], expected = 1)
    expect_equal(simulationResults$rejectPerStage[1, ], 0, label = paste0(simulationResults$rejectPerStage[1, ]))
    expect_equal(simulationResults$rejectPerStage[2, ], 0.07, tolerance = 1e-07, label = paste0(simulationResults$rejectPerStage[2, ]))
    expect_equal(simulationResults$rejectPerStage[3, ], 0.19, tolerance = 1e-07, label = paste0(simulationResults$rejectPerStage[3, ]))
    expect_equal(simulationResults$futilityStop, 0, label = paste0(simulationResults$futilityStop))
    expect_equal(simulationResults$futilityPerStage[1, ], 0, label = paste0(simulationResults$futilityPerStage[1, ]))
    expect_equal(simulationResults$futilityPerStage[2, ], 0, label = paste0(simulationResults$futilityPerStage[2, ]))
    expect_equal(simulationResults$earlyStop, 0.07, tolerance = 1e-07, label = paste0(simulationResults$earlyStop))
    expect_equal(simulationResults$expectedNumberOfSubjects, 259.9986, tolerance = 1e-07, label = paste0(simulationResults$expectedNumberOfSubjects))
    expect_equal(simulationResults$expectedNumberOfEvents, 194.37, tolerance = 1e-07, label = paste0(simulationResults$expectedNumberOfEvents))
    expect_equal(simulationResults$conditionalPowerAchieved[1, ], NA_real_, label = paste0(simulationResults$conditionalPowerAchieved[1, ]))
    expect_equal(simulationResults$conditionalPowerAchieved[2, ], 0.26815489, tolerance = 1e-07, label = paste0(simulationResults$conditionalPowerAchieved[2, ]))
    expect_equal(simulationResults$conditionalPowerAchieved[3, ], 0.24457773, tolerance = 1e-07, label = paste0(simulationResults$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(simulationResults), NA)))
        expect_output(print(simulationResults)$show())
        invisible(capture.output(expect_error(summary(simulationResults), NA)))
        expect_output(summary(simulationResults)$show())
        simulationResultsCodeBased <- eval(parse(text = getObjectRCode(simulationResults, stringWrapParagraphWidth = NULL)))
        expect_equal(simulationResultsCodeBased$median1, simulationResults$median1, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$median2, simulationResults$median2, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$accrualIntensity, simulationResults$accrualIntensity, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$lambda1, simulationResults$lambda1, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$analysisTime, simulationResults$analysisTime, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$studyDuration, simulationResults$studyDuration, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$eventsNotAchieved, simulationResults$eventsNotAchieved, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$numberOfSubjects, simulationResults$numberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$cumulativeEventsPerStage, simulationResults$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$iterations, simulationResults$iterations, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$overallReject, simulationResults$overallReject, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$rejectPerStage, simulationResults$rejectPerStage, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$futilityStop, simulationResults$futilityStop, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$futilityPerStage, simulationResults$futilityPerStage, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$earlyStop, simulationResults$earlyStop, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$expectedNumberOfSubjects, simulationResults$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$expectedNumberOfEvents, simulationResults$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$conditionalPowerAchieved, simulationResults$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(simulationResults), "character")
        df <- as.data.frame(simulationResults)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(simulationResults)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSimulationSurvival': configuration 8", {
    .skipTestIfDisabled()

    # @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
    # @refFS[Sec.]{fs:sec:simulatingTestingOneHypothesis}
    # @refFS[Tab.]{fs:tab:output:getSimulationSurvival}
    # @refFS[Formula]{fs:simulationSurvivalTimeGenerate}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    # @refFS[Formula]{fs:pieceWiseExponentialRandomVariable}
    # @refFS[Formula]{fs:simulationSurvivalLogRank}
    # @refFS[Formula]{fs:simulationSurvivalIncrements}
    # @refFS[Formula]{fs:simulationSurvivalHazardEstimate}
    # @refFS[Formula]{fs:testStatisticGroupSequential}
    design <- getDesignGroupSequential(kMax = 3, typeOfDesign = "WT", deltaWT = 0.25)
    simulationResults <- getSimulationSurvival(
        design = design,
        directionUpper = FALSE, maxNumberOfSubjects = 200, plannedEvents = (1:design$kMax) * 20,
        allocation1 = 1, allocation2 = 1, accrualTime = c(0, 3, 6, 12),
        piecewiseSurvivalTime = c(0, 6), lambda2 = c(0.01, 0.03), hazardRatio = 0.8,
        accrualIntensity = c(0.1, 0.2, 0.2), dropoutRate1 = 0.04, dropoutRate2 = 0.08,
        dropoutTime = 12, maxNumberOfIterations = 100,
        seed = 1234567890
    )

    ## Comparison of the results of SimulationResultsSurvival object 'simulationResults' with expected results
    expect_equal(simulationResults$accrualIntensity, c(9.5238095, 19.047619, 19.047619), tolerance = 1e-07, label = paste0(simulationResults$accrualIntensity))
    expect_equal(simulationResults$lambda1, c(0.008, 0.024), tolerance = 1e-07, label = paste0(simulationResults$lambda1))
    expect_equal(simulationResults$analysisTime[1, ], 14.155697, tolerance = 1e-07, label = paste0(simulationResults$analysisTime[1, ]))
    expect_equal(simulationResults$analysisTime[2, ], 19.508242, tolerance = 1e-07, label = paste0(simulationResults$analysisTime[2, ]))
    expect_equal(simulationResults$analysisTime[3, ], 25.008056, tolerance = 1e-07, label = paste0(simulationResults$analysisTime[3, ]))
    expect_equal(simulationResults$studyDuration, 24.627971, tolerance = 1e-07, label = paste0(simulationResults$studyDuration))
    expect_equal(simulationResults$eventsNotAchieved[1, ], 0, label = paste0(simulationResults$eventsNotAchieved[1, ]))
    expect_equal(simulationResults$eventsNotAchieved[2, ], 0, label = paste0(simulationResults$eventsNotAchieved[2, ]))
    expect_equal(simulationResults$eventsNotAchieved[3, ], 0, label = paste0(simulationResults$eventsNotAchieved[3, ]))
    expect_equal(simulationResults$numberOfSubjects[1, ], 199.73, tolerance = 1e-07, label = paste0(simulationResults$numberOfSubjects[1, ]))
    expect_equal(simulationResults$numberOfSubjects[2, ], 200, label = paste0(simulationResults$numberOfSubjects[2, ]))
    expect_equal(simulationResults$numberOfSubjects[3, ], 200, label = paste0(simulationResults$numberOfSubjects[3, ]))
    expect_equal(simulationResults$cumulativeEventsPerStage[1, ], 20, label = paste0(simulationResults$cumulativeEventsPerStage[1, ]))
    expect_equal(simulationResults$cumulativeEventsPerStage[2, ], 40, label = paste0(simulationResults$cumulativeEventsPerStage[2, ]))
    expect_equal(simulationResults$cumulativeEventsPerStage[3, ], 60, label = paste0(simulationResults$cumulativeEventsPerStage[3, ]))
    expect_equal(simulationResults$iterations[1, ], 100, label = paste0(simulationResults$iterations[1, ]))
    expect_equal(simulationResults$iterations[2, ], 99, label = paste0(simulationResults$iterations[2, ]))
    expect_equal(simulationResults$iterations[3, ], 95, label = paste0(simulationResults$iterations[3, ]))
    expect_equal(simulationResults$overallReject, 0.11, tolerance = 1e-07, label = paste0(simulationResults$overallReject))
    expect_gte(object = simulationResults$overallReject[1], expected = 0)
    expect_lte(object = simulationResults$overallReject[1], expected = 1)
    expect_equal(simulationResults$rejectPerStage[1, ], 0.01, tolerance = 1e-07, label = paste0(simulationResults$rejectPerStage[1, ]))
    expect_equal(simulationResults$rejectPerStage[2, ], 0.04, tolerance = 1e-07, label = paste0(simulationResults$rejectPerStage[2, ]))
    expect_equal(simulationResults$rejectPerStage[3, ], 0.06, tolerance = 1e-07, label = paste0(simulationResults$rejectPerStage[3, ]))
    expect_equal(simulationResults$futilityStop, 0, label = paste0(simulationResults$futilityStop))
    expect_equal(simulationResults$futilityPerStage[1, ], 0, label = paste0(simulationResults$futilityPerStage[1, ]))
    expect_equal(simulationResults$futilityPerStage[2, ], 0, label = paste0(simulationResults$futilityPerStage[2, ]))
    expect_equal(simulationResults$earlyStop, 0.05, tolerance = 1e-07, label = paste0(simulationResults$earlyStop))
    expect_equal(simulationResults$expectedNumberOfSubjects, 199.9973, tolerance = 1e-07, label = paste0(simulationResults$expectedNumberOfSubjects))
    expect_equal(simulationResults$expectedNumberOfEvents, 58.8, tolerance = 1e-07, label = paste0(simulationResults$expectedNumberOfEvents))
    expect_equal(simulationResults$conditionalPowerAchieved[1, ], NA_real_, label = paste0(simulationResults$conditionalPowerAchieved[1, ]))
    expect_equal(simulationResults$conditionalPowerAchieved[2, ], 0.13387917, tolerance = 1e-07, label = paste0(simulationResults$conditionalPowerAchieved[2, ]))
    expect_equal(simulationResults$conditionalPowerAchieved[3, ], 0.12806393, tolerance = 1e-07, label = paste0(simulationResults$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(simulationResults), NA)))
        expect_output(print(simulationResults)$show())
        invisible(capture.output(expect_error(summary(simulationResults), NA)))
        expect_output(summary(simulationResults)$show())
        simulationResultsCodeBased <- eval(parse(text = getObjectRCode(simulationResults, stringWrapParagraphWidth = NULL)))
        expect_equal(simulationResultsCodeBased$accrualIntensity, simulationResults$accrualIntensity, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$lambda1, simulationResults$lambda1, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$analysisTime, simulationResults$analysisTime, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$studyDuration, simulationResults$studyDuration, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$eventsNotAchieved, simulationResults$eventsNotAchieved, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$numberOfSubjects, simulationResults$numberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$cumulativeEventsPerStage, simulationResults$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$iterations, simulationResults$iterations, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$overallReject, simulationResults$overallReject, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$rejectPerStage, simulationResults$rejectPerStage, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$futilityStop, simulationResults$futilityStop, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$futilityPerStage, simulationResults$futilityPerStage, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$earlyStop, simulationResults$earlyStop, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$expectedNumberOfSubjects, simulationResults$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$expectedNumberOfEvents, simulationResults$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$conditionalPowerAchieved, simulationResults$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(simulationResults), "character")
        df <- as.data.frame(simulationResults)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(simulationResults)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSimulationSurvival': configuration 9;", {
    .skipTestIfDisabled()

    # @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
    # @refFS[Sec.]{fs:sec:simulatingTestingOneHypothesis}
    # @refFS[Tab.]{fs:tab:output:getSimulationSurvival}
    # @refFS[Formula]{fs:simulationSurvivalTimeGenerate}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    # @refFS[Formula]{fs:pieceWiseExponentialRandomVariable}
    # @refFS[Formula]{fs:simulationSurvivalLogRank}
    # @refFS[Formula]{fs:simulationSurvivalIncrements}
    # @refFS[Formula]{fs:simulationSurvivalHazardEstimate}
    # @refFS[Formula]{fs:testStatisticGroupSequential}
    design <- getDesignGroupSequential(kMax = 3, typeOfDesign = "WT", deltaWT = 0.25)
    simulationResults <- getSimulationSurvival(
        design = design,
        directionUpper = FALSE, maxNumberOfSubjects = 260, plannedEvents = (1:design$kMax) * 20,
        allocation1 = 1, allocation2 = 1, accrualTime = c(0, 3, 6, 12),
        piecewiseSurvivalTime = c(0, 6), lambda2 = c(0.01, 0.03), hazardRatio = c(0.75),
        accrualIntensity = c(0.1, 0.2, 0.2), dropoutRate1 = 0, dropoutRate2 = 0,
        dropoutTime = 12, conditionalPower = 0.8, minNumberOfEventsPerStage = c(NA_real_, 10, 10),
        maxNumberOfEventsPerStage = c(NA_real_, 100, 100), maxNumberOfIterations = 100,
        seed = 1234567890
    )

    ## Comparison of the results of SimulationResultsSurvival object 'simulationResults' with expected results
    expect_equal(simulationResults$accrualIntensity, c(12.380952, 24.761905, 24.761905), tolerance = 1e-07, label = paste0(simulationResults$accrualIntensity))
    expect_equal(simulationResults$lambda1, c(0.0075, 0.0225), tolerance = 1e-07, label = paste0(simulationResults$lambda1))
    expect_equal(simulationResults$analysisTime[1, ], 12.905156, tolerance = 1e-07, label = paste0(simulationResults$analysisTime[1, ]))
    expect_equal(simulationResults$analysisTime[2, ], 31.363371, tolerance = 1e-07, label = paste0(simulationResults$analysisTime[2, ]))
    expect_equal(simulationResults$analysisTime[3, ], 71.176717, tolerance = 1e-07, label = paste0(simulationResults$analysisTime[3, ]))
    expect_equal(simulationResults$studyDuration, 65.836001, tolerance = 1e-07, label = paste0(simulationResults$studyDuration))
    expect_equal(simulationResults$eventsNotAchieved[1, ], 0, label = paste0(simulationResults$eventsNotAchieved[1, ]))
    expect_equal(simulationResults$eventsNotAchieved[2, ], 0, label = paste0(simulationResults$eventsNotAchieved[2, ]))
    expect_equal(simulationResults$eventsNotAchieved[3, ], 0, label = paste0(simulationResults$eventsNotAchieved[3, ]))
    expect_equal(simulationResults$numberOfSubjects[1, ], 257.27, tolerance = 1e-07, label = paste0(simulationResults$numberOfSubjects[1, ]))
    expect_equal(simulationResults$numberOfSubjects[2, ], 260, label = paste0(simulationResults$numberOfSubjects[2, ]))
    expect_equal(simulationResults$numberOfSubjects[3, ], 260, label = paste0(simulationResults$numberOfSubjects[3, ]))
    expect_equal(simulationResults$cumulativeEventsPerStage[1, ], 20, label = paste0(simulationResults$cumulativeEventsPerStage[1, ]))
    expect_equal(simulationResults$cumulativeEventsPerStage[2, ], 106.16162, tolerance = 1e-07, label = paste0(simulationResults$cumulativeEventsPerStage[2, ]))
    expect_equal(simulationResults$cumulativeEventsPerStage[3, ], 195.73633, tolerance = 1e-07, label = paste0(simulationResults$cumulativeEventsPerStage[3, ]))
    expect_equal(simulationResults$iterations[1, ], 100, label = paste0(simulationResults$iterations[1, ]))
    expect_equal(simulationResults$iterations[2, ], 99, label = paste0(simulationResults$iterations[2, ]))
    expect_equal(simulationResults$iterations[3, ], 87, label = paste0(simulationResults$iterations[3, ]))
    expect_equal(simulationResults$overallReject, 0.47, tolerance = 1e-07, label = paste0(simulationResults$overallReject))
    expect_gte(object = simulationResults$overallReject[1], expected = 0)
    expect_lte(object = simulationResults$overallReject[1], expected = 1)
    expect_equal(simulationResults$rejectPerStage[1, ], 0.01, tolerance = 1e-07, label = paste0(simulationResults$rejectPerStage[1, ]))
    expect_equal(simulationResults$rejectPerStage[2, ], 0.12, tolerance = 1e-07, label = paste0(simulationResults$rejectPerStage[2, ]))
    expect_equal(simulationResults$rejectPerStage[3, ], 0.34, tolerance = 1e-07, label = paste0(simulationResults$rejectPerStage[3, ]))
    expect_equal(simulationResults$futilityStop, 0, label = paste0(simulationResults$futilityStop))
    expect_equal(simulationResults$futilityPerStage[1, ], 0, label = paste0(simulationResults$futilityPerStage[1, ]))
    expect_equal(simulationResults$futilityPerStage[2, ], 0, label = paste0(simulationResults$futilityPerStage[2, ]))
    expect_equal(simulationResults$earlyStop, 0.13, tolerance = 1e-07, label = paste0(simulationResults$earlyStop))
    expect_equal(simulationResults$expectedNumberOfSubjects, 259.9727, tolerance = 1e-07, label = paste0(simulationResults$expectedNumberOfSubjects))
    expect_equal(simulationResults$expectedNumberOfEvents, 183.23, tolerance = 1e-07, label = paste0(simulationResults$expectedNumberOfEvents))
    expect_equal(simulationResults$conditionalPowerAchieved[1, ], NA_real_, label = paste0(simulationResults$conditionalPowerAchieved[1, ]))
    expect_equal(simulationResults$conditionalPowerAchieved[2, ], 0.28641702, tolerance = 1e-07, label = paste0(simulationResults$conditionalPowerAchieved[2, ]))
    expect_equal(simulationResults$conditionalPowerAchieved[3, ], 0.33103011, tolerance = 1e-07, label = paste0(simulationResults$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(simulationResults), NA)))
        expect_output(print(simulationResults)$show())
        invisible(capture.output(expect_error(summary(simulationResults), NA)))
        expect_output(summary(simulationResults)$show())
        simulationResultsCodeBased <- eval(parse(text = getObjectRCode(simulationResults, stringWrapParagraphWidth = NULL)))
        expect_equal(simulationResultsCodeBased$accrualIntensity, simulationResults$accrualIntensity, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$lambda1, simulationResults$lambda1, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$analysisTime, simulationResults$analysisTime, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$studyDuration, simulationResults$studyDuration, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$eventsNotAchieved, simulationResults$eventsNotAchieved, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$numberOfSubjects, simulationResults$numberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$cumulativeEventsPerStage, simulationResults$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$iterations, simulationResults$iterations, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$overallReject, simulationResults$overallReject, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$rejectPerStage, simulationResults$rejectPerStage, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$futilityStop, simulationResults$futilityStop, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$futilityPerStage, simulationResults$futilityPerStage, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$earlyStop, simulationResults$earlyStop, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$expectedNumberOfSubjects, simulationResults$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$expectedNumberOfEvents, simulationResults$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$conditionalPowerAchieved, simulationResults$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(simulationResults), "character")
        df <- as.data.frame(simulationResults)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(simulationResults)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSimulationSurvival': configuration 10;", {
    .skipTestIfDisabled()

    # @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
    # @refFS[Sec.]{fs:sec:simulatingTestingOneHypothesis}
    # @refFS[Tab.]{fs:tab:output:getSimulationSurvival}
    # @refFS[Formula]{fs:simulationSurvivalTimeGenerate}
    # @refFS[Formula]{fs:pieceWiseExponentialRandomVariable}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    # @refFS[Formula]{fs:simulationSurvivalLogRank}
    # @refFS[Formula]{fs:simulationSurvivalIncrements}
    # @refFS[Formula]{fs:simulationSurvivalHazardEstimate}
    # @refFS[Formula]{fs:testStatisticGroupSequential}
    design <- getDesignGroupSequential(kMax = 3, typeOfDesign = "WT", deltaWT = 0.25)
    simulationResults <- getSimulationSurvival(
        design = design,
        directionUpper = FALSE, maxNumberOfSubjects = 260, plannedEvents = (1:design$kMax) * 20,
        allocation1 = 1, allocation2 = 1, accrualTime = c(0, 3, 6, 12),
        lambda2 = 0.03, hazardRatio = c(0.75, 0.8, 0.9),
        accrualIntensity = c(0.1, 0.2, 0.2), dropoutRate1 = 0, dropoutRate2 = 0,
        dropoutTime = 12, conditionalPower = 0.8, minNumberOfEventsPerStage = c(NA_real_, 10, 10),
        maxNumberOfEventsPerStage = c(NA_real_, 100, 100), maxNumberOfIterations = 100,
        seed = 1234567890
    )

    ## Comparison of the results of SimulationResultsSurvival object 'simulationResults' with expected results
    expect_equal(simulationResults$pi1, c(0.23662051, 0.25023841, 0.27674976), tolerance = 1e-07, label = paste0(simulationResults$pi1))
    expect_equal(simulationResults$pi2, 0.30232367, tolerance = 1e-07, label = paste0(simulationResults$pi2))
    expect_equal(simulationResults$median1, c(30.806541, 28.881133, 25.672118), tolerance = 1e-07, label = paste0(simulationResults$median1))
    expect_equal(simulationResults$median2, 23.104906, tolerance = 1e-07, label = paste0(simulationResults$median2))
    expect_equal(simulationResults$accrualIntensity, c(12.380952, 24.761905, 24.761905), tolerance = 1e-07, label = paste0(simulationResults$accrualIntensity))
    expect_equal(simulationResults$lambda1, c(0.0225, 0.024, 0.027), tolerance = 1e-07, label = paste0(simulationResults$lambda1))
    expect_equal(simulationResults$analysisTime[1, ], c(9.4112305, 9.2753297, 9.1968922), tolerance = 1e-07, label = paste0(simulationResults$analysisTime[1, ]))
    expect_equal(simulationResults$analysisTime[2, ], c(27.054738, 27.519552, 26.652741), tolerance = 1e-07, label = paste0(simulationResults$analysisTime[2, ]))
    expect_equal(simulationResults$analysisTime[3, ], c(67.286427, 67.154864, 68.163763), tolerance = 1e-07, label = paste0(simulationResults$analysisTime[3, ]))
    expect_equal(simulationResults$studyDuration, c(61.651041, 62.169663, 64.720225), tolerance = 1e-07, label = paste0(simulationResults$studyDuration))
    expect_equal(simulationResults$eventsNotAchieved[1, ], c(0, 0, 0), label = paste0(simulationResults$eventsNotAchieved[1, ]))
    expect_equal(simulationResults$eventsNotAchieved[2, ], c(0, 0, 0), label = paste0(simulationResults$eventsNotAchieved[2, ]))
    expect_equal(simulationResults$eventsNotAchieved[3, ], c(0, 0, 0), label = paste0(simulationResults$eventsNotAchieved[3, ]))
    expect_equal(simulationResults$numberOfSubjects[1, ], c(195.58, 192.19, 190.21), tolerance = 1e-07, label = paste0(simulationResults$numberOfSubjects[1, ]))
    expect_equal(simulationResults$numberOfSubjects[2, ], c(258.86, 259.77778, 259.64646), tolerance = 1e-07, label = paste0(simulationResults$numberOfSubjects[2, ]))
    expect_equal(simulationResults$numberOfSubjects[3, ], c(260, 260, 260), label = paste0(simulationResults$numberOfSubjects[3, ]))
    expect_equal(simulationResults$cumulativeEventsPerStage[1, ], c(20, 20, 20), label = paste0(simulationResults$cumulativeEventsPerStage[1, ]))
    expect_equal(simulationResults$cumulativeEventsPerStage[2, ], c(105.16, 109.35354, 112.36364), tolerance = 1e-07, label = paste0(simulationResults$cumulativeEventsPerStage[2, ]))
    expect_equal(simulationResults$cumulativeEventsPerStage[3, ], c(197.16, 199.53535, 210.98729), tolerance = 1e-07, label = paste0(simulationResults$cumulativeEventsPerStage[3, ]))
    expect_equal(simulationResults$iterations[1, ], c(100, 100, 100), label = paste0(simulationResults$iterations[1, ]))
    expect_equal(simulationResults$iterations[2, ], c(100, 99, 99), label = paste0(simulationResults$iterations[2, ]))
    expect_equal(simulationResults$iterations[3, ], c(86, 88, 93), label = paste0(simulationResults$iterations[3, ]))
    expect_equal(simulationResults$overallReject, c(0.46, 0.36, 0.13), tolerance = 1e-07, label = paste0(simulationResults$overallReject))
    expect_gte(object = simulationResults$overallReject[1], expected = 0)
    expect_gte(object = simulationResults$overallReject[2], expected = 0)
    expect_gte(object = simulationResults$overallReject[3], expected = 0)
    expect_lte(object = simulationResults$overallReject[1], expected = 1)
    expect_lte(object = simulationResults$overallReject[2], expected = 1)
    expect_lte(object = simulationResults$overallReject[3], expected = 1)
    expect_equal(simulationResults$rejectPerStage[1, ], c(0, 0.01, 0.01), tolerance = 1e-07, label = paste0(simulationResults$rejectPerStage[1, ]))
    expect_equal(simulationResults$rejectPerStage[2, ], c(0.14, 0.11, 0.06), tolerance = 1e-07, label = paste0(simulationResults$rejectPerStage[2, ]))
    expect_equal(simulationResults$rejectPerStage[3, ], c(0.32, 0.24, 0.06), tolerance = 1e-07, label = paste0(simulationResults$rejectPerStage[3, ]))
    expect_equal(simulationResults$futilityStop, c(0, 0, 0), label = paste0(simulationResults$futilityStop))
    expect_equal(simulationResults$futilityPerStage[1, ], c(0, 0, 0), label = paste0(simulationResults$futilityPerStage[1, ]))
    expect_equal(simulationResults$futilityPerStage[2, ], c(0, 0, 0), label = paste0(simulationResults$futilityPerStage[2, ]))
    expect_equal(simulationResults$earlyStop, c(0.14, 0.12, 0.07), tolerance = 1e-07, label = paste0(simulationResults$earlyStop))
    expect_equal(simulationResults$expectedNumberOfSubjects, c(259.8404, 259.29746, 259.28089), tolerance = 1e-07, label = paste0(simulationResults$expectedNumberOfSubjects))
    expect_equal(simulationResults$expectedNumberOfEvents, c(184.28, 187.82, 203.16), tolerance = 1e-07, label = paste0(simulationResults$expectedNumberOfEvents))
    expect_equal(simulationResults$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_), label = paste0(simulationResults$conditionalPowerAchieved[1, ]))
    expect_equal(simulationResults$conditionalPowerAchieved[2, ], c(0.30728214, 0.23928832, 0.1863817), tolerance = 1e-07, label = paste0(simulationResults$conditionalPowerAchieved[2, ]))
    expect_equal(simulationResults$conditionalPowerAchieved[3, ], c(0.33344952, 0.28614054, 0.14302818), tolerance = 1e-07, label = paste0(simulationResults$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(simulationResults), NA)))
        expect_output(print(simulationResults)$show())
        invisible(capture.output(expect_error(summary(simulationResults), NA)))
        expect_output(summary(simulationResults)$show())
        simulationResultsCodeBased <- eval(parse(text = getObjectRCode(simulationResults, stringWrapParagraphWidth = NULL)))
        expect_equal(simulationResultsCodeBased$pi1, simulationResults$pi1, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$pi2, simulationResults$pi2, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$median1, simulationResults$median1, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$median2, simulationResults$median2, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$accrualIntensity, simulationResults$accrualIntensity, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$lambda1, simulationResults$lambda1, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$analysisTime, simulationResults$analysisTime, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$studyDuration, simulationResults$studyDuration, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$eventsNotAchieved, simulationResults$eventsNotAchieved, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$numberOfSubjects, simulationResults$numberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$cumulativeEventsPerStage, simulationResults$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$iterations, simulationResults$iterations, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$overallReject, simulationResults$overallReject, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$rejectPerStage, simulationResults$rejectPerStage, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$futilityStop, simulationResults$futilityStop, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$futilityPerStage, simulationResults$futilityPerStage, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$earlyStop, simulationResults$earlyStop, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$expectedNumberOfSubjects, simulationResults$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$expectedNumberOfEvents, simulationResults$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$conditionalPowerAchieved, simulationResults$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(simulationResults), "character")
        df <- as.data.frame(simulationResults)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(simulationResults)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSimulationSurvival': configuration 11;", {
    .skipTestIfDisabled()

    # @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
    # @refFS[Sec.]{fs:sec:simulatingTestingOneHypothesis}
    # @refFS[Tab.]{fs:tab:output:getSimulationSurvival}
    # @refFS[Formula]{fs:simulationSurvivalTimeGenerate}
    # @refFS[Formula]{fs:simulationSurvivalLogRank}
    # @refFS[Formula]{fs:simulationSurvivalIncrements}
    # @refFS[Formula]{fs:testStatisticNormalCombinationTest}
    # @refFS[Formula]{fs:testStatisticGroupSequential}
    design <- getDesignInverseNormal(informationRates = c(0.4, 0.7, 1))
    myCalcEventsFunction <- function(...,
            stage, conditionalPower, estimatedTheta,
            plannedEvents, eventsOverStages,
            minNumberOfEventsPerStage, maxNumberOfEventsPerStage,
            conditionalCriticalValue) {
        theta <- max(1 + 1e-12, estimatedTheta)
        if (stage == 2) {
            requiredStageEvents <- max(0, conditionalCriticalValue + qnorm(conditionalPower))^2 / log(theta)^2
            requiredStageEvents <- min(
                max(minNumberOfEventsPerStage[stage], requiredStageEvents),
                maxNumberOfEventsPerStage[stage]
            ) + eventsOverStages[stage - 1]
        } else {
            requiredStageEvents <- 2 * eventsOverStages[stage - 1] - eventsOverStages[1]
        }
        return(requiredStageEvents)
    }
    simulationResults <- getSimulationSurvival(
        design = design,
        hazardRatio = seq(1, 2.6, 0.5),
        pi2 = 0.3,
        conditionalPower = 0.8,
        plannedEvents = c(58, 102, 146),
        minNumberOfEventsPerStage = c(NA, 44, 4),
        maxNumberOfEventsPerStage = 4 * c(NA, 44, 4),
        maxNumberOfSubjects = 800,
        calcEventsFunction = myCalcEventsFunction,
        seed = 1234567890,
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsSurvival object 'simulationResults' with expected results
    expect_equal(simulationResults$pi1, c(0.3, 0.41433798, 0.51, 0.59003659), tolerance = 1e-07, label = paste0(simulationResults$pi1))
    expect_equal(simulationResults$median1, c(23.320299, 15.546866, 11.660149, 9.3281194), tolerance = 1e-07, label = paste0(simulationResults$median1))
    expect_equal(simulationResults$median2, 23.320299, tolerance = 1e-07, label = paste0(simulationResults$median2))
    expect_equal(simulationResults$accrualIntensity, 66.666667, tolerance = 1e-07, label = paste0(simulationResults$accrualIntensity))
    expect_equal(simulationResults$lambda1, c(0.029722912, 0.044584368, 0.059445824, 0.07430728), tolerance = 1e-07, label = paste0(simulationResults$lambda1))
    expect_equal(simulationResults$lambda2, 0.029722912, tolerance = 1e-07, label = paste0(simulationResults$lambda2))
    expect_equal(simulationResults$analysisTime[1, ], c(7.9897023, 7.1475519, 6.494373, 6.2294549), tolerance = 1e-07, label = paste0(simulationResults$analysisTime[1, ]))
    expect_equal(simulationResults$analysisTime[2, ], c(17.213741, 10.852607, 9.0020285, 8.4492782), tolerance = 1e-07, label = paste0(simulationResults$analysisTime[2, ]))
    expect_equal(simulationResults$analysisTime[3, ], c(28.255907, 20.342199, NA_real_, NA_real_), tolerance = 1e-07, label = paste0(simulationResults$analysisTime[3, ]))
    expect_equal(simulationResults$studyDuration, c(28.255907, 12.725375, 7.9469034, 7.3207992), tolerance = 1e-07, label = paste0(simulationResults$studyDuration))
    expect_equal(simulationResults$eventsNotAchieved[1, ], c(0, 0, 0, 0), label = paste0(simulationResults$eventsNotAchieved[1, ]))
    expect_equal(simulationResults$eventsNotAchieved[2, ], c(0, 0, 0, 0), label = paste0(simulationResults$eventsNotAchieved[2, ]))
    expect_equal(simulationResults$eventsNotAchieved[3, ], c(0, 0, 0, 0), label = paste0(simulationResults$eventsNotAchieved[3, ]))
    expect_equal(simulationResults$numberOfSubjects[1, ], c(532.2, 476.1, 432.4, 414.9), tolerance = 1e-07, label = paste0(simulationResults$numberOfSubjects[1, ]))
    expect_equal(simulationResults$numberOfSubjects[2, ], c(788.2, 681.33333, 599.66667, 562.8), tolerance = 1e-07, label = paste0(simulationResults$numberOfSubjects[2, ]))
    expect_equal(simulationResults$numberOfSubjects[3, ], c(800, 778.66667, NA_real_, NA_real_), tolerance = 1e-07, label = paste0(simulationResults$numberOfSubjects[3, ]))
    expect_equal(simulationResults$cumulativeEventsPerStage[1, ], c(58, 58, 58, 58), label = paste0(simulationResults$cumulativeEventsPerStage[1, ]))
    expect_equal(simulationResults$cumulativeEventsPerStage[2, ], c(220.8, 132.88889, 104, 102), tolerance = 1e-07, label = paste0(simulationResults$cumulativeEventsPerStage[2, ]))
    expect_equal(simulationResults$cumulativeEventsPerStage[3, ], c(383.6, 264.88889, 104, 102), tolerance = 1e-07, label = paste0(simulationResults$cumulativeEventsPerStage[3, ]))
    expect_equal(simulationResults$iterations[1, ], c(10, 10, 10, 10), label = paste0(simulationResults$iterations[1, ]))
    expect_equal(simulationResults$iterations[2, ], c(10, 9, 6, 5), label = paste0(simulationResults$iterations[2, ]))
    expect_equal(simulationResults$iterations[3, ], c(10, 3, 0, 0), label = paste0(simulationResults$iterations[3, ]))
    expect_equal(simulationResults$overallReject, c(0.1, 1, 1, 1), tolerance = 1e-07, label = paste0(simulationResults$overallReject))
    expect_gte(object = simulationResults$overallReject[1], expected = 0)
    expect_gte(object = simulationResults$overallReject[2], expected = 0)
    expect_gte(object = simulationResults$overallReject[3], expected = 0)
    expect_gte(object = simulationResults$overallReject[4], expected = 0)
    expect_lte(object = simulationResults$overallReject[1], expected = 1)
    expect_lte(object = simulationResults$overallReject[2], expected = 1)
    expect_lte(object = simulationResults$overallReject[3], expected = 1)
    expect_lte(object = simulationResults$overallReject[4], expected = 1)
    expect_equal(simulationResults$rejectPerStage[1, ], c(0, 0.1, 0.4, 0.5), tolerance = 1e-07, label = paste0(simulationResults$rejectPerStage[1, ]))
    expect_equal(simulationResults$rejectPerStage[2, ], c(0, 0.6, 0.6, 0.5), tolerance = 1e-07, label = paste0(simulationResults$rejectPerStage[2, ]))
    expect_equal(simulationResults$rejectPerStage[3, ], c(0.1, 0.3, 0, 0), tolerance = 1e-07, label = paste0(simulationResults$rejectPerStage[3, ]))
    expect_equal(simulationResults$futilityStop, c(0, 0, 0, 0), label = paste0(simulationResults$futilityStop))
    expect_equal(simulationResults$futilityPerStage[1, ], c(0, 0, 0, 0), label = paste0(simulationResults$futilityPerStage[1, ]))
    expect_equal(simulationResults$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(simulationResults$futilityPerStage[2, ]))
    expect_equal(simulationResults$earlyStop, c(0, 0.7, 1, 1), tolerance = 1e-07, label = paste0(simulationResults$earlyStop))
    expect_equal(simulationResults$expectedNumberOfSubjects, c(800, 690.01, 532.76, 488.85), tolerance = 1e-07, label = paste0(simulationResults$expectedNumberOfSubjects))
    expect_equal(simulationResults$expectedNumberOfEvents, c(383.6, 165, 85.6, 80), tolerance = 1e-07, label = paste0(simulationResults$expectedNumberOfEvents))
    expect_equal(simulationResults$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(simulationResults$conditionalPowerAchieved[1, ]))
    expect_equal(simulationResults$conditionalPowerAchieved[2, ], c(0.044313374, 0.60865148, 0.72866566, 0.82381582), tolerance = 1e-07, label = paste0(simulationResults$conditionalPowerAchieved[2, ]))
    expect_equal(simulationResults$conditionalPowerAchieved[3, ], c(0.013892246, 0.84869906, NA_real_, NA_real_), tolerance = 1e-07, label = paste0(simulationResults$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(simulationResults), NA)))
        expect_output(print(simulationResults)$show())
        invisible(capture.output(expect_error(summary(simulationResults), NA)))
        expect_output(summary(simulationResults)$show())
        simulationResultsCodeBased <- eval(parse(text = getObjectRCode(simulationResults, stringWrapParagraphWidth = NULL)))
        expect_equal(simulationResultsCodeBased$pi1, simulationResults$pi1, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$median1, simulationResults$median1, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$median2, simulationResults$median2, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$accrualIntensity, simulationResults$accrualIntensity, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$lambda1, simulationResults$lambda1, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$lambda2, simulationResults$lambda2, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$analysisTime, simulationResults$analysisTime, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$studyDuration, simulationResults$studyDuration, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$eventsNotAchieved, simulationResults$eventsNotAchieved, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$numberOfSubjects, simulationResults$numberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$cumulativeEventsPerStage, simulationResults$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$iterations, simulationResults$iterations, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$overallReject, simulationResults$overallReject, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$rejectPerStage, simulationResults$rejectPerStage, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$futilityStop, simulationResults$futilityStop, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$futilityPerStage, simulationResults$futilityPerStage, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$earlyStop, simulationResults$earlyStop, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$expectedNumberOfSubjects, simulationResults$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$expectedNumberOfEvents, simulationResults$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(simulationResultsCodeBased$conditionalPowerAchieved, simulationResults$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(simulationResults), "character")
        df <- as.data.frame(simulationResults)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(simulationResults)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSimulationSurvival': test accrual time and intensity definition", {
    .skipTestIfDisabled()

    maxNumberOfSubjects <- getSimulationSurvival(
        plannedEvents = 100,
        accrualTime = c(0, 6, 12), accrualIntensity = c(22, 33),
        maxNumberOfIterations = 100
    )$maxNumberOfSubjects
    expect_equal(maxNumberOfSubjects, 330)

    accrualIntensity <- getSimulationSurvival(
        plannedEvents = 100,
        accrualTime = c(0, 6, 12), accrualIntensity = c(0.2, 0.3),
        maxNumberOfSubjects = 330, maxNumberOfIterations = 100,
        seed = 1234567890
    )$accrualIntensity
    expect_equal(accrualIntensity, c(22, 33))
})

test_that("'getSimulationSurvival': test expected warnings and errors", {
    # @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
    # @refFS[Sec.]{fs:sec:simulatingTestingOneHypothesis}
    # @refFS[Tab.]{fs:tab:output:getSimulationSurvival}
    # @refFS[Formula]{fs:simulationSurvivalTimeGenerate}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    # @refFS[Formula]{fs:simulationSurvivalLogRank}
    # @refFS[Formula]{fs:simulationSurvivalIncrements}
    # @refFS[Formula]{fs:simulationSurvivalHazardEstimate}
    # @refFS[Formula]{fs:testStatisticNormalCombinationTest}
    .skipTestIfDisabled()

    dIN <- getDesignInverseNormal(informationRates = c(0.4, 0.7, 1))

    expect_warning(
        getSimulationSurvival(
            design = dIN, hazardRatio = seq(1, 1.6, 0.1),
            pi2 = 0.3, plannedEvents = c(58, 102, 146),
            minNumberOfEventsPerStage = c(NA_real_, 44, 44),
            maxNumberOfSubjects = 800, maxNumberOfIterations = 100, seed = 1234567890
        ),
        "'minNumberOfEventsPerStage' (NA, 44, 44) will be ignored because 'conditionalPower' is not defined",
        fixed = TRUE
    )

    expect_warning(
        getSimulationSurvival(
            design = dIN, hazardRatio = seq(1, 1.6, 0.1),
            pi2 = 0.3, plannedEvents = c(58, 102, 146),
            maxNumberOfEventsPerStage = 4 * c(NA_real_, 44, 44),
            maxNumberOfSubjects = 800, maxNumberOfIterations = 100, seed = 1234567890
        ),
        "'maxNumberOfEventsPerStage' (NA, 176, 176) will be ignored because 'conditionalPower' is not defined",
        fixed = TRUE
    )

    expect_error(
        getSimulationSurvival(
            design = dIN, hazardRatio = seq(1, 1.6, 0.1),
            pi2 = 0.3, conditionalPower = 0.8, plannedEvents = c(58, 102, 146),
            maxNumberOfSubjects = 800, maxNumberOfIterations = 100, seed = 1234567890
        ),
        "Missing argument: 'minNumberOfEventsPerStage' must be defined because 'conditionalPower' is defined",
        fixed = TRUE
    )

    expect_error(
        getSimulationSurvival(
            design = dIN, hazardRatio = seq(1, 1.6, 0.1),
            pi2 = 0.3, conditionalPower = 0.8, plannedEvents = c(58, 102, 146),
            minNumberOfEventsPerStage = c(NA_real_, 44, 44),
            maxNumberOfSubjects = 800, maxNumberOfIterations = 100, seed = 1234567890
        ),
        "Missing argument: 'maxNumberOfEventsPerStage' must be defined because 'conditionalPower' is defined",
        fixed = TRUE
    )

    expect_error(
        getSimulationSurvival(
            design = dIN, hazardRatio = seq(1, 1.6, 0.1),
            pi2 = 0.3, conditionalPower = -0.1, plannedEvents = c(58, 102, 146),
            minNumberOfEventsPerStage = c(NA_real_, 44, 44),
            maxNumberOfEventsPerStage = 4 * c(NA_real_, 44, 44),
            maxNumberOfSubjects = 800, maxNumberOfIterations = 100, seed = 1234567890
        ),
        "Argument out of bounds: 'conditionalPower' (-0.1) is out of bounds (0; 1)",
        fixed = TRUE
    )

    expect_error(
        getSimulationSurvival(
            design = dIN, hazardRatio = seq(1, 1.6, 0.1),
            pi2 = 0.3, conditionalPower = 1.1, plannedEvents = c(58, 102, 146),
            minNumberOfEventsPerStage = c(NA_real_, 44, 44),
            maxNumberOfEventsPerStage = 4 * c(NA_real_, 44, 44),
            maxNumberOfSubjects = 800, maxNumberOfIterations = 100, seed = 1234567890
        ),
        "Argument out of bounds: 'conditionalPower' (1.1) is out of bounds (0; 1)",
        fixed = TRUE
    )

    expect_error(
        getSimulationSurvival(
            plannedEvents = -100,
            accrualTime = c(0, 6, 12), accrualIntensity = c(22, 33),
            maxNumberOfIterations = 100, seed = 1234567890
        ),
        "Argument out of bounds: 'plannedEvents' (-100) must be >= 1",
        fixed = TRUE
    )

    expect_error(
        getSimulationSurvival(
            design = dIN, plannedEvents = c(100, 100, 150),
            accrualTime = c(0, 6, 12), accrualIntensity = c(22, 33),
            maxNumberOfIterations = 100, seed = 1234567890
        ),
        "Illegal argument: 'plannedEvents' (100, 100, 150) must be strictly increasing: x_1 < .. < x_3",
        fixed = TRUE
    )

    expect_error(
        getSimulationSurvival(
            design = dIN, hazardRatio = seq(1, 1.6, 0.1),
            pi2 = 0.3, conditionalPower = 0.8, plannedEvents = c(58, 102, 146),
            minNumberOfEventsPerStage = c(NA_real_, 44, -44),
            maxNumberOfEventsPerStage = 4 * c(NA_real_, 44, 44),
            maxNumberOfSubjects = 800, maxNumberOfIterations = 100, seed = 1234567890
        ),
        "Argument out of bounds: each value of 'minNumberOfEventsPerStage' (58, 44, -44) must be >= 1",
        fixed = TRUE
    )

    expect_error(
        getSimulationSurvival(
            design = dIN, hazardRatio = seq(1, 1.6, 0.1),
            pi2 = 0.3, conditionalPower = 0.8, plannedEvents = c(58, 102, 146),
            minNumberOfEventsPerStage = c(NA_real_, 44, 44),
            maxNumberOfEventsPerStage = 4 * c(NA_real_, 10, 44),
            maxNumberOfSubjects = 800, maxNumberOfIterations = 100, seed = 1234567890
        ),
        "Illegal argument: 'maxNumberOfEventsPerStage' (58, 40, 176) must be not smaller than minNumberOfEventsPerStage' (58, 44, 44)",
        fixed = TRUE
    )

    expect_error(getSimulationSurvival(plannedEvents = 100, maxNumberOfIterations = 100, seed = 1234567890),
        "Illegal argument: 'maxNumberOfSubjects' must be defined",
        fixed = TRUE
    )

    expect_error(
        getSimulationSurvival(
            plannedEvents = 100, accrualTime = c(0, 12),
            accrualIntensity = 20, thetaH1 = 0, maxNumberOfIterations = 100, seed = 1234567890
        ),
        "Argument out of bounds: 'thetaH1' (0) must be > 0",
        fixed = TRUE
    )

    expect_error(
        getSimulationSurvival(
            plannedEvents = 100, accrualTime = c(0, 12),
            accrualIntensity = 20, conditionalPower = 0, maxNumberOfIterations = 100, seed = 1234567890
        ),
        "Argument out of bounds: 'conditionalPower' (0) is out of bounds (0; 1)",
        fixed = TRUE
    )

    expect_error(
        getSimulationSurvival(
            plannedEvents = 100, accrualTime = c(0, 12),
            accrualIntensity = 20, conditionalPower = 1, maxNumberOfIterations = 100, seed = 1234567890
        ),
        "Argument out of bounds: 'conditionalPower' (1) is out of bounds (0; 1)",
        fixed = TRUE
    )

    expect_error(
        getSimulationSurvival(
            plannedEvents = 100, accrualTime = c(0, 12),
            accrualIntensity = 20, conditionalPower = c(0.5, 0.8),
            maxNumberOfIterations = 100, seed = 1234567890
        ),
        "Illegal argument: 'conditionalPower' c(0.5, 0.8) must be a single numeric value",
        fixed = TRUE
    )
})

test_plan_section("Testing the Simulation of Survival Data for Different Parameter Variants")


test_that("'getSimulationSurvival': Fixed sample size with minimum required definitions, pi1 = c(0.4, 0.5, 0.6) and pi2 = 0.2 at event time 12, accrual time 12 and follow-up time 6 as default", {
    .skipTestIfDisabled()

    # @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
    # @refFS[Sec.]{fs:sec:simulatingTestingOneHypothesis}
    # @refFS[Tab.]{fs:tab:output:getSimulationSurvival}
    # @refFS[Formula]{fs:simulationSurvivalTimeGenerate}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    # @refFS[Formula]{fs:simulationSurvivalLogRank}
    # @refFS[Formula]{fs:simulationSurvivalIncrements}
    # @refFS[Formula]{fs:simulationSurvivalHazardEstimate}
    simulationResult <- getSimulationSurvival(
        plannedEvents = 40, maxNumberOfSubjects = 200,
        maxNumberOfIterations = 100, seed = 1234567890
    )

    ## Comparison of the results of SimulationResultsSurvival object 'simulationResult' with expected results
    expect_equal(simulationResult$median1, c(37.275405, 23.320299, 16.282985, 12), tolerance = 1e-07, label = paste0(simulationResult$median1))
    expect_equal(simulationResult$median2, 37.275405, tolerance = 1e-07, label = paste0(simulationResult$median2))
    expect_equal(simulationResult$accrualIntensity, 16.666667, tolerance = 1e-07, label = paste0(simulationResult$accrualIntensity))
    expect_equal(simulationResult$lambda1, c(0.018595296, 0.029722912, 0.042568802, 0.057762265), tolerance = 1e-07, label = paste0(simulationResult$lambda1))
    expect_equal(simulationResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0(simulationResult$lambda2))
    expect_equal(simulationResult$hazardRatio, c(1, 1.5984103, 2.2892242, 3.1062837), tolerance = 1e-07, label = paste0(simulationResult$hazardRatio))
    expect_equal(simulationResult$analysisTime[1, ], c(17.941133, 15.499503, 13.535749, 12.34), tolerance = 1e-07, label = paste0(simulationResult$analysisTime[1, ]))
    expect_equal(simulationResult$studyDuration, c(17.941133, 15.499503, 13.535749, 12.34), tolerance = 1e-07, label = paste0(simulationResult$studyDuration))
    expect_equal(simulationResult$eventsNotAchieved[1, ], c(0, 0, 0, 0), label = paste0(simulationResult$eventsNotAchieved[1, ]))
    expect_equal(simulationResult$cumulativeEventsPerStage[1, ], c(40, 40, 40, 40), label = paste0(simulationResult$cumulativeEventsPerStage[1, ]))
    expect_equal(simulationResult$iterations[1, ], c(100, 100, 100, 100), label = paste0(simulationResult$iterations[1, ]))
    expect_equal(simulationResult$overallReject, c(0.01, 0.3, 0.68, 0.95), tolerance = 1e-07, label = paste0(simulationResult$overallReject))
    expect_equal(simulationResult$rejectPerStage[1, ], c(0.01, 0.3, 0.68, 0.95), tolerance = 1e-07, label = paste0(simulationResult$rejectPerStage[1, ]))
    expect_equal(simulationResult$earlyStop, c(0, 0, 0, 0), label = paste0(simulationResult$earlyStop))
    expect_equal(simulationResult$expectedNumberOfSubjects, c(200, 200, 199.71, 196.74), tolerance = 1e-07, label = paste0(simulationResult$expectedNumberOfSubjects))
    expect_equal(simulationResult$expectedNumberOfEvents, c(40, 40, 40, 40), label = paste0(simulationResult$expectedNumberOfEvents))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(simulationResult), NA)))
        expect_output(print(simulationResult)$show())
        invisible(capture.output(expect_error(summary(simulationResult), NA)))
        expect_output(summary(simulationResult)$show())
        simulationResultCodeBased <- eval(parse(text = getObjectRCode(simulationResult, stringWrapParagraphWidth = NULL)))
        expect_equal(simulationResultCodeBased$median1, simulationResult$median1, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$median2, simulationResult$median2, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$accrualIntensity, simulationResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$lambda1, simulationResult$lambda1, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$lambda2, simulationResult$lambda2, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$hazardRatio, simulationResult$hazardRatio, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$analysisTime, simulationResult$analysisTime, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$studyDuration, simulationResult$studyDuration, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$eventsNotAchieved, simulationResult$eventsNotAchieved, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$cumulativeEventsPerStage, simulationResult$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$iterations, simulationResult$iterations, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$overallReject, simulationResult$overallReject, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$rejectPerStage, simulationResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$futilityPerStage, simulationResult$futilityPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$earlyStop, simulationResult$earlyStop, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$expectedNumberOfSubjects, simulationResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$expectedNumberOfEvents, simulationResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_type(names(simulationResult), "character")
        df <- as.data.frame(simulationResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(simulationResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSimulationSurvival': Determine necessary accrual time if 200 subjects and 30 subjects per time unit can be recruited", {
    .skipTestIfDisabled()

    simulationResult <- getSimulationSurvival(
        plannedEvents = 40, accrualTime = 0,
        accrualIntensity = 30, maxNumberOfSubjects = 200,
        maxNumberOfIterations = 100, seed = 1234567890
    )

    ## Comparison of the results of SimulationResultsSurvival object 'simulationResult' with expected results
    expect_equal(simulationResult$median1, c(37.275405, 23.320299, 16.282985, 12), tolerance = 1e-07, label = paste0(simulationResult$median1))
    expect_equal(simulationResult$median2, 37.275405, tolerance = 1e-07, label = paste0(simulationResult$median2))
    expect_equal(simulationResult$lambda1, c(0.018595296, 0.029722912, 0.042568802, 0.057762265), tolerance = 1e-07, label = paste0(simulationResult$lambda1))
    expect_equal(simulationResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0(simulationResult$lambda2))
    expect_equal(simulationResult$hazardRatio, c(1, 1.5984103, 2.2892242, 3.1062837), tolerance = 1e-07, label = paste0(simulationResult$hazardRatio))
    expect_equal(simulationResult$analysisTime[1, ], c(15.255121, 12.685136, 10.656532, 9.4294312), tolerance = 1e-07, label = paste0(simulationResult$analysisTime[1, ]))
    expect_equal(simulationResult$studyDuration, c(15.255121, 12.685136, 10.656532, 9.4294312), tolerance = 1e-07, label = paste0(simulationResult$studyDuration))
    expect_equal(simulationResult$eventsNotAchieved[1, ], c(0, 0, 0, 0), label = paste0(simulationResult$eventsNotAchieved[1, ]))
    expect_equal(simulationResult$cumulativeEventsPerStage[1, ], c(40, 40, 40, 40), label = paste0(simulationResult$cumulativeEventsPerStage[1, ]))
    expect_equal(simulationResult$iterations[1, ], c(100, 100, 100, 100), label = paste0(simulationResult$iterations[1, ]))
    expect_equal(simulationResult$overallReject, c(0.02, 0.28, 0.77, 0.96), tolerance = 1e-07, label = paste0(simulationResult$overallReject))
    expect_equal(simulationResult$rejectPerStage[1, ], c(0.02, 0.28, 0.77, 0.96), tolerance = 1e-07, label = paste0(simulationResult$rejectPerStage[1, ]))
    expect_equal(simulationResult$earlyStop, c(0, 0, 0, 0), label = paste0(simulationResult$earlyStop))
    expect_equal(simulationResult$expectedNumberOfSubjects, c(200, 200, 200, 200), label = paste0(simulationResult$expectedNumberOfSubjects))
    expect_equal(simulationResult$expectedNumberOfEvents, c(40, 40, 40, 40), label = paste0(simulationResult$expectedNumberOfEvents))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(simulationResult), NA)))
        expect_output(print(simulationResult)$show())
        invisible(capture.output(expect_error(summary(simulationResult), NA)))
        expect_output(summary(simulationResult)$show())
        simulationResultCodeBased <- eval(parse(text = getObjectRCode(simulationResult, stringWrapParagraphWidth = NULL)))
        expect_equal(simulationResultCodeBased$median1, simulationResult$median1, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$median2, simulationResult$median2, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$lambda1, simulationResult$lambda1, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$lambda2, simulationResult$lambda2, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$hazardRatio, simulationResult$hazardRatio, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$analysisTime, simulationResult$analysisTime, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$studyDuration, simulationResult$studyDuration, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$eventsNotAchieved, simulationResult$eventsNotAchieved, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$cumulativeEventsPerStage, simulationResult$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$iterations, simulationResult$iterations, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$overallReject, simulationResult$overallReject, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$rejectPerStage, simulationResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$futilityPerStage, simulationResult$futilityPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$earlyStop, simulationResult$earlyStop, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$expectedNumberOfSubjects, simulationResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$expectedNumberOfEvents, simulationResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_type(names(simulationResult), "character")
        df <- as.data.frame(simulationResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(simulationResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSimulationSurvival': Determine necessary accrual time if 200 subjects and if the first 6 time units 20 subjects per time unit can be recruited, then 30 subjects per time unit", {
    .skipTestIfDisabled()

    simulationResult <- getSimulationSurvival(
        plannedEvents = 40, accrualTime = c(0, 6),
        accrualIntensity = c(20, 30), maxNumberOfSubjects = 200,
        maxNumberOfIterations = 100, seed = 1234567890
    )

    ## Comparison of the results of SimulationResultsSurvival object 'simulationResult' with expected results
    expect_equal(simulationResult$median1, c(37.275405, 23.320299, 16.282985, 12), tolerance = 1e-07, label = paste0(simulationResult$median1))
    expect_equal(simulationResult$median2, 37.275405, tolerance = 1e-07, label = paste0(simulationResult$median2))
    expect_equal(simulationResult$lambda1, c(0.018595296, 0.029722912, 0.042568802, 0.057762265), tolerance = 1e-07, label = paste0(simulationResult$lambda1))
    expect_equal(simulationResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0(simulationResult$lambda2))
    expect_equal(simulationResult$hazardRatio, c(1, 1.5984103, 2.2892242, 3.1062837), tolerance = 1e-07, label = paste0(simulationResult$hazardRatio))
    expect_equal(simulationResult$analysisTime[1, ], c(16.683961, 14.141068, 12.109744, 10.96314), tolerance = 1e-07, label = paste0(simulationResult$analysisTime[1, ]))
    expect_equal(simulationResult$studyDuration, c(16.683961, 14.141068, 12.109744, 10.96314), tolerance = 1e-07, label = paste0(simulationResult$studyDuration))
    expect_equal(simulationResult$eventsNotAchieved[1, ], c(0, 0, 0, 0), label = paste0(simulationResult$eventsNotAchieved[1, ]))
    expect_equal(simulationResult$cumulativeEventsPerStage[1, ], c(40, 40, 40, 40), label = paste0(simulationResult$cumulativeEventsPerStage[1, ]))
    expect_equal(simulationResult$iterations[1, ], c(100, 100, 100, 100), label = paste0(simulationResult$iterations[1, ]))
    expect_equal(simulationResult$overallReject, c(0.01, 0.28, 0.72, 0.96), tolerance = 1e-07, label = paste0(simulationResult$overallReject))
    expect_equal(simulationResult$rejectPerStage[1, ], c(0.01, 0.28, 0.72, 0.96), tolerance = 1e-07, label = paste0(simulationResult$rejectPerStage[1, ]))
    expect_equal(simulationResult$earlyStop, c(0, 0, 0, 0), label = paste0(simulationResult$earlyStop))
    expect_equal(simulationResult$expectedNumberOfSubjects, c(200, 200, 200, 200), label = paste0(simulationResult$expectedNumberOfSubjects))
    expect_equal(simulationResult$expectedNumberOfEvents, c(40, 40, 40, 40), label = paste0(simulationResult$expectedNumberOfEvents))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(simulationResult), NA)))
        expect_output(print(simulationResult)$show())
        invisible(capture.output(expect_error(summary(simulationResult), NA)))
        expect_output(summary(simulationResult)$show())
        simulationResultCodeBased <- eval(parse(text = getObjectRCode(simulationResult, stringWrapParagraphWidth = NULL)))
        expect_equal(simulationResultCodeBased$median1, simulationResult$median1, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$median2, simulationResult$median2, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$lambda1, simulationResult$lambda1, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$lambda2, simulationResult$lambda2, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$hazardRatio, simulationResult$hazardRatio, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$analysisTime, simulationResult$analysisTime, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$studyDuration, simulationResult$studyDuration, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$eventsNotAchieved, simulationResult$eventsNotAchieved, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$cumulativeEventsPerStage, simulationResult$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$iterations, simulationResult$iterations, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$overallReject, simulationResult$overallReject, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$rejectPerStage, simulationResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$futilityPerStage, simulationResult$futilityPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$earlyStop, simulationResult$earlyStop, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$expectedNumberOfSubjects, simulationResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$expectedNumberOfEvents, simulationResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_type(names(simulationResult), "character")
        df <- as.data.frame(simulationResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(simulationResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSimulationSurvival': Determine maximum number of Subjects if the first 6 time units 20 subjects per time unit can be recruited, and after 10 time units 30 subjects per time unit", {
    .skipTestIfDisabled()

    simulationResult <- getSimulationSurvival(
        plannedEvents = 40, accrualTime = c(0, 6, 10),
        accrualIntensity = c(20, 30), maxNumberOfIterations = 100, seed = 1234567890
    )

    ## Comparison of the results of SimulationResultsSurvival object 'simulationResult' with expected results
    expect_equal(simulationResult$median1, c(37.275405, 23.320299, 16.282985, 12), tolerance = 1e-07, label = paste0(simulationResult$median1))
    expect_equal(simulationResult$median2, 37.275405, tolerance = 1e-07, label = paste0(simulationResult$median2))
    expect_equal(simulationResult$maxNumberOfSubjects, 240, label = paste0(simulationResult$maxNumberOfSubjects))
    expect_equal(simulationResult$lambda1, c(0.018595296, 0.029722912, 0.042568802, 0.057762265), tolerance = 1e-07, label = paste0(simulationResult$lambda1))
    expect_equal(simulationResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0(simulationResult$lambda2))
    expect_equal(simulationResult$hazardRatio, c(1, 1.5984103, 2.2892242, 3.1062837), tolerance = 1e-07, label = paste0(simulationResult$hazardRatio))
    expect_equal(simulationResult$analysisTime[1, ], c(15.210978, 13.172199, 11.59631, 10.698373), tolerance = 1e-07, label = paste0(simulationResult$analysisTime[1, ]))
    expect_equal(simulationResult$studyDuration, c(15.210978, 13.172199, 11.59631, 10.698373), tolerance = 1e-07, label = paste0(simulationResult$studyDuration))
    expect_equal(simulationResult$eventsNotAchieved[1, ], c(0, 0, 0, 0), label = paste0(simulationResult$eventsNotAchieved[1, ]))
    expect_equal(simulationResult$cumulativeEventsPerStage[1, ], c(40, 40, 40, 40), label = paste0(simulationResult$cumulativeEventsPerStage[1, ]))
    expect_equal(simulationResult$iterations[1, ], c(100, 100, 100, 100), label = paste0(simulationResult$iterations[1, ]))
    expect_equal(simulationResult$overallReject, c(0.04, 0.33, 0.75, 0.95), tolerance = 1e-07, label = paste0(simulationResult$overallReject))
    expect_equal(simulationResult$rejectPerStage[1, ], c(0.04, 0.33, 0.75, 0.95), tolerance = 1e-07, label = paste0(simulationResult$rejectPerStage[1, ]))
    expect_equal(simulationResult$earlyStop, c(0, 0, 0, 0), label = paste0(simulationResult$earlyStop))
    expect_equal(simulationResult$expectedNumberOfSubjects, c(240, 240, 239.63, 237.56), tolerance = 1e-07, label = paste0(simulationResult$expectedNumberOfSubjects))
    expect_equal(simulationResult$expectedNumberOfEvents, c(40, 40, 40, 40), label = paste0(simulationResult$expectedNumberOfEvents))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(simulationResult), NA)))
        expect_output(print(simulationResult)$show())
        invisible(capture.output(expect_error(summary(simulationResult), NA)))
        expect_output(summary(simulationResult)$show())
        simulationResultCodeBased <- eval(parse(text = getObjectRCode(simulationResult, stringWrapParagraphWidth = NULL)))
        expect_equal(simulationResultCodeBased$median1, simulationResult$median1, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$median2, simulationResult$median2, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$maxNumberOfSubjects, simulationResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$lambda1, simulationResult$lambda1, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$lambda2, simulationResult$lambda2, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$hazardRatio, simulationResult$hazardRatio, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$analysisTime, simulationResult$analysisTime, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$studyDuration, simulationResult$studyDuration, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$eventsNotAchieved, simulationResult$eventsNotAchieved, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$cumulativeEventsPerStage, simulationResult$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$iterations, simulationResult$iterations, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$overallReject, simulationResult$overallReject, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$rejectPerStage, simulationResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$futilityPerStage, simulationResult$futilityPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$earlyStop, simulationResult$earlyStop, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$expectedNumberOfSubjects, simulationResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$expectedNumberOfEvents, simulationResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_type(names(simulationResult), "character")
        df <- as.data.frame(simulationResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(simulationResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSimulationSurvival': Specify accrual time as a list", {
    .skipTestIfDisabled()

    at <- list("0 - <6" = 20, "6 - Inf" = 30)
    simulationResult <- getSimulationSurvival(
        plannedEvents = 40, accrualTime = at,
        maxNumberOfSubjects = 200, maxNumberOfIterations = 100, seed = 1234567890
    )

    ## Comparison of the results of SimulationResultsSurvival object 'simulationResult' with expected results
    expect_equal(simulationResult$median1, c(37.275405, 23.320299, 16.282985, 12), tolerance = 1e-07, label = paste0(simulationResult$median1))
    expect_equal(simulationResult$median2, 37.275405, tolerance = 1e-07, label = paste0(simulationResult$median2))
    expect_equal(simulationResult$lambda1, c(0.018595296, 0.029722912, 0.042568802, 0.057762265), tolerance = 1e-07, label = paste0(simulationResult$lambda1))
    expect_equal(simulationResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0(simulationResult$lambda2))
    expect_equal(simulationResult$hazardRatio, c(1, 1.5984103, 2.2892242, 3.1062837), tolerance = 1e-07, label = paste0(simulationResult$hazardRatio))
    expect_equal(simulationResult$analysisTime[1, ], c(16.683961, 14.141068, 12.109744, 10.96314), tolerance = 1e-07, label = paste0(simulationResult$analysisTime[1, ]))
    expect_equal(simulationResult$studyDuration, c(16.683961, 14.141068, 12.109744, 10.96314), tolerance = 1e-07, label = paste0(simulationResult$studyDuration))
    expect_equal(simulationResult$eventsNotAchieved[1, ], c(0, 0, 0, 0), label = paste0(simulationResult$eventsNotAchieved[1, ]))
    expect_equal(simulationResult$cumulativeEventsPerStage[1, ], c(40, 40, 40, 40), label = paste0(simulationResult$cumulativeEventsPerStage[1, ]))
    expect_equal(simulationResult$iterations[1, ], c(100, 100, 100, 100), label = paste0(simulationResult$iterations[1, ]))
    expect_equal(simulationResult$overallReject, c(0.01, 0.28, 0.72, 0.96), tolerance = 1e-07, label = paste0(simulationResult$overallReject))
    expect_equal(simulationResult$rejectPerStage[1, ], c(0.01, 0.28, 0.72, 0.96), tolerance = 1e-07, label = paste0(simulationResult$rejectPerStage[1, ]))
    expect_equal(simulationResult$earlyStop, c(0, 0, 0, 0), label = paste0(simulationResult$earlyStop))
    expect_equal(simulationResult$expectedNumberOfSubjects, c(200, 200, 200, 200), label = paste0(simulationResult$expectedNumberOfSubjects))
    expect_equal(simulationResult$expectedNumberOfEvents, c(40, 40, 40, 40), label = paste0(simulationResult$expectedNumberOfEvents))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(simulationResult), NA)))
        expect_output(print(simulationResult)$show())
        invisible(capture.output(expect_error(summary(simulationResult), NA)))
        expect_output(summary(simulationResult)$show())
        simulationResultCodeBased <- eval(parse(text = getObjectRCode(simulationResult, stringWrapParagraphWidth = NULL)))
        expect_equal(simulationResultCodeBased$median1, simulationResult$median1, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$median2, simulationResult$median2, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$lambda1, simulationResult$lambda1, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$lambda2, simulationResult$lambda2, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$hazardRatio, simulationResult$hazardRatio, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$analysisTime, simulationResult$analysisTime, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$studyDuration, simulationResult$studyDuration, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$eventsNotAchieved, simulationResult$eventsNotAchieved, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$cumulativeEventsPerStage, simulationResult$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$iterations, simulationResult$iterations, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$overallReject, simulationResult$overallReject, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$rejectPerStage, simulationResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$futilityPerStage, simulationResult$futilityPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$earlyStop, simulationResult$earlyStop, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$expectedNumberOfSubjects, simulationResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$expectedNumberOfEvents, simulationResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_type(names(simulationResult), "character")
        df <- as.data.frame(simulationResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(simulationResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSimulationSurvival': Specify accrual time as a list, if maximum number of subjects need to be calculated", {
    .skipTestIfDisabled()

    at <- list("0 - <6" = 20, "6 - <=10" = 30)
    simulationResult <- getSimulationSurvival(
        plannedEvents = 40, accrualTime = at,
        maxNumberOfIterations = 100, seed = 1234567890
    )

    ## Comparison of the results of SimulationResultsSurvival object 'simulationResult' with expected results
    expect_equal(simulationResult$median1, c(37.275405, 23.320299, 16.282985, 12), tolerance = 1e-07, label = paste0(simulationResult$median1))
    expect_equal(simulationResult$median2, 37.275405, tolerance = 1e-07, label = paste0(simulationResult$median2))
    expect_equal(simulationResult$maxNumberOfSubjects, 240, label = paste0(simulationResult$maxNumberOfSubjects))
    expect_equal(simulationResult$lambda1, c(0.018595296, 0.029722912, 0.042568802, 0.057762265), tolerance = 1e-07, label = paste0(simulationResult$lambda1))
    expect_equal(simulationResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0(simulationResult$lambda2))
    expect_equal(simulationResult$hazardRatio, c(1, 1.5984103, 2.2892242, 3.1062837), tolerance = 1e-07, label = paste0(simulationResult$hazardRatio))
    expect_equal(simulationResult$analysisTime[1, ], c(15.210978, 13.172199, 11.59631, 10.698373), tolerance = 1e-07, label = paste0(simulationResult$analysisTime[1, ]))
    expect_equal(simulationResult$studyDuration, c(15.210978, 13.172199, 11.59631, 10.698373), tolerance = 1e-07, label = paste0(simulationResult$studyDuration))
    expect_equal(simulationResult$eventsNotAchieved[1, ], c(0, 0, 0, 0), label = paste0(simulationResult$eventsNotAchieved[1, ]))
    expect_equal(simulationResult$cumulativeEventsPerStage[1, ], c(40, 40, 40, 40), label = paste0(simulationResult$cumulativeEventsPerStage[1, ]))
    expect_equal(simulationResult$iterations[1, ], c(100, 100, 100, 100), label = paste0(simulationResult$iterations[1, ]))
    expect_equal(simulationResult$overallReject, c(0.04, 0.33, 0.75, 0.95), tolerance = 1e-07, label = paste0(simulationResult$overallReject))
    expect_equal(simulationResult$rejectPerStage[1, ], c(0.04, 0.33, 0.75, 0.95), tolerance = 1e-07, label = paste0(simulationResult$rejectPerStage[1, ]))
    expect_equal(simulationResult$earlyStop, c(0, 0, 0, 0), label = paste0(simulationResult$earlyStop))
    expect_equal(simulationResult$expectedNumberOfSubjects, c(240, 240, 239.63, 237.56), tolerance = 1e-07, label = paste0(simulationResult$expectedNumberOfSubjects))
    expect_equal(simulationResult$expectedNumberOfEvents, c(40, 40, 40, 40), label = paste0(simulationResult$expectedNumberOfEvents))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(simulationResult), NA)))
        expect_output(print(simulationResult)$show())
        invisible(capture.output(expect_error(summary(simulationResult), NA)))
        expect_output(summary(simulationResult)$show())
        simulationResultCodeBased <- eval(parse(text = getObjectRCode(simulationResult, stringWrapParagraphWidth = NULL)))
        expect_equal(simulationResultCodeBased$median1, simulationResult$median1, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$median2, simulationResult$median2, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$maxNumberOfSubjects, simulationResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$lambda1, simulationResult$lambda1, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$lambda2, simulationResult$lambda2, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$hazardRatio, simulationResult$hazardRatio, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$analysisTime, simulationResult$analysisTime, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$studyDuration, simulationResult$studyDuration, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$eventsNotAchieved, simulationResult$eventsNotAchieved, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$cumulativeEventsPerStage, simulationResult$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$iterations, simulationResult$iterations, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$overallReject, simulationResult$overallReject, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$rejectPerStage, simulationResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$futilityPerStage, simulationResult$futilityPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$earlyStop, simulationResult$earlyStop, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$expectedNumberOfSubjects, simulationResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$expectedNumberOfEvents, simulationResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_type(names(simulationResult), "character")
        df <- as.data.frame(simulationResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(simulationResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSimulationSurvival': Specify effect size for a two-stage group design with O'Brien & Fleming boundaries Effect size is based on event rates at specified event time, directionUpper = FALSE needs to be specified because it should be shown that hazard ratio < 1", {
    .skipTestIfDisabled()

    simulationResult <- getSimulationSurvival(
        design = getDesignGroupSequential(kMax = 2),
        pi1 = 0.2, pi2 = 0.3, eventTime = 24, plannedEvents = c(20, 40),
        maxNumberOfSubjects = 200, directionUpper = FALSE, maxNumberOfIterations = 100, seed = 1234567890
    )

    ## Comparison of the results of SimulationResultsSurvival object 'simulationResult' with expected results
    expect_equal(simulationResult$median1, 74.550809, tolerance = 1e-07, label = paste0(simulationResult$median1))
    expect_equal(simulationResult$median2, 46.640597, tolerance = 1e-07, label = paste0(simulationResult$median2))
    expect_equal(simulationResult$accrualIntensity, 16.666667, tolerance = 1e-07, label = paste0(simulationResult$accrualIntensity))
    expect_equal(simulationResult$lambda1, 0.009297648, tolerance = 1e-07, label = paste0(simulationResult$lambda1))
    expect_equal(simulationResult$lambda2, 0.014861456, tolerance = 1e-07, label = paste0(simulationResult$lambda2))
    expect_equal(simulationResult$hazardRatio, 0.62562161, tolerance = 1e-07, label = paste0(simulationResult$hazardRatio))
    expect_equal(simulationResult$analysisTime[1, ], 14.769473, tolerance = 1e-07, label = paste0(simulationResult$analysisTime[1, ]))
    expect_equal(simulationResult$analysisTime[2, ], 24.499634, tolerance = 1e-07, label = paste0(simulationResult$analysisTime[2, ]))
    expect_equal(simulationResult$studyDuration, 24.198958, tolerance = 1e-07, label = paste0(simulationResult$studyDuration))
    expect_equal(simulationResult$eventsNotAchieved[1, ], 0, label = paste0(simulationResult$eventsNotAchieved[1, ]))
    expect_equal(simulationResult$eventsNotAchieved[2, ], 0, label = paste0(simulationResult$eventsNotAchieved[2, ]))
    expect_equal(simulationResult$numberOfSubjects[1, ], 199.47, tolerance = 1e-07, label = paste0(simulationResult$numberOfSubjects[1, ]))
    expect_equal(simulationResult$numberOfSubjects[2, ], 200, label = paste0(simulationResult$numberOfSubjects[2, ]))
    expect_equal(simulationResult$cumulativeEventsPerStage[1, ], 20, label = paste0(simulationResult$cumulativeEventsPerStage[1, ]))
    expect_equal(simulationResult$cumulativeEventsPerStage[2, ], 40, label = paste0(simulationResult$cumulativeEventsPerStage[2, ]))
    expect_equal(simulationResult$iterations[1, ], 100, label = paste0(simulationResult$iterations[1, ]))
    expect_equal(simulationResult$iterations[2, ], 97, label = paste0(simulationResult$iterations[2, ]))
    expect_equal(simulationResult$overallReject, 0.27, tolerance = 1e-07, label = paste0(simulationResult$overallReject))
    expect_equal(simulationResult$rejectPerStage[1, ], 0.03, tolerance = 1e-07, label = paste0(simulationResult$rejectPerStage[1, ]))
    expect_equal(simulationResult$rejectPerStage[2, ], 0.24, tolerance = 1e-07, label = paste0(simulationResult$rejectPerStage[2, ]))
    expect_equal(simulationResult$futilityPerStage[1, ], 0, label = paste0(simulationResult$futilityPerStage[1, ]))
    expect_equal(simulationResult$earlyStop, 0.03, tolerance = 1e-07, label = paste0(simulationResult$earlyStop))
    expect_equal(simulationResult$expectedNumberOfSubjects, 199.9841, tolerance = 1e-07, label = paste0(simulationResult$expectedNumberOfSubjects))
    expect_equal(simulationResult$expectedNumberOfEvents, 39.4, tolerance = 1e-07, label = paste0(simulationResult$expectedNumberOfEvents))
    expect_equal(simulationResult$conditionalPowerAchieved[1, ], NA_real_, label = paste0(simulationResult$conditionalPowerAchieved[1, ]))
    expect_equal(simulationResult$conditionalPowerAchieved[2, ], 0.29516222, tolerance = 1e-07, label = paste0(simulationResult$conditionalPowerAchieved[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(simulationResult), NA)))
        expect_output(print(simulationResult)$show())
        invisible(capture.output(expect_error(summary(simulationResult), NA)))
        expect_output(summary(simulationResult)$show())
        simulationResultCodeBased <- eval(parse(text = getObjectRCode(simulationResult, stringWrapParagraphWidth = NULL)))
        expect_equal(simulationResultCodeBased$median1, simulationResult$median1, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$median2, simulationResult$median2, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$accrualIntensity, simulationResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$lambda1, simulationResult$lambda1, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$lambda2, simulationResult$lambda2, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$hazardRatio, simulationResult$hazardRatio, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$analysisTime, simulationResult$analysisTime, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$studyDuration, simulationResult$studyDuration, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$eventsNotAchieved, simulationResult$eventsNotAchieved, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$numberOfSubjects, simulationResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$cumulativeEventsPerStage, simulationResult$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$iterations, simulationResult$iterations, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$overallReject, simulationResult$overallReject, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$rejectPerStage, simulationResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$futilityPerStage, simulationResult$futilityPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$earlyStop, simulationResult$earlyStop, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$expectedNumberOfSubjects, simulationResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$expectedNumberOfEvents, simulationResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$conditionalPowerAchieved, simulationResult$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(simulationResult), "character")
        df <- as.data.frame(simulationResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(simulationResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSimulationSurvival': As above, but with a three-stage O'Brien and Fleming design with specified information rates, note that planned events consists of integer values", {
    .skipTestIfDisabled()

    d3 <- getDesignGroupSequential(informationRates = c(0.4, 0.7, 1))
    simulationResult <- getSimulationSurvival(
        design = d3, pi1 = 0.2, pi2 = 0.3, eventTime = 24,
        plannedEvents = round(d3$informationRates * 40),
        maxNumberOfSubjects = 200, directionUpper = FALSE, seed = 1234567890
    )

    ## Comparison of the results of SimulationResultsSurvival object 'simulationResult' with expected results
    expect_equal(simulationResult$median1, 74.550809, tolerance = 1e-07, label = paste0(simulationResult$median1))
    expect_equal(simulationResult$median2, 46.640597, tolerance = 1e-07, label = paste0(simulationResult$median2))
    expect_equal(simulationResult$accrualIntensity, 16.666667, tolerance = 1e-07, label = paste0(simulationResult$accrualIntensity))
    expect_equal(simulationResult$lambda1, 0.009297648, tolerance = 1e-07, label = paste0(simulationResult$lambda1))
    expect_equal(simulationResult$lambda2, 0.014861456, tolerance = 1e-07, label = paste0(simulationResult$lambda2))
    expect_equal(simulationResult$hazardRatio, 0.62562161, tolerance = 1e-07, label = paste0(simulationResult$hazardRatio))
    expect_equal(simulationResult$analysisTime[1, ], 13.073331, tolerance = 1e-07, label = paste0(simulationResult$analysisTime[1, ]))
    expect_equal(simulationResult$analysisTime[2, ], 18.748105, tolerance = 1e-07, label = paste0(simulationResult$analysisTime[2, ]))
    expect_equal(simulationResult$analysisTime[3, ], 24.810251, tolerance = 1e-07, label = paste0(simulationResult$analysisTime[3, ]))
    expect_equal(simulationResult$studyDuration, 23.877826, tolerance = 1e-07, label = paste0(simulationResult$studyDuration))
    expect_equal(simulationResult$eventsNotAchieved[1, ], 0, label = paste0(simulationResult$eventsNotAchieved[1, ]))
    expect_equal(simulationResult$eventsNotAchieved[2, ], 0, label = paste0(simulationResult$eventsNotAchieved[2, ]))
    expect_equal(simulationResult$eventsNotAchieved[3, ], 0, label = paste0(simulationResult$eventsNotAchieved[3, ]))
    expect_equal(simulationResult$numberOfSubjects[1, ], 195.313, tolerance = 1e-07, label = paste0(simulationResult$numberOfSubjects[1, ]))
    expect_equal(simulationResult$numberOfSubjects[2, ], 200, label = paste0(simulationResult$numberOfSubjects[2, ]))
    expect_equal(simulationResult$numberOfSubjects[3, ], 200, label = paste0(simulationResult$numberOfSubjects[3, ]))
    expect_equal(simulationResult$cumulativeEventsPerStage[1, ], 16, label = paste0(simulationResult$cumulativeEventsPerStage[1, ]))
    expect_equal(simulationResult$cumulativeEventsPerStage[2, ], 28, label = paste0(simulationResult$cumulativeEventsPerStage[2, ]))
    expect_equal(simulationResult$cumulativeEventsPerStage[3, ], 40, label = paste0(simulationResult$cumulativeEventsPerStage[3, ]))
    expect_equal(simulationResult$iterations[1, ], 1000, label = paste0(simulationResult$iterations[1, ]))
    expect_equal(simulationResult$iterations[2, ], 985, label = paste0(simulationResult$iterations[2, ]))
    expect_equal(simulationResult$iterations[3, ], 861, label = paste0(simulationResult$iterations[3, ]))
    expect_equal(simulationResult$overallReject, 0.322, tolerance = 1e-07, label = paste0(simulationResult$overallReject))
    expect_equal(simulationResult$rejectPerStage[1, ], 0.015, tolerance = 1e-07, label = paste0(simulationResult$rejectPerStage[1, ]))
    expect_equal(simulationResult$rejectPerStage[2, ], 0.124, tolerance = 1e-07, label = paste0(simulationResult$rejectPerStage[2, ]))
    expect_equal(simulationResult$rejectPerStage[3, ], 0.183, tolerance = 1e-07, label = paste0(simulationResult$rejectPerStage[3, ]))
    expect_equal(simulationResult$futilityStop, 0, label = paste0(simulationResult$futilityStop))
    expect_equal(simulationResult$futilityPerStage[1, ], 0, label = paste0(simulationResult$futilityPerStage[1, ]))
    expect_equal(simulationResult$futilityPerStage[2, ], 0, label = paste0(simulationResult$futilityPerStage[2, ]))
    expect_equal(simulationResult$earlyStop, 0.139, tolerance = 1e-07, label = paste0(simulationResult$earlyStop))
    expect_equal(simulationResult$expectedNumberOfSubjects, 199.92969, tolerance = 1e-07, label = paste0(simulationResult$expectedNumberOfSubjects))
    expect_equal(simulationResult$expectedNumberOfEvents, 38.152, tolerance = 1e-07, label = paste0(simulationResult$expectedNumberOfEvents))
    expect_equal(simulationResult$conditionalPowerAchieved[1, ], NA_real_, label = paste0(simulationResult$conditionalPowerAchieved[1, ]))
    expect_equal(simulationResult$conditionalPowerAchieved[2, ], 0.19637573, tolerance = 1e-07, label = paste0(simulationResult$conditionalPowerAchieved[2, ]))
    expect_equal(simulationResult$conditionalPowerAchieved[3, ], 0.23542216, tolerance = 1e-07, label = paste0(simulationResult$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(simulationResult), NA)))
        expect_output(print(simulationResult)$show())
        invisible(capture.output(expect_error(summary(simulationResult), NA)))
        expect_output(summary(simulationResult)$show())
        simulationResultCodeBased <- eval(parse(text = getObjectRCode(simulationResult, stringWrapParagraphWidth = NULL)))
        expect_equal(simulationResultCodeBased$median1, simulationResult$median1, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$median2, simulationResult$median2, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$accrualIntensity, simulationResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$lambda1, simulationResult$lambda1, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$lambda2, simulationResult$lambda2, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$hazardRatio, simulationResult$hazardRatio, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$analysisTime, simulationResult$analysisTime, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$studyDuration, simulationResult$studyDuration, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$eventsNotAchieved, simulationResult$eventsNotAchieved, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$numberOfSubjects, simulationResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$cumulativeEventsPerStage, simulationResult$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$iterations, simulationResult$iterations, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$overallReject, simulationResult$overallReject, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$rejectPerStage, simulationResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$futilityStop, simulationResult$futilityStop, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$futilityPerStage, simulationResult$futilityPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$earlyStop, simulationResult$earlyStop, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$expectedNumberOfSubjects, simulationResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$expectedNumberOfEvents, simulationResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$conditionalPowerAchieved, simulationResult$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(simulationResult), "character")
        df <- as.data.frame(simulationResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(simulationResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSimulationSurvival': Effect size is based on event rate at specified event time for the reference group and hazard ratio, directionUpper = FALSE needs to be specified because it should be shown that hazard ratio < 1", {
    .skipTestIfDisabled()

    simulationResult <- getSimulationSurvival(
        design = getDesignGroupSequential(kMax = 2), hazardRatio = 0.5,
        pi2 = 0.3, eventTime = 24, plannedEvents = c(20, 40), maxNumberOfSubjects = 200,
        directionUpper = FALSE, maxNumberOfIterations = 100, seed = 1234567890
    )

    ## Comparison of the results of SimulationResultsSurvival object 'simulationResult' with expected results
    expect_equal(simulationResult$pi1, 0.16333997, tolerance = 1e-07, label = paste0(simulationResult$pi1))
    expect_equal(simulationResult$median1, 93.281194, tolerance = 1e-07, label = paste0(simulationResult$median1))
    expect_equal(simulationResult$median2, 46.640597, tolerance = 1e-07, label = paste0(simulationResult$median2))
    expect_equal(simulationResult$accrualIntensity, 16.666667, tolerance = 1e-07, label = paste0(simulationResult$accrualIntensity))
    expect_equal(simulationResult$lambda1, 0.007430728, tolerance = 1e-07, label = paste0(simulationResult$lambda1))
    expect_equal(simulationResult$lambda2, 0.014861456, tolerance = 1e-07, label = paste0(simulationResult$lambda2))
    expect_equal(simulationResult$analysisTime[1, ], 15.596955, tolerance = 1e-07, label = paste0(simulationResult$analysisTime[1, ]))
    expect_equal(simulationResult$analysisTime[2, ], 26.310745, tolerance = 1e-07, label = paste0(simulationResult$analysisTime[2, ]))
    expect_equal(simulationResult$studyDuration, 25.440402, tolerance = 1e-07, label = paste0(simulationResult$studyDuration))
    expect_equal(simulationResult$eventsNotAchieved[1, ], 0, label = paste0(simulationResult$eventsNotAchieved[1, ]))
    expect_equal(simulationResult$eventsNotAchieved[2, ], 0, label = paste0(simulationResult$eventsNotAchieved[2, ]))
    expect_equal(simulationResult$numberOfSubjects[1, ], 199.69, tolerance = 1e-07, label = paste0(simulationResult$numberOfSubjects[1, ]))
    expect_equal(simulationResult$numberOfSubjects[2, ], 200, label = paste0(simulationResult$numberOfSubjects[2, ]))
    expect_equal(simulationResult$cumulativeEventsPerStage[1, ], 20, label = paste0(simulationResult$cumulativeEventsPerStage[1, ]))
    expect_equal(simulationResult$cumulativeEventsPerStage[2, ], 40, label = paste0(simulationResult$cumulativeEventsPerStage[2, ]))
    expect_equal(simulationResult$iterations[1, ], 100, label = paste0(simulationResult$iterations[1, ]))
    expect_equal(simulationResult$iterations[2, ], 92, label = paste0(simulationResult$iterations[2, ]))
    expect_equal(simulationResult$overallReject, 0.52, tolerance = 1e-07, label = paste0(simulationResult$overallReject))
    expect_equal(simulationResult$rejectPerStage[1, ], 0.08, tolerance = 1e-07, label = paste0(simulationResult$rejectPerStage[1, ]))
    expect_equal(simulationResult$rejectPerStage[2, ], 0.44, tolerance = 1e-07, label = paste0(simulationResult$rejectPerStage[2, ]))
    expect_equal(simulationResult$futilityPerStage[1, ], 0, label = paste0(simulationResult$futilityPerStage[1, ]))
    expect_equal(simulationResult$earlyStop, 0.08, tolerance = 1e-07, label = paste0(simulationResult$earlyStop))
    expect_equal(simulationResult$expectedNumberOfSubjects, 199.9752, tolerance = 1e-07, label = paste0(simulationResult$expectedNumberOfSubjects))
    expect_equal(simulationResult$expectedNumberOfEvents, 38.4, tolerance = 1e-07, label = paste0(simulationResult$expectedNumberOfEvents))
    expect_equal(simulationResult$conditionalPowerAchieved[1, ], NA_real_, label = paste0(simulationResult$conditionalPowerAchieved[1, ]))
    expect_equal(simulationResult$conditionalPowerAchieved[2, ], 0.43087375, tolerance = 1e-07, label = paste0(simulationResult$conditionalPowerAchieved[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(simulationResult), NA)))
        expect_output(print(simulationResult)$show())
        invisible(capture.output(expect_error(summary(simulationResult), NA)))
        expect_output(summary(simulationResult)$show())
        simulationResultCodeBased <- eval(parse(text = getObjectRCode(simulationResult, stringWrapParagraphWidth = NULL)))
        expect_equal(simulationResultCodeBased$pi1, simulationResult$pi1, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$median1, simulationResult$median1, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$median2, simulationResult$median2, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$accrualIntensity, simulationResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$lambda1, simulationResult$lambda1, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$lambda2, simulationResult$lambda2, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$analysisTime, simulationResult$analysisTime, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$studyDuration, simulationResult$studyDuration, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$eventsNotAchieved, simulationResult$eventsNotAchieved, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$numberOfSubjects, simulationResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$cumulativeEventsPerStage, simulationResult$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$iterations, simulationResult$iterations, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$overallReject, simulationResult$overallReject, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$rejectPerStage, simulationResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$futilityPerStage, simulationResult$futilityPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$earlyStop, simulationResult$earlyStop, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$expectedNumberOfSubjects, simulationResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$expectedNumberOfEvents, simulationResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$conditionalPowerAchieved, simulationResult$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(simulationResult), "character")
        df <- as.data.frame(simulationResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(simulationResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSimulationSurvival': Effect size is based on hazard rate for the reference group and hazard ratio, directionUpper = FALSE needs to be specified because it should be shown that hazard ratio < 1", {
    .skipTestIfDisabled()

    simulationResult <- getSimulationSurvival(
        design = getDesignGroupSequential(kMax = 2), hazardRatio = 0.5,
        lambda2 = 0.02, plannedEvents = c(20, 40), maxNumberOfSubjects = 200,
        directionUpper = FALSE, maxNumberOfIterations = 100, seed = 1234567890
    )

    ## Comparison of the results of SimulationResultsSurvival object 'simulationResult' with expected results
    expect_equal(simulationResult$median1, 69.314718, tolerance = 1e-07, label = paste0(simulationResult$median1))
    expect_equal(simulationResult$median2, 34.657359, tolerance = 1e-07, label = paste0(simulationResult$median2))
    expect_equal(simulationResult$accrualIntensity, 16.666667, tolerance = 1e-07, label = paste0(simulationResult$accrualIntensity))
    expect_equal(simulationResult$lambda1, 0.01, tolerance = 1e-07, label = paste0(simulationResult$lambda1))
    expect_equal(simulationResult$analysisTime[1, ], 13.132525, tolerance = 1e-07, label = paste0(simulationResult$analysisTime[1, ]))
    expect_equal(simulationResult$analysisTime[2, ], 21.186744, tolerance = 1e-07, label = paste0(simulationResult$analysisTime[2, ]))
    expect_equal(simulationResult$studyDuration, 20.690944, tolerance = 1e-07, label = paste0(simulationResult$studyDuration))
    expect_equal(simulationResult$eventsNotAchieved[1, ], 0, label = paste0(simulationResult$eventsNotAchieved[1, ]))
    expect_equal(simulationResult$eventsNotAchieved[2, ], 0, label = paste0(simulationResult$eventsNotAchieved[2, ]))
    expect_equal(simulationResult$numberOfSubjects[1, ], 195.5, tolerance = 1e-07, label = paste0(simulationResult$numberOfSubjects[1, ]))
    expect_equal(simulationResult$numberOfSubjects[2, ], 200, label = paste0(simulationResult$numberOfSubjects[2, ]))
    expect_equal(simulationResult$cumulativeEventsPerStage[1, ], 20, label = paste0(simulationResult$cumulativeEventsPerStage[1, ]))
    expect_equal(simulationResult$cumulativeEventsPerStage[2, ], 40, label = paste0(simulationResult$cumulativeEventsPerStage[2, ]))
    expect_equal(simulationResult$iterations[1, ], 100, label = paste0(simulationResult$iterations[1, ]))
    expect_equal(simulationResult$iterations[2, ], 94, label = paste0(simulationResult$iterations[2, ]))
    expect_equal(simulationResult$overallReject, 0.49, tolerance = 1e-07, label = paste0(simulationResult$overallReject))
    expect_equal(simulationResult$rejectPerStage[1, ], 0.06, tolerance = 1e-07, label = paste0(simulationResult$rejectPerStage[1, ]))
    expect_equal(simulationResult$rejectPerStage[2, ], 0.43, tolerance = 1e-07, label = paste0(simulationResult$rejectPerStage[2, ]))
    expect_equal(simulationResult$futilityPerStage[1, ], 0, label = paste0(simulationResult$futilityPerStage[1, ]))
    expect_equal(simulationResult$earlyStop, 0.06, tolerance = 1e-07, label = paste0(simulationResult$earlyStop))
    expect_equal(simulationResult$expectedNumberOfSubjects, 199.73, tolerance = 1e-07, label = paste0(simulationResult$expectedNumberOfSubjects))
    expect_equal(simulationResult$expectedNumberOfEvents, 38.8, tolerance = 1e-07, label = paste0(simulationResult$expectedNumberOfEvents))
    expect_equal(simulationResult$conditionalPowerAchieved[1, ], NA_real_, label = paste0(simulationResult$conditionalPowerAchieved[1, ]))
    expect_equal(simulationResult$conditionalPowerAchieved[2, ], 0.48014443, tolerance = 1e-07, label = paste0(simulationResult$conditionalPowerAchieved[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(simulationResult), NA)))
        expect_output(print(simulationResult)$show())
        invisible(capture.output(expect_error(summary(simulationResult), NA)))
        expect_output(summary(simulationResult)$show())
        simulationResultCodeBased <- eval(parse(text = getObjectRCode(simulationResult, stringWrapParagraphWidth = NULL)))
        expect_equal(simulationResultCodeBased$median1, simulationResult$median1, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$median2, simulationResult$median2, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$accrualIntensity, simulationResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$lambda1, simulationResult$lambda1, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$analysisTime, simulationResult$analysisTime, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$studyDuration, simulationResult$studyDuration, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$eventsNotAchieved, simulationResult$eventsNotAchieved, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$numberOfSubjects, simulationResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$cumulativeEventsPerStage, simulationResult$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$iterations, simulationResult$iterations, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$overallReject, simulationResult$overallReject, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$rejectPerStage, simulationResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$futilityPerStage, simulationResult$futilityPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$earlyStop, simulationResult$earlyStop, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$expectedNumberOfSubjects, simulationResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$expectedNumberOfEvents, simulationResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$conditionalPowerAchieved, simulationResult$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(simulationResult), "character")
        df <- as.data.frame(simulationResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(simulationResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSimulationSurvival': Specification of piecewise exponential survival time and hazard ratios, note that in getSimulationSurvival only one hazard ratio is used in the case that the survival time is piecewise exponential", {
    .skipTestIfDisabled()

    simulationResult <- getSimulationSurvival(
        design = getDesignGroupSequential(kMax = 2),
        piecewiseSurvivalTime = c(0, 5, 10), lambda2 = c(0.01, 0.02, 0.04),
        hazardRatio = 1.5, plannedEvents = c(20, 40), maxNumberOfSubjects = 200,
        maxNumberOfIterations = 100, seed = 1234567890
    )

    ## Comparison of the results of SimulationResultsSurvival object 'simulationResult' with expected results
    expect_equal(simulationResult$accrualIntensity, 16.666667, tolerance = 1e-07, label = paste0(simulationResult$accrualIntensity))
    expect_equal(simulationResult$lambda1, c(0.015, 0.03, 0.06), tolerance = 1e-07, label = paste0(simulationResult$lambda1))
    expect_equal(simulationResult$analysisTime[1, ], 12.106711, tolerance = 1e-07, label = paste0(simulationResult$analysisTime[1, ]))
    expect_equal(simulationResult$analysisTime[2, ], 16.150578, tolerance = 1e-07, label = paste0(simulationResult$analysisTime[2, ]))
    expect_equal(simulationResult$studyDuration, 16.020702, tolerance = 1e-07, label = paste0(simulationResult$studyDuration))
    expect_equal(simulationResult$eventsNotAchieved[1, ], 0, label = paste0(simulationResult$eventsNotAchieved[1, ]))
    expect_equal(simulationResult$eventsNotAchieved[2, ], 0, label = paste0(simulationResult$eventsNotAchieved[2, ]))
    expect_equal(simulationResult$numberOfSubjects[1, ], 193.51, tolerance = 1e-07, label = paste0(simulationResult$numberOfSubjects[1, ]))
    expect_equal(simulationResult$numberOfSubjects[2, ], 200, label = paste0(simulationResult$numberOfSubjects[2, ]))
    expect_equal(simulationResult$cumulativeEventsPerStage[1, ], 20, label = paste0(simulationResult$cumulativeEventsPerStage[1, ]))
    expect_equal(simulationResult$cumulativeEventsPerStage[2, ], 40, label = paste0(simulationResult$cumulativeEventsPerStage[2, ]))
    expect_equal(simulationResult$iterations[1, ], 100, label = paste0(simulationResult$iterations[1, ]))
    expect_equal(simulationResult$iterations[2, ], 96, label = paste0(simulationResult$iterations[2, ]))
    expect_equal(simulationResult$overallReject, 0.32, tolerance = 1e-07, label = paste0(simulationResult$overallReject))
    expect_equal(simulationResult$rejectPerStage[1, ], 0.04, tolerance = 1e-07, label = paste0(simulationResult$rejectPerStage[1, ]))
    expect_equal(simulationResult$rejectPerStage[2, ], 0.28, tolerance = 1e-07, label = paste0(simulationResult$rejectPerStage[2, ]))
    expect_equal(simulationResult$futilityPerStage[1, ], 0, label = paste0(simulationResult$futilityPerStage[1, ]))
    expect_equal(simulationResult$earlyStop, 0.04, tolerance = 1e-07, label = paste0(simulationResult$earlyStop))
    expect_equal(simulationResult$expectedNumberOfSubjects, 199.7404, tolerance = 1e-07, label = paste0(simulationResult$expectedNumberOfSubjects))
    expect_equal(simulationResult$expectedNumberOfEvents, 39.2, tolerance = 1e-07, label = paste0(simulationResult$expectedNumberOfEvents))
    expect_equal(simulationResult$conditionalPowerAchieved[1, ], NA_real_, label = paste0(simulationResult$conditionalPowerAchieved[1, ]))
    expect_equal(simulationResult$conditionalPowerAchieved[2, ], 0.33404702, tolerance = 1e-07, label = paste0(simulationResult$conditionalPowerAchieved[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(simulationResult), NA)))
        expect_output(print(simulationResult)$show())
        invisible(capture.output(expect_error(summary(simulationResult), NA)))
        expect_output(summary(simulationResult)$show())
        simulationResultCodeBased <- eval(parse(text = getObjectRCode(simulationResult, stringWrapParagraphWidth = NULL)))
        expect_equal(simulationResultCodeBased$accrualIntensity, simulationResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$lambda1, simulationResult$lambda1, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$analysisTime, simulationResult$analysisTime, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$studyDuration, simulationResult$studyDuration, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$eventsNotAchieved, simulationResult$eventsNotAchieved, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$numberOfSubjects, simulationResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$cumulativeEventsPerStage, simulationResult$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$iterations, simulationResult$iterations, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$overallReject, simulationResult$overallReject, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$rejectPerStage, simulationResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$futilityPerStage, simulationResult$futilityPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$earlyStop, simulationResult$earlyStop, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$expectedNumberOfSubjects, simulationResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$expectedNumberOfEvents, simulationResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$conditionalPowerAchieved, simulationResult$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(simulationResult), "character")
        df <- as.data.frame(simulationResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(simulationResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    pws <- list("0 - <5" = 0.01, "5 - <10" = 0.02, ">=10" = 0.04)
    simulationResult <- getSimulationSurvival(
        design = getDesignGroupSequential(kMax = 2),
        piecewiseSurvivalTime = pws, hazardRatio = c(1.5),
        plannedEvents = c(20, 40), maxNumberOfSubjects = 200,
        maxNumberOfIterations = 100, seed = 1234567890
    )

    ## Comparison of the results of SimulationResultsSurvival object 'simulationResult' with expected results
    expect_equal(simulationResult$accrualIntensity, 16.666667, tolerance = 1e-07, label = paste0(simulationResult$accrualIntensity))
    expect_equal(simulationResult$lambda1, c(0.015, 0.03, 0.06), tolerance = 1e-07, label = paste0(simulationResult$lambda1))
    expect_equal(simulationResult$analysisTime[1, ], 12.106711, tolerance = 1e-07, label = paste0(simulationResult$analysisTime[1, ]))
    expect_equal(simulationResult$analysisTime[2, ], 16.150578, tolerance = 1e-07, label = paste0(simulationResult$analysisTime[2, ]))
    expect_equal(simulationResult$studyDuration, 16.020702, tolerance = 1e-07, label = paste0(simulationResult$studyDuration))
    expect_equal(simulationResult$eventsNotAchieved[1, ], 0, label = paste0(simulationResult$eventsNotAchieved[1, ]))
    expect_equal(simulationResult$eventsNotAchieved[2, ], 0, label = paste0(simulationResult$eventsNotAchieved[2, ]))
    expect_equal(simulationResult$numberOfSubjects[1, ], 193.51, tolerance = 1e-07, label = paste0(simulationResult$numberOfSubjects[1, ]))
    expect_equal(simulationResult$numberOfSubjects[2, ], 200, label = paste0(simulationResult$numberOfSubjects[2, ]))
    expect_equal(simulationResult$cumulativeEventsPerStage[1, ], 20, label = paste0(simulationResult$cumulativeEventsPerStage[1, ]))
    expect_equal(simulationResult$cumulativeEventsPerStage[2, ], 40, label = paste0(simulationResult$cumulativeEventsPerStage[2, ]))
    expect_equal(simulationResult$iterations[1, ], 100, label = paste0(simulationResult$iterations[1, ]))
    expect_equal(simulationResult$iterations[2, ], 96, label = paste0(simulationResult$iterations[2, ]))
    expect_equal(simulationResult$overallReject, 0.32, tolerance = 1e-07, label = paste0(simulationResult$overallReject))
    expect_equal(simulationResult$rejectPerStage[1, ], 0.04, tolerance = 1e-07, label = paste0(simulationResult$rejectPerStage[1, ]))
    expect_equal(simulationResult$rejectPerStage[2, ], 0.28, tolerance = 1e-07, label = paste0(simulationResult$rejectPerStage[2, ]))
    expect_equal(simulationResult$futilityPerStage[1, ], 0, label = paste0(simulationResult$futilityPerStage[1, ]))
    expect_equal(simulationResult$earlyStop, 0.04, tolerance = 1e-07, label = paste0(simulationResult$earlyStop))
    expect_equal(simulationResult$expectedNumberOfSubjects, 199.7404, tolerance = 1e-07, label = paste0(simulationResult$expectedNumberOfSubjects))
    expect_equal(simulationResult$expectedNumberOfEvents, 39.2, tolerance = 1e-07, label = paste0(simulationResult$expectedNumberOfEvents))
    expect_equal(simulationResult$conditionalPowerAchieved[1, ], NA_real_, label = paste0(simulationResult$conditionalPowerAchieved[1, ]))
    expect_equal(simulationResult$conditionalPowerAchieved[2, ], 0.33404702, tolerance = 1e-07, label = paste0(simulationResult$conditionalPowerAchieved[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(simulationResult), NA)))
        expect_output(print(simulationResult)$show())
        invisible(capture.output(expect_error(summary(simulationResult), NA)))
        expect_output(summary(simulationResult)$show())
        simulationResultCodeBased <- eval(parse(text = getObjectRCode(simulationResult, stringWrapParagraphWidth = NULL)))
        expect_equal(simulationResultCodeBased$accrualIntensity, simulationResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$lambda1, simulationResult$lambda1, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$analysisTime, simulationResult$analysisTime, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$studyDuration, simulationResult$studyDuration, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$eventsNotAchieved, simulationResult$eventsNotAchieved, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$numberOfSubjects, simulationResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$cumulativeEventsPerStage, simulationResult$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$iterations, simulationResult$iterations, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$overallReject, simulationResult$overallReject, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$rejectPerStage, simulationResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$futilityPerStage, simulationResult$futilityPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$earlyStop, simulationResult$earlyStop, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$expectedNumberOfSubjects, simulationResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$expectedNumberOfEvents, simulationResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$conditionalPowerAchieved, simulationResult$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(simulationResult), "character")
        df <- as.data.frame(simulationResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(simulationResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSimulationSurvival': Specification of piecewise exponential survival time for both treatment arms", {
    .skipTestIfDisabled()

    simulationResult <- getSimulationSurvival(
        design = getDesignGroupSequential(kMax = 2),
        piecewiseSurvivalTime = c(0, 5, 10), lambda2 = c(0.01, 0.02, 0.04),
        lambda1 = c(0.015, 0.03, 0.06), plannedEvents = c(20, 40),
        maxNumberOfSubjects = 200, maxNumberOfIterations = 100, seed = 1234567890
    )

    ## Comparison of the results of SimulationResultsSurvival object 'simulationResult' with expected results
    expect_equal(simulationResult$accrualIntensity, 16.666667, tolerance = 1e-07, label = paste0(simulationResult$accrualIntensity))
    expect_equal(simulationResult$hazardRatio, 1.5, tolerance = 1e-07, label = paste0(simulationResult$hazardRatio))
    expect_equal(simulationResult$analysisTime[1, ], 12.106711, tolerance = 1e-07, label = paste0(simulationResult$analysisTime[1, ]))
    expect_equal(simulationResult$analysisTime[2, ], 16.150578, tolerance = 1e-07, label = paste0(simulationResult$analysisTime[2, ]))
    expect_equal(simulationResult$studyDuration, 16.020702, tolerance = 1e-07, label = paste0(simulationResult$studyDuration))
    expect_equal(simulationResult$eventsNotAchieved[1, ], 0, label = paste0(simulationResult$eventsNotAchieved[1, ]))
    expect_equal(simulationResult$eventsNotAchieved[2, ], 0, label = paste0(simulationResult$eventsNotAchieved[2, ]))
    expect_equal(simulationResult$numberOfSubjects[1, ], 193.51, tolerance = 1e-07, label = paste0(simulationResult$numberOfSubjects[1, ]))
    expect_equal(simulationResult$numberOfSubjects[2, ], 200, label = paste0(simulationResult$numberOfSubjects[2, ]))
    expect_equal(simulationResult$cumulativeEventsPerStage[1, ], 20, label = paste0(simulationResult$cumulativeEventsPerStage[1, ]))
    expect_equal(simulationResult$cumulativeEventsPerStage[2, ], 40, label = paste0(simulationResult$cumulativeEventsPerStage[2, ]))
    expect_equal(simulationResult$iterations[1, ], 100, label = paste0(simulationResult$iterations[1, ]))
    expect_equal(simulationResult$iterations[2, ], 96, label = paste0(simulationResult$iterations[2, ]))
    expect_equal(simulationResult$overallReject, 0.32, tolerance = 1e-07, label = paste0(simulationResult$overallReject))
    expect_equal(simulationResult$rejectPerStage[1, ], 0.04, tolerance = 1e-07, label = paste0(simulationResult$rejectPerStage[1, ]))
    expect_equal(simulationResult$rejectPerStage[2, ], 0.28, tolerance = 1e-07, label = paste0(simulationResult$rejectPerStage[2, ]))
    expect_equal(simulationResult$futilityPerStage[1, ], 0, label = paste0(simulationResult$futilityPerStage[1, ]))
    expect_equal(simulationResult$earlyStop, 0.04, tolerance = 1e-07, label = paste0(simulationResult$earlyStop))
    expect_equal(simulationResult$expectedNumberOfSubjects, 199.7404, tolerance = 1e-07, label = paste0(simulationResult$expectedNumberOfSubjects))
    expect_equal(simulationResult$expectedNumberOfEvents, 39.2, tolerance = 1e-07, label = paste0(simulationResult$expectedNumberOfEvents))
    expect_equal(simulationResult$conditionalPowerAchieved[1, ], NA_real_, label = paste0(simulationResult$conditionalPowerAchieved[1, ]))
    expect_equal(simulationResult$conditionalPowerAchieved[2, ], 0.33404702, tolerance = 1e-07, label = paste0(simulationResult$conditionalPowerAchieved[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(simulationResult), NA)))
        expect_output(print(simulationResult)$show())
        invisible(capture.output(expect_error(summary(simulationResult), NA)))
        expect_output(summary(simulationResult)$show())
        simulationResultCodeBased <- eval(parse(text = getObjectRCode(simulationResult, stringWrapParagraphWidth = NULL)))
        expect_equal(simulationResultCodeBased$accrualIntensity, simulationResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$hazardRatio, simulationResult$hazardRatio, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$analysisTime, simulationResult$analysisTime, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$studyDuration, simulationResult$studyDuration, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$eventsNotAchieved, simulationResult$eventsNotAchieved, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$numberOfSubjects, simulationResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$cumulativeEventsPerStage, simulationResult$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$iterations, simulationResult$iterations, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$overallReject, simulationResult$overallReject, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$rejectPerStage, simulationResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$futilityPerStage, simulationResult$futilityPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$earlyStop, simulationResult$earlyStop, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$expectedNumberOfSubjects, simulationResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$expectedNumberOfEvents, simulationResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$conditionalPowerAchieved, simulationResult$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(simulationResult), "character")
        df <- as.data.frame(simulationResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(simulationResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    pws <- list("0 - <5" = 0.01, "5 - <10" = 0.02, ">=10" = 0.04)
    simulationResult <- getSimulationSurvival(
        design = getDesignGroupSequential(kMax = 2),
        piecewiseSurvivalTime = pws, hazardRatio = 1.5,
        plannedEvents = c(20, 40), maxNumberOfSubjects = 200,
        maxNumberOfIterations = 100, seed = 1234567890
    )

    ## Comparison of the results of SimulationResultsSurvival object 'simulationResult' with expected results
    expect_equal(simulationResult$accrualIntensity, 16.666667, tolerance = 1e-07, label = paste0(simulationResult$accrualIntensity))
    expect_equal(simulationResult$lambda1, c(0.015, 0.03, 0.06), tolerance = 1e-07, label = paste0(simulationResult$lambda1))
    expect_equal(simulationResult$analysisTime[1, ], 12.106711, tolerance = 1e-07, label = paste0(simulationResult$analysisTime[1, ]))
    expect_equal(simulationResult$analysisTime[2, ], 16.150578, tolerance = 1e-07, label = paste0(simulationResult$analysisTime[2, ]))
    expect_equal(simulationResult$studyDuration, 16.020702, tolerance = 1e-07, label = paste0(simulationResult$studyDuration))
    expect_equal(simulationResult$eventsNotAchieved[1, ], 0, label = paste0(simulationResult$eventsNotAchieved[1, ]))
    expect_equal(simulationResult$eventsNotAchieved[2, ], 0, label = paste0(simulationResult$eventsNotAchieved[2, ]))
    expect_equal(simulationResult$numberOfSubjects[1, ], 193.51, tolerance = 1e-07, label = paste0(simulationResult$numberOfSubjects[1, ]))
    expect_equal(simulationResult$numberOfSubjects[2, ], 200, label = paste0(simulationResult$numberOfSubjects[2, ]))
    expect_equal(simulationResult$cumulativeEventsPerStage[1, ], 20, label = paste0(simulationResult$cumulativeEventsPerStage[1, ]))
    expect_equal(simulationResult$cumulativeEventsPerStage[2, ], 40, label = paste0(simulationResult$cumulativeEventsPerStage[2, ]))
    expect_equal(simulationResult$iterations[1, ], 100, label = paste0(simulationResult$iterations[1, ]))
    expect_equal(simulationResult$iterations[2, ], 96, label = paste0(simulationResult$iterations[2, ]))
    expect_equal(simulationResult$overallReject, 0.32, tolerance = 1e-07, label = paste0(simulationResult$overallReject))
    expect_equal(simulationResult$rejectPerStage[1, ], 0.04, tolerance = 1e-07, label = paste0(simulationResult$rejectPerStage[1, ]))
    expect_equal(simulationResult$rejectPerStage[2, ], 0.28, tolerance = 1e-07, label = paste0(simulationResult$rejectPerStage[2, ]))
    expect_equal(simulationResult$futilityPerStage[1, ], 0, label = paste0(simulationResult$futilityPerStage[1, ]))
    expect_equal(simulationResult$earlyStop, 0.04, tolerance = 1e-07, label = paste0(simulationResult$earlyStop))
    expect_equal(simulationResult$expectedNumberOfSubjects, 199.7404, tolerance = 1e-07, label = paste0(simulationResult$expectedNumberOfSubjects))
    expect_equal(simulationResult$expectedNumberOfEvents, 39.2, tolerance = 1e-07, label = paste0(simulationResult$expectedNumberOfEvents))
    expect_equal(simulationResult$conditionalPowerAchieved[1, ], NA_real_, label = paste0(simulationResult$conditionalPowerAchieved[1, ]))
    expect_equal(simulationResult$conditionalPowerAchieved[2, ], 0.33404702, tolerance = 1e-07, label = paste0(simulationResult$conditionalPowerAchieved[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(simulationResult), NA)))
        expect_output(print(simulationResult)$show())
        invisible(capture.output(expect_error(summary(simulationResult), NA)))
        expect_output(summary(simulationResult)$show())
        simulationResultCodeBased <- eval(parse(text = getObjectRCode(simulationResult, stringWrapParagraphWidth = NULL)))
        expect_equal(simulationResultCodeBased$accrualIntensity, simulationResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$lambda1, simulationResult$lambda1, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$analysisTime, simulationResult$analysisTime, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$studyDuration, simulationResult$studyDuration, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$eventsNotAchieved, simulationResult$eventsNotAchieved, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$numberOfSubjects, simulationResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$cumulativeEventsPerStage, simulationResult$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$iterations, simulationResult$iterations, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$overallReject, simulationResult$overallReject, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$rejectPerStage, simulationResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$futilityPerStage, simulationResult$futilityPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$earlyStop, simulationResult$earlyStop, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$expectedNumberOfSubjects, simulationResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$expectedNumberOfEvents, simulationResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$conditionalPowerAchieved, simulationResult$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(simulationResult), "character")
        df <- as.data.frame(simulationResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(simulationResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    simulationResult <- getSimulationSurvival(
        design = getDesignGroupSequential(kMax = 2),
        piecewiseSurvivalTime = c(0, 5, 10), lambda2 = c(0.01, 0.02, 0.04),
        lambda1 = c(0.01, 0.02, 0.06), plannedEvents = c(20, 40), maxNumberOfSubjects = 200,
        maxNumberOfIterations = 100, seed = 1234567890
    )

    ## Comparison of the results of SimulationResultsSurvival object 'simulationResult' with expected results
    expect_equal(simulationResult$accrualIntensity, 16.666667, tolerance = 1e-07, label = paste0(simulationResult$accrualIntensity))
    expect_equal(simulationResult$hazardRatio, c(1, 1, 1.5), tolerance = 1e-07, label = paste0(simulationResult$hazardRatio))
    expect_equal(simulationResult$analysisTime[1, ], 12.973056, tolerance = 1e-07, label = paste0(simulationResult$analysisTime[1, ]))
    expect_equal(simulationResult$analysisTime[2, ], 17.030809, tolerance = 1e-07, label = paste0(simulationResult$analysisTime[2, ]))
    expect_equal(simulationResult$studyDuration, 17.030809, tolerance = 1e-07, label = paste0(simulationResult$studyDuration))
    expect_equal(simulationResult$eventsNotAchieved[1, ], 0, label = paste0(simulationResult$eventsNotAchieved[1, ]))
    expect_equal(simulationResult$eventsNotAchieved[2, ], 0, label = paste0(simulationResult$eventsNotAchieved[2, ]))
    expect_equal(simulationResult$numberOfSubjects[1, ], 197.81, tolerance = 1e-07, label = paste0(simulationResult$numberOfSubjects[1, ]))
    expect_equal(simulationResult$numberOfSubjects[2, ], 200, label = paste0(simulationResult$numberOfSubjects[2, ]))
    expect_equal(simulationResult$cumulativeEventsPerStage[1, ], 20, label = paste0(simulationResult$cumulativeEventsPerStage[1, ]))
    expect_equal(simulationResult$cumulativeEventsPerStage[2, ], 40, label = paste0(simulationResult$cumulativeEventsPerStage[2, ]))
    expect_equal(simulationResult$iterations[1, ], 100, label = paste0(simulationResult$iterations[1, ]))
    expect_equal(simulationResult$iterations[2, ], 100, label = paste0(simulationResult$iterations[2, ]))
    expect_equal(simulationResult$overallReject, 0.06, tolerance = 1e-07, label = paste0(simulationResult$overallReject))
    expect_equal(simulationResult$rejectPerStage[1, ], 0, label = paste0(simulationResult$rejectPerStage[1, ]))
    expect_equal(simulationResult$rejectPerStage[2, ], 0.06, tolerance = 1e-07, label = paste0(simulationResult$rejectPerStage[2, ]))
    expect_equal(simulationResult$futilityPerStage[1, ], 0, label = paste0(simulationResult$futilityPerStage[1, ]))
    expect_equal(simulationResult$earlyStop, 0, label = paste0(simulationResult$earlyStop))
    expect_equal(simulationResult$expectedNumberOfSubjects, 200, label = paste0(simulationResult$expectedNumberOfSubjects))
    expect_equal(simulationResult$expectedNumberOfEvents, 40, label = paste0(simulationResult$expectedNumberOfEvents))
    expect_equal(simulationResult$conditionalPowerAchieved[1, ], NA_real_, label = paste0(simulationResult$conditionalPowerAchieved[1, ]))
    expect_equal(simulationResult$conditionalPowerAchieved[2, ], 0.1789388, tolerance = 1e-07, label = paste0(simulationResult$conditionalPowerAchieved[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(simulationResult), NA)))
        expect_output(print(simulationResult)$show())
        invisible(capture.output(expect_error(summary(simulationResult), NA)))
        expect_output(summary(simulationResult)$show())
        simulationResultCodeBased <- eval(parse(text = getObjectRCode(simulationResult, stringWrapParagraphWidth = NULL)))
        expect_equal(simulationResultCodeBased$accrualIntensity, simulationResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$hazardRatio, simulationResult$hazardRatio, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$analysisTime, simulationResult$analysisTime, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$studyDuration, simulationResult$studyDuration, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$eventsNotAchieved, simulationResult$eventsNotAchieved, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$numberOfSubjects, simulationResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$cumulativeEventsPerStage, simulationResult$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$iterations, simulationResult$iterations, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$overallReject, simulationResult$overallReject, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$rejectPerStage, simulationResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$futilityPerStage, simulationResult$futilityPerStage, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$earlyStop, simulationResult$earlyStop, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$expectedNumberOfSubjects, simulationResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$expectedNumberOfEvents, simulationResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(simulationResultCodeBased$conditionalPowerAchieved, simulationResult$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(simulationResult), "character")
        df <- as.data.frame(simulationResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(simulationResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSimulationSurvival': Perform recalculation of number of events based on conditional power", {
    .skipTestIfDisabled()

    # Perform recalculation of number of events based on conditional power for a
    # three-stage design with inverse normal combination test, where the conditional power
    # is calculated under the specified effect size thetaH1 = 1.3 and up to a four-fold
    # increase in originally planned sample size (number of events) is allowed
    # Note that the first value in \code{minNumberOfEventsPerStage} and
    # \code{maxNumberOfEventsPerStage} is arbitrary, i.e., it has no effect.

    dIN <- getDesignInverseNormal(informationRates = c(0.4, 0.7, 1))

    resultsWithSSR1 <- getSimulationSurvival(
        design = dIN, hazardRatio = seq(1, 1.6, 0.1),
        pi2 = 0.3, conditionalPower = 0.8, thetaH1 = 1.3, plannedEvents = c(58, 102, 146),
        minNumberOfEventsPerStage = c(NA_real_, 44, 44),
        maxNumberOfEventsPerStage = 4 * c(NA_real_, 44, 44),
        maxNumberOfSubjects = 800, maxNumberOfIterations = 100, seed = 1234567890
    )

    ## Comparison of the results of SimulationResultsSurvival object 'resultsWithSSR1' with expected results
    expect_equal(resultsWithSSR1$pi1, c(0.3, 0.32452723, 0.34819506, 0.37103359, 0.39307188, 0.41433798, 0.43485894), tolerance = 1e-07, label = paste0(resultsWithSSR1$pi1))
    expect_equal(resultsWithSSR1$median1, c(23.320299, 21.200271, 19.433582, 17.938691, 16.657356, 15.546866, 14.575187), tolerance = 1e-07, label = paste0(resultsWithSSR1$median1))
    expect_equal(resultsWithSSR1$median2, 23.320299, tolerance = 1e-07, label = paste0(resultsWithSSR1$median2))
    expect_equal(resultsWithSSR1$accrualIntensity, 66.666667, tolerance = 1e-07, label = paste0(resultsWithSSR1$accrualIntensity))
    expect_equal(resultsWithSSR1$lambda1, c(0.029722912, 0.032695203, 0.035667494, 0.038639786, 0.041612077, 0.044584368, 0.047556659), tolerance = 1e-07, label = paste0(resultsWithSSR1$lambda1))
    expect_equal(resultsWithSSR1$lambda2, 0.029722912, tolerance = 1e-07, label = paste0(resultsWithSSR1$lambda2))
    expect_equal(resultsWithSSR1$analysisTime[1, ], c(7.9761501, 7.8239889, 7.5191849, 7.4832292, 7.3291066, 7.1091953, 6.9737455), tolerance = 1e-07, label = paste0(resultsWithSSR1$analysisTime[1, ]))
    expect_equal(resultsWithSSR1$analysisTime[2, ], c(17.76189, 17.229038, 16.567328, 16.175906, 15.668575, 15.328143, 14.604753), tolerance = 1e-07, label = paste0(resultsWithSSR1$analysisTime[2, ]))
    expect_equal(resultsWithSSR1$analysisTime[3, ], c(30.192276, 28.615009, 26.463502, 25.657109, 23.821118, 23.34898, 22.534023), tolerance = 1e-07, label = paste0(resultsWithSSR1$analysisTime[3, ]))
    expect_equal(resultsWithSSR1$studyDuration, c(29.683899, 28.160756, 25.20615, 22.190278, 19.319577, 18.030286, 14.789904), tolerance = 1e-07, label = paste0(resultsWithSSR1$studyDuration))
    expect_equal(resultsWithSSR1$eventsNotAchieved[1, ], c(0, 0, 0, 0, 0, 0, 0), label = paste0(resultsWithSSR1$eventsNotAchieved[1, ]))
    expect_equal(resultsWithSSR1$eventsNotAchieved[2, ], c(0, 0, 0, 0, 0, 0, 0), label = paste0(resultsWithSSR1$eventsNotAchieved[2, ]))
    expect_equal(resultsWithSSR1$eventsNotAchieved[3, ], c(0, 0, 0, 0, 0, 0, 0), label = paste0(resultsWithSSR1$eventsNotAchieved[3, ]))
    expect_equal(resultsWithSSR1$numberOfSubjects[1, ], c(531.25, 521.07, 500.8, 498.42, 488.13, 473.47, 464.37), tolerance = 1e-07, label = paste0(resultsWithSSR1$numberOfSubjects[1, ]))
    expect_equal(resultsWithSSR1$numberOfSubjects[2, ], c(800, 800, 799.45, 798.66327, 796.55208, 797.06061, 793.47826), tolerance = 1e-07, label = paste0(resultsWithSSR1$numberOfSubjects[2, ]))
    expect_equal(resultsWithSSR1$numberOfSubjects[3, ], c(800, 800, 800, 800, 800, 800, 800), label = paste0(resultsWithSSR1$numberOfSubjects[3, ]))
    expect_equal(resultsWithSSR1$cumulativeEventsPerStage[1, ], c(58, 58, 58, 58, 58, 58, 58), label = paste0(resultsWithSSR1$cumulativeEventsPerStage[1, ]))
    expect_equal(resultsWithSSR1$cumulativeEventsPerStage[2, ], c(233.65, 231.27, 229.84, 229.43878, 228.57292, 227.67677, 219.44565), tolerance = 1e-07, label = paste0(resultsWithSSR1$cumulativeEventsPerStage[2, ]))
    expect_equal(resultsWithSSR1$cumulativeEventsPerStage[3, ], c(408.93125, 400.78042, 383.94227, 378.3343, 365.87292, 369.8482, 353.17292), tolerance = 1e-07, label = paste0(resultsWithSSR1$cumulativeEventsPerStage[3, ]))
    expect_equal(resultsWithSSR1$iterations[1, ], c(100, 100, 100, 100, 100, 100, 100), label = paste0(resultsWithSSR1$iterations[1, ]))
    expect_equal(resultsWithSSR1$iterations[2, ], c(100, 100, 100, 98, 96, 99, 92), label = paste0(resultsWithSSR1$iterations[2, ]))
    expect_equal(resultsWithSSR1$iterations[3, ], c(96, 96, 88, 67, 50, 35, 11), label = paste0(resultsWithSSR1$iterations[3, ]))
    expect_equal(resultsWithSSR1$overallReject, c(0.04, 0.16, 0.38, 0.75, 0.91, 0.95, 1), tolerance = 1e-07, label = paste0(resultsWithSSR1$overallReject))
    expect_equal(resultsWithSSR1$rejectPerStage[1, ], c(0, 0, 0, 0.02, 0.04, 0.01, 0.08), tolerance = 1e-07, label = paste0(resultsWithSSR1$rejectPerStage[1, ]))
    expect_equal(resultsWithSSR1$rejectPerStage[2, ], c(0.04, 0.04, 0.12, 0.31, 0.46, 0.64, 0.81), tolerance = 1e-07, label = paste0(resultsWithSSR1$rejectPerStage[2, ]))
    expect_equal(resultsWithSSR1$rejectPerStage[3, ], c(0, 0.12, 0.26, 0.42, 0.41, 0.3, 0.11), tolerance = 1e-07, label = paste0(resultsWithSSR1$rejectPerStage[3, ]))
    expect_equal(resultsWithSSR1$futilityStop, c(0, 0, 0, 0, 0, 0, 0), label = paste0(resultsWithSSR1$futilityStop))
    expect_equal(resultsWithSSR1$futilityPerStage[1, ], c(0, 0, 0, 0, 0, 0, 0), label = paste0(resultsWithSSR1$futilityPerStage[1, ]))
    expect_equal(resultsWithSSR1$futilityPerStage[2, ], c(0, 0, 0, 0, 0, 0, 0), label = paste0(resultsWithSSR1$futilityPerStage[2, ]))
    expect_equal(resultsWithSSR1$earlyStop, c(0.04, 0.04, 0.12, 0.33, 0.5, 0.65, 0.89), tolerance = 1e-07, label = paste0(resultsWithSSR1$earlyStop))
    expect_equal(resultsWithSSR1$expectedNumberOfSubjects, c(800, 800, 799.934, 793.55401, 785.93916, 794.85349, 767.86699), tolerance = 1e-07, label = paste0(resultsWithSSR1$expectedNumberOfSubjects))
    expect_equal(resultsWithSSR1$expectedNumberOfEvents, c(401.92, 394, 365.45, 325.77, 290.4, 275.74, 221.24), tolerance = 1e-07, label = paste0(resultsWithSSR1$expectedNumberOfEvents))
    expect_equal(resultsWithSSR1$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(resultsWithSSR1$conditionalPowerAchieved[1, ]))
    expect_equal(resultsWithSSR1$conditionalPowerAchieved[2, ], c(0.12165751, 0.15502837, 0.23497758, 0.29890789, 0.33886493, 0.41286728, 0.49916888), tolerance = 1e-07, label = paste0(resultsWithSSR1$conditionalPowerAchieved[2, ]))
    expect_equal(resultsWithSSR1$conditionalPowerAchieved[3, ], c(0.14749827, 0.23857933, 0.44868993, 0.59763371, 0.65378645, 0.66059558, 0.69812096), tolerance = 1e-07, label = paste0(resultsWithSSR1$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(resultsWithSSR1), NA)))
        expect_output(print(resultsWithSSR1)$show())
        invisible(capture.output(expect_error(summary(resultsWithSSR1), NA)))
        expect_output(summary(resultsWithSSR1)$show())
        resultsWithSSR1CodeBased <- eval(parse(text = getObjectRCode(resultsWithSSR1, stringWrapParagraphWidth = NULL)))
        expect_equal(resultsWithSSR1CodeBased$pi1, resultsWithSSR1$pi1, tolerance = 1e-07)
        expect_equal(resultsWithSSR1CodeBased$median1, resultsWithSSR1$median1, tolerance = 1e-07)
        expect_equal(resultsWithSSR1CodeBased$median2, resultsWithSSR1$median2, tolerance = 1e-07)
        expect_equal(resultsWithSSR1CodeBased$accrualIntensity, resultsWithSSR1$accrualIntensity, tolerance = 1e-07)
        expect_equal(resultsWithSSR1CodeBased$lambda1, resultsWithSSR1$lambda1, tolerance = 1e-07)
        expect_equal(resultsWithSSR1CodeBased$lambda2, resultsWithSSR1$lambda2, tolerance = 1e-07)
        expect_equal(resultsWithSSR1CodeBased$analysisTime, resultsWithSSR1$analysisTime, tolerance = 1e-07)
        expect_equal(resultsWithSSR1CodeBased$studyDuration, resultsWithSSR1$studyDuration, tolerance = 1e-07)
        expect_equal(resultsWithSSR1CodeBased$eventsNotAchieved, resultsWithSSR1$eventsNotAchieved, tolerance = 1e-07)
        expect_equal(resultsWithSSR1CodeBased$numberOfSubjects, resultsWithSSR1$numberOfSubjects, tolerance = 1e-07)
        expect_equal(resultsWithSSR1CodeBased$cumulativeEventsPerStage, resultsWithSSR1$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(resultsWithSSR1CodeBased$iterations, resultsWithSSR1$iterations, tolerance = 1e-07)
        expect_equal(resultsWithSSR1CodeBased$overallReject, resultsWithSSR1$overallReject, tolerance = 1e-07)
        expect_equal(resultsWithSSR1CodeBased$rejectPerStage, resultsWithSSR1$rejectPerStage, tolerance = 1e-07)
        expect_equal(resultsWithSSR1CodeBased$futilityStop, resultsWithSSR1$futilityStop, tolerance = 1e-07)
        expect_equal(resultsWithSSR1CodeBased$futilityPerStage, resultsWithSSR1$futilityPerStage, tolerance = 1e-07)
        expect_equal(resultsWithSSR1CodeBased$earlyStop, resultsWithSSR1$earlyStop, tolerance = 1e-07)
        expect_equal(resultsWithSSR1CodeBased$expectedNumberOfSubjects, resultsWithSSR1$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(resultsWithSSR1CodeBased$expectedNumberOfEvents, resultsWithSSR1$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(resultsWithSSR1CodeBased$conditionalPowerAchieved, resultsWithSSR1$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(resultsWithSSR1), "character")
        df <- as.data.frame(resultsWithSSR1)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(resultsWithSSR1)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # If thetaH1 is unspecified, the observed hazard ratio estimate
    # (calculated from the log-rank statistic) is used for performing the
    # recalculation of the number of events
    resultsWithSSR2 <- getSimulationSurvival(
        design = dIN, hazardRatio = seq(1, 1.6, 0.1),
        pi2 = 0.3, conditionalPower = 0.8, plannedEvents = c(58, 102, 146),
        minNumberOfEventsPerStage = c(NA_real_, 44, 44),
        maxNumberOfEventsPerStage = 4 * c(NA_real_, 44, 44),
        maxNumberOfSubjects = 800, maxNumberOfIterations = 100, seed = 1234567890
    )

    ## Comparison of the results of SimulationResultsSurvival object 'resultsWithSSR2' with expected results
    expect_equal(resultsWithSSR2$pi1, c(0.3, 0.32452723, 0.34819506, 0.37103359, 0.39307188, 0.41433798, 0.43485894), tolerance = 1e-07, label = paste0(resultsWithSSR2$pi1))
    expect_equal(resultsWithSSR2$median1, c(23.320299, 21.200271, 19.433582, 17.938691, 16.657356, 15.546866, 14.575187), tolerance = 1e-07, label = paste0(resultsWithSSR2$median1))
    expect_equal(resultsWithSSR2$median2, 23.320299, tolerance = 1e-07, label = paste0(resultsWithSSR2$median2))
    expect_equal(resultsWithSSR2$accrualIntensity, 66.666667, tolerance = 1e-07, label = paste0(resultsWithSSR2$accrualIntensity))
    expect_equal(resultsWithSSR2$lambda1, c(0.029722912, 0.032695203, 0.035667494, 0.038639786, 0.041612077, 0.044584368, 0.047556659), tolerance = 1e-07, label = paste0(resultsWithSSR2$lambda1))
    expect_equal(resultsWithSSR2$lambda2, 0.029722912, tolerance = 1e-07, label = paste0(resultsWithSSR2$lambda2))
    expect_equal(resultsWithSSR2$analysisTime[1, ], c(7.9761501, 7.8239889, 7.5191849, 7.4832292, 7.3291066, 7.1091953, 6.9737455), tolerance = 1e-07, label = paste0(resultsWithSSR2$analysisTime[1, ]))
    expect_equal(resultsWithSSR2$analysisTime[2, ], c(17.532866, 16.792737, 15.753436, 15.242772, 14.414526, 13.395253, 12.536642), tolerance = 1e-07, label = paste0(resultsWithSSR2$analysisTime[2, ]))
    expect_equal(resultsWithSSR2$analysisTime[3, ], c(29.782185, 28.27297, 25.249508, 24.235039, 21.407797, 20.846814, 17.625231), tolerance = 1e-07, label = paste0(resultsWithSSR2$analysisTime[3, ]))
    expect_equal(resultsWithSSR2$studyDuration, c(29.663096, 27.530562, 24.305604, 21.136576, 18.176787, 16.398878, 13.170673), tolerance = 1e-07, label = paste0(resultsWithSSR2$studyDuration))
    expect_equal(resultsWithSSR2$eventsNotAchieved[1, ], c(0, 0, 0, 0, 0, 0, 0), label = paste0(resultsWithSSR2$eventsNotAchieved[1, ]))
    expect_equal(resultsWithSSR2$eventsNotAchieved[2, ], c(0, 0, 0, 0, 0, 0, 0), label = paste0(resultsWithSSR2$eventsNotAchieved[2, ]))
    expect_equal(resultsWithSSR2$eventsNotAchieved[3, ], c(0, 0, 0, 0, 0, 0, 0), label = paste0(resultsWithSSR2$eventsNotAchieved[3, ]))
    expect_equal(resultsWithSSR2$numberOfSubjects[1, ], c(531.25, 521.07, 500.8, 498.42, 488.13, 473.47, 464.37), tolerance = 1e-07, label = paste0(resultsWithSSR2$numberOfSubjects[1, ]))
    expect_equal(resultsWithSSR2$numberOfSubjects[2, ], c(798.3, 792.67, 784.71, 785.72449, 774.40625, 754.47475, 731), tolerance = 1e-07, label = paste0(resultsWithSSR2$numberOfSubjects[2, ]))
    expect_equal(resultsWithSSR2$numberOfSubjects[3, ], c(800, 800, 800, 800, 799.08333, 797.51111, 794.95238), tolerance = 1e-07, label = paste0(resultsWithSSR2$numberOfSubjects[3, ]))
    expect_equal(resultsWithSSR2$cumulativeEventsPerStage[1, ], c(58, 58, 58, 58, 58, 58, 58), label = paste0(resultsWithSSR2$cumulativeEventsPerStage[1, ]))
    expect_equal(resultsWithSSR2$cumulativeEventsPerStage[2, ], c(229.71, 222.76, 213.91, 210.63265, 201.21875, 185.82828, 171.84783), tolerance = 1e-07, label = paste0(resultsWithSSR2$cumulativeEventsPerStage[2, ]))
    expect_equal(resultsWithSSR2$cumulativeEventsPerStage[3, ], c(403.59889, 392.21263, 361.25783, 350.23829, 321.46875, 311.49495, 272.08592), tolerance = 1e-07, label = paste0(resultsWithSSR2$cumulativeEventsPerStage[3, ]))
    expect_equal(resultsWithSSR2$iterations[1, ], c(100, 100, 100, 100, 100, 100, 100), label = paste0(resultsWithSSR2$iterations[1, ]))
    expect_equal(resultsWithSSR2$iterations[2, ], c(100, 100, 100, 98, 96, 99, 92), label = paste0(resultsWithSSR2$iterations[2, ]))
    expect_equal(resultsWithSSR2$iterations[3, ], c(99, 95, 92, 71, 60, 45, 21), label = paste0(resultsWithSSR2$iterations[3, ]))
    expect_equal(resultsWithSSR2$overallReject, c(0.04, 0.16, 0.37, 0.68, 0.88, 0.92, 0.98), tolerance = 1e-07, label = paste0(resultsWithSSR2$overallReject))
    expect_equal(resultsWithSSR2$rejectPerStage[1, ], c(0, 0, 0, 0.02, 0.04, 0.01, 0.08), tolerance = 1e-07, label = paste0(resultsWithSSR2$rejectPerStage[1, ]))
    expect_equal(resultsWithSSR2$rejectPerStage[2, ], c(0.01, 0.05, 0.08, 0.27, 0.36, 0.54, 0.71), tolerance = 1e-07, label = paste0(resultsWithSSR2$rejectPerStage[2, ]))
    expect_equal(resultsWithSSR2$rejectPerStage[3, ], c(0.03, 0.11, 0.29, 0.39, 0.48, 0.37, 0.19), tolerance = 1e-07, label = paste0(resultsWithSSR2$rejectPerStage[3, ]))
    expect_equal(resultsWithSSR2$futilityStop, c(0, 0, 0, 0, 0, 0, 0), label = paste0(resultsWithSSR2$futilityStop))
    expect_equal(resultsWithSSR2$futilityPerStage[1, ], c(0, 0, 0, 0, 0, 0, 0), label = paste0(resultsWithSSR2$futilityPerStage[1, ]))
    expect_equal(resultsWithSSR2$futilityPerStage[2, ], c(0, 0, 0, 0, 0, 0, 0), label = paste0(resultsWithSSR2$futilityPerStage[2, ]))
    expect_equal(resultsWithSSR2$earlyStop, c(0.01, 0.05, 0.08, 0.29, 0.4, 0.55, 0.79), tolerance = 1e-07, label = paste0(resultsWithSSR2$earlyStop))
    expect_equal(resultsWithSSR2$expectedNumberOfSubjects, c(799.983, 799.6335, 798.7768, 790.11401, 777.76145, 771.03106, 723.0996), tolerance = 1e-07, label = paste0(resultsWithSSR2$expectedNumberOfSubjects))
    expect_equal(resultsWithSSR2$expectedNumberOfEvents, c(401.86, 383.74, 349.47, 306.7, 267.64, 241.1, 183.79), tolerance = 1e-07, label = paste0(resultsWithSSR2$expectedNumberOfEvents))
    expect_equal(resultsWithSSR2$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(resultsWithSSR2$conditionalPowerAchieved[1, ]))
    expect_equal(resultsWithSSR2$conditionalPowerAchieved[2, ], c(0.13442705, 0.17515425, 0.27216274, 0.37121019, 0.42163288, 0.51345413, 0.62679958), tolerance = 1e-07, label = paste0(resultsWithSSR2$conditionalPowerAchieved[2, ]))
    expect_equal(resultsWithSSR2$conditionalPowerAchieved[3, ], c(0.088787205, 0.13342075, 0.37806621, 0.51790868, 0.64116584, 0.64220287, 0.73456911), tolerance = 1e-07, label = paste0(resultsWithSSR2$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(resultsWithSSR2), NA)))
        expect_output(print(resultsWithSSR2)$show())
        invisible(capture.output(expect_error(summary(resultsWithSSR2), NA)))
        expect_output(summary(resultsWithSSR2)$show())
        resultsWithSSR2CodeBased <- eval(parse(text = getObjectRCode(resultsWithSSR2, stringWrapParagraphWidth = NULL)))
        expect_equal(resultsWithSSR2CodeBased$pi1, resultsWithSSR2$pi1, tolerance = 1e-07)
        expect_equal(resultsWithSSR2CodeBased$median1, resultsWithSSR2$median1, tolerance = 1e-07)
        expect_equal(resultsWithSSR2CodeBased$median2, resultsWithSSR2$median2, tolerance = 1e-07)
        expect_equal(resultsWithSSR2CodeBased$accrualIntensity, resultsWithSSR2$accrualIntensity, tolerance = 1e-07)
        expect_equal(resultsWithSSR2CodeBased$lambda1, resultsWithSSR2$lambda1, tolerance = 1e-07)
        expect_equal(resultsWithSSR2CodeBased$lambda2, resultsWithSSR2$lambda2, tolerance = 1e-07)
        expect_equal(resultsWithSSR2CodeBased$analysisTime, resultsWithSSR2$analysisTime, tolerance = 1e-07)
        expect_equal(resultsWithSSR2CodeBased$studyDuration, resultsWithSSR2$studyDuration, tolerance = 1e-07)
        expect_equal(resultsWithSSR2CodeBased$eventsNotAchieved, resultsWithSSR2$eventsNotAchieved, tolerance = 1e-07)
        expect_equal(resultsWithSSR2CodeBased$numberOfSubjects, resultsWithSSR2$numberOfSubjects, tolerance = 1e-07)
        expect_equal(resultsWithSSR2CodeBased$cumulativeEventsPerStage, resultsWithSSR2$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(resultsWithSSR2CodeBased$iterations, resultsWithSSR2$iterations, tolerance = 1e-07)
        expect_equal(resultsWithSSR2CodeBased$overallReject, resultsWithSSR2$overallReject, tolerance = 1e-07)
        expect_equal(resultsWithSSR2CodeBased$rejectPerStage, resultsWithSSR2$rejectPerStage, tolerance = 1e-07)
        expect_equal(resultsWithSSR2CodeBased$futilityStop, resultsWithSSR2$futilityStop, tolerance = 1e-07)
        expect_equal(resultsWithSSR2CodeBased$futilityPerStage, resultsWithSSR2$futilityPerStage, tolerance = 1e-07)
        expect_equal(resultsWithSSR2CodeBased$earlyStop, resultsWithSSR2$earlyStop, tolerance = 1e-07)
        expect_equal(resultsWithSSR2CodeBased$expectedNumberOfSubjects, resultsWithSSR2$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(resultsWithSSR2CodeBased$expectedNumberOfEvents, resultsWithSSR2$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(resultsWithSSR2CodeBased$conditionalPowerAchieved, resultsWithSSR2$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(resultsWithSSR2), "character")
        df <- as.data.frame(resultsWithSSR2)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(resultsWithSSR2)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # Compare it with design without event size recalculation
    resultsWithoutSSR <- getSimulationSurvival(
        design = dIN, hazardRatio = seq(1, 1.6, 0.1),
        pi2 = 0.3, plannedEvents = c(58, 102, 145), maxNumberOfSubjects = 800,
        maxNumberOfIterations = 100, seed = 1234567890
    )


    ## Comparison of the results of numeric object 'resultsWithoutSSR$overallReject' with expected results
    expect_equal(resultsWithoutSSR$overallReject, c(0.06, 0.09, 0.26, 0.36, 0.5, 0.62, 0.8), tolerance = 1e-07, label = paste0(resultsWithoutSSR$overallReject))

    ## Comparison of the results of numeric object 'resultsWithSSR1$overallReject' with expected results
    expect_equal(resultsWithSSR1$overallReject, c(0.04, 0.16, 0.38, 0.75, 0.91, 0.95, 1), tolerance = 1e-07, label = paste0(resultsWithSSR1$overallReject))

    ## Comparison of the results of numeric object 'resultsWithSSR2$overallReject' with expected results
    expect_equal(resultsWithSSR2$overallReject, c(0.04, 0.16, 0.37, 0.68, 0.88, 0.92, 0.98), tolerance = 1e-07, label = paste0(resultsWithSSR2$overallReject))
})

test_that("'getSimulationSurvival': Confirm that event size racalcuation increases the Type I error rate, i.e., you have to use the combination test", {
    .skipTestIfDisabled()

    dGS <- getDesignGroupSequential(informationRates = c(0.4, 0.7, 1))
    resultsWithSSRGS <- getSimulationSurvival(
        design = dGS, hazardRatio = seq(1),
        pi2 = 0.3, conditionalPower = 0.8, plannedEvents = c(58, 102, 145),
        minNumberOfEventsPerStage = c(NA_real_, 44, 44),
        maxNumberOfEventsPerStage = 4 * c(NA_real_, 44, 44),
        maxNumberOfSubjects = 800, maxNumberOfIterations = 100, seed = 1234567890
    )

    ## Comparison of the results of numeric object 'resultsWithSSRGS$overallReject' with expected results
    expect_equal(resultsWithSSRGS$overallReject, 0.05, tolerance = 1e-07, label = paste0(resultsWithSSRGS$overallReject))
})

test_that("'getSimulationSurvival': Confirm that different inputs of lambda, median, and pi with the identical meaning result in the same output", {
    .skipTestIfDisabled()

    x1 <- getSimulationSurvival(
        lambda2 = 0.4,
        hazardRatio = c(0.65, 0.7),
        plannedEvents = 98,
        maxNumberOfSubjects = 120,
        directionUpper = FALSE,
        maxNumberOfIterations = 1000,
        sided = 1, alpha = 0.1, seed = 123
    )

    x2 <- getSimulationSurvival(
        lambda2 = x1$.piecewiseSurvivalTime$lambda2,
        lambda1 = x1$.piecewiseSurvivalTime$lambda1,
        plannedEvents = 98,
        maxNumberOfSubjects = 120,
        directionUpper = FALSE,
        maxNumberOfIterations = 1000,
        sided = 1, alpha = 0.1, seed = 123
    )

    x3 <- getSimulationSurvival(
        piecewiseSurvivalTime = x2$.piecewiseSurvivalTime,
        plannedEvents = 98,
        maxNumberOfSubjects = 120,
        directionUpper = FALSE,
        maxNumberOfIterations = 1000,
        sided = 1, alpha = 0.1, seed = 123
    )

    x4 <- getSimulationSurvival(
        pi2 = getPiByLambda(x1$.piecewiseSurvivalTime$lambda2, 12L),
        hazardRatio = c(0.65, 0.7),
        plannedEvents = 98,
        maxNumberOfSubjects = 120,
        directionUpper = FALSE,
        maxNumberOfIterations = 1000,
        sided = 1, alpha = 0.1, seed = 123
    )

    x5 <- getSimulationSurvival(
        lambda2 = 0.4,
        lambda1 = x4$lambda1,
        plannedEvents = 98,
        maxNumberOfSubjects = 120,
        directionUpper = FALSE,
        maxNumberOfIterations = 1000,
        sided = 1, alpha = 0.1, seed = 123
    )

    x6 <- getSimulationSurvival(
        median2 = x5$median2,
        hazardRatio = c(0.65, 0.7),
        plannedEvents = 98,
        maxNumberOfSubjects = 120,
        directionUpper = FALSE,
        maxNumberOfIterations = 1000,
        sided = 1, alpha = 0.1, seed = 123
    )

    x7 <- getSimulationSurvival(
        median2 = x5$median2,
        median1 = x5$median1,
        plannedEvents = 98,
        maxNumberOfSubjects = 120,
        directionUpper = FALSE,
        maxNumberOfIterations = 1000,
        sided = 1, alpha = 0.1, seed = 123
    )


    ## Pairwise comparison of the results of x1 with the results of x2, x3, x4, x5, x6, and x7
    expect_equal(x1$maxNumberOfIterations, x2$maxNumberOfIterations)
    expect_equal(x1$seed, x2$seed)
    expect_equal(x1$allocationRatioPlanned, x2$allocationRatioPlanned)
    expect_equal(x1$conditionalPower, x2$conditionalPower)
    expect_equal(x2$iterations[1, ], x1$iterations[1, ])
    expect_equal(x1$futilityStop, x2$futilityStop)
    expect_equal(x1$directionUpper, x2$directionUpper)
    expect_equal(x1$plannedEvents, x2$plannedEvents)
    expect_equal(x1$minNumberOfEventsPerStage, x2$minNumberOfEventsPerStage)
    expect_equal(x1$maxNumberOfEventsPerStage, x2$maxNumberOfEventsPerStage)
    expect_equal(x1$thetaH1, x2$thetaH1)
    expect_equal(x1$expectedNumberOfEvents, x2$expectedNumberOfEvents)
    expect_equal(x1$pi1, x2$pi1, tolerance = 1e-07)
    expect_equal(x1$pi2, x2$pi2, tolerance = 1e-07)
    expect_equal(x1$median1, x2$median1, tolerance = 1e-07)
    expect_equal(x1$median2, x2$median2, tolerance = 1e-07)
    expect_equal(x1$maxNumberOfSubjects, x2$maxNumberOfSubjects)
    expect_equal(x1$accrualTime, x2$accrualTime)
    expect_equal(x1$accrualIntensity, x2$accrualIntensity)
    expect_equal(x1$dropoutRate1, x2$dropoutRate1)
    expect_equal(x1$dropoutRate2, x2$dropoutRate2)
    expect_equal(x1$dropoutTime, x2$dropoutTime)
    expect_equal(x1$eventTime, x2$eventTime)
    expect_equal(x1$thetaH0, x2$thetaH0)
    expect_equal(x1$allocation1, x2$allocation1)
    expect_equal(x1$allocation2, x2$allocation2)
    expect_equal(x1$kappa, x2$kappa)
    expect_equal(x1$piecewiseSurvivalTime, x2$piecewiseSurvivalTime)
    expect_equal(x1$lambda1, x2$lambda1, tolerance = 1e-07)
    expect_equal(x1$lambda2, x2$lambda2, tolerance = 1e-07)
    expect_equal(x1$earlyStop, x2$earlyStop)
    expect_equal(x1$hazardRatio, x2$hazardRatio, tolerance = 1e-07)
    expect_equal(x2$analysisTime[1, ], x1$analysisTime[1, ], tolerance = 1e-07)
    expect_equal(x1$studyDuration, x2$studyDuration, tolerance = 1e-07)
    expect_equal(x2$eventsNotAchieved[1, ], x1$eventsNotAchieved[1, ])
    expect_equal(x2$numberOfSubjects[1, ], x1$numberOfSubjects[1, ], tolerance = 1e-07)
    expect_equal(x2$numberOfSubjects1[1, ], x1$numberOfSubjects1[1, ], tolerance = 1e-07)
    expect_equal(x2$numberOfSubjects2[1, ], x1$numberOfSubjects2[1, ], tolerance = 1e-07)
    expect_equal(x2$singleEventsPerStage[1, ], x1$singleEventsPerStage[1, ])
    expect_equal(x2$cumulativeEventsPerStage[1, ], x1$cumulativeEventsPerStage[1, ])
    expect_equal(x1$expectedNumberOfSubjects, x2$expectedNumberOfSubjects, tolerance = 1e-07)
    expect_equal(x2$rejectPerStage[1, ], x1$rejectPerStage[1, ], tolerance = 1e-07)
    expect_equal(x1$overallReject, x2$overallReject, tolerance = 1e-07)
    expect_equal(x2$conditionalPowerAchieved[1, ], x1$conditionalPowerAchieved[1, ])
    expect_equal(x1$maxNumberOfIterations, x3$maxNumberOfIterations)
    expect_equal(x1$seed, x3$seed)
    expect_equal(x1$allocationRatioPlanned, x3$allocationRatioPlanned)
    expect_equal(x1$conditionalPower, x3$conditionalPower)
    expect_equal(x3$iterations[1, ], x1$iterations[1, ])
    expect_equal(x1$futilityStop, x3$futilityStop)
    expect_equal(x1$directionUpper, x3$directionUpper)
    expect_equal(x1$plannedEvents, x3$plannedEvents)
    expect_equal(x1$minNumberOfEventsPerStage, x3$minNumberOfEventsPerStage)
    expect_equal(x1$maxNumberOfEventsPerStage, x3$maxNumberOfEventsPerStage)
    expect_equal(x1$thetaH1, x3$thetaH1)
    expect_equal(x1$expectedNumberOfEvents, x3$expectedNumberOfEvents)
    expect_equal(x1$pi1, x3$pi1, tolerance = 1e-07)
    expect_equal(x1$pi2, x3$pi2, tolerance = 1e-07)
    expect_equal(x1$median1, x3$median1, tolerance = 1e-07)
    expect_equal(x1$median2, x3$median2, tolerance = 1e-07)
    expect_equal(x1$maxNumberOfSubjects, x3$maxNumberOfSubjects)
    expect_equal(x1$accrualTime, x3$accrualTime)
    expect_equal(x1$accrualIntensity, x3$accrualIntensity)
    expect_equal(x1$dropoutRate1, x3$dropoutRate1)
    expect_equal(x1$dropoutRate2, x3$dropoutRate2)
    expect_equal(x1$dropoutTime, x3$dropoutTime)
    expect_equal(x1$eventTime, x3$eventTime)
    expect_equal(x1$thetaH0, x3$thetaH0)
    expect_equal(x1$allocation1, x3$allocation1)
    expect_equal(x1$allocation2, x3$allocation2)
    expect_equal(x1$kappa, x3$kappa)
    expect_equal(x1$piecewiseSurvivalTime, x3$piecewiseSurvivalTime)
    expect_equal(x1$lambda1, x3$lambda1, tolerance = 1e-07)
    expect_equal(x1$lambda2, x3$lambda2, tolerance = 1e-07)
    expect_equal(x1$earlyStop, x3$earlyStop)
    expect_equal(x1$hazardRatio, x3$hazardRatio, tolerance = 1e-07)
    expect_equal(x3$analysisTime[1, ], x1$analysisTime[1, ], tolerance = 1e-07)
    expect_equal(x1$studyDuration, x3$studyDuration, tolerance = 1e-07)
    expect_equal(x3$eventsNotAchieved[1, ], x1$eventsNotAchieved[1, ])
    expect_equal(x3$numberOfSubjects[1, ], x1$numberOfSubjects[1, ], tolerance = 1e-07)
    expect_equal(x3$numberOfSubjects1[1, ], x1$numberOfSubjects1[1, ], tolerance = 1e-07)
    expect_equal(x3$numberOfSubjects2[1, ], x1$numberOfSubjects2[1, ], tolerance = 1e-07)
    expect_equal(x3$singleEventsPerStage[1, ], x1$singleEventsPerStage[1, ])
    expect_equal(x3$cumulativeEventsPerStage[1, ], x1$cumulativeEventsPerStage[1, ])
    expect_equal(x1$expectedNumberOfSubjects, x3$expectedNumberOfSubjects, tolerance = 1e-07)
    expect_equal(x3$rejectPerStage[1, ], x1$rejectPerStage[1, ], tolerance = 1e-07)
    expect_equal(x1$overallReject, x3$overallReject, tolerance = 1e-07)
    expect_equal(x3$conditionalPowerAchieved[1, ], x1$conditionalPowerAchieved[1, ])
    expect_equal(x1$maxNumberOfIterations, x4$maxNumberOfIterations)
    expect_equal(x1$seed, x4$seed)
    expect_equal(x1$allocationRatioPlanned, x4$allocationRatioPlanned)
    expect_equal(x1$conditionalPower, x4$conditionalPower)
    expect_equal(x4$iterations[1, ], x1$iterations[1, ])
    expect_equal(x1$futilityStop, x4$futilityStop)
    expect_equal(x1$directionUpper, x4$directionUpper)
    expect_equal(x1$plannedEvents, x4$plannedEvents)
    expect_equal(x1$minNumberOfEventsPerStage, x4$minNumberOfEventsPerStage)
    expect_equal(x1$maxNumberOfEventsPerStage, x4$maxNumberOfEventsPerStage)
    expect_equal(x1$thetaH1, x4$thetaH1)
    expect_equal(x1$expectedNumberOfEvents, x4$expectedNumberOfEvents)
    expect_equal(x1$pi1, x4$pi1, tolerance = 1e-07)
    expect_equal(x1$pi2, x4$pi2, tolerance = 1e-07)
    expect_equal(x1$median1, x4$median1, tolerance = 1e-07)
    expect_equal(x1$median2, x4$median2, tolerance = 1e-07)
    expect_equal(x1$maxNumberOfSubjects, x4$maxNumberOfSubjects)
    expect_equal(x1$accrualTime, x4$accrualTime)
    expect_equal(x1$accrualIntensity, x4$accrualIntensity)
    expect_equal(x1$dropoutRate1, x4$dropoutRate1)
    expect_equal(x1$dropoutRate2, x4$dropoutRate2)
    expect_equal(x1$dropoutTime, x4$dropoutTime)
    expect_equal(x1$thetaH0, x4$thetaH0)
    expect_equal(x1$allocation1, x4$allocation1)
    expect_equal(x1$allocation2, x4$allocation2)
    expect_equal(x1$kappa, x4$kappa)
    expect_equal(x1$piecewiseSurvivalTime, x4$piecewiseSurvivalTime)
    expect_equal(x1$lambda1, x4$lambda1, tolerance = 1e-07)
    expect_equal(x1$lambda2, x4$lambda2, tolerance = 1e-07)
    expect_equal(x1$earlyStop, x4$earlyStop)
    expect_equal(x1$hazardRatio, x4$hazardRatio, tolerance = 1e-07)
    expect_equal(x4$analysisTime[1, ], x1$analysisTime[1, ], tolerance = 1e-07)
    expect_equal(x1$studyDuration, x4$studyDuration, tolerance = 1e-07)
    expect_equal(x4$eventsNotAchieved[1, ], x1$eventsNotAchieved[1, ])
    expect_equal(x4$numberOfSubjects[1, ], x1$numberOfSubjects[1, ], tolerance = 1e-07)
    expect_equal(x4$numberOfSubjects1[1, ], x1$numberOfSubjects1[1, ], tolerance = 1e-07)
    expect_equal(x4$numberOfSubjects2[1, ], x1$numberOfSubjects2[1, ], tolerance = 1e-07)
    expect_equal(x4$singleEventsPerStage[1, ], x1$singleEventsPerStage[1, ])
    expect_equal(x4$cumulativeEventsPerStage[1, ], x1$cumulativeEventsPerStage[1, ])
    expect_equal(x1$expectedNumberOfSubjects, x4$expectedNumberOfSubjects, tolerance = 1e-07)
    expect_equal(x4$rejectPerStage[1, ], x1$rejectPerStage[1, ], tolerance = 1e-07)
    expect_equal(x1$overallReject, x4$overallReject, tolerance = 1e-07)
    expect_equal(x4$conditionalPowerAchieved[1, ], x1$conditionalPowerAchieved[1, ])
    expect_equal(x1$maxNumberOfIterations, x5$maxNumberOfIterations)
    expect_equal(x1$seed, x5$seed)
    expect_equal(x1$allocationRatioPlanned, x5$allocationRatioPlanned)
    expect_equal(x1$conditionalPower, x5$conditionalPower)
    expect_equal(x5$iterations[1, ], x1$iterations[1, ])
    expect_equal(x1$futilityStop, x5$futilityStop)
    expect_equal(x1$directionUpper, x5$directionUpper)
    expect_equal(x1$plannedEvents, x5$plannedEvents)
    expect_equal(x1$minNumberOfEventsPerStage, x5$minNumberOfEventsPerStage)
    expect_equal(x1$maxNumberOfEventsPerStage, x5$maxNumberOfEventsPerStage)
    expect_equal(x1$thetaH1, x5$thetaH1)
    expect_equal(x1$expectedNumberOfEvents, x5$expectedNumberOfEvents)
    expect_equal(x1$pi1, x5$pi1, tolerance = 1e-07)
    expect_equal(x1$pi2, x5$pi2, tolerance = 1e-07)
    expect_equal(x1$median1, x5$median1, tolerance = 1e-07)
    expect_equal(x1$median2, x5$median2, tolerance = 1e-07)
    expect_equal(x1$maxNumberOfSubjects, x5$maxNumberOfSubjects)
    expect_equal(x1$accrualTime, x5$accrualTime)
    expect_equal(x1$accrualIntensity, x5$accrualIntensity)
    expect_equal(x1$dropoutRate1, x5$dropoutRate1)
    expect_equal(x1$dropoutRate2, x5$dropoutRate2)
    expect_equal(x1$dropoutTime, x5$dropoutTime)
    expect_equal(x1$eventTime, x5$eventTime)
    expect_equal(x1$thetaH0, x5$thetaH0)
    expect_equal(x1$allocation1, x5$allocation1)
    expect_equal(x1$allocation2, x5$allocation2)
    expect_equal(x1$kappa, x5$kappa)
    expect_equal(x1$piecewiseSurvivalTime, x5$piecewiseSurvivalTime)
    expect_equal(x1$lambda1, x5$lambda1, tolerance = 1e-07)
    expect_equal(x1$lambda2, x5$lambda2, tolerance = 1e-07)
    expect_equal(x1$earlyStop, x5$earlyStop)
    expect_equal(x1$hazardRatio, x5$hazardRatio, tolerance = 1e-07)
    expect_equal(x5$analysisTime[1, ], x1$analysisTime[1, ], tolerance = 1e-07)
    expect_equal(x1$studyDuration, x5$studyDuration, tolerance = 1e-07)
    expect_equal(x5$eventsNotAchieved[1, ], x1$eventsNotAchieved[1, ])
    expect_equal(x5$numberOfSubjects[1, ], x1$numberOfSubjects[1, ], tolerance = 1e-07)
    expect_equal(x5$numberOfSubjects1[1, ], x1$numberOfSubjects1[1, ], tolerance = 1e-07)
    expect_equal(x5$numberOfSubjects2[1, ], x1$numberOfSubjects2[1, ], tolerance = 1e-07)
    expect_equal(x5$singleEventsPerStage[1, ], x1$singleEventsPerStage[1, ])
    expect_equal(x5$cumulativeEventsPerStage[1, ], x1$cumulativeEventsPerStage[1, ])
    expect_equal(x1$expectedNumberOfSubjects, x5$expectedNumberOfSubjects, tolerance = 1e-07)
    expect_equal(x5$rejectPerStage[1, ], x1$rejectPerStage[1, ], tolerance = 1e-07)
    expect_equal(x1$overallReject, x5$overallReject, tolerance = 1e-07)
    expect_equal(x5$conditionalPowerAchieved[1, ], x1$conditionalPowerAchieved[1, ])
    expect_equal(x1$maxNumberOfIterations, x6$maxNumberOfIterations)
    expect_equal(x1$seed, x6$seed)
    expect_equal(x1$allocationRatioPlanned, x6$allocationRatioPlanned)
    expect_equal(x1$conditionalPower, x6$conditionalPower)
    expect_equal(x6$iterations[1, ], x1$iterations[1, ])
    expect_equal(x1$futilityStop, x6$futilityStop)
    expect_equal(x1$directionUpper, x6$directionUpper)
    expect_equal(x1$plannedEvents, x6$plannedEvents)
    expect_equal(x1$minNumberOfEventsPerStage, x6$minNumberOfEventsPerStage)
    expect_equal(x1$maxNumberOfEventsPerStage, x6$maxNumberOfEventsPerStage)
    expect_equal(x1$thetaH1, x6$thetaH1)
    expect_equal(x1$expectedNumberOfEvents, x6$expectedNumberOfEvents)
    expect_equal(x1$pi1, x6$pi1, tolerance = 1e-07)
    expect_equal(x1$pi2, x6$pi2, tolerance = 1e-07)
    expect_equal(x1$median1, x6$median1, tolerance = 1e-07)
    expect_equal(x1$median2, x6$median2, tolerance = 1e-07)
    expect_equal(x1$maxNumberOfSubjects, x6$maxNumberOfSubjects)
    expect_equal(x1$accrualTime, x6$accrualTime)
    expect_equal(x1$accrualIntensity, x6$accrualIntensity)
    expect_equal(x1$dropoutRate1, x6$dropoutRate1)
    expect_equal(x1$dropoutRate2, x6$dropoutRate2)
    expect_equal(x1$dropoutTime, x6$dropoutTime)
    expect_equal(x1$eventTime, x6$eventTime)
    expect_equal(x1$thetaH0, x6$thetaH0)
    expect_equal(x1$allocation1, x6$allocation1)
    expect_equal(x1$allocation2, x6$allocation2)
    expect_equal(x1$kappa, x6$kappa)
    expect_equal(x1$piecewiseSurvivalTime, x6$piecewiseSurvivalTime)
    expect_equal(x1$lambda1, x6$lambda1, tolerance = 1e-07)
    expect_equal(x1$lambda2, x6$lambda2, tolerance = 1e-07)
    expect_equal(x1$earlyStop, x6$earlyStop)
    expect_equal(x1$hazardRatio, x6$hazardRatio, tolerance = 1e-07)
    expect_equal(x6$analysisTime[1, ], x1$analysisTime[1, ], tolerance = 1e-07)
    expect_equal(x1$studyDuration, x6$studyDuration, tolerance = 1e-07)
    expect_equal(x6$eventsNotAchieved[1, ], x1$eventsNotAchieved[1, ])
    expect_equal(x6$numberOfSubjects[1, ], x1$numberOfSubjects[1, ], tolerance = 1e-07)
    expect_equal(x6$numberOfSubjects1[1, ], x1$numberOfSubjects1[1, ], tolerance = 1e-07)
    expect_equal(x6$numberOfSubjects2[1, ], x1$numberOfSubjects2[1, ], tolerance = 1e-07)
    expect_equal(x6$singleEventsPerStage[1, ], x1$singleEventsPerStage[1, ])
    expect_equal(x6$cumulativeEventsPerStage[1, ], x1$cumulativeEventsPerStage[1, ])
    expect_equal(x1$expectedNumberOfSubjects, x6$expectedNumberOfSubjects, tolerance = 1e-07)
    expect_equal(x6$rejectPerStage[1, ], x1$rejectPerStage[1, ], tolerance = 1e-07)
    expect_equal(x1$overallReject, x6$overallReject, tolerance = 1e-07)
    expect_equal(x6$conditionalPowerAchieved[1, ], x1$conditionalPowerAchieved[1, ])
    expect_equal(x1$maxNumberOfIterations, x7$maxNumberOfIterations)
    expect_equal(x1$seed, x7$seed)
    expect_equal(x1$allocationRatioPlanned, x7$allocationRatioPlanned)
    expect_equal(x1$conditionalPower, x7$conditionalPower)
    expect_equal(x7$iterations[1, ], x1$iterations[1, ])
    expect_equal(x1$futilityStop, x7$futilityStop)
    expect_equal(x1$directionUpper, x7$directionUpper)
    expect_equal(x1$plannedEvents, x7$plannedEvents)
    expect_equal(x1$minNumberOfEventsPerStage, x7$minNumberOfEventsPerStage)
    expect_equal(x1$maxNumberOfEventsPerStage, x7$maxNumberOfEventsPerStage)
    expect_equal(x1$thetaH1, x7$thetaH1)
    expect_equal(x1$expectedNumberOfEvents, x7$expectedNumberOfEvents)
    expect_equal(x1$pi1, x7$pi1, tolerance = 1e-07)
    expect_equal(x1$pi2, x7$pi2, tolerance = 1e-07)
    expect_equal(x1$median1, x7$median1, tolerance = 1e-07)
    expect_equal(x1$median2, x7$median2, tolerance = 1e-07)
    expect_equal(x1$maxNumberOfSubjects, x7$maxNumberOfSubjects)
    expect_equal(x1$accrualTime, x7$accrualTime)
    expect_equal(x1$accrualIntensity, x7$accrualIntensity)
    expect_equal(x1$dropoutRate1, x7$dropoutRate1)
    expect_equal(x1$dropoutRate2, x7$dropoutRate2)
    expect_equal(x1$dropoutTime, x7$dropoutTime)
    expect_equal(x1$eventTime, x7$eventTime)
    expect_equal(x1$thetaH0, x7$thetaH0)
    expect_equal(x1$allocation1, x7$allocation1)
    expect_equal(x1$allocation2, x7$allocation2)
    expect_equal(x1$kappa, x7$kappa)
    expect_equal(x1$piecewiseSurvivalTime, x7$piecewiseSurvivalTime)
    expect_equal(x1$lambda1, x7$lambda1, tolerance = 1e-07)
    expect_equal(x1$lambda2, x7$lambda2, tolerance = 1e-07)
    expect_equal(x1$earlyStop, x7$earlyStop)
    expect_equal(x1$hazardRatio, x7$hazardRatio, tolerance = 1e-07)
    expect_equal(x7$analysisTime[1, ], x1$analysisTime[1, ], tolerance = 1e-07)
    expect_equal(x1$studyDuration, x7$studyDuration, tolerance = 1e-07)
    expect_equal(x7$eventsNotAchieved[1, ], x1$eventsNotAchieved[1, ])
    expect_equal(x7$numberOfSubjects[1, ], x1$numberOfSubjects[1, ], tolerance = 1e-07)
    expect_equal(x7$numberOfSubjects1[1, ], x1$numberOfSubjects1[1, ], tolerance = 1e-07)
    expect_equal(x7$numberOfSubjects2[1, ], x1$numberOfSubjects2[1, ], tolerance = 1e-07)
    expect_equal(x7$singleEventsPerStage[1, ], x1$singleEventsPerStage[1, ])
    expect_equal(x7$cumulativeEventsPerStage[1, ], x1$cumulativeEventsPerStage[1, ])
    expect_equal(x1$expectedNumberOfSubjects, x7$expectedNumberOfSubjects, tolerance = 1e-07)
    expect_equal(x7$rejectPerStage[1, ], x1$rejectPerStage[1, ], tolerance = 1e-07)
    expect_equal(x1$overallReject, x7$overallReject, tolerance = 1e-07)
    expect_equal(x7$conditionalPowerAchieved[1, ], x1$conditionalPowerAchieved[1, ])
})

test_that("'getSimulationSurvival': Confirm that different definitions of delayed response with the identical meaning result in the same output", {
    .skipTestIfDisabled()

    x1 <- getSimulationSurvival(
        piecewiseSurvivalTime = c(0, 6),
        lambda2 = c(1.7, 1.2),
        hazardRatio = c(0.65, 0.7),
        plannedEvents = 98,
        maxNumberOfSubjects = 120,
        directionUpper = FALSE,
        maxNumberOfIterations = 1000,
        sided = 1, alpha = 0.1, seed = 123
    )

    x2 <- getSimulationSurvival(
        piecewiseSurvivalTime = list("<6" = 1.7, "6 - Inf" = 1.2),
        hazardRatio = c(0.65, 0.7),
        plannedEvents = 98,
        maxNumberOfSubjects = 120,
        directionUpper = FALSE,
        maxNumberOfIterations = 1000,
        sided = 1, alpha = 0.1, seed = 123
    )


    ## Pairwise comparison of the results of x1 with the results of x2
    expect_equal(x1$maxNumberOfIterations, x2$maxNumberOfIterations)
    expect_equal(x1$seed, x2$seed)
    expect_equal(x1$allocationRatioPlanned, x2$allocationRatioPlanned)
    expect_equal(x1$conditionalPower, x2$conditionalPower)
    expect_equal(x2$iterations[1, ], x1$iterations[1, ])
    expect_equal(x1$futilityStop, x2$futilityStop)
    expect_equal(x1$directionUpper, x2$directionUpper)
    expect_equal(x1$plannedEvents, x2$plannedEvents)
    expect_equal(x1$minNumberOfEventsPerStage, x2$minNumberOfEventsPerStage)
    expect_equal(x1$maxNumberOfEventsPerStage, x2$maxNumberOfEventsPerStage)
    expect_equal(x1$thetaH1, x2$thetaH1)
    expect_equal(x1$expectedNumberOfEvents, x2$expectedNumberOfEvents)
    expect_equal(x1$pi1, x2$pi1)
    expect_equal(x1$pi2, x2$pi2)
    expect_equal(x1$median1, x2$median1)
    expect_equal(x1$median2, x2$median2)
    expect_equal(x1$maxNumberOfSubjects, x2$maxNumberOfSubjects)
    expect_equal(x1$accrualTime, x2$accrualTime)
    expect_equal(x1$accrualIntensity, x2$accrualIntensity)
    expect_equal(x1$dropoutRate1, x2$dropoutRate1)
    expect_equal(x1$dropoutRate2, x2$dropoutRate2)
    expect_equal(x1$dropoutTime, x2$dropoutTime)
    expect_equal(x1$eventTime, x2$eventTime)
    expect_equal(x1$thetaH0, x2$thetaH0)
    expect_equal(x1$allocation1, x2$allocation1)
    expect_equal(x1$allocation2, x2$allocation2)
    expect_equal(x1$kappa, x2$kappa)
    expect_equal(x1$piecewiseSurvivalTime, x2$piecewiseSurvivalTime)
    expect_equal(x1$lambda1, x2$lambda1, tolerance = 1e-07)
    expect_equal(x1$lambda2, x2$lambda2, tolerance = 1e-07)
    expect_equal(x1$earlyStop, x2$earlyStop)
    expect_equal(x1$hazardRatio, x2$hazardRatio, tolerance = 1e-07)
    expect_equal(x2$analysisTime[1, ], x1$analysisTime[1, ], tolerance = 1e-07)
    expect_equal(x1$studyDuration, x2$studyDuration, tolerance = 1e-07)
    expect_equal(x2$eventsNotAchieved[1, ], x1$eventsNotAchieved[1, ])
    expect_equal(x2$numberOfSubjects[1, ], x1$numberOfSubjects[1, ], tolerance = 1e-07)
    expect_equal(x2$numberOfSubjects1[1, ], x1$numberOfSubjects1[1, ], tolerance = 1e-07)
    expect_equal(x2$numberOfSubjects2[1, ], x1$numberOfSubjects2[1, ], tolerance = 1e-07)
    expect_equal(x2$singleEventsPerStage[1, ], x1$singleEventsPerStage[1, ])
    expect_equal(x2$cumulativeEventsPerStage[1, ], x1$cumulativeEventsPerStage[1, ])
    expect_equal(x1$expectedNumberOfSubjects, x2$expectedNumberOfSubjects, tolerance = 1e-07)
    expect_equal(x2$rejectPerStage[1, ], x1$rejectPerStage[1, ], tolerance = 1e-07)
    expect_equal(x1$overallReject, x2$overallReject, tolerance = 1e-07)
    expect_equal(x2$conditionalPowerAchieved[1, ], x1$conditionalPowerAchieved[1, ])
})

test_that("'getSimulationSurvival': Confirm that the function works correctly with a user defined 'calcEventsFunction'", {
    design <- getDesignInverseNormal(futilityBounds = c(0.5, 0.5))

    myCalcEventsFunction <- function(...,
            stage, conditionalPower, thetaH0, estimatedTheta,
            plannedEvents, eventsOverStages, minNumberOfEventsPerStage, maxNumberOfEventsPerStage,
            allocationRatioPlanned, conditionalCriticalValue) {
        theta <- max(1 + 1e-12, estimatedTheta)

        requiredStageEvents <- max(0, conditionalCriticalValue + qnorm(conditionalPower))^2 *
            (1 + allocationRatioPlanned)^2 / (allocationRatioPlanned) / log(theta / thetaH0)^2

        requiredStageEvents <- min(
            max(minNumberOfEventsPerStage[stage], requiredStageEvents),
            maxNumberOfEventsPerStage[stage]
        ) + eventsOverStages[stage - 1]

        return(requiredStageEvents)
    }

    simResults <- getSimulationSurvival(
        design = design,
        directionUpper = FALSE, maxNumberOfSubjects = 260, plannedEvents = (1:design$kMax) * 20,
        allocation1 = 1, allocation2 = 1, accrualTime = c(0, 3, 6, 12),
        lambda2 = 0.03, hazardRatio = c(0.75, 0.8, 0.9),
        accrualIntensity = c(0.1, 0.2, 0.2), dropoutRate1 = 0, dropoutRate2 = 0,
        dropoutTime = 12, conditionalPower = 0.8, minNumberOfEventsPerStage = c(NA_real_, 10, 10),
        maxNumberOfEventsPerStage = c(NA_real_, 100, 100),
        maxNumberOfIterations = 10,
        seed = 1234567890,
        calcEventsFunction = myCalcEventsFunction
    )


    ## Comparison of the results of SimulationResultsSurvival object 'simResults' with expected results
    expect_equal(simResults$pi1, c(0.23662051, 0.25023841, 0.27674976), tolerance = 1e-07, label = paste0(simResults$pi1))
    expect_equal(simResults$pi2, 0.30232367, tolerance = 1e-07, label = paste0(simResults$pi2))
    expect_equal(simResults$median1, c(30.806541, 28.881133, 25.672118), tolerance = 1e-07, label = paste0(simResults$median1))
    expect_equal(simResults$median2, 23.104906, tolerance = 1e-07, label = paste0(simResults$median2))
    expect_equal(simResults$accrualIntensity, c(12.380952, 24.761905, 24.761905), tolerance = 1e-07, label = paste0(simResults$accrualIntensity))
    expect_equal(simResults$lambda1, c(0.0225, 0.024, 0.027), tolerance = 1e-07, label = paste0(simResults$lambda1))
    expect_equal(simResults$analysisTime[1, ], c(9.5194269, 8.9064508, 9.2223494), tolerance = 1e-07, label = paste0(simResults$analysisTime[1, ]))
    expect_equal(simResults$analysisTime[2, ], c(27.784104, 21.407573, 24.009249), tolerance = 1e-07, label = paste0(simResults$analysisTime[2, ]))
    expect_equal(simResults$analysisTime[3, ], c(55.740006, 35.109064, 70.374189), tolerance = 1e-07, label = paste0(simResults$analysisTime[3, ]))
    expect_equal(simResults$studyDuration, c(34.589904, 24.072277, 24.081522), tolerance = 1e-07, label = paste0(simResults$studyDuration))
    expect_equal(simResults$eventsNotAchieved[1, ], c(0, 0, 0), label = paste0(simResults$eventsNotAchieved[1, ]))
    expect_equal(simResults$eventsNotAchieved[2, ], c(0, 0, 0), label = paste0(simResults$eventsNotAchieved[2, ]))
    expect_equal(simResults$eventsNotAchieved[3, ], c(0, 0, 0), label = paste0(simResults$eventsNotAchieved[3, ]))
    expect_equal(simResults$numberOfSubjects[1, ], c(198.3, 183, 190.8), tolerance = 1e-07, label = paste0(simResults$numberOfSubjects[1, ]))
    expect_equal(simResults$numberOfSubjects[2, ], c(260, 256, 260), label = paste0(simResults$numberOfSubjects[2, ]))
    expect_equal(simResults$numberOfSubjects[3, ], c(260, 260, 260), label = paste0(simResults$numberOfSubjects[3, ]))
    expect_equal(simResults$cumulativeEventsPerStage[1, ], c(20, 20, 20), label = paste0(simResults$cumulativeEventsPerStage[1, ]))
    expect_equal(simResults$cumulativeEventsPerStage[2, ], c(105.33333, 84.833333, 97), tolerance = 1e-07, label = paste0(simResults$cumulativeEventsPerStage[2, ]))
    expect_equal(simResults$cumulativeEventsPerStage[3, ], c(174.53333, 142.63333, 197), tolerance = 1e-07, label = paste0(simResults$cumulativeEventsPerStage[3, ]))
    expect_equal(simResults$iterations[1, ], c(10, 10, 10), label = paste0(simResults$iterations[1, ]))
    expect_equal(simResults$iterations[2, ], c(6, 6, 4), label = paste0(simResults$iterations[2, ]))
    expect_equal(simResults$iterations[3, ], c(5, 5, 2), label = paste0(simResults$iterations[3, ]))
    expect_equal(simResults$overallReject, c(0.2, 0.2, 0.1), tolerance = 1e-07, label = paste0(simResults$overallReject))
    expect_equal(simResults$rejectPerStage[1, ], c(0, 0, 0), label = paste0(simResults$rejectPerStage[1, ]))
    expect_equal(simResults$rejectPerStage[2, ], c(0.1, 0, 0.1), tolerance = 1e-07, label = paste0(simResults$rejectPerStage[2, ]))
    expect_equal(simResults$rejectPerStage[3, ], c(0.1, 0.2, 0), tolerance = 1e-07, label = paste0(simResults$rejectPerStage[3, ]))
    expect_equal(simResults$futilityStop, c(0.4, 0.5, 0.7), tolerance = 1e-07, label = paste0(simResults$futilityStop))
    expect_equal(simResults$futilityPerStage[1, ], c(0.4, 0.4, 0.6), tolerance = 1e-07, label = paste0(simResults$futilityPerStage[1, ]))
    expect_equal(simResults$futilityPerStage[2, ], c(0, 0.1, 0.1), tolerance = 1e-07, label = paste0(simResults$futilityPerStage[2, ]))
    expect_equal(simResults$earlyStop, c(0.5, 0.5, 0.8), tolerance = 1e-07, label = paste0(simResults$earlyStop))
    expect_equal(simResults$expectedNumberOfSubjects, c(235.32, 228.8, 218.48), tolerance = 1e-07, label = paste0(simResults$expectedNumberOfSubjects))
    expect_equal(simResults$expectedNumberOfEvents, c(105.8, 87.8, 70.8), tolerance = 1e-07, label = paste0(simResults$expectedNumberOfEvents))
    expect_equal(simResults$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_), label = paste0(simResults$conditionalPowerAchieved[1, ]))
    expect_equal(simResults$conditionalPowerAchieved[2, ], c(0.27873409, 0.6677915, 0.61838719), tolerance = 1e-07, label = paste0(simResults$conditionalPowerAchieved[2, ]))
    expect_equal(simResults$conditionalPowerAchieved[3, ], c(0.71007174, 0.61706552, 0.10609026), tolerance = 1e-07, label = paste0(simResults$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(simResults), NA)))
        expect_output(print(simResults)$show())
        invisible(capture.output(expect_error(summary(simResults), NA)))
        expect_output(summary(simResults)$show())
        simResultsCodeBased <- eval(parse(text = getObjectRCode(simResults, stringWrapParagraphWidth = NULL)))
        expect_equal(simResultsCodeBased$pi1, simResults$pi1, tolerance = 1e-07)
        expect_equal(simResultsCodeBased$pi2, simResults$pi2, tolerance = 1e-07)
        expect_equal(simResultsCodeBased$median1, simResults$median1, tolerance = 1e-07)
        expect_equal(simResultsCodeBased$median2, simResults$median2, tolerance = 1e-07)
        expect_equal(simResultsCodeBased$accrualIntensity, simResults$accrualIntensity, tolerance = 1e-07)
        expect_equal(simResultsCodeBased$lambda1, simResults$lambda1, tolerance = 1e-07)
        expect_equal(simResultsCodeBased$analysisTime, simResults$analysisTime, tolerance = 1e-07)
        expect_equal(simResultsCodeBased$studyDuration, simResults$studyDuration, tolerance = 1e-07)
        expect_equal(simResultsCodeBased$eventsNotAchieved, simResults$eventsNotAchieved, tolerance = 1e-07)
        expect_equal(simResultsCodeBased$numberOfSubjects, simResults$numberOfSubjects, tolerance = 1e-07)
        expect_equal(simResultsCodeBased$cumulativeEventsPerStage, simResults$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(simResultsCodeBased$iterations, simResults$iterations, tolerance = 1e-07)
        expect_equal(simResultsCodeBased$overallReject, simResults$overallReject, tolerance = 1e-07)
        expect_equal(simResultsCodeBased$rejectPerStage, simResults$rejectPerStage, tolerance = 1e-07)
        expect_equal(simResultsCodeBased$futilityStop, simResults$futilityStop, tolerance = 1e-07)
        expect_equal(simResultsCodeBased$futilityPerStage, simResults$futilityPerStage, tolerance = 1e-07)
        expect_equal(simResultsCodeBased$earlyStop, simResults$earlyStop, tolerance = 1e-07)
        expect_equal(simResultsCodeBased$expectedNumberOfSubjects, simResults$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(simResultsCodeBased$expectedNumberOfEvents, simResults$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(simResultsCodeBased$conditionalPowerAchieved, simResults$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(simResults), "character")
        df <- as.data.frame(simResults)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(simResults)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})
