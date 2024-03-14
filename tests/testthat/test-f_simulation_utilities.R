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

test_plan_section("Testing Simulation Utilities")

test_that("Set invalid seed", {
    .skipTestIfDisabled()

    expect_error(.setSeed(""), "Illegal argument: 'seed' must be a valid integer")
})

test_that("Get simulation parameters from raw data", {
    .skipTestIfDisabled()

    expect_error(.getSimulationParametersFromRawData(1))

    simulationResults <- getSimulationSurvival(
        maxNumberOfSubjects = 200, plannedEvents = 50,
        accrualTime = c(0, 3, 6, 12), accrualIntensity = c(0.1, 0.2, 0.2),
        maxNumberOfIterations = 10, seed = 1234567890
    )

    ## Comparison of the results of list object 'simulationResults$getRawDataResults()' with expected results
    expect_equal(simulationResults$getRawDataResults()$sampleSizes[1, ], c(200, 200, 200, 200), label = paste0(simulationResults$getRawDataResults()$sampleSizes[1, ]))
    expect_equal(simulationResults$getRawDataResults()$rejectPerStage[1, ], c(0, 0.4, 0.9, 1), tolerance = 1e-07, label = paste0(simulationResults$getRawDataResults()$rejectPerStage[1, ]))
    expect_equal(simulationResults$getRawDataResults()$overallReject, c(0, 0.4, 0.9, 1), tolerance = 1e-07, label = paste0(simulationResults$getRawDataResults()$overallReject))
    expect_equal(simulationResults$getRawDataResults()$futilityStop, c(0, 0, 0, 0), label = paste0(simulationResults$getRawDataResults()$futilityStop))
    expect_equal(simulationResults$getRawDataResults()$iterations[1, ], c("0.2" = 10, "0.3" = 10, "0.4" = 10, "0.5" = 10), label = paste0(simulationResults$getRawDataResults()$iterations[1, ]))
    expect_equal(simulationResults$getRawDataResults()$earlyStop, c(0, 0, 0, 0), label = paste0(simulationResults$getRawDataResults()$earlyStop))
    expect_equal(simulationResults$getRawDataResults()$expectedNumberOfSubjects, c(200, 200, 200, 200), label = paste0(simulationResults$getRawDataResults()$expectedNumberOfSubjects))
    expect_equal(simulationResults$getRawDataResults()$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(simulationResults$getRawDataResults()$conditionalPowerAchieved[1, ]))


    variantName <- simulationResults$.getVariedParameterName()
    expect_equal(variantName, "pi1")

    simParams <- .getSimulationParametersFromRawData(simulationResults, variantName = variantName)

    ## Comparison of the results of list object 'simParams' with expected results
    expect_equal(simParams$sampleSizes[1, ], c(200, 200, 200, 200), label = paste0(simParams$sampleSizes[1, ]))
    expect_equal(simParams$rejectPerStage[1, ], c(0, 0.4, 0.9, 1), tolerance = 1e-07, label = paste0(simParams$rejectPerStage[1, ]))
    expect_equal(simParams$overallReject, c(0, 0.4, 0.9, 1), tolerance = 1e-07, label = paste0(simParams$overallReject))
    expect_equal(simParams$futilityStop, c(0, 0, 0, 0), label = paste0(simParams$futilityStop))
    expect_equal(simParams$iterations[1, ], c("0.2" = 10, "0.3" = 10, "0.4" = 10, "0.5" = 10), label = paste0(simParams$iterations[1, ]))
    expect_equal(simParams$earlyStop, c(0, 0, 0, 0), label = paste0(simParams$earlyStop))
    expect_equal(simParams$expectedNumberOfSubjects, c(200, 200, 200, 200), label = paste0(simParams$expectedNumberOfSubjects))
    expect_equal(simParams$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(simParams$conditionalPowerAchieved[1, ]))
})

test_that("Get enrichment effect data", {
    .skipTestIfDisabled()

    expect_null(.getEffectData(effectList = NULL))

    expect_error(.getEffectData(effectList = NULL, nullAllowed = FALSE), "Illegal argument: 'effectList' must be a non-empty list")

    expect_error(.getEffectData(effectList = list(1, 2)), "Illegal argument: 'effectList' must be named. Current names are NULL")

    expect_error(.getEffectData(effectList = list(a = 1, b = 2)), "Illegal argument: 'effectList' must contain 'subGroups'")

    expect_error(.getEffectData(effectList = list(subGroups = 1, b = 2)),
        "Illegal argument: 'effectList$subGroups' must be a non-empty character vector or factor",
        fixed = TRUE
    )

    expect_error(.getEffectData(effectList = list(subGroups = "1", b = 2)),
        "Illegal argument: 'effectList$subGroups' must contain \"F\"",
        fixed = TRUE
    )

    effects <- matrix(c(0, 0, 0, 0.25, 0.25, 0.25, 0.5, 0.5, 0.5, 0, 0.25, 0.5, 0, 0.25, 0.5, 0, 0.25, 0.5), ncol = 2)

    effectList <- list(
        subGroups = c("S", "R"),
        prevalences = c(0.2, 0.8),
        stDevs = 0.8,
        effects = effects
    )

    expect_true(is.data.frame(.getEffectData(effectList = effectList)))
})

test_that("Get simulation raw data", {
    .skipTestIfDisabled()

    simulationResults <- getSimulationSurvival(
        maxNumberOfSubjects = 200, plannedEvents = 50,
        accrualTime = c(0, 3, 6, 12), accrualIntensity = c(0.1, 0.2, 0.2),
        maxNumberOfIterations = 10, seed = 1234567890
    )

    dat <- getData(simulationResults)
    expect_true(is.data.frame(dat))

    ## Comparison of the results of data.frame object 'dat' with expected results
    expect_equal(dat$iterationNumber, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), label = paste0(dat$iterationNumber))
    expect_equal(dat$stageNumber, c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), label = paste0(dat$stageNumber))
    expect_equal(dat$pi1, c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5), tolerance = 1e-07, label = paste0(dat$pi1))
    expect_equal(dat$pi2, c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2), tolerance = 1e-07, label = paste0(dat$pi2))
    expect_equal(dat$hazardRatio, c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1.5984103, 1.5984103, 1.5984103, 1.5984103, 1.5984103, 1.5984103, 1.5984103, 1.5984103, 1.5984103, 1.5984103, 2.2892242, 2.2892242, 2.2892242, 2.2892242, 2.2892242, 2.2892242, 2.2892242, 2.2892242, 2.2892242, 2.2892242, 3.1062837, 3.1062837, 3.1062837, 3.1062837, 3.1062837, 3.1062837, 3.1062837, 3.1062837, 3.1062837, 3.1062837), tolerance = 1e-07, label = paste0(dat$hazardRatio))
    expect_equal(dat$analysisTime, c(20.514834, 24.084587, 21.080477, 26.19736, 20.816485, 22.486086, 18.042946, 24.639412, 18.751854, 20.489993, 19.056033, 20.306068, 19.783043, 19.969414, 19.134895, 19.383827, 19.207207, 18.404971, 17.612017, 17.532565, 17.941348, 17.45093, 16.319552, 14.464684, 18.114825, 15.360217, 16.475543, 15.624453, 15.133018, 14.790423, 14.632174, 14.553606, 15.674248, 15.03612, 14.73305, 17.260704, 16.18145, 15.132037, 14.48952, 14.35598), tolerance = 1e-07, label = paste0(dat$analysisTime))
    expect_equal(dat$numberOfSubjects, c(200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200), label = paste0(dat$numberOfSubjects))
    expect_equal(dat$overallEvents1, c(20, 29, 25, 25, 29, 26, 25, 28, 25, 23, 29, 26, 30, 30, 28, 33, 32, 28, 34, 34, 39, 33, 32, 28, 35, 34, 34, 36, 31, 32, 39, 39, 34, 39, 38, 37, 34, 39, 37, 31), label = paste0(dat$overallEvents1))
    expect_equal(dat$overallEvents2, c(30, 21, 25, 25, 21, 24, 25, 22, 25, 27, 21, 24, 20, 20, 22, 17, 18, 22, 16, 16, 11, 17, 18, 22, 15, 16, 16, 14, 19, 18, 11, 11, 16, 11, 12, 13, 16, 11, 13, 19), label = paste0(dat$overallEvents2))
    expect_equal(dat$eventsPerStage, c(50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50), label = paste0(dat$eventsPerStage))
    expect_equal(dat$rejectPerStage, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), label = paste0(dat$rejectPerStage))
    expect_equal(dat$eventsNotAchieved, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), label = paste0(dat$eventsNotAchieved))
    expect_equal(dat$futilityPerStage, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), label = paste0(dat$futilityPerStage))
    expect_equal(dat$testStatistic, c(-1.5720981, 1.3369311, -0.18337222, 0.16781459, 1.3209188, 0.34145802, -0.022511768, 1.1050441, -0.06354815, -0.67363591, 1.1763986, 0.32983704, 1.6220522, 1.5273371, 0.8565017, 2.598005, 2.4684197, 0.70495618, 2.9492281, 2.9129406, 4.4733105, 2.5179861, 2.3188569, 0.90475344, 3.2252453, 2.8475978, 2.7676792, 3.5248633, 2.0301764, 2.1786778, 4.8135874, 4.7805904, 2.9145875, 4.5655884, 4.4275555, 4.0089856, 2.9786445, 4.6679518, 4.2300453, 1.9980206), tolerance = 1e-07, label = paste0(dat$testStatistic))
    expect_equal(dat$logRankStatistic, c(-1.5720981, 1.3369311, -0.18337222, 0.16781459, 1.3209188, 0.34145802, -0.022511768, 1.1050441, -0.06354815, -0.67363591, 1.1763986, 0.32983704, 1.6220522, 1.5273371, 0.8565017, 2.598005, 2.4684197, 0.70495618, 2.9492281, 2.9129406, 4.4733105, 2.5179861, 2.3188569, 0.90475344, 3.2252453, 2.8475978, 2.7676792, 3.5248633, 2.0301764, 2.1786778, 4.8135874, 4.7805904, 2.9145875, 4.5655884, 4.4275555, 4.0089856, 2.9786445, 4.6679518, 4.2300453, 1.9980206), tolerance = 1e-07, label = paste0(dat$logRankStatistic))
    expect_equal(dat$conditionalPowerAchieved, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(dat$conditionalPowerAchieved))
    expect_equal(dat$pValuesSeparate, c(0.9420361, 0.090622564, 0.57274702, 0.43336457, 0.093264215, 0.3663794, 0.50898014, 0.13457024, 0.52533499, 0.74972859, 0.11971782, 0.37076155, 0.05239609, 0.063338599, 0.19586017, 0.004688357, 0.006785555, 0.24041875, 0.001592844, 0.0017902134, 3.8508903e-06, 0.0059013988, 0.010201397, 0.18279801, 0.0006293238, 0.0022025282, 0.0028228496, 0.00021185069, 0.021169308, 0.014677805, 7.4122368e-07, 8.7390559e-07, 0.001780795, 2.4904766e-06, 4.7653534e-06, 3.0490064e-05, 0.0014476325, 1.5210865e-06, 1.1682211e-05, 0.022857216), tolerance = 1e-07, label = paste0(dat$pValuesSeparate))
    expect_equal(dat$trialStop, c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), label = paste0(dat$trialStop))
    expect_equal(dat$hazardRatioEstimateLR, c(0.64104444, 1.459569, 0.94945656, 1.0486096, 1.4529737, 1.1013965, 0.99365294, 1.3669113, 0.98218644, 0.82651847, 1.3947787, 1.0977822, 1.5821515, 1.5403292, 1.2741194, 2.0851208, 2.01008, 1.22066, 2.3028965, 2.2793813, 3.5439548, 2.0384588, 1.9268216, 1.2916273, 2.4898865, 2.2376412, 2.1876281, 2.7100901, 1.775746, 1.8519204, 3.9019956, 3.8657479, 2.2804432, 3.6376701, 3.4983863, 3.1077916, 2.322137, 3.7445301, 3.30831, 1.7596687), tolerance = 1e-07, label = paste0(dat$hazardRatioEstimateLR))

    expect_error(getRawData(1))

    expect_error(
        getRawData(simulationResults),
        "^Illegal argument\\: simulation results contain no raw data"
    )

    simulationResults2 <- getSimulationSurvival(
        maxNumberOfSubjects = 200, plannedEvents = 50,
        accrualTime = c(0, 3, 6, 12), accrualIntensity = c(0.1, 0.2, 0.2),
        maxNumberOfIterations = 1, seed = 1234567890,
        maxNumberOfRawDatasetsPerStage = 2
    )
    rawDat <- getRawData(simulationResults2)
    expect_true(is.data.frame(rawDat))
    expect_equal(dim(rawDat), c(400, 13))

    simulationResults3 <- getSimulationSurvival(
        maxNumberOfSubjects = 200, plannedEvents = 50,
        accrualTime = c(0, 3, 6, 12), accrualIntensity = c(0.1, 0.2, 0.2),
        maxNumberOfIterations = 100, seed = 1234567890,
        maxNumberOfRawDatasetsPerStage = 3
    )
    rawDat2 <- getRawData(simulationResults3, aggregate = TRUE)
    expect_true(is.data.frame(rawDat2))

    ## Comparison of the results of data.frame object 'rawDat2' with expected results
    expect_equal(rawDat2$iterationNumber, c(1, 2, 3), label = paste0(rawDat2$iterationNumber))
    expect_equal(rawDat2$pi1, c(0.2, 0.2, 0.2), tolerance = 1e-07, label = paste0(rawDat2$pi1))
    expect_equal(rawDat2$stageNumber, c(1, 1, 1), label = paste0(rawDat2$stageNumber))
    expect_equal(rawDat2$analysisTime, c(20.514834, 24.084587, 21.080477), tolerance = 1e-07, label = paste0(rawDat2$analysisTime))
    expect_equal(rawDat2$numberOfSubjects, c(200, 200, 200), label = paste0(rawDat2$numberOfSubjects))
    expect_equal(rawDat2$eventsPerStage1, c(20, 29, 25), label = paste0(rawDat2$eventsPerStage1))
    expect_equal(rawDat2$eventsPerStage2, c(30, 21, 25), label = paste0(rawDat2$eventsPerStage2))
    expect_equal(rawDat2$eventsPerStage, c(50, 50, 50), label = paste0(rawDat2$eventsPerStage))
})
