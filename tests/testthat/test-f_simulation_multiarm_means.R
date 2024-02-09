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
## |  File name: test-f_simulation_multiarm_means.R
## |  Creation date: 09 February 2024, 10:57:28
## |  File version: $Revision: 7620 $
## |  Last changed: $Date: 2024-02-09 12:57:37 +0100 (Fr, 09 Feb 2024) $
## |  Last changed by: $Author: pahlke $
## |

test_plan_section("Testing Simulation Multi-Arm Means Function")


test_that("'getSimulationMultiArmMeans': several configurations", {
    .skipTestIfDisabled()

    # @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
    # @refFS[Sec.]{fs:sec:simulatingMultiArmDesigns}
    # @refFS[Sec.]{fs:sec:simulatingMultiArmDoseResponseRelationShips}
    # @refFS[Sec.]{fs:sec:simulatingMultiArmSelections}
    # @refFS[Tab.]{fs:tab:output:getSimulationMultiArmMeans}
    # @refFS[Formula]{fs:simulationMultiArmDoseResponse}
    # @refFS[Formula]{fs:simulationMultiArmMeansGenerate}
    # @refFS[Formula]{fs:simulationMultiArmMeansTestStatistics}
    # @refFS[Formula]{fs:simulationMultiArmSelections}
    # @refFS[Formula]{fs:multiarmRejectionRule}
    # @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
    # @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
    # @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
    # @refFS[Formula]{fs:adjustedPValueSubsetSidak}
    # @refFS[Formula]{fs:adjustedPValueSubsetSimes}
    x1 <- getSimulationMultiArmMeans(
        seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
        typeOfShape = "linear", activeArms = 4, plannedSubjects = c(10, 30, 50), stDev = 1.2,
        muMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
        conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmMeans object 'x1' with expected results
    expect_equal(x1$iterations[1, ], c(10, 10, 10, 10), label = paste0(x1$iterations[1, ]))
    expect_equal(x1$iterations[2, ], c(10, 10, 10, 10), label = paste0(x1$iterations[2, ]))
    expect_equal(x1$iterations[3, ], c(9, 8, 8, 5), label = paste0(x1$iterations[3, ]))
    expect_equal(x1$rejectAtLeastOne, c(0.3, 0.6, 0.8, 0.9), tolerance = 1e-07, label = paste0(x1$rejectAtLeastOne))
    expect_equal(unlist(as.list(x1$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0.1, 0, 0.2, 0.1, 0, 0, 0.3, 0, 0.1, 0, 0, 0.1, 0.1, 0, 0, 0.1, 0, 0.2, 0.3, 0, 0.3, 0.3), tolerance = 1e-07, label = paste0(unlist(as.list(x1$rejectedArmsPerStage))))
    expect_equal(x1$futilityStop, c(0, 0, 0, 0), label = paste0(x1$futilityStop))
    expect_equal(x1$futilityPerStage[1, ], c(0, 0, 0, 0), label = paste0(x1$futilityPerStage[1, ]))
    expect_equal(x1$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(x1$futilityPerStage[2, ]))
    expect_equal(x1$earlyStop[1, ], c(0, 0, 0, 0), label = paste0(x1$earlyStop[1, ]))
    expect_equal(x1$earlyStop[2, ], c(0.1, 0.2, 0.2, 0.5), tolerance = 1e-07, label = paste0(x1$earlyStop[2, ]))
    expect_equal(x1$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x1$successPerStage[1, ]))
    expect_equal(x1$successPerStage[2, ], c(0.1, 0.2, 0.2, 0.5), tolerance = 1e-07, label = paste0(x1$successPerStage[2, ]))
    expect_equal(x1$successPerStage[3, ], c(0.2, 0.4, 0.6, 0.4), tolerance = 1e-07, label = paste0(x1$successPerStage[3, ]))
    expect_equal(unlist(as.list(x1$selectedArms)), c(1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0, 0, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0, 0, 1, 0.3, 0.2, 1, 0.1, 0.1, 1, 0.3, 0.1, 1, 0.4, 0.4, 1, 0.1, 0, 1, 0.5, 0.4, 1, 0.2, 0.2, 1, 0.5, 0.3, 1, 0.6, 0.3, 1, 1, 0.9, 1, 1, 0.8, 1, 1, 0.8, 1, 1, 0.5), tolerance = 1e-07, label = paste0(unlist(as.list(x1$selectedArms))))
    expect_equal(x1$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x1$numberOfActiveArms[1, ]))
    expect_equal(x1$numberOfActiveArms[2, ], c(1, 1, 1, 1), label = paste0(x1$numberOfActiveArms[2, ]))
    expect_equal(x1$numberOfActiveArms[3, ], c(1, 1, 1, 1), label = paste0(x1$numberOfActiveArms[3, ]))
    expect_equal(x1$expectedNumberOfSubjects, c(268.55306, 310.74423, 296.80608, 214.56859), tolerance = 1e-07, label = paste0(x1$expectedNumberOfSubjects))
    expect_equal(unlist(as.list(x1$sampleSizes)), c(10, 1.1840544, 11.111111, 10, 10, 12.5, 10, 0.74314427, 1.9878756, 10, 0, 0, 10, 7.3350068, 25.517647, 10, 26.989766, 43.604406, 10, 0, 0, 10, 21.344686, 26.724319, 10, 2.6348908, 7.2351621, 10, 21.298615, 12.5, 10, 40, 44.643278, 10, 10, 0, 10, 33.493936, 27.945681, 10, 4.3287276, 16.089351, 10, 25.258173, 25.120998, 10, 23.39578, 28.363338, 10, 44.647888, 71.809601, 10, 62.617108, 84.693757, 10, 66.001318, 71.752151, 10, 54.740466, 55.087656), tolerance = 1e-07, label = paste0(unlist(as.list(x1$sampleSizes))))
    expect_equal(x1$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(x1$conditionalPowerAchieved[1, ]))
    expect_equal(x1$conditionalPowerAchieved[2, ], c(0.046651357, 0.022479034, 0.083769211, 0.082365248), tolerance = 1e-07, label = paste0(x1$conditionalPowerAchieved[2, ]))
    expect_equal(x1$conditionalPowerAchieved[3, ], c(0.49123587, 0.2668344, 0.64496483, 0.65218675), tolerance = 1e-07, label = paste0(x1$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x1), NA)))
        expect_output(print(x1)$show())
        invisible(capture.output(expect_error(summary(x1), NA)))
        expect_output(summary(x1)$show())
        x1CodeBased <- eval(parse(text = getObjectRCode(x1, stringWrapParagraphWidth = NULL)))
        expect_equal(x1CodeBased$iterations, x1$iterations, tolerance = 1e-07)
        expect_equal(x1CodeBased$rejectAtLeastOne, x1$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x1CodeBased$rejectedArmsPerStage, x1$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x1CodeBased$futilityStop, x1$futilityStop, tolerance = 1e-07)
        expect_equal(x1CodeBased$futilityPerStage, x1$futilityPerStage, tolerance = 1e-07)
        expect_equal(x1CodeBased$earlyStop, x1$earlyStop, tolerance = 1e-07)
        expect_equal(x1CodeBased$successPerStage, x1$successPerStage, tolerance = 1e-07)
        expect_equal(x1CodeBased$selectedArms, x1$selectedArms, tolerance = 1e-07)
        expect_equal(x1CodeBased$numberOfActiveArms, x1$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x1CodeBased$expectedNumberOfSubjects, x1$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(x1CodeBased$sampleSizes, x1$sampleSizes, tolerance = 1e-07)
        expect_equal(x1CodeBased$conditionalPowerAchieved, x1$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x1), "character")
        df <- as.data.frame(x1)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x1)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x2 <- getSimulationMultiArmMeans(
        seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
        typeOfShape = "userDefined", activeArms = 4,
        plannedSubjects = c(10, 30, 50), stDev = 1.2, adaptations = rep(TRUE, 2),
        effectMatrix = matrix(c(0.1, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.5), ncol = 4),
        conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmMeans object 'x2' with expected results
    expect_equal(x2$iterations[1, ], c(10, 10), label = paste0(x2$iterations[1, ]))
    expect_equal(x2$iterations[2, ], c(10, 10), label = paste0(x2$iterations[2, ]))
    expect_equal(x2$iterations[3, ], c(8, 8), label = paste0(x2$iterations[3, ]))
    expect_equal(x2$rejectAtLeastOne, c(0.5, 0.6), tolerance = 1e-07, label = paste0(x2$rejectAtLeastOne))
    expect_equal(unlist(as.list(x2$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.3, 0, 0, 0, 0, 0.2, 0, 0, 0.2, 0.2, 0, 0, 0.1), tolerance = 1e-07, label = paste0(unlist(as.list(x2$rejectedArmsPerStage))))
    expect_equal(x2$futilityStop, c(0, 0), label = paste0(x2$futilityStop))
    expect_equal(x2$futilityPerStage[1, ], c(0, 0), label = paste0(x2$futilityPerStage[1, ]))
    expect_equal(x2$futilityPerStage[2, ], c(0, 0), label = paste0(x2$futilityPerStage[2, ]))
    expect_equal(x2$earlyStop[1, ], c(0, 0), label = paste0(x2$earlyStop[1, ]))
    expect_equal(x2$earlyStop[2, ], c(0.2, 0.2), tolerance = 1e-07, label = paste0(x2$earlyStop[2, ]))
    expect_equal(x2$successPerStage[1, ], c(0, 0), label = paste0(x2$successPerStage[1, ]))
    expect_equal(x2$successPerStage[2, ], c(0.2, 0.2), tolerance = 1e-07, label = paste0(x2$successPerStage[2, ]))
    expect_equal(x2$successPerStage[3, ], c(0.3, 0.4), tolerance = 1e-07, label = paste0(x2$successPerStage[3, ]))
    expect_equal(unlist(as.list(x2$selectedArms)), c(1, 0.1, 0.1, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0.1, 0.1, 1, 0.2, 0, 1, 0.5, 0.3, 1, 0.2, 0.2, 1, 1, 0.8, 1, 1, 0.8), tolerance = 1e-07, label = paste0(unlist(as.list(x2$selectedArms))))
    expect_equal(x2$numberOfActiveArms[1, ], c(4, 4), label = paste0(x2$numberOfActiveArms[1, ]))
    expect_equal(x2$numberOfActiveArms[2, ], c(1, 1), label = paste0(x2$numberOfActiveArms[2, ]))
    expect_equal(x2$numberOfActiveArms[3, ], c(1, 1), label = paste0(x2$numberOfActiveArms[3, ]))
    expect_equal(x2$expectedNumberOfSubjects, c(238.96461, 281.13648), tolerance = 1e-07, label = paste0(x2$expectedNumberOfSubjects))
    expect_equal(unlist(as.list(x2$sampleSizes)), c(10, 1.1060693, 12.5, 10, 20, 25, 10, 4.7297328, 25.346201, 10, 18.776011, 38.686485, 10, 2.8470245, 10.408309, 10, 11.298615, 0, 10, 26.795872, 25.5, 10, 3.2314462, 14.141225, 10, 35.478699, 73.75451, 10, 53.306071, 77.82771), tolerance = 1e-07, label = paste0(unlist(as.list(x2$sampleSizes))))
    expect_equal(x2$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_), label = paste0(x2$conditionalPowerAchieved[1, ]))
    expect_equal(x2$conditionalPowerAchieved[2, ], c(0.064857702, 0.041878984), tolerance = 1e-07, label = paste0(x2$conditionalPowerAchieved[2, ]))
    expect_equal(x2$conditionalPowerAchieved[3, ], c(0.72573181, 0.45099208), tolerance = 1e-07, label = paste0(x2$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x2), NA)))
        expect_output(print(x2)$show())
        invisible(capture.output(expect_error(summary(x2), NA)))
        expect_output(summary(x2)$show())
        x2CodeBased <- eval(parse(text = getObjectRCode(x2, stringWrapParagraphWidth = NULL)))
        expect_equal(x2CodeBased$iterations, x2$iterations, tolerance = 1e-07)
        expect_equal(x2CodeBased$rejectAtLeastOne, x2$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x2CodeBased$rejectedArmsPerStage, x2$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x2CodeBased$futilityStop, x2$futilityStop, tolerance = 1e-07)
        expect_equal(x2CodeBased$futilityPerStage, x2$futilityPerStage, tolerance = 1e-07)
        expect_equal(x2CodeBased$earlyStop, x2$earlyStop, tolerance = 1e-07)
        expect_equal(x2CodeBased$successPerStage, x2$successPerStage, tolerance = 1e-07)
        expect_equal(x2CodeBased$selectedArms, x2$selectedArms, tolerance = 1e-07)
        expect_equal(x2CodeBased$numberOfActiveArms, x2$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x2CodeBased$expectedNumberOfSubjects, x2$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(x2CodeBased$sampleSizes, x2$sampleSizes, tolerance = 1e-07)
        expect_equal(x2CodeBased$conditionalPowerAchieved, x2$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x2), "character")
        df <- as.data.frame(x2)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x2)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x3 <- getSimulationMultiArmMeans(
        seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
        typeOfShape = "sigmoidEmax", gED50 = 2, slope = 0.5, activeArms = 4,
        plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
        conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmMeans object 'x3' with expected results
    expect_equal(x3$iterations[1, ], c(10, 10, 10, 10), label = paste0(x3$iterations[1, ]))
    expect_equal(x3$iterations[2, ], c(10, 10, 10, 10), label = paste0(x3$iterations[2, ]))
    expect_equal(x3$iterations[3, ], c(10, 9, 9, 8), label = paste0(x3$iterations[3, ]))
    expect_equal(x3$rejectAtLeastOne, c(0, 0.3, 0.6, 0.7), tolerance = 1e-07, label = paste0(x3$rejectAtLeastOne))
    expect_equal(unlist(as.list(x3$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0.1, 0, 0, 0, 0.2, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.2, 0, 0, 0.4), tolerance = 1e-07, label = paste0(unlist(as.list(x3$rejectedArmsPerStage))))
    expect_equal(x3$futilityStop, c(0, 0, 0, 0), label = paste0(x3$futilityStop))
    expect_equal(x3$futilityPerStage[1, ], c(0, 0, 0, 0), label = paste0(x3$futilityPerStage[1, ]))
    expect_equal(x3$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(x3$futilityPerStage[2, ]))
    expect_equal(x3$earlyStop[1, ], c(0, 0, 0, 0), label = paste0(x3$earlyStop[1, ]))
    expect_equal(x3$earlyStop[2, ], c(0, 0.1, 0.1, 0.2), tolerance = 1e-07, label = paste0(x3$earlyStop[2, ]))
    expect_equal(x3$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x3$successPerStage[1, ]))
    expect_equal(x3$successPerStage[2, ], c(0, 0.1, 0.1, 0.2), tolerance = 1e-07, label = paste0(x3$successPerStage[2, ]))
    expect_equal(x3$successPerStage[3, ], c(0, 0.2, 0.5, 0.5), tolerance = 1e-07, label = paste0(x3$successPerStage[3, ]))
    expect_equal(unlist(as.list(x3$selectedArms)), c(1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0, 0, 1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0, 0, 1, 0.3, 0.2, 1, 0.2, 0.2, 1, 0.2, 0.1, 1, 0.4, 0.4, 1, 0.1, 0, 1, 0.4, 0.4, 1, 0.2, 0.2, 1, 0.5, 0.4, 1, 0.6, 0.6, 1, 1, 1, 1, 1, 0.9, 1, 1, 0.9, 1, 1, 0.8), tolerance = 1e-07, label = paste0(unlist(as.list(x3$selectedArms))))
    expect_equal(x3$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x3$numberOfActiveArms[1, ]))
    expect_equal(x3$numberOfActiveArms[2, ], c(1, 1, 1, 1), label = paste0(x3$numberOfActiveArms[2, ]))
    expect_equal(x3$numberOfActiveArms[3, ], c(1, 1, 1, 1), label = paste0(x3$numberOfActiveArms[3, ]))
    expect_equal(x3$expectedNumberOfSubjects, c(295.76875, 343.71408, 335.10548, 281.56474), tolerance = 1e-07, label = paste0(x3$expectedNumberOfSubjects))
    expect_equal(unlist(as.list(x3$sampleSizes)), c(10, 1.0357205, 10, 10, 30, 33.333333, 10, 0.59871171, 1.0418812, 10, 0, 0, 10, 7.3350068, 22.965882, 10, 16.989766, 27.64836, 10, 0, 0, 10, 21.344686, 16.702699, 10, 13.17796, 20, 10, 15.323901, 2.6274327, 10, 40, 44.444444, 10, 10, 0, 10, 25.447372, 22.922435, 10, 7.2951578, 22.222222, 10, 38.282522, 25.259795, 10, 36.742398, 42.916408, 10, 46.996059, 75.888318, 10, 69.608825, 85.831349, 10, 78.881233, 70.74612, 10, 68.087084, 59.619107), tolerance = 1e-07, label = paste0(unlist(as.list(x3$sampleSizes))))
    expect_equal(x3$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(x3$conditionalPowerAchieved[1, ]))
    expect_equal(x3$conditionalPowerAchieved[2, ], c(0.042062266, 0.013174936, 0.075843331, 0.053971766), tolerance = 1e-07, label = paste0(x3$conditionalPowerAchieved[2, ]))
    expect_equal(x3$conditionalPowerAchieved[3, ], c(0.41527426, 0.27301585, 0.35639557, 0.62491311), tolerance = 1e-07, label = paste0(x3$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x3), NA)))
        expect_output(print(x3)$show())
        invisible(capture.output(expect_error(summary(x3), NA)))
        expect_output(summary(x3)$show())
        x3CodeBased <- eval(parse(text = getObjectRCode(x3, stringWrapParagraphWidth = NULL)))
        expect_equal(x3CodeBased$iterations, x3$iterations, tolerance = 1e-07)
        expect_equal(x3CodeBased$rejectAtLeastOne, x3$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x3CodeBased$rejectedArmsPerStage, x3$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x3CodeBased$futilityStop, x3$futilityStop, tolerance = 1e-07)
        expect_equal(x3CodeBased$futilityPerStage, x3$futilityPerStage, tolerance = 1e-07)
        expect_equal(x3CodeBased$earlyStop, x3$earlyStop, tolerance = 1e-07)
        expect_equal(x3CodeBased$successPerStage, x3$successPerStage, tolerance = 1e-07)
        expect_equal(x3CodeBased$selectedArms, x3$selectedArms, tolerance = 1e-07)
        expect_equal(x3CodeBased$numberOfActiveArms, x3$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x3CodeBased$expectedNumberOfSubjects, x3$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(x3CodeBased$sampleSizes, x3$sampleSizes, tolerance = 1e-07)
        expect_equal(x3CodeBased$conditionalPowerAchieved, x3$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x3), "character")
        df <- as.data.frame(x3)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x3)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x4 <- getSimulationMultiArmMeans(
        seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
        activeArms = 4, typeOfSelection = "all",
        plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
        conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmMeans object 'x4' with expected results
    expect_equal(x4$iterations[1, ], c(10, 10, 10, 10), label = paste0(x4$iterations[1, ]))
    expect_equal(x4$iterations[2, ], c(10, 10, 10, 10), label = paste0(x4$iterations[2, ]))
    expect_equal(x4$iterations[3, ], c(10, 10, 10, 10), label = paste0(x4$iterations[3, ]))
    expect_equal(x4$rejectAtLeastOne, c(0.4, 0.8, 1, 1), tolerance = 1e-07, label = paste0(x4$rejectAtLeastOne))
    expect_equal(unlist(as.list(x4$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0, 0, 0.1, 0.2, 0, 0, 0.1, 0, 0, 0.3, 0, 0.2, 0.3, 0, 0.6, 0.2, 0, 0, 0.4, 0, 0.1, 0.7, 0, 0.4, 0.6, 0, 0.7, 0.3), tolerance = 1e-07, label = paste0(unlist(as.list(x4$rejectedArmsPerStage))))
    expect_equal(x4$futilityStop, c(0, 0, 0, 0), label = paste0(x4$futilityStop))
    expect_equal(x4$futilityPerStage[1, ], c(0, 0, 0, 0), label = paste0(x4$futilityPerStage[1, ]))
    expect_equal(x4$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(x4$futilityPerStage[2, ]))
    expect_equal(x4$earlyStop[1, ], c(0, 0, 0, 0), label = paste0(x4$earlyStop[1, ]))
    expect_equal(x4$earlyStop[2, ], c(0, 0, 0, 0), label = paste0(x4$earlyStop[2, ]))
    expect_equal(x4$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x4$successPerStage[1, ]))
    expect_equal(x4$successPerStage[2, ], c(0, 0, 0, 0), label = paste0(x4$successPerStage[2, ]))
    expect_equal(x4$successPerStage[3, ], c(0, 0.1, 0, 0), tolerance = 1e-07, label = paste0(x4$successPerStage[3, ]))
    expect_equal(unlist(as.list(x4$selectedArms)), c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), label = paste0(unlist(as.list(x4$selectedArms))))
    expect_equal(x4$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x4$numberOfActiveArms[1, ]))
    expect_equal(x4$numberOfActiveArms[2, ], c(4, 4, 4, 4), label = paste0(x4$numberOfActiveArms[2, ]))
    expect_equal(x4$numberOfActiveArms[3, ], c(4, 4, 4, 4), label = paste0(x4$numberOfActiveArms[3, ]))
    expect_equal(x4$expectedNumberOfSubjects, c(1050, 891.96665, 849.19143, 705.05343), tolerance = 1e-07, label = paste0(x4$expectedNumberOfSubjects))
    expect_equal(unlist(as.list(x4$sampleSizes)), c(10, 100, 100, 10, 87.259377, 81.133954, 10, 94.901963, 64.936322, 10, 98.210686, 32.8, 10, 100, 100, 10, 87.259377, 81.133954, 10, 94.901963, 64.936322, 10, 98.210686, 32.8, 10, 100, 100, 10, 87.259377, 81.133954, 10, 94.901963, 64.936322, 10, 98.210686, 32.8, 10, 100, 100, 10, 87.259377, 81.133954, 10, 94.901963, 64.936322, 10, 98.210686, 32.8, 10, 100, 100, 10, 87.259377, 81.133954, 10, 94.901963, 64.936322, 10, 98.210686, 32.8), tolerance = 1e-07, label = paste0(unlist(as.list(x4$sampleSizes))))
    expect_equal(x4$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(x4$conditionalPowerAchieved[1, ]))
    expect_equal(x4$conditionalPowerAchieved[2, ], c(0.0086377938, 0.22005253, 0.081022458, 0.15135806), tolerance = 1e-07, label = paste0(x4$conditionalPowerAchieved[2, ]))
    expect_equal(x4$conditionalPowerAchieved[3, ], c(0.17779298, 0.23451185, 0.45925582, 0.77364695), tolerance = 1e-07, label = paste0(x4$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x4), NA)))
        expect_output(print(x4)$show())
        invisible(capture.output(expect_error(summary(x4), NA)))
        expect_output(summary(x4)$show())
        x4CodeBased <- eval(parse(text = getObjectRCode(x4, stringWrapParagraphWidth = NULL)))
        expect_equal(x4CodeBased$iterations, x4$iterations, tolerance = 1e-07)
        expect_equal(x4CodeBased$rejectAtLeastOne, x4$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x4CodeBased$rejectedArmsPerStage, x4$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x4CodeBased$futilityStop, x4$futilityStop, tolerance = 1e-07)
        expect_equal(x4CodeBased$futilityPerStage, x4$futilityPerStage, tolerance = 1e-07)
        expect_equal(x4CodeBased$earlyStop, x4$earlyStop, tolerance = 1e-07)
        expect_equal(x4CodeBased$successPerStage, x4$successPerStage, tolerance = 1e-07)
        expect_equal(x4CodeBased$selectedArms, x4$selectedArms, tolerance = 1e-07)
        expect_equal(x4CodeBased$numberOfActiveArms, x4$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x4CodeBased$expectedNumberOfSubjects, x4$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(x4CodeBased$sampleSizes, x4$sampleSizes, tolerance = 1e-07)
        expect_equal(x4CodeBased$conditionalPowerAchieved, x4$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x4), "character")
        df <- as.data.frame(x4)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x4)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x5 <- getSimulationMultiArmMeans(
        seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
        activeArms = 4, typeOfSelection = "rBest", rValue = 2,
        plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
        conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmMeans object 'x5' with expected results
    expect_equal(x5$iterations[1, ], c(10, 10, 10, 10), label = paste0(x5$iterations[1, ]))
    expect_equal(x5$iterations[2, ], c(10, 10, 10, 10), label = paste0(x5$iterations[2, ]))
    expect_equal(x5$iterations[3, ], c(10, 9, 8, 8), label = paste0(x5$iterations[3, ]))
    expect_equal(x5$rejectAtLeastOne, c(0.5, 0.9, 1, 0.9), tolerance = 1e-07, label = paste0(x5$rejectAtLeastOne))
    expect_equal(unlist(as.list(x5$rejectedArmsPerStage)), c(0, 0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.2, 0.3, 0, 0.1, 0, 0, 0, 0.1, 0, 0, 0.3, 0, 0.3, 0.2, 0, 0.2, 0.2, 0, 0.1, 0.3, 0, 0.1, 0.4, 0, 0.1, 0.3, 0, 0.6, 0.3), tolerance = 1e-07, label = paste0(unlist(as.list(x5$rejectedArmsPerStage))))
    expect_equal(x5$futilityStop, c(0, 0, 0, 0), label = paste0(x5$futilityStop))
    expect_equal(x5$futilityPerStage[1, ], c(0, 0, 0, 0), label = paste0(x5$futilityPerStage[1, ]))
    expect_equal(x5$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(x5$futilityPerStage[2, ]))
    expect_equal(x5$earlyStop[1, ], c(0, 0, 0, 0), label = paste0(x5$earlyStop[1, ]))
    expect_equal(x5$earlyStop[2, ], c(0, 0.1, 0.2, 0.2), tolerance = 1e-07, label = paste0(x5$earlyStop[2, ]))
    expect_equal(x5$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x5$successPerStage[1, ]))
    expect_equal(x5$successPerStage[2, ], c(0, 0.1, 0.2, 0.2), tolerance = 1e-07, label = paste0(x5$successPerStage[2, ]))
    expect_equal(x5$successPerStage[3, ], c(0, 0.1, 0.2, 0.3), tolerance = 1e-07, label = paste0(x5$successPerStage[3, ]))
    expect_equal(unlist(as.list(x5$selectedArms)), c(1, 0.5, 0.5, 1, 0.7, 0.6, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.9, 0.7, 1, 0.4, 0.4, 1, 0.5, 0.5, 1, 0.5, 0.5, 1, 0.6, 0.4, 1, 0.4, 0.2, 1, 0.7, 0.7, 1, 0.5, 0.4, 1, 0.4, 0.4, 1, 0.9, 0.7, 1, 1, 1, 1, 1, 0.9, 1, 1, 0.8, 1, 1, 0.8), tolerance = 1e-07, label = paste0(unlist(as.list(x5$selectedArms))))
    expect_equal(x5$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x5$numberOfActiveArms[1, ]))
    expect_equal(x5$numberOfActiveArms[2, ], c(2, 2, 2, 2), label = paste0(x5$numberOfActiveArms[2, ]))
    expect_equal(x5$numberOfActiveArms[3, ], c(2, 2, 2, 2), label = paste0(x5$numberOfActiveArms[3, ]))
    expect_equal(x5$expectedNumberOfSubjects, c(591.09538, 503.05596, 452.93301, 405.41488), tolerance = 1e-07, label = paste0(x5$expectedNumberOfSubjects))
    expect_equal(unlist(as.list(x5$sampleSizes)), c(10, 42.50248, 47.078471, 10, 45.384313, 50.975979, 10, 10, 12.5, 10, 29.554131, 37.5, 10, 15.855942, 30, 10, 22.437029, 19.843895, 10, 72.307665, 59.768075, 10, 30.61074, 15.281075, 10, 47.430714, 50, 10, 35.976108, 53.08315, 10, 50.052941, 40.398451, 10, 31.50186, 5.7250423, 10, 60.784176, 67.078471, 10, 46.971175, 44.173288, 10, 20.632484, 31.869624, 10, 71.666731, 33.506118, 10, 83.286657, 97.078471, 10, 75.384313, 84.038156, 10, 76.496545, 72.268075, 10, 81.666731, 46.006118), tolerance = 1e-07, label = paste0(unlist(as.list(x5$sampleSizes))))
    expect_equal(x5$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(x5$conditionalPowerAchieved[1, ]))
    expect_equal(x5$conditionalPowerAchieved[2, ], c(0.061919533, 0.10420825, 0.16753344, 0.13874821), tolerance = 1e-07, label = paste0(x5$conditionalPowerAchieved[2, ]))
    expect_equal(x5$conditionalPowerAchieved[3, ], c(0.29816652, 0.52092951, 0.66819594, 0.56533632), tolerance = 1e-07, label = paste0(x5$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x5), NA)))
        expect_output(print(x5)$show())
        invisible(capture.output(expect_error(summary(x5), NA)))
        expect_output(summary(x5)$show())
        x5CodeBased <- eval(parse(text = getObjectRCode(x5, stringWrapParagraphWidth = NULL)))
        expect_equal(x5CodeBased$iterations, x5$iterations, tolerance = 1e-07)
        expect_equal(x5CodeBased$rejectAtLeastOne, x5$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x5CodeBased$rejectedArmsPerStage, x5$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x5CodeBased$futilityStop, x5$futilityStop, tolerance = 1e-07)
        expect_equal(x5CodeBased$futilityPerStage, x5$futilityPerStage, tolerance = 1e-07)
        expect_equal(x5CodeBased$earlyStop, x5$earlyStop, tolerance = 1e-07)
        expect_equal(x5CodeBased$successPerStage, x5$successPerStage, tolerance = 1e-07)
        expect_equal(x5CodeBased$selectedArms, x5$selectedArms, tolerance = 1e-07)
        expect_equal(x5CodeBased$numberOfActiveArms, x5$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x5CodeBased$expectedNumberOfSubjects, x5$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(x5CodeBased$sampleSizes, x5$sampleSizes, tolerance = 1e-07)
        expect_equal(x5CodeBased$conditionalPowerAchieved, x5$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x5), "character")
        df <- as.data.frame(x5)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x5)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x6 <- getSimulationMultiArmMeans(
        seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
        activeArms = 4, typeOfSelection = "epsilon", epsilonValue = 0.1,
        plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
        conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmMeans object 'x6' with expected results
    expect_equal(x6$iterations[1, ], c(10, 10, 10, 10), label = paste0(x6$iterations[1, ]))
    expect_equal(x6$iterations[2, ], c(10, 10, 10, 10), label = paste0(x6$iterations[2, ]))
    expect_equal(x6$iterations[3, ], c(10, 9, 7, 6), label = paste0(x6$iterations[3, ]))
    expect_equal(x6$rejectAtLeastOne, c(0.4, 0.6, 0.8, 0.8), tolerance = 1e-07, label = paste0(x6$rejectAtLeastOne))
    expect_equal(unlist(as.list(x6$rejectedArmsPerStage)), c(0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0.2, 0, 0, 0.1, 0, 0, 0, 0, 0.1, 0, 0, 0, 0.2, 0, 0.1, 0.4, 0, 0.3, 0.5, 0, 0.2, 0.3), tolerance = 1e-07, label = paste0(unlist(as.list(x6$rejectedArmsPerStage))))
    expect_equal(x6$futilityStop, c(0, 0, 0, 0), label = paste0(x6$futilityStop))
    expect_equal(x6$futilityPerStage[1, ], c(0, 0, 0, 0), label = paste0(x6$futilityPerStage[1, ]))
    expect_equal(x6$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(x6$futilityPerStage[2, ]))
    expect_equal(x6$earlyStop[1, ], c(0, 0, 0, 0), label = paste0(x6$earlyStop[1, ]))
    expect_equal(x6$earlyStop[2, ], c(0, 0.1, 0.3, 0.4), tolerance = 1e-07, label = paste0(x6$earlyStop[2, ]))
    expect_equal(x6$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x6$successPerStage[1, ]))
    expect_equal(x6$successPerStage[2, ], c(0, 0.1, 0.3, 0.4), tolerance = 1e-07, label = paste0(x6$successPerStage[2, ]))
    expect_equal(x6$successPerStage[3, ], c(0.4, 0.5, 0.5, 0.4), tolerance = 1e-07, label = paste0(x6$successPerStage[3, ]))
    expect_equal(unlist(as.list(x6$selectedArms)), c(1, 0.1, 0.1, 1, 0.2, 0.1, 1, 0, 0, 1, 0.1, 0.1, 1, 0.5, 0.4, 1, 0.1, 0, 1, 0.3, 0.2, 1, 0.3, 0.2, 1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0, 0, 1, 0.1, 0, 1, 0.4, 0.4, 1, 0.6, 0.5, 1, 0.8, 0.5, 1, 0.5, 0.3, 1, 1, 1, 1, 1, 0.9, 1, 1, 0.7, 1, 1, 0.6), tolerance = 1e-07, label = paste0(unlist(as.list(x6$selectedArms))))
    expect_equal(x6$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x6$numberOfActiveArms[1, ]))
    expect_equal(x6$numberOfActiveArms[2, ], c(1.3, 1.2, 1.1, 1), tolerance = 1e-07, label = paste0(x6$numberOfActiveArms[2, ]))
    expect_equal(x6$numberOfActiveArms[3, ], c(1.2, 1, 1, 1), tolerance = 1e-07, label = paste0(x6$numberOfActiveArms[3, ]))
    expect_equal(x6$expectedNumberOfSubjects, c(436.56282, 365.15193, 284.70045, 253.12175), tolerance = 1e-07, label = paste0(x6$expectedNumberOfSubjects))
    expect_equal(unlist(as.list(x6$sampleSizes)), c(10, 4.7999536, 10, 10, 16.971175, 11.111111, 10, 0, 0, 10, 10, 16.666667, 10, 35.332961, 40, 10, 10, 0, 10, 21.400604, 22.595075, 10, 21.344686, 22.270265, 10, 23.218148, 30, 10, 22.202225, 23.298934, 10, 0, 0, 10, 10, 0, 10, 29.860691, 40, 10, 41.405234, 49.459866, 10, 62.809861, 31.890295, 10, 22.672359, 23.636115, 10, 73.351063, 100, 10, 73.607458, 83.869911, 10, 74.210465, 54.485369, 10, 64.017046, 62.573047), tolerance = 1e-07, label = paste0(unlist(as.list(x6$sampleSizes))))
    expect_equal(x6$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(x6$conditionalPowerAchieved[1, ]))
    expect_equal(x6$conditionalPowerAchieved[2, ], c(0.024687171, 0.015314975, 0.045856815, 0.050229622), tolerance = 1e-07, label = paste0(x6$conditionalPowerAchieved[2, ]))
    expect_equal(x6$conditionalPowerAchieved[3, ], c(0.1883251, 0.40048173, 0.51841906, 0.54348956), tolerance = 1e-07, label = paste0(x6$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x6), NA)))
        expect_output(print(x6)$show())
        invisible(capture.output(expect_error(summary(x6), NA)))
        expect_output(summary(x6)$show())
        x6CodeBased <- eval(parse(text = getObjectRCode(x6, stringWrapParagraphWidth = NULL)))
        expect_equal(x6CodeBased$iterations, x6$iterations, tolerance = 1e-07)
        expect_equal(x6CodeBased$rejectAtLeastOne, x6$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x6CodeBased$rejectedArmsPerStage, x6$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x6CodeBased$futilityStop, x6$futilityStop, tolerance = 1e-07)
        expect_equal(x6CodeBased$futilityPerStage, x6$futilityPerStage, tolerance = 1e-07)
        expect_equal(x6CodeBased$earlyStop, x6$earlyStop, tolerance = 1e-07)
        expect_equal(x6CodeBased$successPerStage, x6$successPerStage, tolerance = 1e-07)
        expect_equal(x6CodeBased$selectedArms, x6$selectedArms, tolerance = 1e-07)
        expect_equal(x6CodeBased$numberOfActiveArms, x6$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x6CodeBased$expectedNumberOfSubjects, x6$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(x6CodeBased$sampleSizes, x6$sampleSizes, tolerance = 1e-07)
        expect_equal(x6CodeBased$conditionalPowerAchieved, x6$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x6), "character")
        df <- as.data.frame(x6)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x6)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x7 <- getSimulationMultiArmMeans(
        seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms = 4,
        plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1), adaptations = c(TRUE, FALSE),
        conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmMeans object 'x7' with expected results
    expect_equal(x7$iterations[1, ], c(10, 10, 10, 10), label = paste0(x7$iterations[1, ]))
    expect_equal(x7$iterations[2, ], c(10, 10, 10, 10), label = paste0(x7$iterations[2, ]))
    expect_equal(x7$iterations[3, ], c(9, 8, 8, 5), label = paste0(x7$iterations[3, ]))
    expect_equal(x7$rejectAtLeastOne, c(0.2, 0.4, 0.7, 0.8), tolerance = 1e-07, label = paste0(x7$rejectAtLeastOne))
    expect_equal(unlist(as.list(x7$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0.2, 0.1, 0, 0, 0.3, 0, 0.1, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0.2, 0.2, 0, 0.3, 0.2), tolerance = 1e-07, label = paste0(unlist(as.list(x7$rejectedArmsPerStage))))
    expect_equal(x7$futilityStop, c(0, 0, 0, 0), label = paste0(x7$futilityStop))
    expect_equal(x7$futilityPerStage[1, ], c(0, 0, 0, 0), label = paste0(x7$futilityPerStage[1, ]))
    expect_equal(x7$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(x7$futilityPerStage[2, ]))
    expect_equal(x7$earlyStop[1, ], c(0, 0, 0, 0), label = paste0(x7$earlyStop[1, ]))
    expect_equal(x7$earlyStop[2, ], c(0.1, 0.2, 0.2, 0.5), tolerance = 1e-07, label = paste0(x7$earlyStop[2, ]))
    expect_equal(x7$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x7$successPerStage[1, ]))
    expect_equal(x7$successPerStage[2, ], c(0.1, 0.2, 0.2, 0.5), tolerance = 1e-07, label = paste0(x7$successPerStage[2, ]))
    expect_equal(x7$successPerStage[3, ], c(0.1, 0.2, 0.5, 0.3), tolerance = 1e-07, label = paste0(x7$successPerStage[3, ]))
    expect_equal(unlist(as.list(x7$selectedArms)), c(1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0, 0, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0, 0, 1, 0.3, 0.2, 1, 0.1, 0.1, 1, 0.3, 0.1, 1, 0.4, 0.4, 1, 0.1, 0, 1, 0.5, 0.4, 1, 0.2, 0.2, 1, 0.5, 0.3, 1, 0.6, 0.3, 1, 1, 0.9, 1, 1, 0.8, 1, 1, 0.8, 1, 1, 0.5), tolerance = 1e-07, label = paste0(unlist(as.list(x7$selectedArms))))
    expect_equal(x7$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x7$numberOfActiveArms[1, ]))
    expect_equal(x7$numberOfActiveArms[2, ], c(1, 1, 1, 1), label = paste0(x7$numberOfActiveArms[2, ]))
    expect_equal(x7$numberOfActiveArms[3, ], c(1, 1, 1, 1), label = paste0(x7$numberOfActiveArms[3, ]))
    expect_equal(x7$expectedNumberOfSubjects, c(222.21727, 277.8712, 297.53775, 227.3405), tolerance = 1e-07, label = paste0(x7$expectedNumberOfSubjects))
    expect_equal(unlist(as.list(x7$sampleSizes)), c(10, 1.1840544, 1.315616, 10, 10, 12.5, 10, 0.74314427, 0.92893034, 10, 0, 0, 10, 7.3350068, 8.1500075, 10, 26.989766, 33.737207, 10, 0, 0, 10, 21.344686, 40, 10, 2.6348908, 2.9276564, 10, 21.298615, 12.5, 10, 40, 50, 10, 10, 0, 10, 33.493936, 33.674217, 10, 4.3287276, 5.4109095, 10, 25.258173, 21.280514, 10, 23.39578, 27.859565, 10, 44.647888, 46.067497, 10, 62.617108, 64.148116, 10, 66.001318, 72.209444, 10, 54.740466, 67.859565), tolerance = 1e-07, label = paste0(unlist(as.list(x7$sampleSizes))))
    expect_equal(x7$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(x7$conditionalPowerAchieved[1, ]))
    expect_equal(x7$conditionalPowerAchieved[2, ], c(0.046651357, 0.022479034, 0.083769211, 0.082365248), tolerance = 1e-07, label = paste0(x7$conditionalPowerAchieved[2, ]))
    expect_equal(x7$conditionalPowerAchieved[3, ], c(0.39772697, 0.18083546, 0.60828997, 0.66318671), tolerance = 1e-07, label = paste0(x7$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x7), NA)))
        expect_output(print(x7)$show())
        invisible(capture.output(expect_error(summary(x7), NA)))
        expect_output(summary(x7)$show())
        x7CodeBased <- eval(parse(text = getObjectRCode(x7, stringWrapParagraphWidth = NULL)))
        expect_equal(x7CodeBased$iterations, x7$iterations, tolerance = 1e-07)
        expect_equal(x7CodeBased$rejectAtLeastOne, x7$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x7CodeBased$rejectedArmsPerStage, x7$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x7CodeBased$futilityStop, x7$futilityStop, tolerance = 1e-07)
        expect_equal(x7CodeBased$futilityPerStage, x7$futilityPerStage, tolerance = 1e-07)
        expect_equal(x7CodeBased$earlyStop, x7$earlyStop, tolerance = 1e-07)
        expect_equal(x7CodeBased$successPerStage, x7$successPerStage, tolerance = 1e-07)
        expect_equal(x7CodeBased$selectedArms, x7$selectedArms, tolerance = 1e-07)
        expect_equal(x7CodeBased$numberOfActiveArms, x7$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x7CodeBased$expectedNumberOfSubjects, x7$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(x7CodeBased$sampleSizes, x7$sampleSizes, tolerance = 1e-07)
        expect_equal(x7CodeBased$conditionalPowerAchieved, x7$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x7), "character")
        df <- as.data.frame(x7)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x7)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x8 <- getSimulationMultiArmMeans(
        seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), activeArms = 4,
        typeOfSelection = "all",
        plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1), adaptations = c(TRUE, FALSE),
        conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmMeans object 'x8' with expected results
    expect_equal(x8$iterations[1, ], c(10, 10, 10, 10), label = paste0(x8$iterations[1, ]))
    expect_equal(x8$iterations[2, ], c(10, 10, 10, 10), label = paste0(x8$iterations[2, ]))
    expect_equal(x8$iterations[3, ], c(10, 10, 10, 10), label = paste0(x8$iterations[3, ]))
    expect_equal(x8$rejectAtLeastOne, c(0.3, 0.6, 1, 1), tolerance = 1e-07, label = paste0(x8$rejectAtLeastOne))
    expect_equal(unlist(as.list(x8$rejectedArmsPerStage)), c(0, 0, 0, 0, 0.1, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0, 0.2, 0, 0, 0.2, 0.3, 0, 0, 0, 0.1, 0, 0.2, 0, 0.4, 0.2, 0, 0.7, 0.2, 0, 0.2, 0.1, 0.1, 0.2, 0.3, 0, 0.7, 0.3, 0, 0.8, 0.2), tolerance = 1e-07, label = paste0(unlist(as.list(x8$rejectedArmsPerStage))))
    expect_equal(x8$futilityStop, c(0, 0, 0, 0), label = paste0(x8$futilityStop))
    expect_equal(x8$futilityPerStage[1, ], c(0, 0, 0, 0), label = paste0(x8$futilityPerStage[1, ]))
    expect_equal(x8$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(x8$futilityPerStage[2, ]))
    expect_equal(x8$earlyStop[1, ], c(0, 0, 0, 0), label = paste0(x8$earlyStop[1, ]))
    expect_equal(x8$earlyStop[2, ], c(0, 0, 0, 0), label = paste0(x8$earlyStop[2, ]))
    expect_equal(x8$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x8$successPerStage[1, ]))
    expect_equal(x8$successPerStage[2, ], c(0, 0, 0, 0), label = paste0(x8$successPerStage[2, ]))
    expect_equal(x8$successPerStage[3, ], c(0, 0, 0, 0), label = paste0(x8$successPerStage[3, ]))
    expect_equal(unlist(as.list(x8$selectedArms)), c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), label = paste0(unlist(as.list(x8$selectedArms))))
    expect_equal(x8$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x8$numberOfActiveArms[1, ]))
    expect_equal(x8$numberOfActiveArms[2, ], c(4, 4, 4, 4), label = paste0(x8$numberOfActiveArms[2, ]))
    expect_equal(x8$numberOfActiveArms[3, ], c(4, 4, 4, 4), label = paste0(x8$numberOfActiveArms[3, ]))
    expect_equal(x8$expectedNumberOfSubjects, c(1050, 914.65115, 996.33236, 1027.6565), tolerance = 1e-07, label = paste0(x8$expectedNumberOfSubjects))
    expect_equal(unlist(as.list(x8$sampleSizes)), c(10, 100, 100, 10, 86.465115, 86.465115, 10, 94.633236, 94.633236, 10, 97.765652, 97.765652, 10, 100, 100, 10, 86.465115, 86.465115, 10, 94.633236, 94.633236, 10, 97.765652, 97.765652, 10, 100, 100, 10, 86.465115, 86.465115, 10, 94.633236, 94.633236, 10, 97.765652, 97.765652, 10, 100, 100, 10, 86.465115, 86.465115, 10, 94.633236, 94.633236, 10, 97.765652, 97.765652, 10, 100, 100, 10, 86.465115, 86.465115, 10, 94.633236, 94.633236, 10, 97.765652, 97.765652), tolerance = 1e-07, label = paste0(unlist(as.list(x8$sampleSizes))))
    expect_equal(x8$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(x8$conditionalPowerAchieved[1, ]))
    expect_equal(x8$conditionalPowerAchieved[2, ], c(0.015572779, 0.22941785, 0.084615364, 0.1668833), tolerance = 1e-07, label = paste0(x8$conditionalPowerAchieved[2, ]))
    expect_equal(x8$conditionalPowerAchieved[3, ], c(0.10350918, 0.24229761, 0.63483372, 0.79913622), tolerance = 1e-07, label = paste0(x8$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x8), NA)))
        expect_output(print(x8)$show())
        invisible(capture.output(expect_error(summary(x8), NA)))
        expect_output(summary(x8)$show())
        x8CodeBased <- eval(parse(text = getObjectRCode(x8, stringWrapParagraphWidth = NULL)))
        expect_equal(x8CodeBased$iterations, x8$iterations, tolerance = 1e-07)
        expect_equal(x8CodeBased$rejectAtLeastOne, x8$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x8CodeBased$rejectedArmsPerStage, x8$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x8CodeBased$futilityStop, x8$futilityStop, tolerance = 1e-07)
        expect_equal(x8CodeBased$futilityPerStage, x8$futilityPerStage, tolerance = 1e-07)
        expect_equal(x8CodeBased$earlyStop, x8$earlyStop, tolerance = 1e-07)
        expect_equal(x8CodeBased$successPerStage, x8$successPerStage, tolerance = 1e-07)
        expect_equal(x8CodeBased$selectedArms, x8$selectedArms, tolerance = 1e-07)
        expect_equal(x8CodeBased$numberOfActiveArms, x8$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x8CodeBased$expectedNumberOfSubjects, x8$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(x8CodeBased$sampleSizes, x8$sampleSizes, tolerance = 1e-07)
        expect_equal(x8CodeBased$conditionalPowerAchieved, x8$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x8), "character")
        df <- as.data.frame(x8)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x8)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x9 <- getSimulationMultiArmMeans(
        seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), activeArms = 4,
        typeOfSelection = "rBest", rValue = 2,
        plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1), adaptations = c(TRUE, FALSE),
        conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmMeans object 'x9' with expected results
    expect_equal(x9$iterations[1, ], c(10, 10, 10, 10), label = paste0(x9$iterations[1, ]))
    expect_equal(x9$iterations[2, ], c(10, 10, 10, 10), label = paste0(x9$iterations[2, ]))
    expect_equal(x9$iterations[3, ], c(10, 9, 8, 7), label = paste0(x9$iterations[3, ]))
    expect_equal(x9$rejectAtLeastOne, c(0.4, 0.6, 0.7, 0.9), tolerance = 1e-07, label = paste0(x9$rejectAtLeastOne))
    expect_equal(unlist(as.list(x9$rejectedArmsPerStage)), c(0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.2, 0.3, 0, 0.1, 0, 0, 0, 0.1, 0, 0, 0.2, 0, 0.5, 0, 0, 0.3, 0, 0, 0.2, 0.1, 0, 0.1, 0.1, 0, 0.1, 0, 0.1, 0.5, 0.3), tolerance = 1e-07, label = paste0(unlist(as.list(x9$rejectedArmsPerStage))))
    expect_equal(x9$futilityStop, c(0, 0, 0, 0), label = paste0(x9$futilityStop))
    expect_equal(x9$futilityPerStage[1, ], c(0, 0, 0, 0), label = paste0(x9$futilityPerStage[1, ]))
    expect_equal(x9$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(x9$futilityPerStage[2, ]))
    expect_equal(x9$earlyStop[1, ], c(0, 0, 0, 0), label = paste0(x9$earlyStop[1, ]))
    expect_equal(x9$earlyStop[2, ], c(0, 0.1, 0.2, 0.3), tolerance = 1e-07, label = paste0(x9$earlyStop[2, ]))
    expect_equal(x9$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x9$successPerStage[1, ]))
    expect_equal(x9$successPerStage[2, ], c(0, 0.1, 0.2, 0.3), tolerance = 1e-07, label = paste0(x9$successPerStage[2, ]))
    expect_equal(x9$successPerStage[3, ], c(0, 0, 0.2, 0.1), tolerance = 1e-07, label = paste0(x9$successPerStage[3, ]))
    expect_equal(unlist(as.list(x9$selectedArms)), c(1, 0.5, 0.5, 1, 0.7, 0.6, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.9, 0.7, 1, 0.4, 0.4, 1, 0.5, 0.5, 1, 0.5, 0.5, 1, 0.6, 0.4, 1, 0.4, 0.1, 1, 0.7, 0.7, 1, 0.5, 0.4, 1, 0.4, 0.4, 1, 0.9, 0.6, 1, 1, 1, 1, 1, 0.9, 1, 1, 0.8, 1, 1, 0.7), tolerance = 1e-07, label = paste0(unlist(as.list(x9$selectedArms))))
    expect_equal(x9$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x9$numberOfActiveArms[1, ]))
    expect_equal(x9$numberOfActiveArms[2, ], c(2, 2, 2, 2), label = paste0(x9$numberOfActiveArms[2, ]))
    expect_equal(x9$numberOfActiveArms[3, ], c(2, 2, 2, 2), label = paste0(x9$numberOfActiveArms[3, ]))
    expect_equal(x9$expectedNumberOfSubjects, c(541.86022, 465.03543, 438.85623, 427.93855), tolerance = 1e-07, label = paste0(x9$expectedNumberOfSubjects))
    expect_equal(unlist(as.list(x9$sampleSizes)), c(10, 42.315846, 42.315846, 10, 43.044196, 41.478749, 10, 10, 12.5, 10, 28.887554, 41.267934, 10, 15.358913, 15.358913, 10, 21.683959, 24.093288, 10, 70.857557, 63.571946, 10, 27.933797, 39.905424, 10, 46.61779, 46.61779, 10, 34.631951, 38.479946, 10, 49.194842, 36.493552, 10, 31.168408, 1.6691539, 10, 59.660857, 59.660857, 10, 44.698358, 43.316707, 10, 19.566345, 24.457932, 10, 67.989758, 54.271083, 10, 81.976703, 81.976703, 10, 72.029232, 73.684344, 10, 74.809372, 68.511715, 10, 77.989758, 68.556797), tolerance = 1e-07, label = paste0(unlist(as.list(x9$sampleSizes))))
    expect_equal(x9$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(x9$conditionalPowerAchieved[1, ]))
    expect_equal(x9$conditionalPowerAchieved[2, ], c(0.085169097, 0.1203719, 0.19239671, 0.15260753), tolerance = 1e-07, label = paste0(x9$conditionalPowerAchieved[2, ]))
    expect_equal(x9$conditionalPowerAchieved[3, ], c(0.20442999, 0.2985599, 0.51072411, 0.55234699), tolerance = 1e-07, label = paste0(x9$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x9), NA)))
        expect_output(print(x9)$show())
        invisible(capture.output(expect_error(summary(x9), NA)))
        expect_output(summary(x9)$show())
        x9CodeBased <- eval(parse(text = getObjectRCode(x9, stringWrapParagraphWidth = NULL)))
        expect_equal(x9CodeBased$iterations, x9$iterations, tolerance = 1e-07)
        expect_equal(x9CodeBased$rejectAtLeastOne, x9$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x9CodeBased$rejectedArmsPerStage, x9$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x9CodeBased$futilityStop, x9$futilityStop, tolerance = 1e-07)
        expect_equal(x9CodeBased$futilityPerStage, x9$futilityPerStage, tolerance = 1e-07)
        expect_equal(x9CodeBased$earlyStop, x9$earlyStop, tolerance = 1e-07)
        expect_equal(x9CodeBased$successPerStage, x9$successPerStage, tolerance = 1e-07)
        expect_equal(x9CodeBased$selectedArms, x9$selectedArms, tolerance = 1e-07)
        expect_equal(x9CodeBased$numberOfActiveArms, x9$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x9CodeBased$expectedNumberOfSubjects, x9$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(x9CodeBased$sampleSizes, x9$sampleSizes, tolerance = 1e-07)
        expect_equal(x9CodeBased$conditionalPowerAchieved, x9$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x9), "character")
        df <- as.data.frame(x9)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x9)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x10 <- getSimulationMultiArmMeans(
        seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), activeArms = 4,
        typeOfSelection = "epsilon", epsilonValue = 0.1,
        plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1),
        adaptations = c(TRUE, FALSE), intersectionTest = "Bonferroni",
        conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmMeans object 'x10' with expected results
    expect_equal(x10$iterations[1, ], c(10, 10, 10, 10), label = paste0(x10$iterations[1, ]))
    expect_equal(x10$iterations[2, ], c(7, 8, 5, 9), label = paste0(x10$iterations[2, ]))
    expect_equal(x10$iterations[3, ], c(7, 6, 4, 4), label = paste0(x10$iterations[3, ]))
    expect_equal(x10$rejectAtLeastOne, c(0.2, 0.4, 0.2, 0.6), tolerance = 1e-07, label = paste0(x10$rejectAtLeastOne))
    expect_equal(unlist(as.list(x10$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0.1, 0, 0, 0.2, 0, 0.1, 0.1, 0, 0.2, 0.1), tolerance = 1e-07, label = paste0(unlist(as.list(x10$rejectedArmsPerStage))))
    expect_equal(x10$futilityStop, c(0.3, 0.2, 0.5, 0.1), tolerance = 1e-07, label = paste0(x10$futilityStop))
    expect_equal(x10$futilityPerStage[1, ], c(0.3, 0.2, 0.5, 0.1), tolerance = 1e-07, label = paste0(x10$futilityPerStage[1, ]))
    expect_equal(x10$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(x10$futilityPerStage[2, ]))
    expect_equal(x10$earlyStop[1, ], c(0.3, 0.2, 0.5, 0.1), tolerance = 1e-07, label = paste0(x10$earlyStop[1, ]))
    expect_equal(x10$earlyStop[2, ], c(0, 0.2, 0.1, 0.5), tolerance = 1e-07, label = paste0(x10$earlyStop[2, ]))
    expect_equal(x10$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x10$successPerStage[1, ]))
    expect_equal(x10$successPerStage[2, ], c(0, 0.2, 0.1, 0.5), tolerance = 1e-07, label = paste0(x10$successPerStage[2, ]))
    expect_equal(x10$successPerStage[3, ], c(0.2, 0.2, 0.1, 0), tolerance = 1e-07, label = paste0(x10$successPerStage[3, ]))
    expect_equal(unlist(as.list(x10$selectedArms)), c(1, 0.1, 0.1, 1, 0, 0, 1, 0, 0, 1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.1, 0, 1, 0.2, 0.2, 1, 0.2, 0, 1, 0.1, 0.1, 1, 0.4, 0.3, 1, 0, 0, 1, 0.2, 0.1, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0.4, 0.3, 1, 0.3, 0.1, 1, 0.7, 0.7, 1, 0.8, 0.6, 1, 0.5, 0.4, 1, 0.9, 0.4), tolerance = 1e-07, label = paste0(unlist(as.list(x10$selectedArms))))
    expect_equal(x10$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x10$numberOfActiveArms[1, ]))
    expect_equal(x10$numberOfActiveArms[2, ], c(1.2857143, 1.125, 1.2, 1.1111111), tolerance = 1e-07, label = paste0(x10$numberOfActiveArms[2, ]))
    expect_equal(x10$numberOfActiveArms[3, ], c(1.2857143, 1.1666667, 1.25, 1.25), tolerance = 1e-07, label = paste0(x10$numberOfActiveArms[3, ]))
    expect_equal(x10$expectedNumberOfSubjects, c(225.54374, 222.86662, 137.52897, 198.07751), tolerance = 1e-07, label = paste0(x10$expectedNumberOfSubjects))
    expect_equal(unlist(as.list(x10$sampleSizes)), c(10, 5.7796177, 5.7796177, 10, 0, 0, 10, 0, 0, 10, 21.972849, 49.43891, 10, 18.318062, 18.318062, 10, 4.015823, 0, 10, 19.919121, 24.898901, 10, 4.2855233, 0, 10, 4.0944014, 4.0944014, 10, 25.284305, 32.792226, 10, 0, 0, 10, 13.080039, 4.4300867, 10, 40.432794, 40.432794, 10, 33.32367, 44.431559, 10, 42.475692, 28.104858, 10, 18.985094, 14.869399, 10, 56.76351, 56.76351, 10, 50.123797, 60.557119, 10, 45.125964, 31.417698, 10, 51.714883, 53.868997), tolerance = 1e-07, label = paste0(unlist(as.list(x10$sampleSizes))))
    expect_equal(x10$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(x10$conditionalPowerAchieved[1, ]))
    expect_equal(x10$conditionalPowerAchieved[2, ], c(0.051011725, 0.14528092, 0.099325934, 0.10008765), tolerance = 1e-07, label = paste0(x10$conditionalPowerAchieved[2, ]))
    expect_equal(x10$conditionalPowerAchieved[3, ], c(0.1199627, 0.35325827, 0.33382798, 0.10956309), tolerance = 1e-07, label = paste0(x10$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x10), NA)))
        expect_output(print(x10)$show())
        invisible(capture.output(expect_error(summary(x10), NA)))
        expect_output(summary(x10)$show())
        x10CodeBased <- eval(parse(text = getObjectRCode(x10, stringWrapParagraphWidth = NULL)))
        expect_equal(x10CodeBased$iterations, x10$iterations, tolerance = 1e-07)
        expect_equal(x10CodeBased$rejectAtLeastOne, x10$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x10CodeBased$rejectedArmsPerStage, x10$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x10CodeBased$futilityStop, x10$futilityStop, tolerance = 1e-07)
        expect_equal(x10CodeBased$futilityPerStage, x10$futilityPerStage, tolerance = 1e-07)
        expect_equal(x10CodeBased$earlyStop, x10$earlyStop, tolerance = 1e-07)
        expect_equal(x10CodeBased$successPerStage, x10$successPerStage, tolerance = 1e-07)
        expect_equal(x10CodeBased$selectedArms, x10$selectedArms, tolerance = 1e-07)
        expect_equal(x10CodeBased$numberOfActiveArms, x10$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x10CodeBased$expectedNumberOfSubjects, x10$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(x10CodeBased$sampleSizes, x10$sampleSizes, tolerance = 1e-07)
        expect_equal(x10CodeBased$conditionalPowerAchieved, x10$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x10), "character")
        df <- as.data.frame(x10)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x10)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x11 <- getSimulationMultiArmMeans(
        seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), activeArms = 4, threshold = 0,
        plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.1, 0.3, 0.1), intersectionTest = "Bonferroni",
        conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmMeans object 'x11' with expected results
    expect_equal(x11$iterations[1, ], c(10, 10, 10), label = paste0(x11$iterations[1, ]))
    expect_equal(x11$iterations[2, ], c(9, 6, 6), label = paste0(x11$iterations[2, ]))
    expect_equal(x11$iterations[3, ], c(9, 5, 4), label = paste0(x11$iterations[3, ]))
    expect_equal(x11$rejectAtLeastOne, c(0, 0, 0.1), tolerance = 1e-07, label = paste0(x11$rejectAtLeastOne))
    expect_equal(unlist(as.list(x11$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1), tolerance = 1e-07, label = paste0(unlist(as.list(x11$rejectedArmsPerStage))))
    expect_equal(x11$futilityStop, c(0.1, 0.5, 0.6), tolerance = 1e-07, label = paste0(x11$futilityStop))
    expect_equal(x11$futilityPerStage[1, ], c(0.1, 0.4, 0.4), tolerance = 1e-07, label = paste0(x11$futilityPerStage[1, ]))
    expect_equal(x11$futilityPerStage[2, ], c(0, 0.1, 0.2), tolerance = 1e-07, label = paste0(x11$futilityPerStage[2, ]))
    expect_equal(x11$earlyStop[1, ], c(0.1, 0.4, 0.4), tolerance = 1e-07, label = paste0(x11$earlyStop[1, ]))
    expect_equal(x11$earlyStop[2, ], c(0, 0.1, 0.2), tolerance = 1e-07, label = paste0(x11$earlyStop[2, ]))
    expect_equal(x11$successPerStage[1, ], c(0, 0, 0), label = paste0(x11$successPerStage[1, ]))
    expect_equal(x11$successPerStage[2, ], c(0, 0, 0), label = paste0(x11$successPerStage[2, ]))
    expect_equal(x11$successPerStage[3, ], c(0, 0, 0.1), tolerance = 1e-07, label = paste0(x11$successPerStage[3, ]))
    expect_equal(unlist(as.list(x11$selectedArms)), c(1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.1, 0, 1, 0.3, 0.3, 1, 0.3, 0.2, 1, 0.2, 0.1, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.9, 0.9, 1, 0.6, 0.5, 1, 0.6, 0.4), tolerance = 1e-07, label = paste0(unlist(as.list(x11$selectedArms))))
    expect_equal(x11$numberOfActiveArms[1, ], c(4, 4, 4), label = paste0(x11$numberOfActiveArms[1, ]))
    expect_equal(x11$numberOfActiveArms[2, ], c(1, 1, 1), label = paste0(x11$numberOfActiveArms[2, ]))
    expect_equal(x11$numberOfActiveArms[3, ], c(1, 1, 1), label = paste0(x11$numberOfActiveArms[3, ]))
    expect_equal(x11$expectedNumberOfSubjects, c(293.83569, 240.03958, 175.41029), tolerance = 1e-07, label = paste0(x11$expectedNumberOfSubjects))
    expect_equal(unlist(as.list(x11$sampleSizes)), c(10, 1.428489, 11.111111, 10, 16.666667, 20, 10, 10.322237, 0, 10, 9.8699583, 33.333333, 10, 41.973847, 40, 10, 21.511686, 25, 10, 15.186109, 22.222222, 10, 6.5876644, 11.765765, 10, 17.33069, 33.465374, 10, 17.556106, 24.756944, 10, 16.666667, 20, 10, 2.0321899, 21.502286, 10, 44.040662, 91.42361, 10, 81.894844, 91.765765, 10, 51.196803, 79.96766), tolerance = 1e-07, label = paste0(unlist(as.list(x11$sampleSizes))))
    expect_equal(x11$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_), label = paste0(x11$conditionalPowerAchieved[1, ]))
    expect_equal(x11$conditionalPowerAchieved[2, ], c(0.038698548, 0.10704476, 0.043430379), tolerance = 1e-07, label = paste0(x11$conditionalPowerAchieved[2, ]))
    expect_equal(x11$conditionalPowerAchieved[3, ], c(0.30869297, 0.27823314, 0.60162296), tolerance = 1e-07, label = paste0(x11$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x11), NA)))
        expect_output(print(x11)$show())
        invisible(capture.output(expect_error(summary(x11), NA)))
        expect_output(summary(x11)$show())
        x11CodeBased <- eval(parse(text = getObjectRCode(x11, stringWrapParagraphWidth = NULL)))
        expect_equal(x11CodeBased$iterations, x11$iterations, tolerance = 1e-07)
        expect_equal(x11CodeBased$rejectAtLeastOne, x11$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x11CodeBased$rejectedArmsPerStage, x11$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x11CodeBased$futilityStop, x11$futilityStop, tolerance = 1e-07)
        expect_equal(x11CodeBased$futilityPerStage, x11$futilityPerStage, tolerance = 1e-07)
        expect_equal(x11CodeBased$earlyStop, x11$earlyStop, tolerance = 1e-07)
        expect_equal(x11CodeBased$successPerStage, x11$successPerStage, tolerance = 1e-07)
        expect_equal(x11CodeBased$selectedArms, x11$selectedArms, tolerance = 1e-07)
        expect_equal(x11CodeBased$numberOfActiveArms, x11$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x11CodeBased$expectedNumberOfSubjects, x11$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(x11CodeBased$sampleSizes, x11$sampleSizes, tolerance = 1e-07)
        expect_equal(x11CodeBased$conditionalPowerAchieved, x11$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x11), "character")
        df <- as.data.frame(x11)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x11)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x12 <- getSimulationMultiArmMeans(
        seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)),
        typeOfShape = "linear", activeArms = 4, threshold = 0,
        plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
        intersectionTest = "Bonferroni",
        conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmMeans object 'x12' with expected results
    expect_equal(x12$iterations[1, ], c(10, 10, 10, 10), label = paste0(x12$iterations[1, ]))
    expect_equal(x12$iterations[2, ], c(10, 6, 8, 8), label = paste0(x12$iterations[2, ]))
    expect_equal(x12$iterations[3, ], c(8, 5, 1, 2), label = paste0(x12$iterations[3, ]))
    expect_equal(x12$rejectAtLeastOne, c(0.3, 0.1, 0.7, 0.7), tolerance = 1e-07, label = paste0(x12$rejectAtLeastOne))
    expect_equal(unlist(as.list(x12$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0.1, 0.1, 0.1, 0.1, 0, 0.1, 0, 0.2, 0.3, 0, 0.1, 0.3, 0), tolerance = 1e-07, label = paste0(unlist(as.list(x12$rejectedArmsPerStage))))
    expect_equal(x12$futilityStop, c(0, 0.4, 0.2, 0.2), tolerance = 1e-07, label = paste0(x12$futilityStop))
    expect_equal(x12$futilityPerStage[1, ], c(0, 0.4, 0.2, 0.2), tolerance = 1e-07, label = paste0(x12$futilityPerStage[1, ]))
    expect_equal(x12$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(x12$futilityPerStage[2, ]))
    expect_equal(x12$earlyStop[1, ], c(0, 0.4, 0.2, 0.2), tolerance = 1e-07, label = paste0(x12$earlyStop[1, ]))
    expect_equal(x12$earlyStop[2, ], c(0.2, 0.1, 0.7, 0.6), tolerance = 1e-07, label = paste0(x12$earlyStop[2, ]))
    expect_equal(x12$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x12$successPerStage[1, ]))
    expect_equal(x12$successPerStage[2, ], c(0.2, 0.1, 0.7, 0.6), tolerance = 1e-07, label = paste0(x12$successPerStage[2, ]))
    expect_equal(x12$successPerStage[3, ], c(0.1, 0, 0, 0.1), tolerance = 1e-07, label = paste0(x12$successPerStage[3, ]))
    expect_equal(unlist(as.list(x12$selectedArms)), c(1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.1, 0, 1, 0, 0, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0, 0, 1, 0.1, 0, 1, 0.1, 0.1, 1, 0.2, 0.2, 1, 0.1, 0, 1, 0.3, 0.2, 1, 0.5, 0.3, 1, 0.2, 0.1, 1, 0.6, 0.1, 1, 0.4, 0, 1, 1, 0.8, 1, 0.6, 0.5, 1, 0.8, 0.1, 1, 0.8, 0.2), tolerance = 1e-07, label = paste0(unlist(as.list(x12$selectedArms))))
    expect_equal(x12$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x12$numberOfActiveArms[1, ]))
    expect_equal(x12$numberOfActiveArms[2, ], c(1, 1, 1, 1), label = paste0(x12$numberOfActiveArms[2, ]))
    expect_equal(x12$numberOfActiveArms[3, ], c(1, 1, 1, 1), label = paste0(x12$numberOfActiveArms[3, ]))
    expect_equal(x12$expectedNumberOfSubjects, c(270.86167, 201.58944, 127.72687, 185.63922), tolerance = 1e-07, label = paste0(x12$expectedNumberOfSubjects))
    expect_equal(unlist(as.list(x12$sampleSizes)), c(10, 1.1167748, 12.5, 10, 8.9578499, 20, 10, 0.5, 0, 10, 0, 0, 10, 6.7277808, 32.819107, 10, 6.3724427, 20, 10, 0, 0, 10, 12.5, 0, 10, 2.4005123, 12.5, 10, 12.766635, 29.774077, 10, 11.503905, 0, 10, 27.658054, 100, 10, 28.865098, 31.331731, 10, 23.415877, 20, 10, 24.075387, 100, 10, 19.616461, 0, 10, 39.110166, 89.150838, 10, 51.512805, 89.774077, 10, 36.079292, 100, 10, 59.774515, 100), tolerance = 1e-07, label = paste0(unlist(as.list(x12$sampleSizes))))
    expect_equal(x12$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(x12$conditionalPowerAchieved[1, ]))
    expect_equal(x12$conditionalPowerAchieved[2, ], c(0.064552587, 0.074113563, 0.13271614, 0.12195746), tolerance = 1e-07, label = paste0(x12$conditionalPowerAchieved[2, ]))
    expect_equal(x12$conditionalPowerAchieved[3, ], c(0.41775137, 0.42792704, 0.6049542, 0.13870598), tolerance = 1e-07, label = paste0(x12$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x12), NA)))
        expect_output(print(x12)$show())
        invisible(capture.output(expect_error(summary(x12), NA)))
        expect_output(summary(x12)$show())
        x12CodeBased <- eval(parse(text = getObjectRCode(x12, stringWrapParagraphWidth = NULL)))
        expect_equal(x12CodeBased$iterations, x12$iterations, tolerance = 1e-07)
        expect_equal(x12CodeBased$rejectAtLeastOne, x12$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x12CodeBased$rejectedArmsPerStage, x12$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x12CodeBased$futilityStop, x12$futilityStop, tolerance = 1e-07)
        expect_equal(x12CodeBased$futilityPerStage, x12$futilityPerStage, tolerance = 1e-07)
        expect_equal(x12CodeBased$earlyStop, x12$earlyStop, tolerance = 1e-07)
        expect_equal(x12CodeBased$successPerStage, x12$successPerStage, tolerance = 1e-07)
        expect_equal(x12CodeBased$selectedArms, x12$selectedArms, tolerance = 1e-07)
        expect_equal(x12CodeBased$numberOfActiveArms, x12$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x12CodeBased$expectedNumberOfSubjects, x12$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(x12CodeBased$sampleSizes, x12$sampleSizes, tolerance = 1e-07)
        expect_equal(x12CodeBased$conditionalPowerAchieved, x12$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x12), "character")
        df <- as.data.frame(x12)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x12)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x13 <- getSimulationMultiArmMeans(
        seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)),
        typeOfShape = "userDefined", activeArms = 4, threshold = 0,
        plannedSubjects = c(10, 30, 50), stDev = 1.2, adaptations = rep(TRUE, 2),
        effectMatrix = matrix(c(0.1, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.5), ncol = 4), intersectionTest = "Sidak",
        conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmMeans object 'x13' with expected results
    expect_equal(x13$iterations[1, ], c(10, 10), label = paste0(x13$iterations[1, ]))
    expect_equal(x13$iterations[2, ], c(10, 9), label = paste0(x13$iterations[2, ]))
    expect_equal(x13$iterations[3, ], c(7, 7), label = paste0(x13$iterations[3, ]))
    expect_equal(x13$rejectAtLeastOne, c(0.3, 0.3), tolerance = 1e-07, label = paste0(x13$rejectAtLeastOne))
    expect_equal(unlist(as.list(x13$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.2, 0, 0, 0.1, 0.1), tolerance = 1e-07, label = paste0(unlist(as.list(x13$rejectedArmsPerStage))))
    expect_equal(x13$futilityStop, c(0, 0.1), tolerance = 1e-07, label = paste0(x13$futilityStop))
    expect_equal(x13$futilityPerStage[1, ], c(0, 0.1), tolerance = 1e-07, label = paste0(x13$futilityPerStage[1, ]))
    expect_equal(x13$futilityPerStage[2, ], c(0, 0), label = paste0(x13$futilityPerStage[2, ]))
    expect_equal(x13$earlyStop[1, ], c(0, 0.1), tolerance = 1e-07, label = paste0(x13$earlyStop[1, ]))
    expect_equal(x13$earlyStop[2, ], c(0.3, 0.2), tolerance = 1e-07, label = paste0(x13$earlyStop[2, ]))
    expect_equal(x13$successPerStage[1, ], c(0, 0), label = paste0(x13$successPerStage[1, ]))
    expect_equal(x13$successPerStage[2, ], c(0.3, 0.2), tolerance = 1e-07, label = paste0(x13$successPerStage[2, ]))
    expect_equal(x13$successPerStage[3, ], c(0, 0.1), tolerance = 1e-07, label = paste0(x13$successPerStage[3, ]))
    expect_equal(unlist(as.list(x13$selectedArms)), c(1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.5, 0.4, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.5, 0.2, 1, 0.2, 0.1, 1, 1, 0.7, 1, 0.9, 0.7), tolerance = 1e-07, label = paste0(unlist(as.list(x13$selectedArms))))
    expect_equal(x13$numberOfActiveArms[1, ], c(4, 4), label = paste0(x13$numberOfActiveArms[1, ]))
    expect_equal(x13$numberOfActiveArms[2, ], c(1, 1), label = paste0(x13$numberOfActiveArms[2, ]))
    expect_equal(x13$numberOfActiveArms[3, ], c(1, 1), label = paste0(x13$numberOfActiveArms[3, ]))
    expect_equal(x13$expectedNumberOfSubjects, c(238.16649, 275.50348), tolerance = 1e-07, label = paste0(x13$expectedNumberOfSubjects))
    expect_equal(unlist(as.list(x13$sampleSizes)), c(10, 1.0395374, 14.285714, 10, 4.3933102, 11.199547, 10, 4.4634729, 31.899994, 10, 38.793234, 57.142857, 10, 2.5722467, 14.285714, 10, 5.3695979, 6.9814836, 10, 23.677991, 28.571429, 10, 11.241946, 8.8667681, 10, 31.753247, 89.042851, 10, 59.798088, 84.190656), tolerance = 1e-07, label = paste0(unlist(as.list(x13$sampleSizes))))
    expect_equal(x13$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_), label = paste0(x13$conditionalPowerAchieved[1, ]))
    expect_equal(x13$conditionalPowerAchieved[2, ], c(0.095374468, 0.085831831), tolerance = 1e-07, label = paste0(x13$conditionalPowerAchieved[2, ]))
    expect_equal(x13$conditionalPowerAchieved[3, ], c(0.56669649, 0.49770257), tolerance = 1e-07, label = paste0(x13$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x13), NA)))
        expect_output(print(x13)$show())
        invisible(capture.output(expect_error(summary(x13), NA)))
        expect_output(summary(x13)$show())
        x13CodeBased <- eval(parse(text = getObjectRCode(x13, stringWrapParagraphWidth = NULL)))
        expect_equal(x13CodeBased$iterations, x13$iterations, tolerance = 1e-07)
        expect_equal(x13CodeBased$rejectAtLeastOne, x13$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x13CodeBased$rejectedArmsPerStage, x13$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x13CodeBased$futilityStop, x13$futilityStop, tolerance = 1e-07)
        expect_equal(x13CodeBased$futilityPerStage, x13$futilityPerStage, tolerance = 1e-07)
        expect_equal(x13CodeBased$earlyStop, x13$earlyStop, tolerance = 1e-07)
        expect_equal(x13CodeBased$successPerStage, x13$successPerStage, tolerance = 1e-07)
        expect_equal(x13CodeBased$selectedArms, x13$selectedArms, tolerance = 1e-07)
        expect_equal(x13CodeBased$numberOfActiveArms, x13$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x13CodeBased$expectedNumberOfSubjects, x13$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(x13CodeBased$sampleSizes, x13$sampleSizes, tolerance = 1e-07)
        expect_equal(x13CodeBased$conditionalPowerAchieved, x13$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x13), "character")
        df <- as.data.frame(x13)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x13)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x14 <- getSimulationMultiArmMeans(
        seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)),
        typeOfShape = "sigmoidEmax", gED50 = 2, slope = 0.5, activeArms = 4, threshold = 0,
        plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1),
        adaptations = rep(TRUE, 2), intersectionTest = "Sidak",
        conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmMeans object 'x14' with expected results
    expect_equal(x14$iterations[1, ], c(10, 10, 10, 10), label = paste0(x14$iterations[1, ]))
    expect_equal(x14$iterations[2, ], c(10, 9, 8, 10), label = paste0(x14$iterations[2, ]))
    expect_equal(x14$iterations[3, ], c(9, 9, 6, 7), label = paste0(x14$iterations[3, ]))
    expect_equal(x14$rejectAtLeastOne, c(0.1, 0, 0.3, 0.4), tolerance = 1e-07, label = paste0(x14$rejectAtLeastOne))
    expect_equal(unlist(as.list(x14$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0.1, 0.1), tolerance = 1e-07, label = paste0(unlist(as.list(x14$rejectedArmsPerStage))))
    expect_equal(x14$futilityStop, c(0, 0.1, 0.2, 0), tolerance = 1e-07, label = paste0(x14$futilityStop))
    expect_equal(x14$futilityPerStage[1, ], c(0, 0.1, 0.2, 0), tolerance = 1e-07, label = paste0(x14$futilityPerStage[1, ]))
    expect_equal(x14$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(x14$futilityPerStage[2, ]))
    expect_equal(x14$earlyStop[1, ], c(0, 0.1, 0.2, 0), tolerance = 1e-07, label = paste0(x14$earlyStop[1, ]))
    expect_equal(x14$earlyStop[2, ], c(0.1, 0, 0.2, 0.3), tolerance = 1e-07, label = paste0(x14$earlyStop[2, ]))
    expect_equal(x14$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x14$successPerStage[1, ]))
    expect_equal(x14$successPerStage[2, ], c(0.1, 0, 0.2, 0.3), tolerance = 1e-07, label = paste0(x14$successPerStage[2, ]))
    expect_equal(x14$successPerStage[3, ], c(0, 0, 0.1, 0.1), tolerance = 1e-07, label = paste0(x14$successPerStage[3, ]))
    expect_equal(unlist(as.list(x14$selectedArms)), c(1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.1, 0, 1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0.1, 0.1, 1, 0.1, 0, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.2, 0.1, 1, 0.2, 0.2, 1, 0.4, 0.3, 1, 0.1, 0.1, 1, 0.4, 0.4, 1, 0.4, 0.2, 1, 1, 0.9, 1, 0.9, 0.9, 1, 0.8, 0.6, 1, 1, 0.7), tolerance = 1e-07, label = paste0(unlist(as.list(x14$selectedArms))))
    expect_equal(x14$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x14$numberOfActiveArms[1, ]))
    expect_equal(x14$numberOfActiveArms[2, ], c(1, 1, 1, 1), label = paste0(x14$numberOfActiveArms[2, ]))
    expect_equal(x14$numberOfActiveArms[3, ], c(1, 1, 1, 1), label = paste0(x14$numberOfActiveArms[3, ]))
    expect_equal(x14$expectedNumberOfSubjects, c(302.82831, 359.55539, 205.66054, 326.21609), tolerance = 1e-07, label = paste0(x14$expectedNumberOfSubjects))
    expect_equal(unlist(as.list(x14$sampleSizes)), c(10, 0.96871141, 11.111111, 10, 4.8692533, 11.111111, 10, 0.5, 0, 10, 30, 42.857143, 10, 6.7277808, 29.172539, 10, 37.581628, 44.444444, 10, 12.5, 16.666667, 10, 10, 0, 10, 12.834638, 22.222222, 10, 21.991558, 33.249006, 10, 17.610119, 16.666667, 10, 12.962323, 28.571429, 10, 24.585127, 27.825125, 10, 7.6171061, 11.111111, 10, 20.182233, 28.660644, 10, 22.561443, 17.977538, 10, 45.116257, 90.330997, 10, 72.059546, 99.915673, 10, 50.792352, 61.993977, 10, 75.523767, 89.406109), tolerance = 1e-07, label = paste0(unlist(as.list(x14$sampleSizes))))
    expect_equal(x14$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(x14$conditionalPowerAchieved[1, ]))
    expect_equal(x14$conditionalPowerAchieved[2, ], c(0.054394525, 0.033810654, 0.16623293, 0.07472066), tolerance = 1e-07, label = paste0(x14$conditionalPowerAchieved[2, ]))
    expect_equal(x14$conditionalPowerAchieved[3, ], c(0.39787587, 0.27550431, 0.64928935, 0.24074486), tolerance = 1e-07, label = paste0(x14$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x14), NA)))
        expect_output(print(x14)$show())
        invisible(capture.output(expect_error(summary(x14), NA)))
        expect_output(summary(x14)$show())
        x14CodeBased <- eval(parse(text = getObjectRCode(x14, stringWrapParagraphWidth = NULL)))
        expect_equal(x14CodeBased$iterations, x14$iterations, tolerance = 1e-07)
        expect_equal(x14CodeBased$rejectAtLeastOne, x14$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x14CodeBased$rejectedArmsPerStage, x14$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x14CodeBased$futilityStop, x14$futilityStop, tolerance = 1e-07)
        expect_equal(x14CodeBased$futilityPerStage, x14$futilityPerStage, tolerance = 1e-07)
        expect_equal(x14CodeBased$earlyStop, x14$earlyStop, tolerance = 1e-07)
        expect_equal(x14CodeBased$successPerStage, x14$successPerStage, tolerance = 1e-07)
        expect_equal(x14CodeBased$selectedArms, x14$selectedArms, tolerance = 1e-07)
        expect_equal(x14CodeBased$numberOfActiveArms, x14$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x14CodeBased$expectedNumberOfSubjects, x14$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(x14CodeBased$sampleSizes, x14$sampleSizes, tolerance = 1e-07)
        expect_equal(x14CodeBased$conditionalPowerAchieved, x14$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x14), "character")
        df <- as.data.frame(x14)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x14)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x15 <- getSimulationMultiArmMeans(
        seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)),
        activeArms = 4, threshold = 0, typeOfSelection = "all",
        plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1),
        adaptations = rep(TRUE, 2), intersectionTest = "Sidak",
        conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmMeans object 'x15' with expected results
    expect_equal(x15$iterations[1, ], c(10, 10, 10, 10), label = paste0(x15$iterations[1, ]))
    expect_equal(x15$iterations[2, ], c(10, 9, 9, 10), label = paste0(x15$iterations[2, ]))
    expect_equal(x15$iterations[3, ], c(10, 8, 8, 10), label = paste0(x15$iterations[3, ]))
    expect_equal(x15$rejectAtLeastOne, c(0.1, 0.6, 0.9, 1), tolerance = 1e-07, label = paste0(x15$rejectAtLeastOne))
    expect_equal(unlist(as.list(x15$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0, 0.1, 0, 0, 0, 0, 0.2, 0, 0.1, 0.2, 0, 0, 0, 0, 0.2, 0.1, 0, 0.3, 0.3, 0, 0.3, 0.4, 0, 0.1, 0, 0, 0.3, 0.1, 0, 0.5, 0.2, 0.1, 0.4, 0.2), tolerance = 1e-07, label = paste0(unlist(as.list(x15$rejectedArmsPerStage))))
    expect_equal(x15$futilityStop, c(0, 0.1, 0.1, 0), tolerance = 1e-07, label = paste0(x15$futilityStop))
    expect_equal(x15$futilityPerStage[1, ], c(0, 0.1, 0.1, 0), tolerance = 1e-07, label = paste0(x15$futilityPerStage[1, ]))
    expect_equal(x15$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(x15$futilityPerStage[2, ]))
    expect_equal(x15$earlyStop[1, ], c(0, 0.1, 0.1, 0), tolerance = 1e-07, label = paste0(x15$earlyStop[1, ]))
    expect_equal(x15$earlyStop[2, ], c(0, 0.1, 0.1, 0), tolerance = 1e-07, label = paste0(x15$earlyStop[2, ]))
    expect_equal(x15$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x15$successPerStage[1, ]))
    expect_equal(x15$successPerStage[2, ], c(0, 0.1, 0.1, 0), tolerance = 1e-07, label = paste0(x15$successPerStage[2, ]))
    expect_equal(x15$successPerStage[3, ], c(0, 0, 0.3, 0.3), tolerance = 1e-07, label = paste0(x15$successPerStage[3, ]))
    expect_equal(unlist(as.list(x15$selectedArms)), c(1, 0.6, 0.6, 1, 0.6, 0.5, 1, 0.2, 0.1, 1, 0.7, 0.5, 1, 0.7, 0.6, 1, 0.8, 0.7, 1, 0.7, 0.7, 1, 0.7, 0.7, 1, 0.7, 0.7, 1, 0.8, 0.7, 1, 0.7, 0.7, 1, 0.9, 0.9, 1, 0.6, 0.6, 1, 0.6, 0.5, 1, 0.7, 0.6, 1, 0.8, 0.8, 1, 1, 1, 1, 0.9, 0.8, 1, 0.9, 0.8, 1, 1, 1), tolerance = 1e-07, label = paste0(unlist(as.list(x15$selectedArms))))
    expect_equal(x15$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x15$numberOfActiveArms[1, ]))
    expect_equal(x15$numberOfActiveArms[2, ], c(2.6, 3.1111111, 2.5555556, 3.1), tolerance = 1e-07, label = paste0(x15$numberOfActiveArms[2, ]))
    expect_equal(x15$numberOfActiveArms[3, ], c(2.5, 3, 2.625, 2.9), tolerance = 1e-07, label = paste0(x15$numberOfActiveArms[3, ]))
    expect_equal(x15$expectedNumberOfSubjects, c(690.38911, 619.77858, 554.02061, 670.88154), tolerance = 1e-07, label = paste0(x15$expectedNumberOfSubjects))
    expect_equal(unlist(as.list(x15$sampleSizes)), c(10, 54.180167, 50.4, 10, 57.917242, 50.5, 10, 16.188147, 12.5, 10, 64.800747, 25.135561, 10, 65.454083, 50.4, 10, 71.01474, 75.5, 10, 71.743702, 62.866861, 10, 64.800747, 45.135561, 10, 69.120607, 60.4, 10, 71.01474, 75.5, 10, 71.743702, 62.866861, 10, 84.800747, 55.535561, 10, 55.454083, 50.4, 10, 48.792518, 50.5, 10, 71.743702, 50.366861, 10, 74.800747, 45.535561, 10, 94.180167, 90.4, 10, 82.125851, 88, 10, 93.965925, 75.366861, 10, 94.800747, 65.535561), tolerance = 1e-07, label = paste0(unlist(as.list(x15$sampleSizes))))
    expect_equal(x15$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(x15$conditionalPowerAchieved[1, ]))
    expect_equal(x15$conditionalPowerAchieved[2, ], c(0.086326519, 0.23897424, 0.15375141, 0.19252038), tolerance = 1e-07, label = paste0(x15$conditionalPowerAchieved[2, ]))
    expect_equal(x15$conditionalPowerAchieved[3, ], c(0.19907656, 0.37086672, 0.52811383, 0.57866018), tolerance = 1e-07, label = paste0(x15$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x15), NA)))
        expect_output(print(x15)$show())
        invisible(capture.output(expect_error(summary(x15), NA)))
        expect_output(summary(x15)$show())
        x15CodeBased <- eval(parse(text = getObjectRCode(x15, stringWrapParagraphWidth = NULL)))
        expect_equal(x15CodeBased$iterations, x15$iterations, tolerance = 1e-07)
        expect_equal(x15CodeBased$rejectAtLeastOne, x15$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x15CodeBased$rejectedArmsPerStage, x15$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x15CodeBased$futilityStop, x15$futilityStop, tolerance = 1e-07)
        expect_equal(x15CodeBased$futilityPerStage, x15$futilityPerStage, tolerance = 1e-07)
        expect_equal(x15CodeBased$earlyStop, x15$earlyStop, tolerance = 1e-07)
        expect_equal(x15CodeBased$successPerStage, x15$successPerStage, tolerance = 1e-07)
        expect_equal(x15CodeBased$selectedArms, x15$selectedArms, tolerance = 1e-07)
        expect_equal(x15CodeBased$numberOfActiveArms, x15$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x15CodeBased$expectedNumberOfSubjects, x15$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(x15CodeBased$sampleSizes, x15$sampleSizes, tolerance = 1e-07)
        expect_equal(x15CodeBased$conditionalPowerAchieved, x15$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x15), "character")
        df <- as.data.frame(x15)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x15)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x16 <- getSimulationMultiArmMeans(
        seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)),
        activeArms = 4, threshold = 0, typeOfSelection = "rBest", rValue = 2,
        plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1),
        adaptations = rep(TRUE, 2), intersectionTest = "Simes",
        conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmMeans object 'x16' with expected results
    expect_equal(x16$iterations[1, ], c(10, 10, 10, 10), label = paste0(x16$iterations[1, ]))
    expect_equal(x16$iterations[2, ], c(8, 8, 9, 10), label = paste0(x16$iterations[2, ]))
    expect_equal(x16$iterations[3, ], c(8, 8, 8, 7), label = paste0(x16$iterations[3, ]))
    expect_equal(x16$rejectAtLeastOne, c(0.1, 0.5, 0.7, 0.8), tolerance = 1e-07, label = paste0(x16$rejectAtLeastOne))
    expect_equal(unlist(as.list(x16$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0.1, 0.1, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0.2, 0.1, 0, 0.1, 0, 0.1, 0.2, 0.2, 0, 0.1, 0.4, 0.1, 0.6, 0), tolerance = 1e-07, label = paste0(unlist(as.list(x16$rejectedArmsPerStage))))
    expect_equal(x16$futilityStop, c(0.2, 0.2, 0.1, 0), tolerance = 1e-07, label = paste0(x16$futilityStop))
    expect_equal(x16$futilityPerStage[1, ], c(0.2, 0.2, 0.1, 0), tolerance = 1e-07, label = paste0(x16$futilityPerStage[1, ]))
    expect_equal(x16$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(x16$futilityPerStage[2, ]))
    expect_equal(x16$earlyStop[1, ], c(0.2, 0.2, 0.1, 0), tolerance = 1e-07, label = paste0(x16$earlyStop[1, ]))
    expect_equal(x16$earlyStop[2, ], c(0, 0, 0.1, 0.3), tolerance = 1e-07, label = paste0(x16$earlyStop[2, ]))
    expect_equal(x16$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x16$successPerStage[1, ]))
    expect_equal(x16$successPerStage[2, ], c(0, 0, 0.1, 0.3), tolerance = 1e-07, label = paste0(x16$successPerStage[2, ]))
    expect_equal(x16$successPerStage[3, ], c(0.1, 0.1, 0.2, 0.1), tolerance = 1e-07, label = paste0(x16$successPerStage[3, ]))
    expect_equal(unlist(as.list(x16$selectedArms)), c(1, 0.2, 0.1, 1, 0.6, 0.6, 1, 0.4, 0.4, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.2, 0.2, 1, 0.3, 0.2, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0.4, 0.3, 1, 0.5, 0.3, 1, 0.8, 0.8, 1, 0.7, 0.7, 1, 0.5, 0.5, 1, 0.7, 0.4, 1, 0.8, 0.8, 1, 0.8, 0.8, 1, 0.9, 0.8, 1, 1, 0.7), tolerance = 1e-07, label = paste0(unlist(as.list(x16$selectedArms))))
    expect_equal(x16$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x16$numberOfActiveArms[1, ]))
    expect_equal(x16$numberOfActiveArms[2, ], c(2, 2, 1.7777778, 1.8), tolerance = 1e-07, label = paste0(x16$numberOfActiveArms[2, ]))
    expect_equal(x16$numberOfActiveArms[3, ], c(1.875, 2, 1.75, 1.8571429), tolerance = 1e-07, label = paste0(x16$numberOfActiveArms[3, ]))
    expect_equal(x16$expectedNumberOfSubjects, c(485.19749, 377.01763, 431.09127, 345.60572), tolerance = 1e-07, label = paste0(x16$expectedNumberOfSubjects))
    expect_equal(unlist(as.list(x16$sampleSizes)), c(10, 25, 12.5, 10, 52.984739, 51, 10, 38.255848, 50, 10, 5.1691192, 14.285714, 10, 28.803833, 37.5, 10, 25, 25, 10, 24.228929, 11.497164, 10, 28.635967, 34.757362, 10, 31.69512, 37.5, 10, 5.6938105, 1.5787961, 10, 40.9155, 37.5, 10, 42.851335, 17.605103, 10, 85.498953, 100, 10, 58.678549, 52.578796, 10, 35.341046, 48.997164, 10, 50.953751, 18.295116, 10, 85.498953, 100, 10, 71.178549, 65.078796, 10, 76.256545, 86.497164, 10, 73.805086, 49.614505), tolerance = 1e-07, label = paste0(unlist(as.list(x16$sampleSizes))))
    expect_equal(x16$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(x16$conditionalPowerAchieved[1, ]))
    expect_equal(x16$conditionalPowerAchieved[2, ], c(0.017664185, 0.17480419, 0.093445917, 0.088580327), tolerance = 1e-07, label = paste0(x16$conditionalPowerAchieved[2, ]))
    expect_equal(x16$conditionalPowerAchieved[3, ], c(0.16524243, 0.38443342, 0.48058247, 0.6510419), tolerance = 1e-07, label = paste0(x16$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x16), NA)))
        expect_output(print(x16)$show())
        invisible(capture.output(expect_error(summary(x16), NA)))
        expect_output(summary(x16)$show())
        x16CodeBased <- eval(parse(text = getObjectRCode(x16, stringWrapParagraphWidth = NULL)))
        expect_equal(x16CodeBased$iterations, x16$iterations, tolerance = 1e-07)
        expect_equal(x16CodeBased$rejectAtLeastOne, x16$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x16CodeBased$rejectedArmsPerStage, x16$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x16CodeBased$futilityStop, x16$futilityStop, tolerance = 1e-07)
        expect_equal(x16CodeBased$futilityPerStage, x16$futilityPerStage, tolerance = 1e-07)
        expect_equal(x16CodeBased$earlyStop, x16$earlyStop, tolerance = 1e-07)
        expect_equal(x16CodeBased$successPerStage, x16$successPerStage, tolerance = 1e-07)
        expect_equal(x16CodeBased$selectedArms, x16$selectedArms, tolerance = 1e-07)
        expect_equal(x16CodeBased$numberOfActiveArms, x16$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x16CodeBased$expectedNumberOfSubjects, x16$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(x16CodeBased$sampleSizes, x16$sampleSizes, tolerance = 1e-07)
        expect_equal(x16CodeBased$conditionalPowerAchieved, x16$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x16), "character")
        df <- as.data.frame(x16)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x16)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x17 <- getSimulationMultiArmMeans(
        seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), activeArms = 4, threshold = 0, typeOfSelection = "epsilon", epsilonValue = 0.1,
        plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1),
        adaptations = rep(TRUE, 2), intersectionTest = "Simes",
        conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmMeans object 'x17' with expected results
    expect_equal(x17$iterations[1, ], c(10, 10, 10, 10), label = paste0(x17$iterations[1, ]))
    expect_equal(x17$iterations[2, ], c(9, 10, 10, 10), label = paste0(x17$iterations[2, ]))
    expect_equal(x17$iterations[3, ], c(7, 8, 5, 5), label = paste0(x17$iterations[3, ]))
    expect_equal(x17$rejectAtLeastOne, c(0.3, 0.2, 0.9, 0.8), tolerance = 1e-07, label = paste0(x17$rejectAtLeastOne))
    expect_equal(unlist(as.list(x17$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0.1, 0, 0.2, 0, 0, 0.1, 0.4, 0, 0.2, 0, 0, 0, 0.1, 0, 0.1, 0, 0.2, 0.3, 0.1, 0, 0.2, 0.4), tolerance = 1e-07, label = paste0(unlist(as.list(x17$rejectedArmsPerStage))))
    expect_equal(x17$futilityStop, c(0.2, 0.1, 0, 0.1), tolerance = 1e-07, label = paste0(x17$futilityStop))
    expect_equal(x17$futilityPerStage[1, ], c(0.1, 0, 0, 0), tolerance = 1e-07, label = paste0(x17$futilityPerStage[1, ]))
    expect_equal(x17$futilityPerStage[2, ], c(0.1, 0.1, 0, 0.1), tolerance = 1e-07, label = paste0(x17$futilityPerStage[2, ]))
    expect_equal(x17$earlyStop[1, ], c(0.1, 0, 0, 0), tolerance = 1e-07, label = paste0(x17$earlyStop[1, ]))
    expect_equal(x17$earlyStop[2, ], c(0.2, 0.2, 0.5, 0.5), tolerance = 1e-07, label = paste0(x17$earlyStop[2, ]))
    expect_equal(x17$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x17$successPerStage[1, ]))
    expect_equal(x17$successPerStage[2, ], c(0.1, 0.1, 0.5, 0.4), tolerance = 1e-07, label = paste0(x17$successPerStage[2, ]))
    expect_equal(x17$successPerStage[3, ], c(0.2, 0.1, 0.4, 0.3), tolerance = 1e-07, label = paste0(x17$successPerStage[3, ]))
    expect_equal(unlist(as.list(x17$selectedArms)), c(1, 0.1, 0.1, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.1, 0, 1, 0.4, 0.3, 1, 0.4, 0.2, 1, 0, 0, 1, 0.2, 0.1, 1, 0.2, 0.1, 1, 0.3, 0.2, 1, 0.6, 0.5, 1, 0.3, 0.1, 1, 0.4, 0.4, 1, 0.3, 0.3, 1, 0.6, 0.1, 1, 0.6, 0.4, 1, 0.9, 0.7, 1, 1, 0.8, 1, 1, 0.5, 1, 1, 0.5), tolerance = 1e-07, label = paste0(unlist(as.list(x17$selectedArms))))
    expect_equal(x17$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x17$numberOfActiveArms[1, ]))
    expect_equal(x17$numberOfActiveArms[2, ], c(1.2222222, 1.2, 1.3, 1.2), tolerance = 1e-07, label = paste0(x17$numberOfActiveArms[2, ]))
    expect_equal(x17$numberOfActiveArms[3, ], c(1.2857143, 1.125, 1.4, 1.2), tolerance = 1e-07, label = paste0(x17$numberOfActiveArms[3, ]))
    expect_equal(x17$expectedNumberOfSubjects, c(328.39002, 302.69421, 285.23022, 240.4545), tolerance = 1e-07, label = paste0(x17$expectedNumberOfSubjects))
    expect_equal(unlist(as.list(x17$sampleSizes)), c(10, 4.4952582, 14.285714, 10, 19.967039, 25, 10, 10, 20, 10, 10, 0, 10, 21.883735, 42.857143, 10, 26.51119, 25, 10, 0, 0, 10, 13.162215, 6.3433684, 10, 14.295646, 14.285714, 10, 12.191217, 8.9015119, 10, 34.361222, 100, 10, 22.260169, 5.4863466, 10, 27.97297, 57.142857, 10, 13.444855, 22.756787, 10, 23.167319, 20, 10, 26.747723, 50.618475, 10, 62.896861, 100, 10, 55.457645, 74.744525, 10, 47.701679, 100, 10, 59.007892, 56.104821), tolerance = 1e-07, label = paste0(unlist(as.list(x17$sampleSizes))))
    expect_equal(x17$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(x17$conditionalPowerAchieved[1, ]))
    expect_equal(x17$conditionalPowerAchieved[2, ], c(0.025620238, 0.099222073, 0.15711506, 0.067612991), tolerance = 1e-07, label = paste0(x17$conditionalPowerAchieved[2, ]))
    expect_equal(x17$conditionalPowerAchieved[3, ], c(0.2137719, 0.30848358, 0.15636561, 0.6965125), tolerance = 1e-07, label = paste0(x17$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x17), NA)))
        expect_output(print(x17)$show())
        invisible(capture.output(expect_error(summary(x17), NA)))
        expect_output(summary(x17)$show())
        x17CodeBased <- eval(parse(text = getObjectRCode(x17, stringWrapParagraphWidth = NULL)))
        expect_equal(x17CodeBased$iterations, x17$iterations, tolerance = 1e-07)
        expect_equal(x17CodeBased$rejectAtLeastOne, x17$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x17CodeBased$rejectedArmsPerStage, x17$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x17CodeBased$futilityStop, x17$futilityStop, tolerance = 1e-07)
        expect_equal(x17CodeBased$futilityPerStage, x17$futilityPerStage, tolerance = 1e-07)
        expect_equal(x17CodeBased$earlyStop, x17$earlyStop, tolerance = 1e-07)
        expect_equal(x17CodeBased$successPerStage, x17$successPerStage, tolerance = 1e-07)
        expect_equal(x17CodeBased$selectedArms, x17$selectedArms, tolerance = 1e-07)
        expect_equal(x17CodeBased$numberOfActiveArms, x17$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x17CodeBased$expectedNumberOfSubjects, x17$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(x17CodeBased$sampleSizes, x17$sampleSizes, tolerance = 1e-07)
        expect_equal(x17CodeBased$conditionalPowerAchieved, x17$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x17), "character")
        df <- as.data.frame(x17)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x17)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x18 <- getSimulationMultiArmMeans(
        seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)),
        activeArms = 4, threshold = 0,
        plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1),
        adaptations = c(TRUE, FALSE), intersectionTest = "Simes",
        conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmMeans object 'x18' with expected results
    expect_equal(x18$iterations[1, ], c(10, 10, 10, 10), label = paste0(x18$iterations[1, ]))
    expect_equal(x18$iterations[2, ], c(10, 9, 8, 10), label = paste0(x18$iterations[2, ]))
    expect_equal(x18$iterations[3, ], c(7, 8, 1, 4), label = paste0(x18$iterations[3, ]))
    expect_equal(x18$rejectAtLeastOne, c(0.3, 0.1, 0.7, 0.6), tolerance = 1e-07, label = paste0(x18$rejectAtLeastOne))
    expect_equal(unlist(as.list(x18$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0.1, 0.2, 0, 0, 0.1, 0, 0.2, 0.3, 0, 0.1, 0.3, 0), tolerance = 1e-07, label = paste0(unlist(as.list(x18$rejectedArmsPerStage))))
    expect_equal(x18$futilityStop, c(0, 0.1, 0.2, 0), tolerance = 1e-07, label = paste0(x18$futilityStop))
    expect_equal(x18$futilityPerStage[1, ], c(0, 0.1, 0.2, 0), tolerance = 1e-07, label = paste0(x18$futilityPerStage[1, ]))
    expect_equal(x18$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(x18$futilityPerStage[2, ]))
    expect_equal(x18$earlyStop[1, ], c(0, 0.1, 0.2, 0), tolerance = 1e-07, label = paste0(x18$earlyStop[1, ]))
    expect_equal(x18$earlyStop[2, ], c(0.3, 0.1, 0.7, 0.6), tolerance = 1e-07, label = paste0(x18$earlyStop[2, ]))
    expect_equal(x18$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x18$successPerStage[1, ]))
    expect_equal(x18$successPerStage[2, ], c(0.3, 0.1, 0.7, 0.6), tolerance = 1e-07, label = paste0(x18$successPerStage[2, ]))
    expect_equal(x18$successPerStage[3, ], c(0, 0, 0, 0), label = paste0(x18$successPerStage[3, ]))
    expect_equal(unlist(as.list(x18$selectedArms)), c(1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.1, 0, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0, 0, 1, 0.1, 0, 1, 0.1, 0.1, 1, 0.2, 0.2, 1, 0.1, 0, 1, 0.4, 0.3, 1, 0.5, 0.2, 1, 0.2, 0.1, 1, 0.6, 0.1, 1, 0.4, 0, 1, 1, 0.7, 1, 0.9, 0.8, 1, 0.8, 0.1, 1, 1, 0.4), tolerance = 1e-07, label = paste0(unlist(as.list(x18$selectedArms))))
    expect_equal(x18$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x18$numberOfActiveArms[1, ]))
    expect_equal(x18$numberOfActiveArms[2, ], c(1, 1, 1, 1), label = paste0(x18$numberOfActiveArms[2, ]))
    expect_equal(x18$numberOfActiveArms[3, ], c(1, 1, 1, 1), label = paste0(x18$numberOfActiveArms[3, ]))
    expect_equal(x18$expectedNumberOfSubjects, c(179.95701, 273.63073, 113.26043, 249.89211), tolerance = 1e-07, label = paste0(x18$expectedNumberOfSubjects))
    expect_equal(unlist(as.list(x18$sampleSizes)), c(10, 1.1167748, 1.5953926, 10, 5.9718999, 6.7183874, 10, 0.5, 0, 10, 10, 25, 10, 6.7277808, 9.6111155, 10, 37.581628, 42.279332, 10, 0, 0, 10, 10, 0, 10, 2.4005123, 3.4293032, 10, 8.5110901, 9.5749763, 10, 11.503905, 0, 10, 32.126443, 55.316107, 10, 28.865098, 22.318956, 10, 15.610585, 5.061908, 10, 24.075387, 27.667829, 10, 15.693169, 0, 10, 39.110166, 36.954767, 10, 67.675203, 63.634604, 10, 36.079292, 27.667829, 10, 67.819612, 80.316107), tolerance = 1e-07, label = paste0(unlist(as.list(x18$sampleSizes))))
    expect_equal(x18$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(x18$conditionalPowerAchieved[1, ]))
    expect_equal(x18$conditionalPowerAchieved[2, ], c(0.064552587, 0.050542809, 0.13271614, 0.098246228), tolerance = 1e-07, label = paste0(x18$conditionalPowerAchieved[2, ]))
    expect_equal(x18$conditionalPowerAchieved[3, ], c(0.1164829, 0.22353174, 0.16556673, 0.12567304), tolerance = 1e-07, label = paste0(x18$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x18), NA)))
        expect_output(print(x18)$show())
        invisible(capture.output(expect_error(summary(x18), NA)))
        expect_output(summary(x18)$show())
        x18CodeBased <- eval(parse(text = getObjectRCode(x18, stringWrapParagraphWidth = NULL)))
        expect_equal(x18CodeBased$iterations, x18$iterations, tolerance = 1e-07)
        expect_equal(x18CodeBased$rejectAtLeastOne, x18$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x18CodeBased$rejectedArmsPerStage, x18$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x18CodeBased$futilityStop, x18$futilityStop, tolerance = 1e-07)
        expect_equal(x18CodeBased$futilityPerStage, x18$futilityPerStage, tolerance = 1e-07)
        expect_equal(x18CodeBased$earlyStop, x18$earlyStop, tolerance = 1e-07)
        expect_equal(x18CodeBased$successPerStage, x18$successPerStage, tolerance = 1e-07)
        expect_equal(x18CodeBased$selectedArms, x18$selectedArms, tolerance = 1e-07)
        expect_equal(x18CodeBased$numberOfActiveArms, x18$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x18CodeBased$expectedNumberOfSubjects, x18$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(x18CodeBased$sampleSizes, x18$sampleSizes, tolerance = 1e-07)
        expect_equal(x18CodeBased$conditionalPowerAchieved, x18$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x18), "character")
        df <- as.data.frame(x18)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x18)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x19 <- getSimulationMultiArmMeans(
        seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)),
        activeArms = 4, threshold = 0, typeOfSelection = "all",
        plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1),
        adaptations = c(TRUE, FALSE), intersectionTest = "Hierarchical",
        conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmMeans object 'x19' with expected results
    expect_equal(x19$iterations[1, ], c(10, 10, 10, 10), label = paste0(x19$iterations[1, ]))
    expect_equal(x19$iterations[2, ], c(10, 7, 9, 10), label = paste0(x19$iterations[2, ]))
    expect_equal(x19$iterations[3, ], c(6, 3, 4, 6), label = paste0(x19$iterations[3, ]))
    expect_equal(x19$rejectAtLeastOne, c(0.1, 0, 0.1, 0), tolerance = 1e-07, label = paste0(x19$rejectAtLeastOne))
    expect_equal(unlist(as.list(x19$rejectedArmsPerStage)), c(0, 0.1, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0), tolerance = 1e-07, label = paste0(unlist(as.list(x19$rejectedArmsPerStage))))
    expect_equal(x19$futilityStop, c(0.4, 0.7, 0.6, 0.4), tolerance = 1e-07, label = paste0(x19$futilityStop))
    expect_equal(x19$futilityPerStage[1, ], c(0, 0.3, 0.1, 0), tolerance = 1e-07, label = paste0(x19$futilityPerStage[1, ]))
    expect_equal(x19$futilityPerStage[2, ], c(0.4, 0.4, 0.5, 0.4), tolerance = 1e-07, label = paste0(x19$futilityPerStage[2, ]))
    expect_equal(x19$earlyStop[1, ], c(0, 0.3, 0.1, 0), tolerance = 1e-07, label = paste0(x19$earlyStop[1, ]))
    expect_equal(x19$earlyStop[2, ], c(0.4, 0.4, 0.5, 0.4), tolerance = 1e-07, label = paste0(x19$earlyStop[2, ]))
    expect_equal(x19$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x19$successPerStage[1, ]))
    expect_equal(x19$successPerStage[2, ], c(0, 0, 0, 0), label = paste0(x19$successPerStage[2, ]))
    expect_equal(x19$successPerStage[3, ], c(0, 0, 0.1, 0), tolerance = 1e-07, label = paste0(x19$successPerStage[3, ]))
    expect_equal(unlist(as.list(x19$selectedArms)), c(1, 0.6, 0.6, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0.6, 0.6, 1, 0.6, 0.4, 1, 0.5, 0.2, 1, 0.7, 0.3, 1, 1, 0.6, 1, 0.6, 0.4, 1, 0.7, 0.3, 1, 0.7, 0.4, 1, 0.7, 0.4, 1, 0.7, 0.3, 1, 0.5, 0.2, 1, 0.9, 0.4, 1, 0.8, 0.5, 1, 1, 0.6, 1, 0.7, 0.3, 1, 0.9, 0.4, 1, 1, 0.6), tolerance = 1e-07, label = paste0(unlist(as.list(x19$selectedArms))))
    expect_equal(x19$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x19$numberOfActiveArms[1, ]))
    expect_equal(x19$numberOfActiveArms[2, ], c(2.5, 2.8571429, 3, 3.1), tolerance = 1e-07, label = paste0(x19$numberOfActiveArms[2, ]))
    expect_equal(x19$numberOfActiveArms[3, ], c(2.8333333, 3.3333333, 3.75, 3.5), tolerance = 1e-07, label = paste0(x19$numberOfActiveArms[3, ]))
    expect_equal(x19$expectedNumberOfSubjects, c(600.66781, 398.09964, 600, 634), tolerance = 1e-07, label = paste0(x19$expectedNumberOfSubjects))
    expect_equal(unlist(as.list(x19$sampleSizes)), c(10, 56.333476, 93.889127, 10, 42.857143, 100, 10, 44.444444, 100, 10, 50.4, 84, 10, 56.333476, 60.555794, 10, 52.89273, 66.666667, 10, 77.777778, 75, 10, 90.4, 84, 10, 60, 66.666667, 10, 81.464159, 100, 10, 77.777778, 100, 10, 60.4, 50.666667, 10, 66.333476, 43.889127, 10, 52.89273, 66.666667, 10, 100, 100, 10, 70.4, 67.333333, 10, 96.333476, 93.889127, 10, 81.464159, 100, 10, 100, 100, 10, 90.4, 84), tolerance = 1e-07, label = paste0(unlist(as.list(x19$sampleSizes))))
    expect_equal(x19$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(x19$conditionalPowerAchieved[1, ]))
    expect_equal(x19$conditionalPowerAchieved[2, ], c(0.014835699, 0.082104288, 0.088043543, 0.18689602), tolerance = 1e-07, label = paste0(x19$conditionalPowerAchieved[2, ]))
    expect_equal(x19$conditionalPowerAchieved[3, ], c(0.35039062, 0.35957167, 0.84477407, 0.62586447), tolerance = 1e-07, label = paste0(x19$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x19), NA)))
        expect_output(print(x19)$show())
        invisible(capture.output(expect_error(summary(x19), NA)))
        expect_output(summary(x19)$show())
        x19CodeBased <- eval(parse(text = getObjectRCode(x19, stringWrapParagraphWidth = NULL)))
        expect_equal(x19CodeBased$iterations, x19$iterations, tolerance = 1e-07)
        expect_equal(x19CodeBased$rejectAtLeastOne, x19$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x19CodeBased$rejectedArmsPerStage, x19$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x19CodeBased$futilityStop, x19$futilityStop, tolerance = 1e-07)
        expect_equal(x19CodeBased$futilityPerStage, x19$futilityPerStage, tolerance = 1e-07)
        expect_equal(x19CodeBased$earlyStop, x19$earlyStop, tolerance = 1e-07)
        expect_equal(x19CodeBased$successPerStage, x19$successPerStage, tolerance = 1e-07)
        expect_equal(x19CodeBased$selectedArms, x19$selectedArms, tolerance = 1e-07)
        expect_equal(x19CodeBased$numberOfActiveArms, x19$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x19CodeBased$expectedNumberOfSubjects, x19$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(x19CodeBased$sampleSizes, x19$sampleSizes, tolerance = 1e-07)
        expect_equal(x19CodeBased$conditionalPowerAchieved, x19$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x19), "character")
        df <- as.data.frame(x19)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x19)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x20 <- getSimulationMultiArmMeans(
        seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), activeArms = 4, threshold = 0,
        typeOfSelection = "rBest", rValue = 2,
        plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1),
        adaptations = c(TRUE, FALSE), intersectionTest = "Hierarchical",
        conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmMeans object 'x20' with expected results
    expect_equal(x20$iterations[1, ], c(10, 10, 10, 10), label = paste0(x20$iterations[1, ]))
    expect_equal(x20$iterations[2, ], c(9, 9, 8, 10), label = paste0(x20$iterations[2, ]))
    expect_equal(x20$iterations[3, ], c(2, 6, 3, 2), label = paste0(x20$iterations[3, ]))
    expect_equal(x20$rejectAtLeastOne, c(0, 0.2, 0, 0.1), tolerance = 1e-07, label = paste0(x20$rejectAtLeastOne))
    expect_equal(unlist(as.list(x20$rejectedArmsPerStage)), c(0, 0, 0, 0.1, 0, 0.1, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), tolerance = 1e-07, label = paste0(unlist(as.list(x20$rejectedArmsPerStage))))
    expect_equal(x20$futilityStop, c(0.8, 0.4, 0.7, 0.7), tolerance = 1e-07, label = paste0(x20$futilityStop))
    expect_equal(x20$futilityPerStage[1, ], c(0.1, 0.1, 0.2, 0), tolerance = 1e-07, label = paste0(x20$futilityPerStage[1, ]))
    expect_equal(x20$futilityPerStage[2, ], c(0.7, 0.3, 0.5, 0.7), tolerance = 1e-07, label = paste0(x20$futilityPerStage[2, ]))
    expect_equal(x20$earlyStop[1, ], c(0.1, 0.1, 0.2, 0), tolerance = 1e-07, label = paste0(x20$earlyStop[1, ]))
    expect_equal(x20$earlyStop[2, ], c(0.7, 0.3, 0.5, 0.8), tolerance = 1e-07, label = paste0(x20$earlyStop[2, ]))
    expect_equal(x20$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x20$successPerStage[1, ]))
    expect_equal(x20$successPerStage[2, ], c(0, 0, 0, 0.1), tolerance = 1e-07, label = paste0(x20$successPerStage[2, ]))
    expect_equal(x20$successPerStage[3, ], c(0, 0, 0, 0), label = paste0(x20$successPerStage[3, ]))
    expect_equal(unlist(as.list(x20$selectedArms)), c(1, 0.2, 0.2, 1, 0.6, 0.6, 1, 0.3, 0.3, 1, 0.3, 0.2, 1, 0.4, 0, 1, 0.1, 0, 1, 0.3, 0, 1, 0.5, 0, 1, 0.3, 0, 1, 0.3, 0.1, 1, 0.3, 0.1, 1, 0.4, 0, 1, 0.8, 0.2, 1, 0.8, 0.5, 1, 0.5, 0.2, 1, 0.8, 0.2, 1, 0.9, 0.2, 1, 0.9, 0.6, 1, 0.8, 0.3, 1, 1, 0.2), tolerance = 1e-07, label = paste0(unlist(as.list(x20$selectedArms))))
    expect_equal(x20$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x20$numberOfActiveArms[1, ]))
    expect_equal(x20$numberOfActiveArms[2, ], c(1.8888889, 2, 1.75, 2), tolerance = 1e-07, label = paste0(x20$numberOfActiveArms[2, ]))
    expect_equal(x20$numberOfActiveArms[3, ], c(2, 2, 2, 2), label = paste0(x20$numberOfActiveArms[3, ]))
    expect_equal(x20$expectedNumberOfSubjects, c(307.09166, 377.99189, 286.78887, 300.60787), tolerance = 1e-07, label = paste0(x20$expectedNumberOfSubjects))
    expect_equal(unlist(as.list(x20$sampleSizes)), c(10, 22.222222, 100, 10, 47.097546, 70.646318, 10, 30.537829, 81.43421, 10, 25.370782, 76.853911, 10, 33.228314, 0, 10, 11.111111, 0, 10, 27.257545, 0, 10, 38.448036, 0, 10, 17.763874, 0, 10, 27.283387, 16.666667, 10, 33.529937, 33.333333, 10, 22.273651, 0, 10, 69.075708, 100, 10, 63.269822, 53.979652, 10, 39.758676, 48.100877, 10, 50.237878, 76.853911, 10, 76.700615, 100, 10, 74.380933, 70.646318, 10, 73.288614, 81.43421, 10, 68.165174, 76.853911), tolerance = 1e-07, label = paste0(unlist(as.list(x20$sampleSizes))))
    expect_equal(x20$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(x20$conditionalPowerAchieved[1, ]))
    expect_equal(x20$conditionalPowerAchieved[2, ], c(0.0535706, 0.15544115, 0.10470149, 0.094637028), tolerance = 1e-07, label = paste0(x20$conditionalPowerAchieved[2, ]))
    expect_equal(x20$conditionalPowerAchieved[3, ], c(0.09464551, 0.36740056, 0.23354895, 0.75738479), tolerance = 1e-07, label = paste0(x20$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x20), NA)))
        expect_output(print(x20)$show())
        invisible(capture.output(expect_error(summary(x20), NA)))
        expect_output(summary(x20)$show())
        x20CodeBased <- eval(parse(text = getObjectRCode(x20, stringWrapParagraphWidth = NULL)))
        expect_equal(x20CodeBased$iterations, x20$iterations, tolerance = 1e-07)
        expect_equal(x20CodeBased$rejectAtLeastOne, x20$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x20CodeBased$rejectedArmsPerStage, x20$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x20CodeBased$futilityStop, x20$futilityStop, tolerance = 1e-07)
        expect_equal(x20CodeBased$futilityPerStage, x20$futilityPerStage, tolerance = 1e-07)
        expect_equal(x20CodeBased$earlyStop, x20$earlyStop, tolerance = 1e-07)
        expect_equal(x20CodeBased$successPerStage, x20$successPerStage, tolerance = 1e-07)
        expect_equal(x20CodeBased$selectedArms, x20$selectedArms, tolerance = 1e-07)
        expect_equal(x20CodeBased$numberOfActiveArms, x20$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x20CodeBased$expectedNumberOfSubjects, x20$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(x20CodeBased$sampleSizes, x20$sampleSizes, tolerance = 1e-07)
        expect_equal(x20CodeBased$conditionalPowerAchieved, x20$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x20), "character")
        df <- as.data.frame(x20)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x20)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x21 <- getSimulationMultiArmMeans(
        seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)),
        activeArms = 4, threshold = 0, typeOfSelection = "epsilon", epsilonValue = 0.1,
        plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1),
        adaptations = c(TRUE, FALSE), intersectionTest = "Hierarchical",
        conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmMeans object 'x21' with expected results
    expect_equal(x21$iterations[1, ], c(10, 10, 10, 10), label = paste0(x21$iterations[1, ]))
    expect_equal(x21$iterations[2, ], c(9, 9, 10, 10), label = paste0(x21$iterations[2, ]))
    expect_equal(x21$iterations[3, ], c(1, 1, 3, 0), label = paste0(x21$iterations[3, ]))
    expect_equal(x21$rejectAtLeastOne, c(0, 0.1, 0.1, 0), tolerance = 1e-07, label = paste0(x21$rejectAtLeastOne))
    expect_equal(unlist(as.list(x21$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), tolerance = 1e-07, label = paste0(unlist(as.list(x21$rejectedArmsPerStage))))
    expect_equal(x21$futilityStop, c(0.9, 0.9, 0.6, 1), tolerance = 1e-07, label = paste0(x21$futilityStop))
    expect_equal(x21$futilityPerStage[1, ], c(0.1, 0.1, 0, 0), tolerance = 1e-07, label = paste0(x21$futilityPerStage[1, ]))
    expect_equal(x21$futilityPerStage[2, ], c(0.8, 0.8, 0.6, 1), tolerance = 1e-07, label = paste0(x21$futilityPerStage[2, ]))
    expect_equal(x21$earlyStop[1, ], c(0.1, 0.1, 0, 0), tolerance = 1e-07, label = paste0(x21$earlyStop[1, ]))
    expect_equal(x21$earlyStop[2, ], c(0.8, 0.8, 0.7, 1), tolerance = 1e-07, label = paste0(x21$earlyStop[2, ]))
    expect_equal(x21$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x21$successPerStage[1, ]))
    expect_equal(x21$successPerStage[2, ], c(0, 0, 0.1, 0), tolerance = 1e-07, label = paste0(x21$successPerStage[2, ]))
    expect_equal(x21$successPerStage[3, ], c(0, 0.1, 0, 0), tolerance = 1e-07, label = paste0(x21$successPerStage[3, ]))
    expect_equal(unlist(as.list(x21$selectedArms)), c(1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.4, 0.3, 1, 0, 0, 1, 0.3, 0, 1, 0.3, 0, 1, 0, 0, 1, 0.3, 0, 1, 0.3, 0, 1, 0.2, 0, 1, 0.2, 0, 1, 0.1, 0, 1, 0.3, 0, 1, 0.5, 0, 1, 0.5, 0.1, 1, 0.6, 0, 1, 0.9, 0.1, 1, 0.9, 0.1, 1, 1, 0.3, 1, 1, 0), tolerance = 1e-07, label = paste0(unlist(as.list(x21$selectedArms))))
    expect_equal(x21$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x21$numberOfActiveArms[1, ]))
    expect_equal(x21$numberOfActiveArms[2, ], c(1.1111111, 1.2222222, 1.1, 1), tolerance = 1e-07, label = paste0(x21$numberOfActiveArms[2, ]))
    expect_equal(x21$numberOfActiveArms[3, ], c(1, 1, 1.3333333, NaN), tolerance = 1e-07, label = paste0(x21$numberOfActiveArms[3, ]))
    expect_equal(x21$expectedNumberOfSubjects, c(190.08367, 169.68391, 280.67025, NaN), tolerance = 1e-07, label = paste0(x21$expectedNumberOfSubjects))
    expect_equal(unlist(as.list(x21$sampleSizes)), c(10, 4.4952582, 40.457324, 10, 11.111111, 100, 10, 30.407004, 99.157615, 10, 0, NaN, 10, 19.514172, 0, 10, 21.967417, 0, 10, 0, 0, 10, 21.272121, NaN, 10, 25.406757, 0, 10, 11.52108, 0, 10, 16.622221, 0, 10, 10, NaN, 10, 25.603407, 0, 10, 20.034041, 0, 10, 38.558614, 33.333333, 10, 21.010924, NaN, 10, 71.638409, 40.457324, 10, 46.126253, 100, 10, 75.587839, 99.157615, 10, 52.283045, NaN), tolerance = 1e-07, label = paste0(unlist(as.list(x21$sampleSizes))))
    expect_equal(x21$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(x21$conditionalPowerAchieved[1, ]))
    expect_equal(x21$conditionalPowerAchieved[2, ], c(0.023159424, 0.14301241, 0.046563399, 0.11230633), tolerance = 1e-07, label = paste0(x21$conditionalPowerAchieved[2, ]))
    expect_equal(x21$conditionalPowerAchieved[3, ], c(0.07537462, 0.00060378387, 0.33359002, NaN), tolerance = 1e-07, label = paste0(x21$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x21), NA)))
        expect_output(print(x21)$show())
        invisible(capture.output(expect_error(summary(x21), NA)))
        expect_output(summary(x21)$show())
        x21CodeBased <- eval(parse(text = getObjectRCode(x21, stringWrapParagraphWidth = NULL)))
        expect_equal(x21CodeBased$iterations, x21$iterations, tolerance = 1e-07)
        expect_equal(x21CodeBased$rejectAtLeastOne, x21$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x21CodeBased$rejectedArmsPerStage, x21$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x21CodeBased$futilityStop, x21$futilityStop, tolerance = 1e-07)
        expect_equal(x21CodeBased$futilityPerStage, x21$futilityPerStage, tolerance = 1e-07)
        expect_equal(x21CodeBased$earlyStop, x21$earlyStop, tolerance = 1e-07)
        expect_equal(x21CodeBased$successPerStage, x21$successPerStage, tolerance = 1e-07)
        expect_equal(x21CodeBased$selectedArms, x21$selectedArms, tolerance = 1e-07)
        expect_equal(x21CodeBased$numberOfActiveArms, x21$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x21CodeBased$expectedNumberOfSubjects, x21$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(x21CodeBased$sampleSizes, x21$sampleSizes, tolerance = 1e-07)
        expect_equal(x21CodeBased$conditionalPowerAchieved, x21$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x21), "character")
        df <- as.data.frame(x21)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x21)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x22 <- getSimulationMultiArmMeans(
        seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)),
        activeArms = 4, threshold = 0.1,
        plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.1, 0.3, 0.1),
        conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 1
    )

    ## Comparison of the results of SimulationResultsMultiArmMeans object 'x22' with expected results
    expect_equal(x22$iterations[1, ], c(1, 1, 1), label = paste0(x22$iterations[1, ]))
    expect_equal(x22$iterations[2, ], c(1, 1, 1), label = paste0(x22$iterations[2, ]))
    expect_equal(x22$iterations[3, ], c(0, 1, 1), label = paste0(x22$iterations[3, ]))
    expect_equal(x22$rejectAtLeastOne, c(0, 0, 0), label = paste0(x22$rejectAtLeastOne))
    expect_equal(unlist(as.list(x22$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), label = paste0(unlist(as.list(x22$rejectedArmsPerStage))))
    expect_equal(x22$futilityStop, c(1, 0, 0), label = paste0(x22$futilityStop))
    expect_equal(x22$futilityPerStage[1, ], c(0, 0, 0), label = paste0(x22$futilityPerStage[1, ]))
    expect_equal(x22$futilityPerStage[2, ], c(1, 0, 0), label = paste0(x22$futilityPerStage[2, ]))
    expect_equal(x22$earlyStop[1, ], c(0, 0, 0), label = paste0(x22$earlyStop[1, ]))
    expect_equal(x22$earlyStop[2, ], c(1, 0, 0), label = paste0(x22$earlyStop[2, ]))
    expect_equal(x22$successPerStage[1, ], c(0, 0, 0), label = paste0(x22$successPerStage[1, ]))
    expect_equal(x22$successPerStage[2, ], c(0, 0, 0), label = paste0(x22$successPerStage[2, ]))
    expect_equal(x22$successPerStage[3, ], c(0, 0, 0), label = paste0(x22$successPerStage[3, ]))
    expect_equal(unlist(as.list(x22$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1), label = paste0(unlist(as.list(x22$selectedArms))))
    expect_equal(x22$numberOfActiveArms[1, ], c(4, 4, 4), label = paste0(x22$numberOfActiveArms[1, ]))
    expect_equal(x22$numberOfActiveArms[2, ], c(1, 1, 1), label = paste0(x22$numberOfActiveArms[2, ]))
    expect_equal(x22$numberOfActiveArms[3, ], c(NaN, 1, 1), label = paste0(x22$numberOfActiveArms[3, ]))
    expect_equal(x22$expectedNumberOfSubjects, c(NaN, 450, 148.90979), tolerance = 1e-07, label = paste0(x22$expectedNumberOfSubjects))
    expect_equal(unlist(as.list(x22$sampleSizes)), c(10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 32.875253, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 100, 100, 10, 10.358511, 39.096382, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 32.875253, 0, 10, 100, 100, 10, 10.358511, 39.096382), tolerance = 1e-07, label = paste0(unlist(as.list(x22$sampleSizes))))
    expect_equal(x22$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_), label = paste0(x22$conditionalPowerAchieved[1, ]))
    expect_equal(x22$conditionalPowerAchieved[2, ], c(0.011749146, 0.0034013018, 0.045375018), tolerance = 1e-07, label = paste0(x22$conditionalPowerAchieved[2, ]))
    expect_equal(x22$conditionalPowerAchieved[3, ], c(NaN, 0.15769372, 0.8), tolerance = 1e-07, label = paste0(x22$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x22), NA)))
        expect_output(print(x22)$show())
        invisible(capture.output(expect_error(summary(x22), NA)))
        expect_output(summary(x22)$show())
        x22CodeBased <- eval(parse(text = getObjectRCode(x22, stringWrapParagraphWidth = NULL)))
        expect_equal(x22CodeBased$iterations, x22$iterations, tolerance = 1e-07)
        expect_equal(x22CodeBased$rejectAtLeastOne, x22$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x22CodeBased$rejectedArmsPerStage, x22$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x22CodeBased$futilityStop, x22$futilityStop, tolerance = 1e-07)
        expect_equal(x22CodeBased$futilityPerStage, x22$futilityPerStage, tolerance = 1e-07)
        expect_equal(x22CodeBased$earlyStop, x22$earlyStop, tolerance = 1e-07)
        expect_equal(x22CodeBased$successPerStage, x22$successPerStage, tolerance = 1e-07)
        expect_equal(x22CodeBased$selectedArms, x22$selectedArms, tolerance = 1e-07)
        expect_equal(x22CodeBased$numberOfActiveArms, x22$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x22CodeBased$expectedNumberOfSubjects, x22$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(x22CodeBased$sampleSizes, x22$sampleSizes, tolerance = 1e-07)
        expect_equal(x22CodeBased$conditionalPowerAchieved, x22$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x22), "character")
        df <- as.data.frame(x22)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x22)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSimulationMultiArmMeans': using calcSubjectsFunction", {
    .skipTestIfDisabled()

    calcSubjectsFunctionSimulationMultiArmMeans <- function(..., stage, minNumberOfSubjectsPerStage) {
        return(ifelse(stage == 3, 33, minNumberOfSubjectsPerStage[stage]))
    }

    x <- getSimulationMultiArmMeans(
        seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), typeOfShape = "linear", activeArms = 4,
        plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
        minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 10, calcSubjectsFunction = calcSubjectsFunctionSimulationMultiArmMeans
    )

    ## Comparison of the results of SimulationResultsMultiArmMeans object 'x' with expected results
    expect_equal(x$iterations[1, ], c(10, 10, 10, 10), label = paste0(x$iterations[1, ]))
    expect_equal(x$iterations[2, ], c(10, 10, 10, 10), label = paste0(x$iterations[2, ]))
    expect_equal(x$iterations[3, ], c(9, 9, 8, 8), label = paste0(x$iterations[3, ]))
    expect_equal(x$rejectAtLeastOne, c(0.1, 0.1, 0.3, 0.4), tolerance = 1e-07, label = paste0(x$rejectAtLeastOne))
    expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0.1, 0, 0.2), tolerance = 1e-07, label = paste0(unlist(as.list(x$rejectedArmsPerStage))))
    expect_equal(x$futilityStop, c(0, 0, 0, 0), label = paste0(x$futilityStop))
    expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0), label = paste0(x$futilityPerStage[1, ]))
    expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(x$futilityPerStage[2, ]))
    expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0), label = paste0(x$earlyStop[1, ]))
    expect_equal(x$earlyStop[2, ], c(0.1, 0.1, 0.2, 0.2), tolerance = 1e-07, label = paste0(x$earlyStop[2, ]))
    expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x$successPerStage[1, ]))
    expect_equal(x$successPerStage[2, ], c(0.1, 0.1, 0.2, 0.2), tolerance = 1e-07, label = paste0(x$successPerStage[2, ]))
    expect_equal(x$successPerStage[3, ], c(0, 0, 0.1, 0.2), tolerance = 1e-07, label = paste0(x$successPerStage[3, ]))
    expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.1, 0, 1, 0, 0, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0, 0, 1, 0.3, 0.2, 1, 0.1, 0.1, 1, 0.3, 0.2, 1, 0.4, 0.4, 1, 0.1, 0.1, 1, 0.5, 0.4, 1, 0.2, 0.2, 1, 0.5, 0.4, 1, 0.6, 0.5, 1, 1, 0.9, 1, 1, 0.9, 1, 1, 0.8, 1, 1, 0.8), tolerance = 1e-07, label = paste0(unlist(as.list(x$selectedArms))))
    expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x$numberOfActiveArms[1, ]))
    expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1), label = paste0(x$numberOfActiveArms[2, ]))
    expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1), label = paste0(x$numberOfActiveArms[3, ]))
    expect_equal(x$expectedNumberOfSubjects, c(117.4, 117.4, 110.8, 110.8), tolerance = 1e-07, label = paste0(x$expectedNumberOfSubjects))
    expect_equal(unlist(as.list(x$sampleSizes)), c(10, 0.4, 3.6666667, 10, 0.4, 3.6666667, 10, 0.4, 0, 10, 0, 0, 10, 1.2, 11, 10, 1.6, 14.666667, 10, 0, 0, 10, 1.2, 8.25, 10, 0.4, 3.6666667, 10, 1.2, 7.3333333, 10, 1.6, 16.5, 10, 0.4, 4.125, 10, 2, 14.666667, 10, 0.8, 7.3333333, 10, 2, 16.5, 10, 2.4, 20.625, 10, 4, 33, 10, 4, 33, 10, 4, 33, 10, 4, 33), tolerance = 1e-07, label = paste0(unlist(as.list(x$sampleSizes))))
    expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(x$conditionalPowerAchieved[1, ]))
    expect_equal(x$conditionalPowerAchieved[2, ], c(0.054038913, 0.015750083, 0.11207917, 0.055949011), tolerance = 1e-07, label = paste0(x$conditionalPowerAchieved[2, ]))
    expect_equal(x$conditionalPowerAchieved[3, ], c(0.44922292, 0.31010643, 0.28872426, 0.56321232), tolerance = 1e-07, label = paste0(x$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x), NA)))
        expect_output(print(x)$show())
        invisible(capture.output(expect_error(summary(x), NA)))
        expect_output(summary(x)$show())
        xCodeBased <- eval(parse(text = getObjectRCode(x, stringWrapParagraphWidth = NULL)))
        expect_equal(xCodeBased$iterations, x$iterations, tolerance = 1e-07)
        expect_equal(xCodeBased$rejectAtLeastOne, x$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(xCodeBased$rejectedArmsPerStage, x$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(xCodeBased$futilityStop, x$futilityStop, tolerance = 1e-07)
        expect_equal(xCodeBased$futilityPerStage, x$futilityPerStage, tolerance = 1e-07)
        expect_equal(xCodeBased$earlyStop, x$earlyStop, tolerance = 1e-07)
        expect_equal(xCodeBased$successPerStage, x$successPerStage, tolerance = 1e-07)
        expect_equal(xCodeBased$selectedArms, x$selectedArms, tolerance = 1e-07)
        expect_equal(xCodeBased$numberOfActiveArms, x$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(xCodeBased$expectedNumberOfSubjects, x$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(xCodeBased$sampleSizes, x$sampleSizes, tolerance = 1e-07)
        expect_equal(xCodeBased$conditionalPowerAchieved, x$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x), "character")
        df <- as.data.frame(x)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSimulationMultiArmMeans': using selectArmsFunction", {
    .skipTestIfDisabled()

    # @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
    # @refFS[Sec.]{fs:sec:simulatingMultiArmDesigns}
    # @refFS[Sec.]{fs:sec:simulatingMultiArmDoseResponseRelationShips}
    # @refFS[Sec.]{fs:sec:simulatingMultiArmSelections}
    # @refFS[Tab.]{fs:tab:output:getSimulationMultiArmMeans}
    # @refFS[Formula]{fs:simulationMultiArmDoseResponse}
    # @refFS[Formula]{fs:simulationMultiArmMeansGenerate}
    # @refFS[Formula]{fs:simulationMultiArmMeansTestStatistics}
    # @refFS[Formula]{fs:simulationMultiArmSelections}
    # @refFS[Formula]{fs:multiarmRejectionRule}
    # @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
    # @refFS[Formula]{fs:adjustedPValueSubsetSidak}
    # @refFS[Formula]{fs:adjustedPValueSubsetSimes}
    selectArmsFunctionSimulationMultiArmMeans <- function(effectSizes) {
        return(c(TRUE, FALSE, FALSE, FALSE))
    }

    x <- getSimulationMultiArmMeans(
        seed = 1234,
        getDesignFisher(informationRates = c(0.2, 0.6, 1)), typeOfShape = "linear", activeArms = 4,
        plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
        maxNumberOfIterations = 10, selectArmsFunction = selectArmsFunctionSimulationMultiArmMeans, typeOfSelection = "userDefined"
    )

    ## Comparison of the results of SimulationResultsMultiArmMeans object 'x' with expected results
    expect_equal(x$iterations[1, ], c(10, 10, 10, 10), label = paste0(x$iterations[1, ]))
    expect_equal(x$iterations[2, ], c(10, 10, 10, 10), label = paste0(x$iterations[2, ]))
    expect_equal(x$iterations[3, ], c(10, 9, 9, 10), label = paste0(x$iterations[3, ]))
    expect_equal(x$rejectAtLeastOne, c(0.1, 0.1, 0.2, 0.1), tolerance = 1e-07, label = paste0(x$rejectAtLeastOne))
    expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0.1, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0), tolerance = 1e-07, label = paste0(unlist(as.list(x$rejectedArmsPerStage))))
    expect_equal(x$futilityStop, c(0, 0, 0, 0), label = paste0(x$futilityStop))
    expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0), label = paste0(x$futilityPerStage[1, ]))
    expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(x$futilityPerStage[2, ]))
    expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0), label = paste0(x$earlyStop[1, ]))
    expect_equal(x$earlyStop[2, ], c(0, 0.1, 0.1, 0), tolerance = 1e-07, label = paste0(x$earlyStop[2, ]))
    expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x$successPerStage[1, ]))
    expect_equal(x$successPerStage[2, ], c(0, 0.1, 0.1, 0), tolerance = 1e-07, label = paste0(x$successPerStage[2, ]))
    expect_equal(x$successPerStage[3, ], c(0, 0, 0, 0), label = paste0(x$successPerStage[3, ]))
    expect_equal(unlist(as.list(x$selectedArms)), c(1, 1, 1, 1, 1, 0.9, 1, 1, 0.9, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0.9, 1, 1, 0.9, 1, 1, 1), tolerance = 1e-07, label = paste0(unlist(as.list(x$selectedArms))))
    expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x$numberOfActiveArms[1, ]))
    expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1), label = paste0(x$numberOfActiveArms[2, ]))
    expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1), label = paste0(x$numberOfActiveArms[3, ]))
    expect_equal(x$expectedNumberOfSubjects, c(130, 126, 126, 130), label = paste0(x$expectedNumberOfSubjects))
    expect_equal(unlist(as.list(x$sampleSizes)), c(10, 20, 20, 10, 20, 20, 10, 20, 20, 10, 20, 20, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 20, 20, 10, 20, 20, 10, 20, 20, 10, 20, 20), label = paste0(unlist(as.list(x$sampleSizes))))
    expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(x$conditionalPowerAchieved[1, ]))
    expect_equal(x$conditionalPowerAchieved[2, ], c(0.091251689, 0.027836233, 0.13855746, 0.12908437), tolerance = 1e-07, label = paste0(x$conditionalPowerAchieved[2, ]))
    expect_equal(x$conditionalPowerAchieved[3, ], c(0.071420101, 0.027813347, 0.076509581, 0.21688562), tolerance = 1e-07, label = paste0(x$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x), NA)))
        expect_output(print(x)$show())
        invisible(capture.output(expect_error(summary(x), NA)))
        expect_output(summary(x)$show())
        xCodeBased <- eval(parse(text = getObjectRCode(x, stringWrapParagraphWidth = NULL)))
        expect_equal(xCodeBased$iterations, x$iterations, tolerance = 1e-07)
        expect_equal(xCodeBased$rejectAtLeastOne, x$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(xCodeBased$rejectedArmsPerStage, x$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(xCodeBased$futilityStop, x$futilityStop, tolerance = 1e-07)
        expect_equal(xCodeBased$futilityPerStage, x$futilityPerStage, tolerance = 1e-07)
        expect_equal(xCodeBased$earlyStop, x$earlyStop, tolerance = 1e-07)
        expect_equal(xCodeBased$successPerStage, x$successPerStage, tolerance = 1e-07)
        expect_equal(xCodeBased$selectedArms, x$selectedArms, tolerance = 1e-07)
        expect_equal(xCodeBased$numberOfActiveArms, x$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(xCodeBased$expectedNumberOfSubjects, x$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(xCodeBased$sampleSizes, x$sampleSizes, tolerance = 1e-07)
        expect_equal(xCodeBased$conditionalPowerAchieved, x$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x), "character")
        df <- as.data.frame(x)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSimulationMultiArmMeans': using intersectionTest = 'Sidak' and typeOfSelection = 'rBest'", {
    .skipTestIfDisabled()

    designIN <- getDesignInverseNormal(typeOfDesign = "P", kMax = 3, futilityBounds = c(0, 0))
    x <- getSimulationMultiArmMeans(designIN,
        activeArms = 3, typeOfShape = "sigmoidEmax",
        muMaxVector = seq(0, 1, 0.2), gED50 = 2, plannedSubjects = cumsum(rep(20, 3)),
        intersectionTest = "Sidak", typeOfSelection = "rBest", rValue = 2, threshold = -Inf,
        successCriterion = "all", maxNumberOfIterations = 100, seed = 3456
    )

    ## Comparison of the results of SimulationResultsMultiArmMeans object 'x' with expected results
    expect_equal(x$iterations[1, ], c(100, 100, 100, 100, 100, 100), label = paste0(x$iterations[1, ]))
    expect_equal(x$iterations[2, ], c(42, 52, 69, 77, 88, 87), label = paste0(x$iterations[2, ]))
    expect_equal(x$iterations[3, ], c(30, 33, 61, 73, 80, 61), label = paste0(x$iterations[3, ]))
    expect_equal(x$rejectAtLeastOne, c(0.02, 0.03, 0.18, 0.33, 0.49, 0.8), tolerance = 1e-07, label = paste0(x$rejectAtLeastOne))
    expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0.01, 0, 0, 0, 0.01, 0.01, 0.02, 0.03, 0.01, 0.02, 0.01, 0.01, 0.06, 0.1, 0.04, 0.04, 0.01, 0, 0, 0.01, 0.02, 0, 0.03, 0, 0.03, 0.04, 0.06, 0.08, 0.08, 0.11, 0.1, 0.14, 0.27, 0.12, 0, 0, 0, 0, 0.01, 0, 0.02, 0.01, 0.08, 0.08, 0.05, 0.11, 0.09, 0.16, 0.13, 0.18, 0.25, 0.24), tolerance = 1e-07, label = paste0(unlist(as.list(x$rejectedArmsPerStage))))
    expect_equal(x$futilityStop, c(0.7, 0.66, 0.39, 0.23, 0.11, 0.07), tolerance = 1e-07, label = paste0(x$futilityStop))
    expect_equal(x$futilityPerStage[1, ], c(0.58, 0.48, 0.31, 0.22, 0.11, 0.07), tolerance = 1e-07, label = paste0(x$futilityPerStage[1, ]))
    expect_equal(x$futilityPerStage[2, ], c(0.12, 0.18, 0.08, 0.01, 0, 0), tolerance = 1e-07, label = paste0(x$futilityPerStage[2, ]))
    expect_equal(x$earlyStop[1, ], c(0.58, 0.48, 0.31, 0.23, 0.12, 0.13), tolerance = 1e-07, label = paste0(x$earlyStop[1, ]))
    expect_equal(x$earlyStop[2, ], c(0.12, 0.19, 0.08, 0.04, 0.08, 0.26), tolerance = 1e-07, label = paste0(x$earlyStop[2, ]))
    expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0.01, 0.01, 0.06), tolerance = 1e-07, label = paste0(x$successPerStage[1, ]))
    expect_equal(x$successPerStage[2, ], c(0, 0.01, 0, 0.03, 0.08, 0.26), tolerance = 1e-07, label = paste0(x$successPerStage[2, ]))
    expect_equal(x$successPerStage[3, ], c(0, 0, 0.03, 0.1, 0.16, 0.2), tolerance = 1e-07, label = paste0(x$successPerStage[3, ]))
    expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.25, 0.17, 1, 0.25, 0.16, 1, 0.31, 0.26, 1, 0.32, 0.3, 1, 0.42, 0.41, 1, 0.32, 0.26, 1, 0.32, 0.22, 1, 0.43, 0.26, 1, 0.48, 0.45, 1, 0.56, 0.54, 1, 0.63, 0.56, 1, 0.7, 0.47, 1, 0.27, 0.21, 1, 0.36, 0.24, 1, 0.59, 0.51, 1, 0.66, 0.62, 1, 0.71, 0.63, 1, 0.72, 0.49, 1, 0.42, 0.3, 1, 0.52, 0.33, 1, 0.69, 0.61, 1, 0.77, 0.73, 1, 0.88, 0.8, 1, 0.87, 0.61), tolerance = 1e-07, label = paste0(unlist(as.list(x$selectedArms))))
    expect_equal(x$numberOfActiveArms[1, ], c(3, 3, 3, 3, 3, 3), label = paste0(x$numberOfActiveArms[1, ]))
    expect_equal(x$numberOfActiveArms[2, ], c(2, 2, 2, 2, 2, 2), label = paste0(x$numberOfActiveArms[2, ]))
    expect_equal(x$numberOfActiveArms[3, ], c(2, 2, 2, 2, 2, 2), label = paste0(x$numberOfActiveArms[3, ]))
    expect_equal(x$expectedNumberOfSubjects, c(123.2, 131, 158, 170, 180.8, 168.8), tolerance = 1e-07, label = paste0(x$expectedNumberOfSubjects))
    expect_equal(unlist(as.list(x$sampleSizes)), c(20, 11.904762, 11.333333, 20, 9.6153846, 9.6969697, 20, 8.9855072, 8.5245902, 20, 8.3116883, 8.2191781, 20, 9.5454545, 10.25, 20, 7.3563218, 8.5245902, 20, 15.238095, 14.666667, 20, 16.538462, 15.757576, 20, 13.913043, 14.754098, 20, 14.545455, 14.794521, 20, 14.318182, 14, 20, 16.091954, 15.409836, 20, 12.857143, 14, 20, 13.846154, 14.545455, 20, 17.101449, 16.721311, 20, 17.142857, 16.986301, 20, 16.136364, 15.75, 20, 16.551724, 16.065574, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20), tolerance = 1e-07, label = paste0(unlist(as.list(x$sampleSizes))))
    expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(x$conditionalPowerAchieved[1, ]))
    expect_equal(x$conditionalPowerAchieved[2, ], c(0.058967382, 0.048523877, 0.17154294, 0.22180985, 0.2182802, 0.37414282), tolerance = 1e-07, label = paste0(x$conditionalPowerAchieved[2, ]))
    expect_equal(x$conditionalPowerAchieved[3, ], c(0.077820194, 0.14430526, 0.21266388, 0.28752608, 0.40185892, 0.5016109), tolerance = 1e-07, label = paste0(x$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x), NA)))
        expect_output(print(x)$show())
        invisible(capture.output(expect_error(summary(x), NA)))
        expect_output(summary(x)$show())
        xCodeBased <- eval(parse(text = getObjectRCode(x, stringWrapParagraphWidth = NULL)))
        expect_equal(xCodeBased$iterations, x$iterations, tolerance = 1e-07)
        expect_equal(xCodeBased$rejectAtLeastOne, x$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(xCodeBased$rejectedArmsPerStage, x$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(xCodeBased$futilityStop, x$futilityStop, tolerance = 1e-07)
        expect_equal(xCodeBased$futilityPerStage, x$futilityPerStage, tolerance = 1e-07)
        expect_equal(xCodeBased$earlyStop, x$earlyStop, tolerance = 1e-07)
        expect_equal(xCodeBased$successPerStage, x$successPerStage, tolerance = 1e-07)
        expect_equal(xCodeBased$selectedArms, x$selectedArms, tolerance = 1e-07)
        expect_equal(xCodeBased$numberOfActiveArms, x$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(xCodeBased$expectedNumberOfSubjects, x$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(xCodeBased$sampleSizes, x$sampleSizes, tolerance = 1e-07)
        expect_equal(xCodeBased$conditionalPowerAchieved, x$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x), "character")
        df <- as.data.frame(x)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSimulationMultiArmMeans': plot drift - comparison of raw values", {
    .skipTestIfDisabled()

    designPureConditionalDunnett <- getDesignInverseNormal(typeOfDesign = "asUser", userAlphaSpending = c(0, 0.025))
    designCombinationDunnett <- getDesignConditionalDunnett(informationAtInterim = 0.5, secondStageConditioning = TRUE)

    resultsPureConditionalDunnett <- getSimulationMultiArmMeans(designPureConditionalDunnett,
        activeArms = 3, muMaxVector = seq(0, 1, 0.2),
        typeOfShape = "linear", plannedSubjects = cumsum(rep(20, 2)), intersectionTest = "Dunnett",
        adaptations = TRUE, typeOfSelection = "best", effectMeasure = "effectEstimate",
        threshold = -Inf, maxNumberOfIterations = 100,
        allocationRatioPlanned = 1, seed = 123
    )

    resultsCombinationDunnett <- getSimulationMultiArmMeans(designCombinationDunnett,
        activeArms = 3, muMaxVector = seq(0, 1, 0.2),
        typeOfShape = "linear", plannedSubjects = cumsum(rep(20, 2)), intersectionTest = "Dunnett",
        adaptations = TRUE, typeOfSelection = "best", effectMeasure = "effectEstimate",
        threshold = -Inf, maxNumberOfIterations = 100,
        allocationRatioPlanned = 1, seed = 123
    )

    drift <- resultsPureConditionalDunnett$effectMatrix[nrow(resultsPureConditionalDunnett$effectMatrix), ]

    ## Comparison of the results of numeric object 'drift' with expected results
    expect_equal(drift, c(0, 0.2, 0.4, 0.6, 0.8, 1), tolerance = 1e-07, label = paste0(drift))
    expect_equal(resultsPureConditionalDunnett$rejectAtLeastOne, resultsCombinationDunnett$rejectAtLeastOne, tolerance = 0.06)
})

test_that("'getSimulationMultiArmMeans': comparison of base and multi-arm", {
    .skipTestIfDisabled()

    # @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
    # @refFS[Sec.]{fs:sec:simulatingMultiArmDesigns}
    # @refFS[Sec.]{fs:sec:simulatingMultiArmDoseResponseRelationShips}
    # @refFS[Sec.]{fs:sec:simulatingMultiArmSelections}
    # @refFS[Tab.]{fs:tab:output:getSimulationMultiArmMeans}
    # @refFS[Formula]{fs:simulationMultiArmDoseResponse}
    # @refFS[Formula]{fs:simulationMultiArmMeansGenerate}
    # @refFS[Formula]{fs:simulationMultiArmMeansTestStatistics}
    # @refFS[Formula]{fs:simulationMultiArmSelections}
    # @refFS[Formula]{fs:multiarmRejectionRule}
    design <- getDesignInverseNormal(typeOfDesign = "WT", deltaWT = 0.15, futilityBounds = c(-0.5, 0), informationRates = c(0.4, 0.8, 1))
    x <- getSimulationMultiArmMeans(
        design = design, activeArms = 1,
        plannedSubjects = c(20, 40, 60), stDev = 1.5, muMaxVector = seq(0, 1, 0.2),
        conditionalPower = 0.80, minNumberOfSubjectsPerStage = c(NA, 20, 20),
        maxNumberOfSubjectsPerStage = c(NA, 80, 80), # thetaH1 = 0.5,
        maxNumberOfIterations = 100, allocationRatioPlanned = 2, seed = 1234
    )

    ## Comparison of the results of SimulationResultsMultiArmMeans object 'x' with expected results
    expect_equal(x$iterations[1, ], c(100, 100, 100, 100, 100, 100), label = paste0(x$iterations[1, ]))
    expect_equal(x$iterations[2, ], c(81, 88, 89, 88, 93, 79), label = paste0(x$iterations[2, ]))
    expect_equal(x$iterations[3, ], c(53, 70, 64, 51, 37, 12), label = paste0(x$iterations[3, ]))
    expect_equal(x$rejectAtLeastOne, c(0.01, 0.11, 0.39, 0.73, 0.93, 0.98), tolerance = 1e-07, label = paste0(x$rejectAtLeastOne))
    expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0.01, 0, 0.05, 0.06, 0.01, 0.22, 0.16, 0.02, 0.37, 0.34, 0.06, 0.56, 0.31, 0.2, 0.67, 0.11), tolerance = 1e-07, label = paste0(unlist(as.list(x$rejectedArmsPerStage))))
    expect_equal(x$futilityStop, c(0.47, 0.25, 0.13, 0.1, 0.01, 0.01), tolerance = 1e-07, label = paste0(x$futilityStop))
    expect_equal(x$futilityPerStage[1, ], c(0.19, 0.12, 0.1, 0.1, 0.01, 0.01), tolerance = 1e-07, label = paste0(x$futilityPerStage[1, ]))
    expect_equal(x$futilityPerStage[2, ], c(0.28, 0.13, 0.03, 0, 0, 0), tolerance = 1e-07, label = paste0(x$futilityPerStage[2, ]))
    expect_equal(x$earlyStop[1, ], c(0.19, 0.12, 0.11, 0.12, 0.07, 0.21), tolerance = 1e-07, label = paste0(x$earlyStop[1, ]))
    expect_equal(x$earlyStop[2, ], c(0.28, 0.18, 0.25, 0.37, 0.56, 0.67), tolerance = 1e-07, label = paste0(x$earlyStop[2, ]))
    expect_equal(x$successPerStage[1, ], c(0, 0, 0.01, 0.02, 0.06, 0.2), tolerance = 1e-07, label = paste0(x$successPerStage[1, ]))
    expect_equal(x$successPerStage[2, ], c(0, 0.05, 0.22, 0.37, 0.56, 0.67), tolerance = 1e-07, label = paste0(x$successPerStage[2, ]))
    expect_equal(x$successPerStage[3, ], c(0.01, 0.06, 0.16, 0.34, 0.31, 0.11), tolerance = 1e-07, label = paste0(x$successPerStage[3, ]))
    expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.81, 0.53, 1, 0.88, 0.7, 1, 0.89, 0.64, 1, 0.88, 0.51, 1, 0.93, 0.37, 1, 0.79, 0.12, 1, 0.81, 0.53, 1, 0.88, 0.7, 1, 0.89, 0.64, 1, 0.88, 0.51, 1, 0.93, 0.37, 1, 0.79, 0.12), tolerance = 1e-07, label = paste0(unlist(as.list(x$selectedArms))))
    expect_equal(x$numberOfActiveArms[1, ], c(1, 1, 1, 1, 1, 1), label = paste0(x$numberOfActiveArms[1, ]))
    expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1, 1, 1), label = paste0(x$numberOfActiveArms[2, ]))
    expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1, 1, 1), label = paste0(x$numberOfActiveArms[3, ]))
    expect_equal(x$expectedNumberOfSubjects, c(182.97526, 204.64426, 195.25807, 156.41809, 139.22312, 94.296637), tolerance = 1e-07, label = paste0(x$expectedNumberOfSubjects))
    expect_equal(unlist(as.list(x$sampleSizes)), c(20, 74.777896, 78.138507, 20, 71.766138, 76.107578, 20, 69.720212, 75.189157, 20, 60.637889, 60.622327, 20, 55.732819, 56.713222, 20, 47.895918, 41.888746, 10, 37.388948, 39.069254, 10, 35.883069, 38.053789, 10, 34.860106, 37.594578, 10, 30.318944, 30.311164, 10, 27.86641, 28.356611, 10, 23.947959, 20.944373), tolerance = 1e-07, label = paste0(unlist(as.list(x$sampleSizes))))
    expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(x$conditionalPowerAchieved[1, ]))
    expect_equal(x$conditionalPowerAchieved[2, ], c(0.22017652, 0.27054625, 0.3536952, 0.48224278, 0.56831776, 0.65933958), tolerance = 1e-07, label = paste0(x$conditionalPowerAchieved[2, ]))
    expect_equal(x$conditionalPowerAchieved[3, ], c(0.12006552, 0.18276066, 0.26908136, 0.50518351, 0.66786884, 0.67359844), tolerance = 1e-07, label = paste0(x$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x), NA)))
        expect_output(print(x)$show())
        invisible(capture.output(expect_error(summary(x), NA)))
        expect_output(summary(x)$show())
        xCodeBased <- eval(parse(text = getObjectRCode(x, stringWrapParagraphWidth = NULL)))
        expect_equal(xCodeBased$iterations, x$iterations, tolerance = 1e-07)
        expect_equal(xCodeBased$rejectAtLeastOne, x$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(xCodeBased$rejectedArmsPerStage, x$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(xCodeBased$futilityStop, x$futilityStop, tolerance = 1e-07)
        expect_equal(xCodeBased$futilityPerStage, x$futilityPerStage, tolerance = 1e-07)
        expect_equal(xCodeBased$earlyStop, x$earlyStop, tolerance = 1e-07)
        expect_equal(xCodeBased$successPerStage, x$successPerStage, tolerance = 1e-07)
        expect_equal(xCodeBased$selectedArms, x$selectedArms, tolerance = 1e-07)
        expect_equal(xCodeBased$numberOfActiveArms, x$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(xCodeBased$expectedNumberOfSubjects, x$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(xCodeBased$sampleSizes, x$sampleSizes, tolerance = 1e-07)
        expect_equal(xCodeBased$conditionalPowerAchieved, x$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x), "character")
        df <- as.data.frame(x)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    allocationRatioPlanned <- 2
    factor <- 1 + 1 / allocationRatioPlanned
    y <- getSimulationMeans(design,
        plannedSubjects = round(factor * c(20, 40, 60)), normalApproximation = TRUE, stDev = 1.5,
        conditionalPower = 0.80, minNumberOfSubjectsPerStage = round(factor * c(NA, 20, 20)),
        maxNumberOfSubjectsPerStage = round(factor * c(NA, 80, 80)), alternative = seq(0, 1, 0.2), # thetaH1 = 0.5,
        maxNumberOfIterations = 100, allocationRatioPlanned = allocationRatioPlanned, seed = 5678
    )

    comp1 <- y$overallReject - x$rejectAtLeastOne

    ## Comparison of the results of numeric object 'comp1' with expected results
    expect_equal(comp1, c(0.03, 0.07, -0.04, 0.01, 0.02, -0.02), tolerance = 1e-07, label = paste0(comp1))

    comp2 <- y$rejectPerStage - x$rejectedArmsPerStage[, , 1]

    ## Comparison of the results of matrixarray object 'comp2' with expected results
    expect_equal(comp2[1, ], c(0, 0.02, 0.01, -0.01, 0.04, -0.09), tolerance = 1e-07, label = paste0(comp2[1, ]))
    expect_equal(comp2[2, ], c(0.03, 0, -0.07, 0.06, 0.02, 0.03), tolerance = 1e-07, label = paste0(comp2[2, ]))
    expect_equal(comp2[3, ], c(0, 0.05, 0.02, -0.04, -0.04, 0.04), tolerance = 1e-07, label = paste0(comp2[3, ]))

    comp3 <- y$futilityPerStage - x$futilityPerStage

    ## Comparison of the results of matrixarray object 'comp3' with expected results
    expect_equal(comp3[1, ], c(0.17, 0, 0.04, -0.04, 0, 0.02), tolerance = 1e-07, label = paste0(comp3[1, ]))
    expect_equal(comp3[2, ], c(-0.05, 0.01, 0, 0, 0, 0), tolerance = 1e-07, label = paste0(comp3[2, ]))

    comp4 <- round(y$sampleSizes - (x$sampleSizes[, , 1] + x$sampleSizes[, , 2]), 1)

    ## Comparison of the results of matrixarray object 'comp4' with expected results
    expect_equal(comp4[1, ], c(0, 0, 0, 0, 0, 0), label = paste0(comp4[1, ]))
    expect_equal(comp4[2, ], c(-2.8, -1.3, -0.3, -0.1, -1.4, 10.8), tolerance = 1e-07, label = paste0(comp4[2, ]))
    expect_equal(comp4[3, ], c(1.7, -3.3, -3.4, 13.2, -9.7, -6.7), tolerance = 1e-07, label = paste0(comp4[3, ]))

    comp5 <- round(y$expectedNumberOfSubjects - x$expectedNumberOfSubjects, 1)

    ## Comparison of the results of numeric object 'comp5' with expected results
    expect_equal(comp5, c(-37.8, -8.9, -5.5, 10.1, -12.7, 15.8), tolerance = 1e-07, label = paste0(comp5))

    comp6 <- x$earlyStop - y$earlyStop

    ## Comparison of the results of matrixarray object 'comp6' with expected results
    expect_equal(comp6[1, ], c(-0.43, -0.22, -0.58, -0.5, -0.27, -0.48), tolerance = 1e-07, label = paste0(comp6[1, ]))
    expect_equal(comp6[2, ], c(-0.05, -0.32, -0.59, 0.04, 0.06, -0.17), tolerance = 1e-07, label = paste0(comp6[2, ]))
})

test_that("'getSimulationMultiArmMeans': comparison of base and multi-arm, Fisher design", {
    .skipTestIfDisabled()

    # @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
    # @refFS[Sec.]{fs:sec:simulatingMultiArmDesigns}
    # @refFS[Sec.]{fs:sec:simulatingMultiArmDoseResponseRelationShips}
    # @refFS[Sec.]{fs:sec:simulatingMultiArmSelections}
    # @refFS[Tab.]{fs:tab:output:getSimulationMultiArmMeans}
    # @refFS[Formula]{fs:simulationMultiArmDoseResponse}
    # @refFS[Formula]{fs:simulationMultiArmMeansGenerate}
    # @refFS[Formula]{fs:simulationMultiArmMeansTestStatistics}
    # @refFS[Formula]{fs:simulationMultiArmSelections}
    # @refFS[Formula]{fs:multiarmRejectionRule}
    design <- getDesignFisher(alpha0Vec = c(0.3, 0.4), informationRates = c(0.3, 0.6, 1))
    x <- getSimulationMultiArmMeans(
        design = design, activeArms = 1,
        plannedSubjects = c(20, 40, 60), stDev = 1.5, muMaxVector = seq(0, 1, 0.2),
        conditionalPower = 0.80, minNumberOfSubjectsPerStage = c(NA, 20, 20),
        maxNumberOfSubjectsPerStage = c(NA, 80, 80), # thetaH1 = 0.5,
        maxNumberOfIterations = 100, allocationRatioPlanned = 2, seed = 1234
    )

    ## Comparison of the results of SimulationResultsMultiArmMeans object 'x' with expected results
    expect_equal(x$iterations[1, ], c(100, 100, 100, 100, 100, 100), label = paste0(x$iterations[1, ]))
    expect_equal(x$iterations[2, ], c(28, 41, 50, 54, 56, 51), label = paste0(x$iterations[2, ]))
    expect_equal(x$iterations[3, ], c(7, 24, 27, 21, 24, 7), label = paste0(x$iterations[3, ]))
    expect_equal(x$rejectAtLeastOne, c(0.03, 0.08, 0.28, 0.61, 0.75, 0.89), tolerance = 1e-07, label = paste0(x$rejectAtLeastOne))
    expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0.02, 0, 0.01, 0.05, 0.01, 0.02, 0.05, 0.16, 0.07, 0.17, 0.3, 0.14, 0.24, 0.31, 0.2, 0.39, 0.44, 0.06), tolerance = 1e-07, label = paste0(unlist(as.list(x$rejectedArmsPerStage))))
    expect_equal(x$futilityStop, c(0.91, 0.7, 0.52, 0.32, 0.21, 0.1), tolerance = 1e-07, label = paste0(x$futilityStop))
    expect_equal(x$futilityPerStage[1, ], c(0.7, 0.54, 0.45, 0.29, 0.2, 0.1), tolerance = 1e-07, label = paste0(x$futilityPerStage[1, ]))
    expect_equal(x$futilityPerStage[2, ], c(0.21, 0.16, 0.07, 0.03, 0.01, 0), tolerance = 1e-07, label = paste0(x$futilityPerStage[2, ]))
    expect_equal(x$earlyStop[1, ], c(0.72, 0.59, 0.5, 0.46, 0.44, 0.49), tolerance = 1e-07, label = paste0(x$earlyStop[1, ]))
    expect_equal(x$earlyStop[2, ], c(0.21, 0.17, 0.23, 0.33, 0.32, 0.44), tolerance = 1e-07, label = paste0(x$earlyStop[2, ]))
    expect_equal(x$successPerStage[1, ], c(0.02, 0.05, 0.05, 0.17, 0.24, 0.39), tolerance = 1e-07, label = paste0(x$successPerStage[1, ]))
    expect_equal(x$successPerStage[2, ], c(0, 0.01, 0.16, 0.3, 0.31, 0.44), tolerance = 1e-07, label = paste0(x$successPerStage[2, ]))
    expect_equal(x$successPerStage[3, ], c(0.01, 0.02, 0.07, 0.14, 0.2, 0.06), tolerance = 1e-07, label = paste0(x$successPerStage[3, ]))
    expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.28, 0.07, 1, 0.41, 0.24, 1, 0.5, 0.27, 1, 0.54, 0.21, 1, 0.56, 0.24, 1, 0.51, 0.07, 1, 0.28, 0.07, 1, 0.41, 0.24, 1, 0.5, 0.27, 1, 0.54, 0.21, 1, 0.56, 0.24, 1, 0.51, 0.07), tolerance = 1e-07, label = paste0(unlist(as.list(x$selectedArms))))
    expect_equal(x$numberOfActiveArms[1, ], c(1, 1, 1, 1, 1, 1), label = paste0(x$numberOfActiveArms[1, ]))
    expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1, 1, 1), label = paste0(x$numberOfActiveArms[2, ]))
    expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1, 1, 1), label = paste0(x$numberOfActiveArms[3, ]))
    expect_equal(x$expectedNumberOfSubjects, c(68.211396, 101.92536, 114.30453, 107.14861, 109.24288, 79.622055), tolerance = 1e-07, label = paste0(x$expectedNumberOfSubjects))
    expect_equal(unlist(as.list(x$sampleSizes)), c(20, 70.979514, 80, 20, 71.410143, 77.800325, 20, 69.572428, 79.321509, 20, 66.884783, 72.926791, 20, 62.423876, 74.4634, 20, 55.785406, 66.154471, 10, 35.489757, 40, 10, 35.705072, 38.900163, 10, 34.786214, 39.660755, 10, 33.442392, 36.463396, 10, 31.211938, 37.2317, 10, 27.892703, 33.077236), tolerance = 1e-07, label = paste0(unlist(as.list(x$sampleSizes))))
    expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(x$conditionalPowerAchieved[1, ]))
    expect_equal(x$conditionalPowerAchieved[2, ], c(0.53965216, 0.44870166, 0.54176291, 0.51257459, 0.62161545, 0.65580386), tolerance = 1e-07, label = paste0(x$conditionalPowerAchieved[2, ]))
    expect_equal(x$conditionalPowerAchieved[3, ], c(0.33271205, 0.28302479, 0.35942136, 0.59988705, 0.63386368, 0.5469144), tolerance = 1e-07, label = paste0(x$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x), NA)))
        expect_output(print(x)$show())
        invisible(capture.output(expect_error(summary(x), NA)))
        expect_output(summary(x)$show())
        xCodeBased <- eval(parse(text = getObjectRCode(x, stringWrapParagraphWidth = NULL)))
        expect_equal(xCodeBased$iterations, x$iterations, tolerance = 1e-07)
        expect_equal(xCodeBased$rejectAtLeastOne, x$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(xCodeBased$rejectedArmsPerStage, x$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(xCodeBased$futilityStop, x$futilityStop, tolerance = 1e-07)
        expect_equal(xCodeBased$futilityPerStage, x$futilityPerStage, tolerance = 1e-07)
        expect_equal(xCodeBased$earlyStop, x$earlyStop, tolerance = 1e-07)
        expect_equal(xCodeBased$successPerStage, x$successPerStage, tolerance = 1e-07)
        expect_equal(xCodeBased$selectedArms, x$selectedArms, tolerance = 1e-07)
        expect_equal(xCodeBased$numberOfActiveArms, x$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(xCodeBased$expectedNumberOfSubjects, x$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(xCodeBased$sampleSizes, x$sampleSizes, tolerance = 1e-07)
        expect_equal(xCodeBased$conditionalPowerAchieved, x$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x), "character")
        df <- as.data.frame(x)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    allocationRatioPlanned <- 2
    factor <- 1 + 1 / allocationRatioPlanned
    y <- getSimulationMeans(design,
        plannedSubjects = round(factor * c(20, 40, 60)), normalApproximation = TRUE, stDev = 1.5,
        conditionalPower = 0.80, minNumberOfSubjectsPerStage = round(factor * c(NA, 20, 20)),
        maxNumberOfSubjectsPerStage = round(factor * c(NA, 80, 80)), alternative = seq(0, 1, 0.2), # thetaH1 = 0.5,
        maxNumberOfIterations = 100, allocationRatioPlanned = allocationRatioPlanned, seed = 5678
    )

    comp1 <- y$overallReject - x$rejectAtLeastOne

    ## Comparison of the results of numeric object 'comp1' with expected results
    expect_equal(comp1, c(-0.01, 0.02, 0.05, -0.03, -0.04, -0.04), tolerance = 1e-07, label = paste0(comp1))

    comp2 <- y$rejectPerStage - x$rejectedArmsPerStage[, , 1]

    ## Comparison of the results of matrixarray object 'comp2' with expected results
    expect_equal(comp2[1, ], c(-0.01, -0.02, 0.05, -0.01, -0.05, -0.06), tolerance = 1e-07, label = paste0(comp2[1, ]))
    expect_equal(comp2[2, ], c(0.01, 0.03, -0.07, -0.08, 0.04, 0.05), tolerance = 1e-07, label = paste0(comp2[2, ]))
    expect_equal(comp2[3, ], c(-0.01, 0.01, 0.07, 0.06, -0.03, -0.03), tolerance = 1e-07, label = paste0(comp2[3, ]))

    comp3 <- y$futilityPerStage - x$futilityPerStage

    ## Comparison of the results of matrixarray object 'comp3' with expected results
    expect_equal(comp3[1, ], c(0.08, 0.03, 0.01, 0.04, 0.02, 0.04), tolerance = 1e-07, label = paste0(comp3[1, ]))
    expect_equal(comp3[2, ], c(-0.1, 0.03, 0, 0, 0.03, 0.01), tolerance = 1e-07, label = paste0(comp3[2, ]))

    comp4 <- round(y$sampleSizes - (x$sampleSizes[, , 1] + x$sampleSizes[, , 2]), 1)

    ## Comparison of the results of matrixarray object 'comp4' with expected results
    expect_equal(comp4[1, ], c(0, 0, 0, 0, 0, 0), label = paste0(comp4[1, ]))
    expect_equal(comp4[2, ], c(-3.6, -5.8, 8.4, 5.5, -3.5, 4.7), tolerance = 1e-07, label = paste0(comp4[2, ]))
    expect_equal(comp4[3, ], c(0, -1.8, -3.2, 7.1, -0.8, -19), tolerance = 1e-07, label = paste0(comp4[3, ]))

    comp5 <- round(y$expectedNumberOfSubjects - x$expectedNumberOfSubjects, 1)

    ## Comparison of the results of numeric object 'comp5' with expected results
    expect_equal(comp5, c(-5.8, -11.9, -2.3, 7.1, -3.9, -0.3), tolerance = 1e-07, label = paste0(comp5))

    comp6 <- x$earlyStop - y$earlyStop

    ## Comparison of the results of matrixarray object 'comp6' with expected results
    expect_equal(comp6[1, ], c(-0.19, -0.13, -0.3, -0.45, -0.28, -0.31), tolerance = 1e-07, label = paste0(comp6[1, ]))
    expect_equal(comp6[2, ], c(-0.62, -0.57, -0.74, -0.5, -0.42, -0.53), tolerance = 1e-07, label = paste0(comp6[2, ]))
})
