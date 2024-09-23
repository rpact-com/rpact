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
## |  File name: test-f_simulation_multiarm_survival.R
## |  Creation date: 12 September 2024, 12:52:12
## |  File version: $Revision: 8200 $
## |  Last changed: $Date: 2024-09-12 15:05:38 +0200 (Do, 12 Sep 2024) $
## |  Last changed by: $Author: pahlke $
## |

test_plan_section("Testing Simulation Multi-Arm Survival Function")


test_that("'getSimulationMultiArmSurvival': several configurations", {
    # @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
    # @refFS[Sec.]{fs:sec:simulatingMultiArmDesigns}
    # @refFS[Sec.]{fs:sec:simulatingMultiArmDoseResponseRelationShips}
    # @refFS[Sec.]{fs:sec:simulatingMultiArmSelections}
    # @refFS[Tab.]{fs:tab:output:getSimulationMultiArmSurvival}
    # @refFS[Formula]{fs:simulationMultiArmDoseResponse}
    # @refFS[Formula]{fs:simulationMultiArmSurvivalCholeskyTransformation}
    # @refFS[Formula]{fs:simulationMultiArmSurvivalCorrMatrix}
    # @refFS[Formula]{fs:simulationMultiArmSurvivalEvents}
    # @refFS[Formula]{fs:simulationMultiArmSurvivalLogRanks}
    # @refFS[Formula]{fs:simulationMultiArmSelections}
    # @refFS[Formula]{fs:multiarmRejectionRule}
    # @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
    # @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
    # @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
    # @refFS[Formula]{fs:adjustedPValueSubsetSidak}
    # @refFS[Formula]{fs:adjustedPValueSubsetSimes}
    x1 <- getSimulationMultiArmSurvival(
        seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
        typeOfShape = "linear", activeArms = 4,
        plannedEvents = c(10, 30, 50), omegaMaxVector = seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2),
        conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmSurvival object 'x1' with expected results
    expect_equal(unlist(as.list(x1$cumulativeEventsPerStage)), c(4, 56.544006, 118.06218, 3.7272727, 45.869846, 102.75827, 3.5, 30.37664, 63.385959, 3.3076923, 32.448585, 72.264513, 4, 49.635155, 106.15332, 3.8181818, 36.042521, 83.474993, 3.6666667, 30.759757, 64.009577, 3.5384615, 32.768737, 72.921706, 4, 65.124183, 133.16052, 3.9090909, 38.113637, 101.03155, 3.8333333, 29.450577, 70.659781, 3.7692308, 37.063433, 93.1602, 4, 43.825836, 90.344006, 4, 31.654176, 76.670094, 4, 38.617451, 74.294998, 4, 39.885794, 87.433784), tolerance = 1e-07, label = paste0(unlist(as.list(x1$cumulativeEventsPerStage))))
    expect_equal(x1$iterations[1, ], c(10, 10, 10, 10), label = paste0(x1$iterations[1, ]))
    expect_equal(x1$iterations[2, ], c(10, 10, 10, 10), label = paste0(x1$iterations[2, ]))
    expect_equal(x1$iterations[3, ], c(10, 10, 9, 9), label = paste0(x1$iterations[3, ]))
    expect_equal(x1$rejectAtLeastOne, c(0, 0.1, 0.4, 0.5), tolerance = 1e-07, label = paste0(x1$rejectAtLeastOne))
    expect_equal(unlist(as.list(x1$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0.2, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0.2), tolerance = 1e-07, label = paste0(unlist(as.list(x1$rejectedArmsPerStage))))
    expect_equal(x1$futilityStop, c(0, 0, 0, 0), label = paste0(x1$futilityStop))
    expect_equal(x1$futilityPerStage[1, ], c(0, 0, 0, 0), label = paste0(x1$futilityPerStage[1, ]))
    expect_equal(x1$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(x1$futilityPerStage[2, ]))
    expect_equal(x1$earlyStop[1, ], c(0, 0, 0, 0), label = paste0(x1$earlyStop[1, ]))
    expect_equal(x1$earlyStop[2, ], c(0, 0, 0.1, 0.1), tolerance = 1e-07, label = paste0(x1$earlyStop[2, ]))
    expect_equal(x1$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x1$successPerStage[1, ]))
    expect_equal(x1$successPerStage[2, ], c(0, 0, 0.1, 0.1), tolerance = 1e-07, label = paste0(x1$successPerStage[2, ]))
    expect_equal(x1$successPerStage[3, ], c(0, 0.1, 0.3, 0.4), tolerance = 1e-07, label = paste0(x1$successPerStage[3, ]))
    expect_equal(unlist(as.list(x1$selectedArms)), c(1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0.2, 0.1, 1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.5, 0.5, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0.5, 0.5, 1, 0, 0, 1, 0.1, 0.1, 1, 0.4, 0.3, 1, 0.2, 0.2), tolerance = 1e-07, label = paste0(unlist(as.list(x1$selectedArms))))
    expect_equal(x1$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x1$numberOfActiveArms[1, ]))
    expect_equal(x1$numberOfActiveArms[2, ], c(1, 1, 1, 1), label = paste0(x1$numberOfActiveArms[2, ]))
    expect_equal(x1$numberOfActiveArms[3, ], c(1, 1, 1, 1), label = paste0(x1$numberOfActiveArms[3, ]))
    expect_equal(x1$expectedNumberOfEvents, c(182.68801, 153.5825, 114.70922, 140.61265), tolerance = 1e-07, label = paste0(x1$expectedNumberOfEvents))
    expect_equal(unlist(as.list(x1$singleEventsPerArmAndStage)), c(2, 12.71817, 15, 1.9090909, 15.365854, 15.365854, 1.8333333, 5.2380952, 5.8201058, 1.7692308, 5.5627907, 5.9431525, 2, 5.8093191, 10, 2, 5.447619, 5.9099062, 2, 5.4545455, 6.0606061, 2, 5.6521739, 6.2801932, 2, 21.298347, 21.51817, 2.0909091, 7.4278263, 21.395349, 2.1666667, 3.9786992, 14.01999, 2.2307692, 9.7161004, 22.223992, 2, 0, 0, 2.1818182, 0.87745601, 3.4933517, 2.3333333, 12.978906, 8.4883336, 2.4615385, 12.307692, 13.675214, 2, 39.825836, 46.51817, 1.8181818, 26.77672, 41.522566, 1.6666667, 21.638545, 27.189214, 1.5384615, 23.578102, 33.872776), tolerance = 1e-07, label = paste0(unlist(as.list(x1$singleEventsPerArmAndStage))))
    expect_equal(x1$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(x1$conditionalPowerAchieved[1, ]))
    expect_equal(x1$conditionalPowerAchieved[2, ], c(5.8245202e-05, 0.033918251, 0.017570415, 0.062651459), tolerance = 1e-07, label = paste0(x1$conditionalPowerAchieved[2, ]))
    expect_equal(x1$conditionalPowerAchieved[3, ], c(0.081443645, 0.17714318, 0.49831, 0.30622362), tolerance = 1e-07, label = paste0(x1$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x1), NA)))
        expect_output(print(x1)$show())
        invisible(capture.output(expect_error(summary(x1), NA)))
        expect_output(summary(x1)$show())
        x1CodeBased <- eval(parse(text = getObjectRCode(x1, stringWrapParagraphWidth = NULL)))
        expect_equal(x1CodeBased$cumulativeEventsPerStage, x1$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(x1CodeBased$iterations, x1$iterations, tolerance = 1e-07)
        expect_equal(x1CodeBased$rejectAtLeastOne, x1$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x1CodeBased$rejectedArmsPerStage, x1$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x1CodeBased$futilityStop, x1$futilityStop, tolerance = 1e-07)
        expect_equal(x1CodeBased$futilityPerStage, x1$futilityPerStage, tolerance = 1e-07)
        expect_equal(x1CodeBased$earlyStop, x1$earlyStop, tolerance = 1e-07)
        expect_equal(x1CodeBased$successPerStage, x1$successPerStage, tolerance = 1e-07)
        expect_equal(x1CodeBased$selectedArms, x1$selectedArms, tolerance = 1e-07)
        expect_equal(x1CodeBased$numberOfActiveArms, x1$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x1CodeBased$expectedNumberOfEvents, x1$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(x1CodeBased$singleEventsPerArmAndStage, x1$singleEventsPerArmAndStage, tolerance = 1e-07)
        expect_equal(x1CodeBased$conditionalPowerAchieved, x1$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x1), "character")
        df <- as.data.frame(x1)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x1)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    .skipTestIfDisabled()

    x2 <- getSimulationMultiArmSurvival(
        seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
        typeOfShape = "userDefined", activeArms = 4,
        plannedEvents = c(10, 30, 50), adaptations = rep(TRUE, 2),
        effectMatrix = matrix(c(0.1, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.5), ncol = 4),
        conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmSurvival object 'x2' with expected results
    expect_equal(unlist(as.list(x2$cumulativeEventsPerStage)), c(5.5, 83.888278, 161.11661, 5, 67.731433, 137.0436, 6.5, 94.119048, 179.03968, 5.8333333, 80.884792, 166.06998, 6, 91.054945, 173.83883, 5.4166667, 70.455792, 139.76796, 7, 91.102564, 177.8547, 6.25, 81.100963, 165.22795), tolerance = 1e-07, label = paste0(unlist(as.list(x2$cumulativeEventsPerStage))))
    expect_equal(x2$iterations[1, ], c(10, 10), label = paste0(x2$iterations[1, ]))
    expect_equal(x2$iterations[2, ], c(10, 10), label = paste0(x2$iterations[2, ]))
    expect_equal(x2$iterations[3, ], c(3, 9), label = paste0(x2$iterations[3, ]))
    expect_equal(x2$rejectAtLeastOne, c(0, 0), label = paste0(x2$rejectAtLeastOne))
    expect_equal(unlist(as.list(x2$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), label = paste0(unlist(as.list(x2$rejectedArmsPerStage))))
    expect_equal(x2$futilityStop, c(0.7, 0.1), tolerance = 1e-07, label = paste0(x2$futilityStop))
    expect_equal(x2$futilityPerStage[1, ], c(0, 0), label = paste0(x2$futilityPerStage[1, ]))
    expect_equal(x2$futilityPerStage[2, ], c(0.7, 0.1), tolerance = 1e-07, label = paste0(x2$futilityPerStage[2, ]))
    expect_equal(x2$earlyStop[1, ], c(0, 0), label = paste0(x2$earlyStop[1, ]))
    expect_equal(x2$earlyStop[2, ], c(0.7, 0.1), tolerance = 1e-07, label = paste0(x2$earlyStop[2, ]))
    expect_equal(x2$successPerStage[1, ], c(0, 0), label = paste0(x2$successPerStage[1, ]))
    expect_equal(x2$successPerStage[2, ], c(0, 0), label = paste0(x2$successPerStage[2, ]))
    expect_equal(x2$successPerStage[3, ], c(0, 0), label = paste0(x2$successPerStage[3, ]))
    expect_equal(unlist(as.list(x2$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0.4, 0.1, 1, 0.5, 0.5, 1, 0.4, 0.1, 1, 0.1, 0, 1, 0.2, 0.1, 1, 0.4, 0.4), tolerance = 1e-07, label = paste0(unlist(as.list(x2$selectedArms))))
    expect_equal(x2$numberOfActiveArms[1, ], c(4, 4), label = paste0(x2$numberOfActiveArms[1, ]))
    expect_equal(x2$numberOfActiveArms[2, ], c(1, 1), label = paste0(x2$numberOfActiveArms[2, ]))
    expect_equal(x2$numberOfActiveArms[3, ], c(1, 1), label = paste0(x2$numberOfActiveArms[3, ]))
    expect_equal(x2$expectedNumberOfEvents, c(140, 189.47868), tolerance = 1e-07, label = paste0(x2$expectedNumberOfEvents))
    expect_equal(unlist(as.list(x2$singleEventsPerArmAndStage)), c(0.5, 0, 0, 0.83333333, 0, 0, 1.5, 9.2307692, 7.6923077, 1.6666667, 12.320026, 15.873016, 1, 6.6666667, 5.5555556, 1.25, 2.3076923, 0, 2, 5.7142857, 9.5238095, 2.0833333, 12.119531, 14.814815, 5, 78.388278, 77.228327, 4.1666667, 62.731433, 69.312169), tolerance = 1e-07, label = paste0(unlist(as.list(x2$singleEventsPerArmAndStage))))
    expect_equal(x2$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_), label = paste0(x2$conditionalPowerAchieved[1, ]))
    expect_equal(x2$conditionalPowerAchieved[2, ], c(0, 1.5253195e-09), tolerance = 1e-07, label = paste0(x2$conditionalPowerAchieved[2, ]))
    expect_equal(x2$conditionalPowerAchieved[3, ], c(0, 1.1842379e-15), tolerance = 1e-07, label = paste0(x2$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x2), NA)))
        expect_output(print(x2)$show())
        invisible(capture.output(expect_error(summary(x2), NA)))
        expect_output(summary(x2)$show())
        x2CodeBased <- eval(parse(text = getObjectRCode(x2, stringWrapParagraphWidth = NULL)))
        expect_equal(x2CodeBased$cumulativeEventsPerStage, x2$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(x2CodeBased$iterations, x2$iterations, tolerance = 1e-07)
        expect_equal(x2CodeBased$rejectAtLeastOne, x2$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x2CodeBased$rejectedArmsPerStage, x2$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x2CodeBased$futilityStop, x2$futilityStop, tolerance = 1e-07)
        expect_equal(x2CodeBased$futilityPerStage, x2$futilityPerStage, tolerance = 1e-07)
        expect_equal(x2CodeBased$earlyStop, x2$earlyStop, tolerance = 1e-07)
        expect_equal(x2CodeBased$successPerStage, x2$successPerStage, tolerance = 1e-07)
        expect_equal(x2CodeBased$selectedArms, x2$selectedArms, tolerance = 1e-07)
        expect_equal(x2CodeBased$numberOfActiveArms, x2$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x2CodeBased$expectedNumberOfEvents, x2$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(x2CodeBased$singleEventsPerArmAndStage, x2$singleEventsPerArmAndStage, tolerance = 1e-07)
        expect_equal(x2CodeBased$conditionalPowerAchieved, x2$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x2), "character")
        df <- as.data.frame(x2)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x2)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x3 <- getSimulationMultiArmSurvival(
        seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
        typeOfShape = "sigmoidEmax", gED50 = 2, slope = 0.5, activeArms = 4,
        plannedEvents = c(10, 30, 50), omegaMaxVector = seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2),
        conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmSurvival object 'x3' with expected results
    expect_equal(unlist(as.list(x3$cumulativeEventsPerStage)), c(4, 56.544006, 118.06218, 3.8499139, 46.858849, 105.91374, 3.7209785, 37.392533, 79.701207, 3.6090171, 41.322916, 96.584767, 4, 49.635155, 106.15332, 3.8816273, 36.741574, 86.112527, 3.7799362, 32.140988, 69.139159, 3.6916324, 34.296961, 77.330183, 4, 65.124183, 133.16052, 3.9002999, 39.328667, 103.83053, 3.8146499, 31.005549, 75.358715, 3.7402755, 36.850923, 90.063067, 4, 43.825836, 90.344006, 3.9133408, 32.25912, 80.994092, 3.8388939, 33.67594, 74.187296, 3.7742477, 34.474746, 77.613714), tolerance = 1e-07, label = paste0(unlist(as.list(x3$cumulativeEventsPerStage))))
    expect_equal(x3$iterations[1, ], c(10, 10, 10, 10), label = paste0(x3$iterations[1, ]))
    expect_equal(x3$iterations[2, ], c(10, 10, 10, 10), label = paste0(x3$iterations[2, ]))
    expect_equal(x3$iterations[3, ], c(10, 10, 10, 9), label = paste0(x3$iterations[3, ]))
    expect_equal(x3$rejectAtLeastOne, c(0, 0.1, 0.3, 0.3), tolerance = 1e-07, label = paste0(x3$rejectAtLeastOne))
    expect_equal(unlist(as.list(x3$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1), tolerance = 1e-07, label = paste0(unlist(as.list(x3$rejectedArmsPerStage))))
    expect_equal(x3$futilityStop, c(0, 0, 0, 0), label = paste0(x3$futilityStop))
    expect_equal(x3$futilityPerStage[1, ], c(0, 0, 0, 0), label = paste0(x3$futilityPerStage[1, ]))
    expect_equal(x3$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(x3$futilityPerStage[2, ]))
    expect_equal(x3$earlyStop[1, ], c(0, 0, 0, 0), label = paste0(x3$earlyStop[1, ]))
    expect_equal(x3$earlyStop[2, ], c(0, 0, 0, 0.1), tolerance = 1e-07, label = paste0(x3$earlyStop[2, ]))
    expect_equal(x3$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x3$successPerStage[1, ]))
    expect_equal(x3$successPerStage[2, ], c(0, 0, 0, 0.1), tolerance = 1e-07, label = paste0(x3$successPerStage[2, ]))
    expect_equal(x3$successPerStage[3, ], c(0, 0.1, 0.3, 0.2), tolerance = 1e-07, label = paste0(x3$successPerStage[3, ]))
    expect_equal(unlist(as.list(x3$selectedArms)), c(1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.2, 0.2, 1, 0.4, 0.3, 1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.5, 0.5, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0, 0, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.1, 0.1), tolerance = 1e-07, label = paste0(unlist(as.list(x3$selectedArms))))
    expect_equal(x3$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x3$numberOfActiveArms[1, ]))
    expect_equal(x3$numberOfActiveArms[2, ], c(1, 1, 1, 1), label = paste0(x3$numberOfActiveArms[2, ]))
    expect_equal(x3$numberOfActiveArms[3, ], c(1, 1, 1, 1), label = paste0(x3$numberOfActiveArms[3, ]))
    expect_equal(x3$expectedNumberOfEvents, c(182.68801, 158.69386, 129.88152, 143.2193), tolerance = 1e-07, label = paste0(x3$expectedNumberOfEvents))
    expect_equal(unlist(as.list(x3$singleEventsPerArmAndStage)), c(2, 12.71817, 15, 2.0015199, 15.596608, 15.596608, 2.0028257, 10.765048, 10.765048, 2.0039595, 12.760745, 18.508821, 2, 5.8093191, 10, 2.0332334, 5.447619, 5.9126663, 2.0617834, 5.4545455, 5.4545455, 2.0865748, 5.6521739, 6.2801932, 2, 21.298347, 21.51817, 2.0519059, 8.0160405, 21.043571, 2.0964971, 4.2843932, 12.80954, 2.135218, 8.1574931, 16.459114, 2, 0, 0, 2.0649468, 0.93345197, 5.2766854, 2.120741, 6.9305404, 8.9677303, 2.1691901, 5.7473444, 6.3859382, 2, 39.825836, 46.51817, 1.848394, 27.412327, 43.458287, 1.7181528, 22.906506, 31.543625, 1.6050576, 24.953154, 36.753029), tolerance = 1e-07, label = paste0(unlist(as.list(x3$singleEventsPerArmAndStage))))
    expect_equal(x3$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(x3$conditionalPowerAchieved[1, ]))
    expect_equal(x3$conditionalPowerAchieved[2, ], c(5.8245202e-05, 0.027881828, 0.017394693, 0.05621525), tolerance = 1e-07, label = paste0(x3$conditionalPowerAchieved[2, ]))
    expect_equal(x3$conditionalPowerAchieved[3, ], c(0.081443645, 0.17047212, 0.40326875, 0.20898924), tolerance = 1e-07, label = paste0(x3$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x3), NA)))
        expect_output(print(x3)$show())
        invisible(capture.output(expect_error(summary(x3), NA)))
        expect_output(summary(x3)$show())
        x3CodeBased <- eval(parse(text = getObjectRCode(x3, stringWrapParagraphWidth = NULL)))
        expect_equal(x3CodeBased$cumulativeEventsPerStage, x3$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(x3CodeBased$iterations, x3$iterations, tolerance = 1e-07)
        expect_equal(x3CodeBased$rejectAtLeastOne, x3$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x3CodeBased$rejectedArmsPerStage, x3$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x3CodeBased$futilityStop, x3$futilityStop, tolerance = 1e-07)
        expect_equal(x3CodeBased$futilityPerStage, x3$futilityPerStage, tolerance = 1e-07)
        expect_equal(x3CodeBased$earlyStop, x3$earlyStop, tolerance = 1e-07)
        expect_equal(x3CodeBased$successPerStage, x3$successPerStage, tolerance = 1e-07)
        expect_equal(x3CodeBased$selectedArms, x3$selectedArms, tolerance = 1e-07)
        expect_equal(x3CodeBased$numberOfActiveArms, x3$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x3CodeBased$expectedNumberOfEvents, x3$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(x3CodeBased$singleEventsPerArmAndStage, x3$singleEventsPerArmAndStage, tolerance = 1e-07)
        expect_equal(x3CodeBased$conditionalPowerAchieved, x3$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x3), "character")
        df <- as.data.frame(x3)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x3)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x4 <- getSimulationMultiArmSurvival(
        seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
        activeArms = 4, typeOfSelection = "all",
        plannedEvents = c(10, 30, 50), omegaMaxVector = seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2),
        conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmSurvival object 'x4' with expected results
    expect_equal(unlist(as.list(x4$cumulativeEventsPerStage)), c(4, 43.80534, 83.80534, 3.7272727, 41, 78.272727, 3.5, 36.991095, 71.991095, 3.3076923, 31.601422, 64.678345, 4, 43.80534, 83.80534, 3.8181818, 42, 80.181818, 3.6666667, 38.752575, 75.419242, 3.5384615, 33.806172, 69.190787, 4, 43.80534, 83.80534, 3.9090909, 43, 82.090909, 3.8333333, 40.514056, 78.847389, 3.7692308, 36.010922, 73.70323, 4, 43.80534, 83.80534, 4, 44, 84, 4, 42.275537, 82.275537, 4, 38.215673, 78.215673), tolerance = 1e-07, label = paste0(unlist(as.list(x4$cumulativeEventsPerStage))))
    expect_equal(x4$iterations[1, ], c(10, 10, 10, 10), label = paste0(x4$iterations[1, ]))
    expect_equal(x4$iterations[2, ], c(10, 10, 10, 10), label = paste0(x4$iterations[2, ]))
    expect_equal(x4$iterations[3, ], c(10, 10, 10, 10), label = paste0(x4$iterations[3, ]))
    expect_equal(x4$rejectAtLeastOne, c(0, 0.1, 0.2, 0.2), tolerance = 1e-07, label = paste0(x4$rejectAtLeastOne))
    expect_equal(unlist(as.list(x4$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.2, 0, 0, 0.1), tolerance = 1e-07, label = paste0(unlist(as.list(x4$rejectedArmsPerStage))))
    expect_equal(x4$futilityStop, c(0, 0, 0, 0), label = paste0(x4$futilityStop))
    expect_equal(x4$futilityPerStage[1, ], c(0, 0, 0, 0), label = paste0(x4$futilityPerStage[1, ]))
    expect_equal(x4$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(x4$futilityPerStage[2, ]))
    expect_equal(x4$earlyStop[1, ], c(0, 0, 0, 0), label = paste0(x4$earlyStop[1, ]))
    expect_equal(x4$earlyStop[2, ], c(0, 0, 0, 0), label = paste0(x4$earlyStop[2, ]))
    expect_equal(x4$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x4$successPerStage[1, ]))
    expect_equal(x4$successPerStage[2, ], c(0, 0, 0, 0), label = paste0(x4$successPerStage[2, ]))
    expect_equal(x4$successPerStage[3, ], c(0, 0, 0, 0), label = paste0(x4$successPerStage[3, ]))
    expect_equal(unlist(as.list(x4$selectedArms)), c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), label = paste0(unlist(as.list(x4$selectedArms))))
    expect_equal(x4$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x4$numberOfActiveArms[1, ]))
    expect_equal(x4$numberOfActiveArms[2, ], c(4, 4, 4, 4), label = paste0(x4$numberOfActiveArms[2, ]))
    expect_equal(x4$numberOfActiveArms[3, ], c(4, 4, 4, 4), label = paste0(x4$numberOfActiveArms[3, ]))
    expect_equal(x4$expectedNumberOfEvents, c(209.51335, 210, 205.68884, 195.53918), tolerance = 1e-07, label = paste0(x4$expectedNumberOfEvents))
    expect_equal(unlist(as.list(x4$singleEventsPerArmAndStage)), c(2, 19.90267, 20, 1.9090909, 19.090909, 19.090909, 1.8333333, 17.542954, 18.333333, 1.7692308, 15.133855, 17.692308, 2, 19.90267, 20, 2, 20, 20, 2, 19.137768, 20, 2, 17.107836, 20, 2, 19.90267, 20, 2.0909091, 20.909091, 20.909091, 2.1666667, 20.732582, 21.666667, 2.2307692, 19.081818, 22.307692, 2, 19.90267, 20, 2.1818182, 21.818182, 21.818182, 2.3333333, 22.327396, 23.333333, 2.4615385, 21.055799, 24.615385, 2, 19.90267, 20, 1.8181818, 18.181818, 18.181818, 1.6666667, 15.94814, 16.666667, 1.5384615, 13.159874, 15.384615), tolerance = 1e-07, label = paste0(unlist(as.list(x4$singleEventsPerArmAndStage))))
    expect_equal(x4$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(x4$conditionalPowerAchieved[1, ]))
    expect_equal(x4$conditionalPowerAchieved[2, ], c(0.09225544, 0.10755451, 0.080008195, 0.16137979), tolerance = 1e-07, label = paste0(x4$conditionalPowerAchieved[2, ]))
    expect_equal(x4$conditionalPowerAchieved[3, ], c(0.011907723, 0.030096405, 0.063317228, 0.080810126), tolerance = 1e-07, label = paste0(x4$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x4), NA)))
        expect_output(print(x4)$show())
        invisible(capture.output(expect_error(summary(x4), NA)))
        expect_output(summary(x4)$show())
        x4CodeBased <- eval(parse(text = getObjectRCode(x4, stringWrapParagraphWidth = NULL)))
        expect_equal(x4CodeBased$cumulativeEventsPerStage, x4$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(x4CodeBased$iterations, x4$iterations, tolerance = 1e-07)
        expect_equal(x4CodeBased$rejectAtLeastOne, x4$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x4CodeBased$rejectedArmsPerStage, x4$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x4CodeBased$futilityStop, x4$futilityStop, tolerance = 1e-07)
        expect_equal(x4CodeBased$futilityPerStage, x4$futilityPerStage, tolerance = 1e-07)
        expect_equal(x4CodeBased$earlyStop, x4$earlyStop, tolerance = 1e-07)
        expect_equal(x4CodeBased$successPerStage, x4$successPerStage, tolerance = 1e-07)
        expect_equal(x4CodeBased$selectedArms, x4$selectedArms, tolerance = 1e-07)
        expect_equal(x4CodeBased$numberOfActiveArms, x4$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x4CodeBased$expectedNumberOfEvents, x4$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(x4CodeBased$singleEventsPerArmAndStage, x4$singleEventsPerArmAndStage, tolerance = 1e-07)
        expect_equal(x4CodeBased$conditionalPowerAchieved, x4$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x4), "character")
        df <- as.data.frame(x4)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x4)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x5 <- getSimulationMultiArmSurvival(
        seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
        activeArms = 4, typeOfSelection = "rBest", rValue = 2,
        plannedEvents = c(10, 30, 50), omegaMaxVector = seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2),
        conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmSurvival object 'x5' with expected results
    expect_equal(unlist(as.list(x5$cumulativeEventsPerStage)), c(4, 52.52163, 101.87923, 3.7272727, 38.874416, 80.596598, 3.5, 26.84484, 58.886153, 3.3076923, 30.949369, 59.030095, 4, 46.265898, 91.178205, 3.8181818, 38.846483, 77.651274, 3.6666667, 31.816991, 74.831476, 3.5384615, 34.40256, 64.30431, 4, 45.854963, 88.5459, 3.9090909, 42.746334, 86.637949, 3.8333333, 33.812131, 81.900895, 3.7692308, 37.761125, 70.330365, 4, 39.599231, 77.844872, 4, 51.153533, 106.61534, 4, 33.295158, 78.303665, 4, 52.301815, 100.0206), tolerance = 1e-07, label = paste0(unlist(as.list(x5$cumulativeEventsPerStage))))
    expect_equal(x5$iterations[1, ], c(10, 10, 10, 10), label = paste0(x5$iterations[1, ]))
    expect_equal(x5$iterations[2, ], c(10, 10, 10, 10), label = paste0(x5$iterations[2, ]))
    expect_equal(x5$iterations[3, ], c(10, 10, 10, 10), label = paste0(x5$iterations[3, ]))
    expect_equal(x5$rejectAtLeastOne, c(0.1, 0, 0.2, 0.6), tolerance = 1e-07, label = paste0(x5$rejectAtLeastOne))
    expect_equal(unlist(as.list(x5$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0.4), tolerance = 1e-07, label = paste0(unlist(as.list(x5$rejectedArmsPerStage))))
    expect_equal(x5$futilityStop, c(0, 0, 0, 0), label = paste0(x5$futilityStop))
    expect_equal(x5$futilityPerStage[1, ], c(0, 0, 0, 0), label = paste0(x5$futilityPerStage[1, ]))
    expect_equal(x5$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(x5$futilityPerStage[2, ]))
    expect_equal(x5$earlyStop[1, ], c(0, 0, 0, 0), label = paste0(x5$earlyStop[1, ]))
    expect_equal(x5$earlyStop[2, ], c(0, 0, 0, 0), label = paste0(x5$earlyStop[2, ]))
    expect_equal(x5$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x5$successPerStage[1, ]))
    expect_equal(x5$successPerStage[2, ], c(0, 0, 0, 0), label = paste0(x5$successPerStage[2, ]))
    expect_equal(x5$successPerStage[3, ], c(0, 0, 0.2, 0.1), tolerance = 1e-07, label = paste0(x5$successPerStage[3, ]))
    expect_equal(unlist(as.list(x5$selectedArms)), c(1, 0.7, 0.7, 1, 0.4, 0.4, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.6, 0.6, 1, 0.4, 0.4, 1, 0.5, 0.5, 1, 0.5, 0.5, 1, 0.7, 0.7, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.8, 0.8, 1, 0.5, 0.5, 1, 0.8, 0.8), tolerance = 1e-07, label = paste0(unlist(as.list(x5$selectedArms))))
    expect_equal(x5$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x5$numberOfActiveArms[1, ]))
    expect_equal(x5$numberOfActiveArms[2, ], c(2, 2, 2, 2), label = paste0(x5$numberOfActiveArms[2, ]))
    expect_equal(x5$numberOfActiveArms[3, ], c(2, 2, 2, 2), label = paste0(x5$numberOfActiveArms[3, ]))
    expect_equal(x5$expectedNumberOfEvents, c(181.7241, 185.49972, 161.03264, 167.26743), tolerance = 1e-07, label = paste0(x5$expectedNumberOfEvents))
    expect_equal(unlist(as.list(x5$singleEventsPerArmAndStage)), c(2, 20.481343, 20.156522, 1.9090909, 10.277572, 13.076122, 1.8333333, 6.3781513, 6.3781513, 1.7692308, 7.1692484, 7.9523038, 2, 14.225611, 15.711226, 2, 10.15873, 10.15873, 2, 11.183635, 17.351324, 2, 10.39167, 9.7733283, 2, 13.814676, 13.489856, 2.0909091, 13.967672, 15.245555, 2.1666667, 13.012108, 22.425602, 2.2307692, 13.519466, 12.440818, 2, 7.5589445, 9.044559, 2.1818182, 22.283962, 26.815748, 2.3333333, 12.328469, 19.345345, 2.4615385, 27.829386, 27.590364, 2, 28.040287, 29.201081, 1.8181818, 24.869571, 28.64606, 1.6666667, 16.966689, 25.663162, 1.5384615, 20.472429, 20.128422), tolerance = 1e-07, label = paste0(unlist(as.list(x5$singleEventsPerArmAndStage))))
    expect_equal(x5$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(x5$conditionalPowerAchieved[1, ]))
    expect_equal(x5$conditionalPowerAchieved[2, ], c(0.0011884888, 0.025687618, 0.050936222, 0.056920177), tolerance = 1e-07, label = paste0(x5$conditionalPowerAchieved[2, ]))
    expect_equal(x5$conditionalPowerAchieved[3, ], c(0.16000064, 0.17717891, 0.25226702, 0.41435883), tolerance = 1e-07, label = paste0(x5$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x5), NA)))
        expect_output(print(x5)$show())
        invisible(capture.output(expect_error(summary(x5), NA)))
        expect_output(summary(x5)$show())
        x5CodeBased <- eval(parse(text = getObjectRCode(x5, stringWrapParagraphWidth = NULL)))
        expect_equal(x5CodeBased$cumulativeEventsPerStage, x5$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(x5CodeBased$iterations, x5$iterations, tolerance = 1e-07)
        expect_equal(x5CodeBased$rejectAtLeastOne, x5$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x5CodeBased$rejectedArmsPerStage, x5$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x5CodeBased$futilityStop, x5$futilityStop, tolerance = 1e-07)
        expect_equal(x5CodeBased$futilityPerStage, x5$futilityPerStage, tolerance = 1e-07)
        expect_equal(x5CodeBased$earlyStop, x5$earlyStop, tolerance = 1e-07)
        expect_equal(x5CodeBased$successPerStage, x5$successPerStage, tolerance = 1e-07)
        expect_equal(x5CodeBased$selectedArms, x5$selectedArms, tolerance = 1e-07)
        expect_equal(x5CodeBased$numberOfActiveArms, x5$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x5CodeBased$expectedNumberOfEvents, x5$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(x5CodeBased$singleEventsPerArmAndStage, x5$singleEventsPerArmAndStage, tolerance = 1e-07)
        expect_equal(x5CodeBased$conditionalPowerAchieved, x5$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x5), "character")
        df <- as.data.frame(x5)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x5)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x6 <- getSimulationMultiArmSurvival(
        seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
        activeArms = 4, typeOfSelection = "epsilon", epsilonValue = 0.1,
        plannedEvents = c(10, 30, 50), omegaMaxVector = seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2),
        conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmSurvival object 'x6' with expected results
    expect_equal(unlist(as.list(x6$cumulativeEventsPerStage)), c(4, 61.733546, 127.87237, 3.7272727, 31.938683, 70.204069, 3.5, 41.1271, 91.550286, 3.3076923, 34.649784, 81.031044, 4, 56.48818, 112.05759, 3.8181818, 40.038722, 105.34522, 3.6666667, 42.689301, 87.532881, 3.5384615, 28.026291, 80.666398, 4, 48.154846, 98.724256, 3.9090909, 30.896746, 79.036909, 3.8333333, 37.59905, 82.662218, 3.7692308, 38.754446, 86.274767, 4, 48.730993, 104.3004, 4, 35.685987, 84.168898, 4, 44.550112, 108.98044, 4, 36.742663, 80.705475), tolerance = 1e-07, label = paste0(unlist(as.list(x6$cumulativeEventsPerStage))))
    expect_equal(x6$iterations[1, ], c(10, 10, 10, 10), label = paste0(x6$iterations[1, ]))
    expect_equal(x6$iterations[2, ], c(10, 10, 10, 10), label = paste0(x6$iterations[2, ]))
    expect_equal(x6$iterations[3, ], c(10, 9, 9, 10), label = paste0(x6$iterations[3, ]))
    expect_equal(x6$rejectAtLeastOne, c(0, 0.3, 0.5, 0.4), tolerance = 1e-07, label = paste0(x6$rejectAtLeastOne))
    expect_equal(unlist(as.list(x6$rejectedArmsPerStage)), c(0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0, 0, 0, 0.2, 0, 0, 0.3, 0, 0, 0.2), tolerance = 1e-07, label = paste0(unlist(as.list(x6$rejectedArmsPerStage))))
    expect_equal(x6$futilityStop, c(0, 0, 0, 0), label = paste0(x6$futilityStop))
    expect_equal(x6$futilityPerStage[1, ], c(0, 0, 0, 0), label = paste0(x6$futilityPerStage[1, ]))
    expect_equal(x6$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(x6$futilityPerStage[2, ]))
    expect_equal(x6$earlyStop[1, ], c(0, 0, 0, 0), label = paste0(x6$earlyStop[1, ]))
    expect_equal(x6$earlyStop[2, ], c(0, 0.1, 0.1, 0), tolerance = 1e-07, label = paste0(x6$earlyStop[2, ]))
    expect_equal(x6$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x6$successPerStage[1, ]))
    expect_equal(x6$successPerStage[2, ], c(0, 0.1, 0.1, 0), tolerance = 1e-07, label = paste0(x6$successPerStage[2, ]))
    expect_equal(x6$successPerStage[3, ], c(0, 0.2, 0.4, 0.4), tolerance = 1e-07, label = paste0(x6$successPerStage[3, ]))
    expect_equal(unlist(as.list(x6$selectedArms)), c(1, 0.5, 0.5, 1, 0.3, 0, 1, 0.3, 0.2, 1, 0.3, 0.2, 1, 0.3, 0.2, 1, 0.5, 0.5, 1, 0.2, 0.1, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.3, 0.2, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.5, 0.5, 1, 0.3, 0.3), tolerance = 1e-07, label = paste0(unlist(as.list(x6$selectedArms))))
    expect_equal(x6$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x6$numberOfActiveArms[1, ]))
    expect_equal(x6$numberOfActiveArms[2, ], c(1.1, 1.3, 1.1, 1.2), tolerance = 1e-07, label = paste0(x6$numberOfActiveArms[2, ]))
    expect_equal(x6$numberOfActiveArms[3, ], c(1, 1.1111111, 1, 1), tolerance = 1e-07, label = paste0(x6$numberOfActiveArms[3, ]))
    expect_equal(x6$expectedNumberOfEvents, c(182.78185, 142.9628, 156.19514, 150.78355), tolerance = 1e-07, label = paste0(x6$expectedNumberOfEvents))
    expect_equal(unlist(as.list(x6$singleEventsPerArmAndStage)), c(2, 18.5787, 20.56941, 1.9090909, 5.8775077, 0, 1.8333333, 9.5135564, 11.640212, 1.7692308, 9.2660953, 10.697674, 2, 13.333333, 10, 2, 13.886638, 27.041107, 2, 10.909091, 6.0606061, 2, 2.4118335, 16.956522, 2, 5, 5, 2.0909091, 4.6537525, 9.8747764, 2.1666667, 5.6521739, 6.2801932, 2.2307692, 12.909219, 11.836735, 2, 5.5761462, 10, 2.1818182, 9.3520845, 10.217524, 2.3333333, 12.436568, 25.647358, 2.4615385, 10.666667, 8.2792264, 2, 39.154846, 45.56941, 1.8181818, 22.333902, 38.265387, 1.6666667, 28.113543, 38.782975, 1.5384615, 22.075996, 35.683586), tolerance = 1e-07, label = paste0(unlist(as.list(x6$singleEventsPerArmAndStage))))
    expect_equal(x6$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(x6$conditionalPowerAchieved[1, ]))
    expect_equal(x6$conditionalPowerAchieved[2, ], c(0.018816179, 0.071905821, 0.002298516, 0.067085771), tolerance = 1e-07, label = paste0(x6$conditionalPowerAchieved[2, ]))
    expect_equal(x6$conditionalPowerAchieved[3, ], c(0.080015186, 0.29125387, 0.18887123, 0.4033636), tolerance = 1e-07, label = paste0(x6$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x6), NA)))
        expect_output(print(x6)$show())
        invisible(capture.output(expect_error(summary(x6), NA)))
        expect_output(summary(x6)$show())
        x6CodeBased <- eval(parse(text = getObjectRCode(x6, stringWrapParagraphWidth = NULL)))
        expect_equal(x6CodeBased$cumulativeEventsPerStage, x6$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(x6CodeBased$iterations, x6$iterations, tolerance = 1e-07)
        expect_equal(x6CodeBased$rejectAtLeastOne, x6$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x6CodeBased$rejectedArmsPerStage, x6$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x6CodeBased$futilityStop, x6$futilityStop, tolerance = 1e-07)
        expect_equal(x6CodeBased$futilityPerStage, x6$futilityPerStage, tolerance = 1e-07)
        expect_equal(x6CodeBased$earlyStop, x6$earlyStop, tolerance = 1e-07)
        expect_equal(x6CodeBased$successPerStage, x6$successPerStage, tolerance = 1e-07)
        expect_equal(x6CodeBased$selectedArms, x6$selectedArms, tolerance = 1e-07)
        expect_equal(x6CodeBased$numberOfActiveArms, x6$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x6CodeBased$expectedNumberOfEvents, x6$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(x6CodeBased$singleEventsPerArmAndStage, x6$singleEventsPerArmAndStage, tolerance = 1e-07)
        expect_equal(x6CodeBased$conditionalPowerAchieved, x6$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x6), "character")
        df <- as.data.frame(x6)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x6)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x7 <- getSimulationMultiArmSurvival(
        seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
        activeArms = 4,
        plannedEvents = c(10, 30, 50), omegaMaxVector = seq(1, 1.6, 0.2), adaptations = c(TRUE, FALSE),
        conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmSurvival object 'x7' with expected results
    expect_equal(unlist(as.list(x7$cumulativeEventsPerStage)), c(4, 56.544006, 109.08801, 3.7272727, 45.869846, 88.01242, 3.5, 30.37664, 55.609943, 3.3076923, 32.448585, 64.38291, 4, 49.635155, 95.27031, 3.8181818, 36.042521, 68.26686, 3.6666667, 30.759757, 56.23356, 3.5384615, 32.768737, 65.040103, 4, 65.124183, 126.24837, 3.9090909, 38.113637, 72.318183, 3.8333333, 29.450577, 53.284552, 3.7692308, 37.063433, 73.850273, 4, 43.825836, 83.651672, 4, 31.654176, 59.308352, 4, 38.617451, 65.970174, 4, 39.885794, 79.552181), tolerance = 1e-07, label = paste0(unlist(as.list(x7$cumulativeEventsPerStage))))
    expect_equal(x7$iterations[1, ], c(10, 10, 10, 10), label = paste0(x7$iterations[1, ]))
    expect_equal(x7$iterations[2, ], c(10, 10, 10, 10), label = paste0(x7$iterations[2, ]))
    expect_equal(x7$iterations[3, ], c(10, 10, 9, 9), label = paste0(x7$iterations[3, ]))
    expect_equal(x7$rejectAtLeastOne, c(0, 0.1, 0.4, 0.4), tolerance = 1e-07, label = paste0(x7$rejectAtLeastOne))
    expect_equal(unlist(as.list(x7$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0.2), tolerance = 1e-07, label = paste0(unlist(as.list(x7$rejectedArmsPerStage))))
    expect_equal(x7$futilityStop, c(0, 0, 0, 0), label = paste0(x7$futilityStop))
    expect_equal(x7$futilityPerStage[1, ], c(0, 0, 0, 0), label = paste0(x7$futilityPerStage[1, ]))
    expect_equal(x7$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(x7$futilityPerStage[2, ]))
    expect_equal(x7$earlyStop[1, ], c(0, 0, 0, 0), label = paste0(x7$earlyStop[1, ]))
    expect_equal(x7$earlyStop[2, ], c(0, 0, 0.1, 0.1), tolerance = 1e-07, label = paste0(x7$earlyStop[2, ]))
    expect_equal(x7$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x7$successPerStage[1, ]))
    expect_equal(x7$successPerStage[2, ], c(0, 0, 0.1, 0.1), tolerance = 1e-07, label = paste0(x7$successPerStage[2, ]))
    expect_equal(x7$successPerStage[3, ], c(0, 0.1, 0.3, 0.3), tolerance = 1e-07, label = paste0(x7$successPerStage[3, ]))
    expect_equal(unlist(as.list(x7$selectedArms)), c(1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0.2, 0.1, 1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.5, 0.5, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0.5, 0.5, 1, 0, 0, 1, 0.1, 0.1, 1, 0.4, 0.3, 1, 0.2, 0.2), tolerance = 1e-07, label = paste0(unlist(as.list(x7$selectedArms))))
    expect_equal(x7$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x7$numberOfActiveArms[1, ]))
    expect_equal(x7$numberOfActiveArms[2, ], c(1, 1, 1, 1), label = paste0(x7$numberOfActiveArms[2, ]))
    expect_equal(x7$numberOfActiveArms[3, ], c(1, 1, 1, 1), label = paste0(x7$numberOfActiveArms[3, ]))
    expect_equal(x7$expectedNumberOfEvents, c(169.30334, 121.79095, 98.577582, 123.23372), tolerance = 1e-07, label = paste0(x7$expectedNumberOfEvents))
    expect_equal(unlist(as.list(x7$singleEventsPerArmAndStage)), c(2, 12.71817, 12.71817, 1.9090909, 15.365854, 15.365854, 1.8333333, 5.2380952, 5.8201058, 1.7692308, 5.5627907, 5.9431525, 2, 5.8093191, 5.8093191, 2, 5.447619, 5.447619, 2, 5.4545455, 6.0606061, 2, 5.6521739, 6.2801932, 2, 21.298347, 21.298347, 2.0909091, 7.4278263, 7.4278263, 2.1666667, 3.9786992, 4.4207768, 2.2307692, 9.7161004, 10.795667, 2, 0, 0, 2.1818182, 0.87745601, 0.87745601, 2.3333333, 12.978906, 7.9395257, 2.4615385, 12.307692, 13.675214, 2, 39.825836, 39.825836, 1.8181818, 26.77672, 26.77672, 1.6666667, 21.638545, 19.413198, 1.5384615, 23.578102, 25.991173), tolerance = 1e-07, label = paste0(unlist(as.list(x7$singleEventsPerArmAndStage))))
    expect_equal(x7$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(x7$conditionalPowerAchieved[1, ]))
    expect_equal(x7$conditionalPowerAchieved[2, ], c(5.8245202e-05, 0.033918251, 0.017570415, 0.062651459), tolerance = 1e-07, label = paste0(x7$conditionalPowerAchieved[2, ]))
    expect_equal(x7$conditionalPowerAchieved[3, ], c(0.075858531, 0.086024261, 0.37522404, 0.19729909), tolerance = 1e-07, label = paste0(x7$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x7), NA)))
        expect_output(print(x7)$show())
        invisible(capture.output(expect_error(summary(x7), NA)))
        expect_output(summary(x7)$show())
        x7CodeBased <- eval(parse(text = getObjectRCode(x7, stringWrapParagraphWidth = NULL)))
        expect_equal(x7CodeBased$cumulativeEventsPerStage, x7$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(x7CodeBased$iterations, x7$iterations, tolerance = 1e-07)
        expect_equal(x7CodeBased$rejectAtLeastOne, x7$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x7CodeBased$rejectedArmsPerStage, x7$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x7CodeBased$futilityStop, x7$futilityStop, tolerance = 1e-07)
        expect_equal(x7CodeBased$futilityPerStage, x7$futilityPerStage, tolerance = 1e-07)
        expect_equal(x7CodeBased$earlyStop, x7$earlyStop, tolerance = 1e-07)
        expect_equal(x7CodeBased$successPerStage, x7$successPerStage, tolerance = 1e-07)
        expect_equal(x7CodeBased$selectedArms, x7$selectedArms, tolerance = 1e-07)
        expect_equal(x7CodeBased$numberOfActiveArms, x7$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x7CodeBased$expectedNumberOfEvents, x7$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(x7CodeBased$singleEventsPerArmAndStage, x7$singleEventsPerArmAndStage, tolerance = 1e-07)
        expect_equal(x7CodeBased$conditionalPowerAchieved, x7$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x7), "character")
        df <- as.data.frame(x7)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x7)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x8 <- getSimulationMultiArmSurvival(
        seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
        activeArms = 4, typeOfSelection = "all",
        plannedEvents = c(10, 30, 50), omegaMaxVector = seq(1, 1.6, 0.2), adaptations = c(TRUE, FALSE),
        conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmSurvival object 'x8' with expected results
    expect_equal(unlist(as.list(x8$cumulativeEventsPerStage)), c(4, 43.80534, 83.61068, 3.7272727, 41, 78.272727, 3.5, 36.991095, 70.482189, 3.3076923, 31.601422, 59.895151, 4, 43.80534, 83.61068, 3.8181818, 42, 80.181818, 3.6666667, 38.752575, 73.838484, 3.5384615, 33.806172, 64.073883, 4, 43.80534, 83.61068, 3.9090909, 43, 82.090909, 3.8333333, 40.514056, 77.194778, 3.7692308, 36.010922, 68.252614, 4, 43.80534, 83.61068, 4, 44, 84, 4, 42.275537, 80.551073, 4, 38.215673, 72.431346), tolerance = 1e-07, label = paste0(unlist(as.list(x8$cumulativeEventsPerStage))))
    expect_equal(x8$iterations[1, ], c(10, 10, 10, 10), label = paste0(x8$iterations[1, ]))
    expect_equal(x8$iterations[2, ], c(10, 10, 10, 10), label = paste0(x8$iterations[2, ]))
    expect_equal(x8$iterations[3, ], c(10, 10, 10, 10), label = paste0(x8$iterations[3, ]))
    expect_equal(x8$rejectAtLeastOne, c(0, 0.1, 0.2, 0.2), tolerance = 1e-07, label = paste0(x8$rejectAtLeastOne))
    expect_equal(unlist(as.list(x8$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.2, 0, 0, 0.1), tolerance = 1e-07, label = paste0(unlist(as.list(x8$rejectedArmsPerStage))))
    expect_equal(x8$futilityStop, c(0, 0, 0, 0), label = paste0(x8$futilityStop))
    expect_equal(x8$futilityPerStage[1, ], c(0, 0, 0, 0), label = paste0(x8$futilityPerStage[1, ]))
    expect_equal(x8$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(x8$futilityPerStage[2, ]))
    expect_equal(x8$earlyStop[1, ], c(0, 0, 0, 0), label = paste0(x8$earlyStop[1, ]))
    expect_equal(x8$earlyStop[2, ], c(0, 0, 0, 0), label = paste0(x8$earlyStop[2, ]))
    expect_equal(x8$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x8$successPerStage[1, ]))
    expect_equal(x8$successPerStage[2, ], c(0, 0, 0, 0), label = paste0(x8$successPerStage[2, ]))
    expect_equal(x8$successPerStage[3, ], c(0, 0, 0, 0), label = paste0(x8$successPerStage[3, ]))
    expect_equal(unlist(as.list(x8$selectedArms)), c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), label = paste0(unlist(as.list(x8$selectedArms))))
    expect_equal(x8$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x8$numberOfActiveArms[1, ]))
    expect_equal(x8$numberOfActiveArms[2, ], c(4, 4, 4, 4), label = paste0(x8$numberOfActiveArms[2, ]))
    expect_equal(x8$numberOfActiveArms[3, ], c(4, 4, 4, 4), label = paste0(x8$numberOfActiveArms[3, ]))
    expect_equal(x8$expectedNumberOfEvents, c(209.0267, 210, 201.37768, 181.07836), tolerance = 1e-07, label = paste0(x8$expectedNumberOfEvents))
    expect_equal(unlist(as.list(x8$singleEventsPerArmAndStage)), c(2, 19.90267, 19.90267, 1.9090909, 19.090909, 19.090909, 1.8333333, 17.542954, 17.542954, 1.7692308, 15.133855, 15.133855, 2, 19.90267, 19.90267, 2, 20, 20, 2, 19.137768, 19.137768, 2, 17.107836, 17.107836, 2, 19.90267, 19.90267, 2.0909091, 20.909091, 20.909091, 2.1666667, 20.732582, 20.732582, 2.2307692, 19.081818, 19.081818, 2, 19.90267, 19.90267, 2.1818182, 21.818182, 21.818182, 2.3333333, 22.327396, 22.327396, 2.4615385, 21.055799, 21.055799, 2, 19.90267, 19.90267, 1.8181818, 18.181818, 18.181818, 1.6666667, 15.94814, 15.94814, 1.5384615, 13.159874, 13.159874), tolerance = 1e-07, label = paste0(unlist(as.list(x8$singleEventsPerArmAndStage))))
    expect_equal(x8$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(x8$conditionalPowerAchieved[1, ]))
    expect_equal(x8$conditionalPowerAchieved[2, ], c(0.09225544, 0.10755451, 0.080008195, 0.16137979), tolerance = 1e-07, label = paste0(x8$conditionalPowerAchieved[2, ]))
    expect_equal(x8$conditionalPowerAchieved[3, ], c(0.011968708, 0.030096405, 0.063317862, 0.066369104), tolerance = 1e-07, label = paste0(x8$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x8), NA)))
        expect_output(print(x8)$show())
        invisible(capture.output(expect_error(summary(x8), NA)))
        expect_output(summary(x8)$show())
        x8CodeBased <- eval(parse(text = getObjectRCode(x8, stringWrapParagraphWidth = NULL)))
        expect_equal(x8CodeBased$cumulativeEventsPerStage, x8$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(x8CodeBased$iterations, x8$iterations, tolerance = 1e-07)
        expect_equal(x8CodeBased$rejectAtLeastOne, x8$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x8CodeBased$rejectedArmsPerStage, x8$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x8CodeBased$futilityStop, x8$futilityStop, tolerance = 1e-07)
        expect_equal(x8CodeBased$futilityPerStage, x8$futilityPerStage, tolerance = 1e-07)
        expect_equal(x8CodeBased$earlyStop, x8$earlyStop, tolerance = 1e-07)
        expect_equal(x8CodeBased$successPerStage, x8$successPerStage, tolerance = 1e-07)
        expect_equal(x8CodeBased$selectedArms, x8$selectedArms, tolerance = 1e-07)
        expect_equal(x8CodeBased$numberOfActiveArms, x8$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x8CodeBased$expectedNumberOfEvents, x8$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(x8CodeBased$singleEventsPerArmAndStage, x8$singleEventsPerArmAndStage, tolerance = 1e-07)
        expect_equal(x8CodeBased$conditionalPowerAchieved, x8$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x8), "character")
        df <- as.data.frame(x8)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x8)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x9 <- getSimulationMultiArmSurvival(
        seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
        activeArms = 4, typeOfSelection = "rBest", rValue = 2,
        plannedEvents = c(10, 30, 50), omegaMaxVector = seq(1, 1.6, 0.2), adaptations = c(TRUE, FALSE),
        conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmSurvival object 'x9' with expected results
    expect_equal(unlist(as.list(x9$cumulativeEventsPerStage)), c(4, 52.52163, 101.04326, 3.7272727, 38.874416, 74.021559, 3.5, 26.84484, 50.189681, 3.3076923, 30.949369, 58.591046, 4, 46.265898, 88.531796, 3.8181818, 38.846483, 73.874785, 3.6666667, 31.816991, 59.967314, 3.5384615, 34.40256, 65.266658, 4, 45.854963, 87.709926, 3.9090909, 42.746334, 81.583577, 3.8333333, 33.812131, 63.790928, 3.7692308, 37.761125, 71.75302, 4, 39.599231, 75.198463, 4, 51.153533, 98.307067, 4, 33.295158, 62.590316, 4, 52.301815, 100.60363), tolerance = 1e-07, label = paste0(unlist(as.list(x9$cumulativeEventsPerStage))))
    expect_equal(x9$iterations[1, ], c(10, 10, 10, 10), label = paste0(x9$iterations[1, ]))
    expect_equal(x9$iterations[2, ], c(10, 10, 10, 10), label = paste0(x9$iterations[2, ]))
    expect_equal(x9$iterations[3, ], c(10, 10, 10, 10), label = paste0(x9$iterations[3, ]))
    expect_equal(x9$rejectAtLeastOne, c(0.1, 0, 0.2, 0.6), tolerance = 1e-07, label = paste0(x9$rejectAtLeastOne))
    expect_equal(unlist(as.list(x9$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0.4), tolerance = 1e-07, label = paste0(unlist(as.list(x9$rejectedArmsPerStage))))
    expect_equal(x9$futilityStop, c(0, 0, 0, 0), label = paste0(x9$futilityStop))
    expect_equal(x9$futilityPerStage[1, ], c(0, 0, 0, 0), label = paste0(x9$futilityPerStage[1, ]))
    expect_equal(x9$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(x9$futilityPerStage[2, ]))
    expect_equal(x9$earlyStop[1, ], c(0, 0, 0, 0), label = paste0(x9$earlyStop[1, ]))
    expect_equal(x9$earlyStop[2, ], c(0, 0, 0, 0), label = paste0(x9$earlyStop[2, ]))
    expect_equal(x9$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x9$successPerStage[1, ]))
    expect_equal(x9$successPerStage[2, ], c(0, 0, 0, 0), label = paste0(x9$successPerStage[2, ]))
    expect_equal(x9$successPerStage[3, ], c(0, 0, 0.2, 0.1), tolerance = 1e-07, label = paste0(x9$successPerStage[3, ]))
    expect_equal(unlist(as.list(x9$selectedArms)), c(1, 0.7, 0.7, 1, 0.4, 0.4, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.6, 0.6, 1, 0.4, 0.4, 1, 0.5, 0.5, 1, 0.5, 0.5, 1, 0.7, 0.7, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.8, 0.8, 1, 0.5, 0.5, 1, 0.8, 0.8), tolerance = 1e-07, label = paste0(unlist(as.list(x9$selectedArms))))
    expect_equal(x9$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x9$numberOfActiveArms[1, ]))
    expect_equal(x9$numberOfActiveArms[2, ], c(2, 2, 2, 2), label = paste0(x9$numberOfActiveArms[2, ]))
    expect_equal(x9$numberOfActiveArms[3, ], c(2, 2, 2, 2), label = paste0(x9$numberOfActiveArms[3, ]))
    expect_equal(x9$expectedNumberOfEvents, c(178.24172, 173.11501, 129.7381, 168.7644), tolerance = 1e-07, label = paste0(x9$expectedNumberOfEvents))
    expect_equal(unlist(as.list(x9$singleEventsPerArmAndStage)), c(2, 20.481343, 20.481343, 1.9090909, 10.277572, 10.277572, 1.8333333, 6.3781513, 6.3781513, 1.7692308, 7.1692484, 7.1692484, 2, 14.225611, 14.225611, 2, 10.15873, 10.15873, 2, 11.183635, 11.183635, 2, 10.39167, 10.39167, 2, 13.814676, 13.814676, 2.0909091, 13.967672, 13.967672, 2.1666667, 13.012108, 13.012108, 2.2307692, 13.519466, 13.519466, 2, 7.5589445, 7.5589445, 2.1818182, 22.283962, 22.283962, 2.3333333, 12.328469, 12.328469, 2.4615385, 27.829386, 27.829386, 2, 28.040287, 28.040287, 1.8181818, 24.869571, 24.869571, 1.6666667, 16.966689, 16.966689, 1.5384615, 20.472429, 20.472429), tolerance = 1e-07, label = paste0(unlist(as.list(x9$singleEventsPerArmAndStage))))
    expect_equal(x9$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(x9$conditionalPowerAchieved[1, ]))
    expect_equal(x9$conditionalPowerAchieved[2, ], c(0.0011884888, 0.025687618, 0.050936222, 0.056920177), tolerance = 1e-07, label = paste0(x9$conditionalPowerAchieved[2, ]))
    expect_equal(x9$conditionalPowerAchieved[3, ], c(0.13630501, 0.14441052, 0.13257023, 0.41932885), tolerance = 1e-07, label = paste0(x9$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x9), NA)))
        expect_output(print(x9)$show())
        invisible(capture.output(expect_error(summary(x9), NA)))
        expect_output(summary(x9)$show())
        x9CodeBased <- eval(parse(text = getObjectRCode(x9, stringWrapParagraphWidth = NULL)))
        expect_equal(x9CodeBased$cumulativeEventsPerStage, x9$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(x9CodeBased$iterations, x9$iterations, tolerance = 1e-07)
        expect_equal(x9CodeBased$rejectAtLeastOne, x9$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x9CodeBased$rejectedArmsPerStage, x9$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x9CodeBased$futilityStop, x9$futilityStop, tolerance = 1e-07)
        expect_equal(x9CodeBased$futilityPerStage, x9$futilityPerStage, tolerance = 1e-07)
        expect_equal(x9CodeBased$earlyStop, x9$earlyStop, tolerance = 1e-07)
        expect_equal(x9CodeBased$successPerStage, x9$successPerStage, tolerance = 1e-07)
        expect_equal(x9CodeBased$selectedArms, x9$selectedArms, tolerance = 1e-07)
        expect_equal(x9CodeBased$numberOfActiveArms, x9$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x9CodeBased$expectedNumberOfEvents, x9$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(x9CodeBased$singleEventsPerArmAndStage, x9$singleEventsPerArmAndStage, tolerance = 1e-07)
        expect_equal(x9CodeBased$conditionalPowerAchieved, x9$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x9), "character")
        df <- as.data.frame(x9)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x9)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x10 <- getSimulationMultiArmSurvival(
        seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
        activeArms = 4, typeOfSelection = "epsilon", epsilonValue = 0.1,
        plannedEvents = c(10, 30, 50), omegaMaxVector = seq(1, 1.6, 0.2), adaptations = c(TRUE, FALSE),
        conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),
        intersectionTest = "Hierarchical",
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmSurvival object 'x10' with expected results
    expect_equal(unlist(as.list(x10$cumulativeEventsPerStage)), c(4, 56.657929, 125.67531, 3.7272727, 49.785816, 137.47812, 3.5, 31.249399, 83.970115, 3.3076923, 24.380086, 39.12662, 4, 50.333545, 107.06446, 3.8181818, 41.640149, 84.416884, 3.6666667, 39.507667, 64.61277, 3.5384615, 24.167144, 31.025997, 4, 45.952714, 80.461405, 3.9090909, 47.662083, 90.438818, 3.8333333, 33.911202, 59.016305, 3.7692308, 36.126326, 42.985179, 4, 39.486047, 79.550294, 4, 50.19027, 105.2747, 4, 37.6469, 62.752003, 4, 41.892876, 48.751729), tolerance = 1e-07, label = paste0(unlist(as.list(x10$cumulativeEventsPerStage))))
    expect_equal(x10$iterations[1, ], c(10, 10, 10, 10), label = paste0(x10$iterations[1, ]))
    expect_equal(x10$iterations[2, ], c(10, 10, 10, 10), label = paste0(x10$iterations[2, ]))
    expect_equal(x10$iterations[3, ], c(6, 3, 2, 1), label = paste0(x10$iterations[3, ]))
    expect_equal(x10$rejectAtLeastOne, c(0, 0, 0, 0), label = paste0(x10$rejectAtLeastOne))
    expect_equal(unlist(as.list(x10$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), label = paste0(unlist(as.list(x10$rejectedArmsPerStage))))
    expect_equal(x10$futilityStop, c(0.4, 0.7, 0.8, 0.9), tolerance = 1e-07, label = paste0(x10$futilityStop))
    expect_equal(x10$futilityPerStage[1, ], c(0, 0, 0, 0), label = paste0(x10$futilityPerStage[1, ]))
    expect_equal(x10$futilityPerStage[2, ], c(0.4, 0.7, 0.8, 0.9), tolerance = 1e-07, label = paste0(x10$futilityPerStage[2, ]))
    expect_equal(x10$earlyStop[1, ], c(0, 0, 0, 0), label = paste0(x10$earlyStop[1, ]))
    expect_equal(x10$earlyStop[2, ], c(0.4, 0.7, 0.8, 0.9), tolerance = 1e-07, label = paste0(x10$earlyStop[2, ]))
    expect_equal(x10$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x10$successPerStage[1, ]))
    expect_equal(x10$successPerStage[2, ], c(0, 0, 0, 0), label = paste0(x10$successPerStage[2, ]))
    expect_equal(x10$successPerStage[3, ], c(0, 0, 0, 0), label = paste0(x10$successPerStage[3, ]))
    expect_equal(unlist(as.list(x10$selectedArms)), c(1, 0.6, 0.6, 1, 0.3, 0.3, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.5, 0.4, 1, 0.1, 0, 1, 0.4, 0, 1, 0.1, 0, 1, 0.2, 0, 1, 0.4, 0, 1, 0.3, 0, 1, 0.3, 0, 1, 0.2, 0.1, 1, 0.4, 0.1, 1, 0.4, 0, 1, 0.5, 0), tolerance = 1e-07, label = paste0(unlist(as.list(x10$selectedArms))))
    expect_equal(x10$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x10$numberOfActiveArms[1, ]))
    expect_equal(x10$numberOfActiveArms[2, ], c(1.5, 1.2, 1.3, 1), tolerance = 1e-07, label = paste0(x10$numberOfActiveArms[2, ]))
    expect_equal(x10$numberOfActiveArms[3, ], c(1.8333333, 1.3333333, 1, 1), tolerance = 1e-07, label = paste0(x10$numberOfActiveArms[3, ]))
    expect_equal(x10$expectedNumberOfEvents, c(148.64919, 116.07216, 81.180483, 62.574824), tolerance = 1e-07, label = paste0(x10$expectedNumberOfEvents))
    expect_equal(unlist(as.list(x10$singleEventsPerArmAndStage)), c(2, 20.705215, 34.508692, 1.9090909, 13.474672, 44.915572, 1.8333333, 5.5231227, 27.615613, 1.7692308, 0.78876811, 7.8876811, 2, 14.380832, 22.222222, 2, 5.2380952, 0, 2, 13.614724, 0, 2, 0.34505655, 0, 2, 10, 0, 2.0909091, 11.16912, 0, 2.1666667, 7.8515929, 0, 2.2307692, 12.073469, 0, 2, 3.5333333, 5.5555556, 2.1818182, 13.606399, 12.307692, 2.3333333, 11.420624, 0, 2.4615385, 17.609251, 0, 2, 31.952714, 34.508692, 1.8181818, 32.583872, 42.776735, 1.6666667, 22.226276, 25.105103, 1.5384615, 20.283626, 6.8588531), tolerance = 1e-07, label = paste0(unlist(as.list(x10$singleEventsPerArmAndStage))))
    expect_equal(x10$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(x10$conditionalPowerAchieved[1, ]))
    expect_equal(x10$conditionalPowerAchieved[2, ], c(0.0031444794, 0.00037604601, 0.038145414, 0.045847923), tolerance = 1e-07, label = paste0(x10$conditionalPowerAchieved[2, ]))
    expect_equal(x10$conditionalPowerAchieved[3, ], c(7.9302274e-08, 1.361166e-06, 0.16667791, 0.040805908), tolerance = 1e-07, label = paste0(x10$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x10), NA)))
        expect_output(print(x10)$show())
        invisible(capture.output(expect_error(summary(x10), NA)))
        expect_output(summary(x10)$show())
        x10CodeBased <- eval(parse(text = getObjectRCode(x10, stringWrapParagraphWidth = NULL)))
        expect_equal(x10CodeBased$cumulativeEventsPerStage, x10$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(x10CodeBased$iterations, x10$iterations, tolerance = 1e-07)
        expect_equal(x10CodeBased$rejectAtLeastOne, x10$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x10CodeBased$rejectedArmsPerStage, x10$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x10CodeBased$futilityStop, x10$futilityStop, tolerance = 1e-07)
        expect_equal(x10CodeBased$futilityPerStage, x10$futilityPerStage, tolerance = 1e-07)
        expect_equal(x10CodeBased$earlyStop, x10$earlyStop, tolerance = 1e-07)
        expect_equal(x10CodeBased$successPerStage, x10$successPerStage, tolerance = 1e-07)
        expect_equal(x10CodeBased$selectedArms, x10$selectedArms, tolerance = 1e-07)
        expect_equal(x10CodeBased$numberOfActiveArms, x10$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x10CodeBased$expectedNumberOfEvents, x10$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(x10CodeBased$singleEventsPerArmAndStage, x10$singleEventsPerArmAndStage, tolerance = 1e-07)
        expect_equal(x10CodeBased$conditionalPowerAchieved, x10$conditionalPowerAchieved, tolerance = 1e-07)
        expect_type(names(x10), "character")
        df <- as.data.frame(x10)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x10)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x11 <- getSimulationMultiArmSurvival(
        seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
        activeArms = 4, directionUpper = FALSE, threshold = 0,
        plannedEvents = c(10, 30, 50), omegaMaxVector = 1 / seq(0.1, 0.3, 0.1),
        conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),
        intersectionTest = "Hierarchical",
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmSurvival object 'x11' with expected results
    expect_equal(unlist(as.list(x11$cumulativeEventsPerStage)), c(1.5454545, 1.5454545, 1.5454545, 2, 2, 2, 2.3846154, 2.3846154, 2.3846154, 2.3636364, 2.3636364, 2.3636364, 2.6666667, 2.6666667, 2.6666667, 2.9230769, 2.9230769, 2.9230769, 3.1818182, 3.1818182, 3.1818182, 3.3333333, 3.3333333, 3.3333333, 3.4615385, 3.4615385, 3.4615385, 4, 4, 4, 4, 4, 4, 4, 4, 4), tolerance = 1e-07, label = paste0(unlist(as.list(x11$cumulativeEventsPerStage))))
    expect_equal(x11$iterations[1, ], c(10, 10, 10), label = paste0(x11$iterations[1, ]))
    expect_equal(x11$iterations[2, ], c(0, 0, 0), label = paste0(x11$iterations[2, ]))
    expect_equal(x11$iterations[3, ], c(0, 0, 0), label = paste0(x11$iterations[3, ]))
    expect_equal(x11$rejectAtLeastOne, c(0, 0, 0), label = paste0(x11$rejectAtLeastOne))
    expect_equal(unlist(as.list(x11$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), label = paste0(unlist(as.list(x11$rejectedArmsPerStage))))
    expect_equal(x11$futilityStop, c(1, 1, 1), label = paste0(x11$futilityStop))
    expect_equal(x11$futilityPerStage[1, ], c(1, 1, 1), label = paste0(x11$futilityPerStage[1, ]))
    expect_equal(x11$futilityPerStage[2, ], c(0, 0, 0), label = paste0(x11$futilityPerStage[2, ]))
    expect_equal(x11$earlyStop[1, ], c(1, 1, 1), label = paste0(x11$earlyStop[1, ]))
    expect_equal(x11$earlyStop[2, ], c(0, 0, 0), label = paste0(x11$earlyStop[2, ]))
    expect_equal(x11$successPerStage[1, ], c(0, 0, 0), label = paste0(x11$successPerStage[1, ]))
    expect_equal(x11$successPerStage[2, ], c(0, 0, 0), label = paste0(x11$successPerStage[2, ]))
    expect_equal(x11$successPerStage[3, ], c(0, 0, 0), label = paste0(x11$successPerStage[3, ]))
    expect_equal(unlist(as.list(x11$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0), label = paste0(unlist(as.list(x11$selectedArms))))
    expect_equal(x11$numberOfActiveArms[1, ], c(4, 4, 4), label = paste0(x11$numberOfActiveArms[1, ]))
    expect_equal(x11$numberOfActiveArms[2, ], c(NaN, NaN, NaN), label = paste0(x11$numberOfActiveArms[2, ]))
    expect_equal(x11$numberOfActiveArms[3, ], c(NaN, NaN, NaN), label = paste0(x11$numberOfActiveArms[3, ]))
    expect_equal(x11$expectedNumberOfEvents, c(NaN, NaN, NaN), label = paste0(x11$expectedNumberOfEvents))
    expect_equal(unlist(as.list(x11$singleEventsPerArmAndStage)), c(1.1818182, NaN, NaN, 1.3333333, NaN, NaN, 1.4615385, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2.8181818, NaN, NaN, 2.6666667, NaN, NaN, 2.5384615, NaN, NaN, 3.6363636, NaN, NaN, 3.3333333, NaN, NaN, 3.0769231, NaN, NaN, 0.36363636, NaN, NaN, 0.66666667, NaN, NaN, 0.92307692, NaN, NaN), tolerance = 1e-07, label = paste0(unlist(as.list(x11$singleEventsPerArmAndStage))))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x11), NA)))
        expect_output(print(x11)$show())
        invisible(capture.output(expect_error(summary(x11), NA)))
        expect_output(summary(x11)$show())
        x11CodeBased <- eval(parse(text = getObjectRCode(x11, stringWrapParagraphWidth = NULL)))
        expect_equal(x11CodeBased$cumulativeEventsPerStage, x11$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(x11CodeBased$iterations, x11$iterations, tolerance = 1e-07)
        expect_equal(x11CodeBased$rejectAtLeastOne, x11$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x11CodeBased$rejectedArmsPerStage, x11$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x11CodeBased$futilityStop, x11$futilityStop, tolerance = 1e-07)
        expect_equal(x11CodeBased$futilityPerStage, x11$futilityPerStage, tolerance = 1e-07)
        expect_equal(x11CodeBased$earlyStop, x11$earlyStop, tolerance = 1e-07)
        expect_equal(x11CodeBased$successPerStage, x11$successPerStage, tolerance = 1e-07)
        expect_equal(x11CodeBased$selectedArms, x11$selectedArms, tolerance = 1e-07)
        expect_equal(x11CodeBased$numberOfActiveArms, x11$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x11CodeBased$expectedNumberOfEvents, x11$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(x11CodeBased$singleEventsPerArmAndStage, x11$singleEventsPerArmAndStage, tolerance = 1e-07)
        expect_type(names(x11), "character")
        df <- as.data.frame(x11)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x11)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x11b <- getSimulationMultiArmSurvival(
        seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1), directionUpper = FALSE),
        activeArms = 4, threshold = 0,
        plannedEvents = c(10, 30, 50), omegaMaxVector = 1 / seq(0.1, 0.3, 0.1),
        conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),
        intersectionTest = "Hierarchical",
        maxNumberOfIterations = 10
    )

    ## Pairwise comparison of the results of x11 with the results of x11b
    expect_equal(unlist(as.list(x11$cumulativeEventsPerStage)), unlist(as.list(x11b$cumulativeEventsPerStage)), tolerance = 1e-07)
    expect_equal(x11b$iterations[1, ], x11$iterations[1, ])
    expect_equal(x11b$iterations[2, ], x11$iterations[2, ])
    expect_equal(x11b$iterations[3, ], x11$iterations[3, ])
    expect_equal(x11b$earlyStop[1, ], x11$earlyStop[1, ])
    expect_equal(x11b$earlyStop[2, ], x11$earlyStop[2, ])
    expect_equal(unlist(as.list(x11$selectedArms)), unlist(as.list(x11b$selectedArms)))
    expect_equal(x11b$numberOfActiveArms[1, ], x11$numberOfActiveArms[1, ])
    expect_equal(x11b$numberOfActiveArms[2, ], x11$numberOfActiveArms[2, ])
    expect_equal(x11b$numberOfActiveArms[3, ], x11$numberOfActiveArms[3, ])
    expect_equal(x11$expectedNumberOfEvents, x11b$expectedNumberOfEvents)
    expect_equal(unlist(as.list(x11$singleEventsPerArmAndStage)), unlist(as.list(x11b$singleEventsPerArmAndStage)), tolerance = 1e-07)

    x12 <- getSimulationMultiArmSurvival(
        seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
        typeOfShape = "linear", activeArms = 4, directionUpper = FALSE, threshold = 0,
        plannedEvents = c(10, 30, 50), omegaMaxVector = 1 / seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2),
        conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),
        intersectionTest = "Hierarchical",
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmSurvival object 'x12' with expected results
    expect_equal(unlist(as.list(x12$cumulativeEventsPerStage)), c(4, 4, 4, 4.2727273, 4.2727273, 4.2727273, 4.5, 4.5, 4.5, 4.6923077, 4.6923077, 4.6923077, 4, 4, 4, 4.1818182, 4.1818182, 4.1818182, 4.3333333, 4.3333333, 4.3333333, 4.4615385, 4.4615385, 4.4615385, 4, 4, 4, 4.0909091, 4.0909091, 4.0909091, 4.1666667, 4.1666667, 4.1666667, 4.2307692, 4.2307692, 4.2307692, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4), tolerance = 1e-07, label = paste0(unlist(as.list(x12$cumulativeEventsPerStage))))
    expect_equal(x12$iterations[1, ], c(10, 10, 10, 10), label = paste0(x12$iterations[1, ]))
    expect_equal(x12$iterations[2, ], c(0, 0, 0, 0), label = paste0(x12$iterations[2, ]))
    expect_equal(x12$iterations[3, ], c(0, 0, 0, 0), label = paste0(x12$iterations[3, ]))
    expect_equal(x12$rejectAtLeastOne, c(0, 0, 0, 0), label = paste0(x12$rejectAtLeastOne))
    expect_equal(unlist(as.list(x12$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), label = paste0(unlist(as.list(x12$rejectedArmsPerStage))))
    expect_equal(x12$futilityStop, c(1, 1, 1, 1), label = paste0(x12$futilityStop))
    expect_equal(x12$futilityPerStage[1, ], c(1, 1, 1, 1), label = paste0(x12$futilityPerStage[1, ]))
    expect_equal(x12$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(x12$futilityPerStage[2, ]))
    expect_equal(x12$earlyStop[1, ], c(1, 1, 1, 1), label = paste0(x12$earlyStop[1, ]))
    expect_equal(x12$earlyStop[2, ], c(0, 0, 0, 0), label = paste0(x12$earlyStop[2, ]))
    expect_equal(x12$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x12$successPerStage[1, ]))
    expect_equal(x12$successPerStage[2, ], c(0, 0, 0, 0), label = paste0(x12$successPerStage[2, ]))
    expect_equal(x12$successPerStage[3, ], c(0, 0, 0, 0), label = paste0(x12$successPerStage[3, ]))
    expect_equal(unlist(as.list(x12$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0), label = paste0(unlist(as.list(x12$selectedArms))))
    expect_equal(x12$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x12$numberOfActiveArms[1, ]))
    expect_equal(x12$numberOfActiveArms[2, ], c(NaN, NaN, NaN, NaN), label = paste0(x12$numberOfActiveArms[2, ]))
    expect_equal(x12$numberOfActiveArms[3, ], c(NaN, NaN, NaN, NaN), label = paste0(x12$numberOfActiveArms[3, ]))
    expect_equal(x12$expectedNumberOfEvents, c(NaN, NaN, NaN, NaN), label = paste0(x12$expectedNumberOfEvents))
    expect_equal(unlist(as.list(x12$singleEventsPerArmAndStage)), c(2, NaN, NaN, 2.0909091, NaN, NaN, 2.1666667, NaN, NaN, 2.2307692, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 1.9090909, NaN, NaN, 1.8333333, NaN, NaN, 1.7692308, NaN, NaN, 2, NaN, NaN, 1.8181818, NaN, NaN, 1.6666667, NaN, NaN, 1.5384615, NaN, NaN, 2, NaN, NaN, 2.1818182, NaN, NaN, 2.3333333, NaN, NaN, 2.4615385, NaN, NaN), tolerance = 1e-07, label = paste0(unlist(as.list(x12$singleEventsPerArmAndStage))))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x12), NA)))
        expect_output(print(x12)$show())
        invisible(capture.output(expect_error(summary(x12), NA)))
        expect_output(summary(x12)$show())
        x12CodeBased <- eval(parse(text = getObjectRCode(x12, stringWrapParagraphWidth = NULL)))
        expect_equal(x12CodeBased$cumulativeEventsPerStage, x12$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(x12CodeBased$iterations, x12$iterations, tolerance = 1e-07)
        expect_equal(x12CodeBased$rejectAtLeastOne, x12$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x12CodeBased$rejectedArmsPerStage, x12$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x12CodeBased$futilityStop, x12$futilityStop, tolerance = 1e-07)
        expect_equal(x12CodeBased$futilityPerStage, x12$futilityPerStage, tolerance = 1e-07)
        expect_equal(x12CodeBased$earlyStop, x12$earlyStop, tolerance = 1e-07)
        expect_equal(x12CodeBased$successPerStage, x12$successPerStage, tolerance = 1e-07)
        expect_equal(x12CodeBased$selectedArms, x12$selectedArms, tolerance = 1e-07)
        expect_equal(x12CodeBased$numberOfActiveArms, x12$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x12CodeBased$expectedNumberOfEvents, x12$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(x12CodeBased$singleEventsPerArmAndStage, x12$singleEventsPerArmAndStage, tolerance = 1e-07)
        expect_type(names(x12), "character")
        df <- as.data.frame(x12)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x12)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x13 <- getSimulationMultiArmSurvival(
        seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
        typeOfShape = "userDefined",
        activeArms = 4, directionUpper = FALSE, threshold = 0,
        plannedEvents = c(10, 30, 50), adaptations = rep(TRUE, 2),
        effectMatrix = matrix(c(0.1, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.5), ncol = 4),
        conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),
        intersectionTest = "Sidak",
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmSurvival object 'x13' with expected results
    expect_equal(unlist(as.list(x13$cumulativeEventsPerStage)), c(5.5, 5.5, 5.5, 5, 5, 5, 6.5, 6.5, 6.5, 5.8333333, 5.8333333, 5.8333333, 6, 6, 6, 5.4166667, 5.4166667, 5.4166667, 7, 7, 7, 6.25, 6.25, 6.25), tolerance = 1e-07, label = paste0(unlist(as.list(x13$cumulativeEventsPerStage))))
    expect_equal(x13$iterations[1, ], c(10, 10), label = paste0(x13$iterations[1, ]))
    expect_equal(x13$iterations[2, ], c(0, 0), label = paste0(x13$iterations[2, ]))
    expect_equal(x13$iterations[3, ], c(0, 0), label = paste0(x13$iterations[3, ]))
    expect_equal(x13$rejectAtLeastOne, c(0, 0), label = paste0(x13$rejectAtLeastOne))
    expect_equal(unlist(as.list(x13$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), label = paste0(unlist(as.list(x13$rejectedArmsPerStage))))
    expect_equal(x13$futilityStop, c(1, 1), label = paste0(x13$futilityStop))
    expect_equal(x13$futilityPerStage[1, ], c(1, 1), label = paste0(x13$futilityPerStage[1, ]))
    expect_equal(x13$futilityPerStage[2, ], c(0, 0), label = paste0(x13$futilityPerStage[2, ]))
    expect_equal(x13$earlyStop[1, ], c(1, 1), label = paste0(x13$earlyStop[1, ]))
    expect_equal(x13$earlyStop[2, ], c(0, 0), label = paste0(x13$earlyStop[2, ]))
    expect_equal(x13$successPerStage[1, ], c(0, 0), label = paste0(x13$successPerStage[1, ]))
    expect_equal(x13$successPerStage[2, ], c(0, 0), label = paste0(x13$successPerStage[2, ]))
    expect_equal(x13$successPerStage[3, ], c(0, 0), label = paste0(x13$successPerStage[3, ]))
    expect_equal(unlist(as.list(x13$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0), label = paste0(unlist(as.list(x13$selectedArms))))
    expect_equal(x13$numberOfActiveArms[1, ], c(4, 4), label = paste0(x13$numberOfActiveArms[1, ]))
    expect_equal(x13$numberOfActiveArms[2, ], c(NaN, NaN), label = paste0(x13$numberOfActiveArms[2, ]))
    expect_equal(x13$numberOfActiveArms[3, ], c(NaN, NaN), label = paste0(x13$numberOfActiveArms[3, ]))
    expect_equal(x13$expectedNumberOfEvents, c(NaN, NaN), label = paste0(x13$expectedNumberOfEvents))
    expect_equal(unlist(as.list(x13$singleEventsPerArmAndStage)), c(0.5, NaN, NaN, 0.83333333, NaN, NaN, 1.5, NaN, NaN, 1.6666667, NaN, NaN, 1, NaN, NaN, 1.25, NaN, NaN, 2, NaN, NaN, 2.0833333, NaN, NaN, 5, NaN, NaN, 4.1666667, NaN, NaN), tolerance = 1e-07, label = paste0(unlist(as.list(x13$singleEventsPerArmAndStage))))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x13), NA)))
        expect_output(print(x13)$show())
        invisible(capture.output(expect_error(summary(x13), NA)))
        expect_output(summary(x13)$show())
        x13CodeBased <- eval(parse(text = getObjectRCode(x13, stringWrapParagraphWidth = NULL)))
        expect_equal(x13CodeBased$cumulativeEventsPerStage, x13$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(x13CodeBased$iterations, x13$iterations, tolerance = 1e-07)
        expect_equal(x13CodeBased$rejectAtLeastOne, x13$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x13CodeBased$rejectedArmsPerStage, x13$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x13CodeBased$futilityStop, x13$futilityStop, tolerance = 1e-07)
        expect_equal(x13CodeBased$futilityPerStage, x13$futilityPerStage, tolerance = 1e-07)
        expect_equal(x13CodeBased$earlyStop, x13$earlyStop, tolerance = 1e-07)
        expect_equal(x13CodeBased$successPerStage, x13$successPerStage, tolerance = 1e-07)
        expect_equal(x13CodeBased$selectedArms, x13$selectedArms, tolerance = 1e-07)
        expect_equal(x13CodeBased$numberOfActiveArms, x13$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x13CodeBased$expectedNumberOfEvents, x13$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(x13CodeBased$singleEventsPerArmAndStage, x13$singleEventsPerArmAndStage, tolerance = 1e-07)
        expect_type(names(x13), "character")
        df <- as.data.frame(x13)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x13)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x14 <- getSimulationMultiArmSurvival(
        seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
        typeOfShape = "sigmoidEmax", gED50 = 2, slope = 0.5, activeArms = 4, directionUpper = FALSE, threshold = 0,
        plannedEvents = c(10, 30, 50), omegaMaxVector = 1 / seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2),
        conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),
        intersectionTest = "Sidak",
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmSurvival object 'x14' with expected results
    expect_equal(unlist(as.list(x14$cumulativeEventsPerStage)), c(4, 4, 4, 4.1452587, 4.1452587, 4.1452587, 4.2627857, 4.2627857, 4.2627857, 4.3598306, 4.3598306, 4.3598306, 4, 4, 4, 4.1145653, 4.1145653, 4.1145653, 4.2072587, 4.2072587, 4.2072587, 4.2837979, 4.2837979, 4.2837979, 4, 4, 4, 4.0964933, 4.0964933, 4.0964933, 4.1745649, 4.1745649, 4.1745649, 4.2390305, 4.2390305, 4.2390305, 4, 4, 4, 4.0838719, 4.0838719, 4.0838719, 4.1517317, 4.1517317, 4.1517317, 4.2077651, 4.2077651, 4.2077651), tolerance = 1e-07, label = paste0(unlist(as.list(x14$cumulativeEventsPerStage))))
    expect_equal(x14$iterations[1, ], c(10, 10, 10, 10), label = paste0(x14$iterations[1, ]))
    expect_equal(x14$iterations[2, ], c(0, 0, 0, 0), label = paste0(x14$iterations[2, ]))
    expect_equal(x14$iterations[3, ], c(0, 0, 0, 0), label = paste0(x14$iterations[3, ]))
    expect_equal(x14$rejectAtLeastOne, c(0, 0, 0, 0), label = paste0(x14$rejectAtLeastOne))
    expect_equal(unlist(as.list(x14$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), label = paste0(unlist(as.list(x14$rejectedArmsPerStage))))
    expect_equal(x14$futilityStop, c(1, 1, 1, 1), label = paste0(x14$futilityStop))
    expect_equal(x14$futilityPerStage[1, ], c(1, 1, 1, 1), label = paste0(x14$futilityPerStage[1, ]))
    expect_equal(x14$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(x14$futilityPerStage[2, ]))
    expect_equal(x14$earlyStop[1, ], c(1, 1, 1, 1), label = paste0(x14$earlyStop[1, ]))
    expect_equal(x14$earlyStop[2, ], c(0, 0, 0, 0), label = paste0(x14$earlyStop[2, ]))
    expect_equal(x14$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x14$successPerStage[1, ]))
    expect_equal(x14$successPerStage[2, ], c(0, 0, 0, 0), label = paste0(x14$successPerStage[2, ]))
    expect_equal(x14$successPerStage[3, ], c(0, 0, 0, 0), label = paste0(x14$successPerStage[3, ]))
    expect_equal(unlist(as.list(x14$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0), label = paste0(unlist(as.list(x14$selectedArms))))
    expect_equal(x14$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x14$numberOfActiveArms[1, ]))
    expect_equal(x14$numberOfActiveArms[2, ], c(NaN, NaN, NaN, NaN), label = paste0(x14$numberOfActiveArms[2, ]))
    expect_equal(x14$numberOfActiveArms[3, ], c(NaN, NaN, NaN, NaN), label = paste0(x14$numberOfActiveArms[3, ]))
    expect_equal(x14$expectedNumberOfEvents, c(NaN, NaN, NaN, NaN), label = paste0(x14$expectedNumberOfEvents))
    expect_equal(unlist(as.list(x14$singleEventsPerArmAndStage)), c(2, NaN, NaN, 1.9985289, NaN, NaN, 1.9973387, NaN, NaN, 1.996356, NaN, NaN, 2, NaN, NaN, 1.9678356, NaN, NaN, 1.9418117, NaN, NaN, 1.9203232, NaN, NaN, 2, NaN, NaN, 1.9497636, NaN, NaN, 1.9091179, NaN, NaN, 1.8755558, NaN, NaN, 2, NaN, NaN, 1.9371422, NaN, NaN, 1.8862847, NaN, NaN, 1.8442904, NaN, NaN, 2, NaN, NaN, 2.1467297, NaN, NaN, 2.265447, NaN, NaN, 2.3634747, NaN, NaN), tolerance = 1e-07, label = paste0(unlist(as.list(x14$singleEventsPerArmAndStage))))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x14), NA)))
        expect_output(print(x14)$show())
        invisible(capture.output(expect_error(summary(x14), NA)))
        expect_output(summary(x14)$show())
        x14CodeBased <- eval(parse(text = getObjectRCode(x14, stringWrapParagraphWidth = NULL)))
        expect_equal(x14CodeBased$cumulativeEventsPerStage, x14$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(x14CodeBased$iterations, x14$iterations, tolerance = 1e-07)
        expect_equal(x14CodeBased$rejectAtLeastOne, x14$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x14CodeBased$rejectedArmsPerStage, x14$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x14CodeBased$futilityStop, x14$futilityStop, tolerance = 1e-07)
        expect_equal(x14CodeBased$futilityPerStage, x14$futilityPerStage, tolerance = 1e-07)
        expect_equal(x14CodeBased$earlyStop, x14$earlyStop, tolerance = 1e-07)
        expect_equal(x14CodeBased$successPerStage, x14$successPerStage, tolerance = 1e-07)
        expect_equal(x14CodeBased$selectedArms, x14$selectedArms, tolerance = 1e-07)
        expect_equal(x14CodeBased$numberOfActiveArms, x14$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x14CodeBased$expectedNumberOfEvents, x14$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(x14CodeBased$singleEventsPerArmAndStage, x14$singleEventsPerArmAndStage, tolerance = 1e-07)
        expect_type(names(x14), "character")
        df <- as.data.frame(x14)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x14)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x15 <- getSimulationMultiArmSurvival(
        seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
        activeArms = 4, directionUpper = FALSE, threshold = 0, typeOfSelection = "all",
        plannedEvents = c(10, 30, 50), omegaMaxVector = 1 / seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2),
        conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),
        intersectionTest = "Sidak",
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmSurvival object 'x15' with expected results
    expect_equal(unlist(as.list(x15$cumulativeEventsPerStage)), c(4, 4, 4, 4.2727273, 4.2727273, 4.2727273, 4.5, 4.5, 4.5, 4.6923077, 4.6923077, 4.6923077, 4, 4, 4, 4.1818182, 4.1818182, 4.1818182, 4.3333333, 4.3333333, 4.3333333, 4.4615385, 4.4615385, 4.4615385, 4, 4, 4, 4.0909091, 4.0909091, 4.0909091, 4.1666667, 4.1666667, 4.1666667, 4.2307692, 4.2307692, 4.2307692, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4), tolerance = 1e-07, label = paste0(unlist(as.list(x15$cumulativeEventsPerStage))))
    expect_equal(x15$iterations[1, ], c(10, 10, 10, 10), label = paste0(x15$iterations[1, ]))
    expect_equal(x15$iterations[2, ], c(0, 0, 0, 0), label = paste0(x15$iterations[2, ]))
    expect_equal(x15$iterations[3, ], c(0, 0, 0, 0), label = paste0(x15$iterations[3, ]))
    expect_equal(x15$rejectAtLeastOne, c(0, 0, 0, 0), label = paste0(x15$rejectAtLeastOne))
    expect_equal(unlist(as.list(x15$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), label = paste0(unlist(as.list(x15$rejectedArmsPerStage))))
    expect_equal(x15$futilityStop, c(1, 1, 1, 1), label = paste0(x15$futilityStop))
    expect_equal(x15$futilityPerStage[1, ], c(1, 1, 1, 1), label = paste0(x15$futilityPerStage[1, ]))
    expect_equal(x15$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(x15$futilityPerStage[2, ]))
    expect_equal(x15$earlyStop[1, ], c(1, 1, 1, 1), label = paste0(x15$earlyStop[1, ]))
    expect_equal(x15$earlyStop[2, ], c(0, 0, 0, 0), label = paste0(x15$earlyStop[2, ]))
    expect_equal(x15$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x15$successPerStage[1, ]))
    expect_equal(x15$successPerStage[2, ], c(0, 0, 0, 0), label = paste0(x15$successPerStage[2, ]))
    expect_equal(x15$successPerStage[3, ], c(0, 0, 0, 0), label = paste0(x15$successPerStage[3, ]))
    expect_equal(unlist(as.list(x15$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0), label = paste0(unlist(as.list(x15$selectedArms))))
    expect_equal(x15$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x15$numberOfActiveArms[1, ]))
    expect_equal(x15$numberOfActiveArms[2, ], c(NaN, NaN, NaN, NaN), label = paste0(x15$numberOfActiveArms[2, ]))
    expect_equal(x15$numberOfActiveArms[3, ], c(NaN, NaN, NaN, NaN), label = paste0(x15$numberOfActiveArms[3, ]))
    expect_equal(x15$expectedNumberOfEvents, c(NaN, NaN, NaN, NaN), label = paste0(x15$expectedNumberOfEvents))
    expect_equal(unlist(as.list(x15$singleEventsPerArmAndStage)), c(2, NaN, NaN, 2.0909091, NaN, NaN, 2.1666667, NaN, NaN, 2.2307692, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 1.9090909, NaN, NaN, 1.8333333, NaN, NaN, 1.7692308, NaN, NaN, 2, NaN, NaN, 1.8181818, NaN, NaN, 1.6666667, NaN, NaN, 1.5384615, NaN, NaN, 2, NaN, NaN, 2.1818182, NaN, NaN, 2.3333333, NaN, NaN, 2.4615385, NaN, NaN), tolerance = 1e-07, label = paste0(unlist(as.list(x15$singleEventsPerArmAndStage))))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x15), NA)))
        expect_output(print(x15)$show())
        invisible(capture.output(expect_error(summary(x15), NA)))
        expect_output(summary(x15)$show())
        x15CodeBased <- eval(parse(text = getObjectRCode(x15, stringWrapParagraphWidth = NULL)))
        expect_equal(x15CodeBased$cumulativeEventsPerStage, x15$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(x15CodeBased$iterations, x15$iterations, tolerance = 1e-07)
        expect_equal(x15CodeBased$rejectAtLeastOne, x15$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x15CodeBased$rejectedArmsPerStage, x15$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x15CodeBased$futilityStop, x15$futilityStop, tolerance = 1e-07)
        expect_equal(x15CodeBased$futilityPerStage, x15$futilityPerStage, tolerance = 1e-07)
        expect_equal(x15CodeBased$earlyStop, x15$earlyStop, tolerance = 1e-07)
        expect_equal(x15CodeBased$successPerStage, x15$successPerStage, tolerance = 1e-07)
        expect_equal(x15CodeBased$selectedArms, x15$selectedArms, tolerance = 1e-07)
        expect_equal(x15CodeBased$numberOfActiveArms, x15$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x15CodeBased$expectedNumberOfEvents, x15$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(x15CodeBased$singleEventsPerArmAndStage, x15$singleEventsPerArmAndStage, tolerance = 1e-07)
        expect_type(names(x15), "character")
        df <- as.data.frame(x15)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x15)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x16 <- getSimulationMultiArmSurvival(
        seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
        activeArms = 4, directionUpper = FALSE, threshold = 0, typeOfSelection = "rBest", rValue = 2,
        plannedEvents = c(10, 30, 50), omegaMaxVector = 1 / seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2),
        conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),
        intersectionTest = "Simes",
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmSurvival object 'x16' with expected results
    expect_equal(unlist(as.list(x16$cumulativeEventsPerStage)), c(4, 4, 4, 4.2727273, 4.2727273, 4.2727273, 4.5, 4.5, 4.5, 4.6923077, 4.6923077, 4.6923077, 4, 4, 4, 4.1818182, 4.1818182, 4.1818182, 4.3333333, 4.3333333, 4.3333333, 4.4615385, 4.4615385, 4.4615385, 4, 4, 4, 4.0909091, 4.0909091, 4.0909091, 4.1666667, 4.1666667, 4.1666667, 4.2307692, 4.2307692, 4.2307692, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4), tolerance = 1e-07, label = paste0(unlist(as.list(x16$cumulativeEventsPerStage))))
    expect_equal(x16$iterations[1, ], c(10, 10, 10, 10), label = paste0(x16$iterations[1, ]))
    expect_equal(x16$iterations[2, ], c(0, 0, 0, 0), label = paste0(x16$iterations[2, ]))
    expect_equal(x16$iterations[3, ], c(0, 0, 0, 0), label = paste0(x16$iterations[3, ]))
    expect_equal(x16$rejectAtLeastOne, c(0, 0, 0, 0), label = paste0(x16$rejectAtLeastOne))
    expect_equal(unlist(as.list(x16$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), label = paste0(unlist(as.list(x16$rejectedArmsPerStage))))
    expect_equal(x16$futilityStop, c(1, 1, 1, 1), label = paste0(x16$futilityStop))
    expect_equal(x16$futilityPerStage[1, ], c(1, 1, 1, 1), label = paste0(x16$futilityPerStage[1, ]))
    expect_equal(x16$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(x16$futilityPerStage[2, ]))
    expect_equal(x16$earlyStop[1, ], c(1, 1, 1, 1), label = paste0(x16$earlyStop[1, ]))
    expect_equal(x16$earlyStop[2, ], c(0, 0, 0, 0), label = paste0(x16$earlyStop[2, ]))
    expect_equal(x16$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x16$successPerStage[1, ]))
    expect_equal(x16$successPerStage[2, ], c(0, 0, 0, 0), label = paste0(x16$successPerStage[2, ]))
    expect_equal(x16$successPerStage[3, ], c(0, 0, 0, 0), label = paste0(x16$successPerStage[3, ]))
    expect_equal(unlist(as.list(x16$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0), label = paste0(unlist(as.list(x16$selectedArms))))
    expect_equal(x16$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x16$numberOfActiveArms[1, ]))
    expect_equal(x16$numberOfActiveArms[2, ], c(NaN, NaN, NaN, NaN), label = paste0(x16$numberOfActiveArms[2, ]))
    expect_equal(x16$numberOfActiveArms[3, ], c(NaN, NaN, NaN, NaN), label = paste0(x16$numberOfActiveArms[3, ]))
    expect_equal(x16$expectedNumberOfEvents, c(NaN, NaN, NaN, NaN), label = paste0(x16$expectedNumberOfEvents))
    expect_equal(unlist(as.list(x16$singleEventsPerArmAndStage)), c(2, NaN, NaN, 2.0909091, NaN, NaN, 2.1666667, NaN, NaN, 2.2307692, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 1.9090909, NaN, NaN, 1.8333333, NaN, NaN, 1.7692308, NaN, NaN, 2, NaN, NaN, 1.8181818, NaN, NaN, 1.6666667, NaN, NaN, 1.5384615, NaN, NaN, 2, NaN, NaN, 2.1818182, NaN, NaN, 2.3333333, NaN, NaN, 2.4615385, NaN, NaN), tolerance = 1e-07, label = paste0(unlist(as.list(x16$singleEventsPerArmAndStage))))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x16), NA)))
        expect_output(print(x16)$show())
        invisible(capture.output(expect_error(summary(x16), NA)))
        expect_output(summary(x16)$show())
        x16CodeBased <- eval(parse(text = getObjectRCode(x16, stringWrapParagraphWidth = NULL)))
        expect_equal(x16CodeBased$cumulativeEventsPerStage, x16$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(x16CodeBased$iterations, x16$iterations, tolerance = 1e-07)
        expect_equal(x16CodeBased$rejectAtLeastOne, x16$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x16CodeBased$rejectedArmsPerStage, x16$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x16CodeBased$futilityStop, x16$futilityStop, tolerance = 1e-07)
        expect_equal(x16CodeBased$futilityPerStage, x16$futilityPerStage, tolerance = 1e-07)
        expect_equal(x16CodeBased$earlyStop, x16$earlyStop, tolerance = 1e-07)
        expect_equal(x16CodeBased$successPerStage, x16$successPerStage, tolerance = 1e-07)
        expect_equal(x16CodeBased$selectedArms, x16$selectedArms, tolerance = 1e-07)
        expect_equal(x16CodeBased$numberOfActiveArms, x16$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x16CodeBased$expectedNumberOfEvents, x16$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(x16CodeBased$singleEventsPerArmAndStage, x16$singleEventsPerArmAndStage, tolerance = 1e-07)
        expect_type(names(x16), "character")
        df <- as.data.frame(x16)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x16)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x17 <- getSimulationMultiArmSurvival(
        seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
        activeArms = 4, directionUpper = FALSE, threshold = 0, typeOfSelection = "epsilon", epsilonValue = 0.1,
        plannedEvents = c(10, 30, 50), omegaMaxVector = 1 / seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2),
        conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),
        intersectionTest = "Simes",
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmSurvival object 'x17' with expected results
    expect_equal(unlist(as.list(x17$cumulativeEventsPerStage)), c(4, 4, 4, 4.2727273, 4.2727273, 4.2727273, 4.5, 4.5, 4.5, 4.6923077, 4.6923077, 4.6923077, 4, 4, 4, 4.1818182, 4.1818182, 4.1818182, 4.3333333, 4.3333333, 4.3333333, 4.4615385, 4.4615385, 4.4615385, 4, 4, 4, 4.0909091, 4.0909091, 4.0909091, 4.1666667, 4.1666667, 4.1666667, 4.2307692, 4.2307692, 4.2307692, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4), tolerance = 1e-07, label = paste0(unlist(as.list(x17$cumulativeEventsPerStage))))
    expect_equal(x17$iterations[1, ], c(10, 10, 10, 10), label = paste0(x17$iterations[1, ]))
    expect_equal(x17$iterations[2, ], c(0, 0, 0, 0), label = paste0(x17$iterations[2, ]))
    expect_equal(x17$iterations[3, ], c(0, 0, 0, 0), label = paste0(x17$iterations[3, ]))
    expect_equal(x17$rejectAtLeastOne, c(0, 0, 0, 0), label = paste0(x17$rejectAtLeastOne))
    expect_equal(unlist(as.list(x17$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), label = paste0(unlist(as.list(x17$rejectedArmsPerStage))))
    expect_equal(x17$futilityStop, c(1, 1, 1, 1), label = paste0(x17$futilityStop))
    expect_equal(x17$futilityPerStage[1, ], c(1, 1, 1, 1), label = paste0(x17$futilityPerStage[1, ]))
    expect_equal(x17$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(x17$futilityPerStage[2, ]))
    expect_equal(x17$earlyStop[1, ], c(1, 1, 1, 1), label = paste0(x17$earlyStop[1, ]))
    expect_equal(x17$earlyStop[2, ], c(0, 0, 0, 0), label = paste0(x17$earlyStop[2, ]))
    expect_equal(x17$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x17$successPerStage[1, ]))
    expect_equal(x17$successPerStage[2, ], c(0, 0, 0, 0), label = paste0(x17$successPerStage[2, ]))
    expect_equal(x17$successPerStage[3, ], c(0, 0, 0, 0), label = paste0(x17$successPerStage[3, ]))
    expect_equal(unlist(as.list(x17$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0), label = paste0(unlist(as.list(x17$selectedArms))))
    expect_equal(x17$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x17$numberOfActiveArms[1, ]))
    expect_equal(x17$numberOfActiveArms[2, ], c(NaN, NaN, NaN, NaN), label = paste0(x17$numberOfActiveArms[2, ]))
    expect_equal(x17$numberOfActiveArms[3, ], c(NaN, NaN, NaN, NaN), label = paste0(x17$numberOfActiveArms[3, ]))
    expect_equal(x17$expectedNumberOfEvents, c(NaN, NaN, NaN, NaN), label = paste0(x17$expectedNumberOfEvents))
    expect_equal(unlist(as.list(x17$singleEventsPerArmAndStage)), c(2, NaN, NaN, 2.0909091, NaN, NaN, 2.1666667, NaN, NaN, 2.2307692, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 1.9090909, NaN, NaN, 1.8333333, NaN, NaN, 1.7692308, NaN, NaN, 2, NaN, NaN, 1.8181818, NaN, NaN, 1.6666667, NaN, NaN, 1.5384615, NaN, NaN, 2, NaN, NaN, 2.1818182, NaN, NaN, 2.3333333, NaN, NaN, 2.4615385, NaN, NaN), tolerance = 1e-07, label = paste0(unlist(as.list(x17$singleEventsPerArmAndStage))))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x17), NA)))
        expect_output(print(x17)$show())
        invisible(capture.output(expect_error(summary(x17), NA)))
        expect_output(summary(x17)$show())
        x17CodeBased <- eval(parse(text = getObjectRCode(x17, stringWrapParagraphWidth = NULL)))
        expect_equal(x17CodeBased$cumulativeEventsPerStage, x17$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(x17CodeBased$iterations, x17$iterations, tolerance = 1e-07)
        expect_equal(x17CodeBased$rejectAtLeastOne, x17$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x17CodeBased$rejectedArmsPerStage, x17$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x17CodeBased$futilityStop, x17$futilityStop, tolerance = 1e-07)
        expect_equal(x17CodeBased$futilityPerStage, x17$futilityPerStage, tolerance = 1e-07)
        expect_equal(x17CodeBased$earlyStop, x17$earlyStop, tolerance = 1e-07)
        expect_equal(x17CodeBased$successPerStage, x17$successPerStage, tolerance = 1e-07)
        expect_equal(x17CodeBased$selectedArms, x17$selectedArms, tolerance = 1e-07)
        expect_equal(x17CodeBased$numberOfActiveArms, x17$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x17CodeBased$expectedNumberOfEvents, x17$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(x17CodeBased$singleEventsPerArmAndStage, x17$singleEventsPerArmAndStage, tolerance = 1e-07)
        expect_type(names(x17), "character")
        df <- as.data.frame(x17)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x17)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x18 <- getSimulationMultiArmSurvival(
        seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
        activeArms = 4, directionUpper = FALSE, threshold = 0,
        plannedEvents = c(10, 30, 50), omegaMaxVector = 1 / seq(1, 1.6, 0.2), adaptations = c(TRUE, FALSE),
        conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),
        intersectionTest = "Simes",
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmSurvival object 'x18' with expected results
    expect_equal(unlist(as.list(x18$cumulativeEventsPerStage)), c(4, 4, 4, 4.2727273, 4.2727273, 4.2727273, 4.5, 4.5, 4.5, 4.6923077, 4.6923077, 4.6923077, 4, 4, 4, 4.1818182, 4.1818182, 4.1818182, 4.3333333, 4.3333333, 4.3333333, 4.4615385, 4.4615385, 4.4615385, 4, 4, 4, 4.0909091, 4.0909091, 4.0909091, 4.1666667, 4.1666667, 4.1666667, 4.2307692, 4.2307692, 4.2307692, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4), tolerance = 1e-07, label = paste0(unlist(as.list(x18$cumulativeEventsPerStage))))
    expect_equal(x18$iterations[1, ], c(10, 10, 10, 10), label = paste0(x18$iterations[1, ]))
    expect_equal(x18$iterations[2, ], c(0, 0, 0, 0), label = paste0(x18$iterations[2, ]))
    expect_equal(x18$iterations[3, ], c(0, 0, 0, 0), label = paste0(x18$iterations[3, ]))
    expect_equal(x18$rejectAtLeastOne, c(0, 0, 0, 0), label = paste0(x18$rejectAtLeastOne))
    expect_equal(unlist(as.list(x18$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), label = paste0(unlist(as.list(x18$rejectedArmsPerStage))))
    expect_equal(x18$futilityStop, c(1, 1, 1, 1), label = paste0(x18$futilityStop))
    expect_equal(x18$futilityPerStage[1, ], c(1, 1, 1, 1), label = paste0(x18$futilityPerStage[1, ]))
    expect_equal(x18$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(x18$futilityPerStage[2, ]))
    expect_equal(x18$earlyStop[1, ], c(1, 1, 1, 1), label = paste0(x18$earlyStop[1, ]))
    expect_equal(x18$earlyStop[2, ], c(0, 0, 0, 0), label = paste0(x18$earlyStop[2, ]))
    expect_equal(x18$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x18$successPerStage[1, ]))
    expect_equal(x18$successPerStage[2, ], c(0, 0, 0, 0), label = paste0(x18$successPerStage[2, ]))
    expect_equal(x18$successPerStage[3, ], c(0, 0, 0, 0), label = paste0(x18$successPerStage[3, ]))
    expect_equal(unlist(as.list(x18$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0), label = paste0(unlist(as.list(x18$selectedArms))))
    expect_equal(x18$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x18$numberOfActiveArms[1, ]))
    expect_equal(x18$numberOfActiveArms[2, ], c(NaN, NaN, NaN, NaN), label = paste0(x18$numberOfActiveArms[2, ]))
    expect_equal(x18$numberOfActiveArms[3, ], c(NaN, NaN, NaN, NaN), label = paste0(x18$numberOfActiveArms[3, ]))
    expect_equal(x18$expectedNumberOfEvents, c(NaN, NaN, NaN, NaN), label = paste0(x18$expectedNumberOfEvents))
    expect_equal(unlist(as.list(x18$singleEventsPerArmAndStage)), c(2, NaN, NaN, 2.0909091, NaN, NaN, 2.1666667, NaN, NaN, 2.2307692, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 1.9090909, NaN, NaN, 1.8333333, NaN, NaN, 1.7692308, NaN, NaN, 2, NaN, NaN, 1.8181818, NaN, NaN, 1.6666667, NaN, NaN, 1.5384615, NaN, NaN, 2, NaN, NaN, 2.1818182, NaN, NaN, 2.3333333, NaN, NaN, 2.4615385, NaN, NaN), tolerance = 1e-07, label = paste0(unlist(as.list(x18$singleEventsPerArmAndStage))))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x18), NA)))
        expect_output(print(x18)$show())
        invisible(capture.output(expect_error(summary(x18), NA)))
        expect_output(summary(x18)$show())
        x18CodeBased <- eval(parse(text = getObjectRCode(x18, stringWrapParagraphWidth = NULL)))
        expect_equal(x18CodeBased$cumulativeEventsPerStage, x18$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(x18CodeBased$iterations, x18$iterations, tolerance = 1e-07)
        expect_equal(x18CodeBased$rejectAtLeastOne, x18$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x18CodeBased$rejectedArmsPerStage, x18$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x18CodeBased$futilityStop, x18$futilityStop, tolerance = 1e-07)
        expect_equal(x18CodeBased$futilityPerStage, x18$futilityPerStage, tolerance = 1e-07)
        expect_equal(x18CodeBased$earlyStop, x18$earlyStop, tolerance = 1e-07)
        expect_equal(x18CodeBased$successPerStage, x18$successPerStage, tolerance = 1e-07)
        expect_equal(x18CodeBased$selectedArms, x18$selectedArms, tolerance = 1e-07)
        expect_equal(x18CodeBased$numberOfActiveArms, x18$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x18CodeBased$expectedNumberOfEvents, x18$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(x18CodeBased$singleEventsPerArmAndStage, x18$singleEventsPerArmAndStage, tolerance = 1e-07)
        expect_type(names(x18), "character")
        df <- as.data.frame(x18)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x18)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x19 <- getSimulationMultiArmSurvival(
        seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
        activeArms = 4, directionUpper = FALSE, threshold = 0, typeOfSelection = "all",
        plannedEvents = c(10, 30, 50), omegaMaxVector = 1 / seq(1, 1.6, 0.2), adaptations = c(TRUE, FALSE),
        conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),
        intersectionTest = "Bonferroni",
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmSurvival object 'x19' with expected results
    expect_equal(unlist(as.list(x19$cumulativeEventsPerStage)), c(4, 4, 4, 4.2727273, 4.2727273, 4.2727273, 4.5, 4.5, 4.5, 4.6923077, 4.6923077, 4.6923077, 4, 4, 4, 4.1818182, 4.1818182, 4.1818182, 4.3333333, 4.3333333, 4.3333333, 4.4615385, 4.4615385, 4.4615385, 4, 4, 4, 4.0909091, 4.0909091, 4.0909091, 4.1666667, 4.1666667, 4.1666667, 4.2307692, 4.2307692, 4.2307692, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4), tolerance = 1e-07, label = paste0(unlist(as.list(x19$cumulativeEventsPerStage))))
    expect_equal(x19$iterations[1, ], c(10, 10, 10, 10), label = paste0(x19$iterations[1, ]))
    expect_equal(x19$iterations[2, ], c(0, 0, 0, 0), label = paste0(x19$iterations[2, ]))
    expect_equal(x19$iterations[3, ], c(0, 0, 0, 0), label = paste0(x19$iterations[3, ]))
    expect_equal(x19$rejectAtLeastOne, c(0, 0, 0, 0), label = paste0(x19$rejectAtLeastOne))
    expect_equal(unlist(as.list(x19$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), label = paste0(unlist(as.list(x19$rejectedArmsPerStage))))
    expect_equal(x19$futilityStop, c(1, 1, 1, 1), label = paste0(x19$futilityStop))
    expect_equal(x19$futilityPerStage[1, ], c(1, 1, 1, 1), label = paste0(x19$futilityPerStage[1, ]))
    expect_equal(x19$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(x19$futilityPerStage[2, ]))
    expect_equal(x19$earlyStop[1, ], c(1, 1, 1, 1), label = paste0(x19$earlyStop[1, ]))
    expect_equal(x19$earlyStop[2, ], c(0, 0, 0, 0), label = paste0(x19$earlyStop[2, ]))
    expect_equal(x19$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x19$successPerStage[1, ]))
    expect_equal(x19$successPerStage[2, ], c(0, 0, 0, 0), label = paste0(x19$successPerStage[2, ]))
    expect_equal(x19$successPerStage[3, ], c(0, 0, 0, 0), label = paste0(x19$successPerStage[3, ]))
    expect_equal(unlist(as.list(x19$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0), label = paste0(unlist(as.list(x19$selectedArms))))
    expect_equal(x19$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x19$numberOfActiveArms[1, ]))
    expect_equal(x19$numberOfActiveArms[2, ], c(NaN, NaN, NaN, NaN), label = paste0(x19$numberOfActiveArms[2, ]))
    expect_equal(x19$numberOfActiveArms[3, ], c(NaN, NaN, NaN, NaN), label = paste0(x19$numberOfActiveArms[3, ]))
    expect_equal(x19$expectedNumberOfEvents, c(NaN, NaN, NaN, NaN), label = paste0(x19$expectedNumberOfEvents))
    expect_equal(unlist(as.list(x19$singleEventsPerArmAndStage)), c(2, NaN, NaN, 2.0909091, NaN, NaN, 2.1666667, NaN, NaN, 2.2307692, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 1.9090909, NaN, NaN, 1.8333333, NaN, NaN, 1.7692308, NaN, NaN, 2, NaN, NaN, 1.8181818, NaN, NaN, 1.6666667, NaN, NaN, 1.5384615, NaN, NaN, 2, NaN, NaN, 2.1818182, NaN, NaN, 2.3333333, NaN, NaN, 2.4615385, NaN, NaN), tolerance = 1e-07, label = paste0(unlist(as.list(x19$singleEventsPerArmAndStage))))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x19), NA)))
        expect_output(print(x19)$show())
        invisible(capture.output(expect_error(summary(x19), NA)))
        expect_output(summary(x19)$show())
        x19CodeBased <- eval(parse(text = getObjectRCode(x19, stringWrapParagraphWidth = NULL)))
        expect_equal(x19CodeBased$cumulativeEventsPerStage, x19$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(x19CodeBased$iterations, x19$iterations, tolerance = 1e-07)
        expect_equal(x19CodeBased$rejectAtLeastOne, x19$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x19CodeBased$rejectedArmsPerStage, x19$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x19CodeBased$futilityStop, x19$futilityStop, tolerance = 1e-07)
        expect_equal(x19CodeBased$futilityPerStage, x19$futilityPerStage, tolerance = 1e-07)
        expect_equal(x19CodeBased$earlyStop, x19$earlyStop, tolerance = 1e-07)
        expect_equal(x19CodeBased$successPerStage, x19$successPerStage, tolerance = 1e-07)
        expect_equal(x19CodeBased$selectedArms, x19$selectedArms, tolerance = 1e-07)
        expect_equal(x19CodeBased$numberOfActiveArms, x19$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x19CodeBased$expectedNumberOfEvents, x19$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(x19CodeBased$singleEventsPerArmAndStage, x19$singleEventsPerArmAndStage, tolerance = 1e-07)
        expect_type(names(x19), "character")
        df <- as.data.frame(x19)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x19)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x20 <- getSimulationMultiArmSurvival(
        seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
        activeArms = 4, directionUpper = FALSE, threshold = 0, typeOfSelection = "rBest", rValue = 2,
        plannedEvents = c(10, 30, 50), omegaMaxVector = 1 / seq(1, 1.6, 0.2), adaptations = c(TRUE, FALSE),
        intersectionTest = "Bonferroni",
        conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmSurvival object 'x20' with expected results
    expect_equal(unlist(as.list(x20$cumulativeEventsPerStage)), c(4, 4, 4, 4.2727273, 4.2727273, 4.2727273, 4.5, 4.5, 4.5, 4.6923077, 4.6923077, 4.6923077, 4, 4, 4, 4.1818182, 4.1818182, 4.1818182, 4.3333333, 4.3333333, 4.3333333, 4.4615385, 4.4615385, 4.4615385, 4, 4, 4, 4.0909091, 4.0909091, 4.0909091, 4.1666667, 4.1666667, 4.1666667, 4.2307692, 4.2307692, 4.2307692, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4), tolerance = 1e-07, label = paste0(unlist(as.list(x20$cumulativeEventsPerStage))))
    expect_equal(x20$iterations[1, ], c(10, 10, 10, 10), label = paste0(x20$iterations[1, ]))
    expect_equal(x20$iterations[2, ], c(0, 0, 0, 0), label = paste0(x20$iterations[2, ]))
    expect_equal(x20$iterations[3, ], c(0, 0, 0, 0), label = paste0(x20$iterations[3, ]))
    expect_equal(x20$rejectAtLeastOne, c(0, 0, 0, 0), label = paste0(x20$rejectAtLeastOne))
    expect_equal(unlist(as.list(x20$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), label = paste0(unlist(as.list(x20$rejectedArmsPerStage))))
    expect_equal(x20$futilityStop, c(1, 1, 1, 1), label = paste0(x20$futilityStop))
    expect_equal(x20$futilityPerStage[1, ], c(1, 1, 1, 1), label = paste0(x20$futilityPerStage[1, ]))
    expect_equal(x20$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(x20$futilityPerStage[2, ]))
    expect_equal(x20$earlyStop[1, ], c(1, 1, 1, 1), label = paste0(x20$earlyStop[1, ]))
    expect_equal(x20$earlyStop[2, ], c(0, 0, 0, 0), label = paste0(x20$earlyStop[2, ]))
    expect_equal(x20$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x20$successPerStage[1, ]))
    expect_equal(x20$successPerStage[2, ], c(0, 0, 0, 0), label = paste0(x20$successPerStage[2, ]))
    expect_equal(x20$successPerStage[3, ], c(0, 0, 0, 0), label = paste0(x20$successPerStage[3, ]))
    expect_equal(unlist(as.list(x20$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0), label = paste0(unlist(as.list(x20$selectedArms))))
    expect_equal(x20$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x20$numberOfActiveArms[1, ]))
    expect_equal(x20$numberOfActiveArms[2, ], c(NaN, NaN, NaN, NaN), label = paste0(x20$numberOfActiveArms[2, ]))
    expect_equal(x20$numberOfActiveArms[3, ], c(NaN, NaN, NaN, NaN), label = paste0(x20$numberOfActiveArms[3, ]))
    expect_equal(x20$expectedNumberOfEvents, c(NaN, NaN, NaN, NaN), label = paste0(x20$expectedNumberOfEvents))
    expect_equal(unlist(as.list(x20$singleEventsPerArmAndStage)), c(2, NaN, NaN, 2.0909091, NaN, NaN, 2.1666667, NaN, NaN, 2.2307692, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 1.9090909, NaN, NaN, 1.8333333, NaN, NaN, 1.7692308, NaN, NaN, 2, NaN, NaN, 1.8181818, NaN, NaN, 1.6666667, NaN, NaN, 1.5384615, NaN, NaN, 2, NaN, NaN, 2.1818182, NaN, NaN, 2.3333333, NaN, NaN, 2.4615385, NaN, NaN), tolerance = 1e-07, label = paste0(unlist(as.list(x20$singleEventsPerArmAndStage))))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x20), NA)))
        expect_output(print(x20)$show())
        invisible(capture.output(expect_error(summary(x20), NA)))
        expect_output(summary(x20)$show())
        x20CodeBased <- eval(parse(text = getObjectRCode(x20, stringWrapParagraphWidth = NULL)))
        expect_equal(x20CodeBased$cumulativeEventsPerStage, x20$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(x20CodeBased$iterations, x20$iterations, tolerance = 1e-07)
        expect_equal(x20CodeBased$rejectAtLeastOne, x20$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x20CodeBased$rejectedArmsPerStage, x20$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x20CodeBased$futilityStop, x20$futilityStop, tolerance = 1e-07)
        expect_equal(x20CodeBased$futilityPerStage, x20$futilityPerStage, tolerance = 1e-07)
        expect_equal(x20CodeBased$earlyStop, x20$earlyStop, tolerance = 1e-07)
        expect_equal(x20CodeBased$successPerStage, x20$successPerStage, tolerance = 1e-07)
        expect_equal(x20CodeBased$selectedArms, x20$selectedArms, tolerance = 1e-07)
        expect_equal(x20CodeBased$numberOfActiveArms, x20$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x20CodeBased$expectedNumberOfEvents, x20$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(x20CodeBased$singleEventsPerArmAndStage, x20$singleEventsPerArmAndStage, tolerance = 1e-07)
        expect_type(names(x20), "character")
        df <- as.data.frame(x20)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x20)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x21 <- getSimulationMultiArmSurvival(
        seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
        activeArms = 4, directionUpper = FALSE, threshold = 0, typeOfSelection = "epsilon", epsilonValue = 0.1,
        plannedEvents = c(10, 30, 50), omegaMaxVector = 1 / seq(1, 1.6, 0.2), adaptations = c(TRUE, FALSE),
        intersectionTest = "Bonferroni",
        conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmSurvival object 'x21' with expected results
    expect_equal(unlist(as.list(x21$cumulativeEventsPerStage)), c(4, 4, 4, 4.2727273, 4.2727273, 4.2727273, 4.5, 4.5, 4.5, 4.6923077, 4.6923077, 4.6923077, 4, 4, 4, 4.1818182, 4.1818182, 4.1818182, 4.3333333, 4.3333333, 4.3333333, 4.4615385, 4.4615385, 4.4615385, 4, 4, 4, 4.0909091, 4.0909091, 4.0909091, 4.1666667, 4.1666667, 4.1666667, 4.2307692, 4.2307692, 4.2307692, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4), tolerance = 1e-07, label = paste0(unlist(as.list(x21$cumulativeEventsPerStage))))
    expect_equal(x21$iterations[1, ], c(10, 10, 10, 10), label = paste0(x21$iterations[1, ]))
    expect_equal(x21$iterations[2, ], c(0, 0, 0, 0), label = paste0(x21$iterations[2, ]))
    expect_equal(x21$iterations[3, ], c(0, 0, 0, 0), label = paste0(x21$iterations[3, ]))
    expect_equal(x21$rejectAtLeastOne, c(0, 0, 0, 0), label = paste0(x21$rejectAtLeastOne))
    expect_equal(unlist(as.list(x21$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), label = paste0(unlist(as.list(x21$rejectedArmsPerStage))))
    expect_equal(x21$futilityStop, c(1, 1, 1, 1), label = paste0(x21$futilityStop))
    expect_equal(x21$futilityPerStage[1, ], c(1, 1, 1, 1), label = paste0(x21$futilityPerStage[1, ]))
    expect_equal(x21$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(x21$futilityPerStage[2, ]))
    expect_equal(x21$earlyStop[1, ], c(1, 1, 1, 1), label = paste0(x21$earlyStop[1, ]))
    expect_equal(x21$earlyStop[2, ], c(0, 0, 0, 0), label = paste0(x21$earlyStop[2, ]))
    expect_equal(x21$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x21$successPerStage[1, ]))
    expect_equal(x21$successPerStage[2, ], c(0, 0, 0, 0), label = paste0(x21$successPerStage[2, ]))
    expect_equal(x21$successPerStage[3, ], c(0, 0, 0, 0), label = paste0(x21$successPerStage[3, ]))
    expect_equal(unlist(as.list(x21$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0), label = paste0(unlist(as.list(x21$selectedArms))))
    expect_equal(x21$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x21$numberOfActiveArms[1, ]))
    expect_equal(x21$numberOfActiveArms[2, ], c(NaN, NaN, NaN, NaN), label = paste0(x21$numberOfActiveArms[2, ]))
    expect_equal(x21$numberOfActiveArms[3, ], c(NaN, NaN, NaN, NaN), label = paste0(x21$numberOfActiveArms[3, ]))
    expect_equal(x21$expectedNumberOfEvents, c(NaN, NaN, NaN, NaN), label = paste0(x21$expectedNumberOfEvents))
    expect_equal(unlist(as.list(x21$singleEventsPerArmAndStage)), c(2, NaN, NaN, 2.0909091, NaN, NaN, 2.1666667, NaN, NaN, 2.2307692, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 1.9090909, NaN, NaN, 1.8333333, NaN, NaN, 1.7692308, NaN, NaN, 2, NaN, NaN, 1.8181818, NaN, NaN, 1.6666667, NaN, NaN, 1.5384615, NaN, NaN, 2, NaN, NaN, 2.1818182, NaN, NaN, 2.3333333, NaN, NaN, 2.4615385, NaN, NaN), tolerance = 1e-07, label = paste0(unlist(as.list(x21$singleEventsPerArmAndStage))))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x21), NA)))
        expect_output(print(x21)$show())
        invisible(capture.output(expect_error(summary(x21), NA)))
        expect_output(summary(x21)$show())
        x21CodeBased <- eval(parse(text = getObjectRCode(x21, stringWrapParagraphWidth = NULL)))
        expect_equal(x21CodeBased$cumulativeEventsPerStage, x21$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(x21CodeBased$iterations, x21$iterations, tolerance = 1e-07)
        expect_equal(x21CodeBased$rejectAtLeastOne, x21$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x21CodeBased$rejectedArmsPerStage, x21$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x21CodeBased$futilityStop, x21$futilityStop, tolerance = 1e-07)
        expect_equal(x21CodeBased$futilityPerStage, x21$futilityPerStage, tolerance = 1e-07)
        expect_equal(x21CodeBased$earlyStop, x21$earlyStop, tolerance = 1e-07)
        expect_equal(x21CodeBased$successPerStage, x21$successPerStage, tolerance = 1e-07)
        expect_equal(x21CodeBased$selectedArms, x21$selectedArms, tolerance = 1e-07)
        expect_equal(x21CodeBased$numberOfActiveArms, x21$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x21CodeBased$expectedNumberOfEvents, x21$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(x21CodeBased$singleEventsPerArmAndStage, x21$singleEventsPerArmAndStage, tolerance = 1e-07)
        expect_type(names(x21), "character")
        df <- as.data.frame(x21)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x21)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x22 <- getSimulationMultiArmSurvival(
        seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
        activeArms = 4, directionUpper = FALSE, threshold = 0.1,
        plannedEvents = c(10, 30, 50), omegaMaxVector = seq(0.1, 0.3, 0.1), intersectionTest = "Bonferroni",
        conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 10
    )

    ## Comparison of the results of SimulationResultsMultiArmSurvival object 'x22' with expected results
    expect_equal(unlist(as.list(x22$cumulativeEventsPerStage)), c(6.4545455, 10.090909, 10.090909, 6, 9.1343922, 9.1343922, 5.6153846, 8.7178796, 8.7178796, 5.6363636, 9.2727273, 9.2727273, 5.3333333, 8.8427255, 8.8427255, 5.0769231, 8.7046706, 8.7046706, 4.8181818, 8.4545455, 8.4545455, 4.6666667, 8.1381491, 8.1381491, 4.5384615, 7.6409565, 7.6409565, 4, 8, 8, 4, 7.4677255, 7.4677255, 4, 7.7908192, 7.7908192), tolerance = 1e-07, label = paste0(unlist(as.list(x22$cumulativeEventsPerStage))))
    expect_equal(x22$iterations[1, ], c(10, 10, 10), label = paste0(x22$iterations[1, ]))
    expect_equal(x22$iterations[2, ], c(1, 4, 3), label = paste0(x22$iterations[2, ]))
    expect_equal(x22$iterations[3, ], c(0, 0, 0), label = paste0(x22$iterations[3, ]))
    expect_equal(x22$rejectAtLeastOne, c(0.1, 0.3, 0.2), tolerance = 1e-07, label = paste0(x22$rejectAtLeastOne))
    expect_equal(unlist(as.list(x22$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.2, 0, 0, 0.2, 0), tolerance = 1e-07, label = paste0(unlist(as.list(x22$rejectedArmsPerStage))))
    expect_equal(x22$futilityStop, c(0.9, 0.7, 0.8), tolerance = 1e-07, label = paste0(x22$futilityStop))
    expect_equal(x22$futilityPerStage[1, ], c(0.9, 0.6, 0.7), tolerance = 1e-07, label = paste0(x22$futilityPerStage[1, ]))
    expect_equal(x22$futilityPerStage[2, ], c(0, 0.1, 0.1), tolerance = 1e-07, label = paste0(x22$futilityPerStage[2, ]))
    expect_equal(x22$earlyStop[1, ], c(0.9, 0.6, 0.7), tolerance = 1e-07, label = paste0(x22$earlyStop[1, ]))
    expect_equal(x22$earlyStop[2, ], c(0.1, 0.4, 0.3), tolerance = 1e-07, label = paste0(x22$earlyStop[2, ]))
    expect_equal(x22$successPerStage[1, ], c(0, 0, 0), label = paste0(x22$successPerStage[1, ]))
    expect_equal(x22$successPerStage[2, ], c(0.1, 0.3, 0.2), tolerance = 1e-07, label = paste0(x22$successPerStage[2, ]))
    expect_equal(x22$successPerStage[3, ], c(0, 0, 0), label = paste0(x22$successPerStage[3, ]))
    expect_equal(unlist(as.list(x22$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0.1, 0, 1, 0.1, 0, 1, 0, 0, 1, 0.1, 0, 1, 0, 0, 1, 0.1, 0, 1, 0.2, 0, 1, 0.2, 0), tolerance = 1e-07, label = paste0(unlist(as.list(x22$selectedArms))))
    expect_equal(x22$numberOfActiveArms[1, ], c(4, 4, 4), label = paste0(x22$numberOfActiveArms[1, ]))
    expect_equal(x22$numberOfActiveArms[2, ], c(1, 1, 1), label = paste0(x22$numberOfActiveArms[2, ]))
    expect_equal(x22$numberOfActiveArms[3, ], c(NaN, NaN, NaN), label = paste0(x22$numberOfActiveArms[3, ]))
    expect_equal(x22$expectedNumberOfEvents, c(NaN, NaN, NaN), label = paste0(x22$expectedNumberOfEvents))
    expect_equal(unlist(as.list(x22$singleEventsPerArmAndStage)), c(2.8181818, 0, NaN, 2.6666667, 0, NaN, 2.5384615, 0, NaN, 2, 0, NaN, 2, 0.375, NaN, 2, 0.52525253, NaN, 1.1818182, 0, NaN, 1.3333333, 0.33709021, NaN, 1.4615385, 0, NaN, 0.36363636, 0.36363636, NaN, 0.66666667, 0.33333333, NaN, 0.92307692, 0.68832425, NaN, 3.6363636, 3.6363636, NaN, 3.3333333, 3.1343922, NaN, 3.0769231, 3.102495, NaN), tolerance = 1e-07, label = paste0(unlist(as.list(x22$singleEventsPerArmAndStage))))
    expect_equal(x22$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_), label = paste0(x22$conditionalPowerAchieved[1, ]))
    expect_equal(x22$conditionalPowerAchieved[2, ], c(0.99998124, 0.93006261, 0.86196268), tolerance = 1e-07, label = paste0(x22$conditionalPowerAchieved[2, ]))
    expect_equal(x22$conditionalPowerAchieved[3, ], c(NaN, NaN, NaN), label = paste0(x22$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x22), NA)))
        expect_output(print(x22)$show())
        invisible(capture.output(expect_error(summary(x22), NA)))
        expect_output(summary(x22)$show())
        x22CodeBased <- eval(parse(text = getObjectRCode(x22, stringWrapParagraphWidth = NULL)))
        expect_equal(x22CodeBased$cumulativeEventsPerStage, x22$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(x22CodeBased$iterations, x22$iterations, tolerance = 1e-07)
        expect_equal(x22CodeBased$rejectAtLeastOne, x22$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(x22CodeBased$rejectedArmsPerStage, x22$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(x22CodeBased$futilityStop, x22$futilityStop, tolerance = 1e-07)
        expect_equal(x22CodeBased$futilityPerStage, x22$futilityPerStage, tolerance = 1e-07)
        expect_equal(x22CodeBased$earlyStop, x22$earlyStop, tolerance = 1e-07)
        expect_equal(x22CodeBased$successPerStage, x22$successPerStage, tolerance = 1e-07)
        expect_equal(x22CodeBased$selectedArms, x22$selectedArms, tolerance = 1e-07)
        expect_equal(x22CodeBased$numberOfActiveArms, x22$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(x22CodeBased$expectedNumberOfEvents, x22$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(x22CodeBased$singleEventsPerArmAndStage, x22$singleEventsPerArmAndStage, tolerance = 1e-07)
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

test_that("'getSimulationMultiArmSurvival': using calcSubjectsFunction", {
    .skipTestIfDisabled()

    # @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
    # @refFS[Sec.]{fs:sec:simulatingMultiArmDesigns}
    # @refFS[Sec.]{fs:sec:simulatingMultiArmDoseResponseRelationShips}
    # @refFS[Sec.]{fs:sec:simulatingMultiArmSelections}
    # @refFS[Tab.]{fs:tab:output:getSimulationMultiArmSurvival}
    # @refFS[Formula]{fs:simulationMultiArmDoseResponse}
    # @refFS[Formula]{fs:simulationMultiArmSurvivalCholeskyTransformation}
    # @refFS[Formula]{fs:simulationMultiArmSurvivalCorrMatrix}
    # @refFS[Formula]{fs:simulationMultiArmSurvivalEvents}
    # @refFS[Formula]{fs:simulationMultiArmSurvivalLogRanks}
    # @refFS[Formula]{fs:simulationMultiArmSelections}
    # @refFS[Formula]{fs:multiarmRejectionRule}
    # @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
    calcSubjectsFunctionSimulationMultiArmSurvival <- function(..., stage, minNumberOfEventsPerStage) {
        return(ifelse(stage == 3, 33, minNumberOfEventsPerStage[stage]))
    }

    x <- getSimulationMultiArmSurvival(
        seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
        typeOfShape = "linear", activeArms = 4,
        plannedEvents = c(10, 30, 50), omegaMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
        directionUpper = FALSE,
        minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),
        maxNumberOfIterations = 10, calcEventsFunction = calcSubjectsFunctionSimulationMultiArmSurvival
    )

    ## Comparison of the results of SimulationResultsMultiArmSurvival object 'x' with expected results
    expect_equal(unlist(as.list(x$cumulativeEventsPerStage)), c(5.6153846, 8.5080558, 32.538332, 5.2857143, 7.9818379, 30.224858, 5, 7.6798535, 30.027381, 4.75, 7.4486928, 28.520044, 5.0769231, 8.1039238, 33.365555, 4.8571429, 7.7179724, 31.319816, 4.6666667, 7.3312821, 29.521667, 4.5, 6.9975232, 27.961662, 4.5384615, 7.765565, 33.68068, 4.4285714, 7.692437, 34.619328, 4.3333333, 7.4419048, 32.624048, 4.25, 7.3932749, 34.276803, 4, 6.9887723, 31.899976, 4, 7.2675522, 34.224858, 4, 7.0265201, 31.574048, 4, 6.6197454, 28.704254), tolerance = 1e-07, label = paste0(unlist(as.list(x$cumulativeEventsPerStage))))
    expect_equal(x$iterations[1, ], c(10, 10, 10, 10), label = paste0(x$iterations[1, ]))
    expect_equal(x$iterations[2, ], c(10, 10, 10, 10), label = paste0(x$iterations[2, ]))
    expect_equal(x$iterations[3, ], c(9, 10, 8, 9), label = paste0(x$iterations[3, ]))
    expect_equal(x$rejectAtLeastOne, c(0.3, 0.4, 0.7, 0.3), tolerance = 1e-07, label = paste0(x$rejectAtLeastOne))
    expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0.1, 0.2, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0.3, 0, 0.1, 0.3, 0, 0, 0.1), tolerance = 1e-07, label = paste0(unlist(as.list(x$rejectedArmsPerStage))))
    expect_equal(x$futilityStop, c(0, 0, 0, 0), label = paste0(x$futilityStop))
    expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0), label = paste0(x$futilityPerStage[1, ]))
    expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(x$futilityPerStage[2, ]))
    expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0), label = paste0(x$earlyStop[1, ]))
    expect_equal(x$earlyStop[2, ], c(0.1, 0, 0.2, 0.1), tolerance = 1e-07, label = paste0(x$earlyStop[2, ]))
    expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x$successPerStage[1, ]))
    expect_equal(x$successPerStage[2, ], c(0.1, 0, 0.2, 0.1), tolerance = 1e-07, label = paste0(x$successPerStage[2, ]))
    expect_equal(x$successPerStage[3, ], c(0.2, 0.4, 0.5, 0.2), tolerance = 1e-07, label = paste0(x$successPerStage[3, ]))
    expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.1, 0.1, 1, 0, 0, 1, 0.1, 0.1, 1, 0.2, 0.1, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.4, 0.3, 1, 0.4, 0.4, 1, 0.4, 0.3, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.5, 0.5, 1, 0.4, 0.3, 1, 0.2, 0.2), tolerance = 1e-07, label = paste0(unlist(as.list(x$selectedArms))))
    expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x$numberOfActiveArms[1, ]))
    expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1), label = paste0(x$numberOfActiveArms[2, ]))
    expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1), label = paste0(x$numberOfActiveArms[3, ]))
    expect_equal(x$expectedNumberOfEvents, c(43.7, 47, 40.4, 43.7), tolerance = 1e-07, label = paste0(x$expectedNumberOfEvents))
    expect_equal(unlist(as.list(x$singleEventsPerArmAndStage)), c(2.5384615, 0.18082192, 1.6575342, 2.4285714, 0, 0, 2.3333333, 0.18666667, 1.925, 2.25, 0.37894737, 1.7368421, 2, 0.31515152, 2.8888889, 2, 0.16470588, 1.3588235, 2, 0.17142857, 1.7678571, 2, 0.17777778, 1.6296296, 1.4615385, 0.51525424, 3.5423729, 1.5714286, 0.56774194, 4.683871, 1.6666667, 0.61538462, 4.7596154, 1.75, 0.82352941, 7.5490196, 0.92307692, 0.27692308, 2.5384615, 1.1428571, 0.57142857, 4.7142857, 1.3333333, 0.53333333, 4.125, 1.5, 0.3, 2.75, 3.0769231, 2.7118493, 22.372742, 2.8571429, 2.6961236, 22.24302, 2.6666667, 2.4931868, 20.422527, 2.5, 2.3197454, 19.334509), tolerance = 1e-07, label = paste0(unlist(as.list(x$singleEventsPerArmAndStage))))
    expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(x$conditionalPowerAchieved[1, ]))
    expect_equal(x$conditionalPowerAchieved[2, ], c(0.13227215, 0.33500952, 0.32478794, 0.19174696), tolerance = 1e-07, label = paste0(x$conditionalPowerAchieved[2, ]))
    expect_equal(x$conditionalPowerAchieved[3, ], c(0.28682503, 0.6076832, 0.60939504, 0.37477275), tolerance = 1e-07, label = paste0(x$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x), NA)))
        expect_output(print(x)$show())
        invisible(capture.output(expect_error(summary(x), NA)))
        expect_output(summary(x)$show())
        xCodeBased <- eval(parse(text = getObjectRCode(x, stringWrapParagraphWidth = NULL)))
        expect_equal(xCodeBased$cumulativeEventsPerStage, x$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(xCodeBased$iterations, x$iterations, tolerance = 1e-07)
        expect_equal(xCodeBased$rejectAtLeastOne, x$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(xCodeBased$rejectedArmsPerStage, x$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(xCodeBased$futilityStop, x$futilityStop, tolerance = 1e-07)
        expect_equal(xCodeBased$futilityPerStage, x$futilityPerStage, tolerance = 1e-07)
        expect_equal(xCodeBased$earlyStop, x$earlyStop, tolerance = 1e-07)
        expect_equal(xCodeBased$successPerStage, x$successPerStage, tolerance = 1e-07)
        expect_equal(xCodeBased$selectedArms, x$selectedArms, tolerance = 1e-07)
        expect_equal(xCodeBased$numberOfActiveArms, x$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(xCodeBased$expectedNumberOfEvents, x$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(xCodeBased$singleEventsPerArmAndStage, x$singleEventsPerArmAndStage, tolerance = 1e-07)
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

test_that("'getSimulationMultiArmSurvival': using selectArmsFunction", {
    .skipTestIfDisabled()

    # @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
    # @refFS[Sec.]{fs:sec:simulatingMultiArmDesigns}
    # @refFS[Sec.]{fs:sec:simulatingMultiArmDoseResponseRelationShips}
    # @refFS[Sec.]{fs:sec:simulatingMultiArmSelections}
    # @refFS[Tab.]{fs:tab:output:getSimulationMultiArmSurvival}
    # @refFS[Formula]{fs:simulationMultiArmDoseResponse}
    # @refFS[Formula]{fs:simulationMultiArmSurvivalCholeskyTransformation}
    # @refFS[Formula]{fs:simulationMultiArmSurvivalCorrMatrix}
    # @refFS[Formula]{fs:simulationMultiArmSurvivalEvents}
    # @refFS[Formula]{fs:simulationMultiArmSurvivalLogRanks}
    # @refFS[Formula]{fs:simulationMultiArmSelections}
    # @refFS[Formula]{fs:multiarmRejectionRule}
    # @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
    selectArmsFunctionSimulationMultiArmSurvival <- function(effectVector) {
        return(c(TRUE, FALSE, FALSE, FALSE))
    }

    x <- getSimulationMultiArmSurvival(
        seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
        typeOfShape = "linear", activeArms = 4,
        plannedEvents = c(10, 30, 50), omegaMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2), directionUpper = FALSE,
        maxNumberOfIterations = 10, selectArmsFunction = selectArmsFunctionSimulationMultiArmSurvival, typeOfSelection = "userDefined"
    )

    ## Comparison of the results of SimulationResultsMultiArmSurvival object 'x' with expected results
    expect_equal(unlist(as.list(x$cumulativeEventsPerStage)), c(5.6153846, 25.615385, 45.615385, 5.2857143, 25.285714, 45.285714, 5, 25, 45, 4.75, 24.75, 44.75, 5.0769231, 16.035827, 26.994731, 4.8571429, 15.667954, 26.478764, 4.6666667, 15.333333, 26, 4.5, 15.026316, 25.552632, 4.5384615, 15.497366, 26.45627, 4.4285714, 15.239382, 26.050193, 4.3333333, 15, 25.666667, 4.25, 14.776316, 25.302632, 4, 14.958904, 25.917808, 4, 14.810811, 25.621622, 4, 14.666667, 25.333333, 4, 14.526316, 25.052632), tolerance = 1e-07, label = paste0(unlist(as.list(x$cumulativeEventsPerStage))))
    expect_equal(x$iterations[1, ], c(10, 10, 10, 10), label = paste0(x$iterations[1, ]))
    expect_equal(x$iterations[2, ], c(10, 10, 10, 10), label = paste0(x$iterations[2, ]))
    expect_equal(x$iterations[3, ], c(10, 10, 10, 9), label = paste0(x$iterations[3, ]))
    expect_equal(x$rejectAtLeastOne, c(0, 0, 0, 0.1), tolerance = 1e-07, label = paste0(x$rejectAtLeastOne))
    expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), tolerance = 1e-07, label = paste0(unlist(as.list(x$rejectedArmsPerStage))))
    expect_equal(x$futilityStop, c(0, 0, 0, 0), label = paste0(x$futilityStop))
    expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0), label = paste0(x$futilityPerStage[1, ]))
    expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0(x$futilityPerStage[2, ]))
    expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0), label = paste0(x$earlyStop[1, ]))
    expect_equal(x$earlyStop[2, ], c(0, 0, 0, 0.1), tolerance = 1e-07, label = paste0(x$earlyStop[2, ]))
    expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x$successPerStage[1, ]))
    expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0.1), tolerance = 1e-07, label = paste0(x$successPerStage[2, ]))
    expect_equal(x$successPerStage[3, ], c(0, 0, 0, 0), label = paste0(x$successPerStage[3, ]))
    expect_equal(unlist(as.list(x$selectedArms)), c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.9, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0), tolerance = 1e-07, label = paste0(unlist(as.list(x$selectedArms))))
    expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4), label = paste0(x$numberOfActiveArms[1, ]))
    expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1), label = paste0(x$numberOfActiveArms[2, ]))
    expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1), label = paste0(x$numberOfActiveArms[3, ]))
    expect_equal(x$expectedNumberOfEvents, c(50, 50, 50, 48), label = paste0(x$expectedNumberOfEvents))
    expect_equal(unlist(as.list(x$singleEventsPerArmAndStage)), c(2.5384615, 9.0410959, 9.0410959, 2.4285714, 9.1891892, 9.1891892, 2.3333333, 9.3333333, 9.3333333, 2.25, 9.4736842, 9.4736842, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 1.4615385, 0, 0, 1.5714286, 0, 0, 1.6666667, 0, 0, 1.75, 0, 0, 0.92307692, 0, 0, 1.1428571, 0, 0, 1.3333333, 0, 0, 1.5, 0, 0, 3.0769231, 10.958904, 10.958904, 2.8571429, 10.810811, 10.810811, 2.6666667, 10.666667, 10.666667, 2.5, 10.526316, 10.526316), tolerance = 1e-07, label = paste0(unlist(as.list(x$singleEventsPerArmAndStage))))
    expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(x$conditionalPowerAchieved[1, ]))
    expect_equal(x$conditionalPowerAchieved[2, ], c(0.33564601, 0.59192905, 0.61161484, 0.44432847), tolerance = 1e-07, label = paste0(x$conditionalPowerAchieved[2, ]))
    expect_equal(x$conditionalPowerAchieved[3, ], c(0.10158651, 0.080642472, 0.3234231, 0.034914809), tolerance = 1e-07, label = paste0(x$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x), NA)))
        expect_output(print(x)$show())
        invisible(capture.output(expect_error(summary(x), NA)))
        expect_output(summary(x)$show())
        xCodeBased <- eval(parse(text = getObjectRCode(x, stringWrapParagraphWidth = NULL)))
        expect_equal(xCodeBased$cumulativeEventsPerStage, x$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(xCodeBased$iterations, x$iterations, tolerance = 1e-07)
        expect_equal(xCodeBased$rejectAtLeastOne, x$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(xCodeBased$rejectedArmsPerStage, x$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(xCodeBased$futilityStop, x$futilityStop, tolerance = 1e-07)
        expect_equal(xCodeBased$futilityPerStage, x$futilityPerStage, tolerance = 1e-07)
        expect_equal(xCodeBased$earlyStop, x$earlyStop, tolerance = 1e-07)
        expect_equal(xCodeBased$successPerStage, x$successPerStage, tolerance = 1e-07)
        expect_equal(xCodeBased$selectedArms, x$selectedArms, tolerance = 1e-07)
        expect_equal(xCodeBased$numberOfActiveArms, x$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(xCodeBased$expectedNumberOfEvents, x$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(xCodeBased$singleEventsPerArmAndStage, x$singleEventsPerArmAndStage, tolerance = 1e-07)
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

test_that("'getSimulationMultiArmSurvival': typeOfShape = sigmoidEmax", {
    .skipTestIfDisabled()

    # @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
    # @refFS[Sec.]{fs:sec:simulatingMultiArmDesigns}
    # @refFS[Sec.]{fs:sec:simulatingMultiArmDoseResponseRelationShips}
    # @refFS[Sec.]{fs:sec:simulatingMultiArmSelections}
    # @refFS[Tab.]{fs:tab:output:getSimulationMultiArmSurvival}
    # @refFS[Formula]{fs:simulationMultiArmDoseResponse}
    # @refFS[Formula]{fs:simulationMultiArmSurvivalCholeskyTransformation}
    # @refFS[Formula]{fs:simulationMultiArmSurvivalCorrMatrix}
    # @refFS[Formula]{fs:simulationMultiArmSurvivalEvents}
    # @refFS[Formula]{fs:simulationMultiArmSurvivalLogRanks}
    # @refFS[Formula]{fs:simulationMultiArmSelections}
    # @refFS[Formula]{fs:multiarmRejectionRule}
    # @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
    designIN <- getDesignInverseNormal(typeOfDesign = "P", kMax = 3, futilityBounds = c(0, 0))
    x <- getSimulationMultiArmSurvival(designIN,
        activeArms = 3, typeOfShape = "sigmoidEmax",
        omegaMaxVector = seq(1, 1.9, 0.3), gED50 = 2, plannedEvents = cumsum(rep(50, 3)),
        intersectionTest = "Sidak", typeOfSelection = "rBest", rValue = 2, threshold = -Inf,
        successCriterion = "all", maxNumberOfIterations = 100, seed = 3456
    )

    ## Comparison of the results of SimulationResultsMultiArmSurvival object 'x' with expected results
    expect_equal(unlist(as.list(x$cumulativeEventsPerStage)), c(25, 54.166667, 83.179012, 23.702032, 48.059626, 73.088162, 22.633745, 47.376736, 72.759392, 21.73913, 42.12314, 62.760088, 25, 52.5, 80.895062, 24.266366, 53.226501, 81.514068, 23.662551, 48.016442, 72.194538, 23.1569, 49.755556, 76.303771, 25, 51.666667, 77.592593, 24.604966, 51.66004, 78.734811, 24.279835, 53.095902, 81.487679, 24.007561, 52.639961, 81.090004), tolerance = 1e-07, label = paste0(unlist(as.list(x$cumulativeEventsPerStage))))
    expect_equal(x$iterations[1, ], c(100, 100, 100, 100), label = paste0(x$iterations[1, ]))
    expect_equal(x$iterations[2, ], c(40, 57, 66, 79), label = paste0(x$iterations[2, ]))
    expect_equal(x$iterations[3, ], c(27, 48, 55, 70), label = paste0(x$iterations[3, ]))
    expect_equal(x$rejectAtLeastOne, c(0.02, 0.07, 0.19, 0.21), tolerance = 1e-07, label = paste0(x$rejectAtLeastOne))
    expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0.01, 0.01, 0.01, 0.02, 0.01, 0.02, 0, 0.02, 0.02, 0.01, 0, 0, 0, 0.01, 0.02, 0.03, 0.03, 0.01, 0.03, 0.06, 0.01, 0.01, 0, 0, 0.01, 0.01, 0.02, 0.04, 0.03, 0.07, 0.03, 0.09, 0.06), tolerance = 1e-07, label = paste0(unlist(as.list(x$rejectedArmsPerStage))))
    expect_equal(x$futilityStop, c(0.73, 0.51, 0.41, 0.24), tolerance = 1e-07, label = paste0(x$futilityStop))
    expect_equal(x$futilityPerStage[1, ], c(0.6, 0.43, 0.34, 0.21), tolerance = 1e-07, label = paste0(x$futilityPerStage[1, ]))
    expect_equal(x$futilityPerStage[2, ], c(0.13, 0.08, 0.07, 0.03), tolerance = 1e-07, label = paste0(x$futilityPerStage[2, ]))
    expect_equal(x$earlyStop[1, ], c(0.6, 0.43, 0.34, 0.21), tolerance = 1e-07, label = paste0(x$earlyStop[1, ]))
    expect_equal(x$earlyStop[2, ], c(0.13, 0.09, 0.11, 0.09), tolerance = 1e-07, label = paste0(x$earlyStop[2, ]))
    expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0), label = paste0(x$successPerStage[1, ]))
    expect_equal(x$successPerStage[2, ], c(0, 0.01, 0.04, 0.06), tolerance = 1e-07, label = paste0(x$successPerStage[2, ]))
    expect_equal(x$successPerStage[3, ], c(0, 0.02, 0.03, 0.05), tolerance = 1e-07, label = paste0(x$successPerStage[3, ]))
    expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.3, 0.2, 1, 0.31, 0.28, 1, 0.42, 0.37, 1, 0.35, 0.32, 1, 0.26, 0.19, 1, 0.45, 0.36, 1, 0.38, 0.31, 1, 0.59, 0.52, 1, 0.24, 0.15, 1, 0.38, 0.32, 1, 0.52, 0.42, 1, 0.64, 0.56), tolerance = 1e-07, label = paste0(unlist(as.list(x$selectedArms))))
    expect_equal(x$numberOfActiveArms[1, ], c(3, 3, 3, 3), label = paste0(x$numberOfActiveArms[1, ]))
    expect_equal(x$numberOfActiveArms[2, ], c(2, 2, 2, 2), label = paste0(x$numberOfActiveArms[2, ]))
    expect_equal(x$numberOfActiveArms[3, ], c(2, 2, 2, 2), label = paste0(x$numberOfActiveArms[3, ]))
    expect_equal(x$expectedNumberOfEvents, c(83.5, 102.5, 110.5, 124.5), tolerance = 1e-07, label = paste0(x$expectedNumberOfEvents))
    expect_equal(unlist(as.list(x$singleEventsPerArmAndStage)), c(12.5, 12.5, 12.345679, 12.41535, 9.1711925, 9.8330988, 12.345679, 10.786517, 11.406391, 12.287335, 7.5764768, 7.8193452, 12.5, 10.833333, 11.728395, 12.979684, 13.773733, 13.092131, 13.374486, 10.397417, 10.201831, 13.705104, 13.791123, 13.730612, 12.5, 10, 9.2592593, 13.318284, 11.868672, 11.879334, 13.99177, 14.859592, 14.415513, 14.555766, 15.824867, 15.63244, 12.5, 16.666667, 16.666667, 11.286682, 15.186402, 15.195437, 10.288066, 13.956474, 13.976265, 9.4517958, 12.807533, 12.817602), tolerance = 1e-07, label = paste0(unlist(as.list(x$singleEventsPerArmAndStage))))
    expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(x$conditionalPowerAchieved[1, ]))
    expect_equal(x$conditionalPowerAchieved[2, ], c(0.066083689, 0.14406787, 0.27240426, 0.24161087), tolerance = 1e-07, label = paste0(x$conditionalPowerAchieved[2, ]))
    expect_equal(x$conditionalPowerAchieved[3, ], c(0.13321164, 0.19096794, 0.29528894, 0.30979546), tolerance = 1e-07, label = paste0(x$conditionalPowerAchieved[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x), NA)))
        expect_output(print(x)$show())
        invisible(capture.output(expect_error(summary(x), NA)))
        expect_output(summary(x)$show())
        xCodeBased <- eval(parse(text = getObjectRCode(x, stringWrapParagraphWidth = NULL)))
        expect_equal(xCodeBased$cumulativeEventsPerStage, x$cumulativeEventsPerStage, tolerance = 1e-07)
        expect_equal(xCodeBased$iterations, x$iterations, tolerance = 1e-07)
        expect_equal(xCodeBased$rejectAtLeastOne, x$rejectAtLeastOne, tolerance = 1e-07)
        expect_equal(xCodeBased$rejectedArmsPerStage, x$rejectedArmsPerStage, tolerance = 1e-07)
        expect_equal(xCodeBased$futilityStop, x$futilityStop, tolerance = 1e-07)
        expect_equal(xCodeBased$futilityPerStage, x$futilityPerStage, tolerance = 1e-07)
        expect_equal(xCodeBased$earlyStop, x$earlyStop, tolerance = 1e-07)
        expect_equal(xCodeBased$successPerStage, x$successPerStage, tolerance = 1e-07)
        expect_equal(xCodeBased$selectedArms, x$selectedArms, tolerance = 1e-07)
        expect_equal(xCodeBased$numberOfActiveArms, x$numberOfActiveArms, tolerance = 1e-07)
        expect_equal(xCodeBased$expectedNumberOfEvents, x$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(xCodeBased$singleEventsPerArmAndStage, x$singleEventsPerArmAndStage, tolerance = 1e-07)
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

test_that("'getSimulationMultiArmSurvival': comparison of base and multi-arm, inverse normal design", {
    .skipTestIfDisabled()

    # @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
    # @refFS[Sec.]{fs:sec:simulatingMultiArmDesigns}
    # @refFS[Sec.]{fs:sec:simulatingMultiArmDoseResponseRelationShips}
    # @refFS[Sec.]{fs:sec:simulatingMultiArmSelections}
    # @refFS[Tab.]{fs:tab:output:getSimulationMultiArmSurvival}
    # @refFS[Formula]{fs:simulationMultiArmDoseResponse}
    # @refFS[Formula]{fs:simulationMultiArmSurvivalCholeskyTransformation}
    # @refFS[Formula]{fs:simulationMultiArmSurvivalCorrMatrix}
    # @refFS[Formula]{fs:simulationMultiArmSurvivalEvents}
    # @refFS[Formula]{fs:simulationMultiArmSurvivalLogRanks}
    # @refFS[Formula]{fs:simulationMultiArmSelections}
    # @refFS[Formula]{fs:multiarmRejectionRule}
    allocationRatioPlanned <- 1
    design <- getDesignInverseNormal(
        typeOfDesign = "WT", deltaWT = 0.05,
        futilityBounds = c(-0.5, 0.5), informationRates = c(0.2, 0.8, 1)
    )

    x <- getSimulationMultiArmSurvival(design,
        activeArms = 1, omegaMaxVector = 1 / seq(1, 1.8, 0.4), plannedEvents = c(20, 40, 60),
        conditionalPower = 0.99, maxNumberOfEventsPerStage = c(NA, 100, 100), minNumberOfEventsPerStage = c(NA, 10, 10),
        maxNumberOfIterations = 100, directionUpper = FALSE, allocationRatioPlanned = allocationRatioPlanned, seed = 1234
    )

    y <- getSimulationSurvival(design,
        pi2 = 0.2, hazardRatio = 1 / seq(1, 1.8, 0.4),
        plannedEvents = c(20, 40, 60), maxNumberOfSubjects = 500,
        conditionalPower = 0.99, maxNumberOfEventsPerStage = c(NA, 100, 100), minNumberOfEventsPerStage = c(NA, 10, 10),
        maxNumberOfIterations = 100, directionUpper = FALSE, allocation1 = 1, allocation2 = 1, seed = 1234
    )

    comp1 <- y$overallReject - x$rejectAtLeastOne

    ## Comparison of the results of numeric object 'comp1' with expected results
    expect_equal(comp1, c(-0.02, 0.01, 0.06), tolerance = 1e-07, label = paste0(comp1))

    comp2 <- y$rejectPerStage - x$rejectedArmsPerStage[, , 1]

    ## Comparison of the results of matrix object 'comp2' with expected results
    expect_equal(comp2[1, ], c(0, 0, 0), label = paste0(comp2[1, ]))
    expect_equal(comp2[2, ], c(-0.02, 0.02, 0.03), tolerance = 1e-07, label = paste0(comp2[2, ]))
    expect_equal(comp2[3, ], c(0, -0.01, 0.03), tolerance = 1e-07, label = paste0(comp2[3, ]))

    comp3 <- y$futilityPerStage - x$futilityPerStage

    ## Comparison of the results of matrix object 'comp3' with expected results
    expect_equal(comp3[1, ], c(-0.06, -0.02, -0.03), tolerance = 1e-07, label = paste0(comp3[1, ]))
    expect_equal(comp3[2, ], c(0.08, 0.06, 0), tolerance = 1e-07, label = paste0(comp3[2, ]))

    comp4 <- round(y$cumulativeEventsPerStage - x$cumulativeEventsPerStage[, , 1], 1)

    ## Comparison of the results of matrix object 'comp4' with expected results
    expect_equal(comp4[1, ], c(0, 0, 0), label = paste0(comp4[1, ]))
    expect_equal(comp4[2, ], c(1.2, -0.4, 1), tolerance = 1e-07, label = paste0(comp4[2, ]))
    expect_equal(comp4[3, ], c(1.7, -0.8, 1), tolerance = 1e-07, label = paste0(comp4[3, ]))

    comp5 <- round(y$expectedNumberOfEvents - x$expectedNumberOfEvents, 1)

    ## Comparison of the results of numeric object 'comp5' with expected results
    expect_equal(comp5, c(6.9, -4.7, 3.6), tolerance = 1e-07, label = paste0(comp5))

    comp6 <- x$earlyStop - y$earlyStop

    ## Comparison of the results of matrix object 'comp6' with expected results
    expect_equal(comp6[1, ], c(-0.43, -0.73, -0.52), tolerance = 1e-07, label = paste0(comp6[1, ]))
    expect_equal(comp6[2, ], c(-0.13, -0.32, -0.04), tolerance = 1e-07, label = paste0(comp6[2, ]))
})

test_that("'getSimulationMultiArmSurvival': comparison of base and multi-arm, Fisher design", {
    .skipTestIfDisabled()

    # @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
    # @refFS[Sec.]{fs:sec:simulatingMultiArmDesigns}
    # @refFS[Sec.]{fs:sec:simulatingMultiArmDoseResponseRelationShips}
    # @refFS[Sec.]{fs:sec:simulatingMultiArmSelections}
    # @refFS[Tab.]{fs:tab:output:getSimulationMultiArmSurvival}
    # @refFS[Formula]{fs:simulationMultiArmDoseResponse}
    # @refFS[Formula]{fs:simulationMultiArmSurvivalCholeskyTransformation}
    # @refFS[Formula]{fs:simulationMultiArmSurvivalCorrMatrix}
    # @refFS[Formula]{fs:simulationMultiArmSurvivalEvents}
    # @refFS[Formula]{fs:simulationMultiArmSurvivalLogRanks}
    # @refFS[Formula]{fs:simulationMultiArmSelections}
    # @refFS[Formula]{fs:multiarmRejectionRule}
    design <- getDesignFisher(alpha0Vec = c(0.6, 0.4), informationRates = c(0.5, 0.6, 1))

    x <- getSimulationMultiArmSurvival(design,
        activeArms = 1, omegaMaxVector = 1 / seq(1, 1.8, 0.4),
        plannedEvents = c(20, 40, 60),
        conditionalPower = 0.99, maxNumberOfEventsPerStage = c(NA, 100, 100), minNumberOfEventsPerStage = c(NA, 10, 10),
        maxNumberOfIterations = 100, directionUpper = FALSE, seed = 1234
    )

    y <- getSimulationSurvival(design,
        pi2 = 0.2, hazardRatio = 1 / seq(1, 1.8, 0.4),
        plannedEvents = c(20, 40, 60), maxNumberOfSubjects = 500,
        conditionalPower = 0.99, maxNumberOfEventsPerStage = c(NA, 100, 100), minNumberOfEventsPerStage = c(NA, 10, 10),
        maxNumberOfIterations = 100, directionUpper = FALSE, allocation1 = 1, allocation2 = 1, seed = 1234
    )

    comp1 <- y$overallReject - x$rejectAtLeastOne

    ## Comparison of the results of numeric object 'comp1' with expected results
    expect_equal(comp1, c(-0.02, -0.01, 0.02), tolerance = 1e-07, label = paste0(comp1))

    comp2 <- y$rejectPerStage - x$rejectedArmsPerStage[, , 1]

    ## Comparison of the results of matrix object 'comp2' with expected results
    expect_equal(comp2[1, ], c(-0.02, 0.01, -0.01), tolerance = 1e-07, label = paste0(comp2[1, ]))
    expect_equal(comp2[2, ], c(0, -0.03, 0.01), tolerance = 1e-07, label = paste0(comp2[2, ]))
    expect_equal(comp2[3, ], c(0, 0.01, 0.02), tolerance = 1e-07, label = paste0(comp2[3, ]))

    comp3 <- y$futilityPerStage - x$futilityPerStage

    ## Comparison of the results of matrix object 'comp3' with expected results
    expect_equal(comp3[1, ], c(-0.03, 0.01, -0.01), tolerance = 1e-07, label = paste0(comp3[1, ]))
    expect_equal(comp3[2, ], c(0.05, 0.05, -0.01), tolerance = 1e-07, label = paste0(comp3[2, ]))

    comp4 <- round(y$cumulativeEventsPerStage - x$cumulativeEventsPerStage[, , 1], 1)

    ## Comparison of the results of matrix object 'comp4' with expected results
    expect_equal(comp4[1, ], c(0, 0, 0), label = paste0(comp4[1, ]))
    expect_equal(comp4[2, ], c(-0.6, 0.8, -0.3), tolerance = 1e-07, label = paste0(comp4[2, ]))
    expect_equal(comp4[3, ], c(-0.6, 0.8, -0.3), tolerance = 1e-07, label = paste0(comp4[3, ]))

    comp5 <- round(y$expectedNumberOfEvents - x$expectedNumberOfEvents, 1)

    ## Comparison of the results of numeric object 'comp5' with expected results
    expect_equal(comp5, c(4.7, -5.3, 3.6), tolerance = 1e-07, label = paste0(comp5))

    comp6 <- x$earlyStop - y$earlyStop

    ## Comparison of the results of matrix object 'comp6' with expected results
    expect_equal(comp6[1, ], c(-0.27, -0.42, -0.29), tolerance = 1e-07, label = paste0(comp6[1, ]))
    expect_equal(comp6[2, ], c(-0.22, -0.54, -0.18), tolerance = 1e-07, label = paste0(comp6[2, ]))
})

test_that("'getSimulationMultiArmSurvival': comparison of base and multi-arm, inverse normal design with user alpha spending", {
    .skipTestIfDisabled()

    # @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
    # @refFS[Sec.]{fs:sec:simulatingMultiArmDesigns}
    # @refFS[Sec.]{fs:sec:simulatingMultiArmDoseResponseRelationShips}
    # @refFS[Sec.]{fs:sec:simulatingMultiArmSelections}
    # @refFS[Tab.]{fs:tab:output:getSimulationMultiArmSurvival}
    # @refFS[Formula]{fs:simulationMultiArmDoseResponse}
    # @refFS[Formula]{fs:simulationMultiArmSurvivalCholeskyTransformation}
    # @refFS[Formula]{fs:simulationMultiArmSurvivalCorrMatrix}
    # @refFS[Formula]{fs:simulationMultiArmSurvivalEvents}
    # @refFS[Formula]{fs:simulationMultiArmSurvivalLogRanks}
    # @refFS[Formula]{fs:simulationMultiArmSelections}
    # @refFS[Formula]{fs:multiarmRejectionRule}
    design <- getDesignInverseNormal(
        typeOfDesign = "asUser",
        userAlphaSpending = c(0, 0, 0.025), informationRates = c(0.2, 0.8, 1)
    )

    x <- getSimulationMultiArmSurvival(design,
        activeArms = 1, omegaMaxVector = 1 / seq(1, 1.8, 0.4), plannedEvents = c(20, 40, 60),
        conditionalPower = 0.99, maxNumberOfEventsPerStage = c(NA, 100, 100), minNumberOfEventsPerStage = c(NA, 10, 10),
        maxNumberOfIterations = 100, directionUpper = FALSE, seed = 1234
    )

    y <- getSimulationSurvival(design,
        pi2 = 0.2, hazardRatio = 1 / seq(1, 1.8, 0.4),
        plannedEvents = c(20, 40, 60), maxNumberOfSubjects = 500,
        conditionalPower = 0.99, maxNumberOfEventsPerStage = c(NA, 100, 100), minNumberOfEventsPerStage = c(NA, 10, 10),
        maxNumberOfIterations = 100, directionUpper = FALSE, allocation1 = 1, allocation2 = 1, seed = 1234
    )

    comp1 <- y$overallReject - x$rejectAtLeastOne

    ## Comparison of the results of numeric object 'comp1' with expected results
    expect_equal(comp1, c(-0.01, 0.02, 0.01), tolerance = 1e-07, label = paste0(comp1))

    comp2 <- y$rejectPerStage - x$rejectedArmsPerStage[, , 1]

    ## Comparison of the results of matrix object 'comp2' with expected results
    expect_equal(comp2[1, ], c(0, 0, 0), label = paste0(comp2[1, ]))
    expect_equal(comp2[2, ], c(0, 0, 0), label = paste0(comp2[2, ]))
    expect_equal(comp2[3, ], c(-0.01, 0.02, 0.01), tolerance = 1e-07, label = paste0(comp2[3, ]))

    comp3 <- y$futilityPerStage - x$futilityPerStage

    ## Comparison of the results of matrix object 'comp3' with expected results
    expect_equal(comp3[1, ], c(0, 0, 0), label = paste0(comp3[1, ]))
    expect_equal(comp3[2, ], c(0, 0, 0), label = paste0(comp3[2, ]))

    comp4 <- round(y$cumulativeEventsPerStage - x$cumulativeEventsPerStage[, , 1], 1)

    ## Comparison of the results of matrix object 'comp4' with expected results
    expect_equal(comp4[1, ], c(0, 0, 0), label = paste0(comp4[1, ]))
    expect_equal(comp4[2, ], c(0, 0, 0), label = paste0(comp4[2, ]))
    expect_equal(comp4[3, ], c(-0.2, -3.5, 0.6), tolerance = 1e-07, label = paste0(comp4[3, ]))

    comp5 <- round(y$expectedNumberOfEvents - x$expectedNumberOfEvents, 1)

    ## Comparison of the results of numeric object 'comp5' with expected results
    expect_equal(comp5, c(-0.2, -3.5, 0.6), tolerance = 1e-07, label = paste0(comp5))

    comp6 <- x$earlyStop - y$earlyStop

    ## Comparison of the results of matrix object 'comp6' with expected results
    expect_equal(comp6[1, ], c(0, 0, 0), label = paste0(comp6[1, ]))
    expect_equal(comp6[2, ], c(0, 0, 0), label = paste0(comp6[2, ]))
})
