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
## |  File name: test-f_simulation_base_count_data.R
## |  Creation date: 23 May 2024, 10:12:16
## |  File version: $Revision$
## |  Last changed: $Date$
## |  Last changed by: $Author$
## |

test_plan_section("Testing Simulation Counts Function")


test_that("'getSimulationCounts': variable exposure", {
    .skipTestIfDisabled()

    design <- getDesignGroupSequential(
        informationRates = c(0.3, 0.55, 1),
        alpha = 0.025, beta = 0.2, typeOfDesign = "asOF",
        typeBetaSpending = "bsOF", bindingFutility = TRUE
    )

    suppressWarnings(result1 <- getSimulationCounts(
        design = design,
        directionUpper = FALSE,
        plannedMaxSubjects = 110,
        plannedCalendarTime = c(4.886914, 7.878929, 14),
        lambda1 = c(0.7, 0.3),
        lambda2 = 0.7,
        overdispersion = 2,
        maxNumberOfIterations = 100,
        accrualTime = 12,
        followUpTime = 2,
        seed = 4378628
    ))

    ## Comparison of the results of SimulationResultsBaseCountDataSimulationResultsParameterSetFieldSetR6 object 'result1' with expected results
    expect_equal(result1$theta, c(1, 0.42857143), tolerance = 1e-07, label = paste0(result1$theta))
    expect_equal(result1$iterations[1, ], c(100, 100), label = paste0(result1$iterations[1, ]))
    expect_equal(result1$iterations[2, ], c(74, 95), label = paste0(result1$iterations[2, ]))
    expect_equal(result1$iterations[3, ], c(19, 72), label = paste0(result1$iterations[3, ]))
    expect_equal(result1$overallReject, c(0.04, 0.78), tolerance = 1e-07, label = paste0(result1$overallReject))
    expect_equal(result1$rejectPerStage[1, ], c(0, 0.01), tolerance = 1e-07, label = paste0(result1$rejectPerStage[1, ]))
    expect_equal(result1$rejectPerStage[2, ], c(0, 0.17), tolerance = 1e-07, label = paste0(result1$rejectPerStage[2, ]))
    expect_equal(result1$rejectPerStage[3, ], c(0.04, 0.6), tolerance = 1e-07, label = paste0(result1$rejectPerStage[3, ]))
    expect_equal(result1$futilityStop, c(0.81, 0.1), tolerance = 1e-07, label = paste0(result1$futilityStop))
    expect_equal(result1$futilityPerStage[1, ], c(0.26, 0.04), tolerance = 1e-07, label = paste0(result1$futilityPerStage[1, ]))
    expect_equal(result1$futilityPerStage[2, ], c(0.55, 0.06), tolerance = 1e-07, label = paste0(result1$futilityPerStage[2, ]))
    expect_equal(result1$earlyStop[1, ], c(0.81, 0.28), tolerance = 1e-07, label = paste0(result1$earlyStop[1, ]))

    suppressWarnings(result2 <- getSimulationCounts(
        getDesignGroupSequential(
            alpha = 0.025,
            futilityBounds = c(-0.5, 0.5)
        ),
        lambda1 = seq(1, 1.4, 0.1),
        lambda2 = 1.4,
        overdispersion = 1.1,
        plannedCalendarTime = c(4, 5, 6),
        allocationRatioPlanned = 3,
        accrualTime = c(4),
        accrualIntensity = c(100),
        maxNumberOfIterations = 100,
        followUpTime = 3,
        directionUpper = FALSE,
        seed = 4378628
    ))


    ## Comparison of the results of SimulationResultsBaseCountDataSimulationResultsParameterSetFieldSetR6 object 'result2' with expected results
    expect_equal(result2$theta, c(0.71428571, 0.78571429, 0.85714286, 0.92857143, 1), tolerance = 1e-07, label = paste0(result2$theta))
    expect_equal(result2$numberOfSubjects, 400, label = paste0(result2$numberOfSubjects))
    expect_equal(result2$numberOfSubjects1, 300, label = paste0(result2$numberOfSubjects1))
    expect_equal(result2$numberOfSubjects2, 100, label = paste0(result2$numberOfSubjects2))
    expect_equal(result2$iterations[1, ], c(100, 100, 100, 100, 100), label = paste0(result2$iterations[1, ]))
    expect_equal(result2$iterations[2, ], c(86, 94, 96, 86, 69), label = paste0(result2$iterations[2, ]))
    expect_equal(result2$iterations[3, ], c(40, 53, 72, 50, 25), label = paste0(result2$iterations[3, ]))
    expect_equal(result2$overallReject, c(0.77, 0.47, 0.26, 0.08, 0.01), tolerance = 1e-07, label = paste0(result2$overallReject))
    expect_equal(result2$rejectPerStage[1, ], c(0.14, 0.05, 0.02, 0, 0), tolerance = 1e-07, label = paste0(result2$rejectPerStage[1, ]))
    expect_equal(result2$rejectPerStage[2, ], c(0.44, 0.28, 0.09, 0.02, 0.01), tolerance = 1e-07, label = paste0(result2$rejectPerStage[2, ]))
    expect_equal(result2$rejectPerStage[3, ], c(0.19, 0.14, 0.15, 0.06, 0), tolerance = 1e-07, label = paste0(result2$rejectPerStage[3, ]))
    expect_equal(result2$futilityStop, c(0.02, 0.14, 0.17, 0.48, 0.74), tolerance = 1e-07, label = paste0(result2$futilityStop))
    expect_equal(result2$futilityPerStage[1, ], c(0, 0.01, 0.02, 0.14, 0.31), tolerance = 1e-07, label = paste0(result2$futilityPerStage[1, ]))
    expect_equal(result2$futilityPerStage[2, ], c(0.02, 0.13, 0.15, 0.34, 0.43), tolerance = 1e-07, label = paste0(result2$futilityPerStage[2, ]))
    expect_equal(result2$earlyStop[1, ], c(0.6, 0.47, 0.28, 0.5, 0.75), tolerance = 1e-07, label = paste0(result2$earlyStop[1, ]))
})

test_that("'getSimulationCounts': fixed exposure", {
    design <- getDesignGroupSequential(
        informationRates = c(0.3, 0.55, 1),
        alpha = 0.025, beta = 0.2, typeOfDesign = "asOF",
        typeBetaSpending = "bsOF", bindingFutility = TRUE
    )

    suppressWarnings(result1 <- getSimulationCounts(
        design = design,
        directionUpper = FALSE,
        plannedMaxSubjects = 110,
        plannedCalendarTime = c(4.886914, 7.878929, 14),
        lambda1 = c(0.7, 0.3),
        lambda2 = 0.7,
        overdispersion = 2,
        accrualTime = 12,
        fixedExposureTime = 1,
        maxNumberOfIterations = 100,
        seed = 4378628
    ))


    ## Comparison of the results of SimulationResultsBaseCountDataSimulationResultsParameterSetFieldSetR6 object 'result1' with expected results
    expect_equal(result1$theta, c(1, 0.42857143), tolerance = 1e-07, label = paste0(result1$theta))
    expect_equal(result1$iterations[1, ], c(100, 100), label = paste0(result1$iterations[1, ]))
    expect_equal(result1$iterations[2, ], c(62, 97), label = paste0(result1$iterations[2, ]))
    expect_equal(result1$iterations[3, ], c(27, 69), label = paste0(result1$iterations[3, ]))
    expect_equal(result1$overallReject, c(0.02, 0.55), tolerance = 1e-07, label = paste0(result1$overallReject))
    expect_equal(result1$rejectPerStage[1, ], c(0, 0), label = paste0(result1$rejectPerStage[1, ]))
    expect_equal(result1$rejectPerStage[2, ], c(0, 0.08), tolerance = 1e-07, label = paste0(result1$rejectPerStage[2, ]))
    expect_equal(result1$rejectPerStage[3, ], c(0.02, 0.47), tolerance = 1e-07, label = paste0(result1$rejectPerStage[3, ]))
    expect_equal(result1$futilityStop, c(0.73, 0.23), tolerance = 1e-07, label = paste0(result1$futilityStop))
    expect_equal(result1$futilityPerStage[1, ], c(0.38, 0.03), tolerance = 1e-07, label = paste0(result1$futilityPerStage[1, ]))
    expect_equal(result1$futilityPerStage[2, ], c(0.35, 0.2), tolerance = 1e-07, label = paste0(result1$futilityPerStage[2, ]))
    expect_equal(result1$earlyStop[1, ], c(0.73, 0.31), tolerance = 1e-07, label = paste0(result1$earlyStop[1, ]))

    suppressWarnings(result2 <- getSimulationCounts(
        maxNumberOfSubjects = 1000,
        plannedMaxSubjects = 110,
        plannedCalendarTime = as.matrix(10),
        accrualTime = 5,
        directionUpper = FALSE,
        lambda2 = 1.4,
        theta = c(0.75, 1.133333),
        overdispersion = 1.5,
        fixedExposureTime = 2,
        maxNumberOfIterations = 100,
        seed = 4378628
    ))

    ## Comparison of the results of SimulationResultsBaseCountDataSimulationResultsParameterSetFieldSetR6 object 'result2' with expected results
    expect_equal(result2$lambda1, c(1.05, 1.5866662), tolerance = 1e-07, label = paste0(result2$lambda1))
    expect_equal(result2$iterations[1, ], c(100, 100), label = paste0(result2$iterations[1, ]))
    expect_equal(result2$overallReject, c(0.14, 0), tolerance = 1e-07, label = paste0(result2$overallReject))

    suppressWarnings(result3 <- getSimulationCounts(
        getDesignGroupSequential(kMax = 3, futilityBounds = c(0, 0)),
        plannedMaxSubjects = 100,
        plannedCalendarTime = as.matrix(c(2, 4, 6)),
        maxNumberOfIterations = 100,
        fixedExposureTime = 1,
        accrualTime = 6,
        lambda1 = seq(1.05, 1.35, 0.05),
        lambda2 = 1.4,
        seed = 4378628
    ))

    ## Comparison of the results of SimulationResultsBaseCountDataSimulationResultsParameterSetFieldSetR6 object 'result3' with expected results
    expect_equal(result3$theta, c(0.75, 0.78571429, 0.82142857, 0.85714286, 0.89285714, 0.92857143, 0.96428571), tolerance = 1e-07, label = paste0(result3$theta))
    expect_equal(result3$iterations[1, ], c(100, 100, 100, 100, 100, 100, 100), label = paste0(result3$iterations[1, ]))
    expect_equal(result3$iterations[2, ], c(25, 32, 34, 38, 34, 47, 56), label = paste0(result3$iterations[2, ]))
    expect_equal(result3$iterations[3, ], c(6, 11, 17, 18, 20, 31, 35), label = paste0(result3$iterations[3, ]))
    expect_equal(result3$overallReject, c(0, 0, 0.01, 0, 0, 0.01, 0), tolerance = 1e-07, label = paste0(result3$overallReject))
    expect_equal(result3$rejectPerStage[1, ], c(0, 0, 0, 0, 0, 0, 0), label = paste0(result3$rejectPerStage[1, ]))
    expect_equal(result3$rejectPerStage[2, ], c(0, 0, 0.01, 0, 0, 0, 0), tolerance = 1e-07, label = paste0(result3$rejectPerStage[2, ]))
    expect_equal(result3$rejectPerStage[3, ], c(0, 0, 0, 0, 0, 0.01, 0), tolerance = 1e-07, label = paste0(result3$rejectPerStage[3, ]))
    expect_equal(result3$futilityStop, c(0.94, 0.89, 0.82, 0.82, 0.8, 0.69, 0.65), tolerance = 1e-07, label = paste0(result3$futilityStop))
    expect_equal(result3$futilityPerStage[1, ], c(0.75, 0.68, 0.66, 0.62, 0.66, 0.53, 0.44), tolerance = 1e-07, label = paste0(result3$futilityPerStage[1, ]))
    expect_equal(result3$futilityPerStage[2, ], c(0.19, 0.21, 0.16, 0.2, 0.14, 0.16, 0.21), tolerance = 1e-07, label = paste0(result3$futilityPerStage[2, ]))
    expect_equal(result3$earlyStop[1, ], c(0.94, 0.89, 0.83, 0.82, 0.8, 0.69, 0.65), tolerance = 1e-07, label = paste0(result3$earlyStop[1, ]))
})
