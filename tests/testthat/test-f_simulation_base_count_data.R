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
## |  Creation date: 19 September 2024, 07:20:21
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

    design2 <- getDesignGroupSequential(
        informationRates = c(0.3, 0.55, 1),
        alpha = 0.025, beta = 0.2, typeOfDesign = "asOF",
        typeBetaSpending = "bsOF", bindingFutility = TRUE,
        directionUpper = FALSE
    )

    suppressWarnings(result1 <- getSimulationCounts(
        design = design,
        directionUpper = FALSE,
        maxNumberOfSubjects = 110,
        plannedCalendarTime = c(4.886914, 7.878929, 14),
        lambda1 = c(0.7, 0.3),
        lambda2 = 0.7,
        overdispersion = 2,
        maxNumberOfIterations = 100,
        accrualTime = 12,
        followUpTime = 2,
        seed = 4378628
    ))

    ## Comparison of the results of SimulationResultsCountData object 'result1' with expected results
    expect_equal(result1$theta, c(1, 0.42857143), tolerance = 1e-07, label = paste0(result1$theta))
    expect_equal(result1$numberOfSubjects[1, ], c(44, 44), label = paste0(result1$numberOfSubjects[1, ]))
    expect_equal(result1$numberOfSubjects[2, ], c(72, 72), label = paste0(result1$numberOfSubjects[2, ]))
    expect_equal(result1$numberOfSubjects[3, ], c(110, 110), label = paste0(result1$numberOfSubjects[3, ]))
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
    expect_equal(result1$expectedNumberOfSubjects, c(71.94, 97.96), tolerance = 1e-07, label = paste0(result1$expectedNumberOfSubjects))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result1), NA)))
        expect_output(print(result1)$show())
        invisible(capture.output(expect_error(summary(result1), NA)))
        expect_output(summary(result1)$show())
        suppressWarnings(result1CodeBased <- eval(parse(text = getObjectRCode(result1, stringWrapParagraphWidth = NULL))))
        expect_equal(result1CodeBased$theta, result1$theta, tolerance = 1e-07)
        expect_equal(result1CodeBased$numberOfSubjects, result1$numberOfSubjects, tolerance = 1e-07)
        expect_equal(result1CodeBased$iterations, result1$iterations, tolerance = 1e-07)
        expect_equal(result1CodeBased$overallReject, result1$overallReject, tolerance = 1e-07)
        expect_equal(result1CodeBased$rejectPerStage, result1$rejectPerStage, tolerance = 1e-07)
        expect_equal(result1CodeBased$futilityStop, result1$futilityStop, tolerance = 1e-07)
        expect_equal(result1CodeBased$futilityPerStage, result1$futilityPerStage, tolerance = 1e-07)
        expect_equal(result1CodeBased$earlyStop, result1$earlyStop, tolerance = 1e-07)
        expect_equal(result1CodeBased$expectedNumberOfSubjects, result1$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_type(names(result1), "character")
        df <- as.data.frame(result1)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result1)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    suppressWarnings(result2 <- getSimulationCounts(
        design = design2,
        maxNumberOfSubjects = 110,
        plannedCalendarTime = c(4.886914, 7.878929, 14),
        lambda1 = c(0.7, 0.3),
        lambda2 = 0.7,
        overdispersion = 2,
        maxNumberOfIterations = 100,
        accrualTime = 12,
        followUpTime = 2,
        seed = 4378628
    ))

    ## Pairwise comparison of the results of result1 with the results of result2
    expect_equal(result1$theta, result2$theta, tolerance = 1e-07)
    expect_equal(result2$numberOfSubjects[1, ], result1$numberOfSubjects[1, ])
    expect_equal(result2$numberOfSubjects[2, ], result1$numberOfSubjects[2, ])
    expect_equal(result2$numberOfSubjects[3, ], result1$numberOfSubjects[3, ])
    expect_equal(result2$iterations[1, ], result1$iterations[1, ])
    expect_equal(result2$iterations[2, ], result1$iterations[2, ])
    expect_equal(result2$iterations[3, ], result1$iterations[3, ])
    expect_equal(result1$overallReject, result2$overallReject, tolerance = 1e-07)
    expect_equal(result2$rejectPerStage[1, ], result1$rejectPerStage[1, ], tolerance = 1e-07)
    expect_equal(result2$rejectPerStage[2, ], result1$rejectPerStage[2, ], tolerance = 1e-07)
    expect_equal(result2$rejectPerStage[3, ], result1$rejectPerStage[3, ], tolerance = 1e-07)
    expect_equal(result1$futilityStop, result2$futilityStop, tolerance = 1e-07)
    expect_equal(result2$futilityPerStage[1, ], result1$futilityPerStage[1, ], tolerance = 1e-07)
    expect_equal(result2$futilityPerStage[2, ], result1$futilityPerStage[2, ], tolerance = 1e-07)
    expect_equal(result2$earlyStop[1, ], result1$earlyStop[1, ], tolerance = 1e-07)
    expect_equal(result1$expectedNumberOfSubjects, result2$expectedNumberOfSubjects, tolerance = 1e-07)

    suppressWarnings(result3 <- getSimulationCounts(
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

    ## Comparison of the results of SimulationResultsCountData object 'result3' with expected results
    expect_equal(result3$theta, c(0.71428571, 0.78571429, 0.85714286, 0.92857143, 1), tolerance = 1e-07, label = paste0(result3$theta))
    expect_equal(result3$numberOfSubjects[1, ], c(400, 400, 400, 400, 400), label = paste0(result3$numberOfSubjects[1, ]))
    expect_equal(result3$numberOfSubjects[2, ], c(400, 400, 400, 400, 400), label = paste0(result3$numberOfSubjects[2, ]))
    expect_equal(result3$numberOfSubjects[3, ], c(400, 400, 400, 400, 400), label = paste0(result3$numberOfSubjects[3, ]))
    expect_equal(result3$numberOfSubjects1, 300, label = paste0(result3$numberOfSubjects1))
    expect_equal(result3$numberOfSubjects2, 100, label = paste0(result3$numberOfSubjects2))
    expect_equal(result3$iterations[1, ], c(100, 100, 100, 100, 100), label = paste0(result3$iterations[1, ]))
    expect_equal(result3$iterations[2, ], c(86, 94, 96, 86, 69), label = paste0(result3$iterations[2, ]))
    expect_equal(result3$iterations[3, ], c(40, 53, 72, 50, 25), label = paste0(result3$iterations[3, ]))
    expect_equal(result3$overallReject, c(0.77, 0.47, 0.26, 0.08, 0.01), tolerance = 1e-07, label = paste0(result3$overallReject))
    expect_equal(result3$rejectPerStage[1, ], c(0.14, 0.05, 0.02, 0, 0), tolerance = 1e-07, label = paste0(result3$rejectPerStage[1, ]))
    expect_equal(result3$rejectPerStage[2, ], c(0.44, 0.28, 0.09, 0.02, 0.01), tolerance = 1e-07, label = paste0(result3$rejectPerStage[2, ]))
    expect_equal(result3$rejectPerStage[3, ], c(0.19, 0.14, 0.15, 0.06, 0), tolerance = 1e-07, label = paste0(result3$rejectPerStage[3, ]))
    expect_equal(result3$futilityStop, c(0.02, 0.14, 0.17, 0.48, 0.74), tolerance = 1e-07, label = paste0(result3$futilityStop))
    expect_equal(result3$futilityPerStage[1, ], c(0, 0.01, 0.02, 0.14, 0.31), tolerance = 1e-07, label = paste0(result3$futilityPerStage[1, ]))
    expect_equal(result3$futilityPerStage[2, ], c(0.02, 0.13, 0.15, 0.34, 0.43), tolerance = 1e-07, label = paste0(result3$futilityPerStage[2, ]))
    expect_equal(result3$earlyStop[1, ], c(0.6, 0.47, 0.28, 0.5, 0.75), tolerance = 1e-07, label = paste0(result3$earlyStop[1, ]))
    expect_equal(result3$expectedNumberOfSubjects, c(400, 400, 400, 400, 400), label = paste0(result3$expectedNumberOfSubjects))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result3), NA)))
        expect_output(print(result3)$show())
        invisible(capture.output(expect_error(summary(result3), NA)))
        expect_output(summary(result3)$show())
        suppressWarnings(result3CodeBased <- eval(parse(text = getObjectRCode(result3, stringWrapParagraphWidth = NULL))))
        expect_equal(result3CodeBased$theta, result3$theta, tolerance = 1e-07)
        expect_equal(result3CodeBased$numberOfSubjects, result3$numberOfSubjects, tolerance = 1e-07)
        expect_equal(result3CodeBased$numberOfSubjects1, result3$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(result3CodeBased$numberOfSubjects2, result3$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(result3CodeBased$iterations, result3$iterations, tolerance = 1e-07)
        expect_equal(result3CodeBased$overallReject, result3$overallReject, tolerance = 1e-07)
        expect_equal(result3CodeBased$rejectPerStage, result3$rejectPerStage, tolerance = 1e-07)
        expect_equal(result3CodeBased$futilityStop, result3$futilityStop, tolerance = 1e-07)
        expect_equal(result3CodeBased$futilityPerStage, result3$futilityPerStage, tolerance = 1e-07)
        expect_equal(result3CodeBased$earlyStop, result3$earlyStop, tolerance = 1e-07)
        expect_equal(result3CodeBased$expectedNumberOfSubjects, result3$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_type(names(result3), "character")
        df <- as.data.frame(result3)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result3)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSimulationCounts': fixed exposure", {
    .skipTestIfDisabled()

    design <- getDesignGroupSequential(
        informationRates = c(0.3, 0.55, 1),
        alpha = 0.025, beta = 0.2, typeOfDesign = "asOF",
        typeBetaSpending = "bsOF", bindingFutility = TRUE
    )

    suppressWarnings(result1 <- getSimulationCounts(
        design = design,
        directionUpper = FALSE,
        maxNumberOfSubjects = 110,
        plannedCalendarTime = c(4.886914, 7.878929, 14),
        lambda1 = c(0.7, 0.3),
        lambda2 = 0.7,
        overdispersion = 2,
        accrualTime = 12,
        fixedExposureTime = 1,
        maxNumberOfIterations = 100,
        seed = 4378628
    ))


    ## Comparison of the results of SimulationResultsCountData object 'result1' with expected results
    expect_equal(result1$theta, c(1, 0.42857143), tolerance = 1e-07, label = paste0(result1$theta))
    expect_equal(result1$numberOfSubjects[1, ], c(44, 44), label = paste0(result1$numberOfSubjects[1, ]))
    expect_equal(result1$numberOfSubjects[2, ], c(72, 72), label = paste0(result1$numberOfSubjects[2, ]))
    expect_equal(result1$numberOfSubjects[3, ], c(110, 110), label = paste0(result1$numberOfSubjects[3, ]))
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
    expect_equal(result1$expectedNumberOfSubjects, c(71.62, 97.38), tolerance = 1e-07, label = paste0(result1$expectedNumberOfSubjects))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result1), NA)))
        expect_output(print(result1)$show())
        invisible(capture.output(expect_error(summary(result1), NA)))
        expect_output(summary(result1)$show())
        suppressWarnings(result1CodeBased <- eval(parse(text = getObjectRCode(result1, stringWrapParagraphWidth = NULL))))
        expect_equal(result1CodeBased$theta, result1$theta, tolerance = 1e-07)
        expect_equal(result1CodeBased$numberOfSubjects, result1$numberOfSubjects, tolerance = 1e-07)
        expect_equal(result1CodeBased$iterations, result1$iterations, tolerance = 1e-07)
        expect_equal(result1CodeBased$overallReject, result1$overallReject, tolerance = 1e-07)
        expect_equal(result1CodeBased$rejectPerStage, result1$rejectPerStage, tolerance = 1e-07)
        expect_equal(result1CodeBased$futilityStop, result1$futilityStop, tolerance = 1e-07)
        expect_equal(result1CodeBased$futilityPerStage, result1$futilityPerStage, tolerance = 1e-07)
        expect_equal(result1CodeBased$earlyStop, result1$earlyStop, tolerance = 1e-07)
        expect_equal(result1CodeBased$expectedNumberOfSubjects, result1$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_type(names(result1), "character")
        df <- as.data.frame(result1)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result1)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
    suppressWarnings(result2 <- getSimulationCounts(
        maxNumberOfSubjects = 110,
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


    ## Comparison of the results of SimulationResultsCountData object 'result2' with expected results
    expect_equal(result2$lambda1, c(1.05, 1.5866662), tolerance = 1e-07, label = paste0(result2$lambda1))
    expect_equal(result2$numberOfSubjects[1, ], c(0, 0), label = paste0(result2$numberOfSubjects[1, ]))
    expect_equal(result2$iterations[1, ], c(100, 100), label = paste0(result2$iterations[1, ]))
    expect_equal(result2$overallReject, c(0.14, 0), tolerance = 1e-07, label = paste0(result2$overallReject))
    expect_equal(result2$expectedNumberOfSubjects, c(0, 0), label = paste0(result2$expectedNumberOfSubjects))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result2), NA)))
        expect_output(print(result2)$show())
        invisible(capture.output(expect_error(summary(result2), NA)))
        expect_output(summary(result2)$show())
        suppressWarnings(result2CodeBased <- eval(parse(text = getObjectRCode(result2, stringWrapParagraphWidth = NULL))))
        expect_equal(result2CodeBased$lambda1, result2$lambda1, tolerance = 1e-07)
        expect_equal(result2CodeBased$numberOfSubjects, result2$numberOfSubjects, tolerance = 1e-07)
        expect_equal(result2CodeBased$iterations, result2$iterations, tolerance = 1e-07)
        expect_equal(result2CodeBased$overallReject, result2$overallReject, tolerance = 1e-07)
        expect_equal(result2CodeBased$expectedNumberOfSubjects, result2$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_type(names(result2), "character")
        df <- as.data.frame(result2)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result2)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
    suppressWarnings(result3 <- getSimulationCounts(
        getDesignGroupSequential(kMax = 3, futilityBounds = c(0, 0)),
        maxNumberOfSubjects = 100,
        plannedCalendarTime = as.matrix(c(2, 4, 6)),
        maxNumberOfIterations = 100,
        fixedExposureTime = 1,
        accrualTime = 6,
        lambda1 = seq(1.05, 1.35, 0.05),
        lambda2 = 1.4,
        seed = 4378628
    ))


    ## Comparison of the results of SimulationResultsCountData object 'result3' with expected results
    expect_equal(result3$theta, c(0.75, 0.78571429, 0.82142857, 0.85714286, 0.89285714, 0.92857143, 0.96428571), tolerance = 1e-07, label = paste0(result3$theta))
    expect_equal(result3$numberOfSubjects[1, ], c(34, 34, 34, 34, 34, 34, 34), label = paste0(result3$numberOfSubjects[1, ]))
    expect_equal(result3$numberOfSubjects[2, ], c(66, 66, 66, 66, 66, 66, 66), label = paste0(result3$numberOfSubjects[2, ]))
    expect_equal(result3$numberOfSubjects[3, ], c(100, 100, 100, 100, 100, 100, 100), label = paste0(result3$numberOfSubjects[3, ]))
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
    expect_equal(result3$expectedNumberOfSubjects, c(44.04, 47.98, 50.66, 52.28, 51.68, 59.58, 63.82), tolerance = 1e-07, label = paste0(result3$expectedNumberOfSubjects))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result3), NA)))
        expect_output(print(result3)$show())
        invisible(capture.output(expect_error(summary(result3), NA)))
        expect_output(summary(result3)$show())
        suppressWarnings(result3CodeBased <- eval(parse(text = getObjectRCode(result3, stringWrapParagraphWidth = NULL))))
        expect_equal(result3CodeBased$theta, result3$theta, tolerance = 1e-07)
        expect_equal(result3CodeBased$numberOfSubjects, result3$numberOfSubjects, tolerance = 1e-07)
        expect_equal(result3CodeBased$iterations, result3$iterations, tolerance = 1e-07)
        expect_equal(result3CodeBased$overallReject, result3$overallReject, tolerance = 1e-07)
        expect_equal(result3CodeBased$rejectPerStage, result3$rejectPerStage, tolerance = 1e-07)
        expect_equal(result3CodeBased$futilityStop, result3$futilityStop, tolerance = 1e-07)
        expect_equal(result3CodeBased$futilityPerStage, result3$futilityPerStage, tolerance = 1e-07)
        expect_equal(result3CodeBased$earlyStop, result3$earlyStop, tolerance = 1e-07)
        expect_equal(result3CodeBased$expectedNumberOfSubjects, result3$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_type(names(result3), "character")
        df <- as.data.frame(result3)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result3)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})
