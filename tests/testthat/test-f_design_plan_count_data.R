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
## |  File name: test-f_design_plan_count_data.R
## |  Creation date: 08 January 2024, 11:50:12
## |  File version: $Revision: 7554 $
## |  Last changed: $Date: 2024-01-12 10:19:05 +0100 (Fr, 12 Jan 2024) $
## |  Last changed by: $Author: pahlke $
## |

test_plan_section("Testing the Sample Size Calculation of Count Data Designs for Different Designs and Arguments")


test_that("'getSampleSizeCounts': Sample size calculation of testing count data for fixed sample design", {
    .skipTestIfDisabled()

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizeCountsFixedExposureSingleStage}
    # @refFS[Formula]{fs:sampleSizePerStageFixedExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result <- getSampleSizeCounts(
        alpha = 0.01, beta = 0.05, lambda = 0.234, theta = 0.7,
        overDispersion = 0.71, accrualTime = 7, fixedExposureTime = 1
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result' with expected results
    expect_equal(result$directionUpper, FALSE, label = paste0(result$directionUpper))
    expect_equal(result$lambda1, 0.19270588, tolerance = 1e-07, label = paste0(result$lambda1))
    expect_equal(result$lambda2, 0.27529412, tolerance = 1e-07, label = paste0(result$lambda2))
    expect_equal(result$nFixed, 2540, label = paste0(result$nFixed))
    expect_equal(result$calendarTime[1, ], 8, label = paste0(result$calendarTime[1, ]))
    expect_equal(result$expectedStudyDurationH1, 8, label = paste0(result$expectedStudyDurationH1))
    expect_equal(result$expectedNumberOfSubjectsH1, 2540, label = paste0(result$expectedNumberOfSubjectsH1))
    expect_equal(result$maxInformation, 123.96487, tolerance = 1e-07, label = paste0(result$maxInformation))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result), NA)))
        expect_output(print(result)$show())
        invisible(capture.output(expect_error(summary(result), NA)))
        expect_output(summary(result)$show())
        resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
        expect_equal(resultCodeBased$directionUpper, result$directionUpper, tolerance = 1e-07)
        expect_equal(resultCodeBased$lambda1, result$lambda1, tolerance = 1e-07)
        expect_equal(resultCodeBased$lambda2, result$lambda2, tolerance = 1e-07)
        expect_equal(resultCodeBased$nFixed, result$nFixed, tolerance = 1e-07)
        expect_equal(resultCodeBased$calendarTime, result$calendarTime, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedStudyDurationH1, result$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedNumberOfSubjectsH1, result$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxInformation, result$maxInformation, tolerance = 1e-07)
        expect_type(names(result), "character")
        df <- as.data.frame(result)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizePerStageVariableExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result <- getSampleSizeCounts(
        alpha = 0.01, beta = 0.05, lambda = 0.234, theta = 0.7,
        overDispersion = 0.71, accrualTime = 7, followUpTime = 1
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result' with expected results
    expect_equal(result$directionUpper, FALSE, label = paste0(result$directionUpper))
    expect_equal(result$lambda1, 0.19270588, tolerance = 1e-07, label = paste0(result$lambda1))
    expect_equal(result$lambda2, 0.27529412, tolerance = 1e-07, label = paste0(result$lambda2))
    expect_equal(result$nFixed, 886, label = paste0(result$nFixed))
    expect_equal(result$calendarTime[1, ], 8, label = paste0(result$calendarTime[1, ]))
    expect_equal(result$expectedStudyDurationH1, 8, label = paste0(result$expectedStudyDurationH1))
    expect_equal(result$expectedNumberOfSubjectsH1, 886, label = paste0(result$expectedNumberOfSubjectsH1))
    expect_equal(result$maxInformation, 123.96487, tolerance = 1e-07, label = paste0(result$maxInformation))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result), NA)))
        expect_output(print(result)$show())
        invisible(capture.output(expect_error(summary(result), NA)))
        expect_output(summary(result)$show())
        resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
        expect_equal(resultCodeBased$directionUpper, result$directionUpper, tolerance = 1e-07)
        expect_equal(resultCodeBased$lambda1, result$lambda1, tolerance = 1e-07)
        expect_equal(resultCodeBased$lambda2, result$lambda2, tolerance = 1e-07)
        expect_equal(resultCodeBased$nFixed, result$nFixed, tolerance = 1e-07)
        expect_equal(resultCodeBased$calendarTime, result$calendarTime, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedStudyDurationH1, result$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedNumberOfSubjectsH1, result$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxInformation, result$maxInformation, tolerance = 1e-07)
        expect_type(names(result), "character")
        df <- as.data.frame(result)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizeCountsFixedExposureSingleStage}
    # @refFS[Formula]{fs:sampleSizePerStageFixedExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result <- getSampleSizeCounts(
        alpha = 0.01, beta = 0.05, lambda2 = 0.02, theta = c(0.7, 0.8),
        overDispersion = 13.1, accrualTime = 7, fixedExposureTime = 2
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result' with expected results
    expect_equal(result$directionUpper, c(FALSE, FALSE), label = paste0(result$directionUpper))
    expect_equal(result$lambda1, c(0.014, 0.016), tolerance = 1e-07, label = paste0(result$lambda1))
    expect_equal(result$nFixed, c(21550, 52228), label = paste0(result$nFixed))
    expect_equal(result$calendarTime[1, ], c(9, 9), label = paste0(result$calendarTime[1, ]))
    expect_equal(result$expectedStudyDurationH1, c(9, 9), label = paste0(result$expectedStudyDurationH1))
    expect_equal(result$expectedNumberOfSubjectsH1, c(21550, 52228), label = paste0(result$expectedNumberOfSubjectsH1))
    expect_equal(result$maxInformation, c(123.96487, 316.71977), tolerance = 1e-07, label = paste0(result$maxInformation))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result), NA)))
        expect_output(print(result)$show())
        invisible(capture.output(expect_error(summary(result), NA)))
        expect_output(summary(result)$show())
        resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
        expect_equal(resultCodeBased$directionUpper, result$directionUpper, tolerance = 1e-07)
        expect_equal(resultCodeBased$lambda1, result$lambda1, tolerance = 1e-07)
        expect_equal(resultCodeBased$nFixed, result$nFixed, tolerance = 1e-07)
        expect_equal(resultCodeBased$calendarTime, result$calendarTime, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedStudyDurationH1, result$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedNumberOfSubjectsH1, result$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxInformation, result$maxInformation, tolerance = 1e-07)
        expect_type(names(result), "character")
        df <- as.data.frame(result)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizePerStageVariableExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result <- getSampleSizeCounts(
        alpha = 0.01, beta = 0.05, lambda1 = seq(1.05, 1.35, 0.15),
        lambda2 = 1.4, accrualTime = c(0, 20), allocationRatioPlanned = 4, maxNumberOfSubjects = 100
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result' with expected results
    expect_equal(result$directionUpper, c(FALSE, FALSE, FALSE), label = paste0(result$directionUpper))
    expect_equal(result$theta, c(0.75, 0.85714286, 0.96428571), tolerance = 1e-07, label = paste0(result$theta))
    expect_equal(result$nFixed1, c(80, 80, 80), label = paste0(result$nFixed1))
    expect_equal(result$nFixed2, c(20, 20, 20), label = paste0(result$nFixed2))
    expect_equal(result$calendarTime[1, ], c(19.030272, 40.615784, 546.25411), tolerance = 1e-07, label = paste0(result$calendarTime[1, ]))
    expect_equal(result$expectedStudyDurationH1, c(19.030272, 40.615784, 546.25411), tolerance = 1e-07, label = paste0(result$expectedStudyDurationH1))
    expect_equal(result$expectedNumberOfSubjectsH1, c(95, 100, 100), label = paste0(result$expectedNumberOfSubjectsH1))
    expect_equal(result$maxInformation, c(190.55408, 663.67118, 11923.768), tolerance = 1e-07, label = paste0(result$maxInformation))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result), NA)))
        expect_output(print(result)$show())
        invisible(capture.output(expect_error(summary(result), NA)))
        expect_output(summary(result)$show())
        resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
        expect_equal(resultCodeBased$directionUpper, result$directionUpper, tolerance = 1e-07)
        expect_equal(resultCodeBased$theta, result$theta, tolerance = 1e-07)
        expect_equal(resultCodeBased$nFixed1, result$nFixed1, tolerance = 1e-07)
        expect_equal(resultCodeBased$nFixed2, result$nFixed2, tolerance = 1e-07)
        expect_equal(resultCodeBased$calendarTime, result$calendarTime, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedStudyDurationH1, result$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedNumberOfSubjectsH1, result$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxInformation, result$maxInformation, tolerance = 1e-07)
        expect_type(names(result), "character")
        df <- as.data.frame(result)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizePerStageVariableExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    expect_error(getSampleSizeCounts(
        alpha = 0.01, beta = 0.05, lambda1 = seq(1.35, 1.35, 0.15),
        overDispersion = 0.01, lambda2 = 1.4, accrualTime = c(0, 10, 20), accrualIntensity = c(5, 10)
    ))

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizePerStageVariableExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result <- getSampleSizeCounts(
        alpha = 0.01, beta = 0.05, thetaH0 = 0.9, lambda1 = seq(1.1, 1.15, 0.05),
        overDispersion = 0.2, lambda2 = 1.7, accrualTime = c(0, 2), maxNumberOfSubjects = 300
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result' with expected results
    expect_equal(result$directionUpper, c(FALSE, FALSE), label = paste0(result$directionUpper))
    expect_equal(result$theta, c(0.64705882, 0.67647059), tolerance = 1e-07, label = paste0(result$theta))
    expect_equal(result$calendarTime[1, ], c(3.4107281, 4.9286263), tolerance = 1e-07, label = paste0(result$calendarTime[1, ]))
    expect_equal(result$expectedStudyDurationH1, c(3.4107281, 4.9286263), tolerance = 1e-07, label = paste0(result$expectedStudyDurationH1))
    expect_equal(result$expectedNumberOfSubjectsH1, c(300, 300), label = paste0(result$expectedNumberOfSubjectsH1))
    expect_equal(result$maxInformation, c(144.85307, 193.47016), tolerance = 1e-07, label = paste0(result$maxInformation))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result), NA)))
        expect_output(print(result)$show())
        invisible(capture.output(expect_error(summary(result), NA)))
        expect_output(summary(result)$show())
        resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
        expect_equal(resultCodeBased$directionUpper, result$directionUpper, tolerance = 1e-07)
        expect_equal(resultCodeBased$theta, result$theta, tolerance = 1e-07)
        expect_equal(resultCodeBased$calendarTime, result$calendarTime, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedStudyDurationH1, result$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedNumberOfSubjectsH1, result$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxInformation, result$maxInformation, tolerance = 1e-07)
        expect_type(names(result), "character")
        df <- as.data.frame(result)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizePerStageVariableExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result <- getSampleSizeCounts(
        alpha = 0.01, beta = 0.05, theta = seq(1.05, 1.35, 0.15),
        lambda2 = 1.4, accrualTime = c(0, 10, 20), accrualIntensity = c(5, 10)
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result' with expected results
    expect_equal(result$directionUpper, c(TRUE, TRUE, TRUE), label = paste0(result$directionUpper))
    expect_equal(result$lambda1, c(1.47, 1.68, 1.89), tolerance = 1e-07, label = paste0(result$lambda1))
    expect_equal(result$nFixed, c(150, 150, 150), label = paste0(result$nFixed))
    expect_equal(result$calendarTime[1, ], c(134.88407, 19.983394, 12.884843), tolerance = 1e-07, label = paste0(result$calendarTime[1, ]))
    expect_equal(result$expectedStudyDurationH1, c(134.88407, 19.983394, 12.884843), tolerance = 1e-07, label = paste0(result$expectedStudyDurationH1))
    expect_equal(result$expectedNumberOfSubjectsH1, c(150, 148, 78), label = paste0(result$expectedNumberOfSubjectsH1))
    expect_equal(result$maxInformation, c(6624.8994, 474.42526, 175.10501), tolerance = 1e-07, label = paste0(result$maxInformation))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result), NA)))
        expect_output(print(result)$show())
        invisible(capture.output(expect_error(summary(result), NA)))
        expect_output(summary(result)$show())
        resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
        expect_equal(resultCodeBased$directionUpper, result$directionUpper, tolerance = 1e-07)
        expect_equal(resultCodeBased$lambda1, result$lambda1, tolerance = 1e-07)
        expect_equal(resultCodeBased$nFixed, result$nFixed, tolerance = 1e-07)
        expect_equal(resultCodeBased$calendarTime, result$calendarTime, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedStudyDurationH1, result$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedNumberOfSubjectsH1, result$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxInformation, result$maxInformation, tolerance = 1e-07)
        expect_type(names(result), "character")
        df <- as.data.frame(result)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizeCountsFixedExposureSingleStage}
    # @refFS[Formula]{fs:sampleSizePerStageFixedExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    # @refFS[Formula]{fs:sampleSizeCountsFixedExposureOptimumAllocationRatio}
    result <- getSampleSizeCounts(
        alpha = 0.01, beta = 0.05, lambda2 = 0.234, lambda1 = c(0.1, 0.2),
        allocationRatioPlanned = 0, accrualTime = 7, fixedExposureTime = 1
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result' with expected results
    expect_equal(result$allocationRatioPlanned, c(1.5297048, 1.0816655), tolerance = 1e-07, label = paste0(result$allocationRatioPlanned))
    expect_equal(result$directionUpper, c(FALSE, FALSE), label = paste0(result$directionUpper))
    expect_equal(result$theta, c(0.42735043, 0.85470085), tolerance = 1e-07, label = paste0(result$theta))
    expect_equal(result$nFixed, c(597, 11849), label = paste0(result$nFixed))
    expect_equal(result$nFixed1, c(361, 6157), label = paste0(result$nFixed1))
    expect_equal(result$nFixed2, c(236, 5692), label = paste0(result$nFixed2))
    expect_equal(result$calendarTime[1, ], c(8, 8), label = paste0(result$calendarTime[1, ]))
    expect_equal(result$expectedStudyDurationH1, c(8, 8), label = paste0(result$expectedStudyDurationH1))
    expect_equal(result$expectedNumberOfSubjectsH1, c(597, 11849), label = paste0(result$expectedNumberOfSubjectsH1))
    expect_equal(result$maxInformation, c(21.819851, 639.7699), tolerance = 1e-07, label = paste0(result$maxInformation))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result), NA)))
        expect_output(print(result)$show())
        invisible(capture.output(expect_error(summary(result), NA)))
        expect_output(summary(result)$show())
        resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
        expect_equal(resultCodeBased$allocationRatioPlanned, result$allocationRatioPlanned, tolerance = 1e-07)
        expect_equal(resultCodeBased$directionUpper, result$directionUpper, tolerance = 1e-07)
        expect_equal(resultCodeBased$theta, result$theta, tolerance = 1e-07)
        expect_equal(resultCodeBased$nFixed, result$nFixed, tolerance = 1e-07)
        expect_equal(resultCodeBased$nFixed1, result$nFixed1, tolerance = 1e-07)
        expect_equal(resultCodeBased$nFixed2, result$nFixed2, tolerance = 1e-07)
        expect_equal(resultCodeBased$calendarTime, result$calendarTime, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedStudyDurationH1, result$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedNumberOfSubjectsH1, result$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxInformation, result$maxInformation, tolerance = 1e-07)
        expect_type(names(result), "character")
        df <- as.data.frame(result)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizePerStageVariableExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    # @refFS[Formula]{fs:sampleSizeCountsVariableExposureOptimumAllocationRatio}
    result <- getSampleSizeCounts(
        alpha = 0.01, beta = 0.05, lambda2 = 0.234, lambda1 = c(0.1, 0.2),
        allocationRatioPlanned = 0, accrualTime = 7, followUpTime = 1
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result' with expected results
    expect_equal(result$allocationRatioPlanned, c(1.1613522, 1.0659374), tolerance = 1e-07, label = paste0(result$allocationRatioPlanned))
    expect_equal(result$directionUpper, c(FALSE, FALSE), label = paste0(result$directionUpper))
    expect_equal(result$theta, c(0.42735043, 0.85470085), tolerance = 1e-07, label = paste0(result$theta))
    expect_equal(result$nFixed, c(135, 2633), label = paste0(result$nFixed))
    expect_equal(result$nFixed1, c(73, 1359), label = paste0(result$nFixed1))
    expect_equal(result$nFixed2, c(62, 1274), label = paste0(result$nFixed2))
    expect_equal(result$calendarTime[1, ], c(8, 8), label = paste0(result$calendarTime[1, ]))
    expect_equal(result$expectedStudyDurationH1, c(8, 8), label = paste0(result$expectedStudyDurationH1))
    expect_equal(result$expectedNumberOfSubjectsH1, c(135, 2633), label = paste0(result$expectedNumberOfSubjectsH1))
    expect_equal(result$maxInformation, c(21.819851, 639.7699), tolerance = 1e-07, label = paste0(result$maxInformation))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result), NA)))
        expect_output(print(result)$show())
        invisible(capture.output(expect_error(summary(result), NA)))
        expect_output(summary(result)$show())
        resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
        expect_equal(resultCodeBased$allocationRatioPlanned, result$allocationRatioPlanned, tolerance = 1e-07)
        expect_equal(resultCodeBased$directionUpper, result$directionUpper, tolerance = 1e-07)
        expect_equal(resultCodeBased$theta, result$theta, tolerance = 1e-07)
        expect_equal(resultCodeBased$nFixed, result$nFixed, tolerance = 1e-07)
        expect_equal(resultCodeBased$nFixed1, result$nFixed1, tolerance = 1e-07)
        expect_equal(resultCodeBased$nFixed2, result$nFixed2, tolerance = 1e-07)
        expect_equal(resultCodeBased$calendarTime, result$calendarTime, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedStudyDurationH1, result$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedNumberOfSubjectsH1, result$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxInformation, result$maxInformation, tolerance = 1e-07)
        expect_type(names(result), "character")
        df <- as.data.frame(result)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeCounts': Sample size calculation of testing count data for one-sided group sequential design", {
    .skipTestIfDisabled()

    designGS1 <- getDesignGroupSequential(
        informationRates = c(0.32, 0.57, 1), futilityBounds = c(-0.4, 0.1),
        sided = 1, beta = 0.1, typeOfDesign = "WT", deltaWT = 0.25
    )

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizeCountsFixedExposureSingleStage}
    # @refFS[Formula]{fs:sampleSizePerStageFixedExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result <- getSampleSizeCounts(
        design = designGS1, lambda = 0.234, theta = 0.7,
        overDispersion = 0.71, accrualTime = 7, fixedExposureTime = 1
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result' with expected results
    expect_equal(result$directionUpper, FALSE, label = paste0(result$directionUpper))
    expect_equal(result$lambda1, 0.19270588, tolerance = 1e-07, label = paste0(result$lambda1))
    expect_equal(result$lambda2, 0.27529412, tolerance = 1e-07, label = paste0(result$lambda2))
    expect_equal(result$maxNumberOfSubjects, 1806, label = paste0(result$maxNumberOfSubjects))
    expect_equal(result$rejectPerStage[1, ], 0.19514116, tolerance = 1e-07, label = paste0(result$rejectPerStage[1, ]))
    expect_equal(result$rejectPerStage[2, ], 0.37204423, tolerance = 1e-07, label = paste0(result$rejectPerStage[2, ]))
    expect_equal(result$rejectPerStage[3, ], 0.33281461, tolerance = 1e-07, label = paste0(result$rejectPerStage[3, ]))
    expect_equal(result$futilityStop, 0.015698246, tolerance = 1e-07, label = paste0(result$futilityStop))
    expect_equal(result$futilityPerStage[1, ], 0.01090105, tolerance = 1e-07, label = paste0(result$futilityPerStage[1, ]))
    expect_equal(result$futilityPerStage[2, ], 0.004797196, tolerance = 1e-07, label = paste0(result$futilityPerStage[2, ]))
    expect_equal(result$earlyStop, 0.58288364, tolerance = 1e-07, label = paste0(result$earlyStop))
    expect_equal(result$calendarTime[1, ], 2.7119374, tolerance = 1e-07, label = paste0(result$calendarTime[1, ]))
    expect_equal(result$calendarTime[2, ], 4.4624998, tolerance = 1e-07, label = paste0(result$calendarTime[2, ]))
    expect_equal(result$calendarTime[3, ], 8, label = paste0(result$calendarTime[3, ]))
    expect_equal(result$expectedStudyDurationH1, 5.5773593, tolerance = 1e-07, label = paste0(result$expectedStudyDurationH1))
    expect_equal(result$numberOfSubjects[1, ], 700, label = paste0(result$numberOfSubjects[1, ]))
    expect_equal(result$numberOfSubjects[2, ], 1152, label = paste0(result$numberOfSubjects[2, ]))
    expect_equal(result$numberOfSubjects[3, ], 1806, label = paste0(result$numberOfSubjects[3, ]))
    expect_equal(result$expectedNumberOfSubjectsH1, 1331.663, tolerance = 1e-07, label = paste0(result$expectedNumberOfSubjectsH1))
    expect_equal(result$informationOverStages[1, ], 28.191756, tolerance = 1e-07, label = paste0(result$informationOverStages[1, ]))
    expect_equal(result$informationOverStages[2, ], 50.216566, tolerance = 1e-07, label = paste0(result$informationOverStages[2, ]))
    expect_equal(result$informationOverStages[3, ], 88.099238, tolerance = 1e-07, label = paste0(result$informationOverStages[3, ]))
    expect_equal(result$expectedInformationH0, 58.055315, tolerance = 1e-07, label = paste0(result$expectedInformationH0))
    expect_equal(result$expectedInformationH01, 74.084094, tolerance = 1e-07, label = paste0(result$expectedInformationH01))
    expect_equal(result$expectedInformationH1, 61.480008, tolerance = 1e-07, label = paste0(result$expectedInformationH1))
    expect_equal(result$maxInformation, 88.099238, tolerance = 1e-07, label = paste0(result$maxInformation))
    expect_equal(result$futilityBoundsPValueScale[1, ], 0.65542174, tolerance = 1e-07, label = paste0(result$futilityBoundsPValueScale[1, ]))
    expect_equal(result$futilityBoundsPValueScale[2, ], 0.46017216, tolerance = 1e-07, label = paste0(result$futilityBoundsPValueScale[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result), NA)))
        expect_output(print(result)$show())
        invisible(capture.output(expect_error(summary(result), NA)))
        expect_output(summary(result)$show())
        resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
        expect_equal(resultCodeBased$directionUpper, result$directionUpper, tolerance = 1e-07)
        expect_equal(resultCodeBased$lambda1, result$lambda1, tolerance = 1e-07)
        expect_equal(resultCodeBased$lambda2, result$lambda2, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxNumberOfSubjects, result$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(resultCodeBased$rejectPerStage, result$rejectPerStage, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityStop, result$futilityStop, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityPerStage, result$futilityPerStage, tolerance = 1e-07)
        expect_equal(resultCodeBased$earlyStop, result$earlyStop, tolerance = 1e-07)
        expect_equal(resultCodeBased$calendarTime, result$calendarTime, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedStudyDurationH1, result$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$numberOfSubjects, result$numberOfSubjects, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedNumberOfSubjectsH1, result$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$informationOverStages, result$informationOverStages, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH0, result$expectedInformationH0, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH01, result$expectedInformationH01, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH1, result$expectedInformationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxInformation, result$maxInformation, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityBoundsPValueScale, result$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(result), "character")
        df <- as.data.frame(result)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizePerStageVariableExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result <- getSampleSizeCounts(
        design = designGS1, lambda = 0.234, theta = 0.7,
        overDispersion = 0.71, accrualTime = 7, followUpTime = 1
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result' with expected results
    expect_equal(result$directionUpper, FALSE, label = paste0(result$directionUpper))
    expect_equal(result$lambda1, 0.19270588, tolerance = 1e-07, label = paste0(result$lambda1))
    expect_equal(result$lambda2, 0.27529412, tolerance = 1e-07, label = paste0(result$lambda2))
    expect_equal(result$maxNumberOfSubjects, 630, label = paste0(result$maxNumberOfSubjects))
    expect_equal(result$rejectPerStage[1, ], 0.19514116, tolerance = 1e-07, label = paste0(result$rejectPerStage[1, ]))
    expect_equal(result$rejectPerStage[2, ], 0.37204423, tolerance = 1e-07, label = paste0(result$rejectPerStage[2, ]))
    expect_equal(result$rejectPerStage[3, ], 0.33281461, tolerance = 1e-07, label = paste0(result$rejectPerStage[3, ]))
    expect_equal(result$futilityStop, 0.015698246, tolerance = 1e-07, label = paste0(result$futilityStop))
    expect_equal(result$futilityPerStage[1, ], 0.01090105, tolerance = 1e-07, label = paste0(result$futilityPerStage[1, ]))
    expect_equal(result$futilityPerStage[2, ], 0.004797196, tolerance = 1e-07, label = paste0(result$futilityPerStage[2, ]))
    expect_equal(result$earlyStop, 0.58288364, tolerance = 1e-07, label = paste0(result$earlyStop))
    expect_equal(result$calendarTime[1, ], 3.9379751, tolerance = 1e-07, label = paste0(result$calendarTime[1, ]))
    expect_equal(result$calendarTime[2, ], 5.5492716, tolerance = 1e-07, label = paste0(result$calendarTime[2, ]))
    expect_equal(result$calendarTime[3, ], 8, label = paste0(result$calendarTime[3, ]))
    expect_equal(result$expectedStudyDurationH1, 6.2395154, tolerance = 1e-07, label = paste0(result$expectedStudyDurationH1))
    expect_equal(result$numberOfSubjects[1, ], 354, label = paste0(result$numberOfSubjects[1, ]))
    expect_equal(result$numberOfSubjects[2, ], 498, label = paste0(result$numberOfSubjects[2, ]))
    expect_equal(result$numberOfSubjects[3, ], 630, label = paste0(result$numberOfSubjects[3, ]))
    expect_equal(result$expectedNumberOfSubjectsH1, 523.38928, tolerance = 1e-07, label = paste0(result$expectedNumberOfSubjectsH1))
    expect_equal(result$informationOverStages[1, ], 28.191756, tolerance = 1e-07, label = paste0(result$informationOverStages[1, ]))
    expect_equal(result$informationOverStages[2, ], 50.216566, tolerance = 1e-07, label = paste0(result$informationOverStages[2, ]))
    expect_equal(result$informationOverStages[3, ], 88.099238, tolerance = 1e-07, label = paste0(result$informationOverStages[3, ]))
    expect_equal(result$expectedInformationH0, 58.055315, tolerance = 1e-07, label = paste0(result$expectedInformationH0))
    expect_equal(result$expectedInformationH01, 74.084094, tolerance = 1e-07, label = paste0(result$expectedInformationH01))
    expect_equal(result$expectedInformationH1, 61.480008, tolerance = 1e-07, label = paste0(result$expectedInformationH1))
    expect_equal(result$maxInformation, 88.099238, tolerance = 1e-07, label = paste0(result$maxInformation))
    expect_equal(result$futilityBoundsPValueScale[1, ], 0.65542174, tolerance = 1e-07, label = paste0(result$futilityBoundsPValueScale[1, ]))
    expect_equal(result$futilityBoundsPValueScale[2, ], 0.46017216, tolerance = 1e-07, label = paste0(result$futilityBoundsPValueScale[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result), NA)))
        expect_output(print(result)$show())
        invisible(capture.output(expect_error(summary(result), NA)))
        expect_output(summary(result)$show())
        resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
        expect_equal(resultCodeBased$directionUpper, result$directionUpper, tolerance = 1e-07)
        expect_equal(resultCodeBased$lambda1, result$lambda1, tolerance = 1e-07)
        expect_equal(resultCodeBased$lambda2, result$lambda2, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxNumberOfSubjects, result$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(resultCodeBased$rejectPerStage, result$rejectPerStage, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityStop, result$futilityStop, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityPerStage, result$futilityPerStage, tolerance = 1e-07)
        expect_equal(resultCodeBased$earlyStop, result$earlyStop, tolerance = 1e-07)
        expect_equal(resultCodeBased$calendarTime, result$calendarTime, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedStudyDurationH1, result$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$numberOfSubjects, result$numberOfSubjects, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedNumberOfSubjectsH1, result$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$informationOverStages, result$informationOverStages, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH0, result$expectedInformationH0, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH01, result$expectedInformationH01, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH1, result$expectedInformationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxInformation, result$maxInformation, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityBoundsPValueScale, result$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(result), "character")
        df <- as.data.frame(result)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizeCountsFixedExposureSingleStage}
    # @refFS[Formula]{fs:sampleSizePerStageFixedExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result <- getSampleSizeCounts(
        design = designGS1, lambda2 = 0.02, theta = c(0.7, 0.8),
        accrualTime = 7, fixedExposureTime = 2
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result' with expected results
    expect_equal(result$directionUpper, c(FALSE, FALSE), label = paste0(result$directionUpper))
    expect_equal(result$lambda1, c(0.014, 0.016), tolerance = 1e-07, label = paste0(result$lambda1))
    expect_equal(result$maxNumberOfSubjects, c(10698, 25324), label = paste0(result$maxNumberOfSubjects))
    expect_equal(result$rejectPerStage[1, ], 0.19514116, tolerance = 1e-07, label = paste0(result$rejectPerStage[1, ]))
    expect_equal(result$rejectPerStage[2, ], 0.37204423, tolerance = 1e-07, label = paste0(result$rejectPerStage[2, ]))
    expect_equal(result$rejectPerStage[3, ], 0.33281461, tolerance = 1e-07, label = paste0(result$rejectPerStage[3, ]))
    expect_equal(result$futilityStop, 0.015698246, tolerance = 1e-07, label = paste0(result$futilityStop))
    expect_equal(result$futilityPerStage[1, ], 0.01090105, tolerance = 1e-07, label = paste0(result$futilityPerStage[1, ]))
    expect_equal(result$futilityPerStage[2, ], 0.004797196, tolerance = 1e-07, label = paste0(result$futilityPerStage[2, ]))
    expect_equal(result$earlyStop, 0.58288364, tolerance = 1e-07, label = paste0(result$earlyStop))
    expect_equal(result$calendarTime[1, ], c(3.2397151, 3.23974), tolerance = 1e-07, label = paste0(result$calendarTime[1, ]))
    expect_equal(result$calendarTime[2, ], c(4.9900038, 4.9897529), tolerance = 1e-07, label = paste0(result$calendarTime[2, ]))
    expect_equal(result$calendarTime[3, ], c(9, 9), label = paste0(result$calendarTime[3, ]))
    expect_equal(result$expectedStudyDurationH1, c(6.3020055, 6.3019161), tolerance = 1e-07, label = paste0(result$expectedStudyDurationH1))
    expect_equal(result$numberOfSubjects[1, ], c(4952, 11720), label = paste0(result$numberOfSubjects[1, ]))
    expect_equal(result$numberOfSubjects[2, ], c(7626, 18052), label = paste0(result$numberOfSubjects[2, ]))
    expect_equal(result$numberOfSubjects[3, ], c(10698, 25324), label = paste0(result$numberOfSubjects[3, ]))
    expect_equal(result$expectedNumberOfSubjectsH1, c(8356.4246, 19780.611), tolerance = 1e-07, label = paste0(result$expectedNumberOfSubjectsH1))
    expect_equal(result$informationOverStages[1, ], c(28.191756, 72.027552), tolerance = 1e-07, label = paste0(result$informationOverStages[1, ]))
    expect_equal(result$informationOverStages[2, ], c(50.216566, 128.29908), tolerance = 1e-07, label = paste0(result$informationOverStages[2, ]))
    expect_equal(result$informationOverStages[3, ], c(88.099238, 225.0861), tolerance = 1e-07, label = paste0(result$informationOverStages[3, ]))
    expect_equal(result$expectedInformationH0, c(58.055315, 148.32642), tolerance = 1e-07, label = paste0(result$expectedInformationH0))
    expect_equal(result$expectedInformationH01, c(74.084094, 189.27859), tolerance = 1e-07, label = paste0(result$expectedInformationH01))
    expect_equal(result$expectedInformationH1, c(61.480008, 157.07622), tolerance = 1e-07, label = paste0(result$expectedInformationH1))
    expect_equal(result$maxInformation, c(88.099238, 225.0861), tolerance = 1e-07, label = paste0(result$maxInformation))
    expect_equal(result$futilityBoundsPValueScale[1, ], 0.65542174, tolerance = 1e-07, label = paste0(result$futilityBoundsPValueScale[1, ]))
    expect_equal(result$futilityBoundsPValueScale[2, ], 0.46017216, tolerance = 1e-07, label = paste0(result$futilityBoundsPValueScale[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result), NA)))
        expect_output(print(result)$show())
        invisible(capture.output(expect_error(summary(result), NA)))
        expect_output(summary(result)$show())
        resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
        expect_equal(resultCodeBased$directionUpper, result$directionUpper, tolerance = 1e-07)
        expect_equal(resultCodeBased$lambda1, result$lambda1, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxNumberOfSubjects, result$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(resultCodeBased$rejectPerStage, result$rejectPerStage, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityStop, result$futilityStop, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityPerStage, result$futilityPerStage, tolerance = 1e-07)
        expect_equal(resultCodeBased$earlyStop, result$earlyStop, tolerance = 1e-07)
        expect_equal(resultCodeBased$calendarTime, result$calendarTime, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedStudyDurationH1, result$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$numberOfSubjects, result$numberOfSubjects, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedNumberOfSubjectsH1, result$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$informationOverStages, result$informationOverStages, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH0, result$expectedInformationH0, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH01, result$expectedInformationH01, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH1, result$expectedInformationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxInformation, result$maxInformation, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityBoundsPValueScale, result$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(result), "character")
        df <- as.data.frame(result)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizePerStageVariableExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result <- getSampleSizeCounts(
        design = designGS1, lambda1 = seq(1.05, 1.35, 0.15),
        lambda2 = 1.4, accrualTime = c(0, 20), allocationRatioPlanned = 4, maxNumberOfSubjects = 100
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result' with expected results
    expect_equal(result$directionUpper, c(FALSE, FALSE, FALSE), label = paste0(result$directionUpper))
    expect_equal(result$theta, c(0.75, 0.85714286, 0.96428571), tolerance = 1e-07, label = paste0(result$theta))
    expect_equal(result$maxNumberOfSubjects1, c(80, 80, 80), label = paste0(result$maxNumberOfSubjects1))
    expect_equal(result$maxNumberOfSubjects2, c(20, 20, 20), label = paste0(result$maxNumberOfSubjects2))
    expect_equal(result$rejectPerStage[1, ], 0.19514116, tolerance = 1e-07, label = paste0(result$rejectPerStage[1, ]))
    expect_equal(result$rejectPerStage[2, ], 0.37204423, tolerance = 1e-07, label = paste0(result$rejectPerStage[2, ]))
    expect_equal(result$rejectPerStage[3, ], 0.33281461, tolerance = 1e-07, label = paste0(result$rejectPerStage[3, ]))
    expect_equal(result$futilityStop, 0.015698246, tolerance = 1e-07, label = paste0(result$futilityStop))
    expect_equal(result$futilityPerStage[1, ], 0.01090105, tolerance = 1e-07, label = paste0(result$futilityPerStage[1, ]))
    expect_equal(result$futilityPerStage[2, ], 0.004797196, tolerance = 1e-07, label = paste0(result$futilityPerStage[2, ]))
    expect_equal(result$earlyStop, 0.58288364, tolerance = 1e-07, label = paste0(result$earlyStop))
    expect_equal(result$calendarTime[1, ], c(8.8532025, 16.614079, 131.95346), tolerance = 1e-07, label = paste0(result$calendarTime[1, ]))
    expect_equal(result$calendarTime[2, ], c(11.956509, 22.402058, 227.2296), tolerance = 1e-07, label = paste0(result$calendarTime[2, ]))
    expect_equal(result$calendarTime[3, ], c(15.975753, 31.757996, 391.10455), tolerance = 1e-07, label = paste0(result$calendarTime[3, ]))
    expect_equal(result$expectedStudyDurationH1, c(12.993589, 25.112005, 275.95362), tolerance = 1e-07, label = paste0(result$expectedStudyDurationH1))
    expect_equal(result$numberOfSubjects[1, ], c(44, 82, 100), label = paste0(result$numberOfSubjects[1, ]))
    expect_equal(result$numberOfSubjects[2, ], c(60, 100, 100), label = paste0(result$numberOfSubjects[2, ]))
    expect_equal(result$numberOfSubjects[3, ], c(80, 100, 100), label = paste0(result$numberOfSubjects[3, ]))
    expect_equal(result$expectedNumberOfSubjectsH1, c(65.045652, 96.29124, 100), tolerance = 1e-07, label = paste0(result$expectedNumberOfSubjectsH1))
    expect_equal(result$informationOverStages[1, ], c(43.335293, 150.9303, 2711.671), tolerance = 1e-07, label = paste0(result$informationOverStages[1, ]))
    expect_equal(result$informationOverStages[2, ], c(77.190991, 268.8446, 4830.164), tolerance = 1e-07, label = paste0(result$informationOverStages[2, ]))
    expect_equal(result$informationOverStages[3, ], c(135.42279, 471.6572, 8473.9719), tolerance = 1e-07, label = paste0(result$informationOverStages[3, ]))
    expect_equal(result$expectedInformationH0, c(89.240417, 310.81094, 5584.1471), tolerance = 1e-07, label = paste0(result$expectedInformationH0))
    expect_equal(result$expectedInformationH01, c(113.87925, 396.62427, 7125.9019), tolerance = 1e-07, label = paste0(result$expectedInformationH01))
    expect_equal(result$expectedInformationH1, c(94.504724, 329.14573, 5913.5569), tolerance = 1e-07, label = paste0(result$expectedInformationH1))
    expect_equal(result$maxInformation, c(135.42279, 471.6572, 8473.9719), tolerance = 1e-07, label = paste0(result$maxInformation))
    expect_equal(result$futilityBoundsPValueScale[1, ], 0.65542174, tolerance = 1e-07, label = paste0(result$futilityBoundsPValueScale[1, ]))
    expect_equal(result$futilityBoundsPValueScale[2, ], 0.46017216, tolerance = 1e-07, label = paste0(result$futilityBoundsPValueScale[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result), NA)))
        expect_output(print(result)$show())
        invisible(capture.output(expect_error(summary(result), NA)))
        expect_output(summary(result)$show())
        resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
        expect_equal(resultCodeBased$directionUpper, result$directionUpper, tolerance = 1e-07)
        expect_equal(resultCodeBased$theta, result$theta, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxNumberOfSubjects1, result$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxNumberOfSubjects2, result$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(resultCodeBased$rejectPerStage, result$rejectPerStage, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityStop, result$futilityStop, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityPerStage, result$futilityPerStage, tolerance = 1e-07)
        expect_equal(resultCodeBased$earlyStop, result$earlyStop, tolerance = 1e-07)
        expect_equal(resultCodeBased$calendarTime, result$calendarTime, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedStudyDurationH1, result$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$numberOfSubjects, result$numberOfSubjects, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedNumberOfSubjectsH1, result$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$informationOverStages, result$informationOverStages, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH0, result$expectedInformationH0, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH01, result$expectedInformationH01, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH1, result$expectedInformationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxInformation, result$maxInformation, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityBoundsPValueScale, result$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(result), "character")
        df <- as.data.frame(result)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizePerStageVariableExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result <- getSampleSizeCounts(
        design = designGS1, lambda1 = seq(1.05, 1.35, 0.15),
        lambda2 = 1.4, accrualTime = c(0, 10, 20), accrualIntensity = c(5, 10)
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result' with expected results
    expect_equal(result$directionUpper, c(FALSE, FALSE, FALSE), label = paste0(result$directionUpper))
    expect_equal(result$theta, c(0.75, 0.85714286, 0.96428571), tolerance = 1e-07, label = paste0(result$theta))
    expect_equal(result$maxNumberOfSubjects, c(150, 150, 150), label = paste0(result$maxNumberOfSubjects))
    expect_equal(result$rejectPerStage[1, ], 0.19514116, tolerance = 1e-07, label = paste0(result$rejectPerStage[1, ]))
    expect_equal(result$rejectPerStage[2, ], 0.37204423, tolerance = 1e-07, label = paste0(result$rejectPerStage[2, ]))
    expect_equal(result$rejectPerStage[3, ], 0.33281461, tolerance = 1e-07, label = paste0(result$rejectPerStage[3, ]))
    expect_equal(result$futilityStop, 0.015698246, tolerance = 1e-07, label = paste0(result$futilityStop))
    expect_equal(result$futilityPerStage[1, ], 0.01090105, tolerance = 1e-07, label = paste0(result$futilityPerStage[1, ]))
    expect_equal(result$futilityPerStage[2, ], 0.004797196, tolerance = 1e-07, label = paste0(result$futilityPerStage[2, ]))
    expect_equal(result$earlyStop, 0.58288364, tolerance = 1e-07, label = paste0(result$earlyStop))
    expect_equal(result$calendarTime[1, ], c(7.5513415, 13.27872, 64.307374), tolerance = 1e-07, label = paste0(result$calendarTime[1, ]))
    expect_equal(result$calendarTime[2, ], c(10.144294, 16.9121, 105.40688), tolerance = 1e-07, label = paste0(result$calendarTime[2, ]))
    expect_equal(result$calendarTime[3, ], c(13.085473, 21.432609, 176.09804), tolerance = 1e-07, label = paste0(result$calendarTime[3, ]))
    expect_equal(result$expectedStudyDurationH1, c(10.83685, 18.049049, 126.42509), tolerance = 1e-07, label = paste0(result$expectedStudyDurationH1))
    expect_equal(result$numberOfSubjects[1, ], c(38, 82, 150), label = paste0(result$numberOfSubjects[1, ]))
    expect_equal(result$numberOfSubjects[2, ], c(52, 118, 150), label = paste0(result$numberOfSubjects[2, ]))
    expect_equal(result$numberOfSubjects[3, ], c(80, 150, 150), label = paste0(result$numberOfSubjects[3, ]))
    expect_equal(result$expectedNumberOfSubjectsH1, c(60.794667, 123.9302, 150), tolerance = 1e-07, label = paste0(result$expectedNumberOfSubjectsH1))
    expect_equal(result$informationOverStages[1, ], c(43.335293, 150.9303, 2711.671), tolerance = 1e-07, label = paste0(result$informationOverStages[1, ]))
    expect_equal(result$informationOverStages[2, ], c(77.190991, 268.8446, 4830.164), tolerance = 1e-07, label = paste0(result$informationOverStages[2, ]))
    expect_equal(result$informationOverStages[3, ], c(135.42279, 471.6572, 8473.9719), tolerance = 1e-07, label = paste0(result$informationOverStages[3, ]))
    expect_equal(result$expectedInformationH0, c(89.240417, 310.81094, 5584.1471), tolerance = 1e-07, label = paste0(result$expectedInformationH0))
    expect_equal(result$expectedInformationH01, c(113.87925, 396.62427, 7125.9019), tolerance = 1e-07, label = paste0(result$expectedInformationH01))
    expect_equal(result$expectedInformationH1, c(94.504724, 329.14573, 5913.5569), tolerance = 1e-07, label = paste0(result$expectedInformationH1))
    expect_equal(result$maxInformation, c(135.42279, 471.6572, 8473.9719), tolerance = 1e-07, label = paste0(result$maxInformation))
    expect_equal(result$futilityBoundsPValueScale[1, ], 0.65542174, tolerance = 1e-07, label = paste0(result$futilityBoundsPValueScale[1, ]))
    expect_equal(result$futilityBoundsPValueScale[2, ], 0.46017216, tolerance = 1e-07, label = paste0(result$futilityBoundsPValueScale[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result), NA)))
        expect_output(print(result)$show())
        invisible(capture.output(expect_error(summary(result), NA)))
        expect_output(summary(result)$show())
        resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
        expect_equal(resultCodeBased$directionUpper, result$directionUpper, tolerance = 1e-07)
        expect_equal(resultCodeBased$theta, result$theta, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxNumberOfSubjects, result$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(resultCodeBased$rejectPerStage, result$rejectPerStage, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityStop, result$futilityStop, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityPerStage, result$futilityPerStage, tolerance = 1e-07)
        expect_equal(resultCodeBased$earlyStop, result$earlyStop, tolerance = 1e-07)
        expect_equal(resultCodeBased$calendarTime, result$calendarTime, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedStudyDurationH1, result$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$numberOfSubjects, result$numberOfSubjects, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedNumberOfSubjectsH1, result$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$informationOverStages, result$informationOverStages, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH0, result$expectedInformationH0, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH01, result$expectedInformationH01, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH1, result$expectedInformationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxInformation, result$maxInformation, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityBoundsPValueScale, result$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(result), "character")
        df <- as.data.frame(result)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizePerStageVariableExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result <- getSampleSizeCounts(
        design = designGS1, thetaH0 = 0.9, lambda1 = seq(1.1, 1.15, 0.05),
        overDispersion = 0.2, lambda2 = 1.7, accrualTime = c(0, 2), maxNumberOfSubjects = 300
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result' with expected results
    expect_equal(result$directionUpper, c(FALSE, FALSE), label = paste0(result$directionUpper))
    expect_equal(result$theta, c(0.64705882, 0.67647059), tolerance = 1e-07, label = paste0(result$theta))
    expect_equal(result$rejectPerStage[1, ], 0.19514116, tolerance = 1e-07, label = paste0(result$rejectPerStage[1, ]))
    expect_equal(result$rejectPerStage[2, ], 0.37204423, tolerance = 1e-07, label = paste0(result$rejectPerStage[2, ]))
    expect_equal(result$rejectPerStage[3, ], 0.33281461, tolerance = 1e-07, label = paste0(result$rejectPerStage[3, ]))
    expect_equal(result$futilityStop, 0.015698246, tolerance = 1e-07, label = paste0(result$futilityStop))
    expect_equal(result$futilityPerStage[1, ], 0.01090105, tolerance = 1e-07, label = paste0(result$futilityPerStage[1, ]))
    expect_equal(result$futilityPerStage[2, ], 0.004797196, tolerance = 1e-07, label = paste0(result$futilityPerStage[2, ]))
    expect_equal(result$earlyStop, 0.58288364, tolerance = 1e-07, label = paste0(result$earlyStop))
    expect_equal(result$calendarTime[1, ], c(1.264719, 1.4673637), tolerance = 1e-07, label = paste0(result$calendarTime[1, ]))
    expect_equal(result$calendarTime[2, ], c(1.7456908, 2.0357161), tolerance = 1e-07, label = paste0(result$calendarTime[2, ]))
    expect_equal(result$calendarTime[3, ], c(2.4812416, 3.1679677), tolerance = 1e-07, label = paste0(result$calendarTime[3, ]))
    expect_equal(result$expectedStudyDurationH1, c(1.9534006, 2.3908922), tolerance = 1e-07, label = paste0(result$expectedStudyDurationH1))
    expect_equal(result$numberOfSubjects[1, ], c(190, 220), label = paste0(result$numberOfSubjects[1, ]))
    expect_equal(result$numberOfSubjects[2, ], c(262, 300), label = paste0(result$numberOfSubjects[2, ]))
    expect_equal(result$numberOfSubjects[3, ], c(300, 300), label = paste0(result$numberOfSubjects[3, ]))
    expect_equal(result$expectedNumberOfSubjectsH1, c(263.01538, 283.51662), tolerance = 1e-07, label = paste0(result$expectedNumberOfSubjectsH1))
    expect_equal(result$informationOverStages[1, ], c(32.942092, 43.998461), tolerance = 1e-07, label = paste0(result$informationOverStages[1, ]))
    expect_equal(result$informationOverStages[2, ], c(58.678102, 78.372259), tolerance = 1e-07, label = paste0(result$informationOverStages[2, ]))
    expect_equal(result$informationOverStages[3, ], c(102.94404, 137.49519), tolerance = 1e-07, label = paste0(result$informationOverStages[3, ]))
    expect_equal(result$expectedInformationH0, c(67.837688, 90.60608), tolerance = 1e-07, label = paste0(result$expectedInformationH0))
    expect_equal(result$expectedInformationH01, c(86.56733, 115.62196), tolerance = 1e-07, label = paste0(result$expectedInformationH01))
    expect_equal(result$expectedInformationH1, c(71.839444, 95.950948), tolerance = 1e-07, label = paste0(result$expectedInformationH1))
    expect_equal(result$maxInformation, c(102.94404, 137.49519), tolerance = 1e-07, label = paste0(result$maxInformation))
    expect_equal(result$futilityBoundsPValueScale[1, ], 0.65542174, tolerance = 1e-07, label = paste0(result$futilityBoundsPValueScale[1, ]))
    expect_equal(result$futilityBoundsPValueScale[2, ], 0.46017216, tolerance = 1e-07, label = paste0(result$futilityBoundsPValueScale[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result), NA)))
        expect_output(print(result)$show())
        invisible(capture.output(expect_error(summary(result), NA)))
        expect_output(summary(result)$show())
        resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
        expect_equal(resultCodeBased$directionUpper, result$directionUpper, tolerance = 1e-07)
        expect_equal(resultCodeBased$theta, result$theta, tolerance = 1e-07)
        expect_equal(resultCodeBased$rejectPerStage, result$rejectPerStage, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityStop, result$futilityStop, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityPerStage, result$futilityPerStage, tolerance = 1e-07)
        expect_equal(resultCodeBased$earlyStop, result$earlyStop, tolerance = 1e-07)
        expect_equal(resultCodeBased$calendarTime, result$calendarTime, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedStudyDurationH1, result$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$numberOfSubjects, result$numberOfSubjects, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedNumberOfSubjectsH1, result$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$informationOverStages, result$informationOverStages, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH0, result$expectedInformationH0, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH01, result$expectedInformationH01, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH1, result$expectedInformationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxInformation, result$maxInformation, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityBoundsPValueScale, result$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(result), "character")
        df <- as.data.frame(result)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizePerStageVariableExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result <- getSampleSizeCounts(
        design = designGS1, lambda1 = seq(1.05, 1.35, 0.15),
        lambda2 = 1.4, accrualTime = c(0, 10, 20), accrualIntensity = c(5, 10)
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result' with expected results
    expect_equal(result$directionUpper, c(FALSE, FALSE, FALSE), label = paste0(result$directionUpper))
    expect_equal(result$theta, c(0.75, 0.85714286, 0.96428571), tolerance = 1e-07, label = paste0(result$theta))
    expect_equal(result$maxNumberOfSubjects, c(150, 150, 150), label = paste0(result$maxNumberOfSubjects))
    expect_equal(result$rejectPerStage[1, ], 0.19514116, tolerance = 1e-07, label = paste0(result$rejectPerStage[1, ]))
    expect_equal(result$rejectPerStage[2, ], 0.37204423, tolerance = 1e-07, label = paste0(result$rejectPerStage[2, ]))
    expect_equal(result$rejectPerStage[3, ], 0.33281461, tolerance = 1e-07, label = paste0(result$rejectPerStage[3, ]))
    expect_equal(result$futilityStop, 0.015698246, tolerance = 1e-07, label = paste0(result$futilityStop))
    expect_equal(result$futilityPerStage[1, ], 0.01090105, tolerance = 1e-07, label = paste0(result$futilityPerStage[1, ]))
    expect_equal(result$futilityPerStage[2, ], 0.004797196, tolerance = 1e-07, label = paste0(result$futilityPerStage[2, ]))
    expect_equal(result$earlyStop, 0.58288364, tolerance = 1e-07, label = paste0(result$earlyStop))
    expect_equal(result$calendarTime[1, ], c(7.5513415, 13.27872, 64.307374), tolerance = 1e-07, label = paste0(result$calendarTime[1, ]))
    expect_equal(result$calendarTime[2, ], c(10.144294, 16.9121, 105.40688), tolerance = 1e-07, label = paste0(result$calendarTime[2, ]))
    expect_equal(result$calendarTime[3, ], c(13.085473, 21.432609, 176.09804), tolerance = 1e-07, label = paste0(result$calendarTime[3, ]))
    expect_equal(result$expectedStudyDurationH1, c(10.83685, 18.049049, 126.42509), tolerance = 1e-07, label = paste0(result$expectedStudyDurationH1))
    expect_equal(result$numberOfSubjects[1, ], c(38, 82, 150), label = paste0(result$numberOfSubjects[1, ]))
    expect_equal(result$numberOfSubjects[2, ], c(52, 118, 150), label = paste0(result$numberOfSubjects[2, ]))
    expect_equal(result$numberOfSubjects[3, ], c(80, 150, 150), label = paste0(result$numberOfSubjects[3, ]))
    expect_equal(result$expectedNumberOfSubjectsH1, c(60.794667, 123.9302, 150), tolerance = 1e-07, label = paste0(result$expectedNumberOfSubjectsH1))
    expect_equal(result$informationOverStages[1, ], c(43.335293, 150.9303, 2711.671), tolerance = 1e-07, label = paste0(result$informationOverStages[1, ]))
    expect_equal(result$informationOverStages[2, ], c(77.190991, 268.8446, 4830.164), tolerance = 1e-07, label = paste0(result$informationOverStages[2, ]))
    expect_equal(result$informationOverStages[3, ], c(135.42279, 471.6572, 8473.9719), tolerance = 1e-07, label = paste0(result$informationOverStages[3, ]))
    expect_equal(result$expectedInformationH0, c(89.240417, 310.81094, 5584.1471), tolerance = 1e-07, label = paste0(result$expectedInformationH0))
    expect_equal(result$expectedInformationH01, c(113.87925, 396.62427, 7125.9019), tolerance = 1e-07, label = paste0(result$expectedInformationH01))
    expect_equal(result$expectedInformationH1, c(94.504724, 329.14573, 5913.5569), tolerance = 1e-07, label = paste0(result$expectedInformationH1))
    expect_equal(result$maxInformation, c(135.42279, 471.6572, 8473.9719), tolerance = 1e-07, label = paste0(result$maxInformation))
    expect_equal(result$futilityBoundsPValueScale[1, ], 0.65542174, tolerance = 1e-07, label = paste0(result$futilityBoundsPValueScale[1, ]))
    expect_equal(result$futilityBoundsPValueScale[2, ], 0.46017216, tolerance = 1e-07, label = paste0(result$futilityBoundsPValueScale[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result), NA)))
        expect_output(print(result)$show())
        invisible(capture.output(expect_error(summary(result), NA)))
        expect_output(summary(result)$show())
        resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
        expect_equal(resultCodeBased$directionUpper, result$directionUpper, tolerance = 1e-07)
        expect_equal(resultCodeBased$theta, result$theta, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxNumberOfSubjects, result$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(resultCodeBased$rejectPerStage, result$rejectPerStage, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityStop, result$futilityStop, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityPerStage, result$futilityPerStage, tolerance = 1e-07)
        expect_equal(resultCodeBased$earlyStop, result$earlyStop, tolerance = 1e-07)
        expect_equal(resultCodeBased$calendarTime, result$calendarTime, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedStudyDurationH1, result$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$numberOfSubjects, result$numberOfSubjects, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedNumberOfSubjectsH1, result$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$informationOverStages, result$informationOverStages, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH0, result$expectedInformationH0, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH01, result$expectedInformationH01, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH1, result$expectedInformationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxInformation, result$maxInformation, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityBoundsPValueScale, result$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(result), "character")
        df <- as.data.frame(result)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizeCountsFixedExposureSingleStage}
    # @refFS[Formula]{fs:sampleSizePerStageFixedExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    # @refFS[Formula]{fs:sampleSizeCountsFixedExposureOptimumAllocationRatio}
    result <- getSampleSizeCounts(
        design = designGS1, lambda2 = 0.234, lambda1 = c(0.1, 0.2),
        allocationRatioPlanned = 0, accrualTime = 7, fixedExposureTime = 1
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result' with expected results
    expect_equal(result$allocationRatioPlanned, c(1.5297048, 1.0816655), tolerance = 1e-07, label = paste0(result$allocationRatioPlanned))
    expect_equal(result$directionUpper, c(FALSE, FALSE), label = paste0(result$directionUpper))
    expect_equal(result$theta, c(0.42735043, 0.85470085), tolerance = 1e-07, label = paste0(result$theta))
    expect_equal(result$maxNumberOfSubjects, c(425, 8421), label = paste0(result$maxNumberOfSubjects))
    expect_equal(result$maxNumberOfSubjects1, c(257, 4376), label = paste0(result$maxNumberOfSubjects1))
    expect_equal(result$maxNumberOfSubjects2, c(168, 4045), label = paste0(result$maxNumberOfSubjects2))
    expect_equal(result$rejectPerStage[1, ], 0.19514116, tolerance = 1e-07, label = paste0(result$rejectPerStage[1, ]))
    expect_equal(result$rejectPerStage[2, ], 0.37204423, tolerance = 1e-07, label = paste0(result$rejectPerStage[2, ]))
    expect_equal(result$rejectPerStage[3, ], 0.33281461, tolerance = 1e-07, label = paste0(result$rejectPerStage[3, ]))
    expect_equal(result$futilityStop, 0.015698246, tolerance = 1e-07, label = paste0(result$futilityStop))
    expect_equal(result$futilityPerStage[1, ], 0.01090105, tolerance = 1e-07, label = paste0(result$futilityPerStage[1, ]))
    expect_equal(result$futilityPerStage[2, ], 0.004797196, tolerance = 1e-07, label = paste0(result$futilityPerStage[2, ]))
    expect_equal(result$earlyStop, 0.58288364, tolerance = 1e-07, label = paste0(result$earlyStop))
    expect_equal(result$calendarTime[1, ], c(2.7292182, 2.7393892), tolerance = 1e-07, label = paste0(result$calendarTime[1, ]))
    expect_equal(result$calendarTime[2, ], c(4.4837277, 4.4895615), tolerance = 1e-07, label = paste0(result$calendarTime[2, ]))
    expect_equal(result$calendarTime[3, ], c(8, 8), label = paste0(result$calendarTime[3, ]))
    expect_equal(result$expectedStudyDurationH1, c(5.5889194, 5.5932135), tolerance = 1e-07, label = paste0(result$expectedStudyDurationH1))
    expect_equal(result$numberOfSubjects[1, ], c(166, 3296), label = paste0(result$numberOfSubjects[1, ]))
    expect_equal(result$numberOfSubjects[2, ], c(271, 5400), label = paste0(result$numberOfSubjects[2, ]))
    expect_equal(result$numberOfSubjects[3, ], c(425, 8421), label = paste0(result$numberOfSubjects[3, ]))
    expect_equal(result$expectedNumberOfSubjectsH1, c(313.60149, 6226.5957), tolerance = 1e-07, label = paste0(result$expectedNumberOfSubjectsH1))
    expect_equal(result$informationOverStages[1, ], c(4.9622115, 145.49474), tolerance = 1e-07, label = paste0(result$informationOverStages[1, ]))
    expect_equal(result$informationOverStages[2, ], c(8.8389391, 259.1625), tolerance = 1e-07, label = paste0(result$informationOverStages[2, ]))
    expect_equal(result$informationOverStages[3, ], c(15.506911, 454.67106), tolerance = 1e-07, label = paste0(result$informationOverStages[3, ]))
    expect_equal(result$expectedInformationH0, c(10.218688, 299.61748), tolerance = 1e-07, label = paste0(result$expectedInformationH0))
    expect_equal(result$expectedInformationH01, c(13.040016, 382.34035), tolerance = 1e-07, label = paste0(result$expectedInformationH01))
    expect_equal(result$expectedInformationH1, c(10.82149, 317.29196), tolerance = 1e-07, label = paste0(result$expectedInformationH1))
    expect_equal(result$maxInformation, c(15.506911, 454.67106), tolerance = 1e-07, label = paste0(result$maxInformation))
    expect_equal(result$futilityBoundsPValueScale[1, ], 0.65542174, tolerance = 1e-07, label = paste0(result$futilityBoundsPValueScale[1, ]))
    expect_equal(result$futilityBoundsPValueScale[2, ], 0.46017216, tolerance = 1e-07, label = paste0(result$futilityBoundsPValueScale[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result), NA)))
        expect_output(print(result)$show())
        invisible(capture.output(expect_error(summary(result), NA)))
        expect_output(summary(result)$show())
        resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
        expect_equal(resultCodeBased$allocationRatioPlanned, result$allocationRatioPlanned, tolerance = 1e-07)
        expect_equal(resultCodeBased$directionUpper, result$directionUpper, tolerance = 1e-07)
        expect_equal(resultCodeBased$theta, result$theta, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxNumberOfSubjects, result$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxNumberOfSubjects1, result$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxNumberOfSubjects2, result$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(resultCodeBased$rejectPerStage, result$rejectPerStage, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityStop, result$futilityStop, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityPerStage, result$futilityPerStage, tolerance = 1e-07)
        expect_equal(resultCodeBased$earlyStop, result$earlyStop, tolerance = 1e-07)
        expect_equal(resultCodeBased$calendarTime, result$calendarTime, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedStudyDurationH1, result$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$numberOfSubjects, result$numberOfSubjects, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedNumberOfSubjectsH1, result$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$informationOverStages, result$informationOverStages, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH0, result$expectedInformationH0, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH01, result$expectedInformationH01, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH1, result$expectedInformationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxInformation, result$maxInformation, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityBoundsPValueScale, result$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(result), "character")
        df <- as.data.frame(result)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizePerStageVariableExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    # @refFS[Formula]{fs:sampleSizeCountsVariableExposureOptimumAllocationRatio}
    result <- getSampleSizeCounts(
        design = designGS1, lambda2 = 0.234, lambda1 = c(0.1, 0.2),
        allocationRatioPlanned = 0, accrualTime = 7, followUpTime = 1
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result' with expected results
    expect_equal(result$allocationRatioPlanned, c(1.1591008, 1.1237283), tolerance = 1e-07, label = paste0(result$allocationRatioPlanned))
    expect_equal(result$directionUpper, c(FALSE, FALSE), label = paste0(result$directionUpper))
    expect_equal(result$theta, c(0.42735043, 0.85470085), tolerance = 1e-07, label = paste0(result$theta))
    expect_equal(result$maxNumberOfSubjects, c(96, 1872), label = paste0(result$maxNumberOfSubjects))
    expect_equal(result$maxNumberOfSubjects1, c(52, 991), label = paste0(result$maxNumberOfSubjects1))
    expect_equal(result$maxNumberOfSubjects2, c(44, 881), label = paste0(result$maxNumberOfSubjects2))
    expect_equal(result$rejectPerStage[1, ], 0.19514116, tolerance = 1e-07, label = paste0(result$rejectPerStage[1, ]))
    expect_equal(result$rejectPerStage[2, ], 0.37204423, tolerance = 1e-07, label = paste0(result$rejectPerStage[2, ]))
    expect_equal(result$rejectPerStage[3, ], 0.33281461, tolerance = 1e-07, label = paste0(result$rejectPerStage[3, ]))
    expect_equal(result$futilityStop, 0.015698246, tolerance = 1e-07, label = paste0(result$futilityStop))
    expect_equal(result$futilityPerStage[1, ], 0.01090105, tolerance = 1e-07, label = paste0(result$futilityPerStage[1, ]))
    expect_equal(result$futilityPerStage[2, ], 0.004797196, tolerance = 1e-07, label = paste0(result$futilityPerStage[2, ]))
    expect_equal(result$earlyStop, 0.58288364, tolerance = 1e-07, label = paste0(result$earlyStop))
    expect_equal(result$calendarTime[1, ], c(4.457672, 4.4883905), tolerance = 1e-07, label = paste0(result$calendarTime[1, ]))
    expect_equal(result$calendarTime[2, ], c(5.9737692, 5.9916173), tolerance = 1e-07, label = paste0(result$calendarTime[2, ]))
    expect_equal(result$calendarTime[3, ], c(8, 8), label = paste0(result$calendarTime[3, ]))
    expect_equal(result$expectedStudyDurationH1, c(6.5065632, 6.5196184), tolerance = 1e-07, label = paste0(result$expectedStudyDurationH1))
    expect_equal(result$numberOfSubjects[1, ], c(61, 1200), label = paste0(result$numberOfSubjects[1, ]))
    expect_equal(result$numberOfSubjects[2, ], c(81, 1602), label = paste0(result$numberOfSubjects[2, ]))
    expect_equal(result$numberOfSubjects[3, ], c(96, 1872), label = paste0(result$numberOfSubjects[3, ]))
    expect_equal(result$expectedNumberOfSubjectsH1, c(83.135901, 1631.7924), tolerance = 1e-07, label = paste0(result$expectedNumberOfSubjectsH1))
    expect_equal(result$informationOverStages[1, ], c(4.9622115, 145.49474), tolerance = 1e-07, label = paste0(result$informationOverStages[1, ]))
    expect_equal(result$informationOverStages[2, ], c(8.8389391, 259.1625), tolerance = 1e-07, label = paste0(result$informationOverStages[2, ]))
    expect_equal(result$informationOverStages[3, ], c(15.506911, 454.67106), tolerance = 1e-07, label = paste0(result$informationOverStages[3, ]))
    expect_equal(result$expectedInformationH0, c(10.218688, 299.61748), tolerance = 1e-07, label = paste0(result$expectedInformationH0))
    expect_equal(result$expectedInformationH01, c(13.040016, 382.34035), tolerance = 1e-07, label = paste0(result$expectedInformationH01))
    expect_equal(result$expectedInformationH1, c(10.82149, 317.29196), tolerance = 1e-07, label = paste0(result$expectedInformationH1))
    expect_equal(result$maxInformation, c(15.506911, 454.67106), tolerance = 1e-07, label = paste0(result$maxInformation))
    expect_equal(result$futilityBoundsPValueScale[1, ], 0.65542174, tolerance = 1e-07, label = paste0(result$futilityBoundsPValueScale[1, ]))
    expect_equal(result$futilityBoundsPValueScale[2, ], 0.46017216, tolerance = 1e-07, label = paste0(result$futilityBoundsPValueScale[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result), NA)))
        expect_output(print(result)$show())
        invisible(capture.output(expect_error(summary(result), NA)))
        expect_output(summary(result)$show())
        resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
        expect_equal(resultCodeBased$allocationRatioPlanned, result$allocationRatioPlanned, tolerance = 1e-07)
        expect_equal(resultCodeBased$directionUpper, result$directionUpper, tolerance = 1e-07)
        expect_equal(resultCodeBased$theta, result$theta, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxNumberOfSubjects, result$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxNumberOfSubjects1, result$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxNumberOfSubjects2, result$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(resultCodeBased$rejectPerStage, result$rejectPerStage, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityStop, result$futilityStop, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityPerStage, result$futilityPerStage, tolerance = 1e-07)
        expect_equal(resultCodeBased$earlyStop, result$earlyStop, tolerance = 1e-07)
        expect_equal(resultCodeBased$calendarTime, result$calendarTime, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedStudyDurationH1, result$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$numberOfSubjects, result$numberOfSubjects, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedNumberOfSubjectsH1, result$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$informationOverStages, result$informationOverStages, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH0, result$expectedInformationH0, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH01, result$expectedInformationH01, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH1, result$expectedInformationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxInformation, result$maxInformation, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityBoundsPValueScale, result$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(result), "character")
        df <- as.data.frame(result)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeCounts': Sample size calculation of testing count data for two-sided group sequential design", {
    .skipTestIfDisabled()

    designGS2 <- getDesignGroupSequential(
        informationRates = c(0.32, 0.57, 1),
        sided = 2, beta = 0.2, typeOfDesign = "WT", deltaWT = 0.1
    )

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizeCountsFixedExposureSingleStage}
    # @refFS[Formula]{fs:sampleSizePerStageFixedExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result <- getSampleSizeCounts(
        design = designGS2, lambda = 0.234, theta = 0.7,
        overDispersion = 0.71, accrualTime = 7, fixedExposureTime = 1
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result' with expected results
    expect_equal(result$directionUpper, FALSE, label = paste0(result$directionUpper))
    expect_equal(result$lambda1, 0.19270588, tolerance = 1e-07, label = paste0(result$lambda1))
    expect_equal(result$lambda2, 0.27529412, tolerance = 1e-07, label = paste0(result$lambda2))
    expect_equal(result$maxNumberOfSubjects, 1554, label = paste0(result$maxNumberOfSubjects))
    expect_equal(result$rejectPerStage[1, ], 0.033479189, tolerance = 1e-07, label = paste0(result$rejectPerStage[1, ]))
    expect_equal(result$rejectPerStage[2, ], 0.27519472, tolerance = 1e-07, label = paste0(result$rejectPerStage[2, ]))
    expect_equal(result$rejectPerStage[3, ], 0.49132609, tolerance = 1e-07, label = paste0(result$rejectPerStage[3, ]))
    expect_equal(result$earlyStop, 0.30867391, tolerance = 1e-07, label = paste0(result$earlyStop))
    expect_equal(result$calendarTime[1, ], 2.7108183, tolerance = 1e-07, label = paste0(result$calendarTime[1, ]))
    expect_equal(result$calendarTime[2, ], 4.4609941, tolerance = 1e-07, label = paste0(result$calendarTime[2, ]))
    expect_equal(result$calendarTime[3, ], 8, label = paste0(result$calendarTime[3, ]))
    expect_equal(result$expectedStudyDurationH1, 6.8490067, tolerance = 1e-07, label = paste0(result$expectedStudyDurationH1))
    expect_equal(result$numberOfSubjects[1, ], 602, label = paste0(result$numberOfSubjects[1, ]))
    expect_equal(result$numberOfSubjects[2, ], 990, label = paste0(result$numberOfSubjects[2, ]))
    expect_equal(result$numberOfSubjects[3, ], 1554, label = paste0(result$numberOfSubjects[3, ]))
    expect_equal(result$expectedNumberOfSubjectsH1, 1366.918, tolerance = 1e-07, label = paste0(result$expectedNumberOfSubjectsH1))
    expect_equal(result$informationOverStages[1, ], 24.248379, tolerance = 1e-07, label = paste0(result$informationOverStages[1, ]))
    expect_equal(result$informationOverStages[2, ], 43.192425, tolerance = 1e-07, label = paste0(result$informationOverStages[2, ]))
    expect_equal(result$informationOverStages[3, ], 75.776183, tolerance = 1e-07, label = paste0(result$informationOverStages[3, ]))
    expect_equal(result$expectedInformationH0, 75.621421, tolerance = 1e-07, label = paste0(result$expectedInformationH0))
    expect_equal(result$expectedInformationH01, 74.163254, tolerance = 1e-07, label = paste0(result$expectedInformationH01))
    expect_equal(result$expectedInformationH1, 65.08419, tolerance = 1e-07, label = paste0(result$expectedInformationH1))
    expect_equal(result$maxInformation, 75.776183, tolerance = 1e-07, label = paste0(result$maxInformation))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result), NA)))
        expect_output(print(result)$show())
        invisible(capture.output(expect_error(summary(result), NA)))
        expect_output(summary(result)$show())
        resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
        expect_equal(resultCodeBased$directionUpper, result$directionUpper, tolerance = 1e-07)
        expect_equal(resultCodeBased$lambda1, result$lambda1, tolerance = 1e-07)
        expect_equal(resultCodeBased$lambda2, result$lambda2, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxNumberOfSubjects, result$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(resultCodeBased$rejectPerStage, result$rejectPerStage, tolerance = 1e-07)
        expect_equal(resultCodeBased$earlyStop, result$earlyStop, tolerance = 1e-07)
        expect_equal(resultCodeBased$calendarTime, result$calendarTime, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedStudyDurationH1, result$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$numberOfSubjects, result$numberOfSubjects, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedNumberOfSubjectsH1, result$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$informationOverStages, result$informationOverStages, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH0, result$expectedInformationH0, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH01, result$expectedInformationH01, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH1, result$expectedInformationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxInformation, result$maxInformation, tolerance = 1e-07)
        expect_type(names(result), "character")
        df <- as.data.frame(result)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizePerStageVariableExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result <- getSampleSizeCounts(
        design = designGS2, lambda = 0.234, theta = 0.7,
        overDispersion = 0.71, accrualTime = 7, followUpTime = 1
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result' with expected results
    expect_equal(result$directionUpper, FALSE, label = paste0(result$directionUpper))
    expect_equal(result$lambda1, 0.19270588, tolerance = 1e-07, label = paste0(result$lambda1))
    expect_equal(result$lambda2, 0.27529412, tolerance = 1e-07, label = paste0(result$lambda2))
    expect_equal(result$maxNumberOfSubjects, 542, label = paste0(result$maxNumberOfSubjects))
    expect_equal(result$rejectPerStage[1, ], 0.033479189, tolerance = 1e-07, label = paste0(result$rejectPerStage[1, ]))
    expect_equal(result$rejectPerStage[2, ], 0.27519472, tolerance = 1e-07, label = paste0(result$rejectPerStage[2, ]))
    expect_equal(result$rejectPerStage[3, ], 0.49132609, tolerance = 1e-07, label = paste0(result$rejectPerStage[3, ]))
    expect_equal(result$earlyStop, 0.30867391, tolerance = 1e-07, label = paste0(result$earlyStop))
    expect_equal(result$calendarTime[1, ], 3.9368401, tolerance = 1e-07, label = paste0(result$calendarTime[1, ]))
    expect_equal(result$calendarTime[2, ], 5.5484451, tolerance = 1e-07, label = paste0(result$calendarTime[2, ]))
    expect_equal(result$calendarTime[3, ], 8, label = paste0(result$calendarTime[3, ]))
    expect_equal(result$expectedStudyDurationH1, 7.1893137, tolerance = 1e-07, label = paste0(result$expectedStudyDurationH1))
    expect_equal(result$numberOfSubjects[1, ], 304, label = paste0(result$numberOfSubjects[1, ]))
    expect_equal(result$numberOfSubjects[2, ], 430, label = paste0(result$numberOfSubjects[2, ]))
    expect_equal(result$numberOfSubjects[3, ], 542, label = paste0(result$numberOfSubjects[3, ]))
    expect_equal(result$expectedNumberOfSubjectsH1, 503.21014, tolerance = 1e-07, label = paste0(result$expectedNumberOfSubjectsH1))
    expect_equal(result$informationOverStages[1, ], 24.248379, tolerance = 1e-07, label = paste0(result$informationOverStages[1, ]))
    expect_equal(result$informationOverStages[2, ], 43.192425, tolerance = 1e-07, label = paste0(result$informationOverStages[2, ]))
    expect_equal(result$informationOverStages[3, ], 75.776183, tolerance = 1e-07, label = paste0(result$informationOverStages[3, ]))
    expect_equal(result$expectedInformationH0, 75.621421, tolerance = 1e-07, label = paste0(result$expectedInformationH0))
    expect_equal(result$expectedInformationH01, 74.163254, tolerance = 1e-07, label = paste0(result$expectedInformationH01))
    expect_equal(result$expectedInformationH1, 65.08419, tolerance = 1e-07, label = paste0(result$expectedInformationH1))
    expect_equal(result$maxInformation, 75.776183, tolerance = 1e-07, label = paste0(result$maxInformation))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result), NA)))
        expect_output(print(result)$show())
        invisible(capture.output(expect_error(summary(result), NA)))
        expect_output(summary(result)$show())
        resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
        expect_equal(resultCodeBased$directionUpper, result$directionUpper, tolerance = 1e-07)
        expect_equal(resultCodeBased$lambda1, result$lambda1, tolerance = 1e-07)
        expect_equal(resultCodeBased$lambda2, result$lambda2, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxNumberOfSubjects, result$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(resultCodeBased$rejectPerStage, result$rejectPerStage, tolerance = 1e-07)
        expect_equal(resultCodeBased$earlyStop, result$earlyStop, tolerance = 1e-07)
        expect_equal(resultCodeBased$calendarTime, result$calendarTime, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedStudyDurationH1, result$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$numberOfSubjects, result$numberOfSubjects, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedNumberOfSubjectsH1, result$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$informationOverStages, result$informationOverStages, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH0, result$expectedInformationH0, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH01, result$expectedInformationH01, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH1, result$expectedInformationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxInformation, result$maxInformation, tolerance = 1e-07)
        expect_type(names(result), "character")
        df <- as.data.frame(result)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizeCountsFixedExposureSingleStage}
    # @refFS[Formula]{fs:sampleSizePerStageFixedExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result <- getSampleSizeCounts(
        design = designGS2, lambda2 = 0.02, theta = c(0.7, 0.8),
        overDispersion = 0.71, accrualTime = 7, fixedExposureTime = 2
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result' with expected results
    expect_equal(result$directionUpper, c(FALSE, FALSE), label = paste0(result$directionUpper))
    expect_equal(result$lambda1, c(0.014, 0.016), tolerance = 1e-07, label = paste0(result$lambda1))
    expect_equal(result$maxNumberOfSubjects, c(9418, 22332), label = paste0(result$maxNumberOfSubjects))
    expect_equal(result$rejectPerStage[1, ], 0.033479189, tolerance = 1e-07, label = paste0(result$rejectPerStage[1, ]))
    expect_equal(result$rejectPerStage[2, ], 0.27519472, tolerance = 1e-07, label = paste0(result$rejectPerStage[2, ]))
    expect_equal(result$rejectPerStage[3, ], 0.49132609, tolerance = 1e-07, label = paste0(result$rejectPerStage[3, ]))
    expect_equal(result$earlyStop, 0.30867391, tolerance = 1e-07, label = paste0(result$earlyStop))
    expect_equal(result$calendarTime[1, ], c(3.2316907, 3.2313779), tolerance = 1e-07, label = paste0(result$calendarTime[1, ]))
    expect_equal(result$calendarTime[2, ], c(4.9818015, 4.9813797), tolerance = 1e-07, label = paste0(result$calendarTime[2, ]))
    expect_equal(result$calendarTime[3, ], c(9, 9), label = paste0(result$calendarTime[3, ]))
    expect_equal(result$expectedStudyDurationH1, c(7.7010947, 7.7009681), tolerance = 1e-07, label = paste0(result$expectedStudyDurationH1))
    expect_equal(result$numberOfSubjects[1, ], c(4348, 10310), label = paste0(result$numberOfSubjects[1, ]))
    expect_equal(result$numberOfSubjects[2, ], c(6702, 15892), label = paste0(result$numberOfSubjects[2, ]))
    expect_equal(result$numberOfSubjects[3, ], c(9418, 22332), label = paste0(result$numberOfSubjects[3, ]))
    expect_equal(result$expectedNumberOfSubjectsH1, c(8500.8316, 20157.259), tolerance = 1e-07, label = paste0(result$expectedNumberOfSubjectsH1))
    expect_equal(result$informationOverStages[1, ], c(24.248379, 61.952556), tolerance = 1e-07, label = paste0(result$informationOverStages[1, ]))
    expect_equal(result$informationOverStages[2, ], c(43.192425, 110.35299), tolerance = 1e-07, label = paste0(result$informationOverStages[2, ]))
    expect_equal(result$informationOverStages[3, ], c(75.776183, 193.60174), tolerance = 1e-07, label = paste0(result$informationOverStages[3, ]))
    expect_equal(result$expectedInformationH0, c(75.621421, 193.20633), tolerance = 1e-07, label = paste0(result$expectedInformationH0))
    expect_equal(result$expectedInformationH01, c(74.163254, 189.48084), tolerance = 1e-07, label = paste0(result$expectedInformationH01))
    expect_equal(result$expectedInformationH1, c(65.08419, 166.2846), tolerance = 1e-07, label = paste0(result$expectedInformationH1))
    expect_equal(result$maxInformation, c(75.776183, 193.60174), tolerance = 1e-07, label = paste0(result$maxInformation))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result), NA)))
        expect_output(print(result)$show())
        invisible(capture.output(expect_error(summary(result), NA)))
        expect_output(summary(result)$show())
        resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
        expect_equal(resultCodeBased$directionUpper, result$directionUpper, tolerance = 1e-07)
        expect_equal(resultCodeBased$lambda1, result$lambda1, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxNumberOfSubjects, result$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(resultCodeBased$rejectPerStage, result$rejectPerStage, tolerance = 1e-07)
        expect_equal(resultCodeBased$earlyStop, result$earlyStop, tolerance = 1e-07)
        expect_equal(resultCodeBased$calendarTime, result$calendarTime, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedStudyDurationH1, result$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$numberOfSubjects, result$numberOfSubjects, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedNumberOfSubjectsH1, result$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$informationOverStages, result$informationOverStages, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH0, result$expectedInformationH0, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH01, result$expectedInformationH01, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH1, result$expectedInformationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxInformation, result$maxInformation, tolerance = 1e-07)
        expect_type(names(result), "character")
        df <- as.data.frame(result)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizePerStageVariableExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result <- getSampleSizeCounts(
        design = designGS2, lambda1 = seq(1.05, 1.35, 0.15),
        lambda2 = 1.4, accrualTime = c(0, 20), allocationRatioPlanned = 4, maxNumberOfSubjects = 100
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result' with expected results
    expect_equal(result$directionUpper, c(FALSE, FALSE, FALSE), label = paste0(result$directionUpper))
    expect_equal(result$theta, c(0.75, 0.85714286, 0.96428571), tolerance = 1e-07, label = paste0(result$theta))
    expect_equal(result$maxNumberOfSubjects1, c(80, 80, 80), label = paste0(result$maxNumberOfSubjects1))
    expect_equal(result$maxNumberOfSubjects2, c(20, 20, 20), label = paste0(result$maxNumberOfSubjects2))
    expect_equal(result$rejectPerStage[1, ], 0.033479189, tolerance = 1e-07, label = paste0(result$rejectPerStage[1, ]))
    expect_equal(result$rejectPerStage[2, ], 0.27519472, tolerance = 1e-07, label = paste0(result$rejectPerStage[2, ]))
    expect_equal(result$rejectPerStage[3, ], 0.49132609, tolerance = 1e-07, label = paste0(result$rejectPerStage[3, ]))
    expect_equal(result$earlyStop, 0.30867391, tolerance = 1e-07, label = paste0(result$earlyStop))
    expect_equal(result$calendarTime[1, ], c(8.1833157, 15.375371, 114.89498), tolerance = 1e-07, label = paste0(result$calendarTime[1, ]))
    expect_equal(result$calendarTime[2, ], c(11.057576, 20.667295, 196.84418), tolerance = 1e-07, label = paste0(result$calendarTime[2, ]))
    expect_equal(result$calendarTime[3, ], c(14.788602, 28.714553, 337.7968), tolerance = 1e-07, label = paste0(result$calendarTime[3, ]))
    expect_equal(result$expectedStudyDurationH1, c(13.540704, 26.053405, 291.54481), tolerance = 1e-07, label = paste0(result$expectedStudyDurationH1))
    expect_equal(result$numberOfSubjects[1, ], c(41, 76, 100), label = paste0(result$numberOfSubjects[1, ]))
    expect_equal(result$numberOfSubjects[2, ], c(55, 100, 100), label = paste0(result$numberOfSubjects[2, ]))
    expect_equal(result$numberOfSubjects[3, ], c(74, 100, 100), label = paste0(result$numberOfSubjects[3, ]))
    expect_equal(result$expectedNumberOfSubjectsH1, c(67.666487, 99.196499, 100), tolerance = 1e-07, label = paste0(result$expectedNumberOfSubjectsH1))
    expect_equal(result$informationOverStages[1, ], c(37.273683, 129.81863, 2332.3707), tolerance = 1e-07, label = paste0(result$informationOverStages[1, ]))
    expect_equal(result$informationOverStages[2, ], c(66.393749, 231.23943, 4154.5353), tolerance = 1e-07, label = paste0(result$informationOverStages[2, ]))
    expect_equal(result$informationOverStages[3, ], c(116.48026, 405.68322, 7288.6583), tolerance = 1e-07, label = paste0(result$informationOverStages[3, ]))
    expect_equal(result$expectedInformationH0, c(116.24237, 404.85466, 7273.7722), tolerance = 1e-07, label = paste0(result$expectedInformationH0))
    expect_equal(result$expectedInformationH01, c(114.00093, 397.04807, 7133.516), tolerance = 1e-07, label = paste0(result$expectedInformationH01))
    expect_equal(result$expectedInformationH1, c(100.04494, 348.44146, 6260.2312), tolerance = 1e-07, label = paste0(result$expectedInformationH1))
    expect_equal(result$maxInformation, c(116.48026, 405.68322, 7288.6583), tolerance = 1e-07, label = paste0(result$maxInformation))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result), NA)))
        expect_output(print(result)$show())
        invisible(capture.output(expect_error(summary(result), NA)))
        expect_output(summary(result)$show())
        resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
        expect_equal(resultCodeBased$directionUpper, result$directionUpper, tolerance = 1e-07)
        expect_equal(resultCodeBased$theta, result$theta, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxNumberOfSubjects1, result$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxNumberOfSubjects2, result$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(resultCodeBased$rejectPerStage, result$rejectPerStage, tolerance = 1e-07)
        expect_equal(resultCodeBased$earlyStop, result$earlyStop, tolerance = 1e-07)
        expect_equal(resultCodeBased$calendarTime, result$calendarTime, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedStudyDurationH1, result$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$numberOfSubjects, result$numberOfSubjects, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedNumberOfSubjectsH1, result$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$informationOverStages, result$informationOverStages, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH0, result$expectedInformationH0, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH01, result$expectedInformationH01, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH1, result$expectedInformationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxInformation, result$maxInformation, tolerance = 1e-07)
        expect_type(names(result), "character")
        df <- as.data.frame(result)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizePerStageVariableExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result <- getSampleSizeCounts(
        design = designGS2, lambda1 = seq(1.05, 1.35, 0.15),
        lambda2 = 1.4, accrualTime = c(0, 10, 20), accrualIntensity = c(5, 10)
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result' with expected results
    expect_equal(result$directionUpper, c(FALSE, FALSE, FALSE), label = paste0(result$directionUpper))
    expect_equal(result$theta, c(0.75, 0.85714286, 0.96428571), tolerance = 1e-07, label = paste0(result$theta))
    expect_equal(result$maxNumberOfSubjects, c(150, 150, 150), label = paste0(result$maxNumberOfSubjects))
    expect_equal(result$rejectPerStage[1, ], 0.033479189, tolerance = 1e-07, label = paste0(result$rejectPerStage[1, ]))
    expect_equal(result$rejectPerStage[2, ], 0.27519472, tolerance = 1e-07, label = paste0(result$rejectPerStage[2, ]))
    expect_equal(result$rejectPerStage[3, ], 0.49132609, tolerance = 1e-07, label = paste0(result$rejectPerStage[3, ]))
    expect_equal(result$earlyStop, 0.30867391, tolerance = 1e-07, label = paste0(result$earlyStop))
    expect_equal(result$calendarTime[1, ], c(6.987616, 12.444445, 56.948814), tolerance = 1e-07, label = paste0(result$calendarTime[1, ]))
    expect_equal(result$calendarTime[2, ], c(9.3944745, 15.885169, 92.299449), tolerance = 1e-07, label = paste0(result$calendarTime[2, ]))
    expect_equal(result$calendarTime[3, ], c(12.259611, 20.071241, 153.10254), tolerance = 1e-07, label = paste0(result$calendarTime[3, ]))
    expect_equal(result$expectedStudyDurationH1, c(11.294639, 18.663917, 133.1507), tolerance = 1e-07, label = paste0(result$expectedStudyDurationH1))
    expect_equal(result$numberOfSubjects[1, ], c(34, 74, 150), label = paste0(result$numberOfSubjects[1, ]))
    expect_equal(result$numberOfSubjects[2, ], c(46, 108, 150), label = paste0(result$numberOfSubjects[2, ]))
    expect_equal(result$numberOfSubjects[3, ], c(72, 150, 150), label = paste0(result$numberOfSubjects[3, ]))
    expect_equal(result$expectedNumberOfSubjectsH1, c(63.572728, 135.8974, 150), tolerance = 1e-07, label = paste0(result$expectedNumberOfSubjectsH1))
    expect_equal(result$informationOverStages[1, ], c(37.273683, 129.81863, 2332.3707), tolerance = 1e-07, label = paste0(result$informationOverStages[1, ]))
    expect_equal(result$informationOverStages[2, ], c(66.393749, 231.23943, 4154.5353), tolerance = 1e-07, label = paste0(result$informationOverStages[2, ]))
    expect_equal(result$informationOverStages[3, ], c(116.48026, 405.68322, 7288.6583), tolerance = 1e-07, label = paste0(result$informationOverStages[3, ]))
    expect_equal(result$expectedInformationH0, c(116.24237, 404.85466, 7273.7722), tolerance = 1e-07, label = paste0(result$expectedInformationH0))
    expect_equal(result$expectedInformationH01, c(114.00093, 397.04807, 7133.516), tolerance = 1e-07, label = paste0(result$expectedInformationH01))
    expect_equal(result$expectedInformationH1, c(100.04494, 348.44146, 6260.2312), tolerance = 1e-07, label = paste0(result$expectedInformationH1))
    expect_equal(result$maxInformation, c(116.48026, 405.68322, 7288.6583), tolerance = 1e-07, label = paste0(result$maxInformation))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result), NA)))
        expect_output(print(result)$show())
        invisible(capture.output(expect_error(summary(result), NA)))
        expect_output(summary(result)$show())
        resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
        expect_equal(resultCodeBased$directionUpper, result$directionUpper, tolerance = 1e-07)
        expect_equal(resultCodeBased$theta, result$theta, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxNumberOfSubjects, result$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(resultCodeBased$rejectPerStage, result$rejectPerStage, tolerance = 1e-07)
        expect_equal(resultCodeBased$earlyStop, result$earlyStop, tolerance = 1e-07)
        expect_equal(resultCodeBased$calendarTime, result$calendarTime, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedStudyDurationH1, result$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$numberOfSubjects, result$numberOfSubjects, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedNumberOfSubjectsH1, result$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$informationOverStages, result$informationOverStages, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH0, result$expectedInformationH0, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH01, result$expectedInformationH01, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH1, result$expectedInformationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxInformation, result$maxInformation, tolerance = 1e-07)
        expect_type(names(result), "character")
        df <- as.data.frame(result)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizePerStageVariableExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result <- getSampleSizeCounts(
        design = designGS2, lambda1 = seq(1.05, 1.35, 0.15),
        overDispersion = 0, lambda2 = 1.4, accrualTime = c(0, 10, 20), accrualIntensity = c(5, 10)
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result' with expected results
    expect_equal(result$directionUpper, c(FALSE, FALSE, FALSE), label = paste0(result$directionUpper))
    expect_equal(result$theta, c(0.75, 0.85714286, 0.96428571), tolerance = 1e-07, label = paste0(result$theta))
    expect_equal(result$maxNumberOfSubjects, c(150, 150, 150), label = paste0(result$maxNumberOfSubjects))
    expect_equal(result$rejectPerStage[1, ], 0.033479189, tolerance = 1e-07, label = paste0(result$rejectPerStage[1, ]))
    expect_equal(result$rejectPerStage[2, ], 0.27519472, tolerance = 1e-07, label = paste0(result$rejectPerStage[2, ]))
    expect_equal(result$rejectPerStage[3, ], 0.49132609, tolerance = 1e-07, label = paste0(result$rejectPerStage[3, ]))
    expect_equal(result$earlyStop, 0.30867391, tolerance = 1e-07, label = paste0(result$earlyStop))
    expect_equal(result$calendarTime[1, ], c(6.987616, 12.444445, 56.948814), tolerance = 1e-07, label = paste0(result$calendarTime[1, ]))
    expect_equal(result$calendarTime[2, ], c(9.3944745, 15.885169, 92.299449), tolerance = 1e-07, label = paste0(result$calendarTime[2, ]))
    expect_equal(result$calendarTime[3, ], c(12.259611, 20.071241, 153.10254), tolerance = 1e-07, label = paste0(result$calendarTime[3, ]))
    expect_equal(result$expectedStudyDurationH1, c(11.294639, 18.663917, 133.1507), tolerance = 1e-07, label = paste0(result$expectedStudyDurationH1))
    expect_equal(result$numberOfSubjects[1, ], c(34, 74, 150), label = paste0(result$numberOfSubjects[1, ]))
    expect_equal(result$numberOfSubjects[2, ], c(46, 108, 150), label = paste0(result$numberOfSubjects[2, ]))
    expect_equal(result$numberOfSubjects[3, ], c(72, 150, 150), label = paste0(result$numberOfSubjects[3, ]))
    expect_equal(result$expectedNumberOfSubjectsH1, c(63.572728, 135.8974, 150), tolerance = 1e-07, label = paste0(result$expectedNumberOfSubjectsH1))
    expect_equal(result$informationOverStages[1, ], c(37.273683, 129.81863, 2332.3707), tolerance = 1e-07, label = paste0(result$informationOverStages[1, ]))
    expect_equal(result$informationOverStages[2, ], c(66.393749, 231.23943, 4154.5353), tolerance = 1e-07, label = paste0(result$informationOverStages[2, ]))
    expect_equal(result$informationOverStages[3, ], c(116.48026, 405.68322, 7288.6583), tolerance = 1e-07, label = paste0(result$informationOverStages[3, ]))
    expect_equal(result$expectedInformationH0, c(116.24237, 404.85466, 7273.7722), tolerance = 1e-07, label = paste0(result$expectedInformationH0))
    expect_equal(result$expectedInformationH01, c(114.00093, 397.04807, 7133.516), tolerance = 1e-07, label = paste0(result$expectedInformationH01))
    expect_equal(result$expectedInformationH1, c(100.04494, 348.44146, 6260.2312), tolerance = 1e-07, label = paste0(result$expectedInformationH1))
    expect_equal(result$maxInformation, c(116.48026, 405.68322, 7288.6583), tolerance = 1e-07, label = paste0(result$maxInformation))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result), NA)))
        expect_output(print(result)$show())
        invisible(capture.output(expect_error(summary(result), NA)))
        expect_output(summary(result)$show())
        resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
        expect_equal(resultCodeBased$directionUpper, result$directionUpper, tolerance = 1e-07)
        expect_equal(resultCodeBased$theta, result$theta, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxNumberOfSubjects, result$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(resultCodeBased$rejectPerStage, result$rejectPerStage, tolerance = 1e-07)
        expect_equal(resultCodeBased$earlyStop, result$earlyStop, tolerance = 1e-07)
        expect_equal(resultCodeBased$calendarTime, result$calendarTime, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedStudyDurationH1, result$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$numberOfSubjects, result$numberOfSubjects, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedNumberOfSubjectsH1, result$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$informationOverStages, result$informationOverStages, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH0, result$expectedInformationH0, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH01, result$expectedInformationH01, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH1, result$expectedInformationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxInformation, result$maxInformation, tolerance = 1e-07)
        expect_type(names(result), "character")
        df <- as.data.frame(result)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizeCountsFixedExposureSingleStage}
    # @refFS[Formula]{fs:sampleSizePerStageFixedExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    # @refFS[Formula]{fs:sampleSizeCountsFixedExposureOptimumAllocationRatio}
    result <- getSampleSizeCounts(
        design = designGS2, lambda2 = 0.234, lambda1 = c(0.1, 0.2),
        allocationRatioPlanned = 0, accrualTime = 7, fixedExposureTime = 1
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result' with expected results
    expect_equal(result$allocationRatioPlanned, c(1.5297048, 1.0816655), tolerance = 1e-07, label = paste0(result$allocationRatioPlanned))
    expect_equal(result$directionUpper, c(FALSE, FALSE), label = paste0(result$directionUpper))
    expect_equal(result$theta, c(0.42735043, 0.85470085), tolerance = 1e-07, label = paste0(result$theta))
    expect_equal(result$maxNumberOfSubjects, c(366, 7243), label = paste0(result$maxNumberOfSubjects))
    expect_equal(result$maxNumberOfSubjects1, c(221, 3764), label = paste0(result$maxNumberOfSubjects1))
    expect_equal(result$maxNumberOfSubjects2, c(145, 3479), label = paste0(result$maxNumberOfSubjects2))
    expect_equal(result$rejectPerStage[1, ], 0.033479189, tolerance = 1e-07, label = paste0(result$rejectPerStage[1, ]))
    expect_equal(result$rejectPerStage[2, ], 0.27519472, tolerance = 1e-07, label = paste0(result$rejectPerStage[2, ]))
    expect_equal(result$rejectPerStage[3, ], 0.49132609, tolerance = 1e-07, label = paste0(result$rejectPerStage[3, ]))
    expect_equal(result$earlyStop, 0.30867391, tolerance = 1e-07, label = paste0(result$earlyStop))
    expect_equal(result$calendarTime[1, ], c(2.7255466, 2.7393702), tolerance = 1e-07, label = paste0(result$calendarTime[1, ]))
    expect_equal(result$calendarTime[2, ], c(4.4792097, 4.4896333), tolerance = 1e-07, label = paste0(result$calendarTime[2, ]))
    expect_equal(result$calendarTime[3, ], c(8, 8), label = paste0(result$calendarTime[3, ]))
    expect_equal(result$expectedStudyDurationH1, c(6.8545127, 6.857844), tolerance = 1e-07, label = paste0(result$expectedStudyDurationH1))
    expect_equal(result$numberOfSubjects[1, ], c(143, 2835), label = paste0(result$numberOfSubjects[1, ]))
    expect_equal(result$numberOfSubjects[2, ], c(234, 4645), label = paste0(result$numberOfSubjects[2, ]))
    expect_equal(result$numberOfSubjects[3, ], c(366, 7243), label = paste0(result$numberOfSubjects[3, ]))
    expect_equal(result$expectedNumberOfSubjectsH1, c(322.20844, 6380.4678), tolerance = 1e-07, label = paste0(result$expectedNumberOfSubjectsH1))
    expect_equal(result$informationOverStages[1, ], c(4.2681123, 125.14338), tolerance = 1e-07, label = paste0(result$informationOverStages[1, ]))
    expect_equal(result$informationOverStages[2, ], c(7.6025751, 222.91164), tolerance = 1e-07, label = paste0(result$informationOverStages[2, ]))
    expect_equal(result$informationOverStages[3, ], c(13.337851, 391.07305), tolerance = 1e-07, label = paste0(result$informationOverStages[3, ]))
    expect_equal(result$expectedInformationH0, c(13.31061, 390.27433), tolerance = 1e-07, label = paste0(result$expectedInformationH0))
    expect_equal(result$expectedInformationH01, c(13.053949, 382.74888), tolerance = 1e-07, label = paste0(result$expectedInformationH01))
    expect_equal(result$expectedInformationH1, c(11.455885, 335.89278), tolerance = 1e-07, label = paste0(result$expectedInformationH1))
    expect_equal(result$maxInformation, c(13.337851, 391.07305), tolerance = 1e-07, label = paste0(result$maxInformation))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result), NA)))
        expect_output(print(result)$show())
        invisible(capture.output(expect_error(summary(result), NA)))
        expect_output(summary(result)$show())
        resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
        expect_equal(resultCodeBased$allocationRatioPlanned, result$allocationRatioPlanned, tolerance = 1e-07)
        expect_equal(resultCodeBased$directionUpper, result$directionUpper, tolerance = 1e-07)
        expect_equal(resultCodeBased$theta, result$theta, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxNumberOfSubjects, result$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxNumberOfSubjects1, result$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxNumberOfSubjects2, result$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(resultCodeBased$rejectPerStage, result$rejectPerStage, tolerance = 1e-07)
        expect_equal(resultCodeBased$earlyStop, result$earlyStop, tolerance = 1e-07)
        expect_equal(resultCodeBased$calendarTime, result$calendarTime, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedStudyDurationH1, result$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$numberOfSubjects, result$numberOfSubjects, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedNumberOfSubjectsH1, result$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$informationOverStages, result$informationOverStages, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH0, result$expectedInformationH0, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH01, result$expectedInformationH01, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH1, result$expectedInformationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxInformation, result$maxInformation, tolerance = 1e-07)
        expect_type(names(result), "character")
        df <- as.data.frame(result)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizePerStageVariableExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    # @refFS[Formula]{fs:sampleSizeCountsVariableExposureOptimumAllocationRatio}
    result <- getSampleSizeCounts(
        design = designGS2, lambda2 = 0.234, lambda1 = c(0.1, 0.2),
        allocationRatioPlanned = 0, accrualTime = 7, followUpTime = 1
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result' with expected results
    expect_equal(result$allocationRatioPlanned, c(1.7934046, 1.1087896), tolerance = 1e-07, label = paste0(result$allocationRatioPlanned))
    expect_equal(result$directionUpper, c(FALSE, FALSE), label = paste0(result$directionUpper))
    expect_equal(result$theta, c(0.42735043, 0.85470085), tolerance = 1e-07, label = paste0(result$theta))
    expect_equal(result$maxNumberOfSubjects, c(82, 1610), label = paste0(result$maxNumberOfSubjects))
    expect_equal(result$maxNumberOfSubjects1, c(53, 847), label = paste0(result$maxNumberOfSubjects1))
    expect_equal(result$maxNumberOfSubjects2, c(29, 763), label = paste0(result$maxNumberOfSubjects2))
    expect_equal(result$rejectPerStage[1, ], 0.033479189, tolerance = 1e-07, label = paste0(result$rejectPerStage[1, ]))
    expect_equal(result$rejectPerStage[2, ], 0.27519472, tolerance = 1e-07, label = paste0(result$rejectPerStage[2, ]))
    expect_equal(result$rejectPerStage[3, ], 0.49132609, tolerance = 1e-07, label = paste0(result$rejectPerStage[3, ]))
    expect_equal(result$earlyStop, 0.30867391, tolerance = 1e-07, label = paste0(result$earlyStop))
    expect_equal(result$calendarTime[1, ], c(4.4478569, 4.4878996), tolerance = 1e-07, label = paste0(result$calendarTime[1, ]))
    expect_equal(result$calendarTime[2, ], c(5.9668445, 5.9911665), tolerance = 1e-07, label = paste0(result$calendarTime[2, ]))
    expect_equal(result$calendarTime[3, ], c(8, 8), label = paste0(result$calendarTime[3, ]))
    expect_equal(result$expectedStudyDurationH1, c(7.3215635, 7.3295973), tolerance = 1e-07, label = paste0(result$expectedStudyDurationH1))
    expect_equal(result$numberOfSubjects[1, ], c(52, 1032), label = paste0(result$numberOfSubjects[1, ]))
    expect_equal(result$numberOfSubjects[2, ], c(69, 1378), label = paste0(result$numberOfSubjects[2, ]))
    expect_equal(result$numberOfSubjects[3, ], c(82, 1610), label = paste0(result$numberOfSubjects[3, ]))
    expect_equal(result$expectedNumberOfSubjectsH1, c(77.418093, 1526.8039), tolerance = 1e-07, label = paste0(result$expectedNumberOfSubjectsH1))
    expect_equal(result$informationOverStages[1, ], c(4.2681123, 125.14338), tolerance = 1e-07, label = paste0(result$informationOverStages[1, ]))
    expect_equal(result$informationOverStages[2, ], c(7.6025751, 222.91164), tolerance = 1e-07, label = paste0(result$informationOverStages[2, ]))
    expect_equal(result$informationOverStages[3, ], c(13.337851, 391.07305), tolerance = 1e-07, label = paste0(result$informationOverStages[3, ]))
    expect_equal(result$expectedInformationH0, c(13.31061, 390.27433), tolerance = 1e-07, label = paste0(result$expectedInformationH0))
    expect_equal(result$expectedInformationH01, c(13.053949, 382.74888), tolerance = 1e-07, label = paste0(result$expectedInformationH01))
    expect_equal(result$expectedInformationH1, c(11.455885, 335.89278), tolerance = 1e-07, label = paste0(result$expectedInformationH1))
    expect_equal(result$maxInformation, c(13.337851, 391.07305), tolerance = 1e-07, label = paste0(result$maxInformation))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result), NA)))
        expect_output(print(result)$show())
        invisible(capture.output(expect_error(summary(result), NA)))
        expect_output(summary(result)$show())
        resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
        expect_equal(resultCodeBased$allocationRatioPlanned, result$allocationRatioPlanned, tolerance = 1e-07)
        expect_equal(resultCodeBased$directionUpper, result$directionUpper, tolerance = 1e-07)
        expect_equal(resultCodeBased$theta, result$theta, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxNumberOfSubjects, result$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxNumberOfSubjects1, result$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxNumberOfSubjects2, result$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(resultCodeBased$rejectPerStage, result$rejectPerStage, tolerance = 1e-07)
        expect_equal(resultCodeBased$earlyStop, result$earlyStop, tolerance = 1e-07)
        expect_equal(resultCodeBased$calendarTime, result$calendarTime, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedStudyDurationH1, result$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$numberOfSubjects, result$numberOfSubjects, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedNumberOfSubjectsH1, result$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$informationOverStages, result$informationOverStages, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH0, result$expectedInformationH0, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH01, result$expectedInformationH01, tolerance = 1e-07)
        expect_equal(resultCodeBased$expectedInformationH1, result$expectedInformationH1, tolerance = 1e-07)
        expect_equal(resultCodeBased$maxInformation, result$maxInformation, tolerance = 1e-07)
        expect_type(names(result), "character")
        df <- as.data.frame(result)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_plan_section("Testing the Power Calculation of Count Data Designs for Different Designs and Arguments")


test_that("'getPowerCounts': Power calculation of testing count data for one-sided group sequential design", {
    .skipTestIfDisabled()

    designGS1 <- getDesignGroupSequential(
        informationRates = c(0.32, 0.57, 1), futilityBounds = c(-0.4, 0.1),
        sided = 1, beta = 0.1, typeOfDesign = "WT", deltaWT = 0.25
    )

    # @refFS[Formula]{fs:ShiftParameterCountData}
    # @refFS[Formula]{fs:VarianceEstimateFixedCountData}
    result <- getPowerCounts(
        design = designGS1, maxNumberOfSubjects = 400, directionUpper = FALSE,
        overDispersion = 1, fixedExposureTime = 1, lambda1 = seq(1.05, 1.55, 0.1), lambda2 = 1.4
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result' with expected results
    expect_equal(result$overallReject, c(0.53364515, 0.28922288, 0.1246922, 0.044025943, 0.013287033, 0.0036093301), tolerance = 1e-07, label = paste0(result$overallReject))
    expect_equal(result$rejectPerStage[1, ], c(0.060449997, 0.027325942, 0.011625783, 0.0047105893, 0.0018358964, 0.00069394873), tolerance = 1e-07, label = paste0(result$rejectPerStage[1, ]))
    expect_equal(result$rejectPerStage[2, ], c(0.16740522, 0.081280885, 0.033932049, 0.012489808, 0.0041413056, 0.0012595369), tolerance = 1e-07, label = paste0(result$rejectPerStage[2, ]))
    expect_equal(result$rejectPerStage[3, ], c(0.30578993, 0.18061605, 0.079134367, 0.026825545, 0.0073098308, 0.0016558445), tolerance = 1e-07, label = paste0(result$rejectPerStage[3, ]))
    expect_equal(result$futilityStop, c(0.093456372, 0.19549875, 0.33823583, 0.50079777, 0.65574892, 0.78246895), tolerance = 1e-07, label = paste0(result$futilityStop))
    expect_equal(result$futilityPerStage[1, ], c(0.054589249, 0.10911095, 0.18844857, 0.28892653, 0.40212886, 0.5176485), tolerance = 1e-07, label = paste0(result$futilityPerStage[1, ]))
    expect_equal(result$futilityPerStage[2, ], c(0.038867123, 0.086387802, 0.14978727, 0.21187124, 0.25362007, 0.26482046), tolerance = 1e-07, label = paste0(result$futilityPerStage[2, ]))
    expect_equal(result$earlyStop, c(0.32131159, 0.30410558, 0.38379367, 0.51799816, 0.66172612, 0.78442244), tolerance = 1e-07, label = paste0(result$earlyStop))
    expect_equal(result$futilityBoundsPValueScale[1, ], 0.65542174, tolerance = 1e-07, label = paste0(result$futilityBoundsPValueScale[1, ]))
    expect_equal(result$futilityBoundsPValueScale[2, ], 0.46017216, tolerance = 1e-07, label = paste0(result$futilityBoundsPValueScale[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result), NA)))
        expect_output(print(result)$show())
        invisible(capture.output(expect_error(summary(result), NA)))
        expect_output(summary(result)$show())
        resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
        expect_equal(resultCodeBased$overallReject, result$overallReject, tolerance = 1e-07)
        expect_equal(resultCodeBased$rejectPerStage, result$rejectPerStage, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityStop, result$futilityStop, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityPerStage, result$futilityPerStage, tolerance = 1e-07)
        expect_equal(resultCodeBased$earlyStop, result$earlyStop, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityBoundsPValueScale, result$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(result), "character")
        df <- as.data.frame(result)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:ShiftParameterCountData}
    # @refFS[Formula]{fs:VarianceEstimateFixedCountData}
    result <- getPowerCounts(
        design = designGS1, maxNumberOfSubjects = 400, directionUpper = FALSE,
        thetaH0 = 0.9, overDispersion = 0.3, fixedExposureTime = 1, lambda1 = seq(1.05, 1.55, 0.1), lambda2 = 1.4
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result' with expected results
    expect_equal(result$overallReject, c(0.37505189, 0.12855316, 0.029101752, 0.004756001, 0.00064764079, 8.6285962e-05), tolerance = 1e-07, label = paste0(result$overallReject))
    expect_equal(result$rejectPerStage[1, ], c(0.037203429, 0.011960954, 0.0033736668, 0.00085074056, 0.00019509054, 4.1296965e-05), tolerance = 1e-07, label = paste0(result$rejectPerStage[1, ]))
    expect_equal(result$rejectPerStage[2, ], c(0.10894254, 0.034976115, 0.0085007361, 0.0016219598, 0.00025117618, 3.2519937e-05), tolerance = 1e-07, label = paste0(result$rejectPerStage[2, ]))
    expect_equal(result$rejectPerStage[3, ], c(0.22890592, 0.081616089, 0.01722735, 0.0022833007, 0.00020137408, 1.2469061e-05), tolerance = 1e-07, label = paste0(result$rejectPerStage[3, ]))
    expect_equal(result$futilityStop, c(0.15143926, 0.33317053, 0.55855018, 0.75910283, 0.8919586, 0.95982659), tolerance = 1e-07, label = paste0(result$futilityStop))
    expect_equal(result$futilityPerStage[1, ], c(0.085530884, 0.18552122, 0.32857927, 0.49402485, 0.65311126, 0.78343373), tolerance = 1e-07, label = paste0(result$futilityPerStage[1, ]))
    expect_equal(result$futilityPerStage[2, ], c(0.065908377, 0.14764931, 0.22997091, 0.26507798, 0.23884734, 0.17639285), tolerance = 1e-07, label = paste0(result$futilityPerStage[2, ]))
    expect_equal(result$earlyStop, c(0.29758523, 0.3801076, 0.57042459, 0.76157553, 0.89240487, 0.9599004), tolerance = 1e-07, label = paste0(result$earlyStop))
    expect_equal(result$futilityBoundsPValueScale[1, ], 0.65542174, tolerance = 1e-07, label = paste0(result$futilityBoundsPValueScale[1, ]))
    expect_equal(result$futilityBoundsPValueScale[2, ], 0.46017216, tolerance = 1e-07, label = paste0(result$futilityBoundsPValueScale[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result), NA)))
        expect_output(print(result)$show())
        invisible(capture.output(expect_error(summary(result), NA)))
        expect_output(summary(result)$show())
        resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
        expect_equal(resultCodeBased$overallReject, result$overallReject, tolerance = 1e-07)
        expect_equal(resultCodeBased$rejectPerStage, result$rejectPerStage, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityStop, result$futilityStop, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityPerStage, result$futilityPerStage, tolerance = 1e-07)
        expect_equal(resultCodeBased$earlyStop, result$earlyStop, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityBoundsPValueScale, result$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(result), "character")
        df <- as.data.frame(result)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:ShiftParameterCountData}
    # @refFS[Formula]{fs:VarianceEstimateVariableCountData}
    result <- getPowerCounts(
        design = designGS1, maxNumberOfSubjects = 400, directionUpper = FALSE,
        overDispersion = 0.4, accrualTime = 4, followUpTime = 3, lambda1 = seq(1.05, 1.55, 0.1), lambda2 = 1.4
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result' with expected results
    expect_equal(result$overallReject, c(0.95733458, 0.71461505, 0.30541815, 0.066441103, 0.0080473097, 0.00068660473), tolerance = 1e-07, label = paste0(result$overallReject))
    expect_equal(result$rejectPerStage[1, ], c(0.27300416, 0.10162379, 0.02908059, 0.0066447778, 0.0012578105, 0.00020378378), tolerance = 1e-07, label = paste0(result$rejectPerStage[1, ]))
    expect_equal(result$rejectPerStage[2, ], c(0.42120665, 0.25113898, 0.086321841, 0.018416222, 0.0026215993, 0.00026573717), tolerance = 1e-07, label = paste0(result$rejectPerStage[2, ]))
    expect_equal(result$rejectPerStage[3, ], c(0.26312377, 0.36185228, 0.19001572, 0.041380104, 0.0041678999, 0.00021708378), tolerance = 1e-07, label = paste0(result$rejectPerStage[3, ]))
    expect_equal(result$futilityStop, c(0.0072113169, 0.048422794, 0.18620987, 0.43924942, 0.70950882, 0.88913782), tolerance = 1e-07, label = paste0(result$futilityStop))
    expect_equal(result$futilityPerStage[1, ], c(0.005399273, 0.03001657, 0.10411951, 0.24920632, 0.44771069, 0.64885499), tolerance = 1e-07, label = paste0(result$futilityPerStage[1, ]))
    expect_equal(result$futilityPerStage[2, ], c(0.0018120439, 0.018406224, 0.082090356, 0.19004311, 0.26179813, 0.24028283), tolerance = 1e-07, label = paste0(result$futilityPerStage[2, ]))
    expect_equal(result$earlyStop, c(0.70142213, 0.40118557, 0.3016123, 0.46431042, 0.71338823, 0.88960734), tolerance = 1e-07, label = paste0(result$earlyStop))
    expect_equal(result$futilityBoundsPValueScale[1, ], 0.65542174, tolerance = 1e-07, label = paste0(result$futilityBoundsPValueScale[1, ]))
    expect_equal(result$futilityBoundsPValueScale[2, ], 0.46017216, tolerance = 1e-07, label = paste0(result$futilityBoundsPValueScale[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result), NA)))
        expect_output(print(result)$show())
        invisible(capture.output(expect_error(summary(result), NA)))
        expect_output(summary(result)$show())
        resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
        expect_equal(resultCodeBased$overallReject, result$overallReject, tolerance = 1e-07)
        expect_equal(resultCodeBased$rejectPerStage, result$rejectPerStage, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityStop, result$futilityStop, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityPerStage, result$futilityPerStage, tolerance = 1e-07)
        expect_equal(resultCodeBased$earlyStop, result$earlyStop, tolerance = 1e-07)
        expect_equal(resultCodeBased$futilityBoundsPValueScale, result$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(result), "character")
        df <- as.data.frame(result)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:ShiftParameterCountData}
    # @refFS[Formula]{fs:VarianceEstimateVariableCountData}
    result <- getPowerCounts(
        design = designGS1, accrualTime = c(0, 5, 10), accrualIntensity = c(8, 15),
        followUpTime = 12, lambda1 = seq(0.85, 1.35, 0.15), lambda2 = 1
    )

    # @refFS[Formula]{fs:ShiftParameterCountData}
    # @refFS[Formula]{fs:VarianceEstimateVariableCountData}
    result <- getPowerCounts(
        design = designGS1, accrualTime = c(0, 5, 10), accrualIntensity = c(8, 15),
        thetaH0 = 1.1, followUpTime = 12, lambda1 = seq(0.85, 1.35, 0.15), lambda2 = 1
    )
})

test_that("'getPowerCounts': Power calculation of testing count data for two-sided group sequential design", {
    .skipTestIfDisabled()

    designGS2 <- getDesignGroupSequential(
        informationRates = c(0.57, 1),
        sided = 2, beta = 0.2, typeOfDesign = "WT", deltaWT = 0.1
    )

    # @refFS[Formula]{fs:ShiftParameterCountData}
    # @refFS[Formula]{fs:VarianceEstimateFixedCountData}
    result <- getPowerCounts(
        design = designGS2, maxNumberOfSubjects = 400,
        overDispersion = 1, fixedExposureTime = 1, lambda1 = seq(1.05, 1.35, 0.15),
        lambda2 = 1.4
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result' with expected results
    expect_equal(result$overallReject, c(0.44675032, 0.13692139, 0.030483803), tolerance = 1e-07, label = paste0(result$overallReject))
    expect_equal(result$rejectPerStage[1, ], c(0.10712223, 0.024389615, 0.005305078), tolerance = 1e-07, label = paste0(result$rejectPerStage[1, ]))
    expect_equal(result$rejectPerStage[2, ], c(0.33962809, 0.11253177, 0.025178725), tolerance = 1e-07, label = paste0(result$rejectPerStage[2, ]))
    expect_equal(result$earlyStop, c(0.10712223, 0.024389615, 0.005305078), tolerance = 1e-07, label = paste0(result$earlyStop))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result), NA)))
        expect_output(print(result)$show())
        invisible(capture.output(expect_error(summary(result), NA)))
        expect_output(summary(result)$show())
        resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
        expect_equal(resultCodeBased$overallReject, result$overallReject, tolerance = 1e-07)
        expect_equal(resultCodeBased$rejectPerStage, result$rejectPerStage, tolerance = 1e-07)
        expect_equal(resultCodeBased$earlyStop, result$earlyStop, tolerance = 1e-07)
        expect_type(names(result), "character")
        df <- as.data.frame(result)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:ShiftParameterCountData}
    # @refFS[Formula]{fs:VarianceEstimateFixedCountData}
    result <- getPowerCounts(
        design = designGS2, maxNumberOfSubjects = 400,
        overDispersion = 0.3, fixedExposureTime = 1, theta = seq(0.7, 1.55, 0.2), lambda2 = 1.4
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result' with expected results
    expect_equal(result$lambda1, c(0.98, 1.26, 1.54, 1.82, 2.1), tolerance = 1e-07, label = paste0(result$lambda1))
    expect_equal(result$overallReject, c(0.85058327, 0.11065739, 0.099246729, 0.67623038, 0.97836543), tolerance = 1e-07, label = paste0(result$overallReject))
    expect_equal(result$rejectPerStage[1, ], c(0.36176735, 0.019313406, 0.017184738, 0.21355347, 0.65145735), tolerance = 1e-07, label = paste0(result$rejectPerStage[1, ]))
    expect_equal(result$rejectPerStage[2, ], c(0.48881593, 0.091343983, 0.082061991, 0.46267692, 0.32690808), tolerance = 1e-07, label = paste0(result$rejectPerStage[2, ]))
    expect_equal(result$earlyStop, c(0.36176735, 0.019313406, 0.017184738, 0.21355347, 0.65145735), tolerance = 1e-07, label = paste0(result$earlyStop))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result), NA)))
        expect_output(print(result)$show())
        invisible(capture.output(expect_error(summary(result), NA)))
        expect_output(summary(result)$show())
        resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
        expect_equal(resultCodeBased$lambda1, result$lambda1, tolerance = 1e-07)
        expect_equal(resultCodeBased$overallReject, result$overallReject, tolerance = 1e-07)
        expect_equal(resultCodeBased$rejectPerStage, result$rejectPerStage, tolerance = 1e-07)
        expect_equal(resultCodeBased$earlyStop, result$earlyStop, tolerance = 1e-07)
        expect_type(names(result), "character")
        df <- as.data.frame(result)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:ShiftParameterCountData}
    # @refFS[Formula]{fs:VarianceEstimateVariableCountData}
    result <- getPowerCounts(
        design = designGS2, maxNumberOfSubjects = 400,
        overDispersion = 0.4, accrualTime = 4, followUpTime = 3, lambda1 = seq(1.05, 1.55, 0.1), lambda2 = 1.4
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result' with expected results
    expect_equal(result$overallReject, c(0.93771217, 0.63941931, 0.2303106, 0.042619466, 0.041526385, 0.19175607), tolerance = 1e-07, label = paste0(result$overallReject))
    expect_equal(result$rejectPerStage[1, ], c(0.50887066, 0.19212331, 0.044559304, 0.007285927, 0.0071055093, 0.035814782), tolerance = 1e-07, label = paste0(result$rejectPerStage[1, ]))
    expect_equal(result$rejectPerStage[2, ], c(0.42884152, 0.447296, 0.1857513, 0.035333539, 0.034420876, 0.15594129), tolerance = 1e-07, label = paste0(result$rejectPerStage[2, ]))
    expect_equal(result$earlyStop, c(0.50887066, 0.19212331, 0.044559304, 0.007285927, 0.0071055093, 0.035814782), tolerance = 1e-07, label = paste0(result$earlyStop))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result), NA)))
        expect_output(print(result)$show())
        invisible(capture.output(expect_error(summary(result), NA)))
        expect_output(summary(result)$show())
        resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
        expect_equal(resultCodeBased$overallReject, result$overallReject, tolerance = 1e-07)
        expect_equal(resultCodeBased$rejectPerStage, result$rejectPerStage, tolerance = 1e-07)
        expect_equal(resultCodeBased$earlyStop, result$earlyStop, tolerance = 1e-07)
        expect_type(names(result), "character")
        df <- as.data.frame(result)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:ShiftParameterCountData}
    # @refFS[Formula]{fs:VarianceEstimateVariableCountData}
    result <- getPowerCounts(
        design = designGS2, accrualTime = c(0, 5, 10), accrualIntensity = c(8, 15),
        followUpTime = 12, lambda1 = seq(0.85, 1.35, 0.15), lambda2 = 1
    )
})
