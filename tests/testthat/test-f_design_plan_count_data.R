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
## |  Creation date: 16 January 2024, 11:26:11
## |  File version: $Revision$
## |  Last changed: $Date$
## |  Last changed by: $Author$
## |

test_plan_section("Testing the Sample Size Calculation of Count Data Designs for Different Designs and Arguments")


test_that("'getSampleSizeCounts': Sample size calculation of testing count data for fixed sample design", {
    .skipTestIfDisabled()

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizeCountsFixedExposureSingleStage}
    # @refFS[Formula]{fs:sampleSizePerStageFixedExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result1 <- getSampleSizeCounts(
        alpha = 0.01, beta = 0.05, lambda = 0.234, theta = 0.7,
        overdispersion = 0.71, accrualTime = 7, fixedExposureTime = 1
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result1' with expected results
    expect_equal(result1$directionUpper, FALSE, label = paste0(result1$directionUpper))
    expect_equal(result1$lambda1, 0.19270588, tolerance = 1e-07, label = paste0(result1$lambda1))
    expect_equal(result1$lambda2, 0.27529412, tolerance = 1e-07, label = paste0(result1$lambda2))
    expect_equal(result1$nFixed, 2540, label = paste0(result1$nFixed))
    expect_equal(result1$nFixed1, 1270, label = paste0(result1$nFixed1))
    expect_equal(result1$nFixed2, 1270, label = paste0(result1$nFixed2))
    expect_equal(result1$maxNumberOfSubjects, 2540, label = paste0(result1$maxNumberOfSubjects))
    expect_equal(result1$calendarTime[1, ], 8, label = paste0(result1$calendarTime[1, ]))
    expect_equal(result1$expectedStudyDurationH1, 8, label = paste0(result1$expectedStudyDurationH1))
    expect_equal(result1$expectedNumberOfSubjectsH1, 2540, label = paste0(result1$expectedNumberOfSubjectsH1))
    expect_equal(result1$maxInformation, 123.96487, tolerance = 1e-07, label = paste0(result1$maxInformation))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result1), NA)))
        expect_output(print(result1)$show())
        invisible(capture.output(expect_error(summary(result1), NA)))
        expect_output(summary(result1)$show())
        result1CodeBased <- eval(parse(text = getObjectRCode(result1, stringWrapParagraphWidth = NULL)))
        expect_equal(result1CodeBased$directionUpper, result1$directionUpper, tolerance = 1e-07)
        expect_equal(result1CodeBased$lambda1, result1$lambda1, tolerance = 1e-07)
        expect_equal(result1CodeBased$lambda2, result1$lambda2, tolerance = 1e-07)
        expect_equal(result1CodeBased$nFixed, result1$nFixed, tolerance = 1e-07)
        expect_equal(result1CodeBased$nFixed1, result1$nFixed1, tolerance = 1e-07)
        expect_equal(result1CodeBased$nFixed2, result1$nFixed2, tolerance = 1e-07)
        expect_equal(result1CodeBased$maxNumberOfSubjects, result1$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(result1CodeBased$calendarTime, result1$calendarTime, tolerance = 1e-07)
        expect_equal(result1CodeBased$expectedStudyDurationH1, result1$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(result1CodeBased$expectedNumberOfSubjectsH1, result1$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(result1CodeBased$maxInformation, result1$maxInformation, tolerance = 1e-07)
        expect_type(names(result1), "character")
        df <- as.data.frame(result1)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result1)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizePerStageVariableExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result2 <- getSampleSizeCounts(
        alpha = 0.01, beta = 0.05, lambda = 0.234, theta = 0.7,
        overdispersion = 0.71, accrualTime = 7, followUpTime = 1
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result2' with expected results
    expect_equal(result2$directionUpper, FALSE, label = paste0(result2$directionUpper))
    expect_equal(result2$lambda1, 0.19270588, tolerance = 1e-07, label = paste0(result2$lambda1))
    expect_equal(result2$lambda2, 0.27529412, tolerance = 1e-07, label = paste0(result2$lambda2))
    expect_equal(result2$nFixed, 886, label = paste0(result2$nFixed))
    expect_equal(result2$nFixed1, 443, label = paste0(result2$nFixed1))
    expect_equal(result2$nFixed2, 443, label = paste0(result2$nFixed2))
    expect_equal(result2$maxNumberOfSubjects, 886, label = paste0(result2$maxNumberOfSubjects))
    expect_equal(result2$calendarTime[1, ], 8, label = paste0(result2$calendarTime[1, ]))
    expect_equal(result2$expectedStudyDurationH1, 8, label = paste0(result2$expectedStudyDurationH1))
    expect_equal(result2$expectedNumberOfSubjectsH1, 886, label = paste0(result2$expectedNumberOfSubjectsH1))
    expect_equal(result2$maxInformation, 123.96487, tolerance = 1e-07, label = paste0(result2$maxInformation))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result2), NA)))
        expect_output(print(result2)$show())
        invisible(capture.output(expect_error(summary(result2), NA)))
        expect_output(summary(result2)$show())
        result2CodeBased <- eval(parse(text = getObjectRCode(result2, stringWrapParagraphWidth = NULL)))
        expect_equal(result2CodeBased$directionUpper, result2$directionUpper, tolerance = 1e-07)
        expect_equal(result2CodeBased$lambda1, result2$lambda1, tolerance = 1e-07)
        expect_equal(result2CodeBased$lambda2, result2$lambda2, tolerance = 1e-07)
        expect_equal(result2CodeBased$nFixed, result2$nFixed, tolerance = 1e-07)
        expect_equal(result2CodeBased$nFixed1, result2$nFixed1, tolerance = 1e-07)
        expect_equal(result2CodeBased$nFixed2, result2$nFixed2, tolerance = 1e-07)
        expect_equal(result2CodeBased$maxNumberOfSubjects, result2$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(result2CodeBased$calendarTime, result2$calendarTime, tolerance = 1e-07)
        expect_equal(result2CodeBased$expectedStudyDurationH1, result2$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(result2CodeBased$expectedNumberOfSubjectsH1, result2$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(result2CodeBased$maxInformation, result2$maxInformation, tolerance = 1e-07)
        expect_type(names(result2), "character")
        df <- as.data.frame(result2)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result2)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizeCountsFixedExposureSingleStage}
    # @refFS[Formula]{fs:sampleSizePerStageFixedExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result3 <- getSampleSizeCounts(
        alpha = 0.01, beta = 0.05, lambda2 = 0.02, theta = c(0.7, 0.8),
        overdispersion = 13.1, accrualTime = 7, fixedExposureTime = 2
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result3' with expected results
    expect_equal(result3$directionUpper, c(FALSE, FALSE), label = paste0(result3$directionUpper))
    expect_equal(result3$lambda1, c(0.014, 0.016), tolerance = 1e-07, label = paste0(result3$lambda1))
    expect_equal(result3$nFixed, c(21550, 52228), label = paste0(result3$nFixed))
    expect_equal(result3$nFixed1, c(10775, 26114), label = paste0(result3$nFixed1))
    expect_equal(result3$nFixed2, c(10775, 26114), label = paste0(result3$nFixed2))
    expect_equal(result3$maxNumberOfSubjects, c(21550, 52228), label = paste0(result3$maxNumberOfSubjects))
    expect_equal(result3$calendarTime[1, ], c(9, 9), label = paste0(result3$calendarTime[1, ]))
    expect_equal(result3$expectedStudyDurationH1, c(9, 9), label = paste0(result3$expectedStudyDurationH1))
    expect_equal(result3$expectedNumberOfSubjectsH1, c(21550, 52228), label = paste0(result3$expectedNumberOfSubjectsH1))
    expect_equal(result3$maxInformation, c(123.96487, 316.71977), tolerance = 1e-07, label = paste0(result3$maxInformation))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result3), NA)))
        expect_output(print(result3)$show())
        invisible(capture.output(expect_error(summary(result3), NA)))
        expect_output(summary(result3)$show())
        result3CodeBased <- eval(parse(text = getObjectRCode(result3, stringWrapParagraphWidth = NULL)))
        expect_equal(result3CodeBased$directionUpper, result3$directionUpper, tolerance = 1e-07)
        expect_equal(result3CodeBased$lambda1, result3$lambda1, tolerance = 1e-07)
        expect_equal(result3CodeBased$nFixed, result3$nFixed, tolerance = 1e-07)
        expect_equal(result3CodeBased$nFixed1, result3$nFixed1, tolerance = 1e-07)
        expect_equal(result3CodeBased$nFixed2, result3$nFixed2, tolerance = 1e-07)
        expect_equal(result3CodeBased$maxNumberOfSubjects, result3$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(result3CodeBased$calendarTime, result3$calendarTime, tolerance = 1e-07)
        expect_equal(result3CodeBased$expectedStudyDurationH1, result3$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(result3CodeBased$expectedNumberOfSubjectsH1, result3$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(result3CodeBased$maxInformation, result3$maxInformation, tolerance = 1e-07)
        expect_type(names(result3), "character")
        df <- as.data.frame(result3)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result3)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizePerStageVariableExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result4 <- getSampleSizeCounts(
        alpha = 0.01, beta = 0.05, lambda1 = seq(1.05, 1.35, 0.15),
        lambda2 = 1.4, accrualTime = c(0, 20), allocationRatioPlanned = 1, maxNumberOfSubjects = 100
    )


    ## Comparison of the results of TrialDesignPlanCountData object 'result4' with expected results
    expect_equal(result4$directionUpper, c(FALSE, FALSE, FALSE), label = paste0(result4$directionUpper))
    expect_equal(result4$theta, c(0.75, 0.85714286, 0.96428571), tolerance = 1e-07, label = paste0(result4$theta))
    expect_equal(result4$nFixed, c(100, 100, 100), label = paste0(result4$nFixed))
    expect_equal(result4$nFixed1, c(50, 50, 50), label = paste0(result4$nFixed1))
    expect_equal(result4$nFixed2, c(50, 50, 50), label = paste0(result4$nFixed2))
    expect_equal(result4$calendarTime[1, ], c(15.898439, 30.542203, 356.98795), tolerance = 1e-07, label = paste0(result4$calendarTime[1, ]))
    expect_equal(result4$expectedStudyDurationH1, c(15.898439, 30.542203, 356.98795), tolerance = 1e-07, label = paste0(result4$expectedStudyDurationH1))
    expect_equal(result4$expectedNumberOfSubjectsH1, c(78, 100, 100), label = paste0(result4$expectedNumberOfSubjectsH1))
    expect_equal(result4$maxInformation, c(190.55408, 663.67118, 11923.768), tolerance = 1e-07, label = paste0(result4$maxInformation))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result4), NA)))
        expect_output(print(result4)$show())
        invisible(capture.output(expect_error(summary(result4), NA)))
        expect_output(summary(result4)$show())
        result4CodeBased <- eval(parse(text = getObjectRCode(result4, stringWrapParagraphWidth = NULL)))
        expect_equal(result4CodeBased$directionUpper, result4$directionUpper, tolerance = 1e-07)
        expect_equal(result4CodeBased$theta, result4$theta, tolerance = 1e-07)
        expect_equal(result4CodeBased$nFixed, result4$nFixed, tolerance = 1e-07)
        expect_equal(result4CodeBased$nFixed1, result4$nFixed1, tolerance = 1e-07)
        expect_equal(result4CodeBased$nFixed2, result4$nFixed2, tolerance = 1e-07)
        expect_equal(result4CodeBased$calendarTime, result4$calendarTime, tolerance = 1e-07)
        expect_equal(result4CodeBased$expectedStudyDurationH1, result4$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(result4CodeBased$expectedNumberOfSubjectsH1, result4$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(result4CodeBased$maxInformation, result4$maxInformation, tolerance = 1e-07)
        expect_type(names(result4), "character")
        df <- as.data.frame(result4)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result4)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizePerStageVariableExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    expect_error(getSampleSizeCounts(
        alpha = 0.01, beta = 0.05, lambda1 = seq(1.35, 1.35, 0.15),
        overdispersion = 0.01, lambda2 = 1.4, accrualTime = c(0, 10, 20), accrualIntensity = c(5, 10)
    ))

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizePerStageVariableExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result5 <- getSampleSizeCounts(
        alpha = 0.01, beta = 0.05, thetaH0 = 0.9, lambda1 = seq(1.1, 1.15, 0.05),
        overdispersion = 0.2, lambda2 = 1.7, accrualTime = c(0, 2), maxNumberOfSubjects = 300
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result5' with expected results
    expect_equal(result5$directionUpper, c(FALSE, FALSE), label = paste0(result5$directionUpper))
    expect_equal(result5$theta, c(0.64705882, 0.67647059), tolerance = 1e-07, label = paste0(result5$theta))
    expect_equal(result5$nFixed, c(300, 300), label = paste0(result5$nFixed))
    expect_equal(result5$nFixed1, c(150, 150), label = paste0(result5$nFixed1))
    expect_equal(result5$nFixed2, c(150, 150), label = paste0(result5$nFixed2))
    expect_equal(result5$calendarTime[1, ], c(3.4107281, 4.9286263), tolerance = 1e-07, label = paste0(result5$calendarTime[1, ]))
    expect_equal(result5$expectedStudyDurationH1, c(3.4107281, 4.9286263), tolerance = 1e-07, label = paste0(result5$expectedStudyDurationH1))
    expect_equal(result5$expectedNumberOfSubjectsH1, c(300, 300), label = paste0(result5$expectedNumberOfSubjectsH1))
    expect_equal(result5$maxInformation, c(144.85307, 193.47016), tolerance = 1e-07, label = paste0(result5$maxInformation))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result5), NA)))
        expect_output(print(result5)$show())
        invisible(capture.output(expect_error(summary(result5), NA)))
        expect_output(summary(result5)$show())
        result5CodeBased <- eval(parse(text = getObjectRCode(result5, stringWrapParagraphWidth = NULL)))
        expect_equal(result5CodeBased$directionUpper, result5$directionUpper, tolerance = 1e-07)
        expect_equal(result5CodeBased$theta, result5$theta, tolerance = 1e-07)
        expect_equal(result5CodeBased$nFixed, result5$nFixed, tolerance = 1e-07)
        expect_equal(result5CodeBased$nFixed1, result5$nFixed1, tolerance = 1e-07)
        expect_equal(result5CodeBased$nFixed2, result5$nFixed2, tolerance = 1e-07)
        expect_equal(result5CodeBased$calendarTime, result5$calendarTime, tolerance = 1e-07)
        expect_equal(result5CodeBased$expectedStudyDurationH1, result5$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(result5CodeBased$expectedNumberOfSubjectsH1, result5$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(result5CodeBased$maxInformation, result5$maxInformation, tolerance = 1e-07)
        expect_type(names(result5), "character")
        df <- as.data.frame(result5)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result5)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizePerStageVariableExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result6 <- getSampleSizeCounts(
        alpha = 0.01, beta = 0.05, theta = seq(1.05, 1.35, 0.15),
        lambda2 = 1.4, accrualTime = c(0, 10, 20), accrualIntensity = c(5, 10)
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result6' with expected results
    expect_equal(result6$directionUpper, c(TRUE, TRUE, TRUE), label = paste0(result6$directionUpper))
    expect_equal(result6$lambda1, c(1.47, 1.68, 1.89), tolerance = 1e-07, label = paste0(result6$lambda1))
    expect_equal(result6$nFixed, c(150, 150, 150), label = paste0(result6$nFixed))
    expect_equal(result6$nFixed1, c(75, 75, 75), label = paste0(result6$nFixed1))
    expect_equal(result6$nFixed2, c(75, 75, 75), label = paste0(result6$nFixed2))
    expect_equal(result6$maxNumberOfSubjects, c(150, 150, 150), label = paste0(result6$maxNumberOfSubjects))
    expect_equal(result6$calendarTime[1, ], c(134.88407, 19.983394, 12.884843), tolerance = 1e-07, label = paste0(result6$calendarTime[1, ]))
    expect_equal(result6$expectedStudyDurationH1, c(134.88407, 19.983394, 12.884843), tolerance = 1e-07, label = paste0(result6$expectedStudyDurationH1))
    expect_equal(result6$expectedNumberOfSubjectsH1, c(150, 148, 78), label = paste0(result6$expectedNumberOfSubjectsH1))
    expect_equal(result6$maxInformation, c(6624.8994, 474.42526, 175.10501), tolerance = 1e-07, label = paste0(result6$maxInformation))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result6), NA)))
        expect_output(print(result6)$show())
        invisible(capture.output(expect_error(summary(result6), NA)))
        expect_output(summary(result6)$show())
        result6CodeBased <- eval(parse(text = getObjectRCode(result6, stringWrapParagraphWidth = NULL)))
        expect_equal(result6CodeBased$directionUpper, result6$directionUpper, tolerance = 1e-07)
        expect_equal(result6CodeBased$lambda1, result6$lambda1, tolerance = 1e-07)
        expect_equal(result6CodeBased$nFixed, result6$nFixed, tolerance = 1e-07)
        expect_equal(result6CodeBased$nFixed1, result6$nFixed1, tolerance = 1e-07)
        expect_equal(result6CodeBased$nFixed2, result6$nFixed2, tolerance = 1e-07)
        expect_equal(result6CodeBased$maxNumberOfSubjects, result6$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(result6CodeBased$calendarTime, result6$calendarTime, tolerance = 1e-07)
        expect_equal(result6CodeBased$expectedStudyDurationH1, result6$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(result6CodeBased$expectedNumberOfSubjectsH1, result6$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(result6CodeBased$maxInformation, result6$maxInformation, tolerance = 1e-07)
        expect_type(names(result6), "character")
        df <- as.data.frame(result6)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result6)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizeCountsFixedExposureSingleStage}
    # @refFS[Formula]{fs:sampleSizePerStageFixedExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    # @refFS[Formula]{fs:sampleSizeCountsFixedExposureOptimumAllocationRatio}
    result7 <- getSampleSizeCounts(
        alpha = 0.01, beta = 0.05, lambda2 = 0.234, lambda1 = c(0.1, 0.2),
        allocationRatioPlanned = 0, accrualTime = 7, fixedExposureTime = 1
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result7' with expected results
    expect_equal(result7$allocationRatioPlanned, c(1.5297048, 1.0816655), tolerance = 1e-07, label = paste0(result7$allocationRatioPlanned))
    expect_equal(result7$directionUpper, c(FALSE, FALSE), label = paste0(result7$directionUpper))
    expect_equal(result7$theta, c(0.42735043, 0.85470085), tolerance = 1e-07, label = paste0(result7$theta))
    expect_equal(result7$nFixed, c(597, 11849), label = paste0(result7$nFixed))
    expect_equal(result7$nFixed1, c(361, 6157), label = paste0(result7$nFixed1))
    expect_equal(result7$nFixed2, c(236, 5692), label = paste0(result7$nFixed2))
    expect_equal(result7$maxNumberOfSubjects, c(597, 11849), label = paste0(result7$maxNumberOfSubjects))
    expect_equal(result7$calendarTime[1, ], c(8, 8), label = paste0(result7$calendarTime[1, ]))
    expect_equal(result7$expectedStudyDurationH1, c(8, 8), label = paste0(result7$expectedStudyDurationH1))
    expect_equal(result7$expectedNumberOfSubjectsH1, c(597, 11849), label = paste0(result7$expectedNumberOfSubjectsH1))
    expect_equal(result7$maxInformation, c(21.819851, 639.7699), tolerance = 1e-07, label = paste0(result7$maxInformation))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result7), NA)))
        expect_output(print(result7)$show())
        invisible(capture.output(expect_error(summary(result7), NA)))
        expect_output(summary(result7)$show())
        result7CodeBased <- eval(parse(text = getObjectRCode(result7, stringWrapParagraphWidth = NULL)))
        expect_equal(result7CodeBased$allocationRatioPlanned, result7$allocationRatioPlanned, tolerance = 1e-07)
        expect_equal(result7CodeBased$directionUpper, result7$directionUpper, tolerance = 1e-07)
        expect_equal(result7CodeBased$theta, result7$theta, tolerance = 1e-07)
        expect_equal(result7CodeBased$nFixed, result7$nFixed, tolerance = 1e-07)
        expect_equal(result7CodeBased$nFixed1, result7$nFixed1, tolerance = 1e-07)
        expect_equal(result7CodeBased$nFixed2, result7$nFixed2, tolerance = 1e-07)
        expect_equal(result7CodeBased$maxNumberOfSubjects, result7$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(result7CodeBased$calendarTime, result7$calendarTime, tolerance = 1e-07)
        expect_equal(result7CodeBased$expectedStudyDurationH1, result7$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(result7CodeBased$expectedNumberOfSubjectsH1, result7$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(result7CodeBased$maxInformation, result7$maxInformation, tolerance = 1e-07)
        expect_type(names(result7), "character")
        df <- as.data.frame(result7)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result7)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizePerStageVariableExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    # @refFS[Formula]{fs:sampleSizeCountsVariableExposureOptimumAllocationRatio}
    result8 <- getSampleSizeCounts(
        alpha = 0.01, beta = 0.05, lambda2 = 0.234, lambda1 = c(0.1, 0.2),
        allocationRatioPlanned = 0, accrualTime = 7, followUpTime = 1
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result8' with expected results
    expect_equal(result8$allocationRatioPlanned, c(1.1613522, 1.0659374), tolerance = 1e-07, label = paste0(result8$allocationRatioPlanned))
    expect_equal(result8$directionUpper, c(FALSE, FALSE), label = paste0(result8$directionUpper))
    expect_equal(result8$theta, c(0.42735043, 0.85470085), tolerance = 1e-07, label = paste0(result8$theta))
    expect_equal(result8$nFixed, c(135, 2633), label = paste0(result8$nFixed))
    expect_equal(result8$nFixed1, c(73, 1359), label = paste0(result8$nFixed1))
    expect_equal(result8$nFixed2, c(62, 1274), label = paste0(result8$nFixed2))
    expect_equal(result8$maxNumberOfSubjects, c(135, 2633), label = paste0(result8$maxNumberOfSubjects))
    expect_equal(result8$calendarTime[1, ], c(8, 8), label = paste0(result8$calendarTime[1, ]))
    expect_equal(result8$expectedStudyDurationH1, c(8, 8), label = paste0(result8$expectedStudyDurationH1))
    expect_equal(result8$expectedNumberOfSubjectsH1, c(135, 2633), label = paste0(result8$expectedNumberOfSubjectsH1))
    expect_equal(result8$maxInformation, c(21.819851, 639.7699), tolerance = 1e-07, label = paste0(result8$maxInformation))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result8), NA)))
        expect_output(print(result8)$show())
        invisible(capture.output(expect_error(summary(result8), NA)))
        expect_output(summary(result8)$show())
        result8CodeBased <- eval(parse(text = getObjectRCode(result8, stringWrapParagraphWidth = NULL)))
        expect_equal(result8CodeBased$allocationRatioPlanned, result8$allocationRatioPlanned, tolerance = 1e-07)
        expect_equal(result8CodeBased$directionUpper, result8$directionUpper, tolerance = 1e-07)
        expect_equal(result8CodeBased$theta, result8$theta, tolerance = 1e-07)
        expect_equal(result8CodeBased$nFixed, result8$nFixed, tolerance = 1e-07)
        expect_equal(result8CodeBased$nFixed1, result8$nFixed1, tolerance = 1e-07)
        expect_equal(result8CodeBased$nFixed2, result8$nFixed2, tolerance = 1e-07)
        expect_equal(result8CodeBased$maxNumberOfSubjects, result8$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(result8CodeBased$calendarTime, result8$calendarTime, tolerance = 1e-07)
        expect_equal(result8CodeBased$expectedStudyDurationH1, result8$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(result8CodeBased$expectedNumberOfSubjectsH1, result8$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(result8CodeBased$maxInformation, result8$maxInformation, tolerance = 1e-07)
        expect_type(names(result8), "character")
        df <- as.data.frame(result8)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result8)
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
    result1 <- getSampleSizeCounts(
        design = designGS1, lambda = 0.234, theta = 0.7,
        overdispersion = 0.71, accrualTime = 7, fixedExposureTime = 1
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result1' with expected results
    expect_equal(result1$directionUpper, FALSE, label = paste0(result1$directionUpper))
    expect_equal(result1$lambda1, 0.19270588, tolerance = 1e-07, label = paste0(result1$lambda1))
    expect_equal(result1$lambda2, 0.27529412, tolerance = 1e-07, label = paste0(result1$lambda2))
    expect_equal(result1$maxNumberOfSubjects, 1806, label = paste0(result1$maxNumberOfSubjects))
    expect_equal(result1$maxNumberOfSubjects1, 903, label = paste0(result1$maxNumberOfSubjects1))
    expect_equal(result1$maxNumberOfSubjects2, 903, label = paste0(result1$maxNumberOfSubjects2))
    expect_equal(result1$rejectPerStage[1, ], 0.19514116, tolerance = 1e-07, label = paste0(result1$rejectPerStage[1, ]))
    expect_equal(result1$rejectPerStage[2, ], 0.37204423, tolerance = 1e-07, label = paste0(result1$rejectPerStage[2, ]))
    expect_equal(result1$rejectPerStage[3, ], 0.33281461, tolerance = 1e-07, label = paste0(result1$rejectPerStage[3, ]))
    expect_equal(result1$futilityStop, 0.015698246, tolerance = 1e-07, label = paste0(result1$futilityStop))
    expect_equal(result1$futilityPerStage[1, ], 0.01090105, tolerance = 1e-07, label = paste0(result1$futilityPerStage[1, ]))
    expect_equal(result1$futilityPerStage[2, ], 0.004797196, tolerance = 1e-07, label = paste0(result1$futilityPerStage[2, ]))
    expect_equal(result1$earlyStop, 0.58288364, tolerance = 1e-07, label = paste0(result1$earlyStop))
    expect_equal(result1$calendarTime[1, ], 2.7119374, tolerance = 1e-07, label = paste0(result1$calendarTime[1, ]))
    expect_equal(result1$calendarTime[2, ], 4.4624998, tolerance = 1e-07, label = paste0(result1$calendarTime[2, ]))
    expect_equal(result1$calendarTime[3, ], 8, label = paste0(result1$calendarTime[3, ]))
    expect_equal(result1$expectedStudyDurationH1, 5.5773593, tolerance = 1e-07, label = paste0(result1$expectedStudyDurationH1))
    expect_equal(result1$numberOfSubjects[1, ], 700, label = paste0(result1$numberOfSubjects[1, ]))
    expect_equal(result1$numberOfSubjects[2, ], 1152, label = paste0(result1$numberOfSubjects[2, ]))
    expect_equal(result1$numberOfSubjects[3, ], 1806, label = paste0(result1$numberOfSubjects[3, ]))
    expect_equal(result1$expectedNumberOfSubjectsH1, 1331.663, tolerance = 1e-07, label = paste0(result1$expectedNumberOfSubjectsH1))
    expect_equal(result1$informationOverStages[1, ], 28.191756, tolerance = 1e-07, label = paste0(result1$informationOverStages[1, ]))
    expect_equal(result1$informationOverStages[2, ], 50.216566, tolerance = 1e-07, label = paste0(result1$informationOverStages[2, ]))
    expect_equal(result1$informationOverStages[3, ], 88.099238, tolerance = 1e-07, label = paste0(result1$informationOverStages[3, ]))
    expect_equal(result1$expectedInformationH0, 58.055315, tolerance = 1e-07, label = paste0(result1$expectedInformationH0))
    expect_equal(result1$expectedInformationH01, 74.084094, tolerance = 1e-07, label = paste0(result1$expectedInformationH01))
    expect_equal(result1$expectedInformationH1, 61.480008, tolerance = 1e-07, label = paste0(result1$expectedInformationH1))
    expect_equal(result1$maxInformation, 88.099238, tolerance = 1e-07, label = paste0(result1$maxInformation))
    expect_equal(result1$futilityBoundsPValueScale[1, ], 0.65542174, tolerance = 1e-07, label = paste0(result1$futilityBoundsPValueScale[1, ]))
    expect_equal(result1$futilityBoundsPValueScale[2, ], 0.46017216, tolerance = 1e-07, label = paste0(result1$futilityBoundsPValueScale[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result1), NA)))
        expect_output(print(result1)$show())
        invisible(capture.output(expect_error(summary(result1), NA)))
        expect_output(summary(result1)$show())
        result1CodeBased <- eval(parse(text = getObjectRCode(result1, stringWrapParagraphWidth = NULL)))
        expect_equal(result1CodeBased$directionUpper, result1$directionUpper, tolerance = 1e-07)
        expect_equal(result1CodeBased$lambda1, result1$lambda1, tolerance = 1e-07)
        expect_equal(result1CodeBased$lambda2, result1$lambda2, tolerance = 1e-07)
        expect_equal(result1CodeBased$maxNumberOfSubjects, result1$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(result1CodeBased$maxNumberOfSubjects1, result1$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(result1CodeBased$maxNumberOfSubjects2, result1$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(result1CodeBased$rejectPerStage, result1$rejectPerStage, tolerance = 1e-07)
        expect_equal(result1CodeBased$futilityStop, result1$futilityStop, tolerance = 1e-07)
        expect_equal(result1CodeBased$futilityPerStage, result1$futilityPerStage, tolerance = 1e-07)
        expect_equal(result1CodeBased$earlyStop, result1$earlyStop, tolerance = 1e-07)
        expect_equal(result1CodeBased$calendarTime, result1$calendarTime, tolerance = 1e-07)
        expect_equal(result1CodeBased$expectedStudyDurationH1, result1$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(result1CodeBased$numberOfSubjects, result1$numberOfSubjects, tolerance = 1e-07)
        expect_equal(result1CodeBased$expectedNumberOfSubjectsH1, result1$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(result1CodeBased$informationOverStages, result1$informationOverStages, tolerance = 1e-07)
        expect_equal(result1CodeBased$expectedInformationH0, result1$expectedInformationH0, tolerance = 1e-07)
        expect_equal(result1CodeBased$expectedInformationH01, result1$expectedInformationH01, tolerance = 1e-07)
        expect_equal(result1CodeBased$expectedInformationH1, result1$expectedInformationH1, tolerance = 1e-07)
        expect_equal(result1CodeBased$maxInformation, result1$maxInformation, tolerance = 1e-07)
        expect_equal(result1CodeBased$futilityBoundsPValueScale, result1$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(result1), "character")
        df <- as.data.frame(result1)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result1)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizePerStageVariableExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result2 <- getSampleSizeCounts(
        design = designGS1, lambda = 0.234, theta = 0.7,
        overdispersion = 0.71, accrualTime = 7, followUpTime = 1
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result2' with expected results
    expect_equal(result2$directionUpper, FALSE, label = paste0(result2$directionUpper))
    expect_equal(result2$lambda1, 0.19270588, tolerance = 1e-07, label = paste0(result2$lambda1))
    expect_equal(result2$lambda2, 0.27529412, tolerance = 1e-07, label = paste0(result2$lambda2))
    expect_equal(result2$maxNumberOfSubjects, 630, label = paste0(result2$maxNumberOfSubjects))
    expect_equal(result2$maxNumberOfSubjects1, 315, label = paste0(result2$maxNumberOfSubjects1))
    expect_equal(result2$maxNumberOfSubjects2, 315, label = paste0(result2$maxNumberOfSubjects2))
    expect_equal(result2$rejectPerStage[1, ], 0.19514116, tolerance = 1e-07, label = paste0(result2$rejectPerStage[1, ]))
    expect_equal(result2$rejectPerStage[2, ], 0.37204423, tolerance = 1e-07, label = paste0(result2$rejectPerStage[2, ]))
    expect_equal(result2$rejectPerStage[3, ], 0.33281461, tolerance = 1e-07, label = paste0(result2$rejectPerStage[3, ]))
    expect_equal(result2$futilityStop, 0.015698246, tolerance = 1e-07, label = paste0(result2$futilityStop))
    expect_equal(result2$futilityPerStage[1, ], 0.01090105, tolerance = 1e-07, label = paste0(result2$futilityPerStage[1, ]))
    expect_equal(result2$futilityPerStage[2, ], 0.004797196, tolerance = 1e-07, label = paste0(result2$futilityPerStage[2, ]))
    expect_equal(result2$earlyStop, 0.58288364, tolerance = 1e-07, label = paste0(result2$earlyStop))
    expect_equal(result2$calendarTime[1, ], 3.9379751, tolerance = 1e-07, label = paste0(result2$calendarTime[1, ]))
    expect_equal(result2$calendarTime[2, ], 5.5492716, tolerance = 1e-07, label = paste0(result2$calendarTime[2, ]))
    expect_equal(result2$calendarTime[3, ], 8, label = paste0(result2$calendarTime[3, ]))
    expect_equal(result2$expectedStudyDurationH1, 6.2395154, tolerance = 1e-07, label = paste0(result2$expectedStudyDurationH1))
    expect_equal(result2$numberOfSubjects[1, ], 354, label = paste0(result2$numberOfSubjects[1, ]))
    expect_equal(result2$numberOfSubjects[2, ], 498, label = paste0(result2$numberOfSubjects[2, ]))
    expect_equal(result2$numberOfSubjects[3, ], 630, label = paste0(result2$numberOfSubjects[3, ]))
    expect_equal(result2$expectedNumberOfSubjectsH1, 523.38928, tolerance = 1e-07, label = paste0(result2$expectedNumberOfSubjectsH1))
    expect_equal(result2$informationOverStages[1, ], 28.191756, tolerance = 1e-07, label = paste0(result2$informationOverStages[1, ]))
    expect_equal(result2$informationOverStages[2, ], 50.216566, tolerance = 1e-07, label = paste0(result2$informationOverStages[2, ]))
    expect_equal(result2$informationOverStages[3, ], 88.099238, tolerance = 1e-07, label = paste0(result2$informationOverStages[3, ]))
    expect_equal(result2$expectedInformationH0, 58.055315, tolerance = 1e-07, label = paste0(result2$expectedInformationH0))
    expect_equal(result2$expectedInformationH01, 74.084094, tolerance = 1e-07, label = paste0(result2$expectedInformationH01))
    expect_equal(result2$expectedInformationH1, 61.480008, tolerance = 1e-07, label = paste0(result2$expectedInformationH1))
    expect_equal(result2$maxInformation, 88.099238, tolerance = 1e-07, label = paste0(result2$maxInformation))
    expect_equal(result2$futilityBoundsPValueScale[1, ], 0.65542174, tolerance = 1e-07, label = paste0(result2$futilityBoundsPValueScale[1, ]))
    expect_equal(result2$futilityBoundsPValueScale[2, ], 0.46017216, tolerance = 1e-07, label = paste0(result2$futilityBoundsPValueScale[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result2), NA)))
        expect_output(print(result2)$show())
        invisible(capture.output(expect_error(summary(result2), NA)))
        expect_output(summary(result2)$show())
        result2CodeBased <- eval(parse(text = getObjectRCode(result2, stringWrapParagraphWidth = NULL)))
        expect_equal(result2CodeBased$directionUpper, result2$directionUpper, tolerance = 1e-07)
        expect_equal(result2CodeBased$lambda1, result2$lambda1, tolerance = 1e-07)
        expect_equal(result2CodeBased$lambda2, result2$lambda2, tolerance = 1e-07)
        expect_equal(result2CodeBased$maxNumberOfSubjects, result2$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(result2CodeBased$maxNumberOfSubjects1, result2$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(result2CodeBased$maxNumberOfSubjects2, result2$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(result2CodeBased$rejectPerStage, result2$rejectPerStage, tolerance = 1e-07)
        expect_equal(result2CodeBased$futilityStop, result2$futilityStop, tolerance = 1e-07)
        expect_equal(result2CodeBased$futilityPerStage, result2$futilityPerStage, tolerance = 1e-07)
        expect_equal(result2CodeBased$earlyStop, result2$earlyStop, tolerance = 1e-07)
        expect_equal(result2CodeBased$calendarTime, result2$calendarTime, tolerance = 1e-07)
        expect_equal(result2CodeBased$expectedStudyDurationH1, result2$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(result2CodeBased$numberOfSubjects, result2$numberOfSubjects, tolerance = 1e-07)
        expect_equal(result2CodeBased$expectedNumberOfSubjectsH1, result2$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(result2CodeBased$informationOverStages, result2$informationOverStages, tolerance = 1e-07)
        expect_equal(result2CodeBased$expectedInformationH0, result2$expectedInformationH0, tolerance = 1e-07)
        expect_equal(result2CodeBased$expectedInformationH01, result2$expectedInformationH01, tolerance = 1e-07)
        expect_equal(result2CodeBased$expectedInformationH1, result2$expectedInformationH1, tolerance = 1e-07)
        expect_equal(result2CodeBased$maxInformation, result2$maxInformation, tolerance = 1e-07)
        expect_equal(result2CodeBased$futilityBoundsPValueScale, result2$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(result2), "character")
        df <- as.data.frame(result2)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result2)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizeCountsFixedExposureSingleStage}
    # @refFS[Formula]{fs:sampleSizePerStageFixedExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result3 <- getSampleSizeCounts(
        design = designGS1, lambda2 = 0.02, theta = c(0.7, 0.8),
        accrualTime = 7, fixedExposureTime = 2
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result3' with expected results
    expect_equal(result3$directionUpper, c(FALSE, FALSE), label = paste0(result3$directionUpper))
    expect_equal(result3$lambda1, c(0.014, 0.016), tolerance = 1e-07, label = paste0(result3$lambda1))
    expect_equal(result3$maxNumberOfSubjects, c(10698, 25324), label = paste0(result3$maxNumberOfSubjects))
    expect_equal(result3$maxNumberOfSubjects1, c(5349, 12662), label = paste0(result3$maxNumberOfSubjects1))
    expect_equal(result3$maxNumberOfSubjects2, c(5349, 12662), label = paste0(result3$maxNumberOfSubjects2))
    expect_equal(result3$rejectPerStage[1, ], 0.19514116, tolerance = 1e-07, label = paste0(result3$rejectPerStage[1, ]))
    expect_equal(result3$rejectPerStage[2, ], 0.37204423, tolerance = 1e-07, label = paste0(result3$rejectPerStage[2, ]))
    expect_equal(result3$rejectPerStage[3, ], 0.33281461, tolerance = 1e-07, label = paste0(result3$rejectPerStage[3, ]))
    expect_equal(result3$futilityStop, 0.015698246, tolerance = 1e-07, label = paste0(result3$futilityStop))
    expect_equal(result3$futilityPerStage[1, ], 0.01090105, tolerance = 1e-07, label = paste0(result3$futilityPerStage[1, ]))
    expect_equal(result3$futilityPerStage[2, ], 0.004797196, tolerance = 1e-07, label = paste0(result3$futilityPerStage[2, ]))
    expect_equal(result3$earlyStop, 0.58288364, tolerance = 1e-07, label = paste0(result3$earlyStop))
    expect_equal(result3$calendarTime[1, ], c(3.2397151, 3.23974), tolerance = 1e-07, label = paste0(result3$calendarTime[1, ]))
    expect_equal(result3$calendarTime[2, ], c(4.9900038, 4.9897529), tolerance = 1e-07, label = paste0(result3$calendarTime[2, ]))
    expect_equal(result3$calendarTime[3, ], c(9, 9), label = paste0(result3$calendarTime[3, ]))
    expect_equal(result3$expectedStudyDurationH1, c(6.3020055, 6.3019161), tolerance = 1e-07, label = paste0(result3$expectedStudyDurationH1))
    expect_equal(result3$numberOfSubjects[1, ], c(4952, 11720), label = paste0(result3$numberOfSubjects[1, ]))
    expect_equal(result3$numberOfSubjects[2, ], c(7626, 18052), label = paste0(result3$numberOfSubjects[2, ]))
    expect_equal(result3$numberOfSubjects[3, ], c(10698, 25324), label = paste0(result3$numberOfSubjects[3, ]))
    expect_equal(result3$expectedNumberOfSubjectsH1, c(8356.4246, 19780.611), tolerance = 1e-07, label = paste0(result3$expectedNumberOfSubjectsH1))
    expect_equal(result3$informationOverStages[1, ], c(28.191756, 72.027552), tolerance = 1e-07, label = paste0(result3$informationOverStages[1, ]))
    expect_equal(result3$informationOverStages[2, ], c(50.216566, 128.29908), tolerance = 1e-07, label = paste0(result3$informationOverStages[2, ]))
    expect_equal(result3$informationOverStages[3, ], c(88.099238, 225.0861), tolerance = 1e-07, label = paste0(result3$informationOverStages[3, ]))
    expect_equal(result3$expectedInformationH0, c(58.055315, 148.32642), tolerance = 1e-07, label = paste0(result3$expectedInformationH0))
    expect_equal(result3$expectedInformationH01, c(74.084094, 189.27859), tolerance = 1e-07, label = paste0(result3$expectedInformationH01))
    expect_equal(result3$expectedInformationH1, c(61.480008, 157.07622), tolerance = 1e-07, label = paste0(result3$expectedInformationH1))
    expect_equal(result3$maxInformation, c(88.099238, 225.0861), tolerance = 1e-07, label = paste0(result3$maxInformation))
    expect_equal(result3$futilityBoundsPValueScale[1, ], 0.65542174, tolerance = 1e-07, label = paste0(result3$futilityBoundsPValueScale[1, ]))
    expect_equal(result3$futilityBoundsPValueScale[2, ], 0.46017216, tolerance = 1e-07, label = paste0(result3$futilityBoundsPValueScale[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result3), NA)))
        expect_output(print(result3)$show())
        invisible(capture.output(expect_error(summary(result3), NA)))
        expect_output(summary(result3)$show())
        result3CodeBased <- eval(parse(text = getObjectRCode(result3, stringWrapParagraphWidth = NULL)))
        expect_equal(result3CodeBased$directionUpper, result3$directionUpper, tolerance = 1e-07)
        expect_equal(result3CodeBased$lambda1, result3$lambda1, tolerance = 1e-07)
        expect_equal(result3CodeBased$maxNumberOfSubjects, result3$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(result3CodeBased$maxNumberOfSubjects1, result3$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(result3CodeBased$maxNumberOfSubjects2, result3$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(result3CodeBased$rejectPerStage, result3$rejectPerStage, tolerance = 1e-07)
        expect_equal(result3CodeBased$futilityStop, result3$futilityStop, tolerance = 1e-07)
        expect_equal(result3CodeBased$futilityPerStage, result3$futilityPerStage, tolerance = 1e-07)
        expect_equal(result3CodeBased$earlyStop, result3$earlyStop, tolerance = 1e-07)
        expect_equal(result3CodeBased$calendarTime, result3$calendarTime, tolerance = 1e-07)
        expect_equal(result3CodeBased$expectedStudyDurationH1, result3$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(result3CodeBased$numberOfSubjects, result3$numberOfSubjects, tolerance = 1e-07)
        expect_equal(result3CodeBased$expectedNumberOfSubjectsH1, result3$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(result3CodeBased$informationOverStages, result3$informationOverStages, tolerance = 1e-07)
        expect_equal(result3CodeBased$expectedInformationH0, result3$expectedInformationH0, tolerance = 1e-07)
        expect_equal(result3CodeBased$expectedInformationH01, result3$expectedInformationH01, tolerance = 1e-07)
        expect_equal(result3CodeBased$expectedInformationH1, result3$expectedInformationH1, tolerance = 1e-07)
        expect_equal(result3CodeBased$maxInformation, result3$maxInformation, tolerance = 1e-07)
        expect_equal(result3CodeBased$futilityBoundsPValueScale, result3$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(result3), "character")
        df <- as.data.frame(result3)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result3)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizePerStageVariableExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result4 <- getSampleSizeCounts(
        design = designGS1, lambda1 = seq(1.05, 1.35, 0.15),
        lambda2 = 1.4, accrualTime = c(0, 20), allocationRatioPlanned = 4, maxNumberOfSubjects = 100
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result4' with expected results
    expect_equal(result4$directionUpper, c(FALSE, FALSE, FALSE), label = paste0(result4$directionUpper))
    expect_equal(result4$theta, c(0.75, 0.85714286, 0.96428571), tolerance = 1e-07, label = paste0(result4$theta))
    expect_equal(result4$rejectPerStage[1, ], 0.19514116, tolerance = 1e-07, label = paste0(result4$rejectPerStage[1, ]))
    expect_equal(result4$rejectPerStage[2, ], 0.37204423, tolerance = 1e-07, label = paste0(result4$rejectPerStage[2, ]))
    expect_equal(result4$rejectPerStage[3, ], 0.33281461, tolerance = 1e-07, label = paste0(result4$rejectPerStage[3, ]))
    expect_equal(result4$futilityStop, 0.015698246, tolerance = 1e-07, label = paste0(result4$futilityStop))
    expect_equal(result4$futilityPerStage[1, ], 0.01090105, tolerance = 1e-07, label = paste0(result4$futilityPerStage[1, ]))
    expect_equal(result4$futilityPerStage[2, ], 0.004797196, tolerance = 1e-07, label = paste0(result4$futilityPerStage[2, ]))
    expect_equal(result4$earlyStop, 0.58288364, tolerance = 1e-07, label = paste0(result4$earlyStop))
    expect_equal(result4$calendarTime[1, ], c(8.8532025, 16.614079, 131.95346), tolerance = 1e-07, label = paste0(result4$calendarTime[1, ]))
    expect_equal(result4$calendarTime[2, ], c(11.956509, 22.402058, 227.2296), tolerance = 1e-07, label = paste0(result4$calendarTime[2, ]))
    expect_equal(result4$calendarTime[3, ], c(15.975753, 31.757996, 391.10455), tolerance = 1e-07, label = paste0(result4$calendarTime[3, ]))
    expect_equal(result4$expectedStudyDurationH1, c(12.993589, 25.112005, 275.95362), tolerance = 1e-07, label = paste0(result4$expectedStudyDurationH1))
    expect_equal(result4$numberOfSubjects[1, ], c(44, 82, 100), label = paste0(result4$numberOfSubjects[1, ]))
    expect_equal(result4$numberOfSubjects[2, ], c(60, 100, 100), label = paste0(result4$numberOfSubjects[2, ]))
    expect_equal(result4$numberOfSubjects[3, ], c(80, 100, 100), label = paste0(result4$numberOfSubjects[3, ]))
    expect_equal(result4$expectedNumberOfSubjectsH1, c(65.045652, 96.29124, 100), tolerance = 1e-07, label = paste0(result4$expectedNumberOfSubjectsH1))
    expect_equal(result4$informationOverStages[1, ], c(43.335293, 150.9303, 2711.671), tolerance = 1e-07, label = paste0(result4$informationOverStages[1, ]))
    expect_equal(result4$informationOverStages[2, ], c(77.190991, 268.8446, 4830.164), tolerance = 1e-07, label = paste0(result4$informationOverStages[2, ]))
    expect_equal(result4$informationOverStages[3, ], c(135.42279, 471.6572, 8473.9719), tolerance = 1e-07, label = paste0(result4$informationOverStages[3, ]))
    expect_equal(result4$expectedInformationH0, c(89.240417, 310.81094, 5584.1471), tolerance = 1e-07, label = paste0(result4$expectedInformationH0))
    expect_equal(result4$expectedInformationH01, c(113.87925, 396.62427, 7125.9019), tolerance = 1e-07, label = paste0(result4$expectedInformationH01))
    expect_equal(result4$expectedInformationH1, c(94.504724, 329.14573, 5913.5569), tolerance = 1e-07, label = paste0(result4$expectedInformationH1))
    expect_equal(result4$maxInformation, c(135.42279, 471.6572, 8473.9719), tolerance = 1e-07, label = paste0(result4$maxInformation))
    expect_equal(result4$futilityBoundsPValueScale[1, ], 0.65542174, tolerance = 1e-07, label = paste0(result4$futilityBoundsPValueScale[1, ]))
    expect_equal(result4$futilityBoundsPValueScale[2, ], 0.46017216, tolerance = 1e-07, label = paste0(result4$futilityBoundsPValueScale[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result4), NA)))
        expect_output(print(result4)$show())
        invisible(capture.output(expect_error(summary(result4), NA)))
        expect_output(summary(result4)$show())
        result4CodeBased <- eval(parse(text = getObjectRCode(result4, stringWrapParagraphWidth = NULL)))
        expect_equal(result4CodeBased$directionUpper, result4$directionUpper, tolerance = 1e-07)
        expect_equal(result4CodeBased$theta, result4$theta, tolerance = 1e-07)
        expect_equal(result4CodeBased$rejectPerStage, result4$rejectPerStage, tolerance = 1e-07)
        expect_equal(result4CodeBased$futilityStop, result4$futilityStop, tolerance = 1e-07)
        expect_equal(result4CodeBased$futilityPerStage, result4$futilityPerStage, tolerance = 1e-07)
        expect_equal(result4CodeBased$earlyStop, result4$earlyStop, tolerance = 1e-07)
        expect_equal(result4CodeBased$calendarTime, result4$calendarTime, tolerance = 1e-07)
        expect_equal(result4CodeBased$expectedStudyDurationH1, result4$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(result4CodeBased$numberOfSubjects, result4$numberOfSubjects, tolerance = 1e-07)
        expect_equal(result4CodeBased$expectedNumberOfSubjectsH1, result4$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(result4CodeBased$informationOverStages, result4$informationOverStages, tolerance = 1e-07)
        expect_equal(result4CodeBased$expectedInformationH0, result4$expectedInformationH0, tolerance = 1e-07)
        expect_equal(result4CodeBased$expectedInformationH01, result4$expectedInformationH01, tolerance = 1e-07)
        expect_equal(result4CodeBased$expectedInformationH1, result4$expectedInformationH1, tolerance = 1e-07)
        expect_equal(result4CodeBased$maxInformation, result4$maxInformation, tolerance = 1e-07)
        expect_equal(result4CodeBased$futilityBoundsPValueScale, result4$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(result4), "character")
        df <- as.data.frame(result4)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result4)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizePerStageVariableExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result5 <- getSampleSizeCounts(
        design = designGS1, lambda1 = seq(1.05, 1.35, 0.15),
        lambda2 = 1.4, accrualTime = c(0, 10, 20), accrualIntensity = c(5, 10)
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result5' with expected results
    expect_equal(result5$directionUpper, c(FALSE, FALSE, FALSE), label = paste0(result5$directionUpper))
    expect_equal(result5$theta, c(0.75, 0.85714286, 0.96428571), tolerance = 1e-07, label = paste0(result5$theta))
    expect_equal(result5$maxNumberOfSubjects, c(150, 150, 150), label = paste0(result5$maxNumberOfSubjects))
    expect_equal(result5$maxNumberOfSubjects1, c(75, 75, 75), label = paste0(result5$maxNumberOfSubjects1))
    expect_equal(result5$maxNumberOfSubjects2, c(75, 75, 75), label = paste0(result5$maxNumberOfSubjects2))
    expect_equal(result5$rejectPerStage[1, ], 0.19514116, tolerance = 1e-07, label = paste0(result5$rejectPerStage[1, ]))
    expect_equal(result5$rejectPerStage[2, ], 0.37204423, tolerance = 1e-07, label = paste0(result5$rejectPerStage[2, ]))
    expect_equal(result5$rejectPerStage[3, ], 0.33281461, tolerance = 1e-07, label = paste0(result5$rejectPerStage[3, ]))
    expect_equal(result5$futilityStop, 0.015698246, tolerance = 1e-07, label = paste0(result5$futilityStop))
    expect_equal(result5$futilityPerStage[1, ], 0.01090105, tolerance = 1e-07, label = paste0(result5$futilityPerStage[1, ]))
    expect_equal(result5$futilityPerStage[2, ], 0.004797196, tolerance = 1e-07, label = paste0(result5$futilityPerStage[2, ]))
    expect_equal(result5$earlyStop, 0.58288364, tolerance = 1e-07, label = paste0(result5$earlyStop))
    expect_equal(result5$calendarTime[1, ], c(7.5513415, 13.27872, 64.307374), tolerance = 1e-07, label = paste0(result5$calendarTime[1, ]))
    expect_equal(result5$calendarTime[2, ], c(10.144294, 16.9121, 105.40688), tolerance = 1e-07, label = paste0(result5$calendarTime[2, ]))
    expect_equal(result5$calendarTime[3, ], c(13.085473, 21.432609, 176.09804), tolerance = 1e-07, label = paste0(result5$calendarTime[3, ]))
    expect_equal(result5$expectedStudyDurationH1, c(10.83685, 18.049049, 126.42509), tolerance = 1e-07, label = paste0(result5$expectedStudyDurationH1))
    expect_equal(result5$numberOfSubjects[1, ], c(38, 82, 150), label = paste0(result5$numberOfSubjects[1, ]))
    expect_equal(result5$numberOfSubjects[2, ], c(52, 118, 150), label = paste0(result5$numberOfSubjects[2, ]))
    expect_equal(result5$numberOfSubjects[3, ], c(80, 150, 150), label = paste0(result5$numberOfSubjects[3, ]))
    expect_equal(result5$expectedNumberOfSubjectsH1, c(60.794667, 123.9302, 150), tolerance = 1e-07, label = paste0(result5$expectedNumberOfSubjectsH1))
    expect_equal(result5$informationOverStages[1, ], c(43.335293, 150.9303, 2711.671), tolerance = 1e-07, label = paste0(result5$informationOverStages[1, ]))
    expect_equal(result5$informationOverStages[2, ], c(77.190991, 268.8446, 4830.164), tolerance = 1e-07, label = paste0(result5$informationOverStages[2, ]))
    expect_equal(result5$informationOverStages[3, ], c(135.42279, 471.6572, 8473.9719), tolerance = 1e-07, label = paste0(result5$informationOverStages[3, ]))
    expect_equal(result5$expectedInformationH0, c(89.240417, 310.81094, 5584.1471), tolerance = 1e-07, label = paste0(result5$expectedInformationH0))
    expect_equal(result5$expectedInformationH01, c(113.87925, 396.62427, 7125.9019), tolerance = 1e-07, label = paste0(result5$expectedInformationH01))
    expect_equal(result5$expectedInformationH1, c(94.504724, 329.14573, 5913.5569), tolerance = 1e-07, label = paste0(result5$expectedInformationH1))
    expect_equal(result5$maxInformation, c(135.42279, 471.6572, 8473.9719), tolerance = 1e-07, label = paste0(result5$maxInformation))
    expect_equal(result5$futilityBoundsPValueScale[1, ], 0.65542174, tolerance = 1e-07, label = paste0(result5$futilityBoundsPValueScale[1, ]))
    expect_equal(result5$futilityBoundsPValueScale[2, ], 0.46017216, tolerance = 1e-07, label = paste0(result5$futilityBoundsPValueScale[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result5), NA)))
        expect_output(print(result5)$show())
        invisible(capture.output(expect_error(summary(result5), NA)))
        expect_output(summary(result5)$show())
        result5CodeBased <- eval(parse(text = getObjectRCode(result5, stringWrapParagraphWidth = NULL)))
        expect_equal(result5CodeBased$directionUpper, result5$directionUpper, tolerance = 1e-07)
        expect_equal(result5CodeBased$theta, result5$theta, tolerance = 1e-07)
        expect_equal(result5CodeBased$maxNumberOfSubjects, result5$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(result5CodeBased$maxNumberOfSubjects1, result5$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(result5CodeBased$maxNumberOfSubjects2, result5$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(result5CodeBased$rejectPerStage, result5$rejectPerStage, tolerance = 1e-07)
        expect_equal(result5CodeBased$futilityStop, result5$futilityStop, tolerance = 1e-07)
        expect_equal(result5CodeBased$futilityPerStage, result5$futilityPerStage, tolerance = 1e-07)
        expect_equal(result5CodeBased$earlyStop, result5$earlyStop, tolerance = 1e-07)
        expect_equal(result5CodeBased$calendarTime, result5$calendarTime, tolerance = 1e-07)
        expect_equal(result5CodeBased$expectedStudyDurationH1, result5$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(result5CodeBased$numberOfSubjects, result5$numberOfSubjects, tolerance = 1e-07)
        expect_equal(result5CodeBased$expectedNumberOfSubjectsH1, result5$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(result5CodeBased$informationOverStages, result5$informationOverStages, tolerance = 1e-07)
        expect_equal(result5CodeBased$expectedInformationH0, result5$expectedInformationH0, tolerance = 1e-07)
        expect_equal(result5CodeBased$expectedInformationH01, result5$expectedInformationH01, tolerance = 1e-07)
        expect_equal(result5CodeBased$expectedInformationH1, result5$expectedInformationH1, tolerance = 1e-07)
        expect_equal(result5CodeBased$maxInformation, result5$maxInformation, tolerance = 1e-07)
        expect_equal(result5CodeBased$futilityBoundsPValueScale, result5$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(result5), "character")
        df <- as.data.frame(result5)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result5)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizePerStageVariableExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result6 <- getSampleSizeCounts(
        design = designGS1, thetaH0 = 0.9, lambda1 = seq(1.1, 1.15, 0.05),
        overdispersion = 0.2, lambda2 = 1.7, accrualTime = c(0, 2), maxNumberOfSubjects = 300
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result6' with expected results
    expect_equal(result6$directionUpper, c(FALSE, FALSE), label = paste0(result6$directionUpper))
    expect_equal(result6$theta, c(0.64705882, 0.67647059), tolerance = 1e-07, label = paste0(result6$theta))
    expect_equal(result6$rejectPerStage[1, ], 0.19514116, tolerance = 1e-07, label = paste0(result6$rejectPerStage[1, ]))
    expect_equal(result6$rejectPerStage[2, ], 0.37204423, tolerance = 1e-07, label = paste0(result6$rejectPerStage[2, ]))
    expect_equal(result6$rejectPerStage[3, ], 0.33281461, tolerance = 1e-07, label = paste0(result6$rejectPerStage[3, ]))
    expect_equal(result6$futilityStop, 0.015698246, tolerance = 1e-07, label = paste0(result6$futilityStop))
    expect_equal(result6$futilityPerStage[1, ], 0.01090105, tolerance = 1e-07, label = paste0(result6$futilityPerStage[1, ]))
    expect_equal(result6$futilityPerStage[2, ], 0.004797196, tolerance = 1e-07, label = paste0(result6$futilityPerStage[2, ]))
    expect_equal(result6$earlyStop, 0.58288364, tolerance = 1e-07, label = paste0(result6$earlyStop))
    expect_equal(result6$calendarTime[1, ], c(1.264719, 1.4673637), tolerance = 1e-07, label = paste0(result6$calendarTime[1, ]))
    expect_equal(result6$calendarTime[2, ], c(1.7456908, 2.0357161), tolerance = 1e-07, label = paste0(result6$calendarTime[2, ]))
    expect_equal(result6$calendarTime[3, ], c(2.4812416, 3.1679677), tolerance = 1e-07, label = paste0(result6$calendarTime[3, ]))
    expect_equal(result6$expectedStudyDurationH1, c(1.9534006, 2.3908922), tolerance = 1e-07, label = paste0(result6$expectedStudyDurationH1))
    expect_equal(result6$numberOfSubjects[1, ], c(190, 220), label = paste0(result6$numberOfSubjects[1, ]))
    expect_equal(result6$numberOfSubjects[2, ], c(262, 300), label = paste0(result6$numberOfSubjects[2, ]))
    expect_equal(result6$numberOfSubjects[3, ], c(300, 300), label = paste0(result6$numberOfSubjects[3, ]))
    expect_equal(result6$expectedNumberOfSubjectsH1, c(263.01538, 283.51662), tolerance = 1e-07, label = paste0(result6$expectedNumberOfSubjectsH1))
    expect_equal(result6$informationOverStages[1, ], c(32.942092, 43.998461), tolerance = 1e-07, label = paste0(result6$informationOverStages[1, ]))
    expect_equal(result6$informationOverStages[2, ], c(58.678102, 78.372259), tolerance = 1e-07, label = paste0(result6$informationOverStages[2, ]))
    expect_equal(result6$informationOverStages[3, ], c(102.94404, 137.49519), tolerance = 1e-07, label = paste0(result6$informationOverStages[3, ]))
    expect_equal(result6$expectedInformationH0, c(67.837688, 90.60608), tolerance = 1e-07, label = paste0(result6$expectedInformationH0))
    expect_equal(result6$expectedInformationH01, c(86.56733, 115.62196), tolerance = 1e-07, label = paste0(result6$expectedInformationH01))
    expect_equal(result6$expectedInformationH1, c(71.839444, 95.950948), tolerance = 1e-07, label = paste0(result6$expectedInformationH1))
    expect_equal(result6$maxInformation, c(102.94404, 137.49519), tolerance = 1e-07, label = paste0(result6$maxInformation))
    expect_equal(result6$futilityBoundsPValueScale[1, ], 0.65542174, tolerance = 1e-07, label = paste0(result6$futilityBoundsPValueScale[1, ]))
    expect_equal(result6$futilityBoundsPValueScale[2, ], 0.46017216, tolerance = 1e-07, label = paste0(result6$futilityBoundsPValueScale[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result6), NA)))
        expect_output(print(result6)$show())
        invisible(capture.output(expect_error(summary(result6), NA)))
        expect_output(summary(result6)$show())
        result6CodeBased <- eval(parse(text = getObjectRCode(result6, stringWrapParagraphWidth = NULL)))
        expect_equal(result6CodeBased$directionUpper, result6$directionUpper, tolerance = 1e-07)
        expect_equal(result6CodeBased$theta, result6$theta, tolerance = 1e-07)
        expect_equal(result6CodeBased$rejectPerStage, result6$rejectPerStage, tolerance = 1e-07)
        expect_equal(result6CodeBased$futilityStop, result6$futilityStop, tolerance = 1e-07)
        expect_equal(result6CodeBased$futilityPerStage, result6$futilityPerStage, tolerance = 1e-07)
        expect_equal(result6CodeBased$earlyStop, result6$earlyStop, tolerance = 1e-07)
        expect_equal(result6CodeBased$calendarTime, result6$calendarTime, tolerance = 1e-07)
        expect_equal(result6CodeBased$expectedStudyDurationH1, result6$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(result6CodeBased$numberOfSubjects, result6$numberOfSubjects, tolerance = 1e-07)
        expect_equal(result6CodeBased$expectedNumberOfSubjectsH1, result6$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(result6CodeBased$informationOverStages, result6$informationOverStages, tolerance = 1e-07)
        expect_equal(result6CodeBased$expectedInformationH0, result6$expectedInformationH0, tolerance = 1e-07)
        expect_equal(result6CodeBased$expectedInformationH01, result6$expectedInformationH01, tolerance = 1e-07)
        expect_equal(result6CodeBased$expectedInformationH1, result6$expectedInformationH1, tolerance = 1e-07)
        expect_equal(result6CodeBased$maxInformation, result6$maxInformation, tolerance = 1e-07)
        expect_equal(result6CodeBased$futilityBoundsPValueScale, result6$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(result6), "character")
        df <- as.data.frame(result6)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result6)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizePerStageVariableExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result7 <- getSampleSizeCounts(
        design = designGS1, lambda1 = seq(1.05, 1.35, 0.15),
        lambda2 = 1.4, accrualTime = c(0, 10, 20), accrualIntensity = c(5, 10)
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result7' with expected results
    expect_equal(result7$directionUpper, c(FALSE, FALSE, FALSE), label = paste0(result7$directionUpper))
    expect_equal(result7$theta, c(0.75, 0.85714286, 0.96428571), tolerance = 1e-07, label = paste0(result7$theta))
    expect_equal(result7$maxNumberOfSubjects, c(150, 150, 150), label = paste0(result7$maxNumberOfSubjects))
    expect_equal(result7$maxNumberOfSubjects1, c(75, 75, 75), label = paste0(result7$maxNumberOfSubjects1))
    expect_equal(result7$maxNumberOfSubjects2, c(75, 75, 75), label = paste0(result7$maxNumberOfSubjects2))
    expect_equal(result7$rejectPerStage[1, ], 0.19514116, tolerance = 1e-07, label = paste0(result7$rejectPerStage[1, ]))
    expect_equal(result7$rejectPerStage[2, ], 0.37204423, tolerance = 1e-07, label = paste0(result7$rejectPerStage[2, ]))
    expect_equal(result7$rejectPerStage[3, ], 0.33281461, tolerance = 1e-07, label = paste0(result7$rejectPerStage[3, ]))
    expect_equal(result7$futilityStop, 0.015698246, tolerance = 1e-07, label = paste0(result7$futilityStop))
    expect_equal(result7$futilityPerStage[1, ], 0.01090105, tolerance = 1e-07, label = paste0(result7$futilityPerStage[1, ]))
    expect_equal(result7$futilityPerStage[2, ], 0.004797196, tolerance = 1e-07, label = paste0(result7$futilityPerStage[2, ]))
    expect_equal(result7$earlyStop, 0.58288364, tolerance = 1e-07, label = paste0(result7$earlyStop))
    expect_equal(result7$calendarTime[1, ], c(7.5513415, 13.27872, 64.307374), tolerance = 1e-07, label = paste0(result7$calendarTime[1, ]))
    expect_equal(result7$calendarTime[2, ], c(10.144294, 16.9121, 105.40688), tolerance = 1e-07, label = paste0(result7$calendarTime[2, ]))
    expect_equal(result7$calendarTime[3, ], c(13.085473, 21.432609, 176.09804), tolerance = 1e-07, label = paste0(result7$calendarTime[3, ]))
    expect_equal(result7$expectedStudyDurationH1, c(10.83685, 18.049049, 126.42509), tolerance = 1e-07, label = paste0(result7$expectedStudyDurationH1))
    expect_equal(result7$numberOfSubjects[1, ], c(38, 82, 150), label = paste0(result7$numberOfSubjects[1, ]))
    expect_equal(result7$numberOfSubjects[2, ], c(52, 118, 150), label = paste0(result7$numberOfSubjects[2, ]))
    expect_equal(result7$numberOfSubjects[3, ], c(80, 150, 150), label = paste0(result7$numberOfSubjects[3, ]))
    expect_equal(result7$expectedNumberOfSubjectsH1, c(60.794667, 123.9302, 150), tolerance = 1e-07, label = paste0(result7$expectedNumberOfSubjectsH1))
    expect_equal(result7$informationOverStages[1, ], c(43.335293, 150.9303, 2711.671), tolerance = 1e-07, label = paste0(result7$informationOverStages[1, ]))
    expect_equal(result7$informationOverStages[2, ], c(77.190991, 268.8446, 4830.164), tolerance = 1e-07, label = paste0(result7$informationOverStages[2, ]))
    expect_equal(result7$informationOverStages[3, ], c(135.42279, 471.6572, 8473.9719), tolerance = 1e-07, label = paste0(result7$informationOverStages[3, ]))
    expect_equal(result7$expectedInformationH0, c(89.240417, 310.81094, 5584.1471), tolerance = 1e-07, label = paste0(result7$expectedInformationH0))
    expect_equal(result7$expectedInformationH01, c(113.87925, 396.62427, 7125.9019), tolerance = 1e-07, label = paste0(result7$expectedInformationH01))
    expect_equal(result7$expectedInformationH1, c(94.504724, 329.14573, 5913.5569), tolerance = 1e-07, label = paste0(result7$expectedInformationH1))
    expect_equal(result7$maxInformation, c(135.42279, 471.6572, 8473.9719), tolerance = 1e-07, label = paste0(result7$maxInformation))
    expect_equal(result7$futilityBoundsPValueScale[1, ], 0.65542174, tolerance = 1e-07, label = paste0(result7$futilityBoundsPValueScale[1, ]))
    expect_equal(result7$futilityBoundsPValueScale[2, ], 0.46017216, tolerance = 1e-07, label = paste0(result7$futilityBoundsPValueScale[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result7), NA)))
        expect_output(print(result7)$show())
        invisible(capture.output(expect_error(summary(result7), NA)))
        expect_output(summary(result7)$show())
        result7CodeBased <- eval(parse(text = getObjectRCode(result7, stringWrapParagraphWidth = NULL)))
        expect_equal(result7CodeBased$directionUpper, result7$directionUpper, tolerance = 1e-07)
        expect_equal(result7CodeBased$theta, result7$theta, tolerance = 1e-07)
        expect_equal(result7CodeBased$maxNumberOfSubjects, result7$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(result7CodeBased$maxNumberOfSubjects1, result7$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(result7CodeBased$maxNumberOfSubjects2, result7$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(result7CodeBased$rejectPerStage, result7$rejectPerStage, tolerance = 1e-07)
        expect_equal(result7CodeBased$futilityStop, result7$futilityStop, tolerance = 1e-07)
        expect_equal(result7CodeBased$futilityPerStage, result7$futilityPerStage, tolerance = 1e-07)
        expect_equal(result7CodeBased$earlyStop, result7$earlyStop, tolerance = 1e-07)
        expect_equal(result7CodeBased$calendarTime, result7$calendarTime, tolerance = 1e-07)
        expect_equal(result7CodeBased$expectedStudyDurationH1, result7$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(result7CodeBased$numberOfSubjects, result7$numberOfSubjects, tolerance = 1e-07)
        expect_equal(result7CodeBased$expectedNumberOfSubjectsH1, result7$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(result7CodeBased$informationOverStages, result7$informationOverStages, tolerance = 1e-07)
        expect_equal(result7CodeBased$expectedInformationH0, result7$expectedInformationH0, tolerance = 1e-07)
        expect_equal(result7CodeBased$expectedInformationH01, result7$expectedInformationH01, tolerance = 1e-07)
        expect_equal(result7CodeBased$expectedInformationH1, result7$expectedInformationH1, tolerance = 1e-07)
        expect_equal(result7CodeBased$maxInformation, result7$maxInformation, tolerance = 1e-07)
        expect_equal(result7CodeBased$futilityBoundsPValueScale, result7$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(result7), "character")
        df <- as.data.frame(result7)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result7)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizeCountsFixedExposureSingleStage}
    # @refFS[Formula]{fs:sampleSizePerStageFixedExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    # @refFS[Formula]{fs:sampleSizeCountsFixedExposureOptimumAllocationRatio}
    result8 <- getSampleSizeCounts(
        design = designGS1, lambda2 = 0.234, lambda1 = c(0.1, 0.2),
        allocationRatioPlanned = 0, accrualTime = 7, fixedExposureTime = 1
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result8' with expected results
    expect_equal(result8$allocationRatioPlanned, c(1.5297048, 1.0816655), tolerance = 1e-07, label = paste0(result8$allocationRatioPlanned))
    expect_equal(result8$directionUpper, c(FALSE, FALSE), label = paste0(result8$directionUpper))
    expect_equal(result8$theta, c(0.42735043, 0.85470085), tolerance = 1e-07, label = paste0(result8$theta))
    expect_equal(result8$maxNumberOfSubjects, c(425, 8421), label = paste0(result8$maxNumberOfSubjects))
    expect_equal(result8$maxNumberOfSubjects1, c(257, 4376), label = paste0(result8$maxNumberOfSubjects1))
    expect_equal(result8$maxNumberOfSubjects2, c(168, 4045), label = paste0(result8$maxNumberOfSubjects2))
    expect_equal(result8$rejectPerStage[1, ], 0.19514116, tolerance = 1e-07, label = paste0(result8$rejectPerStage[1, ]))
    expect_equal(result8$rejectPerStage[2, ], 0.37204423, tolerance = 1e-07, label = paste0(result8$rejectPerStage[2, ]))
    expect_equal(result8$rejectPerStage[3, ], 0.33281461, tolerance = 1e-07, label = paste0(result8$rejectPerStage[3, ]))
    expect_equal(result8$futilityStop, 0.015698246, tolerance = 1e-07, label = paste0(result8$futilityStop))
    expect_equal(result8$futilityPerStage[1, ], 0.01090105, tolerance = 1e-07, label = paste0(result8$futilityPerStage[1, ]))
    expect_equal(result8$futilityPerStage[2, ], 0.004797196, tolerance = 1e-07, label = paste0(result8$futilityPerStage[2, ]))
    expect_equal(result8$earlyStop, 0.58288364, tolerance = 1e-07, label = paste0(result8$earlyStop))
    expect_equal(result8$calendarTime[1, ], c(2.7292182, 2.7393892), tolerance = 1e-07, label = paste0(result8$calendarTime[1, ]))
    expect_equal(result8$calendarTime[2, ], c(4.4837277, 4.4895615), tolerance = 1e-07, label = paste0(result8$calendarTime[2, ]))
    expect_equal(result8$calendarTime[3, ], c(8, 8), label = paste0(result8$calendarTime[3, ]))
    expect_equal(result8$expectedStudyDurationH1, c(5.5889194, 5.5932135), tolerance = 1e-07, label = paste0(result8$expectedStudyDurationH1))
    expect_equal(result8$numberOfSubjects[1, ], c(166, 3296), label = paste0(result8$numberOfSubjects[1, ]))
    expect_equal(result8$numberOfSubjects[2, ], c(271, 5400), label = paste0(result8$numberOfSubjects[2, ]))
    expect_equal(result8$numberOfSubjects[3, ], c(425, 8421), label = paste0(result8$numberOfSubjects[3, ]))
    expect_equal(result8$expectedNumberOfSubjectsH1, c(313.60149, 6226.5957), tolerance = 1e-07, label = paste0(result8$expectedNumberOfSubjectsH1))
    expect_equal(result8$informationOverStages[1, ], c(4.9622115, 145.49474), tolerance = 1e-07, label = paste0(result8$informationOverStages[1, ]))
    expect_equal(result8$informationOverStages[2, ], c(8.8389391, 259.1625), tolerance = 1e-07, label = paste0(result8$informationOverStages[2, ]))
    expect_equal(result8$informationOverStages[3, ], c(15.506911, 454.67106), tolerance = 1e-07, label = paste0(result8$informationOverStages[3, ]))
    expect_equal(result8$expectedInformationH0, c(10.218688, 299.61748), tolerance = 1e-07, label = paste0(result8$expectedInformationH0))
    expect_equal(result8$expectedInformationH01, c(13.040016, 382.34035), tolerance = 1e-07, label = paste0(result8$expectedInformationH01))
    expect_equal(result8$expectedInformationH1, c(10.82149, 317.29196), tolerance = 1e-07, label = paste0(result8$expectedInformationH1))
    expect_equal(result8$maxInformation, c(15.506911, 454.67106), tolerance = 1e-07, label = paste0(result8$maxInformation))
    expect_equal(result8$futilityBoundsPValueScale[1, ], 0.65542174, tolerance = 1e-07, label = paste0(result8$futilityBoundsPValueScale[1, ]))
    expect_equal(result8$futilityBoundsPValueScale[2, ], 0.46017216, tolerance = 1e-07, label = paste0(result8$futilityBoundsPValueScale[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result8), NA)))
        expect_output(print(result8)$show())
        invisible(capture.output(expect_error(summary(result8), NA)))
        expect_output(summary(result8)$show())
        result8CodeBased <- eval(parse(text = getObjectRCode(result8, stringWrapParagraphWidth = NULL)))
        expect_equal(result8CodeBased$allocationRatioPlanned, result8$allocationRatioPlanned, tolerance = 1e-07)
        expect_equal(result8CodeBased$directionUpper, result8$directionUpper, tolerance = 1e-07)
        expect_equal(result8CodeBased$theta, result8$theta, tolerance = 1e-07)
        expect_equal(result8CodeBased$maxNumberOfSubjects, result8$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(result8CodeBased$maxNumberOfSubjects1, result8$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(result8CodeBased$maxNumberOfSubjects2, result8$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(result8CodeBased$rejectPerStage, result8$rejectPerStage, tolerance = 1e-07)
        expect_equal(result8CodeBased$futilityStop, result8$futilityStop, tolerance = 1e-07)
        expect_equal(result8CodeBased$futilityPerStage, result8$futilityPerStage, tolerance = 1e-07)
        expect_equal(result8CodeBased$earlyStop, result8$earlyStop, tolerance = 1e-07)
        expect_equal(result8CodeBased$calendarTime, result8$calendarTime, tolerance = 1e-07)
        expect_equal(result8CodeBased$expectedStudyDurationH1, result8$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(result8CodeBased$numberOfSubjects, result8$numberOfSubjects, tolerance = 1e-07)
        expect_equal(result8CodeBased$expectedNumberOfSubjectsH1, result8$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(result8CodeBased$informationOverStages, result8$informationOverStages, tolerance = 1e-07)
        expect_equal(result8CodeBased$expectedInformationH0, result8$expectedInformationH0, tolerance = 1e-07)
        expect_equal(result8CodeBased$expectedInformationH01, result8$expectedInformationH01, tolerance = 1e-07)
        expect_equal(result8CodeBased$expectedInformationH1, result8$expectedInformationH1, tolerance = 1e-07)
        expect_equal(result8CodeBased$maxInformation, result8$maxInformation, tolerance = 1e-07)
        expect_equal(result8CodeBased$futilityBoundsPValueScale, result8$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(result8), "character")
        df <- as.data.frame(result8)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result8)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizePerStageVariableExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    # @refFS[Formula]{fs:sampleSizeCountsVariableExposureOptimumAllocationRatio}
    result9 <- getSampleSizeCounts(
        design = designGS1, lambda2 = 0.234, lambda1 = c(0.1, 0.2),
        allocationRatioPlanned = 0, accrualTime = 7, followUpTime = 1
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result9' with expected results
    expect_equal(result9$allocationRatioPlanned, c(1.1591008, 1.1237283), tolerance = 1e-07, label = paste0(result9$allocationRatioPlanned))
    expect_equal(result9$directionUpper, c(FALSE, FALSE), label = paste0(result9$directionUpper))
    expect_equal(result9$theta, c(0.42735043, 0.85470085), tolerance = 1e-07, label = paste0(result9$theta))
    expect_equal(result9$maxNumberOfSubjects, c(96, 1872), label = paste0(result9$maxNumberOfSubjects))
    expect_equal(result9$maxNumberOfSubjects1, c(52, 991), label = paste0(result9$maxNumberOfSubjects1))
    expect_equal(result9$maxNumberOfSubjects2, c(44, 881), label = paste0(result9$maxNumberOfSubjects2))
    expect_equal(result9$rejectPerStage[1, ], 0.19514116, tolerance = 1e-07, label = paste0(result9$rejectPerStage[1, ]))
    expect_equal(result9$rejectPerStage[2, ], 0.37204423, tolerance = 1e-07, label = paste0(result9$rejectPerStage[2, ]))
    expect_equal(result9$rejectPerStage[3, ], 0.33281461, tolerance = 1e-07, label = paste0(result9$rejectPerStage[3, ]))
    expect_equal(result9$futilityStop, 0.015698246, tolerance = 1e-07, label = paste0(result9$futilityStop))
    expect_equal(result9$futilityPerStage[1, ], 0.01090105, tolerance = 1e-07, label = paste0(result9$futilityPerStage[1, ]))
    expect_equal(result9$futilityPerStage[2, ], 0.004797196, tolerance = 1e-07, label = paste0(result9$futilityPerStage[2, ]))
    expect_equal(result9$earlyStop, 0.58288364, tolerance = 1e-07, label = paste0(result9$earlyStop))
    expect_equal(result9$calendarTime[1, ], c(4.457672, 4.4883905), tolerance = 1e-07, label = paste0(result9$calendarTime[1, ]))
    expect_equal(result9$calendarTime[2, ], c(5.9737692, 5.9916173), tolerance = 1e-07, label = paste0(result9$calendarTime[2, ]))
    expect_equal(result9$calendarTime[3, ], c(8, 8), label = paste0(result9$calendarTime[3, ]))
    expect_equal(result9$expectedStudyDurationH1, c(6.5065632, 6.5196184), tolerance = 1e-07, label = paste0(result9$expectedStudyDurationH1))
    expect_equal(result9$numberOfSubjects[1, ], c(61, 1200), label = paste0(result9$numberOfSubjects[1, ]))
    expect_equal(result9$numberOfSubjects[2, ], c(81, 1602), label = paste0(result9$numberOfSubjects[2, ]))
    expect_equal(result9$numberOfSubjects[3, ], c(96, 1872), label = paste0(result9$numberOfSubjects[3, ]))
    expect_equal(result9$expectedNumberOfSubjectsH1, c(83.135901, 1631.7924), tolerance = 1e-07, label = paste0(result9$expectedNumberOfSubjectsH1))
    expect_equal(result9$informationOverStages[1, ], c(4.9622115, 145.49474), tolerance = 1e-07, label = paste0(result9$informationOverStages[1, ]))
    expect_equal(result9$informationOverStages[2, ], c(8.8389391, 259.1625), tolerance = 1e-07, label = paste0(result9$informationOverStages[2, ]))
    expect_equal(result9$informationOverStages[3, ], c(15.506911, 454.67106), tolerance = 1e-07, label = paste0(result9$informationOverStages[3, ]))
    expect_equal(result9$expectedInformationH0, c(10.218688, 299.61748), tolerance = 1e-07, label = paste0(result9$expectedInformationH0))
    expect_equal(result9$expectedInformationH01, c(13.040016, 382.34035), tolerance = 1e-07, label = paste0(result9$expectedInformationH01))
    expect_equal(result9$expectedInformationH1, c(10.82149, 317.29196), tolerance = 1e-07, label = paste0(result9$expectedInformationH1))
    expect_equal(result9$maxInformation, c(15.506911, 454.67106), tolerance = 1e-07, label = paste0(result9$maxInformation))
    expect_equal(result9$futilityBoundsPValueScale[1, ], 0.65542174, tolerance = 1e-07, label = paste0(result9$futilityBoundsPValueScale[1, ]))
    expect_equal(result9$futilityBoundsPValueScale[2, ], 0.46017216, tolerance = 1e-07, label = paste0(result9$futilityBoundsPValueScale[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result9), NA)))
        expect_output(print(result9)$show())
        invisible(capture.output(expect_error(summary(result9), NA)))
        expect_output(summary(result9)$show())
        result9CodeBased <- eval(parse(text = getObjectRCode(result9, stringWrapParagraphWidth = NULL)))
        expect_equal(result9CodeBased$allocationRatioPlanned, result9$allocationRatioPlanned, tolerance = 1e-07)
        expect_equal(result9CodeBased$directionUpper, result9$directionUpper, tolerance = 1e-07)
        expect_equal(result9CodeBased$theta, result9$theta, tolerance = 1e-07)
        expect_equal(result9CodeBased$maxNumberOfSubjects, result9$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(result9CodeBased$maxNumberOfSubjects1, result9$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(result9CodeBased$maxNumberOfSubjects2, result9$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(result9CodeBased$rejectPerStage, result9$rejectPerStage, tolerance = 1e-07)
        expect_equal(result9CodeBased$futilityStop, result9$futilityStop, tolerance = 1e-07)
        expect_equal(result9CodeBased$futilityPerStage, result9$futilityPerStage, tolerance = 1e-07)
        expect_equal(result9CodeBased$earlyStop, result9$earlyStop, tolerance = 1e-07)
        expect_equal(result9CodeBased$calendarTime, result9$calendarTime, tolerance = 1e-07)
        expect_equal(result9CodeBased$expectedStudyDurationH1, result9$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(result9CodeBased$numberOfSubjects, result9$numberOfSubjects, tolerance = 1e-07)
        expect_equal(result9CodeBased$expectedNumberOfSubjectsH1, result9$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(result9CodeBased$informationOverStages, result9$informationOverStages, tolerance = 1e-07)
        expect_equal(result9CodeBased$expectedInformationH0, result9$expectedInformationH0, tolerance = 1e-07)
        expect_equal(result9CodeBased$expectedInformationH01, result9$expectedInformationH01, tolerance = 1e-07)
        expect_equal(result9CodeBased$expectedInformationH1, result9$expectedInformationH1, tolerance = 1e-07)
        expect_equal(result9CodeBased$maxInformation, result9$maxInformation, tolerance = 1e-07)
        expect_equal(result9CodeBased$futilityBoundsPValueScale, result9$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(result9), "character")
        df <- as.data.frame(result9)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result9)
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
    result1 <- getSampleSizeCounts(
        design = designGS2, lambda = 0.234, theta = 0.7,
        overdispersion = 0.71, accrualTime = 7, fixedExposureTime = 1
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result1' with expected results
    expect_equal(result1$directionUpper, FALSE, label = paste0(result1$directionUpper))
    expect_equal(result1$lambda1, 0.19270588, tolerance = 1e-07, label = paste0(result1$lambda1))
    expect_equal(result1$lambda2, 0.27529412, tolerance = 1e-07, label = paste0(result1$lambda2))
    expect_equal(result1$maxNumberOfSubjects, 1554, label = paste0(result1$maxNumberOfSubjects))
    expect_equal(result1$maxNumberOfSubjects1, 777, label = paste0(result1$maxNumberOfSubjects1))
    expect_equal(result1$maxNumberOfSubjects2, 777, label = paste0(result1$maxNumberOfSubjects2))
    expect_equal(result1$rejectPerStage[1, ], 0.033479189, tolerance = 1e-07, label = paste0(result1$rejectPerStage[1, ]))
    expect_equal(result1$rejectPerStage[2, ], 0.27519472, tolerance = 1e-07, label = paste0(result1$rejectPerStage[2, ]))
    expect_equal(result1$rejectPerStage[3, ], 0.49132609, tolerance = 1e-07, label = paste0(result1$rejectPerStage[3, ]))
    expect_equal(result1$earlyStop, 0.30867391, tolerance = 1e-07, label = paste0(result1$earlyStop))
    expect_equal(result1$calendarTime[1, ], 2.7108183, tolerance = 1e-07, label = paste0(result1$calendarTime[1, ]))
    expect_equal(result1$calendarTime[2, ], 4.4609941, tolerance = 1e-07, label = paste0(result1$calendarTime[2, ]))
    expect_equal(result1$calendarTime[3, ], 8, label = paste0(result1$calendarTime[3, ]))
    expect_equal(result1$expectedStudyDurationH1, 6.8490067, tolerance = 1e-07, label = paste0(result1$expectedStudyDurationH1))
    expect_equal(result1$numberOfSubjects[1, ], 602, label = paste0(result1$numberOfSubjects[1, ]))
    expect_equal(result1$numberOfSubjects[2, ], 990, label = paste0(result1$numberOfSubjects[2, ]))
    expect_equal(result1$numberOfSubjects[3, ], 1554, label = paste0(result1$numberOfSubjects[3, ]))
    expect_equal(result1$expectedNumberOfSubjectsH1, 1366.918, tolerance = 1e-07, label = paste0(result1$expectedNumberOfSubjectsH1))
    expect_equal(result1$informationOverStages[1, ], 24.248379, tolerance = 1e-07, label = paste0(result1$informationOverStages[1, ]))
    expect_equal(result1$informationOverStages[2, ], 43.192425, tolerance = 1e-07, label = paste0(result1$informationOverStages[2, ]))
    expect_equal(result1$informationOverStages[3, ], 75.776183, tolerance = 1e-07, label = paste0(result1$informationOverStages[3, ]))
    expect_equal(result1$expectedInformationH0, 75.621421, tolerance = 1e-07, label = paste0(result1$expectedInformationH0))
    expect_equal(result1$expectedInformationH01, 74.163254, tolerance = 1e-07, label = paste0(result1$expectedInformationH01))
    expect_equal(result1$expectedInformationH1, 65.08419, tolerance = 1e-07, label = paste0(result1$expectedInformationH1))
    expect_equal(result1$maxInformation, 75.776183, tolerance = 1e-07, label = paste0(result1$maxInformation))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result1), NA)))
        expect_output(print(result1)$show())
        invisible(capture.output(expect_error(summary(result1), NA)))
        expect_output(summary(result1)$show())
        result1CodeBased <- eval(parse(text = getObjectRCode(result1, stringWrapParagraphWidth = NULL)))
        expect_equal(result1CodeBased$directionUpper, result1$directionUpper, tolerance = 1e-07)
        expect_equal(result1CodeBased$lambda1, result1$lambda1, tolerance = 1e-07)
        expect_equal(result1CodeBased$lambda2, result1$lambda2, tolerance = 1e-07)
        expect_equal(result1CodeBased$maxNumberOfSubjects, result1$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(result1CodeBased$maxNumberOfSubjects1, result1$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(result1CodeBased$maxNumberOfSubjects2, result1$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(result1CodeBased$rejectPerStage, result1$rejectPerStage, tolerance = 1e-07)
        expect_equal(result1CodeBased$earlyStop, result1$earlyStop, tolerance = 1e-07)
        expect_equal(result1CodeBased$calendarTime, result1$calendarTime, tolerance = 1e-07)
        expect_equal(result1CodeBased$expectedStudyDurationH1, result1$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(result1CodeBased$numberOfSubjects, result1$numberOfSubjects, tolerance = 1e-07)
        expect_equal(result1CodeBased$expectedNumberOfSubjectsH1, result1$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(result1CodeBased$informationOverStages, result1$informationOverStages, tolerance = 1e-07)
        expect_equal(result1CodeBased$expectedInformationH0, result1$expectedInformationH0, tolerance = 1e-07)
        expect_equal(result1CodeBased$expectedInformationH01, result1$expectedInformationH01, tolerance = 1e-07)
        expect_equal(result1CodeBased$expectedInformationH1, result1$expectedInformationH1, tolerance = 1e-07)
        expect_equal(result1CodeBased$maxInformation, result1$maxInformation, tolerance = 1e-07)
        expect_type(names(result1), "character")
        df <- as.data.frame(result1)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result1)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizePerStageVariableExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result2 <- getSampleSizeCounts(
        design = designGS2, lambda = 0.234, theta = 0.7,
        overdispersion = 0.71, accrualTime = 7, followUpTime = 1
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result2' with expected results
    expect_equal(result2$directionUpper, FALSE, label = paste0(result2$directionUpper))
    expect_equal(result2$lambda1, 0.19270588, tolerance = 1e-07, label = paste0(result2$lambda1))
    expect_equal(result2$lambda2, 0.27529412, tolerance = 1e-07, label = paste0(result2$lambda2))
    expect_equal(result2$maxNumberOfSubjects, 542, label = paste0(result2$maxNumberOfSubjects))
    expect_equal(result2$maxNumberOfSubjects1, 271, label = paste0(result2$maxNumberOfSubjects1))
    expect_equal(result2$maxNumberOfSubjects2, 271, label = paste0(result2$maxNumberOfSubjects2))
    expect_equal(result2$rejectPerStage[1, ], 0.033479189, tolerance = 1e-07, label = paste0(result2$rejectPerStage[1, ]))
    expect_equal(result2$rejectPerStage[2, ], 0.27519472, tolerance = 1e-07, label = paste0(result2$rejectPerStage[2, ]))
    expect_equal(result2$rejectPerStage[3, ], 0.49132609, tolerance = 1e-07, label = paste0(result2$rejectPerStage[3, ]))
    expect_equal(result2$earlyStop, 0.30867391, tolerance = 1e-07, label = paste0(result2$earlyStop))
    expect_equal(result2$calendarTime[1, ], 3.9368401, tolerance = 1e-07, label = paste0(result2$calendarTime[1, ]))
    expect_equal(result2$calendarTime[2, ], 5.5484451, tolerance = 1e-07, label = paste0(result2$calendarTime[2, ]))
    expect_equal(result2$calendarTime[3, ], 8, label = paste0(result2$calendarTime[3, ]))
    expect_equal(result2$expectedStudyDurationH1, 7.1893137, tolerance = 1e-07, label = paste0(result2$expectedStudyDurationH1))
    expect_equal(result2$numberOfSubjects[1, ], 304, label = paste0(result2$numberOfSubjects[1, ]))
    expect_equal(result2$numberOfSubjects[2, ], 430, label = paste0(result2$numberOfSubjects[2, ]))
    expect_equal(result2$numberOfSubjects[3, ], 542, label = paste0(result2$numberOfSubjects[3, ]))
    expect_equal(result2$expectedNumberOfSubjectsH1, 503.21014, tolerance = 1e-07, label = paste0(result2$expectedNumberOfSubjectsH1))
    expect_equal(result2$informationOverStages[1, ], 24.248379, tolerance = 1e-07, label = paste0(result2$informationOverStages[1, ]))
    expect_equal(result2$informationOverStages[2, ], 43.192425, tolerance = 1e-07, label = paste0(result2$informationOverStages[2, ]))
    expect_equal(result2$informationOverStages[3, ], 75.776183, tolerance = 1e-07, label = paste0(result2$informationOverStages[3, ]))
    expect_equal(result2$expectedInformationH0, 75.621421, tolerance = 1e-07, label = paste0(result2$expectedInformationH0))
    expect_equal(result2$expectedInformationH01, 74.163254, tolerance = 1e-07, label = paste0(result2$expectedInformationH01))
    expect_equal(result2$expectedInformationH1, 65.08419, tolerance = 1e-07, label = paste0(result2$expectedInformationH1))
    expect_equal(result2$maxInformation, 75.776183, tolerance = 1e-07, label = paste0(result2$maxInformation))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result2), NA)))
        expect_output(print(result2)$show())
        invisible(capture.output(expect_error(summary(result2), NA)))
        expect_output(summary(result2)$show())
        result2CodeBased <- eval(parse(text = getObjectRCode(result2, stringWrapParagraphWidth = NULL)))
        expect_equal(result2CodeBased$directionUpper, result2$directionUpper, tolerance = 1e-07)
        expect_equal(result2CodeBased$lambda1, result2$lambda1, tolerance = 1e-07)
        expect_equal(result2CodeBased$lambda2, result2$lambda2, tolerance = 1e-07)
        expect_equal(result2CodeBased$maxNumberOfSubjects, result2$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(result2CodeBased$maxNumberOfSubjects1, result2$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(result2CodeBased$maxNumberOfSubjects2, result2$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(result2CodeBased$rejectPerStage, result2$rejectPerStage, tolerance = 1e-07)
        expect_equal(result2CodeBased$earlyStop, result2$earlyStop, tolerance = 1e-07)
        expect_equal(result2CodeBased$calendarTime, result2$calendarTime, tolerance = 1e-07)
        expect_equal(result2CodeBased$expectedStudyDurationH1, result2$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(result2CodeBased$numberOfSubjects, result2$numberOfSubjects, tolerance = 1e-07)
        expect_equal(result2CodeBased$expectedNumberOfSubjectsH1, result2$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(result2CodeBased$informationOverStages, result2$informationOverStages, tolerance = 1e-07)
        expect_equal(result2CodeBased$expectedInformationH0, result2$expectedInformationH0, tolerance = 1e-07)
        expect_equal(result2CodeBased$expectedInformationH01, result2$expectedInformationH01, tolerance = 1e-07)
        expect_equal(result2CodeBased$expectedInformationH1, result2$expectedInformationH1, tolerance = 1e-07)
        expect_equal(result2CodeBased$maxInformation, result2$maxInformation, tolerance = 1e-07)
        expect_type(names(result2), "character")
        df <- as.data.frame(result2)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result2)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizeCountsFixedExposureSingleStage}
    # @refFS[Formula]{fs:sampleSizePerStageFixedExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result3 <- getSampleSizeCounts(
        design = designGS2, lambda2 = 0.02, theta = c(0.7, 0.8),
        overdispersion = 0.71, accrualTime = 7, fixedExposureTime = 2
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result3' with expected results
    expect_equal(result3$directionUpper, c(FALSE, FALSE), label = paste0(result3$directionUpper))
    expect_equal(result3$lambda1, c(0.014, 0.016), tolerance = 1e-07, label = paste0(result3$lambda1))
    expect_equal(result3$maxNumberOfSubjects, c(9418, 22332), label = paste0(result3$maxNumberOfSubjects))
    expect_equal(result3$maxNumberOfSubjects1, c(4709, 11166), label = paste0(result3$maxNumberOfSubjects1))
    expect_equal(result3$maxNumberOfSubjects2, c(4709, 11166), label = paste0(result3$maxNumberOfSubjects2))
    expect_equal(result3$rejectPerStage[1, ], 0.033479189, tolerance = 1e-07, label = paste0(result3$rejectPerStage[1, ]))
    expect_equal(result3$rejectPerStage[2, ], 0.27519472, tolerance = 1e-07, label = paste0(result3$rejectPerStage[2, ]))
    expect_equal(result3$rejectPerStage[3, ], 0.49132609, tolerance = 1e-07, label = paste0(result3$rejectPerStage[3, ]))
    expect_equal(result3$earlyStop, 0.30867391, tolerance = 1e-07, label = paste0(result3$earlyStop))
    expect_equal(result3$calendarTime[1, ], c(3.2316907, 3.2313779), tolerance = 1e-07, label = paste0(result3$calendarTime[1, ]))
    expect_equal(result3$calendarTime[2, ], c(4.9818015, 4.9813797), tolerance = 1e-07, label = paste0(result3$calendarTime[2, ]))
    expect_equal(result3$calendarTime[3, ], c(9, 9), label = paste0(result3$calendarTime[3, ]))
    expect_equal(result3$expectedStudyDurationH1, c(7.7010947, 7.7009681), tolerance = 1e-07, label = paste0(result3$expectedStudyDurationH1))
    expect_equal(result3$numberOfSubjects[1, ], c(4348, 10310), label = paste0(result3$numberOfSubjects[1, ]))
    expect_equal(result3$numberOfSubjects[2, ], c(6702, 15892), label = paste0(result3$numberOfSubjects[2, ]))
    expect_equal(result3$numberOfSubjects[3, ], c(9418, 22332), label = paste0(result3$numberOfSubjects[3, ]))
    expect_equal(result3$expectedNumberOfSubjectsH1, c(8500.8316, 20157.259), tolerance = 1e-07, label = paste0(result3$expectedNumberOfSubjectsH1))
    expect_equal(result3$informationOverStages[1, ], c(24.248379, 61.952556), tolerance = 1e-07, label = paste0(result3$informationOverStages[1, ]))
    expect_equal(result3$informationOverStages[2, ], c(43.192425, 110.35299), tolerance = 1e-07, label = paste0(result3$informationOverStages[2, ]))
    expect_equal(result3$informationOverStages[3, ], c(75.776183, 193.60174), tolerance = 1e-07, label = paste0(result3$informationOverStages[3, ]))
    expect_equal(result3$expectedInformationH0, c(75.621421, 193.20633), tolerance = 1e-07, label = paste0(result3$expectedInformationH0))
    expect_equal(result3$expectedInformationH01, c(74.163254, 189.48084), tolerance = 1e-07, label = paste0(result3$expectedInformationH01))
    expect_equal(result3$expectedInformationH1, c(65.08419, 166.2846), tolerance = 1e-07, label = paste0(result3$expectedInformationH1))
    expect_equal(result3$maxInformation, c(75.776183, 193.60174), tolerance = 1e-07, label = paste0(result3$maxInformation))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result3), NA)))
        expect_output(print(result3)$show())
        invisible(capture.output(expect_error(summary(result3), NA)))
        expect_output(summary(result3)$show())
        result3CodeBased <- eval(parse(text = getObjectRCode(result3, stringWrapParagraphWidth = NULL)))
        expect_equal(result3CodeBased$directionUpper, result3$directionUpper, tolerance = 1e-07)
        expect_equal(result3CodeBased$lambda1, result3$lambda1, tolerance = 1e-07)
        expect_equal(result3CodeBased$maxNumberOfSubjects, result3$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(result3CodeBased$maxNumberOfSubjects1, result3$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(result3CodeBased$maxNumberOfSubjects2, result3$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(result3CodeBased$rejectPerStage, result3$rejectPerStage, tolerance = 1e-07)
        expect_equal(result3CodeBased$earlyStop, result3$earlyStop, tolerance = 1e-07)
        expect_equal(result3CodeBased$calendarTime, result3$calendarTime, tolerance = 1e-07)
        expect_equal(result3CodeBased$expectedStudyDurationH1, result3$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(result3CodeBased$numberOfSubjects, result3$numberOfSubjects, tolerance = 1e-07)
        expect_equal(result3CodeBased$expectedNumberOfSubjectsH1, result3$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(result3CodeBased$informationOverStages, result3$informationOverStages, tolerance = 1e-07)
        expect_equal(result3CodeBased$expectedInformationH0, result3$expectedInformationH0, tolerance = 1e-07)
        expect_equal(result3CodeBased$expectedInformationH01, result3$expectedInformationH01, tolerance = 1e-07)
        expect_equal(result3CodeBased$expectedInformationH1, result3$expectedInformationH1, tolerance = 1e-07)
        expect_equal(result3CodeBased$maxInformation, result3$maxInformation, tolerance = 1e-07)
        expect_type(names(result3), "character")
        df <- as.data.frame(result3)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result3)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizePerStageVariableExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result4 <- getSampleSizeCounts(
        design = designGS2, lambda1 = seq(1.05, 1.35, 0.15),
        lambda2 = 1.4, accrualTime = c(0, 20), allocationRatioPlanned = 4, maxNumberOfSubjects = 100
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result4' with expected results
    expect_equal(result4$directionUpper, c(FALSE, FALSE, FALSE), label = paste0(result4$directionUpper))
    expect_equal(result4$theta, c(0.75, 0.85714286, 0.96428571), tolerance = 1e-07, label = paste0(result4$theta))
    expect_equal(result4$rejectPerStage[1, ], 0.033479189, tolerance = 1e-07, label = paste0(result4$rejectPerStage[1, ]))
    expect_equal(result4$rejectPerStage[2, ], 0.27519472, tolerance = 1e-07, label = paste0(result4$rejectPerStage[2, ]))
    expect_equal(result4$rejectPerStage[3, ], 0.49132609, tolerance = 1e-07, label = paste0(result4$rejectPerStage[3, ]))
    expect_equal(result4$earlyStop, 0.30867391, tolerance = 1e-07, label = paste0(result4$earlyStop))
    expect_equal(result4$calendarTime[1, ], c(8.1833157, 15.375371, 114.89498), tolerance = 1e-07, label = paste0(result4$calendarTime[1, ]))
    expect_equal(result4$calendarTime[2, ], c(11.057576, 20.667295, 196.84418), tolerance = 1e-07, label = paste0(result4$calendarTime[2, ]))
    expect_equal(result4$calendarTime[3, ], c(14.788602, 28.714553, 337.7968), tolerance = 1e-07, label = paste0(result4$calendarTime[3, ]))
    expect_equal(result4$expectedStudyDurationH1, c(13.540704, 26.053405, 291.54481), tolerance = 1e-07, label = paste0(result4$expectedStudyDurationH1))
    expect_equal(result4$numberOfSubjects[1, ], c(41, 76, 100), label = paste0(result4$numberOfSubjects[1, ]))
    expect_equal(result4$numberOfSubjects[2, ], c(55, 100, 100), label = paste0(result4$numberOfSubjects[2, ]))
    expect_equal(result4$numberOfSubjects[3, ], c(74, 100, 100), label = paste0(result4$numberOfSubjects[3, ]))
    expect_equal(result4$expectedNumberOfSubjectsH1, c(67.666487, 99.196499, 100), tolerance = 1e-07, label = paste0(result4$expectedNumberOfSubjectsH1))
    expect_equal(result4$informationOverStages[1, ], c(37.273683, 129.81863, 2332.3707), tolerance = 1e-07, label = paste0(result4$informationOverStages[1, ]))
    expect_equal(result4$informationOverStages[2, ], c(66.393749, 231.23943, 4154.5353), tolerance = 1e-07, label = paste0(result4$informationOverStages[2, ]))
    expect_equal(result4$informationOverStages[3, ], c(116.48026, 405.68322, 7288.6583), tolerance = 1e-07, label = paste0(result4$informationOverStages[3, ]))
    expect_equal(result4$expectedInformationH0, c(116.24237, 404.85466, 7273.7722), tolerance = 1e-07, label = paste0(result4$expectedInformationH0))
    expect_equal(result4$expectedInformationH01, c(114.00093, 397.04807, 7133.516), tolerance = 1e-07, label = paste0(result4$expectedInformationH01))
    expect_equal(result4$expectedInformationH1, c(100.04494, 348.44146, 6260.2312), tolerance = 1e-07, label = paste0(result4$expectedInformationH1))
    expect_equal(result4$maxInformation, c(116.48026, 405.68322, 7288.6583), tolerance = 1e-07, label = paste0(result4$maxInformation))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result4), NA)))
        expect_output(print(result4)$show())
        invisible(capture.output(expect_error(summary(result4), NA)))
        expect_output(summary(result4)$show())
        result4CodeBased <- eval(parse(text = getObjectRCode(result4, stringWrapParagraphWidth = NULL)))
        expect_equal(result4CodeBased$directionUpper, result4$directionUpper, tolerance = 1e-07)
        expect_equal(result4CodeBased$theta, result4$theta, tolerance = 1e-07)
        expect_equal(result4CodeBased$rejectPerStage, result4$rejectPerStage, tolerance = 1e-07)
        expect_equal(result4CodeBased$earlyStop, result4$earlyStop, tolerance = 1e-07)
        expect_equal(result4CodeBased$calendarTime, result4$calendarTime, tolerance = 1e-07)
        expect_equal(result4CodeBased$expectedStudyDurationH1, result4$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(result4CodeBased$numberOfSubjects, result4$numberOfSubjects, tolerance = 1e-07)
        expect_equal(result4CodeBased$expectedNumberOfSubjectsH1, result4$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(result4CodeBased$informationOverStages, result4$informationOverStages, tolerance = 1e-07)
        expect_equal(result4CodeBased$expectedInformationH0, result4$expectedInformationH0, tolerance = 1e-07)
        expect_equal(result4CodeBased$expectedInformationH01, result4$expectedInformationH01, tolerance = 1e-07)
        expect_equal(result4CodeBased$expectedInformationH1, result4$expectedInformationH1, tolerance = 1e-07)
        expect_equal(result4CodeBased$maxInformation, result4$maxInformation, tolerance = 1e-07)
        expect_type(names(result4), "character")
        df <- as.data.frame(result4)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result4)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizePerStageVariableExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result5 <- getSampleSizeCounts(
        design = designGS2, lambda1 = seq(1.05, 1.35, 0.15),
        lambda2 = 1.4, accrualTime = c(0, 10, 20), accrualIntensity = c(5, 10)
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result5' with expected results
    expect_equal(result5$directionUpper, c(FALSE, FALSE, FALSE), label = paste0(result5$directionUpper))
    expect_equal(result5$theta, c(0.75, 0.85714286, 0.96428571), tolerance = 1e-07, label = paste0(result5$theta))
    expect_equal(result5$maxNumberOfSubjects, c(150, 150, 150), label = paste0(result5$maxNumberOfSubjects))
    expect_equal(result5$maxNumberOfSubjects1, c(75, 75, 75), label = paste0(result5$maxNumberOfSubjects1))
    expect_equal(result5$maxNumberOfSubjects2, c(75, 75, 75), label = paste0(result5$maxNumberOfSubjects2))
    expect_equal(result5$rejectPerStage[1, ], 0.033479189, tolerance = 1e-07, label = paste0(result5$rejectPerStage[1, ]))
    expect_equal(result5$rejectPerStage[2, ], 0.27519472, tolerance = 1e-07, label = paste0(result5$rejectPerStage[2, ]))
    expect_equal(result5$rejectPerStage[3, ], 0.49132609, tolerance = 1e-07, label = paste0(result5$rejectPerStage[3, ]))
    expect_equal(result5$earlyStop, 0.30867391, tolerance = 1e-07, label = paste0(result5$earlyStop))
    expect_equal(result5$calendarTime[1, ], c(6.987616, 12.444445, 56.948814), tolerance = 1e-07, label = paste0(result5$calendarTime[1, ]))
    expect_equal(result5$calendarTime[2, ], c(9.3944745, 15.885169, 92.299449), tolerance = 1e-07, label = paste0(result5$calendarTime[2, ]))
    expect_equal(result5$calendarTime[3, ], c(12.259611, 20.071241, 153.10254), tolerance = 1e-07, label = paste0(result5$calendarTime[3, ]))
    expect_equal(result5$expectedStudyDurationH1, c(11.294639, 18.663917, 133.1507), tolerance = 1e-07, label = paste0(result5$expectedStudyDurationH1))
    expect_equal(result5$numberOfSubjects[1, ], c(34, 74, 150), label = paste0(result5$numberOfSubjects[1, ]))
    expect_equal(result5$numberOfSubjects[2, ], c(46, 108, 150), label = paste0(result5$numberOfSubjects[2, ]))
    expect_equal(result5$numberOfSubjects[3, ], c(72, 150, 150), label = paste0(result5$numberOfSubjects[3, ]))
    expect_equal(result5$expectedNumberOfSubjectsH1, c(63.572728, 135.8974, 150), tolerance = 1e-07, label = paste0(result5$expectedNumberOfSubjectsH1))
    expect_equal(result5$informationOverStages[1, ], c(37.273683, 129.81863, 2332.3707), tolerance = 1e-07, label = paste0(result5$informationOverStages[1, ]))
    expect_equal(result5$informationOverStages[2, ], c(66.393749, 231.23943, 4154.5353), tolerance = 1e-07, label = paste0(result5$informationOverStages[2, ]))
    expect_equal(result5$informationOverStages[3, ], c(116.48026, 405.68322, 7288.6583), tolerance = 1e-07, label = paste0(result5$informationOverStages[3, ]))
    expect_equal(result5$expectedInformationH0, c(116.24237, 404.85466, 7273.7722), tolerance = 1e-07, label = paste0(result5$expectedInformationH0))
    expect_equal(result5$expectedInformationH01, c(114.00093, 397.04807, 7133.516), tolerance = 1e-07, label = paste0(result5$expectedInformationH01))
    expect_equal(result5$expectedInformationH1, c(100.04494, 348.44146, 6260.2312), tolerance = 1e-07, label = paste0(result5$expectedInformationH1))
    expect_equal(result5$maxInformation, c(116.48026, 405.68322, 7288.6583), tolerance = 1e-07, label = paste0(result5$maxInformation))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result5), NA)))
        expect_output(print(result5)$show())
        invisible(capture.output(expect_error(summary(result5), NA)))
        expect_output(summary(result5)$show())
        result5CodeBased <- eval(parse(text = getObjectRCode(result5, stringWrapParagraphWidth = NULL)))
        expect_equal(result5CodeBased$directionUpper, result5$directionUpper, tolerance = 1e-07)
        expect_equal(result5CodeBased$theta, result5$theta, tolerance = 1e-07)
        expect_equal(result5CodeBased$maxNumberOfSubjects, result5$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(result5CodeBased$maxNumberOfSubjects1, result5$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(result5CodeBased$maxNumberOfSubjects2, result5$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(result5CodeBased$rejectPerStage, result5$rejectPerStage, tolerance = 1e-07)
        expect_equal(result5CodeBased$earlyStop, result5$earlyStop, tolerance = 1e-07)
        expect_equal(result5CodeBased$calendarTime, result5$calendarTime, tolerance = 1e-07)
        expect_equal(result5CodeBased$expectedStudyDurationH1, result5$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(result5CodeBased$numberOfSubjects, result5$numberOfSubjects, tolerance = 1e-07)
        expect_equal(result5CodeBased$expectedNumberOfSubjectsH1, result5$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(result5CodeBased$informationOverStages, result5$informationOverStages, tolerance = 1e-07)
        expect_equal(result5CodeBased$expectedInformationH0, result5$expectedInformationH0, tolerance = 1e-07)
        expect_equal(result5CodeBased$expectedInformationH01, result5$expectedInformationH01, tolerance = 1e-07)
        expect_equal(result5CodeBased$expectedInformationH1, result5$expectedInformationH1, tolerance = 1e-07)
        expect_equal(result5CodeBased$maxInformation, result5$maxInformation, tolerance = 1e-07)
        expect_type(names(result5), "character")
        df <- as.data.frame(result5)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result5)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizePerStageVariableExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    result6 <- getSampleSizeCounts(
        design = designGS2, lambda1 = seq(1.05, 1.35, 0.15),
        overdispersion = 0, lambda2 = 1.4, accrualTime = c(0, 10, 20), accrualIntensity = c(5, 10)
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result6' with expected results
    expect_equal(result6$directionUpper, c(FALSE, FALSE, FALSE), label = paste0(result6$directionUpper))
    expect_equal(result6$theta, c(0.75, 0.85714286, 0.96428571), tolerance = 1e-07, label = paste0(result6$theta))
    expect_equal(result6$maxNumberOfSubjects, c(150, 150, 150), label = paste0(result6$maxNumberOfSubjects))
    expect_equal(result6$maxNumberOfSubjects1, c(75, 75, 75), label = paste0(result6$maxNumberOfSubjects1))
    expect_equal(result6$maxNumberOfSubjects2, c(75, 75, 75), label = paste0(result6$maxNumberOfSubjects2))
    expect_equal(result6$rejectPerStage[1, ], 0.033479189, tolerance = 1e-07, label = paste0(result6$rejectPerStage[1, ]))
    expect_equal(result6$rejectPerStage[2, ], 0.27519472, tolerance = 1e-07, label = paste0(result6$rejectPerStage[2, ]))
    expect_equal(result6$rejectPerStage[3, ], 0.49132609, tolerance = 1e-07, label = paste0(result6$rejectPerStage[3, ]))
    expect_equal(result6$earlyStop, 0.30867391, tolerance = 1e-07, label = paste0(result6$earlyStop))
    expect_equal(result6$calendarTime[1, ], c(6.987616, 12.444445, 56.948814), tolerance = 1e-07, label = paste0(result6$calendarTime[1, ]))
    expect_equal(result6$calendarTime[2, ], c(9.3944745, 15.885169, 92.299449), tolerance = 1e-07, label = paste0(result6$calendarTime[2, ]))
    expect_equal(result6$calendarTime[3, ], c(12.259611, 20.071241, 153.10254), tolerance = 1e-07, label = paste0(result6$calendarTime[3, ]))
    expect_equal(result6$expectedStudyDurationH1, c(11.294639, 18.663917, 133.1507), tolerance = 1e-07, label = paste0(result6$expectedStudyDurationH1))
    expect_equal(result6$numberOfSubjects[1, ], c(34, 74, 150), label = paste0(result6$numberOfSubjects[1, ]))
    expect_equal(result6$numberOfSubjects[2, ], c(46, 108, 150), label = paste0(result6$numberOfSubjects[2, ]))
    expect_equal(result6$numberOfSubjects[3, ], c(72, 150, 150), label = paste0(result6$numberOfSubjects[3, ]))
    expect_equal(result6$expectedNumberOfSubjectsH1, c(63.572728, 135.8974, 150), tolerance = 1e-07, label = paste0(result6$expectedNumberOfSubjectsH1))
    expect_equal(result6$informationOverStages[1, ], c(37.273683, 129.81863, 2332.3707), tolerance = 1e-07, label = paste0(result6$informationOverStages[1, ]))
    expect_equal(result6$informationOverStages[2, ], c(66.393749, 231.23943, 4154.5353), tolerance = 1e-07, label = paste0(result6$informationOverStages[2, ]))
    expect_equal(result6$informationOverStages[3, ], c(116.48026, 405.68322, 7288.6583), tolerance = 1e-07, label = paste0(result6$informationOverStages[3, ]))
    expect_equal(result6$expectedInformationH0, c(116.24237, 404.85466, 7273.7722), tolerance = 1e-07, label = paste0(result6$expectedInformationH0))
    expect_equal(result6$expectedInformationH01, c(114.00093, 397.04807, 7133.516), tolerance = 1e-07, label = paste0(result6$expectedInformationH01))
    expect_equal(result6$expectedInformationH1, c(100.04494, 348.44146, 6260.2312), tolerance = 1e-07, label = paste0(result6$expectedInformationH1))
    expect_equal(result6$maxInformation, c(116.48026, 405.68322, 7288.6583), tolerance = 1e-07, label = paste0(result6$maxInformation))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result6), NA)))
        expect_output(print(result6)$show())
        invisible(capture.output(expect_error(summary(result6), NA)))
        expect_output(summary(result6)$show())
        result6CodeBased <- eval(parse(text = getObjectRCode(result6, stringWrapParagraphWidth = NULL)))
        expect_equal(result6CodeBased$directionUpper, result6$directionUpper, tolerance = 1e-07)
        expect_equal(result6CodeBased$theta, result6$theta, tolerance = 1e-07)
        expect_equal(result6CodeBased$maxNumberOfSubjects, result6$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(result6CodeBased$maxNumberOfSubjects1, result6$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(result6CodeBased$maxNumberOfSubjects2, result6$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(result6CodeBased$rejectPerStage, result6$rejectPerStage, tolerance = 1e-07)
        expect_equal(result6CodeBased$earlyStop, result6$earlyStop, tolerance = 1e-07)
        expect_equal(result6CodeBased$calendarTime, result6$calendarTime, tolerance = 1e-07)
        expect_equal(result6CodeBased$expectedStudyDurationH1, result6$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(result6CodeBased$numberOfSubjects, result6$numberOfSubjects, tolerance = 1e-07)
        expect_equal(result6CodeBased$expectedNumberOfSubjectsH1, result6$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(result6CodeBased$informationOverStages, result6$informationOverStages, tolerance = 1e-07)
        expect_equal(result6CodeBased$expectedInformationH0, result6$expectedInformationH0, tolerance = 1e-07)
        expect_equal(result6CodeBased$expectedInformationH01, result6$expectedInformationH01, tolerance = 1e-07)
        expect_equal(result6CodeBased$expectedInformationH1, result6$expectedInformationH1, tolerance = 1e-07)
        expect_equal(result6CodeBased$maxInformation, result6$maxInformation, tolerance = 1e-07)
        expect_type(names(result6), "character")
        df <- as.data.frame(result6)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result6)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizeCountsFixedExposureSingleStage}
    # @refFS[Formula]{fs:sampleSizePerStageFixedExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    # @refFS[Formula]{fs:sampleSizeCountsFixedExposureOptimumAllocationRatio}
    result7 <- getSampleSizeCounts(
        design = designGS2, lambda2 = 0.234, lambda1 = c(0.1, 0.2),
        allocationRatioPlanned = 0, accrualTime = 7, fixedExposureTime = 1
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result7' with expected results
    expect_equal(result7$allocationRatioPlanned, c(1.5297048, 1.0816655), tolerance = 1e-07, label = paste0(result7$allocationRatioPlanned))
    expect_equal(result7$directionUpper, c(FALSE, FALSE), label = paste0(result7$directionUpper))
    expect_equal(result7$theta, c(0.42735043, 0.85470085), tolerance = 1e-07, label = paste0(result7$theta))
    expect_equal(result7$maxNumberOfSubjects, c(366, 7243), label = paste0(result7$maxNumberOfSubjects))
    expect_equal(result7$maxNumberOfSubjects1, c(221, 3764), label = paste0(result7$maxNumberOfSubjects1))
    expect_equal(result7$maxNumberOfSubjects2, c(145, 3479), label = paste0(result7$maxNumberOfSubjects2))
    expect_equal(result7$rejectPerStage[1, ], 0.033479189, tolerance = 1e-07, label = paste0(result7$rejectPerStage[1, ]))
    expect_equal(result7$rejectPerStage[2, ], 0.27519472, tolerance = 1e-07, label = paste0(result7$rejectPerStage[2, ]))
    expect_equal(result7$rejectPerStage[3, ], 0.49132609, tolerance = 1e-07, label = paste0(result7$rejectPerStage[3, ]))
    expect_equal(result7$earlyStop, 0.30867391, tolerance = 1e-07, label = paste0(result7$earlyStop))
    expect_equal(result7$calendarTime[1, ], c(2.7255466, 2.7393702), tolerance = 1e-07, label = paste0(result7$calendarTime[1, ]))
    expect_equal(result7$calendarTime[2, ], c(4.4792097, 4.4896333), tolerance = 1e-07, label = paste0(result7$calendarTime[2, ]))
    expect_equal(result7$calendarTime[3, ], c(8, 8), label = paste0(result7$calendarTime[3, ]))
    expect_equal(result7$expectedStudyDurationH1, c(6.8545127, 6.857844), tolerance = 1e-07, label = paste0(result7$expectedStudyDurationH1))
    expect_equal(result7$numberOfSubjects[1, ], c(143, 2835), label = paste0(result7$numberOfSubjects[1, ]))
    expect_equal(result7$numberOfSubjects[2, ], c(234, 4645), label = paste0(result7$numberOfSubjects[2, ]))
    expect_equal(result7$numberOfSubjects[3, ], c(366, 7243), label = paste0(result7$numberOfSubjects[3, ]))
    expect_equal(result7$expectedNumberOfSubjectsH1, c(322.20844, 6380.4678), tolerance = 1e-07, label = paste0(result7$expectedNumberOfSubjectsH1))
    expect_equal(result7$informationOverStages[1, ], c(4.2681123, 125.14338), tolerance = 1e-07, label = paste0(result7$informationOverStages[1, ]))
    expect_equal(result7$informationOverStages[2, ], c(7.6025751, 222.91164), tolerance = 1e-07, label = paste0(result7$informationOverStages[2, ]))
    expect_equal(result7$informationOverStages[3, ], c(13.337851, 391.07305), tolerance = 1e-07, label = paste0(result7$informationOverStages[3, ]))
    expect_equal(result7$expectedInformationH0, c(13.31061, 390.27433), tolerance = 1e-07, label = paste0(result7$expectedInformationH0))
    expect_equal(result7$expectedInformationH01, c(13.053949, 382.74888), tolerance = 1e-07, label = paste0(result7$expectedInformationH01))
    expect_equal(result7$expectedInformationH1, c(11.455885, 335.89278), tolerance = 1e-07, label = paste0(result7$expectedInformationH1))
    expect_equal(result7$maxInformation, c(13.337851, 391.07305), tolerance = 1e-07, label = paste0(result7$maxInformation))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result7), NA)))
        expect_output(print(result7)$show())
        invisible(capture.output(expect_error(summary(result7), NA)))
        expect_output(summary(result7)$show())
        result7CodeBased <- eval(parse(text = getObjectRCode(result7, stringWrapParagraphWidth = NULL)))
        expect_equal(result7CodeBased$allocationRatioPlanned, result7$allocationRatioPlanned, tolerance = 1e-07)
        expect_equal(result7CodeBased$directionUpper, result7$directionUpper, tolerance = 1e-07)
        expect_equal(result7CodeBased$theta, result7$theta, tolerance = 1e-07)
        expect_equal(result7CodeBased$maxNumberOfSubjects, result7$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(result7CodeBased$maxNumberOfSubjects1, result7$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(result7CodeBased$maxNumberOfSubjects2, result7$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(result7CodeBased$rejectPerStage, result7$rejectPerStage, tolerance = 1e-07)
        expect_equal(result7CodeBased$earlyStop, result7$earlyStop, tolerance = 1e-07)
        expect_equal(result7CodeBased$calendarTime, result7$calendarTime, tolerance = 1e-07)
        expect_equal(result7CodeBased$expectedStudyDurationH1, result7$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(result7CodeBased$numberOfSubjects, result7$numberOfSubjects, tolerance = 1e-07)
        expect_equal(result7CodeBased$expectedNumberOfSubjectsH1, result7$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(result7CodeBased$informationOverStages, result7$informationOverStages, tolerance = 1e-07)
        expect_equal(result7CodeBased$expectedInformationH0, result7$expectedInformationH0, tolerance = 1e-07)
        expect_equal(result7CodeBased$expectedInformationH01, result7$expectedInformationH01, tolerance = 1e-07)
        expect_equal(result7CodeBased$expectedInformationH1, result7$expectedInformationH1, tolerance = 1e-07)
        expect_equal(result7CodeBased$maxInformation, result7$maxInformation, tolerance = 1e-07)
        expect_type(names(result7), "character")
        df <- as.data.frame(result7)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result7)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:FisherInfCounts}
    # @refFS[Formula]{fs:sampleSizePerStageVariableExposureCounts}
    # @refFS[Formula]{fs:observationTimePerStageCounts}
    # @refFS[Formula]{fs:sampleSizeCountsVariableExposureOptimumAllocationRatio}
    result8 <- getSampleSizeCounts(
        design = designGS2, lambda2 = 0.234, lambda1 = c(0.1, 0.2),
        allocationRatioPlanned = 0, accrualTime = 7, followUpTime = 1
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result8' with expected results
    expect_equal(result8$allocationRatioPlanned, c(1.7934046, 1.1087896), tolerance = 1e-07, label = paste0(result8$allocationRatioPlanned))
    expect_equal(result8$directionUpper, c(FALSE, FALSE), label = paste0(result8$directionUpper))
    expect_equal(result8$theta, c(0.42735043, 0.85470085), tolerance = 1e-07, label = paste0(result8$theta))
    expect_equal(result8$maxNumberOfSubjects, c(82, 1610), label = paste0(result8$maxNumberOfSubjects))
    expect_equal(result8$maxNumberOfSubjects1, c(53, 847), label = paste0(result8$maxNumberOfSubjects1))
    expect_equal(result8$maxNumberOfSubjects2, c(29, 763), label = paste0(result8$maxNumberOfSubjects2))
    expect_equal(result8$rejectPerStage[1, ], 0.033479189, tolerance = 1e-07, label = paste0(result8$rejectPerStage[1, ]))
    expect_equal(result8$rejectPerStage[2, ], 0.27519472, tolerance = 1e-07, label = paste0(result8$rejectPerStage[2, ]))
    expect_equal(result8$rejectPerStage[3, ], 0.49132609, tolerance = 1e-07, label = paste0(result8$rejectPerStage[3, ]))
    expect_equal(result8$earlyStop, 0.30867391, tolerance = 1e-07, label = paste0(result8$earlyStop))
    expect_equal(result8$calendarTime[1, ], c(4.4478569, 4.4878996), tolerance = 1e-07, label = paste0(result8$calendarTime[1, ]))
    expect_equal(result8$calendarTime[2, ], c(5.9668445, 5.9911665), tolerance = 1e-07, label = paste0(result8$calendarTime[2, ]))
    expect_equal(result8$calendarTime[3, ], c(8, 8), label = paste0(result8$calendarTime[3, ]))
    expect_equal(result8$expectedStudyDurationH1, c(7.3215635, 7.3295973), tolerance = 1e-07, label = paste0(result8$expectedStudyDurationH1))
    expect_equal(result8$numberOfSubjects[1, ], c(52, 1032), label = paste0(result8$numberOfSubjects[1, ]))
    expect_equal(result8$numberOfSubjects[2, ], c(69, 1378), label = paste0(result8$numberOfSubjects[2, ]))
    expect_equal(result8$numberOfSubjects[3, ], c(82, 1610), label = paste0(result8$numberOfSubjects[3, ]))
    expect_equal(result8$expectedNumberOfSubjectsH1, c(77.418093, 1526.8039), tolerance = 1e-07, label = paste0(result8$expectedNumberOfSubjectsH1))
    expect_equal(result8$informationOverStages[1, ], c(4.2681123, 125.14338), tolerance = 1e-07, label = paste0(result8$informationOverStages[1, ]))
    expect_equal(result8$informationOverStages[2, ], c(7.6025751, 222.91164), tolerance = 1e-07, label = paste0(result8$informationOverStages[2, ]))
    expect_equal(result8$informationOverStages[3, ], c(13.337851, 391.07305), tolerance = 1e-07, label = paste0(result8$informationOverStages[3, ]))
    expect_equal(result8$expectedInformationH0, c(13.31061, 390.27433), tolerance = 1e-07, label = paste0(result8$expectedInformationH0))
    expect_equal(result8$expectedInformationH01, c(13.053949, 382.74888), tolerance = 1e-07, label = paste0(result8$expectedInformationH01))
    expect_equal(result8$expectedInformationH1, c(11.455885, 335.89278), tolerance = 1e-07, label = paste0(result8$expectedInformationH1))
    expect_equal(result8$maxInformation, c(13.337851, 391.07305), tolerance = 1e-07, label = paste0(result8$maxInformation))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result8), NA)))
        expect_output(print(result8)$show())
        invisible(capture.output(expect_error(summary(result8), NA)))
        expect_output(summary(result8)$show())
        result8CodeBased <- eval(parse(text = getObjectRCode(result8, stringWrapParagraphWidth = NULL)))
        expect_equal(result8CodeBased$allocationRatioPlanned, result8$allocationRatioPlanned, tolerance = 1e-07)
        expect_equal(result8CodeBased$directionUpper, result8$directionUpper, tolerance = 1e-07)
        expect_equal(result8CodeBased$theta, result8$theta, tolerance = 1e-07)
        expect_equal(result8CodeBased$maxNumberOfSubjects, result8$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(result8CodeBased$maxNumberOfSubjects1, result8$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(result8CodeBased$maxNumberOfSubjects2, result8$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(result8CodeBased$rejectPerStage, result8$rejectPerStage, tolerance = 1e-07)
        expect_equal(result8CodeBased$earlyStop, result8$earlyStop, tolerance = 1e-07)
        expect_equal(result8CodeBased$calendarTime, result8$calendarTime, tolerance = 1e-07)
        expect_equal(result8CodeBased$expectedStudyDurationH1, result8$expectedStudyDurationH1, tolerance = 1e-07)
        expect_equal(result8CodeBased$numberOfSubjects, result8$numberOfSubjects, tolerance = 1e-07)
        expect_equal(result8CodeBased$expectedNumberOfSubjectsH1, result8$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(result8CodeBased$informationOverStages, result8$informationOverStages, tolerance = 1e-07)
        expect_equal(result8CodeBased$expectedInformationH0, result8$expectedInformationH0, tolerance = 1e-07)
        expect_equal(result8CodeBased$expectedInformationH01, result8$expectedInformationH01, tolerance = 1e-07)
        expect_equal(result8CodeBased$expectedInformationH1, result8$expectedInformationH1, tolerance = 1e-07)
        expect_equal(result8CodeBased$maxInformation, result8$maxInformation, tolerance = 1e-07)
        expect_type(names(result8), "character")
        df <- as.data.frame(result8)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result8)
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
    result1 <- getPowerCounts(
        design = designGS1, maxNumberOfSubjects = 400, directionUpper = FALSE,
        overdispersion = 1, fixedExposureTime = 1, lambda1 = seq(1.05, 1.55, 0.1), lambda2 = 1.4
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result1' with expected results
    expect_equal(result1$theta, c(0.75, 0.82142857, 0.89285714, 0.96428571, 1.0357143, 1.1071429), tolerance = 1e-07, label = paste0(result1$theta))
    expect_equal(result1$overallReject, c(0.53364515, 0.28922288, 0.1246922, 0.044025943, 0.013287033, 0.0036093301), tolerance = 1e-07, label = paste0(result1$overallReject))
    expect_equal(result1$rejectPerStage[1, ], c(0.060449997, 0.027325942, 0.011625783, 0.0047105893, 0.0018358964, 0.00069394873), tolerance = 1e-07, label = paste0(result1$rejectPerStage[1, ]))
    expect_equal(result1$rejectPerStage[2, ], c(0.16740522, 0.081280885, 0.033932049, 0.012489808, 0.0041413056, 0.0012595369), tolerance = 1e-07, label = paste0(result1$rejectPerStage[2, ]))
    expect_equal(result1$rejectPerStage[3, ], c(0.30578993, 0.18061605, 0.079134367, 0.026825545, 0.0073098308, 0.0016558445), tolerance = 1e-07, label = paste0(result1$rejectPerStage[3, ]))
    expect_equal(result1$futilityStop, c(0.093456372, 0.19549875, 0.33823583, 0.50079777, 0.65574892, 0.78246895), tolerance = 1e-07, label = paste0(result1$futilityStop))
    expect_equal(result1$futilityPerStage[1, ], c(0.054589249, 0.10911095, 0.18844857, 0.28892653, 0.40212886, 0.5176485), tolerance = 1e-07, label = paste0(result1$futilityPerStage[1, ]))
    expect_equal(result1$futilityPerStage[2, ], c(0.038867123, 0.086387802, 0.14978727, 0.21187124, 0.25362007, 0.26482046), tolerance = 1e-07, label = paste0(result1$futilityPerStage[2, ]))
    expect_equal(result1$earlyStop, c(0.32131159, 0.30410558, 0.38379367, 0.51799816, 0.66172612, 0.78442244), tolerance = 1e-07, label = paste0(result1$earlyStop))
    expect_equal(result1$futilityBoundsPValueScale[1, ], 0.65542174, tolerance = 1e-07, label = paste0(result1$futilityBoundsPValueScale[1, ]))
    expect_equal(result1$futilityBoundsPValueScale[2, ], 0.46017216, tolerance = 1e-07, label = paste0(result1$futilityBoundsPValueScale[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result1), NA)))
        expect_output(print(result1)$show())
        invisible(capture.output(expect_error(summary(result1), NA)))
        expect_output(summary(result1)$show())
        result1CodeBased <- eval(parse(text = getObjectRCode(result1, stringWrapParagraphWidth = NULL)))
        expect_equal(result1CodeBased$theta, result1$theta, tolerance = 1e-07)
        expect_equal(result1CodeBased$overallReject, result1$overallReject, tolerance = 1e-07)
        expect_equal(result1CodeBased$rejectPerStage, result1$rejectPerStage, tolerance = 1e-07)
        expect_equal(result1CodeBased$futilityStop, result1$futilityStop, tolerance = 1e-07)
        expect_equal(result1CodeBased$futilityPerStage, result1$futilityPerStage, tolerance = 1e-07)
        expect_equal(result1CodeBased$earlyStop, result1$earlyStop, tolerance = 1e-07)
        expect_equal(result1CodeBased$futilityBoundsPValueScale, result1$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(result1), "character")
        df <- as.data.frame(result1)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result1)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:ShiftParameterCountData}
    # @refFS[Formula]{fs:VarianceEstimateFixedCountData}
    result2 <- getPowerCounts(
        design = designGS1, maxNumberOfSubjects = 400, directionUpper = FALSE,
        thetaH0 = 0.9, overdispersion = 0.3, fixedExposureTime = 1, lambda1 = seq(1.05, 1.55, 0.1), lambda2 = 1.4
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result2' with expected results
    expect_equal(result2$theta, c(0.75, 0.82142857, 0.89285714, 0.96428571, 1.0357143, 1.1071429), tolerance = 1e-07, label = paste0(result2$theta))
    expect_equal(result2$overallReject, c(0.37505189, 0.12855316, 0.029101752, 0.004756001, 0.00064764079, 8.6285962e-05), tolerance = 1e-07, label = paste0(result2$overallReject))
    expect_equal(result2$rejectPerStage[1, ], c(0.037203429, 0.011960954, 0.0033736668, 0.00085074056, 0.00019509054, 4.1296965e-05), tolerance = 1e-07, label = paste0(result2$rejectPerStage[1, ]))
    expect_equal(result2$rejectPerStage[2, ], c(0.10894254, 0.034976115, 0.0085007361, 0.0016219598, 0.00025117618, 3.2519937e-05), tolerance = 1e-07, label = paste0(result2$rejectPerStage[2, ]))
    expect_equal(result2$rejectPerStage[3, ], c(0.22890592, 0.081616089, 0.01722735, 0.0022833007, 0.00020137408, 1.2469061e-05), tolerance = 1e-07, label = paste0(result2$rejectPerStage[3, ]))
    expect_equal(result2$futilityStop, c(0.15143926, 0.33317053, 0.55855018, 0.75910283, 0.8919586, 0.95982659), tolerance = 1e-07, label = paste0(result2$futilityStop))
    expect_equal(result2$futilityPerStage[1, ], c(0.085530884, 0.18552122, 0.32857927, 0.49402485, 0.65311126, 0.78343373), tolerance = 1e-07, label = paste0(result2$futilityPerStage[1, ]))
    expect_equal(result2$futilityPerStage[2, ], c(0.065908377, 0.14764931, 0.22997091, 0.26507798, 0.23884734, 0.17639285), tolerance = 1e-07, label = paste0(result2$futilityPerStage[2, ]))
    expect_equal(result2$earlyStop, c(0.29758523, 0.3801076, 0.57042459, 0.76157553, 0.89240487, 0.9599004), tolerance = 1e-07, label = paste0(result2$earlyStop))
    expect_equal(result2$futilityBoundsPValueScale[1, ], 0.65542174, tolerance = 1e-07, label = paste0(result2$futilityBoundsPValueScale[1, ]))
    expect_equal(result2$futilityBoundsPValueScale[2, ], 0.46017216, tolerance = 1e-07, label = paste0(result2$futilityBoundsPValueScale[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result2), NA)))
        expect_output(print(result2)$show())
        invisible(capture.output(expect_error(summary(result2), NA)))
        expect_output(summary(result2)$show())
        result2CodeBased <- eval(parse(text = getObjectRCode(result2, stringWrapParagraphWidth = NULL)))
        expect_equal(result2CodeBased$theta, result2$theta, tolerance = 1e-07)
        expect_equal(result2CodeBased$overallReject, result2$overallReject, tolerance = 1e-07)
        expect_equal(result2CodeBased$rejectPerStage, result2$rejectPerStage, tolerance = 1e-07)
        expect_equal(result2CodeBased$futilityStop, result2$futilityStop, tolerance = 1e-07)
        expect_equal(result2CodeBased$futilityPerStage, result2$futilityPerStage, tolerance = 1e-07)
        expect_equal(result2CodeBased$earlyStop, result2$earlyStop, tolerance = 1e-07)
        expect_equal(result2CodeBased$futilityBoundsPValueScale, result2$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(result2), "character")
        df <- as.data.frame(result2)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result2)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:ShiftParameterCountData}
    # @refFS[Formula]{fs:VarianceEstimateVariableCountData}
    result3 <- getPowerCounts(
        design = designGS1, maxNumberOfSubjects = 400, directionUpper = FALSE,
        overdispersion = 0.4, accrualTime = 4, followUpTime = 3, lambda1 = seq(1.05, 1.55, 0.1), lambda2 = 1.4
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result3' with expected results
    expect_equal(result3$theta, c(0.75, 0.82142857, 0.89285714, 0.96428571, 1.0357143, 1.1071429), tolerance = 1e-07, label = paste0(result3$theta))
    expect_equal(result3$overallReject, c(0.95733458, 0.71461505, 0.30541815, 0.066441103, 0.0080473097, 0.00068660473), tolerance = 1e-07, label = paste0(result3$overallReject))
    expect_equal(result3$rejectPerStage[1, ], c(0.27300416, 0.10162379, 0.02908059, 0.0066447778, 0.0012578105, 0.00020378378), tolerance = 1e-07, label = paste0(result3$rejectPerStage[1, ]))
    expect_equal(result3$rejectPerStage[2, ], c(0.42120665, 0.25113898, 0.086321841, 0.018416222, 0.0026215993, 0.00026573717), tolerance = 1e-07, label = paste0(result3$rejectPerStage[2, ]))
    expect_equal(result3$rejectPerStage[3, ], c(0.26312377, 0.36185228, 0.19001572, 0.041380104, 0.0041678999, 0.00021708378), tolerance = 1e-07, label = paste0(result3$rejectPerStage[3, ]))
    expect_equal(result3$futilityStop, c(0.0072113169, 0.048422794, 0.18620987, 0.43924942, 0.70950882, 0.88913782), tolerance = 1e-07, label = paste0(result3$futilityStop))
    expect_equal(result3$futilityPerStage[1, ], c(0.005399273, 0.03001657, 0.10411951, 0.24920632, 0.44771069, 0.64885499), tolerance = 1e-07, label = paste0(result3$futilityPerStage[1, ]))
    expect_equal(result3$futilityPerStage[2, ], c(0.0018120439, 0.018406224, 0.082090356, 0.19004311, 0.26179813, 0.24028283), tolerance = 1e-07, label = paste0(result3$futilityPerStage[2, ]))
    expect_equal(result3$earlyStop, c(0.70142213, 0.40118557, 0.3016123, 0.46431042, 0.71338823, 0.88960734), tolerance = 1e-07, label = paste0(result3$earlyStop))
    expect_equal(result3$futilityBoundsPValueScale[1, ], 0.65542174, tolerance = 1e-07, label = paste0(result3$futilityBoundsPValueScale[1, ]))
    expect_equal(result3$futilityBoundsPValueScale[2, ], 0.46017216, tolerance = 1e-07, label = paste0(result3$futilityBoundsPValueScale[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result3), NA)))
        expect_output(print(result3)$show())
        invisible(capture.output(expect_error(summary(result3), NA)))
        expect_output(summary(result3)$show())
        result3CodeBased <- eval(parse(text = getObjectRCode(result3, stringWrapParagraphWidth = NULL)))
        expect_equal(result3CodeBased$theta, result3$theta, tolerance = 1e-07)
        expect_equal(result3CodeBased$overallReject, result3$overallReject, tolerance = 1e-07)
        expect_equal(result3CodeBased$rejectPerStage, result3$rejectPerStage, tolerance = 1e-07)
        expect_equal(result3CodeBased$futilityStop, result3$futilityStop, tolerance = 1e-07)
        expect_equal(result3CodeBased$futilityPerStage, result3$futilityPerStage, tolerance = 1e-07)
        expect_equal(result3CodeBased$earlyStop, result3$earlyStop, tolerance = 1e-07)
        expect_equal(result3CodeBased$futilityBoundsPValueScale, result3$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(result3), "character")
        df <- as.data.frame(result3)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result3)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:ShiftParameterCountData}
    # @refFS[Formula]{fs:VarianceEstimateVariableCountData}
    result4 <- getPowerCounts(
        design = designGS1, accrualTime = c(0, 5, 10), accrualIntensity = c(8, 15),
        followUpTime = 12, lambda1 = seq(0.85, 1.35, 0.15), lambda2 = 1
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result4' with expected results
    expect_equal(result4$theta, c(0.85, 1, 1.15, 1.3), tolerance = 1e-07, label = paste0(result4$theta))
    expect_equal(result4$maxNumberOfSubjects, 116, label = paste0(result4$maxNumberOfSubjects))
    expect_equal(result4$overallReject, c(1.937885e-06, 0.024590075, 0.85818227, 0.99990061), tolerance = 1e-07, label = paste0(result4$overallReject))
    expect_equal(result4$rejectPerStage[1, ], c(1.5565135e-06, 0.0029534648, 0.16345323, 0.74780476), tolerance = 1e-07, label = paste0(result4$rejectPerStage[1, ]))
    expect_equal(result4$rejectPerStage[2, ], c(3.6073915e-07, 0.0072776996, 0.33995936, 0.23820024), tolerance = 1e-07, label = paste0(result4$rejectPerStage[2, ]))
    expect_equal(result4$rejectPerStage[3, ], c(2.0632389e-08, 0.014358911, 0.35476968, 0.013895607), tolerance = 1e-07, label = paste0(result4$rejectPerStage[3, ]))
    expect_equal(result4$futilityStop, c(0.99659248, 0.58079374, 0.022238098, 6.9322346e-05), tolerance = 1e-07, label = paste0(result4$futilityStop))
    expect_equal(result4$futilityPerStage[1, ], c(0.93451839, 0.34457826, 0.014907497, 6.6590384e-05), tolerance = 1e-07, label = paste0(result4$futilityPerStage[1, ]))
    expect_equal(result4$futilityPerStage[2, ], c(0.06207409, 0.23621548, 0.007330601, 2.7319617e-06), tolerance = 1e-07, label = paste0(result4$futilityPerStage[2, ]))
    expect_equal(result4$earlyStop, c(0.9965944, 0.59102491, 0.52565069, 0.98607433), tolerance = 1e-07, label = paste0(result4$earlyStop))
    expect_equal(result4$futilityBoundsPValueScale[1, ], 0.65542174, tolerance = 1e-07, label = paste0(result4$futilityBoundsPValueScale[1, ]))
    expect_equal(result4$futilityBoundsPValueScale[2, ], 0.46017216, tolerance = 1e-07, label = paste0(result4$futilityBoundsPValueScale[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result4), NA)))
        expect_output(print(result4)$show())
        invisible(capture.output(expect_error(summary(result4), NA)))
        expect_output(summary(result4)$show())
        result4CodeBased <- eval(parse(text = getObjectRCode(result4, stringWrapParagraphWidth = NULL)))
        expect_equal(result4CodeBased$theta, result4$theta, tolerance = 1e-07)
        expect_equal(result4CodeBased$maxNumberOfSubjects, result4$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(result4CodeBased$overallReject, result4$overallReject, tolerance = 1e-07)
        expect_equal(result4CodeBased$rejectPerStage, result4$rejectPerStage, tolerance = 1e-07)
        expect_equal(result4CodeBased$futilityStop, result4$futilityStop, tolerance = 1e-07)
        expect_equal(result4CodeBased$futilityPerStage, result4$futilityPerStage, tolerance = 1e-07)
        expect_equal(result4CodeBased$earlyStop, result4$earlyStop, tolerance = 1e-07)
        expect_equal(result4CodeBased$futilityBoundsPValueScale, result4$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(result4), "character")
        df <- as.data.frame(result4)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result4)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:ShiftParameterCountData}
    # @refFS[Formula]{fs:VarianceEstimateVariableCountData}
    result5 <- getPowerCounts(
        design = designGS1, accrualTime = c(0, 5, 10), accrualIntensity = c(8, 15),
        thetaH0 = 1.1, followUpTime = 12, lambda1 = seq(0.85, 1.35, 0.15), lambda2 = 1
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result5' with expected results
    expect_equal(result5$theta, c(0.85, 1, 1.15, 1.3), tolerance = 1e-07, label = paste0(result5$theta))
    expect_equal(result5$maxNumberOfSubjects, 116, label = paste0(result5$maxNumberOfSubjects))
    expect_equal(result5$overallReject, c(3.7156364e-09, 9.3368852e-05, 0.15506472, 0.96161905), tolerance = 1e-07, label = paste0(result5$overallReject))
    expect_equal(result5$rejectPerStage[1, ], c(3.6573895e-09, 4.3981527e-05, 0.014293285, 0.28265162), tolerance = 1e-07, label = paste0(result5$rejectPerStage[1, ]))
    expect_equal(result5$rejectPerStage[2, ], c(5.8201725e-11, 3.5377143e-05, 0.04221655, 0.42481312), tolerance = 1e-07, label = paste0(result5$rejectPerStage[2, ]))
    expect_equal(result5$rejectPerStage[3, ], c(4.5192294e-14, 1.4010181e-05, 0.098554881, 0.25415431), tolerance = 1e-07, label = paste0(result5$rejectPerStage[3, ]))
    expect_equal(result5$futilityStop, c(0.99998595, 0.95807542, 0.30175979, 0.0065824903), tolerance = 1e-07, label = paste0(result5$futilityStop))
    expect_equal(result5$futilityPerStage[1, ], c(0.99573866, 0.77896313, 0.16758257, 0.0049698331), tolerance = 1e-07, label = paste0(result5$futilityPerStage[1, ]))
    expect_equal(result5$futilityPerStage[2, ], c(0.0042472923, 0.17911229, 0.13417722, 0.0016126572), tolerance = 1e-07, label = paste0(result5$futilityPerStage[2, ]))
    expect_equal(result5$earlyStop, c(0.99998595, 0.95815478, 0.35826962, 0.71404723), tolerance = 1e-07, label = paste0(result5$earlyStop))
    expect_equal(result5$futilityBoundsPValueScale[1, ], 0.65542174, tolerance = 1e-07, label = paste0(result5$futilityBoundsPValueScale[1, ]))
    expect_equal(result5$futilityBoundsPValueScale[2, ], 0.46017216, tolerance = 1e-07, label = paste0(result5$futilityBoundsPValueScale[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result5), NA)))
        expect_output(print(result5)$show())
        invisible(capture.output(expect_error(summary(result5), NA)))
        expect_output(summary(result5)$show())
        result5CodeBased <- eval(parse(text = getObjectRCode(result5, stringWrapParagraphWidth = NULL)))
        expect_equal(result5CodeBased$theta, result5$theta, tolerance = 1e-07)
        expect_equal(result5CodeBased$maxNumberOfSubjects, result5$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(result5CodeBased$overallReject, result5$overallReject, tolerance = 1e-07)
        expect_equal(result5CodeBased$rejectPerStage, result5$rejectPerStage, tolerance = 1e-07)
        expect_equal(result5CodeBased$futilityStop, result5$futilityStop, tolerance = 1e-07)
        expect_equal(result5CodeBased$futilityPerStage, result5$futilityPerStage, tolerance = 1e-07)
        expect_equal(result5CodeBased$earlyStop, result5$earlyStop, tolerance = 1e-07)
        expect_equal(result5CodeBased$futilityBoundsPValueScale, result5$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(result5), "character")
        df <- as.data.frame(result5)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result5)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getPowerCounts': Power calculation of testing count data for two-sided group sequential design", {
    .skipTestIfDisabled()

    designGS2 <- getDesignGroupSequential(
        informationRates = c(0.57, 1),
        sided = 2, beta = 0.2, typeOfDesign = "WT", deltaWT = 0.1
    )

    # @refFS[Formula]{fs:ShiftParameterCountData}
    # @refFS[Formula]{fs:VarianceEstimateFixedCountData}
    result1 <- getPowerCounts(
        design = designGS2, maxNumberOfSubjects = 400,
        overdispersion = 1, fixedExposureTime = 1, lambda1 = seq(1.05, 1.65, 0.15),
        lambda2 = 1.4
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result1' with expected results
    expect_equal(result1$theta, c(0.75, 0.85714286, 0.96428571, 1.0714286, 1.1785714), tolerance = 1e-07, label = paste0(result1$theta))
    expect_equal(result1$overallReject, c(0.44675032, 0.13692139, 0.030483803, 0.045822437, 0.16403486), tolerance = 1e-07, label = paste0(result1$overallReject))
    expect_equal(result1$rejectPerStage[1, ], c(0.10712223, 0.024389615, 0.005305078, 0.0078168805, 0.02989557), tolerance = 1e-07, label = paste0(result1$rejectPerStage[1, ]))
    expect_equal(result1$rejectPerStage[2, ], c(0.33962809, 0.11253177, 0.025178725, 0.038005557, 0.13413929), tolerance = 1e-07, label = paste0(result1$rejectPerStage[2, ]))
    expect_equal(result1$earlyStop, c(0.10712223, 0.024389615, 0.005305078, 0.0078168805, 0.02989557), tolerance = 1e-07, label = paste0(result1$earlyStop))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result1), NA)))
        expect_output(print(result1)$show())
        invisible(capture.output(expect_error(summary(result1), NA)))
        expect_output(summary(result1)$show())
        result1CodeBased <- eval(parse(text = getObjectRCode(result1, stringWrapParagraphWidth = NULL)))
        expect_equal(result1CodeBased$theta, result1$theta, tolerance = 1e-07)
        expect_equal(result1CodeBased$overallReject, result1$overallReject, tolerance = 1e-07)
        expect_equal(result1CodeBased$rejectPerStage, result1$rejectPerStage, tolerance = 1e-07)
        expect_equal(result1CodeBased$earlyStop, result1$earlyStop, tolerance = 1e-07)
        expect_type(names(result1), "character")
        df <- as.data.frame(result1)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result1)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:ShiftParameterCountData}
    # @refFS[Formula]{fs:VarianceEstimateFixedCountData}
    result2 <- getPowerCounts(
        design = designGS2, maxNumberOfSubjects = 400,
        overdispersion = 0.3, fixedExposureTime = 1, theta = seq(0.7, 1.55, 0.2), lambda2 = 1.4
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result2' with expected results
    expect_equal(result2$lambda1, c(0.98, 1.26, 1.54, 1.82, 2.1), tolerance = 1e-07, label = paste0(result2$lambda1))
    expect_equal(result2$overallReject, c(0.85058327, 0.11065739, 0.099246729, 0.67623038, 0.97836543), tolerance = 1e-07, label = paste0(result2$overallReject))
    expect_equal(result2$rejectPerStage[1, ], c(0.36176735, 0.019313406, 0.017184738, 0.21355347, 0.65145735), tolerance = 1e-07, label = paste0(result2$rejectPerStage[1, ]))
    expect_equal(result2$rejectPerStage[2, ], c(0.48881593, 0.091343983, 0.082061991, 0.46267692, 0.32690808), tolerance = 1e-07, label = paste0(result2$rejectPerStage[2, ]))
    expect_equal(result2$earlyStop, c(0.36176735, 0.019313406, 0.017184738, 0.21355347, 0.65145735), tolerance = 1e-07, label = paste0(result2$earlyStop))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result2), NA)))
        expect_output(print(result2)$show())
        invisible(capture.output(expect_error(summary(result2), NA)))
        expect_output(summary(result2)$show())
        result2CodeBased <- eval(parse(text = getObjectRCode(result2, stringWrapParagraphWidth = NULL)))
        expect_equal(result2CodeBased$lambda1, result2$lambda1, tolerance = 1e-07)
        expect_equal(result2CodeBased$overallReject, result2$overallReject, tolerance = 1e-07)
        expect_equal(result2CodeBased$rejectPerStage, result2$rejectPerStage, tolerance = 1e-07)
        expect_equal(result2CodeBased$earlyStop, result2$earlyStop, tolerance = 1e-07)
        expect_type(names(result2), "character")
        df <- as.data.frame(result2)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result2)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:ShiftParameterCountData}
    # @refFS[Formula]{fs:VarianceEstimateVariableCountData}
    result3 <- getPowerCounts(
        design = designGS2, maxNumberOfSubjects = 400,
        overdispersion = 0.4, accrualTime = 4, followUpTime = 3, lambda1 = seq(1.05, 1.55, 0.1), lambda2 = 1.4
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result3' with expected results
    expect_equal(result3$theta, c(0.75, 0.82142857, 0.89285714, 0.96428571, 1.0357143, 1.1071429), tolerance = 1e-07, label = paste0(result3$theta))
    expect_equal(result3$overallReject, c(0.93771217, 0.63941931, 0.2303106, 0.042619466, 0.041526385, 0.19175607), tolerance = 1e-07, label = paste0(result3$overallReject))
    expect_equal(result3$rejectPerStage[1, ], c(0.50887066, 0.19212331, 0.044559304, 0.007285927, 0.0071055093, 0.035814782), tolerance = 1e-07, label = paste0(result3$rejectPerStage[1, ]))
    expect_equal(result3$rejectPerStage[2, ], c(0.42884152, 0.447296, 0.1857513, 0.035333539, 0.034420876, 0.15594129), tolerance = 1e-07, label = paste0(result3$rejectPerStage[2, ]))
    expect_equal(result3$earlyStop, c(0.50887066, 0.19212331, 0.044559304, 0.007285927, 0.0071055093, 0.035814782), tolerance = 1e-07, label = paste0(result3$earlyStop))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result3), NA)))
        expect_output(print(result3)$show())
        invisible(capture.output(expect_error(summary(result3), NA)))
        expect_output(summary(result3)$show())
        result3CodeBased <- eval(parse(text = getObjectRCode(result3, stringWrapParagraphWidth = NULL)))
        expect_equal(result3CodeBased$theta, result3$theta, tolerance = 1e-07)
        expect_equal(result3CodeBased$overallReject, result3$overallReject, tolerance = 1e-07)
        expect_equal(result3CodeBased$rejectPerStage, result3$rejectPerStage, tolerance = 1e-07)
        expect_equal(result3CodeBased$earlyStop, result3$earlyStop, tolerance = 1e-07)
        expect_type(names(result3), "character")
        df <- as.data.frame(result3)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result3)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:ShiftParameterCountData}
    # @refFS[Formula]{fs:VarianceEstimateVariableCountData}
    result4 <- getPowerCounts(
        design = designGS2, accrualTime = c(0, 5, 10), accrualIntensity = c(8, 15),
        followUpTime = 12, lambda1 = seq(0.85, 1.35, 0.15), lambda2 = 1
    )

    ## Comparison of the results of TrialDesignPlanCountData object 'result4' with expected results
    expect_equal(result4$theta, c(0.85, 1, 1.15, 1.3), tolerance = 1e-07, label = paste0(result4$theta))
    expect_equal(result4$maxNumberOfSubjects, 116, label = paste0(result4$maxNumberOfSubjects))
    expect_equal(result4$overallReject, c(0.86747706, 0.025, 0.8082828, 0.99992165), tolerance = 1e-07, label = paste0(result4$overallReject))
    expect_equal(result4$rejectPerStage[1, ], c(0.38341552, 0.0044259026, 0.31547093, 0.9571951), tolerance = 1e-07, label = paste0(result4$rejectPerStage[1, ]))
    expect_equal(result4$rejectPerStage[2, ], c(0.48406154, 0.020574097, 0.49281187, 0.042726555), tolerance = 1e-07, label = paste0(result4$rejectPerStage[2, ]))
    expect_equal(result4$earlyStop, c(0.38341552, 0.0044259026, 0.31547093, 0.9571951), tolerance = 1e-07, label = paste0(result4$earlyStop))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(result4), NA)))
        expect_output(print(result4)$show())
        invisible(capture.output(expect_error(summary(result4), NA)))
        expect_output(summary(result4)$show())
        result4CodeBased <- eval(parse(text = getObjectRCode(result4, stringWrapParagraphWidth = NULL)))
        expect_equal(result4CodeBased$theta, result4$theta, tolerance = 1e-07)
        expect_equal(result4CodeBased$maxNumberOfSubjects, result4$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(result4CodeBased$overallReject, result4$overallReject, tolerance = 1e-07)
        expect_equal(result4CodeBased$rejectPerStage, result4$rejectPerStage, tolerance = 1e-07)
        expect_equal(result4CodeBased$earlyStop, result4$earlyStop, tolerance = 1e-07)
        expect_type(names(result4), "character")
        df <- as.data.frame(result4)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(result4)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})
