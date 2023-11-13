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
## |  File name: test-f_design_sample_size_calculator.R
## |  Creation date: 08 November 2023, 09:10:27
## |  File version: $Revision: 7412 $
## |  Last changed: $Date: 2023-11-09 14:55:46 +0100 (Do, 09 Nov 2023) $
## |  Last changed by: $Author: pahlke $
## |

test_plan_section("Testing the Sample Size Calculation of Testing Means for Different Designs and Arguments")


test_that("'getSampleSizeMeans': Sample size calculation of testing means for one sided group sequential design", {
    # @refFS[Formula]{fs:criticalValuesWangTiatis}
    # @refFS[Formula]{fs:inflationFactor}
    # @refFS[Formula]{fs:expectedReduction}
    designGS1pretest <- getDesignGroupSequential(
        informationRates = c(0.2, 0.5, 1), sided = 1,
        beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3
    )

    ## Comparison of the results of TrialDesignGroupSequential object 'designGS1pretest' with expected results
    expect_equal(designGS1pretest$alphaSpent, c(0.0020595603, 0.0098772988, 0.02499999), tolerance = 1e-07, label = paste0("c(", paste0(designGS1pretest$alphaSpent, collapse = ", "), ")"))
    expect_equal(designGS1pretest$criticalValues, c(2.8688923, 2.3885055, 2.0793148), tolerance = 1e-07, label = paste0("c(", paste0(designGS1pretest$criticalValues, collapse = ", "), ")"))
    expect_equal(designGS1pretest$stageLevels, c(0.0020595603, 0.0084585282, 0.018794214), tolerance = 1e-07, label = paste0("c(", paste0(designGS1pretest$stageLevels, collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(designGS1pretest), NA)))
        expect_output(print(designGS1pretest)$show())
        invisible(capture.output(expect_error(summary(designGS1pretest), NA)))
        expect_output(summary(designGS1pretest)$show())
        designGS1pretestCodeBased <- eval(parse(text = getObjectRCode(designGS1pretest, stringWrapParagraphWidth = NULL)))
        expect_equal(designGS1pretestCodeBased$alphaSpent, designGS1pretest$alphaSpent, tolerance = 1e-07)
        expect_equal(designGS1pretestCodeBased$criticalValues, designGS1pretest$criticalValues, tolerance = 1e-07)
        expect_equal(designGS1pretestCodeBased$stageLevels, designGS1pretest$stageLevels, tolerance = 1e-07)
        expect_type(names(designGS1pretest), "character")
        df <- as.data.frame(designGS1pretest)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(designGS1pretest)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    designGS1 <- getDesignGroupSequential(
        informationRates = c(0.2, 0.5, 1), sided = 1,
        beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3
    )

    # @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
    # @refFS[Formula]{fs:sampleSizeOneMeanVarianceUnknownOnesided}
    # @refFS[Formula]{fs:sampleSizePerStageOneMean}
    sampleSizeResult <- getSampleSizeMeans(designGS1,
        groups = 1, thetaH0 = 0.5, stDev = 2,
        normalApproximation = FALSE, alternative = 0.8
    )

    ## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 494.6455, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 98.929099, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 247.32275, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 494.6455, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 491.89699, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 462.87248, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 360.24062, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 1.090771, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 0.80583608, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 0.68748891, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH0, sampleSizeResult$expectedNumberOfSubjectsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH01, sampleSizeResult$expectedNumberOfSubjectsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
    # @refFS[Formula]{fs:sampleSizeOneMeanVarianceKnownOnesided}
    # @refFS[Formula]{fs:sampleSizePerStageOneMean}
    sampleSizeResult <- getSampleSizeMeans(designGS1,
        groups = 1, thetaH0 = 0.5, stDev = 2,
        normalApproximation = TRUE, alternative = 0.8
    )

    ## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 492.61495, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 98.522991, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 246.30748, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 492.61495, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 489.87773, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 460.97237, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 358.76182, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 1.0780634, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 0.80438093, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 0.68736844, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH0, sampleSizeResult$expectedNumberOfSubjectsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH01, sampleSizeResult$expectedNumberOfSubjectsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
    # @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceUnknownOnesided}
    # @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
    sampleSizeResult <- getSampleSizeMeans(designGS1,
        groups = 2, thetaH0 = 0, stDev = 2,
        normalApproximation = FALSE, alternative = 1.3, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 107.00299, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 53.501497, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 53.501497, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 21.400599, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 53.501497, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 107.00299, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 106.40843, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 100.12977, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 77.928183, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 2.8110917, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 1.3500437, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 0.81436669, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH0, sampleSizeResult$expectedNumberOfSubjectsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH01, sampleSizeResult$expectedNumberOfSubjectsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
    # @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceKnownOnesided}
    # @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
    sampleSizeResult <- getSampleSizeMeans(designGS1,
        groups = 2, thetaH0 = 0, stDev = 2,
        normalApproximation = TRUE, alternative = 1.3, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 104.93573, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 52.467865, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 52.467865, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 20.987146, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 52.467865, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 104.93573, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 104.35265, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 98.195298, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 76.422636, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 2.5049412, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 1.318984, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 0.81192991, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH0, sampleSizeResult$expectedNumberOfSubjectsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH01, sampleSizeResult$expectedNumberOfSubjectsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
    # @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceUnknownOnesided}
    # @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
    sampleSizeResult <- getSampleSizeMeans(designGS1,
        groups = 2, thetaH0 = 0, stDev = 2,
        normalApproximation = FALSE, alternative = 1.3, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 141.97133, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 106.4785, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 35.492832, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 28.394266, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 70.985664, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 141.97133, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 21.295699, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 53.239248, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 106.4785, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 7.0985664, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 17.746416, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 35.492832, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 141.18246, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 132.85195, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 103.39494, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 2.7228801, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 1.3419598, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 0.81376184, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH0, sampleSizeResult$expectedNumberOfSubjectsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH01, sampleSizeResult$expectedNumberOfSubjectsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
    # @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceKnownOnesided}
    # @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
    sampleSizeResult <- getSampleSizeMeans(designGS1,
        groups = 2, thetaH0 = 0, stDev = 2,
        normalApproximation = TRUE, alternative = 1.3, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 139.91431, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 104.93573, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 34.978577, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 27.982861, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 69.957153, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 139.91431, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 20.987146, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 52.467865, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 104.93573, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 6.9957153, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 17.489288, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 34.978577, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 139.13687, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 130.92706, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 101.89685, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 2.5049412, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 1.318984, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 0.81192991, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH0, sampleSizeResult$expectedNumberOfSubjectsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH01, sampleSizeResult$expectedNumberOfSubjectsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
    # @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceUnknownOnesided}
    # @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
    sampleSizeResult <- getSampleSizeMeans(designGS1,
        groups = 2, thetaH0 = 0.5, stDev = 2,
        normalApproximation = FALSE, alternative = 2.1, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 71.36231, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 35.681155, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 35.681155, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 14.272462, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 35.681155, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 71.36231, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 70.965784, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 66.77843, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 51.971772, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.222748, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.1829515, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.5038177, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH0, sampleSizeResult$expectedNumberOfSubjectsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH01, sampleSizeResult$expectedNumberOfSubjectsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
    # @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceKnownOnesided}
    # @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
    sampleSizeResult <- getSampleSizeMeans(designGS1,
        groups = 2, thetaH0 = 0.5, stDev = 2,
        normalApproximation = TRUE, alternative = 2.1, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 69.273978, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 34.636989, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 34.636989, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 13.854796, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 34.636989, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 69.273978, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 68.889056, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 64.824239, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 50.450881, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 3.5830046, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.123365, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.4992983, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH0, sampleSizeResult$expectedNumberOfSubjectsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH01, sampleSizeResult$expectedNumberOfSubjectsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
    # @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceUnknownOnesided}
    # @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
    sampleSizeResult <- getSampleSizeMeans(designGS1,
        groups = 2, thetaH0 = 0.5, stDev = 2,
        normalApproximation = FALSE, alternative = 2.1, allocationRatioPlanned = 0.4
    )

    ## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 86.937573, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 24.839307, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 62.098267, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 17.387515, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 43.468787, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 86.937573, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 4.9678613, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 12.419653, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 24.839307, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 12.419653, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 31.049133, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 62.098267, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 86.454503, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 81.353233, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 63.314931, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.0734522, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.1712593, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.5029983, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH0, sampleSizeResult$expectedNumberOfSubjectsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH01, sampleSizeResult$expectedNumberOfSubjectsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
    # @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceKnownOnesided}
    # @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
    sampleSizeResult <- getSampleSizeMeans(designGS1,
        groups = 2, thetaH0 = 0.5, stDev = 2,
        normalApproximation = TRUE, alternative = 2.1, allocationRatioPlanned = 0.4
    )

    ## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 84.860623, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 24.245892, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 60.614731, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 16.972125, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 42.430311, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 84.860623, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 4.8491785, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 12.122946, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 24.245892, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 12.122946, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 30.307365, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 60.614731, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 84.389093, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 79.409693, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 61.802329, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 3.5830046, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.123365, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.4992983, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH0, sampleSizeResult$expectedNumberOfSubjectsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH01, sampleSizeResult$expectedNumberOfSubjectsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
    # @refFS[Formula]{fs:sampleSizeTwoMeansRatioVarianceUnknownOnesided}
    # @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
    sampleSizeResult <- getSampleSizeMeans(designGS1,
        groups = 2, meanRatio = TRUE, thetaH0 = 0.9,
        stDev = 3, normalApproximation = FALSE, alternative = 1.9, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 363.14949, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 181.57474, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 181.57474, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 72.629897, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 181.57474, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 363.14949, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 361.13164, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 339.82298, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 264.47466, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 2.8861856, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 1.9212807, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.5251098, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH0, sampleSizeResult$expectedNumberOfSubjectsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH01, sampleSizeResult$expectedNumberOfSubjectsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
    # @refFS[Formula]{fs:sampleSizeTwoMeansRatioVarianceKnownOnesided}
    # @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
    sampleSizeResult <- getSampleSizeMeans(designGS1,
        groups = 2, meanRatio = TRUE, thetaH0 = 0.9,
        stDev = 3, normalApproximation = TRUE, alternative = 1.9, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 361.11139, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 180.5557, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 180.5557, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 72.222278, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 180.5557, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 361.11139, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 359.10487, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 337.9158, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 262.99035, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 2.8268779, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 1.9146031, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.5245615, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH0, sampleSizeResult$expectedNumberOfSubjectsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH01, sampleSizeResult$expectedNumberOfSubjectsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
    # @refFS[Formula]{fs:sampleSizeTwoMeansRatioVarianceUnknownOnesided}
    # @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
    sampleSizeResult <- getSampleSizeMeans(designGS1,
        groups = 2, meanRatio = TRUE, thetaH0 = 0.9,
        stDev = 3, normalApproximation = FALSE, alternative = 1.9, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 458.2463, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 343.68473, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 114.56158, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 91.64926, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 229.12315, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 458.2463, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 68.736945, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 171.84236, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 343.68473, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 22.912315, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 57.280788, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 114.56158, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 455.70005, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 428.81135, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 333.7318, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 2.8732837, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 1.9198713, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.5249957, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH0, sampleSizeResult$expectedNumberOfSubjectsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH01, sampleSizeResult$expectedNumberOfSubjectsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
    # @refFS[Formula]{fs:sampleSizeTwoMeansRatioVarianceKnownOnesided}
    # @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
    sampleSizeResult <- getSampleSizeMeans(designGS1,
        groups = 2, meanRatio = TRUE, thetaH0 = 0.9,
        stDev = 3, normalApproximation = TRUE, alternative = 1.9, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 456.21071, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 342.15803, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 114.05268, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 91.242142, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 228.10535, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 456.21071, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 68.431606, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 171.07902, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 342.15803, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 22.810535, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 57.026339, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 114.05268, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 453.67577, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 426.90651, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 332.24932, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 2.8268779, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 1.9146031, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.5245615, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH0, sampleSizeResult$expectedNumberOfSubjectsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH01, sampleSizeResult$expectedNumberOfSubjectsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
    # @refFS[Formula]{fs:sampleSizeTwoMeansRatioVarianceKnownOnesided}
    # @refFS[Formula]{fs:sampleSizeRatioMeansOptimumAllocationRatio}
    # @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
    sampleSizeResult <- getSampleSizeMeans(designGS1,
        groups = 2, meanRatio = TRUE, thetaH0 = 0.9,
        stDev = 3, normalApproximation = TRUE, alternative = 1.9, allocationRatioPlanned = 0
    )

    ## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$allocationRatioPlanned, 1.1111111, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$allocationRatioPlanned, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 360.11385, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 189.5336, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 170.58024, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 72.022769, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 180.05692, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 360.11385, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 37.906721, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 94.766802, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 189.5336, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 34.116049, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 85.290122, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 170.58024, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 358.11287, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 336.98233, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 262.26386, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 2.8268779, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 1.9146031, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.5245615, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$allocationRatioPlanned, sampleSizeResult$allocationRatioPlanned, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH0, sampleSizeResult$expectedNumberOfSubjectsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH01, sampleSizeResult$expectedNumberOfSubjectsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeMeans': Sample size calculation of testing means for two sided group sequential design", {
    .skipTestIfDisabled()

    # @refFS[Formula]{fs:criticalValuesWangTiatis}
    # @refFS[Formula]{fs:inflationFactor}
    # @refFS[Formula]{fs:expectedReduction}
    designGS2pretest <- getDesignGroupSequential(
        informationRates = c(0.2, 0.5, 1), alpha = 0.4,
        sided = 2, beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3
    )

    ## Comparison of the results of TrialDesignGroupSequential object 'designGS2pretest' with expected results
    expect_equal(designGS2pretest$alphaSpent, c(0.12265406, 0.26238998, 0.39999999), tolerance = 1e-07, label = paste0("c(", paste0(designGS2pretest$alphaSpent, collapse = ", "), ")"))
    expect_equal(designGS2pretest$criticalValues, c(1.5437287, 1.2852363, 1.1188632), tolerance = 1e-07, label = paste0("c(", paste0(designGS2pretest$criticalValues, collapse = ", "), ")"))
    expect_equal(designGS2pretest$stageLevels, c(0.06132703, 0.099354859, 0.13159925), tolerance = 1e-07, label = paste0("c(", paste0(designGS2pretest$stageLevels, collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(designGS2pretest), NA)))
        expect_output(print(designGS2pretest)$show())
        invisible(capture.output(expect_error(summary(designGS2pretest), NA)))
        expect_output(summary(designGS2pretest)$show())
        designGS2pretestCodeBased <- eval(parse(text = getObjectRCode(designGS2pretest, stringWrapParagraphWidth = NULL)))
        expect_equal(designGS2pretestCodeBased$alphaSpent, designGS2pretest$alphaSpent, tolerance = 1e-07)
        expect_equal(designGS2pretestCodeBased$criticalValues, designGS2pretest$criticalValues, tolerance = 1e-07)
        expect_equal(designGS2pretestCodeBased$stageLevels, designGS2pretest$stageLevels, tolerance = 1e-07)
        expect_type(names(designGS2pretest), "character")
        df <- as.data.frame(designGS2pretest)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(designGS2pretest)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    designGS2 <- getDesignGroupSequential(
        informationRates = c(0.2, 0.5, 1), alpha = 0.4,
        sided = 2, beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3
    )

    # @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
    # @refFS[Formula]{fs:sampleSizeOneMeanVarianceUnknownOnesided}
    # @refFS[Formula]{fs:sampleSizePerStageOneMean}
    sampleSizeResult <- getSampleSizeMeans(designGS2,
        groups = 1, thetaH0 = 0.5, stDev = 2,
        normalApproximation = FALSE, alternative = 0.8
    )

    ## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 234.92433, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 46.984866, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 117.46217, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 234.92433, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 195.45911, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 176.81177, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 134.60888, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.041134725, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.26146972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.3536511, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 0.95886527, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 0.73853028, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 0.6463489, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH0, sampleSizeResult$expectedNumberOfSubjectsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH01, sampleSizeResult$expectedNumberOfSubjectsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
    # @refFS[Formula]{fs:sampleSizeOneMeanVarianceKnownOnesided}
    # @refFS[Formula]{fs:sampleSizePerStageOneMean}
    sampleSizeResult <- getSampleSizeMeans(designGS2,
        groups = 1, thetaH0 = 0.5, stDev = 2,
        normalApproximation = TRUE, alternative = 0.8
    )

    ## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 234.50706, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 46.901412, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 117.25353, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 234.50706, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 195.11194, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 176.49772, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 134.36979, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.049174965, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.26261678, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.35387349, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 0.95082503, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 0.73738322, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 0.64612651, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH0, sampleSizeResult$expectedNumberOfSubjectsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH01, sampleSizeResult$expectedNumberOfSubjectsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
    # @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceUnknownOnesided}
    # @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
    sampleSizeResult <- getSampleSizeMeans(designGS2,
        groups = 2, thetaH0 = 0, stDev = 2,
        normalApproximation = FALSE, alternative = 1.3, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 50.39219, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 25.196095, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 25.196095, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 10.078438, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 25.196095, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 50.39219, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 41.926745, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 37.926818, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 28.874132, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], -2.1720469, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], -1.0543228, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], -0.63787834, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 2.1720469, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.0543228, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 0.63787834, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH0, sampleSizeResult$expectedNumberOfSubjectsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH01, sampleSizeResult$expectedNumberOfSubjectsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
    # @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceKnownOnesided}
    # @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
    sampleSizeResult <- getSampleSizeMeans(designGS2,
        groups = 2, thetaH0 = 0, stDev = 2,
        normalApproximation = TRUE, alternative = 1.3, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 49.954167, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 24.977083, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 24.977083, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 9.9908334, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 24.977083, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 49.954167, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 41.562306, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 37.597148, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 28.62315, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], -1.9535752, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], -1.0286606, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], -0.63321489, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 1.9535752, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.0286606, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 0.63321489, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH0, sampleSizeResult$expectedNumberOfSubjectsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH01, sampleSizeResult$expectedNumberOfSubjectsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
    # @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceUnknownOnesided}
    # @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
    sampleSizeResult <- getSampleSizeMeans(designGS2,
        groups = 2, thetaH0 = 0, stDev = 2,
        normalApproximation = FALSE, alternative = 1.3, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 67.037534, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 50.27815, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 16.759383, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 13.407507, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 33.518767, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 67.037534, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 10.05563, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 25.139075, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 50.27815, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 3.3518767, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 8.3796917, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 16.759383, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 55.775818, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 50.454651, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 38.411718, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], -2.1030977, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], -1.0473776, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], -0.63668307, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 2.1030977, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.0473776, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 0.63668307, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH0, sampleSizeResult$expectedNumberOfSubjectsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH01, sampleSizeResult$expectedNumberOfSubjectsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
    # @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceKnownOnesided}
    # @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
    sampleSizeResult <- getSampleSizeMeans(designGS2,
        groups = 2, thetaH0 = 0, stDev = 2,
        normalApproximation = TRUE, alternative = 1.3, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 66.605556, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 49.954167, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 16.651389, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 13.321111, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 33.302778, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 66.605556, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 9.9908334, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 24.977083, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 49.954167, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 3.3302778, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 8.3256945, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 16.651389, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 55.416408, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 50.12953, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 38.164199, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], -1.9535752, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], -1.0286606, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], -0.63321489, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 1.9535752, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.0286606, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 0.63321489, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH0, sampleSizeResult$expectedNumberOfSubjectsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH01, sampleSizeResult$expectedNumberOfSubjectsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_plan_section("Testing the Sample Size Calculation of Testing Rates for Different Designs and Arguments")


test_that("'getSampleSizeRates': Sample size calculation of testing rates for one sided group sequential design", {
    .skipTestIfDisabled()

    designGS1 <- getDesignGroupSequential(informationRates = c(0.2, 0.5, 1), sided = 1, beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3)

    # @refFS[Tab.]{fs:tab:output:getSampleSizeRates}
    # @refFS[Formula]{fs:sampleSizeOneRateExactOnesidedLargerpi1}
    # @refFS[Formula]{fs:sampleSizePerStageOneRate}
    sampleSizeResult <- getSampleSizeRates(designGS1, groups = 1, thetaH0 = 0.5, pi1 = 0.8, normalApproximation = FALSE)

    ## Comparison of the results of TrialDesignPlanRates object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 29.536017, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 5.9072033, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 14.768008, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 29.536017, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 29.371899, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 27.638803, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 21.510502, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 1.090192, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 0.81076728, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 0.6912997, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH0, sampleSizeResult$expectedNumberOfSubjectsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH01, sampleSizeResult$expectedNumberOfSubjectsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeRates}
    # @refFS[Formula]{fs:sampleSizeOneRateExactOnesidedSmallerpi1}
    # @refFS[Formula]{fs:sampleSizePerStageOneRate}
    sampleSizeResult <- getSampleSizeRates(designGS1, groups = 1, thetaH0 = 0.5, pi1 = 0.2, normalApproximation = FALSE)

    ## Comparison of the results of TrialDesignPlanRates object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 29.536017, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 5.9072033, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 14.768008, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 29.536017, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 29.371899, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 27.638803, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 21.510502, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], -0.090191958, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 0.18923272, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 0.3087003, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH0, sampleSizeResult$expectedNumberOfSubjectsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH01, sampleSizeResult$expectedNumberOfSubjectsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeRates}
    # @refFS[Formula]{fs:sampleSizeOneRateApproximation}
    # @refFS[Formula]{fs:sampleSizePerStageOneRate}
    sampleSizeResult <- getSampleSizeRates(designGS1, groups = 1, thetaH0 = 0.5, pi1 = 0.8, normalApproximation = TRUE)

    ## Comparison of the results of TrialDesignPlanRates object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 26.111979, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 5.2223957, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 13.055989, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 26.111979, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 25.966887, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 24.434704, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 19.016842, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 1.127696, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 0.83051514, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 0.70345593, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH0, sampleSizeResult$expectedNumberOfSubjectsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH01, sampleSizeResult$expectedNumberOfSubjectsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeRates}
    # @refFS[Formula]{fs:sampleSizeOneRateExactOnesidedSmallerpi1}
    # @refFS[Formula]{fs:sampleSizePerStageTwoRates}
    sampleSizeResult <- getSampleSizeRates(designGS1, groups = 2, thetaH0 = 0, pi1 = 0.5, pi2 = 0.3, allocationRatioPlanned = 1)

    ## Comparison of the results of TrialDesignPlanRates object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 261.60183, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 130.80091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 130.80091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 52.320365, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 130.80091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 261.60183, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 260.14823, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 244.79812, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 190.51949, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 0.39662162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 0.20482715, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 0.12354802, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH0, sampleSizeResult$expectedNumberOfSubjectsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH01, sampleSizeResult$expectedNumberOfSubjectsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeRates}
    # @refFS[Formula]{fs:sampleSizeTwoRatesDiff}
    # @refFS[Formula]{fs:sampleSizePerStageTwoRates}
    sampleSizeResult <- getSampleSizeRates(designGS1, groups = 2, thetaH0 = 0, pi1 = 0.5, pi2 = 0.3, allocationRatioPlanned = 3)

    ## Comparison of the results of TrialDesignPlanRates object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 349.41307, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 262.0598, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 87.353268, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 69.882614, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 174.70654, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 349.41307, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 52.411961, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 131.0299, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 262.0598, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 17.470654, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 43.676634, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 87.353268, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 347.47155, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 326.9689, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 254.47069, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 0.38949339, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 0.20784714, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 0.12553463, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH0, sampleSizeResult$expectedNumberOfSubjectsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH01, sampleSizeResult$expectedNumberOfSubjectsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeRates}
    # @refFS[Formula]{fs:EstimatesDiffFarringtonManning}
    # @refFS[Formula]{fs:sampleSizeTwoRatesDiff}
    # @refFS[Formula]{fs:sampleSizePerStageTwoRates}
    sampleSizeResult <- getSampleSizeRates(designGS1, groups = 2, thetaH0 = 0.2, pi1 = 0.5, pi2 = 0.1, allocationRatioPlanned = 1)

    ## Comparison of the results of TrialDesignPlanRates object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 201.70565, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 100.85283, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 100.85283, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 40.341131, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 100.85283, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 201.70565, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 200.58487, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 188.74931, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 146.89828, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 0.6326463, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 0.40827798, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 0.32212934, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH0, sampleSizeResult$expectedNumberOfSubjectsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH01, sampleSizeResult$expectedNumberOfSubjectsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeRates}
    # @refFS[Formula]{fs:EstimatesDiffFarringtonManning}
    # @refFS[Formula]{fs:sampleSizeTwoRatesDiff}
    # @refFS[Formula]{fs:sampleSizePerStageTwoRates}
    sampleSizeResult <- getSampleSizeRates(designGS1, groups = 2, thetaH0 = 0.2, pi1 = 0.5, pi2 = 0.1, allocationRatioPlanned = 0.4)

    ## Comparison of the results of TrialDesignPlanRates object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 267.48868, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 76.425337, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 191.06334, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 53.497736, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 133.74434, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 267.48868, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 15.285067, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 38.212668, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 76.425337, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 38.212668, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 95.531671, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 191.06334, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 266.00237, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 250.30683, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 194.80676, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 0.59822838, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 0.40051537, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 0.32119139, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH0, sampleSizeResult$expectedNumberOfSubjectsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH01, sampleSizeResult$expectedNumberOfSubjectsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeRates}
    # @refFS[Formula]{fs:EstimatesDiffFarringtonManning}
    # @refFS[Formula]{fs:sampleSizeTwoRatesDiff}
    # @refFS[Formula]{fs:sampleSizeRatesDiffOptimumAllocationRatio}
    # @refFS[Formula]{fs:sampleSizePerStageTwoRates}
    sampleSizeResult <- getSampleSizeRates(designGS1, groups = 2, thetaH0 = 0.2, pi1 = 0.5, pi2 = 0.1, allocationRatioPlanned = 0)

    ## Comparison of the results of TrialDesignPlanRates object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$allocationRatioPlanned, 1.1669392, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$allocationRatioPlanned, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 200.45189, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 107.94727, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 92.504622, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 40.090378, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 100.22594, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 200.45189, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 21.589453, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 53.973634, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 107.94727, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 18.500924, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 46.252311, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 92.504622, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 199.33807, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 187.57608, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 145.98518, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 0.63834776, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 0.41018483, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 0.32243267, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$allocationRatioPlanned, sampleSizeResult$allocationRatioPlanned, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH0, sampleSizeResult$expectedNumberOfSubjectsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH01, sampleSizeResult$expectedNumberOfSubjectsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeRates}
    # @refFS[Formula]{fs:EstimatesRatioFarringtonManning}
    # @refFS[Formula]{fs:sampleSizeTwoRatesRatio}
    # @refFS[Formula]{fs:sampleSizePerStageTwoRates}
    sampleSizeResult <- getSampleSizeRates(designGS1,
        groups = 2, riskRatio = TRUE, thetaH0 = 0.9,
        pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanRates object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 171.20812, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 85.604059, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 85.604059, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 34.241624, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 85.604059, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 171.20812, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 170.2568, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 160.21075, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 124.68752, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 3.1899424, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.0225352, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.5569402, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH0, sampleSizeResult$expectedNumberOfSubjectsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH01, sampleSizeResult$expectedNumberOfSubjectsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeRates}
    # @refFS[Formula]{fs:EstimatesRatioFarringtonManning}
    # @refFS[Formula]{fs:sampleSizeTwoRatesRatio}
    # @refFS[Formula]{fs:sampleSizePerStageTwoRates}
    sampleSizeResult <- getSampleSizeRates(designGS1,
        groups = 2, riskRatio = TRUE, thetaH0 = 0.9,
        pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanRates object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 221.72371, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 166.29278, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 55.430927, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 44.344741, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 110.86185, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 221.72371, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 33.258556, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 83.14639, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 166.29278, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 11.086185, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 27.715463, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 55.430927, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 220.4917, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 207.48153, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 161.47703, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 3.1917697, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.0740853, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.5843199, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH0, sampleSizeResult$expectedNumberOfSubjectsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH01, sampleSizeResult$expectedNumberOfSubjectsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeRates}
    # @refFS[Formula]{fs:EstimatesRatioFarringtonManning}
    # @refFS[Formula]{fs:sampleSizePerStageTwoRates}
    # @refFS[Formula]{fs:sampleSizeTwoRatesRatioOptimumAllocationRatio}
    sampleSizeResult <- getSampleSizeRates(designGS1,
        groups = 2, riskRatio = TRUE, thetaH0 = 0.9,
        pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 0
    )

    ## Comparison of the results of TrialDesignPlanRates object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$allocationRatioPlanned, 1.0304199, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$allocationRatioPlanned, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 171.17189, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 86.868201, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 84.303693, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 34.234379, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 85.585947, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 171.17189, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 17.37364, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 43.434101, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 86.868201, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 16.860739, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 42.151846, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 84.303693, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 170.22077, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 160.17685, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 124.66114, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 3.1919838, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.0241846, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.5576701, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$allocationRatioPlanned, sampleSizeResult$allocationRatioPlanned, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH0, sampleSizeResult$expectedNumberOfSubjectsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH01, sampleSizeResult$expectedNumberOfSubjectsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeRates': Sample size calculation of testing rates for two sided group sequential design", {
    .skipTestIfDisabled()

    designGS2 <- getDesignGroupSequential(
        informationRates = c(0.2, 0.5, 1), alpha = 0.4,
        sided = 2, beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3
    )

    # @refFS[Tab.]{fs:tab:output:getSampleSizeRates}
    # @refFS[Formula]{fs:sampleSizeOneRateApproximation}
    # @refFS[Formula]{fs:sampleSizePerStageOneRate}
    sampleSizeResult <- getSampleSizeRates(designGS2, groups = 1, thetaH0 = 0.5, pi1 = 0.8, normalApproximation = TRUE)

    ## Comparison of the results of TrialDesignPlanRates object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 11.331566, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 2.2663131, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 5.6657828, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 11.331566, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 9.4279622, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 8.5285086, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 6.4928537, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], -0.01272092, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.23002532, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.33381109, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 1.0127209, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 0.76997468, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 0.66618891, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH0, sampleSizeResult$expectedNumberOfSubjectsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH01, sampleSizeResult$expectedNumberOfSubjectsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeRates}
    # @refFS[Formula]{fs:sampleSizeTwoRatesDiff}
    # @refFS[Formula]{fs:sampleSizePerStageTwoRates}
    sampleSizeResult <- getSampleSizeRates(designGS2, groups = 2, thetaH0 = 0, pi1 = 0.5, pi2 = 0.3, allocationRatioPlanned = 1)

    ## Comparison of the results of TrialDesignPlanRates object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 123.43553, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 61.717765, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 61.717765, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 24.687106, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 61.717765, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 123.43553, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 102.69945, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 92.901636, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 70.727105, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], -0.23899172, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], -0.13791313, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], -0.087906186, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 0.30941892, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 0.15876644, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 0.095938144, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH0, sampleSizeResult$expectedNumberOfSubjectsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH01, sampleSizeResult$expectedNumberOfSubjectsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeRates}
    # @refFS[Formula]{fs:sampleSizeTwoRatesDiff}
    # @refFS[Formula]{fs:sampleSizePerStageTwoRates}
    sampleSizeResult <- getSampleSizeRates(designGS2, groups = 2, thetaH0 = 0, pi1 = 0.5, pi2 = 0.3, allocationRatioPlanned = 3)

    ## Comparison of the results of TrialDesignPlanRates object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 162.30744, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 121.73058, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 40.576859, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 32.461488, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 81.153719, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 162.30744, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 24.346116, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 60.865289, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 121.73058, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 8.1153719, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 20.28843, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 40.576859, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 135.04122, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 122.15791, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 93.000251, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], -0.21587527, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], -0.13203224, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], -0.086052993, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 0.31213587, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 0.16272503, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 0.09811449, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH0, sampleSizeResult$expectedNumberOfSubjectsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH01, sampleSizeResult$expectedNumberOfSubjectsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_plan_section("Testing the Sample Size Calculation of Survival Designs for Different Designs and Arguments")


test_that("'getSampleSizeSurvival': Fixed sample size with minimum required definitions, pi1 = c(0.4, 0.5, 0.6) and pi2 = 0.2 at event time 12, accrual time 12 and follow-up time 6 as default, only alpha = 0.01 is specified", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(alpha = 0.01)

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE, TRUE), label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, c(16.282985, 12, 9.0776496), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, c(0.042568802, 0.057762265, 0.076357561), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, c(2.2892242, 3.1062837, 4.1062837), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, c(58.52451, 31.248898, 20.120262), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, c(16.482222, 7.5670212, 4.2761841), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsFixed, c(58.52451, 31.248898, 20.120262), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed, c(197.78666, 90.804254, 51.314209), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed1, c(98.893329, 45.402127, 25.657105), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed2, c(98.893329, 45.402127, 25.657105), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], c(1.8370942, 2.2986321, 2.821477), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsFixed, sampleSizeResult$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed, sampleSizeResult$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed1, sampleSizeResult$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed2, sampleSizeResult$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': Sample size calculation of survival designs for one sided group sequential design and typeOfComputation = 'Schoenfeld'", {
    .skipTestIfDisabled()

    designGS1 <- getDesignGroupSequential(
        informationRates = c(0.2, 0.5, 1), sided = 1,
        beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3
    )

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        accountForObservationTimes = FALSE,
        typeOfComputation = "Schoenfeld", pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 2
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 218.14225, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 145.42817, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 72.714085, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 72.714085, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 18.178521, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 14.542817, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 36.357042, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 72.714085, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 72.310048, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 68.043375, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 52.956243, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 218.14225, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 145.42817, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 72.714085, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.9325954, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3170793, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6774418, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Schoenfeld",
        pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 250.03082, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 187.52311, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 62.507704, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 86.179656, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 20.835901, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 7.0363139, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 11.430238, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 14.391854, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 17.235931, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 43.089828, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 86.179656, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 85.700797, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 80.644, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 62.762955, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 146.60794, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 238.15931, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 250.03082, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 109.95596, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 178.61948, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 187.52311, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 36.651986, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 59.539826, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 62.507704, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 236.50498, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.9325954, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3170793, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6774418, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        accountForObservationTimes = FALSE,
        typeOfComputation = "Schoenfeld", thetaH0 = 1.2, pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 354.24994, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 177.12497, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 177.12497, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 106.27498, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 29.520829, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 21.254997, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 53.137492, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 106.27498, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 105.68446, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 99.448526, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 77.397988, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 354.24994, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.1656631, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3109184, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.7962847, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        accountForObservationTimes = FALSE,
        typeOfComputation = "Schoenfeld", thetaH0 = 1.2, pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 404.85708, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 303.64281, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 101.21427, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 141.69998, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 33.73809, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 28.339996, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 70.849989, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 141.69998, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 140.91262, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 132.59803, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 103.19732, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 404.85708, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 303.64281, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 101.21427, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.1656631, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3109184, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.7962847, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Schoenfeld",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 216.4138, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 108.2069, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 108.2069, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 64.634742, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 27.051725, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.2267714, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.327532, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 13.872604, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 12.926948, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 32.317371, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 64.634742, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 64.275598, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 60.483, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 47.072216, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 168.44491, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 216.4138, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 216.4138, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 212.39441, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.9325954, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3170793, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6774418, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Schoenfeld",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 247.45413, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 185.5906, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 61.863534, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 86.179656, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 30.931767, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1694167, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.240925, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 13.832344, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 17.235931, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 43.089828, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 86.179656, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 85.700797, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 80.644, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 62.762955, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 190.83096, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 247.45413, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 247.45413, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 143.12322, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 185.5906, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 185.5906, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 47.70774, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 61.863534, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 61.863534, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 242.70959, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.9325954, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3170793, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6774418, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Schoenfeld",
        thetaH0 = 1.2, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 355.83608, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 177.91804, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 177.91804, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 106.27498, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 44.47951, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.2267714, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.327532, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 13.872604, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 21.254997, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 53.137492, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 106.27498, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 105.68446, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 99.448526, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 77.397988, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 276.96374, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 355.83608, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 355.83608, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 349.22724, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.1656631, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3109184, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.7962847, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Schoenfeld",
        thetaH0 = 1.2, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 406.87381, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 305.15536, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 101.71845, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 141.69998, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 50.859227, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1694167, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.240925, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 13.832344, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 28.339996, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 70.849989, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 141.69998, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 140.91262, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 132.59803, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 103.19732, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 313.77176, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 406.87381, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 406.87381, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 235.32882, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 305.15536, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 305.15536, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 78.442941, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 101.71845, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 101.71845, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 399.07264, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.1656631, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3109184, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.7962847, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Schoenfeld",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16, accountForObservationTimes = TRUE, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 224.258, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 112.129, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 112.129, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 64.634742, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 28.03225, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1480342, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.21033, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 13.818027, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 12.926948, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 32.317371, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 64.634742, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 64.275598, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 60.483, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 47.072216, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 172.34323, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 224.258, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 224.258, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 219.90797, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.9325954, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3170793, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6774418, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Schoenfeld",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16, accountForObservationTimes = TRUE, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 257.43359, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 193.07519, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 64.358398, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 86.179656, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 32.179199, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.0820828, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.109775, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 13.771337, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 17.235931, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 43.089828, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 86.179656, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 85.700797, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 80.644, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 62.762955, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 195.71655, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 257.43359, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 257.43359, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 146.78741, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 193.07519, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 193.07519, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 48.929138, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 64.358398, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 64.358398, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 252.26222, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.9325954, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3170793, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6774418, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Schoenfeld",
        thetaH0 = 1.2, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16, accountForObservationTimes = TRUE, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 368.73381, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 184.36691, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 184.36691, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 106.27498, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 46.091727, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1480342, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.21033, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 13.818027, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 21.254997, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 53.137492, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 106.27498, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 105.68446, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 99.448526, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 77.397988, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 283.37351, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 368.73381, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 368.73381, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 361.58134, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.1656631, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3109184, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.7962847, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Schoenfeld",
        thetaH0 = 1.2, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16, accountForObservationTimes = TRUE, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 423.28243, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 317.46182, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 105.82061, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 141.69998, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 52.910303, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.0820828, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.109775, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 13.771337, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 28.339996, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 70.849989, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 141.69998, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 140.91262, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 132.59803, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 103.19732, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 321.80485, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 423.28243, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 423.28243, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 241.35364, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 317.46182, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 317.46182, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 80.451212, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 105.82061, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 105.82061, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 414.77946, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.1656631, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3109184, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.7962847, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    # @refFS[Formula]{fs:sampleSizeSurvivalOptimumAllocationRatio}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Schoenfeld",
        thetaH0 = 1.2, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16, accountForObservationTimes = TRUE, allocationRatioPlanned = 0
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 359.78876, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 206.97289, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 152.81587, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 108.73874, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$allocationRatioPlanned, 1.3543939, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$allocationRatioPlanned, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 44.973595, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1258711, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.176695, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 13.802401, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 21.747749, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 54.369372, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 108.73874, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 108.13454, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 101.75403, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 79.192297, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 275.50245, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 359.78876, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 359.78876, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 158.48615, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 206.97289, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 206.97289, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 117.01629, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 152.81587, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 152.81587, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 352.72627, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.1656631, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3109184, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.7962847, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$allocationRatioPlanned, sampleSizeResult$allocationRatioPlanned, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': sample size calculation of survival designs for one sided group sequential design and typeOfComputation = 'Freedman'", {
    .skipTestIfDisabled()

    designGS1 <- getDesignGroupSequential(
        informationRates = c(0.2, 0.5, 1), sided = 1,
        beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3
    )

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        accountForObservationTimes = FALSE,
        typeOfComputation = "Freedman", pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 240.49104, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 120.24552, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 120.24552, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 72.147312, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 20.04092, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 14.429462, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 36.073656, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 72.147312, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 71.746424, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 67.513008, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 52.543474, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 240.49104, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.52897, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.2152278, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6316611, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        accountForObservationTimes = FALSE,
        typeOfComputation = "Freedman", pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 393.13025, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 294.84769, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 98.282562, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 137.59559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 32.760854, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 27.519117, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 68.797794, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 137.59559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 136.83103, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 128.75728, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 100.20817, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 393.13025, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 294.84769, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 98.282562, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 3.5359417, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 1.9445403, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.5058707, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Freedman",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 241.56782, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 120.78391, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 120.78391, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 72.147312, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 30.195978, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.2267714, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.327532, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 13.872604, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 14.429462, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 36.073656, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 72.147312, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 71.746424, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 67.513008, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 52.543474, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 188.02345, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 241.56782, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 241.56782, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 237.08125, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.52897, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.2152278, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6316611, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Freedman",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 395.08857, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 296.31643, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 98.772142, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 137.59559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 49.386071, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1694167, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.240925, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 13.832344, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 27.519117, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 68.797794, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 137.59559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 136.83103, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 128.75728, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 100.20817, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 304.68325, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 395.08857, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 395.08857, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 228.51244, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 296.31643, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 296.31643, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 76.170813, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 98.772142, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 98.772142, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 387.51336, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 3.5359417, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 1.9445403, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.5058707, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Freedman",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16, accountForObservationTimes = TRUE, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 250.32376, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 125.16188, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 125.16188, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 72.147312, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 31.29047, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1480342, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.21033, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 13.818027, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 14.429462, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 36.073656, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 72.147312, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 71.746424, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 67.513008, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 52.543474, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 192.37488, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 250.32376, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 250.32376, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 245.46813, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.52897, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.2152278, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6316611, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    # @refFS[Formula]{fs:sampleSizeSurvivalOptimumAllocationRatio}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Freedman", thetaH0 = 1,
        pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10, dropoutRate1 = 0.1,
        dropoutRate2 = 0.05, dropoutTime = 16, accountForObservationTimes = TRUE, allocationRatioPlanned = 0
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 236.31869, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 89.479288, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 146.8394, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 62.770758, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$allocationRatioPlanned, 0.60936839, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$allocationRatioPlanned, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 29.539836, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1891498, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.272294, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 13.846839, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 12.554152, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 31.385379, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 62.770758, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 62.421971, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 58.738747, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 45.714713, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 182.82647, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 236.31869, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 236.31869, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 69.22509, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 89.479288, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 89.479288, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 113.60138, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 146.8394, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 146.8394, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 231.83649, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 5.3084847, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.4084373, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.7178517, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$allocationRatioPlanned, sampleSizeResult$allocationRatioPlanned, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': sample size calculation of survival designs for one sided group sequential design and typeOfComputation = 'HsiehFreedman'", {
    .skipTestIfDisabled()

    designGS1 <- getDesignGroupSequential(
        informationRates = c(0.2, 0.5, 1), sided = 1,
        beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3
    )

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        accountForObservationTimes = FALSE,
        typeOfComputation = "HsiehFreedman", pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 240.49104, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 120.24552, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 120.24552, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 72.147312, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 20.04092, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 14.429462, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 36.073656, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 72.147312, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 71.746424, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 67.513008, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 52.543474, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 240.49104, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.52897, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.2152278, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6316611, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        accountForObservationTimes = FALSE,
        typeOfComputation = "HsiehFreedman", pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 274.8469, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 206.13518, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 68.711726, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 96.196416, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 22.903909, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 19.239283, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 48.098208, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 96.196416, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 95.661899, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 90.017344, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 70.057965, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 274.8469, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 206.13518, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 68.711726, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.52897, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.2152278, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6316611, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "HsiehFreedman",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 241.56782, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 120.78391, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 120.78391, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 72.147312, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 30.195978, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.2267714, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.327532, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 13.872604, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 14.429462, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 36.073656, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 72.147312, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 71.746424, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 67.513008, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 52.543474, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 188.02345, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 241.56782, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 241.56782, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 237.08125, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.52897, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.2152278, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6316611, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "HsiehFreedman",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 276.21601, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 207.16201, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 69.054003, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 96.196416, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 34.527001, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1694167, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.240925, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 13.832344, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 19.239283, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 48.098208, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 96.196416, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 95.661899, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 90.017344, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 70.057965, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 213.01146, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 276.21601, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 276.21601, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 159.75859, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 207.16201, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 207.16201, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 53.252865, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 69.054003, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 69.054003, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 270.92, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.52897, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.2152278, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6316611, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "HsiehFreedman",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16, accountForObservationTimes = TRUE,
        allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 250.32376, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 125.16188, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 125.16188, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 72.147312, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 31.29047, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1480342, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.21033, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 13.818027, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 14.429462, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 36.073656, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 72.147312, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 71.746424, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 67.513008, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 52.543474, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 192.37488, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 250.32376, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 250.32376, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 245.46813, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.52897, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.2152278, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6316611, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    # @refFS[Formula]{fs:sampleSizeSurvivalOptimumAllocationRatio}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "HsiehFreedman",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16, accountForObservationTimes = TRUE,
        allocationRatioPlanned = 0
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 244.2512, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 140.50849, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 103.74271, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 73.819895, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$allocationRatioPlanned, 1.3543939, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$allocationRatioPlanned, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 30.5314, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1258711, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.176695, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 13.802401, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 14.763979, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 36.909947, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 73.819895, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 73.409713, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 69.078154, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 53.761583, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 187.03142, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 244.2512, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 244.2512, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 107.59211, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 140.50849, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 140.50849, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 79.439306, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 103.74271, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 103.74271, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 239.45666, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.52897, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.2152278, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6316611, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$allocationRatioPlanned, sampleSizeResult$allocationRatioPlanned, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': sample size calculation of survival data for two sided group sequential design and typeOfComputation = 'Schoenfeld'", {
    .skipTestIfDisabled()

    designGS2 <- getDesignGroupSequential(
        informationRates = c(0.2, 0.5, 1), alpha = 0.4,
        sided = 2, beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3
    )

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        accountForObservationTimes = FALSE,
        typeOfComputation = "Schoenfeld", pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 102.56356, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 51.281781, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 51.281781, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 30.769069, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 8.5469636, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 6.1538138, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 15.384534, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 30.769069, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 25.600136, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 23.157812, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 17.630314, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 102.56356, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.28805692, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.51926225, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.66803619, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.4715361, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.9258092, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.4969249, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        accountForObservationTimes = FALSE,
        typeOfComputation = "Schoenfeld", pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 117.2155, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 87.911625, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 29.303875, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 41.025425, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 9.7679584, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 8.205085, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 20.512713, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 41.025425, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 34.133514, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 30.877083, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 23.507086, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 117.2155, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 87.911625, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 29.303875, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.28805692, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.51926225, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.66803619, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.4715361, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.9258092, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.4969249, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        typeOfComputation = "Schoenfeld",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 103.02279, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 51.511393, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 51.511393, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 30.769069, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 12.877848, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.2267714, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.327532, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 11.672467, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 6.1538138, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 15.384534, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 30.769069, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 25.600136, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 23.157812, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 17.630314, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 80.187417, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 103.02279, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 103.02279, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 96.108996, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.28805692, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.51926225, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.66803619, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.4715361, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.9258092, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.4969249, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        typeOfComputation = "Schoenfeld",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 117.79939, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 88.349544, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 29.449848, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 41.025425, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 14.724924, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1694167, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.240925, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 11.623913, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 8.205085, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 20.512713, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 41.025425, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 34.133514, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 30.877083, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 23.507086, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 90.844192, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 117.79939, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 117.79939, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 68.133144, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 88.349544, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 88.349544, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 22.711048, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 29.449848, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 29.449848, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 109.63825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.28805692, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.51926225, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.66803619, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.4715361, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.9258092, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.4969249, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        typeOfComputation = "Schoenfeld",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16, accountForObservationTimes = TRUE,
        allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 106.75698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 53.378489, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 53.378489, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 30.769069, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 13.344622, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1480342, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.21033, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 11.606421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 6.1538138, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 15.384534, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 30.769069, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 25.600136, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 23.157812, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 17.630314, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 82.043195, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 106.75698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 106.75698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 99.274467, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.28805692, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.51926225, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.66803619, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.4715361, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.9258092, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.4969249, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    # @refFS[Formula]{fs:sampleSizeSurvivalOptimumAllocationRatio}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        typeOfComputation = "Schoenfeld",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16, accountForObservationTimes = TRUE,
        allocationRatioPlanned = 0
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 104.16718, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 59.923444, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 44.243734, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 31.482385, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$allocationRatioPlanned, 1.3543939, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$allocationRatioPlanned, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 13.020897, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1258711, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.176695, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 11.587598, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 6.2964769, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 15.741192, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 31.482385, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 26.193621, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 23.694677, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 18.039036, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 79.764338, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 104.16718, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 104.16718, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 45.885412, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 59.923444, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 59.923444, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 33.878926, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 44.243734, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 44.243734, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 96.778811, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.28805692, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.51926225, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.66803619, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.4715361, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.9258092, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.4969249, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$allocationRatioPlanned, sampleSizeResult$allocationRatioPlanned, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': sample size calculation of survival data for two sided group sequential design and typeOfComputation = 'Freedman'", {
    .skipTestIfDisabled()

    designGS2 <- getDesignGroupSequential(
        informationRates = c(0.2, 0.5, 1), alpha = 0.25,
        sided = 2, beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3
    )

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        typeOfComputation = "Freedman",
        pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 146.14538, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 73.072689, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 73.072689, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 43.244002, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 12.178781, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.22742698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.38942935, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.28314367, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.61685633, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 7.0974672, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 11.495939, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 12.987598, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 8.6488004, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 21.622001, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 43.244002, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 39.334079, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 35.691647, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 26.91074, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 86.438503, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 140.00653, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 146.14538, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 130.17577, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.27467837, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.50642065, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.65781752, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.6406215, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.974643, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.5201784, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.057428091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.11367628, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.16847851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        accountForObservationTimes = FALSE,
        typeOfComputation = "Freedman", pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 235.6363, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 176.72722, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 58.909074, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 82.472703, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 19.636358, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.22742698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.38942935, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.28314367, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.61685633, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 16.494541, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 41.236352, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 82.472703, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 75.015902, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 68.069247, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 51.322759, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 235.6363, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 176.72722, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 58.909074, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.33945377, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.56614959, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.70454917, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 2.9459092, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.7663176, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.4193473, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.057428091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.11367628, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.16847851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        typeOfComputation = "Freedman",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 144.79208, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 72.396041, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 72.396041, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 43.244002, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 18.09901, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.22742698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.38942935, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.28314367, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.61685633, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.2267714, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.327532, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 12.334566, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 8.6488004, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 21.622001, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 43.244002, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 39.334079, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 35.691647, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 26.91074, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 112.6984, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 144.79208, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 144.79208, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 137.49311, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.27467837, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.50642065, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.65781752, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.6406215, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.974643, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.5201784, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.057428091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.11367628, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.16847851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        typeOfComputation = "Freedman",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 236.81008, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 177.60756, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 59.20252, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 82.472703, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 29.60126, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.22742698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.38942935, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.28314367, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.61685633, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1694167, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.240925, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 12.287795, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 16.494541, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 41.236352, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 82.472703, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 75.015902, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 68.069247, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 51.322759, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 182.62251, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 236.81008, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 236.81008, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 136.96688, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 177.60756, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 177.60756, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 45.655628, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 59.20252, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 59.20252, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 224.48637, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.33945377, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.56614959, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.70454917, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 2.9459092, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.7663176, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.4193473, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.057428091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.11367628, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.16847851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        typeOfComputation = "Freedman",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16, accountForObservationTimes = TRUE,
        allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 150.04026, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 75.020128, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 75.020128, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 43.244002, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 18.755032, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.22742698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.38942935, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.28314367, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.61685633, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1480342, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.21033, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 12.271017, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 8.6488004, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 21.622001, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 43.244002, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 39.334079, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 35.691647, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 26.91074, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 115.30658, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 150.04026, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 150.04026, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 142.14088, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.27467837, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.50642065, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.65781752, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.6406215, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.974643, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.5201784, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.057428091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.11367628, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.16847851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    # @refFS[Formula]{fs:sampleSizeSurvivalOptimumAllocationRatio}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        typeOfComputation = "Freedman",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16, accountForObservationTimes = TRUE,
        allocationRatioPlanned = 0
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 141.64583, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 53.632525, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 88.013303, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 37.623838, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$allocationRatioPlanned, 0.60936839, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$allocationRatioPlanned, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 17.705728, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.22742698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.38942935, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.28314367, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.61685633, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1891498, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.272294, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 12.304499, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 7.5247676, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 18.811919, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 37.623838, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 34.222064, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 31.053018, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 23.413313, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 109.58341, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 141.64583, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 141.64583, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 41.492467, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 53.632525, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 53.632525, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 68.09094, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 88.013303, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 88.013303, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 134.35397, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.23978557, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.47145911, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.62947897, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 4.1703928, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 2.1210747, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.5886154, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.057428091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.11367628, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.16847851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$allocationRatioPlanned, sampleSizeResult$allocationRatioPlanned, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
    getDesignCharacteristics(designGS2)
})

test_that("'getSampleSizeSurvival': sample size calculation of survival data for two sided group sequential design and typeOfComputation = 'HsiehFreedman'", {
    .skipTestIfDisabled()

    designGS2 <- getDesignGroupSequential(
        informationRates = c(0.2, 0.5, 1), alpha = 0.25,
        sided = 2, beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3
    )

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        accountForObservationTimes = FALSE,
        typeOfComputation = "HsiehFreedman", pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 144.14667, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 72.073337, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 72.073337, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 43.244002, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 12.012223, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.22742698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.38942935, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.28314367, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.61685633, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 8.6488004, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 21.622001, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 43.244002, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 39.334079, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 35.691647, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 26.91074, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 144.14667, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.27467837, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.50642065, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.65781752, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.6406215, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.974643, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.5201784, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.057428091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.11367628, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.16847851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        typeOfComputation = "HsiehFreedman",
        pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 167.28361, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 125.46271, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 41.820903, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 57.658669, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 13.940301, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.22742698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.38942935, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.28314367, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.61685633, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 7.0363139, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 11.430238, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 12.948104, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 11.531734, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 28.829335, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 57.658669, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 52.445438, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 47.588863, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 35.880987, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 98.088334, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 159.34095, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 167.28361, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 73.56625, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 119.50572, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 125.46271, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 24.522083, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 39.835239, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 41.820903, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 148.45363, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.27467837, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.50642065, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.65781752, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.6406215, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.974643, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.5201784, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.057428091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.11367628, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.16847851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        typeOfComputation = "HsiehFreedman",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 144.79208, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 72.396041, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 72.396041, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 43.244002, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 18.09901, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.22742698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.38942935, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.28314367, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.61685633, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.2267714, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.327532, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 12.334566, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 8.6488004, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 21.622001, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 43.244002, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 39.334079, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 35.691647, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 26.91074, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 112.6984, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 144.79208, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 144.79208, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 137.49311, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.27467837, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.50642065, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.65781752, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.6406215, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.974643, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.5201784, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.057428091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.11367628, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.16847851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        typeOfComputation = "HsiehFreedman",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 165.55968, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 124.16976, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 41.389919, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 57.658669, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 20.69496, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.22742698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.38942935, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.28314367, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.61685633, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1694167, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.240925, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 12.287795, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 11.531734, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 28.829335, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 57.658669, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 52.445438, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 47.588863, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 35.880987, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 127.67583, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 165.55968, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 165.55968, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 95.756873, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 124.16976, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 124.16976, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 31.918958, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 41.389919, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 41.389919, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 156.94387, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.27467837, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.50642065, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.65781752, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.6406215, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.974643, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.5201784, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.057428091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.11367628, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.16847851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        typeOfComputation = "HsiehFreedman",
        thetaH0 = 1, pi1 = c(0.3, 0.4), pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16, accountForObservationTimes = TRUE,
        allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE), label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, c(27.207015, 18.996816), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, c(0.025476782, 0.036487545), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, c(1.5984103, 2.2892242), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, c(634.39599, 172.23645), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, c(475.79699, 129.17734), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, c(158.599, 43.059113), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, c(167.01364, 57.658669), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, c(79.299498, 21.529557), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.22742698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.38942935, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.28314367, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.61685633, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], c(6.2267383, 6.0820828), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], c(10.326085, 10.109775), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], c(18, 18), label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, c(12.333995, 12.216859), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, c(18, 18), label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], c(33.402728, 11.531734), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], c(83.50682, 28.829335), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], c(167.01364, 57.658669), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, c(151.91304, 52.445438), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, c(137.84552, 47.588863), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, c(103.93258, 35.880987), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], c(493.77723, 130.94455), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], c(634.39599, 172.23645), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], c(634.39599, 172.23645), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], c(370.33292, 98.20841), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], c(475.79699, 129.17734), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], c(475.79699, 129.17734), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], c(123.44431, 32.736137), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], c(158.599, 43.059113), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], c(158.599, 43.059113), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, c(602.41549, 162.84556), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], c(0.4680288, 0.27467837), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], c(0.67047266, 0.50642065), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], c(0.78185284, 0.65781752), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], c(2.1366206, 3.6406215), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], c(1.4914851, 1.974643), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], c(1.2790131, 1.5201784), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.057428091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.11367628, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.16847851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        typeOfComputation = "HsiehFreedman",
        thetaH0 = 1, pi1 = 0.3, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16, accountForObservationTimes = TRUE,
        allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 27.207015, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.025476782, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 1.5984103, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 634.39599, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 475.79699, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 158.599, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 167.01364, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 79.299498, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.22742698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.38942935, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.28314367, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.61685633, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.2267383, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.326085, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 12.333995, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 33.402728, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 83.50682, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 167.01364, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 151.91304, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 137.84552, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 103.93258, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 493.77723, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 634.39599, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 634.39599, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 370.33292, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 475.79699, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 475.79699, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 123.44431, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 158.599, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 158.599, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 602.41549, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.4680288, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.67047266, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.78185284, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 2.1366206, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.4914851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.2790131, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.057428091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.11367628, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.16847851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        typeOfComputation = "HsiehFreedman",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16, accountForObservationTimes = TRUE,
        allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 172.23645, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 129.17734, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 43.059113, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 57.658669, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 21.529557, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.22742698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.38942935, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.28314367, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.61685633, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.0820828, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.109775, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 12.216859, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 11.531734, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 28.829335, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 57.658669, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 52.445438, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 47.588863, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 35.880987, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 130.94455, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 172.23645, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 172.23645, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 98.20841, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 129.17734, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 129.17734, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 32.736137, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 43.059113, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 43.059113, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 162.84556, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.27467837, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.50642065, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.65781752, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.6406215, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.974643, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.5201784, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.057428091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.11367628, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.16847851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        maxNumberOfSubjects = 0,
        typeOfComputation = "HsiehFreedman", thetaH0 = 1,
        pi1 = c(0.3, 0.4), pi2 = 0.2, eventTime = 14, accrualTime = 8,
        followUpTime = 10, dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE), label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, c(27.207015, 18.996816), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, c(0.025476782, 0.036487545), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, c(1.5984103, 2.2892242), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, c(634.39599, 172.23645), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, c(475.79699, 129.17734), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, c(158.599, 43.059113), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, c(167.01364, 57.658669), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, c(79.299498, 21.529557), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.22742698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.38942935, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.28314367, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.61685633, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], c(6.2267383, 6.0820828), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], c(10.326085, 10.109775), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], c(18, 18), label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, c(12.333995, 12.216859), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, c(18, 18), label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], c(33.402728, 11.531734), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], c(83.50682, 28.829335), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], c(167.01364, 57.658669), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, c(151.91304, 52.445438), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, c(137.84552, 47.588863), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, c(103.93258, 35.880987), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], c(493.77723, 130.94455), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], c(634.39599, 172.23645), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], c(634.39599, 172.23645), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], c(370.33292, 98.20841), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], c(475.79699, 129.17734), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], c(475.79699, 129.17734), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], c(123.44431, 32.736137), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], c(158.599, 43.059113), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], c(158.599, 43.059113), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, c(602.41549, 162.84556), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], c(0.4680288, 0.27467837), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], c(0.67047266, 0.50642065), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], c(0.78185284, 0.65781752), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], c(2.1366206, 3.6406215), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], c(1.4914851, 1.974643), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], c(1.2790131, 1.5201784), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.057428091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.11367628, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.16847851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    # @refFS[Formula]{fs:sampleSizeSurvivalOptimumAllocationRatio}
    sampleSizeResult <- getSampleSizeSurvival(
        maxNumberOfSubjects = 468, designGS2,
        typeOfComputation = "HsiehFreedman", thetaH0 = 1, pi1 = c(0.3, 0.4), pi2 = 0.2,
        eventTime = 14, accrualTime = 8, dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE), label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, c(27.207015, 18.996816), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, c(0.025476782, 0.036487545), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, c(1.5984103, 2.2892242), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, c(468, 468), label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, c(351, 351), label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, c(117, 117), label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, c(167.01364, 57.658669), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 58.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$followUpTime, c(16.753912, 0.380791), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$followUpTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.22742698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.38942935, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.28314367, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.61685633, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], c(7.2865998, 3.6319976), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], c(12.859517, 5.8243524), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], c(24.753912, 8.380791), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, c(16.149347, 6.305235), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, c(24.753912, 8.380791), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], c(33.402728, 11.531734), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], c(83.50682, 28.829335), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], c(167.01364, 57.658669), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, c(151.91304, 52.445438), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, c(137.84552, 47.588863), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, c(103.93258, 35.880987), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], c(426.26609, 212.47186), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], c(468, 340.72461), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], c(468, 468), label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], c(319.69957, 159.35389), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], c(351, 255.54346), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], c(351, 351), label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], c(106.56652, 53.117965), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], c(117, 85.181153), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], c(117, 117), label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, c(458.50858, 360.32124), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], c(0.4680288, 0.27467837), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], c(0.67047266, 0.50642065), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], c(0.78185284, 0.65781752), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], c(2.1366206, 3.6406215), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], c(1.4914851, 1.974643), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], c(1.2790131, 1.5201784), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.057428091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.11367628, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.16847851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$followUpTime, sampleSizeResult$followUpTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_plan_section("Testing the Sample Size Calculation of Survival Designs for Other Parameter Variants")


test_that("'getSampleSizeSurvival': For fixed sample design, determine necessary accrual time if 200 subjects and 30 subjects per time unit can be recruited", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(
        accrualTime = c(0), accrualIntensity = c(30),
        maxNumberOfSubjects = 120
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE, TRUE), label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, c(16.282985, 12, 9.0776496), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, c(0.042568802, 0.057762265, 0.076357561), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, c(2.2892242, 3.1062837, 4.1062837), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, c(45.770282, 24.438835, 15.735459), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$followUpTime, c(14.350651, 4.1854022, 1.0840261), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$followUpTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsFixed, c(45.770282, 24.438835, 15.735459), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed, c(120, 120, 120), label = paste0("c(", paste0(sampleSizeResult$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed1, c(60, 60, 60), label = paste0("c(", paste0(sampleSizeResult$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed2, c(60, 60, 60), label = paste0("c(", paste0(sampleSizeResult$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], c(18.350651, 8.1854022, 5.0840261), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, c(18.350651, 8.1854022, 5.0840261), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], c(1.7849857, 2.2098739, 2.686355), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$followUpTime, sampleSizeResult$followUpTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsFixed, sampleSizeResult$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed, sampleSizeResult$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed1, sampleSizeResult$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed2, sampleSizeResult$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': Determine necessary accrual time if 200 subjects and if the first 6 time units 20 subjects per time unit can be recruited, then 30 subjects per time unit", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(
        beta = 0.01, accrualTime = c(0, 4), accrualIntensity = c(10, 20),
        maxNumberOfSubjects = 180
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE, TRUE), label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, c(16.282985, 12, 9.0776496), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, c(0.042568802, 0.057762265, 0.076357561), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, c(2.2892242, 3.1062837, 4.1062837), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, c(107.13798, 57.20584, 36.833186), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$totalAccrualTime, 11, label = paste0("c(", paste0(sampleSizeResult$totalAccrualTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$followUpTime, c(27.319035, 6.0447949, 0.58657023), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$followUpTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsFixed, c(107.13798, 57.20584, 36.833186), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed, c(180, 180, 180), label = paste0("c(", paste0(sampleSizeResult$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed1, c(90, 90, 90), label = paste0("c(", paste0(sampleSizeResult$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed2, c(90, 90, 90), label = paste0("c(", paste0(sampleSizeResult$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], c(38.319035, 17.044795, 11.58657), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, c(38.319035, 17.044795, 11.58657), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], c(1.4603989, 1.6791239, 1.9076838), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$totalAccrualTime, sampleSizeResult$totalAccrualTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$followUpTime, sampleSizeResult$followUpTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsFixed, sampleSizeResult$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed, sampleSizeResult$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed1, sampleSizeResult$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed2, sampleSizeResult$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': Determine maximum number of Subjects if the first 6 time units 20 subjects per time unit can be recruited, and after 10 time units 30 subjects per time unit", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(accrualTime = c(0, 3, 5), accrualIntensity = c(20, 30))

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE, TRUE), label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, c(16.282985, 12, 9.0776496), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, c(0.042568802, 0.057762265, 0.076357561), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, c(2.2892242, 3.1062837, 4.1062837), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, c(120, 120, 120), label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, c(45.770282, 24.438835, 15.735459), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$totalAccrualTime, 5, label = paste0("c(", paste0(sampleSizeResult$totalAccrualTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$followUpTime, c(14.113265, 3.9529427, 0.85781252), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$followUpTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsFixed, c(45.770282, 24.438835, 15.735459), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed, c(120, 120, 120), label = paste0("c(", paste0(sampleSizeResult$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed1, c(60, 60, 60), label = paste0("c(", paste0(sampleSizeResult$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed2, c(60, 60, 60), label = paste0("c(", paste0(sampleSizeResult$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], c(19.113265, 8.9529427, 5.8578125), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, c(19.113265, 8.9529427, 5.8578125), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], c(1.7849857, 2.2098739, 2.686355), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$totalAccrualTime, sampleSizeResult$totalAccrualTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$followUpTime, sampleSizeResult$followUpTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsFixed, sampleSizeResult$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed, sampleSizeResult$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed1, sampleSizeResult$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed2, sampleSizeResult$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': Specify accrual time as a list", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    at <- list("0 - <3" = 20, "3 - Inf" = 30)
    sampleSizeResult <- getSampleSizeSurvival(accrualTime = at, maxNumberOfSubjects = 120)

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE, TRUE), label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, c(16.282985, 12, 9.0776496), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, c(0.042568802, 0.057762265, 0.076357561), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, c(2.2892242, 3.1062837, 4.1062837), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, c(45.770282, 24.438835, 15.735459), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$totalAccrualTime, 5, label = paste0("c(", paste0(sampleSizeResult$totalAccrualTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$followUpTime, c(14.113265, 3.9529427, 0.85781252), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$followUpTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsFixed, c(45.770282, 24.438835, 15.735459), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed, c(120, 120, 120), label = paste0("c(", paste0(sampleSizeResult$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed1, c(60, 60, 60), label = paste0("c(", paste0(sampleSizeResult$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed2, c(60, 60, 60), label = paste0("c(", paste0(sampleSizeResult$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], c(19.113265, 8.9529427, 5.8578125), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, c(19.113265, 8.9529427, 5.8578125), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], c(1.7849857, 2.2098739, 2.686355), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$totalAccrualTime, sampleSizeResult$totalAccrualTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$followUpTime, sampleSizeResult$followUpTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsFixed, sampleSizeResult$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed, sampleSizeResult$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed1, sampleSizeResult$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed2, sampleSizeResult$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': Specify accrual time as a list, if maximum number of subjects need to be calculated", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    at <- list("0 - <3" = 20, "3 - <=5" = 30)
    sampleSizeResult <- getSampleSizeSurvival(accrualTime = at)

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE, TRUE), label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, c(16.282985, 12, 9.0776496), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, c(0.042568802, 0.057762265, 0.076357561), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, c(2.2892242, 3.1062837, 4.1062837), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, c(120, 120, 120), label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, c(45.770282, 24.438835, 15.735459), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$totalAccrualTime, 5, label = paste0("c(", paste0(sampleSizeResult$totalAccrualTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$followUpTime, c(14.113265, 3.9529427, 0.85781252), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$followUpTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsFixed, c(45.770282, 24.438835, 15.735459), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed, c(120, 120, 120), label = paste0("c(", paste0(sampleSizeResult$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed1, c(60, 60, 60), label = paste0("c(", paste0(sampleSizeResult$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed2, c(60, 60, 60), label = paste0("c(", paste0(sampleSizeResult$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], c(19.113265, 8.9529427, 5.8578125), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, c(19.113265, 8.9529427, 5.8578125), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], c(1.7849857, 2.2098739, 2.686355), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$totalAccrualTime, sampleSizeResult$totalAccrualTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$followUpTime, sampleSizeResult$followUpTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsFixed, sampleSizeResult$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed, sampleSizeResult$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed1, sampleSizeResult$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed2, sampleSizeResult$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': Effect size is based on event rate at specified event time for the reference group and hazard ratio", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(design = getDesignGroupSequential(kMax = 2), hazardRatio = 0.5, pi2 = 0.3, eventTime = 24)

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$pi1, 0.16333997, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$pi1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 93.281194, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 46.640597, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.007430728, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.014861456, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 532.72433, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 266.36217, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 266.36217, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 65.854457, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 44.393694, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.20960506, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.59039494, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.20960506, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 11.816947, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 16.704001, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 32.927229, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 65.854457, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 65.76941, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 64.676952, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 58.952743, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 524.59793, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 532.72433, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 531.021, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 0.37730742, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 0.61425355, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$pi1, sampleSizeResult$pi1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': Effect size is based on hazard rate for the reference group and hazard ratio", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(design = getDesignGroupSequential(kMax = 2), hazardRatio = 0.5, lambda2 = 0.02)

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 69.314718, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 34.657359, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.01, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 406.47112, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 203.23556, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 203.23556, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 65.854457, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 33.872594, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.20960506, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.59039494, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.20960506, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 11.754955, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 16.691007, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 32.927229, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 65.854457, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 65.76941, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 64.676952, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 58.952743, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 398.17083, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 406.47112, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 404.73134, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 0.37730742, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 0.61425355, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': Specification of piecewise exponential survival time and hazard ratios", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalExponentialPieceWiseAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalGeneralPieceWiseAccrual}
    # @refFS[Formula]{fs:pieceWiseExponentialSurvival}
    sampleSizeResult <- getSampleSizeSurvival(
        design = getDesignGroupSequential(kMax = 2),
        piecewiseSurvivalTime = c(0, 5, 10), lambda2 = c(0.01, 0.02, 0.04), hazardRatio = c(1.5, 1.8, 2)
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE, TRUE), label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, c(762.70199, 333.19844, 227.99548), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, c(381.35099, 166.59922, 113.99774), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, c(381.35099, 166.59922, 113.99774), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, c(192.45497, 91.579156, 65.854457), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, c(63.558499, 27.766537, 18.999624), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.20960506, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.59039494, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.20960506, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], c(13.350554, 13.286013, 13.241069), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], c(18, 18, 18), label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, c(17.025453, 17.011925, 17.002504), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, c(18, 18, 18), label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], c(96.227483, 45.789578, 32.927229), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], c(192.45497, 91.579156, 65.854457), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, c(192.20642, 91.460887, 65.76941), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, c(189.01379, 89.941683, 64.676952), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, c(172.2852, 81.981429, 58.952743), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], c(762.70199, 333.19844, 227.99548), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], c(762.70199, 333.19844, 227.99548), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, c(762.70199, 333.19844, 227.99548), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], c(1.76855, 2.2853938, 2.6503587), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], c(1.3298684, 1.5117519, 1.6279922), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': Specification of piecewise exponential survival time as a list and hazard ratios", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalExponentialPieceWiseAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalGeneralPieceWiseAccrual}
    # @refFS[Formula]{fs:pieceWiseExponentialSurvival}
    pws <- list("0 - <5" = 0.01, "5 - <10" = 0.02, ">=10" = 0.04)
    sampleSizeResult <- getSampleSizeSurvival(
        design = getDesignGroupSequential(kMax = 2),
        piecewiseSurvivalTime = pws, hazardRatio = c(1.5, 1.8, 2)
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE, TRUE), label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, c(762.70199, 333.19844, 227.99548), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, c(381.35099, 166.59922, 113.99774), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, c(381.35099, 166.59922, 113.99774), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, c(192.45497, 91.579156, 65.854457), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, c(63.558499, 27.766537, 18.999624), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.20960506, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.59039494, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.20960506, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], c(13.350554, 13.286013, 13.241069), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], c(18, 18, 18), label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, c(17.025453, 17.011925, 17.002504), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, c(18, 18, 18), label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], c(96.227483, 45.789578, 32.927229), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], c(192.45497, 91.579156, 65.854457), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, c(192.20642, 91.460887, 65.76941), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, c(189.01379, 89.941683, 64.676952), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, c(172.2852, 81.981429, 58.952743), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], c(762.70199, 333.19844, 227.99548), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], c(762.70199, 333.19844, 227.99548), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, c(762.70199, 333.19844, 227.99548), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], c(1.76855, 2.2853938, 2.6503587), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], c(1.3298684, 1.5117519, 1.6279922), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': Specification of piecewise exponential survival time for both treatment arms", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalExponentialPieceWiseAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalGeneralPieceWiseAccrual}
    # @refFS[Formula]{fs:pieceWiseExponentialSurvival}
    sampleSizeResult <- getSampleSizeSurvival(
        design = getDesignGroupSequential(kMax = 2),
        piecewiseSurvivalTime = c(0, 5, 10), lambda2 = c(0.01, 0.02, 0.04), lambda1 = c(0.015, 0.03, 0.06)
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 1.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 762.70199, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 381.35099, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 381.35099, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 192.45497, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 63.558499, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.20960506, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.59039494, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.20960506, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 13.350554, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 17.025453, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 96.227483, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 192.45497, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 192.20642, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 189.01379, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 172.2852, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 762.70199, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 762.70199, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 762.70199, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 1.76855, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 1.3298684, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': Specification of piecewise exponential survival time as a list", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalExponentialPieceWiseAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalGeneralPieceWiseAccrual}
    # @refFS[Formula]{fs:pieceWiseExponentialSurvival}
    pws <- list("0 - <5" = 0.01, "5 - <10" = 0.02, ">=10" = 0.04)
    sampleSizeResult <- getSampleSizeSurvival(
        design = getDesignGroupSequential(kMax = 2),
        piecewiseSurvivalTime = pws, hazardRatio = c(1.5, 1.8, 2)
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE, TRUE), label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, c(762.70199, 333.19844, 227.99548), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, c(381.35099, 166.59922, 113.99774), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, c(381.35099, 166.59922, 113.99774), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, c(192.45497, 91.579156, 65.854457), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, c(63.558499, 27.766537, 18.999624), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.20960506, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.59039494, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.20960506, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], c(13.350554, 13.286013, 13.241069), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], c(18, 18, 18), label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, c(17.025453, 17.011925, 17.002504), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, c(18, 18, 18), label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], c(96.227483, 45.789578, 32.927229), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], c(192.45497, 91.579156, 65.854457), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, c(192.20642, 91.460887, 65.76941), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, c(189.01379, 89.941683, 64.676952), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, c(172.2852, 81.981429, 58.952743), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], c(762.70199, 333.19844, 227.99548), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], c(762.70199, 333.19844, 227.99548), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, c(762.70199, 333.19844, 227.99548), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], c(1.76855, 2.2853938, 2.6503587), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], c(1.3298684, 1.5117519, 1.6279922), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': Specify effect size based on median survival times (median1 = 5, median2 = 3)", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:lambdabymedian}
    sampleSizeResult <- getSampleSizeSurvival(lambda1 = log(2) / 5, lambda2 = log(2) / 3)

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 5, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 3, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 0.6, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 120.3157, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 11.772201, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsFixed, 120.3157, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed, 141.26641, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed1, 70.633206, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed2, 70.633206, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 0.6995143, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-06)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-06)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-06)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-06)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-06)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-06)
        expect_equal(sampleSizeResultCodeBased$eventsFixed, sampleSizeResult$eventsFixed, tolerance = 1e-06)
        expect_equal(sampleSizeResultCodeBased$nFixed, sampleSizeResult$nFixed, tolerance = 1e-06)
        expect_equal(sampleSizeResultCodeBased$nFixed1, sampleSizeResult$nFixed1, tolerance = 1e-06)
        expect_equal(sampleSizeResultCodeBased$nFixed2, sampleSizeResult$nFixed2, tolerance = 1e-06)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-06)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-06)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-06)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:lambdabymedian}
    sampleSizeResult2 <- getSampleSizeSurvival(median1 = 5, median2 = 3)

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult2' with expected results
    expect_equal(sampleSizeResult2$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeResult2$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$lambda1, 0.13862944, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$lambda2, 0.23104906, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$hazardRatio, 0.6, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$maxNumberOfEvents, 120.3157, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$accrualIntensity, 11.772201, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$eventsFixed, 120.3157, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$nFixed, 141.26641, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$nFixed1, 70.633206, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$nFixed2, 70.633206, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$analysisTime[1, ], 18, label = paste0("c(", paste0(sampleSizeResult2$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$studyDuration, 18, label = paste0("c(", paste0(sampleSizeResult2$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$criticalValuesEffectScale[1, ], 0.6995143, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult2), NA)))
        expect_output(print(sampleSizeResult2)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult2), NA)))
        expect_output(summary(sampleSizeResult2)$show())
        sampleSizeResult2CodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult2, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResult2CodeBased$directionUpper, sampleSizeResult2$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$lambda1, sampleSizeResult2$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$lambda2, sampleSizeResult2$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$hazardRatio, sampleSizeResult2$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$maxNumberOfEvents, sampleSizeResult2$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$accrualIntensity, sampleSizeResult2$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$eventsFixed, sampleSizeResult2$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$nFixed, sampleSizeResult2$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$nFixed1, sampleSizeResult2$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$nFixed2, sampleSizeResult2$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$analysisTime, sampleSizeResult2$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$studyDuration, sampleSizeResult2$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$criticalValuesEffectScale, sampleSizeResult2$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult2), "character")
        df <- as.data.frame(sampleSizeResult2)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult2)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:lambdabymedian}
    sampleSizeResult <- getSampleSizeSurvival(
        lambda1 = getLambdaByMedian(median = 5, kappa = 2),
        lambda2 = getLambdaByMedian(median = 3, kappa = 2), kappa = 2
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 5, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 3, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 0.36, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 30.078926, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 2.6040472, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsFixed, 30.078926, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed, 31.248566, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed1, 15.624283, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed2, 15.624283, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 0.48932026, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsFixed, sampleSizeResult$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed, sampleSizeResult$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed1, sampleSizeResult$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed2, sampleSizeResult$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:lambdabymedian}
    sampleSizeResult2 <- getSampleSizeSurvival(median1 = 5, median2 = 3, kappa = 2)

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult2' with expected results
    expect_equal(sampleSizeResult2$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeResult2$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$lambda1, 0.16651092, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$lambda2, 0.2775182, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$hazardRatio, 0.36, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$maxNumberOfEvents, 30.078926, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$accrualIntensity, 2.6040472, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$eventsFixed, 30.078926, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$nFixed, 31.248566, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$nFixed1, 15.624283, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$nFixed2, 15.624283, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$analysisTime[1, ], 18, label = paste0("c(", paste0(sampleSizeResult2$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$studyDuration, 18, label = paste0("c(", paste0(sampleSizeResult2$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$criticalValuesEffectScale[1, ], 0.48932026, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult2), NA)))
        expect_output(print(sampleSizeResult2)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult2), NA)))
        expect_output(summary(sampleSizeResult2)$show())
        sampleSizeResult2CodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult2, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResult2CodeBased$directionUpper, sampleSizeResult2$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$lambda1, sampleSizeResult2$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$lambda2, sampleSizeResult2$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$hazardRatio, sampleSizeResult2$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$maxNumberOfEvents, sampleSizeResult2$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$accrualIntensity, sampleSizeResult2$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$eventsFixed, sampleSizeResult2$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$nFixed, sampleSizeResult2$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$nFixed1, sampleSizeResult2$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$nFixed2, sampleSizeResult2$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$analysisTime, sampleSizeResult2$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$studyDuration, sampleSizeResult2$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$criticalValuesEffectScale, sampleSizeResult2$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult2), "character")
        df <- as.data.frame(sampleSizeResult2)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult2)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': Specify effect size based on rates with kappa = 3", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:lambdabypi}
    sampleSizeResult <- getSampleSizeSurvival(
        lambda1 = (-log(1 - 0.23))^(1 / 3) / 14,
        lambda2 = (-log(1 - 0.38))^(1 / 3) / 14, kappa = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 19.378531, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 15.845881, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 0.54674726, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 86.124472, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 30.926108, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsFixed, 86.124472, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed, 371.1133, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed1, 185.55665, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed2, 185.55665, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 0.65547761, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsFixed, sampleSizeResult$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed, sampleSizeResult$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed1, sampleSizeResult$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed2, sampleSizeResult$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:lambdabypi}
    sampleSizeResult2 <- getSampleSizeSurvival(pi1 = 0.23, pi2 = 0.38, eventTime = 14, kappa = 3)

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult2' with expected results
    expect_equal(sampleSizeResult2$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeResult2$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$median1, 19.378531, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$median2, 15.845881, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$lambda1, 0.045668945, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$lambda2, 0.055850291, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$hazardRatio, 0.54674726, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$maxNumberOfEvents, 86.124472, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$accrualIntensity, 30.926108, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$eventsFixed, 86.124472, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$nFixed, 371.1133, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$nFixed1, 185.55665, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$nFixed2, 185.55665, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$analysisTime[1, ], 18, label = paste0("c(", paste0(sampleSizeResult2$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$studyDuration, 18, label = paste0("c(", paste0(sampleSizeResult2$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$criticalValuesEffectScale[1, ], 0.65547761, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult2), NA)))
        expect_output(print(sampleSizeResult2)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult2), NA)))
        expect_output(summary(sampleSizeResult2)$show())
        sampleSizeResult2CodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult2, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResult2CodeBased$directionUpper, sampleSizeResult2$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$median1, sampleSizeResult2$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$median2, sampleSizeResult2$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$lambda1, sampleSizeResult2$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$lambda2, sampleSizeResult2$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$hazardRatio, sampleSizeResult2$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$maxNumberOfEvents, sampleSizeResult2$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$accrualIntensity, sampleSizeResult2$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$eventsFixed, sampleSizeResult2$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$nFixed, sampleSizeResult2$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$nFixed1, sampleSizeResult2$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$nFixed2, sampleSizeResult2$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$analysisTime, sampleSizeResult2$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$studyDuration, sampleSizeResult2$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$criticalValuesEffectScale, sampleSizeResult2$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult2), "character")
        df <- as.data.frame(sampleSizeResult2)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult2)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': Calculation of maximum number of subjects for given follow-up time", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    sampleSizeResult <- getSampleSizeSurvival(
        accrualTime = c(0, 6), accrualIntensity = c(22, 53),
        lambda2 = 0.02, lambda1 = c(0.01), followUpTime = 5
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 69.314718, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 34.657359, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 477.30924, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 65.345659, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualTime, c(6, 12.515269), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$totalAccrualTime, 12.515269, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$totalAccrualTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsFixed, 65.345659, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed, 477.30924, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed1, 238.65462, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed2, 238.65462, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 17.515269, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 17.515269, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 0.61574672, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualTime, sampleSizeResult$accrualTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$totalAccrualTime, sampleSizeResult$totalAccrualTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsFixed, sampleSizeResult$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed, sampleSizeResult$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed1, sampleSizeResult$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed2, sampleSizeResult$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    sampleSizeResult3 <- getSampleSizeSurvival(
        accrualTime = c(0, 6), accrualIntensity = c(22),
        lambda2 = 0.02, lambda1 = c(0.01)
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult3' with expected results
    expect_equal(sampleSizeResult3$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeResult3$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult3$median1, 69.314718, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult3$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult3$median2, 34.657359, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult3$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult3$hazardRatio, 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult3$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult3$maxNumberOfSubjects, 132, label = paste0("c(", paste0(sampleSizeResult3$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult3$maxNumberOfEvents, 65.345659, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult3$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult3$followUpTime, 44.431065, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult3$followUpTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult3$eventsFixed, 65.345659, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult3$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult3$nFixed, 132, label = paste0("c(", paste0(sampleSizeResult3$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult3$nFixed1, 66, label = paste0("c(", paste0(sampleSizeResult3$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult3$nFixed2, 66, label = paste0("c(", paste0(sampleSizeResult3$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult3$analysisTime[1, ], 50.431065, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult3$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult3$studyDuration, 50.431065, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult3$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult3$criticalValuesEffectScale[1, ], 0.61574672, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult3$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult3), NA)))
        expect_output(print(sampleSizeResult3)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult3), NA)))
        expect_output(summary(sampleSizeResult3)$show())
        sampleSizeResult3CodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult3, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResult3CodeBased$directionUpper, sampleSizeResult3$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResult3CodeBased$median1, sampleSizeResult3$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResult3CodeBased$median2, sampleSizeResult3$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResult3CodeBased$hazardRatio, sampleSizeResult3$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResult3CodeBased$maxNumberOfSubjects, sampleSizeResult3$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResult3CodeBased$maxNumberOfEvents, sampleSizeResult3$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResult3CodeBased$followUpTime, sampleSizeResult3$followUpTime, tolerance = 1e-07)
        expect_equal(sampleSizeResult3CodeBased$eventsFixed, sampleSizeResult3$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResult3CodeBased$nFixed, sampleSizeResult3$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResult3CodeBased$nFixed1, sampleSizeResult3$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeResult3CodeBased$nFixed2, sampleSizeResult3$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeResult3CodeBased$analysisTime, sampleSizeResult3$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResult3CodeBased$studyDuration, sampleSizeResult3$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResult3CodeBased$criticalValuesEffectScale, sampleSizeResult3$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult3), "character")
        df <- as.data.frame(sampleSizeResult3)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult3)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    sampleSizeResult4 <- getSampleSizeSurvival(
        accrualTime = c(0, 6), accrualIntensity = c(22),
        lambda2 = 0.02, lambda1 = c(0.01)
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult4' with expected results
    expect_equal(sampleSizeResult4$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeResult4$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult4$median1, 69.314718, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult4$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult4$median2, 34.657359, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult4$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult4$hazardRatio, 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult4$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult4$maxNumberOfSubjects, 132, label = paste0("c(", paste0(sampleSizeResult4$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult4$maxNumberOfEvents, 65.345659, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult4$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult4$followUpTime, 44.431065, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult4$followUpTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult4$eventsFixed, 65.345659, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult4$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult4$nFixed, 132, label = paste0("c(", paste0(sampleSizeResult4$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult4$nFixed1, 66, label = paste0("c(", paste0(sampleSizeResult4$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult4$nFixed2, 66, label = paste0("c(", paste0(sampleSizeResult4$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult4$analysisTime[1, ], 50.431065, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult4$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult4$studyDuration, 50.431065, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult4$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult4$criticalValuesEffectScale[1, ], 0.61574672, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult4$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult4), NA)))
        expect_output(print(sampleSizeResult4)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult4), NA)))
        expect_output(summary(sampleSizeResult4)$show())
        sampleSizeResult4CodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult4, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResult4CodeBased$directionUpper, sampleSizeResult4$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResult4CodeBased$median1, sampleSizeResult4$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResult4CodeBased$median2, sampleSizeResult4$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResult4CodeBased$hazardRatio, sampleSizeResult4$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResult4CodeBased$maxNumberOfSubjects, sampleSizeResult4$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResult4CodeBased$maxNumberOfEvents, sampleSizeResult4$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResult4CodeBased$followUpTime, sampleSizeResult4$followUpTime, tolerance = 1e-07)
        expect_equal(sampleSizeResult4CodeBased$eventsFixed, sampleSizeResult4$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResult4CodeBased$nFixed, sampleSizeResult4$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResult4CodeBased$nFixed1, sampleSizeResult4$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeResult4CodeBased$nFixed2, sampleSizeResult4$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeResult4CodeBased$analysisTime, sampleSizeResult4$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResult4CodeBased$studyDuration, sampleSizeResult4$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResult4CodeBased$criticalValuesEffectScale, sampleSizeResult4$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult4), "character")
        df <- as.data.frame(sampleSizeResult4)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult4)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    sampleSizeResult6 <- getSampleSizeSurvival(
        accrualTime = c(0), accrualIntensity = c(22),
        lambda2 = 0.02, lambda1 = c(0.01), maxNumberOfSubjects = 300
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult6' with expected results
    expect_equal(sampleSizeResult6$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeResult6$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult6$median1, 69.314718, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult6$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult6$median2, 34.657359, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult6$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult6$hazardRatio, 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult6$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult6$maxNumberOfEvents, 65.345659, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult6$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult6$followUpTime, 9.9154676, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult6$followUpTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult6$eventsFixed, 65.345659, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult6$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult6$nFixed, 300, label = paste0("c(", paste0(sampleSizeResult6$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult6$nFixed1, 150, label = paste0("c(", paste0(sampleSizeResult6$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult6$nFixed2, 150, label = paste0("c(", paste0(sampleSizeResult6$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult6$analysisTime[1, ], 23.551831, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult6$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult6$studyDuration, 23.551831, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult6$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult6$criticalValuesEffectScale[1, ], 0.61574672, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult6$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult6), NA)))
        expect_output(print(sampleSizeResult6)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult6), NA)))
        expect_output(summary(sampleSizeResult6)$show())
        sampleSizeResult6CodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult6, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResult6CodeBased$directionUpper, sampleSizeResult6$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResult6CodeBased$median1, sampleSizeResult6$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResult6CodeBased$median2, sampleSizeResult6$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResult6CodeBased$hazardRatio, sampleSizeResult6$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResult6CodeBased$maxNumberOfEvents, sampleSizeResult6$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResult6CodeBased$followUpTime, sampleSizeResult6$followUpTime, tolerance = 1e-07)
        expect_equal(sampleSizeResult6CodeBased$eventsFixed, sampleSizeResult6$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResult6CodeBased$nFixed, sampleSizeResult6$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResult6CodeBased$nFixed1, sampleSizeResult6$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeResult6CodeBased$nFixed2, sampleSizeResult6$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeResult6CodeBased$analysisTime, sampleSizeResult6$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResult6CodeBased$studyDuration, sampleSizeResult6$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResult6CodeBased$criticalValuesEffectScale, sampleSizeResult6$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult6), "character")
        df <- as.data.frame(sampleSizeResult6)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult6)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    sampleSizeResult7 <- getSampleSizeSurvival(
        accrualTime = c(0, 3), accrualIntensity = c(22, 53),
        lambda2 = 0.02, lambda1 = c(0.01), followUpTime = 44
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult7' with expected results
    expect_equal(sampleSizeResult7$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeResult7$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult7$median1, 69.314718, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult7$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult7$median2, 34.657359, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult7$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult7$hazardRatio, 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult7$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult7$maxNumberOfSubjects, 135.32074, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult7$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult7$maxNumberOfEvents, 65.345659, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult7$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult7$accrualTime, c(3, 4.3079386), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult7$accrualTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult7$totalAccrualTime, 4.3079386, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult7$totalAccrualTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult7$eventsFixed, 65.345659, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult7$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult7$nFixed, 135.32074, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult7$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult7$nFixed1, 67.660372, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult7$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult7$nFixed2, 67.660372, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult7$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult7$analysisTime[1, ], 48.307942, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult7$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult7$studyDuration, 48.307942, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult7$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult7$criticalValuesEffectScale[1, ], 0.61574672, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult7$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult7), NA)))
        expect_output(print(sampleSizeResult7)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult7), NA)))
        expect_output(summary(sampleSizeResult7)$show())
        sampleSizeResult7CodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult7, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResult7CodeBased$directionUpper, sampleSizeResult7$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResult7CodeBased$median1, sampleSizeResult7$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResult7CodeBased$median2, sampleSizeResult7$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResult7CodeBased$hazardRatio, sampleSizeResult7$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResult7CodeBased$maxNumberOfSubjects, sampleSizeResult7$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResult7CodeBased$maxNumberOfEvents, sampleSizeResult7$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResult7CodeBased$accrualTime, sampleSizeResult7$accrualTime, tolerance = 1e-07)
        expect_equal(sampleSizeResult7CodeBased$totalAccrualTime, sampleSizeResult7$totalAccrualTime, tolerance = 1e-07)
        expect_equal(sampleSizeResult7CodeBased$eventsFixed, sampleSizeResult7$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResult7CodeBased$nFixed, sampleSizeResult7$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResult7CodeBased$nFixed1, sampleSizeResult7$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeResult7CodeBased$nFixed2, sampleSizeResult7$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeResult7CodeBased$analysisTime, sampleSizeResult7$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResult7CodeBased$studyDuration, sampleSizeResult7$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResult7CodeBased$criticalValuesEffectScale, sampleSizeResult7$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult7), "character")
        df <- as.data.frame(sampleSizeResult7)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult7)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    sampleSizeResult8 <- getSampleSizeSurvival(
        accrualTime = c(0, 6), accrualIntensity = c(22),
        lambda2 = 0.02, lambda1 = c(0.01)
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult8' with expected results
    expect_equal(sampleSizeResult8$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeResult8$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult8$median1, 69.314718, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult8$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult8$median2, 34.657359, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult8$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult8$hazardRatio, 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult8$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult8$maxNumberOfSubjects, 132, label = paste0("c(", paste0(sampleSizeResult8$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult8$maxNumberOfEvents, 65.345659, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult8$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult8$followUpTime, 44.431065, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult8$followUpTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult8$eventsFixed, 65.345659, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult8$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult8$nFixed, 132, label = paste0("c(", paste0(sampleSizeResult8$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult8$nFixed1, 66, label = paste0("c(", paste0(sampleSizeResult8$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult8$nFixed2, 66, label = paste0("c(", paste0(sampleSizeResult8$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult8$analysisTime[1, ], 50.431065, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult8$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult8$studyDuration, 50.431065, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult8$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult8$criticalValuesEffectScale[1, ], 0.61574672, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult8$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult8), NA)))
        expect_output(print(sampleSizeResult8)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult8), NA)))
        expect_output(summary(sampleSizeResult8)$show())
        sampleSizeResult8CodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult8, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResult8CodeBased$directionUpper, sampleSizeResult8$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResult8CodeBased$median1, sampleSizeResult8$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResult8CodeBased$median2, sampleSizeResult8$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResult8CodeBased$hazardRatio, sampleSizeResult8$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResult8CodeBased$maxNumberOfSubjects, sampleSizeResult8$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResult8CodeBased$maxNumberOfEvents, sampleSizeResult8$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResult8CodeBased$followUpTime, sampleSizeResult8$followUpTime, tolerance = 1e-07)
        expect_equal(sampleSizeResult8CodeBased$eventsFixed, sampleSizeResult8$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResult8CodeBased$nFixed, sampleSizeResult8$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResult8CodeBased$nFixed1, sampleSizeResult8$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeResult8CodeBased$nFixed2, sampleSizeResult8$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeResult8CodeBased$analysisTime, sampleSizeResult8$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResult8CodeBased$studyDuration, sampleSizeResult8$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResult8CodeBased$criticalValuesEffectScale, sampleSizeResult8$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult8), "character")
        df <- as.data.frame(sampleSizeResult8)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult8)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': analysis time at last stage equals accrual time + follow-up time", {
    .skipTestIfDisabled()

    x1 <- getSampleSizeSurvival(getDesignGroupSequential(typeOfDesign = "P"),
        accrualTime = 12, maxNumberOfSubjects = 766,
        pi2 = 0.05, pi1 = 0.1
    )
    expect_equal(x1$analysisTime[3], x1$accrualTime + x1$followUpTime)

    x2 <- getSampleSizeSurvival(getDesignGroupSequential(typeOfDesign = "P"),
        accrualTime = 12, maxNumberOfSubjects = 766,
        lambda2 = 0.005, lambda1 = 0.01
    )

    expect_equal(x2$analysisTime[3], x2$accrualTime + x2$followUpTime)

    x3 <- getSampleSizeSurvival(getDesignGroupSequential(typeOfDesign = "WT", deltaWT = 0),
        accrualTime = c(0, 12, 15), accrualIntensity = c(20, 30),
        lambda2 = 0.005, lambda1 = 0.01
    )

    expect_equal(x3$analysisTime[length(x3$analysisTime)], x3$accrualTime[length(x3$accrualTime)] + x3$followUpTime)

    x4 <- getSampleSizeSurvival(getDesignGroupSequential(typeOfDesign = "WT", deltaWT = 0),
        accrualTime = c(0, 12, 15), accrualIntensity = c(40, 60),
        piecewiseSurvivalTime = c(0, 5), lambda2 = c(0.005, 0.01), hazardRatio = 0.8
    )

    expect_equal(x4$analysisTime[length(x4$analysisTime)], x4$accrualTime[length(x4$accrualTime)] + x4$followUpTime)
})

test_that("'getSampleSizeSurvival': follow-up time is equal for different argument-target constellations", {
    .skipTestIfDisabled()

    designGS1 <- getDesignGroupSequential(
        informationRates = c(0.2, 0.5, 1), sided = 1,
        beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3
    )

    x5 <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Schoenfeld", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2,
        eventTime = 14, accrualTime = 8, followUpTime = 10,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 1
    )
    x6 <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Schoenfeld", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2,
        eventTime = 14, accrualTime = 8, maxNumberOfSubjects = x5$maxNumberOfSubjects,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 1
    )
    expect_equal(x5$followUpTime, x6$followUpTime)

    .skipTestIfDisabled()

    x7 <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Freedman", thetaH0 = 1, median1 = 44, median2 = 66,
        accrualTime = 43, followUpTime = 22,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 2
    )
    x8 <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Freedman", thetaH0 = 1, median1 = 44, median2 = 66,
        accrualTime = 43, maxNumberOfSubjects = x7$maxNumberOfSubjects,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 2
    )
    expect_equal(x7$followUpTime, x8$followUpTime)

    x9 <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "HsiehFreedman", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2,
        eventTime = 16, accrualTime = 8, followUpTime = 10,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 0.2
    )
    x10 <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "HsiehFreedman", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2,
        eventTime = 16, accrualTime = 8, maxNumberOfSubjects = x9$maxNumberOfSubjects,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 0.2
    )
    expect_equal(x9$followUpTime, x10$followUpTime)
})

test_that("'getSampleSizeSurvival': testing expected warnings and errors", {
    .skipTestIfDisabled()

    expect_error(
        getSampleSizeSurvival(
            accrualTime = c(0, 6), accrualIntensity = c(22, 53),
            lambda2 = 0.02, lambda1 = c(0.01, 0.015), followUpTime = 5
        ),
        paste0(
            "Illegal argument: the calculation of 'maxNumberOfSubjects' for given 'followUpTime' ",
            "is only available for a single 'lambda1'; lambda1 = c(0.01, 0.015)"
        ),
        fixed = TRUE
    )

    expect_error(
        getSampleSizeSurvival(
            accrualTime = c(0, 6), accrualIntensity = c(22, 53),
            lambda2 = 0.02, median1 = c(5, 6), followUpTime = 5
        ),
        paste0(
            "Illegal argument: the calculation of 'maxNumberOfSubjects' for given 'followUpTime' ",
            "is only available for a single 'lambda1'; lambda1 = c(0.139, 0.116)"
        ),
        fixed = TRUE
    )

    expect_error(
        getSampleSizeSurvival(
            accrualTime = c(0, 6), accrualIntensity = c(22, 53),
            median2 = 4, median1 = c(5, 6), followUpTime = 5
        ),
        paste0(
            "Illegal argument: the calculation of 'maxNumberOfSubjects' for given 'followUpTime' ",
            "is only available for a single 'median1'; median1 = c(5, 6)"
        ),
        fixed = TRUE
    )

    expect_error(
        getSampleSizeSurvival(
            accrualTime = c(0, 6), accrualIntensity = c(22, 53),
            pi2 = 0.213, pi1 = c(0.113, 0.165), followUpTime = 5
        ),
        paste0(
            "Illegal argument: the calculation of 'maxNumberOfSubjects' for given 'followUpTime' ",
            "is only available for a single 'pi1'; pi1 = c(0.113, 0.165)"
        ),
        fixed = TRUE
    )

    expect_error(
        getSampleSizeSurvival(
            accrualTime = c(0), pi1 = c(0.4, 0.5),
            accrualIntensity = c(22), followUpTime = 6
        ),
        paste0(
            "Illegal argument: the calculation of 'maxNumberOfSubjects' for given 'followUpTime' ",
            "is only available for a single 'pi1'; pi1 = c(0.4, 0.5)"
        ),
        fixed = TRUE
    )

    expect_error(getSampleSizeSurvival(lambda2 = -1, hazardRatio = 2),
        "Argument out of bounds: 'lambda2' (-1) must be >= 0",
        fixed = TRUE
    )

    expect_error(getSampleSizeSurvival(lambda2 = 0, hazardRatio = 2),
        "Illegal argument: 'lambda2' (0) not allowed: at least one lambda value must be > 0",
        fixed = TRUE
    )

    expect_error(getSampleSizeSurvival(lambda2 = 0.9, hazardRatio = 0.8, kappa = 0),
        "Argument out of bounds: 'kappa' (0) must be > 0",
        fixed = TRUE
    )

    expect_error(getSampleSizeSurvival(pi1 = getPiByMedian(0.1), pi2 = getPiByMedian(0.2)))

    expect_warning(getSampleSizeSurvival(median1 = 0.1, median2 = 0.2, eventTime = 0.5),
        "'eventTime' (0.5) will be ignored",
        fixed = TRUE
    )

    expect_error(getSampleSizeSurvival(
        lambda2 = 0.2, hazardRatio = c(0.6, 0.7),
        followUpTime = 8, accrualIntensity = 30, accrualTime = 0
    ))

    expect_error(getSampleSizeSurvival(
        lambda1 = c(0.02, 0.03), lambda2 = 0.2, hazardRatio = 0.6,
        followUpTime = 8, accrualIntensity = 30, accrualTime = 0
    ))

    expect_error(getSampleSizeSurvival(
        lambda2 = c(0.02, 0.03),
        piecewiseSurvivalTime = c(0, 12), hazardRatio = c(0.6, 0.8),
        followUpTime = 8, accrualIntensity = 30, accrualTime = 0
    ))

    expect_warning(getSampleSizeSurvival(median1 = 0.1, median2 = 0.2, eventTime = 4),
        "'eventTime' (4) will be ignored",
        fixed = TRUE
    )

    .skipTestIfDisabled()

    expect_warning(
        getSampleSizeSurvival(
            accrualTime = c(0, 6), accrualIntensity = c(22, 53),
            lambda2 = 0.02, lambda1 = c(0.01), followUpTime = -1
        ),
        "Accrual duration longer than maximal study duration (time to maximal number of events); followUpTime = -1",
        fixed = TRUE
    )

    expect_warning(
        getSampleSizeSurvival(
            accrualTime = c(0, 6, 30), pi1 = 0.4,
            accrualIntensity = c(0.22, 0.53), maxNumberOfSubjects = 1000
        ),
        "Accrual duration longer than maximal study duration (time to maximal number of events); followUpTime = -17.501",
        fixed = TRUE
    )
})

test_plan_section("Testing Other Functions of the Sample Size Calculator for Survival Designs")


test_that("'getEventProbabilities': check expected events over time for overall survival (case 1)", {
    .skipTestIfDisabled()

    design <- getDesignGroupSequential(
        sided = 1, alpha = 0.025, beta = 0.2,
        informationRates = c(0.33, 0.7, 1),
        futilityBounds = c(0, 0),
        bindingFutility = FALSE
    )

    piecewiseSurvivalTime <- list(
        "0 - <6"   = 0.025,
        "6 - <9"   = 0.04,
        "9 - <15"  = 0.015,
        "15 - <21" = 0.01,
        ">=21"     = 0.007
    )

    accrualTime <- list(
        "0  - <12" = 15,
        "12 - <13" = 21,
        "13 - <14" = 27,
        "14 - <15" = 33,
        "15 - <16" = 39,
        ">=16"     = 45
    )

    powerResults <- getPowerSurvival(
        design = design, typeOfComputation = "Schoenfeld",
        thetaH0 = 1, directionUpper = FALSE,
        dropoutRate1 = 0.05, dropoutRate2 = 0.05, dropoutTime = 12,
        allocationRatioPlanned = 1,
        accrualTime = accrualTime,
        piecewiseSurvivalTime = piecewiseSurvivalTime,
        hazardRatio = seq(0.6, 1, 0.05),
        maxNumberOfEvents = 404,
        maxNumberOfSubjects = 1405
    )

    piecewiseSurvivalTimeOS <- list(
        "0  - <14" = 0.015,
        "14 - <24" = 0.01,
        "24 - <44" = 0.005,
        ">=44"     = 0.0025
    )

    timeOS <- c(powerResults$analysisTime[2:3, 4], 17 + 3.5 * 12)
    eventsOS <- getEventProbabilities(
        timeOS,
        accrualTime = accrualTime,
        piecewiseSurvivalTime = piecewiseSurvivalTimeOS, kappa = 1,
        allocationRatioPlanned = 1, hazardRatio = 0.8,
        dropoutRate1 = 0.05, dropoutRate2 = 0.05, dropoutTime = 12,
        maxNumberOfSubjects = 1405
    )$cumulativeEventProbabilities
    eventsOS <- eventsOS * 1405

    expect_equal(round(timeOS, 2), c(37.60, 46.72, 59.00))
    expect_equal(round(eventsOS, 1), c(194.1, 288.7, 365.1))
})

test_that("'getEventProbabilities': check expected events over time for overall survival (case 2)", {
    .skipTestIfDisabled()

    accrualTime <- list(
        "0  - <12" = 15,
        "12 - <13" = 21,
        "13 - <14" = 27,
        "14 - <15" = 33,
        "15 - <16" = 39,
        ">=16"     = 45
    )

    piecewiseSurvivalTimeOS <- list(
        "0  - <14" = 0.015,
        "14 - <24" = 0.01,
        "24 - <44" = 0.005,
        ">=44"     = 0.0025
    )

    timeOS <- c(37.59823, 46.71658, 59)
    eventsOS <- getEventProbabilities(
        timeOS,
        accrualTime = accrualTime,
        piecewiseSurvivalTime = piecewiseSurvivalTimeOS, kappa = 1,
        allocationRatioPlanned = 1, hazardRatio = 0.8,
        dropoutRate1 = 0.05, dropoutRate2 = 0.05, dropoutTime = 12,
        maxNumberOfSubjects = 1405
    )

    # @refFS[Formula]{fs:sampleSizeSurvivalExponentialPieceWiseAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalGeneralPieceWiseAccrual}
    # @refFS[Formula]{fs:pieceWiseExponentialSurvival}
    # @refFS[Formula]{fs:pieceWiseExponentialSurvivalWithDropOuts}

    ## Comparison of the results of EventProbabilities object 'eventsOS' with expected results
    expect_equal(eventsOS$lambda1, c(0.012, 0.008, 0.004, 0.002), tolerance = 1e-07, label = paste0("c(", paste0(eventsOS$lambda1, collapse = ", "), ")"))
    expect_equal(eventsOS$cumulativeEventProbabilities, c(0.13811859, 0.20546928, 0.2598385), tolerance = 1e-07, label = paste0("c(", paste0(eventsOS$cumulativeEventProbabilities, collapse = ", "), ")"))
    expect_equal(eventsOS$eventProbabilities1, c(0.12437783, 0.18544801, 0.23527681), tolerance = 1e-07, label = paste0("c(", paste0(eventsOS$eventProbabilities1, collapse = ", "), ")"))
    expect_equal(eventsOS$eventProbabilities2, c(0.15185935, 0.22549055, 0.28440019), tolerance = 1e-07, label = paste0("c(", paste0(eventsOS$eventProbabilities2, collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(eventsOS), NA)))
        expect_output(print(eventsOS)$show())
        invisible(capture.output(expect_error(summary(eventsOS), NA)))
        expect_output(summary(eventsOS)$show())
        eventsOSCodeBased <- eval(parse(text = getObjectRCode(eventsOS, stringWrapParagraphWidth = NULL)))
        expect_equal(eventsOSCodeBased$lambda1, eventsOS$lambda1, tolerance = 1e-07)
        expect_equal(eventsOSCodeBased$cumulativeEventProbabilities, eventsOS$cumulativeEventProbabilities, tolerance = 1e-07)
        expect_equal(eventsOSCodeBased$eventProbabilities1, eventsOS$eventProbabilities1, tolerance = 1e-07)
        expect_equal(eventsOSCodeBased$eventProbabilities2, eventsOS$eventProbabilities2, tolerance = 1e-07)
        expect_type(names(eventsOS), "character")
        df <- as.data.frame(eventsOS)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(eventsOS)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getNumberOfSubjects': check the number of recruited subjects at given time vector", {
    .skipTestIfDisabled()

    accrualTime1 <- list(
        "0  - <12" = 12,
        "12 - <13" = 21,
        "13 - <14" = 27,
        "14 - <15" = 33,
        "15 - <16" = 39,
        ">=16"     = 45
    )

    numberOfSubjects1 <- getNumberOfSubjects(
        time = 1:3,
        accrualTime = getAccrualTime(accrualTime1, maxNumberOfSubjects = 1405)
    )

    ## Comparison of the results of NumberOfSubjects object 'numberOfSubjects1' with expected results
    expect_equal(numberOfSubjects1$numberOfSubjects, c(12, 24, 36), tolerance = 1e-07, label = paste0("c(", paste0(numberOfSubjects1$numberOfSubjects, collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(numberOfSubjects1), NA)))
        expect_output(print(numberOfSubjects1)$show())
        invisible(capture.output(expect_error(summary(numberOfSubjects1), NA)))
        expect_output(summary(numberOfSubjects1)$show())
        numberOfSubjects1CodeBased <- eval(parse(text = getObjectRCode(numberOfSubjects1, stringWrapParagraphWidth = NULL)))
        expect_equal(numberOfSubjects1CodeBased$numberOfSubjects, numberOfSubjects1$numberOfSubjects, tolerance = 1e-07)
        expect_type(names(numberOfSubjects1), "character")
        df <- as.data.frame(numberOfSubjects1)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(numberOfSubjects1)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    accrualTime2 <- list(
        "0  - <12" = 12,
        "12 - <13" = 21,
        "13 - <14" = 27,
        "14 - <15" = 33,
        "15 - <16" = 39
    )

    # @refFS[Formula]{fs:sampleSizeSurvivalExponentialPieceWiseAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalGeneralPieceWiseAccrual}
    numberOfSubjects2 <- getNumberOfSubjects(time = 1:3, accrualTime = getAccrualTime(accrualTime2))

    ## Comparison of the results of NumberOfSubjects object 'numberOfSubjects2' with expected results
    expect_equal(numberOfSubjects2$maxNumberOfSubjects, 264, label = paste0("c(", paste0(numberOfSubjects2$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(numberOfSubjects2$numberOfSubjects, c(12, 24, 36), label = paste0("c(", paste0(numberOfSubjects2$numberOfSubjects, collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(numberOfSubjects2), NA)))
        expect_output(print(numberOfSubjects2)$show())
        invisible(capture.output(expect_error(summary(numberOfSubjects2), NA)))
        expect_output(summary(numberOfSubjects2)$show())
        numberOfSubjects2CodeBased <- eval(parse(text = getObjectRCode(numberOfSubjects2, stringWrapParagraphWidth = NULL)))
        expect_equal(numberOfSubjects2CodeBased$maxNumberOfSubjects, numberOfSubjects2$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(numberOfSubjects2CodeBased$numberOfSubjects, numberOfSubjects2$numberOfSubjects, tolerance = 1e-07)
        expect_type(names(numberOfSubjects2), "character")
        df <- as.data.frame(numberOfSubjects2)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(numberOfSubjects2)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': check the calculation of 'maxNumberOfSubjects' for given 'followUpTime'", {
    .skipTestIfDisabled()

    sampleSizeSurvival1 <- getSampleSizeSurvival(
        lambda2 = c(0.02, 0.03),
        piecewiseSurvivalTime = c(0, 12), hazardRatio = 0.6,
        followUpTime = 8, accrualIntensity = 30, accrualTime = 0
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeSurvival1' with expected results
    expect_equal(sampleSizeSurvival1$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeSurvival1$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$lambda1, c(0.012, 0.018), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$maxNumberOfSubjects, 484.65038, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$maxNumberOfEvents, 120.3157, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$accrualTime, 16.155013, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$accrualTime, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$eventsFixed, 120.3157, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$nFixed, 484.65038, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$nFixed1, 242.32519, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$nFixed2, 242.32519, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$analysisTime[1, ], 24.155014, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$studyDuration, 24.155014, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$criticalValuesEffectScale[1, ], 0.6995143, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeSurvival1), NA)))
        expect_output(print(sampleSizeSurvival1)$show())
        invisible(capture.output(expect_error(summary(sampleSizeSurvival1), NA)))
        expect_output(summary(sampleSizeSurvival1)$show())
        sampleSizeSurvival1CodeBased <- eval(parse(text = getObjectRCode(sampleSizeSurvival1, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeSurvival1CodeBased$directionUpper, sampleSizeSurvival1$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$lambda1, sampleSizeSurvival1$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$maxNumberOfSubjects, sampleSizeSurvival1$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$maxNumberOfEvents, sampleSizeSurvival1$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$accrualTime, sampleSizeSurvival1$accrualTime, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$eventsFixed, sampleSizeSurvival1$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$nFixed, sampleSizeSurvival1$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$nFixed1, sampleSizeSurvival1$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$nFixed2, sampleSizeSurvival1$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$analysisTime, sampleSizeSurvival1$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$studyDuration, sampleSizeSurvival1$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$criticalValuesEffectScale, sampleSizeSurvival1$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeSurvival1), "character")
        df <- as.data.frame(sampleSizeSurvival1)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeSurvival1)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    sampleSizeSurvival2 <- getSampleSizeSurvival(
        piecewiseSurvivalTime = list(
            "<12"  = 0.02,
            ">=12" = 0.03
        ), hazardRatio = 0.6,
        followUpTime = 8, accrualIntensity = 30, accrualTime = 0
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeSurvival2' with expected results
    expect_equal(sampleSizeSurvival2$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeSurvival2$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$lambda1, c(0.012, 0.018), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$maxNumberOfSubjects, 484.65038, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$maxNumberOfEvents, 120.3157, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$accrualTime, 16.155013, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$accrualTime, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$eventsFixed, 120.3157, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$nFixed, 484.65038, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$nFixed1, 242.32519, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$nFixed2, 242.32519, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$analysisTime[1, ], 24.155014, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$studyDuration, 24.155014, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$criticalValuesEffectScale[1, ], 0.6995143, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeSurvival2), NA)))
        expect_output(print(sampleSizeSurvival2)$show())
        invisible(capture.output(expect_error(summary(sampleSizeSurvival2), NA)))
        expect_output(summary(sampleSizeSurvival2)$show())
        sampleSizeSurvival2CodeBased <- eval(parse(text = getObjectRCode(sampleSizeSurvival2, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeSurvival2CodeBased$directionUpper, sampleSizeSurvival2$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$lambda1, sampleSizeSurvival2$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$maxNumberOfSubjects, sampleSizeSurvival2$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$maxNumberOfEvents, sampleSizeSurvival2$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$accrualTime, sampleSizeSurvival2$accrualTime, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$eventsFixed, sampleSizeSurvival2$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$nFixed, sampleSizeSurvival2$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$nFixed1, sampleSizeSurvival2$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$nFixed2, sampleSizeSurvival2$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$analysisTime, sampleSizeSurvival2$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$studyDuration, sampleSizeSurvival2$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$criticalValuesEffectScale, sampleSizeSurvival2$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeSurvival2), "character")
        df <- as.data.frame(sampleSizeSurvival2)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeSurvival2)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    sampleSizeSurvival3 <- getSampleSizeSurvival(
        lambda2 = c(0.02, 0.03),
        piecewiseSurvivalTime = c(0, 12), hazardRatio = 0.6,
        followUpTime = 8, accrualIntensity = 30, accrualTime = 0
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeSurvival3' with expected results
    expect_equal(sampleSizeSurvival3$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeSurvival3$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$lambda1, c(0.012, 0.018), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$maxNumberOfSubjects, 484.65038, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$maxNumberOfEvents, 120.3157, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$accrualTime, 16.155013, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$accrualTime, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$eventsFixed, 120.3157, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$nFixed, 484.65038, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$nFixed1, 242.32519, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$nFixed2, 242.32519, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$analysisTime[1, ], 24.155014, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$studyDuration, 24.155014, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$criticalValuesEffectScale[1, ], 0.6995143, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeSurvival3), NA)))
        expect_output(print(sampleSizeSurvival3)$show())
        invisible(capture.output(expect_error(summary(sampleSizeSurvival3), NA)))
        expect_output(summary(sampleSizeSurvival3)$show())
        sampleSizeSurvival3CodeBased <- eval(parse(text = getObjectRCode(sampleSizeSurvival3, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeSurvival3CodeBased$directionUpper, sampleSizeSurvival3$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$lambda1, sampleSizeSurvival3$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$maxNumberOfSubjects, sampleSizeSurvival3$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$maxNumberOfEvents, sampleSizeSurvival3$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$accrualTime, sampleSizeSurvival3$accrualTime, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$eventsFixed, sampleSizeSurvival3$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$nFixed, sampleSizeSurvival3$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$nFixed1, sampleSizeSurvival3$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$nFixed2, sampleSizeSurvival3$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$analysisTime, sampleSizeSurvival3$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$studyDuration, sampleSizeSurvival3$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$criticalValuesEffectScale, sampleSizeSurvival3$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeSurvival3), "character")
        df <- as.data.frame(sampleSizeSurvival3)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeSurvival3)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    sampleSizeSurvival4 <- getSampleSizeSurvival(
        lambda2 = c(0.02, 0.03),
        piecewiseSurvivalTime = c(0, 12), hazardRatio = 0.8,
        followUpTime = 8, accrualIntensity = 30, accrualTime = 0
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeSurvival4' with expected results
    expect_equal(sampleSizeSurvival4$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeSurvival4$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$lambda1, c(0.016, 0.024), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$maxNumberOfSubjects, 1325.4661, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$maxNumberOfEvents, 630.52017, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$accrualTime, 44.182203, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$accrualTime, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$eventsFixed, 630.52017, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$nFixed, 1325.4661, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$nFixed1, 662.73305, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$nFixed2, 662.73305, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$analysisTime[1, ], 52.182201, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$studyDuration, 52.182201, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$criticalValuesEffectScale[1, ], 0.85546574, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeSurvival4), NA)))
        expect_output(print(sampleSizeSurvival4)$show())
        invisible(capture.output(expect_error(summary(sampleSizeSurvival4), NA)))
        expect_output(summary(sampleSizeSurvival4)$show())
        sampleSizeSurvival4CodeBased <- eval(parse(text = getObjectRCode(sampleSizeSurvival4, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeSurvival4CodeBased$directionUpper, sampleSizeSurvival4$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$lambda1, sampleSizeSurvival4$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$maxNumberOfSubjects, sampleSizeSurvival4$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$maxNumberOfEvents, sampleSizeSurvival4$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$accrualTime, sampleSizeSurvival4$accrualTime, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$eventsFixed, sampleSizeSurvival4$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$nFixed, sampleSizeSurvival4$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$nFixed1, sampleSizeSurvival4$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$nFixed2, sampleSizeSurvival4$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$analysisTime, sampleSizeSurvival4$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$studyDuration, sampleSizeSurvival4$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$criticalValuesEffectScale, sampleSizeSurvival4$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeSurvival4), "character")
        df <- as.data.frame(sampleSizeSurvival4)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeSurvival4)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    sampleSizeSurvival5 <- getSampleSizeSurvival(
        lambda1 = 0.03, lambda2 = 0.02,
        followUpTime = 8, accrualIntensity = 30, accrualTime = 0
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeSurvival5' with expected results
    expect_equal(sampleSizeSurvival5$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeSurvival5$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival5$median1, 23.104906, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival5$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival5$median2, 34.657359, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival5$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival5$hazardRatio, 1.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival5$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival5$maxNumberOfSubjects, 557.38443, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival5$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival5$maxNumberOfEvents, 190.96804, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival5$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival5$accrualTime, 18.579481, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival5$accrualTime, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival5$eventsFixed, 190.96804, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival5$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival5$nFixed, 557.38443, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival5$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival5$nFixed1, 278.69222, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival5$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival5$nFixed2, 278.69222, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival5$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival5$analysisTime[1, ], 26.579477, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival5$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival5$studyDuration, 26.579477, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival5$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival5$criticalValuesEffectScale[1, ], 1.327981, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival5$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeSurvival5), NA)))
        expect_output(print(sampleSizeSurvival5)$show())
        invisible(capture.output(expect_error(summary(sampleSizeSurvival5), NA)))
        expect_output(summary(sampleSizeSurvival5)$show())
        sampleSizeSurvival5CodeBased <- eval(parse(text = getObjectRCode(sampleSizeSurvival5, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeSurvival5CodeBased$directionUpper, sampleSizeSurvival5$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival5CodeBased$median1, sampleSizeSurvival5$median1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival5CodeBased$median2, sampleSizeSurvival5$median2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival5CodeBased$hazardRatio, sampleSizeSurvival5$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival5CodeBased$maxNumberOfSubjects, sampleSizeSurvival5$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival5CodeBased$maxNumberOfEvents, sampleSizeSurvival5$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival5CodeBased$accrualTime, sampleSizeSurvival5$accrualTime, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival5CodeBased$eventsFixed, sampleSizeSurvival5$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival5CodeBased$nFixed, sampleSizeSurvival5$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival5CodeBased$nFixed1, sampleSizeSurvival5$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival5CodeBased$nFixed2, sampleSizeSurvival5$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival5CodeBased$analysisTime, sampleSizeSurvival5$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival5CodeBased$studyDuration, sampleSizeSurvival5$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival5CodeBased$criticalValuesEffectScale, sampleSizeSurvival5$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeSurvival5), "character")
        df <- as.data.frame(sampleSizeSurvival5)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeSurvival5)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': check calculations for fixed design with relative accrual intensity", {
    .skipTestIfDisabled()

    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    sampleSizeSurvival1 <- getSampleSizeSurvival(accrualIntensity = 0.1, accrualTime = 10)

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeSurvival1' with expected results
    expect_equal(sampleSizeSurvival1$directionUpper, c(TRUE, TRUE, TRUE), label = paste0("c(", paste0(sampleSizeSurvival1$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$median1, c(16.282985, 12, 9.0776496), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$lambda1, c(0.042568802, 0.057762265, 0.076357561), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$hazardRatio, c(2.2892242, 3.1062837, 4.1062837), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$maxNumberOfEvents, c(45.770282, 24.438835, 15.735459), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$accrualIntensity, c(16.554072, 7.5582097, 4.2441939), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$eventsFixed, c(45.770282, 24.438835, 15.735459), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$nFixed, c(165.54072, 75.582097, 42.441939), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$nFixed1, c(82.77036, 37.791048, 21.220969), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$nFixed2, c(82.77036, 37.791048, 21.220969), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$analysisTime[1, ], 16, label = paste0("c(", paste0(sampleSizeSurvival1$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$studyDuration, 16, label = paste0("c(", paste0(sampleSizeSurvival1$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$criticalValuesEffectScale[1, ], c(1.7849857, 2.2098739, 2.686355), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeSurvival1), NA)))
        expect_output(print(sampleSizeSurvival1)$show())
        invisible(capture.output(expect_error(summary(sampleSizeSurvival1), NA)))
        expect_output(summary(sampleSizeSurvival1)$show())
        sampleSizeSurvival1CodeBased <- eval(parse(text = getObjectRCode(sampleSizeSurvival1, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeSurvival1CodeBased$directionUpper, sampleSizeSurvival1$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$median1, sampleSizeSurvival1$median1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$median2, sampleSizeSurvival1$median2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$lambda1, sampleSizeSurvival1$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$lambda2, sampleSizeSurvival1$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$hazardRatio, sampleSizeSurvival1$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$maxNumberOfEvents, sampleSizeSurvival1$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$accrualIntensity, sampleSizeSurvival1$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$eventsFixed, sampleSizeSurvival1$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$nFixed, sampleSizeSurvival1$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$nFixed1, sampleSizeSurvival1$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$nFixed2, sampleSizeSurvival1$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$analysisTime, sampleSizeSurvival1$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$studyDuration, sampleSizeSurvival1$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$criticalValuesEffectScale, sampleSizeSurvival1$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeSurvival1), "character")
        df <- as.data.frame(sampleSizeSurvival1)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeSurvival1)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    sampleSizeSurvival2 <- getSampleSizeSurvival(accrualIntensity = 0.99, accrualTime = c(0, 10))

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeSurvival2' with expected results
    expect_equal(sampleSizeSurvival2$directionUpper, c(TRUE, TRUE, TRUE), label = paste0("c(", paste0(sampleSizeSurvival2$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$median1, c(16.282985, 12, 9.0776496), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$lambda1, c(0.042568802, 0.057762265, 0.076357561), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$hazardRatio, c(2.2892242, 3.1062837, 4.1062837), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$maxNumberOfEvents, c(45.770282, 24.438835, 15.735459), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$accrualIntensity, c(16.554072, 7.5582097, 4.2441939), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$eventsFixed, c(45.770282, 24.438835, 15.735459), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$nFixed, c(165.54072, 75.582097, 42.441939), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$nFixed1, c(82.77036, 37.791048, 21.220969), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$nFixed2, c(82.77036, 37.791048, 21.220969), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$analysisTime[1, ], 16, label = paste0("c(", paste0(sampleSizeSurvival2$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$studyDuration, 16, label = paste0("c(", paste0(sampleSizeSurvival2$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$criticalValuesEffectScale[1, ], c(1.7849857, 2.2098739, 2.686355), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeSurvival2), NA)))
        expect_output(print(sampleSizeSurvival2)$show())
        invisible(capture.output(expect_error(summary(sampleSizeSurvival2), NA)))
        expect_output(summary(sampleSizeSurvival2)$show())
        sampleSizeSurvival2CodeBased <- eval(parse(text = getObjectRCode(sampleSizeSurvival2, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeSurvival2CodeBased$directionUpper, sampleSizeSurvival2$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$median1, sampleSizeSurvival2$median1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$median2, sampleSizeSurvival2$median2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$lambda1, sampleSizeSurvival2$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$lambda2, sampleSizeSurvival2$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$hazardRatio, sampleSizeSurvival2$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$maxNumberOfEvents, sampleSizeSurvival2$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$accrualIntensity, sampleSizeSurvival2$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$eventsFixed, sampleSizeSurvival2$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$nFixed, sampleSizeSurvival2$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$nFixed1, sampleSizeSurvival2$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$nFixed2, sampleSizeSurvival2$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$analysisTime, sampleSizeSurvival2$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$studyDuration, sampleSizeSurvival2$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$criticalValuesEffectScale, sampleSizeSurvival2$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeSurvival2), "character")
        df <- as.data.frame(sampleSizeSurvival2)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeSurvival2)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    sampleSizeSurvival3 <- getSampleSizeSurvival(accrualIntensity = 1e-12, accrualTime = c(0, 10))

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeSurvival3' with expected results
    expect_equal(sampleSizeSurvival3$directionUpper, c(TRUE, TRUE, TRUE), label = paste0("c(", paste0(sampleSizeSurvival3$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$median1, c(16.282985, 12, 9.0776496), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$lambda1, c(0.042568802, 0.057762265, 0.076357561), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$hazardRatio, c(2.2892242, 3.1062837, 4.1062837), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$maxNumberOfEvents, c(45.770282, 24.438835, 15.735459), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$accrualIntensity, c(16.554072, 7.5582097, 4.2441939), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$eventsFixed, c(45.770282, 24.438835, 15.735459), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$nFixed, c(165.54072, 75.582097, 42.441939), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$nFixed1, c(82.77036, 37.791048, 21.220969), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$nFixed2, c(82.77036, 37.791048, 21.220969), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$analysisTime[1, ], 16, label = paste0("c(", paste0(sampleSizeSurvival3$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$studyDuration, 16, label = paste0("c(", paste0(sampleSizeSurvival3$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$criticalValuesEffectScale[1, ], c(1.7849857, 2.2098739, 2.686355), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeSurvival3), NA)))
        expect_output(print(sampleSizeSurvival3)$show())
        invisible(capture.output(expect_error(summary(sampleSizeSurvival3), NA)))
        expect_output(summary(sampleSizeSurvival3)$show())
        sampleSizeSurvival3CodeBased <- eval(parse(text = getObjectRCode(sampleSizeSurvival3, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeSurvival3CodeBased$directionUpper, sampleSizeSurvival3$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$median1, sampleSizeSurvival3$median1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$median2, sampleSizeSurvival3$median2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$lambda1, sampleSizeSurvival3$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$lambda2, sampleSizeSurvival3$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$hazardRatio, sampleSizeSurvival3$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$maxNumberOfEvents, sampleSizeSurvival3$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$accrualIntensity, sampleSizeSurvival3$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$eventsFixed, sampleSizeSurvival3$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$nFixed, sampleSizeSurvival3$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$nFixed1, sampleSizeSurvival3$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$nFixed2, sampleSizeSurvival3$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$analysisTime, sampleSizeSurvival3$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$studyDuration, sampleSizeSurvival3$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$criticalValuesEffectScale, sampleSizeSurvival3$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeSurvival3), "character")
        df <- as.data.frame(sampleSizeSurvival3)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeSurvival3)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    expect_equal(sampleSizeSurvival1$accrualIntensity, sampleSizeSurvival2$accrualIntensity)
    expect_equal(sampleSizeSurvival1$accrualIntensity, sampleSizeSurvival3$accrualIntensity)

    sampleSizeSurvival4 <- getSampleSizeSurvival(accrualIntensity = 1, accrualTime = c(0, 50), pi1 = 0.4)

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeSurvival4' with expected results
    expect_equal(sampleSizeSurvival4$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeSurvival4$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$median1, 16.282985, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$lambda1, 0.042568802, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$maxNumberOfSubjects, 50, label = paste0("c(", paste0(sampleSizeSurvival4$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$maxNumberOfEvents, 45.770282, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$followUpTime, 77.550073, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$followUpTime, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$eventsFixed, 45.770282, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$nFixed, 50, label = paste0("c(", paste0(sampleSizeSurvival4$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$nFixed1, 25, label = paste0("c(", paste0(sampleSizeSurvival4$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$nFixed2, 25, label = paste0("c(", paste0(sampleSizeSurvival4$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$analysisTime[1, ], 127.55007, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$studyDuration, 127.55007, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$criticalValuesEffectScale[1, ], 1.7849857, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeSurvival4), NA)))
        expect_output(print(sampleSizeSurvival4)$show())
        invisible(capture.output(expect_error(summary(sampleSizeSurvival4), NA)))
        expect_output(summary(sampleSizeSurvival4)$show())
        sampleSizeSurvival4CodeBased <- eval(parse(text = getObjectRCode(sampleSizeSurvival4, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeSurvival4CodeBased$directionUpper, sampleSizeSurvival4$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$median1, sampleSizeSurvival4$median1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$median2, sampleSizeSurvival4$median2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$lambda1, sampleSizeSurvival4$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$lambda2, sampleSizeSurvival4$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$hazardRatio, sampleSizeSurvival4$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$maxNumberOfSubjects, sampleSizeSurvival4$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$maxNumberOfEvents, sampleSizeSurvival4$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$followUpTime, sampleSizeSurvival4$followUpTime, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$eventsFixed, sampleSizeSurvival4$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$nFixed, sampleSizeSurvival4$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$nFixed1, sampleSizeSurvival4$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$nFixed2, sampleSizeSurvival4$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$analysisTime, sampleSizeSurvival4$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$studyDuration, sampleSizeSurvival4$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$criticalValuesEffectScale, sampleSizeSurvival4$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeSurvival4), "character")
        df <- as.data.frame(sampleSizeSurvival4)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeSurvival4)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'.getLambdaStepFunctionByTime': return correct lambda for specified time and piecewise exponential bounds", {
    .skipTestIfDisabled()

    # @refFS[Formula]{fs:pieceWiseExponentialSurvival}
    # @refFS[Formula]{fs:pieceWiseExponentialRandomVariable}
    lambda1 <- .getLambdaStepFunctionByTime(time = 1, c(6, 9, 15, 21), c(0.025, 0.04, 0.015, 0.01, 0.007))

    ## Comparison of the results of numeric object 'lambda1' with expected results
    expect_equal(lambda1, 0.025, tolerance = 1e-07, label = paste0("c(", paste0(lambda1, collapse = ", "), ")"))

    # @refFS[Formula]{fs:pieceWiseExponentialSurvival}
    # @refFS[Formula]{fs:pieceWiseExponentialRandomVariable}
    lambda2 <- .getLambdaStepFunctionByTime(time = 6, c(6, 9, 15, 21), c(0.025, 0.04, 0.015, 0.01, 0.007))

    ## Comparison of the results of numeric object 'lambda2' with expected results
    expect_equal(lambda2, 0.025, tolerance = 1e-07, label = paste0("c(", paste0(lambda2, collapse = ", "), ")"))

    # @refFS[Formula]{fs:pieceWiseExponentialSurvival}
    # @refFS[Formula]{fs:pieceWiseExponentialRandomVariable}
    lambda3 <- .getLambdaStepFunctionByTime(time = 7, c(6, 9, 15, 21), c(0.025, 0.04, 0.015, 0.01, 0.007))

    ## Comparison of the results of numeric object 'lambda3' with expected results
    expect_equal(lambda3, 0.04, tolerance = 1e-07, label = paste0("c(", paste0(lambda3, collapse = ", "), ")"))

    # @refFS[Formula]{fs:pieceWiseExponentialSurvival}
    # @refFS[Formula]{fs:pieceWiseExponentialRandomVariable}
    lambda4 <- .getLambdaStepFunctionByTime(time = 9, c(6, 9, 15, 21), c(0.025, 0.04, 0.015, 0.01, 0.007))

    ## Comparison of the results of numeric object 'lambda4' with expected results
    expect_equal(lambda4, 0.04, tolerance = 1e-07, label = paste0("c(", paste0(lambda4, collapse = ", "), ")"))

    # @refFS[Formula]{fs:pieceWiseExponentialSurvival}
    # @refFS[Formula]{fs:pieceWiseExponentialRandomVariable}
    lambda5 <- .getLambdaStepFunctionByTime(time = 14, c(6, 9, 15, 21), c(0.025, 0.04, 0.015, 0.01, 0.007))

    ## Comparison of the results of numeric object 'lambda5' with expected results
    expect_equal(lambda5, 0.015, tolerance = 1e-07, label = paste0("c(", paste0(lambda5, collapse = ", "), ")"))

    # @refFS[Formula]{fs:pieceWiseExponentialSurvival}
    # @refFS[Formula]{fs:pieceWiseExponentialRandomVariable}
    lambda6 <- .getLambdaStepFunctionByTime(time = 15, c(6, 9, 15, 21), c(0.025, 0.04, 0.015, 0.01, 0.007))

    ## Comparison of the results of numeric object 'lambda6' with expected results
    expect_equal(lambda6, 0.015, tolerance = 1e-07, label = paste0("c(", paste0(lambda6, collapse = ", "), ")"))

    # @refFS[Formula]{fs:pieceWiseExponentialSurvival}
    # @refFS[Formula]{fs:pieceWiseExponentialRandomVariable}
    lambda7 <- .getLambdaStepFunctionByTime(time = 16, c(6, 9, 15, 21), c(0.025, 0.04, 0.015, 0.01, 0.007))

    ## Comparison of the results of numeric object 'lambda7' with expected results
    expect_equal(lambda7, 0.01, tolerance = 1e-07, label = paste0("c(", paste0(lambda7, collapse = ", "), ")"))

    # @refFS[Formula]{fs:pieceWiseExponentialSurvival}
    # @refFS[Formula]{fs:pieceWiseExponentialRandomVariable}
    lambda8 <- .getLambdaStepFunctionByTime(time = 21, c(6, 9, 15, 21), c(0.025, 0.04, 0.015, 0.01, 0.007))

    ## Comparison of the results of numeric object 'lambda8' with expected results
    expect_equal(lambda8, 0.01, tolerance = 1e-07, label = paste0("c(", paste0(lambda8, collapse = ", "), ")"))

    # @refFS[Formula]{fs:pieceWiseExponentialSurvival}
    # @refFS[Formula]{fs:pieceWiseExponentialRandomVariable}
    lambda9 <- rpact:::.getLambdaStepFunctionByTime(time = 50, c(6, 9, 15, 21), c(0.025, 0.04, 0.015, 0.01, 0.007))

    ## Comparison of the results of numeric object 'lambda9' with expected results
    expect_equal(lambda9, 0.007, tolerance = 1e-07, label = paste0("c(", paste0(lambda9, collapse = ", "), ")"))
})
