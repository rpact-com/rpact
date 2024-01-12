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
## |  File name: test-f_design_plan_rates.R
## |  Creation date: 21 December 2023, 08:52:47
## |  File version: $Revision: 7554 $
## |  Last changed: $Date: 2024-01-12 10:19:05 +0100 (Fr, 12 Jan 2024) $
## |  Last changed by: $Author: pahlke $
## |

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
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 29.536017, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 5.9072033, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 14.768008, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 29.536017, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
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
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
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
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 29.536017, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 5.9072033, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 14.768008, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 29.536017, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
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
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
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
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 26.111979, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 5.2223957, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 13.055989, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 26.111979, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
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
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
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
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 261.60183, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 130.80091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 130.80091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 52.320365, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 130.80091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 261.60183, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
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
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
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
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
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
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
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
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 201.70565, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 100.85283, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 100.85283, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 40.341131, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 100.85283, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 201.70565, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
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
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
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
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
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
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
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
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
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
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
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
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 171.20812, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 85.604059, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 85.604059, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 34.241624, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 85.604059, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 171.20812, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
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
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
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
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
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
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
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
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
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
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
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
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 11.331566, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 2.2663131, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 5.6657828, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 11.331566, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
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
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
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
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 123.43553, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 61.717765, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 61.717765, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 24.687106, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 61.717765, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 123.43553, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
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
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
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
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
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
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
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

test_plan_section("Testing the Power Calculation of Testing Rates for Different Designs and Arguments")


test_that("'getPowerRates': Power calculation of rate in one sample for one-sided group sequential design", {
    .skipTestIfDisabled()

    designGS1 <- getDesignGroupSequential(
        informationRates = c(0.3, 0.7, 1), sided = 1, alpha = 0.07,
        beta = 0.1, futilityBounds = c(-0.5, 0.5), typeOfDesign = "WT", deltaWT = 0.22
    )

    # @refFS[Tab.]{fs:tab:output:getSampleSizeRates}
    # @refFS[Formula]{fs:PowerGroupSequentialOneSided}
    # @refFS[Formula]{fs:AdjShiftParameterOneSampleRate}
    powerResult <- getPowerRates(designGS1,
        groups = 1, thetaH0 = 0.4,
        pi1 = c(0.2, 0.3, 0.4), directionUpper = FALSE, maxNumberOfSubjects = 40
    )

    ## Comparison of the results of TrialDesignPlanRates object 'powerResult' with expected results
    expect_equal(powerResult$effect, c(-0.2, -0.1, 0), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$effect, collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[1, ], 12, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[2, ], 28, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[3, ], 40, label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, c(0.8850078, 0.38742607, 0.067448723), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[1, ], c(0.23143452, 0.056551742, 0.011170644), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[2, ], c(0.48990786, 0.18729986, 0.030436001), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[3, ], c(0.16366541, 0.14357447, 0.025842077), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityStop, c(0.043768704, 0.31327331, 0.71047424), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityStop, collapse = ", "), ")"))
    expect_equal(powerResult$futilityPerStage[1, ], c(0.020163481, 0.11504671, 0.30853754), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityPerStage[2, ], c(0.023605223, 0.1982266, 0.40193671), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$earlyStop, c(0.76511109, 0.55712491, 0.75208089), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, c(26.793099, 30.568926, 25.859698), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[1, ], 0.076920806, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[2, ], 0.23316503, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[3, ], 0.27368249, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsEffectScale[1, ], 0.47071068, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsEffectScale[2, ], 0.353709, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsPValueScale[1, ], 0.69146246, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsPValueScale[2, ], 0.30853754, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsPValueScale[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityStop, powerResult$futilityStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityPerStage, powerResult$futilityPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityBoundsEffectScale, powerResult$futilityBoundsEffectScale, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityBoundsPValueScale, powerResult$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeRates}
    # @refFS[Formula]{fs:PowerGroupSequentialOneSided}
    # @refFS[Formula]{fs:AdjShiftParameterOneSampleRate}
    powerResult <- getPowerRates(designGS1,
        groups = 1, thetaH0 = 0.4, pi1 = c(0.4, 0.5, 0.6),
        directionUpper = , maxNumberOfSubjects = 40
    )

    ## Comparison of the results of TrialDesignPlanRates object 'powerResult' with expected results
    expect_equal(powerResult$effect, c(0, 0.1, 0.2), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$effect, collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[1, ], 12, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[2, ], 28, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[3, ], 40, label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, c(0.067448723, 0.39348465, 0.83236985), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[1, ], c(0.011170644, 0.057586328, 0.19206788), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[2, ], c(0.030436001, 0.19052871, 0.45635017), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[3, ], c(0.025842077, 0.14536961, 0.1839518), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityStop, c(0.71047424, 0.30857493, 0.064469377), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityStop, collapse = ", "), ")"))
    expect_equal(powerResult$futilityPerStage[1, ], c(0.30853754, 0.11330227, 0.027796437), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityPerStage[2, ], c(0.40193671, 0.19527267, 0.03667294), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$earlyStop, c(0.75208089, 0.55668998, 0.71288743), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, c(25.859698, 30.585503, 27.927522), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[1, ], 0.72307919, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[2, ], 0.56683497, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[3, ], 0.52631751, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsEffectScale[1, ], 0.32928932, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsEffectScale[2, ], 0.446291, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsPValueScale[1, ], 0.69146246, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsPValueScale[2, ], 0.30853754, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsPValueScale[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityStop, powerResult$futilityStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityPerStage, powerResult$futilityPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityBoundsEffectScale, powerResult$futilityBoundsEffectScale, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityBoundsPValueScale, powerResult$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getPowerRates': Power calculation of rate in one sample for two-sided group sequential design", {
    .skipTestIfDisabled()

    designGS2 <- getDesignGroupSequential(
        informationRates = c(0.3, 0.7, 1), alpha = 0.4,
        sided = 2, beta = 0.1, typeOfDesign = "WT", deltaWT = 0.22
    )

    # @refFS[Tab.]{fs:tab:output:getSampleSizeRates}
    # @refFS[Formula]{fs:PowerGroupSequentialTwoSided}
    # @refFS[Formula]{fs:AdjShiftParameterOneSampleRate}
    powerResult <- getPowerRates(designGS2,
        groups = 1, thetaH0 = 0.4,
        pi1 = seq(0.2, 0.6, 0.1), maxNumberOfSubjects = 40
    )

    ## Comparison of the results of TrialDesignPlanRates object 'powerResult' with expected results
    expect_equal(powerResult$effect, c(-0.2, -0.1, 0, 0.1, 0.2), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$effect, collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[1, ], 12, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[2, ], 28, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[3, ], 40, label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, c(0.97746912, 0.67692518, 0.4, 0.66457209, 0.94801088), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[1, ], c(0.54595705, 0.22704321, 0.1297467, 0.22142183, 0.46151826), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[2, ], c(0.36616073, 0.29278043, 0.16207777, 0.28691724, 0.38813612), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[3, ], c(0.065351333, 0.15710154, 0.10817552, 0.15623302, 0.098356497), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$earlyStop, c(0.91211779, 0.51982364, 0.29182448, 0.50833906, 0.84965439), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, c(20.319274, 30.129425, 34.422159, 30.357182, 22.419855), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleLower[1, ], 0.18573229, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleLower[2, ], 0.28935423, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleLower[3, ], 0.3162256, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleUpper[1, ], 0.61426771, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleUpper[2, ], 0.51064577, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleUpper[3, ], 0.4837744, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesPValueScale[1, ], 0.1297467, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesPValueScale[2, ], 0.23204368, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesPValueScale[3, ], 0.27946463, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScaleLower, powerResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScaleUpper, powerResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesPValueScale, powerResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getPowerRates': Power calculation of rate in two samples for one-sided group sequential design, riskRatio = FALSE", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeRates}
    # @refFS[Formula]{fs:PowerGroupSequentialOneSided}
    # @refFS[Formula]{fs:AdjShiftParameterTwoSamplesRateDiff}
    # @refFS[Formula]{fs:EstimatesDiffFarringtonManning}
    designGS1 <- getDesignGroupSequential(
        informationRates = c(0.3, 0.7, 1), sided = 1, alpha = 0.07,
        beta = 0.1, futilityBounds = c(-0.5, 0.5), typeOfDesign = "WT", deltaWT = 0.22
    )

    # @refFS[Tab.]{fs:tab:output:getSampleSizeRates}
    # @refFS[Formula]{fs:PowerGroupSequentialOneSided}
    # @refFS[Formula]{fs:AdjShiftParameterTwoSamplesRateDiff}
    # @refFS[Formula]{fs:EstimatesDiffFarringtonManning}
    powerResult <- getPowerRates(designGS1,
        groups = 2, thetaH0 = 0.1,
        pi2 = 0.4, pi1 = c(0.1, 0.2, 0.3), directionUpper = FALSE,
        maxNumberOfSubjects = 40, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanRates object 'powerResult' with expected results
    expect_equal(powerResult$effect, c(-0.4, -0.3, -0.2), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$effect, collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[1, ], 12, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[2, ], 28, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[3, ], 40, label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects1[1, ], 9, label = paste0("c(", paste0(powerResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects1[2, ], 21, label = paste0("c(", paste0(powerResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects1[3, ], 30, label = paste0("c(", paste0(powerResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects2[1, ], 3, label = paste0("c(", paste0(powerResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects2[2, ], 7, label = paste0("c(", paste0(powerResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects2[3, ], 10, label = paste0("c(", paste0(powerResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, c(0.86217083, 0.63525529, 0.37370586), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[1, ], c(0.21254585, 0.11056737, 0.054245237), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[2, ], c(0.47569558, 0.32910884, 0.18002797), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[3, ], c(0.17392941, 0.19557908, 0.13943265), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityStop, c(0.05259588, 0.1553509, 0.32411639), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityStop, collapse = ", "), ")"))
    expect_equal(powerResult$futilityPerStage[1, ], c(0.023466961, 0.059262043, 0.11909962), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityPerStage[2, ], c(0.029128919, 0.096088854, 0.20501677), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$earlyStop, c(0.74083731, 0.59502711, 0.5583896), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, c(27.333747, 30.142404, 30.525807), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[1, ], -0.3905544, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[2, ], -0.21681979, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[3, ], -0.15504053, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsEffectScale[1, ], 0.26517501, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsEffectScale[2, ], -0.00361566, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsPValueScale[1, ], 0.69146246, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsPValueScale[2, ], 0.30853754, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsPValueScale[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects1, powerResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects2, powerResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityStop, powerResult$futilityStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityPerStage, powerResult$futilityPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityBoundsEffectScale, powerResult$futilityBoundsEffectScale, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityBoundsPValueScale, powerResult$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeRates}
    # @refFS[Formula]{fs:PowerGroupSequentialOneSided}
    # @refFS[Formula]{fs:AdjShiftParameterTwoSamplesRateDiff}
    # @refFS[Formula]{fs:EstimatesDiffFarringtonManning}
    powerResult <- getPowerRates(designGS1,
        groups = 2, thetaH0 = -0.1,
        pi2 = 0.4, pi1 = c(0.2, 0.3, 0.4, 0.5), directionUpper = TRUE,
        maxNumberOfSubjects = 80, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanRates object 'powerResult' with expected results
    expect_equal(powerResult$effect, c(-0.1, -2.7755576e-17, 0.1, 0.2), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$effect, collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[1, ], 24, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[2, ], 56, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[3, ], 80, label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects1[1, ], 18, label = paste0("c(", paste0(powerResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects1[2, ], 42, label = paste0("c(", paste0(powerResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects1[3, ], 60, label = paste0("c(", paste0(powerResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects2[1, ], 6, label = paste0("c(", paste0(powerResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects2[2, ], 14, label = paste0("c(", paste0(powerResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects2[3, ], 20, label = paste0("c(", paste0(powerResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, c(0.011153335, 0.067448723, 0.22125497, 0.49276327), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[1, ], c(0.0028716829, 0.011170644, 0.031364648, 0.076178456), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[2, ], c(0.0049229598, 0.030436001, 0.1027412, 0.24505539), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[3, ], c(0.0033586921, 0.025842077, 0.087149125, 0.17152942), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityStop, c(0.89841517, 0.71047424, 0.46922933, 0.23841544), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityStop, collapse = ", "), ")"))
    expect_equal(powerResult$futilityPerStage[1, ], c(0.49105221, 0.30853754, 0.17789692, 0.08798644), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityPerStage[2, ], c(0.40736296, 0.40193671, 0.29133241, 0.150429), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$earlyStop, c(0.90620981, 0.75208089, 0.60333518, 0.55964928), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, c(42.4454, 51.719397, 58.823585, 61.315141), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[1, ], 0.38186802, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[2, ], 0.17360028, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[3, ], 0.10931124, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsEffectScale[1, ], -0.20652185, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsEffectScale[2, ], -0.02383242, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsPValueScale[1, ], 0.69146246, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsPValueScale[2, ], 0.30853754, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsPValueScale[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects1, powerResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects2, powerResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityStop, powerResult$futilityStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityPerStage, powerResult$futilityPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityBoundsEffectScale, powerResult$futilityBoundsEffectScale, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityBoundsPValueScale, powerResult$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getPowerRates': Power calculation of rate in two samples for one-sided group sequential design, riskRatio = TRUE", {
    .skipTestIfDisabled()

    designGS1 <- getDesignGroupSequential(
        informationRates = c(0.3, 0.7, 1), sided = 1, alpha = 0.07,
        beta = 0.1, futilityBounds = c(-0.5, 0.5), typeOfDesign = "WT", deltaWT = 0.22
    )

    # @refFS[Tab.]{fs:tab:output:getSampleSizeRates}
    # @refFS[Formula]{fs:PowerGroupSequentialOneSided}
    # @refFS[Formula]{fs:AdjShiftParameterTwoSamplesRateRatio}
    # @refFS[Formula]{fs:EstimatesRatioFarringtonManning}
    powerResult <- getPowerRates(designGS1,
        groups = 2, thetaH0 = 0.8,
        pi2 = 0.5, pi1 = c(0.1, 0.2, 0.3), riskRatio = TRUE, directionUpper = FALSE,
        maxNumberOfSubjects = 40, allocationRatioPlanned = 5
    )

    ## Comparison of the results of TrialDesignPlanRates object 'powerResult' with expected results
    expect_equal(powerResult$effect, c(-0.6, -0.4, -0.2), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$effect, collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[1, ], 12, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[2, ], 28, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[3, ], 40, label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects1[1, ], 10, label = paste0("c(", paste0(powerResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects1[2, ], 23.333333, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects1[3, ], 33.333333, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects2[1, ], 2, label = paste0("c(", paste0(powerResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects2[2, ], 4.6666667, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects2[3, ], 6.6666667, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, c(0.67404635, 0.37979679, 0.17337279), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[1, ], c(0.12233203, 0.055263055, 0.02493902), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[2, ], c(0.35325438, 0.1832494, 0.079687483), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[3, ], c(0.19845995, 0.14128433, 0.068746287), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityStop, c(0.13554504, 0.31926733, 0.52845861), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityStop, collapse = ", "), ")"))
    expect_equal(powerResult$futilityPerStage[1, ], c(0.052497346, 0.11728241, 0.20511002), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityPerStage[2, ], c(0.083047698, 0.20198492, 0.32334859), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$earlyStop, c(0.61113145, 0.55777979, 0.63308512), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, c(29.869153, 30.545915, 28.722194), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[1, ], NA_real_, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[2, ], 0.19789883, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[3, ], 0.30397209, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsEffectScale[1, ], 1.1132916, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsEffectScale[2, ], 0.59448494, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsPValueScale[1, ], 0.69146246, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsPValueScale[2, ], 0.30853754, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsPValueScale[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects1, powerResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects2, powerResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityStop, powerResult$futilityStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityPerStage, powerResult$futilityPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityBoundsEffectScale, powerResult$futilityBoundsEffectScale, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityBoundsPValueScale, powerResult$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeRates}
    # @refFS[Formula]{fs:PowerGroupSequentialOneSided}
    # @refFS[Formula]{fs:AdjShiftParameterTwoSamplesRateRatio}
    # @refFS[Formula]{fs:EstimatesRatioFarringtonManning}
    powerResult <- getPowerRates(designGS1,
        groups = 2, thetaH0 = 0.8,
        pi2 = 0.4, pi1 = c(0.4, 0.5, 0.6), riskRatio = TRUE, directionUpper = TRUE,
        maxNumberOfSubjects = 80, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanRates object 'powerResult' with expected results
    expect_equal(powerResult$effect, c(0.2, 0.45, 0.7), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$effect, collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[1, ], 24, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[2, ], 56, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[3, ], 80, label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects1[1, ], 18, label = paste0("c(", paste0(powerResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects1[2, ], 42, label = paste0("c(", paste0(powerResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects1[3, ], 60, label = paste0("c(", paste0(powerResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects2[1, ], 6, label = paste0("c(", paste0(powerResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects2[2, ], 14, label = paste0("c(", paste0(powerResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects2[3, ], 20, label = paste0("c(", paste0(powerResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, c(0.20890064, 0.52512104, 0.83467468), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[1, ], c(0.029681783, 0.083038809, 0.19351805), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[2, ], c(0.096741134, 0.26351903, 0.45786385), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[3, ], c(0.082477726, 0.17856321, 0.18329277), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityStop, c(0.48366053, 0.21795048, 0.063536004), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityStop, collapse = ", "), ")"))
    expect_equal(powerResult$futilityPerStage[1, ], c(0.18431999, 0.080816996, 0.027459911), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityPerStage[2, ], c(0.29934054, 0.13713348, 0.036076093), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$earlyStop, c(0.61008345, 0.56450831, 0.71491791), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, c(58.50994, 61.208415, 55.770675), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[1, ], 1.8651141, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[2, ], 1.3871263, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[3, ], 1.2471692, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsEffectScale[1, ], 0.57000905, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsEffectScale[2, ], 0.96223105, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsPValueScale[1, ], 0.69146246, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsPValueScale[2, ], 0.30853754, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsPValueScale[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects1, powerResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects2, powerResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityStop, powerResult$futilityStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityPerStage, powerResult$futilityPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityBoundsEffectScale, powerResult$futilityBoundsEffectScale, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityBoundsPValueScale, powerResult$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getPowerRates': Power calculation of rate in two samples for two-sided group sequential design", {
    .skipTestIfDisabled()

    designGS2 <- getDesignGroupSequential(
        informationRates = c(0.3, 0.7, 1), alpha = 0.4,
        sided = 2, beta = 0.1, typeOfDesign = "WT", deltaWT = 0.22
    )

    # @refFS[Tab.]{fs:tab:output:getSampleSizeRates}
    # @refFS[Formula]{fs:PowerGroupSequentialTwoSided}
    # @refFS[Formula]{fs:AdjShiftParameterTwoSamplesRateDiff}
    # @refFS[Formula]{fs:EstimatesDiffFarringtonManning}
    powerResult <- getPowerRates(designGS2,
        groups = 2, pi2 = 0.5, pi1 = c(0.1, 0.2, 0.3),
        riskRatio = FALSE, maxNumberOfSubjects = 40, allocationRatioPlanned = 0.5
    )

    ## Comparison of the results of TrialDesignPlanRates object 'powerResult' with expected results
    expect_equal(powerResult$effect, c(-0.4, -0.3, -0.2), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$effect, collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[1, ], 12, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[2, ], 28, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[3, ], 40, label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects1[1, ], 4, label = paste0("c(", paste0(powerResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects1[2, ], 9.3333333, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects1[3, ], 13.333333, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects2[1, ], 8, label = paste0("c(", paste0(powerResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects2[2, ], 18.666667, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects2[3, ], 26.666667, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, c(0.9745822, 0.84688722, 0.64568809), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[1, ], c(0.53456929, 0.33187612, 0.2131539), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[2, ], c(0.37045799, 0.36871195, 0.27793629), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[3, ], c(0.06955493, 0.14629915, 0.1545979), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$earlyStop, c(0.90502727, 0.70058807, 0.49109019), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, c(20.586564, 26.282925, 30.696455), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleLower[1, ], -0.44319209, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleLower[2, ], -0.2365574, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleLower[3, ], -0.18006528, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleUpper[1, ], 0.44319209, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleUpper[2, ], 0.2365574, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleUpper[3, ], 0.18006528, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesPValueScale[1, ], 0.1297467, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesPValueScale[2, ], 0.23204368, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesPValueScale[3, ], 0.27946463, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects1, powerResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects2, powerResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScaleLower, powerResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScaleUpper, powerResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesPValueScale, powerResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeRates}
    # @refFS[Formula]{fs:PowerGroupSequentialTwoSided}
    # @refFS[Formula]{fs:AdjShiftParameterTwoSamplesRateRatio}
    # @refFS[Formula]{fs:EstimatesRatioFarringtonManning}
    powerResult <- getPowerRates(designGS2,
        groups = 2, pi2 = 0.4, pi1 = c(0.4, 0.5, 0.6),
        riskRatio = TRUE, maxNumberOfSubjects = 80, allocationRatioPlanned = 7
    )

    ## Comparison of the results of TrialDesignPlanRates object 'powerResult' with expected results
    expect_equal(powerResult$effect, c(0, 0.25, 0.5), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$effect, collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[1, ], 24, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[2, ], 56, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[3, ], 80, label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects1[1, ], 21, label = paste0("c(", paste0(powerResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects1[2, ], 49, label = paste0("c(", paste0(powerResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects1[3, ], 70, label = paste0("c(", paste0(powerResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects2[1, ], 3, label = paste0("c(", paste0(powerResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects2[2, ], 7, label = paste0("c(", paste0(powerResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects2[3, ], 10, label = paste0("c(", paste0(powerResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, c(0.4, 0.46817413, 0.63921164), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[1, ], c(0.1297467, 0.14947843, 0.21040306), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[2, ], c(0.16207777, 0.19381617, 0.27485292), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[3, ], c(0.10817552, 0.12487952, 0.15395566), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$earlyStop, c(0.29182448, 0.3432946, 0.48525598), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, c(68.844318, 66.97762, 61.620959), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleLower[1, ], 0.22081341, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleLower[2, ], 0.49677588, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleLower[3, ], 0.5992042, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleUpper[1, ], 2.0083461, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleUpper[2, ], 1.5897897, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleUpper[3, ], 1.4538504, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesPValueScale[1, ], 0.1297467, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesPValueScale[2, ], 0.23204368, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesPValueScale[3, ], 0.27946463, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects1, powerResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects2, powerResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScaleLower, powerResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScaleUpper, powerResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesPValueScale, powerResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})
