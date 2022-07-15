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
## |  File name: test-f_design_power_calculator.R
## |  Creation date: 23 February 2022, 14:06:00
## |  File version: $Revision: 5881 $
## |  Last changed: $Date: 2022-02-24 12:35:06 +0100 (Do, 24 Feb 2022) $
## |  Last changed by: $Author: pahlke $
## |  

context("Testing the Power Calculation of Testing Means for Different Designs and Arguments")


test_that("'getPowerMeans': Power calculation of means in one sample for one-sided group sequential design", {
	designGS1 <- getDesignGroupSequential(
	    informationRates = c(0.3, 0.7, 1), sided = 1, alpha = 0.07,
	    beta = 0.1, futilityBounds = c(-0.5, 0.5), typeOfDesign = "WT", deltaWT = 0.22
	)

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:PowerGroupSequentialOneSided}
	# @refFS[Formula]{fs:AdjShiftParameterOneSampleMean}
	powerResult <- getPowerMeans(designGS1,
	    groups = 1, thetaH0 = 0.5, stDev = 2,
	    normalApproximation = FALSE, alternative = c(-1, 1.2, 1.4),
	    directionUpper = TRUE, maxNumberOfSubjects = 50
	)

	## Comparison of the results of TrialDesignPlanMeans object 'powerResult' with expected results
	expect_equal(powerResult$effect, c(-1.5, 0.7, 0.9), tolerance = 1e-07)
	expect_equal(powerResult$overallReject, c(1.2624119e-07, 0.79805947, 0.93305789), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c(1.2596734e-07, 0.17254516, 0.28730882), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c(2.7189458e-10, 0.43368823, 0.5145435), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c(1.9550892e-12, 0.19182608, 0.13120557), tolerance = 1e-07)
	expect_equal(powerResult$futilityStop, c(0.99999942, 0.078678761, 0.02585129), tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[1, ], c(0.99114779, 0.032857727, 0.013099441), tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[2, ], c(0.008851635, 0.045821034, 0.01275185), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, c(0.99999955, 0.68491215, 0.82770361), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfSubjects, c(15.177049, 35.61826, 31.576281), tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[1, ], 15)
	expect_equal(powerResult$numberOfSubjects[2, ], 35)
	expect_equal(powerResult$numberOfSubjects[3, ], 50)
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 1.8259013, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[2, ], 1.1288256, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[3, ], 0.97002208, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[1, ], 0.2359398, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[2, ], 0.67059547, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[1, ], 0.69146246, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[2, ], 0.30853754, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityStop, powerResult$futilityStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityPerStage, powerResult$futilityPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsEffectScale, powerResult$futilityBoundsEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsPValueScale, powerResult$futilityBoundsPValueScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:PowerGroupSequentialOneSided}
	# @refFS[Formula]{fs:AdjShiftParameterOneSampleMean}
	powerResult <- getPowerMeans(designGS1,
	    groups = 1, thetaH0 = -0.5, stDev = 2,
	    normalApproximation = FALSE, alternative = c(-1.2, -1),
	    directionUpper = FALSE, maxNumberOfSubjects = 50
	)

	## Comparison of the results of TrialDesignPlanMeans object 'powerResult' with expected results
	expect_equal(powerResult$effect, c(-0.7, -0.5), tolerance = 1e-07)
	expect_equal(powerResult$overallReject, c(0.79805947, 0.56526867), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c(0.17254516, 0.092241599), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c(0.43368823, 0.28692789), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c(0.19182608, 0.18609918), tolerance = 1e-07)
	expect_equal(powerResult$futilityStop, c(0.078678761, 0.19394481), tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[1, ], c(0.032857727, 0.072497778), tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[2, ], c(0.045821034, 0.12144703), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, c(0.68491215, 0.5731143), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfSubjects, c(35.61826, 38.108498), tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[1, ], 15)
	expect_equal(powerResult$numberOfSubjects[2, ], 35)
	expect_equal(powerResult$numberOfSubjects[3, ], 50)
	expect_equal(powerResult$criticalValuesEffectScale[1, ], -1.8259013, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[2, ], -1.1288256, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[3, ], -0.97002208, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[1, ], -0.2359398, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[2, ], -0.67059547, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[1, ], 0.69146246, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[2, ], 0.30853754, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityStop, powerResult$futilityStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityPerStage, powerResult$futilityPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsEffectScale, powerResult$futilityBoundsEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsPValueScale, powerResult$futilityBoundsPValueScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:PowerGroupSequentialOneSided}
	# @refFS[Formula]{fs:ShiftParameterOneSampleMean}
	powerResult <- getPowerMeans(designGS1,
	    groups = 1, thetaH0 = 0.5, stDev = 2,
	    normalApproximation = TRUE, alternative = 1.2, maxNumberOfSubjects = 50
	)

	## Comparison of the results of TrialDesignPlanMeans object 'powerResult' with expected results
	expect_equal(powerResult$effect, 0.7, tolerance = 1e-07)
	expect_equal(powerResult$overallReject, 0.80544254, tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.17645213), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.43857394), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c("stage = 3" = 0.19041646), tolerance = 1e-07)
	expect_equal(powerResult$futilityStop, 0.075570189, tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[1, ], c("stage = 1" = 0.031759279), tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[2, ], c("stage = 2" = 0.04381091), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, 0.69059627, tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfSubjects, 35.476828, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[1, ], 15)
	expect_equal(powerResult$numberOfSubjects[2, ], 35)
	expect_equal(powerResult$numberOfSubjects[3, ], 50)
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 1.6797184, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[2, ], 1.1091952, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[3, ], 0.96124634, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[1, ], 0.24180111, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[2, ], 0.66903085, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[1, ], 0.69146246, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[2, ], 0.30853754, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityStop, powerResult$futilityStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityPerStage, powerResult$futilityPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsEffectScale, powerResult$futilityBoundsEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsPValueScale, powerResult$futilityBoundsPValueScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:PowerGroupSequentialOneSided}
	# @refFS[Formula]{fs:ShiftParameterOneSampleMean}
	powerResult <- getPowerMeans(designGS1,
	    groups = 2, thetaH0 = -0.5, stDev = 2,
	    normalApproximation = TRUE, alternative = -1.2, directionUpper = FALSE, maxNumberOfSubjects = 50
	)

	## Comparison of the results of TrialDesignPlanMeans object 'powerResult' with expected results
	expect_equal(powerResult$effect, -0.7, tolerance = 1e-07)
	expect_equal(powerResult$overallReject, 0.37256342, tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.0540554), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.17942496), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c("stage = 3" = 0.13908306), tolerance = 1e-07)
	expect_equal(powerResult$futilityStop, 0.32503231, tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[1, ], c("stage = 1" = 0.11944374), tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[2, ], c("stage = 2" = 0.20558857), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, 0.55851267, tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfSubjects, 38.152327, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[1, ], 15)
	expect_equal(powerResult$numberOfSubjects[2, ], 35)
	expect_equal(powerResult$numberOfSubjects[3, ], 50)
	expect_equal(powerResult$criticalValuesEffectScale[1, ], -2.8594368, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[2, ], -1.7183904, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[3, ], -1.4224927, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[1, ], 0.01639778, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[2, ], -0.8380617, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[1, ], 0.69146246, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[2, ], 0.30853754, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityStop, powerResult$futilityStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityPerStage, powerResult$futilityPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsEffectScale, powerResult$futilityBoundsEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsPValueScale, powerResult$futilityBoundsPValueScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getPowerMeans': Power calculation of means in one sample for two-sided group sequential design", {

	designGS2 <- getDesignGroupSequential(
	    informationRates = c(0.34, 0.66, 1), alpha = 0.12,
	    sided = 2, beta = 0.15, typeOfDesign = "WT", deltaWT = 0.12
	)

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:PowerGroupSequentialTwoSided}
	# @refFS[Formula]{fs:AdjShiftParameterOneSampleMean}
	powerResult <- getPowerMeans(designGS2,
	    groups = 1, thetaH0 = 0.5, stDev = 2,
	    normalApproximation = FALSE, alternative = 1.2, maxNumberOfSubjects = 50
	)

	## Comparison of the results of TrialDesignPlanMeans object 'powerResult' with expected results
	expect_equal(powerResult$effect, 0.7, tolerance = 1e-07)
	expect_equal(powerResult$overallReject, 0.79752024, tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.14049601), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.38370336), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c("stage = 3" = 0.27332087), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, 0.52419937, tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfSubjects, 38.840675, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[1, ], 17)
	expect_equal(powerResult$numberOfSubjects[2, ], 33)
	expect_equal(powerResult$numberOfSubjects[3, ], 50)
	expect_equal(powerResult$criticalValuesEffectScaleLower[1, ], -0.86833341, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[2, ], -0.20368487, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[3, ], 0.020865698, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[1, ], 1.8683334, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[2, ], 1.2036849, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[3, ], 0.9791343, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[1, ], 0.01229935, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[2, ], 0.051692876, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[3, ], 0.096614336, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScaleLower, powerResult$criticalValuesEffectScaleLower, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScaleUpper, powerResult$criticalValuesEffectScaleUpper, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesPValueScale, powerResult$criticalValuesPValueScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:PowerGroupSequentialTwoSided}
	# @refFS[Formula]{fs:AdjShiftParameterOneSampleMean}
	powerResult <- getPowerMeans(designGS2,
	    groups = 1, thetaH0 = -0.5, stDev = 2,
	    normalApproximation = FALSE, alternative = -1.2, maxNumberOfSubjects = 50
	)

	## Comparison of the results of TrialDesignPlanMeans object 'powerResult' with expected results
	expect_equal(powerResult$effect, -0.7, tolerance = 1e-07)
	expect_equal(powerResult$overallReject, 0.79752024, tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.14049601), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.38370336), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c("stage = 3" = 0.27332087), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, 0.52419937, tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfSubjects, 38.840675, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[1, ], 17)
	expect_equal(powerResult$numberOfSubjects[2, ], 33)
	expect_equal(powerResult$numberOfSubjects[3, ], 50)
	expect_equal(powerResult$criticalValuesEffectScaleLower[1, ], -1.8683334, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[2, ], -1.2036849, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[3, ], -0.9791343, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[1, ], 0.86833341, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[2, ], 0.20368487, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[3, ], -0.020865698, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[1, ], 0.01229935, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[2, ], 0.051692876, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[3, ], 0.096614336, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScaleLower, powerResult$criticalValuesEffectScaleLower, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScaleUpper, powerResult$criticalValuesEffectScaleUpper, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesPValueScale, powerResult$criticalValuesPValueScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:PowerGroupSequentialTwoSided}
	# @refFS[Formula]{fs:ShiftParameterOneSampleMean}
	powerResult <- getPowerMeans(designGS2,
	    groups = 1, thetaH0 = 0.5, stDev = 2,
	    normalApproximation = TRUE, alternative = 1.2, maxNumberOfSubjects = 50
	)

	## Comparison of the results of TrialDesignPlanMeans object 'powerResult' with expected results
	expect_equal(powerResult$effect, 0.7, tolerance = 1e-07)
	expect_equal(powerResult$overallReject, 0.80597731, tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.14453229), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.38954071), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c("stage = 3" = 0.27190431), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, 0.534073, tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfSubjects, 38.608242, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[1, ], 17)
	expect_equal(powerResult$numberOfSubjects[2, ], 33)
	expect_equal(powerResult$numberOfSubjects[3, ], 50)
	expect_equal(powerResult$criticalValuesEffectScaleLower[1, ], -0.71434543, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[2, ], -0.17739974, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[3, ], 0.03005862, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[1, ], 1.7143454, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[2, ], 1.1773997, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[3, ], 0.96994138, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[1, ], 0.01229935, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[2, ], 0.051692876, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[3, ], 0.096614336, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScaleLower, powerResult$criticalValuesEffectScaleLower, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScaleUpper, powerResult$criticalValuesEffectScaleUpper, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesPValueScale, powerResult$criticalValuesPValueScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:PowerGroupSequentialTwoSided}
	# @refFS[Formula]{fs:ShiftParameterOneSampleMean}
	powerResult <- getPowerMeans(designGS2,
	    groups = 1, thetaH0 = -0.5, stDev = 2,
	    normalApproximation = TRUE, alternative = -1.2, maxNumberOfSubjects = 50
	)

	## Comparison of the results of TrialDesignPlanMeans object 'powerResult' with expected results
	expect_equal(powerResult$effect, -0.7, tolerance = 1e-07)
	expect_equal(powerResult$overallReject, 0.80597731, tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.14453229), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.38954071), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c("stage = 3" = 0.27190431), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, 0.534073, tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfSubjects, 38.608242, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[1, ], 17)
	expect_equal(powerResult$numberOfSubjects[2, ], 33)
	expect_equal(powerResult$numberOfSubjects[3, ], 50)
	expect_equal(powerResult$criticalValuesEffectScaleLower[1, ], -1.7143454, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[2, ], -1.1773997, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[3, ], -0.96994138, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[1, ], 0.71434543, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[2, ], 0.17739974, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[3, ], -0.03005862, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[1, ], 0.01229935, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[2, ], 0.051692876, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[3, ], 0.096614336, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScaleLower, powerResult$criticalValuesEffectScaleLower, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScaleUpper, powerResult$criticalValuesEffectScaleUpper, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesPValueScale, powerResult$criticalValuesPValueScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getPowerMeans': Power calculation of mean difference in two samples for one-sided group sequential design", {

	designGS1 <- getDesignGroupSequential(
	    informationRates = c(0.3, 0.7, 1), sided = 1, alpha = 0.07,
	    beta = 0.1, futilityBounds = c(-0.5, 0.5), typeOfDesign = "WT", deltaWT = 0.22
	)

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:PowerGroupSequentialOneSided}
	# @refFS[Formula]{fs:AdjShiftParameterTwoSamplesMeanDiff}
	powerResult <- getPowerMeans(designGS1,
	    groups = 2, thetaH0 = 0.5, stDev = 1.5,
	    meanRatio = FALSE, normalApproximation = FALSE, alternative = 1.8,
	    directionUpper = TRUE, maxNumberOfSubjects = 50, allocationRatioPlanned = 3
	)

	## Comparison of the results of TrialDesignPlanMeans object 'powerResult' with expected results
	expect_equal(powerResult$effect, 1.3, tolerance = 1e-07)
	expect_equal(powerResult$overallReject, 0.84205533, tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.19830007), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.46269628), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c("stage = 3" = 0.18105899), tolerance = 1e-07)
	expect_equal(powerResult$futilityStop, 0.060564406, tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[1, ], c("stage = 1" = 0.026384529), tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[2, ], c("stage = 2" = 0.034179878), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, 0.72156075, tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfSubjects, 34.682897, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[1, ], 15)
	expect_equal(powerResult$numberOfSubjects[2, ], 35)
	expect_equal(powerResult$numberOfSubjects[3, ], 50)
	expect_equal(powerResult$numberOfSubjects1[1, ], 11.25, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects1[2, ], 26.25, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects1[3, ], 37.5, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects2[1, ], 3.75, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects2[2, ], 8.75, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects2[3, ], 12.5, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 2.8183805, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[2, ], 1.5902217, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[3, ], 1.3144249, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[1, ], 0.04183972, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[2, ], 0.79556274, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[1, ], 0.69146246, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[2, ], 0.30853754, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityStop, powerResult$futilityStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityPerStage, powerResult$futilityPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects1, powerResult$numberOfSubjects1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects2, powerResult$numberOfSubjects2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsEffectScale, powerResult$futilityBoundsEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsPValueScale, powerResult$futilityBoundsPValueScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:PowerGroupSequentialOneSided}
	# @refFS[Formula]{fs:AdjShiftParameterTwoSamplesMeanDiff}
	powerResult <- getPowerMeans(designGS1,
	    groups = 2, thetaH0 = -0.5, stDev = 1.5,
	    meanRatio = FALSE, normalApproximation = FALSE, alternative = -1.8,
	    directionUpper = FALSE, maxNumberOfSubjects = 50, allocationRatioPlanned = 3
	)

	## Comparison of the results of TrialDesignPlanMeans object 'powerResult' with expected results
	expect_equal(powerResult$effect, -1.3, tolerance = 1e-07)
	expect_equal(powerResult$overallReject, 0.84205533, tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.19830007), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.46269628), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c("stage = 3" = 0.18105899), tolerance = 1e-07)
	expect_equal(powerResult$futilityStop, 0.060564406, tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[1, ], c("stage = 1" = 0.026384529), tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[2, ], c("stage = 2" = 0.034179878), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, 0.72156075, tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfSubjects, 34.682897, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[1, ], 15)
	expect_equal(powerResult$numberOfSubjects[2, ], 35)
	expect_equal(powerResult$numberOfSubjects[3, ], 50)
	expect_equal(powerResult$numberOfSubjects1[1, ], 11.25, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects1[2, ], 26.25, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects1[3, ], 37.5, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects2[1, ], 3.75, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects2[2, ], 8.75, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects2[3, ], 12.5, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[1, ], -2.8183805, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[2, ], -1.5902217, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[3, ], -1.3144249, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[1, ], -0.04183972, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[2, ], -0.79556274, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[1, ], 0.69146246, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[2, ], 0.30853754, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityStop, powerResult$futilityStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityPerStage, powerResult$futilityPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects1, powerResult$numberOfSubjects1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects2, powerResult$numberOfSubjects2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsEffectScale, powerResult$futilityBoundsEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsPValueScale, powerResult$futilityBoundsPValueScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:PowerGroupSequentialOneSided}
	# @refFS[Formula]{fs:ShiftParameterTwoSamplesMean}
	powerResult <- getPowerMeans(designGS1,
	    groups = 2, thetaH0 = 0.5, stDev = 1.5,
	    meanRatio = FALSE, normalApproximation = TRUE, alternative = 1.8,
	    maxNumberOfSubjects = 50, allocationRatioPlanned = 3
	)

	## Comparison of the results of TrialDesignPlanMeans object 'powerResult' with expected results
	expect_equal(powerResult$effect, 1.3, tolerance = 1e-07)
	expect_equal(powerResult$overallReject, 0.84894434, tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.20296684), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.46718133), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c("stage = 3" = 0.17879617), tolerance = 1e-07)
	expect_equal(powerResult$futilityStop, 0.057814211, tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[1, ], c("stage = 1" = 0.025383492), tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[2, ], c("stage = 2" = 0.032430719), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, 0.72796238, tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfSubjects, 34.513558, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[1, ], 15)
	expect_equal(powerResult$numberOfSubjects[2, ], 35)
	expect_equal(powerResult$numberOfSubjects[3, ], 50)
	expect_equal(powerResult$numberOfSubjects1[1, ], 11.25, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects1[2, ], 26.25, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects1[3, ], 37.5, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects2[1, ], 3.75, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects2[2, ], 8.75, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects2[3, ], 12.5, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 2.5433322, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[2, ], 1.555157, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[3, ], 1.2989021, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[1, ], 0.0527864, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[2, ], 0.79277002, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[1, ], 0.69146246, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[2, ], 0.30853754, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityStop, powerResult$futilityStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityPerStage, powerResult$futilityPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects1, powerResult$numberOfSubjects1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects2, powerResult$numberOfSubjects2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsEffectScale, powerResult$futilityBoundsEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsPValueScale, powerResult$futilityBoundsPValueScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:PowerGroupSequentialOneSided}
	# @refFS[Formula]{fs:ShiftParameterTwoSamplesMean}
	powerResult <- getPowerMeans(designGS1,
	    groups = 2, thetaH0 = -0.5, stDev = 1.5,
	    meanRatio = FALSE, normalApproximation = TRUE, alternative = -1.8,
	    directionUpper = FALSE, maxNumberOfSubjects = 50, allocationRatioPlanned = 3
	)

	## Comparison of the results of TrialDesignPlanMeans object 'powerResult' with expected results
	expect_equal(powerResult$effect, -1.3, tolerance = 1e-07)
	expect_equal(powerResult$overallReject, 0.84894434, tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.20296684), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.46718133), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c("stage = 3" = 0.17879617), tolerance = 1e-07)
	expect_equal(powerResult$futilityStop, 0.057814211, tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[1, ], c("stage = 1" = 0.025383492), tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[2, ], c("stage = 2" = 0.032430719), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, 0.72796238, tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfSubjects, 34.513558, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[1, ], 15)
	expect_equal(powerResult$numberOfSubjects[2, ], 35)
	expect_equal(powerResult$numberOfSubjects[3, ], 50)
	expect_equal(powerResult$numberOfSubjects1[1, ], 11.25, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects1[2, ], 26.25, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects1[3, ], 37.5, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects2[1, ], 3.75, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects2[2, ], 8.75, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects2[3, ], 12.5, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[1, ], -2.5433322, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[2, ], -1.555157, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[3, ], -1.2989021, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[1, ], -0.0527864, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[2, ], -0.79277002, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[1, ], 0.69146246, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[2, ], 0.30853754, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityStop, powerResult$futilityStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityPerStage, powerResult$futilityPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects1, powerResult$numberOfSubjects1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects2, powerResult$numberOfSubjects2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsEffectScale, powerResult$futilityBoundsEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsPValueScale, powerResult$futilityBoundsPValueScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:PowerGroupSequentialOneSided}
	# @refFS[Formula]{fs:AdjShiftParameterTwoSamplesMeanRatio}
	powerResult <- getPowerMeans(designGS1,
	    groups = 2, thetaH0 = 0.8,
	    stDev = 1.5, meanRatio = TRUE, normalApproximation = FALSE, alternative = 1.8,
	    directionUpper = TRUE, maxNumberOfSubjects = 50, allocationRatioPlanned = 3
	)

	## Comparison of the results of TrialDesignPlanMeans object 'powerResult' with expected results
	expect_equal(powerResult$effect, 1)
	expect_equal(powerResult$overallReject, 0.77427796, tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.16086364), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.41797637), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c("stage = 3" = 0.19543795), tolerance = 1e-07)
	expect_equal(powerResult$futilityStop, 0.08888951, tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[1, ], c("stage = 1" = 0.036438496), tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[2, ], c("stage = 2" = 0.052451014), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, 0.66772952, tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfSubjects, 36.038015, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[1, ], 15)
	expect_equal(powerResult$numberOfSubjects[2, ], 35)
	expect_equal(powerResult$numberOfSubjects[3, ], 50)
	expect_equal(powerResult$numberOfSubjects1[1, ], 11.25, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects1[2, ], 26.25, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects1[3, ], 37.5, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects2[1, ], 3.75, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects2[2, ], 8.75, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects2[3, ], 12.5, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 2.7808252, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[2, ], 1.7314858, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[3, ], 1.495845, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[1, ], 0.40854768, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[2, ], 1.0525289, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[1, ], 0.69146246, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[2, ], 0.30853754, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityStop, powerResult$futilityStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityPerStage, powerResult$futilityPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects1, powerResult$numberOfSubjects1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects2, powerResult$numberOfSubjects2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsEffectScale, powerResult$futilityBoundsEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsPValueScale, powerResult$futilityBoundsPValueScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:PowerGroupSequentialOneSided}
	# @refFS[Formula]{fs:ShiftParameterTwoSamplesMean}
	powerResult <- getPowerMeans(designGS1,
	    groups = 2, thetaH0 = 0.8,
	    stDev = 1.5, meanRatio = TRUE, normalApproximation = TRUE, alternative = 1.8,
	    maxNumberOfSubjects = 50, allocationRatioPlanned = 3
	)

	## Comparison of the results of TrialDesignPlanMeans object 'powerResult' with expected results
	expect_equal(powerResult$effect, 1)
	expect_equal(powerResult$overallReject, 0.7820561, tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.16454336), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.42310788), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c("stage = 3" = 0.19440486), tolerance = 1e-07)
	expect_equal(powerResult$futilityStop, 0.085516174, tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[1, ], c("stage = 1" = 0.035259709), tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[2, ], c("stage = 2" = 0.050256465), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, 0.67316741, tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfSubjects, 35.906427, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[1, ], 15)
	expect_equal(powerResult$numberOfSubjects[2, ], 35)
	expect_equal(powerResult$numberOfSubjects[3, ], 50)
	expect_equal(powerResult$numberOfSubjects1[1, ], 11.25, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects1[2, ], 26.25, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects1[3, ], 37.5, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects2[1, ], 3.75, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects2[2, ], 8.75, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects2[3, ], 12.5, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 2.5458238, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[2, ], 1.7015266, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[3, ], 1.4825823, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[1, ], 0.41790054, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[2, ], 1.0501428, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[1, ], 0.69146246, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[2, ], 0.30853754, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityStop, powerResult$futilityStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityPerStage, powerResult$futilityPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects1, powerResult$numberOfSubjects1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects2, powerResult$numberOfSubjects2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsEffectScale, powerResult$futilityBoundsEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsPValueScale, powerResult$futilityBoundsPValueScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getPowerMeans': Power calculation of mean difference in two samples for two-sided group sequential design", {

	designGS2 <- getDesignGroupSequential(
	    informationRates = c(0.3, 0.7, 1), alpha = 0.4,
	    sided = 2, beta = 0.1, typeOfDesign = "WT", deltaWT = 0.22
	)

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:PowerGroupSequentialTwoSided}
	# @refFS[Formula]{fs:AdjShiftParameterTwoSamplesMeanDiff}
	powerResult <- getPowerMeans(designGS2,
	    groups = 2, stDev = 2,
	    normalApproximation = FALSE, alternative = 1.2,
	    maxNumberOfSubjects = 50, allocationRatioPlanned = 0.7
	)

	## Comparison of the results of TrialDesignPlanMeans object 'powerResult' with expected results
	expect_equal(powerResult$effect, 1.2, tolerance = 1e-07)
	expect_equal(powerResult$overallReject, 0.87442088, tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.35754296), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.37848399), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c("stage = 3" = 0.13839393), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, 0.73602695, tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfSubjects, 31.808737, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[1, ], 15)
	expect_equal(powerResult$numberOfSubjects[2, ], 35)
	expect_equal(powerResult$numberOfSubjects[3, ], 50)
	expect_equal(powerResult$numberOfSubjects1[1, ], 6.1764706, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects1[2, ], 14.411765, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects1[3, ], 20.588235, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects2[1, ], 8.8235294, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects2[2, ], 20.588235, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects2[3, ], 29.411765, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[1, ], -1.6972761, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[2, ], -0.83631454, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[3, ], -0.62866109, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[1, ], 1.6972761, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[2, ], 0.83631454, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[3, ], 0.62866109, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[1, ], 0.1297467, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[2, ], 0.23204368, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[3, ], 0.27946463, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects1, powerResult$numberOfSubjects1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects2, powerResult$numberOfSubjects2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScaleLower, powerResult$criticalValuesEffectScaleLower, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScaleUpper, powerResult$criticalValuesEffectScaleUpper, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesPValueScale, powerResult$criticalValuesPValueScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:PowerGroupSequentialTwoSided}
	# @refFS[Formula]{fs:AdjShiftParameterTwoSamplesMeanDiff}
	powerResult <- getPowerMeans(designGS2,
	    groups = 2, stDev = 2,
	    normalApproximation = FALSE, alternative = -1.2,
	    maxNumberOfSubjects = 50, allocationRatioPlanned = 0.7
	)

	## Comparison of the results of TrialDesignPlanMeans object 'powerResult' with expected results
	expect_equal(powerResult$effect, -1.2, tolerance = 1e-07)
	expect_equal(powerResult$overallReject, 0.87442088, tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.35754296), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.37848399), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c("stage = 3" = 0.13839393), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, 0.73602695, tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfSubjects, 31.808737, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[1, ], 15)
	expect_equal(powerResult$numberOfSubjects[2, ], 35)
	expect_equal(powerResult$numberOfSubjects[3, ], 50)
	expect_equal(powerResult$numberOfSubjects1[1, ], 6.1764706, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects1[2, ], 14.411765, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects1[3, ], 20.588235, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects2[1, ], 8.8235294, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects2[2, ], 20.588235, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects2[3, ], 29.411765, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[1, ], -1.6972761, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[2, ], -0.83631454, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[3, ], -0.62866109, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[1, ], 1.6972761, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[2, ], 0.83631454, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[3, ], 0.62866109, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[1, ], 0.1297467, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[2, ], 0.23204368, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[3, ], 0.27946463, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects1, powerResult$numberOfSubjects1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects2, powerResult$numberOfSubjects2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScaleLower, powerResult$criticalValuesEffectScaleLower, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScaleUpper, powerResult$criticalValuesEffectScaleUpper, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesPValueScale, powerResult$criticalValuesPValueScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:PowerGroupSequentialTwoSided}
	# @refFS[Formula]{fs:ShiftParameterTwoSamplesMean}
	powerResult <- getPowerMeans(designGS2,
	    groups = 2, thetaH0 = 0,
	    stDev = 2, normalApproximation = TRUE, alternative = 1.2,
	    maxNumberOfSubjects = 50, allocationRatioPlanned = 0.7
	)

	## Comparison of the results of TrialDesignPlanMeans object 'powerResult' with expected results
	expect_equal(powerResult$effect, 1.2, tolerance = 1e-07)
	expect_equal(powerResult$overallReject, 0.87592587, tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.35907583), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.37896773), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c("stage = 3" = 0.13788231), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, 0.73804356, tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfSubjects, 31.74783, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[1, ], 15)
	expect_equal(powerResult$numberOfSubjects[2, ], 35)
	expect_equal(powerResult$numberOfSubjects[3, ], 50)
	expect_equal(powerResult$numberOfSubjects1[1, ], 6.1764706, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects1[2, ], 14.411765, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects1[3, ], 20.588235, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects2[1, ], 8.8235294, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects2[2, ], 20.588235, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects2[3, ], 29.411765, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[1, ], -1.5897396, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[2, ], -0.82092617, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[3, ], -0.62155644, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[1, ], 1.5897396, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[2, ], 0.82092617, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[3, ], 0.62155644, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[1, ], 0.1297467, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[2, ], 0.23204368, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[3, ], 0.27946463, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects1, powerResult$numberOfSubjects1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects2, powerResult$numberOfSubjects2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScaleLower, powerResult$criticalValuesEffectScaleLower, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScaleUpper, powerResult$criticalValuesEffectScaleUpper, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesPValueScale, powerResult$criticalValuesPValueScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:PowerGroupSequentialTwoSided}
	# @refFS[Formula]{fs:ShiftParameterTwoSamplesMean}
	powerResult <- getPowerMeans(designGS2,
	    groups = 2, stDev = 2,
	    normalApproximation = TRUE, alternative = -1.2,
	    maxNumberOfSubjects = 50, allocationRatioPlanned = 0.7
	)

	## Comparison of the results of TrialDesignPlanMeans object 'powerResult' with expected results
	expect_equal(powerResult$effect, -1.2, tolerance = 1e-07)
	expect_equal(powerResult$overallReject, 0.87592587, tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.35907583), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.37896773), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c("stage = 3" = 0.13788231), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, 0.73804356, tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfSubjects, 31.74783, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[1, ], 15)
	expect_equal(powerResult$numberOfSubjects[2, ], 35)
	expect_equal(powerResult$numberOfSubjects[3, ], 50)
	expect_equal(powerResult$numberOfSubjects1[1, ], 6.1764706, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects1[2, ], 14.411765, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects1[3, ], 20.588235, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects2[1, ], 8.8235294, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects2[2, ], 20.588235, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects2[3, ], 29.411765, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[1, ], -1.5897396, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[2, ], -0.82092617, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[3, ], -0.62155644, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[1, ], 1.5897396, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[2, ], 0.82092617, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[3, ], 0.62155644, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[1, ], 0.1297467, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[2, ], 0.23204368, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[3, ], 0.27946463, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects1, powerResult$numberOfSubjects1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects2, powerResult$numberOfSubjects2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScaleLower, powerResult$criticalValuesEffectScaleLower, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScaleUpper, powerResult$criticalValuesEffectScaleUpper, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesPValueScale, powerResult$criticalValuesPValueScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	###################################################################################################
	###################################################################################################

})

context("Testing the Power Calculation of Testing Rates for Different Designs and Arguments")


test_that("'getPowerRates': Power calculation of rate in one sample for one-sided group sequential design", {
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
	expect_equal(powerResult$effect, c(-0.2, -0.1, 0), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfSubjects, c(26.793099, 30.568926, 25.859698), tolerance = 1e-07)
	expect_equal(powerResult$overallReject, c(0.8850078, 0.38742607, 0.067448723), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c(0.23143452, 0.056551742, 0.011170644), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c(0.48990786, 0.18729986, 0.030436001), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c(0.16366541, 0.14357447, 0.025842077), tolerance = 1e-07)
	expect_equal(powerResult$futilityStop, c(0.043768704, 0.31327331, 0.71047424), tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[1, ], c(0.020163481, 0.11504671, 0.30853754), tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[2, ], c(0.023605223, 0.1982266, 0.40193671), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, c(0.76511109, 0.55712491, 0.75208089), tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[1, ], 12)
	expect_equal(powerResult$numberOfSubjects[2, ], 28)
	expect_equal(powerResult$numberOfSubjects[3, ], 40)
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 0.076920806, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[2, ], 0.23316503, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[3, ], 0.27368249, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[1, ], 0.47071068, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[2, ], 0.353709, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[1, ], 0.69146246, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[2, ], 0.30853754, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityStop, powerResult$futilityStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityPerStage, powerResult$futilityPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsEffectScale, powerResult$futilityBoundsEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsPValueScale, powerResult$futilityBoundsPValueScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getSampleSizeRates}
	# @refFS[Formula]{fs:PowerGroupSequentialOneSided}
	# @refFS[Formula]{fs:AdjShiftParameterOneSampleRate}
	powerResult <- getPowerRates(designGS1,
	    groups = 1, thetaH0 = 0.4, pi1 = c(0.4, 0.5, 0.6),
	    directionUpper = , maxNumberOfSubjects = 40
	)

	## Comparison of the results of TrialDesignPlanRates object 'powerResult' with expected results
	expect_equal(powerResult$effect, c(0, 0.1, 0.2), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfSubjects, c(25.859698, 30.585503, 27.927522), tolerance = 1e-07)
	expect_equal(powerResult$overallReject, c(0.067448723, 0.39348465, 0.83236985), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c(0.011170644, 0.057586328, 0.19206788), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c(0.030436001, 0.19052871, 0.45635017), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c(0.025842077, 0.14536961, 0.1839518), tolerance = 1e-07)
	expect_equal(powerResult$futilityStop, c(0.71047424, 0.30857493, 0.064469377), tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[1, ], c(0.30853754, 0.11330227, 0.027796437), tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[2, ], c(0.40193671, 0.19527267, 0.03667294), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, c(0.75208089, 0.55668998, 0.71288743), tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[1, ], 12)
	expect_equal(powerResult$numberOfSubjects[2, ], 28)
	expect_equal(powerResult$numberOfSubjects[3, ], 40)
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 0.72307919, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[2, ], 0.56683497, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[3, ], 0.52631751, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[1, ], 0.32928932, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[2, ], 0.446291, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[1, ], 0.69146246, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[2, ], 0.30853754, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityStop, powerResult$futilityStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityPerStage, powerResult$futilityPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsEffectScale, powerResult$futilityBoundsEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsPValueScale, powerResult$futilityBoundsPValueScale, tolerance = 1e-05)
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
	expect_equal(powerResult$effect, c(-0.2, -0.1, 0, 0.1, 0.2), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfSubjects, c(20.319274, 30.129425, 34.422159, 30.357182, 22.419855), tolerance = 1e-07)
	expect_equal(powerResult$overallReject, c(0.97746912, 0.67692518, 0.4, 0.66457209, 0.94801088), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c(0.54595705, 0.22704321, 0.1297467, 0.22142183, 0.46151826), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c(0.36616073, 0.29278043, 0.16207777, 0.28691724, 0.38813612), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c(0.065351333, 0.15710154, 0.10817552, 0.15623302, 0.098356497), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, c(0.91211779, 0.51982364, 0.29182448, 0.50833906, 0.84965439), tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[1, ], 12)
	expect_equal(powerResult$numberOfSubjects[2, ], 28)
	expect_equal(powerResult$numberOfSubjects[3, ], 40)
	expect_equal(powerResult$criticalValuesEffectScaleLower[1, ], 0.18573229, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[2, ], 0.28935423, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[3, ], 0.3162256, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[1, ], 0.61426771, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[2, ], 0.51064577, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[3, ], 0.4837744, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[1, ], 0.1297467, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[2, ], 0.23204368, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[3, ], 0.27946463, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScaleLower, powerResult$criticalValuesEffectScaleLower, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScaleUpper, powerResult$criticalValuesEffectScaleUpper, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesPValueScale, powerResult$criticalValuesPValueScale, tolerance = 1e-05)
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
	expect_equal(powerResult$effect, c(-0.4, -0.3, -0.2), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfSubjects, c(27.333747, 30.142404, 30.525807), tolerance = 1e-07)
	expect_equal(powerResult$overallReject, c(0.86217083, 0.63525529, 0.37370586), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c(0.21254585, 0.11056737, 0.054245237), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c(0.47569558, 0.32910884, 0.18002797), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c(0.17392941, 0.19557908, 0.13943265), tolerance = 1e-07)
	expect_equal(powerResult$futilityStop, c(0.05259588, 0.1553509, 0.32411639), tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[1, ], c(0.023466961, 0.059262043, 0.11909962), tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[2, ], c(0.029128919, 0.096088854, 0.20501677), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, c(0.74083731, 0.59502711, 0.5583896), tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[1, ], 12)
	expect_equal(powerResult$numberOfSubjects[2, ], 28)
	expect_equal(powerResult$numberOfSubjects[3, ], 40)
	expect_equal(powerResult$numberOfSubjects1[1, ], 9)
	expect_equal(powerResult$numberOfSubjects1[2, ], 21)
	expect_equal(powerResult$numberOfSubjects1[3, ], 30)
	expect_equal(powerResult$numberOfSubjects2[1, ], 3)
	expect_equal(powerResult$numberOfSubjects2[2, ], 7)
	expect_equal(powerResult$numberOfSubjects2[3, ], 10)
	expect_equal(powerResult$criticalValuesEffectScale[1, ], -0.3905544, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[2, ], -0.21681979, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[3, ], -0.15504053, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[1, ], 0.26517501, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[2, ], -0.00361566, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[1, ], 0.69146246, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[2, ], 0.30853754, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityStop, powerResult$futilityStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityPerStage, powerResult$futilityPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects1, powerResult$numberOfSubjects1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects2, powerResult$numberOfSubjects2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsEffectScale, powerResult$futilityBoundsEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsPValueScale, powerResult$futilityBoundsPValueScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	.skipTestIfDisabled()

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
	expect_equal(powerResult$effect, c(-0.1, -2.7755576e-17, 0.1, 0.2), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfSubjects, c(42.4454, 51.719397, 58.823585, 61.315141), tolerance = 1e-07)
	expect_equal(powerResult$overallReject, c(0.011153335, 0.067448723, 0.22125497, 0.49276327), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c(0.0028716829, 0.011170644, 0.031364648, 0.076178456), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c(0.0049229598, 0.030436001, 0.1027412, 0.24505539), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c(0.0033586921, 0.025842077, 0.087149125, 0.17152942), tolerance = 1e-07)
	expect_equal(powerResult$futilityStop, c(0.89841517, 0.71047424, 0.46922933, 0.23841544), tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[1, ], c(0.49105221, 0.30853754, 0.17789692, 0.08798644), tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[2, ], c(0.40736296, 0.40193671, 0.29133241, 0.150429), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, c(0.90620981, 0.75208089, 0.60333518, 0.55964928), tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[1, ], 24)
	expect_equal(powerResult$numberOfSubjects[2, ], 56)
	expect_equal(powerResult$numberOfSubjects[3, ], 80)
	expect_equal(powerResult$numberOfSubjects1[1, ], 18)
	expect_equal(powerResult$numberOfSubjects1[2, ], 42)
	expect_equal(powerResult$numberOfSubjects1[3, ], 60)
	expect_equal(powerResult$numberOfSubjects2[1, ], 6)
	expect_equal(powerResult$numberOfSubjects2[2, ], 14)
	expect_equal(powerResult$numberOfSubjects2[3, ], 20)
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 0.38186802, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[2, ], 0.17360028, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[3, ], 0.10931124, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[1, ], -0.20652185, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[2, ], -0.02383242, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[1, ], 0.69146246, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[2, ], 0.30853754, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityStop, powerResult$futilityStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityPerStage, powerResult$futilityPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects1, powerResult$numberOfSubjects1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects2, powerResult$numberOfSubjects2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsEffectScale, powerResult$futilityBoundsEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsPValueScale, powerResult$futilityBoundsPValueScale, tolerance = 1e-05)
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
	expect_equal(powerResult$effect, c(-0.6, -0.4, -0.2), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfSubjects, c(29.869153, 30.545915, 28.722194), tolerance = 1e-07)
	expect_equal(powerResult$overallReject, c(0.67404635, 0.37979679, 0.17337279), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c(0.12233203, 0.055263055, 0.02493902), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c(0.35325438, 0.1832494, 0.079687483), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c(0.19845995, 0.14128433, 0.068746287), tolerance = 1e-07)
	expect_equal(powerResult$futilityStop, c(0.13554504, 0.31926733, 0.52845861), tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[1, ], c(0.052497346, 0.11728241, 0.20511002), tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[2, ], c(0.083047698, 0.20198492, 0.32334859), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, c(0.61113145, 0.55777979, 0.63308512), tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[1, ], 12)
	expect_equal(powerResult$numberOfSubjects[2, ], 28)
	expect_equal(powerResult$numberOfSubjects[3, ], 40)
	expect_equal(powerResult$numberOfSubjects1[1, ], 10)
	expect_equal(powerResult$numberOfSubjects1[2, ], 23.333333, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects1[3, ], 33.333333, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects2[1, ], 2)
	expect_equal(powerResult$numberOfSubjects2[2, ], 4.6666667, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects2[3, ], 6.6666667, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[1, ], NA_real_)
	expect_equal(powerResult$criticalValuesEffectScale[2, ], 0.19789883, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[3, ], 0.30397209, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[1, ], 1.1132916, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[2, ], 0.59448494, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[1, ], 0.69146246, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[2, ], 0.30853754, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityStop, powerResult$futilityStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityPerStage, powerResult$futilityPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects1, powerResult$numberOfSubjects1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects2, powerResult$numberOfSubjects2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsEffectScale, powerResult$futilityBoundsEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsPValueScale, powerResult$futilityBoundsPValueScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	.skipTestIfDisabled()

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
	expect_equal(powerResult$effect, c(0.2, 0.45, 0.7), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfSubjects, c(58.50994, 61.208415, 55.770675), tolerance = 1e-07)
	expect_equal(powerResult$overallReject, c(0.20890064, 0.52512104, 0.83467468), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c(0.029681783, 0.083038809, 0.19351805), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c(0.096741134, 0.26351903, 0.45786385), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c(0.082477726, 0.17856321, 0.18329277), tolerance = 1e-07)
	expect_equal(powerResult$futilityStop, c(0.48366053, 0.21795048, 0.063536004), tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[1, ], c(0.18431999, 0.080816996, 0.027459911), tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[2, ], c(0.29934054, 0.13713348, 0.036076093), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, c(0.61008345, 0.56450831, 0.71491791), tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[1, ], 24)
	expect_equal(powerResult$numberOfSubjects[2, ], 56)
	expect_equal(powerResult$numberOfSubjects[3, ], 80)
	expect_equal(powerResult$numberOfSubjects1[1, ], 18)
	expect_equal(powerResult$numberOfSubjects1[2, ], 42)
	expect_equal(powerResult$numberOfSubjects1[3, ], 60)
	expect_equal(powerResult$numberOfSubjects2[1, ], 6)
	expect_equal(powerResult$numberOfSubjects2[2, ], 14)
	expect_equal(powerResult$numberOfSubjects2[3, ], 20)
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 1.8651141, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[2, ], 1.3871263, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[3, ], 1.2471692, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[1, ], 0.57000905, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[2, ], 0.96223105, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[1, ], 0.69146246, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[2, ], 0.30853754, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityStop, powerResult$futilityStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityPerStage, powerResult$futilityPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects1, powerResult$numberOfSubjects1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects2, powerResult$numberOfSubjects2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsEffectScale, powerResult$futilityBoundsEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsPValueScale, powerResult$futilityBoundsPValueScale, tolerance = 1e-05)
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
	expect_equal(powerResult$effect, c(-0.4, -0.3, -0.2), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfSubjects, c(20.586564, 26.282925, 30.696455), tolerance = 1e-07)
	expect_equal(powerResult$overallReject, c(0.9745822, 0.84688722, 0.64568809), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c(0.53456929, 0.33187612, 0.2131539), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c(0.37045799, 0.36871195, 0.27793629), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c(0.06955493, 0.14629915, 0.1545979), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, c(0.90502727, 0.70058807, 0.49109019), tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[1, ], 12)
	expect_equal(powerResult$numberOfSubjects[2, ], 28)
	expect_equal(powerResult$numberOfSubjects[3, ], 40)
	expect_equal(powerResult$numberOfSubjects1[1, ], 4)
	expect_equal(powerResult$numberOfSubjects1[2, ], 9.3333333, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects1[3, ], 13.333333, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects2[1, ], 8)
	expect_equal(powerResult$numberOfSubjects2[2, ], 18.666667, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects2[3, ], 26.666667, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[1, ], -0.44319209, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[2, ], -0.2365574, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[3, ], -0.18006528, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[1, ], 0.44319209, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[2, ], 0.2365574, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[3, ], 0.18006528, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[1, ], 0.1297467, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[2, ], 0.23204368, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[3, ], 0.27946463, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects1, powerResult$numberOfSubjects1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects2, powerResult$numberOfSubjects2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScaleLower, powerResult$criticalValuesEffectScaleLower, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScaleUpper, powerResult$criticalValuesEffectScaleUpper, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesPValueScale, powerResult$criticalValuesPValueScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getSampleSizeRates}
	# @refFS[Formula]{fs:PowerGroupSequentialTwoSided}
	# @refFS[Formula]{fs:AdjShiftParameterTwoSamplesRateRatio}
	# @refFS[Formula]{fs:EstimatesRatioFarringtonManning}
	powerResult <- getPowerRates(designGS2,
	    groups = 2, pi2 = 0.4, pi1 = c(0.4, 0.5, 0.6),
	    riskRatio = TRUE, maxNumberOfSubjects = 80, allocationRatioPlanned = 7
	)

	## Comparison of the results of TrialDesignPlanRates object 'powerResult' with expected results
	expect_equal(powerResult$effect, c(0, 0.25, 0.5), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfSubjects, c(68.844318, 66.97762, 61.620959), tolerance = 1e-07)
	expect_equal(powerResult$overallReject, c(0.4, 0.46817413, 0.63921164), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c(0.1297467, 0.14947843, 0.21040306), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c(0.16207777, 0.19381617, 0.27485292), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c(0.10817552, 0.12487952, 0.15395566), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, c(0.29182448, 0.3432946, 0.48525598), tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[1, ], 24)
	expect_equal(powerResult$numberOfSubjects[2, ], 56)
	expect_equal(powerResult$numberOfSubjects[3, ], 80)
	expect_equal(powerResult$numberOfSubjects1[1, ], 21)
	expect_equal(powerResult$numberOfSubjects1[2, ], 49)
	expect_equal(powerResult$numberOfSubjects1[3, ], 70)
	expect_equal(powerResult$numberOfSubjects2[1, ], 3)
	expect_equal(powerResult$numberOfSubjects2[2, ], 7)
	expect_equal(powerResult$numberOfSubjects2[3, ], 10)
	expect_equal(powerResult$criticalValuesEffectScaleLower[1, ], 0.22081341, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[2, ], 0.49677588, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[3, ], 0.5992042, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[1, ], 2.0083461, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[2, ], 1.5897897, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[3, ], 1.4538504, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[1, ], 0.1297467, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[2, ], 0.23204368, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[3, ], 0.27946463, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$effect, powerResult$effect, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects1, powerResult$numberOfSubjects1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects2, powerResult$numberOfSubjects2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScaleLower, powerResult$criticalValuesEffectScaleLower, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScaleUpper, powerResult$criticalValuesEffectScaleUpper, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesPValueScale, powerResult$criticalValuesPValueScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	###################################################################################################
	###################################################################################################

})

context("Testing the Power Calculation of Survival Designs for Different Designs and Arguments")


test_that("'getPowerSurvival': Fixed sample size with minimum required definitions, pi1 = c(0.4, 0.5, 0.6) and pi2 = 0.2 at event time 12, accrual time 12 and follow-up time 6 as default", {
	# @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
	# @refFS[Formula]{fs:ShiftParameterSurvivalSchoenfeld}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedPatientAccrual}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	powerResult <- getPowerSurvival(maxNumberOfEvents = 40, maxNumberOfSubjects = 200)

	## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
	expect_equal(powerResult$median1, c(37.275405, 23.320299, 16.282985, 12), tolerance = 1e-07)
	expect_equal(powerResult$median2, 37.275405, tolerance = 1e-07)
	expect_equal(powerResult$lambda1, c(0.018595296, 0.029722912, 0.042568802, 0.057762265), tolerance = 1e-07)
	expect_equal(powerResult$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(powerResult$hazardRatio, c(1, 1.5984103, 2.2892242, 3.1062837), tolerance = 1e-07)
	expect_equal(powerResult$accrualIntensity, 16.666667, tolerance = 1e-07)
	expect_equal(powerResult$followUpTime, c(6.1115255, 3.442577, 1.6316894, 0.30440109), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfEvents, c(40, 40, 40, 40))
	expect_equal(powerResult$overallReject, c(0.025, 0.31674317, 0.74507635, 0.94783846), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[1, ], c(18.111525, 15.442577, 13.631689, 12.304401), tolerance = 1e-07)
	expect_equal(powerResult$studyDuration, c(18.111525, 15.442577, 13.631689, 12.304401), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfSubjects, c(200, 200, 200, 200))
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 1.8585471, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda2, powerResult$lambda2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$hazardRatio, powerResult$hazardRatio, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getPowerSurvival': Power calculation of survival designs for one-sided group sequential design", {

	designGS1 <- getDesignGroupSequential(
	    informationRates = c(0.3, 0.7, 1), sided = 1, alpha = 0.07,
	    beta = 0.1, futilityBounds = c(-0.5, 0.5), typeOfDesign = "WT", deltaWT = 0.22
	)

	# @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
	# @refFS[Formula]{fs:PowerGroupSequentialOneSided}
	# @refFS[Formula]{fs:ShiftParameterSurvivalSchoenfeld}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedPatientAccrual}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalFindFollowUpTime}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	powerResult <- getPowerSurvival(designGS1,
	    pi2 = 0.4, pi1 = c(0.4, 0.5, 0.6),
	    dropoutRate1 = 0.1, dropoutTime = 12, eventTime = 24,
	    maxNumberOfSubjects = 80, maxNumberOfEvents = 45, allocationRatioPlanned = 3
	)

	## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
	expect_equal(powerResult$median1, c(32.565971, 24, 18.155299), tolerance = 1e-07)
	expect_equal(powerResult$median2, 32.565971, tolerance = 1e-07)
	expect_equal(powerResult$lambda1, c(0.021284401, 0.028881133, 0.03817878), tolerance = 1e-07)
	expect_equal(powerResult$lambda2, 0.021284401, tolerance = 1e-07)
	expect_equal(powerResult$hazardRatio, c(1, 1.3569154, 1.7937447), tolerance = 1e-07)
	expect_equal(powerResult$accrualIntensity, 6.6666667, tolerance = 1e-07)
	expect_equal(powerResult$followUpTime, c(41.628872, 30.417026, 22.638977), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfEvents, c(29.092161, 33.496718, 34.368969), tolerance = 1e-07)
	expect_equal(powerResult$overallReject, c(0.067448723, 0.25463139, 0.54601962), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c(0.011170644, 0.036015488, 0.087726198), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c(0.030436001, 0.11913846, 0.27563412), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c(0.025842077, 0.099477436, 0.1826593), tolerance = 1e-07)
	expect_equal(powerResult$futilityStop, c(0.71047424, 0.43269831, 0.2052719), tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[1, ], c(0.30853754, 0.16216653, 0.076412449), tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[2, ], c(0.40193671, 0.27053178, 0.12885945), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, c(0.75208089, 0.58785226, 0.56863222), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[1, ], c(15.123713, 13.244904, 11.839868), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[2, ], c(32.118539, 26.401459, 22.217088), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[3, ], c(53.628872, 42.417026, 34.638977), tolerance = 1e-07)
	expect_equal(powerResult$studyDuration, c(32.017976, 30.394846, 25.872188), tolerance = 1e-07)
	expect_equal(powerResult$maxStudyDuration, c(53.628872, 42.417026, 34.638977), tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[1, ], 13.5, tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[2, ], 31.5, tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[3, ], 45)
	expect_equal(powerResult$numberOfSubjects[1, ], c(80, 80, 78.932452), tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[2, ], c(80, 80, 80))
	expect_equal(powerResult$numberOfSubjects[3, ], c(80, 80, 80))
	expect_equal(powerResult$expectedNumberOfSubjects, c(80, 80, 79.824774), tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 4.203458, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[2, ], 2.0990582, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[3, ], 1.7531447, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[1, ], 0.73032205, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[2, ], 1.2284311, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[1, ], 0.69146246, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[2, ], 0.30853754, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda2, powerResult$lambda2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$hazardRatio, powerResult$hazardRatio, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityStop, powerResult$futilityStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityPerStage, powerResult$futilityPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsEffectScale, powerResult$futilityBoundsEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsPValueScale, powerResult$futilityBoundsPValueScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
	# @refFS[Formula]{fs:PowerGroupSequentialOneSided}
	# @refFS[Formula]{fs:ShiftParameterSurvivalFreedman}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedPatientAccrual}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalFindFollowUpTime}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	powerResult <- getPowerSurvival(designGS1,
	    typeOfComputation = "Freedman",
	    pi2 = 0.4, pi1 = c(0.4, 0.5, 0.6), dropoutRate1 = 0.1, dropoutTime = 12, eventTime = 24,
	    maxNumberOfSubjects = 80, maxNumberOfEvents = 45, allocationRatioPlanned = 3
	)

	## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
	expect_equal(powerResult$median1, c(32.565971, 24, 18.155299), tolerance = 1e-07)
	expect_equal(powerResult$median2, 32.565971, tolerance = 1e-07)
	expect_equal(powerResult$lambda1, c(0.021284401, 0.028881133, 0.03817878), tolerance = 1e-07)
	expect_equal(powerResult$lambda2, 0.021284401, tolerance = 1e-07)
	expect_equal(powerResult$hazardRatio, c(1, 1.3569154, 1.7937447), tolerance = 1e-07)
	expect_equal(powerResult$accrualIntensity, 6.6666667, tolerance = 1e-07)
	expect_equal(powerResult$followUpTime, c(41.628872, 30.417026, 22.638977), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfEvents, c(29.092161, 33.256688, 34.504982), tolerance = 1e-07)
	expect_equal(powerResult$overallReject, c(0.067448723, 0.23410594, 0.44983629), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c(0.011170644, 0.033136424, 0.067729226), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c(0.030436001, 0.10902189, 0.22109606), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c(0.025842077, 0.091947627, 0.16101101), tolerance = 1e-07)
	expect_equal(powerResult$futilityStop, c(0.71047424, 0.45476178, 0.26727979), tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[1, ], c(0.30853754, 0.1715797, 0.098248524), tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[2, ], c(0.40193671, 0.28318207, 0.16903127), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, c(0.75208089, 0.59692009, 0.55610508), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[1, ], c(15.123713, 13.244904, 11.839868), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[2, ], c(32.118539, 26.401459, 22.217088), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[3, ], c(53.628872, 42.417026, 34.638977), tolerance = 1e-07)
	expect_equal(powerResult$studyDuration, c(32.017976, 30.163653, 26.008714), tolerance = 1e-07)
	expect_equal(powerResult$maxStudyDuration, c(53.628872, 42.417026, 34.638977), tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[1, ], 13.5, tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[2, ], 31.5, tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[3, ], 45)
	expect_equal(powerResult$numberOfSubjects[1, ], c(80, 80, 78.932452), tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[2, ], c(80, 80, 80))
	expect_equal(powerResult$numberOfSubjects[3, ], c(80, 80, 80))
	expect_equal(powerResult$expectedNumberOfSubjects, c(80, 80, 79.822811), tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 4.203458, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[2, ], 2.0990582, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[3, ], 1.7531447, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[1, ], 0.73032205, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[2, ], 1.2284311, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[1, ], 0.69146246, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[2, ], 0.30853754, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda2, powerResult$lambda2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$hazardRatio, powerResult$hazardRatio, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityStop, powerResult$futilityStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityPerStage, powerResult$futilityPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsEffectScale, powerResult$futilityBoundsEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsPValueScale, powerResult$futilityBoundsPValueScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
	# @refFS[Formula]{fs:PowerGroupSequentialOneSided}
	# @refFS[Formula]{fs:ShiftParameterSurvivalHsieh}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedPatientAccrual}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalFindFollowUpTime}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	powerResult <- getPowerSurvival(designGS1,
	    typeOfComputation = "HsiehFreedman",
	    pi2 = 0.4, pi1 = c(0.4, 0.5, 0.6), dropoutRate1 = 0.1, dropoutTime = 12, eventTime = 24,
	    maxNumberOfSubjects = 80, maxNumberOfEvents = 45, allocationRatioPlanned = 3
	)

	## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
	expect_equal(powerResult$median1, c(32.565971, 24, 18.155299), tolerance = 1e-07)
	expect_equal(powerResult$median2, 32.565971, tolerance = 1e-07)
	expect_equal(powerResult$lambda1, c(0.021284401, 0.028881133, 0.03817878), tolerance = 1e-07)
	expect_equal(powerResult$lambda2, 0.021284401, tolerance = 1e-07)
	expect_equal(powerResult$hazardRatio, c(1, 1.3569154, 1.7937447), tolerance = 1e-07)
	expect_equal(powerResult$accrualIntensity, 6.6666667, tolerance = 1e-07)
	expect_equal(powerResult$followUpTime, c(41.628872, 30.417026, 22.638977), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfEvents, c(29.092161, 33.473935, 34.421802), tolerance = 1e-07)
	expect_equal(powerResult$overallReject, c(0.067448723, 0.25255296, 0.52822452), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c(0.011170644, 0.03572104, 0.083721511), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c(0.030436001, 0.11810922, 0.2653086), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c(0.025842077, 0.098722701, 0.17919441), tolerance = 1e-07)
	expect_equal(powerResult$futilityStop, c(0.71047424, 0.43487767, 0.2160418), tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[1, ], c(0.30853754, 0.16308496, 0.080152238), tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[2, ], c(0.40193671, 0.27179271, 0.13588956), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, c(0.75208089, 0.58870793, 0.56507191), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[1, ], c(15.123713, 13.244904, 11.839868), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[2, ], c(32.118539, 26.401459, 22.217088), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[3, ], c(53.628872, 42.417026, 34.638977), tolerance = 1e-07)
	expect_equal(powerResult$studyDuration, c(32.017976, 30.372933, 25.919163), tolerance = 1e-07)
	expect_equal(powerResult$maxStudyDuration, c(53.628872, 42.417026, 34.638977), tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[1, ], 13.5, tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[2, ], 31.5, tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[3, ], 45)
	expect_equal(powerResult$numberOfSubjects[1, ], c(80, 80, 78.932452), tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[2, ], c(80, 80, 80))
	expect_equal(powerResult$numberOfSubjects[3, ], c(80, 80, 80))
	expect_equal(powerResult$expectedNumberOfSubjects, c(80, 80, 79.825057), tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 4.203458, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[2, ], 2.0990582, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[3, ], 1.7531447, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[1, ], 0.73032205, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[2, ], 1.2284311, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[1, ], 0.69146246, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[2, ], 0.30853754, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda2, powerResult$lambda2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$hazardRatio, powerResult$hazardRatio, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityStop, powerResult$futilityStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityPerStage, powerResult$futilityPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsEffectScale, powerResult$futilityBoundsEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsPValueScale, powerResult$futilityBoundsPValueScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
	# @refFS[Formula]{fs:PowerGroupSequentialOneSided}
	# @refFS[Formula]{fs:ShiftParameterSurvivalSchoenfeld}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedPatientAccrual}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalFindFollowUpTime}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	powerResult <- getPowerSurvival(designGS1,
	    lambda2 = 0.04, thetaH0 = 1.25,
	    hazardRatio = 0.8, directionUpper = FALSE,
	    maxNumberOfSubjects = 200, maxNumberOfEvents = 65, allocationRatioPlanned = 3
	)

	## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
	expect_equal(powerResult$median1, 21.660849, tolerance = 1e-07)
	expect_equal(powerResult$median2, 17.32868, tolerance = 1e-07)
	expect_equal(powerResult$lambda1, 0.032, tolerance = 1e-07)
	expect_equal(powerResult$accrualIntensity, 16.666667, tolerance = 1e-07)
	expect_equal(powerResult$followUpTime, 5.7883102, tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfEvents, 49.818428, tolerance = 1e-07)
	expect_equal(powerResult$overallReject, 0.49283375, tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.076192913), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.24509523), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c("stage = 3" = 0.17154561), tolerance = 1e-07)
	expect_equal(powerResult$futilityStop, 0.2383697, tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[1, ], c("stage = 1" = 0.087970326), tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[2, ], c("stage = 2" = 0.15039938), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, 0.55965784, tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[1, ], 8.7091306, tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[2, ], 13.807185, tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[3, ], 17.78831, tolerance = 1e-07)
	expect_equal(powerResult$studyDuration, 14.723329, tolerance = 1e-07)
	expect_equal(powerResult$maxStudyDuration, 17.78831, tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[1, ], 19.5, tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[2, ], 45.5, tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[3, ], 65)
	expect_equal(powerResult$numberOfSubjects[1, ], 145.15218, tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[2, ], 200)
	expect_equal(powerResult$numberOfSubjects[3, ], 200)
	expect_equal(powerResult$expectedNumberOfSubjects, 190.996, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 0.37847558, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[2, ], 0.67448058, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[3, ], 0.78350426, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[1, ], 1.623577, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[2, ], 1.0533329, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[1, ], 0.69146246, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[2, ], 0.30853754, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityStop, powerResult$futilityStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityPerStage, powerResult$futilityPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsEffectScale, powerResult$futilityBoundsEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsPValueScale, powerResult$futilityBoundsPValueScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
	# @refFS[Formula]{fs:PowerGroupSequentialOneSided}
	# @refFS[Formula]{fs:ShiftParameterSurvivalSchoenfeld}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedPatientAccrual}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalFindFollowUpTime}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	powerResult <- getPowerSurvival(designGS1,
	    lambda2 = 0.04, thetaH0 = 0.8,
	    hazardRatio = seq(0.8, 1.4, 0.2), directionUpper = TRUE,
	    maxNumberOfSubjects = 200, maxNumberOfEvents = 65, allocationRatioPlanned = 1
	)

	## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
	expect_equal(powerResult$median1, c(21.660849, 17.32868, 14.440566, 12.377628), tolerance = 1e-07)
	expect_equal(powerResult$median2, 17.32868, tolerance = 1e-07)
	expect_equal(powerResult$lambda1, c(0.032, 0.04, 0.048, 0.056), tolerance = 1e-07)
	expect_equal(powerResult$accrualIntensity, 16.666667, tolerance = 1e-07)
	expect_equal(powerResult$followUpTime, c(5.1617391, 4.0656056, 3.2120436, 2.5256004), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfEvents, c(42.02201, 48.445748, 49.742518, 47.47852), tolerance = 1e-07)
	expect_equal(powerResult$overallReject, c(0.067448723, 0.25860493, 0.52208361, 0.74266051), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c(0.011170644, 0.03658032, 0.082375002, 0.14710823), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c(0.030436001, 0.12110923, 0.26177073, 0.39724295), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c(0.025842077, 0.10091538, 0.17793787, 0.19830932), tolerance = 1e-07)
	expect_equal(powerResult$futilityStop, c(0.71047424, 0.42856456, 0.21982747, 0.10295201), tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[1, ], c(0.30853754, 0.1604311, 0.08147133, 0.041317452), tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[2, ], c(0.40193671, 0.26813346, 0.13835614, 0.061634556), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, c(0.75208089, 0.58625412, 0.5639732, 0.6473032), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[1, ], c(8.4767885, 8.0592408, 7.7076518, 7.4060255), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[2, ], c(13.399188, 12.692623, 12.137705, 11.68467), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[3, ], c(17.161739, 16.065606, 15.212044, 14.5256), tolerance = 1e-07)
	expect_equal(powerResult$studyDuration, c(12.758265, 13.175351, 12.752351, 11.880451), tolerance = 1e-07)
	expect_equal(powerResult$maxStudyDuration, c(17.161739, 16.065606, 15.212044, 14.5256), tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[1, ], 19.5, tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[2, ], 45.5, tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[3, ], 65)
	expect_equal(powerResult$numberOfSubjects[1, ], c(141.27981, 134.32068, 128.46086, 123.43376), tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[2, ], c(200, 200, 200, 194.7445), tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[3, ], c(200, 200, 200, 200))
	expect_equal(powerResult$expectedNumberOfSubjects, c(181.22667, 187.06042, 188.27858, 183.16132), tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 2.2513678, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[2, ], 1.3650021, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[3, ], 1.1988902, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[1, ], 0.63788392, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[2, ], 0.92784212, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[1, ], 0.69146246, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[2, ], 0.30853754, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityStop, powerResult$futilityStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityPerStage, powerResult$futilityPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsEffectScale, powerResult$futilityBoundsEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsPValueScale, powerResult$futilityBoundsPValueScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
	# @refFS[Formula]{fs:PowerGroupSequentialOneSided}
	# @refFS[Formula]{fs:ShiftParameterSurvivalSchoenfeld}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedPatientAccrual}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalFindFollowUpTime}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	powerResult <- getPowerSurvival(designGS1,
	    eventTime = 120, pi2 = 0.4,
	    thetaH0 = 0.8, hazardRatio = seq(0.8, 1.4, 0.2), directionUpper = TRUE,
	    maxNumberOfSubjects = 200, maxNumberOfEvents = 65, allocationRatioPlanned = 1
	)

	## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
	expect_equal(powerResult$pi1, c(0.33546019, 0.4, 0.45827173, 0.51088413), tolerance = 1e-07)
	expect_equal(powerResult$median1, c(203.53732, 162.82985, 135.69154, 116.30704), tolerance = 1e-07)
	expect_equal(powerResult$median2, 162.82985, tolerance = 1e-07)
	expect_equal(powerResult$lambda1, c(0.0034055042, 0.0042568802, 0.0051082562, 0.0059596323), tolerance = 1e-07)
	expect_equal(powerResult$lambda2, 0.0042568802, tolerance = 1e-07)
	expect_equal(powerResult$accrualIntensity, 16.666667, tolerance = 1e-07)
	expect_equal(powerResult$followUpTime, c(96.86335, 86.356678, 78.102375, 71.398147), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfEvents, c(42.02201, 48.445748, 49.742518, 47.47852), tolerance = 1e-07)
	expect_equal(powerResult$overallReject, c(0.067448723, 0.25860493, 0.52208361, 0.74266051), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c(0.011170644, 0.03658032, 0.082375002, 0.14710823), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c(0.030436001, 0.12110923, 0.26177073, 0.39724295), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c(0.025842077, 0.10091538, 0.17793787, 0.19830932), tolerance = 1e-07)
	expect_equal(powerResult$futilityStop, c(0.71047424, 0.42856456, 0.21982747, 0.10295201), tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[1, ], c(0.30853754, 0.1604311, 0.08147133, 0.041317452), tolerance = 1e-07)
	expect_equal(powerResult$futilityPerStage[2, ], c(0.40193671, 0.26813346, 0.13835614, 0.061634556), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, c(0.75208089, 0.58625412, 0.5639732, 0.6473032), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[1, ], c(32.816894, 30.124548, 27.945787, 26.142615), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[2, ], c(73.505015, 66.662265, 61.211479, 56.744296), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[3, ], c(108.86335, 98.356678, 90.102375, 83.398147), tolerance = 1e-07)
	expect_equal(powerResult$studyDuration, c(69.262697, 72.57735, 68.358222, 60.378881), tolerance = 1e-07)
	expect_equal(powerResult$maxStudyDuration, c(108.86335, 98.356678, 90.102375, 83.398147), tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[1, ], 19.5, tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[2, ], 45.5, tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[3, ], 65)
	expect_equal(powerResult$numberOfSubjects[1, ], c(200, 200, 200, 200))
	expect_equal(powerResult$numberOfSubjects[2, ], c(200, 200, 200, 200))
	expect_equal(powerResult$numberOfSubjects[3, ], c(200, 200, 200, 200))
	expect_equal(powerResult$expectedNumberOfSubjects, c(200, 200, 200, 200))
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 2.2513678, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[2, ], 1.3650021, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[3, ], 1.1988902, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[1, ], 0.63788392, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsEffectScale[2, ], 0.92784212, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[1, ], 0.69146246, tolerance = 1e-07)
	expect_equal(powerResult$futilityBoundsPValueScale[2, ], 0.30853754, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$pi1, powerResult$pi1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda2, powerResult$lambda2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityStop, powerResult$futilityStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityPerStage, powerResult$futilityPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsEffectScale, powerResult$futilityBoundsEffectScale, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$futilityBoundsPValueScale, powerResult$futilityBoundsPValueScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getPowerSurvival': Power calculation of survival designs for two-sided group sequential design", {

	designGS2 <- getDesignGroupSequential(
	    informationRates = c(0.3, 0.7, 1), alpha = 0.11,
	    sided = 2, beta = 0.1, typeOfDesign = "WT", deltaWT = 0.32
	)

	# @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
	# @refFS[Formula]{fs:PowerGroupSequentialTwoSided}
	# @refFS[Formula]{fs:ShiftParameterSurvivalSchoenfeld}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedPatientAccrual}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalFindFollowUpTime}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	powerResult <- getPowerSurvival(designGS2,
	    pi2 = 0.4, pi1 = c(0.2, 0.4, 0.6),
	    dropoutRate1 = 0.1, dropoutTime = 12, eventTime = 24,
	    maxNumberOfSubjects = 180, maxNumberOfEvents = 55, allocationRatioPlanned = 0.3
	)

	## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
	expect_equal(powerResult$median1, c(74.550809, 32.565971, 18.155299), tolerance = 1e-07)
	expect_equal(powerResult$median2, 32.565971, tolerance = 1e-07)
	expect_equal(powerResult$lambda1, c(0.009297648, 0.021284401, 0.03817878), tolerance = 1e-07)
	expect_equal(powerResult$lambda2, 0.021284401, tolerance = 1e-07)
	expect_equal(powerResult$hazardRatio, c(0.43682921, 1, 1.7937447), tolerance = 1e-07)
	expect_equal(powerResult$accrualIntensity, 15)
	expect_equal(powerResult$followUpTime, c(14.361102, 11.603566, 9.1966475), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfEvents, c(40.275667, 53.258703, 46.484493), tolerance = 1e-07)
	expect_equal(powerResult$overallReject, c(0.80955491, 0.11, 0.5536311), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c(0.20812766, 0.025692757, 0.10981107), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c(0.4067526, 0.045583354, 0.25986553), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c(0.19467465, 0.038723888, 0.1839545), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, c(0.61488026, 0.071276112, 0.3696766), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[1, ], c(11.329861, 10.584003, 9.8033045), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[2, ], c(19.345216, 17.58497, 15.96575), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[3, ], c(26.361102, 23.603566, 21.196648), tolerance = 1e-07)
	expect_equal(powerResult$studyDuration, c(20.378955, 22.994709, 18.586202), tolerance = 1e-07)
	expect_equal(powerResult$maxStudyDuration, c(26.361102, 23.603566, 21.196648), tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[1, ], 16.5, tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[2, ], 38.5, tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[3, ], 55)
	expect_equal(powerResult$numberOfSubjects[1, ], c(169.94792, 158.76005, 147.04957), tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[2, ], c(180, 180, 180))
	expect_equal(powerResult$numberOfSubjects[3, ], c(180, 180, 180))
	expect_equal(powerResult$expectedNumberOfSubjects, c(177.90788, 179.45429, 176.38168), tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[1, ], 0.27158358, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[2, ], 0.48064547, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[3, ], 0.5627937, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[1, ], 3.6821078, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[2, ], 2.0805356, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[3, ], 1.77685, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[1, ], 0.025692757, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[2, ], 0.055458318, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[3, ], 0.072467622, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda2, powerResult$lambda2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$hazardRatio, powerResult$hazardRatio, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScaleLower, powerResult$criticalValuesEffectScaleLower, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScaleUpper, powerResult$criticalValuesEffectScaleUpper, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesPValueScale, powerResult$criticalValuesPValueScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
	# @refFS[Formula]{fs:PowerGroupSequentialTwoSided}
	# @refFS[Formula]{fs:ShiftParameterSurvivalFreedman}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedPatientAccrual}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalFindFollowUpTime}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	powerResult <- getPowerSurvival(designGS2,
	    typeOfComputation = "Freedman",
	    pi2 = 0.4, pi1 = c(0.2, 0.4, 0.6), dropoutRate1 = 0.1, dropoutTime = 12, eventTime = 24,
	    maxNumberOfSubjects = 180, maxNumberOfEvents = 55, allocationRatioPlanned = 0.3
	)

	## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
	expect_equal(powerResult$median1, c(74.550809, 32.565971, 18.155299), tolerance = 1e-07)
	expect_equal(powerResult$median2, 32.565971, tolerance = 1e-07)
	expect_equal(powerResult$lambda1, c(0.009297648, 0.021284401, 0.03817878), tolerance = 1e-07)
	expect_equal(powerResult$lambda2, 0.021284401, tolerance = 1e-07)
	expect_equal(powerResult$hazardRatio, c(0.43682921, 1, 1.7937447), tolerance = 1e-07)
	expect_equal(powerResult$accrualIntensity, 15)
	expect_equal(powerResult$followUpTime, c(14.361102, 11.603566, 9.1966475), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfEvents, c(44.992896, 53.258703, 44.408918), tolerance = 1e-07)
	expect_equal(powerResult$overallReject, c(0.62751278, 0.11, 0.65422406), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c(0.13113454, 0.025692757, 0.13983652), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c(0.30051056, 0.045583354, 0.31559857), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c(0.19586767, 0.038723888, 0.19878897), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, c(0.4316451, 0.071276112, 0.45543509), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[1, ], c(11.329861, 10.584003, 9.8033045), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[2, ], c(19.345216, 17.58497, 15.96575), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[3, ], c(26.361102, 23.603566, 21.196648), tolerance = 1e-07)
	expect_equal(powerResult$studyDuration, c(22.281639, 22.994709, 17.952578), tolerance = 1e-07)
	expect_equal(powerResult$maxStudyDuration, c(26.361102, 23.603566, 21.196648), tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[1, ], 16.5, tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[2, ], 38.5, tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[3, ], 55)
	expect_equal(powerResult$numberOfSubjects[1, ], c(169.94792, 158.76005, 147.04957), tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[2, ], c(180, 180, 180))
	expect_equal(powerResult$numberOfSubjects[3, ], c(180, 180, 180))
	expect_equal(powerResult$expectedNumberOfSubjects, c(178.68182, 179.45429, 175.39233), tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[1, ], 0.27158358, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[2, ], 0.48064547, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[3, ], 0.5627937, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[1, ], 3.6821078, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[2, ], 2.0805356, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[3, ], 1.77685, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[1, ], 0.025692757, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[2, ], 0.055458318, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[3, ], 0.072467622, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda2, powerResult$lambda2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$hazardRatio, powerResult$hazardRatio, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScaleLower, powerResult$criticalValuesEffectScaleLower, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScaleUpper, powerResult$criticalValuesEffectScaleUpper, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesPValueScale, powerResult$criticalValuesPValueScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
	# @refFS[Formula]{fs:PowerGroupSequentialTwoSided}
	# @refFS[Formula]{fs:ShiftParameterSurvivalHsieh}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedPatientAccrual}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalFindFollowUpTime}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	powerResult <- getPowerSurvival(designGS2,
	    typeOfComputation = "HsiehFreedman",
	    pi2 = 0.4, pi1 = c(0.2, 0.4, 0.6), dropoutRate1 = 0.1, dropoutTime = 12, eventTime = 24,
	    maxNumberOfSubjects = 180, maxNumberOfEvents = 55, allocationRatioPlanned = 0.3
	)

	## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
	expect_equal(powerResult$median1, c(74.550809, 32.565971, 18.155299), tolerance = 1e-07)
	expect_equal(powerResult$median2, 32.565971, tolerance = 1e-07)
	expect_equal(powerResult$lambda1, c(0.009297648, 0.021284401, 0.03817878), tolerance = 1e-07)
	expect_equal(powerResult$lambda2, 0.021284401, tolerance = 1e-07)
	expect_equal(powerResult$hazardRatio, c(0.43682921, 1, 1.7937447), tolerance = 1e-07)
	expect_equal(powerResult$accrualIntensity, 15)
	expect_equal(powerResult$followUpTime, c(14.361102, 11.603566, 9.1966475), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfEvents, c(41.467466, 53.258703, 46.846888), tolerance = 1e-07)
	expect_equal(powerResult$overallReject, c(0.77062516, 0.11, 0.53442991), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c(0.18711904, 0.025692757, 0.10481397), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c(0.38354247, 0.045583354, 0.24956205), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c(0.19996364, 0.038723888, 0.18005389), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, c(0.57066151, 0.071276112, 0.35437602), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[1, ], c(11.329861, 10.584003, 9.8033045), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[2, ], c(19.345216, 17.58497, 15.96575), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[3, ], c(26.361102, 23.603566, 21.196648), tolerance = 1e-07)
	expect_equal(powerResult$studyDuration, c(20.85758, 22.994709, 18.697033), tolerance = 1e-07)
	expect_equal(powerResult$maxStudyDuration, c(26.361102, 23.603566, 21.196648), tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[1, ], 16.5, tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[2, ], 38.5, tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[3, ], 55)
	expect_equal(powerResult$numberOfSubjects[1, ], c(169.94792, 158.76005, 147.04957), tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[2, ], c(180, 180, 180))
	expect_equal(powerResult$numberOfSubjects[3, ], c(180, 180, 180))
	expect_equal(powerResult$expectedNumberOfSubjects, c(178.11906, 179.45429, 176.54633), tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[1, ], 0.27158358, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[2, ], 0.48064547, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[3, ], 0.5627937, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[1, ], 3.6821078, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[2, ], 2.0805356, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[3, ], 1.77685, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[1, ], 0.025692757, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[2, ], 0.055458318, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[3, ], 0.072467622, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda2, powerResult$lambda2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$hazardRatio, powerResult$hazardRatio, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScaleLower, powerResult$criticalValuesEffectScaleLower, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScaleUpper, powerResult$criticalValuesEffectScaleUpper, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesPValueScale, powerResult$criticalValuesPValueScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
	# @refFS[Formula]{fs:PowerGroupSequentialTwoSided}
	# @refFS[Formula]{fs:ShiftParameterSurvivalSchoenfeld}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedPatientAccrual}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalFindFollowUpTime}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	powerResult <- getPowerSurvival(designGS2,
	    lambda2 = 0.04, hazardRatio = c(0.4, 1, 1.8),
	    dropoutRate1 = 0.1, dropoutTime = 12,
	    maxNumberOfSubjects = 180, maxNumberOfEvents = 55, allocationRatioPlanned = 0.3
	)

	## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
	expect_equal(powerResult$median1, c(43.321699, 17.32868, 9.6270442), tolerance = 1e-07)
	expect_equal(powerResult$median2, 17.32868, tolerance = 1e-07)
	expect_equal(powerResult$lambda1, c(0.016, 0.04, 0.072), tolerance = 1e-07)
	expect_equal(powerResult$accrualIntensity, 15)
	expect_equal(powerResult$followUpTime, c(5.0603074, 3.4618446, 2.2113432), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfEvents, c(37.895698, 53.258703, 46.404972), tolerance = 1e-07)
	expect_equal(powerResult$overallReject, c(0.8740886, 0.11, 0.55777827), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c(0.25384788, 0.025692757, 0.11091682), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c(0.44431262, 0.045583354, 0.26210486), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c(0.17592811, 0.038723888, 0.18475659), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, c(0.6981605, 0.071276112, 0.37302168), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[1, ], c(8.4193774, 7.8220347, 7.2555625), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[2, ], c(13.316286, 12.307189, 11.466641), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[3, ], c(17.060307, 15.461845, 14.211343), tolerance = 1e-07)
	expect_equal(powerResult$studyDuration, c(13.20331, 15.121757, 12.72043), tolerance = 1e-07)
	expect_equal(powerResult$maxStudyDuration, c(17.060307, 15.461845, 14.211343), tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[1, ], 16.5, tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[2, ], 38.5, tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[3, ], 55)
	expect_equal(powerResult$numberOfSubjects[1, ], c(126.29066, 117.33052, 108.83344), tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[2, ], c(180, 180, 171.99961), tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[3, ], c(180, 180, 180))
	expect_equal(powerResult$expectedNumberOfSubjects, c(166.366, 178.38985, 170.00949), tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[1, ], 0.27158358, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[2, ], 0.48064547, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[3, ], 0.5627937, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[1, ], 3.6821078, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[2, ], 2.0805356, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[3, ], 1.77685, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[1, ], 0.025692757, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[2, ], 0.055458318, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[3, ], 0.072467622, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScaleLower, powerResult$criticalValuesEffectScaleLower, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScaleUpper, powerResult$criticalValuesEffectScaleUpper, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesPValueScale, powerResult$criticalValuesPValueScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
	# @refFS[Formula]{fs:PowerGroupSequentialTwoSided}
	# @refFS[Formula]{fs:ShiftParameterSurvivalFreedman}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedPatientAccrual}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalFindFollowUpTime}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	powerResult <- getPowerSurvival(designGS2,
	    typeOfComputation = "Freedman",
	    lambda2 = 0.04, hazardRatio = c(0.4, 1, 1.8), dropoutRate1 = 0.1, dropoutTime = 12,
	    maxNumberOfSubjects = 180, maxNumberOfEvents = 55, allocationRatioPlanned = 0.3
	)

	## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
	expect_equal(powerResult$median1, c(43.321699, 17.32868, 9.6270442), tolerance = 1e-07)
	expect_equal(powerResult$median2, 17.32868, tolerance = 1e-07)
	expect_equal(powerResult$lambda1, c(0.016, 0.04, 0.072), tolerance = 1e-07)
	expect_equal(powerResult$accrualIntensity, 15)
	expect_equal(powerResult$followUpTime, c(5.0603074, 3.4618446, 2.2113432), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfEvents, c(43.761896, 53.258703, 44.296935), tolerance = 1e-07)
	expect_equal(powerResult$overallReject, c(0.68239647, 0.11, 0.65920633), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c(0.14972738, 0.025692757, 0.14152926), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c(0.33173334, 0.045583354, 0.31843565), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c(0.20093576, 0.038723888, 0.19924141), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, c(0.48146072, 0.071276112, 0.45996492), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[1, ], c(8.4193774, 7.8220347, 7.2555625), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[2, ], c(13.316286, 12.307189, 11.466641), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[3, ], c(17.060307, 15.461845, 14.211343), tolerance = 1e-07)
	expect_equal(powerResult$studyDuration, c(14.524507, 15.121757, 12.352885), tolerance = 1e-07)
	expect_equal(powerResult$maxStudyDuration, c(17.060307, 15.461845, 14.211343), tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[1, ], 16.5, tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[2, ], 38.5, tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[3, ], 55)
	expect_equal(powerResult$numberOfSubjects[1, ], c(126.29066, 117.33052, 108.83344), tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[2, ], c(180, 180, 171.99961), tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[3, ], c(180, 180, 180))
	expect_equal(powerResult$expectedNumberOfSubjects, c(171.95824, 178.38985, 167.38024), tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[1, ], 0.27158358, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[2, ], 0.48064547, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[3, ], 0.5627937, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[1, ], 3.6821078, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[2, ], 2.0805356, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[3, ], 1.77685, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[1, ], 0.025692757, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[2, ], 0.055458318, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[3, ], 0.072467622, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScaleLower, powerResult$criticalValuesEffectScaleLower, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScaleUpper, powerResult$criticalValuesEffectScaleUpper, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesPValueScale, powerResult$criticalValuesPValueScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
	# @refFS[Formula]{fs:PowerGroupSequentialTwoSided}
	# @refFS[Formula]{fs:ShiftParameterSurvivalHsieh}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedPatientAccrual}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalFindFollowUpTime}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	powerResult <- getPowerSurvival(designGS2,
	    typeOfComputation = "HsiehFreedman",
	    lambda2 = 0.04, hazardRatio = c(0.4, 1, 1.8), dropoutRate1 = 0.1, dropoutTime = 12,
	    maxNumberOfSubjects = 180, maxNumberOfEvents = 55, allocationRatioPlanned = 0.3
	)

	## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
	expect_equal(powerResult$median1, c(43.321699, 17.32868, 9.6270442), tolerance = 1e-07)
	expect_equal(powerResult$median2, 17.32868, tolerance = 1e-07)
	expect_equal(powerResult$lambda1, c(0.016, 0.04, 0.072), tolerance = 1e-07)
	expect_equal(powerResult$accrualIntensity, 15)
	expect_equal(powerResult$followUpTime, c(5.0603074, 3.4618446, 2.2113432), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfEvents, c(39.493229, 53.258703, 46.77542), tolerance = 1e-07)
	expect_equal(powerResult$overallReject, c(0.83266548, 0.11, 0.53825584), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c(0.2225769, 0.025692757, 0.10579404), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c(0.42045819, 0.045583354, 0.25160664), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c(0.18963039, 0.038723888, 0.18085515), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, c(0.64303509, 0.071276112, 0.35740069), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[1, ], c(8.4193774, 7.8220347, 7.2555625), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[2, ], c(13.316286, 12.307189, 11.466641), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[3, ], c(17.060307, 15.461845, 14.211343), tolerance = 1e-07)
	expect_equal(powerResult$studyDuration, c(13.562832, 15.121757, 12.784878), tolerance = 1e-07)
	expect_equal(powerResult$maxStudyDuration, c(17.060307, 15.461845, 14.211343), tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[1, ], 16.5, tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[2, ], 38.5, tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[3, ], 55)
	expect_equal(powerResult$numberOfSubjects[1, ], c(126.29066, 117.33052, 108.83344), tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[2, ], c(180, 180, 171.99961), tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[3, ], c(180, 180, 180))
	expect_equal(powerResult$expectedNumberOfSubjects, c(168.04554, 178.38985, 170.45805), tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[1, ], 0.27158358, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[2, ], 0.48064547, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleLower[3, ], 0.5627937, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[1, ], 3.6821078, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[2, ], 2.0805356, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScaleUpper[3, ], 1.77685, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[1, ], 0.025692757, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[2, ], 0.055458318, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesPValueScale[3, ], 0.072467622, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScaleLower, powerResult$criticalValuesEffectScaleLower, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScaleUpper, powerResult$criticalValuesEffectScaleUpper, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesPValueScale, powerResult$criticalValuesPValueScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	###################################################################################################
	###################################################################################################

})

context("Testing the Power Calculation of Survival Designs for Other Parameter Variants")


test_that("'getPowerSurvival': Four stage O'Brien and Fleming group sequential design with minimum required definitions, pi1 = c(0.4, 0.5, 0.6) and pi2 = 0.2 at event time 12, accrual time 12 and follow-up time 6 as default", {
	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
	# @refFS[Formula]{fs:PowerGroupSequentialOneSided}
	# @refFS[Formula]{fs:ShiftParameterSurvivalSchoenfeld}
	powerResult <- getPowerSurvival(
	    design = getDesignGroupSequential(kMax = 4),
	    maxNumberOfEvents = 40, maxNumberOfSubjects = 200
	)

	## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
	expect_equal(powerResult$median1, c(37.275405, 23.320299, 16.282985, 12), tolerance = 1e-07)
	expect_equal(powerResult$median2, 37.275405, tolerance = 1e-07)
	expect_equal(powerResult$lambda1, c(0.018595296, 0.029722912, 0.042568802, 0.057762265), tolerance = 1e-07)
	expect_equal(powerResult$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(powerResult$hazardRatio, c(1, 1.5984103, 2.2892242, 3.1062837), tolerance = 1e-07)
	expect_equal(powerResult$accrualIntensity, 16.666667, tolerance = 1e-07)
	expect_equal(powerResult$followUpTime, c(6.1115255, 3.442577, 1.6316894, 0.30440109), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfEvents, c(39.87408, 38.142534, 33.62741, 28.346513), tolerance = 1e-07)
	expect_equal(powerResult$overallReject, c(0.025, 0.30882929, 0.73475105, 0.94374207), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c(2.5763449e-05, 0.00047146778, 0.0030806507, 0.012020122), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c(0.0020845834, 0.034441261, 0.15314753, 0.35953485), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[3, ], c(0.0083455469, 0.11544971, 0.32172195, 0.41021864), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[4, ], c(0.014544106, 0.15846685, 0.25680093, 0.16196846), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, c(0.010455897, 0.15036244, 0.47795013, 0.78177362), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[1, ], c(8.2382885, 7.2643376, 6.5021817, 5.8683997), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[2, ], c(11.775158, 10.405299, 9.3411982, 8.4606249), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[3, ], c(14.851313, 12.90759, 11.580651, 10.517763), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[4, ], c(18.111525, 15.442577, 13.631689, 12.304401), tolerance = 1e-07)
	expect_equal(powerResult$studyDuration, c(18.070854, 14.972567, 12.292784, 10.112156), tolerance = 1e-07)
	expect_equal(powerResult$maxStudyDuration, c(18.111525, 15.442577, 13.631689, 12.304401), tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[1, ], 10)
	expect_equal(powerResult$eventsPerStage[2, ], 20)
	expect_equal(powerResult$eventsPerStage[3, ], 30)
	expect_equal(powerResult$eventsPerStage[4, ], 40)
	expect_equal(powerResult$numberOfSubjects[1, ], c(137.30481, 121.07229, 108.36969, 97.806661), tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[2, ], c(196.25264, 173.42164, 155.68664, 141.01041), tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[3, ], c(200, 200, 193.01085, 175.29605), tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[4, ], c(200, 200, 200, 200))
	expect_equal(powerResult$expectedNumberOfSubjects, c(199.99057, 199.0474, 190.68267, 167.42879), tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 12.942983, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[2, ], 3.5976357, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[3, ], 2.3478921, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[4, ], 1.8967435, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda2, powerResult$lambda2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$hazardRatio, powerResult$hazardRatio, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getPowerSurvival': For fixed sample design, determine necessary accrual time if 200 subjects and 30 subjects per time unit can be recruited", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
	# @refFS[Formula]{fs:ShiftParameterSurvivalSchoenfeld}
	# @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
	powerResult <- getPowerSurvival(
	    maxNumberOfEvents = 40,
	    accrualTime = c(0), accrualIntensity = 30, maxNumberOfSubjects = 200
	)

	## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
	expect_equal(powerResult$median1, c(37.275405, 23.320299, 16.282985, 12), tolerance = 1e-07)
	expect_equal(powerResult$median2, 37.275405, tolerance = 1e-07)
	expect_equal(powerResult$lambda1, c(0.018595296, 0.029722912, 0.042568802, 0.057762265), tolerance = 1e-07)
	expect_equal(powerResult$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(powerResult$hazardRatio, c(1, 1.5984103, 2.2892242, 3.1062837), tolerance = 1e-07)
	expect_equal(powerResult$followUpTime, c(8.7010979, 6.004962, 4.1561659, 2.779256), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfEvents, c(40, 40, 40, 40))
	expect_equal(powerResult$overallReject, c(0.025, 0.31674317, 0.74507635, 0.94783846), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[1, ], c(15.367765, 12.671629, 10.822833, 9.4459226), tolerance = 1e-07)
	expect_equal(powerResult$studyDuration, c(15.367765, 12.671629, 10.822833, 9.4459226), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfSubjects, c(200, 200, 200, 200))
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 1.8585471, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda2, powerResult$lambda2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$hazardRatio, powerResult$hazardRatio, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getPowerSurvival': Determine necessary accrual time if 200 subjects and if the first 6 time units 20 subjects per time unit can be recruited, then 30 subjects per time unit", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
	# @refFS[Formula]{fs:ShiftParameterSurvivalSchoenfeld}
	# @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
	powerResult <- getPowerSurvival(
	    maxNumberOfEvents = 40,
	    accrualTime = c(0, 6), accrualIntensity = c(20, 30), maxNumberOfSubjects = 200
	)

	## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
	expect_equal(powerResult$median1, c(37.275405, 23.320299, 16.282985, 12), tolerance = 1e-07)
	expect_equal(powerResult$median2, 37.275405, tolerance = 1e-07)
	expect_equal(powerResult$lambda1, c(0.018595296, 0.029722912, 0.042568802, 0.057762265), tolerance = 1e-07)
	expect_equal(powerResult$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(powerResult$hazardRatio, c(1, 1.5984103, 2.2892242, 3.1062837), tolerance = 1e-07)
	expect_equal(powerResult$totalAccrualTime, 8.6666667, tolerance = 1e-07)
	expect_equal(powerResult$followUpTime, c(8.127286, 5.4402735, 3.6040872, 2.2435211), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfEvents, c(40, 40, 40, 40))
	expect_equal(powerResult$overallReject, c(0.025, 0.31674317, 0.74507635, 0.94783846), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[1, ], c(16.793953, 14.10694, 12.270754, 10.910188), tolerance = 1e-07)
	expect_equal(powerResult$studyDuration, c(16.793953, 14.10694, 12.270754, 10.910188), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfSubjects, c(200, 200, 200, 200))
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 1.8585471, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda2, powerResult$lambda2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$hazardRatio, powerResult$hazardRatio, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$totalAccrualTime, powerResult$totalAccrualTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getPowerSurvival': Determine maximum number of Subjects if the first 6 time units 20 subjects per time unit can be recruited, and after 10 time units 30 subjects per time unit", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
	# @refFS[Formula]{fs:ShiftParameterSurvivalSchoenfeld}
	# @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
	powerResult <- getPowerSurvival(
	    maxNumberOfEvents = 40,
	    accrualTime = c(0, 6, 10), accrualIntensity = c(20, 30)
	)

	## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
	expect_equal(powerResult$median1, c(37.275405, 23.320299, 16.282985, 12), tolerance = 1e-07)
	expect_equal(powerResult$median2, 37.275405, tolerance = 1e-07)
	expect_equal(powerResult$lambda1, c(0.018595296, 0.029722912, 0.042568802, 0.057762265), tolerance = 1e-07)
	expect_equal(powerResult$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(powerResult$hazardRatio, c(1, 1.5984103, 2.2892242, 3.1062837), tolerance = 1e-07)
	expect_equal(powerResult$maxNumberOfSubjects, 240)
	expect_equal(powerResult$totalAccrualTime, 10)
	expect_equal(powerResult$followUpTime, c(5.3825871, 3.1889048, 1.691326, 0.58951828), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfEvents, c(40, 40, 40, 40))
	expect_equal(powerResult$overallReject, c(0.025, 0.31674317, 0.74507635, 0.94783846), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[1, ], c(15.382587, 13.188905, 11.691326, 10.589518), tolerance = 1e-07)
	expect_equal(powerResult$studyDuration, c(15.382587, 13.188905, 11.691326, 10.589518), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfSubjects, c(240, 240, 240, 240))
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 1.8585471, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda2, powerResult$lambda2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$hazardRatio, powerResult$hazardRatio, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$maxNumberOfSubjects, powerResult$maxNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$totalAccrualTime, powerResult$totalAccrualTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getPowerSurvival': Specify accrual time as a list", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
	# @refFS[Formula]{fs:ShiftParameterSurvivalSchoenfeld}
	# @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
	at <- list("0 - <6" = 20, "6 - Inf" = 30)
	powerResult <- getPowerSurvival(maxNumberOfEvents = 40, accrualTime = at, maxNumberOfSubjects = 200)

	## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
	expect_equal(powerResult$median1, c(37.275405, 23.320299, 16.282985, 12), tolerance = 1e-07)
	expect_equal(powerResult$median2, 37.275405, tolerance = 1e-07)
	expect_equal(powerResult$lambda1, c(0.018595296, 0.029722912, 0.042568802, 0.057762265), tolerance = 1e-07)
	expect_equal(powerResult$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(powerResult$hazardRatio, c(1, 1.5984103, 2.2892242, 3.1062837), tolerance = 1e-07)
	expect_equal(powerResult$totalAccrualTime, 8.6666667, tolerance = 1e-07)
	expect_equal(powerResult$followUpTime, c(8.127286, 5.4402735, 3.6040872, 2.2435211), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfEvents, c(40, 40, 40, 40))
	expect_equal(powerResult$overallReject, c(0.025, 0.31674317, 0.74507635, 0.94783846), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[1, ], c(16.793953, 14.10694, 12.270754, 10.910188), tolerance = 1e-07)
	expect_equal(powerResult$studyDuration, c(16.793953, 14.10694, 12.270754, 10.910188), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfSubjects, c(200, 200, 200, 200))
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 1.8585471, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda2, powerResult$lambda2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$hazardRatio, powerResult$hazardRatio, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$totalAccrualTime, powerResult$totalAccrualTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getPowerSurvival': Specify accrual time as a list, if maximum number of subjects need to be calculated", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
	# @refFS[Formula]{fs:ShiftParameterSurvivalSchoenfeld}
	# @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
	at <- list("0 - <6" = 20, "6 - <=10" = 30)
	powerResult <- getPowerSurvival(maxNumberOfEvents = 40, accrualTime = at)

	## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
	expect_equal(powerResult$median1, c(37.275405, 23.320299, 16.282985, 12), tolerance = 1e-07)
	expect_equal(powerResult$median2, 37.275405, tolerance = 1e-07)
	expect_equal(powerResult$lambda1, c(0.018595296, 0.029722912, 0.042568802, 0.057762265), tolerance = 1e-07)
	expect_equal(powerResult$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(powerResult$hazardRatio, c(1, 1.5984103, 2.2892242, 3.1062837), tolerance = 1e-07)
	expect_equal(powerResult$maxNumberOfSubjects, 240)
	expect_equal(powerResult$totalAccrualTime, 10)
	expect_equal(powerResult$followUpTime, c(5.3825871, 3.1889048, 1.691326, 0.58951828), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfEvents, c(40, 40, 40, 40))
	expect_equal(powerResult$overallReject, c(0.025, 0.31674317, 0.74507635, 0.94783846), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[1, ], c(15.382587, 13.188905, 11.691326, 10.589518), tolerance = 1e-07)
	expect_equal(powerResult$studyDuration, c(15.382587, 13.188905, 11.691326, 10.589518), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfSubjects, c(240, 240, 240, 240))
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 1.8585471, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda2, powerResult$lambda2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$hazardRatio, powerResult$hazardRatio, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$maxNumberOfSubjects, powerResult$maxNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$totalAccrualTime, powerResult$totalAccrualTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getPowerSurvival': Specify effect size for a two-stage group design with O'Brien & Fleming boundaries Effect size is based on event rates at specified event time, directionUpper = FALSE needs to be specified because it should be shown that hazard ratio < 1", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
	# @refFS[Formula]{fs:ShiftParameterSurvivalSchoenfeld}
	# @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
	powerResult <- getPowerSurvival(
	    design = getDesignGroupSequential(kMax = 2),
	    pi1 = 0.2, pi2 = 0.3, eventTime = 24, maxNumberOfEvents = 40, maxNumberOfSubjects = 200, directionUpper = FALSE
	)

	## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
	expect_equal(powerResult$median1, 74.550809, tolerance = 1e-07)
	expect_equal(powerResult$median2, 46.640597, tolerance = 1e-07)
	expect_equal(powerResult$lambda1, 0.009297648, tolerance = 1e-07)
	expect_equal(powerResult$lambda2, 0.014861456, tolerance = 1e-07)
	expect_equal(powerResult$hazardRatio, 0.62562161, tolerance = 1e-07)
	expect_equal(powerResult$accrualIntensity, 16.666667, tolerance = 1e-07)
	expect_equal(powerResult$followUpTime, 12.65889, tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfEvents, 39.194966, tolerance = 1e-07)
	expect_equal(powerResult$overallReject, 0.31394451, tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.04025172), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.27369279), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, 0.040251721, tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[1, ], 14.822645, tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[2, ], 24.65889, tolerance = 1e-07)
	expect_equal(powerResult$studyDuration, 24.262964, tolerance = 1e-07)
	expect_equal(powerResult$maxStudyDuration, 24.65889, tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[1, ], 20)
	expect_equal(powerResult$eventsPerStage[2, ], 40)
	expect_equal(powerResult$numberOfSubjects[1, ], 200)
	expect_equal(powerResult$numberOfSubjects[2, ], 200)
	expect_equal(powerResult$expectedNumberOfSubjects, 200)
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 0.28632231, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[2, ], 0.53509093, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda2, powerResult$lambda2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$hazardRatio, powerResult$hazardRatio, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getPowerSurvival': Effect size is based on event rate at specified event time for the reference group and hazard ratio, directionUpper = FALSE needs to be specified because it should be shown that hazard ratio < 1", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
	# @refFS[Formula]{fs:ShiftParameterSurvivalSchoenfeld}
	# @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
	powerResult <- getPowerSurvival(
	    design = getDesignGroupSequential(kMax = 2),
	    hazardRatio = 0.5, pi2 = 0.3, eventTime = 24, maxNumberOfEvents = 40, maxNumberOfSubjects = 200, directionUpper = FALSE
	)

	## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
	expect_equal(powerResult$pi1, 0.16333997, tolerance = 1e-07)
	expect_equal(powerResult$median1, 93.281194, tolerance = 1e-07)
	expect_equal(powerResult$median2, 46.640597, tolerance = 1e-07)
	expect_equal(powerResult$lambda1, 0.007430728, tolerance = 1e-07)
	expect_equal(powerResult$lambda2, 0.014861456, tolerance = 1e-07)
	expect_equal(powerResult$accrualIntensity, 16.666667, tolerance = 1e-07)
	expect_equal(powerResult$followUpTime, 14.346945, tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfEvents, 37.874505, tolerance = 1e-07)
	expect_equal(powerResult$overallReject, 0.5879328, tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.10627477), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.48165803), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, 0.10627477, tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[1, ], 15.582247, tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[2, ], 26.346945, tolerance = 1e-07)
	expect_equal(powerResult$studyDuration, 25.202929, tolerance = 1e-07)
	expect_equal(powerResult$maxStudyDuration, 26.346945, tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[1, ], 20)
	expect_equal(powerResult$eventsPerStage[2, ], 40)
	expect_equal(powerResult$numberOfSubjects[1, ], 200)
	expect_equal(powerResult$numberOfSubjects[2, ], 200)
	expect_equal(powerResult$expectedNumberOfSubjects, 200)
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 0.28632231, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[2, ], 0.53509093, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$pi1, powerResult$pi1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda2, powerResult$lambda2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getPowerSurvival': Effect size is based on hazard rate for the reference group and hazard ratio, directionUpper = FALSE needs to be specified because it should be shown that hazard ratio < 1", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
	# @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
	powerResult <- getPowerSurvival(
	    design = getDesignGroupSequential(kMax = 2),
	    hazardRatio = 0.5, lambda2 = 0.02, maxNumberOfEvents = 40, maxNumberOfSubjects = 200, directionUpper = FALSE
	)

	## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
	expect_equal(powerResult$median1, 69.314718, tolerance = 1e-07)
	expect_equal(powerResult$median2, 34.657359, tolerance = 1e-07)
	expect_equal(powerResult$lambda1, 0.01, tolerance = 1e-07)
	expect_equal(powerResult$accrualIntensity, 16.666667, tolerance = 1e-07)
	expect_equal(powerResult$followUpTime, 9.1631017, tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfEvents, 37.874505, tolerance = 1e-07)
	expect_equal(powerResult$overallReject, 0.5879328, tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.10627477), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.48165803), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, 0.10627477, tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[1, ], 13.164641, tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[2, ], 21.163102, tolerance = 1e-07)
	expect_equal(powerResult$studyDuration, 20.313067, tolerance = 1e-07)
	expect_equal(powerResult$maxStudyDuration, 21.163102, tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[1, ], 20)
	expect_equal(powerResult$eventsPerStage[2, ], 40)
	expect_equal(powerResult$numberOfSubjects[1, ], 200)
	expect_equal(powerResult$numberOfSubjects[2, ], 200)
	expect_equal(powerResult$expectedNumberOfSubjects, 200)
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 0.28632231, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[2, ], 0.53509093, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getPowerSurvival': Specification of piecewise exponential survival time and hazard ratios", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
	# @refFS[Formula]{fs:sampleSizeSurvivalExponentialPieceWiseAccrual}
	# @refFS[Formula]{fs:sampleSizeSurvivalGeneralPieceWiseAccrual}
	# @refFS[Formula]{fs:pieceWiseExponentialSurvival}
	powerResult <- getPowerSurvival(
	    design = getDesignGroupSequential(kMax = 2),
	    piecewiseSurvivalTime = c(0, 5, 10), lambda2 = c(0.01, 0.02, 0.04), hazardRatio = c(1.5, 1.8, 2), maxNumberOfEvents = 40, maxNumberOfSubjects = 200
	)

	## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
	expect_equal(powerResult$accrualIntensity, 16.666667, tolerance = 1e-07)
	expect_equal(powerResult$followUpTime, c(4.2070411, 3.5734432, 3.2068918), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfEvents, c(39.412236, 38.617073, 37.874505), tolerance = 1e-07)
	expect_equal(powerResult$overallReject, c(0.24668111, 0.45613948, 0.5879328), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c(0.0293882, 0.069146371, 0.10627477), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c(0.21729291, 0.38699311, 0.48165803), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, c(0.029388201, 0.069146372, 0.10627477), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[1, ], c(12.173669, 11.705285, 11.428161), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[2, ], c(16.207041, 15.573443, 15.206892), tolerance = 1e-07)
	expect_equal(powerResult$studyDuration, c(16.088508, 15.305974, 14.805308), tolerance = 1e-07)
	expect_equal(powerResult$maxStudyDuration, c(16.207041, 15.573443, 15.206892), tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[1, ], 20)
	expect_equal(powerResult$eventsPerStage[2, ], 40)
	expect_equal(powerResult$numberOfSubjects[1, ], c(200, 195.08808, 190.46935), tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[2, ], c(200, 200, 200))
	expect_equal(powerResult$expectedNumberOfSubjects, c(200, 199.66036, 198.98713), tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 3.4925675, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[2, ], 1.8688412, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getPowerSurvival': Specification of piecewise exponential survival time as list and hazard ratios", {

	.skipTestIfDisabled()

	# @refFS[Formula]{fs:sampleSizeSurvivalExponentialPieceWiseAccrual}
	# @refFS[Formula]{fs:sampleSizeSurvivalGeneralPieceWiseAccrual}
	# @refFS[Formula]{fs:pieceWiseExponentialSurvival}
	pws <- list("0 - <5" = 0.01, "5 - <10" = 0.02, ">=10" = 0.04)
	powerResult <- getPowerSurvival(
	    design = getDesignGroupSequential(kMax = 2),
	    piecewiseSurvivalTime = pws, hazardRatio = c(1.5, 1.8, 2), maxNumberOfEvents = 40, maxNumberOfSubjects = 200
	)

	## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
	expect_equal(powerResult$accrualIntensity, 16.666667, tolerance = 1e-07)
	expect_equal(powerResult$followUpTime, c(4.2070411, 3.5734432, 3.2068918), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfEvents, c(39.412236, 38.617073, 37.874505), tolerance = 1e-07)
	expect_equal(powerResult$overallReject, c(0.24668111, 0.45613948, 0.5879328), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c(0.0293882, 0.069146371, 0.10627477), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c(0.21729291, 0.38699311, 0.48165803), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, c(0.029388201, 0.069146372, 0.10627477), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[1, ], c(12.173669, 11.705285, 11.428161), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[2, ], c(16.207041, 15.573443, 15.206892), tolerance = 1e-07)
	expect_equal(powerResult$studyDuration, c(16.088508, 15.305974, 14.805308), tolerance = 1e-07)
	expect_equal(powerResult$maxStudyDuration, c(16.207041, 15.573443, 15.206892), tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[1, ], 20)
	expect_equal(powerResult$eventsPerStage[2, ], 40)
	expect_equal(powerResult$numberOfSubjects[1, ], c(200, 195.08808, 190.46935), tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[2, ], c(200, 200, 200))
	expect_equal(powerResult$expectedNumberOfSubjects, c(200, 199.66036, 198.98713), tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 3.4925675, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[2, ], 1.8688412, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getPowerSurvival': Specification of piecewise exponential survival time for both treatment arms", {

	.skipTestIfDisabled()

	# @refFS[Formula]{fs:sampleSizeSurvivalExponentialPieceWiseAccrual}
	# @refFS[Formula]{fs:sampleSizeSurvivalGeneralPieceWiseAccrual}
	# @refFS[Formula]{fs:pieceWiseExponentialSurvival}
	powerResult <- getPowerSurvival(
	    design = getDesignGroupSequential(kMax = 2),
	    piecewiseSurvivalTime = c(0, 5, 10), lambda2 = c(0.01, 0.02, 0.04), lambda1 = c(0.015, 0.03, 0.06), maxNumberOfEvents = 40, maxNumberOfSubjects = 200
	)

	## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
	expect_equal(powerResult$hazardRatio, 1.5, tolerance = 1e-07)
	expect_equal(powerResult$accrualIntensity, 16.666667, tolerance = 1e-07)
	expect_equal(powerResult$followUpTime, 4.2070411, tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfEvents, 39.412236, tolerance = 1e-07)
	expect_equal(powerResult$overallReject, 0.24668111, tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.0293882), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.21729291), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, 0.029388201, tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[1, ], 12.173669, tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[2, ], 16.207041, tolerance = 1e-07)
	expect_equal(powerResult$studyDuration, 16.088508, tolerance = 1e-07)
	expect_equal(powerResult$maxStudyDuration, 16.207041, tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[1, ], 20)
	expect_equal(powerResult$eventsPerStage[2, ], 40)
	expect_equal(powerResult$numberOfSubjects[1, ], 200)
	expect_equal(powerResult$numberOfSubjects[2, ], 200)
	expect_equal(powerResult$expectedNumberOfSubjects, 200)
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 3.4925675, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[2, ], 1.8688412, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$hazardRatio, powerResult$hazardRatio, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getPowerSurvival': Specification of piecewise exponential survival time as a list", {

	.skipTestIfDisabled()

	# @refFS[Formula]{fs:sampleSizeSurvivalExponentialPieceWiseAccrual}
	# @refFS[Formula]{fs:sampleSizeSurvivalGeneralPieceWiseAccrual}
	# @refFS[Formula]{fs:pieceWiseExponentialSurvival}
	pws <- list("0 - <5" = 0.01, "5 - <10" = 0.02, ">=10" = 0.04)
	powerResult <- getPowerSurvival(
	    design = getDesignGroupSequential(kMax = 2),
	    piecewiseSurvivalTime = pws, hazardRatio = c(1.5, 1.8, 2), maxNumberOfEvents = 40, maxNumberOfSubjects = 200
	)

	## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
	expect_equal(powerResult$accrualIntensity, 16.666667, tolerance = 1e-07)
	expect_equal(powerResult$followUpTime, c(4.2070411, 3.5734432, 3.2068918), tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfEvents, c(39.412236, 38.617073, 37.874505), tolerance = 1e-07)
	expect_equal(powerResult$overallReject, c(0.24668111, 0.45613948, 0.5879328), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[1, ], c(0.0293882, 0.069146371, 0.10627477), tolerance = 1e-07)
	expect_equal(powerResult$rejectPerStage[2, ], c(0.21729291, 0.38699311, 0.48165803), tolerance = 1e-07)
	expect_equal(powerResult$earlyStop, c(0.029388201, 0.069146372, 0.10627477), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[1, ], c(12.173669, 11.705285, 11.428161), tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[2, ], c(16.207041, 15.573443, 15.206892), tolerance = 1e-07)
	expect_equal(powerResult$studyDuration, c(16.088508, 15.305974, 14.805308), tolerance = 1e-07)
	expect_equal(powerResult$maxStudyDuration, c(16.207041, 15.573443, 15.206892), tolerance = 1e-07)
	expect_equal(powerResult$eventsPerStage[1, ], 20)
	expect_equal(powerResult$eventsPerStage[2, ], 40)
	expect_equal(powerResult$numberOfSubjects[1, ], c(200, 195.08808, 190.46935), tolerance = 1e-07)
	expect_equal(powerResult$numberOfSubjects[2, ], c(200, 200, 200))
	expect_equal(powerResult$expectedNumberOfSubjects, c(200, 199.66036, 198.98713), tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 3.4925675, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[2, ], 1.8688412, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getPowerSurvival': Specify effect size based on median survival times (median1 = 5, median2 = 3)", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
	# @refFS[Formula]{fs:lambdabymedian}
	powerResult <- getPowerSurvival(
	    lambda1 = log(2) / 5, lambda2 = log(2) / 3,
	    maxNumberOfEvents = 40, maxNumberOfSubjects = 200, directionUpper = FALSE
	)

	## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
	expect_equal(powerResult$median1, 5)
	expect_equal(powerResult$median2, 3)
	expect_equal(powerResult$hazardRatio, 0.6, tolerance = 1e-07)
	expect_equal(powerResult$accrualIntensity, 16.666667, tolerance = 1e-07)
	expect_equal(powerResult$followUpTime, -5.9093279, tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfEvents, 40)
	expect_equal(powerResult$overallReject, 0.36520074, tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[1, ], 6.0906721, tolerance = 1e-07)
	expect_equal(powerResult$studyDuration, 6.0906721, tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfSubjects, 101.5112, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 0.53805471, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$hazardRatio, powerResult$hazardRatio, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getPowerSurvival': Specify effect size based on median survival times of Weibull distribtion with kappa = 2 (median1 = 5, median2 = 3)", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
	# @refFS[Formula]{fs:lambdabymedian}
	powerResult <- getPowerSurvival(
	    lambda1 = getLambdaByMedian(median = 5, kappa = 2),
	    lambda2 = getLambdaByMedian(median = 3, kappa = 2),
	    kappa = 2, maxNumberOfEvents = 40,
	    maxNumberOfSubjects = 200, directionUpper = FALSE
	)

	## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
	expect_equal(powerResult$median1, 5)
	expect_equal(powerResult$median2, 3)
	expect_equal(powerResult$hazardRatio, 0.36, tolerance = 1e-07)
	expect_equal(powerResult$accrualIntensity, 16.666667, tolerance = 1e-07)
	expect_equal(powerResult$followUpTime, -5.7378582, tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfEvents, 40)
	expect_equal(powerResult$overallReject, 0.8980967, tolerance = 1e-07)
	expect_equal(powerResult$analysisTime[1, ], 6.2621418, tolerance = 1e-07)
	expect_equal(powerResult$studyDuration, 6.2621418, tolerance = 1e-07)
	expect_equal(powerResult$expectedNumberOfSubjects, 104.36903, tolerance = 1e-07)
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 0.53805471, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(powerResult), NA)))
	    expect_output(print(powerResult)$show())
	    invisible(capture.output(expect_error(summary(powerResult), NA)))
	    expect_output(summary(powerResult)$show())
	    powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
	    expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$hazardRatio, powerResult$hazardRatio, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-05)
	    expect_type(names(powerResult), "character")
	    df <- as.data.frame(powerResult)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(powerResult)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getPowerSurvival': Analysis time at last stage equals accrual time + follow-up time", {

	x1 <- getPowerSurvival(getDesignGroupSequential(typeOfDesign = "P"),
	    accrualTime = 12,
	    lambda2 = 0.005, lambda1 = 0.01,
	    maxNumberOfSubjects = 766, maxNumberOfEvents = 76
	)

	expect_equal(x1$overallReject, 1 - x1$.design$beta, tolerance = 0.01)
	expect_equal(x1$analysisTime[3], x1$accrualTime + x1$followUpTime)

	x2 <- getPowerSurvival(getDesignGroupSequential(typeOfDesign = "P"),
	    accrualTime = 12, maxNumberOfEvents = 76, maxNumberOfSubjects = 766,
	    lambda2 = 0.005, lambda1 = 0.01
	)

	expect_equal(x2$analysisTime[3], x2$accrualTime + x2$followUpTime)

	x3 <- getPowerSurvival(getDesignGroupSequential(typeOfDesign = "WT", deltaWT = 0.3),
	    accrualTime = c(0, 12, 15), accrualIntensity = c(20, 30),
	    lambda2 = 0.005, lambda1 = 0.01, maxNumberOfEvents = 76
	)

	expect_equal(x3$analysisTime[length(x3$analysisTime)], x3$accrualTime[length(x3$accrualTime)] + x3$followUpTime)

	x4 <- getPowerSurvival(getDesignGroupSequential(typeOfDesign = "WT", deltaWT = 0.3),
	    accrualTime = c(0, 12, 15), accrualIntensity = c(40, 60), maxNumberOfEvents = 76,
	    piecewiseSurvivalTime = c(0, 5), lambda2 = c(0.005, 0.01), hazardRatio = 0.8
	)

	expect_equal(x4$analysisTime[length(x4$analysisTime)], x4$accrualTime[length(x4$accrualTime)] + x4$followUpTime)
})

