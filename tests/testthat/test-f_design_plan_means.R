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
## |  File name: test-f_design_plan_means.R
## |  Creation date: 21 December 2023, 08:52:45
## |  File version: $Revision$
## |  Last changed: $Date$
## |  Last changed by: $Author$
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
	sampleSizeResult <- getSampleSizeMeans(designGS1, groups = 1, thetaH0 = 0.5, stDev = 2, 
	    normalApproximation = FALSE, alternative = 0.8)

	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 494.6455, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 98.929099, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 247.32275, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 494.6455, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
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

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:sampleSizeOneMeanVarianceKnownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageOneMean}
	sampleSizeResult <- getSampleSizeMeans(designGS1, groups = 1, thetaH0 = 0.5, stDev = 2, 
	    normalApproximation = TRUE, alternative = 0.8)

	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 492.61495, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 98.522991, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 246.30748, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 492.61495, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
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

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceUnknownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
	sampleSizeResult <- getSampleSizeMeans(designGS1, groups = 2, thetaH0 = 0, stDev = 2, 
	    normalApproximation = FALSE, alternative = 1.3, allocationRatioPlanned = 1)

	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 107.00299, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 53.501497, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 53.501497, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 21.400599, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 53.501497, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 107.00299, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
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

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceKnownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
	sampleSizeResult <- getSampleSizeMeans(designGS1, groups = 2, thetaH0 = 0, stDev = 2, 
	    normalApproximation = TRUE, alternative = 1.3, allocationRatioPlanned = 1)

	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 104.93573, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 52.467865, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 52.467865, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 20.987146, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 52.467865, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 104.93573, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
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

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceUnknownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
	sampleSizeResult <- getSampleSizeMeans(designGS1, groups = 2, thetaH0 = 0, stDev = 2, 
	    normalApproximation = FALSE, alternative = 1.3, allocationRatioPlanned = 3)

	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
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
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
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

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceKnownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
	sampleSizeResult <- getSampleSizeMeans(designGS1, groups = 2, thetaH0 = 0, stDev = 2, 
	    normalApproximation = TRUE, alternative = 1.3, allocationRatioPlanned = 3)

	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
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
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
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

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceUnknownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
	sampleSizeResult <- getSampleSizeMeans(designGS1, groups = 2, thetaH0 = 0.5, stDev = 2, 
	    normalApproximation = FALSE, alternative = 2.1, allocationRatioPlanned = 1)

	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 71.36231, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 35.681155, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 35.681155, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 14.272462, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 35.681155, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 71.36231, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
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

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceKnownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
	sampleSizeResult <- getSampleSizeMeans(designGS1, groups = 2, thetaH0 = 0.5, stDev = 2, 
	    normalApproximation = TRUE, alternative = 2.1, allocationRatioPlanned = 1)

	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 69.273978, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 34.636989, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 34.636989, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 13.854796, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 34.636989, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 69.273978, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
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

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceUnknownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
	sampleSizeResult <- getSampleSizeMeans(designGS1, groups = 2, thetaH0 = 0.5, stDev = 2, 
	    normalApproximation = FALSE, alternative = 2.1, allocationRatioPlanned = 0.4)

	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
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
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
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

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceKnownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
	sampleSizeResult <- getSampleSizeMeans(designGS1, groups = 2, thetaH0 = 0.5, stDev = 2, 
	    normalApproximation = TRUE, alternative = 2.1, allocationRatioPlanned = 0.4)

	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
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
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
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

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:sampleSizeTwoMeansRatioVarianceUnknownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
	sampleSizeResult <- getSampleSizeMeans(designGS1, groups = 2, meanRatio = TRUE, thetaH0 = 0.9, 
	    stDev = 3, normalApproximation = FALSE, alternative = 1.9, allocationRatioPlanned = 1)

	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 363.14949, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 181.57474, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 181.57474, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 72.629897, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 181.57474, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 363.14949, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
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

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:sampleSizeTwoMeansRatioVarianceKnownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
	sampleSizeResult <- getSampleSizeMeans(designGS1, groups = 2, meanRatio = TRUE, thetaH0 = 0.9, 
	    stDev = 3, normalApproximation = TRUE, alternative = 1.9, allocationRatioPlanned = 1)

	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 361.11139, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 180.5557, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 180.5557, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 72.222278, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 180.5557, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 361.11139, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
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

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:sampleSizeTwoMeansRatioVarianceUnknownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
	sampleSizeResult <- getSampleSizeMeans(designGS1, groups = 2, meanRatio = TRUE, thetaH0 = 0.9, 
	    stDev = 3, normalApproximation = FALSE, alternative = 1.9, allocationRatioPlanned = 3)

	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
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
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
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

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:sampleSizeTwoMeansRatioVarianceKnownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
	sampleSizeResult <- getSampleSizeMeans(designGS1, groups = 2, meanRatio = TRUE, thetaH0 = 0.9, 
	    stDev = 3, normalApproximation = TRUE, alternative = 1.9, allocationRatioPlanned = 3)

	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
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
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
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

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:sampleSizeTwoMeansRatioVarianceKnownOnesided}
	# @refFS[Formula]{fs:sampleSizeRatioMeansOptimumAllocationRatio}
	# @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
	sampleSizeResult <- getSampleSizeMeans(designGS1, groups = 2, meanRatio = TRUE, thetaH0 = 0.9, 
	    stDev = 3, normalApproximation = TRUE, alternative = 1.9, allocationRatioPlanned = 0)

	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
	expect_equal(sampleSizeResult$allocationRatioPlanned, 1.1111111, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$allocationRatioPlanned, collapse = ", "), ")"))
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
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
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
	sampleSizeResult <- getSampleSizeMeans(designGS2, groups = 1, thetaH0 = 0.5, stDev = 2, 
	    normalApproximation = FALSE, alternative = 0.8)

	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 234.92433, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 46.984866, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 117.46217, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 234.92433, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
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

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:sampleSizeOneMeanVarianceKnownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageOneMean}
	sampleSizeResult <- getSampleSizeMeans(designGS2, groups = 1, thetaH0 = 0.5, stDev = 2, 
	    normalApproximation = TRUE, alternative = 0.8)

	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 234.50706, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 46.901412, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 117.25353, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 234.50706, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
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

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceUnknownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
	sampleSizeResult <- getSampleSizeMeans(designGS2, groups = 2, thetaH0 = 0, stDev = 2, 
	    normalApproximation = FALSE, alternative = 1.3, allocationRatioPlanned = 1)

	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 50.39219, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 25.196095, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 25.196095, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 10.078438, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 25.196095, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 50.39219, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
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

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceKnownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
	sampleSizeResult <- getSampleSizeMeans(designGS2, groups = 2, thetaH0 = 0, stDev = 2, 
	    normalApproximation = TRUE, alternative = 1.3, allocationRatioPlanned = 1)

	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 49.954167, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 24.977083, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 24.977083, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 9.9908334, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 24.977083, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 49.954167, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
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

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceUnknownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
	sampleSizeResult <- getSampleSizeMeans(designGS2, groups = 2, thetaH0 = 0, stDev = 2, 
	    normalApproximation = FALSE, alternative = 1.3, allocationRatioPlanned = 3)

	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
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
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
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

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceKnownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
	sampleSizeResult <- getSampleSizeMeans(designGS2, groups = 2, thetaH0 = 0, stDev = 2, 
	    normalApproximation = TRUE, alternative = 1.3, allocationRatioPlanned = 3)

	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
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
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
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

test_plan_section("Testing the Power Calculation of Testing Means for Different Designs and Arguments")


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
	expect_equal(powerResult$effect, c(-1.5, 0.7, 0.9), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$effect, collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[1, ], 15, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[2, ], 35, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[3, ], 50, label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$overallReject, c(1.2624119e-07, 0.79805947, 0.93305789), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[1, ], c(1.2596734e-07, 0.17254516, 0.28730882), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[2, ], c(2.7189457e-10, 0.43368823, 0.5145435), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[3, ], c(1.9550892e-12, 0.19182608, 0.13120557), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityStop, c(0.99999942, 0.078678761, 0.02585129), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityStop, collapse = ", "), ")"))
	expect_equal(powerResult$futilityPerStage[1, ], c(0.99114779, 0.032857727, 0.013099441), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityPerStage[2, ], c(0.008851635, 0.045821034, 0.01275185), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$earlyStop, c(0.99999955, 0.68491215, 0.82770361), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
	expect_equal(powerResult$expectedNumberOfSubjects, c(15.177049, 35.61826, 31.576281), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 1.8259013, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScale[2, ], 1.1288256, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScale[3, ], 0.97002208, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityBoundsEffectScale[1, ], 0.2359398, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityBoundsEffectScale[2, ], 0.67059547, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[2, ], collapse = ", "), ")"))
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

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:PowerGroupSequentialOneSided}
	# @refFS[Formula]{fs:AdjShiftParameterOneSampleMean}
	powerResult <- getPowerMeans(designGS1,
	    groups = 1, thetaH0 = -0.5, stDev = 2,
	    normalApproximation = FALSE, alternative = c(-1.2, -1),
	    directionUpper = FALSE, maxNumberOfSubjects = 50
	)

	## Comparison of the results of TrialDesignPlanMeans object 'powerResult' with expected results
	expect_equal(powerResult$effect, c(-0.7, -0.5), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$effect, collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[1, ], 15, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[2, ], 35, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[3, ], 50, label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$overallReject, c(0.79805947, 0.56526867), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[1, ], c(0.17254516, 0.092241599), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[2, ], c(0.43368823, 0.28692789), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[3, ], c(0.19182608, 0.18609918), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityStop, c(0.078678761, 0.19394481), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityStop, collapse = ", "), ")"))
	expect_equal(powerResult$futilityPerStage[1, ], c(0.032857727, 0.072497778), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityPerStage[2, ], c(0.045821034, 0.12144703), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$earlyStop, c(0.68491215, 0.5731143), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
	expect_equal(powerResult$expectedNumberOfSubjects, c(35.61826, 38.108498), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScale[1, ], -1.8259013, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScale[2, ], -1.1288256, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScale[3, ], -0.97002208, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityBoundsEffectScale[1, ], -0.2359398, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityBoundsEffectScale[2, ], -0.67059547, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[2, ], collapse = ", "), ")"))
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

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:PowerGroupSequentialOneSided}
	# @refFS[Formula]{fs:ShiftParameterOneSampleMean}
	powerResult <- getPowerMeans(designGS1,
	    groups = 1, thetaH0 = 0.5, stDev = 2,
	    normalApproximation = TRUE, alternative = 1.2, maxNumberOfSubjects = 50
	)

	## Comparison of the results of TrialDesignPlanMeans object 'powerResult' with expected results
	expect_equal(powerResult$effect, 0.7, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$effect, collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[1, ], 15, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[2, ], 35, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[3, ], 50, label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$overallReject, 0.80544254, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.17645213), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.43857394), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[3, ], c("stage = 3" = 0.19041646), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityStop, 0.075570189, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityStop, collapse = ", "), ")"))
	expect_equal(powerResult$futilityPerStage[1, ], c("stage = 1" = 0.031759279), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityPerStage[2, ], c("stage = 2" = 0.04381091), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$earlyStop, 0.69059627, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
	expect_equal(powerResult$expectedNumberOfSubjects, 35.476828, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 1.6797184, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScale[2, ], 1.1091952, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScale[3, ], 0.96124634, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityBoundsEffectScale[1, ], 0.24180111, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityBoundsEffectScale[2, ], 0.66903085, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[2, ], collapse = ", "), ")"))
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

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:PowerGroupSequentialOneSided}
	# @refFS[Formula]{fs:ShiftParameterOneSampleMean}
	powerResult <- getPowerMeans(designGS1,
	    groups = 2, thetaH0 = -0.5, stDev = 2,
	    normalApproximation = TRUE, alternative = -1.2, directionUpper = FALSE, maxNumberOfSubjects = 50
	)

	## Comparison of the results of TrialDesignPlanMeans object 'powerResult' with expected results
	expect_equal(powerResult$effect, -0.7, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$effect, collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[1, ], 15, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[2, ], 35, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[3, ], 50, label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$overallReject, 0.37256342, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.0540554), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.17942496), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[3, ], c("stage = 3" = 0.13908306), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityStop, 0.32503231, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityStop, collapse = ", "), ")"))
	expect_equal(powerResult$futilityPerStage[1, ], c("stage = 1" = 0.11944374), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityPerStage[2, ], c("stage = 2" = 0.20558857), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$earlyStop, 0.55851267, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
	expect_equal(powerResult$expectedNumberOfSubjects, 38.152327, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScale[1, ], -2.8594368, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScale[2, ], -1.7183904, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScale[3, ], -1.4224927, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityBoundsEffectScale[1, ], 0.01639778, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityBoundsEffectScale[2, ], -0.8380617, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[2, ], collapse = ", "), ")"))
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

test_that("'getPowerMeans': Power calculation of means in one sample for two-sided group sequential design", {

	.skipTestIfDisabled()

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
	expect_equal(powerResult$effect, 0.7, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$effect, collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[1, ], 17, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[2, ], 33, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[3, ], 50, label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$overallReject, 0.79752024, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.14049601), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.38370336), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[3, ], c("stage = 3" = 0.27332087), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$earlyStop, 0.52419937, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
	expect_equal(powerResult$expectedNumberOfSubjects, 38.840675, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleLower[1, ], -0.86833341, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleLower[2, ], -0.20368487, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleLower[3, ], 0.020865698, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleUpper[1, ], 1.8683334, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleUpper[2, ], 1.2036849, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleUpper[3, ], 0.9791343, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesPValueScale[1, ], 0.01229935, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesPValueScale[2, ], 0.051692876, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesPValueScale[3, ], 0.096614336, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
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

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:PowerGroupSequentialTwoSided}
	# @refFS[Formula]{fs:AdjShiftParameterOneSampleMean}
	powerResult <- getPowerMeans(designGS2,
	    groups = 1, thetaH0 = -0.5, stDev = 2,
	    normalApproximation = FALSE, alternative = -1.2, maxNumberOfSubjects = 50
	)

	## Comparison of the results of TrialDesignPlanMeans object 'powerResult' with expected results
	expect_equal(powerResult$effect, -0.7, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$effect, collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[1, ], 17, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[2, ], 33, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[3, ], 50, label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$overallReject, 0.79752024, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.14049601), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.38370336), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[3, ], c("stage = 3" = 0.27332087), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$earlyStop, 0.52419937, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
	expect_equal(powerResult$expectedNumberOfSubjects, 38.840675, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleLower[1, ], -1.8683334, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleLower[2, ], -1.2036849, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleLower[3, ], -0.9791343, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleUpper[1, ], 0.86833341, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleUpper[2, ], 0.20368487, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleUpper[3, ], -0.020865698, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesPValueScale[1, ], 0.01229935, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesPValueScale[2, ], 0.051692876, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesPValueScale[3, ], 0.096614336, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
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

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:PowerGroupSequentialTwoSided}
	# @refFS[Formula]{fs:ShiftParameterOneSampleMean}
	powerResult <- getPowerMeans(designGS2,
	    groups = 1, thetaH0 = 0.5, stDev = 2,
	    normalApproximation = TRUE, alternative = 1.2, maxNumberOfSubjects = 50
	)

	## Comparison of the results of TrialDesignPlanMeans object 'powerResult' with expected results
	expect_equal(powerResult$effect, 0.7, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$effect, collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[1, ], 17, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[2, ], 33, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[3, ], 50, label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$overallReject, 0.80597731, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.14453229), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.38954071), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[3, ], c("stage = 3" = 0.27190431), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$earlyStop, 0.534073, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
	expect_equal(powerResult$expectedNumberOfSubjects, 38.608242, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleLower[1, ], -0.71434543, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleLower[2, ], -0.17739974, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleLower[3, ], 0.03005862, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleUpper[1, ], 1.7143454, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleUpper[2, ], 1.1773997, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleUpper[3, ], 0.96994138, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesPValueScale[1, ], 0.01229935, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesPValueScale[2, ], 0.051692876, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesPValueScale[3, ], 0.096614336, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
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

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:PowerGroupSequentialTwoSided}
	# @refFS[Formula]{fs:ShiftParameterOneSampleMean}
	powerResult <- getPowerMeans(designGS2,
	    groups = 1, thetaH0 = -0.5, stDev = 2,
	    normalApproximation = TRUE, alternative = -1.2, maxNumberOfSubjects = 50
	)

	## Comparison of the results of TrialDesignPlanMeans object 'powerResult' with expected results
	expect_equal(powerResult$effect, -0.7, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$effect, collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[1, ], 17, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[2, ], 33, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[3, ], 50, label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$overallReject, 0.80597731, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.14453229), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.38954071), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[3, ], c("stage = 3" = 0.27190431), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$earlyStop, 0.534073, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
	expect_equal(powerResult$expectedNumberOfSubjects, 38.608242, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleLower[1, ], -1.7143454, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleLower[2, ], -1.1773997, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleLower[3, ], -0.96994138, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleUpper[1, ], 0.71434543, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleUpper[2, ], 0.17739974, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleUpper[3, ], -0.03005862, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesPValueScale[1, ], 0.01229935, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesPValueScale[2, ], 0.051692876, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesPValueScale[3, ], 0.096614336, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
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

test_that("'getPowerMeans': Power calculation of mean difference in two samples for one-sided group sequential design", {

	.skipTestIfDisabled()

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
	expect_equal(powerResult$effect, 1.3, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$effect, collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[1, ], 15, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[2, ], 35, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[3, ], 50, label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects1[1, ], 11.25, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects1[2, ], 26.25, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects1[3, ], 37.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects2[1, ], 3.75, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects2[2, ], 8.75, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects2[3, ], 12.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$overallReject, 0.84205533, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.19830007), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.46269628), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[3, ], c("stage = 3" = 0.18105899), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityStop, 0.060564406, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityStop, collapse = ", "), ")"))
	expect_equal(powerResult$futilityPerStage[1, ], c("stage = 1" = 0.026384529), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityPerStage[2, ], c("stage = 2" = 0.034179878), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$earlyStop, 0.72156075, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
	expect_equal(powerResult$expectedNumberOfSubjects, 34.682897, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 2.8183805, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScale[2, ], 1.5902217, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScale[3, ], 1.3144249, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityBoundsEffectScale[1, ], 0.04183972, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityBoundsEffectScale[2, ], 0.79556274, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[2, ], collapse = ", "), ")"))
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

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:PowerGroupSequentialOneSided}
	# @refFS[Formula]{fs:AdjShiftParameterTwoSamplesMeanDiff}
	powerResult <- getPowerMeans(designGS1,
	    groups = 2, thetaH0 = -0.5, stDev = 1.5,
	    meanRatio = FALSE, normalApproximation = FALSE, alternative = -1.8,
	    directionUpper = FALSE, maxNumberOfSubjects = 50, allocationRatioPlanned = 3
	)

	## Comparison of the results of TrialDesignPlanMeans object 'powerResult' with expected results
	expect_equal(powerResult$effect, -1.3, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$effect, collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[1, ], 15, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[2, ], 35, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[3, ], 50, label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects1[1, ], 11.25, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects1[2, ], 26.25, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects1[3, ], 37.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects2[1, ], 3.75, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects2[2, ], 8.75, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects2[3, ], 12.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$overallReject, 0.84205533, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.19830007), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.46269628), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[3, ], c("stage = 3" = 0.18105899), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityStop, 0.060564406, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityStop, collapse = ", "), ")"))
	expect_equal(powerResult$futilityPerStage[1, ], c("stage = 1" = 0.026384529), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityPerStage[2, ], c("stage = 2" = 0.034179878), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$earlyStop, 0.72156075, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
	expect_equal(powerResult$expectedNumberOfSubjects, 34.682897, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScale[1, ], -2.8183805, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScale[2, ], -1.5902217, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScale[3, ], -1.3144249, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityBoundsEffectScale[1, ], -0.04183972, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityBoundsEffectScale[2, ], -0.79556274, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[2, ], collapse = ", "), ")"))
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

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:PowerGroupSequentialOneSided}
	# @refFS[Formula]{fs:ShiftParameterTwoSamplesMean}
	powerResult <- getPowerMeans(designGS1,
	    groups = 2, thetaH0 = 0.5, stDev = 1.5,
	    meanRatio = FALSE, normalApproximation = TRUE, alternative = 1.8,
	    maxNumberOfSubjects = 50, allocationRatioPlanned = 3
	)

	## Comparison of the results of TrialDesignPlanMeans object 'powerResult' with expected results
	expect_equal(powerResult$effect, 1.3, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$effect, collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[1, ], 15, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[2, ], 35, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[3, ], 50, label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects1[1, ], 11.25, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects1[2, ], 26.25, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects1[3, ], 37.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects2[1, ], 3.75, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects2[2, ], 8.75, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects2[3, ], 12.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$overallReject, 0.84894434, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.20296684), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.46718133), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[3, ], c("stage = 3" = 0.17879617), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityStop, 0.057814211, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityStop, collapse = ", "), ")"))
	expect_equal(powerResult$futilityPerStage[1, ], c("stage = 1" = 0.025383492), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityPerStage[2, ], c("stage = 2" = 0.032430719), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$earlyStop, 0.72796238, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
	expect_equal(powerResult$expectedNumberOfSubjects, 34.513558, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 2.5433322, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScale[2, ], 1.555157, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScale[3, ], 1.2989021, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityBoundsEffectScale[1, ], 0.0527864, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityBoundsEffectScale[2, ], 0.79277002, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[2, ], collapse = ", "), ")"))
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

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:PowerGroupSequentialOneSided}
	# @refFS[Formula]{fs:ShiftParameterTwoSamplesMean}
	powerResult <- getPowerMeans(designGS1,
	    groups = 2, thetaH0 = -0.5, stDev = 1.5,
	    meanRatio = FALSE, normalApproximation = TRUE, alternative = -1.8,
	    directionUpper = FALSE, maxNumberOfSubjects = 50, allocationRatioPlanned = 3
	)

	## Comparison of the results of TrialDesignPlanMeans object 'powerResult' with expected results
	expect_equal(powerResult$effect, -1.3, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$effect, collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[1, ], 15, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[2, ], 35, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[3, ], 50, label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects1[1, ], 11.25, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects1[2, ], 26.25, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects1[3, ], 37.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects2[1, ], 3.75, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects2[2, ], 8.75, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects2[3, ], 12.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$overallReject, 0.84894434, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.20296684), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.46718133), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[3, ], c("stage = 3" = 0.17879617), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityStop, 0.057814211, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityStop, collapse = ", "), ")"))
	expect_equal(powerResult$futilityPerStage[1, ], c("stage = 1" = 0.025383492), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityPerStage[2, ], c("stage = 2" = 0.032430719), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$earlyStop, 0.72796238, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
	expect_equal(powerResult$expectedNumberOfSubjects, 34.513558, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScale[1, ], -2.5433322, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScale[2, ], -1.555157, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScale[3, ], -1.2989021, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityBoundsEffectScale[1, ], -0.0527864, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityBoundsEffectScale[2, ], -0.79277002, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[2, ], collapse = ", "), ")"))
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

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:PowerGroupSequentialOneSided}
	# @refFS[Formula]{fs:AdjShiftParameterTwoSamplesMeanRatio}
	powerResult <- getPowerMeans(designGS1,
	    groups = 2, thetaH0 = 0.8,
	    stDev = 1.5, meanRatio = TRUE, normalApproximation = FALSE, alternative = 1.8,
	    directionUpper = TRUE, maxNumberOfSubjects = 50, allocationRatioPlanned = 3
	)

	## Comparison of the results of TrialDesignPlanMeans object 'powerResult' with expected results
	expect_equal(powerResult$effect, 1, label = paste0("c(", paste0(powerResult$effect, collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[1, ], 15, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[2, ], 35, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[3, ], 50, label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects1[1, ], 11.25, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects1[2, ], 26.25, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects1[3, ], 37.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects2[1, ], 3.75, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects2[2, ], 8.75, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects2[3, ], 12.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$overallReject, 0.77427796, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.16086364), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.41797637), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[3, ], c("stage = 3" = 0.19543795), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityStop, 0.08888951, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityStop, collapse = ", "), ")"))
	expect_equal(powerResult$futilityPerStage[1, ], c("stage = 1" = 0.036438496), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityPerStage[2, ], c("stage = 2" = 0.052451014), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$earlyStop, 0.66772952, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
	expect_equal(powerResult$expectedNumberOfSubjects, 36.038015, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 2.7808252, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScale[2, ], 1.7314858, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScale[3, ], 1.495845, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityBoundsEffectScale[1, ], 0.40854768, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityBoundsEffectScale[2, ], 1.0525289, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[2, ], collapse = ", "), ")"))
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

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:PowerGroupSequentialOneSided}
	# @refFS[Formula]{fs:ShiftParameterTwoSamplesMean}
	powerResult <- getPowerMeans(designGS1,
	    groups = 2, thetaH0 = 0.8,
	    stDev = 1.5, meanRatio = TRUE, normalApproximation = TRUE, alternative = 1.8,
	    maxNumberOfSubjects = 50, allocationRatioPlanned = 3
	)

	## Comparison of the results of TrialDesignPlanMeans object 'powerResult' with expected results
	expect_equal(powerResult$effect, 1, label = paste0("c(", paste0(powerResult$effect, collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[1, ], 15, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[2, ], 35, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[3, ], 50, label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects1[1, ], 11.25, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects1[2, ], 26.25, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects1[3, ], 37.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects2[1, ], 3.75, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects2[2, ], 8.75, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects2[3, ], 12.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$overallReject, 0.7820561, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.16454336), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.42310788), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[3, ], c("stage = 3" = 0.19440486), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityStop, 0.085516174, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityStop, collapse = ", "), ")"))
	expect_equal(powerResult$futilityPerStage[1, ], c("stage = 1" = 0.035259709), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityPerStage[2, ], c("stage = 2" = 0.050256465), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$earlyStop, 0.67316741, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
	expect_equal(powerResult$expectedNumberOfSubjects, 35.906427, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScale[1, ], 2.5458238, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScale[2, ], 1.7015266, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScale[3, ], 1.4825823, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityBoundsEffectScale[1, ], 0.41790054, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$futilityBoundsEffectScale[2, ], 1.0501428, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[2, ], collapse = ", "), ")"))
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

test_that("'getPowerMeans': Power calculation of mean difference in two samples for two-sided group sequential design", {

	.skipTestIfDisabled()

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
	expect_equal(powerResult$effect, 1.2, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$effect, collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[1, ], 15, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[2, ], 35, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[3, ], 50, label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects1[1, ], 6.1764706, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects1[2, ], 14.411765, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects1[3, ], 20.588235, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects2[1, ], 8.8235294, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects2[2, ], 20.588235, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects2[3, ], 29.411765, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$overallReject, 0.87442088, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.35754296), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.37848399), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[3, ], c("stage = 3" = 0.13839393), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$earlyStop, 0.73602695, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
	expect_equal(powerResult$expectedNumberOfSubjects, 31.808737, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleLower[1, ], -1.6972761, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleLower[2, ], -0.83631454, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleLower[3, ], -0.62866109, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleUpper[1, ], 1.6972761, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleUpper[2, ], 0.83631454, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleUpper[3, ], 0.62866109, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
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

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:PowerGroupSequentialTwoSided}
	# @refFS[Formula]{fs:AdjShiftParameterTwoSamplesMeanDiff}
	powerResult <- getPowerMeans(designGS2,
	    groups = 2, stDev = 2,
	    normalApproximation = FALSE, alternative = -1.2,
	    maxNumberOfSubjects = 50, allocationRatioPlanned = 0.7
	)

	## Comparison of the results of TrialDesignPlanMeans object 'powerResult' with expected results
	expect_equal(powerResult$effect, -1.2, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$effect, collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[1, ], 15, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[2, ], 35, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[3, ], 50, label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects1[1, ], 6.1764706, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects1[2, ], 14.411765, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects1[3, ], 20.588235, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects2[1, ], 8.8235294, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects2[2, ], 20.588235, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects2[3, ], 29.411765, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$overallReject, 0.87442088, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.35754296), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.37848399), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[3, ], c("stage = 3" = 0.13839393), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$earlyStop, 0.73602695, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
	expect_equal(powerResult$expectedNumberOfSubjects, 31.808737, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleLower[1, ], -1.6972761, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleLower[2, ], -0.83631454, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleLower[3, ], -0.62866109, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleUpper[1, ], 1.6972761, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleUpper[2, ], 0.83631454, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleUpper[3, ], 0.62866109, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
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

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:PowerGroupSequentialTwoSided}
	# @refFS[Formula]{fs:ShiftParameterTwoSamplesMean}
	powerResult <- getPowerMeans(designGS2,
	    groups = 2, thetaH0 = 0,
	    stDev = 2, normalApproximation = TRUE, alternative = 1.2,
	    maxNumberOfSubjects = 50, allocationRatioPlanned = 0.7
	)

	## Comparison of the results of TrialDesignPlanMeans object 'powerResult' with expected results
	expect_equal(powerResult$effect, 1.2, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$effect, collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[1, ], 15, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[2, ], 35, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[3, ], 50, label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects1[1, ], 6.1764706, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects1[2, ], 14.411765, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects1[3, ], 20.588235, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects2[1, ], 8.8235294, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects2[2, ], 20.588235, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects2[3, ], 29.411765, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$overallReject, 0.87592587, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.35907583), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.37896773), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[3, ], c("stage = 3" = 0.13788231), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$earlyStop, 0.73804356, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
	expect_equal(powerResult$expectedNumberOfSubjects, 31.74783, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleLower[1, ], -1.5897396, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleLower[2, ], -0.82092617, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleLower[3, ], -0.62155644, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleUpper[1, ], 1.5897396, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleUpper[2, ], 0.82092617, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleUpper[3, ], 0.62155644, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
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

	# @refFS[Tab.]{fs:tab:output:getSampleSizeMeans}
	# @refFS[Formula]{fs:PowerGroupSequentialTwoSided}
	# @refFS[Formula]{fs:ShiftParameterTwoSamplesMean}
	powerResult <- getPowerMeans(designGS2,
	    groups = 2, stDev = 2,
	    normalApproximation = TRUE, alternative = -1.2,
	    maxNumberOfSubjects = 50, allocationRatioPlanned = 0.7
	)

	## Comparison of the results of TrialDesignPlanMeans object 'powerResult' with expected results
	expect_equal(powerResult$effect, -1.2, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$effect, collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[1, ], 15, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[2, ], 35, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects[3, ], 50, label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects1[1, ], 6.1764706, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects1[2, ], 14.411765, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects1[3, ], 20.588235, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects2[1, ], 8.8235294, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects2[2, ], 20.588235, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$numberOfSubjects2[3, ], 29.411765, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$overallReject, 0.87592587, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.35907583), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.37896773), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$rejectPerStage[3, ], c("stage = 3" = 0.13788231), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$earlyStop, 0.73804356, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
	expect_equal(powerResult$expectedNumberOfSubjects, 31.74783, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleLower[1, ], -1.5897396, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleLower[2, ], -0.82092617, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleLower[3, ], -0.62155644, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleUpper[1, ], 1.5897396, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleUpper[2, ], 0.82092617, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
	expect_equal(powerResult$criticalValuesEffectScaleUpper[3, ], 0.62155644, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
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

