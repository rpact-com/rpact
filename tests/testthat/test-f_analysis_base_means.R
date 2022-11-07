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
## |  File name: test-f_analysis_base_means.R
## |  Creation date: 12 August 2022, 09:04:36
## |  File version: $Revision: 6658 $
## |  Last changed: $Date: 2022-11-04 10:30:20 +0100 (Fr, 04 Nov 2022) $
## |  Last changed by: $Author: pahlke $
## |  

test_plan_section("Testing the Analysis Means Functionality for One Treatment")


test_that("'getAnalysisResults' for two-stage group sequential design and a dataset of one mean per stage (bindingFutility = FALSE)", {
        
    .skipTestIfDisabled()

    dataExample <- getDataset(
	    n = 120,
	    means = 0.45,
	    stDevs = 1.3
	)
	design <- getDesignGroupSequential(
	    kMax = 2, alpha = 0.025, futilityBounds = 0,
	    bindingFutility = FALSE, typeOfDesign = "WT", deltaWT = 0.4
	)

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsGroupSequential}
	# @refFS[Formula]{fs:testStatisticOneMean}
	# @refFS[Formula]{fs:pValuesOneMeanAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticGroupSequential}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:orderingPValueUpper}
	# @refFS[Formula]{fs:finalCIOneMean}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerOneMeanEffect}
	result <- getAnalysisResults(
	    design = design, dataInput = dataExample,
	    nPlanned = 130, thetaH1 = 0.22, assumedStDev = 1, thetaH0 = 0.25
	)

	## Comparison of the results of AnalysisResultsGroupSequential object 'result' with expected results
	expect_equal(result$testActions, c("continue", NA_character_))
	expect_equal(result$conditionalRejectionProbabilities, c(0.094509305, NA_real_), tolerance = 1e-07)
	expect_equal(result$conditionalPower, c(NA_real_, 0.048907456), tolerance = 1e-07)
	expect_equal(result$repeatedConfidenceIntervalLowerBounds, c(0.17801039, NA_real_), tolerance = 1e-07)
	expect_equal(result$repeatedConfidenceIntervalUpperBounds, c(0.7219894, NA_real_), tolerance = 1e-07)
	expect_equal(result$repeatedPValues, c(0.085336561, NA_real_), tolerance = 1e-07)
	expect_equal(result$finalStage, NA_integer_)
	expect_equal(result$finalPValues, c(NA_real_, NA_real_))
	expect_equal(result$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_))
	expect_equal(result$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_))
	expect_equal(result$medianUnbiasedEstimates, c(NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result), NA)))
	    expect_output(print(result)$show())
	    invisible(capture.output(expect_error(summary(result), NA)))
	    expect_output(summary(result)$show())
	    resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
	    expect_equal(resultCodeBased$testActions, result$testActions, tolerance = 1e-05)
	    expect_equal(resultCodeBased$conditionalRejectionProbabilities, result$conditionalRejectionProbabilities, tolerance = 1e-05)
	    expect_equal(resultCodeBased$conditionalPower, result$conditionalPower, tolerance = 1e-05)
	    expect_equal(resultCodeBased$repeatedConfidenceIntervalLowerBounds, result$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(resultCodeBased$repeatedConfidenceIntervalUpperBounds, result$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(resultCodeBased$repeatedPValues, result$repeatedPValues, tolerance = 1e-05)
	    expect_equal(resultCodeBased$finalStage, result$finalStage, tolerance = 1e-05)
	    expect_equal(resultCodeBased$finalPValues, result$finalPValues, tolerance = 1e-05)
	    expect_equal(resultCodeBased$finalConfidenceIntervalLowerBounds, result$finalConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(resultCodeBased$finalConfidenceIntervalUpperBounds, result$finalConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(resultCodeBased$medianUnbiasedEstimates, result$medianUnbiasedEstimates, tolerance = 1e-05)
	    expect_type(names(result), "character")
	    df <- as.data.frame(result)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(result)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getAnalysisResults' for three-stage group sequential design and a dataset of one mean per stage (bindingFutility = FALSE)", {

	.skipTestIfDisabled()

	dataExample <- getDataset(
	    n = c(120, 130),
	    means = c(0.45, 0.41) * 100,
	    stDevs = c(1.3, 1.4) * 100
	)

	design <- getDesignGroupSequential(
	    kMax = 3, alpha = 0.025, futilityBounds = rep(0.5244, 2),
	    bindingFutility = FALSE, typeOfDesign = "WT", deltaWT = 0.4
	)

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsGroupSequential}
	# @refFS[Formula]{fs:testStatisticOneMean}
	# @refFS[Formula]{fs:pValuesOneMeanAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticGroupSequential}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:orderingPValueUpper}
	# @refFS[Formula]{fs:finalCIOneMean}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerOneMeanEffect}
	result <- getAnalysisResults(
	    design = design, dataInput = dataExample,
	    nPlanned = 130, thetaH1 = 22, assumedStDev = 100, thetaH0 = 25
	)

	## Comparison of the results of AnalysisResultsGroupSequential object 'result' with expected results
	expect_equal(result$testActions, c("continue", "continue", NA_character_))
	expect_equal(result$conditionalRejectionProbabilities, c(0.10127313, 0.20204948, NA_real_), tolerance = 1e-07)
	expect_equal(result$conditionalPower, c(NA_real_, NA_real_, 0.11972239), tolerance = 1e-07)
	expect_equal(result$repeatedConfidenceIntervalLowerBounds, c(15.620913, 23.359338, NA_real_), tolerance = 1e-07)
	expect_equal(result$repeatedConfidenceIntervalUpperBounds, c(74.379087, 62.480662, NA_real_), tolerance = 1e-07)
	expect_equal(result$repeatedPValues, c(0.11501103, 0.039167372, NA_real_), tolerance = 1e-07)
	expect_equal(result$finalStage, NA_integer_)
	expect_equal(result$finalPValues, c(NA_real_, NA_real_, NA_real_))
	expect_equal(result$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_))
	expect_equal(result$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_))
	expect_equal(result$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result), NA)))
	    expect_output(print(result)$show())
	    invisible(capture.output(expect_error(summary(result), NA)))
	    expect_output(summary(result)$show())
	    resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
	    expect_equal(resultCodeBased$testActions, result$testActions, tolerance = 1e-05)
	    expect_equal(resultCodeBased$conditionalRejectionProbabilities, result$conditionalRejectionProbabilities, tolerance = 1e-05)
	    expect_equal(resultCodeBased$conditionalPower, result$conditionalPower, tolerance = 1e-05)
	    expect_equal(resultCodeBased$repeatedConfidenceIntervalLowerBounds, result$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(resultCodeBased$repeatedConfidenceIntervalUpperBounds, result$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(resultCodeBased$repeatedPValues, result$repeatedPValues, tolerance = 1e-05)
	    expect_equal(resultCodeBased$finalStage, result$finalStage, tolerance = 1e-05)
	    expect_equal(resultCodeBased$finalPValues, result$finalPValues, tolerance = 1e-05)
	    expect_equal(resultCodeBased$finalConfidenceIntervalLowerBounds, result$finalConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(resultCodeBased$finalConfidenceIntervalUpperBounds, result$finalConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(resultCodeBased$medianUnbiasedEstimates, result$medianUnbiasedEstimates, tolerance = 1e-05)
	    expect_type(names(result), "character")
	    df <- as.data.frame(result)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(result)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getAnalysisResults' for group sequential design and a dataset of one mean per stage (bindingFutility = TRUE)", {

	.skipTestIfDisabled()

	dataExample0 <- getDataset(
	    n = c(120, 130, 130),
	    means = c(0.45, 0.41, 0.45) * 100,
	    stDevs = c(1.3, 1.4, 1.2) * 100
	)

	design1 <- getDesignGroupSequential(
	    kMax = 4, alpha = 0.025, futilityBounds = rep(0.5244, 3),
	    bindingFutility = TRUE, typeOfDesign = "WT", deltaWT = 0.4
	)

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsGroupSequential}
	# @refFS[Formula]{fs:testStatisticOneMean}
	# @refFS[Formula]{fs:pValuesOneMeanAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticGroupSequential}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:orderingPValueUpper}
	# @refFS[Formula]{fs:finalCIOneMean}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerOneMeanEffect}
	result1 <- getAnalysisResults(
	    design = design1, dataInput = dataExample0,
	    nPlanned = 130, thetaH1 = 22, assumedStDev = 100, thetaH0 = 25
	)

	## Comparison of the results of AnalysisResultsGroupSequential object 'result1' with expected results
	expect_equal(result1$testActions, c("continue", "continue", "reject and stop", NA_character_))
	expect_equal(result1$conditionalRejectionProbabilities, c(0.11438278, 0.24787613, 0.68016764, NA_real_), tolerance = 1e-07)
	expect_equal(result1$conditionalPower, c(NA_real_, NA_real_, NA_real_, 0.55017955), tolerance = 1e-07)
	expect_equal(result1$repeatedConfidenceIntervalLowerBounds, c(14.924587, 22.902668, 28.667333, NA_real_), tolerance = 1e-07)
	expect_equal(result1$repeatedConfidenceIntervalUpperBounds, c(75.075413, 62.937332, 58.595825, NA_real_), tolerance = 1e-07)
	expect_equal(result1$repeatedPValues, c(0.10271056, 0.041641198, 0.0060463294, NA_real_), tolerance = 1e-07)
	expect_equal(result1$finalStage, 3)
	expect_equal(result1$finalPValues, c(NA_real_, NA_real_, 0.014723218, NA_real_), tolerance = 1e-07)
	expect_equal(result1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, 26.836053, NA_real_), tolerance = 1e-07)
	expect_equal(result1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, 56.851998, NA_real_), tolerance = 1e-07)
	expect_equal(result1$medianUnbiasedEstimates, c(NA_real_, NA_real_, 42.083093, NA_real_), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result1), NA)))
	    expect_output(print(result1)$show())
	    invisible(capture.output(expect_error(summary(result1), NA)))
	    expect_output(summary(result1)$show())
	    result1CodeBased <- eval(parse(text = getObjectRCode(result1, stringWrapParagraphWidth = NULL)))
	    expect_equal(result1CodeBased$testActions, result1$testActions, tolerance = 1e-05)
	    expect_equal(result1CodeBased$conditionalRejectionProbabilities, result1$conditionalRejectionProbabilities, tolerance = 1e-05)
	    expect_equal(result1CodeBased$conditionalPower, result1$conditionalPower, tolerance = 1e-05)
	    expect_equal(result1CodeBased$repeatedConfidenceIntervalLowerBounds, result1$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result1CodeBased$repeatedConfidenceIntervalUpperBounds, result1$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result1CodeBased$repeatedPValues, result1$repeatedPValues, tolerance = 1e-05)
	    expect_equal(result1CodeBased$finalStage, result1$finalStage, tolerance = 1e-05)
	    expect_equal(result1CodeBased$finalPValues, result1$finalPValues, tolerance = 1e-05)
	    expect_equal(result1CodeBased$finalConfidenceIntervalLowerBounds, result1$finalConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result1CodeBased$finalConfidenceIntervalUpperBounds, result1$finalConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result1CodeBased$medianUnbiasedEstimates, result1$medianUnbiasedEstimates, tolerance = 1e-05)
	    expect_type(names(result1), "character")
	    df <- as.data.frame(result1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(result1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getStageResults' for group sequential design and a dataset of one mean per stage (bindingFutility = TRUE)", {

	.skipTestIfDisabled()

	dataExample1 <- getDataset(
	    n = c(20, 30, 30),
	    means = c(0.45, 0.51, 0.45) * 100,
	    stDevs = c(1.3, 1.4, 1.2) * 100
	)

	design1 <- getDesignGroupSequential(
	    kMax = 4, alpha = 0.025, futilityBounds = rep(0.5244, 3),
	    bindingFutility = TRUE, typeOfDesign = "WT", deltaWT = 0.4
	)

	# @refFS[Formula]{fs:testStatisticOneMean}
	# @refFS[Formula]{fs:pValuesOneMeanAlternativeGreater}
	stageResults1 <- getStageResults(design1, dataExample1, thetaH0 = 10, stage = 2)

	## Comparison of the results of StageResultsMeans object 'stageResults1' with expected results
	expect_equal(stageResults1$overallTestStatistics, c(1.2040366, 2.025312, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$overallPValues, c(0.12168078, 0.02415027, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$overallMeans, c(45, 48.6, 47.25, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$overallStDevs, c(130, 134.76601, 128.66279, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$overallSampleSizes, c(20, 50, NA_real_, NA_real_))
	expect_equal(stageResults1$testStatistics, c(1.2040366, 1.6040446, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$pValues, c(0.12168078, 0.059770605, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$effectSizes, c(45, 48.6, NA_real_, NA_real_), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(stageResults1), NA)))
	    expect_output(print(stageResults1)$show())
	    invisible(capture.output(expect_error(summary(stageResults1), NA)))
	    expect_output(summary(stageResults1)$show())
	    stageResults1CodeBased <- eval(parse(text = getObjectRCode(stageResults1, stringWrapParagraphWidth = NULL)))
	    expect_equal(stageResults1CodeBased$overallTestStatistics, stageResults1$overallTestStatistics, tolerance = 1e-05)
	    expect_equal(stageResults1CodeBased$overallPValues, stageResults1$overallPValues, tolerance = 1e-05)
	    expect_equal(stageResults1CodeBased$overallMeans, stageResults1$overallMeans, tolerance = 1e-05)
	    expect_equal(stageResults1CodeBased$overallStDevs, stageResults1$overallStDevs, tolerance = 1e-05)
	    expect_equal(stageResults1CodeBased$overallSampleSizes, stageResults1$overallSampleSizes, tolerance = 1e-05)
	    expect_equal(stageResults1CodeBased$testStatistics, stageResults1$testStatistics, tolerance = 1e-05)
	    expect_equal(stageResults1CodeBased$pValues, stageResults1$pValues, tolerance = 1e-05)
	    expect_equal(stageResults1CodeBased$effectSizes, stageResults1$effectSizes, tolerance = 1e-05)
	    expect_type(names(stageResults1), "character")
	    df <- as.data.frame(stageResults1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(stageResults1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerOneMeanEffect}
	plotData1 <- testGetStageResultsPlotData(stageResults1,
	    stage = 2, nPlanned = c(30, 20),
	    thetaRange = seq(10, 80, 5), assumedStDev = 100
	)

	## Comparison of the results of list object 'plotData1' with expected results
	expect_equal(plotData1$xValues, c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80))
	expect_equal(plotData1$condPowerValues, c(0.20492816, 0.31007642, 0.43512091, 0.5683138, 0.6950205, 0.80243295, 0.88343665, 0.93770927, 0.96998259, 0.98700232, 0.99495733, 0.99825113, 0.99945881, 0.9998508, 0.9999634), tolerance = 1e-07)
	expect_equal(plotData1$likelihoodValues, c(0.12861339, 0.21139553, 0.32435073, 0.46456173, 0.62112851, 0.77522713, 0.90320416, 0.98231862, 0.99730568, 0.94517816, 0.83619688, 0.69057821, 0.53238607, 0.38313335, 0.25738469), tolerance = 1e-07)
	expect_equal(plotData1$main, "Conditional Power with Likelihood")
	expect_equal(plotData1$xlab, "Effect size")
	expect_equal(plotData1$ylab, "Conditional power / Likelihood")
	expect_equal(plotData1$sub, "Stage = 2, # of remaining subjects = 50, sd = 100")

})

test_that("'getAnalysisResults' for inverse normal and Fisher designs and a dataset of one mean per stage (bindingFutility = TRUE)", {

	.skipTestIfDisabled()

	dataExample1 <- getDataset(
	    n = c(20, 30, 30),
	    means = c(0.45, 0.51, 0.45) * 100,
	    stDevs = c(1.3, 1.4, 1.2) * 100
	)

	design2 <- getDesignInverseNormal(
	    kMax = 4, alpha = 0.025, futilityBounds = rep(0.5244, 3),
	    bindingFutility = FALSE, typeOfDesign = "WT", deltaWT = 0.4
	)

	# @refFS[Formula]{fs:testStatisticOneMean}
	# @refFS[Formula]{fs:pValuesOneMeanAlternativeGreater}
	stageResults2 <- getStageResults(design2, dataExample1, thetaH0 = 10, stage = 2)

	## Comparison of the results of StageResultsMeans object 'stageResults2' with expected results
	expect_equal(stageResults2$overallTestStatistics, c(1.2040366, 2.025312, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$overallPValues, c(0.12168078, 0.02415027, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$overallMeans, c(45, 48.6, 47.25, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$overallStDevs, c(130, 134.76601, 128.66279, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$overallSampleSizes, c(20, 50, NA_real_, NA_real_))
	expect_equal(stageResults2$testStatistics, c(1.2040366, 1.6040446, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$pValues, c(0.12168078, 0.059770605, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$effectSizes, c(45, 48.6, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$combInverseNormal, c(1.1666257, 1.9256836, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$weightsInverseNormal, c(0.5, 0.5, 0.5, 0.5), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(stageResults2), NA)))
	    expect_output(print(stageResults2)$show())
	    invisible(capture.output(expect_error(summary(stageResults2), NA)))
	    expect_output(summary(stageResults2)$show())
	    stageResults2CodeBased <- eval(parse(text = getObjectRCode(stageResults2, stringWrapParagraphWidth = NULL)))
	    expect_equal(stageResults2CodeBased$overallTestStatistics, stageResults2$overallTestStatistics, tolerance = 1e-05)
	    expect_equal(stageResults2CodeBased$overallPValues, stageResults2$overallPValues, tolerance = 1e-05)
	    expect_equal(stageResults2CodeBased$overallMeans, stageResults2$overallMeans, tolerance = 1e-05)
	    expect_equal(stageResults2CodeBased$overallStDevs, stageResults2$overallStDevs, tolerance = 1e-05)
	    expect_equal(stageResults2CodeBased$overallSampleSizes, stageResults2$overallSampleSizes, tolerance = 1e-05)
	    expect_equal(stageResults2CodeBased$testStatistics, stageResults2$testStatistics, tolerance = 1e-05)
	    expect_equal(stageResults2CodeBased$pValues, stageResults2$pValues, tolerance = 1e-05)
	    expect_equal(stageResults2CodeBased$effectSizes, stageResults2$effectSizes, tolerance = 1e-05)
	    expect_equal(stageResults2CodeBased$combInverseNormal, stageResults2$combInverseNormal, tolerance = 1e-05)
	    expect_equal(stageResults2CodeBased$weightsInverseNormal, stageResults2$weightsInverseNormal, tolerance = 1e-05)
	    expect_type(names(stageResults2), "character")
	    df <- as.data.frame(stageResults2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(stageResults2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerOneMeanEffect}
	plotData2 <- testGetStageResultsPlotData(stageResults2,
	    stage = 2, nPlanned = c(30, 20),
	    thetaRange = seq(10, 80, 5), assumedStDev = 100
	)

	## Comparison of the results of list object 'plotData2' with expected results
	expect_equal(plotData2$xValues, c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80))
	expect_equal(plotData2$condPowerValues, c(0.16190431, 0.25577971, 0.37352079, 0.50571299, 0.6381983, 0.75647047, 0.85036513, 0.91657165, 0.95799515, 0.98097554, 0.99227303, 0.99719255, 0.99908935, 0.99973672, 0.99993224), tolerance = 1e-07)
	expect_equal(plotData2$likelihoodValues, c(0.12861339, 0.21139553, 0.32435073, 0.46456173, 0.62112851, 0.77522713, 0.90320416, 0.98231862, 0.99730568, 0.94517816, 0.83619688, 0.69057821, 0.53238607, 0.38313335, 0.25738469), tolerance = 1e-07)
	expect_equal(plotData2$main, "Conditional Power with Likelihood")
	expect_equal(plotData2$xlab, "Effect size")
	expect_equal(plotData2$ylab, "Conditional power / Likelihood")
	expect_equal(plotData2$sub, "Stage = 2, # of remaining subjects = 50, sd = 100")

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsInverseNormal}
	# @refFS[Formula]{fs:testStatisticOneMean}
	# @refFS[Formula]{fs:pValuesOneMeanAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticNormalCombinationTest}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:orderingPValueUpper}
	# @refFS[Formula]{fs:finalCIOneMean}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerOneMeanEffect}
	result2 <- getAnalysisResults(
	    design = design2, dataInput = dataExample1,
	    nPlanned = 30, thetaH1 = 50, assumedStDev = 100, thetaH0 = 10
	)

	## Comparison of the results of AnalysisResultsInverseNormal object 'result2' with expected results
	expect_equal(result2$testActions, c("continue", "continue", "reject and stop", NA_character_))
	expect_equal(result2$conditionalRejectionProbabilities, c(0.046837862, 0.16190673, 0.42383694, NA_real_), tolerance = 1e-07)
	expect_equal(result2$conditionalPower, c(NA_real_, NA_real_, NA_real_, 0.97718516), tolerance = 1e-07)
	expect_equal(result2$repeatedConfidenceIntervalLowerBounds, c(-37.7517, 0.20066782, 12.631309, NA_real_), tolerance = 1e-07)
	expect_equal(result2$repeatedConfidenceIntervalUpperBounds, c(127.7517, 96.240714, 81.345632, NA_real_), tolerance = 1e-07)
	expect_equal(result2$repeatedPValues, c(0.28074785, 0.070627118, 0.016069426, NA_real_), tolerance = 1e-07)
	expect_equal(result2$finalStage, 3)
	expect_equal(result2$finalPValues, c(NA_real_, NA_real_, 0.015631623, NA_real_), tolerance = 1e-07)
	expect_equal(result2$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, 13.353451, NA_real_), tolerance = 1e-07)
	expect_equal(result2$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, 73.21831, NA_real_), tolerance = 1e-07)
	expect_equal(result2$medianUnbiasedEstimates, c(NA_real_, NA_real_, 44.191392, NA_real_), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result2), NA)))
	    expect_output(print(result2)$show())
	    invisible(capture.output(expect_error(summary(result2), NA)))
	    expect_output(summary(result2)$show())
	    result2CodeBased <- eval(parse(text = getObjectRCode(result2, stringWrapParagraphWidth = NULL)))
	    expect_equal(result2CodeBased$testActions, result2$testActions, tolerance = 1e-05)
	    expect_equal(result2CodeBased$conditionalRejectionProbabilities, result2$conditionalRejectionProbabilities, tolerance = 1e-05)
	    expect_equal(result2CodeBased$conditionalPower, result2$conditionalPower, tolerance = 1e-05)
	    expect_equal(result2CodeBased$repeatedConfidenceIntervalLowerBounds, result2$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result2CodeBased$repeatedConfidenceIntervalUpperBounds, result2$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result2CodeBased$repeatedPValues, result2$repeatedPValues, tolerance = 1e-05)
	    expect_equal(result2CodeBased$finalStage, result2$finalStage, tolerance = 1e-05)
	    expect_equal(result2CodeBased$finalPValues, result2$finalPValues, tolerance = 1e-05)
	    expect_equal(result2CodeBased$finalConfidenceIntervalLowerBounds, result2$finalConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result2CodeBased$finalConfidenceIntervalUpperBounds, result2$finalConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result2CodeBased$medianUnbiasedEstimates, result2$medianUnbiasedEstimates, tolerance = 1e-05)
	    expect_type(names(result2), "character")
	    df <- as.data.frame(result2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(result2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	design3 <- getDesignFisher(kMax = 4, alpha = 0.025, alpha0Vec = rep(0.4, 3), bindingFutility = TRUE)

	# @refFS[Formula]{fs:testStatisticOneMean}
	# @refFS[Formula]{fs:pValuesOneMeanAlternativeGreater}
	stageResults3 <- getStageResults(design3, dataExample1, thetaH0 = 10, stage = 2)

	## Comparison of the results of StageResultsMeans object 'stageResults3' with expected results
	expect_equal(stageResults3$overallTestStatistics, c(1.2040366, 2.025312, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$overallPValues, c(0.12168078, 0.02415027, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$overallMeans, c(45, 48.6, 47.25, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$overallStDevs, c(130, 134.76601, 128.66279, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$overallSampleSizes, c(20, 50, NA_real_, NA_real_))
	expect_equal(stageResults3$testStatistics, c(1.2040366, 1.6040446, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$pValues, c(0.12168078, 0.059770605, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$effectSizes, c(45, 48.6, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$combFisher, c(0.12168078, 0.007272934, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$weightsFisher, c(1, 1, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(stageResults3), NA)))
	    expect_output(print(stageResults3)$show())
	    invisible(capture.output(expect_error(summary(stageResults3), NA)))
	    expect_output(summary(stageResults3)$show())
	    stageResults3CodeBased <- eval(parse(text = getObjectRCode(stageResults3, stringWrapParagraphWidth = NULL)))
	    expect_equal(stageResults3CodeBased$overallTestStatistics, stageResults3$overallTestStatistics, tolerance = 1e-05)
	    expect_equal(stageResults3CodeBased$overallPValues, stageResults3$overallPValues, tolerance = 1e-05)
	    expect_equal(stageResults3CodeBased$overallMeans, stageResults3$overallMeans, tolerance = 1e-05)
	    expect_equal(stageResults3CodeBased$overallStDevs, stageResults3$overallStDevs, tolerance = 1e-05)
	    expect_equal(stageResults3CodeBased$overallSampleSizes, stageResults3$overallSampleSizes, tolerance = 1e-05)
	    expect_equal(stageResults3CodeBased$testStatistics, stageResults3$testStatistics, tolerance = 1e-05)
	    expect_equal(stageResults3CodeBased$pValues, stageResults3$pValues, tolerance = 1e-05)
	    expect_equal(stageResults3CodeBased$effectSizes, stageResults3$effectSizes, tolerance = 1e-05)
	    expect_equal(stageResults3CodeBased$combFisher, stageResults3$combFisher, tolerance = 1e-05)
	    expect_equal(stageResults3CodeBased$weightsFisher, stageResults3$weightsFisher, tolerance = 1e-05)
	    expect_type(names(stageResults3), "character")
	    df <- as.data.frame(stageResults3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(stageResults3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Formula]{fs:testStatisticOneMean}
	# @refFS[Formula]{fs:pValuesOneMeanAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticFisherCombinationTest}
	# @refFS[Formula]{fs:definitionRCIFisherCombination}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:finalPValueFisherCombinationTest}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	result3 <- getAnalysisResults(
	    design = design3, dataInput = dataExample1, thetaH0 = 10,
	    nPlanned = 30, thetaH1 = 50, assumedStDev = 100, seed = 123456789
	)

	## Comparison of the results of AnalysisResultsFisher object 'result3' with expected results
	expect_equal(result3$testActions, c("continue", "continue", "continue", NA_character_))
	expect_equal(result3$conditionalRejectionProbabilities, c(0.029249394, 0.067046868, 0.15552139, NA_real_), tolerance = 1e-07)
	expect_equal(result3$conditionalPower, c(NA_real_, NA_real_, NA_real_, 0.88057256), tolerance = 1e-07)
	expect_equal(result3$repeatedConfidenceIntervalLowerBounds, c(-24.226675, 0.014834887, 8.7947814, NA_real_), tolerance = 1e-07)
	expect_equal(result3$repeatedConfidenceIntervalUpperBounds, c(114.22668, 96.713521, 85.125684, NA_real_), tolerance = 1e-07)
	expect_equal(result3$repeatedPValues, c(0.165096, 0.068572907, 0.029926287, NA_real_), tolerance = 1e-07)
	expect_equal(result3$finalStage, NA_integer_)
	expect_equal(result3$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result3), NA)))
	    expect_output(print(result3)$show())
	    invisible(capture.output(expect_error(summary(result3), NA)))
	    expect_output(summary(result3)$show())
	    result3CodeBased <- eval(parse(text = getObjectRCode(result3, stringWrapParagraphWidth = NULL)))
	    expect_equal(result3CodeBased$testActions, result3$testActions, tolerance = 1e-05)
	    expect_equal(result3CodeBased$conditionalRejectionProbabilities, result3$conditionalRejectionProbabilities, tolerance = 1e-05)
	    expect_equal(result3CodeBased$conditionalPower, result3$conditionalPower, tolerance = 1e-05)
	    expect_equal(result3CodeBased$repeatedConfidenceIntervalLowerBounds, result3$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result3CodeBased$repeatedConfidenceIntervalUpperBounds, result3$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result3CodeBased$repeatedPValues, result3$repeatedPValues, tolerance = 1e-05)
	    expect_equal(result3CodeBased$finalStage, result3$finalStage, tolerance = 1e-05)
	    expect_equal(result3CodeBased$finalPValues, result3$finalPValues, tolerance = 1e-05)
	    expect_equal(result3CodeBased$finalConfidenceIntervalLowerBounds, result3$finalConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result3CodeBased$finalConfidenceIntervalUpperBounds, result3$finalConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result3CodeBased$medianUnbiasedEstimates, result3$medianUnbiasedEstimates, tolerance = 1e-05)
	    expect_type(names(result3), "character")
	    df <- as.data.frame(result3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(result3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getAnalysisResults' for different designs and a dataset of one mean per stage (bindingFutility = FALSE)", {

	.skipTestIfDisabled()

	design4 <- getDesignGroupSequential(kMax = 4, alpha = 0.025, typeOfDesign = "WT", deltaWT = 0.4)

	dataExample2 <- getDataset(
	    n = c(20, 20, 20),
	    means = c(0.45, 0.51, 0.45) * 100,
	    stDevs = c(1.3, 1.4, 1.2) * 100
	)

	stageResults1 <- getStageResults(design4, dataExample2, thetaH0 = 10, stage = 2)

	## Comparison of the results of StageResultsMeans object 'stageResults1' with expected results
	expect_equal(stageResults1$overallTestStatistics, c(1.2040366, 1.8018141, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$overallPValues, c(0.12168078, 0.039654359, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$overallMeans, c(45, 48, 47, NA_real_))
	expect_equal(stageResults1$overallStDevs, c(130, 133.38396, 128.06116, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$overallSampleSizes, c(20, 40, NA_real_, NA_real_))
	expect_equal(stageResults1$testStatistics, c(1.2040366, 1.309697, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$pValues, c(0.12168078, 0.10295724, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$effectSizes, c(45, 48, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(stageResults1), NA)))
	    expect_output(print(stageResults1)$show())
	    invisible(capture.output(expect_error(summary(stageResults1), NA)))
	    expect_output(summary(stageResults1)$show())
	    stageResults1CodeBased <- eval(parse(text = getObjectRCode(stageResults1, stringWrapParagraphWidth = NULL)))
	    expect_equal(stageResults1CodeBased$overallTestStatistics, stageResults1$overallTestStatistics, tolerance = 1e-05)
	    expect_equal(stageResults1CodeBased$overallPValues, stageResults1$overallPValues, tolerance = 1e-05)
	    expect_equal(stageResults1CodeBased$overallMeans, stageResults1$overallMeans, tolerance = 1e-05)
	    expect_equal(stageResults1CodeBased$overallStDevs, stageResults1$overallStDevs, tolerance = 1e-05)
	    expect_equal(stageResults1CodeBased$overallSampleSizes, stageResults1$overallSampleSizes, tolerance = 1e-05)
	    expect_equal(stageResults1CodeBased$testStatistics, stageResults1$testStatistics, tolerance = 1e-05)
	    expect_equal(stageResults1CodeBased$pValues, stageResults1$pValues, tolerance = 1e-05)
	    expect_equal(stageResults1CodeBased$effectSizes, stageResults1$effectSizes, tolerance = 1e-05)
	    expect_type(names(stageResults1), "character")
	    df <- as.data.frame(stageResults1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(stageResults1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	plotData1 <- testGetStageResultsPlotData(stageResults1,
	    stage = 2, nPlanned = c(30, 20),
	    thetaRange = seq(10, 80, 5), assumedStDev = 100
	)

	## Comparison of the results of list object 'plotData1' with expected results
	expect_equal(plotData1$xValues, c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80))
	expect_equal(plotData1$condPowerValues, c(0.11518708, 0.19320212, 0.2981846, 0.42448846, 0.55999334, 0.68937861, 0.79916986, 0.8818727, 0.93712809, 0.96985063, 0.98701854, 0.99499503, 0.99827593, 0.99947032, 0.99985507), tolerance = 1e-07)
	expect_equal(plotData1$likelihoodValues, c(0.19725323, 0.29399425, 0.4142314, 0.5517428, 0.69473602, 0.8269751, 0.93058175, 0.98993369, 0.99551351, 0.94640644, 0.85054578, 0.72261535, 0.58037159, 0.44065083, 0.31628057), tolerance = 1e-07)
	expect_equal(plotData1$main, "Conditional Power with Likelihood")
	expect_equal(plotData1$xlab, "Effect size")
	expect_equal(plotData1$ylab, "Conditional power / Likelihood")
	expect_equal(plotData1$sub, "Stage = 2, # of remaining subjects = 50, sd = 100")

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsGroupSequential}
	# @refFS[Formula]{fs:testStatisticOneMean}
	# @refFS[Formula]{fs:pValuesOneMeanAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticGroupSequential}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:orderingPValueUpper}
	# @refFS[Formula]{fs:finalCIOneMean}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerOneMeanEffect}
	result1 <- getAnalysisResults(design = design4, dataInput = dataExample2, thetaH0 = 10)

	## Comparison of the results of AnalysisResultsGroupSequential object 'result1' with expected results
	expect_equal(result1$thetaH1, 47)
	expect_equal(result1$assumedStDev, 128.06116, tolerance = 1e-07)
	expect_equal(result1$testActions, c("continue", "continue", "continue", NA_character_))
	expect_equal(result1$conditionalRejectionProbabilities, c(0.046837862, 0.11518708, 0.2468754, NA_real_), tolerance = 1e-07)
	expect_equal(result1$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result1$repeatedConfidenceIntervalLowerBounds, c(-37.7517, -4.7433931, 7.9671114, NA_real_), tolerance = 1e-07)
	expect_equal(result1$repeatedConfidenceIntervalUpperBounds, c(127.7517, 100.74339, 86.032888, NA_real_), tolerance = 1e-07)
	expect_equal(result1$repeatedPValues, c(0.28074785, 0.098382799, 0.033210734, NA_real_), tolerance = 1e-07)
	expect_equal(result1$finalStage, NA_integer_)
	expect_equal(result1$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result1$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result1), NA)))
	    expect_output(print(result1)$show())
	    invisible(capture.output(expect_error(summary(result1), NA)))
	    expect_output(summary(result1)$show())
	    result1CodeBased <- eval(parse(text = getObjectRCode(result1, stringWrapParagraphWidth = NULL)))
	    expect_equal(result1CodeBased$thetaH1, result1$thetaH1, tolerance = 1e-05)
	    expect_equal(result1CodeBased$assumedStDev, result1$assumedStDev, tolerance = 1e-05)
	    expect_equal(result1CodeBased$testActions, result1$testActions, tolerance = 1e-05)
	    expect_equal(result1CodeBased$conditionalRejectionProbabilities, result1$conditionalRejectionProbabilities, tolerance = 1e-05)
	    expect_equal(result1CodeBased$conditionalPower, result1$conditionalPower, tolerance = 1e-05)
	    expect_equal(result1CodeBased$repeatedConfidenceIntervalLowerBounds, result1$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result1CodeBased$repeatedConfidenceIntervalUpperBounds, result1$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result1CodeBased$repeatedPValues, result1$repeatedPValues, tolerance = 1e-05)
	    expect_equal(result1CodeBased$finalStage, result1$finalStage, tolerance = 1e-05)
	    expect_equal(result1CodeBased$finalPValues, result1$finalPValues, tolerance = 1e-05)
	    expect_equal(result1CodeBased$finalConfidenceIntervalLowerBounds, result1$finalConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result1CodeBased$finalConfidenceIntervalUpperBounds, result1$finalConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result1CodeBased$medianUnbiasedEstimates, result1$medianUnbiasedEstimates, tolerance = 1e-05)
	    expect_type(names(result1), "character")
	    df <- as.data.frame(result1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(result1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	design5 <- getDesignInverseNormal(kMax = 4, alpha = 0.025, typeOfDesign = "WT", deltaWT = 0.4)

	stageResults2 <- getStageResults(design5, dataExample2, thetaH0 = 10, stage = 2)

	## Comparison of the results of StageResultsMeans object 'stageResults2' with expected results
	expect_equal(stageResults2$overallTestStatistics, c(1.2040366, 1.8018141, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$overallPValues, c(0.12168078, 0.039654359, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$overallMeans, c(45, 48, 47, NA_real_))
	expect_equal(stageResults2$overallStDevs, c(130, 133.38396, 128.06116, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$overallSampleSizes, c(20, 40, NA_real_, NA_real_))
	expect_equal(stageResults2$testStatistics, c(1.2040366, 1.309697, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$pValues, c(0.12168078, 0.10295724, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$effectSizes, c(45, 48, NA_real_, NA_real_))
	expect_equal(stageResults2$combInverseNormal, c(1.1666257, 1.7193339, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$weightsInverseNormal, c(0.5, 0.5, 0.5, 0.5), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(stageResults2), NA)))
	    expect_output(print(stageResults2)$show())
	    invisible(capture.output(expect_error(summary(stageResults2), NA)))
	    expect_output(summary(stageResults2)$show())
	    stageResults2CodeBased <- eval(parse(text = getObjectRCode(stageResults2, stringWrapParagraphWidth = NULL)))
	    expect_equal(stageResults2CodeBased$overallTestStatistics, stageResults2$overallTestStatistics, tolerance = 1e-05)
	    expect_equal(stageResults2CodeBased$overallPValues, stageResults2$overallPValues, tolerance = 1e-05)
	    expect_equal(stageResults2CodeBased$overallMeans, stageResults2$overallMeans, tolerance = 1e-05)
	    expect_equal(stageResults2CodeBased$overallStDevs, stageResults2$overallStDevs, tolerance = 1e-05)
	    expect_equal(stageResults2CodeBased$overallSampleSizes, stageResults2$overallSampleSizes, tolerance = 1e-05)
	    expect_equal(stageResults2CodeBased$testStatistics, stageResults2$testStatistics, tolerance = 1e-05)
	    expect_equal(stageResults2CodeBased$pValues, stageResults2$pValues, tolerance = 1e-05)
	    expect_equal(stageResults2CodeBased$effectSizes, stageResults2$effectSizes, tolerance = 1e-05)
	    expect_equal(stageResults2CodeBased$combInverseNormal, stageResults2$combInverseNormal, tolerance = 1e-05)
	    expect_equal(stageResults2CodeBased$weightsInverseNormal, stageResults2$weightsInverseNormal, tolerance = 1e-05)
	    expect_type(names(stageResults2), "character")
	    df <- as.data.frame(stageResults2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(stageResults2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	plotData2 <- testGetStageResultsPlotData(stageResults2,
	    stage = 2, nPlanned = c(30, 20),
	    thetaRange = seq(10, 80, 5), assumedStDev = 100
	)

	## Comparison of the results of list object 'plotData2' with expected results
	expect_equal(plotData2$xValues, c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80))
	expect_equal(plotData2$condPowerValues, c(0.10694528, 0.18165277, 0.28365551, 0.40813694, 0.54357522, 0.6747028, 0.78751068, 0.8736511, 0.93198732, 0.96700264, 0.98562147, 0.9943885, 0.99804297, 0.99939119, 0.99983131), tolerance = 1e-07)
	expect_equal(plotData2$likelihoodValues, c(0.19725323, 0.29399425, 0.4142314, 0.5517428, 0.69473602, 0.8269751, 0.93058175, 0.98993369, 0.99551351, 0.94640644, 0.85054578, 0.72261535, 0.58037159, 0.44065083, 0.31628057), tolerance = 1e-07)
	expect_equal(plotData2$main, "Conditional Power with Likelihood")
	expect_equal(plotData2$xlab, "Effect size")
	expect_equal(plotData2$ylab, "Conditional power / Likelihood")
	expect_equal(plotData2$sub, "Stage = 2, # of remaining subjects = 50, sd = 100")

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsInverseNormal}
	# @refFS[Formula]{fs:testStatisticOneMean}
	# @refFS[Formula]{fs:pValuesOneMeanAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticNormalCombinationTest}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:orderingPValueUpper}
	# @refFS[Formula]{fs:finalCIOneMean}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerOneMeanEffect}
	result2 <- getAnalysisResults(design = design5, dataInput = dataExample2, thetaH0 = 10)

	## Comparison of the results of AnalysisResultsInverseNormal object 'result2' with expected results
	expect_equal(result2$thetaH1, 47)
	expect_equal(result2$assumedStDev, 128.06116, tolerance = 1e-07)
	expect_equal(result2$testActions, c("continue", "continue", "continue", NA_character_))
	expect_equal(result2$conditionalRejectionProbabilities, c(0.046837862, 0.10694527, 0.21929053, NA_real_), tolerance = 1e-07)
	expect_equal(result2$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result2$repeatedConfidenceIntervalLowerBounds, c(-37.7517, -5.8599359, 6.9798507, NA_real_), tolerance = 1e-07)
	expect_equal(result2$repeatedConfidenceIntervalUpperBounds, c(127.7517, 101.68482, 86.758637, NA_real_), tolerance = 1e-07)
	expect_equal(result2$repeatedPValues, c(0.28074785, 0.10502799, 0.037620516, NA_real_), tolerance = 1e-07)
	expect_equal(result2$finalStage, NA_integer_)
	expect_equal(result2$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result2$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result2$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result2$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result2), NA)))
	    expect_output(print(result2)$show())
	    invisible(capture.output(expect_error(summary(result2), NA)))
	    expect_output(summary(result2)$show())
	    result2CodeBased <- eval(parse(text = getObjectRCode(result2, stringWrapParagraphWidth = NULL)))
	    expect_equal(result2CodeBased$thetaH1, result2$thetaH1, tolerance = 1e-05)
	    expect_equal(result2CodeBased$assumedStDev, result2$assumedStDev, tolerance = 1e-05)
	    expect_equal(result2CodeBased$testActions, result2$testActions, tolerance = 1e-05)
	    expect_equal(result2CodeBased$conditionalRejectionProbabilities, result2$conditionalRejectionProbabilities, tolerance = 1e-05)
	    expect_equal(result2CodeBased$conditionalPower, result2$conditionalPower, tolerance = 1e-05)
	    expect_equal(result2CodeBased$repeatedConfidenceIntervalLowerBounds, result2$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result2CodeBased$repeatedConfidenceIntervalUpperBounds, result2$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result2CodeBased$repeatedPValues, result2$repeatedPValues, tolerance = 1e-05)
	    expect_equal(result2CodeBased$finalStage, result2$finalStage, tolerance = 1e-05)
	    expect_equal(result2CodeBased$finalPValues, result2$finalPValues, tolerance = 1e-05)
	    expect_equal(result2CodeBased$finalConfidenceIntervalLowerBounds, result2$finalConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result2CodeBased$finalConfidenceIntervalUpperBounds, result2$finalConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result2CodeBased$medianUnbiasedEstimates, result2$medianUnbiasedEstimates, tolerance = 1e-05)
	    expect_type(names(result2), "character")
	    df <- as.data.frame(result2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(result2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	design6 <- getDesignFisher(kMax = 4, alpha = 0.025)

	stageResults3 <- getStageResults(design6, dataExample2, thetaH0 = 10, stage = 2)

	## Comparison of the results of StageResultsMeans object 'stageResults3' with expected results
	expect_equal(stageResults3$overallTestStatistics, c(1.2040366, 1.8018141, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$overallPValues, c(0.12168078, 0.039654359, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$overallMeans, c(45, 48, 47, NA_real_))
	expect_equal(stageResults3$overallStDevs, c(130, 133.38396, 128.06116, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$overallSampleSizes, c(20, 40, NA_real_, NA_real_))
	expect_equal(stageResults3$testStatistics, c(1.2040366, 1.309697, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$pValues, c(0.12168078, 0.10295724, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$effectSizes, c(45, 48, NA_real_, NA_real_))
	expect_equal(stageResults3$combFisher, c(0.12168078, 0.012527917, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$weightsFisher, c(1, 1, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(stageResults3), NA)))
	    expect_output(print(stageResults3)$show())
	    invisible(capture.output(expect_error(summary(stageResults3), NA)))
	    expect_output(summary(stageResults3)$show())
	    stageResults3CodeBased <- eval(parse(text = getObjectRCode(stageResults3, stringWrapParagraphWidth = NULL)))
	    expect_equal(stageResults3CodeBased$overallTestStatistics, stageResults3$overallTestStatistics, tolerance = 1e-05)
	    expect_equal(stageResults3CodeBased$overallPValues, stageResults3$overallPValues, tolerance = 1e-05)
	    expect_equal(stageResults3CodeBased$overallMeans, stageResults3$overallMeans, tolerance = 1e-05)
	    expect_equal(stageResults3CodeBased$overallStDevs, stageResults3$overallStDevs, tolerance = 1e-05)
	    expect_equal(stageResults3CodeBased$overallSampleSizes, stageResults3$overallSampleSizes, tolerance = 1e-05)
	    expect_equal(stageResults3CodeBased$testStatistics, stageResults3$testStatistics, tolerance = 1e-05)
	    expect_equal(stageResults3CodeBased$pValues, stageResults3$pValues, tolerance = 1e-05)
	    expect_equal(stageResults3CodeBased$effectSizes, stageResults3$effectSizes, tolerance = 1e-05)
	    expect_equal(stageResults3CodeBased$combFisher, stageResults3$combFisher, tolerance = 1e-05)
	    expect_equal(stageResults3CodeBased$weightsFisher, stageResults3$weightsFisher, tolerance = 1e-05)
	    expect_type(names(stageResults3), "character")
	    df <- as.data.frame(stageResults3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(stageResults3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsFisher}
	# @refFS[Formula]{fs:testStatisticOneMean}
	# @refFS[Formula]{fs:pValuesOneMeanAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticFisherCombinationTest}
	# @refFS[Formula]{fs:definitionRCIFisherCombination}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:finalPValueFisherCombinationTest}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	result3 <- getAnalysisResults(
	    design = design6, dataInput = dataExample2, stage = 2,
	    thetaH0 = 10, nPlanned = c(30, 20), thetaH1 = 50, assumedStDev = 100,
	    iterations = 800, seed = 31082018
	)

	## Comparison of the results of AnalysisResultsFisher object 'result3' with expected results
	expect_equal(result3$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(result3$conditionalRejectionProbabilities, c(0.026695414, 0.033302173, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$repeatedConfidenceIntervalLowerBounds, c(-28.274837, -9.0994871, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$repeatedConfidenceIntervalUpperBounds, c(118.27484, 104.78379, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$repeatedPValues, c(0.23830752, 0.14118934, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$finalStage, NA_integer_)
	expect_equal(result3$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$conditionalPowerSimulated, c(NA_real_, NA_real_, 0.54125, 0.8125), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result3), NA)))
	    expect_output(print(result3)$show())
	    invisible(capture.output(expect_error(summary(result3), NA)))
	    expect_output(summary(result3)$show())
	    result3CodeBased <- eval(parse(text = getObjectRCode(result3, stringWrapParagraphWidth = NULL)))
	    expect_equal(result3CodeBased$testActions, result3$testActions, tolerance = 1e-05)
	    expect_equal(result3CodeBased$conditionalRejectionProbabilities, result3$conditionalRejectionProbabilities, tolerance = 1e-05)
	    expect_equal(result3CodeBased$repeatedConfidenceIntervalLowerBounds, result3$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result3CodeBased$repeatedConfidenceIntervalUpperBounds, result3$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result3CodeBased$repeatedPValues, result3$repeatedPValues, tolerance = 1e-05)
	    expect_equal(result3CodeBased$finalStage, result3$finalStage, tolerance = 1e-05)
	    expect_equal(result3CodeBased$finalPValues, result3$finalPValues, tolerance = 1e-05)
	    expect_equal(result3CodeBased$finalConfidenceIntervalLowerBounds, result3$finalConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result3CodeBased$finalConfidenceIntervalUpperBounds, result3$finalConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result3CodeBased$medianUnbiasedEstimates, result3$medianUnbiasedEstimates, tolerance = 1e-05)
	    expect_equal(result3CodeBased$conditionalPowerSimulated, result3$conditionalPowerSimulated, tolerance = 1e-05)
	    expect_type(names(result3), "character")
	    df <- as.data.frame(result3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(result3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_plan_section("Testing the Analysis Means Functionality for Two Treatments")


test_that("'getAnalysisResults' for a Fisher design and a dataset of two means per stage", {

    .skipTestIfDisabled()

	# note: if third stage value of means1 (4.5) increases, lower bound of RCI does not increase
	design7 <- getDesignFisher(kMax = 4, informationRates = c(0.2, 0.5, 0.9, 1), alpha = 0.05, alpha0Vec = rep(0.4, 3))

	dataExample3 <- getDataset(
	    n1 = c(23, 13, 22),
	    n2 = c(22, 11, 22),
	    means1 = c(1, 1.1, 1.3) * 100,
	    means2 = c(1.3, 1.4, 2.5) * 100,
	    stds1 = c(1.3, 2.4, 2.2) * 100,
	    stds2 = c(1.2, 2.2, 2.1) * 100
	)

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsFisher}
	# @refFS[Formula]{fs:testStatisticDifferenceMeansEqualVariances}
	# @refFS[Formula]{fs:pValuesTwoMeansAlternativeGreaterEqualVariances}
	# @refFS[Formula]{fs:testStatisticFisherCombinationTest}
	# @refFS[Formula]{fs:definitionRCIFisherCombination}
	# @refFS[Formula]{fs:definitionRCIwithFutilityFisherCombination}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:finalPValueFisherCombinationTest}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	result <- getAnalysisResults(
	    design = design7, dataInput = dataExample3, equalVariances = TRUE, thetaH0 = 0,
	    directionUpper = FALSE, seed = 123456789
	)

	## Comparison of the results of AnalysisResultsFisher object 'result' with expected results
	expect_equal(result$thetaH1, -66.37931, tolerance = 1e-07)
	expect_equal(result$assumedStDev, 189.41921, tolerance = 1e-07)
	expect_equal(result$testActions, c("continue", "continue", "continue", NA_character_))
	expect_equal(result$conditionalRejectionProbabilities, c(0.044249457, 0.020976199, 0.060555322, NA_real_), tolerance = 1e-07)
	expect_equal(result$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result$repeatedConfidenceIntervalLowerBounds, c(-102.25178, -110.95946, -128.224, NA_real_), tolerance = 1e-07)
	expect_equal(result$repeatedConfidenceIntervalUpperBounds, c(42.251781, 50.959457, 11.069379, NA_real_), tolerance = 1e-07)
	expect_equal(result$repeatedPValues, c(0.25752784, 0.32556092, 0.088271965, NA_real_), tolerance = 1e-07)
	expect_equal(result$finalStage, NA_integer_)
	expect_equal(result$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result), NA)))
	    expect_output(print(result)$show())
	    invisible(capture.output(expect_error(summary(result), NA)))
	    expect_output(summary(result)$show())
	    resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
	    expect_equal(resultCodeBased$thetaH1, result$thetaH1, tolerance = 1e-05)
	    expect_equal(resultCodeBased$assumedStDev, result$assumedStDev, tolerance = 1e-05)
	    expect_equal(resultCodeBased$testActions, result$testActions, tolerance = 1e-05)
	    expect_equal(resultCodeBased$conditionalRejectionProbabilities, result$conditionalRejectionProbabilities, tolerance = 1e-05)
	    expect_equal(resultCodeBased$conditionalPower, result$conditionalPower, tolerance = 1e-05)
	    expect_equal(resultCodeBased$repeatedConfidenceIntervalLowerBounds, result$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(resultCodeBased$repeatedConfidenceIntervalUpperBounds, result$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(resultCodeBased$repeatedPValues, result$repeatedPValues, tolerance = 1e-05)
	    expect_equal(resultCodeBased$finalStage, result$finalStage, tolerance = 1e-05)
	    expect_equal(resultCodeBased$finalPValues, result$finalPValues, tolerance = 1e-05)
	    expect_equal(resultCodeBased$finalConfidenceIntervalLowerBounds, result$finalConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(resultCodeBased$finalConfidenceIntervalUpperBounds, result$finalConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(resultCodeBased$medianUnbiasedEstimates, result$medianUnbiasedEstimates, tolerance = 1e-05)
	    expect_type(names(result), "character")
	    df <- as.data.frame(result)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(result)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getAnalysisResults' for a group sequential design and a dataset of two means per stage, stages: default, 2, 3, and 4", {

	.skipTestIfDisabled()

	dataExample4 <- getDataset(
	    n1 = c(23, 23, 22, 23),
	    n2 = c(22, 22, 22, 21),
	    means1 = c(1.7, 1.5, 1.8, 2.5) * 100,
	    means2 = c(1, 1.1, 1.3, 1) * 100,
	    stds1 = c(1.3, 2.4, 2.2, 1.3) * 100,
	    stds2 = c(1.2, 2.2, 2.1, 1.3) * 100
	)

	design8 <- getDesignGroupSequential(kMax = 4, alpha = 0.025, typeOfDesign = "WT", deltaWT = 0.4)

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsGroupSequential}
	# @refFS[Formula]{fs:testStatisticDifferenceMeansEqualVariances}
	# @refFS[Formula]{fs:pValuesTwoMeansAlternativeGreaterEqualVariances}
	# @refFS[Formula]{fs:testStatisticGroupSequential}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:orderingPValueUpper}
	# @refFS[Formula]{fs:finalCITwoMeans}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerTwoMeansEffect}
	result1 <- getAnalysisResults(
	    design = design8, dataInput = dataExample4, equalVariances = TRUE,
	    stage = 2, nPlanned = c(15, 15), thetaH0 = 0, thetaH1 = 130,
	    assumedStDev = 100, allocationRatioPlanned = 2
	)

	## Comparison of the results of AnalysisResultsGroupSequential object 'result1' with expected results
	expect_equal(result1$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(result1$conditionalRejectionProbabilities, c(0.12319684, 0.052938347, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result1$conditionalPower, c(NA_real_, NA_real_, 0.65019157, 0.95040435), tolerance = 1e-07)
	expect_equal(result1$repeatedConfidenceIntervalLowerBounds, c(-30.185323, -39.416167, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result1$repeatedConfidenceIntervalUpperBounds, c(170.18532, 149.41617, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result1$repeatedPValues, c(0.10782416, 0.1777417, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result1$finalStage, NA_integer_)
	expect_equal(result1$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result1$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result1), NA)))
	    expect_output(print(result1)$show())
	    invisible(capture.output(expect_error(summary(result1), NA)))
	    expect_output(summary(result1)$show())
	    result1CodeBased <- eval(parse(text = getObjectRCode(result1, stringWrapParagraphWidth = NULL)))
	    expect_equal(result1CodeBased$testActions, result1$testActions, tolerance = 1e-05)
	    expect_equal(result1CodeBased$conditionalRejectionProbabilities, result1$conditionalRejectionProbabilities, tolerance = 1e-05)
	    expect_equal(result1CodeBased$conditionalPower, result1$conditionalPower, tolerance = 1e-05)
	    expect_equal(result1CodeBased$repeatedConfidenceIntervalLowerBounds, result1$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result1CodeBased$repeatedConfidenceIntervalUpperBounds, result1$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result1CodeBased$repeatedPValues, result1$repeatedPValues, tolerance = 1e-05)
	    expect_equal(result1CodeBased$finalStage, result1$finalStage, tolerance = 1e-05)
	    expect_equal(result1CodeBased$finalPValues, result1$finalPValues, tolerance = 1e-05)
	    expect_equal(result1CodeBased$finalConfidenceIntervalLowerBounds, result1$finalConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result1CodeBased$finalConfidenceIntervalUpperBounds, result1$finalConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result1CodeBased$medianUnbiasedEstimates, result1$medianUnbiasedEstimates, tolerance = 1e-05)
	    expect_type(names(result1), "character")
	    df <- as.data.frame(result1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(result1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsGroupSequential}
	# @refFS[Formula]{fs:testStatisticDifferenceMeansEqualVariances}
	# @refFS[Formula]{fs:pValuesTwoMeansAlternativeGreaterEqualVariances}
	# @refFS[Formula]{fs:testStatisticGroupSequential}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:orderingPValueUpper}
	# @refFS[Formula]{fs:finalCITwoMeans}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerTwoMeansEffect}
	result4 <- getAnalysisResults(
	    design = design8, dataInput = dataExample4, equalVariances = TRUE,
	    stage = 3, nPlanned = 15, thetaH0 = 0, thetaH1 = 130,
	    assumedStDev = 100, allocationRatioPlanned = 2
	)

	## Comparison of the results of AnalysisResultsGroupSequential object 'result4' with expected results
	expect_equal(result4$testActions, c("continue", "continue", "continue", NA_character_))
	expect_equal(result4$conditionalRejectionProbabilities, c(0.12319684, 0.052938347, 0.042196066, NA_real_), tolerance = 1e-07)
	expect_equal(result4$conditionalPower, c(NA_real_, NA_real_, NA_real_, 0.74141468), tolerance = 1e-07)
	expect_equal(result4$repeatedConfidenceIntervalLowerBounds, c(-30.185323, -39.416167, -24.461261, NA_real_), tolerance = 1e-07)
	expect_equal(result4$repeatedConfidenceIntervalUpperBounds, c(170.18532, 149.41617, 130.73577, NA_real_), tolerance = 1e-07)
	expect_equal(result4$repeatedPValues, c(0.10782416, 0.1777417, 0.11951427, NA_real_), tolerance = 1e-07)
	expect_equal(result4$finalStage, NA_integer_)
	expect_equal(result4$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result4$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result4$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result4$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result4), NA)))
	    expect_output(print(result4)$show())
	    invisible(capture.output(expect_error(summary(result4), NA)))
	    expect_output(summary(result4)$show())
	    result4CodeBased <- eval(parse(text = getObjectRCode(result4, stringWrapParagraphWidth = NULL)))
	    expect_equal(result4CodeBased$testActions, result4$testActions, tolerance = 1e-05)
	    expect_equal(result4CodeBased$conditionalRejectionProbabilities, result4$conditionalRejectionProbabilities, tolerance = 1e-05)
	    expect_equal(result4CodeBased$conditionalPower, result4$conditionalPower, tolerance = 1e-05)
	    expect_equal(result4CodeBased$repeatedConfidenceIntervalLowerBounds, result4$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result4CodeBased$repeatedConfidenceIntervalUpperBounds, result4$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result4CodeBased$repeatedPValues, result4$repeatedPValues, tolerance = 1e-05)
	    expect_equal(result4CodeBased$finalStage, result4$finalStage, tolerance = 1e-05)
	    expect_equal(result4CodeBased$finalPValues, result4$finalPValues, tolerance = 1e-05)
	    expect_equal(result4CodeBased$finalConfidenceIntervalLowerBounds, result4$finalConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result4CodeBased$finalConfidenceIntervalUpperBounds, result4$finalConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result4CodeBased$medianUnbiasedEstimates, result4$medianUnbiasedEstimates, tolerance = 1e-05)
	    expect_type(names(result4), "character")
	    df <- as.data.frame(result4)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(result4)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsGroupSequential}
	# @refFS[Formula]{fs:testStatisticDifferenceMeansEqualVariances}
	# @refFS[Formula]{fs:pValuesTwoMeansAlternativeGreaterEqualVariances}
	# @refFS[Formula]{fs:testStatisticGroupSequential}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:orderingPValueUpper}
	# @refFS[Formula]{fs:finalCITwoMeans}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerTwoMeansEffect}
	result7 <- getAnalysisResults(
	    design = design8, dataInput = dataExample4, equalVariances = TRUE,
	    stage = 4, nPlanned = numeric(0), thetaH0 = 0
	)

	## Comparison of the results of AnalysisResultsGroupSequential object 'result7' with expected results
	expect_equal(result7$thetaH1, 77.467475, tolerance = 1e-07)
	expect_equal(result7$assumedStDev, 180.80733, tolerance = 1e-07)
	expect_equal(result7$testActions, c("continue", "continue", "continue", "reject"))
	expect_equal(result7$conditionalRejectionProbabilities, c(0.12319684, 0.052938347, 0.042196066, NA_real_), tolerance = 1e-07)
	expect_equal(result7$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result7$repeatedConfidenceIntervalLowerBounds, c(-30.185323, -39.416167, -24.461261, 16.408896), tolerance = 1e-07)
	expect_equal(result7$repeatedConfidenceIntervalUpperBounds, c(170.18532, 149.41617, 130.73577, 138.52605), tolerance = 1e-07)
	expect_equal(result7$repeatedPValues, c(0.10782416, 0.1777417, 0.11951427, 0.0045471564), tolerance = 1e-07)
	expect_equal(result7$finalStage, 4)
	expect_equal(result7$finalPValues, c(NA_real_, NA_real_, NA_real_, 0.019111276), tolerance = 1e-07)
	expect_equal(result7$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, 3.8518991), tolerance = 1e-07)
	expect_equal(result7$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, 122.8312), tolerance = 1e-07)
	expect_equal(result7$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, 65.8091), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result7), NA)))
	    expect_output(print(result7)$show())
	    invisible(capture.output(expect_error(summary(result7), NA)))
	    expect_output(summary(result7)$show())
	    result7CodeBased <- eval(parse(text = getObjectRCode(result7, stringWrapParagraphWidth = NULL)))
	    expect_equal(result7CodeBased$thetaH1, result7$thetaH1, tolerance = 1e-05)
	    expect_equal(result7CodeBased$assumedStDev, result7$assumedStDev, tolerance = 1e-05)
	    expect_equal(result7CodeBased$testActions, result7$testActions, tolerance = 1e-05)
	    expect_equal(result7CodeBased$conditionalRejectionProbabilities, result7$conditionalRejectionProbabilities, tolerance = 1e-05)
	    expect_equal(result7CodeBased$conditionalPower, result7$conditionalPower, tolerance = 1e-05)
	    expect_equal(result7CodeBased$repeatedConfidenceIntervalLowerBounds, result7$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result7CodeBased$repeatedConfidenceIntervalUpperBounds, result7$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result7CodeBased$repeatedPValues, result7$repeatedPValues, tolerance = 1e-05)
	    expect_equal(result7CodeBased$finalStage, result7$finalStage, tolerance = 1e-05)
	    expect_equal(result7CodeBased$finalPValues, result7$finalPValues, tolerance = 1e-05)
	    expect_equal(result7CodeBased$finalConfidenceIntervalLowerBounds, result7$finalConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result7CodeBased$finalConfidenceIntervalUpperBounds, result7$finalConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result7CodeBased$medianUnbiasedEstimates, result7$medianUnbiasedEstimates, tolerance = 1e-05)
	    expect_type(names(result7), "character")
	    df <- as.data.frame(result7)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(result7)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getAnalysisResults' for an inverse normal design and a dataset of two means per stage, stages: default, 2, 3, and 4", {

	.skipTestIfDisabled()

	dataExample5 <- getDataset(
	    n1 = c(23, 13, 22, 13),
	    n2 = c(22, 11, 22, 11),
	    means1 = c(1.7, 1.5, 1.8, 2.5) * 100,
	    means2 = c(1, 1.1, 1.3, 1) * 100,
	    stds1 = c(1.3, 2.4, 2.2, 1.3) * 100,
	    stds2 = c(1.2, 2.2, 2.1, 1.3) * 100
	)

	design9 <- getDesignInverseNormal(kMax = 4, alpha = 0.025, typeOfDesign = "WT", deltaWT = 0.4)

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsInverseNormal}
	# @refFS[Formula]{fs:pValuesTwoMeansAlternativeGreaterUnequalVariances}
	# @refFS[Formula]{fs:testStatisticNormalCombinationTest}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:orderingPValueUpper}
	# @refFS[Formula]{fs:finalCITwoMeans}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerTwoMeansEffect}
	result2 <- getAnalysisResults(
	    design = design9, dataInput = dataExample5, equalVariances = FALSE,
	    stage = 2, nPlanned = c(15, 15), thetaH0 = 0, thetaH1 = 130,
	    assumedStDev = 100, allocationRatioPlanned = 2
	)

	## Comparison of the results of AnalysisResultsInverseNormal object 'result2' with expected results
	expect_equal(result2$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(result2$conditionalRejectionProbabilities, c(0.12372016, 0.08089089, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result2$conditionalPower, c(NA_real_, NA_real_, 0.7399771, 0.96741599), tolerance = 1e-07)
	expect_equal(result2$repeatedConfidenceIntervalLowerBounds, c(-30.008991, -32.585516, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result2$repeatedConfidenceIntervalUpperBounds, c(170.00899, 154.76457, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result2$repeatedPValues, c(0.10725005, 0.13184907, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result2$finalStage, NA_integer_)
	expect_equal(result2$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result2$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result2$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result2$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result2), NA)))
	    expect_output(print(result2)$show())
	    invisible(capture.output(expect_error(summary(result2), NA)))
	    expect_output(summary(result2)$show())
	    result2CodeBased <- eval(parse(text = getObjectRCode(result2, stringWrapParagraphWidth = NULL)))
	    expect_equal(result2CodeBased$testActions, result2$testActions, tolerance = 1e-05)
	    expect_equal(result2CodeBased$conditionalRejectionProbabilities, result2$conditionalRejectionProbabilities, tolerance = 1e-05)
	    expect_equal(result2CodeBased$conditionalPower, result2$conditionalPower, tolerance = 1e-05)
	    expect_equal(result2CodeBased$repeatedConfidenceIntervalLowerBounds, result2$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result2CodeBased$repeatedConfidenceIntervalUpperBounds, result2$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result2CodeBased$repeatedPValues, result2$repeatedPValues, tolerance = 1e-05)
	    expect_equal(result2CodeBased$finalStage, result2$finalStage, tolerance = 1e-05)
	    expect_equal(result2CodeBased$finalPValues, result2$finalPValues, tolerance = 1e-05)
	    expect_equal(result2CodeBased$finalConfidenceIntervalLowerBounds, result2$finalConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result2CodeBased$finalConfidenceIntervalUpperBounds, result2$finalConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result2CodeBased$medianUnbiasedEstimates, result2$medianUnbiasedEstimates, tolerance = 1e-05)
	    expect_type(names(result2), "character")
	    df <- as.data.frame(result2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(result2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsInverseNormal}
	# @refFS[Formula]{fs:testStatisticDifferenceMeansUnequalVariances}
	# @refFS[Formula]{fs:pValuesTwoMeansAlternativeGreaterUnequalVariances}
	# @refFS[Formula]{fs:testStatisticNormalCombinationTest}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:orderingPValueUpper}
	# @refFS[Formula]{fs:finalCITwoMeans}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerTwoMeansEffect}
	result5 <- getAnalysisResults(
	    design = design9, dataInput = dataExample5, equalVariances = FALSE,
	    stage = 3, nPlanned = 15, thetaH0 = 0, thetaH1 = 130,
	    assumedStDev = 100, allocationRatioPlanned = 2
	)

	## Comparison of the results of AnalysisResultsInverseNormal object 'result5' with expected results
	expect_equal(result5$testActions, c("continue", "continue", "continue", NA_character_))
	expect_equal(result5$conditionalRejectionProbabilities, c(0.12372016, 0.08089089, 0.073275512, NA_real_), tolerance = 1e-07)
	expect_equal(result5$conditionalPower, c(NA_real_, NA_real_, NA_real_, 0.82164236), tolerance = 1e-07)
	expect_equal(result5$repeatedConfidenceIntervalLowerBounds, c(-30.008991, -32.585516, -19.230333, NA_real_), tolerance = 1e-07)
	expect_equal(result5$repeatedConfidenceIntervalUpperBounds, c(170.00899, 154.76457, 134.96564, NA_real_), tolerance = 1e-07)
	expect_equal(result5$repeatedPValues, c(0.10725005, 0.13184907, 0.088247169, NA_real_), tolerance = 1e-07)
	expect_equal(result5$finalStage, NA_integer_)
	expect_equal(result5$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result5$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result5$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result5$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result5), NA)))
	    expect_output(print(result5)$show())
	    invisible(capture.output(expect_error(summary(result5), NA)))
	    expect_output(summary(result5)$show())
	    result5CodeBased <- eval(parse(text = getObjectRCode(result5, stringWrapParagraphWidth = NULL)))
	    expect_equal(result5CodeBased$testActions, result5$testActions, tolerance = 1e-05)
	    expect_equal(result5CodeBased$conditionalRejectionProbabilities, result5$conditionalRejectionProbabilities, tolerance = 1e-05)
	    expect_equal(result5CodeBased$conditionalPower, result5$conditionalPower, tolerance = 1e-05)
	    expect_equal(result5CodeBased$repeatedConfidenceIntervalLowerBounds, result5$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result5CodeBased$repeatedConfidenceIntervalUpperBounds, result5$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result5CodeBased$repeatedPValues, result5$repeatedPValues, tolerance = 1e-05)
	    expect_equal(result5CodeBased$finalStage, result5$finalStage, tolerance = 1e-05)
	    expect_equal(result5CodeBased$finalPValues, result5$finalPValues, tolerance = 1e-05)
	    expect_equal(result5CodeBased$finalConfidenceIntervalLowerBounds, result5$finalConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result5CodeBased$finalConfidenceIntervalUpperBounds, result5$finalConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result5CodeBased$medianUnbiasedEstimates, result5$medianUnbiasedEstimates, tolerance = 1e-05)
	    expect_type(names(result5), "character")
	    df <- as.data.frame(result5)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(result5)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsInverseNormal}
	# @refFS[Formula]{fs:testStatisticDifferenceMeansUnequalVariances}
	# @refFS[Formula]{fs:pValuesTwoMeansAlternativeGreaterUnequalVariances}
	# @refFS[Formula]{fs:testStatisticNormalCombinationTest}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:orderingPValueUpper}
	# @refFS[Formula]{fs:finalCITwoMeans}
	result8 <- getAnalysisResults(
	    design = design9, dataInput = dataExample5, equalVariances = FALSE,
	    stage = 4, nPlanned = numeric(0), thetaH0 = 0
	)

	## Comparison of the results of AnalysisResultsInverseNormal object 'result8' with expected results
	expect_equal(result8$thetaH1, 72.41784, tolerance = 1e-07)
	expect_equal(result8$assumedStDev, 177.47472, tolerance = 1e-07)
	expect_equal(result8$testActions, c("continue", "continue", "continue", "reject"))
	expect_equal(result8$conditionalRejectionProbabilities, c(0.12372016, 0.08089089, 0.073275512, NA_real_), tolerance = 1e-07)
	expect_equal(result8$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result8$repeatedConfidenceIntervalLowerBounds, c(-30.008991, -32.585516, -19.230333, 16.862491), tolerance = 1e-07)
	expect_equal(result8$repeatedConfidenceIntervalUpperBounds, c(170.00899, 154.76457, 134.96564, 146.10543), tolerance = 1e-07)
	expect_equal(result8$repeatedPValues, c(0.10725005, 0.13184907, 0.088247169, 0.0050030118), tolerance = 1e-07)
	expect_equal(result8$finalStage, 4)
	expect_equal(result8$finalPValues, c(NA_real_, NA_real_, NA_real_, 0.019192988), tolerance = 1e-07)
	expect_equal(result8$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, 4.0866331), tolerance = 1e-07)
	expect_equal(result8$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, 135.35066), tolerance = 1e-07)
	expect_equal(result8$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, 71.819794), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result8), NA)))
	    expect_output(print(result8)$show())
	    invisible(capture.output(expect_error(summary(result8), NA)))
	    expect_output(summary(result8)$show())
	    result8CodeBased <- eval(parse(text = getObjectRCode(result8, stringWrapParagraphWidth = NULL)))
	    expect_equal(result8CodeBased$thetaH1, result8$thetaH1, tolerance = 1e-05)
	    expect_equal(result8CodeBased$assumedStDev, result8$assumedStDev, tolerance = 1e-05)
	    expect_equal(result8CodeBased$testActions, result8$testActions, tolerance = 1e-05)
	    expect_equal(result8CodeBased$conditionalRejectionProbabilities, result8$conditionalRejectionProbabilities, tolerance = 1e-05)
	    expect_equal(result8CodeBased$conditionalPower, result8$conditionalPower, tolerance = 1e-05)
	    expect_equal(result8CodeBased$repeatedConfidenceIntervalLowerBounds, result8$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result8CodeBased$repeatedConfidenceIntervalUpperBounds, result8$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result8CodeBased$repeatedPValues, result8$repeatedPValues, tolerance = 1e-05)
	    expect_equal(result8CodeBased$finalStage, result8$finalStage, tolerance = 1e-05)
	    expect_equal(result8CodeBased$finalPValues, result8$finalPValues, tolerance = 1e-05)
	    expect_equal(result8CodeBased$finalConfidenceIntervalLowerBounds, result8$finalConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result8CodeBased$finalConfidenceIntervalUpperBounds, result8$finalConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result8CodeBased$medianUnbiasedEstimates, result8$medianUnbiasedEstimates, tolerance = 1e-05)
	    expect_type(names(result8), "character")
	    df <- as.data.frame(result8)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(result8)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getAnalysisResults' for a Fisher design and a dataset of two means per stage, stages: default, 2, 3, and 4", {

	.skipTestIfDisabled()

	informationRates <- c(0.2, 0.5, 0.8, 1)

	dataExample6 <- getDataset(
	    n1 = c(23, 13, 22, 13),
	    n2 = c(22, 11, 22, 11),
	    means1 = c(1.7, 1.5, 1.8, 2.5) * 100,
	    means2 = c(1, 1.1, 1.3, 1) * 100,
	    stds1 = c(1.3, 2.4, 2.2, 1.3) * 100,
	    stds2 = c(1.2, 2.2, 2.1, 1.3) * 100
	)

	design10 <- getDesignFisher(
	    kMax = 4, alpha = 0.035,
	    informationRates = informationRates
	)

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsFisher}
	# @refFS[Formula]{fs:testStatisticDifferenceMeansEqualVariances}
	# @refFS[Formula]{fs:pValuesTwoMeansAlternativeGreaterEqualVariances}
	# @refFS[Formula]{fs:testStatisticFisherCombinationTest}
	# @refFS[Formula]{fs:definitionRCIFisherCombination}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:finalPValueFisherCombinationTest}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	result3 <- getAnalysisResults(
	    design = design10, dataInput = dataExample6, equalVariances = TRUE,
	    stage = 2, nPlanned = c(18, 12), thetaH0 = 0, thetaH1 = 130,
	    assumedStDev = 100, allocationRatioPlanned = 2, seed = 123456789
	)

	## Comparison of the results of AnalysisResultsFisher object 'result3' with expected results
	expect_equal(result3$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(result3$conditionalRejectionProbabilities, c(0.092626641, 0.040500778, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$repeatedConfidenceIntervalLowerBounds, c(-14.62622, -29.188312, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$repeatedConfidenceIntervalUpperBounds, c(154.62622, 155.99339, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$repeatedPValues, c(0.078061948, 0.16270991, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$finalStage, NA_integer_)
	expect_equal(result3$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$conditionalPowerSimulated, c(NA_real_, NA_real_, 0.734, 0.933), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result3), NA)))
	    expect_output(print(result3)$show())
	    invisible(capture.output(expect_error(summary(result3), NA)))
	    expect_output(summary(result3)$show())
	    result3CodeBased <- eval(parse(text = getObjectRCode(result3, stringWrapParagraphWidth = NULL)))
	    expect_equal(result3CodeBased$testActions, result3$testActions, tolerance = 1e-05)
	    expect_equal(result3CodeBased$conditionalRejectionProbabilities, result3$conditionalRejectionProbabilities, tolerance = 1e-05)
	    expect_equal(result3CodeBased$repeatedConfidenceIntervalLowerBounds, result3$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result3CodeBased$repeatedConfidenceIntervalUpperBounds, result3$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result3CodeBased$repeatedPValues, result3$repeatedPValues, tolerance = 1e-05)
	    expect_equal(result3CodeBased$finalStage, result3$finalStage, tolerance = 1e-05)
	    expect_equal(result3CodeBased$finalPValues, result3$finalPValues, tolerance = 1e-05)
	    expect_equal(result3CodeBased$finalConfidenceIntervalLowerBounds, result3$finalConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result3CodeBased$finalConfidenceIntervalUpperBounds, result3$finalConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result3CodeBased$medianUnbiasedEstimates, result3$medianUnbiasedEstimates, tolerance = 1e-05)
	    expect_equal(result3CodeBased$conditionalPowerSimulated, result3$conditionalPowerSimulated, tolerance = 1e-05)
	    expect_type(names(result3), "character")
	    df <- as.data.frame(result3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(result3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsFisher}
	# @refFS[Formula]{fs:testStatisticDifferenceMeansEqualVariances}
	# @refFS[Formula]{fs:pValuesTwoMeansAlternativeGreaterEqualVariances}
	# @refFS[Formula]{fs:testStatisticFisherCombinationTest}
	# @refFS[Formula]{fs:definitionRCIFisherCombination}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:finalPValueFisherCombinationTest}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	result6 <- getAnalysisResults(
	    design = design10, dataInput = dataExample6, equalVariances = TRUE,
	    stage = 3, nPlanned = 15, thetaH0 = 0, thetaH1 = 130,
	    assumedStDev = 100, allocationRatioPlanned = 2, seed = 123456789
	)

	## Comparison of the results of AnalysisResultsFisher object 'result6' with expected results
	expect_equal(result6$testActions, c("continue", "continue", "continue", NA_character_))
	expect_equal(result6$conditionalRejectionProbabilities, c(0.092626641, 0.040500778, 0.016148337, NA_real_), tolerance = 1e-07)
	expect_equal(result6$conditionalPower, c(NA_real_, NA_real_, NA_real_, 0.5920203), tolerance = 1e-07)
	expect_equal(result6$repeatedConfidenceIntervalLowerBounds, c(-14.62622, -29.188312, -25.34531, NA_real_), tolerance = 1e-07)
	expect_equal(result6$repeatedConfidenceIntervalUpperBounds, c(154.62622, 155.99339, 144.38935, NA_real_), tolerance = 1e-07)
	expect_equal(result6$repeatedPValues, c(0.078061948, 0.16270991, 0.16485567, NA_real_), tolerance = 1e-07)
	expect_equal(result6$finalStage, NA_integer_)
	expect_equal(result6$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result6$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result6$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result6$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result6), NA)))
	    expect_output(print(result6)$show())
	    invisible(capture.output(expect_error(summary(result6), NA)))
	    expect_output(summary(result6)$show())
	    result6CodeBased <- eval(parse(text = getObjectRCode(result6, stringWrapParagraphWidth = NULL)))
	    expect_equal(result6CodeBased$testActions, result6$testActions, tolerance = 1e-05)
	    expect_equal(result6CodeBased$conditionalRejectionProbabilities, result6$conditionalRejectionProbabilities, tolerance = 1e-05)
	    expect_equal(result6CodeBased$conditionalPower, result6$conditionalPower, tolerance = 1e-05)
	    expect_equal(result6CodeBased$repeatedConfidenceIntervalLowerBounds, result6$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result6CodeBased$repeatedConfidenceIntervalUpperBounds, result6$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result6CodeBased$repeatedPValues, result6$repeatedPValues, tolerance = 1e-05)
	    expect_equal(result6CodeBased$finalStage, result6$finalStage, tolerance = 1e-05)
	    expect_equal(result6CodeBased$finalPValues, result6$finalPValues, tolerance = 1e-05)
	    expect_equal(result6CodeBased$finalConfidenceIntervalLowerBounds, result6$finalConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result6CodeBased$finalConfidenceIntervalUpperBounds, result6$finalConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result6CodeBased$medianUnbiasedEstimates, result6$medianUnbiasedEstimates, tolerance = 1e-05)
	    expect_type(names(result6), "character")
	    df <- as.data.frame(result6)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(result6)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsFisher}
	# @refFS[Formula]{fs:testStatisticDifferenceMeansEqualVariances}
	# @refFS[Formula]{fs:pValuesTwoMeansAlternativeGreaterEqualVariances}
	# @refFS[Formula]{fs:testStatisticFisherCombinationTest}
	# @refFS[Formula]{fs:definitionRCIFisherCombination}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:finalPValueFisherCombinationTest}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	result9 <- getAnalysisResults(
	    design = design10, dataInput = dataExample6, equalVariances = TRUE,
	    stage = 4, nPlanned = numeric(0), thetaH0 = 0, seed = 123456789
	)

	## Comparison of the results of AnalysisResultsFisher object 'result9' with expected results
	expect_equal(result9$thetaH1, 72.41784, tolerance = 1e-07)
	expect_equal(result9$assumedStDev, 177.47472, tolerance = 1e-07)
	expect_equal(result9$testActions, c("continue", "continue", "continue", "reject"))
	expect_equal(result9$conditionalRejectionProbabilities, c(0.092626641, 0.040500778, 0.016148337, NA_real_), tolerance = 1e-07)
	expect_equal(result9$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result9$repeatedConfidenceIntervalLowerBounds, c(-14.62622, -29.188312, -25.34531, 8.7533154), tolerance = 1e-07)
	expect_equal(result9$repeatedConfidenceIntervalUpperBounds, c(154.62622, 155.99339, 144.38935, 151.28694), tolerance = 1e-07)
	expect_equal(result9$repeatedPValues, c(0.078061948, 0.16270991, 0.16485567, 0.017103207), tolerance = 1e-07)
	expect_equal(result9$finalStage, NA_integer_)
	expect_equal(result9$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result9$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result9$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result9$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result9), NA)))
	    expect_output(print(result9)$show())
	    invisible(capture.output(expect_error(summary(result9), NA)))
	    expect_output(summary(result9)$show())
	    result9CodeBased <- eval(parse(text = getObjectRCode(result9, stringWrapParagraphWidth = NULL)))
	    expect_equal(result9CodeBased$thetaH1, result9$thetaH1, tolerance = 1e-05)
	    expect_equal(result9CodeBased$assumedStDev, result9$assumedStDev, tolerance = 1e-05)
	    expect_equal(result9CodeBased$testActions, result9$testActions, tolerance = 1e-05)
	    expect_equal(result9CodeBased$conditionalRejectionProbabilities, result9$conditionalRejectionProbabilities, tolerance = 1e-05)
	    expect_equal(result9CodeBased$conditionalPower, result9$conditionalPower, tolerance = 1e-05)
	    expect_equal(result9CodeBased$repeatedConfidenceIntervalLowerBounds, result9$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result9CodeBased$repeatedConfidenceIntervalUpperBounds, result9$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result9CodeBased$repeatedPValues, result9$repeatedPValues, tolerance = 1e-05)
	    expect_equal(result9CodeBased$finalStage, result9$finalStage, tolerance = 1e-05)
	    expect_equal(result9CodeBased$finalPValues, result9$finalPValues, tolerance = 1e-05)
	    expect_equal(result9CodeBased$finalConfidenceIntervalLowerBounds, result9$finalConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result9CodeBased$finalConfidenceIntervalUpperBounds, result9$finalConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result9CodeBased$medianUnbiasedEstimates, result9$medianUnbiasedEstimates, tolerance = 1e-05)
	    expect_type(names(result9), "character")
	    df <- as.data.frame(result9)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(result9)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("Check that the conditional power is as expected for different designs and datasets", {

	.skipTestIfDisabled()

	informationRates <- c(0.2, 0.5, 0.8, 1)

	dataExample7 <- getDataset(
	    n1 = c(22, 33, 31, 13),
	    n2 = c(22, 31, 30, 11),
	    means1 = c(1, 1.1, 1, 1),
	    means2 = c(1.4, 1.5, 1, 2.5),
	    stds1 = c(1, 2, 2, 1.3),
	    stds2 = c(1, 2, 2, 1.3)
	)

	design11 <- getDesignGroupSequential(
	    kMax = 4, alpha = 0.025,
	    informationRates = informationRates, futilityBounds = rep(0.5244, 3),
	    bindingFutility = TRUE, typeOfDesign = "WT", deltaWT = 0.45
	)

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsGroupSequential}
	# @refFS[Formula]{fs:testStatisticDifferenceMeansEqualVariances}
	# @refFS[Formula]{fs:pValuesTwoMeansAlternativeSmallerEqualVariances}
	# @refFS[Formula]{fs:testStatisticGroupSequential}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:orderingPValueUpper}
	# @refFS[Formula]{fs:finalCITwoMeans}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerTwoMeansEffect}
	result1 <- getAnalysisResults(
	    design = design11, dataInput = dataExample7, equalVariances = TRUE,
	    directionUpper = FALSE, stage = 2, thetaH0 = 0.2, thetaH1 = -0.2, nPlanned = c(96, 64),
	    allocationRatioPlanned = 3, normalApproximation = FALSE
	)

	## Comparison of the results of AnalysisResultsGroupSequential object 'result1' with expected results
	expect_equal(result1$assumedStDev, 1.6547835, tolerance = 1e-07)
	expect_equal(result1$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(result1$conditionalRejectionProbabilities, c(0.13790633, 0.14848468, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result1$conditionalPower, c(NA_real_, NA_real_, 0.40521176, 0.57857102), tolerance = 1e-07)
	expect_equal(result1$repeatedConfidenceIntervalLowerBounds, c(-1.1558731, -1.1414911, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result1$repeatedConfidenceIntervalUpperBounds, c(0.35587299, 0.34450997, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result1$repeatedPValues, c(0.06267349, 0.061334534, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result1$finalStage, NA_integer_)
	expect_equal(result1$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result1$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result1), NA)))
	    expect_output(print(result1)$show())
	    invisible(capture.output(expect_error(summary(result1), NA)))
	    expect_output(summary(result1)$show())
	    result1CodeBased <- eval(parse(text = getObjectRCode(result1, stringWrapParagraphWidth = NULL)))
	    expect_equal(result1CodeBased$assumedStDev, result1$assumedStDev, tolerance = 1e-05)
	    expect_equal(result1CodeBased$testActions, result1$testActions, tolerance = 1e-05)
	    expect_equal(result1CodeBased$conditionalRejectionProbabilities, result1$conditionalRejectionProbabilities, tolerance = 1e-05)
	    expect_equal(result1CodeBased$conditionalPower, result1$conditionalPower, tolerance = 1e-05)
	    expect_equal(result1CodeBased$repeatedConfidenceIntervalLowerBounds, result1$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result1CodeBased$repeatedConfidenceIntervalUpperBounds, result1$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result1CodeBased$repeatedPValues, result1$repeatedPValues, tolerance = 1e-05)
	    expect_equal(result1CodeBased$finalStage, result1$finalStage, tolerance = 1e-05)
	    expect_equal(result1CodeBased$finalPValues, result1$finalPValues, tolerance = 1e-05)
	    expect_equal(result1CodeBased$finalConfidenceIntervalLowerBounds, result1$finalConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result1CodeBased$finalConfidenceIntervalUpperBounds, result1$finalConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result1CodeBased$medianUnbiasedEstimates, result1$medianUnbiasedEstimates, tolerance = 1e-05)
	    expect_type(names(result1), "character")
	    df <- as.data.frame(result1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(result1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	design12 <- getDesignInverseNormal(
	    kMax = 4, alpha = 0.025,
	    informationRates = informationRates, typeOfDesign = "WT", deltaWT = 0.45
	)

	stageResults <- getStageResults(
	    design = design12, dataInput = dataExample7, equalVariances = TRUE,
	    directionUpper = TRUE, stage = 2, thetaH0 = -1
	)

	## Comparison of the results of StageResultsMeans object 'stageResults' with expected results
	expect_equal(stageResults$overallTestStatistics, c(1.9899749, 1.8884638, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults$overallPValues, c(0.026564837, 0.030848764, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults$overallMeans1, c(1, 1.06, 1.0383721, 1.0333333), tolerance = 1e-07)
	expect_equal(stageResults$overallMeans2, c(1.4, 1.4584906, 1.2927711, 1.4340426), tolerance = 1e-07)
	expect_equal(stageResults$overallStDevs1, c(1, 1.6618374, 1.7796344, 1.7187442), tolerance = 1e-07)
	expect_equal(stageResults$overallStDevs2, c(1, 1.6474262, 1.7846078, 1.7725841), tolerance = 1e-07)
	expect_equal(stageResults$overallSampleSizes1, c(22, 55, NA_real_, NA_real_))
	expect_equal(stageResults$overallSampleSizes2, c(22, 53, NA_real_, NA_real_))
	expect_equal(stageResults$testStatistics, c(1.9899749, 1.1994139, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults$pValues, c(0.026564837, 0.11746538, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults$effectSizes, c(-0.4, -0.39849057, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults$combInverseNormal, c(1.9338654, 2.1431134, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults$weightsInverseNormal, c(0.4472136, 0.54772256, 0.54772256, 0.4472136), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(stageResults), NA)))
	    expect_output(print(stageResults)$show())
	    invisible(capture.output(expect_error(summary(stageResults), NA)))
	    expect_output(summary(stageResults)$show())
	    stageResultsCodeBased <- eval(parse(text = getObjectRCode(stageResults, stringWrapParagraphWidth = NULL)))
	    expect_equal(stageResultsCodeBased$overallTestStatistics, stageResults$overallTestStatistics, tolerance = 1e-05)
	    expect_equal(stageResultsCodeBased$overallPValues, stageResults$overallPValues, tolerance = 1e-05)
	    expect_equal(stageResultsCodeBased$overallMeans1, stageResults$overallMeans1, tolerance = 1e-05)
	    expect_equal(stageResultsCodeBased$overallMeans2, stageResults$overallMeans2, tolerance = 1e-05)
	    expect_equal(stageResultsCodeBased$overallStDevs1, stageResults$overallStDevs1, tolerance = 1e-05)
	    expect_equal(stageResultsCodeBased$overallStDevs2, stageResults$overallStDevs2, tolerance = 1e-05)
	    expect_equal(stageResultsCodeBased$overallSampleSizes1, stageResults$overallSampleSizes1, tolerance = 1e-05)
	    expect_equal(stageResultsCodeBased$overallSampleSizes2, stageResults$overallSampleSizes2, tolerance = 1e-05)
	    expect_equal(stageResultsCodeBased$testStatistics, stageResults$testStatistics, tolerance = 1e-05)
	    expect_equal(stageResultsCodeBased$pValues, stageResults$pValues, tolerance = 1e-05)
	    expect_equal(stageResultsCodeBased$effectSizes, stageResults$effectSizes, tolerance = 1e-05)
	    expect_equal(stageResultsCodeBased$combInverseNormal, stageResults$combInverseNormal, tolerance = 1e-05)
	    expect_equal(stageResultsCodeBased$weightsInverseNormal, stageResults$weightsInverseNormal, tolerance = 1e-05)
	    expect_type(names(stageResults), "character")
	    df <- as.data.frame(stageResults)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(stageResults)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getConditionalPowerMeans}
	# @refFS[Formula]{fs:testStatisticDifferenceMeansEqualVariances}
	# @refFS[Formula]{fs:conditionalPowerTwoMeansEffect}
	conditionalPower <- getConditionalPower(stageResults,
	    thetaH1 = 0.840, nPlanned = c(96, 64), assumedStDev = 2
	)

	## Comparison of the results of ConditionalPowerResultsMeans object 'conditionalPower' with expected results
	expect_equal(conditionalPower$conditionalPower, c(NA_real_, NA_real_, 0.99975751, 0.99999919), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(conditionalPower), NA)))
	    expect_output(print(conditionalPower)$show())
	    invisible(capture.output(expect_error(summary(conditionalPower), NA)))
	    expect_output(summary(conditionalPower)$show())
	    conditionalPowerCodeBased <- eval(parse(text = getObjectRCode(conditionalPower, stringWrapParagraphWidth = NULL)))
	    expect_equal(conditionalPowerCodeBased$conditionalPower, conditionalPower$conditionalPower, tolerance = 1e-05)
	    expect_type(names(conditionalPower), "character")
	    df <- as.data.frame(conditionalPower)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(conditionalPower)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	conditionalPowerPlot <- .getConditionalPowerPlot(
	    stageResults = stageResults,
	    thetaRange = seq(-0.8, 0.5, 0.1), nPlanned = c(96, 64), assumedStDev = 2, allocationRatioPlanned = 3
	)

	## Comparison of the results of list object 'conditionalPowerPlot' with expected results
	expect_equal(conditionalPowerPlot$xValues, c(-0.8, -0.7, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5), tolerance = 1e-07)
	expect_equal(conditionalPowerPlot$condPowerValues, c(0.37570702, 0.47532662, 0.57738365, 0.67516684, 0.76267391, 0.83573986, 0.89261201, 0.9338489, 0.96168572, 0.97917178, 0.98938899, 0.99494036, 0.99774434, 0.99906067), tolerance = 1e-07)
	expect_equal(conditionalPowerPlot$likelihoodValues, c(0.45180702, 0.63888737, 0.81863148, 0.95048525, 0.99998877, 0.95331773, 0.82351787, 0.64461615, 0.45721677, 0.29385692, 0.17113644, 0.090311253, 0.043185112, 0.018711949), tolerance = 1e-07)
	expect_equal(conditionalPowerPlot$main, "Conditional Power with Likelihood")
	expect_equal(conditionalPowerPlot$xlab, "Effect size")
	expect_equal(conditionalPowerPlot$ylab, "Conditional power / Likelihood")
	expect_equal(conditionalPowerPlot$sub, "Stage = 2, # of remaining subjects = 160, sd = 2, allocation ratio = 3")

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsInverseNormal}
	# @refFS[Formula]{fs:testStatisticDifferenceMeansEqualVariances}
	# @refFS[Formula]{fs:pValuesTwoMeansAlternativeGreaterEqualVariances}
	# @refFS[Formula]{fs:testStatisticNormalCombinationTest}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:orderingPValueLower}
	# @refFS[Formula]{fs:finalCITwoMeans}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerTwoMeansEffect}
	result2 <- getAnalysisResults(
	    design = design12, dataInput = dataExample7, equalVariances = TRUE,
	    directionUpper = FALSE, stage = 2, thetaH0 = 0.2, thetaH1 = -0.2, nPlanned = c(96, 64),
	    allocationRatioPlanned = 3, normalApproximation = FALSE
	)

	## Comparison of the results of AnalysisResultsInverseNormal object 'result2' with expected results
	expect_equal(result2$assumedStDev, 1.6547835, tolerance = 1e-07)
	expect_equal(result2$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(result2$conditionalRejectionProbabilities, c(0.11857307, 0.20646025, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result2$conditionalPower, c(NA_real_, NA_real_, 0.50295479, 0.65954708), tolerance = 1e-07)
	expect_equal(result2$repeatedConfidenceIntervalLowerBounds, c(-1.182291, -1.0666303, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result2$repeatedConfidenceIntervalUpperBounds, c(0.3822909, 0.2666303, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result2$repeatedPValues, c(0.081445577, 0.043264349, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result2$finalStage, NA_integer_)
	expect_equal(result2$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result2$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result2$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result2$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result2), NA)))
	    expect_output(print(result2)$show())
	    invisible(capture.output(expect_error(summary(result2), NA)))
	    expect_output(summary(result2)$show())
	    result2CodeBased <- eval(parse(text = getObjectRCode(result2, stringWrapParagraphWidth = NULL)))
	    expect_equal(result2CodeBased$assumedStDev, result2$assumedStDev, tolerance = 1e-05)
	    expect_equal(result2CodeBased$testActions, result2$testActions, tolerance = 1e-05)
	    expect_equal(result2CodeBased$conditionalRejectionProbabilities, result2$conditionalRejectionProbabilities, tolerance = 1e-05)
	    expect_equal(result2CodeBased$conditionalPower, result2$conditionalPower, tolerance = 1e-05)
	    expect_equal(result2CodeBased$repeatedConfidenceIntervalLowerBounds, result2$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result2CodeBased$repeatedConfidenceIntervalUpperBounds, result2$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result2CodeBased$repeatedPValues, result2$repeatedPValues, tolerance = 1e-05)
	    expect_equal(result2CodeBased$finalStage, result2$finalStage, tolerance = 1e-05)
	    expect_equal(result2CodeBased$finalPValues, result2$finalPValues, tolerance = 1e-05)
	    expect_equal(result2CodeBased$finalConfidenceIntervalLowerBounds, result2$finalConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result2CodeBased$finalConfidenceIntervalUpperBounds, result2$finalConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result2CodeBased$medianUnbiasedEstimates, result2$medianUnbiasedEstimates, tolerance = 1e-05)
	    expect_type(names(result2), "character")
	    df <- as.data.frame(result2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(result2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsFisher}
	# @refFS[Formula]{fs:testStatisticDifferenceMeansEqualVariances}
	# @refFS[Formula]{fs:pValuesTwoMeansAlternativeGreaterEqualVariances}
	# @refFS[Formula]{fs:testStatisticFisherCombinationTest}
	# @refFS[Formula]{fs:definitionRCIFisherCombination}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerTwoMeansEffect}
	design13 <- getDesignFisher(kMax = 4, alpha = 0.025, informationRates = informationRates)

	result3 <- getAnalysisResults(
	    design = design13, dataInput = dataExample7, equalVariances = TRUE,
	    directionUpper = FALSE, stage = 2, nPlanned = c(96, 64), thetaH1 = -0.4, allocationRatioPlanned = 2,
	    normalApproximation = FALSE, iterations = 10000, seed = 442018
	)

	## Comparison of the results of AnalysisResultsFisher object 'result3' with expected results
	expect_equal(result3$assumedStDev, 1.6547835, tolerance = 1e-07)
	expect_equal(result3$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(result3$conditionalRejectionProbabilities, c(0.031447357, 0.018451139, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$repeatedConfidenceIntervalLowerBounds, c(-1.1295139, -1.1012297, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$repeatedConfidenceIntervalUpperBounds, c(0.32951385, 0.30122972, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$repeatedPValues, c(0.19930232, 0.21960219, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$finalStage, NA_integer_)
	expect_equal(result3$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$conditionalPowerSimulated, c(NA_real_, NA_real_, 0.1239, 0.2143), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result3), NA)))
	    expect_output(print(result3)$show())
	    invisible(capture.output(expect_error(summary(result3), NA)))
	    expect_output(summary(result3)$show())
	    result3CodeBased <- eval(parse(text = getObjectRCode(result3, stringWrapParagraphWidth = NULL)))
	    expect_equal(result3CodeBased$assumedStDev, result3$assumedStDev, tolerance = 1e-05)
	    expect_equal(result3CodeBased$testActions, result3$testActions, tolerance = 1e-05)
	    expect_equal(result3CodeBased$conditionalRejectionProbabilities, result3$conditionalRejectionProbabilities, tolerance = 1e-05)
	    expect_equal(result3CodeBased$repeatedConfidenceIntervalLowerBounds, result3$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result3CodeBased$repeatedConfidenceIntervalUpperBounds, result3$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result3CodeBased$repeatedPValues, result3$repeatedPValues, tolerance = 1e-05)
	    expect_equal(result3CodeBased$finalStage, result3$finalStage, tolerance = 1e-05)
	    expect_equal(result3CodeBased$finalPValues, result3$finalPValues, tolerance = 1e-05)
	    expect_equal(result3CodeBased$finalConfidenceIntervalLowerBounds, result3$finalConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(result3CodeBased$finalConfidenceIntervalUpperBounds, result3$finalConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(result3CodeBased$medianUnbiasedEstimates, result3$medianUnbiasedEstimates, tolerance = 1e-05)
	    expect_equal(result3CodeBased$conditionalPowerSimulated, result3$conditionalPowerSimulated, tolerance = 1e-05)
	    expect_type(names(result3), "character")
	    df <- as.data.frame(result3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(result3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_plan_section("Testing 'getStageResults'")


test_that("'getStageResults' for an inverse normal design and one or two treatments", {
        
    .skipTestIfDisabled()
        
	designInverseNormal <- getDesignInverseNormal(
	    kMax = 4, alpha = 0.025, sided = 1,
	    typeOfDesign = "WT",
	    deltaWT = 0.25, futilityBounds = rep(qnorm(0.7), 3)
	)

	dataExample8 <- getDataset(
	    n = c(10, 10),
	    means = c(2, 3),
	    stDevs = c(1, 1.5)
	)

	# @refFS[Tab.]{fs:tab:output:getStageResultsMeans}
	# @refFS[Formula]{fs:testStatisticOneMean}
	# @refFS[Formula]{fs:pValuesOneMeanAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticNormalCombinationTest}
	stageResults1 <- getStageResults(
	    design = designInverseNormal, dataInput = dataExample8, stage = 2,
	    thetaH0 = 0,
	    directionUpper = TRUE,
	    normalApproximation = FALSE,
	    equalVariances = TRUE
	)

	## Comparison of the results of StageResultsMeans object 'stageResults1' with expected results
	expect_equal(stageResults1$overallTestStatistics, c(6.3245553, 8.3272484, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$overallPValues, c(6.846828e-05, 4.5964001e-08, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$overallMeans, c(2, 2.5, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$overallStDevs, c(1, 1.3426212, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$overallSampleSizes, c(10, 20, NA_real_, NA_real_))
	expect_equal(stageResults1$testStatistics, c(6.3245553, 6.3245553, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$pValues, c(6.846828e-05, 6.846828e-05, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$effectSizes, c(2, 2.5, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$combInverseNormal, c(3.813637, 5.3932972, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$weightsInverseNormal, c(0.5, 0.5, 0.5, 0.5), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(stageResults1), NA)))
	    expect_output(print(stageResults1)$show())
	    invisible(capture.output(expect_error(summary(stageResults1), NA)))
	    expect_output(summary(stageResults1)$show())
	    stageResults1CodeBased <- eval(parse(text = getObjectRCode(stageResults1, stringWrapParagraphWidth = NULL)))
	    expect_equal(stageResults1CodeBased$overallTestStatistics, stageResults1$overallTestStatistics, tolerance = 1e-05)
	    expect_equal(stageResults1CodeBased$overallPValues, stageResults1$overallPValues, tolerance = 1e-05)
	    expect_equal(stageResults1CodeBased$overallMeans, stageResults1$overallMeans, tolerance = 1e-05)
	    expect_equal(stageResults1CodeBased$overallStDevs, stageResults1$overallStDevs, tolerance = 1e-05)
	    expect_equal(stageResults1CodeBased$overallSampleSizes, stageResults1$overallSampleSizes, tolerance = 1e-05)
	    expect_equal(stageResults1CodeBased$testStatistics, stageResults1$testStatistics, tolerance = 1e-05)
	    expect_equal(stageResults1CodeBased$pValues, stageResults1$pValues, tolerance = 1e-05)
	    expect_equal(stageResults1CodeBased$effectSizes, stageResults1$effectSizes, tolerance = 1e-05)
	    expect_equal(stageResults1CodeBased$combInverseNormal, stageResults1$combInverseNormal, tolerance = 1e-05)
	    expect_equal(stageResults1CodeBased$weightsInverseNormal, stageResults1$weightsInverseNormal, tolerance = 1e-05)
	    expect_type(names(stageResults1), "character")
	    df <- as.data.frame(stageResults1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(stageResults1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	dataExample9 <- getDataset(
	    n1 = c(22, 11, 22, 11),
	    n2 = c(22, 13, 22, 13),
	    means1 = c(1, 1.1, 1, 1),
	    means2 = c(1.4, 1.5, 3, 2.5),
	    stDevs1 = c(1, 2, 2, 1.3),
	    stDevs2 = c(1, 2, 2, 1.3)
	)

	# @refFS[Tab.]{fs:tab:output:getStageResultsMeans}
	# @refFS[Formula]{fs:testStatisticDifferenceMeansEqualVariances}
	# @refFS[Formula]{fs:pValuesTwoMeansAlternativeGreaterEqualVariances}
	# @refFS[Formula]{fs:testStatisticNormalCombinationTest}
	stageResults2 <- getStageResults(
	    design = designInverseNormal, dataInput = dataExample9, stage = 2,
	    thetaH0 = 0,
	    directionUpper = TRUE,
	    normalApproximation = FALSE,
	    equalVariances = TRUE
	)

	## Comparison of the results of StageResultsMeans object 'stageResults2' with expected results
	expect_equal(stageResults2$overallTestStatistics, c(-1.3266499, -1.1850988, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$overallPValues, c(0.90410354, 0.87988596, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$overallMeans1, c(1, 1.0333333, 1.02, 1.0166667), tolerance = 1e-07)
	expect_equal(stageResults2$overallMeans2, c(1.4, 1.4371429, 2.0403509, 2.1257143), tolerance = 1e-07)
	expect_equal(stageResults2$overallStDevs1, c(1, 1.3814998, 1.6391506, 1.5786638), tolerance = 1e-07)
	expect_equal(stageResults2$overallStDevs2, c(1, 1.4254175, 1.8228568, 1.7387056), tolerance = 1e-07)
	expect_equal(stageResults2$overallSampleSizes1, c(22, 33, NA_real_, NA_real_))
	expect_equal(stageResults2$overallSampleSizes2, c(22, 35, NA_real_, NA_real_))
	expect_equal(stageResults2$testStatistics, c(-1.3266499, -0.48819395, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$pValues, c(0.90410354, 0.68487854, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$effectSizes, c(-0.4, -0.40380952, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$combInverseNormal, c(-1.3052935, -1.2633725, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$weightsInverseNormal, c(0.5, 0.5, 0.5, 0.5), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(stageResults2), NA)))
	    expect_output(print(stageResults2)$show())
	    invisible(capture.output(expect_error(summary(stageResults2), NA)))
	    expect_output(summary(stageResults2)$show())
	    stageResults2CodeBased <- eval(parse(text = getObjectRCode(stageResults2, stringWrapParagraphWidth = NULL)))
	    expect_equal(stageResults2CodeBased$overallTestStatistics, stageResults2$overallTestStatistics, tolerance = 1e-05)
	    expect_equal(stageResults2CodeBased$overallPValues, stageResults2$overallPValues, tolerance = 1e-05)
	    expect_equal(stageResults2CodeBased$overallMeans1, stageResults2$overallMeans1, tolerance = 1e-05)
	    expect_equal(stageResults2CodeBased$overallMeans2, stageResults2$overallMeans2, tolerance = 1e-05)
	    expect_equal(stageResults2CodeBased$overallStDevs1, stageResults2$overallStDevs1, tolerance = 1e-05)
	    expect_equal(stageResults2CodeBased$overallStDevs2, stageResults2$overallStDevs2, tolerance = 1e-05)
	    expect_equal(stageResults2CodeBased$overallSampleSizes1, stageResults2$overallSampleSizes1, tolerance = 1e-05)
	    expect_equal(stageResults2CodeBased$overallSampleSizes2, stageResults2$overallSampleSizes2, tolerance = 1e-05)
	    expect_equal(stageResults2CodeBased$testStatistics, stageResults2$testStatistics, tolerance = 1e-05)
	    expect_equal(stageResults2CodeBased$pValues, stageResults2$pValues, tolerance = 1e-05)
	    expect_equal(stageResults2CodeBased$effectSizes, stageResults2$effectSizes, tolerance = 1e-05)
	    expect_equal(stageResults2CodeBased$combInverseNormal, stageResults2$combInverseNormal, tolerance = 1e-05)
	    expect_equal(stageResults2CodeBased$weightsInverseNormal, stageResults2$weightsInverseNormal, tolerance = 1e-05)
	    expect_type(names(stageResults2), "character")
	    df <- as.data.frame(stageResults2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(stageResults2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getStageResults' for a Fisher design and one or two treatments", {

	.skipTestIfDisabled()

	designFisher <- getDesignFisher(
	    kMax = 2, alpha = 0.025,
	    alpha0Vec = 1, informationRates = c(0.5, 1),
	    method = "equalAlpha"
	)

	dataExample10 <- getDataset(
	    n = c(10, 10),
	    means = c(2, 3),
	    stDevs = c(1, 1.5)
	)

	# @refFS[Tab.]{fs:tab:output:getStageResultsMeans}
	# @refFS[Formula]{fs:testStatisticOneMean}
	# @refFS[Formula]{fs:pValuesOneMeanAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticFisherCombinationTest}
	stageResults3 <- getStageResults(
	    design = designFisher, dataInput = dataExample10, stage = 2,
	    thetaH0 = 0,
	    directionUpper = TRUE,
	    normalApproximation = FALSE,
	    equalVariances = TRUE
	)

	## Comparison of the results of StageResultsMeans object 'stageResults3' with expected results
	expect_equal(stageResults3$overallTestStatistics, c(6.3245553, 8.3272484), tolerance = 1e-07)
	expect_equal(stageResults3$overallPValues, c(6.846828e-05, 4.5964001e-08), tolerance = 1e-07)
	expect_equal(stageResults3$overallMeans, c(2, 2.5), tolerance = 1e-07)
	expect_equal(stageResults3$overallStDevs, c(1, 1.3426212), tolerance = 1e-07)
	expect_equal(stageResults3$overallSampleSizes, c(10, 20))
	expect_equal(stageResults3$testStatistics, c(6.3245553, 6.3245553), tolerance = 1e-07)
	expect_equal(stageResults3$pValues, c(6.846828e-05, 6.846828e-05), tolerance = 1e-07)
	expect_equal(stageResults3$effectSizes, c(2, 2.5), tolerance = 1e-07)
	expect_equal(stageResults3$combFisher, c(6.846828e-05, 4.6879053e-09), tolerance = 1e-07)
	expect_equal(stageResults3$weightsFisher, c(1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(stageResults3), NA)))
	    expect_output(print(stageResults3)$show())
	    invisible(capture.output(expect_error(summary(stageResults3), NA)))
	    expect_output(summary(stageResults3)$show())
	    stageResults3CodeBased <- eval(parse(text = getObjectRCode(stageResults3, stringWrapParagraphWidth = NULL)))
	    expect_equal(stageResults3CodeBased$overallTestStatistics, stageResults3$overallTestStatistics, tolerance = 1e-05)
	    expect_equal(stageResults3CodeBased$overallPValues, stageResults3$overallPValues, tolerance = 1e-05)
	    expect_equal(stageResults3CodeBased$overallMeans, stageResults3$overallMeans, tolerance = 1e-05)
	    expect_equal(stageResults3CodeBased$overallStDevs, stageResults3$overallStDevs, tolerance = 1e-05)
	    expect_equal(stageResults3CodeBased$overallSampleSizes, stageResults3$overallSampleSizes, tolerance = 1e-05)
	    expect_equal(stageResults3CodeBased$testStatistics, stageResults3$testStatistics, tolerance = 1e-05)
	    expect_equal(stageResults3CodeBased$pValues, stageResults3$pValues, tolerance = 1e-05)
	    expect_equal(stageResults3CodeBased$effectSizes, stageResults3$effectSizes, tolerance = 1e-05)
	    expect_equal(stageResults3CodeBased$combFisher, stageResults3$combFisher, tolerance = 1e-05)
	    expect_equal(stageResults3CodeBased$weightsFisher, stageResults3$weightsFisher, tolerance = 1e-05)
	    expect_type(names(stageResults3), "character")
	    df <- as.data.frame(stageResults3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(stageResults3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	dataExample11 <- getDataset(
	    n1 = c(22, 11),
	    n2 = c(22, 13),
	    means1 = c(1, 1.1),
	    means2 = c(1.4, 1.5),
	    stDevs1 = c(1, 2),
	    stDevs2 = c(1, 2)
	)

	# @refFS[Tab.]{fs:tab:output:getStageResultsMeans}
	# @refFS[Formula]{fs:testStatisticDifferenceMeansEqualVariances}
	# @refFS[Formula]{fs:pValuesTwoMeansAlternativeGreaterEqualVariances}
	# @refFS[Formula]{fs:testStatisticFisherCombinationTest}
	stageResults4 <- getStageResults(
	    design = designFisher, dataInput = dataExample11, stage = 2,
	    thetaH0 = 0,
	    directionUpper = TRUE,
	    normalApproximation = FALSE,
	    equalVariances = TRUE
	)

	## Comparison of the results of StageResultsMeans object 'stageResults4' with expected results
	expect_equal(stageResults4$overallTestStatistics, c(-1.3266499, -1.1850988), tolerance = 1e-07)
	expect_equal(stageResults4$overallPValues, c(0.90410354, 0.87988596), tolerance = 1e-07)
	expect_equal(stageResults4$overallMeans1, c(1, 1.0333333), tolerance = 1e-07)
	expect_equal(stageResults4$overallMeans2, c(1.4, 1.4371429), tolerance = 1e-07)
	expect_equal(stageResults4$overallStDevs1, c(1, 1.3814998), tolerance = 1e-07)
	expect_equal(stageResults4$overallStDevs2, c(1, 1.4254175), tolerance = 1e-07)
	expect_equal(stageResults4$overallSampleSizes1, c(22, 33))
	expect_equal(stageResults4$overallSampleSizes2, c(22, 35))
	expect_equal(stageResults4$testStatistics, c(-1.3266499, -0.48819395), tolerance = 1e-07)
	expect_equal(stageResults4$pValues, c(0.90410354, 0.68487854), tolerance = 1e-07)
	expect_equal(stageResults4$effectSizes, c(-0.4, -0.40380952), tolerance = 1e-07)
	expect_equal(stageResults4$combFisher, c(0.90410354, 0.61920111), tolerance = 1e-07)
	expect_equal(stageResults4$weightsFisher, c(1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(stageResults4), NA)))
	    expect_output(print(stageResults4)$show())
	    invisible(capture.output(expect_error(summary(stageResults4), NA)))
	    expect_output(summary(stageResults4)$show())
	    stageResults4CodeBased <- eval(parse(text = getObjectRCode(stageResults4, stringWrapParagraphWidth = NULL)))
	    expect_equal(stageResults4CodeBased$overallTestStatistics, stageResults4$overallTestStatistics, tolerance = 1e-05)
	    expect_equal(stageResults4CodeBased$overallPValues, stageResults4$overallPValues, tolerance = 1e-05)
	    expect_equal(stageResults4CodeBased$overallMeans1, stageResults4$overallMeans1, tolerance = 1e-05)
	    expect_equal(stageResults4CodeBased$overallMeans2, stageResults4$overallMeans2, tolerance = 1e-05)
	    expect_equal(stageResults4CodeBased$overallStDevs1, stageResults4$overallStDevs1, tolerance = 1e-05)
	    expect_equal(stageResults4CodeBased$overallStDevs2, stageResults4$overallStDevs2, tolerance = 1e-05)
	    expect_equal(stageResults4CodeBased$overallSampleSizes1, stageResults4$overallSampleSizes1, tolerance = 1e-05)
	    expect_equal(stageResults4CodeBased$overallSampleSizes2, stageResults4$overallSampleSizes2, tolerance = 1e-05)
	    expect_equal(stageResults4CodeBased$testStatistics, stageResults4$testStatistics, tolerance = 1e-05)
	    expect_equal(stageResults4CodeBased$pValues, stageResults4$pValues, tolerance = 1e-05)
	    expect_equal(stageResults4CodeBased$effectSizes, stageResults4$effectSizes, tolerance = 1e-05)
	    expect_equal(stageResults4CodeBased$combFisher, stageResults4$combFisher, tolerance = 1e-05)
	    expect_equal(stageResults4CodeBased$weightsFisher, stageResults4$weightsFisher, tolerance = 1e-05)
	    expect_type(names(stageResults4), "character")
	    df <- as.data.frame(stageResults4)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(stageResults4)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getAnalysisResults' with a dataset of means and without defining a design", {

	.skipTestIfDisabled()

	dataExample12 <- getDataset(
	    n1 = c(22),
	    n2 = c(21),
	    means1 = c(1.63),
	    means2 = c(1.4),
	    stds1 = c(1.2),
	    stds2 = c(1.3)
	)

	# @refFS[Tab.]{fs:tab:output:getStageResultsMeans}
	# @refFS[Formula]{fs:testStatisticDifferenceMeansEqualVariances}
	# @refFS[Formula]{fs:pValuesTwoMeansAlternativeGreaterEqualVariances}
	analysisResults1 <- getAnalysisResults(dataExample12, alpha = 0.02, sided = 2, stage = 1)

	## Comparison of the results of AnalysisResultsInverseNormal object 'analysisResults1' with expected results
	expect_equal(analysisResults1$thetaH1, 0.23, tolerance = 1e-07)
	expect_equal(analysisResults1$assumedStDev, 1.2497805, tolerance = 1e-07)
	expect_equal(analysisResults1$testActions, "accept")
	expect_equal(analysisResults1$repeatedConfidenceIntervalLowerBounds, -0.69301003, tolerance = 1e-07)
	expect_equal(analysisResults1$repeatedConfidenceIntervalUpperBounds, 1.1530101, tolerance = 1e-07)
	expect_equal(analysisResults1$repeatedPValues, 0.54968031, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(analysisResults1), NA)))
	    expect_output(print(analysisResults1)$show())
	    invisible(capture.output(expect_error(summary(analysisResults1), NA)))
	    expect_output(summary(analysisResults1)$show())
	    analysisResults1CodeBased <- eval(parse(text = getObjectRCode(analysisResults1, stringWrapParagraphWidth = NULL)))
	    expect_equal(analysisResults1CodeBased$thetaH1, analysisResults1$thetaH1, tolerance = 1e-05)
	    expect_equal(analysisResults1CodeBased$assumedStDev, analysisResults1$assumedStDev, tolerance = 1e-05)
	    expect_equal(analysisResults1CodeBased$testActions, analysisResults1$testActions, tolerance = 1e-05)
	    expect_equal(analysisResults1CodeBased$repeatedConfidenceIntervalLowerBounds, analysisResults1$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(analysisResults1CodeBased$repeatedConfidenceIntervalUpperBounds, analysisResults1$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(analysisResults1CodeBased$repeatedPValues, analysisResults1$repeatedPValues, tolerance = 1e-05)
	    expect_type(names(analysisResults1), "character")
	    df <- as.data.frame(analysisResults1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(analysisResults1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getAnalysisResults' with a dataset of means and without early efficacy stop", {

	.skipTestIfDisabled()

	design13 <- getDesignInverseNormal(
	    kMax = 2, alpha = 0.05,
	    typeOfDesign = "noEarlyEfficacy"
	)
	dataExample13 <- getDataset(
	    n1 = c(22, 11),
	    n2 = c(22, 13),
	    means1 = c(1, 3.4),
	    means2 = c(2.4, 4.77),
	    stDevs1 = c(2.2, 2.1),
	    stDevs2 = c(3.1, 3.3)
	)

	# @refFS[Tab.]{fs:tab:output:getStageResultsMeans}
	# @refFS[Formula]{fs:testStatisticDifferenceMeansEqualVariances}
	# @refFS[Formula]{fs:pValuesTwoMeansAlternativeGreaterEqualVariances}
	analysisResults1 <- getAnalysisResults(design13, dataExample13, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsInverseNormal object 'analysisResults1' with expected results
	expect_equal(analysisResults1$thetaH1, -1.4802857, tolerance = 1e-07)
	expect_equal(analysisResults1$assumedStDev, 2.9293915, tolerance = 1e-07)
	expect_equal(analysisResults1$testActions, c("continue", "reject"))
	expect_equal(analysisResults1$conditionalRejectionProbabilities, c(0.26163977, NA_real_), tolerance = 1e-07)
	expect_equal(analysisResults1$conditionalPower, c(NA_real_, NA_real_))
	expect_equal(analysisResults1$repeatedConfidenceIntervalLowerBounds, c(NA_real_, -2.5168979), tolerance = 1e-07)
	expect_equal(analysisResults1$repeatedConfidenceIntervalUpperBounds, c(NA_real_, -0.25840683), tolerance = 1e-07)
	expect_equal(analysisResults1$repeatedPValues, c(NA_real_, 0.022205355), tolerance = 1e-07)
	expect_equal(analysisResults1$finalStage, 2)
	expect_equal(analysisResults1$finalPValues, c(NA_real_, 0.02220507), tolerance = 1e-07)
	expect_equal(analysisResults1$finalConfidenceIntervalLowerBounds, c(NA_real_, -2.6299347), tolerance = 1e-07)
	expect_equal(analysisResults1$finalConfidenceIntervalUpperBounds, c(NA_real_, -0.26287837), tolerance = 1e-07)
	expect_equal(analysisResults1$medianUnbiasedEstimates, c(NA_real_, -1.4464065), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(analysisResults1), NA)))
	    expect_output(print(analysisResults1)$show())
	    invisible(capture.output(expect_error(summary(analysisResults1), NA)))
	    expect_output(summary(analysisResults1)$show())
	    analysisResults1CodeBased <- eval(parse(text = getObjectRCode(analysisResults1, stringWrapParagraphWidth = NULL)))
	    expect_equal(analysisResults1CodeBased$thetaH1, analysisResults1$thetaH1, tolerance = 1e-05)
	    expect_equal(analysisResults1CodeBased$assumedStDev, analysisResults1$assumedStDev, tolerance = 1e-05)
	    expect_equal(analysisResults1CodeBased$testActions, analysisResults1$testActions, tolerance = 1e-05)
	    expect_equal(analysisResults1CodeBased$conditionalRejectionProbabilities, analysisResults1$conditionalRejectionProbabilities, tolerance = 1e-05)
	    expect_equal(analysisResults1CodeBased$conditionalPower, analysisResults1$conditionalPower, tolerance = 1e-05)
	    expect_equal(analysisResults1CodeBased$repeatedConfidenceIntervalLowerBounds, analysisResults1$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(analysisResults1CodeBased$repeatedConfidenceIntervalUpperBounds, analysisResults1$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(analysisResults1CodeBased$repeatedPValues, analysisResults1$repeatedPValues, tolerance = 1e-05)
	    expect_equal(analysisResults1CodeBased$finalStage, analysisResults1$finalStage, tolerance = 1e-05)
	    expect_equal(analysisResults1CodeBased$finalPValues, analysisResults1$finalPValues, tolerance = 1e-05)
	    expect_equal(analysisResults1CodeBased$finalConfidenceIntervalLowerBounds, analysisResults1$finalConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(analysisResults1CodeBased$finalConfidenceIntervalUpperBounds, analysisResults1$finalConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(analysisResults1CodeBased$medianUnbiasedEstimates, analysisResults1$medianUnbiasedEstimates, tolerance = 1e-05)
	    expect_type(names(analysisResults1), "character")
	    df <- as.data.frame(analysisResults1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(analysisResults1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}
})

