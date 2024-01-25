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
## |  File name: test-f_analysis_base_rates.R
## |  Creation date: 08 November 2023, 08:51:06
## |  File version: $Revision: 7560 $
## |  Last changed: $Date: 2024-01-15 14:20:32 +0100 (Mo, 15 Jan 2024) $
## |  Last changed by: $Author: pahlke $
## |  

test_plan_section("Testing the Analysis Rates Functionality for One Treatment")


test_that("'getAnalysisResults' for a group sequential design and one treatment", {
	.skipTestIfDisabled()

	design0 <- getDesignGroupSequential(
	    kMax = 2, alpha = 0.025, informationRates = c(0.2, 1),
	    typeOfDesign = "asKD", gammaA = 2.8
	)

	dataExample0 <- getDataset(
	    n = c(33),
	    events = c(23)
	)

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsGroupSequential}
	# @refFS[Formula]{fs:testStatisticOneRateApproximation}
	# @refFS[Formula]{fs:pValuesOneRateAlternativeSmaller}
	# @refFS[Formula]{fs:testStatisticGroupSequential}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	x0 <- getAnalysisResults(
	    design = design0, dataInput = dataExample0,
	    thetaH0 = 0.4, normalApproximation = FALSE, directionUpper = TRUE
	)

	## Comparison of the results of AnalysisResultsGroupSequential object 'x0' with expected results
	expect_equal(x0$pi1, 0.6969697, tolerance = 1e-06, label = paste0("c(", paste0(x0$pi1, collapse = ", "), ")"))
	expect_equal(x0$testActions, c("continue", NA_character_), label = paste0("c(", paste0(x0$testActions, collapse = ", "), ")"))
	expect_equal(x0$conditionalRejectionProbabilities, c(0.28801679, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(x0$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x0$conditionalPower, c(NA_real_, NA_real_), label = paste0("c(", paste0(x0$conditionalPower, collapse = ", "), ")"))
	expect_equal(x0$repeatedConfidenceIntervalLowerBounds, c(0.38475339, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(x0$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x0$repeatedConfidenceIntervalUpperBounds, c(0.91556361, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(x0$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x0$repeatedPValues, c(0.048557231, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(x0$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x0$finalStage, NA_integer_, label = paste0("c(", paste0(x0$finalStage, collapse = ", "), ")"))
	expect_equal(x0$finalPValues, c(NA_real_, NA_real_), label = paste0("c(", paste0(x0$finalPValues, collapse = ", "), ")"))
	expect_equal(x0$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_), label = paste0("c(", paste0(x0$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x0$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_), label = paste0("c(", paste0(x0$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x0$medianUnbiasedEstimates, c(NA_real_, NA_real_), label = paste0("c(", paste0(x0$medianUnbiasedEstimates, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x0), NA)))
	    expect_output(print(x0)$show())
	    invisible(capture.output(expect_error(summary(x0), NA)))
	    expect_output(summary(x0)$show())
	    x0CodeBased <- eval(parse(text = getObjectRCode(x0, stringWrapParagraphWidth = NULL)))
	    expect_equal(x0CodeBased$pi1, x0$pi1, tolerance = 1e-06)
	    expect_equal(x0CodeBased$testActions, x0$testActions, tolerance = 1e-06)
	    expect_equal(x0CodeBased$conditionalRejectionProbabilities, x0$conditionalRejectionProbabilities, tolerance = 1e-06)
	    expect_equal(x0CodeBased$conditionalPower, x0$conditionalPower, tolerance = 1e-06)
	    expect_equal(x0CodeBased$repeatedConfidenceIntervalLowerBounds, x0$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
	    expect_equal(x0CodeBased$repeatedConfidenceIntervalUpperBounds, x0$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
	    expect_equal(x0CodeBased$repeatedPValues, x0$repeatedPValues, tolerance = 1e-06)
	    expect_equal(x0CodeBased$finalStage, x0$finalStage, tolerance = 1e-06)
	    expect_equal(x0CodeBased$finalPValues, x0$finalPValues, tolerance = 1e-06)
	    expect_equal(x0CodeBased$finalConfidenceIntervalLowerBounds, x0$finalConfidenceIntervalLowerBounds, tolerance = 1e-06)
	    expect_equal(x0CodeBased$finalConfidenceIntervalUpperBounds, x0$finalConfidenceIntervalUpperBounds, tolerance = 1e-06)
	    expect_equal(x0CodeBased$medianUnbiasedEstimates, x0$medianUnbiasedEstimates, tolerance = 1e-06)
	    expect_type(names(x0), "character")
	    df <- as.data.frame(x0)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x0)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getAnalysisResults' for a four-stage group sequential design and one treatment", {

	.skipTestIfDisabled()

	design1 <- getDesignGroupSequential(
	    kMax = 4, alpha = 0.025, informationRates = c(0.2, 0.4, 0.8, 1),
	    futilityBounds = c(-0.5, 0, 0.5), typeOfDesign = "asKD", gammaA = 2.8
	)

	dataExample1 <- getDataset(
	    n = c(10, 10, 20, 11),
	    events = c(4, 5, 5, 6)
	)

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsGroupSequential}
	# @refFS[Formula]{fs:testStatisticOneRateApproximation}
	# @refFS[Formula]{fs:pValuesOneRateAlternativeSmaller}
	# @refFS[Formula]{fs:testStatisticGroupSequential}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	x1 <- getAnalysisResults(
	    design = design1, dataInput = dataExample1,
	    stage = 2, thetaH0 = 0.75, normalApproximation = FALSE, directionUpper = FALSE
	)

	## Comparison of the results of AnalysisResultsGroupSequential object 'x1' with expected results
	expect_equal(x1$pi1, 0.45, tolerance = 1e-07, label = paste0("c(", paste0(x1$pi1, collapse = ", "), ")"))
	expect_equal(x1$testActions, c("continue", "continue", NA_character_, NA_character_), label = paste0("c(", paste0(x1$testActions, collapse = ", "), ")"))
	expect_equal(x1$conditionalRejectionProbabilities, c(0.13502024, 0.39663603, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x1$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$conditionalPower, collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(0.035340833, 0.15564775, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(0.88809209, 0.77284164, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x1$repeatedPValues, c(0.49999905, 0.056127482, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x1$finalStage, NA_integer_, label = paste0("c(", paste0(x1$finalStage, collapse = ", "), ")"))
	expect_equal(x1$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$finalPValues, collapse = ", "), ")"))
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$medianUnbiasedEstimates, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x1), NA)))
	    expect_output(print(x1)$show())
	    invisible(capture.output(expect_error(summary(x1), NA)))
	    expect_output(summary(x1)$show())
	    x1CodeBased <- eval(parse(text = getObjectRCode(x1, stringWrapParagraphWidth = NULL)))
	    expect_equal(x1CodeBased$pi1, x1$pi1, tolerance = 1e-07)
	    expect_equal(x1CodeBased$testActions, x1$testActions, tolerance = 1e-07)
	    expect_equal(x1CodeBased$conditionalRejectionProbabilities, x1$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x1CodeBased$conditionalPower, x1$conditionalPower, tolerance = 1e-07)
	    expect_equal(x1CodeBased$repeatedConfidenceIntervalLowerBounds, x1$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$repeatedConfidenceIntervalUpperBounds, x1$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$repeatedPValues, x1$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x1CodeBased$finalStage, x1$finalStage, tolerance = 1e-07)
	    expect_equal(x1CodeBased$finalPValues, x1$finalPValues, tolerance = 1e-07)
	    expect_equal(x1CodeBased$finalConfidenceIntervalLowerBounds, x1$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$finalConfidenceIntervalUpperBounds, x1$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$medianUnbiasedEstimates, x1$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_type(names(x1), "character")
	    df <- as.data.frame(x1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsGroupSequential}
	# @refFS[Formula]{fs:testStatisticOneRateApproximation}
	# @refFS[Formula]{fs:pValuesOneRateApproximationAlternativeSmaller}
	# @refFS[Formula]{fs:testStatisticGroupSequential}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:orderingPValueUpper}
	# @refFS[Formula]{fs:finalCIOneRate}
	# @refFS[Formula]{fs:medianUnbiasedEstimate}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	x2 <- getAnalysisResults(
	    design = design1, dataInput = dataExample1,
	    stage = 3, thetaH0 = 0.75, normalApproximation = TRUE, directionUpper = FALSE
	)

	## Comparison of the results of AnalysisResultsGroupSequential object 'x2' with expected results
	expect_equal(x2$pi1, 0.35, tolerance = 1e-07, label = paste0("c(", paste0(x2$pi1, collapse = ", "), ")"))
	expect_equal(x2$testActions, c("continue", "reject and stop", "reject and stop", NA_character_), label = paste0("c(", paste0(x2$testActions, collapse = ", "), ")"))
	expect_equal(x2$conditionalRejectionProbabilities, c(0.21465031, 0.55995383, 1, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x2$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$conditionalPower, collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(0.08898791, 0.19243551, 0.206358, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(0.81981977, 0.73748168, 0.52720845, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x2$repeatedPValues, c(0.47958473, 0.014066714, 1.9536724e-06, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x2$finalStage, 2, label = paste0("c(", paste0(x2$finalStage, collapse = ", "), ")"))
	expect_equal(x2$finalPValues, c(NA_real_, 0.0011783609, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$finalPValues, collapse = ", "), ")"))
	expect_equal(x2$finalConfidenceIntervalLowerBounds, c(NA_real_, 0.18821106, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x2$finalConfidenceIntervalUpperBounds, c(NA_real_, 0.62661997, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x2$medianUnbiasedEstimates, c(NA_real_, 0.40681825, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$medianUnbiasedEstimates, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x2), NA)))
	    expect_output(print(x2)$show())
	    invisible(capture.output(expect_error(summary(x2), NA)))
	    expect_output(summary(x2)$show())
	    x2CodeBased <- eval(parse(text = getObjectRCode(x2, stringWrapParagraphWidth = NULL)))
	    expect_equal(x2CodeBased$pi1, x2$pi1, tolerance = 1e-07)
	    expect_equal(x2CodeBased$testActions, x2$testActions, tolerance = 1e-07)
	    expect_equal(x2CodeBased$conditionalRejectionProbabilities, x2$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x2CodeBased$conditionalPower, x2$conditionalPower, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedConfidenceIntervalLowerBounds, x2$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedConfidenceIntervalUpperBounds, x2$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedPValues, x2$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x2CodeBased$finalStage, x2$finalStage, tolerance = 1e-07)
	    expect_equal(x2CodeBased$finalPValues, x2$finalPValues, tolerance = 1e-07)
	    expect_equal(x2CodeBased$finalConfidenceIntervalLowerBounds, x2$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$finalConfidenceIntervalUpperBounds, x2$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$medianUnbiasedEstimates, x2$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_type(names(x2), "character")
	    df <- as.data.frame(x2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsGroupSequential}
	# @refFS[Formula]{fs:testStatisticOneRateApproximation}
	# @refFS[Formula]{fs:pValuesOneRateAlternativeSmaller}
	# @refFS[Formula]{fs:testStatisticGroupSequential}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerOneRateEffect}
	x3 <- getAnalysisResults(
	    design = design1, dataInput = dataExample1,
	    stage = 2, thetaH0 = 0.75, nPlanned = c(12, 6), pi1 = 0.5,
	    normalApproximation = FALSE, directionUpper = FALSE
	)

	## Comparison of the results of AnalysisResultsGroupSequential object 'x3' with expected results
	expect_equal(x3$testActions, c("continue", "continue", NA_character_, NA_character_), label = paste0("c(", paste0(x3$testActions, collapse = ", "), ")"))
	expect_equal(x3$conditionalRejectionProbabilities, c(0.13502024, 0.39663603, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x3$conditionalPower, c(NA_real_, NA_real_, 0.85193241, 0.94869662), tolerance = 1e-07, label = paste0("c(", paste0(x3$conditionalPower, collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds, c(0.035340833, 0.15564775, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds, c(0.88809209, 0.77284164, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x3$repeatedPValues, c(0.49999905, 0.056127482, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x3$finalStage, NA_integer_, label = paste0("c(", paste0(x3$finalStage, collapse = ", "), ")"))
	expect_equal(x3$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$finalPValues, collapse = ", "), ")"))
	expect_equal(x3$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x3$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x3$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$medianUnbiasedEstimates, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x3), NA)))
	    expect_output(print(x3)$show())
	    invisible(capture.output(expect_error(summary(x3), NA)))
	    expect_output(summary(x3)$show())
	    x3CodeBased <- eval(parse(text = getObjectRCode(x3, stringWrapParagraphWidth = NULL)))
	    expect_equal(x3CodeBased$testActions, x3$testActions, tolerance = 1e-07)
	    expect_equal(x3CodeBased$conditionalRejectionProbabilities, x3$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x3CodeBased$conditionalPower, x3$conditionalPower, tolerance = 1e-07)
	    expect_equal(x3CodeBased$repeatedConfidenceIntervalLowerBounds, x3$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x3CodeBased$repeatedConfidenceIntervalUpperBounds, x3$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x3CodeBased$repeatedPValues, x3$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x3CodeBased$finalStage, x3$finalStage, tolerance = 1e-07)
	    expect_equal(x3CodeBased$finalPValues, x3$finalPValues, tolerance = 1e-07)
	    expect_equal(x3CodeBased$finalConfidenceIntervalLowerBounds, x3$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x3CodeBased$finalConfidenceIntervalUpperBounds, x3$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x3CodeBased$medianUnbiasedEstimates, x3$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_type(names(x3), "character")
	    df <- as.data.frame(x3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	plotData1 <- testGetAnalysisResultsPlotData(x3, piTreatmentRange = seq(0.45, 0.75, 0.05))

	## Comparison of the results of list object 'plotData1' with expected results
	expect_equal(plotData1$xValues, c(0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75), tolerance = 1e-07, label = paste0("c(", paste0(plotData1$xValues, collapse = ", "), ")"))
	expect_equal(plotData1$condPowerValues, c(0.98024945, 0.94869662, 0.88988709, 0.79611571, 0.66506207, 0.50313626, 0.32784789), tolerance = 1e-07, label = paste0("c(", paste0(plotData1$condPowerValues, collapse = ", "), ")"))
	expect_equal(plotData1$likelihoodValues, c(1, 0.9039239, 0.66761715, 0.40289032, 0.19865977, 0.080038099, 0.026347981), tolerance = 1e-07, label = paste0("c(", paste0(plotData1$likelihoodValues, collapse = ", "), ")"))
	expect_equal(plotData1$main, "Conditional Power with Likelihood", label = paste0("c(", paste0(plotData1$main, collapse = ", "), ")"))
	expect_equal(plotData1$xlab, "pi1", label = paste0("c(", paste0(plotData1$xlab, collapse = ", "), ")"))
	expect_equal(plotData1$ylab, "Conditional power / Likelihood", label = paste0("c(", paste0(plotData1$ylab, collapse = ", "), ")"))
	expect_equal(plotData1$sub, "Stage = 2, # of remaining subjects = 18", label = paste0("c(", paste0(plotData1$sub, collapse = ", "), ")"))

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsGroupSequential}
	# @refFS[Formula]{fs:testStatisticOneRateApproximation}
	# @refFS[Formula]{fs:pValuesOneRateApproximationAlternativeSmaller}
	# @refFS[Formula]{fs:testStatisticGroupSequential}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:orderingPValueUpper}
	# @refFS[Formula]{fs:medianUnbiasedEstimate}
	# @refFS[Formula]{fs:finalCIOneRate}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerOneRateEffect}
	x4 <- getAnalysisResults(
	    design = design1, dataInput = dataExample1,
	    stage = 2, thetaH0 = 0.75, nPlanned = c(12, 6), pi1 = 0.5,
	    normalApproximation = TRUE, directionUpper = FALSE
	)

	## Comparison of the results of AnalysisResultsGroupSequential object 'x4' with expected results
	expect_equal(x4$testActions, c("continue", "reject and stop", NA_character_, NA_character_), label = paste0("c(", paste0(x4$testActions, collapse = ", "), ")"))
	expect_equal(x4$conditionalRejectionProbabilities, c(0.21465031, 0.55995383, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x4$conditionalPower, c(NA_real_, NA_real_, 0.9494174, 0.9843063), tolerance = 1e-07, label = paste0("c(", paste0(x4$conditionalPower, collapse = ", "), ")"))
	expect_equal(x4$repeatedConfidenceIntervalLowerBounds, c(0.08898791, 0.19243551, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x4$repeatedConfidenceIntervalUpperBounds, c(0.81981977, 0.73748168, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x4$repeatedPValues, c(0.47958473, 0.014066714, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x4$finalStage, 2, label = paste0("c(", paste0(x4$finalStage, collapse = ", "), ")"))
	expect_equal(x4$finalPValues, c(NA_real_, 0.0011783609, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$finalPValues, collapse = ", "), ")"))
	expect_equal(x4$finalConfidenceIntervalLowerBounds, c(NA_real_, 0.18821106, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x4$finalConfidenceIntervalUpperBounds, c(NA_real_, 0.62661997, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x4$medianUnbiasedEstimates, c(NA_real_, 0.40681825, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$medianUnbiasedEstimates, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x4), NA)))
	    expect_output(print(x4)$show())
	    invisible(capture.output(expect_error(summary(x4), NA)))
	    expect_output(summary(x4)$show())
	    x4CodeBased <- eval(parse(text = getObjectRCode(x4, stringWrapParagraphWidth = NULL)))
	    expect_equal(x4CodeBased$testActions, x4$testActions, tolerance = 1e-07)
	    expect_equal(x4CodeBased$conditionalRejectionProbabilities, x4$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x4CodeBased$conditionalPower, x4$conditionalPower, tolerance = 1e-07)
	    expect_equal(x4CodeBased$repeatedConfidenceIntervalLowerBounds, x4$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x4CodeBased$repeatedConfidenceIntervalUpperBounds, x4$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x4CodeBased$repeatedPValues, x4$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x4CodeBased$finalStage, x4$finalStage, tolerance = 1e-07)
	    expect_equal(x4CodeBased$finalPValues, x4$finalPValues, tolerance = 1e-07)
	    expect_equal(x4CodeBased$finalConfidenceIntervalLowerBounds, x4$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x4CodeBased$finalConfidenceIntervalUpperBounds, x4$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x4CodeBased$medianUnbiasedEstimates, x4$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_type(names(x4), "character")
	    df <- as.data.frame(x4)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x4)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	plotData2 <- testGetAnalysisResultsPlotData(x4, piTreatmentRange = seq(0.45, 0.75, 0.05))

	## Comparison of the results of list object 'plotData2' with expected results
	expect_equal(plotData2$xValues, c(0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75), tolerance = 1e-07, label = paste0("c(", paste0(plotData2$xValues, collapse = ", "), ")"))
	expect_equal(plotData2$condPowerValues, c(0.99501417, 0.9843063, 0.96005739, 0.91353722, 0.83535366, 0.71802165, 0.55995335), tolerance = 1e-07, label = paste0("c(", paste0(plotData2$condPowerValues, collapse = ", "), ")"))
	expect_equal(plotData2$likelihoodValues, c(1, 0.9039239, 0.66761715, 0.40289032, 0.19865977, 0.080038099, 0.026347981), tolerance = 1e-07, label = paste0("c(", paste0(plotData2$likelihoodValues, collapse = ", "), ")"))
	expect_equal(plotData2$main, "Conditional Power with Likelihood", label = paste0("c(", paste0(plotData2$main, collapse = ", "), ")"))
	expect_equal(plotData2$xlab, "pi1", label = paste0("c(", paste0(plotData2$xlab, collapse = ", "), ")"))
	expect_equal(plotData2$ylab, "Conditional power / Likelihood", label = paste0("c(", paste0(plotData2$ylab, collapse = ", "), ")"))
	expect_equal(plotData2$sub, "Stage = 2, # of remaining subjects = 18", label = paste0("c(", paste0(plotData2$sub, collapse = ", "), ")"))

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsGroupSequential}
	# @refFS[Formula]{fs:testStatisticOneRateApproximation}
	# @refFS[Formula]{fs:pValuesOneRateAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticGroupSequential}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerOneRateEffect}
	x5 <- getAnalysisResults(
	    design = design1, dataInput = dataExample1,
	    stage = 3, thetaH0 = 0.25, normalApproximation = FALSE, directionUpper = TRUE
	)

	## Comparison of the results of AnalysisResultsGroupSequential object 'x5' with expected results
	expect_equal(x5$pi1, 0.35, tolerance = 1e-07, label = paste0("c(", paste0(x5$pi1, collapse = ", "), ")"))
	expect_equal(x5$testActions, c("continue", "continue", "continue", NA_character_), label = paste0("c(", paste0(x5$testActions, collapse = ", "), ")"))
	expect_equal(x5$conditionalRejectionProbabilities, c(0.033369687, 0.13517192, 0.020135528, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x5$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x5$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x5$conditionalPower, collapse = ", "), ")"))
	expect_equal(x5$repeatedConfidenceIntervalLowerBounds, c(0.035340833, 0.15564775, 0.18966473, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x5$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x5$repeatedConfidenceIntervalUpperBounds, c(0.88809209, 0.77284164, 0.53925561, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x5$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x5$repeatedPValues, c(0.49999905, 0.49999905, 0.20027888, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x5$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x5$finalStage, NA_integer_, label = paste0("c(", paste0(x5$finalStage, collapse = ", "), ")"))
	expect_equal(x5$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x5$finalPValues, collapse = ", "), ")"))
	expect_equal(x5$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x5$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x5$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x5$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x5$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x5$medianUnbiasedEstimates, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x5), NA)))
	    expect_output(print(x5)$show())
	    invisible(capture.output(expect_error(summary(x5), NA)))
	    expect_output(summary(x5)$show())
	    x5CodeBased <- eval(parse(text = getObjectRCode(x5, stringWrapParagraphWidth = NULL)))
	    expect_equal(x5CodeBased$pi1, x5$pi1, tolerance = 1e-07)
	    expect_equal(x5CodeBased$testActions, x5$testActions, tolerance = 1e-07)
	    expect_equal(x5CodeBased$conditionalRejectionProbabilities, x5$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x5CodeBased$conditionalPower, x5$conditionalPower, tolerance = 1e-07)
	    expect_equal(x5CodeBased$repeatedConfidenceIntervalLowerBounds, x5$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x5CodeBased$repeatedConfidenceIntervalUpperBounds, x5$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x5CodeBased$repeatedPValues, x5$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x5CodeBased$finalStage, x5$finalStage, tolerance = 1e-07)
	    expect_equal(x5CodeBased$finalPValues, x5$finalPValues, tolerance = 1e-07)
	    expect_equal(x5CodeBased$finalConfidenceIntervalLowerBounds, x5$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x5CodeBased$finalConfidenceIntervalUpperBounds, x5$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x5CodeBased$medianUnbiasedEstimates, x5$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_type(names(x5), "character")
	    df <- as.data.frame(x5)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x5)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsGroupSequential}
	# @refFS[Formula]{fs:testStatisticOneRateApproximation}
	# @refFS[Formula]{fs:pValuesOneRateApproximationAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticGroupSequential}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerOneRateEffect}
	x6 <- getAnalysisResults(
	    design = design1, dataInput = dataExample1,
	    stage = 3, thetaH0 = 0.25, normalApproximation = TRUE, directionUpper = TRUE
	)

	## Comparison of the results of AnalysisResultsGroupSequential object 'x6' with expected results
	expect_equal(x6$pi1, 0.35, tolerance = 1e-07, label = paste0("c(", paste0(x6$pi1, collapse = ", "), ")"))
	expect_equal(x6$testActions, c("continue", "continue", "continue", NA_character_), label = paste0("c(", paste0(x6$testActions, collapse = ", "), ")"))
	expect_equal(x6$conditionalRejectionProbabilities, c(0.049321562, 0.20984263, 0.048813267, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x6$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x6$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x6$conditionalPower, collapse = ", "), ")"))
	expect_equal(x6$repeatedConfidenceIntervalLowerBounds, c(0.08898791, 0.19243551, 0.206358, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x6$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x6$repeatedConfidenceIntervalUpperBounds, c(0.81981977, 0.73748168, 0.52720845, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x6$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x6$repeatedPValues, c(0.49999905, 0.27035282, 0.14086509, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x6$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x6$finalStage, NA_integer_, label = paste0("c(", paste0(x6$finalStage, collapse = ", "), ")"))
	expect_equal(x6$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x6$finalPValues, collapse = ", "), ")"))
	expect_equal(x6$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x6$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x6$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x6$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x6$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x6$medianUnbiasedEstimates, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x6), NA)))
	    expect_output(print(x6)$show())
	    invisible(capture.output(expect_error(summary(x6), NA)))
	    expect_output(summary(x6)$show())
	    x6CodeBased <- eval(parse(text = getObjectRCode(x6, stringWrapParagraphWidth = NULL)))
	    expect_equal(x6CodeBased$pi1, x6$pi1, tolerance = 1e-07)
	    expect_equal(x6CodeBased$testActions, x6$testActions, tolerance = 1e-07)
	    expect_equal(x6CodeBased$conditionalRejectionProbabilities, x6$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x6CodeBased$conditionalPower, x6$conditionalPower, tolerance = 1e-07)
	    expect_equal(x6CodeBased$repeatedConfidenceIntervalLowerBounds, x6$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x6CodeBased$repeatedConfidenceIntervalUpperBounds, x6$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x6CodeBased$repeatedPValues, x6$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x6CodeBased$finalStage, x6$finalStage, tolerance = 1e-07)
	    expect_equal(x6CodeBased$finalPValues, x6$finalPValues, tolerance = 1e-07)
	    expect_equal(x6CodeBased$finalConfidenceIntervalLowerBounds, x6$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x6CodeBased$finalConfidenceIntervalUpperBounds, x6$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x6CodeBased$medianUnbiasedEstimates, x6$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_type(names(x6), "character")
	    df <- as.data.frame(x6)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x6)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsGroupSequential}
	# @refFS[Formula]{fs:testStatisticOneRateApproximation}
	# @refFS[Formula]{fs:pValuesOneRateAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticGroupSequential}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerOneRateEffect}
	x7 <- getAnalysisResults(
	    design = design1, dataInput = dataExample1,
	    stage = 2, thetaH0 = 0.25, nPlanned = c(12, 6), pi1 = 0.5,
	    normalApproximation = FALSE, directionUpper = TRUE
	)

	## Comparison of the results of AnalysisResultsGroupSequential object 'x7' with expected results
	expect_equal(x7$testActions, c("continue", "continue", NA_character_, NA_character_), label = paste0("c(", paste0(x7$testActions, collapse = ", "), ")"))
	expect_equal(x7$conditionalRejectionProbabilities, c(0.033369687, 0.13517192, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x7$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x7$conditionalPower, c(NA_real_, NA_real_, 0.58576815, 0.82581584), tolerance = 1e-07, label = paste0("c(", paste0(x7$conditionalPower, collapse = ", "), ")"))
	expect_equal(x7$repeatedConfidenceIntervalLowerBounds, c(0.035340833, 0.15564775, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x7$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x7$repeatedConfidenceIntervalUpperBounds, c(0.88809209, 0.77284164, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x7$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x7$repeatedPValues, c(0.49999905, 0.49999905, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x7$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x7$finalStage, NA_integer_, label = paste0("c(", paste0(x7$finalStage, collapse = ", "), ")"))
	expect_equal(x7$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x7$finalPValues, collapse = ", "), ")"))
	expect_equal(x7$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x7$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x7$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x7$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x7$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x7$medianUnbiasedEstimates, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x7), NA)))
	    expect_output(print(x7)$show())
	    invisible(capture.output(expect_error(summary(x7), NA)))
	    expect_output(summary(x7)$show())
	    x7CodeBased <- eval(parse(text = getObjectRCode(x7, stringWrapParagraphWidth = NULL)))
	    expect_equal(x7CodeBased$testActions, x7$testActions, tolerance = 1e-07)
	    expect_equal(x7CodeBased$conditionalRejectionProbabilities, x7$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x7CodeBased$conditionalPower, x7$conditionalPower, tolerance = 1e-07)
	    expect_equal(x7CodeBased$repeatedConfidenceIntervalLowerBounds, x7$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x7CodeBased$repeatedConfidenceIntervalUpperBounds, x7$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x7CodeBased$repeatedPValues, x7$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x7CodeBased$finalStage, x7$finalStage, tolerance = 1e-07)
	    expect_equal(x7CodeBased$finalPValues, x7$finalPValues, tolerance = 1e-07)
	    expect_equal(x7CodeBased$finalConfidenceIntervalLowerBounds, x7$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x7CodeBased$finalConfidenceIntervalUpperBounds, x7$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x7CodeBased$medianUnbiasedEstimates, x7$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_type(names(x7), "character")
	    df <- as.data.frame(x7)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x7)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	plotData3 <- testGetAnalysisResultsPlotData(x7, piTreatmentRange = seq(0.25, 0.55, 0.05))

	## Comparison of the results of list object 'plotData3' with expected results
	expect_equal(plotData3$xValues, c(0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55), tolerance = 1e-07, label = paste0("c(", paste0(plotData3$xValues, collapse = ", "), ")"))
	expect_equal(plotData3$condPowerValues, c(0.099723848, 0.21903134, 0.37478113, 0.54310492, 0.6994843, 0.82581584, 0.91388884), tolerance = 1e-07, label = paste0("c(", paste0(plotData3$condPowerValues, collapse = ", "), ")"))
	expect_equal(plotData3$likelihoodValues, c(0.19865977, 0.40289032, 0.66761715, 0.9039239, 1, 0.9039239, 0.66761715), tolerance = 1e-07, label = paste0("c(", paste0(plotData3$likelihoodValues, collapse = ", "), ")"))
	expect_equal(plotData3$main, "Conditional Power with Likelihood", label = paste0("c(", paste0(plotData3$main, collapse = ", "), ")"))
	expect_equal(plotData3$xlab, "pi1", label = paste0("c(", paste0(plotData3$xlab, collapse = ", "), ")"))
	expect_equal(plotData3$ylab, "Conditional power / Likelihood", label = paste0("c(", paste0(plotData3$ylab, collapse = ", "), ")"))
	expect_equal(plotData3$sub, "Stage = 2, # of remaining subjects = 18", label = paste0("c(", paste0(plotData3$sub, collapse = ", "), ")"))

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsGroupSequential}
	# @refFS[Formula]{fs:testStatisticOneRateApproximation}
	# @refFS[Formula]{fs:pValuesOneRateApproximationAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticGroupSequential}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerOneRateEffect}
	x8 <- getAnalysisResults(
	    design = design1, dataInput = dataExample1,
	    stage = 2, thetaH0 = 0.25, nPlanned = c(12, 6), pi1 = 0.5,
	    normalApproximation = TRUE, directionUpper = TRUE
	)

	## Comparison of the results of AnalysisResultsGroupSequential object 'x8' with expected results
	expect_equal(x8$testActions, c("continue", "continue", NA_character_, NA_character_), label = paste0("c(", paste0(x8$testActions, collapse = ", "), ")"))
	expect_equal(x8$conditionalRejectionProbabilities, c(0.049321562, 0.20984263, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x8$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x8$conditionalPower, c(NA_real_, NA_real_, 0.76152324, 0.91259792), tolerance = 1e-07, label = paste0("c(", paste0(x8$conditionalPower, collapse = ", "), ")"))
	expect_equal(x8$repeatedConfidenceIntervalLowerBounds, c(0.08898791, 0.19243551, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x8$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x8$repeatedConfidenceIntervalUpperBounds, c(0.81981977, 0.73748168, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x8$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x8$repeatedPValues, c(0.49999905, 0.27035282, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x8$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x8$finalStage, NA_integer_, label = paste0("c(", paste0(x8$finalStage, collapse = ", "), ")"))
	expect_equal(x8$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x8$finalPValues, collapse = ", "), ")"))
	expect_equal(x8$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x8$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x8$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x8$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x8$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x8$medianUnbiasedEstimates, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x8), NA)))
	    expect_output(print(x8)$show())
	    invisible(capture.output(expect_error(summary(x8), NA)))
	    expect_output(summary(x8)$show())
	    x8CodeBased <- eval(parse(text = getObjectRCode(x8, stringWrapParagraphWidth = NULL)))
	    expect_equal(x8CodeBased$testActions, x8$testActions, tolerance = 1e-07)
	    expect_equal(x8CodeBased$conditionalRejectionProbabilities, x8$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x8CodeBased$conditionalPower, x8$conditionalPower, tolerance = 1e-07)
	    expect_equal(x8CodeBased$repeatedConfidenceIntervalLowerBounds, x8$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x8CodeBased$repeatedConfidenceIntervalUpperBounds, x8$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x8CodeBased$repeatedPValues, x8$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x8CodeBased$finalStage, x8$finalStage, tolerance = 1e-07)
	    expect_equal(x8CodeBased$finalPValues, x8$finalPValues, tolerance = 1e-07)
	    expect_equal(x8CodeBased$finalConfidenceIntervalLowerBounds, x8$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x8CodeBased$finalConfidenceIntervalUpperBounds, x8$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x8CodeBased$medianUnbiasedEstimates, x8$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_type(names(x8), "character")
	    df <- as.data.frame(x8)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x8)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	plotData4 <- testGetAnalysisResultsPlotData(x8, piTreatmentRange = seq(0.25, 0.55, 0.05))

	## Comparison of the results of list object 'plotData4' with expected results
	expect_equal(plotData4$xValues, c(0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55), tolerance = 1e-07, label = paste0("c(", paste0(plotData4$xValues, collapse = ", "), ")"))
	expect_equal(plotData4$condPowerValues, c(0.20983879, 0.3743042, 0.5481143, 0.70471917, 0.82789376, 0.91259792, 0.96272982), tolerance = 1e-07, label = paste0("c(", paste0(plotData4$condPowerValues, collapse = ", "), ")"))
	expect_equal(plotData4$likelihoodValues, c(0.19865977, 0.40289032, 0.66761715, 0.9039239, 1, 0.9039239, 0.66761715), tolerance = 1e-07, label = paste0("c(", paste0(plotData4$likelihoodValues, collapse = ", "), ")"))
	expect_equal(plotData4$main, "Conditional Power with Likelihood", label = paste0("c(", paste0(plotData4$main, collapse = ", "), ")"))
	expect_equal(plotData4$xlab, "pi1", label = paste0("c(", paste0(plotData4$xlab, collapse = ", "), ")"))
	expect_equal(plotData4$ylab, "Conditional power / Likelihood", label = paste0("c(", paste0(plotData4$ylab, collapse = ", "), ")"))
	expect_equal(plotData4$sub, "Stage = 2, # of remaining subjects = 18", label = paste0("c(", paste0(plotData4$sub, collapse = ", "), ")"))

})

test_that("'getAnalysisResults' for a  four-stage inverse sequential design and one treatment", {

	.skipTestIfDisabled()

	design2 <- getDesignInverseNormal(
	    kMax = 4, alpha = 0.025, informationRates = c(0.2, 0.4, 0.8, 1),
	    futilityBounds = c(-0.5, 0, 0.5), typeOfDesign = "asKD", gammaA = 2.8
	)

	dataExample2 <- getDataset(
	    n = c(8, 10, 9, 11), # cumsum, overall n = (8, 18, 27, 38)
	    events = c(4, 5, 5, 6) # cumsum, overall events = (4, 9, 14, 20)
	)
	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsGroupSequential}
	# @refFS[Formula]{fs:testStatisticOneRateApproximation}
	# @refFS[Formula]{fs:pValuesOneRateAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticNormalCombinationTest}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}

	x1 <- getAnalysisResults(
	    design = design2, dataInput = dataExample2,
	    stage = 3, thetaH0 = 0.75, normalApproximation = FALSE, directionUpper = FALSE
	)

	## Comparison of the results of AnalysisResultsInverseNormal object 'x1' with expected results
	expect_equal(x1$pi1, 0.51851852, tolerance = 1e-07, label = paste0("c(", paste0(x1$pi1, collapse = ", "), ")"))
	expect_equal(x1$testActions, c("continue", "continue", "continue", NA_character_), label = paste0("c(", paste0(x1$testActions, collapse = ", "), ")"))
	expect_equal(x1$conditionalRejectionProbabilities, c(0.055828725, 0.15918316, 0.28098687, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x1$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$conditionalPower, collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(0.046266926, 0.16132369, 0.26858957, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(0.95373307, 0.83867631, 0.76870127, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x1$repeatedPValues, c(0.49999905, 0.43799317, 0.045574143, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x1$finalStage, NA_integer_, label = paste0("c(", paste0(x1$finalStage, collapse = ", "), ")"))
	expect_equal(x1$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$finalPValues, collapse = ", "), ")"))
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$medianUnbiasedEstimates, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x1), NA)))
	    expect_output(print(x1)$show())
	    invisible(capture.output(expect_error(summary(x1), NA)))
	    expect_output(summary(x1)$show())
	    x1CodeBased <- eval(parse(text = getObjectRCode(x1, stringWrapParagraphWidth = NULL)))
	    expect_equal(x1CodeBased$pi1, x1$pi1, tolerance = 1e-07)
	    expect_equal(x1CodeBased$testActions, x1$testActions, tolerance = 1e-07)
	    expect_equal(x1CodeBased$conditionalRejectionProbabilities, x1$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x1CodeBased$conditionalPower, x1$conditionalPower, tolerance = 1e-07)
	    expect_equal(x1CodeBased$repeatedConfidenceIntervalLowerBounds, x1$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$repeatedConfidenceIntervalUpperBounds, x1$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$repeatedPValues, x1$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x1CodeBased$finalStage, x1$finalStage, tolerance = 1e-07)
	    expect_equal(x1CodeBased$finalPValues, x1$finalPValues, tolerance = 1e-07)
	    expect_equal(x1CodeBased$finalConfidenceIntervalLowerBounds, x1$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$finalConfidenceIntervalUpperBounds, x1$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$medianUnbiasedEstimates, x1$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_type(names(x1), "character")
	    df <- as.data.frame(x1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsGroupSequential}
	# @refFS[Formula]{fs:testStatisticOneRateApproximation}
	# @refFS[Formula]{fs:pValuesOneRateApproximationAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticNormalCombinationTest}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:orderingPValueUpper}
	# @refFS[Formula]{fs:finalCIOneRate}
	# @refFS[Formula]{fs:medianUnbiasedEstimate}
	x2 <- getAnalysisResults(
	    design = design2, dataInput = dataExample2,
	    stage = 3, thetaH0 = 0.75, normalApproximation = TRUE, directionUpper = FALSE
	)

	## Comparison of the results of AnalysisResultsInverseNormal object 'x2' with expected results
	expect_equal(x2$pi1, 0.51851852, tolerance = 1e-07, label = paste0("c(", paste0(x2$pi1, collapse = ", "), ")"))
	expect_equal(x2$testActions, c("continue", "continue", "reject and stop", NA_character_), label = paste0("c(", paste0(x2$testActions, collapse = ", "), ")"))
	expect_equal(x2$conditionalRejectionProbabilities, c(0.08807963, 0.32350578, 0.78413539, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x2$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$conditionalPower, collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(0.11314487, 0.21610036, 0.31861038, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(0.88685513, 0.78389964, 0.72001945, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x2$repeatedPValues, c(0.49999905, 0.1020964, 0.0075111702, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x2$finalStage, 3, label = paste0("c(", paste0(x2$finalStage, collapse = ", "), ")"))
	expect_equal(x2$finalPValues, c(NA_real_, NA_real_, 0.0050707339, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$finalPValues, collapse = ", "), ")"))
	expect_equal(x2$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, 0.3041323, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x2$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, 0.68870859, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x2$medianUnbiasedEstimates, c(NA_real_, NA_real_, 0.49547717, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$medianUnbiasedEstimates, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x2), NA)))
	    expect_output(print(x2)$show())
	    invisible(capture.output(expect_error(summary(x2), NA)))
	    expect_output(summary(x2)$show())
	    x2CodeBased <- eval(parse(text = getObjectRCode(x2, stringWrapParagraphWidth = NULL)))
	    expect_equal(x2CodeBased$pi1, x2$pi1, tolerance = 1e-07)
	    expect_equal(x2CodeBased$testActions, x2$testActions, tolerance = 1e-07)
	    expect_equal(x2CodeBased$conditionalRejectionProbabilities, x2$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x2CodeBased$conditionalPower, x2$conditionalPower, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedConfidenceIntervalLowerBounds, x2$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedConfidenceIntervalUpperBounds, x2$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedPValues, x2$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x2CodeBased$finalStage, x2$finalStage, tolerance = 1e-07)
	    expect_equal(x2CodeBased$finalPValues, x2$finalPValues, tolerance = 1e-07)
	    expect_equal(x2CodeBased$finalConfidenceIntervalLowerBounds, x2$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$finalConfidenceIntervalUpperBounds, x2$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$medianUnbiasedEstimates, x2$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_type(names(x2), "character")
	    df <- as.data.frame(x2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsGroupSequential}
	# @refFS[Formula]{fs:testStatisticOneRateApproximation}
	# @refFS[Formula]{fs:pValuesOneRateAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticNormalCombinationTest}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	x3 <- getAnalysisResults(
	    design = design2, dataInput = dataExample2,
	    stage = 2, thetaH0 = 0.75, nPlanned = c(12, 6), pi1 = 0.5,
	    normalApproximation = FALSE, directionUpper = FALSE
	)

	## Comparison of the results of AnalysisResultsInverseNormal object 'x3' with expected results
	expect_equal(x3$testActions, c("continue", "continue", NA_character_, NA_character_), label = paste0("c(", paste0(x3$testActions, collapse = ", "), ")"))
	expect_equal(x3$conditionalRejectionProbabilities, c(0.055828725, 0.15918316, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x3$conditionalPower, c(NA_real_, NA_real_, 0.69921202, 0.88465983), tolerance = 1e-07, label = paste0("c(", paste0(x3$conditionalPower, collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds, c(0.046266926, 0.16132369, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds, c(0.95373307, 0.83867631, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x3$repeatedPValues, c(0.49999905, 0.43799317, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x3$finalStage, NA_integer_, label = paste0("c(", paste0(x3$finalStage, collapse = ", "), ")"))
	expect_equal(x3$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$finalPValues, collapse = ", "), ")"))
	expect_equal(x3$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x3$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x3$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$medianUnbiasedEstimates, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x3), NA)))
	    expect_output(print(x3)$show())
	    invisible(capture.output(expect_error(summary(x3), NA)))
	    expect_output(summary(x3)$show())
	    x3CodeBased <- eval(parse(text = getObjectRCode(x3, stringWrapParagraphWidth = NULL)))
	    expect_equal(x3CodeBased$testActions, x3$testActions, tolerance = 1e-07)
	    expect_equal(x3CodeBased$conditionalRejectionProbabilities, x3$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x3CodeBased$conditionalPower, x3$conditionalPower, tolerance = 1e-07)
	    expect_equal(x3CodeBased$repeatedConfidenceIntervalLowerBounds, x3$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x3CodeBased$repeatedConfidenceIntervalUpperBounds, x3$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x3CodeBased$repeatedPValues, x3$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x3CodeBased$finalStage, x3$finalStage, tolerance = 1e-07)
	    expect_equal(x3CodeBased$finalPValues, x3$finalPValues, tolerance = 1e-07)
	    expect_equal(x3CodeBased$finalConfidenceIntervalLowerBounds, x3$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x3CodeBased$finalConfidenceIntervalUpperBounds, x3$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x3CodeBased$medianUnbiasedEstimates, x3$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_type(names(x3), "character")
	    df <- as.data.frame(x3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	plotData1 <- testGetAnalysisResultsPlotData(x3, piTreatmentRange = seq(0.45, 0.75, 0.05))

	## Comparison of the results of list object 'plotData1' with expected results
	expect_equal(plotData1$xValues, c(0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75), tolerance = 1e-07, label = paste0("c(", paste0(plotData1$xValues, collapse = ", "), ")"))
	expect_equal(plotData1$condPowerValues, c(0.94793138, 0.88465983, 0.78396384, 0.64581102, 0.48045808, 0.30888817, 0.15917802), tolerance = 1e-07, label = paste0("c(", paste0(plotData1$condPowerValues, collapse = ", "), ")"))
	expect_equal(plotData1$likelihoodValues, c(0.91393119, 1, 0.91393119, 0.69767633, 0.44485807, 0.23692776, 0.10539922), tolerance = 1e-07, label = paste0("c(", paste0(plotData1$likelihoodValues, collapse = ", "), ")"))
	expect_equal(plotData1$main, "Conditional Power with Likelihood", label = paste0("c(", paste0(plotData1$main, collapse = ", "), ")"))
	expect_equal(plotData1$xlab, "pi1", label = paste0("c(", paste0(plotData1$xlab, collapse = ", "), ")"))
	expect_equal(plotData1$ylab, "Conditional power / Likelihood", label = paste0("c(", paste0(plotData1$ylab, collapse = ", "), ")"))
	expect_equal(plotData1$sub, "Stage = 2, # of remaining subjects = 18", label = paste0("c(", paste0(plotData1$sub, collapse = ", "), ")"))

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsGroupSequential}
	# @refFS[Formula]{fs:testStatisticOneRateApproximation}
	# @refFS[Formula]{fs:pValuesOneRateApproximationAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticNormalCombinationTest}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	x4 <- getAnalysisResults(
	    design = design2, dataInput = dataExample2,
	    stage = 2, thetaH0 = 0.75, nPlanned = c(12, 6), pi1 = 0.5,
	    normalApproximation = TRUE, directionUpper = FALSE
	)

	## Comparison of the results of AnalysisResultsInverseNormal object 'x4' with expected results
	expect_equal(x4$testActions, c("continue", "continue", NA_character_, NA_character_), label = paste0("c(", paste0(x4$testActions, collapse = ", "), ")"))
	expect_equal(x4$conditionalRejectionProbabilities, c(0.08807963, 0.32350578, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x4$conditionalPower, c(NA_real_, NA_real_, 0.85385983, 0.95015898), tolerance = 1e-07, label = paste0("c(", paste0(x4$conditionalPower, collapse = ", "), ")"))
	expect_equal(x4$repeatedConfidenceIntervalLowerBounds, c(0.11314487, 0.21610036, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x4$repeatedConfidenceIntervalUpperBounds, c(0.88685513, 0.78389964, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x4$repeatedPValues, c(0.49999905, 0.1020964, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x4$finalStage, NA_integer_, label = paste0("c(", paste0(x4$finalStage, collapse = ", "), ")"))
	expect_equal(x4$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x4$finalPValues, collapse = ", "), ")"))
	expect_equal(x4$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x4$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x4$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x4$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x4$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x4$medianUnbiasedEstimates, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x4), NA)))
	    expect_output(print(x4)$show())
	    invisible(capture.output(expect_error(summary(x4), NA)))
	    expect_output(summary(x4)$show())
	    x4CodeBased <- eval(parse(text = getObjectRCode(x4, stringWrapParagraphWidth = NULL)))
	    expect_equal(x4CodeBased$testActions, x4$testActions, tolerance = 1e-07)
	    expect_equal(x4CodeBased$conditionalRejectionProbabilities, x4$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x4CodeBased$conditionalPower, x4$conditionalPower, tolerance = 1e-07)
	    expect_equal(x4CodeBased$repeatedConfidenceIntervalLowerBounds, x4$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x4CodeBased$repeatedConfidenceIntervalUpperBounds, x4$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x4CodeBased$repeatedPValues, x4$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x4CodeBased$finalStage, x4$finalStage, tolerance = 1e-07)
	    expect_equal(x4CodeBased$finalPValues, x4$finalPValues, tolerance = 1e-07)
	    expect_equal(x4CodeBased$finalConfidenceIntervalLowerBounds, x4$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x4CodeBased$finalConfidenceIntervalUpperBounds, x4$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x4CodeBased$medianUnbiasedEstimates, x4$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_type(names(x4), "character")
	    df <- as.data.frame(x4)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x4)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	plotData2 <- testGetAnalysisResultsPlotData(x4, piTreatmentRange = seq(0.45, 0.75, 0.05))

	## Comparison of the results of list object 'plotData2' with expected results
	expect_equal(plotData2$xValues, c(0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75), tolerance = 1e-07, label = paste0("c(", paste0(plotData2$xValues, collapse = ", "), ")"))
	expect_equal(plotData2$condPowerValues, c(0.98088099, 0.95015898, 0.89232289, 0.79901831, 0.66708346, 0.50248974, 0.32350375), tolerance = 1e-07, label = paste0("c(", paste0(plotData2$condPowerValues, collapse = ", "), ")"))
	expect_equal(plotData2$likelihoodValues, c(0.91393119, 1, 0.91393119, 0.69767633, 0.44485807, 0.23692776, 0.10539922), tolerance = 1e-07, label = paste0("c(", paste0(plotData2$likelihoodValues, collapse = ", "), ")"))
	expect_equal(plotData2$main, "Conditional Power with Likelihood", label = paste0("c(", paste0(plotData2$main, collapse = ", "), ")"))
	expect_equal(plotData2$xlab, "pi1", label = paste0("c(", paste0(plotData2$xlab, collapse = ", "), ")"))
	expect_equal(plotData2$ylab, "Conditional power / Likelihood", label = paste0("c(", paste0(plotData2$ylab, collapse = ", "), ")"))
	expect_equal(plotData2$sub, "Stage = 2, # of remaining subjects = 18", label = paste0("c(", paste0(plotData2$sub, collapse = ", "), ")"))

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsGroupSequential}
	# @refFS[Formula]{fs:testStatisticOneRateApproximation}
	# @refFS[Formula]{fs:pValuesOneRateAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticNormalCombinationTest}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	x5 <- getAnalysisResults(
	    design = design2, dataInput = dataExample2,
	    stage = 3, thetaH0 = 0.25, normalApproximation = FALSE, directionUpper = TRUE
	)

	## Comparison of the results of AnalysisResultsInverseNormal object 'x5' with expected results
	expect_equal(x5$pi1, 0.51851852, tolerance = 1e-07, label = paste0("c(", paste0(x5$pi1, collapse = ", "), ")"))
	expect_equal(x5$testActions, c("continue", "continue", "reject and stop", NA_character_), label = paste0("c(", paste0(x5$testActions, collapse = ", "), ")"))
	expect_equal(x5$conditionalRejectionProbabilities, c(0.055828725, 0.15918316, 0.65085211, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x5$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x5$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x5$conditionalPower, collapse = ", "), ")"))
	expect_equal(x5$repeatedConfidenceIntervalLowerBounds, c(0.046266926, 0.16132369, 0.26858957, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x5$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x5$repeatedConfidenceIntervalUpperBounds, c(0.95373307, 0.83867631, 0.76870127, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x5$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x5$repeatedPValues, c(0.49999905, 0.43799317, 0.013282796, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x5$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x5$finalStage, 3, label = paste0("c(", paste0(x5$finalStage, collapse = ", "), ")"))
	expect_equal(x5$finalPValues, c(NA_real_, NA_real_, 0.007752129, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x5$finalPValues, collapse = ", "), ")"))
	expect_equal(x5$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, 0.29554194, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x5$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x5$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, 0.67875285, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x5$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x5$medianUnbiasedEstimates, c(NA_real_, NA_real_, 0.48769629, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x5$medianUnbiasedEstimates, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x5), NA)))
	    expect_output(print(x5)$show())
	    invisible(capture.output(expect_error(summary(x5), NA)))
	    expect_output(summary(x5)$show())
	    x5CodeBased <- eval(parse(text = getObjectRCode(x5, stringWrapParagraphWidth = NULL)))
	    expect_equal(x5CodeBased$pi1, x5$pi1, tolerance = 1e-07)
	    expect_equal(x5CodeBased$testActions, x5$testActions, tolerance = 1e-07)
	    expect_equal(x5CodeBased$conditionalRejectionProbabilities, x5$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x5CodeBased$conditionalPower, x5$conditionalPower, tolerance = 1e-07)
	    expect_equal(x5CodeBased$repeatedConfidenceIntervalLowerBounds, x5$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x5CodeBased$repeatedConfidenceIntervalUpperBounds, x5$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x5CodeBased$repeatedPValues, x5$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x5CodeBased$finalStage, x5$finalStage, tolerance = 1e-07)
	    expect_equal(x5CodeBased$finalPValues, x5$finalPValues, tolerance = 1e-07)
	    expect_equal(x5CodeBased$finalConfidenceIntervalLowerBounds, x5$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x5CodeBased$finalConfidenceIntervalUpperBounds, x5$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x5CodeBased$medianUnbiasedEstimates, x5$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_type(names(x5), "character")
	    df <- as.data.frame(x5)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x5)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsGroupSequential}
	# @refFS[Formula]{fs:testStatisticOneRateApproximation}
	# @refFS[Formula]{fs:pValuesOneRateApproximationAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticNormalCombinationTest}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	x6 <- getAnalysisResults(
	    design = design2, dataInput = dataExample2,
	    stage = 3, thetaH0 = 0.25, normalApproximation = TRUE, directionUpper = TRUE
	)

	## Comparison of the results of AnalysisResultsInverseNormal object 'x6' with expected results
	expect_equal(x6$pi1, 0.51851852, tolerance = 1e-07, label = paste0("c(", paste0(x6$pi1, collapse = ", "), ")"))
	expect_equal(x6$testActions, c("continue", "continue", "reject and stop", NA_character_), label = paste0("c(", paste0(x6$testActions, collapse = ", "), ")"))
	expect_equal(x6$conditionalRejectionProbabilities, c(0.08807963, 0.32350578, 0.96959663, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x6$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x6$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x6$conditionalPower, collapse = ", "), ")"))
	expect_equal(x6$repeatedConfidenceIntervalLowerBounds, c(0.11314487, 0.21610036, 0.31861038, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x6$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x6$repeatedConfidenceIntervalUpperBounds, c(0.88685513, 0.78389964, 0.72001945, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x6$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x6$repeatedPValues, c(0.49999905, 0.1020964, 0.0013103922, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x6$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x6$finalStage, 3, label = paste0("c(", paste0(x6$finalStage, collapse = ", "), ")"))
	expect_equal(x6$finalPValues, c(NA_real_, NA_real_, 0.002378519, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x6$finalPValues, collapse = ", "), ")"))
	expect_equal(x6$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, 0.3437363, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x6$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x6$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, 0.73847376, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x6$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x6$medianUnbiasedEstimates, c(NA_real_, NA_real_, 0.54446903, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x6$medianUnbiasedEstimates, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x6), NA)))
	    expect_output(print(x6)$show())
	    invisible(capture.output(expect_error(summary(x6), NA)))
	    expect_output(summary(x6)$show())
	    x6CodeBased <- eval(parse(text = getObjectRCode(x6, stringWrapParagraphWidth = NULL)))
	    expect_equal(x6CodeBased$pi1, x6$pi1, tolerance = 1e-07)
	    expect_equal(x6CodeBased$testActions, x6$testActions, tolerance = 1e-07)
	    expect_equal(x6CodeBased$conditionalRejectionProbabilities, x6$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x6CodeBased$conditionalPower, x6$conditionalPower, tolerance = 1e-07)
	    expect_equal(x6CodeBased$repeatedConfidenceIntervalLowerBounds, x6$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x6CodeBased$repeatedConfidenceIntervalUpperBounds, x6$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x6CodeBased$repeatedPValues, x6$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x6CodeBased$finalStage, x6$finalStage, tolerance = 1e-07)
	    expect_equal(x6CodeBased$finalPValues, x6$finalPValues, tolerance = 1e-07)
	    expect_equal(x6CodeBased$finalConfidenceIntervalLowerBounds, x6$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x6CodeBased$finalConfidenceIntervalUpperBounds, x6$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x6CodeBased$medianUnbiasedEstimates, x6$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_type(names(x6), "character")
	    df <- as.data.frame(x6)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x6)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsGroupSequential}
	# @refFS[Formula]{fs:testStatisticOneRateApproximation}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerOneRateEffect}
	x7 <- getAnalysisResults(
	    design = design2, dataInput = dataExample2,
	    stage = 2, thetaH0 = 0.25, nPlanned = c(12, 6), pi1 = 0.5,
	    normalApproximation = FALSE, directionUpper = TRUE
	)

	## Comparison of the results of AnalysisResultsInverseNormal object 'x7' with expected results
	expect_equal(x7$testActions, c("continue", "continue", NA_character_, NA_character_), label = paste0("c(", paste0(x7$testActions, collapse = ", "), ")"))
	expect_equal(x7$conditionalRejectionProbabilities, c(0.055828725, 0.15918316, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x7$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x7$conditionalPower, c(NA_real_, NA_real_, 0.69921202, 0.88465983), tolerance = 1e-07, label = paste0("c(", paste0(x7$conditionalPower, collapse = ", "), ")"))
	expect_equal(x7$repeatedConfidenceIntervalLowerBounds, c(0.046266926, 0.16132369, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x7$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x7$repeatedConfidenceIntervalUpperBounds, c(0.95373307, 0.83867631, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x7$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x7$repeatedPValues, c(0.49999905, 0.43799317, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x7$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x7$finalStage, NA_integer_, label = paste0("c(", paste0(x7$finalStage, collapse = ", "), ")"))
	expect_equal(x7$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x7$finalPValues, collapse = ", "), ")"))
	expect_equal(x7$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x7$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x7$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x7$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x7$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x7$medianUnbiasedEstimates, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x7), NA)))
	    expect_output(print(x7)$show())
	    invisible(capture.output(expect_error(summary(x7), NA)))
	    expect_output(summary(x7)$show())
	    x7CodeBased <- eval(parse(text = getObjectRCode(x7, stringWrapParagraphWidth = NULL)))
	    expect_equal(x7CodeBased$testActions, x7$testActions, tolerance = 1e-07)
	    expect_equal(x7CodeBased$conditionalRejectionProbabilities, x7$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x7CodeBased$conditionalPower, x7$conditionalPower, tolerance = 1e-07)
	    expect_equal(x7CodeBased$repeatedConfidenceIntervalLowerBounds, x7$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x7CodeBased$repeatedConfidenceIntervalUpperBounds, x7$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x7CodeBased$repeatedPValues, x7$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x7CodeBased$finalStage, x7$finalStage, tolerance = 1e-07)
	    expect_equal(x7CodeBased$finalPValues, x7$finalPValues, tolerance = 1e-07)
	    expect_equal(x7CodeBased$finalConfidenceIntervalLowerBounds, x7$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x7CodeBased$finalConfidenceIntervalUpperBounds, x7$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x7CodeBased$medianUnbiasedEstimates, x7$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_type(names(x7), "character")
	    df <- as.data.frame(x7)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x7)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	plotData3 <- testGetAnalysisResultsPlotData(x7, piTreatmentRange = seq(0.25, 0.55, 0.05))

	## Comparison of the results of list object 'plotData3' with expected results
	expect_equal(plotData3$xValues, c(0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55), tolerance = 1e-07, label = paste0("c(", paste0(plotData3$xValues, collapse = ", "), ")"))
	expect_equal(plotData3$condPowerValues, c(0.15917802, 0.30888817, 0.48045808, 0.64581102, 0.78396384, 0.88465983, 0.94793138), tolerance = 1e-07, label = paste0("c(", paste0(plotData3$condPowerValues, collapse = ", "), ")"))
	expect_equal(plotData3$likelihoodValues, c(0.10539922, 0.23692776, 0.44485807, 0.69767633, 0.91393119, 1, 0.91393119), tolerance = 1e-07, label = paste0("c(", paste0(plotData3$likelihoodValues, collapse = ", "), ")"))
	expect_equal(plotData3$main, "Conditional Power with Likelihood", label = paste0("c(", paste0(plotData3$main, collapse = ", "), ")"))
	expect_equal(plotData3$xlab, "pi1", label = paste0("c(", paste0(plotData3$xlab, collapse = ", "), ")"))
	expect_equal(plotData3$ylab, "Conditional power / Likelihood", label = paste0("c(", paste0(plotData3$ylab, collapse = ", "), ")"))
	expect_equal(plotData3$sub, "Stage = 2, # of remaining subjects = 18", label = paste0("c(", paste0(plotData3$sub, collapse = ", "), ")"))

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsGroupSequential}
	# @refFS[Formula]{fs:testStatisticOneRateApproximation}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerOneRateEffect}
	x8 <- getAnalysisResults(
	    design = design2, dataInput = dataExample2,
	    stage = 2, thetaH0 = 0.25, nPlanned = c(12, 6), pi1 = 0.5,
	    normalApproximation = TRUE, directionUpper = TRUE
	)

	## Comparison of the results of AnalysisResultsInverseNormal object 'x8' with expected results
	expect_equal(x8$testActions, c("continue", "continue", NA_character_, NA_character_), label = paste0("c(", paste0(x8$testActions, collapse = ", "), ")"))
	expect_equal(x8$conditionalRejectionProbabilities, c(0.08807963, 0.32350578, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x8$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x8$conditionalPower, c(NA_real_, NA_real_, 0.85385983, 0.95015898), tolerance = 1e-07, label = paste0("c(", paste0(x8$conditionalPower, collapse = ", "), ")"))
	expect_equal(x8$repeatedConfidenceIntervalLowerBounds, c(0.11314487, 0.21610036, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x8$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x8$repeatedConfidenceIntervalUpperBounds, c(0.88685513, 0.78389964, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x8$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x8$repeatedPValues, c(0.49999905, 0.1020964, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x8$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x8$finalStage, NA_integer_, label = paste0("c(", paste0(x8$finalStage, collapse = ", "), ")"))
	expect_equal(x8$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x8$finalPValues, collapse = ", "), ")"))
	expect_equal(x8$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x8$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x8$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x8$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x8$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x8$medianUnbiasedEstimates, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x8), NA)))
	    expect_output(print(x8)$show())
	    invisible(capture.output(expect_error(summary(x8), NA)))
	    expect_output(summary(x8)$show())
	    x8CodeBased <- eval(parse(text = getObjectRCode(x8, stringWrapParagraphWidth = NULL)))
	    expect_equal(x8CodeBased$testActions, x8$testActions, tolerance = 1e-07)
	    expect_equal(x8CodeBased$conditionalRejectionProbabilities, x8$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x8CodeBased$conditionalPower, x8$conditionalPower, tolerance = 1e-07)
	    expect_equal(x8CodeBased$repeatedConfidenceIntervalLowerBounds, x8$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x8CodeBased$repeatedConfidenceIntervalUpperBounds, x8$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x8CodeBased$repeatedPValues, x8$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x8CodeBased$finalStage, x8$finalStage, tolerance = 1e-07)
	    expect_equal(x8CodeBased$finalPValues, x8$finalPValues, tolerance = 1e-07)
	    expect_equal(x8CodeBased$finalConfidenceIntervalLowerBounds, x8$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x8CodeBased$finalConfidenceIntervalUpperBounds, x8$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x8CodeBased$medianUnbiasedEstimates, x8$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_type(names(x8), "character")
	    df <- as.data.frame(x8)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x8)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	plotData4 <- testGetAnalysisResultsPlotData(x8, piTreatmentRange = seq(0.25, 0.55, 0.05))

	## Comparison of the results of list object 'plotData4' with expected results
	expect_equal(plotData4$xValues, c(0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55), tolerance = 1e-07, label = paste0("c(", paste0(plotData4$xValues, collapse = ", "), ")"))
	expect_equal(plotData4$condPowerValues, c(0.32350375, 0.50248974, 0.66708346, 0.79901831, 0.89232289, 0.95015898, 0.98088099), tolerance = 1e-07, label = paste0("c(", paste0(plotData4$condPowerValues, collapse = ", "), ")"))
	expect_equal(plotData4$likelihoodValues, c(0.10539922, 0.23692776, 0.44485807, 0.69767633, 0.91393119, 1, 0.91393119), tolerance = 1e-07, label = paste0("c(", paste0(plotData4$likelihoodValues, collapse = ", "), ")"))
	expect_equal(plotData4$main, "Conditional Power with Likelihood", label = paste0("c(", paste0(plotData4$main, collapse = ", "), ")"))
	expect_equal(plotData4$xlab, "pi1", label = paste0("c(", paste0(plotData4$xlab, collapse = ", "), ")"))
	expect_equal(plotData4$ylab, "Conditional power / Likelihood", label = paste0("c(", paste0(plotData4$ylab, collapse = ", "), ")"))
	expect_equal(plotData4$sub, "Stage = 2, # of remaining subjects = 18", label = paste0("c(", paste0(plotData4$sub, collapse = ", "), ")"))

})

test_that("'getAnalysisResults' for a  four-stage Fisher design and one treatment", {

	.skipTestIfDisabled()

	design3 <- getDesignFisher(kMax = 4, alpha = 0.025, informationRates = c(0.2, 0.4, 0.8, 1))

	dataExample3 <- getDataset(
	    n = c(8, 10, 9, 11), # cumsum, overall n = (8, 18, 27, 38)
	    events = c(4, 5, 5, 6) # cumsum, overall events = (4, 9, 14, 20)
	)

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsFisher}
	# @refFS[Formula]{fs:testStatisticOneRateApproximation}
	# @refFS[Formula]{fs:pValuesOneRateAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticFisherCombinationTest}
	# @refFS[Formula]{fs:definitionRCIFisherCombination}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	x1 <- getAnalysisResults(
	    design = design3, dataInput = dataExample3,
	    stage = 3, thetaH0 = 0.75, normalApproximation = FALSE,
	    directionUpper = FALSE, iterations = 1000, seed = 123
	)

	## Comparison of the results of AnalysisResultsFisher object 'x1' with expected results
	expect_equal(x1$pi1, 0.51851852, tolerance = 1e-07, label = paste0("c(", paste0(x1$pi1, collapse = ", "), ")"))
	expect_equal(x1$testActions, c("continue", "continue", "continue", NA_character_), label = paste0("c(", paste0(x1$testActions, collapse = ", "), ")"))
	expect_equal(x1$conditionalRejectionProbabilities, c(0.027980027, 0.040164764, 0.018233808, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x1$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$conditionalPower, collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(0.12025548, 0.19023888, 0.23521677, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(0.87974452, 0.80976112, 0.79971589, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x1$repeatedPValues, c(0.23393398, 0.11483365, 0.11050779, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x1$finalStage, NA_integer_, label = paste0("c(", paste0(x1$finalStage, collapse = ", "), ")"))
	expect_equal(x1$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$finalPValues, collapse = ", "), ")"))
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$medianUnbiasedEstimates, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x1), NA)))
	    expect_output(print(x1)$show())
	    invisible(capture.output(expect_error(summary(x1), NA)))
	    expect_output(summary(x1)$show())
	    x1CodeBased <- eval(parse(text = getObjectRCode(x1, stringWrapParagraphWidth = NULL)))
	    expect_equal(x1CodeBased$pi1, x1$pi1, tolerance = 1e-07)
	    expect_equal(x1CodeBased$testActions, x1$testActions, tolerance = 1e-07)
	    expect_equal(x1CodeBased$conditionalRejectionProbabilities, x1$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x1CodeBased$conditionalPower, x1$conditionalPower, tolerance = 1e-07)
	    expect_equal(x1CodeBased$repeatedConfidenceIntervalLowerBounds, x1$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$repeatedConfidenceIntervalUpperBounds, x1$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$repeatedPValues, x1$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x1CodeBased$finalStage, x1$finalStage, tolerance = 1e-07)
	    expect_equal(x1CodeBased$finalPValues, x1$finalPValues, tolerance = 1e-07)
	    expect_equal(x1CodeBased$finalConfidenceIntervalLowerBounds, x1$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$finalConfidenceIntervalUpperBounds, x1$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$medianUnbiasedEstimates, x1$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_type(names(x1), "character")
	    df <- as.data.frame(x1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsFisher}
	# @refFS[Formula]{fs:testStatisticOneRateApproximation}
	# @refFS[Formula]{fs:pValuesOneRateApproximationAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticFisherCombinationTest}
	# @refFS[Formula]{fs:definitionRCIFisherCombination}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	x2 <- getAnalysisResults(
	    design = design3, dataInput = dataExample3,
	    stage = 3, thetaH0 = 0.75, normalApproximation = TRUE,
	    directionUpper = FALSE, iterations = 1000, seed = 123
	)

	## Comparison of the results of AnalysisResultsFisher object 'x2' with expected results
	expect_equal(x2$pi1, 0.51851852, tolerance = 1e-07, label = paste0("c(", paste0(x2$pi1, collapse = ", "), ")"))
	expect_equal(x2$testActions, c("continue", "continue", "reject and stop", NA_character_), label = paste0("c(", paste0(x2$testActions, collapse = ", "), ")"))
	expect_equal(x2$conditionalRejectionProbabilities, c(0.051264101, 0.1206033, 1, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x2$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$conditionalPower, collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(0.18175814, 0.2424364, 0.28642867, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(0.81824186, 0.7575636, 0.7496417, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x2$repeatedPValues, c(0.11554509, 0.032131177, 0.024656293, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x2$finalStage, NA_integer_, label = paste0("c(", paste0(x2$finalStage, collapse = ", "), ")"))
	expect_equal(x2$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$finalPValues, collapse = ", "), ")"))
	expect_equal(x2$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x2$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x2$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$medianUnbiasedEstimates, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x2), NA)))
	    expect_output(print(x2)$show())
	    invisible(capture.output(expect_error(summary(x2), NA)))
	    expect_output(summary(x2)$show())
	    x2CodeBased <- eval(parse(text = getObjectRCode(x2, stringWrapParagraphWidth = NULL)))
	    expect_equal(x2CodeBased$pi1, x2$pi1, tolerance = 1e-07)
	    expect_equal(x2CodeBased$testActions, x2$testActions, tolerance = 1e-07)
	    expect_equal(x2CodeBased$conditionalRejectionProbabilities, x2$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x2CodeBased$conditionalPower, x2$conditionalPower, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedConfidenceIntervalLowerBounds, x2$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedConfidenceIntervalUpperBounds, x2$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedPValues, x2$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x2CodeBased$finalStage, x2$finalStage, tolerance = 1e-07)
	    expect_equal(x2CodeBased$finalPValues, x2$finalPValues, tolerance = 1e-07)
	    expect_equal(x2CodeBased$finalConfidenceIntervalLowerBounds, x2$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$finalConfidenceIntervalUpperBounds, x2$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$medianUnbiasedEstimates, x2$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_type(names(x2), "character")
	    df <- as.data.frame(x2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsFisher}
	# @refFS[Formula]{fs:testStatisticOneRateApproximation}
	# @refFS[Formula]{fs:pValuesOneRateAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticFisherCombinationTest}
	# @refFS[Formula]{fs:definitionRCIFisherCombination}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:conditionalRejectionFisherInterim}
	x3 <- getAnalysisResults(
	    design = design3, dataInput = dataExample3,
	    stage = 2, thetaH0 = 0.75, nPlanned = c(12, 6), pi1 = 0.5, normalApproximation = FALSE,
	    directionUpper = FALSE, iterations = 1000, seed = 123
	)

	## Comparison of the results of AnalysisResultsFisher object 'x3' with expected results
	expect_equal(x3$testActions, c("continue", "continue", NA_character_, NA_character_), label = paste0("c(", paste0(x3$testActions, collapse = ", "), ")"))
	expect_equal(x3$conditionalRejectionProbabilities, c(0.027980027, 0.040164764, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds, c(0.12025548, 0.19023888, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds, c(0.87974452, 0.80976112, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x3$repeatedPValues, c(0.23393398, 0.11483365, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x3$finalStage, NA_integer_, label = paste0("c(", paste0(x3$finalStage, collapse = ", "), ")"))
	expect_equal(x3$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$finalPValues, collapse = ", "), ")"))
	expect_equal(x3$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x3$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x3$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$medianUnbiasedEstimates, collapse = ", "), ")"))
	expect_equal(x3$conditionalPowerSimulated, c(NA_real_, NA_real_, 0.522, 0.672), tolerance = 1e-07, label = paste0("c(", paste0(x3$conditionalPowerSimulated, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x3), NA)))
	    expect_output(print(x3)$show())
	    invisible(capture.output(expect_error(summary(x3), NA)))
	    expect_output(summary(x3)$show())
	    x3CodeBased <- eval(parse(text = getObjectRCode(x3, stringWrapParagraphWidth = NULL)))
	    expect_equal(x3CodeBased$testActions, x3$testActions, tolerance = 1e-07)
	    expect_equal(x3CodeBased$conditionalRejectionProbabilities, x3$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x3CodeBased$repeatedConfidenceIntervalLowerBounds, x3$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x3CodeBased$repeatedConfidenceIntervalUpperBounds, x3$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x3CodeBased$repeatedPValues, x3$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x3CodeBased$finalStage, x3$finalStage, tolerance = 1e-07)
	    expect_equal(x3CodeBased$finalPValues, x3$finalPValues, tolerance = 1e-07)
	    expect_equal(x3CodeBased$finalConfidenceIntervalLowerBounds, x3$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x3CodeBased$finalConfidenceIntervalUpperBounds, x3$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x3CodeBased$medianUnbiasedEstimates, x3$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_equal(x3CodeBased$conditionalPowerSimulated, x3$conditionalPowerSimulated, tolerance = 1e-07)
	    expect_type(names(x3), "character")
	    df <- as.data.frame(x3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	plotData1 <- testGetAnalysisResultsPlotData(x3, piTreatmentRange = seq(0.25, 0.55, 0.05))

	## Comparison of the results of list object 'plotData1' with expected results
	expect_equal(plotData1$xValues, c(0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55), tolerance = 1e-07, label = paste0("c(", paste0(plotData1$xValues, collapse = ", "), ")"))
	expect_equal(plotData1$condPowerValues, c(0.997, 0.99, 0.967, 0.9, 0.822, 0.659, 0.534), tolerance = 1e-07, label = paste0("c(", paste0(plotData1$condPowerValues, collapse = ", "), ")"))
	expect_equal(plotData1$likelihoodValues, c(0.10539922, 0.23692776, 0.44485807, 0.69767633, 0.91393119, 1, 0.91393119), tolerance = 1e-07, label = paste0("c(", paste0(plotData1$likelihoodValues, collapse = ", "), ")"))
	expect_equal(plotData1$main, "Conditional Power with Likelihood", label = paste0("c(", paste0(plotData1$main, collapse = ", "), ")"))
	expect_equal(plotData1$xlab, "pi1", label = paste0("c(", paste0(plotData1$xlab, collapse = ", "), ")"))
	expect_equal(plotData1$ylab, "Conditional power / Likelihood", label = paste0("c(", paste0(plotData1$ylab, collapse = ", "), ")"))
	expect_equal(plotData1$sub, "Stage = 2, # of remaining subjects = 18", label = paste0("c(", paste0(plotData1$sub, collapse = ", "), ")"))

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsFisher}
	# @refFS[Formula]{fs:testStatisticOneRateApproximation}
	# @refFS[Formula]{fs:pValuesOneRateAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticFisherCombinationTest}
	# @refFS[Formula]{fs:definitionRCIFisherCombination}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	x4 <- getAnalysisResults(
	    design = design3, dataInput = dataExample3,
	    stage = 3, thetaH0 = 0.25, normalApproximation = FALSE,
	    directionUpper = TRUE, iterations = 1000, seed = 123
	)

	## Comparison of the results of AnalysisResultsFisher object 'x4' with expected results
	expect_equal(x4$pi1, 0.51851852, tolerance = 1e-07, label = paste0("c(", paste0(x4$pi1, collapse = ", "), ")"))
	expect_equal(x4$testActions, c("continue", "continue", "continue", NA_character_), label = paste0("c(", paste0(x4$testActions, collapse = ", "), ")"))
	expect_equal(x4$conditionalRejectionProbabilities, c(0.027980027, 0.040164764, 0.10237226, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x4$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x4$conditionalPower, collapse = ", "), ")"))
	expect_equal(x4$repeatedConfidenceIntervalLowerBounds, c(0.12025548, 0.19023888, 0.23521677, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x4$repeatedConfidenceIntervalUpperBounds, c(0.87974452, 0.80976112, 0.79971589, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x4$repeatedPValues, c(0.23393398, 0.11483365, 0.040061917, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x4$finalStage, NA_integer_, label = paste0("c(", paste0(x4$finalStage, collapse = ", "), ")"))
	expect_equal(x4$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x4$finalPValues, collapse = ", "), ")"))
	expect_equal(x4$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x4$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x4$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x4$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x4$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x4$medianUnbiasedEstimates, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x4), NA)))
	    expect_output(print(x4)$show())
	    invisible(capture.output(expect_error(summary(x4), NA)))
	    expect_output(summary(x4)$show())
	    x4CodeBased <- eval(parse(text = getObjectRCode(x4, stringWrapParagraphWidth = NULL)))
	    expect_equal(x4CodeBased$pi1, x4$pi1, tolerance = 1e-07)
	    expect_equal(x4CodeBased$testActions, x4$testActions, tolerance = 1e-07)
	    expect_equal(x4CodeBased$conditionalRejectionProbabilities, x4$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x4CodeBased$conditionalPower, x4$conditionalPower, tolerance = 1e-07)
	    expect_equal(x4CodeBased$repeatedConfidenceIntervalLowerBounds, x4$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x4CodeBased$repeatedConfidenceIntervalUpperBounds, x4$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x4CodeBased$repeatedPValues, x4$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x4CodeBased$finalStage, x4$finalStage, tolerance = 1e-07)
	    expect_equal(x4CodeBased$finalPValues, x4$finalPValues, tolerance = 1e-07)
	    expect_equal(x4CodeBased$finalConfidenceIntervalLowerBounds, x4$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x4CodeBased$finalConfidenceIntervalUpperBounds, x4$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x4CodeBased$medianUnbiasedEstimates, x4$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_type(names(x4), "character")
	    df <- as.data.frame(x4)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x4)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsFisher}
	# @refFS[Formula]{fs:testStatisticOneRateApproximation}
	# @refFS[Formula]{fs:pValuesOneRateApproximationAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticFisherCombinationTest}
	# @refFS[Formula]{fs:definitionRCIFisherCombination}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	x5 <- getAnalysisResults(
	    design = design3, dataInput = dataExample3,
	    stage = 3, thetaH0 = 0.25, normalApproximation = TRUE,
	    directionUpper = TRUE, iterations = 1000, seed = 123
	)

	## Comparison of the results of AnalysisResultsFisher object 'x5' with expected results
	expect_equal(x5$pi1, 0.51851852, tolerance = 1e-07, label = paste0("c(", paste0(x5$pi1, collapse = ", "), ")"))
	expect_equal(x5$testActions, c("continue", "continue", "reject and stop", NA_character_), label = paste0("c(", paste0(x5$testActions, collapse = ", "), ")"))
	expect_equal(x5$conditionalRejectionProbabilities, c(0.051264101, 0.1206033, 1, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x5$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x5$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x5$conditionalPower, collapse = ", "), ")"))
	expect_equal(x5$repeatedConfidenceIntervalLowerBounds, c(0.18175814, 0.2424364, 0.28642867, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x5$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x5$repeatedConfidenceIntervalUpperBounds, c(0.81824186, 0.7575636, 0.7496417, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x5$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x5$repeatedPValues, c(0.11554509, 0.032131177, 0.0055275316, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x5$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x5$finalStage, NA_integer_, label = paste0("c(", paste0(x5$finalStage, collapse = ", "), ")"))
	expect_equal(x5$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x5$finalPValues, collapse = ", "), ")"))
	expect_equal(x5$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x5$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x5$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x5$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x5$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x5$medianUnbiasedEstimates, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x5), NA)))
	    expect_output(print(x5)$show())
	    invisible(capture.output(expect_error(summary(x5), NA)))
	    expect_output(summary(x5)$show())
	    x5CodeBased <- eval(parse(text = getObjectRCode(x5, stringWrapParagraphWidth = NULL)))
	    expect_equal(x5CodeBased$pi1, x5$pi1, tolerance = 1e-07)
	    expect_equal(x5CodeBased$testActions, x5$testActions, tolerance = 1e-07)
	    expect_equal(x5CodeBased$conditionalRejectionProbabilities, x5$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x5CodeBased$conditionalPower, x5$conditionalPower, tolerance = 1e-07)
	    expect_equal(x5CodeBased$repeatedConfidenceIntervalLowerBounds, x5$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x5CodeBased$repeatedConfidenceIntervalUpperBounds, x5$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x5CodeBased$repeatedPValues, x5$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x5CodeBased$finalStage, x5$finalStage, tolerance = 1e-07)
	    expect_equal(x5CodeBased$finalPValues, x5$finalPValues, tolerance = 1e-07)
	    expect_equal(x5CodeBased$finalConfidenceIntervalLowerBounds, x5$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x5CodeBased$finalConfidenceIntervalUpperBounds, x5$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x5CodeBased$medianUnbiasedEstimates, x5$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_type(names(x5), "character")
	    df <- as.data.frame(x5)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x5)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsFisher}
	# @refFS[Formula]{fs:testStatisticOneRateApproximation}
	# @refFS[Formula]{fs:pValuesOneRateAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticFisherCombinationTest}
	# @refFS[Formula]{fs:definitionRCIFisherCombination}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:conditionalRejectionFisherInterim}
	x6 <- getAnalysisResults(
	    design = design3, dataInput = dataExample3,
	    stage = 2, thetaH0 = 0.25, nPlanned = c(12, 6), pi1 = 0.5, normalApproximation = FALSE,
	    directionUpper = TRUE, iterations = 1000, seed = 123
	)

	## Comparison of the results of AnalysisResultsFisher object 'x6' with expected results
	expect_equal(x6$testActions, c("continue", "continue", NA_character_, NA_character_), label = paste0("c(", paste0(x6$testActions, collapse = ", "), ")"))
	expect_equal(x6$conditionalRejectionProbabilities, c(0.027980027, 0.040164764, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x6$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x6$repeatedConfidenceIntervalLowerBounds, c(0.12025548, 0.19023888, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x6$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x6$repeatedConfidenceIntervalUpperBounds, c(0.87974452, 0.80976112, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x6$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x6$repeatedPValues, c(0.23393398, 0.11483365, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x6$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x6$finalStage, NA_integer_, label = paste0("c(", paste0(x6$finalStage, collapse = ", "), ")"))
	expect_equal(x6$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x6$finalPValues, collapse = ", "), ")"))
	expect_equal(x6$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x6$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x6$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x6$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x6$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x6$medianUnbiasedEstimates, collapse = ", "), ")"))
	expect_equal(x6$conditionalPowerSimulated, c(NA_real_, NA_real_, 0.522, 0.672), tolerance = 1e-07, label = paste0("c(", paste0(x6$conditionalPowerSimulated, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x6), NA)))
	    expect_output(print(x6)$show())
	    invisible(capture.output(expect_error(summary(x6), NA)))
	    expect_output(summary(x6)$show())
	    x6CodeBased <- eval(parse(text = getObjectRCode(x6, stringWrapParagraphWidth = NULL)))
	    expect_equal(x6CodeBased$testActions, x6$testActions, tolerance = 1e-07)
	    expect_equal(x6CodeBased$conditionalRejectionProbabilities, x6$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x6CodeBased$repeatedConfidenceIntervalLowerBounds, x6$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x6CodeBased$repeatedConfidenceIntervalUpperBounds, x6$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x6CodeBased$repeatedPValues, x6$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x6CodeBased$finalStage, x6$finalStage, tolerance = 1e-07)
	    expect_equal(x6CodeBased$finalPValues, x6$finalPValues, tolerance = 1e-07)
	    expect_equal(x6CodeBased$finalConfidenceIntervalLowerBounds, x6$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x6CodeBased$finalConfidenceIntervalUpperBounds, x6$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x6CodeBased$medianUnbiasedEstimates, x6$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_equal(x6CodeBased$conditionalPowerSimulated, x6$conditionalPowerSimulated, tolerance = 1e-07)
	    expect_type(names(x6), "character")
	    df <- as.data.frame(x6)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x6)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	plotData2 <- testGetAnalysisResultsPlotData(x6, piTreatmentRange = seq(0.25, 0.55, 0.05))

	## Comparison of the results of list object 'plotData2' with expected results
	expect_equal(plotData2$xValues, c(0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55), tolerance = 1e-07, label = paste0("c(", paste0(plotData2$xValues, collapse = ", "), ")"))
	expect_equal(plotData2$condPowerValues, c(0.039, 0.114, 0.208, 0.363, 0.54, 0.659, 0.817), tolerance = 1e-07, label = paste0("c(", paste0(plotData2$condPowerValues, collapse = ", "), ")"))
	expect_equal(plotData2$likelihoodValues, c(0.10539922, 0.23692776, 0.44485807, 0.69767633, 0.91393119, 1, 0.91393119), tolerance = 1e-07, label = paste0("c(", paste0(plotData2$likelihoodValues, collapse = ", "), ")"))
	expect_equal(plotData2$main, "Conditional Power with Likelihood", label = paste0("c(", paste0(plotData2$main, collapse = ", "), ")"))
	expect_equal(plotData2$xlab, "pi1", label = paste0("c(", paste0(plotData2$xlab, collapse = ", "), ")"))
	expect_equal(plotData2$ylab, "Conditional power / Likelihood", label = paste0("c(", paste0(plotData2$ylab, collapse = ", "), ")"))
	expect_equal(plotData2$sub, "Stage = 2, # of remaining subjects = 18", label = paste0("c(", paste0(plotData2$sub, collapse = ", "), ")"))

})

test_plan_section("Testing the Analysis Rates Functionality for Two Treatments")


test_that("'getAnalysisResults' for a  four-stage group sequential design and two treatments", {
	.skipTestIfDisabled()

	design7 <- getDesignGroupSequential(
	    kMax = 4, alpha = 0.025,
	    typeOfDesign = "WT", deltaWT = 0.25, informationRates = c(0.2, 0.4, 0.8, 1),
	    futilityBounds = c(0, 0.5, 0.8), bindingFutility = TRUE
	)

	dataExample5 <- getDataset(
	    n1 = c(17, 18, 22),
	    n2 = c(18, 17, 19),
	    events1 = c(11, 12, 17),
	    events2 = c(5, 10, 7)
	)

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsGroupSequential}
	# @refFS[Formula]{fs:testStatisticTwoRatesApproximation}
	# @refFS[Formula]{fs:pValuesTwoRatesAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticGroupSequential}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerTwoRatesEffect}
	# @refFS[Formula]{fs:conditionalPowerTwoRatesSampleSizes}
	x1 <- getAnalysisResults(design7, dataExample5,
	    thetaH0 = 0, stage = 2, nPlanned = c(60, 30),
	    pi2 = 0.4, pi1 = 0.8, directionUpper = TRUE, allocationRatioPlanned = 2
	)

	## Comparison of the results of AnalysisResultsGroupSequential object 'x1' with expected results
	expect_equal(x1$testActions, c("continue", "continue", NA_character_, NA_character_), label = paste0("c(", paste0(x1$testActions, collapse = ", "), ")"))
	expect_equal(x1$conditionalRejectionProbabilities, c(0.19002543, 0.18837824, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x1$conditionalPower, c(NA_real_, NA_real_, 0.97639752, 0.99770454), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalPower, collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(-0.14000084, -0.07626859, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(0.72492429, 0.4944942, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x1$repeatedPValues, c(0.083297609, 0.074571507, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x1$finalStage, NA_integer_, label = paste0("c(", paste0(x1$finalStage, collapse = ", "), ")"))
	expect_equal(x1$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$finalPValues, collapse = ", "), ")"))
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$medianUnbiasedEstimates, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x1), NA)))
	    expect_output(print(x1)$show())
	    invisible(capture.output(expect_error(summary(x1), NA)))
	    expect_output(summary(x1)$show())
	    x1CodeBased <- eval(parse(text = getObjectRCode(x1, stringWrapParagraphWidth = NULL)))
	    expect_equal(x1CodeBased$testActions, x1$testActions, tolerance = 1e-07)
	    expect_equal(x1CodeBased$conditionalRejectionProbabilities, x1$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x1CodeBased$conditionalPower, x1$conditionalPower, tolerance = 1e-07)
	    expect_equal(x1CodeBased$repeatedConfidenceIntervalLowerBounds, x1$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$repeatedConfidenceIntervalUpperBounds, x1$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$repeatedPValues, x1$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x1CodeBased$finalStage, x1$finalStage, tolerance = 1e-07)
	    expect_equal(x1CodeBased$finalPValues, x1$finalPValues, tolerance = 1e-07)
	    expect_equal(x1CodeBased$finalConfidenceIntervalLowerBounds, x1$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$finalConfidenceIntervalUpperBounds, x1$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$medianUnbiasedEstimates, x1$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_type(names(x1), "character")
	    df <- as.data.frame(x1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	plotData1 <- testGetAnalysisResultsPlotData(x1, piTreatmentRange = seq(0.5, 0.8, 0.05), allocationRatioPlanned = 2)

	## Comparison of the results of list object 'plotData1' with expected results
	expect_equal(plotData1$xValues, c(0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8), tolerance = 1e-07, label = paste0("c(", paste0(plotData1$xValues, collapse = ", "), ")"))
	expect_equal(plotData1$condPowerValues, c(0.47726473, 0.64780315, 0.79588169, 0.90153211, 0.96202912, 0.98889368, 0.99770454), tolerance = 1e-07, label = paste0("c(", paste0(plotData1$condPowerValues, collapse = ", "), ")"))
	expect_equal(plotData1$likelihoodValues, c(0.14689674, 0.40998118, 0.77598415, 0.99604498, 0.86704618, 0.51184997, 0.20491809), tolerance = 1e-07, label = paste0("c(", paste0(plotData1$likelihoodValues, collapse = ", "), ")"))
	expect_equal(plotData1$main, "Conditional Power with Likelihood", label = paste0("c(", paste0(plotData1$main, collapse = ", "), ")"))
	expect_equal(plotData1$xlab, "pi1", label = paste0("c(", paste0(plotData1$xlab, collapse = ", "), ")"))
	expect_equal(plotData1$ylab, "Conditional power / Likelihood", label = paste0("c(", paste0(plotData1$ylab, collapse = ", "), ")"))
	expect_equal(plotData1$sub, "Stage = 2, # of remaining subjects = 90, pi2 = 0.4, allocation ratio = 2", label = paste0("c(", paste0(plotData1$sub, collapse = ", "), ")"))

	# reversed "directionUpper"
	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsGroupSequential}
	# @refFS[Formula]{fs:testStatisticTwoRatesApproximation}
	# @refFS[Formula]{fs:pValuesTwoRatesAlternativeSmaller}
	# @refFS[Formula]{fs:testStatisticGroupSequential}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerTwoRatesEffect}
	# @refFS[Formula]{fs:conditionalPowerTwoRatesSampleSizes}

	x2 <- getAnalysisResults(design7, dataExample5,
	    thetaH0 = 0, stage = 2, nPlanned = c(60, 30),
	    pi2 = 0.8, pi1 = 0.4, directionUpper = FALSE, allocationRatioPlanned = 0.5
	)

	## Comparison of the results of AnalysisResultsGroupSequential object 'x2' with expected results
	expect_equal(x2$testActions, c("accept and stop", "accept and stop", NA_character_, NA_character_), label = paste0("c(", paste0(x2$testActions, collapse = ", "), ")"))
	expect_equal(x2$conditionalRejectionProbabilities, c(0, 0, NA_real_, NA_real_), label = paste0("c(", paste0(x2$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x2$conditionalPower, c(NA_real_, NA_real_, 0.037603851, 0.34743098), tolerance = 1e-07, label = paste0("c(", paste0(x2$conditionalPower, collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(-0.14000084, -0.07626859, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(0.72492429, 0.4944942, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x2$repeatedPValues, c(0.49999905, 0.49999905, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x2$finalStage, 1, label = paste0("c(", paste0(x2$finalStage, collapse = ", "), ")"))
	expect_equal(x2$finalPValues, c(0.98580558, NA_real_, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$finalPValues, collapse = ", "), ")"))
	expect_equal(x2$finalConfidenceIntervalLowerBounds, c(0.039328966, NA_real_, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x2$finalConfidenceIntervalUpperBounds, c(0.62730979, NA_real_, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x2$medianUnbiasedEstimates, c(0.36928105, NA_real_, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$medianUnbiasedEstimates, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x2), NA)))
	    expect_output(print(x2)$show())
	    invisible(capture.output(expect_error(summary(x2), NA)))
	    expect_output(summary(x2)$show())
	    x2CodeBased <- eval(parse(text = getObjectRCode(x2, stringWrapParagraphWidth = NULL)))
	    expect_equal(x2CodeBased$testActions, x2$testActions, tolerance = 1e-07)
	    expect_equal(x2CodeBased$conditionalRejectionProbabilities, x2$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x2CodeBased$conditionalPower, x2$conditionalPower, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedConfidenceIntervalLowerBounds, x2$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedConfidenceIntervalUpperBounds, x2$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedPValues, x2$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x2CodeBased$finalStage, x2$finalStage, tolerance = 1e-07)
	    expect_equal(x2CodeBased$finalPValues, x2$finalPValues, tolerance = 1e-07)
	    expect_equal(x2CodeBased$finalConfidenceIntervalLowerBounds, x2$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$finalConfidenceIntervalUpperBounds, x2$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$medianUnbiasedEstimates, x2$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_type(names(x2), "character")
	    df <- as.data.frame(x2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	plotData2 <- testGetAnalysisResultsPlotData(x2, piTreatmentRange = seq(0.2, 0.5, 0.05), allocationRatioPlanned = 0.5)

	## Comparison of the results of list object 'plotData2' with expected results
	expect_equal(plotData2$xValues, c(0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5), tolerance = 1e-07, label = paste0("c(", paste0(plotData2$xValues, collapse = ", "), ")"))
	expect_equal(plotData2$condPowerValues, c(0.98964948, 0.93182725, 0.78360503, 0.56553646, 0.34743098, 0.18277547, 0.082851862), tolerance = 1e-07, label = paste0("c(", paste0(plotData2$condPowerValues, collapse = ", "), ")"))
	expect_equal(plotData2$likelihoodValues, c(8.9244677e-08, 2.5604189e-06, 4.9816924e-05, 0.00065732471, 0.0058819346, 0.035694195, 0.14689674), tolerance = 1e-07, label = paste0("c(", paste0(plotData2$likelihoodValues, collapse = ", "), ")"))
	expect_equal(plotData2$main, "Conditional Power with Likelihood", label = paste0("c(", paste0(plotData2$main, collapse = ", "), ")"))
	expect_equal(plotData2$xlab, "pi1", label = paste0("c(", paste0(plotData2$xlab, collapse = ", "), ")"))
	expect_equal(plotData2$ylab, "Conditional power / Likelihood", label = paste0("c(", paste0(plotData2$ylab, collapse = ", "), ")"))
	expect_equal(plotData2$sub, "Stage = 2, # of remaining subjects = 90, pi2 = 0.8, allocation ratio = 0.5", label = paste0("c(", paste0(plotData2$sub, collapse = ", "), ")"))

})

test_that("'getAnalysisResults' for a four-stage inverse normal design and two treatments", {

	.skipTestIfDisabled()

	design8 <- getDesignInverseNormal(
	    kMax = 4, alpha = 0.025,
	    typeOfDesign = "WT", deltaWT = 0.25, informationRates = c(0.2, 0.4, 0.8, 1),
	    futilityBounds = c(0, 0.5, 0.8), bindingFutility = TRUE
	)

	dataExample6 <- getDataset(
	    n1 = c(17, 18, 22),
	    n2 = c(18, 17, 19),
	    events1 = c(11, 12, 17),
	    events2 = c(5, 10, 7)
	)

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsInverseNormal}
	# @refFS[Formula]{fs:testStatisticTwoRatesApproximation}
	# @refFS[Formula]{fs:pValuesTwoRatesAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticNormalCombinationTest}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerTwoRatesEffect}
	# @refFS[Formula]{fs:conditionalPowerTwoRatesSampleSizes}
	x1 <- getAnalysisResults(design8, dataExample6,
	    thetaH0 = 0.0, stage = 2, nPlanned = c(30, 30),
	    pi2 = 0.2, pi1 = 0.4, directionUpper = TRUE
	)

	## Comparison of the results of AnalysisResultsInverseNormal object 'x1' with expected results
	expect_equal(x1$testActions, c("continue", "continue", NA_character_, NA_character_), label = paste0("c(", paste0(x1$testActions, collapse = ", "), ")"))
	expect_equal(x1$conditionalRejectionProbabilities, c(0.19002543, 0.18093983, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x1$conditionalPower, c(NA_real_, NA_real_, 0.51829859, 0.74637814), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalPower, collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(-0.14000084, -0.078581193, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(0.72492429, 0.48870113, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x1$repeatedPValues, c(0.083297609, 0.077943692, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x1$finalStage, NA_integer_, label = paste0("c(", paste0(x1$finalStage, collapse = ", "), ")"))
	expect_equal(x1$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$finalPValues, collapse = ", "), ")"))
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$medianUnbiasedEstimates, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x1), NA)))
	    expect_output(print(x1)$show())
	    invisible(capture.output(expect_error(summary(x1), NA)))
	    expect_output(summary(x1)$show())
	    x1CodeBased <- eval(parse(text = getObjectRCode(x1, stringWrapParagraphWidth = NULL)))
	    expect_equal(x1CodeBased$testActions, x1$testActions, tolerance = 1e-07)
	    expect_equal(x1CodeBased$conditionalRejectionProbabilities, x1$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x1CodeBased$conditionalPower, x1$conditionalPower, tolerance = 1e-07)
	    expect_equal(x1CodeBased$repeatedConfidenceIntervalLowerBounds, x1$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$repeatedConfidenceIntervalUpperBounds, x1$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$repeatedPValues, x1$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x1CodeBased$finalStage, x1$finalStage, tolerance = 1e-07)
	    expect_equal(x1CodeBased$finalPValues, x1$finalPValues, tolerance = 1e-07)
	    expect_equal(x1CodeBased$finalConfidenceIntervalLowerBounds, x1$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$finalConfidenceIntervalUpperBounds, x1$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$medianUnbiasedEstimates, x1$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_type(names(x1), "character")
	    df <- as.data.frame(x1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	plotData1 <- testGetAnalysisResultsPlotData(x1, piTreatmentRange = seq(0.4, 0.7, 0.05), nPlanned = c(30, 30))

	## Comparison of the results of list object 'plotData1' with expected results
	expect_equal(plotData1$xValues, c(0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7), tolerance = 1e-07, label = paste0("c(", paste0(plotData1$xValues, collapse = ", "), ")"))
	expect_equal(plotData1$condPowerValues, c(0.74637814, 0.85191228, 0.92421447, 0.96693166, 0.98816058, 0.99670572, 0.99934119), tolerance = 1e-07, label = paste0("c(", paste0(plotData1$condPowerValues, collapse = ", "), ")"))
	expect_equal(plotData1$likelihoodValues, c(0.0058819346, 0.035694195, 0.14689674, 0.40998118, 0.77598415, 0.99604498, 0.86704618), tolerance = 1e-07, label = paste0("c(", paste0(plotData1$likelihoodValues, collapse = ", "), ")"))
	expect_equal(plotData1$main, "Conditional Power with Likelihood", label = paste0("c(", paste0(plotData1$main, collapse = ", "), ")"))
	expect_equal(plotData1$xlab, "pi1", label = paste0("c(", paste0(plotData1$xlab, collapse = ", "), ")"))
	expect_equal(plotData1$ylab, "Conditional power / Likelihood", label = paste0("c(", paste0(plotData1$ylab, collapse = ", "), ")"))
	expect_equal(plotData1$sub, "Stage = 2, # of remaining subjects = 60, pi2 = 0.2, allocation ratio = 1", label = paste0("c(", paste0(plotData1$sub, collapse = ", "), ")"))

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsInverseNormal}
	# @refFS[Formula]{fs:testStatisticTwoRatesApproximation}
	# @refFS[Formula]{fs:pValuesTwoRatesAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticNormalCombinationTest}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerTwoRatesEffect}
	# @refFS[Formula]{fs:conditionalPowerTwoRatesSampleSizes}
	x3 <- getAnalysisResults(design8, dataExample6,
	    thetaH0 = 0, stage = 2, nPlanned = c(60, 30),
	    pi2 = 0.4, pi1 = 0.8, directionUpper = TRUE, allocationRatioPlanned = 2
	)

	## Comparison of the results of AnalysisResultsInverseNormal object 'x3' with expected results
	expect_equal(x3$testActions, c("continue", "continue", NA_character_, NA_character_), label = paste0("c(", paste0(x3$testActions, collapse = ", "), ")"))
	expect_equal(x3$conditionalRejectionProbabilities, c(0.19002543, 0.18093983, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x3$conditionalPower, c(NA_real_, NA_real_, 0.97637134, 0.99770045), tolerance = 1e-07, label = paste0("c(", paste0(x3$conditionalPower, collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds, c(-0.14000084, -0.078581193, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds, c(0.72492429, 0.48870113, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x3$repeatedPValues, c(0.083297609, 0.077943692, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x3$finalStage, NA_integer_, label = paste0("c(", paste0(x3$finalStage, collapse = ", "), ")"))
	expect_equal(x3$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$finalPValues, collapse = ", "), ")"))
	expect_equal(x3$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x3$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x3$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$medianUnbiasedEstimates, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x3), NA)))
	    expect_output(print(x3)$show())
	    invisible(capture.output(expect_error(summary(x3), NA)))
	    expect_output(summary(x3)$show())
	    x3CodeBased <- eval(parse(text = getObjectRCode(x3, stringWrapParagraphWidth = NULL)))
	    expect_equal(x3CodeBased$testActions, x3$testActions, tolerance = 1e-07)
	    expect_equal(x3CodeBased$conditionalRejectionProbabilities, x3$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x3CodeBased$conditionalPower, x3$conditionalPower, tolerance = 1e-07)
	    expect_equal(x3CodeBased$repeatedConfidenceIntervalLowerBounds, x3$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x3CodeBased$repeatedConfidenceIntervalUpperBounds, x3$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x3CodeBased$repeatedPValues, x3$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x3CodeBased$finalStage, x3$finalStage, tolerance = 1e-07)
	    expect_equal(x3CodeBased$finalPValues, x3$finalPValues, tolerance = 1e-07)
	    expect_equal(x3CodeBased$finalConfidenceIntervalLowerBounds, x3$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x3CodeBased$finalConfidenceIntervalUpperBounds, x3$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x3CodeBased$medianUnbiasedEstimates, x3$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_type(names(x3), "character")
	    df <- as.data.frame(x3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	plotData4 <- testGetAnalysisResultsPlotData(x3, piTreatmentRange = seq(0.5, 0.8, 0.05), allocationRatioPlanned = 2)

	## Comparison of the results of list object 'plotData4' with expected results
	expect_equal(plotData4$xValues, c(0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8), tolerance = 1e-07, label = paste0("c(", paste0(plotData4$xValues, collapse = ", "), ")"))
	expect_equal(plotData4$condPowerValues, c(0.4771434, 0.64764919, 0.79574037, 0.90143545, 0.96198044, 0.98887633, 0.99770045), tolerance = 1e-07, label = paste0("c(", paste0(plotData4$condPowerValues, collapse = ", "), ")"))
	expect_equal(plotData4$likelihoodValues, c(0.14689674, 0.40998118, 0.77598415, 0.99604498, 0.86704618, 0.51184997, 0.20491809), tolerance = 1e-07, label = paste0("c(", paste0(plotData4$likelihoodValues, collapse = ", "), ")"))
	expect_equal(plotData4$main, "Conditional Power with Likelihood", label = paste0("c(", paste0(plotData4$main, collapse = ", "), ")"))
	expect_equal(plotData4$xlab, "pi1", label = paste0("c(", paste0(plotData4$xlab, collapse = ", "), ")"))
	expect_equal(plotData4$ylab, "Conditional power / Likelihood", label = paste0("c(", paste0(plotData4$ylab, collapse = ", "), ")"))
	expect_equal(plotData4$sub, "Stage = 2, # of remaining subjects = 90, pi2 = 0.4, allocation ratio = 2", label = paste0("c(", paste0(plotData4$sub, collapse = ", "), ")"))

	# reversed "directionUpper"

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsInverseNormal}
	# @refFS[Formula]{fs:testStatisticTwoRatesApproximation}
	# @refFS[Formula]{fs:pValuesTwoRatesAlternativeSmaller}
	# @refFS[Formula]{fs:testStatisticNormalCombinationTest}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerTwoRatesEffect}
	# @refFS[Formula]{fs:conditionalPowerTwoRatesSampleSizes}
	x4 <- getAnalysisResults(design8, dataExample6,
	    thetaH0 = 0, stage = 2, nPlanned = c(60, 30),
	    pi2 = 0.8, pi1 = 0.4, directionUpper = FALSE, allocationRatioPlanned = 0.5
	)

	## Comparison of the results of AnalysisResultsInverseNormal object 'x4' with expected results
	expect_equal(x4$testActions, c("accept and stop", "accept and stop", NA_character_, NA_character_), label = paste0("c(", paste0(x4$testActions, collapse = ", "), ")"))
	expect_equal(x4$conditionalRejectionProbabilities, c(0, 0, NA_real_, NA_real_), label = paste0("c(", paste0(x4$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x4$conditionalPower, c(NA_real_, NA_real_, 0.037603851, 0.34743098), tolerance = 1e-07, label = paste0("c(", paste0(x4$conditionalPower, collapse = ", "), ")"))
	expect_equal(x4$repeatedConfidenceIntervalLowerBounds, c(-0.14000084, -0.078581193, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x4$repeatedConfidenceIntervalUpperBounds, c(0.72492429, 0.48870113, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x4$repeatedPValues, c(0.49999905, 0.49999905, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x4$finalStage, 1, label = paste0("c(", paste0(x4$finalStage, collapse = ", "), ")"))
	expect_equal(x4$finalPValues, c(0.98580558, NA_real_, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$finalPValues, collapse = ", "), ")"))
	expect_equal(x4$finalConfidenceIntervalLowerBounds, c(0.039328966, NA_real_, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x4$finalConfidenceIntervalUpperBounds, c(0.62730979, NA_real_, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x4$medianUnbiasedEstimates, c(0.36928105, NA_real_, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$medianUnbiasedEstimates, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x4), NA)))
	    expect_output(print(x4)$show())
	    invisible(capture.output(expect_error(summary(x4), NA)))
	    expect_output(summary(x4)$show())
	    x4CodeBased <- eval(parse(text = getObjectRCode(x4, stringWrapParagraphWidth = NULL)))
	    expect_equal(x4CodeBased$testActions, x4$testActions, tolerance = 1e-07)
	    expect_equal(x4CodeBased$conditionalRejectionProbabilities, x4$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x4CodeBased$conditionalPower, x4$conditionalPower, tolerance = 1e-07)
	    expect_equal(x4CodeBased$repeatedConfidenceIntervalLowerBounds, x4$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x4CodeBased$repeatedConfidenceIntervalUpperBounds, x4$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x4CodeBased$repeatedPValues, x4$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x4CodeBased$finalStage, x4$finalStage, tolerance = 1e-07)
	    expect_equal(x4CodeBased$finalPValues, x4$finalPValues, tolerance = 1e-07)
	    expect_equal(x4CodeBased$finalConfidenceIntervalLowerBounds, x4$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x4CodeBased$finalConfidenceIntervalUpperBounds, x4$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x4CodeBased$medianUnbiasedEstimates, x4$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_type(names(x4), "character")
	    df <- as.data.frame(x4)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x4)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	plotData5 <- testGetAnalysisResultsPlotData(x4, piTreatmentRange = seq(0.2, 0.5, 0.05), allocationRatioPlanned = 0.5)

	## Comparison of the results of list object 'plotData5' with expected results
	expect_equal(plotData5$xValues, c(0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5), tolerance = 1e-07, label = paste0("c(", paste0(plotData5$xValues, collapse = ", "), ")"))
	expect_equal(plotData5$condPowerValues, c(0.98964948, 0.93182725, 0.78360503, 0.56553646, 0.34743098, 0.18277547, 0.082851862), tolerance = 1e-07, label = paste0("c(", paste0(plotData5$condPowerValues, collapse = ", "), ")"))
	expect_equal(plotData5$likelihoodValues, c(8.9244677e-08, 2.5604189e-06, 4.9816924e-05, 0.00065732471, 0.0058819346, 0.035694195, 0.14689674), tolerance = 1e-07, label = paste0("c(", paste0(plotData5$likelihoodValues, collapse = ", "), ")"))
	expect_equal(plotData5$main, "Conditional Power with Likelihood", label = paste0("c(", paste0(plotData5$main, collapse = ", "), ")"))
	expect_equal(plotData5$xlab, "pi1", label = paste0("c(", paste0(plotData5$xlab, collapse = ", "), ")"))
	expect_equal(plotData5$ylab, "Conditional power / Likelihood", label = paste0("c(", paste0(plotData5$ylab, collapse = ", "), ")"))
	expect_equal(plotData5$sub, "Stage = 2, # of remaining subjects = 90, pi2 = 0.8, allocation ratio = 0.5", label = paste0("c(", paste0(plotData5$sub, collapse = ", "), ")"))

})

test_that("'getAnalysisResults' for a  four-stage Fisher design and two treatments", {

	.skipTestIfDisabled()

	design9 <- getDesignFisher(
	    kMax = 4, alpha = 0.025, method = "equalAlpha",
	    informationRates = c(0.2, 0.4, 0.8, 1)
	)

	dataExample7 <- getDataset(
	    n1 = c(17, 23, 22),
	    n2 = c(18, 20, 19),
	    events1 = c(11, 12, 17),
	    events2 = c(5, 10, 7)
	)

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsFisher}
	# @refFS[Formula]{fs:testStatisticTwoRatesApproximation}
	# @refFS[Formula]{fs:pValuesTwoRatesAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticFisherCombinationTest}
	# @refFS[Formula]{fs:definitionRCIFisherCombination}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:conditionalRejectionFisherInterim}
	# @refFS[Formula]{fs:conditionalRejectionFisherLastInterim}
	# @refFS[Formula]{fs:conditionalRejectionFisherweights}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerTwoRatesEffect}
	# @refFS[Formula]{fs:conditionalPowerTwoRatesSampleSizes}
	x1 <- getAnalysisResults(design9, dataExample7,
	    thetaH0 = 0, stage = 2, nPlanned = c(60, 30),
	    pi2 = 0.4, pi1 = 0.8, directionUpper = TRUE, allocationRatioPlanned = 2, seed = 123
	)

	## Comparison of the results of AnalysisResultsFisher object 'x1' with expected results
	expect_equal(x1$testActions, c("continue", "continue", NA_character_, NA_character_), label = paste0("c(", paste0(x1$testActions, collapse = ", "), ")"))
	expect_equal(x1$conditionalRejectionProbabilities, c(0.13898608, 0.050808351, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(-0.023853561, -0.068378457, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(0.66428984, 0.40418869, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x1$repeatedPValues, c(0.035427069, 0.088523734, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x1$finalStage, NA_integer_, label = paste0("c(", paste0(x1$finalStage, collapse = ", "), ")"))
	expect_equal(x1$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$finalPValues, collapse = ", "), ")"))
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$medianUnbiasedEstimates, collapse = ", "), ")"))
	expect_equal(x1$conditionalPowerSimulated, c(NA_real_, NA_real_, 0.925, 0.972), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalPowerSimulated, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x1), NA)))
	    expect_output(print(x1)$show())
	    invisible(capture.output(expect_error(summary(x1), NA)))
	    expect_output(summary(x1)$show())
	    x1CodeBased <- eval(parse(text = getObjectRCode(x1, stringWrapParagraphWidth = NULL)))
	    expect_equal(x1CodeBased$testActions, x1$testActions, tolerance = 1e-07)
	    expect_equal(x1CodeBased$conditionalRejectionProbabilities, x1$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x1CodeBased$repeatedConfidenceIntervalLowerBounds, x1$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$repeatedConfidenceIntervalUpperBounds, x1$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$repeatedPValues, x1$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x1CodeBased$finalStage, x1$finalStage, tolerance = 1e-07)
	    expect_equal(x1CodeBased$finalPValues, x1$finalPValues, tolerance = 1e-07)
	    expect_equal(x1CodeBased$finalConfidenceIntervalLowerBounds, x1$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$finalConfidenceIntervalUpperBounds, x1$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$medianUnbiasedEstimates, x1$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_equal(x1CodeBased$conditionalPowerSimulated, x1$conditionalPowerSimulated, tolerance = 1e-07)
	    expect_type(names(x1), "character")
	    df <- as.data.frame(x1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	plotData1 <- testGetAnalysisResultsPlotData(x1, piTreatmentRange = seq(0.5, 0.8, 0.05), allocationRatioPlanned = 2)

	## Comparison of the results of list object 'plotData1' with expected results
	expect_equal(plotData1$xValues, c(0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8), tolerance = 1e-07, label = paste0("c(", paste0(plotData1$xValues, collapse = ", "), ")"))
	expect_equal(plotData1$condPowerValues, c(0.199, 0.364, 0.506, 0.686, 0.839, 0.927, 0.979), tolerance = 1e-07, label = paste0("c(", paste0(plotData1$condPowerValues, collapse = ", "), ")"))
	expect_equal(plotData1$likelihoodValues, c(0.63105765, 0.95013529, 0.95013529, 0.63105765, 0.27837883, 0.081561833, 0.015871623), tolerance = 1e-07, label = paste0("c(", paste0(plotData1$likelihoodValues, collapse = ", "), ")"))
	expect_equal(plotData1$main, "Conditional Power with Likelihood", label = paste0("c(", paste0(plotData1$main, collapse = ", "), ")"))
	expect_equal(plotData1$xlab, "pi1", label = paste0("c(", paste0(plotData1$xlab, collapse = ", "), ")"))
	expect_equal(plotData1$ylab, "Conditional power / Likelihood", label = paste0("c(", paste0(plotData1$ylab, collapse = ", "), ")"))
	expect_equal(plotData1$sub, "Stage = 2, # of remaining subjects = 90, pi2 = 0.4, allocation ratio = 2", label = paste0("c(", paste0(plotData1$sub, collapse = ", "), ")"))

	# reversed "directionUpper"
	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsFisher}
	# @refFS[Formula]{fs:testStatisticTwoRatesApproximation}
	# @refFS[Formula]{fs:pValuesTwoRatesAlternativeSmaller}
	# @refFS[Formula]{fs:testStatisticFisherCombinationTest}
	# @refFS[Formula]{fs:definitionRCIFisherCombination}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:conditionalRejectionFisherInterim}
	# @refFS[Formula]{fs:conditionalRejectionFisherLastInterim}
	# @refFS[Formula]{fs:conditionalRejectionFisherweights}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerTwoRatesEffect}
	# @refFS[Formula]{fs:conditionalPowerTwoRatesSampleSizes}

	x2 <- getAnalysisResults(design9, dataExample7,
	    thetaH0 = 0, stage = 2, nPlanned = c(60, 30),
	    pi2 = 0.8, pi1 = 0.4, directionUpper = FALSE, allocationRatioPlanned = 0.5, seed = 123
	)

	## Comparison of the results of AnalysisResultsFisher object 'x2' with expected results
	expect_equal(x2$testActions, c("continue", "continue", NA_character_, NA_character_), label = paste0("c(", paste0(x2$testActions, collapse = ", "), ")"))
	expect_equal(x2$conditionalRejectionProbabilities, c(0.0056634595, 0.0023089469, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(-0.023853561, -0.068378457, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(0.66428984, 0.40418869, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x2$repeatedPValues, c(0.49999905, 0.49999905, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x2$finalStage, NA_integer_, label = paste0("c(", paste0(x2$finalStage, collapse = ", "), ")"))
	expect_equal(x2$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$finalPValues, collapse = ", "), ")"))
	expect_equal(x2$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x2$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x2$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$medianUnbiasedEstimates, collapse = ", "), ")"))
	expect_equal(x2$conditionalPowerSimulated, c(NA_real_, NA_real_, 0.591, 0.788), tolerance = 1e-07, label = paste0("c(", paste0(x2$conditionalPowerSimulated, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x2), NA)))
	    expect_output(print(x2)$show())
	    invisible(capture.output(expect_error(summary(x2), NA)))
	    expect_output(summary(x2)$show())
	    x2CodeBased <- eval(parse(text = getObjectRCode(x2, stringWrapParagraphWidth = NULL)))
	    expect_equal(x2CodeBased$testActions, x2$testActions, tolerance = 1e-07)
	    expect_equal(x2CodeBased$conditionalRejectionProbabilities, x2$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedConfidenceIntervalLowerBounds, x2$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedConfidenceIntervalUpperBounds, x2$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedPValues, x2$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x2CodeBased$finalStage, x2$finalStage, tolerance = 1e-07)
	    expect_equal(x2CodeBased$finalPValues, x2$finalPValues, tolerance = 1e-07)
	    expect_equal(x2CodeBased$finalConfidenceIntervalLowerBounds, x2$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$finalConfidenceIntervalUpperBounds, x2$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$medianUnbiasedEstimates, x2$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_equal(x2CodeBased$conditionalPowerSimulated, x2$conditionalPowerSimulated, tolerance = 1e-07)
	    expect_type(names(x2), "character")
	    df <- as.data.frame(x2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	plotData2 <- testGetAnalysisResultsPlotData(x2, piTreatmentRange = seq(0.2, 0.5, 0.05), allocationRatioPlanned = 0.5)

	## Comparison of the results of list object 'plotData2' with expected results
	expect_equal(plotData2$xValues, c(0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5), tolerance = 1e-07, label = paste0("c(", paste0(plotData2$xValues, collapse = ", "), ")"))
	expect_equal(plotData2$condPowerValues, c(0.998, 0.992, 0.967, 0.892, 0.807, 0.623, 0.493), tolerance = 1e-07, label = paste0("c(", paste0(plotData2$condPowerValues, collapse = ", "), ")"))
	expect_equal(plotData2$likelihoodValues, c(1.003982e-05, 0.00017609247, 0.0020513476, 0.015871623, 0.081561833, 0.27837883, 0.63105765), tolerance = 1e-07, label = paste0("c(", paste0(plotData2$likelihoodValues, collapse = ", "), ")"))
	expect_equal(plotData2$main, "Conditional Power with Likelihood", label = paste0("c(", paste0(plotData2$main, collapse = ", "), ")"))
	expect_equal(plotData2$xlab, "pi1", label = paste0("c(", paste0(plotData2$xlab, collapse = ", "), ")"))
	expect_equal(plotData2$ylab, "Conditional power / Likelihood", label = paste0("c(", paste0(plotData2$ylab, collapse = ", "), ")"))
	expect_equal(plotData2$sub, "Stage = 2, # of remaining subjects = 90, pi2 = 0.8, allocation ratio = 0.5", label = paste0("c(", paste0(plotData2$sub, collapse = ", "), ")"))

})

test_that("'getAnalysisResults' produces the correct exact tests and final CIs", {

	.skipTestIfDisabled()

	dataExample8 <- getDataset(
	    n2 = c(31, 72),
	    n1 = c(30, 69),
	    events2 = c(8, 54),
	    events1 = c(6, 45)
	)

	design10 <- getDesignGroupSequential(
	    kMax = 2, alpha = 0.025,
	    typeOfDesign = "WT", deltaWT = 0.1, informationRates = c(0.3, 1)
	)

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsInverseNormal}
	# @refFS[Formula]{fs:testStatisticTwoRatesApproximation}
	# @refFS[Formula]{fs:pValuesTwoRatesAlternativeSmaller}
	# @refFS[Formula]{fs:testStatisticNormalCombinationTest}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerTwoRatesEffect}
	# @refFS[Formula]{fs:conditionalPowerTwoRatesSampleSizes}
	# @refFS[Formula]{fs:orderingPValueUpper}
	# @refFS[Formula]{fs:finalCITwoRates}
	# @refFS[Formula]{fs:medianUnbiasedEstimate}
	x1 <- getAnalysisResults(design10, dataExample8,
	    thetaH0 = 0, stage = 2, directionUpper = FALSE,
	    normalApproximation = FALSE
	)

	## Comparison of the results of AnalysisResultsGroupSequential object 'x1' with expected results
	expect_equal(x1$pi1, 0.51515152, tolerance = 1e-07, label = paste0("c(", paste0(x1$pi1, collapse = ", "), ")"))
	expect_equal(x1$pi2, 0.60194175, tolerance = 1e-07, label = paste0("c(", paste0(x1$pi2, collapse = ", "), ")"))
	expect_equal(x1$testActions, c("continue", "accept"), label = paste0("c(", paste0(x1$testActions, collapse = ", "), ")"))
	expect_equal(x1$conditionalRejectionProbabilities, c(0.013966781, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x1$conditionalPower, c(NA_real_, NA_real_), label = paste0("c(", paste0(x1$conditionalPower, collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(-0.39509356, -0.22101238), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(0.29306133, 0.050448655), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x1$repeatedPValues, c(0.49999905, 0.15271161), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x1$finalStage, 2, label = paste0("c(", paste0(x1$finalStage, collapse = ", "), ")"))
	expect_equal(x1$finalPValues, c(NA_real_, 0.13570939), tolerance = 1e-07, label = paste0("c(", paste0(x1$finalPValues, collapse = ", "), ")"))
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, -0.21309581), tolerance = 1e-07, label = paste0("c(", paste0(x1$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, 0.059922132), tolerance = 1e-07, label = paste0("c(", paste0(x1$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, -0.076600295), tolerance = 1e-07, label = paste0("c(", paste0(x1$medianUnbiasedEstimates, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x1), NA)))
	    expect_output(print(x1)$show())
	    invisible(capture.output(expect_error(summary(x1), NA)))
	    expect_output(summary(x1)$show())
	    x1CodeBased <- eval(parse(text = getObjectRCode(x1, stringWrapParagraphWidth = NULL)))
	    expect_equal(x1CodeBased$pi1, x1$pi1, tolerance = 1e-07)
	    expect_equal(x1CodeBased$pi2, x1$pi2, tolerance = 1e-07)
	    expect_equal(x1CodeBased$testActions, x1$testActions, tolerance = 1e-07)
	    expect_equal(x1CodeBased$conditionalRejectionProbabilities, x1$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x1CodeBased$conditionalPower, x1$conditionalPower, tolerance = 1e-07)
	    expect_equal(x1CodeBased$repeatedConfidenceIntervalLowerBounds, x1$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$repeatedConfidenceIntervalUpperBounds, x1$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$repeatedPValues, x1$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x1CodeBased$finalStage, x1$finalStage, tolerance = 1e-07)
	    expect_equal(x1CodeBased$finalPValues, x1$finalPValues, tolerance = 1e-07)
	    expect_equal(x1CodeBased$finalConfidenceIntervalLowerBounds, x1$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$finalConfidenceIntervalUpperBounds, x1$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$medianUnbiasedEstimates, x1$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_type(names(x1), "character")
	    df <- as.data.frame(x1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	design11 <- getDesignInverseNormal(
	    kMax = 2, alpha = 0.025,
	    typeOfDesign = "WT", deltaWT = 0.1, informationRates = c(0.3, 1)
	)

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsInverseNormal}
	# @refFS[Formula]{fs:pValuesTwoRatesAlternativeSmaller}
	# @refFS[Formula]{fs:testStatisticNormalCombinationTest}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerTwoRatesEffect}
	# @refFS[Formula]{fs:conditionalPowerTwoRatesSampleSizes}
	x2 <- getAnalysisResults(design11, dataExample8,
	    thetaH0 = 0, stage = 2, directionUpper = FALSE,
	    normalApproximation = FALSE
	)

	## Comparison of the results of AnalysisResultsInverseNormal object 'x2' with expected results
	expect_equal(x2$pi1, 0.51515152, tolerance = 1e-07, label = paste0("c(", paste0(x2$pi1, collapse = ", "), ")"))
	expect_equal(x2$pi2, 0.60194175, tolerance = 1e-07, label = paste0("c(", paste0(x2$pi2, collapse = ", "), ")"))
	expect_equal(x2$testActions, c("continue", "accept"), label = paste0("c(", paste0(x2$testActions, collapse = ", "), ")"))
	expect_equal(x2$conditionalRejectionProbabilities, c(0.013966781, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x2$conditionalPower, c(NA_real_, NA_real_), label = paste0("c(", paste0(x2$conditionalPower, collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(-0.39509356, -0.20744977), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(0.29306133, 0.038390636), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x2$repeatedPValues, c(0.49999905, 0.171251), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x2$finalStage, 2, label = paste0("c(", paste0(x2$finalStage, collapse = ", "), ")"))
	expect_equal(x2$finalPValues, c(NA_real_, 0.15026298), tolerance = 1e-07, label = paste0("c(", paste0(x2$finalPValues, collapse = ", "), ")"))
	expect_equal(x2$finalConfidenceIntervalLowerBounds, c(NA_real_, -0.20860056), tolerance = 1e-07, label = paste0("c(", paste0(x2$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x2$finalConfidenceIntervalUpperBounds, c(NA_real_, 0.064410651), tolerance = 1e-07, label = paste0("c(", paste0(x2$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x2$medianUnbiasedEstimates, c(NA_real_, -0.072106168), tolerance = 1e-07, label = paste0("c(", paste0(x2$medianUnbiasedEstimates, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x2), NA)))
	    expect_output(print(x2)$show())
	    invisible(capture.output(expect_error(summary(x2), NA)))
	    expect_output(summary(x2)$show())
	    x2CodeBased <- eval(parse(text = getObjectRCode(x2, stringWrapParagraphWidth = NULL)))
	    expect_equal(x2CodeBased$pi1, x2$pi1, tolerance = 1e-07)
	    expect_equal(x2CodeBased$pi2, x2$pi2, tolerance = 1e-07)
	    expect_equal(x2CodeBased$testActions, x2$testActions, tolerance = 1e-07)
	    expect_equal(x2CodeBased$conditionalRejectionProbabilities, x2$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x2CodeBased$conditionalPower, x2$conditionalPower, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedConfidenceIntervalLowerBounds, x2$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedConfidenceIntervalUpperBounds, x2$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedPValues, x2$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x2CodeBased$finalStage, x2$finalStage, tolerance = 1e-07)
	    expect_equal(x2CodeBased$finalPValues, x2$finalPValues, tolerance = 1e-07)
	    expect_equal(x2CodeBased$finalConfidenceIntervalLowerBounds, x2$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$finalConfidenceIntervalUpperBounds, x2$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$medianUnbiasedEstimates, x2$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_type(names(x2), "character")
	    df <- as.data.frame(x2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	design12 <- getDesignFisher(
	    kMax = 2, alpha = 0.025, method = "fullAlpha",
	    informationRates = c(0.3, 1)
	)

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsFisher}
	# @refFS[Formula]{fs:pValuesTwoRatesAlternativeSmaller}
	# @refFS[Formula]{fs:testStatisticFisherCombinationTest}
	# @refFS[Formula]{fs:definitionRCIFisherCombination}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:conditionalRejectionFisherInterim}
	# @refFS[Formula]{fs:conditionalRejectionFisherLastInterim}
	# @refFS[Formula]{fs:conditionalRejectionFisherweights}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerTwoRatesEffect}
	# @refFS[Formula]{fs:conditionalPowerTwoRatesSampleSizes}
	x3 <- getAnalysisResults(design12, dataExample8,
	    thetaH0 = 0, stage = 2, directionUpper = FALSE,
	    normalApproximation = FALSE, seed = 123
	)

	## Comparison of the results of AnalysisResultsFisher object 'x3' with expected results
	expect_equal(x3$pi1, 0.51515152, tolerance = 1e-07, label = paste0("c(", paste0(x3$pi1, collapse = ", "), ")"))
	expect_equal(x3$pi2, 0.60194175, tolerance = 1e-07, label = paste0("c(", paste0(x3$pi2, collapse = ", "), ")"))
	expect_equal(x3$testActions, c("continue", "accept"), label = paste0("c(", paste0(x3$testActions, collapse = ", "), ")"))
	expect_equal(x3$conditionalRejectionProbabilities, c(0.016431334, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x3$conditionalPower, c(NA_real_, NA_real_), label = paste0("c(", paste0(x3$conditionalPower, collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds, c(-0.39357809, -0.2198965), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds, c(0.29140184, 0.047490149), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x3$repeatedPValues, c(0.49999905, 0.18563047), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x3$finalStage, 2, label = paste0("c(", paste0(x3$finalStage, collapse = ", "), ")"))
	expect_equal(x3$finalPValues, c(NA_real_, 0.18562957), tolerance = 1e-07, label = paste0("c(", paste0(x3$finalPValues, collapse = ", "), ")"))
	expect_equal(x3$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_), label = paste0("c(", paste0(x3$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x3$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_), label = paste0("c(", paste0(x3$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x3$medianUnbiasedEstimates, c(NA_real_, NA_real_), label = paste0("c(", paste0(x3$medianUnbiasedEstimates, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x3), NA)))
	    expect_output(print(x3)$show())
	    invisible(capture.output(expect_error(summary(x3), NA)))
	    expect_output(summary(x3)$show())
	    x3CodeBased <- eval(parse(text = getObjectRCode(x3, stringWrapParagraphWidth = NULL)))
	    expect_equal(x3CodeBased$pi1, x3$pi1, tolerance = 1e-07)
	    expect_equal(x3CodeBased$pi2, x3$pi2, tolerance = 1e-07)
	    expect_equal(x3CodeBased$testActions, x3$testActions, tolerance = 1e-07)
	    expect_equal(x3CodeBased$conditionalRejectionProbabilities, x3$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x3CodeBased$conditionalPower, x3$conditionalPower, tolerance = 1e-07)
	    expect_equal(x3CodeBased$repeatedConfidenceIntervalLowerBounds, x3$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x3CodeBased$repeatedConfidenceIntervalUpperBounds, x3$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x3CodeBased$repeatedPValues, x3$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x3CodeBased$finalStage, x3$finalStage, tolerance = 1e-07)
	    expect_equal(x3CodeBased$finalPValues, x3$finalPValues, tolerance = 1e-07)
	    expect_equal(x3CodeBased$finalConfidenceIntervalLowerBounds, x3$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x3CodeBased$finalConfidenceIntervalUpperBounds, x3$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x3CodeBased$medianUnbiasedEstimates, x3$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_type(names(x3), "character")
	    df <- as.data.frame(x3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getAnalysisResults' produces the correct non-inferiority results for a group sequential design", {

	.skipTestIfDisabled()

	design13 <- getDesignGroupSequential(
	    kMax = 2, alpha = 0.025,
	    typeOfDesign = "WT", deltaWT = 0.1, informationRates = c(0.3, 1)
	)

	dataExample9 <- getDataset(
	    n1 = c(29, 70),
	    n2 = c(31, 71),
	    events1 = c(8, 54),
	    events2 = c(6, 45)
	)

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsGroupSequential}
	# @refFS[Formula]{fs:testStatisticTwoRatesApproximation}
	# @refFS[Formula]{fs:pValuesTwoRatesAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticGroupSequential}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:EstimatesDiffFarringtonManning}
	x1 <- getAnalysisResults(design13, dataExample9,
	    thetaH0 = -0.1, stage = 2, directionUpper = TRUE,
	    normalApproximation = TRUE
	)

	## Comparison of the results of AnalysisResultsGroupSequential object 'x1' with expected results
	expect_equal(x1$pi1, 0.62626263, tolerance = 1e-07, label = paste0("c(", paste0(x1$pi1, collapse = ", "), ")"))
	expect_equal(x1$pi2, 0.5, tolerance = 1e-07, label = paste0("c(", paste0(x1$pi2, collapse = ", "), ")"))
	expect_equal(x1$testActions, c("continue", "reject"), label = paste0("c(", paste0(x1$testActions, collapse = ", "), ")"))
	expect_equal(x1$conditionalRejectionProbabilities, c(0.1027905, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x1$conditionalPower, c(NA_real_, NA_real_), label = paste0("c(", paste0(x1$conditionalPower, collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(-0.26992436, -0.011398056), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(0.42527258, 0.25916391), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x1$repeatedPValues, c(0.17488831, 0.00058560119), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x1$finalStage, 2, label = paste0("c(", paste0(x1$finalStage, collapse = ", "), ")"))
	expect_equal(x1$finalPValues, c(NA_real_, 0.0012732763), tolerance = 1e-07, label = paste0("c(", paste0(x1$finalPValues, collapse = ", "), ")"))
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, -0.016122347), tolerance = 1e-07, label = paste0("c(", paste0(x1$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, 0.26034096), tolerance = 1e-07, label = paste0("c(", paste0(x1$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, 0.12355576), tolerance = 1e-07, label = paste0("c(", paste0(x1$medianUnbiasedEstimates, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x1), NA)))
	    expect_output(print(x1)$show())
	    invisible(capture.output(expect_error(summary(x1), NA)))
	    expect_output(summary(x1)$show())
	    x1CodeBased <- eval(parse(text = getObjectRCode(x1, stringWrapParagraphWidth = NULL)))
	    expect_equal(x1CodeBased$pi1, x1$pi1, tolerance = 1e-07)
	    expect_equal(x1CodeBased$pi2, x1$pi2, tolerance = 1e-07)
	    expect_equal(x1CodeBased$testActions, x1$testActions, tolerance = 1e-07)
	    expect_equal(x1CodeBased$conditionalRejectionProbabilities, x1$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x1CodeBased$conditionalPower, x1$conditionalPower, tolerance = 1e-07)
	    expect_equal(x1CodeBased$repeatedConfidenceIntervalLowerBounds, x1$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$repeatedConfidenceIntervalUpperBounds, x1$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$repeatedPValues, x1$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x1CodeBased$finalStage, x1$finalStage, tolerance = 1e-07)
	    expect_equal(x1CodeBased$finalPValues, x1$finalPValues, tolerance = 1e-07)
	    expect_equal(x1CodeBased$finalConfidenceIntervalLowerBounds, x1$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$finalConfidenceIntervalUpperBounds, x1$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$medianUnbiasedEstimates, x1$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_type(names(x1), "character")
	    df <- as.data.frame(x1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsGroupSequential}
	# @refFS[Formula]{fs:testStatisticTwoRatesApproximation}
	# @refFS[Formula]{fs:pValuesTwoRatesAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticGroupSequential}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:EstimatesDiffFarringtonManning}
	x2 <- getAnalysisResults(design13, dataExample9,
	    thetaH0 = -0.1, stage = 1, nPlanned = 40,
	    pi1 = 0.45, pi2 = 0.4, directionUpper = TRUE, normalApproximation = TRUE
	)

	## Comparison of the results of AnalysisResultsGroupSequential object 'x2' with expected results
	expect_equal(x2$testActions, c("continue", NA_character_), label = paste0("c(", paste0(x2$testActions, collapse = ", "), ")"))
	expect_equal(x2$conditionalRejectionProbabilities, c(0.1027905, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x2$conditionalPower, c(NA_real_, 0.38169554), tolerance = 1e-07, label = paste0("c(", paste0(x2$conditionalPower, collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(-0.26992436, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(0.42527258, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x2$repeatedPValues, c(0.17488831, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x2$finalStage, NA_integer_, label = paste0("c(", paste0(x2$finalStage, collapse = ", "), ")"))
	expect_equal(x2$finalPValues, c(NA_real_, NA_real_), label = paste0("c(", paste0(x2$finalPValues, collapse = ", "), ")"))
	expect_equal(x2$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_), label = paste0("c(", paste0(x2$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x2$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_), label = paste0("c(", paste0(x2$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x2$medianUnbiasedEstimates, c(NA_real_, NA_real_), label = paste0("c(", paste0(x2$medianUnbiasedEstimates, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x2), NA)))
	    expect_output(print(x2)$show())
	    invisible(capture.output(expect_error(summary(x2), NA)))
	    expect_output(summary(x2)$show())
	    x2CodeBased <- eval(parse(text = getObjectRCode(x2, stringWrapParagraphWidth = NULL)))
	    expect_equal(x2CodeBased$testActions, x2$testActions, tolerance = 1e-07)
	    expect_equal(x2CodeBased$conditionalRejectionProbabilities, x2$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x2CodeBased$conditionalPower, x2$conditionalPower, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedConfidenceIntervalLowerBounds, x2$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedConfidenceIntervalUpperBounds, x2$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedPValues, x2$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x2CodeBased$finalStage, x2$finalStage, tolerance = 1e-07)
	    expect_equal(x2CodeBased$finalPValues, x2$finalPValues, tolerance = 1e-07)
	    expect_equal(x2CodeBased$finalConfidenceIntervalLowerBounds, x2$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$finalConfidenceIntervalUpperBounds, x2$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$medianUnbiasedEstimates, x2$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_type(names(x2), "character")
	    df <- as.data.frame(x2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	plotData1 <- testGetAnalysisResultsPlotData(x2, piTreatmentRange = seq(0.25, 0.7, 0.05))

	## Comparison of the results of list object 'plotData1' with expected results
	expect_equal(plotData1$xValues, c(0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7), tolerance = 1e-07, label = paste0("c(", paste0(plotData1$xValues, collapse = ", "), ")"))
	expect_equal(plotData1$condPowerValues, c(0.053165998, 0.1027905, 0.17500031, 0.26934912, 0.38169554, 0.50456648, 0.62825352, 0.74249459, 0.83846571, 0.91065807), tolerance = 1e-07, label = paste0("c(", paste0(plotData1$condPowerValues, collapse = ", "), ")"))
	expect_equal(plotData1$likelihoodValues, c(0.95261056, 0.95859015, 0.67101367, 0.32674624, 0.11068039, 0.026080239, 0.0042749722, 0.00048745649, 3.866511e-05, 2.1334549e-06), tolerance = 1e-07, label = paste0("c(", paste0(plotData1$likelihoodValues, collapse = ", "), ")"))
	expect_equal(plotData1$main, "Conditional Power with Likelihood", label = paste0("c(", paste0(plotData1$main, collapse = ", "), ")"))
	expect_equal(plotData1$xlab, "pi1", label = paste0("c(", paste0(plotData1$xlab, collapse = ", "), ")"))
	expect_equal(plotData1$ylab, "Conditional power / Likelihood", label = paste0("c(", paste0(plotData1$ylab, collapse = ", "), ")"))
	expect_equal(plotData1$sub, "Stage = 1, # of remaining subjects = 40, pi2 = 0.4, allocation ratio = 1", label = paste0("c(", paste0(plotData1$sub, collapse = ", "), ")"))

	# non-inferiority, reversed "directionUpper"

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsGroupSequential}
	# @refFS[Formula]{fs:testStatisticTwoRatesApproximation}
	# @refFS[Formula]{fs:pValuesTwoRatesAlternativeSmaller}
	# @refFS[Formula]{fs:testStatisticGroupSequential}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:EstimatesDiffFarringtonManning}
	x3 <- getAnalysisResults(design13, dataExample9,
	    thetaH0 = 0.1, stage = 2, directionUpper = FALSE,
	    normalApproximation = TRUE
	)

	## Comparison of the results of AnalysisResultsGroupSequential object 'x3' with expected results
	expect_equal(x3$pi1, 0.62626263, tolerance = 1e-07, label = paste0("c(", paste0(x3$pi1, collapse = ", "), ")"))
	expect_equal(x3$pi2, 0.5, tolerance = 1e-07, label = paste0("c(", paste0(x3$pi2, collapse = ", "), ")"))
	expect_equal(x3$testActions, c("continue", "accept"), label = paste0("c(", paste0(x3$testActions, collapse = ", "), ")"))
	expect_equal(x3$conditionalRejectionProbabilities, c(0.012395218, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x3$conditionalPower, c(NA_real_, NA_real_), label = paste0("c(", paste0(x3$conditionalPower, collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds, c(-0.26992436, -0.011398056), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds, c(0.42527258, 0.25916391), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x3$repeatedPValues, c(0.49999905, 0.49999905), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x3$finalStage, 2, label = paste0("c(", paste0(x3$finalStage, collapse = ", "), ")"))
	expect_equal(x3$finalPValues, c(NA_real_, 0.64703032), tolerance = 1e-07, label = paste0("c(", paste0(x3$finalPValues, collapse = ", "), ")"))
	expect_equal(x3$finalConfidenceIntervalLowerBounds, c(NA_real_, -0.0098227441), tolerance = 1e-07, label = paste0("c(", paste0(x3$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x3$finalConfidenceIntervalUpperBounds, c(NA_real_, 0.26218829), tolerance = 1e-07, label = paste0("c(", paste0(x3$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x3$medianUnbiasedEstimates, c(NA_real_, 0.12618258), tolerance = 1e-07, label = paste0("c(", paste0(x3$medianUnbiasedEstimates, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x3), NA)))
	    expect_output(print(x3)$show())
	    invisible(capture.output(expect_error(summary(x3), NA)))
	    expect_output(summary(x3)$show())
	    x3CodeBased <- eval(parse(text = getObjectRCode(x3, stringWrapParagraphWidth = NULL)))
	    expect_equal(x3CodeBased$pi1, x3$pi1, tolerance = 1e-07)
	    expect_equal(x3CodeBased$pi2, x3$pi2, tolerance = 1e-07)
	    expect_equal(x3CodeBased$testActions, x3$testActions, tolerance = 1e-07)
	    expect_equal(x3CodeBased$conditionalRejectionProbabilities, x3$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x3CodeBased$conditionalPower, x3$conditionalPower, tolerance = 1e-07)
	    expect_equal(x3CodeBased$repeatedConfidenceIntervalLowerBounds, x3$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x3CodeBased$repeatedConfidenceIntervalUpperBounds, x3$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x3CodeBased$repeatedPValues, x3$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x3CodeBased$finalStage, x3$finalStage, tolerance = 1e-07)
	    expect_equal(x3CodeBased$finalPValues, x3$finalPValues, tolerance = 1e-07)
	    expect_equal(x3CodeBased$finalConfidenceIntervalLowerBounds, x3$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x3CodeBased$finalConfidenceIntervalUpperBounds, x3$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x3CodeBased$medianUnbiasedEstimates, x3$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_type(names(x3), "character")
	    df <- as.data.frame(x3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getAnalysisResultsGroupSequential}
	# @refFS[Formula]{fs:testStatisticTwoRatesApproximation}
	# @refFS[Formula]{fs:pValuesTwoRatesAlternativeSmaller}
	# @refFS[Formula]{fs:testStatisticGroupSequential}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:EstimatesDiffFarringtonManning}
	x4 <- getAnalysisResults(design13, dataExample9,
	    thetaH0 = 0.1, stage = 1, nPlanned = 40,
	    pi1 = 0.4, pi2 = 0.45, directionUpper = FALSE, normalApproximation = TRUE
	)

	## Comparison of the results of AnalysisResultsGroupSequential object 'x4' with expected results
	expect_equal(x4$testActions, c("continue", NA_character_), label = paste0("c(", paste0(x4$testActions, collapse = ", "), ")"))
	expect_equal(x4$conditionalRejectionProbabilities, c(0.012395218, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(x4$conditionalPower, c(NA_real_, 0.10084143), tolerance = 1e-07, label = paste0("c(", paste0(x4$conditionalPower, collapse = ", "), ")"))
	expect_equal(x4$repeatedConfidenceIntervalLowerBounds, c(-0.26992436, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x4$repeatedConfidenceIntervalUpperBounds, c(0.42527258, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x4$repeatedPValues, c(0.49999905, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedPValues, collapse = ", "), ")"))
	expect_equal(x4$finalStage, NA_integer_, label = paste0("c(", paste0(x4$finalStage, collapse = ", "), ")"))
	expect_equal(x4$finalPValues, c(NA_real_, NA_real_), label = paste0("c(", paste0(x4$finalPValues, collapse = ", "), ")"))
	expect_equal(x4$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_), label = paste0("c(", paste0(x4$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(x4$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_), label = paste0("c(", paste0(x4$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(x4$medianUnbiasedEstimates, c(NA_real_, NA_real_), label = paste0("c(", paste0(x4$medianUnbiasedEstimates, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x4), NA)))
	    expect_output(print(x4)$show())
	    invisible(capture.output(expect_error(summary(x4), NA)))
	    expect_output(summary(x4)$show())
	    x4CodeBased <- eval(parse(text = getObjectRCode(x4, stringWrapParagraphWidth = NULL)))
	    expect_equal(x4CodeBased$testActions, x4$testActions, tolerance = 1e-07)
	    expect_equal(x4CodeBased$conditionalRejectionProbabilities, x4$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x4CodeBased$conditionalPower, x4$conditionalPower, tolerance = 1e-07)
	    expect_equal(x4CodeBased$repeatedConfidenceIntervalLowerBounds, x4$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x4CodeBased$repeatedConfidenceIntervalUpperBounds, x4$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x4CodeBased$repeatedPValues, x4$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x4CodeBased$finalStage, x4$finalStage, tolerance = 1e-07)
	    expect_equal(x4CodeBased$finalPValues, x4$finalPValues, tolerance = 1e-07)
	    expect_equal(x4CodeBased$finalConfidenceIntervalLowerBounds, x4$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x4CodeBased$finalConfidenceIntervalUpperBounds, x4$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x4CodeBased$medianUnbiasedEstimates, x4$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_type(names(x4), "character")
	    df <- as.data.frame(x4)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x4)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getAnalysisResults' with a dataset of rates and without defining a design", {

	.skipTestIfDisabled()

	data <- getDataset(
	    n1 = c(10),
	    n2 = c(15),
	    events1 = c(8),
	    events2 = c(6)
	)

	# @refFS[Formula]{fs:testStatisticTwoRatesApproximation}
	# @refFS[Formula]{fs:pValuesTwoRatesAlternativeGreater}
	analysisResults1 <- getAnalysisResults(data, alpha = 0.02)

	## Comparison of the results of AnalysisResultsInverseNormal object 'analysisResults1' with expected results
	expect_equal(analysisResults1$pi1, 0.8, tolerance = 1e-07, label = paste0("c(", paste0(analysisResults1$pi1, collapse = ", "), ")"))
	expect_equal(analysisResults1$pi2, 0.4, tolerance = 1e-07, label = paste0("c(", paste0(analysisResults1$pi2, collapse = ", "), ")"))
	expect_equal(analysisResults1$testActions, "accept", label = paste0("c(", paste0(analysisResults1$testActions, collapse = ", "), ")"))
	expect_equal(analysisResults1$repeatedConfidenceIntervalLowerBounds, -0.016534109, tolerance = 1e-07, label = paste0("c(", paste0(analysisResults1$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(analysisResults1$repeatedConfidenceIntervalUpperBounds, 0.68698828, tolerance = 1e-07, label = paste0("c(", paste0(analysisResults1$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(analysisResults1$repeatedPValues, 0.024199112, tolerance = 1e-07, label = paste0("c(", paste0(analysisResults1$repeatedPValues, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(analysisResults1), NA)))
	    expect_output(print(analysisResults1)$show())
	    invisible(capture.output(expect_error(summary(analysisResults1), NA)))
	    expect_output(summary(analysisResults1)$show())
	    analysisResults1CodeBased <- eval(parse(text = getObjectRCode(analysisResults1, stringWrapParagraphWidth = NULL)))
	    expect_equal(analysisResults1CodeBased$pi1, analysisResults1$pi1, tolerance = 1e-07)
	    expect_equal(analysisResults1CodeBased$pi2, analysisResults1$pi2, tolerance = 1e-07)
	    expect_equal(analysisResults1CodeBased$testActions, analysisResults1$testActions, tolerance = 1e-07)
	    expect_equal(analysisResults1CodeBased$repeatedConfidenceIntervalLowerBounds, analysisResults1$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(analysisResults1CodeBased$repeatedConfidenceIntervalUpperBounds, analysisResults1$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(analysisResults1CodeBased$repeatedPValues, analysisResults1$repeatedPValues, tolerance = 1e-07)
	    expect_type(names(analysisResults1), "character")
	    df <- as.data.frame(analysisResults1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(analysisResults1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getAnalysisResults' produces the correct critical values for a boundary recalculation at the last stage", {

	.skipTestIfDisabled()

	data1 <- getDataset(
	    overallN = c(22, 33, 45),
	    overallEvents = c(11, 18, 28)
	)
	data2 <- getDataset(
	    overallN = c(22, 33, 40),
	    overallEvents = c(11, 18, 23)
	)
	data3 <- getDataset(
	    overallN = c(22, 33, 38),
	    overallEvents = c(11, 18, 21)
	)
	design <- getDesignGroupSequential(
	    typeOfDesign = "asP"
	)

	# @refFS[Formula]{fs:getAnalysisResults:maxInformation}
	# @refFS[Formula]{fs:getAnalysisResults:maxInformation:methods}
	expect_warning(result1 <- getAnalysisResults(design, data1,
	    thetaH0 = 0.5, maxInformation = 40
	))
	result2 <- getAnalysisResults(design, data2,
	    thetaH0 = 0.5, maxInformation = 40
	)
	expect_warning(result3 <- getAnalysisResults(design, data3,
	    thetaH0 = 0.5, maxInformation = 40, informationEpsilon = 2
	))
	expect_equal(result1$.design$criticalValues[1:2], result2$.design$criticalValues[1:2], tolerance = 1e-07)
	expect_equal(result1$.design$criticalValues[1:2], result3$.design$criticalValues[1:2], tolerance = 1e-07)
	expect_equal(result2$.design$criticalValues[1:2], result3$.design$criticalValues[1:2], tolerance = 1e-07)
})

