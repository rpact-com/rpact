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
## |  File name: test-f_analysis_enrichment_rates.R
## |  Creation date: 08 November 2023, 08:54:54
## |  File version: $Revision$
## |  Last changed: $Date$
## |  Last changed by: $Author$
## |  

test_plan_section("Testing Analysis Enrichment Rates Function")


test_that("'getAnalysisResults': enrichment rates, one sub-population, non-stratified input, select S1 at second IA, directionUpper = FALSE, gMax = 2", {
	.skipTestIfDisabled()

	# @refFS[Formula]{fs:adjustedPValueBonferroniEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimesEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCISidakEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCISpiessensEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSidakEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSimesEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSpiessensDeboisEnrichmentRates}
	# @refFS[Formula]{fs:computeRCIsEnrichment}
	# @refFS[Formula]{fs:conditionalPowerEnrichment}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityEnrichment}
	# @refFS[Formula]{fs:stratifiedTestEnrichmentRates}
	# @refFS[Formula]{fs:testStatisticEnrichmentRates}
	design1 <- getDesignInverseNormal(kMax = 3, alpha = 0.02, typeOfDesign = "P", informationRates = c(0.4, 0.7, 1))

	S1 <- getDataset(
	    sampleSize1 = c(22, 31, 37),
	    sampleSize2 = c(28, 33, 39),
	    events1     = c(7, 16, 17),
	    events2     = c(18, 21, 19)
	)

	F <- getDataset(
	    sampleSize1 = c(46, 54, NA),
	    sampleSize2 = c(49, 62, NA),
	    events1     = c(16, 31, NA),
	    events2     = c(29, 35, NA)
	)

	dataInput1 <- getDataset(S1 = S1, F = F)

	## Comparison of the results of DatasetRates object 'dataInput1' with expected results
	expect_equal(dataInput1$overallSampleSizes, c(22, 46, 28, 49, 53, 100, 61, 111, 90, NA_real_, 100, NA_real_), label = paste0("c(", paste0(dataInput1$overallSampleSizes, collapse = ", "), ")"))
	expect_equal(dataInput1$overallEvents, c(7, 16, 18, 29, 23, 47, 39, 64, 40, NA_real_, 58, NA_real_), label = paste0("c(", paste0(dataInput1$overallEvents, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(dataInput1), NA)))
	    expect_output(print(dataInput1)$show())
	    invisible(capture.output(expect_error(summary(dataInput1), NA)))
	    expect_output(summary(dataInput1)$show())
	    dataInput1CodeBased <- eval(parse(text = getObjectRCode(dataInput1, stringWrapParagraphWidth = NULL)))
	    expect_equal(dataInput1CodeBased$overallSampleSizes, dataInput1$overallSampleSizes, tolerance = 1e-07)
	    expect_equal(dataInput1CodeBased$overallEvents, dataInput1$overallEvents, tolerance = 1e-07)
	    expect_type(names(dataInput1), "character")
	    df <- as.data.frame(dataInput1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(dataInput1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x1 <- getAnalysisResults(design1, dataInput1,
	    stratifiedAnalysis = FALSE,
	    intersectionTest = "SpiessensDebois",
	    allocationRatioPlanned = 0.5,
	    directionUpper = FALSE,
	    normalApproximation = TRUE,
	    stage = 2,
	    nPlanned = c(80)
	)

	## Comparison of the results of AnalysisResultsEnrichmentInverseNormal object 'x1' with expected results
	expect_equal(x1$piTreatments[1, ], 0.43396226, tolerance = 1e-07, label = paste0("c(", paste0(x1$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(x1$piTreatments[2, ], 0.47, tolerance = 1e-07, label = paste0("c(", paste0(x1$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(x1$conditionalRejectionProbabilities[1, ], c(0.17935289, 0.13861558, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(x1$conditionalRejectionProbabilities[2, ], c(0.17935289, 0.047432959, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(x1$conditionalPower[1, ], c(NA_real_, NA_real_, 0.74825773), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(x1$conditionalPower[2, ], c(NA_real_, NA_real_, 0.22069678), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.61149604, -0.44933531, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.47492191, -0.29772839, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds[1, ], c(0.040176497, 0.029772743, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds[2, ], c(0.018732449, 0.06513775, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedPValues[1, ], c(0.031827909, 0.031827909, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedPValues[2, ], c(0.031827909, 0.031827909, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedPValues[2, ], collapse = ", "), ")"))
	expect_equal(x1$piControls[1, ], 0.63934426, tolerance = 1e-07, label = paste0("c(", paste0(x1$piControls[1, ], collapse = ", "), ")"))
	expect_equal(x1$piControls[2, ], 0.57657658, tolerance = 1e-07, label = paste0("c(", paste0(x1$piControls[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x1), NA)))
	    expect_output(print(x1)$show())
	    invisible(capture.output(expect_error(summary(x1), NA)))
	    expect_output(summary(x1)$show())
	    x1CodeBased <- eval(parse(text = getObjectRCode(x1, stringWrapParagraphWidth = NULL)))
	    expect_equal(x1CodeBased$piTreatments, x1$piTreatments, tolerance = 1e-07)
	    expect_equal(x1CodeBased$conditionalRejectionProbabilities, x1$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x1CodeBased$conditionalPower, x1$conditionalPower, tolerance = 1e-07)
	    expect_equal(x1CodeBased$repeatedConfidenceIntervalLowerBounds, x1$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$repeatedConfidenceIntervalUpperBounds, x1$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$repeatedPValues, x1$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x1CodeBased$piControls, x1$piControls, tolerance = 1e-07)
	    expect_type(names(x1), "character")
	    df <- as.data.frame(x1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	.skipTestIfDisabled()

	x2 <- getAnalysisResults(design1, dataInput1,
	    stratifiedAnalysis = FALSE,
	    intersectionTest = "Bonferroni",
	    allocationRatioPlanned = 0.5,
	    directionUpper = FALSE,
	    normalApproximation = TRUE,
	    stage = 2,
	    nPlanned = c(80)
	)

	## Comparison of the results of AnalysisResultsEnrichmentInverseNormal object 'x2' with expected results
	expect_equal(x2$piTreatments[1, ], 0.43396226, tolerance = 1e-07, label = paste0("c(", paste0(x2$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(x2$piTreatments[2, ], 0.47, tolerance = 1e-07, label = paste0("c(", paste0(x2$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(x2$conditionalRejectionProbabilities[1, ], c(0.16289564, 0.075460476, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(x2$conditionalRejectionProbabilities[2, ], c(0.16289564, 0.047432959, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(x2$conditionalPower[1, ], c(NA_real_, NA_real_, 0.62405214), tolerance = 1e-07, label = paste0("c(", paste0(x2$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(x2$conditionalPower[2, ], c(NA_real_, NA_real_, 0.22069678), tolerance = 1e-07, label = paste0("c(", paste0(x2$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.61554799, -0.46343398, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.47860086, -0.31516617, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds[1, ], c(0.046721667, 0.044120395, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds[2, ], c(0.02350445, 0.081574104, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedPValues[1, ], c(0.036684009, 0.036684009, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedPValues[2, ], c(0.036684009, 0.036684009, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedPValues[2, ], collapse = ", "), ")"))
	expect_equal(x2$piControls[1, ], 0.63934426, tolerance = 1e-07, label = paste0("c(", paste0(x2$piControls[1, ], collapse = ", "), ")"))
	expect_equal(x2$piControls[2, ], 0.57657658, tolerance = 1e-07, label = paste0("c(", paste0(x2$piControls[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x2), NA)))
	    expect_output(print(x2)$show())
	    invisible(capture.output(expect_error(summary(x2), NA)))
	    expect_output(summary(x2)$show())
	    x2CodeBased <- eval(parse(text = getObjectRCode(x2, stringWrapParagraphWidth = NULL)))
	    expect_equal(x2CodeBased$piTreatments, x2$piTreatments, tolerance = 1e-07)
	    expect_equal(x2CodeBased$conditionalRejectionProbabilities, x2$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x2CodeBased$conditionalPower, x2$conditionalPower, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedConfidenceIntervalLowerBounds, x2$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedConfidenceIntervalUpperBounds, x2$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedPValues, x2$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x2CodeBased$piControls, x2$piControls, tolerance = 1e-07)
	    expect_type(names(x2), "character")
	    df <- as.data.frame(x2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getAnalysisResults': enrichment rates, one sub-population, stratified input, select S1 at second IA, directionUpper = FALSE, gMax = 2", {

	.skipTestIfDisabled()

	# @refFS[Formula]{fs:adjustedPValueBonferroniEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimesEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCISidakEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCISpiessensEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSidakEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSimesEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSpiessensDeboisEnrichmentRates}
	# @refFS[Formula]{fs:computeRCIsEnrichment}
	# @refFS[Formula]{fs:conditionalPowerEnrichment}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityEnrichment}
	# @refFS[Formula]{fs:stratifiedTestEnrichmentRates}
	# @refFS[Formula]{fs:testStatisticEnrichmentRates}
	design1 <- getDesignInverseNormal(kMax = 3, alpha = 0.05, typeOfDesign = "WT", deltaWT = 0.1, informationRates = c(0.4, 0.7, 1))

	S1 <- getDataset(
	    sampleSize1 = c(22, 31, 37),
	    sampleSize2 = c(28, 33, 39),
	    events1     = c(7, 16, 10),
	    events2     = c(18, 21, 19)
	)

	R <- getDataset(
	    sampleSize1 = c(24, 23, NA),
	    sampleSize2 = c(21, 29, NA),
	    events1     = c(9, 15, NA),
	    events2     = c(11, 14, NA)
	)

	dataInput2 <- getDataset(S1 = S1, R = R)

	## Comparison of the results of DatasetRates object 'dataInput2' with expected results
	expect_equal(dataInput2$overallSampleSizes, c(22, 24, 28, 21, 53, 47, 61, 50, 90, NA_real_, 100, NA_real_), label = paste0("c(", paste0(dataInput2$overallSampleSizes, collapse = ", "), ")"))
	expect_equal(dataInput2$overallEvents, c(7, 9, 18, 11, 23, 24, 39, 25, 33, NA_real_, 58, NA_real_), label = paste0("c(", paste0(dataInput2$overallEvents, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(dataInput2), NA)))
	    expect_output(print(dataInput2)$show())
	    invisible(capture.output(expect_error(summary(dataInput2), NA)))
	    expect_output(summary(dataInput2)$show())
	    dataInput2CodeBased <- eval(parse(text = getObjectRCode(dataInput2, stringWrapParagraphWidth = NULL)))
	    expect_equal(dataInput2CodeBased$overallSampleSizes, dataInput2$overallSampleSizes, tolerance = 1e-07)
	    expect_equal(dataInput2CodeBased$overallEvents, dataInput2$overallEvents, tolerance = 1e-07)
	    expect_type(names(dataInput2), "character")
	    df <- as.data.frame(dataInput2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(dataInput2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x3 <- getAnalysisResults(design1, dataInput2,
	    stratifiedAnalysis = FALSE,
	    intersectionTest = "Simes",
	    directionUpper = FALSE,
	    normalApproximation = FALSE
	)

	## Comparison of the results of AnalysisResultsEnrichmentInverseNormal object 'x3' with expected results
	expect_equal(x3$piTreatments[1, ], 0.36666667, tolerance = 1e-07, label = paste0("c(", paste0(x3$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(x3$piTreatments[2, ], NA_real_, label = paste0("c(", paste0(x3$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(x3$conditionalRejectionProbabilities[1, ], c(0.34476337, 0.21123906, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(x3$conditionalRejectionProbabilities[2, ], c(0.34476337, 0.16889178, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(x3$conditionalPower[1, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(x3$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.62776669, -0.44175544, -0.38366304), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.4897991, -0.29886557, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds[1, ], c(0.066751342, 0.016446892, -0.050014598), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds[2, ], c(0.038157503, 0.063536395, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedPValues[1, ], c(0.10653002, 0.10653002, 0.014413851), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedPValues[2, ], c(0.10653002, 0.10653002, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedPValues[2, ], collapse = ", "), ")"))
	expect_equal(x3$piControls[1, ], 0.58, tolerance = 1e-07, label = paste0("c(", paste0(x3$piControls[1, ], collapse = ", "), ")"))
	expect_equal(x3$piControls[2, ], NA_real_, label = paste0("c(", paste0(x3$piControls[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x3), NA)))
	    expect_output(print(x3)$show())
	    invisible(capture.output(expect_error(summary(x3), NA)))
	    expect_output(summary(x3)$show())
	    x3CodeBased <- eval(parse(text = getObjectRCode(x3, stringWrapParagraphWidth = NULL)))
	    expect_equal(x3CodeBased$piTreatments, x3$piTreatments, tolerance = 1e-07)
	    expect_equal(x3CodeBased$conditionalRejectionProbabilities, x3$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x3CodeBased$conditionalPower, x3$conditionalPower, tolerance = 1e-07)
	    expect_equal(x3CodeBased$repeatedConfidenceIntervalLowerBounds, x3$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x3CodeBased$repeatedConfidenceIntervalUpperBounds, x3$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x3CodeBased$repeatedPValues, x3$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x3CodeBased$piControls, x3$piControls, tolerance = 1e-07)
	    expect_type(names(x3), "character")
	    df <- as.data.frame(x3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x4 <- getAnalysisResults(design1, dataInput2,
	    stratifiedAnalysis = TRUE,
	    intersectionTest = "Simes",
	    directionUpper = FALSE,
	    normalApproximation = TRUE
	)

	## Comparison of the results of AnalysisResultsEnrichmentInverseNormal object 'x4' with expected results
	expect_equal(x4$piTreatments[1, ], 0.36666667, tolerance = 1e-07, label = paste0("c(", paste0(x4$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(x4$piTreatments[2, ], NA_real_, label = paste0("c(", paste0(x4$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(x4$conditionalRejectionProbabilities[1, ], c(0.4519333, 0.45336181, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(x4$conditionalRejectionProbabilities[2, ], c(0.4519333, 0.2823056, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(x4$conditionalPower[1, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x4$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(x4$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x4$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(x4$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.62776669, -0.44175544, -0.38366304), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(x4$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.48811625, -0.29740945, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(x4$repeatedConfidenceIntervalUpperBounds[1, ], c(0.066751342, 0.016446892, -0.050014598), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(x4$repeatedConfidenceIntervalUpperBounds[2, ], c(0.041874626, 0.06452777, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(x4$repeatedPValues[1, ], c(0.07212343, 0.050354903, 0.0033350387), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(x4$repeatedPValues[2, ], c(0.07212343, 0.065501128, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedPValues[2, ], collapse = ", "), ")"))
	expect_equal(x4$piControls[1, ], 0.58, tolerance = 1e-07, label = paste0("c(", paste0(x4$piControls[1, ], collapse = ", "), ")"))
	expect_equal(x4$piControls[2, ], NA_real_, label = paste0("c(", paste0(x4$piControls[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x4), NA)))
	    expect_output(print(x4)$show())
	    invisible(capture.output(expect_error(summary(x4), NA)))
	    expect_output(summary(x4)$show())
	    x4CodeBased <- eval(parse(text = getObjectRCode(x4, stringWrapParagraphWidth = NULL)))
	    expect_equal(x4CodeBased$piTreatments, x4$piTreatments, tolerance = 1e-07)
	    expect_equal(x4CodeBased$conditionalRejectionProbabilities, x4$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x4CodeBased$conditionalPower, x4$conditionalPower, tolerance = 1e-07)
	    expect_equal(x4CodeBased$repeatedConfidenceIntervalLowerBounds, x4$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x4CodeBased$repeatedConfidenceIntervalUpperBounds, x4$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x4CodeBased$repeatedPValues, x4$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x4CodeBased$piControls, x4$piControls, tolerance = 1e-07)
	    expect_type(names(x4), "character")
	    df <- as.data.frame(x4)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x4)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getAnalysisResults': enrichment rates, more sub-populations, select S1 and S2 at first IA, select S1 at second, directionUpper = TRUE, gMax = 3", {

	.skipTestIfDisabled()

	# @refFS[Formula]{fs:adjustedPValueBonferroniEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimesEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCISidakEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCISpiessensEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSidakEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSimesEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSpiessensDeboisEnrichmentRates}
	# @refFS[Formula]{fs:computeRCIsEnrichment}
	# @refFS[Formula]{fs:conditionalPowerEnrichment}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityEnrichment}
	# @refFS[Formula]{fs:stratifiedTestEnrichmentRates}
	# @refFS[Formula]{fs:testStatisticEnrichmentRates}
	S1 <- getDataset(
	    sampleSize1 = c(47, 33, 37),
	    sampleSize2 = c(48, 47, 39),
	    events1     = c(18, 13, 17),
	    events2     = c(12, 11, 9)
	)

	S2 <- getDataset(
	    sampleSize1 = c(49, NA, NA),
	    sampleSize2 = c(45, NA, NA),
	    events1     = c(12, NA, NA),
	    events2     = c(13, NA, NA)
	)

	S12 <- getDataset(
	    sampleSize1 = c(35, 42, NA),
	    sampleSize2 = c(36, 47, NA),
	    events1     = c(19, 10, NA),
	    events2     = c(13, 17, NA)
	)

	R <- getDataset(
	    sampleSize1 = c(43, NA, NA),
	    sampleSize2 = c(39, NA, NA),
	    events1     = c(17, NA, NA),
	    events2     = c(14, NA, NA)
	)

	dataInput3 <- getDataset(S1 = S1, S2 = S2, S12 = S12, R = R)

	## Comparison of the results of DatasetRates object 'dataInput3' with expected results
	expect_equal(dataInput3$overallSampleSizes, c(47, 49, 35, 43, 48, 45, 36, 39, 80, NA_real_, 77, NA_real_, 95, NA_real_, 83, NA_real_, 117, NA_real_, NA_real_, NA_real_, 134, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(dataInput3$overallSampleSizes, collapse = ", "), ")"))
	expect_equal(dataInput3$overallEvents, c(18, 12, 19, 17, 12, 13, 13, 14, 31, NA_real_, 29, NA_real_, 23, NA_real_, 30, NA_real_, 48, NA_real_, NA_real_, NA_real_, 32, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(dataInput3$overallEvents, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(dataInput3), NA)))
	    expect_output(print(dataInput3)$show())
	    invisible(capture.output(expect_error(summary(dataInput3), NA)))
	    expect_output(summary(dataInput3)$show())
	    dataInput3CodeBased <- eval(parse(text = getObjectRCode(dataInput3, stringWrapParagraphWidth = NULL)))
	    expect_equal(dataInput3CodeBased$overallSampleSizes, dataInput3$overallSampleSizes, tolerance = 1e-07)
	    expect_equal(dataInput3CodeBased$overallEvents, dataInput3$overallEvents, tolerance = 1e-07)
	    expect_type(names(dataInput3), "character")
	    df <- as.data.frame(dataInput3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(dataInput3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	design1 <- getDesignInverseNormal(kMax = 3, alpha = 0.05, typeOfDesign = "WT", deltaWT = 0.1, informationRates = c(0.4, 0.7, 1))

	x1 <- getAnalysisResults(design1, dataInput3,
	    directionUpper = TRUE,
	    stratifiedAnalysis = FALSE,
	    intersectionTest = "Sidak",
	    allocationRatioPlanned = 3,
	    normalApproximation = FALSE,
	    nPlanned = c(80),
	    piControls = c(0.2, NA, NA),
	    piTreatments = c(0.55, NA, NA),
	    stage = 2
	)

	## Comparison of the results of AnalysisResultsEnrichmentInverseNormal object 'x1' with expected results
	expect_equal(x1$conditionalRejectionProbabilities[1, ], c(0.15297113, 0.049132584, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(x1$conditionalRejectionProbabilities[2, ], c(0.034063149, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(x1$conditionalRejectionProbabilities[3, ], c(0.064895921, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
	expect_equal(x1$conditionalPower[1, ], c(NA_real_, NA_real_, 0.89354539), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(x1$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(x1$conditionalPower[3, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$conditionalPower[3, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.062823383, -0.036086154, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.16425035, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds[3, ], c(-0.078510197, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds[1, ], c(0.35743976, 0.21982839, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds[2, ], c(0.25557989, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds[3, ], c(0.21491638, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedPValues[1, ], c(0.23298603, 0.23298603, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedPValues[2, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedPValues[3, ], c(0.389024, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedPValues[3, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x1), NA)))
	    expect_output(print(x1)$show())
	    invisible(capture.output(expect_error(summary(x1), NA)))
	    expect_output(summary(x1)$show())
	    x1CodeBased <- eval(parse(text = getObjectRCode(x1, stringWrapParagraphWidth = NULL)))
	    expect_equal(x1CodeBased$conditionalRejectionProbabilities, x1$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x1CodeBased$conditionalPower, x1$conditionalPower, tolerance = 1e-07)
	    expect_equal(x1CodeBased$repeatedConfidenceIntervalLowerBounds, x1$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$repeatedConfidenceIntervalUpperBounds, x1$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x1CodeBased$repeatedPValues, x1$repeatedPValues, tolerance = 1e-07)
	    expect_type(names(x1), "character")
	    df <- as.data.frame(x1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	design2 <- getDesignFisher(kMax = 3, method = "equalAlpha", alpha = 0.05, informationRates = c(0.4, 0.7, 1))

	x2 <- getAnalysisResults(design2, dataInput3,
	    directionUpper = TRUE,
	    stratifiedAnalysis = FALSE,
	    intersectionTest = "Sidak",
	    normalApproximation = FALSE,
	    stage = 3
	)

	## Comparison of the results of AnalysisResultsEnrichmentFisher object 'x2' with expected results
	expect_equal(x2$piTreatments[1, ], 0.41025641, tolerance = 1e-07, label = paste0("c(", paste0(x2$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(x2$piTreatments[2, ], NA_real_, label = paste0("c(", paste0(x2$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(x2$piTreatments[3, ], NA_real_, label = paste0("c(", paste0(x2$piTreatments[3, ], collapse = ", "), ")"))
	expect_equal(x2$conditionalRejectionProbabilities[1, ], c(0.075105953, 0.018243594, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(x2$conditionalRejectionProbabilities[2, ], c(0.020009021, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(x2$conditionalRejectionProbabilities[3, ], c(0.031471245, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
	expect_equal(x2$conditionalPower[1, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(x2$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(x2$conditionalPower[3, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$conditionalPower[3, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.023654531, -0.034180226, 0.008300518), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.12625532, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds[3, ], c(-0.051634044, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds[1, ], c(0.32239366, 0.19556, 0.21299371), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds[2, ], c(0.21912956, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds[3, ], c(0.1890798, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedPValues[1, ], c(0.14811777, 0.14811777, 0.07171335), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedPValues[2, ], c(0.46979052, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedPValues[2, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedPValues[3, ], c(0.32146776, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedPValues[3, ], collapse = ", "), ")"))
	expect_equal(x2$piControls[1, ], 0.23880597, tolerance = 1e-07, label = paste0("c(", paste0(x2$piControls[1, ], collapse = ", "), ")"))
	expect_equal(x2$piControls[2, ], NA_real_, label = paste0("c(", paste0(x2$piControls[2, ], collapse = ", "), ")"))
	expect_equal(x2$piControls[3, ], NA_real_, label = paste0("c(", paste0(x2$piControls[3, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x2), NA)))
	    expect_output(print(x2)$show())
	    invisible(capture.output(expect_error(summary(x2), NA)))
	    expect_output(summary(x2)$show())
	    x2CodeBased <- eval(parse(text = getObjectRCode(x2, stringWrapParagraphWidth = NULL)))
	    expect_equal(x2CodeBased$piTreatments, x2$piTreatments, tolerance = 1e-07)
	    expect_equal(x2CodeBased$conditionalRejectionProbabilities, x2$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x2CodeBased$conditionalPower, x2$conditionalPower, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedConfidenceIntervalLowerBounds, x2$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedConfidenceIntervalUpperBounds, x2$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedPValues, x2$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x2CodeBased$piControls, x2$piControls, tolerance = 1e-07)
	    expect_type(names(x2), "character")
	    df <- as.data.frame(x2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getAnalysisResults': enrichment rates, more sub-populations, non-stratified input, select S1 and S2 at first IA, select S1 at second, directionUpper = FALSE, gMax = 4", {

	.skipTestIfDisabled()

	# @refFS[Formula]{fs:adjustedPValueBonferroniEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimesEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCISidakEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCISpiessensEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSidakEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSimesEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSpiessensDeboisEnrichmentRates}
	# @refFS[Formula]{fs:computeRCIsEnrichment}
	# @refFS[Formula]{fs:conditionalPowerEnrichment}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityEnrichment}
	# @refFS[Formula]{fs:stratifiedTestEnrichmentRates}
	# @refFS[Formula]{fs:testStatisticEnrichmentRates}
	S1 <- getDataset(
	    sampleSize1 = c(84, 94, 25),
	    sampleSize2 = c(82, 75, 23),
	    events1     = c(21, 28, 13),
	    events2     = c(32, 23, 20)
	)

	S2 <- getDataset(
	    sampleSize1 = c(81, 95, NA),
	    sampleSize2 = c(84, 64, NA),
	    events1     = c(26, 29, NA),
	    events2     = c(31, 26, NA)
	)

	S3 <- getDataset(
	    sampleSize1 = c(71, NA, NA),
	    sampleSize2 = c(74, NA, NA),
	    events1     = c(16, NA, NA),
	    events2     = c(21, NA, NA)
	)

	F <- getDataset(
	    sampleSize1 = c(248, NA, NA),
	    sampleSize2 = c(254, NA, NA),
	    events1 = c(75, NA, NA),
	    events2 = c(98, NA, NA)
	)

	R <- getDataset(
	    sampleSize1 = c(12, NA, NA),
	    sampleSize2 = c(14, NA, NA),
	    events1 = c(12, NA, NA),
	    events2 = c(14, NA, NA)
	)

	dataInput4 <- getDataset(S1 = S1, S2 = S2, S3 = S3, F = F)

	## Comparison of the results of DatasetRates object 'dataInput4' with expected results
	expect_equal(dataInput4$overallSampleSizes, c(84, 81, 71, 248, 82, 84, 74, 254, 178, 176, NA_real_, NA_real_, 157, 148, NA_real_, NA_real_, 203, NA_real_, NA_real_, NA_real_, 180, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(dataInput4$overallSampleSizes, collapse = ", "), ")"))
	expect_equal(dataInput4$overallEvents, c(21, 26, 16, 75, 32, 31, 21, 98, 49, 55, NA_real_, NA_real_, 55, 57, NA_real_, NA_real_, 62, NA_real_, NA_real_, NA_real_, 75, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(dataInput4$overallEvents, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(dataInput4), NA)))
	    expect_output(print(dataInput4)$show())
	    invisible(capture.output(expect_error(summary(dataInput4), NA)))
	    expect_output(summary(dataInput4)$show())
	    dataInput4CodeBased <- eval(parse(text = getObjectRCode(dataInput4, stringWrapParagraphWidth = NULL)))
	    expect_equal(dataInput4CodeBased$overallSampleSizes, dataInput4$overallSampleSizes, tolerance = 1e-07)
	    expect_equal(dataInput4CodeBased$overallEvents, dataInput4$overallEvents, tolerance = 1e-07)
	    expect_type(names(dataInput4), "character")
	    df <- as.data.frame(dataInput4)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(dataInput4)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	design1 <- getDesignInverseNormal(kMax = 3, alpha = 0.05, typeOfDesign = "asKD", gammaA = 2, informationRates = c(0.4, 0.7, 1))

	x3 <- getAnalysisResults(design1, dataInput4,
	    directionUpper = FALSE,
	    stratifiedAnalysis = FALSE,
	    intersectionTest = "Sidak",
	    allocationRatioPlanned = 1,
	    stage = 3,
	    normalApproximation = TRUE
	)

	## Comparison of the results of AnalysisResultsEnrichmentInverseNormal object 'x3' with expected results
	expect_equal(x3$piTreatments[1, ], 0.30541872, tolerance = 1e-07, label = paste0("c(", paste0(x3$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(x3$piTreatments[2, ], NA_real_, label = paste0("c(", paste0(x3$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(x3$piTreatments[3, ], NA_real_, label = paste0("c(", paste0(x3$piTreatments[3, ], collapse = ", "), ")"))
	expect_equal(x3$piTreatments[4, ], NA_real_, label = paste0("c(", paste0(x3$piTreatments[4, ], collapse = ", "), ")"))
	expect_equal(x3$conditionalRejectionProbabilities[1, ], c(0.13745997, 0.082835151, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(x3$conditionalRejectionProbabilities[2, ], c(0.023915975, 0.064596491, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(x3$conditionalRejectionProbabilities[3, ], c(0.023915975, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
	expect_equal(x3$conditionalRejectionProbabilities[4, ], c(0.13745997, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$conditionalRejectionProbabilities[4, ], collapse = ", "), ")"))
	expect_equal(x3$conditionalPower[1, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(x3$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(x3$conditionalPower[3, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$conditionalPower[3, ], collapse = ", "), ")"))
	expect_equal(x3$conditionalPower[4, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$conditionalPower[4, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.33926099, -0.22469062, -0.248011), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.255132, -0.21555052, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds[3, ], c(-0.26390722, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds[4, ], c(-0.20314825, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalLowerBounds[4, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds[1, ], c(0.068268149, 0.059220127, -0.0081515662), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds[2, ], c(0.16378176, 0.07555087, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds[3, ], c(0.15232186, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds[4, ], c(0.038730826, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalUpperBounds[4, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedPValues[1, ], c(0.5, 0.26483774, 0.01063254), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedPValues[2, ], c(0.5, 0.30264322, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedPValues[2, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedPValues[3, ], c(0.5, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedPValues[3, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedPValues[4, ], c(0.5, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedPValues[4, ], collapse = ", "), ")"))
	expect_equal(x3$piControls[1, ], 0.41666667, tolerance = 1e-07, label = paste0("c(", paste0(x3$piControls[1, ], collapse = ", "), ")"))
	expect_equal(x3$piControls[2, ], NA_real_, label = paste0("c(", paste0(x3$piControls[2, ], collapse = ", "), ")"))
	expect_equal(x3$piControls[3, ], NA_real_, label = paste0("c(", paste0(x3$piControls[3, ], collapse = ", "), ")"))
	expect_equal(x3$piControls[4, ], NA_real_, label = paste0("c(", paste0(x3$piControls[4, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x3), NA)))
	    expect_output(print(x3)$show())
	    invisible(capture.output(expect_error(summary(x3), NA)))
	    expect_output(summary(x3)$show())
	    x3CodeBased <- eval(parse(text = getObjectRCode(x3, stringWrapParagraphWidth = NULL)))
	    expect_equal(x3CodeBased$piTreatments, x3$piTreatments, tolerance = 1e-07)
	    expect_equal(x3CodeBased$conditionalRejectionProbabilities, x3$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x3CodeBased$conditionalPower, x3$conditionalPower, tolerance = 1e-07)
	    expect_equal(x3CodeBased$repeatedConfidenceIntervalLowerBounds, x3$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x3CodeBased$repeatedConfidenceIntervalUpperBounds, x3$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x3CodeBased$repeatedPValues, x3$repeatedPValues, tolerance = 1e-07)
	    expect_equal(x3CodeBased$piControls, x3$piControls, tolerance = 1e-07)
	    expect_type(names(x3), "character")
	    df <- as.data.frame(x3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getAnalysisResults': enrichment rates, expected warning for empty subsets", {

	.skipTestIfDisabled()

	S1 <- getDataset(
	    sampleSize1 = c(84, 94, 25),
	    sampleSize2 = c(82, 75, 23),
	    events1     = c(21, 28, 13),
	    events2     = c(32, 23, 20)
	)

	S2 <- getDataset(
	    sampleSize1 = c(81, 95, NA),
	    sampleSize2 = c(84, 64, NA),
	    events1     = c(26, 29, NA),
	    events2     = c(31, 26, NA)
	)

	S3 <- getDataset(
	    sampleSize1 = c(71, NA, NA),
	    sampleSize2 = c(74, NA, NA),
	    events1     = c(16, NA, NA),
	    events2     = c(21, NA, NA)
	)

	R <- getDataset(
	    sampleSize1 = c(12, NA, NA),
	    sampleSize2 = c(14, NA, NA),
	    events1 = c(12, NA, NA),
	    events2 = c(14, NA, NA)
	)

	expect_warning(getDataset(S1 = S1, S2 = S2, S3 = S3, R = R),
	    "The 4 undefined subsets S12, S13, S23, S123 were defined as empty subsets",
	    fixed = TRUE
	)

})

