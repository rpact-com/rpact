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
## |  File name: test-f_analysis_enrichment_means.R
## |  Creation date: 08 November 2023, 08:53:05
## |  File version: $Revision: 7560 $
## |  Last changed: $Date: 2024-01-15 14:20:32 +0100 (Mo, 15 Jan 2024) $
## |  Last changed by: $Author: pahlke $
## |  

test_plan_section("Testing Analysis Enrichment Means Function (one sub-population)")


test_that("'getAnalysisResults': select S1 at first IA, gMax = 2, inverse normal design", {
	.skipTestIfDisabled()

	# @refFS[Formula]{fs:adjustedPValueBonferroniEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimesEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCISidakEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCISpiessensEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSidakEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSimesEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSpiessensDeboisEnrichment}
	# @refFS[Formula]{fs:computeRCIsEnrichment}
	# @refFS[Formula]{fs:conditionalPowerEnrichment}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityEnrichment}
	# @refFS[Formula]{fs:stratifiedtTestEnrichment}
	S1 <- getDataset(
	    sampleSize1 = c(12, 21),
	    sampleSize2 = c(18, 21),
	    mean1 = c(107.7, 84.9),
	    mean2 = c(165.6, 195.9),
	    stDev1 = c(128.5, 139.5),
	    stDev2 = c(120.1, 185.0)
	)

	F <- getDataset(
	    sampleSize1 = c(26, NA),
	    sampleSize2 = c(29, NA),
	    mean1 = c(86.48462, NA),
	    mean2 = c(148.34138, NA),
	    stDev1 = c(129.1485, NA),
	    stDev2 = c(122.888, NA)
	)

	dataInput1 <- getDataset(S1 = S1, F = F)

	## Comparison of the results of DatasetMeans object 'dataInput1' with expected results
	expect_equal(dataInput1$overallSampleSizes, c(12, 26, 18, 29, 33, NA_real_, 39, NA_real_), label = paste0("c(", paste0(dataInput1$overallSampleSizes, collapse = ", "), ")"))
	expect_equal(dataInput1$overallMeans, c(107.7, 86.48462, 165.6, 148.34138, 93.190909, NA_real_, 181.91538, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(dataInput1$overallMeans, collapse = ", "), ")"))
	expect_equal(dataInput1$overallStDevs, c(128.5, 129.1485, 120.1, 122.888, 134.02535, NA_real_, 157.16289, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(dataInput1$overallStDevs, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(dataInput1), NA)))
	    expect_output(print(dataInput1)$show())
	    invisible(capture.output(expect_error(summary(dataInput1), NA)))
	    expect_output(summary(dataInput1)$show())
	    dataInput1CodeBased <- eval(parse(text = getObjectRCode(dataInput1, stringWrapParagraphWidth = NULL)))
	    expect_equal(dataInput1CodeBased$overallSampleSizes, dataInput1$overallSampleSizes, tolerance = 1e-07)
	    expect_equal(dataInput1CodeBased$overallMeans, dataInput1$overallMeans, tolerance = 1e-07)
	    expect_equal(dataInput1CodeBased$overallStDevs, dataInput1$overallStDevs, tolerance = 1e-07)
	    expect_type(names(dataInput1), "character")
	    df <- as.data.frame(dataInput1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(dataInput1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	design1 <- getDesignInverseNormal(
	    kMax = 3, alpha = 0.02, futilityBounds = c(-0.5, 0),
	    bindingFutility = FALSE, typeOfDesign = "OF", informationRates = c(0.5, 0.7, 1)
	)

	x1 <- getAnalysisResults(
	    design = design1, dataInput = dataInput1,
	    directionUpper = FALSE,
	    normalApproximation = FALSE,
	    varianceOption = "pooledFromFull",
	    intersectionTest = "Bonferroni",
	    stratifiedAnalysis = FALSE,
	    stage = 2,
	    thetaH1 = c(-30, NA),
	    assumedStDevs = c(88, NA),
	    nPlanned = c(30),
	    allocationRatioPlanned = 2
	)

	## Comparison of the results of AnalysisResultsEnrichmentInverseNormal object 'x1' with expected results
	expect_equal(x1$conditionalRejectionProbabilities[1, ], c(0.040655272, 0.29596348, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(x1$conditionalRejectionProbabilities[2, ], c(0.065736952, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(x1$conditionalPower[1, ], c(NA_real_, NA_real_, 0.6346437), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(x1$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds[1, ], c(-215.41406, -176.0794, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds[2, ], c(-176.00816, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds[1, ], c(99.614058, 24.117528, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds[2, ], c(52.294639, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedPValues[1, ], c(0.25380947, 0.041128123, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedPValues[2, ], c(0.19818652, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedPValues[2, ], collapse = ", "), ")"))
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

})

test_that("'getAnalysisResults': stratified analysis, select S1 at first IA, gMax = 2, Fisher design", {

	.skipTestIfDisabled()

	# @refFS[Formula]{fs:adjustedPValueBonferroniEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimesEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCISidakEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCISpiessensEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSidakEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSimesEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSpiessensDeboisEnrichment}
	# @refFS[Formula]{fs:computeRCIsEnrichment}
	# @refFS[Formula]{fs:conditionalPowerEnrichment}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityEnrichment}
	# @refFS[Formula]{fs:stratifiedtTestEnrichment}
	S1 <- getDataset(
	    sampleSize1 = c(12, 21),
	    sampleSize2 = c(18, 21),
	    mean1 = c(107.7, 84.9),
	    mean2 = c(165.6, 195.9),
	    stDev1 = c(128.5, 139.5),
	    stDev2 = c(120.1, 185.0)
	)

	R <- getDataset(
	    sampleSize1 = c(14, NA),
	    sampleSize2 = c(11, NA),
	    mean1 = c(68.3, NA),
	    mean2 = c(120.1, NA),
	    stDev1 = c(124.0, NA),
	    stDev2 = c(116.8, NA)
	)

	dataInput2 <- getDataset(S1 = S1, R = R)

	## Comparison of the results of DatasetMeans object 'dataInput2' with expected results
	expect_equal(dataInput2$overallSampleSizes, c(12, 14, 18, 11, 33, NA_real_, 39, NA_real_), label = paste0("c(", paste0(dataInput2$overallSampleSizes, collapse = ", "), ")"))
	expect_equal(dataInput2$overallMeans, c(107.7, 68.3, 165.6, 120.1, 93.190909, NA_real_, 181.91538, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(dataInput2$overallMeans, collapse = ", "), ")"))
	expect_equal(dataInput2$overallStDevs, c(128.5, 124, 120.1, 116.8, 134.02535, NA_real_, 157.16289, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(dataInput2$overallStDevs, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(dataInput2), NA)))
	    expect_output(print(dataInput2)$show())
	    invisible(capture.output(expect_error(summary(dataInput2), NA)))
	    expect_output(summary(dataInput2)$show())
	    dataInput2CodeBased <- eval(parse(text = getObjectRCode(dataInput2, stringWrapParagraphWidth = NULL)))
	    expect_equal(dataInput2CodeBased$overallSampleSizes, dataInput2$overallSampleSizes, tolerance = 1e-07)
	    expect_equal(dataInput2CodeBased$overallMeans, dataInput2$overallMeans, tolerance = 1e-07)
	    expect_equal(dataInput2CodeBased$overallStDevs, dataInput2$overallStDevs, tolerance = 1e-07)
	    expect_type(names(dataInput2), "character")
	    df <- as.data.frame(dataInput2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(dataInput2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	design2 <- getDesignFisher(
	    kMax = 3, alpha = 0.02, alpha0Vec = c(0.7, 0.5), method = "fullAlpha",
	    bindingFutility = TRUE, informationRates = c(0.3, 0.7, 1)
	)

	x2 <- getAnalysisResults(
	    design = design2, dataInput = dataInput2,
	    directionUpper = FALSE,
	    normalApproximation = FALSE,
	    varianceOption = "pooledFromFull",
	    intersectionTest = "Bonferroni",
	    stratifiedAnalysis = FALSE,
	    stage = 2,
	    thetaH1 = c(-30, NA),
	    assumedStDevs = c(88, NA),
	    nPlanned = c(30),
	    allocationRatioPlanned = 2
	)

	## Comparison of the results of AnalysisResultsEnrichmentFisher object 'x2' with expected results
	expect_equal(x2$conditionalRejectionProbabilities[1, ], c(0.030372979, 0.38266716, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(x2$conditionalRejectionProbabilities[2, ], c(0.042518986, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(x2$conditionalPower[1, ], c(NA_real_, NA_real_, 0.71962915), tolerance = 1e-07, label = paste0("c(", paste0(x2$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(x2$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds[1, ], c(-187.96966, -183.80634, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds[2, ], c(-156.27269, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds[1, ], c(72.16966, 16.133901, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds[2, ], c(32.559163, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedPValues[1, ], c(0.19557155, 0.034517266, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedPValues[2, ], c(0.13877083, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedPValues[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x2), NA)))
	    expect_output(print(x2)$show())
	    invisible(capture.output(expect_error(summary(x2), NA)))
	    expect_output(summary(x2)$show())
	    x2CodeBased <- eval(parse(text = getObjectRCode(x2, stringWrapParagraphWidth = NULL)))
	    expect_equal(x2CodeBased$conditionalRejectionProbabilities, x2$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x2CodeBased$conditionalPower, x2$conditionalPower, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedConfidenceIntervalLowerBounds, x2$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedConfidenceIntervalUpperBounds, x2$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedPValues, x2$repeatedPValues, tolerance = 1e-07)
	    expect_type(names(x2), "character")
	    df <- as.data.frame(x2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	design1 <- getDesignInverseNormal(
	    kMax = 3, alpha = 0.02, futilityBounds = c(-0.5, 0),
	    bindingFutility = FALSE, typeOfDesign = "OF", informationRates = c(0.5, 0.7, 1)
	)

	x3 <- getAnalysisResults(
	    design = design1, dataInput = dataInput2,
	    directionUpper = FALSE,
	    normalApproximation = FALSE,
	    intersectionTest = "Sidak",
	    stratifiedAnalysis = TRUE,
	    stage = 2,
	    thetaH1 = c(-30, NA),
	    assumedStDevs = c(88, NA),
	    nPlanned = c(30),
	    allocationRatioPlanned = 2
	)

	## Comparison of the results of AnalysisResultsEnrichmentInverseNormal object 'x3' with expected results
	expect_equal(x3$conditionalRejectionProbabilities[1, ], c(0.041603465, 0.30059767, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(x3$conditionalRejectionProbabilities[2, ], c(0.044887021, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(x3$conditionalPower[1, ], c(NA_real_, NA_real_, 0.63965664), tolerance = 1e-07, label = paste0("c(", paste0(x3$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(x3$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds[1, ], c(-220.28415, -176.85912, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds[2, ], c(-167.67059, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds[1, ], c(104.48415, 23.636689, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds[2, ], c(57.495741, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedPValues[1, ], c(0.25104477, 0.040430988, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedPValues[2, ], c(0.24199442, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedPValues[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x3), NA)))
	    expect_output(print(x3)$show())
	    invisible(capture.output(expect_error(summary(x3), NA)))
	    expect_output(summary(x3)$show())
	    x3CodeBased <- eval(parse(text = getObjectRCode(x3, stringWrapParagraphWidth = NULL)))
	    expect_equal(x3CodeBased$conditionalRejectionProbabilities, x3$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x3CodeBased$conditionalPower, x3$conditionalPower, tolerance = 1e-07)
	    expect_equal(x3CodeBased$repeatedConfidenceIntervalLowerBounds, x3$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x3CodeBased$repeatedConfidenceIntervalUpperBounds, x3$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x3CodeBased$repeatedPValues, x3$repeatedPValues, tolerance = 1e-07)
	    expect_type(names(x3), "character")
	    df <- as.data.frame(x3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getAnalysisResults': select S1 at first IA, gMax = 2, inverse normal design, Sidak and Spiessens & Debois", {

	.skipTestIfDisabled()

	# @refFS[Formula]{fs:adjustedPValueBonferroniEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimesEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCISidakEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCISpiessensEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSidakEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSimesEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSpiessensDeboisEnrichment}
	# @refFS[Formula]{fs:computeRCIsEnrichment}
	# @refFS[Formula]{fs:conditionalPowerEnrichment}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityEnrichment}
	# @refFS[Formula]{fs:stratifiedtTestEnrichment}
	design1 <- getDesignInverseNormal(
	    kMax = 3, alpha = 0.02, futilityBounds = c(-0.5, 0),
	    bindingFutility = FALSE, typeOfDesign = "OF", informationRates = c(0.5, 0.7, 1)
	)

	S1 <- getDataset(
	    sampleSize1 = c(12, 21),
	    sampleSize2 = c(18, 21),
	    mean1 = c(107.7, 84.9),
	    mean2 = c(165.6, 195.9),
	    stDev1 = c(128.5, 139.5),
	    stDev2 = c(120.1, 185.0)
	)

	F <- getDataset(
	    sampleSize1 = c(26, NA),
	    sampleSize2 = c(29, NA),
	    mean1 = c(86.48462, NA),
	    mean2 = c(148.34138, NA),
	    stDev1 = c(129.1485, NA),
	    stDev2 = c(122.888, NA)
	)

	dataInput1 <- getDataset(S1 = S1, F = F)

	x4 <- getAnalysisResults(
	    design = design1, dataInput = dataInput1,
	    directionUpper = FALSE,
	    normalApproximation = FALSE,
	    varianceOption = "notPooled",
	    intersectionTest = "Sidak",
	    stratifiedAnalysis = FALSE,
	    stage = 2,
	    nPlanned = c(30),
	    allocationRatioPlanned = 2
	)

	## Comparison of the results of AnalysisResultsEnrichmentInverseNormal object 'x4' with expected results
	expect_equal(x4$thetaH1[1, ], -88.724476, tolerance = 1e-07, label = paste0("c(", paste0(x4$thetaH1[1, ], collapse = ", "), ")"))
	expect_equal(x4$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(x4$thetaH1[2, ], collapse = ", "), ")"))
	expect_equal(x4$assumedStDevs[1, ], 147.03819, tolerance = 1e-07, label = paste0("c(", paste0(x4$assumedStDevs[1, ], collapse = ", "), ")"))
	expect_equal(x4$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(x4$assumedStDevs[2, ], collapse = ", "), ")"))
	expect_equal(x4$conditionalRejectionProbabilities[1, ], c(0.039522227, 0.28885292, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(x4$conditionalRejectionProbabilities[2, ], c(0.066220149, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(x4$conditionalPower[1, ], c(NA_real_, NA_real_, 0.84164989), tolerance = 1e-07, label = paste0("c(", paste0(x4$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(x4$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x4$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(x4$repeatedConfidenceIntervalLowerBounds[1, ], c(-226.91549, -179.08628, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(x4$repeatedConfidenceIntervalLowerBounds[2, ], c(-176.48166, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(x4$repeatedConfidenceIntervalUpperBounds[1, ], c(111.11549, 25.050962, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(x4$repeatedConfidenceIntervalUpperBounds[2, ], c(52.768138, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(x4$repeatedPValues[1, ], c(0.25721122, 0.042227707, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(x4$repeatedPValues[2, ], c(0.1973759, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedPValues[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x4), NA)))
	    expect_output(print(x4)$show())
	    invisible(capture.output(expect_error(summary(x4), NA)))
	    expect_output(summary(x4)$show())
	    x4CodeBased <- eval(parse(text = getObjectRCode(x4, stringWrapParagraphWidth = NULL)))
	    expect_equal(x4CodeBased$thetaH1, x4$thetaH1, tolerance = 1e-07)
	    expect_equal(x4CodeBased$assumedStDevs, x4$assumedStDevs, tolerance = 1e-07)
	    expect_equal(x4CodeBased$conditionalRejectionProbabilities, x4$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x4CodeBased$conditionalPower, x4$conditionalPower, tolerance = 1e-07)
	    expect_equal(x4CodeBased$repeatedConfidenceIntervalLowerBounds, x4$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x4CodeBased$repeatedConfidenceIntervalUpperBounds, x4$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x4CodeBased$repeatedPValues, x4$repeatedPValues, tolerance = 1e-07)
	    expect_type(names(x4), "character")
	    df <- as.data.frame(x4)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x4)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x5 <- getAnalysisResults(
	    design = design1, dataInput = dataInput1,
	    directionUpper = FALSE,
	    normalApproximation = FALSE,
	    varianceOption = "pooledFromFull",
	    intersectionTest = "SpiessensDebois",
	    stratifiedAnalysis = TRUE,
	    stage = 2,
	    nPlanned = c(30),
	    allocationRatioPlanned = 2
	)

	## Comparison of the results of AnalysisResultsEnrichmentInverseNormal object 'x5' with expected results
	expect_equal(x5$thetaH1[1, ], -88.724476, tolerance = 1e-07, label = paste0("c(", paste0(x5$thetaH1[1, ], collapse = ", "), ")"))
	expect_equal(x5$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(x5$thetaH1[2, ], collapse = ", "), ")"))
	expect_equal(x5$assumedStDevs[1, ], 147.03819, tolerance = 1e-07, label = paste0("c(", paste0(x5$assumedStDevs[1, ], collapse = ", "), ")"))
	expect_equal(x5$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(x5$assumedStDevs[2, ], collapse = ", "), ")"))
	expect_equal(x5$conditionalRejectionProbabilities[1, ], c(0.039526191, 0.29036799, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x5$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(x5$conditionalRejectionProbabilities[2, ], c(0.083357636, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x5$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(x5$conditionalPower[1, ], c(NA_real_, NA_real_, 0.84271782), tolerance = 1e-07, label = paste0("c(", paste0(x5$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(x5$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x5$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(x5$repeatedConfidenceIntervalLowerBounds[1, ], c(-213.99088, -174.2069, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x5$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(x5$repeatedConfidenceIntervalLowerBounds[2, ], c(-174.97677, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x5$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(x5$repeatedConfidenceIntervalUpperBounds[1, ], c(98.190881, 20.342564, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x5$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(x5$repeatedConfidenceIntervalUpperBounds[2, ], c(51.263255, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x5$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(x5$repeatedPValues[1, ], c(0.25719977, 0.041990242, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x5$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(x5$repeatedPValues[2, ], c(0.17255372, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x5$repeatedPValues[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x5), NA)))
	    expect_output(print(x5)$show())
	    invisible(capture.output(expect_error(summary(x5), NA)))
	    expect_output(summary(x5)$show())
	    x5CodeBased <- eval(parse(text = getObjectRCode(x5, stringWrapParagraphWidth = NULL)))
	    expect_equal(x5CodeBased$thetaH1, x5$thetaH1, tolerance = 1e-07)
	    expect_equal(x5CodeBased$assumedStDevs, x5$assumedStDevs, tolerance = 1e-07)
	    expect_equal(x5CodeBased$conditionalRejectionProbabilities, x5$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x5CodeBased$conditionalPower, x5$conditionalPower, tolerance = 1e-07)
	    expect_equal(x5CodeBased$repeatedConfidenceIntervalLowerBounds, x5$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x5CodeBased$repeatedConfidenceIntervalUpperBounds, x5$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x5CodeBased$repeatedPValues, x5$repeatedPValues, tolerance = 1e-07)
	    expect_type(names(x5), "character")
	    df <- as.data.frame(x5)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x5)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x6 <- getAnalysisResults(
	    design = design1, dataInput = dataInput1,
	    directionUpper = FALSE,
	    normalApproximation = TRUE,
	    varianceOption = "notPooled",
	    intersectionTest = "SpiessensDebois",
	    stratifiedAnalysis = FALSE,
	    stage = 2,
	    nPlanned = c(30),
	    allocationRatioPlanned = 2
	)

	## Comparison of the results of AnalysisResultsEnrichmentInverseNormal object 'x6' with expected results
	expect_equal(x6$thetaH1[1, ], -88.724476, tolerance = 1e-07, label = paste0("c(", paste0(x6$thetaH1[1, ], collapse = ", "), ")"))
	expect_equal(x6$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(x6$thetaH1[2, ], collapse = ", "), ")"))
	expect_equal(x6$assumedStDevs[1, ], 147.03819, tolerance = 1e-07, label = paste0("c(", paste0(x6$assumedStDevs[1, ], collapse = ", "), ")"))
	expect_equal(x6$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(x6$assumedStDevs[2, ], collapse = ", "), ")"))
	expect_equal(x6$conditionalRejectionProbabilities[1, ], c(0.042609088, 0.32732548, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x6$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(x6$conditionalRejectionProbabilities[2, ], c(0.088609159, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x6$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(x6$conditionalPower[1, ], c(NA_real_, NA_real_, 0.86664918), tolerance = 1e-07, label = paste0("c(", paste0(x6$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(x6$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x6$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(x6$repeatedConfidenceIntervalLowerBounds[1, ], c(-205.06578, -171.09275, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x6$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(x6$repeatedConfidenceIntervalLowerBounds[2, ], c(-169.37758, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x6$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(x6$repeatedConfidenceIntervalUpperBounds[1, ], c(89.26578, 17.032517, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x6$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(x6$repeatedConfidenceIntervalUpperBounds[2, ], c(45.664059, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x6$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(x6$repeatedPValues[1, ], c(0.24818852, 0.036684963, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x6$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(x6$repeatedPValues[2, ], c(0.16618986, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x6$repeatedPValues[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x6), NA)))
	    expect_output(print(x6)$show())
	    invisible(capture.output(expect_error(summary(x6), NA)))
	    expect_output(summary(x6)$show())
	    x6CodeBased <- eval(parse(text = getObjectRCode(x6, stringWrapParagraphWidth = NULL)))
	    expect_equal(x6CodeBased$thetaH1, x6$thetaH1, tolerance = 1e-07)
	    expect_equal(x6CodeBased$assumedStDevs, x6$assumedStDevs, tolerance = 1e-07)
	    expect_equal(x6CodeBased$conditionalRejectionProbabilities, x6$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x6CodeBased$conditionalPower, x6$conditionalPower, tolerance = 1e-07)
	    expect_equal(x6CodeBased$repeatedConfidenceIntervalLowerBounds, x6$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x6CodeBased$repeatedConfidenceIntervalUpperBounds, x6$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x6CodeBased$repeatedPValues, x6$repeatedPValues, tolerance = 1e-07)
	    expect_type(names(x6), "character")
	    df <- as.data.frame(x6)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x6)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getAnalysisResults': select S1 at first IA, gMax = 2, Fisher design, Sidak and Bonferroni", {

	.skipTestIfDisabled()

	# @refFS[Formula]{fs:adjustedPValueBonferroniEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimesEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCISidakEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCISpiessensEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSidakEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSimesEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSpiessensDeboisEnrichment}
	# @refFS[Formula]{fs:computeRCIsEnrichment}
	# @refFS[Formula]{fs:conditionalPowerEnrichment}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityEnrichment}
	# @refFS[Formula]{fs:stratifiedtTestEnrichment}
	design2 <- getDesignFisher(
	    kMax = 3, alpha = 0.02, alpha0Vec = c(0.7, 0.5), method = "fullAlpha",
	    bindingFutility = TRUE, informationRates = c(0.3, 0.7, 1)
	)

	S1 <- getDataset(
	    sampleSize1 = c(12, 21),
	    sampleSize2 = c(18, 21),
	    mean1 = c(107.7, 84.9),
	    mean2 = c(165.6, 195.9),
	    stDev1 = c(128.5, 139.5),
	    stDev2 = c(120.1, 185.0)
	)

	F <- getDataset(
	    sampleSize1 = c(26, NA),
	    sampleSize2 = c(29, NA),
	    mean1 = c(86.48462, NA),
	    mean2 = c(148.34138, NA),
	    stDev1 = c(129.1485, NA),
	    stDev2 = c(122.888, NA)
	)

	dataInput1 <- getDataset(S1 = S1, F = F)

	x7 <- getAnalysisResults(
	    design = design2, dataInput = dataInput1,
	    directionUpper = FALSE,
	    normalApproximation = FALSE,
	    varianceOption = "pooled",
	    intersectionTest = "Sidak",
	    stratifiedAnalysis = FALSE,
	    stage = 2,
	    thetaH1 = c(-30, NA),
	    assumedStDevs = c(88, NA),
	    nPlanned = c(30),
	    allocationRatioPlanned = 2
	)

	## Comparison of the results of AnalysisResultsEnrichmentFisher object 'x7' with expected results
	expect_equal(x7$conditionalRejectionProbabilities[1, ], c(0.029419226, 0.36686704, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x7$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(x7$conditionalRejectionProbabilities[2, ], c(0.039811318, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x7$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(x7$conditionalPower[1, ], c(NA_real_, NA_real_, 0.70542247), tolerance = 1e-07, label = paste0("c(", paste0(x7$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(x7$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x7$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(x7$repeatedConfidenceIntervalLowerBounds[1, ], c(-194.17913, -187.01693, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x7$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(x7$repeatedConfidenceIntervalLowerBounds[2, ], c(-158.83149, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x7$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(x7$repeatedConfidenceIntervalUpperBounds[1, ], c(78.379133, 16.599438, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x7$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(x7$repeatedConfidenceIntervalUpperBounds[2, ], c(35.117971, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x7$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(x7$repeatedPValues[1, ], c(0.20187628, 0.035489058, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x7$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(x7$repeatedPValues[2, ], c(0.14858412, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x7$repeatedPValues[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x7), NA)))
	    expect_output(print(x7)$show())
	    invisible(capture.output(expect_error(summary(x7), NA)))
	    expect_output(summary(x7)$show())
	    x7CodeBased <- eval(parse(text = getObjectRCode(x7, stringWrapParagraphWidth = NULL)))
	    expect_equal(x7CodeBased$conditionalRejectionProbabilities, x7$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x7CodeBased$conditionalPower, x7$conditionalPower, tolerance = 1e-07)
	    expect_equal(x7CodeBased$repeatedConfidenceIntervalLowerBounds, x7$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x7CodeBased$repeatedConfidenceIntervalUpperBounds, x7$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x7CodeBased$repeatedPValues, x7$repeatedPValues, tolerance = 1e-07)
	    expect_type(names(x7), "character")
	    df <- as.data.frame(x7)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x7)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x8 <- getAnalysisResults(
	    design = design2, dataInput = dataInput1,
	    directionUpper = FALSE,
	    normalApproximation = FALSE,
	    varianceOption = "notPooled",
	    intersectionTest = "Bonferroni",
	    stratifiedAnalysis = FALSE,
	    stage = 2,
	    nPlanned = c(30),
	    allocationRatioPlanned = 2
	)

	## Comparison of the results of AnalysisResultsEnrichmentFisher object 'x8' with expected results
	expect_equal(x8$thetaH1[1, ], -88.724476, tolerance = 1e-07, label = paste0("c(", paste0(x8$thetaH1[1, ], collapse = ", "), ")"))
	expect_equal(x8$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(x8$thetaH1[2, ], collapse = ", "), ")"))
	expect_equal(x8$assumedStDevs[1, ], 147.03819, tolerance = 1e-07, label = paste0("c(", paste0(x8$assumedStDevs[1, ], collapse = ", "), ")"))
	expect_equal(x8$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(x8$assumedStDevs[2, ], collapse = ", "), ")"))
	expect_equal(x8$conditionalRejectionProbabilities[1, ], c(0.028559196, 0.34741778, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x8$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(x8$conditionalRejectionProbabilities[2, ], c(0.038896649, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x8$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(x8$conditionalPower[1, ], c(NA_real_, NA_real_, 0.878132), tolerance = 1e-07, label = paste0("c(", paste0(x8$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(x8$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x8$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(x8$repeatedConfidenceIntervalLowerBounds[1, ], c(-198.85804, -189.35465, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x8$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(x8$repeatedConfidenceIntervalLowerBounds[2, ], c(-159.22325, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x8$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(x8$repeatedConfidenceIntervalUpperBounds[1, ], c(83.058044, 17.838621, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x8$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(x8$repeatedConfidenceIntervalUpperBounds[2, ], c(35.509728, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x8$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(x8$repeatedPValues[1, ], c(0.20789586, 0.036783191, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x8$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(x8$repeatedPValues[2, ], c(0.15219281, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x8$repeatedPValues[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x8), NA)))
	    expect_output(print(x8)$show())
	    invisible(capture.output(expect_error(summary(x8), NA)))
	    expect_output(summary(x8)$show())
	    x8CodeBased <- eval(parse(text = getObjectRCode(x8, stringWrapParagraphWidth = NULL)))
	    expect_equal(x8CodeBased$thetaH1, x8$thetaH1, tolerance = 1e-07)
	    expect_equal(x8CodeBased$assumedStDevs, x8$assumedStDevs, tolerance = 1e-07)
	    expect_equal(x8CodeBased$conditionalRejectionProbabilities, x8$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x8CodeBased$conditionalPower, x8$conditionalPower, tolerance = 1e-07)
	    expect_equal(x8CodeBased$repeatedConfidenceIntervalLowerBounds, x8$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x8CodeBased$repeatedConfidenceIntervalUpperBounds, x8$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x8CodeBased$repeatedPValues, x8$repeatedPValues, tolerance = 1e-07)
	    expect_type(names(x8), "character")
	    df <- as.data.frame(x8)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x8)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_plan_section("Testing Analysis Enrichment Means Function (two sub-populations)")


test_that("'getAnalysisResults': stratified analysis, select S1 at first IA, gMax = 3", {
	.skipTestIfDisabled()

	# @refFS[Formula]{fs:adjustedPValueBonferroniEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimesEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCISidakEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCISpiessensEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSidakEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSimesEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSpiessensDeboisEnrichment}
	# @refFS[Formula]{fs:computeRCIsEnrichment}
	# @refFS[Formula]{fs:conditionalPowerEnrichment}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityEnrichment}
	# @refFS[Formula]{fs:stratifiedtTestEnrichment}
	S1 <- getDataset(
	    sampleSize2 = c(12, 33, 21),
	    sampleSize1 = c(18, 17, 23),
	    mean2       = c(107.7, 77.7, 84.9),
	    mean1       = c(125.6, 111.1, 99.9),
	    stDev2      = c(128.5, 133.3, 84.9),
	    stDev1      = c(120.1, 145.6, 74.3)
	)

	S2 <- getDataset(
	    sampleSize2 = c(14, NA, NA),
	    sampleSize1 = c(11, NA, NA),
	    mean2       = c(68.3, NA, NA),
	    mean1       = c(100.1, NA, NA),
	    stDev2      = c(124.0, NA, NA),
	    stDev1      = c(116.8, NA, NA)
	)

	S12 <- getDataset(
	    sampleSize2 = c(21, 12, 33),
	    sampleSize1 = c(21, 17, 31),
	    mean2       = c(84.9, 107.7, 77.7),
	    mean1       = c(135.9, 117.7, 97.7),
	    stDev2      = c(139.5, 107.7, 77.7),
	    stDev1      = c(185.0, 92.3, 87.3)
	)

	R <- getDataset(
	    sampleSize2 = c(33, NA, NA),
	    sampleSize1 = c(19, NA, NA),
	    mean2       = c(77.1, NA, NA),
	    mean1       = c(142.4, NA, NA),
	    stDev2      = c(163.5, NA, NA),
	    stDev1      = c(120.6, NA, NA)
	)

	dataInput1 <- getDataset(S1 = S1, S2 = S2, S12 = S12, R = R)

	## Comparison of the results of DatasetMeans object 'dataInput1' with expected results
	expect_equal(dataInput1$overallSampleSizes, c(18, 11, 21, 19, 12, 14, 21, 33, 35, NA_real_, 38, NA_real_, 45, NA_real_, 33, NA_real_, 58, NA_real_, 69, NA_real_, 66, NA_real_, 66, NA_real_), label = paste0("c(", paste0(dataInput1$overallSampleSizes, collapse = ", "), ")"))
	expect_equal(dataInput1$overallMeans, c(125.6, 100.1, 135.9, 142.4, 107.7, 68.3, 84.9, 77.1, 118.55714, NA_real_, 127.75789, NA_real_, 85.7, NA_real_, 93.190909, NA_real_, 111.15862, NA_real_, 114.25362, NA_real_, 85.445455, NA_real_, 85.445455, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(dataInput1$overallMeans, collapse = ", "), ")"))
	expect_equal(dataInput1$overallStDevs, c(120.1, 116.8, 185, 120.6, 128.5, 124, 139.5, 163.5, 131.30971, NA_real_, 149.22508, NA_real_, 131.26649, NA_real_, 127.56945, NA_real_, 111.80482, NA_real_, 125.32216, NA_real_, 117.82181, NA_real_, 105.0948, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(dataInput1$overallStDevs, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(dataInput1), NA)))
	    expect_output(print(dataInput1)$show())
	    invisible(capture.output(expect_error(summary(dataInput1), NA)))
	    expect_output(summary(dataInput1)$show())
	    dataInput1CodeBased <- eval(parse(text = getObjectRCode(dataInput1, stringWrapParagraphWidth = NULL)))
	    expect_equal(dataInput1CodeBased$overallSampleSizes, dataInput1$overallSampleSizes, tolerance = 1e-07)
	    expect_equal(dataInput1CodeBased$overallMeans, dataInput1$overallMeans, tolerance = 1e-07)
	    expect_equal(dataInput1CodeBased$overallStDevs, dataInput1$overallStDevs, tolerance = 1e-07)
	    expect_type(names(dataInput1), "character")
	    df <- as.data.frame(dataInput1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(dataInput1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getAnalysisResults': select S1 and S2 at first IA, select S1 at second, gMax = 3", {

	.skipTestIfDisabled()

	# @refFS[Formula]{fs:adjustedPValueBonferroniEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimesEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCISidakEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCISpiessensEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSidakEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSimesEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSpiessensDeboisEnrichment}
	# @refFS[Formula]{fs:computeRCIsEnrichment}
	# @refFS[Formula]{fs:conditionalPowerEnrichment}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityEnrichment}
	# @refFS[Formula]{fs:stratifiedtTestEnrichment}
	design1 <- getDesignInverseNormal(
	    kMax = 3, alpha = 0.02, futilityBounds = c(-0.5, 0),
	    bindingFutility = TRUE, typeOfDesign = "OF", informationRates = c(0.5, 0.7, 1)
	)

	S1N <- getDataset(
	    sampleSize1 = c(39, 34, NA),
	    sampleSize2 = c(33, 45, NA),
	    stDev1 = c(156.5026, 120.084, NA),
	    stDev2 = c(134.0254, 126.502, NA),
	    mean1 = c(131.146, 114.4, NA),
	    mean2 = c(93.191, 85.7, NA)
	)

	S2N <- getDataset(
	    sampleSize1 = c(32, NA, NA),
	    sampleSize2 = c(35, NA, NA),
	    stDev1 = c(163.645, NA, NA),
	    stDev2 = c(131.888, NA, NA),
	    mean1 = c(123.594, NA, NA),
	    mean2 = c(78.26, NA, NA)
	)

	F <- getDataset(
	    sampleSize1 = c(69, NA, NA),
	    sampleSize2 = c(80, NA, NA),
	    stDev1 = c(165.4682, NA, NA),
	    stDev2 = c(143.9796, NA, NA),
	    mean1 = c(129.2957, NA, NA),
	    mean2 = c(82.1875, NA, NA)
	)

	dataInput2 <- getDataset(S1 = S1N, S2 = S2N, F = F)

	## Comparison of the results of DatasetMeans object 'dataInput2' with expected results
	expect_equal(dataInput2$overallSampleSizes, c(39, 32, 69, 33, 35, 80, 73, NA_real_, NA_real_, 78, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(dataInput2$overallSampleSizes, collapse = ", "), ")"))
	expect_equal(dataInput2$overallMeans, c(131.146, 123.594, 129.2957, 93.191, 78.26, 82.1875, 123.34649, NA_real_, NA_real_, 88.869269, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(dataInput2$overallMeans, collapse = ", "), ")"))
	expect_equal(dataInput2$overallStDevs, c(156.5026, 163.645, 165.4682, 134.0254, 131.888, 143.9796, 140.02459, NA_real_, NA_real_, 128.93165, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(dataInput2$overallStDevs, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(dataInput2), NA)))
	    expect_output(print(dataInput2)$show())
	    invisible(capture.output(expect_error(summary(dataInput2), NA)))
	    expect_output(summary(dataInput2)$show())
	    dataInput2CodeBased <- eval(parse(text = getObjectRCode(dataInput2, stringWrapParagraphWidth = NULL)))
	    expect_equal(dataInput2CodeBased$overallSampleSizes, dataInput2$overallSampleSizes, tolerance = 1e-07)
	    expect_equal(dataInput2CodeBased$overallMeans, dataInput2$overallMeans, tolerance = 1e-07)
	    expect_equal(dataInput2CodeBased$overallStDevs, dataInput2$overallStDevs, tolerance = 1e-07)
	    expect_type(names(dataInput2), "character")
	    df <- as.data.frame(dataInput2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(dataInput2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x1 <- getAnalysisResults(
	    design = design1, dataInput = dataInput2,
	    directionUpper = TRUE,
	    normalApproximation = FALSE,
	    varianceOption = "pooled",
	    intersectionTest = "Sidak",
	    stratifiedAnalysis = FALSE,
	    stage = 2,
	    nPlanned = c(80),
	    allocationRatioPlanned = 2
	)

	## Comparison of the results of AnalysisResultsEnrichmentInverseNormal object 'x1' with expected results
	expect_equal(x1$thetaH1[1, ], 34.477224, tolerance = 1e-07, label = paste0("c(", paste0(x1$thetaH1[1, ], collapse = ", "), ")"))
	expect_equal(x1$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(x1$thetaH1[2, ], collapse = ", "), ")"))
	expect_equal(x1$thetaH1[3, ], NA_real_, label = paste0("c(", paste0(x1$thetaH1[3, ], collapse = ", "), ")"))
	expect_equal(x1$assumedStDevs[1, ], 134.40636, tolerance = 1e-07, label = paste0("c(", paste0(x1$assumedStDevs[1, ], collapse = ", "), ")"))
	expect_equal(x1$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(x1$assumedStDevs[2, ], collapse = ", "), ")"))
	expect_equal(x1$assumedStDevs[3, ], NA_real_, label = paste0("c(", paste0(x1$assumedStDevs[3, ], collapse = ", "), ")"))
	expect_equal(x1$conditionalRejectionProbabilities[1, ], c(0.016142454, 0.02613542, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(x1$conditionalRejectionProbabilities[2, ], c(0.016142454, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(x1$conditionalRejectionProbabilities[3, ], c(0.050007377, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
	expect_equal(x1$conditionalPower[1, ], c(NA_real_, NA_real_, 0.19507788), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(x1$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(x1$conditionalPower[3, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$conditionalPower[3, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds[1, ], c(-81.45856, -34.885408, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds[2, ], c(-79.606691, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds[3, ], c(-38.192738, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds[1, ], c(157.36856, 103.57092, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds[2, ], c(170.27469, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds[3, ], c(132.40914, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedPValues[1, ], c(0.34605439, 0.18712011, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedPValues[2, ], c(0.34605439, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedPValues[2, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedPValues[3, ], c(0.22233542, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedPValues[3, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x1), NA)))
	    expect_output(print(x1)$show())
	    invisible(capture.output(expect_error(summary(x1), NA)))
	    expect_output(summary(x1)$show())
	    x1CodeBased <- eval(parse(text = getObjectRCode(x1, stringWrapParagraphWidth = NULL)))
	    expect_equal(x1CodeBased$thetaH1, x1$thetaH1, tolerance = 1e-07)
	    expect_equal(x1CodeBased$assumedStDevs, x1$assumedStDevs, tolerance = 1e-07)
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

	design3 <- getDesignInverseNormal(
	    kMax = 3, alpha = 0.02, futilityBounds = c(-0.5, 0),
	    bindingFutility = TRUE, typeOfDesign = "OF", informationRates = c(0.5, 0.7, 1)
	)

	design2 <- getDesignFisher(
	    kMax = 3, alpha = 0.02, alpha0Vec = c(0.7, 0.5), method = "equalAlpha",
	    bindingFutility = TRUE, informationRates = c(0.3, 0.7, 1)
	)

	x2 <- getAnalysisResults(
	    design = design3, dataInput = dataInput2,
	    directionUpper = TRUE,
	    normalApproximation = FALSE,
	    varianceOption = "notPooled",
	    intersectionTest = "Simes",
	    stratifiedAnalysis = FALSE,
	    stage = 2,
	    thetaH1 = c(50, 30, NA),
	    assumedStDevs = c(122, 88, NA),
	    nPlanned = 80,
	    allocationRatioPlanned = 0.5
	)

	## Comparison of the results of AnalysisResultsEnrichmentInverseNormal object 'x2' with expected results
	expect_equal(x2$conditionalRejectionProbabilities[1, ], c(0.03098783, 0.056162964, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(x2$conditionalRejectionProbabilities[2, ], c(0.03098783, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(x2$conditionalRejectionProbabilities[3, ], c(0.045486533, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
	expect_equal(x2$conditionalPower[1, ], c(NA_real_, NA_real_, 0.55574729), tolerance = 1e-07, label = paste0("c(", paste0(x2$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(x2$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(x2$conditionalPower[3, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$conditionalPower[3, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds[1, ], c(-79.922689, -34.33441, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds[2, ], c(-81.369964, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds[3, ], c(-39.221831, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds[1, ], c(155.83269, 103.18642, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds[2, ], c(172.03796, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds[3, ], c(133.43823, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedPValues[1, ], c(0.27466247, 0.13478543, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedPValues[2, ], c(0.27466247, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedPValues[2, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedPValues[3, ], c(0.23257404, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedPValues[3, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x2), NA)))
	    expect_output(print(x2)$show())
	    invisible(capture.output(expect_error(summary(x2), NA)))
	    expect_output(summary(x2)$show())
	    x2CodeBased <- eval(parse(text = getObjectRCode(x2, stringWrapParagraphWidth = NULL)))
	    expect_equal(x2CodeBased$conditionalRejectionProbabilities, x2$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x2CodeBased$conditionalPower, x2$conditionalPower, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedConfidenceIntervalLowerBounds, x2$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedConfidenceIntervalUpperBounds, x2$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedPValues, x2$repeatedPValues, tolerance = 1e-07)
	    expect_type(names(x2), "character")
	    df <- as.data.frame(x2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x3 <- getAnalysisResults(
	    design = design2, dataInput = dataInput2,
	    directionUpper = TRUE,
	    normalApproximation = FALSE,
	    varianceOption = "pooled",
	    intersectionTest = "Sidak",
	    stratifiedAnalysis = FALSE,
	    stage = 2,
	    nPlanned = 80,
	    allocationRatioPlanned = 0.5
	)

	## Comparison of the results of AnalysisResultsEnrichmentFisher object 'x3' with expected results
	expect_equal(x3$thetaH1[1, ], 34.477224, tolerance = 1e-07, label = paste0("c(", paste0(x3$thetaH1[1, ], collapse = ", "), ")"))
	expect_equal(x3$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(x3$thetaH1[2, ], collapse = ", "), ")"))
	expect_equal(x3$thetaH1[3, ], NA_real_, label = paste0("c(", paste0(x3$thetaH1[3, ], collapse = ", "), ")"))
	expect_equal(x3$assumedStDevs[1, ], 134.40636, tolerance = 1e-07, label = paste0("c(", paste0(x3$assumedStDevs[1, ], collapse = ", "), ")"))
	expect_equal(x3$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(x3$assumedStDevs[2, ], collapse = ", "), ")"))
	expect_equal(x3$assumedStDevs[3, ], NA_real_, label = paste0("c(", paste0(x3$assumedStDevs[3, ], collapse = ", "), ")"))
	expect_equal(x3$conditionalRejectionProbabilities[1, ], c(0.01300837, 0.0063168592, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(x3$conditionalRejectionProbabilities[2, ], c(0.01300837, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(x3$conditionalRejectionProbabilities[3, ], c(0.024114983, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
	expect_equal(x3$conditionalPower[1, ], c(NA_real_, NA_real_, 0.078920631), tolerance = 1e-07, label = paste0("c(", paste0(x3$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(x3$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(x3$conditionalPower[3, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$conditionalPower[3, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds[1, ], c(-58.494162, -30.46834, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds[2, ], c(-55.474155, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds[3, ], c(-22.271868, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds[1, ], c(134.40416, 94.713072, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds[2, ], c(146.14216, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds[3, ], c(116.48827, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedPValues[1, ], c(0.29239601, 0.21229229, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedPValues[2, ], c(0.29239601, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedPValues[2, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedPValues[3, ], c(0.15217469, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedPValues[3, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x3), NA)))
	    expect_output(print(x3)$show())
	    invisible(capture.output(expect_error(summary(x3), NA)))
	    expect_output(summary(x3)$show())
	    x3CodeBased <- eval(parse(text = getObjectRCode(x3, stringWrapParagraphWidth = NULL)))
	    expect_equal(x3CodeBased$thetaH1, x3$thetaH1, tolerance = 1e-07)
	    expect_equal(x3CodeBased$assumedStDevs, x3$assumedStDevs, tolerance = 1e-07)
	    expect_equal(x3CodeBased$conditionalRejectionProbabilities, x3$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x3CodeBased$conditionalPower, x3$conditionalPower, tolerance = 1e-07)
	    expect_equal(x3CodeBased$repeatedConfidenceIntervalLowerBounds, x3$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x3CodeBased$repeatedConfidenceIntervalUpperBounds, x3$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x3CodeBased$repeatedPValues, x3$repeatedPValues, tolerance = 1e-07)
	    expect_type(names(x3), "character")
	    df <- as.data.frame(x3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x4 <- getAnalysisResults(
	    design = design2, dataInput = dataInput2,
	    directionUpper = TRUE,
	    normalApproximation = FALSE,
	    varianceOption = "notPooled",
	    intersectionTest = "Simes",
	    stratifiedAnalysis = FALSE,
	    stage = 2,
	    thetaH1 = c(50, NA, NA),
	    assumedStDevs = c(122, NA, NA),
	    nPlanned = 80,
	    allocationRatioPlanned = 0.5
	)

	## Comparison of the results of AnalysisResultsEnrichmentFisher object 'x4' with expected results
	expect_equal(x4$conditionalRejectionProbabilities[1, ], c(0.018024059, 0.0095704388, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(x4$conditionalRejectionProbabilities[2, ], c(0.018024059, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(x4$conditionalRejectionProbabilities[3, ], c(0.022674244, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
	expect_equal(x4$conditionalPower[1, ], c(NA_real_, NA_real_, 0.26935817), tolerance = 1e-07, label = paste0("c(", paste0(x4$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(x4$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x4$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(x4$conditionalPower[3, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x4$conditionalPower[3, ], collapse = ", "), ")"))
	expect_equal(x4$repeatedConfidenceIntervalLowerBounds[1, ], c(-57.292213, -30.050759, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(x4$repeatedConfidenceIntervalLowerBounds[2, ], c(-56.802775, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(x4$repeatedConfidenceIntervalLowerBounds[3, ], c(-23.100932, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
	expect_equal(x4$repeatedConfidenceIntervalUpperBounds[1, ], c(133.20221, 94.521132, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(x4$repeatedConfidenceIntervalUpperBounds[2, ], c(147.47078, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(x4$repeatedConfidenceIntervalUpperBounds[3, ], c(117.31733, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
	expect_equal(x4$repeatedPValues[1, ], c(0.20840036, 0.16345568, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(x4$repeatedPValues[2, ], c(0.20840036, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedPValues[2, ], collapse = ", "), ")"))
	expect_equal(x4$repeatedPValues[3, ], c(0.16277762, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x4$repeatedPValues[3, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x4), NA)))
	    expect_output(print(x4)$show())
	    invisible(capture.output(expect_error(summary(x4), NA)))
	    expect_output(summary(x4)$show())
	    x4CodeBased <- eval(parse(text = getObjectRCode(x4, stringWrapParagraphWidth = NULL)))
	    expect_equal(x4CodeBased$conditionalRejectionProbabilities, x4$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x4CodeBased$conditionalPower, x4$conditionalPower, tolerance = 1e-07)
	    expect_equal(x4CodeBased$repeatedConfidenceIntervalLowerBounds, x4$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x4CodeBased$repeatedConfidenceIntervalUpperBounds, x4$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x4CodeBased$repeatedPValues, x4$repeatedPValues, tolerance = 1e-07)
	    expect_type(names(x4), "character")
	    df <- as.data.frame(x4)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x4)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_plan_section("Testing Analysis Enrichment Means Function (more sub-populations)")


test_that("'getAnalysisResults': select S1 and S3 at first IA, select S1 at second, gMax = 4", {
	.skipTestIfDisabled()

	# @refFS[Formula]{fs:adjustedPValueBonferroniEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimesEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCISidakEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCISpiessensEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSidakEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSimesEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSpiessensDeboisEnrichment}
	# @refFS[Formula]{fs:computeRCIsEnrichment}
	# @refFS[Formula]{fs:conditionalPowerEnrichment}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityEnrichment}
	# @refFS[Formula]{fs:stratifiedtTestEnrichment}
	S1 <- getDataset(
	    sampleSize1 = c(14, 22, 24),
	    sampleSize2 = c(11, 18, 21),
	    mean1       = c(68.3, 107.4, 101.2),
	    mean2       = c(100.1, 140.9, 133.8),
	    stDev1      = c(124.0, 134.7, 124.2),
	    stDev2      = c(116.8, 133.7, 131.2)
	)

	S2 <- getDataset(
	    sampleSize1 = c(12, NA, NA),
	    sampleSize2 = c(18, NA, NA),
	    mean1       = c(107.7, NA, NA),
	    mean2       = c(125.6, NA, NA),
	    stDev1      = c(128.5, NA, NA),
	    stDev2      = c(120.1, NA, NA)
	)

	S3 <- getDataset(
	    sampleSize1 = c(17, 24, NA),
	    sampleSize2 = c(14, 19, NA),
	    mean1       = c(64.3, 101.4, NA),
	    mean2       = c(103.1, 170.4, NA),
	    stDev1      = c(128.0, 125.3, NA),
	    stDev2      = c(111.8, 143.6, NA)
	)

	F <- getDataset(
	    sampleSize1 = c(83, NA, NA),
	    sampleSize2 = c(79, NA, NA),
	    mean1       = c(77.1, NA, NA),
	    mean2       = c(142.4, NA, NA),
	    stDev1      = c(163.5, NA, NA),
	    stDev2      = c(120.6, NA, NA)
	)

	dataInput3 <- getDataset(S1 = S1, S2 = S2, S3 = S3, F = F)

	## Comparison of the results of DatasetMeans object 'dataInput3' with expected results
	expect_equal(dataInput3$overallSampleSizes, c(14, 12, 17, 83, 11, 18, 14, 79, 36, NA_real_, 41, NA_real_, 29, NA_real_, 33, NA_real_, 60, NA_real_, NA_real_, NA_real_, 50, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(dataInput3$overallSampleSizes, collapse = ", "), ")"))
	expect_equal(dataInput3$overallMeans, c(68.3, 107.7, 64.3, 77.1, 100.1, 125.6, 103.1, 142.4, 92.194444, NA_real_, 86.017073, NA_real_, 125.42414, NA_real_, 141.84848, NA_real_, 95.796667, NA_real_, NA_real_, NA_real_, 128.942, NA_real_, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(dataInput3$overallMeans, collapse = ", "), ")"))
	expect_equal(dataInput3$overallStDevs, c(124, 128.5, 128, 163.5, 116.8, 120.1, 111.8, 120.6, 130.27375, NA_real_, 126.18865, NA_real_, 127.0088, NA_real_, 133.48411, NA_real_, 126.8892, NA_real_, NA_real_, NA_real_, 127.51934, NA_real_, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(dataInput3$overallStDevs, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(dataInput3), NA)))
	    expect_output(print(dataInput3)$show())
	    invisible(capture.output(expect_error(summary(dataInput3), NA)))
	    expect_output(summary(dataInput3)$show())
	    dataInput3CodeBased <- eval(parse(text = getObjectRCode(dataInput3, stringWrapParagraphWidth = NULL)))
	    expect_equal(dataInput3CodeBased$overallSampleSizes, dataInput3$overallSampleSizes, tolerance = 1e-07)
	    expect_equal(dataInput3CodeBased$overallMeans, dataInput3$overallMeans, tolerance = 1e-07)
	    expect_equal(dataInput3CodeBased$overallStDevs, dataInput3$overallStDevs, tolerance = 1e-07)
	    expect_type(names(dataInput3), "character")
	    df <- as.data.frame(dataInput3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(dataInput3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	design1 <- getDesignInverseNormal(
	    kMax = 3, alpha = 0.025, typeOfDesign = "WT",
	    deltaWT = 0.28, informationRates = c(0.5, 0.7, 1)
	)

	x1 <- getAnalysisResults(
	    design = design1, dataInput = dataInput3,
	    directionUpper = FALSE,
	    normalApproximation = FALSE,
	    varianceOption = "notPooled",
	    intersectionTest = "Simes",
	    stratifiedAnalysis = FALSE
	)

	## Comparison of the results of AnalysisResultsEnrichmentInverseNormal object 'x1' with expected results
	expect_equal(x1$thetaH1[1, ], -33.145333, tolerance = 1e-07, label = paste0("c(", paste0(x1$thetaH1[1, ], collapse = ", "), ")"))
	expect_equal(x1$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(x1$thetaH1[2, ], collapse = ", "), ")"))
	expect_equal(x1$thetaH1[3, ], NA_real_, label = paste0("c(", paste0(x1$thetaH1[3, ], collapse = ", "), ")"))
	expect_equal(x1$thetaH1[4, ], NA_real_, label = paste0("c(", paste0(x1$thetaH1[4, ], collapse = ", "), ")"))
	expect_equal(x1$assumedStDevs[1, ], 127.17548, tolerance = 1e-07, label = paste0("c(", paste0(x1$assumedStDevs[1, ], collapse = ", "), ")"))
	expect_equal(x1$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(x1$assumedStDevs[2, ], collapse = ", "), ")"))
	expect_equal(x1$assumedStDevs[3, ], NA_real_, label = paste0("c(", paste0(x1$assumedStDevs[3, ], collapse = ", "), ")"))
	expect_equal(x1$assumedStDevs[4, ], NA_real_, label = paste0("c(", paste0(x1$assumedStDevs[4, ], collapse = ", "), ")"))
	expect_equal(x1$conditionalRejectionProbabilities[1, ], c(0.0046188669, 0.003141658, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(x1$conditionalRejectionProbabilities[2, ], c(0.0046188669, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(x1$conditionalRejectionProbabilities[3, ], c(0.0046188669, 0.0093523023, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
	expect_equal(x1$conditionalRejectionProbabilities[4, ], c(0.41158519, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalRejectionProbabilities[4, ], collapse = ", "), ")"))
	expect_equal(x1$conditionalPower[1, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(x1$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(x1$conditionalPower[3, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$conditionalPower[3, ], collapse = ", "), ")"))
	expect_equal(x1$conditionalPower[4, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$conditionalPower[4, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds[1, ], c(-189.95235, -137.25075, -108.04127), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds[2, ], c(-170.18127, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds[3, ], c(-175.96326, -146.15913, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds[4, ], c(-132.10549, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalLowerBounds[4, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds[1, ], c(126.35235, 72.344345, 43.127962), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds[2, ], c(134.38127, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds[3, ], c(98.363257, 46.507217, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds[4, ], c(1.5054896, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalUpperBounds[4, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedPValues[1, ], c(0.5, 0.35403281, 0.20618784), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedPValues[2, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedPValues[3, ], c(0.5, 0.26324129, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedPValues[3, ], collapse = ", "), ")"))
	expect_equal(x1$repeatedPValues[4, ], c(0.029329288, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedPValues[4, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x1), NA)))
	    expect_output(print(x1)$show())
	    invisible(capture.output(expect_error(summary(x1), NA)))
	    expect_output(summary(x1)$show())
	    x1CodeBased <- eval(parse(text = getObjectRCode(x1, stringWrapParagraphWidth = NULL)))
	    expect_equal(x1CodeBased$thetaH1, x1$thetaH1, tolerance = 1e-07)
	    expect_equal(x1CodeBased$assumedStDevs, x1$assumedStDevs, tolerance = 1e-07)
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

})

test_that("'getAnalysisResults': stratified analysis, gMax = 4", {

	.skipTestIfDisabled()

	# @refFS[Formula]{fs:adjustedPValueBonferroniEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimesEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCISidakEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCISpiessensEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSidakEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSimesEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSpiessensDeboisEnrichment}
	# @refFS[Formula]{fs:computeRCIsEnrichment}
	# @refFS[Formula]{fs:conditionalPowerEnrichment}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityEnrichment}
	# @refFS[Formula]{fs:stratifiedtTestEnrichment}
	S1 <- getDataset(
	    sampleSize1 = c(14, 22, NA),
	    sampleSize2 = c(11, 18, NA),
	    mean1       = c(68.3, 107.4, NA),
	    mean2       = c(100.1, 140.9, NA),
	    stDev1      = c(124.0, 134.7, NA),
	    stDev2      = c(116.8, 133.7, NA)
	)

	S2 <- getDataset(
	    sampleSize1 = c(12, NA, NA),
	    sampleSize2 = c(18, NA, NA),
	    mean1       = c(107.7, NA, NA),
	    mean2       = c(125.6, NA, NA),
	    stDev1      = c(128.5, NA, NA),
	    stDev2      = c(120.1, NA, NA)
	)

	S3 <- getDataset(
	    sampleSize1 = c(17, 24, NA),
	    sampleSize2 = c(14, 19, NA),
	    mean1       = c(64.3, 101.4, NA),
	    mean2       = c(103.1, 170.4, NA),
	    stDev1      = c(128.0, 125.3, NA),
	    stDev2      = c(111.8, 143.6, NA)
	)

	S12 <- getDataset(
	    sampleSize1 = c(21, 12, 33),
	    sampleSize2 = c(21, 17, 31),
	    mean1       = c(84.9, 107.7, 77.7),
	    mean2       = c(135.9, 117.7, 97.7),
	    stDev1      = c(139.5, 107.7, 77.7),
	    stDev2      = c(185.0, 92.3, 87.3)
	)

	S13 <- getDataset(
	    sampleSize1 = c(21, 12, 33),
	    sampleSize2 = c(21, 17, 31),
	    mean1       = c(84.9, 107.7, 77.7),
	    mean2       = c(135.9, 117.7, 97.7),
	    stDev1      = c(139.5, 107.7, 77.7),
	    stDev2      = c(185.0, 92.3, 87.3)
	)

	S23 <- getDataset(
	    sampleSize1 = c(21, 12, 33),
	    sampleSize2 = c(21, 17, 31),
	    mean1       = c(84.9, 107.7, 77.7),
	    mean2       = c(135.9, 117.7, 97.7),
	    stDev1      = c(139.5, 107.7, 77.7),
	    stDev2      = c(185.0, 92.3, 87.3)
	)

	S123 <- getDataset(
	    sampleSize1 = c(21, 12, 33),
	    sampleSize2 = c(21, 17, 31),
	    mean1       = c(84.9, 107.7, 77.7),
	    mean2       = c(135.9, 117.7, 97.7),
	    stDev1      = c(139.5, 107.7, 77.7),
	    stDev2      = c(185.0, 92.3, 87.3)
	)

	R <- getDataset(
	    sampleSize1 = c(33, NA, NA),
	    sampleSize2 = c(19, NA, NA),
	    mean1       = c(77.1, NA, NA),
	    mean2       = c(142.4, NA, NA),
	    stDev1      = c(163.5, NA, NA),
	    stDev2      = c(120.6, NA, NA)
	)

	dataInput4 <- getDataset(S1 = S1, S2 = S2, S3 = S3, S12 = S12, S23 = S23, S13 = S13, S123 = S123, R = R)

	## Comparison of the results of DatasetMeans object 'dataInput4' with expected results
	expect_equal(dataInput4$overallSampleSizes, c(14, 12, 17, 21, 21, 21, 21, 33, 11, 18, 14, 21, 21, 21, 21, 19, 36, NA_real_, 41, 33, 33, 33, 33, NA_real_, 29, NA_real_, 33, 38, 38, 38, 38, NA_real_, NA_real_, NA_real_, NA_real_, 66, 66, 66, 66, NA_real_, NA_real_, NA_real_, NA_real_, 69, 69, 69, 69, NA_real_), label = paste0("c(", paste0(dataInput4$overallSampleSizes, collapse = ", "), ")"))
	expect_equal(dataInput4$overallMeans, c(68.3, 107.7, 64.3, 84.9, 84.9, 84.9, 84.9, 77.1, 100.1, 125.6, 103.1, 135.9, 135.9, 135.9, 135.9, 142.4, 92.194444, NA_real_, 86.017073, 93.190909, 93.190909, 93.190909, 93.190909, NA_real_, 125.42414, NA_real_, 141.84848, 127.75789, 127.75789, 127.75789, 127.75789, NA_real_, NA_real_, NA_real_, NA_real_, 85.445455, 85.445455, 85.445455, 85.445455, NA_real_, NA_real_, NA_real_, NA_real_, 114.25362, 114.25362, 114.25362, 114.25362, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(dataInput4$overallMeans, collapse = ", "), ")"))
	expect_equal(dataInput4$overallStDevs, c(124, 128.5, 128, 139.5, 139.5, 139.5, 139.5, 163.5, 116.8, 120.1, 111.8, 185, 185, 185, 185, 120.6, 130.27375, NA_real_, 126.18865, 127.56945, 127.56945, 127.56945, 127.56945, NA_real_, 127.0088, NA_real_, 133.48411, 149.22508, 149.22508, 149.22508, 149.22508, NA_real_, NA_real_, NA_real_, NA_real_, 105.0948, 105.0948, 105.0948, 105.0948, NA_real_, NA_real_, NA_real_, NA_real_, 125.32216, 125.32216, 125.32216, 125.32216, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(dataInput4$overallStDevs, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(dataInput4), NA)))
	    expect_output(print(dataInput4)$show())
	    invisible(capture.output(expect_error(summary(dataInput4), NA)))
	    expect_output(summary(dataInput4)$show())
	    dataInput4CodeBased <- eval(parse(text = getObjectRCode(dataInput4, stringWrapParagraphWidth = NULL)))
	    expect_equal(dataInput4CodeBased$overallSampleSizes, dataInput4$overallSampleSizes, tolerance = 1e-07)
	    expect_equal(dataInput4CodeBased$overallMeans, dataInput4$overallMeans, tolerance = 1e-07)
	    expect_equal(dataInput4CodeBased$overallStDevs, dataInput4$overallStDevs, tolerance = 1e-07)
	    expect_type(names(dataInput4), "character")
	    df <- as.data.frame(dataInput4)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(dataInput4)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	design1 <- getDesignInverseNormal(
	    kMax = 3, alpha = 0.025, typeOfDesign = "WT",
	    deltaWT = 0.28, informationRates = c(0.5, 0.7, 1)
	)

	x2 <- getAnalysisResults(
	    design = design1, dataInput = dataInput4,
	    directionUpper = FALSE,
	    normalApproximation = FALSE,
	    varianceOption = "notPooled",
	    intersectionTest = "Simes",
	    stratifiedAnalysis = TRUE,
	    stage = 2
	)

	## Comparison of the results of AnalysisResultsEnrichmentInverseNormal object 'x2' with expected results
	expect_equal(x2$thetaH1[1, ], -34.35943, tolerance = 1e-07, label = paste0("c(", paste0(x2$thetaH1[1, ], collapse = ", "), ")"))
	expect_equal(x2$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(x2$thetaH1[2, ], collapse = ", "), ")"))
	expect_equal(x2$thetaH1[3, ], -39.831088, tolerance = 1e-07, label = paste0("c(", paste0(x2$thetaH1[3, ], collapse = ", "), ")"))
	expect_equal(x2$thetaH1[4, ], NA_real_, label = paste0("c(", paste0(x2$thetaH1[4, ], collapse = ", "), ")"))
	expect_equal(x2$assumedStDevs[1, ], 135.6664, tolerance = 1e-07, label = paste0("c(", paste0(x2$assumedStDevs[1, ], collapse = ", "), ")"))
	expect_equal(x2$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(x2$assumedStDevs[2, ], collapse = ", "), ")"))
	expect_equal(x2$assumedStDevs[3, ], 135.69515, tolerance = 1e-07, label = paste0("c(", paste0(x2$assumedStDevs[3, ], collapse = ", "), ")"))
	expect_equal(x2$assumedStDevs[4, ], NA_real_, label = paste0("c(", paste0(x2$assumedStDevs[4, ], collapse = ", "), ")"))
	expect_equal(x2$conditionalRejectionProbabilities[1, ], c(0.14436944, 0.18888867, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(x2$conditionalRejectionProbabilities[2, ], c(0.14436944, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(x2$conditionalRejectionProbabilities[3, ], c(0.14436944, 0.23567728, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
	expect_equal(x2$conditionalRejectionProbabilities[4, ], c(0.33356756, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$conditionalRejectionProbabilities[4, ], collapse = ", "), ")"))
	expect_equal(x2$conditionalPower[1, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(x2$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(x2$conditionalPower[3, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$conditionalPower[3, ], collapse = ", "), ")"))
	expect_equal(x2$conditionalPower[4, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$conditionalPower[4, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds[1, ], c(-124.13667, -87.790806, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds[2, ], c(-119.97906, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds[3, ], c(-122.68924, -91.731817, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds[4, ], c(-97.969856, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalLowerBounds[4, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds[1, ], c(28.41771, 15.834301, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds[2, ], c(30.295343, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds[3, ], c(25.470801, 9.1408918, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds[4, ], c(3.369313, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalUpperBounds[4, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedPValues[1, ], c(0.096549841, 0.052699984, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedPValues[2, ], c(0.096549841, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedPValues[2, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedPValues[3, ], c(0.096549841, 0.042135201, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedPValues[3, ], collapse = ", "), ")"))
	expect_equal(x2$repeatedPValues[4, ], c(0.039953198, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedPValues[4, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x2), NA)))
	    expect_output(print(x2)$show())
	    invisible(capture.output(expect_error(summary(x2), NA)))
	    expect_output(summary(x2)$show())
	    x2CodeBased <- eval(parse(text = getObjectRCode(x2, stringWrapParagraphWidth = NULL)))
	    expect_equal(x2CodeBased$thetaH1, x2$thetaH1, tolerance = 1e-07)
	    expect_equal(x2CodeBased$assumedStDevs, x2$assumedStDevs, tolerance = 1e-07)
	    expect_equal(x2CodeBased$conditionalRejectionProbabilities, x2$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x2CodeBased$conditionalPower, x2$conditionalPower, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedConfidenceIntervalLowerBounds, x2$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedConfidenceIntervalUpperBounds, x2$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x2CodeBased$repeatedPValues, x2$repeatedPValues, tolerance = 1e-07)
	    expect_type(names(x2), "character")
	    df <- as.data.frame(x2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_plan_section("Testing Analysis Enrichment Means Function (more sub-populations)")


test_that("'getAnalysisResults': select S1 at first IA, gMax = 3, no early efficacy stop", {
	.skipTestIfDisabled()

	# @refFS[Formula]{fs:adjustedPValueBonferroniEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimesEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCISidakEnrichment}
	# @refFS[Formula]{fs:adjustedPValueForRCISpiessensEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSidakEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSimesEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSpiessensDeboisEnrichment}
	# @refFS[Formula]{fs:computeRCIsEnrichment}
	# @refFS[Formula]{fs:conditionalPowerEnrichment}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityEnrichment}
	# @refFS[Formula]{fs:stratifiedtTestEnrichment}
	S1 <- getDataset(
	    sampleSize1 = c(14, 22, 24),
	    sampleSize2 = c(11, 18, 21),
	    mean1       = c(68.3, 107.4, 101.2),
	    mean2       = c(100.1, 140.9, 133.8),
	    stDev1      = c(124.0, 134.7, 124.2),
	    stDev2      = c(116.8, 133.7, 131.2)
	)

	S2 <- getDataset(
	    sampleSize1 = c(12, NA, NA),
	    sampleSize2 = c(18, NA, NA),
	    mean1       = c(107.7, NA, NA),
	    mean2       = c(125.6, NA, NA),
	    stDev1      = c(128.5, NA, NA),
	    stDev2      = c(120.1, NA, NA)
	)

	F <- getDataset(
	    sampleSize1 = c(83, NA, NA),
	    sampleSize2 = c(79, NA, NA),
	    mean1       = c(77.1, NA, NA),
	    mean2       = c(142.4, NA, NA),
	    stDev1      = c(163.5, NA, NA),
	    stDev2      = c(120.6, NA, NA)
	)

	dataInput3 <- getDataset(S1 = S1, S2 = S2, F = F)

	## Comparison of the results of DatasetMeans object 'dataInput3' with expected results
	expect_equal(dataInput3$overallSampleSizes, c(14, 12, 83, 11, 18, 79, 36, NA_real_, NA_real_, 29, NA_real_, NA_real_, 60, NA_real_, NA_real_, 50, NA_real_, NA_real_), label = paste0("c(", paste0(dataInput3$overallSampleSizes, collapse = ", "), ")"))
	expect_equal(dataInput3$overallMeans, c(68.3, 107.7, 77.1, 100.1, 125.6, 142.4, 92.194444, NA_real_, NA_real_, 125.42414, NA_real_, NA_real_, 95.796667, NA_real_, NA_real_, 128.942, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(dataInput3$overallMeans, collapse = ", "), ")"))
	expect_equal(dataInput3$overallStDevs, c(124, 128.5, 163.5, 116.8, 120.1, 120.6, 130.27375, NA_real_, NA_real_, 127.0088, NA_real_, NA_real_, 126.8892, NA_real_, NA_real_, 127.51934, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(dataInput3$overallStDevs, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(dataInput3), NA)))
	    expect_output(print(dataInput3)$show())
	    invisible(capture.output(expect_error(summary(dataInput3), NA)))
	    expect_output(summary(dataInput3)$show())
	    dataInput3CodeBased <- eval(parse(text = getObjectRCode(dataInput3, stringWrapParagraphWidth = NULL)))
	    expect_equal(dataInput3CodeBased$overallSampleSizes, dataInput3$overallSampleSizes, tolerance = 1e-07)
	    expect_equal(dataInput3CodeBased$overallMeans, dataInput3$overallMeans, tolerance = 1e-07)
	    expect_equal(dataInput3CodeBased$overallStDevs, dataInput3$overallStDevs, tolerance = 1e-07)
	    expect_type(names(dataInput3), "character")
	    df <- as.data.frame(dataInput3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(dataInput3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	design3 <- getDesignInverseNormal(
	    kMax = 3, alpha = 0.025, typeOfDesign = "noEarlyEfficacy",
	    informationRates = c(0.4, 0.7, 1)
	)

	x3 <- getAnalysisResults(
	    design = design3, dataInput = dataInput3,
	    thetaH0 = 30,
	    directionUpper = FALSE,
	    normalApproximation = FALSE,
	    varianceOption = "notPooled",
	    intersectionTest = "Simes",
	    stratifiedAnalysis = FALSE
	)

	## Comparison of the results of AnalysisResultsEnrichmentInverseNormal object 'x3' with expected results
	expect_equal(x3$thetaH1[1, ], -33.145333, tolerance = 1e-07, label = paste0("c(", paste0(x3$thetaH1[1, ], collapse = ", "), ")"))
	expect_equal(x3$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(x3$thetaH1[2, ], collapse = ", "), ")"))
	expect_equal(x3$thetaH1[3, ], NA_real_, label = paste0("c(", paste0(x3$thetaH1[3, ], collapse = ", "), ")"))
	expect_equal(x3$assumedStDevs[1, ], 127.17548, tolerance = 1e-07, label = paste0("c(", paste0(x3$assumedStDevs[1, ], collapse = ", "), ")"))
	expect_equal(x3$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(x3$assumedStDevs[2, ], collapse = ", "), ")"))
	expect_equal(x3$assumedStDevs[3, ], NA_real_, label = paste0("c(", paste0(x3$assumedStDevs[3, ], collapse = ", "), ")"))
	expect_equal(x3$conditionalRejectionProbabilities[1, ], c(0.043562209, 0.16805804, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(x3$conditionalRejectionProbabilities[2, ], c(0.043562209, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(x3$conditionalRejectionProbabilities[3, ], c(0.72997271, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
	expect_equal(x3$conditionalPower[1, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(x3$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(x3$conditionalPower[3, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$conditionalPower[3, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds[1, ], c(NA_real_, NA_real_, -94.8291), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds[3, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds[1, ], c(NA_real_, NA_real_, 29.811159), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds[3, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedPValues[1, ], c(NA_real_, NA_real_, 0.010432269), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedPValues[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$repeatedPValues[2, ], collapse = ", "), ")"))
	expect_equal(x3$repeatedPValues[3, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$repeatedPValues[3, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x3), NA)))
	    expect_output(print(x3)$show())
	    invisible(capture.output(expect_error(summary(x3), NA)))
	    expect_output(summary(x3)$show())
	    x3CodeBased <- eval(parse(text = getObjectRCode(x3, stringWrapParagraphWidth = NULL)))
	    expect_equal(x3CodeBased$thetaH1, x3$thetaH1, tolerance = 1e-07)
	    expect_equal(x3CodeBased$assumedStDevs, x3$assumedStDevs, tolerance = 1e-07)
	    expect_equal(x3CodeBased$conditionalRejectionProbabilities, x3$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(x3CodeBased$conditionalPower, x3$conditionalPower, tolerance = 1e-07)
	    expect_equal(x3CodeBased$repeatedConfidenceIntervalLowerBounds, x3$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(x3CodeBased$repeatedConfidenceIntervalUpperBounds, x3$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(x3CodeBased$repeatedPValues, x3$repeatedPValues, tolerance = 1e-07)
	    expect_type(names(x3), "character")
	    df <- as.data.frame(x3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}
})

